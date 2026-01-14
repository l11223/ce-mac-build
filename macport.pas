unit macport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, Unix, BaseUnix, math;

// Process and thread functions
function OpenProcess(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; cdecl;
function ReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: SIZE_T; var lpNumberOfBytesRead: SIZE_T): BOOL; cdecl;
function WriteProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: SIZE_T; var lpNumberOfBytesWritten: SIZE_T): BOOL; cdecl;
function CloseHandle(hObject: THandle): BOOL; cdecl;

// Thread context functions for ARM64
function GetThreadContextArm64(hThread: THandle; var lpContext: pointer): BOOL; cdecl;
function SetThreadContextArm64(hThread: THandle; var lpContext: pointer): BOOL; cdecl;

// System information
function getCPUCount: integer;
procedure macPortFixRegPath;

// Memory functions
function VirtualAllocEx(hProcess: THandle; lpAddress: Pointer; dwSize: SIZE_T; flAllocationType: DWORD; flProtect: DWORD): Pointer; cdecl;
function VirtualFreeEx(hProcess: THandle; lpAddress: Pointer; dwSize: SIZE_T; dwFreeType: DWORD): BOOL; cdecl;

// Debugging functions
function DebugActiveProcess(dwProcessId: DWORD): BOOL; cdecl;
function DebugActiveProcessStop(dwProcessId: DWORD): BOOL; cdecl;
function WaitForDebugEvent(var lpDebugEvent: pointer; dwMilliseconds: DWORD): BOOL; cdecl;
function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; cdecl;

// Module functions
function GetModuleHandleExA(dwFlags: DWORD; lpModuleName: PChar; var phModule: HMODULE): BOOL; cdecl;
function GetModuleFileNameA(hModule: HMODULE; lpFilename: PChar; nSize: DWORD): DWORD; cdecl;

// Registry functions (macOS compatibility layer)
function RegOpenKeyExA(hKey: HKEY; lpSubKey: PChar; ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY): LONG; cdecl;
function RegCloseKey(hKey: HKEY): LONG; cdecl;
function RegQueryValueExA(hKey: HKEY; lpValueName: PChar; lpReserved: LPDWORD; lpType: LPDWORD; lpData: LPBYTE; lpcbData: LPDWORD): LONG; cdecl;
function RegSetValueExA(hKey: HKEY; lpValueName: PChar; Reserved: DWORD; dwType: DWORD; lpData: LPBYTE; cbData: DWORD): LONG; cdecl;

// Constants
const
  PROCESS_ALL_ACCESS = $FFFFFFFF;
  PROCESS_VM_READ = $0010;
  PROCESS_VM_WRITE = $0020;
  PROCESS_VM_OPERATION = $0008;
  PROCESS_QUERY_INFORMATION = $0400;
  
  MEM_COMMIT = $00001000;
  MEM_RESERVE = $00002000;
  MEM_RELEASE = $8000;
  
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_READWRITE = $04;
  PAGE_EXECUTE_READ = $20;
  
  HKEY_LOCAL_MACHINE = $80000002;
  HKEY_CURRENT_USER = $80000001;
  
  KEY_ALL_ACCESS = $F003F;
  KEY_READ = $20019;
  KEY_WRITE = $20006;
  
  REG_SZ = 1;
  REG_DWORD = 4;
  
  DEBUG_PROCESS = $00000001;
  DEBUG_ONLY_THIS_PROCESS = $00000002;
  
  DBG_CONTINUE = $00010002;
  DBG_EXCEPTION_NOT_HANDLED = $80010001;

type
  // Windows-compatible types
  THandle = TLibHandle;
  DWORD = cuint32;
  BOOL = LongBool;
  LPVOID = Pointer;
  SIZE_T = csize_t;
  HMODULE = THandle;
  HKEY = THandle;
  REGSAM = cuint32;
  LONG = clong;
  LPDWORD = ^DWORD;
  LPBYTE = PByte;
  
  // ARM64 context structure (simplified)
  TARM64CONTEXT = record
    ContextFlags: DWORD;
    X0: array[0..28] of DWORD64;
    SP: DWORD64;
    PC: DWORD64;
    CPSR: DWORD;
  end;

implementation

var
  ProcessList: TStringList;
  RegistryData: TStringList;

function OpenProcess(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; cdecl;
var
  task: task_t;
  kernResult: kern_return_t;
begin
  // Use task_for_pid on macOS to get process handle
  kernResult := task_for_pid(mach_task_self(), dwProcessId, task);
  if kernResult = KERN_SUCCESS then
    Result := THandle(task)
  else
    Result := 0;
end;

function ReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: SIZE_T; var lpNumberOfBytesRead: SIZE_T): BOOL; cdecl;
var
  vmSize: vm_size_t;
  vmAddress: vm_address_t;
  kernResult: kern_return_t;
begin
  vmAddress := vm_address_t(lpBaseAddress);
  kernResult := vm_read_overwrite(task_t(hProcess), vmAddress, nSize, vm_address_t(lpBuffer), @vmSize);
  
  if kernResult = KERN_SUCCESS then
  begin
    lpNumberOfBytesRead := SIZE_T(vmSize);
    Result := True;
  end
  else
  begin
    lpNumberOfBytesRead := 0;
    Result := False;
  end;
end;

function WriteProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: SIZE_T; var lpNumberOfBytesWritten: SIZE_T): BOOL; cdecl;
var
  vmAddress: vm_address_t;
  kernResult: kern_return_t;
begin
  vmAddress := vm_address_t(lpBaseAddress);
  kernResult := vm_write(task_t(hProcess), vmAddress, vm_address_t(lpBuffer), mach_msg_type_number_t(nSize));
  
  if kernResult = KERN_SUCCESS then
  begin
    lpNumberOfBytesWritten := nSize;
    Result := True;
  end
  else
  begin
    lpNumberOfBytesWritten := 0;
    Result := False;
  end;
end;

function CloseHandle(hObject: THandle): BOOL; cdecl;
begin
  if hObject <> 0 then
  begin
    mach_port_deallocate(mach_task_self(), mach_port_t(hObject));
    Result := True;
  end
  else
    Result := False;
end;

function GetThreadContextArm64(hThread: THandle; var lpContext: pointer): BOOL; cdecl;
var
  state: arm_thread_state64_t;
  stateCount: mach_msg_type_number_t;
  kernResult: kern_return_t;
begin
  stateCount := ARM_THREAD_STATE64_COUNT;
  kernResult := thread_get_state(thread_t(hThread), ARM_THREAD_STATE64, 
    thread_state_t(@state), @stateCount);
  
  if kernResult = KERN_SUCCESS then
  begin
    // Convert to Windows-compatible format
    TARM64CONTEXT(lpContext^).ContextFlags := CONTEXT_ARM64;
    Move(state.__x, TARM64CONTEXT(lpContext^).X0, SizeOf(state.__x));
    TARM64CONTEXT(lpContext^).SP := state.__sp;
    TARM64CONTEXT(lpContext^).PC := state.__pc;
    TARM64CONTEXT(lpContext^).CPSR := state.__cpsr;
    Result := True;
  end
  else
    Result := False;
end;

function SetThreadContextArm64(hThread: THandle; var lpContext: pointer): BOOL; cdecl;
var
  state: arm_thread_state64_t;
  stateCount: mach_msg_type_number_t;
  kernResult: kern_return_t;
begin
  // Convert from Windows-compatible format
  Move(TARM64CONTEXT(lpContext^).X0, state.__x, SizeOf(state.__x));
  state.__sp := TARM64CONTEXT(lpContext^).SP;
  state.__pc := TARM64CONTEXT(lpContext^).PC;
  state.__cpsr := TARM64CONTEXT(lpContext^).CPSR;
  
  stateCount := ARM_THREAD_STATE64_COUNT;
  kernResult := thread_set_state(thread_t(hThread), ARM_THREAD_STATE64,
    thread_state_t(@state), stateCount);
  
  Result := (kernResult = KERN_SUCCESS);
end;

function getCPUCount: integer;
var
  numCPUs: integer;
  len: size_t;
begin
  len := SizeOf(numCPUs);
  if sysctlbyname('hw.ncpu', @numCPUs, @len, nil, 0) = 0 then
    Result := numCPUs
  else
    Result := 1; // Default fallback
end;

procedure macPortFixRegPath;
begin
  // Initialize registry compatibility layer
  if not Assigned(RegistryData) then
    RegistryData := TStringList.Create;
    
  // Set up default registry paths for macOS
  RegistryData.Values['Software\Cheat Engine'] := GetAppConfigDir(False);
end;

function VirtualAllocEx(hProcess: THandle; lpAddress: Pointer; dwSize: SIZE_T; flAllocationType: DWORD; flProtect: DWORD): Pointer; cdecl;
var
  vmAddress: vm_address_t;
  vmSize: vm_size_t;
  vmProtection: vm_prot_t;
  kernResult: kern_return_t;
begin
  vmAddress := vm_address_t(lpAddress);
  vmSize := dwSize;
  
  // Convert Windows protection flags to macOS
  vmProtection := VM_PROT_READ;
  if (flProtect and PAGE_EXECUTE_READWRITE) <> 0 then
    vmProtection := VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE
  else if (flProtect and PAGE_READWRITE) <> 0 then
    vmProtection := VM_PROT_READ or VM_PROT_WRITE
  else if (flProtect and PAGE_EXECUTE_READ) <> 0 then
    vmProtection := VM_PROT_READ or VM_PROT_EXECUTE;
  
  kernResult := vm_allocate(task_t(hProcess), @vmAddress, vmSize, 
    (flAllocationType and MEM_RESERVE) <> 0 ? VM_FLAGS_ANYWHERE : VM_FLAGS_FIXED);
  
  if kernResult = KERN_SUCCESS then
  begin
    // Set protection
    vm_protect(task_t(hProcess), vmAddress, vmSize, False, vmProtection);
    Result := Pointer(vmAddress);
  end
  else
    Result := nil;
end;

function VirtualFreeEx(hProcess: THandle; lpAddress: Pointer; dwSize: SIZE_T; dwFreeType: DWORD): BOOL; cdecl;
var
  vmAddress: vm_address_t;
  vmSize: vm_size_t;
  kernResult: kern_return_t;
begin
  vmAddress := vm_address_t(lpAddress);
  
  if dwFreeType = MEM_RELEASE then
  begin
    vmSize := 0;
    kernResult := vm_deallocate(task_t(hProcess), vmAddress, vmSize);
  end
  else
  begin
    vmSize := dwSize;
    kernResult := vm_deallocate(task_t(hProcess), vmAddress, vmSize);
  end;
  
  Result := (kernResult = KERN_SUCCESS);
end;

function DebugActiveProcess(dwProcessId: DWORD): BOOL; cdecl;
begin
  // macOS debugging requires different approach
  // This is a simplified implementation
  Result := ptrace(PT_ATTACH, pid_t(dwProcessId), nil, nil) = 0;
end;

function DebugActiveProcessStop(dwProcessId: DWORD): BOOL; cdecl;
begin
  Result := ptrace(PT_DETACH, pid_t(dwProcessId), nil, nil) = 0;
end;

function WaitForDebugEvent(var lpDebugEvent: pointer; dwMilliseconds: DWORD): BOOL; cdecl;
begin
  // Simplified implementation
  // In a real implementation, this would wait for debug events
  Result := False;
end;

function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; cdecl;
begin
  // Simplified implementation
  Result := True;
end;

function GetModuleHandleExA(dwFlags: DWORD; lpModuleName: PChar; var phModule: HMODULE): BOOL; cdecl;
var
  image: NSImage;
  bundle: NSBundle;
begin
  // macOS module handling
  bundle := NSBundle.mainBundle;
  if Assigned(bundle) then
  begin
    phModule := HMODULE(bundle);
    Result := True;
  end
  else
  begin
    phModule := 0;
    Result := False;
  end;
end;

function GetModuleFileNameA(hModule: HMODULE; lpFilename: PChar; nSize: DWORD): DWORD; cdecl;
var
  bundle: NSBundle;
  path: NSString;
begin
  bundle := NSBundle.mainBundle;
  if Assigned(bundle) then
  begin
    path := bundle.bundlePath;
    StrLCopy(lpFilename, PChar(path.UTF8String), nSize - 1);
    Result := Length(path.UTF8String);
  end
  else
  begin
    StrLCopy(lpFilename, '', nSize - 1);
    Result := 0;
  end;
end;

// Registry compatibility functions using file-based storage
function RegOpenKeyExA(hKey: HKEY; lpSubKey: PChar; ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY): LONG; cdecl;
var
  keyPath: string;
begin
  if not Assigned(RegistryData) then
    RegistryData := TStringList.Create;
    
  keyPath := IntToStr(hKey) + '\' + lpSubKey;
  phkResult := HKEY(keyPath);
  Result := ERROR_SUCCESS;
end;

function RegCloseKey(hKey: HKEY): LONG; cdecl;
begin
  Result := ERROR_SUCCESS;
end;

function RegQueryValueExA(hKey: HKEY; lpValueName: PChar; lpReserved: LPDWORD; lpType: LPDWORD; lpData: LPBYTE; lpcbData: LPDWORD): LONG; cdecl;
var
  keyPath: string;
  value: string;
begin
  keyPath := string(hKey);
  value := RegistryData.Values[keyPath + '\' + lpValueName];
  
  if Assigned(lpcbData) then
  begin
    if Length(value) < lpcbData^ then
      lpcbData^ := Length(value)
    else
      lpcbData^ := Length(value) + 1;
  end;
  
  if Assigned(lpData) and (lpcbData^ > 0) then
    StrLCopy(PChar(lpData), PChar(value), lpcbData^ - 1);
    
  if Assigned(lpType) then
    lpType^ := REG_SZ;
    
  Result := ERROR_SUCCESS;
end;

function RegSetValueExA(hKey: HKEY; lpValueName: PChar; Reserved: DWORD; dwType: DWORD; lpData: LPBYTE; cbData: DWORD): LONG; cdecl;
var
  keyPath: string;
begin
  keyPath := string(hKey) + '\' + lpValueName;
  RegistryData.Values[keyPath] := PChar(lpData);
  Result := ERROR_SUCCESS;
end;

initialization
  ProcessList := TStringList.Create;
  RegistryData := TStringList.Create;
  macPortFixRegPath;

finalization
  ProcessList.Free;
  RegistryData.Free;

end.
