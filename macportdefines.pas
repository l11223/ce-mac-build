unit macportdefines;

{$mode objfpc}{$H+}

interface

// macOS-specific defines and constants for Cheat Engine compatibility

// Process access rights
const
  PROCESS_TERMINATE = $0001;
  PROCESS_CREATE_THREAD = $0002;
  PROCESS_SET_SESSIONID = $0004;
  PROCESS_VM_OPERATION = $0008;
  PROCESS_VM_READ = $0010;
  PROCESS_VM_WRITE = $0020;
  PROCESS_DUP_HANDLE = $0040;
  PROCESS_CREATE_PROCESS = $0080;
  PROCESS_SET_QUOTA = $0100;
  PROCESS_SET_INFORMATION = $0200;
  PROCESS_QUERY_INFORMATION = $0400;
  PROCESS_SUSPEND_RESUME = $0800;
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
  PROCESS_SET_LIMITED_INFORMATION = $2000;
  
  PROCESS_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $FFF;

// Memory allocation types
const
  MEM_COMMIT = $00001000;
  MEM_RESERVE = $00002000;
  MEM_DECOMMIT = $00004000;
  MEM_RELEASE = $00008000;
  MEM_RESET = $00080000;
  MEM_TOP_DOWN = $00100000;
  MEM_WRITE_WATCH = $00200000;
  MEM_PHYSICAL = $00400000;
  MEM_ROTATE = $00800000;
  MEM_DIFFERENT_IMAGE_BASE_OK = $00800000;
  MEM_RESET_UNDO = $01000000;
  MEM_LARGE_PAGES = $20000000;
  MEM_4MB_PAGES = $80000000;

// Memory protection
const
  PAGE_EXECUTE = $10;
  PAGE_EXECUTE_READ = $20;
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_EXECUTE_WRITECOPY = $80;
  PAGE_NOACCESS = $01;
  PAGE_READONLY = $02;
  PAGE_READWRITE = $04;
  PAGE_WRITECOPY = $08;
  PAGE_TARGETS_INVALID = $40000000;
  PAGE_TARGETS_NO_UPDATE = $40000000;
  PAGE_GUARD = $100;
  PAGE_NOCACHE = $200;
  PAGE_WRITECOMBINE = $400;

// Registry keys
const
  HKEY_CLASSES_ROOT = $80000000;
  HKEY_CURRENT_USER = $80000001;
  HKEY_LOCAL_MACHINE = $80000002;
  HKEY_USERS = $80000003;
  HKEY_PERFORMANCE_DATA = $80000004;
  HKEY_CURRENT_CONFIG = $80000005;
  HKEY_DYN_DATA = $80000006;

// Registry value types
const
  REG_NONE = 0;
  REG_SZ = 1;
  REG_EXPAND_SZ = 2;
  REG_BINARY = 3;
  REG_DWORD = 4;
  REG_DWORD_LITTLE_ENDIAN = 4;
  REG_DWORD_BIG_ENDIAN = 5;
  REG_LINK = 6;
  REG_MULTI_SZ = 7;
  REG_RESOURCE_LIST = 8;
  REG_FULL_RESOURCE_DESCRIPTOR = 9;
  REG_RESOURCE_REQUIREMENTS_LIST = 10;
  REG_QWORD = 11;
  REG_QWORD_LITTLE_ENDIAN = 11;

// Registry access rights
const
  KEY_QUERY_VALUE = $0001;
  KEY_SET_VALUE = $0002;
  KEY_CREATE_SUB_KEY = $0004;
  KEY_ENUMERATE_SUB_KEYS = $0008;
  KEY_NOTIFY = $0010;
  KEY_CREATE_LINK = $0020;
  KEY_WOW64_32KEY = $0200;
  KEY_WOW64_64KEY = $0100;
  KEY_WOW64_RES = $0300;
  
  KEY_READ = STANDARD_RIGHTS_READ or KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY;
  KEY_WRITE = STANDARD_RIGHTS_WRITE or KEY_SET_VALUE or KEY_CREATE_SUB_KEY;
  KEY_EXECUTE = KEY_READ;
  KEY_ALL_ACCESS = STANDARD_RIGHTS_ALL or KEY_QUERY_VALUE or KEY_SET_VALUE or KEY_CREATE_SUB_KEY or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY or KEY_CREATE_LINK;

// Debugging constants
const
  DEBUG_PROCESS = $00000001;
  DEBUG_ONLY_THIS_PROCESS = $00000002;
  CREATE_SUSPENDED = $00000004;
  DETACHED_PROCESS = $00000008;
  CREATE_NEW_CONSOLE = $00000010;
  NORMAL_PRIORITY_CLASS = $00000020;
  IDLE_PRIORITY_CLASS = $00000040;
  HIGH_PRIORITY_CLASS = $00000080;
  REALTIME_PRIORITY_CLASS = $00000100;
  CREATE_NEW_PROCESS_GROUP = $00000200;
  CREATE_UNICODE_ENVIRONMENT = $00000400;
  CREATE_SEPARATE_WOW_VDM = $00000800;
  CREATE_SHARED_WOW_VDM = $00001000;
  CREATE_FORCEDOS = $00002000;
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
  INHERIT_PARENT_AFFINITY = $00010000;
  
  // Debug continue status
  DBG_CONTINUE = $00010002;
  DBG_TERMINATE_THREAD = $40010003;
  DBG_TERMINATE_PROCESS = $40010004;
  DBG_CONTROL_C = $40010005;
  DBG_PRINTEXCEPTION_C = $40010006;
  DBG_RIPEXCEPTION = $40010007;
  DBG_CONTROL_BREAK = $40010008;
  DBG_COMMAND_EXCEPTION = $40010009;
  DBG_EXCEPTION_NOT_HANDLED = $80010001;

// ARM64 context flags
const
  CONTEXT_ARM64 = $00400000;
  CONTEXT_CONTROL = CONTEXT_ARM64 or $00000001;
  CONTEXT_INTEGER = CONTEXT_ARM64 or $00000002;
  CONTEXT_FLOATING_POINT = CONTEXT_ARM64 or $00000004;
  CONTEXT_DEBUG_REGISTERS = CONTEXT_ARM64 or $00000008;
  
  CONTEXT_FULL = CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_FLOATING_POINT;

// Standard rights
const
  DELETE = $00010000;
  READ_CONTROL = $00020000;
  WRITE_DAC = $00040000;
  WRITE_OWNER = $00080000;
  SYNCHRONIZE = $00100000;
  
  STANDARD_RIGHTS_REQUIRED = $000F0000;
  STANDARD_RIGHTS_READ = READ_CONTROL;
  STANDARD_RIGHTS_WRITE = READ_CONTROL;
  STANDARD_RIGHTS_EXECUTE = READ_CONTROL;
  STANDARD_RIGHTS_ALL = $001F0000;
  SPECIFIC_RIGHTS_ALL = $0000FFFF;

// Error codes
const
  ERROR_SUCCESS = 0;
  ERROR_INVALID_FUNCTION = 1;
  ERROR_FILE_NOT_FOUND = 2;
  ERROR_PATH_NOT_FOUND = 3;
  ERROR_TOO_MANY_OPEN_FILES = 4;
  ERROR_ACCESS_DENIED = 5;
  ERROR_INVALID_HANDLE = 6;
  ERROR_NOT_ENOUGH_MEMORY = 8;
  ERROR_INVALID_PARAMETER = 87;
  ERROR_INSUFFICIENT_BUFFER = 122;
  ERROR_MORE_DATA = 234;
  ERROR_NO_MORE_ITEMS = 259;
  ERROR_KEY_NOT_FOUND = 1638;

// macOS-specific constants
const
  MACH_PORT_NULL = 0;
  KERN_SUCCESS = 0;
  VM_PROT_READ = 1;
  VM_PROT_WRITE = 2;
  VM_PROT_EXECUTE = 4;
  
  VM_FLAGS_ANYWHERE = $0001;
  VM_FLAGS_FIXED = $0002;
  VM_FLAGS_NO_CACHE = $0004;
  VM_FLAGS_RESILIENT_CODESIGN = $0008;
  VM_FLAGS_RESILIENT_MEDIA = $0010;
  VM_FLAGS_OVERWRITE = $4000;
  VM_FLAGS_SUPERPAGE_MASK = $F000;
  VM_FLAGS_SUPERPAGE_SHIFT = 12;
  VM_FLAGS_SUPERPAGE_SIZE_2KB = 0;
  VM_FLAGS_SUPERPAGE_NONE = VM_FLAGS_SUPERPAGE_SIZE_2KB;
  VM_FLAGS_SUPERPAGE_SIZE_ANY = VM_FLAGS_SUPERPAGE_SIZE_2KB;
  VM_FLAGS_RETURN_DATA_ADDR = $0100;
  VM_FLAGS_PURGABLE = $2000;
  VM_FLAGS_4GB_CHUNK = $8000;
  VM_FLAGS_RANDOM_ADDR = $10000;
  VM_FLAGS_NO_ALLOCATION = $100000;
  VM_FLAGS_TAG_REMAP = $200000;
  VM_FLAGS_OVERWRITE = $4000;
  VM_FLAGS_VDA_OBJECT = $800000;

// ARM64 thread state
const
  ARM_THREAD_STATE64 = 6;
  ARM_THREAD_STATE64_COUNT = 34;

// ptrace constants
const
  PT_TRACE_ME = 0;
  PT_READ_I = 1;
  PT_READ_D = 2;
  PT_READ_U = 3;
  PT_WRITE_I = 4;
  PT_WRITE_D = 5;
  PT_WRITE_U = 6;
  PT_CONTINUE = 7;
  PT_KILL = 8;
  PT_STEP = 9;
  PT_ATTACH = 10;
  PT_DETACH = 11;
  PT_SIGEXC = 12;
  PT_THUPDATE = 13;
  PT_ATTACHEXC = 14;

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
  LPCSTR = PChar;
  LPCWSTR = PWideChar;
  LPSTR = PChar;
  LPWSTR = PWideChar;
  
  // Pointer types
  PDWORD = ^DWORD;
  PLONG = ^LONG;
  PHANDLE = ^THandle;
  PHKEY = ^HKEY;
  
  // ARM64 context structure
  TARM64CONTEXT = record
    ContextFlags: DWORD;
    Cpsr: DWORD;
    X0: array[0..28] of DWORD64;
    Fp: DWORD64;
    Lr: DWORD64;
    Sp: DWORD64;
    Pc: DWORD64;
    case Integer of
      0: (
        V: array[0..31] of array[0..1] of DWORD64;
      );
      1: (
        D: array[0..31] of UInt64;
      );
  end;
  PARMSContext = ^TARM64CONTEXT;
  
  // Memory basic information (simplified)
  TMemoryBasicInformation = record
    BaseAddress: Pointer;
    AllocationBase: Pointer;
    AllocationProtect: DWORD;
    RegionSize: SIZE_T;
    State: DWORD;
    Protect: DWORD;
    Type_: DWORD;
  end;
  PMemoryBasicInformation = ^TMemoryBasicInformation;
  
  // Process information
  TProcessInformation = record
    hProcess: THandle;
    hThread: THandle;
    dwProcessId: DWORD;
    dwThreadId: DWORD;
  end;
  PProcessInformation = ^TProcessInformation;
  
  // Startup information
  TStartupInfo = record
    cb: DWORD;
    lpReserved: LPSTR;
    lpDesktop: LPSTR;
    lpTitle: LPSTR;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: Word;
    cbReserved2: Word;
    lpReserved2: LPBYTE;
    hStdInput: THandle;
    hStdOutput: THandle;
    hStdError: THandle;
  end;
  PStartupInfo = ^TStartupInfo;
  
  // Security attributes
  TSecurityAttributes = record
    nLength: DWORD;
    lpSecurityDescriptor: LPVOID;
    bInheritHandle: BOOL;
  end;
  PSecurityAttributes = ^TSecurityAttributes;

// Function prototypes
type
  TOpenProcess = function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; cdecl;
  TReadProcessMemory = function(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: SIZE_T; var lpNumberOfBytesRead: SIZE_T): BOOL; cdecl;
  TWriteProcessMemory = function(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: SIZE_T; var lpNumberOfBytesWritten: SIZE_T): BOOL; cdecl;
  TCloseHandle = function(hObject: THandle): BOOL; cdecl;
  TVirtualAllocEx = function(hProcess: THandle; lpAddress: Pointer; dwSize: SIZE_T; flAllocationType: DWORD; flProtect: DWORD): Pointer; cdecl;
  TVirtualFreeEx = function(hProcess: THandle; lpAddress: Pointer; dwSize: SIZE_T; dwFreeType: DWORD): BOOL; cdecl;
  TGetThreadContextArm64 = function(hThread: THandle; var lpContext: pointer): BOOL; cdecl;
  TSetThreadContextArm64 = function(hThread: THandle; var lpContext: pointer): BOOL; cdecl;

var
  // Function pointers for dynamic linking
  OpenProcess: TOpenProcess = nil;
  ReadProcessMemory: TReadProcessMemory = nil;
  WriteProcessMemory: TWriteProcessMemory = nil;
  CloseHandle: TCloseHandle = nil;
  VirtualAllocEx: TVirtualAllocEx = nil;
  VirtualFreeEx: TVirtualFreeEx = nil;
  GetThreadContextArm64: TGetThreadContextArm64 = nil;
  SetThreadContextArm64: TSetThreadContextArm64 = nil;

implementation

procedure InitializeFunctionPointers;
begin
  // These will be assigned by macport unit initialization
  // This is just a placeholder for type checking
end;

initialization
  InitializeFunctionPointers;

end.
