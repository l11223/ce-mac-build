unit stealth_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Math;

// Stealth utility functions
function GetRandomProcessName: string;
function IsDebuggerPresent: Boolean;
function IsAntiDetectionEnvironment: Boolean;
function IsAnalysisEnvironment: Boolean;
procedure LoadFakeSymbols;
function ObfuscateString(const Input: string): string;
function DeobfuscateString(const Input: string): string;
procedure AddRandomDelay(MinMs, MaxMs: Integer);
function GenerateRandomString(Length: Integer): string;

// Process name list for obfuscation
const
  ProcessNames: array[0..19] of string = (
    'ActivityMonitor',
    'SystemPreferences',
    'Finder',
    'TextEdit',
    'Calculator',
    'Preview',
    'Safari',
    'Mail',
    'Calendar',
    'Contacts',
    'Notes',
    'Reminders',
    'PhotoBooth',
    'Dictionary',
    'Chess',
    'ImageCapture',
    'DVDPlayer',
    'Automator',
    'ScriptEditor',
    'Terminal'
  );

implementation

function GetRandomProcessName: string;
begin
  Result := ProcessNames[Random(High(ProcessNames) + 1)];
end;

function IsDebuggerPresent: Boolean;
var
  info: mach_msg_type_number_t;
  count: integer;
begin
  // Check if we're being debugged on macOS
  Result := False;
  
  // Method 1: Check for debugger using mach calls
  info := 0;
  if task_info(mach_task_self(), TASK_DEBUGGER_PRESENT, @info, @count) = KERN_SUCCESS then
  begin
    Result := info <> 0;
  end;
  
  // Method 2: Check for common debugging tools
  if not Result then
  begin
    Result := FileExists('/usr/bin/lldb') or 
              FileExists('/usr/bin/gdb') or
              FileExists('/Applications/Xcode.app');
  end;
  
  // Method 3: Check parent process
  if not Result then
  begin
    // This would require more complex implementation
    // For now, just use a simple heuristic
    Result := (GetEnvironmentVariable('LLDB_DEBUGGER') <> '');
  end;
end;

function IsAntiDetectionEnvironment: Boolean;
var
  vmSize: vm_size_t;
  vmAddress: vm_address_t;
  kernResult: kern_return_t;
begin
  Result := False;
  
  // Check for virtualization indicators
  vmAddress := 0;
  vmSize := 0;
  
  // Look for hypervisor presence
  kernResult := host_statistics(mach_host_self(), HOST_VM_INFO, 
    host_info_t(@vmAddress), @vmSize);
  
  // Check for common analysis tools
  Result := FileExists('/usr/bin/strings') or
            FileExists('/usr/bin/hexdump') or
            FileExists('/usr/bin/otool') or
            FileExists('/usr/bin/lldb') or
            FileExists('/usr/bin/nm');
            
  // Check for sandbox indicators
  if not Result then
  begin
    Result := GetEnvironmentVariable('SANDBOX') <> '';
  end;
end;

function IsAnalysisEnvironment: Boolean;
begin
  // Check if we're running in an analysis environment
  Result := IsDebuggerPresent or IsAntiDetectionEnvironment;
  
  // Additional checks
  if not Result then
  begin
    // Check for common analysis directories
    Result := DirectoryExists('/tmp/analysis') or
              DirectoryExists('/var/tmp/reverse') or
              GetEnvironmentVariable('ANALYSIS_MODE') <> '';
  end;
end;

procedure LoadFakeSymbols;
begin
  // Load fake symbols to confuse analysis
  // This would populate the symbol handler with fake entries
  // Implementation depends on the symbol handler structure
end;

function ObfuscateString(const Input: string): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 1 to Length(Input) do
  begin
    c := Input[i];
    // Simple XOR obfuscation with a varying key
    c := Char(Byte(c) xor (i mod 255));
    Result := Result + c;
  end;
end;

function DeobfuscateString(const Input: string): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 1 to Length(Input) do
  begin
    c := Input[i];
    // Reverse the XOR operation
    c := Char(Byte(c) xor (i mod 255));
    Result := Result + c;
  end;
end;

procedure AddRandomDelay(MinMs, MaxMs: Integer);
var
  DelayMs: Integer;
begin
  DelayMs := RandomRange(MinMs, MaxMs);
  Sleep(DelayMs);
end;

function GenerateRandomString(Length: Integer): string;
const
  Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
var
  i: Integer;
begin
  Result := '';
  Randomize;
  for i := 1 to Length do
  begin
    Result := Result + Chars[Random(Length(Chars)) + 1];
  end;
end;

// Additional stealth functions

function CheckSandboxStatus: Boolean;
begin
  // Check if running in sandbox
  Result := (GetEnvironmentVariable('APP_SANDBOX_CONTAINER_ID') <> '');
end;

function GetProcessParent: DWORD;
var
  pid: pid_t;
  parentPid: pid_t;
  size: size_t;
begin
  pid := getpid();
  size := SizeOf(parentPid);
  
  if sysctlbyname('kern.proc.pid', @pid, @size, nil, 0) = 0 then
  begin
    // Get parent PID (simplified)
    Result := DWORD(getppid());
  end
  else
    Result := 0;
end;

function IsRunningInVM: Boolean;
begin
  // Check for virtualization indicators
  Result := FileExists('/.dockerenv') or
            GetEnvironmentVariable('VIRTUAL_MACHINE') <> '' or
            CheckSandboxStatus;
end;

procedure AntiDebugTrick;
var
  startTime: TDateTime;
begin
  // Time-based anti-debugging
  startTime := Now;
  
  // Do some operation
  Sleep(100);
  
  // Check if time was manipulated (debugger slowdown)
  if MilliSecondsBetween(Now, startTime) > 200 then
  begin
    // Possible debugger detected
    Halt;
  end;
end;

function HideFromTaskManager: Boolean;
begin
  // Try to hide from process monitoring tools
  // On macOS, this is more complex and would require system calls
  Result := False; // Placeholder
end;

procedure InitializeStealthMode;
begin
  Randomize;
  
  // Set up anti-detection measures
  if IsAntiDetectionEnvironment then
  begin
    // Enable additional stealth features
    AddRandomDelay(50, 150);
  end;
  
  // Check for debugger periodically
  if IsDebuggerPresent then
  begin
    // Take evasive action
    AntiDebugTrick;
  end;
end;

finalization
  // Cleanup if needed

end.
