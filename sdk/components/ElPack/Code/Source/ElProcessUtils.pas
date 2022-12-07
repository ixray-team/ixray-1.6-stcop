{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
{$include elpack2.inc}
{$ifdef ELPACK_SINGLECOMP}
{$I ElPack.inc}
{$else}
{$ifdef LINUX}
{$I ../ElPack.inc}
{$else}
{$I ..\ElPack.inc}
{$endif}
{$endif}

unit ElProcessUtils;

interface

uses ElList, ElTools, Windows, SysUtils, Messages;

type TProcessInfo = class
       ModuleName : string;
       PID        : integer;
       ThreadCount: integer;
       PriorityCls: integer;

       function DiffersFrom(Source : TProcessInfo) : boolean;
     end;

procedure FillProcessList(L : TElList);

function ProcessExists(ModuleName : string) : boolean;
procedure CloseProcess(ModuleName : string; Forced : boolean);

implementation

var APILoaded : boolean;

{$ifndef BUILDER_USED}
const
  TH32CS_SNAPHEAPLIST = $00000001;
  TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD   = $00000004;
  TH32CS_SNAPMODULE   = $00000008;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;
  TH32CS_INHERIT      = $80000000;

{$else}

const

{$IFDEF VCL_4_USED}
{$EXTERNALSYM TH32CS_SNAPHEAPLIST}
{$ENDIF}
  TH32CS_SNAPHEAPLIST = $00000001;
{$IFDEF VCL_4_USED}
{$EXTERNALSYM TH32CS_SNAPPROCESS}
{$ENDIF}
  TH32CS_SNAPPROCESS  = $00000002;          
{$IFDEF VCL_4_USED}
{$EXTERNALSYM TH32CS_SNAPTHREAD}
{$ENDIF}
  TH32CS_SNAPTHREAD   = $00000004;
{$IFDEF VCL_4_USED}
{$EXTERNALSYM TH32CS_SNAPMODULE}
{$ENDIF}
  TH32CS_SNAPMODULE   = $00000008;
{$IFDEF VCL_4_USED}
{$EXTERNALSYM TH32CS_SNAPALL}
{$ENDIF}
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or
    TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;
{$IFDEF VCL_4_USED}
{$EXTERNALSYM TH32CS_INHERIT}
{$ENDIF}
  TH32CS_INHERIT      = $80000000;

{$endif}

type

    PProcessIDArray = ^TProcessIDArray;
    TProcessIDArray = array[0..0] of Integer;

    PHMODULE = ^HMODULE;

    TProcessMemoryCounters = record
        cb:LongInt;                         // The size of the structure, in bytes
        PageFaultCount:LongInt;             // The number of page faults
        PeakWorkingSetSize:LongInt;         // The peak working set size
        WorkingSetSize:LongInt;             // The current working set size
        QuotaPeakPagedPoolUsage:LongInt;    // The peak paged pool usage
        QuotaPagedPoolUsage:LongInt;        // The current paged pool usage
        QuotaPeakNonPagedPoolUsage:LongInt; // The peak nonpaged pool usage
        QuotaNonPagedPoolUsage:LongInt;     // The current nonpaged pool usage
        PagefileUsage:LongInt;              // The current pagefile usage
        PeakPagefileUsage:LongInt;          // The peak pagefile usage
    end;

    TProcessEntry32 = packed record
        dwSize: DWORD;
        cntUsage: DWORD;
        th32ProcessId: DWORD;       // this process
        th32DefaultHeapId: DWORD;
        th32ModuleId: DWORD;        // associated exe
        cntThreads: DWORD;
        th32ParentProcessId: DWORD; // this process's parent process
        pcPriClassBase: Longint;	// Base priority of process's threads
        dwFlags: DWORD;
        szExeFile: array[0..MAX_PATH - 1] of Char;// Path
    end;


    EnumProcessesProc = function (lpidProcess:PProcessIDArray; // указатель на массив ID процессов
                                  cb:LongInt;                  // размер массива ID процессов (в байтах)
                                  var cbNeedee:DWORD         // размер заполненных данных (в байтах)
                                 ):BOOL; stdcall;

    EnumProcessModulesProc = function(hProcess : THANDLE;
                                      lphModule : PHMODULE;
                                      cb : DWORD;
                                      var cbNeedee: DWORD
                                     ): Bool; stdcall;

    GetModuleFileNameExProc = function (hProcess:THandle;  // handle to the process
                                        hMoe:HMODULE;      // handle to the module
                                        lpBaseName:PChar;  // buffer that receives the base name
                                        nSize: DWORD      // size of the buffer
                                       ):LongInt; stdcall;

    GetProcessMemoryInfoProc = function(hProcess:THandle;                      // handle to the process
                                        ppsmemCounters:TProcessMemoryCounters; // structure that receives information
                                        cb : DWORD                          // size of the structure
                                       ) : BOOL; stdcall;

    CreateToolhelp32SnapshotProc = function (dwFlags, th32ProcessIe: DWORD): THandle; stdcall;

    Process32FirstProc = function (hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
    Process32NextProc = function (hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;

const EnumProcesses        : EnumProcessesProc = nil;
      EnumProcessModules   : EnumProcessModulesProc = nil;
      GetModuleFileNameEx  : GetModuleFileNameExProc = nil;
      GetProcessMemoryInfo : GetProcessMemoryInfoProc = nil;

      CreateToolHelp32Snapshot : CreateToolhelp32SnapshotProc = nil;
      Process32First           : Process32FirstProc = nil;
      Process32Next            : Process32NextProc = nil;

function TProcessInfo.DiffersFrom(Source : TProcessInfo) : boolean;
begin
  result := (ModuleName <> Source.ModuleName) or
            (Self.PID <> Source.Pid) or
            (Self.ThreadCount <> Source.ThreadCount) or
            (Self.PriorityCls <> Source.PriorityCls);
end;

function ProcessExists(ModuleName : string) : boolean;
var
  L: TElList;
  I: Integer;
  S: String;
begin
  L := TElList.Create;
  try
    L.AutoClearObjects := true;
    FillProcessList(L);
    S := LowerCase(ModuleName);
    for i := 0 to L.Count - 1 do
    begin
      if LowerCase(ExtractFileName(TProcessInfo(L[i]).ModuleName)) = S then
      begin
        result := true;
        exit;
      end;
    end;
    result := false;
  finally
    L.Free;
  end;
end;

procedure CloseProcess(ModuleName : string; Forced : boolean);
var
  L: TElList;
  S: String;
  I, ID : Integer;
  hProc : THandle;

  function EnumWindowProc(Wnd : HWND; lParam : integer) : boolean; stdcall;
  var IID : DWORD;
  begin
    GetWindowThreadProcessId(Wnd, @IID);
    result := true;
    if Integer(IID) = LParam then
    begin
      PostMessage(Wnd, WM_CLOSE, 0, 0);
      result := false;
    end;
  end;


begin
  L := TElList.Create;
  try
    L.AutoClearObjects := true;
    FillProcessList(L);
    S := LowerCase(ModuleName);
    ID := 0;
    for i := 0 to L.Count - 1 do
    begin
      if LowerCase(ExtractFileName(TProcessInfo(L[i]).ModuleName)) = S then
      begin
        ID := TProcessInfo(L[i]).PID;
        if Forced then
        begin
          hProc := OpenProcess(PROCESS_TERMINATE, false, ID);
          TerminateProcess(hProc, 0);
        end
        else
          EnumWindows(@EnumWindowProc, ID);
      end;
    end;
  finally
    L.Free;
  end;
end;

procedure FillProcessList(L : TElList);
var pidArray : pProcessIDArray;
    mhArray  : array[0..1000] of HMODULE;
    c        : Cardinal;
    cb       : DWORD;
    i,j      : Integer;
    ph       : THandle;
    buf      : array[0..MAX_PATH] of char;
    Info     : TProcessInfo;
    hSnapshot: THandle;
    pe32     : TProcessEntry32;
begin
  L.Clear;
  if not ApiLoaded then exit;
  if IsWinNT and (not IsWin2000) then
  begin
    c := 100 * sizeof(Integer);
    GetMem(pidArray, c);
    while true do
    begin
      if not EnumProcesses(pidArray, c, cb) then exit;
      if cb <= c then break;
      FreeMem(pidArray);
      c := cb;
      GetMem(pidArray, c);
    end;
    j := (cb div sizeof(integer)) - 1;
    for i := 0 to j do
    begin
      Info := TProcessInfo.Create;
      Info.PID := pidArray[i];
      Info.PriorityCls := 0;
      ph := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,pidArray[i]);
      if ph <> 0 then
      begin
        if EnumProcessModules(ph, @mhArray, SizeOf(mhArray), CB) then
        begin
          GetModuleFileNameEx(ph, mhArray[0], Buf, MAX_PATH);
          Info.ModuleName := StrPas(@Buf[0]);
          Info.PriorityCls := GetPriorityClass(ph);
          Info.ThreadCount := 0;
        end;
        CloseHandle(ph);
      end;
      L.Add(Info);
    end;
    FreeMem(pidArray);
  end else
  begin
    hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    pe32.dwSize := sizeof(TProcessEntry32);
    if Process32First(hSnapshot, pe32) then
      repeat
        Info := TProcessInfo.Create;
        Info.PID := pe32.th32ProcessID;
        Info.ModuleName := pe32.szExeFile;
        Info.PriorityCls := pe32.pcPriClassBase;
        Info.ThreadCount := pe32.cntThreads; 
        L.Add(Info);
      until not Process32Next(hSnapshot, pe32);
    CloseHandle(hSnapshot);
  end;
end;

const PsApiDll = 'psapi.dll';
const PsApiDllHandle: THandle = 0;
      KernelHandle  : THandle = 0;

initialization

  if IsWinNT and (not IsWin2000) then
  begin
    PsApiDllHandle := LoadLibrary(PsAPIDll);
    if PsApiDllHandle <> 0 then
    begin
      EnumProcesses        := EnumProcessesProc(GetProcAddress(PsApiDllHandle, 'EnumProcesses'));
      EnumProcessModules   := EnumProcessModulesProc(GetProcAddress(PsApiDllHandle, 'EnumProcessModules'));
      GetModuleFileNameEx  := GetModuleFileNameExProc(GetProcAddress(PsApiDllHandle, 'GetModuleFileNameExA'));
      GetProcessMemoryInfo := GetProcessMemoryInfoProc(GetProcAddress(PsApiDllHandle, 'GetProcessMemoryInfo'));
      if (@EnumProcesses <> nil) and (@EnumProcessModules <> nil) and
         (@GetModuleFileNameEx <> nil) and (@GetProcessMemoryInfo <> nil) then
        ApiLoaded := true;
    end;
  end else
  begin
    KernelHandle := GetModuleHandle('kernel32.dll');
    if KernelHandle <> 0 then
    begin
      CreateToolHelp32Snapshot := CreateToolhelp32SnapshotProc(GetProcAddress(KernelHandle, 'CreateToolhelp32Snapshot'));
      Process32First           := Process32FirstProc(GetProcAddress(KernelHandle, 'Process32First'));
      Process32Next            := Process32NextProc(GetProcAddress(KernelHandle, 'Process32Next'));
      if (@Process32First <> nil) and (@Process32Next <> nil) and (@CreateToolhelp32Snapshot <> nil) then
         APILoaded := true;
    end;
  end;

finalization

  if PsApiDllHandle <> 0 then FreeLibrary(PsApiDllHandle);

end.



