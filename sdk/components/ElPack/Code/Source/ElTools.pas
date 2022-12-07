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

(*

Version history

04/19/2002

  Fixed GetSystemDir function

<...>

  Fixed GetTempDir function in windows

*)

unit ElTools;

interface

uses
{$ifndef KYLIX_USED}
Messages, Windows, ShellAPI,
ActiveX,
mmSystem,
{$ifdef VCL_6_USED}
Types,
{$endif}
{$else}
Types, Libc,
{$endif}
classes, SysUtils, ElStrUtils;

function Sign(a : integer) : integer;
function Max(a, b : integer) : integer;
function Min(a, b : integer) : integer;
function InRange(L, R, x : integer) : boolean;
function InRangeF(L, R, x : double) : boolean;

function SwapDouble(d : double) : double;
function swapInt32(i : integer) : integer;
function swapInt16(w : word) : Word;

function RangesIntersect(L1, R1, L2, R2 : integer) : boolean;
{$IFNDEF VER90}
{$ifdef MSWINDOWS}
function SubtractTimes(Time1, Time2 : TDateTime) : TDateTime;
{$endif}
{$ENDIF}

function GetTime(DateTime : TDateTime) : TDateTime;

// the following two functions write and read the string in the following format:
// <string length><text>
function WriteStringToStream(S : TStream; Str : string) : boolean;
function ReadStringFromStream(S : TStream; var Str : string) : boolean;
{$ifndef BROKEN_UNICODE}
function WriteWideStringToStream(S : TStream; Str : WideString) : boolean;
function ReadWideStringFromStream(S : TStream; var Str : WideString) : boolean;
{$endif}
function WriteFStringToStream(S : TStream; Str : TElFString) : boolean;
function ReadFStringFromStream(S : TStream; var Str : TElFString) : boolean;

// the following two functions write and read the string in the following format:
// <text>#13#10
procedure WriteTextToStream(S : TStream; Data : string);
function ReadTextFromStream(S : TStream; var Data : string) : boolean;

{$IFDEF VCL_4_USED}
function GetFileSize(const FileName: string): Int64;
{$ELSE}
function GetFileSize(const FileName: string): Integer;
{$ENDIF}
function FileDateTime(const FileName: string): TDateTime;
function FileNameValid(FileName : string) : boolean;
function CreateFile(FileName : String) : boolean;
procedure EnsureDirExists(RootName, DirName : string);
function EnsureValidFileName(PathName, FileName : string) : string;

function DirExists(DirName : string) : boolean;
{$ifndef CLX_USED}
function PurgeDir(DirName : string) : boolean;
{$endif}
function GetModulePath : string;

{$ifdef MSWINDOWS}
function GetComputerName : string;
{$endif}

function IsBIn(index : integer; storage : byte) : boolean;

function encode_line(const buf; size : integer) : string;

{$IFDEF VER90}
function CompareMem(Var1, Var2 : pointer; size : integer) : boolean; assembler;
{$ENDIF}

procedure ValFloat(Value : string; Result : Double; var Error : integer);
function StrFloat(Value : double) : String;


type
  TDirectMemoryStream = class(TMemoryStream)
  public
    procedure SetPointer(Ptr : Pointer; Size : Longint);
  end;
  TElMemoryStream = TDirectMemoryStream;

  TNamedFileStream = class(TFileStream)
  private
    FFileName : string;
  public
    constructor Create(const FileName : string; Mode : Word);
    property FileName : string read FFileName;
  end;

const
  MAXPATHLEN = 1024;

// Returns a path name after adding
// a backslash character to the end if it is not already there.
function IncludeTrailingBackslash2(const Path: String): String;

{$ifdef MSWINDOWS}
function GetSystemDir : string;
function GetWindowsDir: string;
function GetTempDir   : string;
function GetShortPath(Path : String) : string;
{$endif}

function AppendSlash(const PathName : String) : string;
{$ifndef D_2}
function GetTempFile(SDir : String) : String;
{$endif}

{$ifdef MSWINDOWS}
function GetSpecialFolder(const CSIDL : integer) : string;
{$endif}
function GetUserLocalAppDataFolder : string;
function GetUserAppDataFolder   : string;
{$ifdef MSWINDOWS}
function GetCommonAppDataFolder : string;
{$endif}

type
  TReducedDateTime = record
    Year,
      Month,
      DOW,
      Day,
      Hour,
      Min : word;
  end;

function DateTimeToReduced(T : TDateTime) : TReducedDateTime;
function ReducedToDateTime(T : TReducedDateTime) : TDateTime;
function CompareReducedDT(T1, T2 : TReducedDateTime) : boolean;
// < -1
// = 0
// > +1
function MakeReducedDT(Year, Month, Day, DOW, Hour, Min : word) : TReducedDateTime;

function RunProgram(StartName, Params, StartDir : string) : THandle;

procedure CenterRects(WS, WT, HS, HT : integer; var R : TRect);
function RectToString(Rect : TRect) : string;
function StringToRect(AString : string) : TRect;

function IncDate(ADate : TDateTime; Days, Months, Years : Integer) : TDateTime;
function IncTime(ATime : TDateTime; Hours, Minutes, Seconds, MSecs : Integer) : TDateTime;
function ExtractTime(ATime : TDateTime) : TDateTime;
function ExtractDate(ATime : TDateTime) : TDateTime;

function  DaysPerMonth(AYear, AMonth : Integer) : Integer;
function  ElDayOfWeek(ADay, AMonth, AYear : Integer) : Integer;
function  DateToJulianDays(ADay, AMonth, AYear : Integer) : integer;
procedure JulianDaysToDate(var ADay, AMonth, AYear :Integer; JulianDate : integer);
function  ElDateTimeToSeconds(ADay, AMonth, AYear, AHours, AMinute, ASecond : integer) : Cardinal;
procedure ElSecondsToDateTime(Seconds : Cardinal; var ADay, AMonth, AYear, AHours, AMinute, ASecond : integer);

function  GetSysStartDayOfWeek : integer;

{$ifdef MSWINDOWS}
procedure ElSystemTimeToTzSpecificLocalTime(lpTimeZoneInformation : PTimeZoneInformation;
  var lpUniversalTime : TSystemTime;
  var lpLocalTime : TSystemTime);

{procedure ElTZSpecificLocalTimeToSystemTime(lpTimeZoneInformation : PTimeZoneInformation;
  var lpUniversalTime : TSystemTime;
  var lpLocalTime : TSystemTime);
}

procedure UTCToZoneLocal(lpTimeZoneInformation : PTimeZoneInformation;
  lpUniversalTime : TSystemTime; var lpLocalTime : TSystemTime);
procedure ZoneLocalToUTC(lpTimeZoneInformation : PTimeZoneInformation;
  var lpUniversalTime : TSystemTime; lpLocalTime : TSystemTime);
{$endif}

{$ifdef MSWINDOWS}
function NowToUTC : TDateTime;
function ZoneIDtoBias (ZoneID : string) : integer;
{$endif}

{$ifndef D_2}
{$ifdef MSWINDOWS}
function GetFormattedTimeString(ADate : TDateTime; Format : string) : string;
{$endif}
{$endif}
function DayNumber(AYear, AMonth, ADay : integer) : integer;
function WeekNumber(AYear, AMonth, ADay : integer) : integer;

function TimeInMask(CronMask : string; T : TReducedDateTime) : boolean;


{$ifdef MSWINDOWS}

function GetShiftState : TShiftState;
function GetKeysState : integer;

{$ifndef D_2}
function SetPrivilege(sPrivilegeName : string; bEnabled : boolean) : boolean;
{$endif}

type TMsgPumpRoutineEvent = procedure of object;
     TElWndMethod = procedure(var Message: TMessage) of object;

var

  OnMessagePump : TMsgPumpRoutineEvent;

function XAllocateHWND(Obj : TObject; WndMethod : TElWndMethod) : HWND;
procedure XDeallocateHWND(Wnd : HWND);

function WindowExists(ClassName, Caption : string; ExactMatch : boolean) : HWND;
function TopWindowExists(ClassName, Caption : string; ExactMatch : boolean) : HWND;
{$endif}

//function ProcessExists(Name : string) : THandle;

procedure PlaySound(Name : Pchar; Flags1, Flags2 : DWORD);

{$ifdef KYlIX_USED}
var
{$else}
const
{$endif}
  IsLinux : boolean = false;
  IsWin95 : boolean = false;
  IsWinNT : boolean = false;
  IsWin2000 : boolean = false;
  IsWinNTUp : boolean = false;
  IsWin2000Up : boolean = false;
  IsWinXP : boolean = false;
  IsWinXPUp : boolean = false;
  IsWin95OSR2 : boolean = false;
  IsWin98 : boolean = false;
  IsWinME : boolean = false;
  IsWin98Up : boolean = false;


{$ifdef MSWINDOWS}
var LastWin : HWND; // the result of last WindowExists or TopWindowExists call
    LastProcessID: DWORD; // the result of last ProcessExists call
{$endif}

const
  ElementFormatList : array[1..38] of string
    = ('D', 'DD', 'w', '', 'ww', 'W', 'd', 'dd',
    'm', 'mm', 'mmm', 'mmmm', 'y', 'yy',
    'n', 'nn', 'Y', 'YY', 'x', 'xx', 'h',
    'hh', 'H', 'HH', 'M', 'MM', 'S', 'SS',
    '', '', '', 'tt', '|', '''', '~', '.', ':', 'TT');

implementation
                    
{$ifdef MSWINDOWS}
uses ShlObj;
{$endif}

procedure PlaySound(Name : Pchar; Flags1, Flags2 : DWORD);
begin
 {$ifdef MSWINDOWS}
 mmSystem.PlaySound(Name, Flags1, Flags2);
 {$else}
   
 {$endif}
end;

function swapInt16(w : word) : Word;
begin
  result := (w shl 8) or (w shr 8); 
end;

function swapInt32(i : integer) : integer;
begin
  result := (i shr 24) or (((i and $FF0000) shr 16) shl 8) or (((i and $FF00) shr 8) shl 16) or ((i and $FF) shl 24);
end;

{$ifndef VCL_4_USED}
type Int64Rec = packed record
       Lo, Hi: DWORD;
     end;
{$endif}

type PInt64Rec = ^Int64Rec;

function SwapDouble(d : double) : double;
var pd, pd1 : PInt64Rec;
begin
  pd := @d;
  pd1 := @result;
  pd1.Lo := SwapInt32(pd.hi);
  pd1.hi := SwapInt32(pd.lo);
end;

{$warnings off}
function GetSysStartDayOfWeek : integer;
var
  LS: String;
const LOCALE_IFIRSTDAYOFWEEK          = $0000100C;
begin
{$ifdef MSWINDOWS}
  LS := GetLocaleStr(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, '0');
{$else}
  LS := '';
{$endif}
  result := StrToIntDef(LS, 0) + 1;
  if Result = 7 then Result := 0;
end;
{$warnings on}

function GetTime(DateTime: TDateTime): TDateTime;
begin
  Result := DateTime - Trunc(DateTime);
end;

{$ifdef MSWINDOWS}
function GetCommonAppDataFolder : string;
const CSIDL_COMMON_APPDATA = $0023;
      CSIDL_COMMON_DOCUMENTS = $002E;
var Buf1, Buf2 : array [0..MAX_PATH] of char;
begin
  result := GetSpecialFolder(CSIDL_COMMON_APPDATA);
  if Length(result) = 0 then
    result := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS);
  if result = '' then
    result := ExtractFilePath(ParamStr(0));
  StrPCopy(Buf1, result);
  ExpandEnvironmentStrings(Buf1, Buf2, MAX_PATH);
  result := IncludeTrailingBackslash2(StrPas(Buf2));
end;
{$endif}

{$ifdef MSWINDOWS}
function GetUserAppDataFolder : string;
const CSIDL_LOCAL_APPDATA = $001C;
      CSIDL_PERSONAL      = $0005;
      CSIDL_APPDATA       = $001a;
var Buf1, Buf2 : array [0..MAX_PATH] of char;
begin
  result := GetSpecialFolder(CSIDL_APPDATA);
  if Length(result) = 0 then
    result := GetSpecialFolder(CSIDL_PERSONAL);
  if result = '' then
    result := ExtractFilePath(ParamStr(0));
  StrPCopy(Buf1, result);
  ExpandEnvironmentStrings(Buf1, Buf2, MAX_PATH);
  result := IncludeTrailingBackslash2(StrPas(Buf2));
end;
{$else}
function GetUserAppDataFolder  : string;
begin
  result := IncludeTrailingBackslash2(GetEnv('HOME'));
end;
{$endif}

{$ifdef MSWINDOWS}
function GetUserLocalAppDataFolder : string;
const CSIDL_LOCAL_APPDATA = $001C;
      CSIDL_PERSONAL      = $0005;
      CSIDL_APPDATA       = $001a;
begin
  result := GetSpecialFolder(CSIDL_LOCAL_APPDATA);
  if Length(result) = 0 then
    result := GetSpecialFolder(CSIDL_APPDATA);
  if Length(result) = 0 then
    result := GetSpecialFolder(CSIDL_PERSONAL);
  if result = '' then
    result := ExtractFilePath(ParamStr(0));
  result := IncludeTrailingBackslash2(Result);
end;
{$else}
function GetUserLocalAppDataFolder  : string;
begin
  result := GetEnv('HOME');
end;
{$endif}

{$ifdef MSWINDOWS}
function GetSpecialFolder (const CSIDL : integer) : string;
var
  ShellMalloc: IMalloc;
  ItemIDList : PItemIDList;
  Buffer: PChar;
begin
  Result := '';
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAXPATHLEN + 1);
    try
      if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, ItemIDList)) then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Result := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;
{$endif}

function IncludeTrailingBackslash2(const Path: String): String;


begin
  Result := Path;
{$ifndef D_2}
  if (Result <> '') and not (AnsiLastChar(Result)^ in ['\', '/']) then
{$else}
  if (Result <> '') and not (Result[Length(Result)] in ['\', '/']) then
{$endif}
{$ifdef MSWINDOWS}
    Result := Result + '\';
{$else}
    Result := Result + '/';
{$endif}
end;

{$ifndef D_2}
function GetTempFile(SDir : String) : String;
var
{$ifdef MSWINDOWS}
  uuid: TGUID;
  SUuidW: WideString;
  SUuid: String;
{$else}
  i    : integer;
{$endif}
begin
{$ifdef MSWINDOWS}
  if SDir = '' then
  begin
    SetLength(SDir, MAX_PATH + 1);
    GetTempPath(MAX_PATH, PChar(SDir));
    SDir := IncludeTrailingBackslash2(StrPas(PChar(SDir)));
  end;
  CoCreateGuid(uuid);
  SetLength(SUuidW, 39);
  StringFromGUID2(uuid, PWideChar(SUuidW), Length(SUuidW));
  SUuid := SUuidW;
  SUuid := Trim(SUuid);
  SUuid := Copy(SUuid, 2, Length(SUuid) - 2);
  Result := AnsiLowerCaseFileName(SDir + SUuid);
{
  SetLength(result, MAX_PATH + 1);
  GetTempFileName(PChar(SDir), 'atp', 0, PChar(result));
}
{$else}
  i := 0;
  while i < MaxInt do
  begin
    SDir := '/tmp/' + 'eltmpfl' + IntToStr(i);
    if not FileExists(Sdir) then
    begin
      result := SDir;
      exit;
    end;
  end;
{$endif}
end;
{$endif}

constructor TNamedFileStream.Create(const FileName : string; Mode : Word);
begin
  inherited;
  FFileName := FileName;
end;

function TimeInMask(CronMask : string; T : TReducedDateTime) : boolean;

  function InPart(S : string; Value : integer) : boolean;
  var
    s1, s2 : string;

  begin
    if s = '*' then
      result := true
    else
    begin
      s1 := s;
      while pos(',', s1) > 0 do
      begin
        s2 := copy(s1, 1, pos(',', s1) - 1);
        delete(s1, 1, pos(',', s1));
        result := s2 = IntToStr(value);
        if result then exit;
      end;
      result := s1 = IntToStr(value);
    end;
  end;

begin
  result := false;
  if not InPart(ExtractWord(CronMask, 1), T.Min) then exit;
  if not InPart(ExtractWord(CronMask, 2), T.Hour) then exit;
  if not InPart(ExtractWord(CronMask, 3), T.Day) then exit;
  if not InPart(ExtractWord(CronMask, 4), T.DOW) then exit;
  if not InPart(ExtractWord(CronMask, 5), T.Month) then exit;
  result := true;
end;

{$ifdef MSWINDOWS}
function GetSystemDir : string;
begin
  SetLength(Result, MAXPATHLEN + 1);
  GetSystemDirectory(PChar(Result), MAXPATHLEN);
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingBackslash2(Result);
end;

function GetShortPath(Path : String) : string;
begin
  SetLength(Result, MAXPATHLEN + 1);
  GetShortPathName(PChar(Path), PChar(Result), MAXPATHLEN);
end;

function GetTempDir : string;
var
{$ifdef MSWINDOWS}
  SDir : String;
{$else}
  i    : integer;
  SDir : String;
{$endif}
begin
{$ifdef MSWINDOWS}
  // behaviour changed because GetTempFile
  // in previous edition always returns
  // one 'acp0000'-coded name.
  //
  // Now it returns unique name.
  SetLength(SDir, MAX_PATH + 1);
  GetTempPath(MAX_PATH, PChar(SDir));
  result := IncludeTrailingBackslash2(StrPas(PChar(SDir)));
{$else}
  result := '/tmp/';
{$endif}
end;

function GetWindowsDir : string;
begin
  SetLength(Result, MAXPATHLEN + 1);
  GetWindowsDirectory(PChar(Result), MAXPATHLEN);
  Result := IncludeTrailingBackslash2(Result);
end;
{$endif}

{$ifndef D_2}
{$ifdef MSWINDOWS}
function GetFormattedTimeString(ADate : TDateTime; Format : string) : string;

  function DefDateSeparator : string;
  var
    S : string;
  begin
    {$ifdef MSWINDOWS}
    SetLength(S, 255);
    GetLocaleInfo(GetThreadLocale, LOCALE_SDATE, PChar(S), 254);
    SetLength(S, StrLen(PChar(S)));
    result := S;
    {$else}
    result := '/';
    {$endif}
  end;

  function DefTimeSeparator : string;
  var
    S : string;
  begin
    {$ifdef MSWINDOWS}
    SetLength(S, 255);
    GetLocaleInfo(GetThreadLocale, LOCALE_STIME, PChar(S), 254);
    SetLength(S, StrLen(PChar(S)));
    result := S;
    {$else}
    result := ':';
    {$endif}
  end;

  {$ifdef MSWINDOWS}
  function LongDateToStr(var ST : TSystemTime) : string;
  {$else}
  function LongDateToStr(ST : TDateTime) : string;
  {$endif}
  var
    S : string;
    {$ifndef MSWINDOWS}
    time : tm;
    t    : time_t;
    {$endif}
  begin
    {$ifdef MSWINDOWS}
    SetLength(S, 255);
    GetDateFormat(GetThreadLocale, LOCALE_NOUSEROVERRIDE or DATE_LONGDATE, @ST, nil, PChar(S), 254);
    SetLength(S, StrLen(PChar(S)));
    result := S;
    {$else}
    result := FormatDateTime(LongDateFormat, ST);
    {$endif}
  end;

  {$ifdef MSWINDOWS}
  function LongTimeToStr(var ST : TSystemTime) : string;
  {$else}
  function LongTimeToStr(ST : TDateTime) : string;
  {$endif}
  var
    S : string;
    F : string;
    s1 : string;
    ff : integer;
  begin
    {$ifdef MSWINDOWS}
    SetLength(S, 255);
    GetLocaleInfo(GetThreadLocale, LOCALE_ITIME, PChar(S), 254);
    SetLength(S, StrLen(PChar(S)));
    s1 := DefTimeSeparator;
    ff := 0;
    if StrToIntDef(S, 0) = 0 then
      F := 'hh' + s1 + 'mm' + s1 + 'ss tt'
    else
    begin F := 'HH' + s1 + 'mm' + s1 + 'ss';
      ff := TIME_FORCE24HOURFORMAT;
    end;

    SetLength(S, 255);
    GetTimeFormat(GetThreadLocale, ff, @ST, PChar(F), PChar(S), 254);
    SetLength(S, StrLen(PChar(S)));
    result := S;
    {$else}
    result := FormatDateTime(LongTimeFormat, ST);
    {$endif}
  end;

  function GetAMPMSign(Period : integer) : string;
  var
    S : string;
  begin
    {$ifdef MSWINDOWS}
    SetLength(S, 255);
    if Period = 0 then
      GetLocaleInfo(GetThreadLocale, LOCALE_S1159, PChar(S), 254)
    else
      GetLocaleInfo(GetThreadLocale, LOCALE_S2359, PChar(S), 254);
    SetLength(S, StrLen(PChar(S)));
    result := S;
    {$else}
    if Period = 0 then
      result := 'am'
    else
      result := 'pm';
    {$endif}
  end;

var
  S1, S2, S3, S4 : string;
  i : integer;
  Dt : TDateTime;
  ST : Windows.TSystemTime;
begin
  S1 := Format;
  S2 := '';
  i := 1;
  S4 := S1;
  DT := ADate;
  DateTimeToSystemTime(DT, ST);
  ST.wDayOfWeek := SysUtils.DayOfWeek(DT) - 1;
  while i <= Length(S1) do
  begin
    if S1[i] = '''' then
    begin
      if (i < Length(S1)) and (S1[i + 1] = '''') then
      begin
        S2 := S2 + '''';
        inc(i);
      end
      else
      begin
        inc(i);
        while (i <= Length(S1)) do
        begin
          if S1[i] = '''' then break;
          S2 := S2 + S1[i];
          inc(i);
        end;
      end;
    end
    else if S1[i] = '~' then
      S2 := S2 + #9
    else if S1[i] = ':' then
      S2 := S2 + DefTimeSeparator
    else if S1[i] = '.' then
      S2 := S2 + DefDateSeparator
    else if S1[i] = '|' then
      S2 := S2 + #13#10
    else if Pos('nn', S4) = 1 then
    begin
      S3 := IntToStr(DayNumber(ST.wYear, ST.wMonth, ST.wDay));
      while Length(S3) < 3 do
        S3 := '0' + S3;
      S2 := S2 + S3;
      inc(i);
    end
    else if S1[i] = 'n' then
      S2 := S2 + IntToStr(DayNumber(ST.wYear, ST.wMonth, ST.wDay))
    else if Pos('YY', S4) = 1 then
    begin
      S3 := IntToStr(WeekNumber(ST.wYear, ST.wMonth, ST.wDay));
      if Length(S3) = 1 then S3 := '0' + S3;
      S2 := S2 + S3;
      inc(i);
    end
    else if S1[i] = 'Y' then
      S2 := S2 + IntToStr(WeekNumber(ST.wYear, ST.wMonth, ST.wDay))
    else if Pos('yy', S4) = 1 then
    begin
      S2 := S2 + IntToStr(ST.wYear);
      inc(i);
    end
    else if S1[i] = 'y' then
      S2 := S2 + IntToStr(ST.wYear mod 100)
    else if Pos('mmmm', S4) = 1 then
    begin
      S2 := S2 + LongMonthNames[ST.wMonth];
      inc(i, 3);
    end
    else if Pos('mmm', S4) = 1 then
    begin
      S2 := S2 + ShortMonthNames[ST.wMonth];
      inc(i, 2);
    end
    else if Pos('mm', S4) = 1 then
    begin
      S3 := IntToStr(ST.wMonth);
      if Length(S3) = 1 then S3 := '0' + S3;
      S2 := S2 + S3;
      inc(i);
    end
    else if S1[i] = 'm' then
      S2 := S2 + IntToStr(ST.wMonth)
    else if Pos('TT', S4) = 1 then
    begin
      S2 := S2 + Uppercase(GetAMPMSign(ST.wHour div 12));
      inc(i);
    end
    else if Pos('tt', S4) = 1 then
    begin
      S2 := S2 + Lowercase(GetAMPMSign(ST.wHour div 12));
      inc(i);
    end
    else if Pos('ww', S4) = 1 then
    begin
      S2 := S2 + LongDayNames[ST.wDayOfWeek + 1];
      inc(i);
    end
    else if S1[i] = 'w' then
      S2 := S2 + ShortDayNames[ST.wDayOfWeek + 1]
    else if S1[i] = 'W' then
      S2 := S2 + IntToStr(ST.wDayOfWeek)
    else if Pos('hh', S4) = 1 then
    begin
      if (ST.wHour = 0) or (ST.wHour = 12) then
        S3 := '12'
      else
        S3 := IntToStr(ST.wHour mod 12);
      if Length(S3) = 1 then S3 := '0' + S3;
      S2 := S2 + S3;
      inc(i);
    end
    else if S1[i] = 'h' then
    begin
      if (ST.wHour = 0) or (ST.wHour = 12) then
        S2 := S2 + '12'
      else
        S2 := S2 + IntToStr(ST.wHour mod 12);
    end
    else if Pos('HH', S4) = 1 then
    begin
      S3 := IntToStr(ST.wHour);
      if Length(S3) = 1 then S3 := '0' + S3;
      S2 := S2 + S3;
      inc(i);
    end
    else if S1[i] = 'H' then
      S2 := S2 + IntToStr(ST.wHour)
    else if Pos('MM', S4) = 1 then
    begin
      S3 := IntToStr(ST.wMinute);
      if Length(S3) = 1 then S3 := '0' + S3;
      S2 := S2 + S3;
      inc(i);
    end
    else if S1[i] = 'M' then
      S2 := S2 + IntToStr(ST.wMinute)
    else if Pos('SS', S4) = 1 then
    begin
      S3 := IntToStr(ST.wSecond);
      if Length(S3) = 1 then S3 := '0' + S3;
      S2 := S2 + S3;
      inc(i);
    end
    else if S1[i] = 'S' then
      S2 := S2 + IntToStr(ST.wSecond)
    else if Pos('dd', S4) = 1 then
    begin
      S3 := IntToStr(ST.wDay);
      if Length(S3) = 1 then S3 := '0' + S3;
      S2 := S2 + S3;
      inc(i);
    end
    else if S1[i] = 'd' then
      S2 := S2 + IntToStr(ST.wDay)
    else if Pos('xx', S4) = 1 then
    begin
      S2 := S2 + LongTimeToStr(ST);
      inc(i);
    end
    else if S1[i] = 'x' then
      S2 := S2 + TimeToStr(DT)
    else if Pos('DD', S4) = 1 then
    begin
      S2 := S2 + LongDateToStr(ST);
      inc(i);
    end
    else if S1[i] = 'D' then
      S2 := S2 + DateToStr(DT)
    else
    begin
      S2 := S2 + S1[i];
    end;
    inc(i);
    S4 := Copy(S1, i, Length(S1));
  end;
  Result := S2;
end;
{$endif}
{$endif}

function DayNumber(AYear, AMonth, ADay : integer) : integer;
var
  i : integer;
begin
  Result := 0;
  for i := 1 to AMonth - 1 do
    Inc(Result, DaysPerMonth(AYear, i));
  Inc(Result, ADay);
end;

function WeekNumber(AYear, AMonth, ADay : integer) : integer;
var
  FirstDay, FirstMonday, YearOffset: integer;
begin
  YearOffset := DayNumber(AYear, AMonth, ADay);
  FirstDay := ElDayOfWeek(1, 1, AYear);
  Dec(FirstDay);
  if FirstDay = 0 then FirstDay := 7;
  if FirstDay = 1 then FirstMonday := 1
    else FirstMonday := 9 - FirstDay;
  if (AMonth = 1) and (ADay < FirstMonday) then
    if FirstDay <= 4 then
      Result := 1
    else
      Result := WeekNumber(AYear - 1, 12, 31)
  else
  begin
    Result := ((YearOffset - FirstMonday) div 7) +1;
    if (FirstDay <= 4) and (FirstDay > 1) then Inc(Result);
  end;  
end;

function ExtractTime(ATime : TDateTime) : TDateTime;
begin
  result := Frac(ATime);
end;

function ExtractDate(ATime : TDateTime) : TDateTime;
begin
  result := Trunc(ATime);
end;

function IncTime(ATime : TDateTime; Hours, Minutes, Seconds, MSecs : Integer) : TDateTime;
begin
  Result := ATime + (Hours div 24) + (((Hours mod 24) * 3600000 +
    Minutes * 60000 + Seconds * 1000 + MSecs) / MSecsPerDay);
end;

procedure CenterRects(WS, WT, HS, HT : integer; var R : TRect);
begin
  R.Left := max(WT div 2 - WS div 2, 0);
  R.Right := min(R.Left + WS - 1, WT + R.Left - 1);//changed line
  R.Top := max(HT div 2 - HS div 2, 0);
  R.Bottom := min(R.Top + HS - 1 , HT + R.Top - 1);//changed line
end;

const
  bin2b64 : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  bin2uue : string = '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';

function ReadTextFromStream;
var
  b : boolean;
  cs, cp, rr : integer;
  p : PChar;
begin
  cs := 256;
  cp := 0;
  Data := '';
  b := false;
  result := false;
  while not b do
  begin
    SetLength(Data, cp + cs);
    p := PChar(Data);
    inc(p, cp);
    rr := S.Read(p^, cs);
    if rr <> cs then
    begin
      b := true;
      SetLength(Data, cp + rr);
    end;
    if (Pos(#13#10, Data) > 0) then
    begin
      b := true;
      result := true;
      SetLength(Data, cp + rr);
      rr := rr - Pos(#13#10, StrPas(p)) - 1;
      S.Seek(-rr, soFromCurrent);
      SetLength(Data, Length(Data) - rr - 2);
    end
    else
      inc(cp, cs);
  end;
end;

procedure WriteTextToStream;
var
  i : integer;
begin
  i := Length(Data);
  if (i < 2) or (not ((Data[i - 2] = #13) and (Data[i - 1] = #10))) then Data := Data + #13#10;
  S.Write(PChar(Data)^, Length(Data));
end;

function encode_line(const buf; size : integer) : string;
type
  ta_8u = packed array[0..65530] of byte;
var
  buff : ta_8u absolute buf;
  offset : shortint;
  pos1, pos2 : byte;
  i : byte;
  out : string;

begin
  setlength(out, size * 4 div 3 + 4);
  fillchar(out[1], size * 4 div 3 + 2, #0); (* worst case *)
  offset := 2;
  pos1 := 0;
  pos2 := 2;
  out[pos2] := #0;
  while pos1 < size do
  begin
    if offset > 0 then
    begin
      out[pos2] := char(ord(out[pos2]) or ((buff[pos1] and ($3F shl offset)) shr offset));
      offset := offset - 6;
      inc(pos2);
      out[pos2] := #0;
    end
    else if offset < 0 then
    begin
      offset := abs(offset);
      out[pos2] := char(ord(out[pos2]) or ((buff[pos1] and ($3F shr offset)) shl offset));
      offset := 8 - offset;
      inc(pos1);
    end
    else
    begin
      out[pos2] := char(ord(out[pos2]) or ((buff[pos1] and $3F)));
      inc(pos2);
      inc(pos1);
      out[pos2] := #0;
      offset := 2;
    end;
  end;
  if offset = 2 then dec(pos2);
  for i := 2 to pos2 do
    out[i] := bin2uue[ord(out[i]) + 1];
  encode_line := copy(out, 1, pos2);
end;

function FileNameValid(FileName : string) : boolean;
var
  i : integer;
  c : char;

const
  UnsafeChars = ['/', '\', '>', '<', ':', '"', '|', '?', '*'];
begin
  result := false;
  for i := 1 to Length(FileName) do // Iterate
  begin
    c := FileName[i];
    if (Ord(C) < 32) or (c in UnsafeChars) then exit;
  end; // for
  result := true;
end;

{$IFDEF VCL_4_USED}
function GetFileSize(const FileName: string): Int64;
var
{$ifdef MSWINDOWS}
  Handle: THandle;
  FindData: TWin32FindData;
{$else}
  astat : TStatBuf;
{$endif}
begin
{$ifdef MSWINDOWS}
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Int64Rec(Result).Lo := FindData.nFileSizeLow;
      Int64Rec(Result).Hi := FindData.nFileSizeHigh;
      Exit;
    end;
  end;
  Result := -1;
{$else}
  if (stat(PChar(FileName), astat) = 0) then
    result := astat.st_size
  else
    result := -1;
{$endif}
end;
{$ELSE}
function GetFileSize(const FileName: string): Integer;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
    Result := SearchRec.Size
  else
    Result := -1;
  FindClose(SearchRec);
end;
{$ENDIF}

function FileDateTime(const FileName: string): TDateTime;
var
  Age: Integer;
begin
  Age := FileAge(FileName);
  if Age = -1 then
    Result := 0
  else
    Result := FileDateToDateTime(Age);
end;

function CreateFile(FileName : String) : boolean;
var S : TFileStream;
begin
  if not FileExists(FileName) then
  begin
    try
      S := TFileStream.Create(FileName, fmCreate or (fmShareDenyNone));
      S.Free;
      result := true;
    except
      result := false;
    end;
  end else result := true;
end;

procedure EnsureDirExists(RootName, DirName : string);
begin

  if (Length(RootName) > 0) and (RootName[Length(RootName)] in ['\', '/']) then
    Delete(RootName, Length(RootName), 1);

  if RootName <> '' then
    if not DirExists(RootName) then
       EnsureDirExists(ExtractFilePath(RootName), ExtractFileName(RootName));

  RootName := AppendSlash(RootName);
  if (DirName  <> '') and (not DirExists(RootName + DirName)) then
     CreateDir(RootName + DirName);
end;

function EnsureValidFileName(PathName, FileName : string) : string;
var pn : string;
begin
  pn := AppendSlash(PathName);
  result := pn + FileName;
  if not CreateFile(result) then
    result := pn + GetTempFile(pn);
end;

function DirExists(DirName : string) : boolean;
var
  SRec : TSearchRec;
begin
  result := false;

  if (Length(DirName) > 0) and (DirName[Length(DirName)] in ['\', '/']) then
    Delete(DirName, Length(DirName), 1);

  if FindFirst(DirName, faAnyFile, SRec) = 0 then
  begin
    if (SRec.Attr and faDirectory) > 0 then result := true;
  end;
  FindClose(SRec);
end;

{$ifndef CLX_USED}
{$warnings off}
function PurgeDir(DirName : string) : boolean;
var SRec : TSearchRec;
begin
  result := false;
  DirName := AppendSlash(DirName);
  if FindFirst(DirName + '*.*', faAnyFile, SRec) = 0 then
  try
    repeat
      if (SRec.Attr and faDirectory) <> faDirectory then
      begin
        if not DeleteFile(DirName + SRec.FindData.cFileName) then
          exit;
      end
      else
      begin
        if not PurgeDir(DirName + SRec.FindData.cFileName) or not Removedirectory(PChar(DirName + SRec.FindData.cFileName)) then
          exit;
      end;
    until FindNext(SRec) <> 0;
  finally
    FindClose(SRec);
  end;
  result := true;
end;
{$warnings on}
{$endif}

function RunProgram(StartName, Params, StartDir : string) : THandle;
{$ifdef MSWINDOWS}
var
  CrFlags : DWORD;
  CurDir : PChar;
  AppName : PChar;
  FParams : PChar;
  s : string;
  StartInfo : TStartupInfo;
  ProcInfo : TProcessInformation;
  res : boolean;
{$endif}

begin
  {$ifdef MSWINDOWS}
  if Pos(' ', StartName) > 0 then
      StartName := '"' + StartName + '"';
  if IsWinNt then
  begin
    s := StartName + #32 + Params;
    AppName := nil;
  end
  else
  begin
    s := StartName;
    GetMem(AppName, 260);
    StrPCopy(AppName, s);
    s := Params;
  end;
  GetMem(FParams, 260);
  StrPCopy(FParams, s);
  GetMem(CurDir, 260);
  s := StartDir;
  StrPCopy(CurDir, s);
  // Set process specific flags
  CrFlags := NORMAL_PRIORITY_CLASS + CREATE_DEFAULT_ERROR_MODE;
  // set Startup information
  FillMemory(@StartInfo, SizeOf(TStartupInfo), 0);
  with StartInfo do
  begin
    cb := SizeOf(TStartupInfo);
    lpDesktop := nil;
    lpTitle := nil;
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_SHOW;
  end; // with
  res := CreateProcess(AppName, FParams, nil, nil, false, CrFlags, nil,
    CurDir, StartInfo, ProcInfo);
  FreeMem(FParams, 260);
  FreeMem(CurDir, 260);
  if AppName <> nil then FreeMem(AppName, 260);
  result := 0;
  if res then
  begin
    CloseHandle(ProcInfo.hThread);
    Result := ProcInfo.hProcess;
  end;
  {$else}
  result := fork();
  if (result = LongWord(-1)) then
    exit;
  if (result = 0) then
  begin
    chdir(StartDir);
    execl('/bin/sh', 'sh', '-c', startname + ' ' + params, 0);
    exit;
  end;
  {$endif}
end;

procedure TDirectMemoryStream.SetPointer(Ptr : Pointer; Size : Longint);
begin
  inherited;
  Position := 0;
  if Ptr = nil then 
    Capacity := 0;
end;

function MakeReducedDT(Year, Month, Day, DOW, Hour, Min : word) : TReducedDateTime;
begin
  result.Year := year;
  result.month := month;
  Result.Day := day;
  result.DOW := dow;
  result.Hour := hour;
  result.Min := min;
end;

{$IFDEF VER90}

function CompareMem(Var1, Var2 : pointer; size : integer) : boolean; assembler;
asm
  push    esi
  push    edi
  xor     eax,eax
  mov     esi, var1
  mov     edi, var2
  mov     ecx, size
  shr     ecx,2
  mov     edx, size
  and     edx,3
  repe    cmpsd
  jne     @@2
  mov     ecx,edx
  repe    cmpsb
  jne     @@2
@@1:
  inc     eax
@@2:
  pop     edi
  pop     esi
end;

{$ENDIF}

function CompareReducedDT;
begin
  result := CompareMem(@T1, @T2, SizeOf(TReducedDateTime));
end;

function DateTimeToReduced(T : TDateTime) : TReducedDateTime;
var
  i, j : word;
begin
  DecodeDate(T, Result.Year, Result.Month, Result.Day);
  DecodeTime(T, Result.Hour, Result.Min, i, j);
  Result.DOW := SysUtils.DayOfWeek(T) - 1;
end;

function ReducedToDateTime(T : TReducedDateTime) : TDateTime;
begin
  try
    result := EncodeDate(T.Year, T.Month, T.Day) + EncodeTime(T.Hour, T.Min, 0, 0);
  except
    on E : EConvertError do
    begin
      Result := Now;
    end;
  end;
end;

procedure DefineOS;
{$ifdef MSWINDOWS}
var
  VerInfo : TOsVersionInfo;
begin
  VerInfo.dwOsVersionInfoSize := SizeOf(VerInfo);
  GetVersionEx(VerInfo);
  IsWin95 := VerInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS;
  IsWin95OSR2 := (VerInfo.dwPlatformID = VER_PLATFORM_WIN32_WINDOWS) and (Loword(VerInfo.dwBuildNumber) >= 1000);
  IsWinNt := VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT;

  IsWin2000 := (VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and (VerInfo.dwMajorVersion >= 5);
  IsWinNTUp := IsWinNT or IsWin2000Up;

  IsWin98 := (VerInfo.dwPlatformID = VER_PLATFORM_WIN32_WINDOWS) and (VerInfo.dwMinorVersion > 0);
  IsWin98Up := IsWin98;

  IsWinME := (VerInfo.dwPlatformID = VER_PLATFORM_WIN32_WINDOWS) and (VerInfo.dwMinorVersion >= 90);

  IsWinXP := (VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and (VerInfo.dwMajorVersion = 5) and (VerInfo.dwMinorVersion = 1);
  IsWinXPUp := (VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and (VerInfo.dwMajorVersion >= 5) and (VerInfo.dwMinorVersion >= 1);
  IsWin2000Up := IsWin2000 or IsWinXP;
{$else}
begin
  IsLinux := true;
{$endif}
end;

function IsBIn;
begin
  result := ((Storage shr (index - 1)) and 1) = 1;
end;

function Sign(a : integer) : integer;
begin
  if a < 0 then
    result := -1
  else
  if a > 0 then
    result := 1
  else
    result := 0;
end;

function InRangeF(L, R, x : double) : boolean;
begin
  result := not ((x < L) or (x > R));
end;

function InRange(L, R, x : integer) : boolean;
begin
  result := not ((x < L) or (x > R));
end;

function Max(a, b : integer) : integer;
begin
  if a < b then
    result := b
  else
    result := a;
end;

function Min(a, b : integer) : integer;
begin
  if a > b then
    result := b
  else
    result := a;
end;
{$IFNDEF VER90}

{$ifdef MSWINDOWS}
function SubtractTimes(Time1, Time2 : TDateTime) : TDateTime;
var
  TT : TSystemTime;
  TFT1, TFT2, TFT3 : TFileTime;
  h1, h2, l1, l2 : integer;
begin
  begin
    DateTimeToSystemTime(Time1, TT);
    SystemTimeToFileTime(TT, TFT1);
    DateTimeToSystemTime(Time2, TT);
    SystemTimeToFileTime(TT, TFT2);
    h1 := TFT1.dwHighDateTime;
    h2 := TFT2.dwHighDateTime;
    l1 := TFT1.dwLowDateTime;
    l2 := TFT2.dwLowDateTime;
    if (h1 < h2) or
      ((h1 = h2) and (l1 < l2)) then
      result := 0
    else
    begin
      asm
           push eax
           mov eax, l1
           cmp eax, l2
           jl @1
           mov eax, l1
           sub eax, l2
           sub eax, $FFFFFFFF
           mov TFT3.dwLowDateTime, eax
           jmp @2
          @1:
          mov eax, l1
          sub eax, l2
          mov TFT3.dwLowDateTime, eax
          @2:
           // subtracting high-order dwords
           mov eax, h1
           sub eax, h2
           mov TFT3.dwHighDateTime, eax
           pop eax
      end;
      FileTimeToSystemTime(TFT3, TT);
      TT.wYear := 299 + TT.wYear;
      result := SystemTimeToDateTime(TT);
    end;
    exit;
  end;
end;
{$endif}
{$ENDIF}

function RangesIntersect(L1, R1, L2, R2 : integer) : boolean;
begin
  result := not((Min(L1, R1) > Max(L2, R2)) or (Min(L2, R2) > Max(L1, R1)));
end;

function WriteStringToStream(S : TStream; Str : string) : boolean;
var
  i : integer;
begin
  i := Length(Str);
  try
    S.WriteBuffer(i, sizeof(integer));
    if i > 0 then
      S.WriteBuffer(Str[1], i);
    result := true;
  except
    result := false;
  end;
end;

function ReadStringFromStream(S : TStream; var Str : string) : boolean;
var
  SS : string;
  i : integer;
begin
  try
    S.ReadBuffer(i, sizeof(integer));
    if i < 0 then raise Exception.Create('Invalid string header read from the stream');
    if i = 0 then
      Str := ''
    else
    begin
      SetLength(SS, i);
      S.ReadBuffer(SS[1], i);
      Str := SS;
    end;
    result := true;
  except
    result := false;
  end;
end;

{$ifndef BROKEN_UNICODE}
function WriteWideStringToStream(S : TStream; Str : WideString) : boolean;
var
  i : integer;
begin
  i := Length(Str);
  try
    S.WriteBuffer(i, sizeof(integer));
    if i > 0 then
      S.WriteBuffer(Str[1], i * sizeof(WideChar));
    result := true;
  except
    result := false;
  end;
end;

function ReadWideStringFromStream(S : TStream; var Str : WideString) : boolean;
var
  SS : WideString;
  i : integer;
begin
  try
    S.ReadBuffer(i, sizeof(integer));
    if i < 0 then
      raise Exception.Create('Invalid string header read from the stream');
    if i = 0 then
      Str := ''
    else
    begin
      SetLength(SS, i);
      S.ReadBuffer(SS[1], i * sizeof(WideChar));
      Str := SS;
    end;
    result := true;
  except
    result := false;
  end;
end;
{$endif}

function WriteFStringToStream(S : TStream; Str : TElFString) : boolean;
begin
  {$IFDEF ELPACK_UNICODE}
  Result := WriteWideStringToStream(S, Str);
  {$ELSE}
  Result := WriteStringToStream(S, Str);
  {$ENDIF}
end;

function ReadFStringFromStream(S : TStream; var Str : TElFString) : boolean;
  {$IFDEF ELPACK_UNICODE}
var
  S2: WideString;
begin
  Result := ReadWideStringFromStream(S, S2);
  {$ELSE}
var
  S2: String;
begin
  Result := ReadStringFromStream(S, S2);
  {$ENDIF}
  Str := S2;
end;
function IsLeapYear(AYear : Integer) : Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

const aDaysCentury = 15020;

function ElDateTimeToSeconds(ADay, AMonth, AYear, AHours, AMinute, ASecond : integer) : Cardinal;
begin
  result := (DateToJulianDays(ADay, AMonth, AYear) - aDaysCentury) * 86400;
  inc(result, aHours * 3600 + AMinute * 60 + aSecond);
end;

procedure ElSecondsToDateTime(Seconds : Cardinal; var ADay, AMonth, AYear, AHours, AMinute, ASecond : integer);
var aDays : integer;
    aSecs : integer;
begin
  aDays   := Seconds div 86400 + aDaysCentury;
  aSecs   := Seconds mod 86400;
  JulianDaysToDate(ADay, AMonth, AYear, aDays);
  AHours  := aSecs div 3600;
  AMinute := (aSecs - (AHours * 3600)) div 60;
  ASecond := aSecs mod 60; 
end;

function  DateToJulianDays(ADay, AMonth, AYear : Integer) : integer;
var A, B : integer;
begin
  a := 10000 * Ayear + 100 * AMonth + ADay;
  if AMonth <= 2 then
  begin
    AMonth := AMonth + 12;
    AYear  := AYear - 1;
  end;
  if a <=15821004 then
    b := -2 + trunc((AYear+4716)/4)-1179
  else
    b:=trunc(AYear/400) - trunc(AYear/100)+trunc(AYear/4);
  a := 365 * AYear-679004;
  Result := a + b + trunc(30.6001*(AMonth + 1))+ ADay;
end;

procedure JulianDaysToDate(var ADay, AMonth, AYear :Integer; JulianDate : integer);
var JD, b, c, d, e, f : integer;
begin
  JD := JulianDate + 2400001;
  if JD < 2299161 then
  begin
    c := JD + 1524;
  end
  else
  begin
    b:=trunc((JD-1867216.25)/36524.25);
    c:=JD+(b-trunc(b/4))+1525;
  end;
  d:=trunc((c-122.1)/365.25);
  e:=365*d+trunc(d/4);
  f:=trunc((c-e)/30.6001);
  ADay:=trunc(c-e+0.5)-trunc(f*30.6001);
  AMonth:=f-1-12*trunc(f/14);
  AYear:=D-4715-trunc((7+AMonth)/10);
end;

function ElDayOfWeek(ADay, AMonth, AYear : Integer) : Integer;
begin
  if AMonth > 2 then inc(AMonth) else
  begin
    Inc(AMonth, 13);
    Dec(AYear);
  end;
  result := trunc(365.25*AYear)+trunc(30.6*AMonth)+ADay-621050;
  Result := result - trunc(result/7) * 7 + 1;
  if result = 7 then result := 0;
  inc(Result); 
end;

function DaysPerMonth(AYear, AMonth : Integer) : Integer;
const
  DaysInMonth : array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result); { leap-year Feb is special }
end;

function IncDate(ADate : TDateTime; Days, Months, Years : Integer) : TDateTime;
var
  D, M, Y : Word;
  Day, Month, Year : Longint;
begin
  DecodeDate(ADate, Y, M, D);
  Year := Y;
  Month := M;
  Day := D;
  Inc(Year, Years);
  Inc(Year, Months div 12);
  Inc(Month, Months mod 12);
  if Month < 1 then
  begin
    Inc(Month, 12);
    Dec(Year);
  end
  else if Month > 12 then
  begin
    Dec(Month, 12);
    Inc(Year);
  end;
  if Day > DaysPerMonth(Year, Month) then Day := DaysPerMonth(Year, Month);
  Result := EncodeDate(Year, Month, Day) + Days + Frac(ADate);
end;

{$IFDEF VER90}

type
  PDayTable = ^TDayTable;
  TDayTable = array[1..12] of Word;

const
  MonthDays : array[Boolean] of TDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
    (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

function DoEncodeDate(Year, Month, Day : Word; var Date : TDateTime) : Boolean;
var
  I : Integer;
  DayTable : PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do
      Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function EncodeDate(Year, Month, Day : Word) : TDateTime;
begin
  if not DoEncodeDate(Year, Month, Day, Result) then raise exception.Create('Invalid argument for date encode.');
end;

procedure DivMod(Dividend : Integer; Divisor : Word;
  var Result, Remainder : Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;

procedure DecodeDate(Date : TDateTime; var Year, Month, Day : Word);
const
  D1 = 365;
  D4 = D1 * 4 + 1;
  D100 = D4 * 25 - 1;
  D400 = D100 * 4 + 1;
var
  Y, M, D, I : Word;
  T : Integer;
  DayTable : PDayTable;
begin
  T := DateTimeToTimeStamp(Date).Date;
  if T <= 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
  end
  else
  begin
    Dec(T);
    Y := 1;
    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;
    DivMod(T, D100, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;
    Inc(Y, I * 100);
    DivMod(D, D4, I, D);
    Inc(Y, I * 4);
    DivMod(D, D1, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D1);
    end;
    Inc(Y, I);
    DayTable := @MonthDays[IsLeapYear(Y)];
    M := 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y;
    Month := M;
    Day := D + 1;
  end;
end;

procedure DateTimeToSystemTime(DateTime : TDateTime; var SystemTime : TSystemTime);
begin
  with SystemTime do
  begin
    DecodeDate(DateTime, wYear, wMonth, wDay);
    DecodeTime(DateTime, wHour, wMinute, wSecond, wMilliseconds);
  end;
end;

function SystemTimeToDateTime(const SystemTime : TSystemTime) : TDateTime;
begin
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
end;

{$ENDIF}

{$ifdef MSWINDOWS}
function NowToUTC : TDateTime;
var ST : TSystemTime;
begin
  GetSystemTime(ST);
  result := SystemTimeToDateTime(ST);
end;
{$endif}

{$ifdef MSWINDOWS}
procedure ToAbsoluteDate(aYear : integer; var ATime : TSystemTime);
var DayNum : integer;
    DIM : Integer;
begin
  ATime.wYear := aYear;
  if (ATime.wDay >= 1) and (ATime.wDay <= 4) then begin
    DayNum := SysUtils.DayOfWeek(EncodeDate(aTime.wYear, aTime.wMonth, 1));      // get first day in month
    DayNum := aTime.wDayOfWeek - DayNum + 1;                  // get first dayInWeek in month
    if DayNum <= 0 then Inc(DayNum, 7);
    DayNum := DayNum + (ATime.wDay - 1) * 7;   // get weekInMonth-th dayInWeek in month
    ATime.wDay := DayNum;
  end
  else
  if ATime.wDay = 5 then
  begin // last week, calculate from end of month
    DIM := DaysPerMonth(aTime.wYear, aTime.wMonth);
    dayNum  := SysUtils.DayOfWeek(EncodeDate(aTime.wYear, aTime.wMonth, DIM));   // get last day in month
    dayNum  := DIM + (aTime.wDayOfWeek - dayNum);
    if dayNum > DIM then Dec(dayNum, 7);              // get last dayInWeek in month
    ATime.wDay := DayNum;
  end;

(*  ATime.wYear := aYear;
  FirstDate := EncodeDate(ATime.wYear, ATime.wMonth, 1);
  FMonthOffset := 2 - ((DateTimeToTimeStamp(FirstDate).Date mod 7 + 8) mod 7); { day of week for 1st of month }
  if FMonthOffset = 2 then FMonthOffset := -5;
  DayNum := FMonthOffset + ATime.wDayOfWeek + (ATime.wDay) * 7;
  if (DayNum < 1) then inc(DayNum, 7);
  Dim := DaysPerMonth(ATime.wYear, ATime.wMonth);
  while (DayNum > Dim) do dec(DayNum, 7);
  ATime.wDay := DayNum;
  aTime.DayOfWeek := DayOfWeek(SystemTimetoDateTime(aTime));*)
end;

function IsInDaylightTime(lpTimeZoneInformation : TTimeZoneInformation; lpLocalTime : TSystemTime) : boolean;
var TempTime1, TempTime2, TempTime3 : TDateTime;
begin
  result := false;
  if lpTimeZoneInformation.StandardDate.wMonth <> 0 then
  begin
    if lpTimeZoneInformation.StandardDate.wYear = 0
       then ToAbsoluteDate(lpLocalTime.wYear, lpTimeZoneInformation.StandardDate);
    TempTime1 := SystemTimeToDateTime(lpTimeZoneInformation.StandardDate);

    if lpTimeZoneInformation.DaylightDate.wYear = 0
       then ToAbsoluteDate(lpLocalTime.wYear, lpTimeZoneInformation.DaylightDate);
    TempTime2 := SystemTimeToDateTime(lpTimeZoneInformation.DaylightDate);

    TempTime3 := SystemTimeToDateTime(lpLocalTime);

    if TempTime2 < TempTime1 {Northern semisphere}
       then Result := (TempTime3 >= TempTime2) and (TempTime3 < TempTime1)
       else Result := not ((TempTime3 >= TempTime1) and (TempTime3 < TempTime2));
  end;
end;

procedure UTCToZoneLocal(lpTimeZoneInformation : PTimeZoneInformation;
  lpUniversalTime : TSystemTime; var lpLocalTime : TSystemTime);
var DTF : TDateTime;
begin
  MoveMemory(@lpLocalTime, @lpUniversalTime, sizeof(TSystemTime));
  DTF := IncTime(SystemTimeToDateTime(lpLocalTime), 0, -lpTimeZoneInformation.Bias, 0, 0);
  DateTimeToSystemTime(DTF, lpLocalTime);

  if not IsInDaylightTime(lpTimeZoneInformation^, lpLocalTime) then
      DTF := IncTime(DTF, 0, -lpTimeZoneInformation.StandardBias, 0, 0)
    else
      DTF := IncTime(DTF, 0, -lpTimeZoneInformation.DaylightBias, 0, 0);

  DateTimeToSystemTime(DTF, lpLocalTime);
end;

procedure ZoneLocalToUTC(lpTimeZoneInformation : PTimeZoneInformation;
  var lpUniversalTime : TSystemTime; lpLocalTime : TSystemTime);
var DTF : TDateTime;
begin
  MoveMemory(@lpUniversalTime, @lpLocalTime, sizeof(TSystemTime));
  DTF := IncTime(SystemTimeToDateTime(lpUniversalTime), 0, lpTimeZoneInformation.Bias, 0, 0);
  if not IsInDaylightTime(lpTimeZoneInformation^, lpLocalTime) then
      DTF := IncTime(DTF, 0, lpTimeZoneInformation.StandardBias, 0, 0)
    else
      DTF := IncTime(DTF, 0, lpTimeZoneInformation.DaylightBias, 0, 0);

  DateTimeToSystemTime(DTF, lpUniversalTime);
end;

procedure ElSystemTimeToTzSpecificLocalTime(lpTimeZoneInformation : PTimeZoneInformation;
  var lpUniversalTime : TSystemTime;
  var lpLocalTime : TSystemTime);
begin
  UTCToZoneLocal(lpTimeZoneInformation, lpUniversalTime, lpLocalTime);
end;
{$endif}

function ZoneIDtoBias (ZoneID : string) : integer;
const IDs : array [0..17] of string = ('NST', 'AST', 'ADT', 'EST', 'EDT',
                                       'CST', 'CDT', 'MST', 'MDT', 'PST',
                                       'PDT', 'YST', 'YDT', 'HST', 'HDT',
                                       'BST', 'BDT', 'GMT');

      biases : array [0..17] of integer = (210, 240, 180, 300, 240, 360, 300, 420, 360,
                                           480, 420, 540, 480, 600, 540, 660, 600, 0);
var i : integer;
    aID: char;
begin
  ZoneID := Uppercase(ZoneID);
  result := 0;
  if Length(ZoneID) = 0 then
  begin
    result := 0;
    exit;
  end;
  if Length(ZoneID) = 1 then
  begin
    aID := ZoneID[1];
    case aID of
      'J', 'Z' : result := 0;
      'A'..'I',
      'K'..'M' : result := -(Ord(aID) - 64);
      'N'..'Y' : result := Ord(aID) - Ord('M');
    end;
  end else
  if ZoneID[1] in ['0' .. '9'] then
  begin
    result := StrToIntDef(Copy(ZoneID, 1, 2), 0) + StrToIntDef(Copy(ZoneID, 3, 2), 0);
  end else
  if ZoneID[1] in ['+', '-'] then
  begin
    result := StrToIntDef(Copy(ZoneID, 2, 2), 0) + StrToIntDef(Copy(ZoneID, 4, 2), 0);
    if ZoneID[1] = '-' then result := -result;
  end else
  begin
    result := 0;
    for i := 0 to 17 do
      if ZoneID = IDs[i] then
      begin
        result := Biases[i];
        exit;
      end;
  end;
end;

{$ifdef MSWINDOWS}
{$ifndef D_2}
function SetPrivilege(sPrivilegeName : string; bEnabled : boolean) : boolean;
var
  TPPrev,
  TP         : TTokenPrivileges;
  Token      : THandle;
  dwRetLen   : DWord;
begin
  Result := False;
  OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token);
  TP.PrivilegeCount := 1;
  if( LookupPrivilegeValue( Nil, PChar( sPrivilegeName ), TP.Privileges[ 0 ].LUID ) ) then
  begin
    if( bEnabled )then
    begin
      TP.Privileges[ 0 ].Attributes  :=
        SE_PRIVILEGE_ENABLED;
    end else
    begin
      TP.Privileges[ 0 ].Attributes  :=
        0;
    end;
    dwRetLen := 0;
    Result := AdjustTokenPrivileges(
                Token,
                False,
                TP,
                SizeOf( TPPrev ),
                TPPrev,
                dwRetLen );
  end;
  CloseHandle( Token );
end;
{$endif}

type PEnumWinRec = ^TEnumWinRec;
     TEnumWinRec = record
       WinFound : boolean;
       Count    : integer;
       WinHandle: HWND;
       Caption  : string;
       ClassName: string;
       Exact    : boolean;
       TopWin   : boolean;
     end;

     PEnumProcRec = ^TEnumProcRec;
     TEnumProcRec = record
       ModuleName  : string;
       ModuleFound : boolean;
       ModuleID    : integer;
     end;

function FindWinCallBack(WinHandle:HWND;Param:integer):boolean; stdcall;
var
  P:PChar;
  s : string;
  s1,
  s2: string;
{$IFDEF VCL_4_USED}
  pr: Cardinal;
{$ELSE}
  pr: integer;
{$ENDIF}
begin
  GetMem(P,256);
  SetLength(s,256);
  if PEnumWinRec(Param)^.ClassName <> '' then
  begin
    GetClassName(WinHandle, P, 255);
    s1:=UpperCase(PEnumWinRec(Param)^.ClassName);
    s2:=UpperCase(P);
    if PEnumWinRec(Param)^.Exact then
       PEnumWinRec(Param)^.WinFound := s1=s2
    else
    begin
      result := true;
      PEnumWinRec(Param)^.WinFound := false;
      exit;
    end;
  end;

  if SendMessageTimeout(WinHandle,WM_GETTEXT, 255,Integer(P), SMTO_ABORTIFHUNG or SMTO_BLOCK, 100, pr) <> 0 then
  begin
    s:=StrPas(P);

    s1:=UpperCase(PEnumWinRec(Param)^.Caption);
    s2:=UpperCase(P);
    if PEnumWinRec(Param)^.Exact then
       PEnumWinRec(Param)^.WinFound := s1=s2
    else
       PEnumWinRec(Param)^.WinFound:= Pos(s1, s2)>0;
  end
  else
    PEnumWinRec(Param)^.WinFound := false;

  if PEnumWinRec(Param)^.WinFound then
    PEnumWinRec(Param)^.WinHandle := WinHandle;

  result:= not (PEnumWinRec(Param)^.WinFound);
  if result and not PEnumWinRec(Param)^.TopWin then
  begin
    EnumChildWindows(WinHandle, @FindWinCallBack, Param);
    result := not PEnumWinRec(Param).WinFound;
  end;

  inc(PEnumWinRec(Param)^.Count);
  FreeMem(P,256);
end;

function WindowExists;
  var SRec : TEnumWinRec;

begin
  SRec.WinFound:=false;
  SRec.Caption := Caption;
  SRec.ClassName := ClassName;
  SRec.Count:=0;
  SRec.WinHandle := 0;
  SRec.TopWin := false;
  SRec.Exact := ExactMatch;
  EnumWindows(@FindWinCallback,integer(@SRec));
  if SRec.WinFound then
  begin
    result := SRec.WinHandle;
    LastWin := result;
  end else result := 0;
end;                           
                                             
function TopWindowExists;
var SRec : TEnumWinRec;            
    P    : PChar;
begin
  if ExactMatch then
  begin
    if ClassName <> '' then P := nil else P := PChar(ClassName);
    LastWin := FindWindow(P, PChar(Caption));
    result := LastWin;
  end else
  begin
    SRec.WinFound:=false;
    SRec.Caption := Caption;
    SRec.ClassName := ClassName;
    SRec.Count:=0;
    SRec.WinHandle := 0;
    SRec.TopWin := true;
    EnumWindows(@FindWinCallback,integer(@SRec));
    if SRec.WinFound then
    begin
      result := SRec.WinHandle;
      LastWin := result;
    end else result := 0;
  end;
end;
(*
function ProcessExists;
{$IFNDEF NT}
var
   hSnapshot: THandle;
   pe32: TProcessEntry32;
{$ELSE}
  var pidArray:array[0..1000] of Integer;
      mhArray:array[0..1000] of HMODULE;
      b:BOOL;
      i,j,pc,mc:Integer;
      ph:THandle;
      mn:array[0..1000] of char;
      SRec : TEnumProcRec;

      function EnumProc(dwThreadId:LongInt; hMod16,hTask16:Word;
               pszModName,pszFileName:PChar; lpUserDefined:LongInt):BOOL; stdcall;
      begin
        if UpperCase(ExtractFileName(StrPas(pszFileName)))=UpperCase(PEnumProcRec(lpUserDefined)^.ModuleName) then
        begin
          PEnumProcRec(lpUserDefined)^.ModuleFound:=true;
          PEnumProcRec(lpUserDefined)^.ModuleID := 0;
        end;
        Result:=True;
      end;
{$ENDIF}
begin
    LastProcessID := 0;
{$IFNDEF NT}
    hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    pe32.dwSize := sizeof(TProcessEntry32);
     // Walk the snapshot of the processes
    if Process32First(hSnapshot, pe32) then
      repeat
         if CompareText(ExtractFileName(pe32.szExeFile), PChar(Name)) = 0 then
         begin
           result:=pe32.th32ProcessID;
           CloseHandle(hSnapshot);
           exit;
         end;
      until not Process32Next(hSnapshot, pe32);
      CloseHandle(hSnapshot);
    Result := 0;
{$ELSE}
  b:=EnumProcesses(PProcessIDArray(@pidArray),SizeOf(pidArray),pc);
  if not b then
  begin
    result := 0;
    exit;
  end;
  pc:=pc div SizeOf(LongInt);
  if (pc>1001) then pc:=1001;
  for i:=1 to pc do
  begin
    ph:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,pidArray[i-1]);
    if (ph<>0) then
    begin
      FillChar(mhArray,SIzeOf(mhArray),0);
      b:=EnumProcessModules(ph,PModuleHandleArray(@mhArray),SizeOf(mhArray),mc);
      if b then
      begin
        mc:=mc div SizeOf(LongInt);
        if (mc>1001) then mc:=1001;
        for j:=1 to mc do begin
          FillChar(mn,SizeOf(mn),0);
          if (GetModuleFileNameEx(ph,mhArray[j-1],mn,SizeOf(mn))<>0) then
          begin
            if (Pos('\SYSTEM32\NTVDM.EXE',UpperCase(StrPas(mn)))>0) then
            begin
              SRec.ModuleFound:=false;
              SRec.ModuleName:=Name;
              VDMEnumTaskWOWEx(pidArray[i-1],@EnumProc,integer(@SRec));
              if SRec.ModuleFound then
              begin
                result := -1;
                if ph<>0 then CloseHandle(ph);
                exit;
              end;
            end else
            begin
              if (GetModuleBaseName(ph, mhArray[j-1], mn, SizeOf(mn))<>0)
                 and (UpperCase(StrPas(mn))=UpperCase(Name)) then
              begin
                result := pidArray[i-1];
                if ph<>0 then CloseHandle(ph);
                exit;
              end;
            end;
          end;
        end;
      end;
    end;
    if (ph<>0) then CloseHandle(ph);
  end;
  result:=0;
{$ENDIF}
end;
*)
{$endif}

{$IFDEF EL_DEMO}
{$ifdef MSWINDOWS}
function DelphiIsRunning : Boolean;
const
  A1 : array[0..12] of char = 'TApplication'#0;
  A2 : array[0..15] of char = 'TAlignPalette'#0;
  A4 : array[0..11] of char = 'TAppBuilder'#0;
begin
  Result := (FindWindow(A1, nil) <> 0) and (FindWindow(A2, nil) <> 0) and (FindWindow(A4, nil) <> 0);
  if not Result then
    MessageBox(0, 'This application makes use of demo version of ElPack.'#13#10'Please request the developer to ship the version of this application with registered version of ElPack components',
      'ElPack Demo', mb_OK + mb_IconStop);
end;
{$endif}
{$ENDIF}

function AppendSlash(const PathName : String) : string;
begin
  Result := IncludeTrailingBackslash2(PathName);
end;

function GetModulePath : string;
begin
  SetLength(Result, MAXPATHLEN + 1);
  GetModuleFileName(HInstance, PChar(Result), MAXPATHLEN);
  Result:= IncludeTrailingBackslash2(ExtractFilePath(Result));
end;

{$ifndef KYLIX_USED}
function GetComputerName : string;
var Size : DWORD;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(result, Size);
  Windows.GetComputerName(PChar(result), Size);
  SetLength(result, size);
end;
{$endif}

function RectToString(Rect : TRect) : string;
begin
  result := IntToStr(Rect.Left) + ',' + IntToStr(Rect.Top) + ',' + IntToStr(Rect.Right) + ',' + IntToStr(Rect.Bottom);
end;

function StringToRect(AString : string) : TRect;
var S : String;
begin
  try
    S := Copy(AString, 1, Pos(',', AString) - 1);
    if S = '' then exit;
    Delete(AString, 1, Pos(',', AString));
    Result.Left := StrToInt(S);

    S := Copy(AString, 1, Pos(',', AString) - 1);
    if S = '' then exit;
    Delete(AString, 1, Pos(',', AString));
    Result.Top := StrToInt(S);

    S := Copy(AString, 1, Pos(',', AString) - 1);
    if S = '' then exit;
    Delete(AString, 1, Pos(',', AString));
    Result.Right := StrToInt(S);

    S := AString;
    if S = '' then exit;
    Result.Bottom := StrToInt(S);
  except
    {$ifdef MSWINDOWS}
    SetRectEmpty(Result);
    {$else}
    FillChar(Result, sizeof(Result), 0);
    {$endif}
  end;
end;

procedure ValFloat(Value : string; Result : Double; var Error : integer);
var p : integer;
    iPart,
    dPart : integer;
    PC: PChar;
begin
  p := Pos('.', Value);
  if (p > 1) and (p < Length(Value)) then
  begin
    Val(Copy(Value, 1, P - 1), iPart, Error);
    if Error <> 0 then
      exit;
    Val(Copy(Value, P + 1, Length(Value)), dPart, Error);
    if Error <> 0 then
    begin
      Inc(Error, P);
      exit;
    end;
    PC := @Result;
    Move(iPart, PC^, sizeof(ipart));
    Inc(PC, sizeof(ipart));
    Move(dPart, PC^, sizeof(dpart));
    Error := 0;
  end
  else
    if p <> 0 then
      Error := p
    else
      Error := -1;
end;

function StrFloat(Value : double) : String;
var p : pinteger;
begin
  p := @Value;
  Result := IntToStr(P^) + '.';
  inc(p);
  Result := Result + IntToStr(P^);
end;

{$ifndef KYLIX_USED}

type TElMessagePump = class
        protected
          function DoPump : boolean;
          procedure ThreadedMessagePump;
      end;

function TElMessagePump.DoPump : boolean;
var
    Msg : TMsg;
begin
  Result := FALSE;
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    Result := TRUE;
    if Msg.Message = WM_QUIT then
    else
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;
end;

procedure TElMessagePump.ThreadedMessagePump;
begin
  while DoPump do ;
end;

type TWndObjInstance = record
       Obj : TObject;
       WndMethod : TElWndMethod;
     end;
     PWndObjInstance = ^TWndObjInstance;

function XDefWindowProc(aWnd : HWND; aMsg : Integer; wParam : WPARAM; lParam : LPARAM): Integer; stdcall;
var
    Msg   : TMessage;
    ObjInst : PWndObjInstance;
begin
  ObjInst := PWndObjInstance(GetWindowLong(aWnd, GWL_USERDATA));
  if Assigned(ObjInst) and Assigned(ObjInst.WndMethod) then
    begin
      Msg.Msg    := aMsg;
      Msg.wParam := wParam;
      Msg.lParam := lParam;
      ObjInst.WndMethod(Msg);
      Result := Msg.Result;
    end else
      Result := DefWindowProc(aWnd, aMsg, wParam, lParam)
end;

var
  XElWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @XDefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'XElUtilWindow');

function XAllocateHWND(Obj : TObject; WndMethod : TElWndMethod) : HWND;
var TempClass       : TWndClass;
    ClassRegistered : Boolean;
    ObjInst : PWndObjInstance;
begin                            
  XelWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, XelWindowClass.lpszClassName, TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(XelWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(XelWindowClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, XelWindowClass.lpszClassName,
   '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
  GetMem(ObjInst, sizeof(TWndObjInstance));
  ObjInst.Obj := Obj;
  ObjInst.WndMethod := WndMethod;
  SetWindowLong(Result, GWL_USERDATA, Integer(ObjInst));
end;

procedure XDeallocateHWND(Wnd : HWND);
var ObjInst : PWndObjInstance;
begin
  ObjInst := PWndObjInstance(GetWindowLong(Wnd, GWL_USERDATA));
  DestroyWindow(Wnd);
  FreeMem(ObjInst);
end;

var MessagePump : TElMessagePump;

{$endif}

{$ifdef MSWINDOWS}
function GetKeysState : integer;
begin
  result := 0;
  if GetKeyState(VK_CONTROL) < 0 then Result := result or MK_CONTROL;
  if GetKeyState(VK_Shift) < 0 then Result := result or MK_SHIFT;
end;

function GetShiftState : TShiftState;
begin
  result := [];
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_Shift) < 0 then Include(Result, ssShift);
end;
{$endif}

initialization
  DefineOS;
{$IFDEF EL_DEMO}
  DelphiIsRunning;
{$ENDIF}

{$ifndef KYLIX_USED}
  MessagePump := TElMessagePump.Create;
  OnMessagePump := MessagePump.ThreadedMessagePump;

finalization
  MessagePump.Free;
{$endif}

end.
