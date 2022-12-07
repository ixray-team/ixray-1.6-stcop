{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Parts:                                           }
{   copyright (c) 2001 Akzhan Abdulin                }
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
  Version History:

07/11/2002

  UTF8 conversion functions added

05/07/2002

  CreateUnicodeHintString function added

02/11/2002

  WideExtractQuotedStr function fixed

08/20/2001

  Unicode functions added (c) Akzhan Abdulin

07/26/2001

  Added Unicode support

03/15/2001

  * StrStartsWith fixed (string end tests were bad).

  * IsAlpha, IsAlphaStr and IsIdentStr now tests for '_' char too.

  + IsAlphaOrDigit added.

03/14/2001

  + StrStartsWith and ContainsAt methods.

  * H2D function updated.

*)

unit ElStrUtils;

interface

{$ifndef ELPACK_UNICODE}
uses Classes;
{$else}
  {$ifdef CLX_USED}
  uses Classes;
  {$endif}
{$endif}
type

{$ifdef MSWINDOWS}
{$ifdef ELPACK_UNICODE}
  TElFString  = type WideString;
  TElFChar    = type WideChar;
  PElFChar    = type PWideChar;
{$else}
  TElFString  = type string;
  TElFChar    = type Char;
  PElFChar    = type PChar;
{$endif}
{$else}
  TElFString  = type string;
  TElFChar    = type Char;
  PElFChar    = type PChar;
{$endif}

{$ifdef BUILDER_USED}
{$ifdef ELPACK_UNICODE}
{$HPPEMIT 'typedef WideString TElFString;'}
{$else}
{$HPPEMIT 'typedef AnsiString TElFString;'}
{$endif}
{$endif}

{$ifdef MSWINDOWS}
{$ifdef ELPACK_UNICODE}
{$else}
type
  TElFStrings = TStrings;
  TElFStringList = TStringList;
{$endif}
{$else}
type
  TElFStrings = TStringList;
  TElFStringList = TStringList;
{$endif}

{$ifdef VCL_3_USED}
const
  WideCRLF = WideString(#13#10#0);
{$endif}

{$IFDEF WIN32}
const
  oleaut = 'oleaut32.dll';

function SysAllocStringLen(P: PWideChar; Len: Integer): PWideChar; stdcall;
  external oleaut name 'SysAllocStringLen';

procedure SysFreeString(S: PWideChar); stdcall;
  external oleaut name 'SysFreeString';

function SysStringLen(S: PWideChar): Integer; stdcall;
  external oleaut name 'SysStringLen';
{$ENDIF WIN32}

function IntToStrFmt(value: integer): string;
function FloatToStrFmt(value: extended; decims: integer): string;

function IntToStrPad(value: integer; MinSize: integer): string;

function CenterStr(Str : string; len : integer) : string;
{ Centers string in a line }

{$ifndef KYLIX_USED}
function OEMToStr(S: string): string;
function StrToOEM(S: string): string;

function MessageRes(Txt: Integer; Title: PChar; TextType: Word; Sounds: boolean): Integer;
{$endif}

function replace(var Str: string; SourceString, DestString: string): boolean;

function ExtractWord(str: string; n: integer): string;

function FstNonSpace(str: string): integer;

function NextWordBegin(str: string; CurrentPos: integer): integer;

function LastPos(SubStr: string; Strn: string): integer;
{ Returns the beginning of the last substring in a string }

function LineIsEmpty(str: string): boolean;
// Returns true, if line doesn't contain any characters except, probably, spaces and tabs

function CompleteLine(Str: string; FLen: integer; symb: char): string;
{ completes a string with some chars }

function PrefixLine(Str: string; FLen: integer; symb: char): string;

function H2D(S: string): integer;
function H2DDef(const S: string; Def: integer): integer;

function Bin2Int(S: string): integer;

function Bin2IntDef(S: string; Default: integer): integer;

function Data2Str(Buffer: pointer; BufLen: integer): string;
function Str2Data(S: string; var Buffer: pointer; var BufLen: integer): boolean;

function IsDigit(ch: char): boolean;
function IsDigitStr(const S: string): boolean;
function IsAlpha(ch: char): boolean;
function IsAlphaOrDigit(ch: char): boolean;
function IsAlphaStr(const S: string): boolean;
function IsIdentStr(const S: string): boolean;

function ExtractStr(var S: string; SPos, SLen: integer): string;

function FileNameLike(FileName : string; Mask : string) : boolean;

function LeftBreak(S: string; Pos: integer): integer;

function EscapeString(aString: string; UnsafeChars: string; EscapeChar: Char): string;
function EscapeURLString(aString: string; EscapeChar: Char): string;
function UnEscapeString(aString: string; EscapeChar: Char): string;

function StrStartsWith(Source: PChar; Seq: PChar): Boolean;
function ContainsAt(Source: string; Index: Integer; Seq: string): Boolean;

function AnsiSameText(const S1, S2: string): Boolean;

function CurrToPrettyStr(const Value: Currency): string;
function PrettyStrToCurr(const Value: string): Currency;

function CurrSign(const Value: Currency): Integer;

function StringDup(S : String) : PChar;

procedure TStrDelete(var S : TElFString; SPos, SLen : integer);
function  TStrExtractStr(var S: TElFString; SPos, SLen: integer): TElFString;
procedure SetTStr(var S: TElFString; Buffer: PElFChar; Len: Integer);

{$ifndef BROKEN_UNICODE}

const
  doti : boolean = true;

function CreateUnicodeHintString(Value : WideString) : string;

function uni2uppers(s:widestring):widestring;
function uni2lowers(s:widestring):widestring;
function uni2upperf(s:widestring):widestring;
function uni2lowerf(s:widestring):widestring;

function WideMakeString(FLen: Integer; Seq : WideString): WideString;
// creates a new string, filled with specified chars

function WideStringDup(S : WideString) : PWideChar;

function WidePos(const Substr, S : WideString) : integer;
function WideStrScan(const Str: PWideChar; Chr: WideChar): PWideChar; assembler;
function WideStrRScan(const Str: PWideChar; Chr: WideChar): PWideChar; assembler;

function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
function WideStrEnd(const Str: PWideChar): PWideChar; assembler;
function WideCompareText(const S1, S2: WideString): Integer;
function WideSameText(const S1, S2: WideString): boolean;

function WideCopy(S : WideString; SPos, SLen : integer) : WideString;

procedure WideDelete(var S : WideString; SPos, SLen : integer);
procedure WideInsert(Text : WideString; var S : WideString; SPos : integer);
function  WideExtractStr(var S: WideString; SPos, SLen: integer): WideString;
procedure SetWideString(var S: WideString; Buffer: PWideChar; Len: Integer);

function WideStrCopy(Target : PWideChar; Source : PWideChar) : PWideChar;
function WideStrPCopy(Target : PWideChar; const Source: WideString): PWideChar;
function WideStrComp(const S1, S2: PWideChar): Integer; assembler;
function WideStrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function WideStrLen(const Str: PWideChar): Cardinal;
function WideStrPas(const Source: PWideChar): WideString;

procedure WideMove(const Source; var Dest; Count : Integer);
procedure FillWord(var X; Count: Integer; Value: Word);
procedure FillWideChar(var X; Count: Integer; Value: WideChar);

function WideStrMove(Dest: PWideChar; const Source: PWideChar; Count: Cardinal): PWideChar;

function WideStrECopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
function WideStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
function WideStrLCat(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
function WideStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;

function CompareWideStr(const S1, S2: WideString): Integer;
function SameWideStr(const S1, S2: WideString): Boolean;
function WideLastChar(const S: WideString): PWideChar;

{$ifndef KYLIX_USED}
function WideStrAlloc(Size: Cardinal): PWideChar;
function WideStrBufSize(const Str: PWideChar): Cardinal;
function WideStrNew(const Str: PWideChar): PWideChar;
procedure WideStrDispose(Str: PWideChar);
{$endif}

function WideUpperCase(const S: WideString): WideString;
function WideLowerCase(const S: WideString): WideString;

const
  SPathDelimiters = String('/\');
  SWidePathDelimiters = WideString('/\');

function IsWideDelimiter(const Delimiters, S: WideString; Index: Integer): Boolean;
function IsWidePathDelimiter(const S: WideString; Index: Integer): Boolean;
function IncludeWideTrailingDelimiter(const S: WideString): WideString;
function ExcludeWideTrailingDelimiter(const S: WideString): WideString;

function GetWideCharRangeString(FirstChar, LastChar: WideChar): WideString;
function GetWideStringOf(Char: WideChar; Len: Cardinal): WideString;

type
  TWideReplaceFlags = set of (wrfReplaceAll, wrfIgnoreCase);

function WideStringReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TWideReplaceFlags): WideString;

function WideReplace(var Str: WideString; SourceString, DestString: WideString) : boolean;

function WideStrPos(const Str1, Str2: PWideChar): PWideChar; assembler;
function WideLastPos(SubStr, Strn: WideString): integer;

{$endif}

function GetCharRangeString(FirstChar, LastChar: Char): String;

function MakeString(FLen: Integer; Seq : string) : string;

{$ifdef ELPACK_UNICODE}
type
  UTF32 = Longword;
  UTF16 = Word;
  UTF8  = Byte;

  pUTF32 = ^UTF32;
  pUTF16 = ^UTF16;
  pUTF8  = ^UTF8;

  ConversionResult = (
    conversionOK,       { conversion successful }
    sourceExhausted,    { partial character in source, but hit end }
    targetExhausted,    { insuff. room in target for conversion }
    sourceIllegal       { source sequence is illegal/malformed }
  );

  ConversionFlags = ( strictConversion, lenientConversion ); { strictConversion = 0 }

const
  UNI_REPLACEMENT_CHAR: UTF32 = $0000FFFD;
  UNI_MAX_BMP: UTF32 = $0000FFFF;
  UNI_MAX_UTF16: UTF32 = $0010FFFF;
  UNI_MAX_UTF32: UTF32 = $7FFFFFFF;
  halfShift: integer = 10;      { used for shifting by 10 bits }
  halfBase: UTF32 = $0010000;
  halfMask: UTF32 = $3FF;
  UNI_SUR_HIGH_START: UTF32 = $0D800;
  UNI_SUR_HIGH_END: UTF32 = $0DBFF;
  UNI_SUR_LOW_START: UTF32 = $0DC00;
  UNI_SUR_LOW_END: UTF32 = $0DFFF;

{
  Index into the table below with the first byte of a UTF-8 sequence to
  get the number of trailing bytes that are supposed to follow it.
}
  trailingBytesForUTF8: array [0..255] of byte = (
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
  );

{
  Magic values subtracted from a buffer value during UTF8 conversion.
  This table contains as many values as there might be trailing bytes
  in a UTF-8 sequence.
}
  offsetsFromUTF8: array [0..5] of UTF32 = ($00000000, $00003080, $000E2080,
                                            $03C82080, $0FA082080, $82082080);
{
  Once the bits are split out into bytes of UTF-8, this is a mask OR-ed
  into the first byte, depending on how many bytes follow.  There are
  as many entries in this table as there are UTF-8 sequence types.
  (I.e., one byte sequence, two byte... six byte sequence.)
}
  firstByteMark: array [0..6] of UTF8 = ($00, $00, $0c0, $0e0, $0f0, $0f8, $0fc);

function ConvertUTF16toUTF8 ( var source: widestring; sourcelen: cardinal;
                              var target: string; targetlen: cardinal;
                              flags: ConversionFlags): ConversionResult;

function ConvertUTF8toUTF16 ( var source: string; sourcelen: cardinal;
                              var target: widestring; targetlen: cardinal;
                              flags: ConversionFlags): ConversionResult;

function isLegalUTF8Sequence(source: string; sourcelen: cardinal): boolean;
{$endif}

implementation

uses
{$ifdef CLX_USED}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef LINUX}
  Libc,
{$endif}
{$else}
  Windows,
  Consts,
{$endif}
  SysUtils,
{$ifdef VCL_6_USED}
  SysConst,
{$endif}
  Math;

type
  PArr = ^TByteArr;
  TByteArr = array[0..$7FFFFFFE] of byte;

const
  CFT_COMMON=1;
  CFT_SIMPLE=2;
  CFT_FULL=4;
  CFT_SPECIAL=8;
  max_trans=814;
  ms_unicode:boolean=false;

procedure WStrError;
begin
  OutOfMemoryError;
end;

function IntToStrFmt(value: integer): string;
var
  S: string;
  sl,
    i,
    j,
    k: integer;
  b: boolean;
begin
  S := IntToStr(Value);
  sl := Length(S);
  if S[1] in ['-', '+'] then
  begin
    dec(sl);
    if sl > 3 then
    begin
      k := sl + sl div 3 + 1;
      b := SL mod 3 = 0;
      if b then
        dec(k);
    end
    else
      k := sl + 1;
    inc(sl);
    SetLength(Result, k);
    Result[1] := S[1];
    i := 2;
  end
  else
  begin
    if sl > 3 then
    begin
      k := sl + sl div 3;
      b := SL mod 3 = 0;
      if b then
        dec(k);
    end
    else
      k := sl;
    SetLength(Result, k);
    i := 1;
  end;
  k := i;
  for j := i to sl do
  begin
    if (sl > 3 + (i - 1)) and
      ((sl - j + 1) mod 3 = 0) and (j > i) then
    begin
      result[k] := ThousandSeparator;
      inc(k);
    end;
    result[k] := S[j];
    inc(k);
  end;
end;

function FloatToStrFmt(value: extended; decims: integer): string;
begin
  result := IntToStrFmt(Trunc(Value)) + DecimalSeparator + IntToStr(Round(Frac(Value) * Power(10, decims)));
end;

function IntToStrPad(value: integer; MinSize: integer): string;
begin
  result := IntToStr(Value);
  while Length(result) < MinSize do
    Result := '0' + result;
end;

function CenterStr(Str : string; Len : integer) : string;
var
  S : string;
  L : integer;
  ls : integer;
begin
  s := '';
  result := '';
  if len < Length(str) then Exit;
  l := (Len - Length(Str)) div 2;
  while l > 0 do
  begin
    s := s + ' ';
    dec(l);
  end;
  ls := Length(Str);
  for l := 1 to ls do
    s := s + str[L];
  CenterStr := s;
end;

{$ifndef KYLIX_USED}
function OEMToStr(S: string): string;
begin
  SetLength(Result, Length(S));
  if Length(S) > 0 then OEMToChar(PChar(S), PChar(result));
end;

function StrToOEM(S: string): string;
begin
  SetLength(Result, Length(S));
  if Length(S) > 0 then CharToOEM(PChar(S), PChar(result));
end;
{$endif}

{$ifndef KYLIX_USED}
function MessageRes(Txt: Integer; Title: PChar; TextType: Word; Sounds: boolean): Integer;
begin
  if Sounds then
    MessageBeep(TextType and
      (mb_IconAsterisk or mb_IconExclamation or mb_IconHand or mb_IconQuestion or mb_Ok));
  result := MessageBox(0, PChar(LoadStr(TXT)), Title, TextType);
end;
{$endif}

function replace;
var
  i: integer;
begin
  i := pos(SourceString, Str);
  if i = 0 then
  begin
    result := false;
    exit;
  end;
  Delete(Str, i, Length(SourceString));
  Insert(DestString, Str, i);
  result := true;
end;

function extractword(str: string; n: integer): string;
var
  count: integer;
  i: integer;
  len: integer;
  done: boolean;
  retstr: string;

begin
  retstr := '';
  len := length(str);
  count := 0;
  i := 1;
  done := false;
  while (i <= len) and (not done) do
  begin
    while ((i <= len) and ((str[i] = #32) or (str[i] = #9) or (str[i] = ';'))) do
      inc(i);
    if i <= len then
      inc(count);
    if count = n then
    begin
      setstring(retstr, nil, 0);
      if (i > 1) then
        if str[i - 1] = ';' then
          retstr := ';';
      while ((i <= len) and ((str[i] <> #32) and (str[i] <> #9) and (str[i] <> ';'))) do
      begin
        setlength(retstr, length(retstr) + 1);
        retstr[ord(length(retstr))] := str[i];
        inc(i);
      end;
      done := true;
    end
    else
      while ((i <= len) and ((str[i] <> #32) and (str[i] <> #9) and (str[i] <> ';'))) do
        inc(i);
  end;
  result := retstr;
end;

function FstNonSpace(str: string): integer;
var //P : PChar;
  i: integer;

begin
  i := 1;
  while (i < Length(Str)) and ((str[i] = #32) or (str[i] = #9)) do
    inc(i);
  if i = Length(Str) then
    result := -1
  else
    result := i;
end;

function NextWordBegin(str: string; CurrentPos: integer): integer;
var
  i: integer;
begin
  i := CurrentPos;
  while (i < Length(str)) and ((Str[i] = #32) or (Str[i] = #9)) do
    inc(i);
  if i = Length(str) then
    result := -1
  else
    result := i;
end;

function LastPos(SubStr: string; Strn: string): integer;
var
  i,
    j: integer;
  ls, // total length of substring
    ld // length of string
    : integer;

begin
  result := 0;
  ls := Length(SubStr);
  ld := Length(Strn);
  if (ls > ld) or (ls = 0) or (ld = 0) then exit;
  for i := ld downto ls do
  begin
    j := ls;
    while j >= 1 do
    begin
      if Strn[i - ls + j] <> SubStr[j] then break;
      dec(j);
    end;
    if j = 0 then
    begin
      result := i - ls + 1;
      exit;
    end;
  end;
end;

function LineIsEmpty(str: string): boolean;
begin
  result := FstNonSpace(str) = -1;
end;

function CompleteLine(Str: string; FLen: integer; symb: char): string;
var
  S: string;
  i, j: integer;
begin
  s := Str;
  if Length(s) >= FLen then
  begin
    result := s;
    Exit;
  end;
  j := Length(S);
  SetLength(S, FLen);
  for i := j to FLen do
  begin
    S[i] := symb;
  end;
  result := s;
end;

function PrefixLine(Str: string; FLen: integer; symb: char): string;
begin
  if FLen = 0 then
  begin
    result := Str;
    exit;
  end;
  SetLength(Result, FLen);
  {$IFNDEF KYLIX_USED}
  FillMemory(PChar(result), FLen, byte(symb));
  {$ELSE}
  FillChar(Result, FLen, byte(symb));
  {$ENDIF}
  result := Result + Str;
end;

function MakeString(FLen: Integer; Seq : string): string;
var i : integer;
begin
  if Length(Seq) = 1 then
    result := StringOfChar(Seq[1], FLen)
  else
  begin
    SetLength(Result, FLen * Length(Seq));
    for i := 0 to FLen -1 do
      Move(Seq[1], Result[FLen * i + 1], Length(Seq));
  end;
end;

function H2D(S: string): integer;
var
  i: integer;
  n: boolean;
  c: char;
begin
  result := 0;
  if Pos('$', S) = 1 then
    Delete(S, 1, 1);
  S := Trim(S);
  if Length(S) = 0 then
{$IFNDEF VER90}
    raise EConvertError.CreateFmt(SInvalidInteger, [S]);
{$ELSE}
    raise EConvertError.CreateResFmt(SInvalidInteger, [S]);
{$ENDIF}
  n := false;
  if S[1] = '-' then n := true;
  if (S[1] = '-') or (S[1] = '+') then
    Delete(S, 1, 1);
  if Length(S) = 0 then
{$IFNDEF VER90}
    raise EConvertError.CreateFmt(SInvalidInteger, [S]);
{$ELSE}
    raise EConvertError.CreateResFmt(SInvalidInteger, [S]);
{$ENDIF}

  for i := 1 to Length(S) do
  begin
    c := S[i];
    if c in ['0'..'9'] then
      result := result shl 4 + Ord(c) - Ord('0')
    else
    begin
      c := UpCase(c);
      if c in ['A'..'F'] then
        result := result shl 4 + Ord(c) - Ord('A') + 10
      else
{$IFNDEF VER90}
        raise EConvertError.CreateFmt(SInvalidInteger, [S]);
{$ELSE}
        raise EConvertError.CreateResFmt(SInvalidInteger, [S]);
{$ENDIF}
    end;
  end;
  if n then result := -result;
end;

function H2DDef(const S: string; Def: integer): integer;
begin
  try
    result := H2D(S);
  except
    result := Def;
  end;
end;

function Bin2Int(S: string): integer;
var
  i, l: integer;
begin
  l := length(S);
  result := 0;
  for i := 1 to l do
  begin
    result := (result shl 1);
    if s[i] = '1' then
      result := result or 1 {and ($ffffffff)}
    else
{$IFNDEF VER90}
      if s[i] <> '0' then
        raise EConvertError.CreateFmt(SInvalidInteger, [S]);
{$ELSE}
      if s[i] <> '0' then
        raise EConvertError.CreateResFmt(SInvalidInteger, [S]);
{$ENDIF}
  end;
end;

function Bin2IntDef(S: string; Default: integer): integer;
begin
  try
    result := Bin2Int(S);
  except
    result := Default;
  end;
end;

function Data2Str(Buffer: pointer; BufLen: integer): string;
var
  p: PArr;
  i: integer;
begin
  P := PArr(Buffer);
  result := '454C ' + IntToHex(BufLen, 1) + ' ';
  for i := 0 to BufLen - 1 do
    result := result + IntToHex(P[i], 2) + ' ';
end;

function Str2Data(S: string; var Buffer: pointer; var BufLen: integer): boolean;
var
  p: PArr;
  i, j: integer;
  S1: string;
  b: Boolean;
begin
  result := false;
  BufLen := -1;
  Buffer := nil;
  if pos('454C', S) <> 1 then exit;
  Delete(S, 1, 5);
  S1 := ExtractStr(S, 1, Pos(' ', S) - 1);
  Delete(S, 1, 1);
  if S1 = '' then exit;
{$ifdef KYLIX_USED}
  j := 0;
{$endif}
  try
    j := H2D(S1);
  except
    exit;
  end;
  GetMem(Buffer, j);
  BufLen := j;
  p := PArr(Buffer);
  try
    for i := 0 to J - 1 do
    begin
      if Pos(#32, S) > 0 then
      begin
        S1 := ExtractStr(S, 1, Pos(#32, S) - 1);
        delete(S, 1, 1);
        b := False;
      end
      else
      begin
        S1 := S;
        b := true;
      end;
      P[i] := H2D(S1);
      if b and (i < j - 1) then break;
    end;
    result := i >= j;
  except
    result := False;
  end;
end;

function IsDigit(ch: char): boolean;
begin
  result := (ch >= '0') and (ch <= '9');
end;

function IsDigitStr(const S: string): boolean;
var
  i, j: integer;
begin
  j := length(S);
  result := false;
  for i := 1 to j do
  begin
    if (S[i] < '0') or (S[i] > '9') then exit;
  end;
  result := true;
end;

function IsAlpha(ch: char): boolean;
begin
  result := ch in ['_', 'A'..'Z', 'a'..'z'];
end;

function IsAlphaOrDigit(ch: char): boolean;
begin
  Result := ch in ['0'..'9', '_', 'A'..'Z', 'a'..'z'];
end;

function IsAlphaStr(const S: string): boolean;
var
  i, j: integer;
begin
  j := length(S);
  result := false;
  for i := 1 to j do
    if not IsAlpha(S[i]) then exit;
  result := true;
end;

function IsIdentStr(const S: string): boolean;
var
  i, j: integer;
begin
  j := length(S);
  result := false;
  if (J = 0) or (not (IsAlpha(s[1]))) then exit;
  for i := 2 to j do
    if not IsAlphaOrDigit(S[i]) then exit;

  result := true;
end;

function ExtractStr(var S: string; SPos, SLen: integer): string;
begin
  result := Copy(S, SPos, SLen);
  Delete(S, SPos, SLen);
end;

function LeftBreak(S: string; Pos: integer): integer;
var
  i: integer;
  b: boolean;
begin
  result := -1;
  if (Pos = 0) or (Pos > Length(S)) then exit;
  b := (S[Pos] = ' ') or (S[Pos] = #9);
  i := Pos;
  while i > 0 do
  begin
    if ((S[i] = ' ') or (S[i] = ' ')) xor b then
    begin
      result := i;
      break;
    end;
    inc(i);
  end;
end;

function EscapeURLString(aString: string; EscapeChar: Char): string;
var S : string;
    i, j : integer;
begin
  setLength(s, 256 - (ord('9') - ord('0') + 1) - (ord('Z') - ord('A') + 1) - (ord('z') - ord('a') + 1));
  j := 1;
  for i := 0 to 255 do
  begin
    if not (Chr(i) in ['0'..'9', 'a'..'z', 'A'..'Z']) then
    begin
      s[j] := Chr(i);
      inc(j);
    end;
  end;
  result := EscapeString(aString, s, EscapeChar);
end;

function EscapeString(aString: string; UnsafeChars: string; EscapeChar: Char): string;
var
  CurSrc, TotSrc: integer;
  CurChar: Char;
  i: integer;
begin
  if UnsafeChars = '' then // if the list is not given, we escape terminal charset
  begin
    SetLength(UnsafeChars, 31 + 128);
    for i := 0 to 31 do
      UnsafeChars[i + 1] := Char(i);
    for i := 128 to 255 do
      UnsafeChars[33 + i - 128] := Char(i);
  end;
  UnsafeChars := UnsafeChars + EscapeChar;
  result := '';
  TotSrc := Length(aString);
  CurSrc := 1;
  while CurSrc <= TotSrc do
  begin
    CurChar := aString[CurSrc];
    if Pos(CurChar, UnsafeChars) > 0 then
      result := result + EscapeChar + IntToHex(Ord(CurChar), 2)
    else
      result := result + CurChar;
    inc(CurSrc);
  end;
end;

function UnEscapeString(aString: string; EscapeChar: Char): string;
var
  CurSrc, TotSrc: integer;
  CurChar: Char;
begin
  result := '';
  TotSrc := Length(aString);
  CurSrc := 1;
  while CurSrc <= TotSrc do
  begin
    CurChar := aString[CurSrc];
    if CurChar = EscapeChar then
    begin
      if CurSrc + 2 <= TotSrc then
      begin
        result := result + Char(h2d(aString[CurSrc + 1] + aString[CurSrc + 2]));
        inc(CurSrc, 2);
      end
      else
        break;
    end
    else
      result := result + CurChar;
    inc(CurSrc);
  end;
end;

function StrStartsWith(Source: PChar; Seq: PChar): Boolean;
begin
  if Assigned(Seq) then
  begin
    Result := False;
    if not Assigned(Source) then Exit;
    while Seq^ <> #0 do
    begin
      if Source^ <> Seq^ then Exit;
      Inc(Source);
      Inc(Seq);
    end;
  end;
  Result := True;
end;

function ContainsAt(Source: string; Index: Integer; Seq: string): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(Source))
    and StrStartsWith(PChar(Source) + Index - 1, PChar(Seq));
end;

function FileNameLike(FileName : string; Mask : string) : boolean;
var sp, p1,
    p2 : PChar;
    IsExt  : boolean;
begin
  result := true;
  if Mask = '*' then
    exit;

  if Pos('.', FileName) = 0 then
    FileName := FileName + '.'; 

  p1 := PChar(FileName);
  p2 := PChar(Mask);

  IsExt := false;

  while true do
  begin
    if P2^ = '*' then
    begin
      if IsExt then
        exit
      else
      begin
        sp := nil;
        while P2^ <> #0 do
        begin
          inc(P2);
          if P2^ = '.' then
          begin
            sp := P2;
          end
          else
          if P2^ = #0 then
          begin
            if sp <> nil then
            begin
              P2 := sp;
              inc(P2);
            end;
            break;
          end;
        end;
        sp := nil;
        while P1^ <> #0 do
        begin
          inc(P1);
          if P1^ = '.' then
          begin
            sp := P1;
          end
          else
          if P1^ = #0 then
          begin
            if sp <> nil then
            begin
              P1 := sp;
              inc(P1);
            end;
            break;
          end;
        end;
        IsExt := true;
      end;
    end
    else
    if P2^ = '?' then
    begin
      inc(P1);
      inc(P2);
    end
    else
    if Upcase(P1^) <> Upcase(P2^) then
    begin
      if (P1^ = #0) and (P2^ = '.') then
      begin
        inc(P2);
        if (P2^ = '*') or (P2^ = #0) then
          result := true
        else
          result := false;
        exit;
      end
      else
      begin
        result := false;
        exit;
      end;
    end
    else
    if P1^ = #0 then
      break
    else
    begin
      inc(P1);
      inc(P2);
    end;
  end;
end;

function AnsiSameText(const S1, S2: string): Boolean;
begin
  {$IFNDEF KYLIX_USED}
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) = 2;
  {$ELSE}
  Result := CompareText(S1, S2) = 0;
  {$ENDIF}
end;

function CurrToPrettyStr(const Value: Currency): string;
begin
  Result := CurrToStrF(Value, ffCurrency, CurrencyDecimals);
end;

function PrettyStrToCurr(const Value: string): Currency;
const
  OneStepMultiplicator = 0.1;
var
  I: Cardinal;
  CurrNegFormatted: Boolean;
  Digit: Currency;
  FractPartMultiplicator: Currency;
  Ch: Char;
  CurrSymbolPosition: Cardinal;
  Len: Cardinal;
  DigitsFound: Boolean;
begin
  Result := 0.0;
  if Value <> '' then
  begin
    CurrNegFormatted := False;
    DigitsFound := False;
    FractPartMultiplicator := 0.0;
    if CurrencyString <> '' then
    begin
      CurrSymbolPosition := Pos(CurrencyString, Value);
    end
    else
    begin
      CurrSymbolPosition := 0;
    end;
    I := 1;
    Len := Length(Value);
    while I <= Len do
    begin
      if I = CurrSymbolPosition then
      begin
        // skip currency symbol(s)
        if DigitsFound and CurrNegFormatted then Break;
        Inc(I, Length(CurrencyString));
        Continue;
      end;
      Ch := Value[I];
      case Ch of
        '0'..'9':
          begin
          // next digit found
            DigitsFound := True;
            Digit := Cardinal(Ord(Ch) - Ord('0'));
            if FractPartMultiplicator > 0.0 then
            begin
              Digit := Digit * FractPartMultiplicator;
              FractPartMultiplicator := FractPartMultiplicator *
                OneStepMultiplicator;
              Result := Result + Digit;
            end
            else
            begin
              Result := (Result * 10) + Digit;
            end;
          end;
        '-', '(':
          begin
            CurrNegFormatted := True;
            if DigitsFound then Break;
          end;
        '.', ',':
          begin
            DigitsFound := True;
            if FractPartMultiplicator > 0.0 then
            begin
              Break;
            end;
            FractPartMultiplicator := OneStepMultiplicator;
          end;
      else
        begin
          if Ch <> ThousandSeparator then
          begin
            if Ch = DecimalSeparator then
            begin
              DigitsFound := True;
              if FractPartMultiplicator > 0.0 then
              begin
                Break;
              end;
              FractPartMultiplicator := OneStepMultiplicator;
            end
            else
            begin
              if Ch > #32 then
              begin
                Break;
              end;
            end;
          end;
        end;
      end;
      Inc(I);
    end;
    if CurrNegFormatted then
    begin
      Result := -Result;
    end;
  end;
end;

function CurrSign(const Value: Currency): Integer;
begin
  if Value <> 0.0 then
  begin
    if Value > 0.0 then
    begin
      Result := +1;
    end
    else
    begin
      Result := -1;
    end;
  end
  else
  begin
    Result := 0;
  end;
end;

function StringDup(S : String) : PChar;
begin
  GetMem(Result, Length(S) + 1);
  StrPCopy(Result, S);
end;

{$ifndef BROKEN_UNICODE}

type
  w3arr=array[1..3] of widechar;
  tr=record
    fl:byte;
    lc:widechar;
    uc:w3arr;
  end;

const
  rcc:array[1..max_trans] of tr=(
    (fl:CFT_COMMON;lc:#$0041;uc:(#$0061,#0,#0)),
    (fl:CFT_COMMON;lc:#$0042;uc:(#$0062,#0,#0)),
    (fl:CFT_COMMON;lc:#$0043;uc:(#$0063,#0,#0)),
    (fl:CFT_COMMON;lc:#$0044;uc:(#$0064,#0,#0)),
    (fl:CFT_COMMON;lc:#$0045;uc:(#$0065,#0,#0)),
    (fl:CFT_COMMON;lc:#$0046;uc:(#$0066,#0,#0)),
    (fl:CFT_COMMON;lc:#$0047;uc:(#$0067,#0,#0)),
    (fl:CFT_COMMON;lc:#$0048;uc:(#$0068,#0,#0)),
    (fl:CFT_COMMON;lc:#$0049;uc:(#$0069,#0,#0)),
    (fl:CFT_COMMON;lc:#$004A;uc:(#$006A,#0,#0)),
    (fl:CFT_COMMON;lc:#$004B;uc:(#$006B,#0,#0)),
    (fl:CFT_COMMON;lc:#$004C;uc:(#$006C,#0,#0)),
    (fl:CFT_COMMON;lc:#$004D;uc:(#$006D,#0,#0)),
    (fl:CFT_COMMON;lc:#$004E;uc:(#$006E,#0,#0)),
    (fl:CFT_COMMON;lc:#$004F;uc:(#$006F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0050;uc:(#$0070,#0,#0)),
    (fl:CFT_COMMON;lc:#$0051;uc:(#$0071,#0,#0)),
    (fl:CFT_COMMON;lc:#$0052;uc:(#$0072,#0,#0)),
    (fl:CFT_COMMON;lc:#$0053;uc:(#$0073,#0,#0)),
    (fl:CFT_COMMON;lc:#$0054;uc:(#$0074,#0,#0)),
    (fl:CFT_COMMON;lc:#$0055;uc:(#$0075,#0,#0)),
    (fl:CFT_COMMON;lc:#$0056;uc:(#$0076,#0,#0)),
    (fl:CFT_COMMON;lc:#$0057;uc:(#$0077,#0,#0)),
    (fl:CFT_COMMON;lc:#$0058;uc:(#$0078,#0,#0)),
    (fl:CFT_COMMON;lc:#$0059;uc:(#$0079,#0,#0)),
    (fl:CFT_COMMON;lc:#$005A;uc:(#$007A,#0,#0)),
    (fl:CFT_COMMON;lc:#$00B5;uc:(#$03BC,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C0;uc:(#$00E0,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C1;uc:(#$00E1,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C2;uc:(#$00E2,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C3;uc:(#$00E3,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C4;uc:(#$00E4,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C5;uc:(#$00E5,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C6;uc:(#$00E6,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C7;uc:(#$00E7,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C8;uc:(#$00E8,#0,#0)),
    (fl:CFT_COMMON;lc:#$00C9;uc:(#$00E9,#0,#0)),
    (fl:CFT_COMMON;lc:#$00CA;uc:(#$00EA,#0,#0)),
    (fl:CFT_COMMON;lc:#$00CB;uc:(#$00EB,#0,#0)),
    (fl:CFT_COMMON;lc:#$00CC;uc:(#$00EC,#0,#0)),
    (fl:CFT_COMMON;lc:#$00CD;uc:(#$00ED,#0,#0)),
    (fl:CFT_COMMON;lc:#$00CE;uc:(#$00EE,#0,#0)),
    (fl:CFT_COMMON;lc:#$00CF;uc:(#$00EF,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D0;uc:(#$00F0,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D1;uc:(#$00F1,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D2;uc:(#$00F2,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D3;uc:(#$00F3,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D4;uc:(#$00F4,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D5;uc:(#$00F5,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D6;uc:(#$00F6,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D8;uc:(#$00F8,#0,#0)),
    (fl:CFT_COMMON;lc:#$00D9;uc:(#$00F9,#0,#0)),
    (fl:CFT_COMMON;lc:#$00DA;uc:(#$00FA,#0,#0)),
    (fl:CFT_COMMON;lc:#$00DB;uc:(#$00FB,#0,#0)),
    (fl:CFT_COMMON;lc:#$00DC;uc:(#$00FC,#0,#0)),
    (fl:CFT_COMMON;lc:#$00DD;uc:(#$00FD,#0,#0)),
    (fl:CFT_COMMON;lc:#$00DE;uc:(#$00FE,#0,#0)),
    (fl:CFT_FULL;lc:#$00DF;uc:(#$0073,#$0073,#0)),
    (fl:CFT_COMMON;lc:#$0100;uc:(#$0101,#0,#0)),
    (fl:CFT_COMMON;lc:#$0102;uc:(#$0103,#0,#0)),
    (fl:CFT_COMMON;lc:#$0104;uc:(#$0105,#0,#0)),
    (fl:CFT_COMMON;lc:#$0106;uc:(#$0107,#0,#0)),
    (fl:CFT_COMMON;lc:#$0108;uc:(#$0109,#0,#0)),
    (fl:CFT_COMMON;lc:#$010A;uc:(#$010B,#0,#0)),
    (fl:CFT_COMMON;lc:#$010C;uc:(#$010D,#0,#0)),
    (fl:CFT_COMMON;lc:#$010E;uc:(#$010F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0110;uc:(#$0111,#0,#0)),
    (fl:CFT_COMMON;lc:#$0112;uc:(#$0113,#0,#0)),
    (fl:CFT_COMMON;lc:#$0114;uc:(#$0115,#0,#0)),
    (fl:CFT_COMMON;lc:#$0116;uc:(#$0117,#0,#0)),
    (fl:CFT_COMMON;lc:#$0118;uc:(#$0119,#0,#0)),
    (fl:CFT_COMMON;lc:#$011A;uc:(#$011B,#0,#0)),
    (fl:CFT_COMMON;lc:#$011C;uc:(#$011D,#0,#0)),
    (fl:CFT_COMMON;lc:#$011E;uc:(#$011F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0120;uc:(#$0121,#0,#0)),
    (fl:CFT_COMMON;lc:#$0122;uc:(#$0123,#0,#0)),
    (fl:CFT_COMMON;lc:#$0124;uc:(#$0125,#0,#0)),
    (fl:CFT_COMMON;lc:#$0126;uc:(#$0127,#0,#0)),
    (fl:CFT_COMMON;lc:#$0128;uc:(#$0129,#0,#0)),
    (fl:CFT_COMMON;lc:#$012A;uc:(#$012B,#0,#0)),
    (fl:CFT_COMMON;lc:#$012C;uc:(#$012D,#0,#0)),
    (fl:CFT_COMMON;lc:#$012E;uc:(#$012F,#0,#0)),
    (fl:CFT_SPECIAL;lc:#$0130;uc:(#$0069,#0,#0)),
    (fl:CFT_SPECIAL;lc:#$0131;uc:(#$0069,#0,#0)),
    (fl:CFT_COMMON;lc:#$0132;uc:(#$0133,#0,#0)),
    (fl:CFT_COMMON;lc:#$0134;uc:(#$0135,#0,#0)),
    (fl:CFT_COMMON;lc:#$0136;uc:(#$0137,#0,#0)),
    (fl:CFT_COMMON;lc:#$0139;uc:(#$013A,#0,#0)),
    (fl:CFT_COMMON;lc:#$013B;uc:(#$013C,#0,#0)),
    (fl:CFT_COMMON;lc:#$013D;uc:(#$013E,#0,#0)),
    (fl:CFT_COMMON;lc:#$013F;uc:(#$0140,#0,#0)),
    (fl:CFT_COMMON;lc:#$0141;uc:(#$0142,#0,#0)),
    (fl:CFT_COMMON;lc:#$0143;uc:(#$0144,#0,#0)),
    (fl:CFT_COMMON;lc:#$0145;uc:(#$0146,#0,#0)),
    (fl:CFT_COMMON;lc:#$0147;uc:(#$0148,#0,#0)),
    (fl:CFT_FULL;lc:#$0149;uc:(#$02BC,#$006E,#0)),
    (fl:CFT_COMMON;lc:#$014A;uc:(#$014B,#0,#0)),
    (fl:CFT_COMMON;lc:#$014C;uc:(#$014D,#0,#0)),
    (fl:CFT_COMMON;lc:#$014E;uc:(#$014F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0150;uc:(#$0151,#0,#0)),
    (fl:CFT_COMMON;lc:#$0152;uc:(#$0153,#0,#0)),
    (fl:CFT_COMMON;lc:#$0154;uc:(#$0155,#0,#0)),
    (fl:CFT_COMMON;lc:#$0156;uc:(#$0157,#0,#0)),
    (fl:CFT_COMMON;lc:#$0158;uc:(#$0159,#0,#0)),
    (fl:CFT_COMMON;lc:#$015A;uc:(#$015B,#0,#0)),
    (fl:CFT_COMMON;lc:#$015C;uc:(#$015D,#0,#0)),
    (fl:CFT_COMMON;lc:#$015E;uc:(#$015F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0160;uc:(#$0161,#0,#0)),
    (fl:CFT_COMMON;lc:#$0162;uc:(#$0163,#0,#0)),
    (fl:CFT_COMMON;lc:#$0164;uc:(#$0165,#0,#0)),
    (fl:CFT_COMMON;lc:#$0166;uc:(#$0167,#0,#0)),
    (fl:CFT_COMMON;lc:#$0168;uc:(#$0169,#0,#0)),
    (fl:CFT_COMMON;lc:#$016A;uc:(#$016B,#0,#0)),
    (fl:CFT_COMMON;lc:#$016C;uc:(#$016D,#0,#0)),
    (fl:CFT_COMMON;lc:#$016E;uc:(#$016F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0170;uc:(#$0171,#0,#0)),
    (fl:CFT_COMMON;lc:#$0172;uc:(#$0173,#0,#0)),
    (fl:CFT_COMMON;lc:#$0174;uc:(#$0175,#0,#0)),
    (fl:CFT_COMMON;lc:#$0176;uc:(#$0177,#0,#0)),
    (fl:CFT_COMMON;lc:#$0178;uc:(#$00FF,#0,#0)),
    (fl:CFT_COMMON;lc:#$0179;uc:(#$017A,#0,#0)),
    (fl:CFT_COMMON;lc:#$017B;uc:(#$017C,#0,#0)),
    (fl:CFT_COMMON;lc:#$017D;uc:(#$017E,#0,#0)),
    (fl:CFT_COMMON;lc:#$017F;uc:(#$0073,#0,#0)),
    (fl:CFT_COMMON;lc:#$0181;uc:(#$0253,#0,#0)),
    (fl:CFT_COMMON;lc:#$0182;uc:(#$0183,#0,#0)),
    (fl:CFT_COMMON;lc:#$0184;uc:(#$0185,#0,#0)),
    (fl:CFT_COMMON;lc:#$0186;uc:(#$0254,#0,#0)),
    (fl:CFT_COMMON;lc:#$0187;uc:(#$0188,#0,#0)),
    (fl:CFT_COMMON;lc:#$0189;uc:(#$0256,#0,#0)),
    (fl:CFT_COMMON;lc:#$018A;uc:(#$0257,#0,#0)),
    (fl:CFT_COMMON;lc:#$018B;uc:(#$018C,#0,#0)),
    (fl:CFT_COMMON;lc:#$018E;uc:(#$01DD,#0,#0)),
    (fl:CFT_COMMON;lc:#$018F;uc:(#$0259,#0,#0)),
    (fl:CFT_COMMON;lc:#$0190;uc:(#$025B,#0,#0)),
    (fl:CFT_COMMON;lc:#$0191;uc:(#$0192,#0,#0)),
    (fl:CFT_COMMON;lc:#$0193;uc:(#$0260,#0,#0)),
    (fl:CFT_COMMON;lc:#$0194;uc:(#$0263,#0,#0)),
    (fl:CFT_COMMON;lc:#$0196;uc:(#$0269,#0,#0)),
    (fl:CFT_COMMON;lc:#$0197;uc:(#$0268,#0,#0)),
    (fl:CFT_COMMON;lc:#$0198;uc:(#$0199,#0,#0)),
    (fl:CFT_COMMON;lc:#$019C;uc:(#$026F,#0,#0)),
    (fl:CFT_COMMON;lc:#$019D;uc:(#$0272,#0,#0)),
    (fl:CFT_COMMON;lc:#$019F;uc:(#$0275,#0,#0)),
    (fl:CFT_COMMON;lc:#$01A0;uc:(#$01A1,#0,#0)),
    (fl:CFT_COMMON;lc:#$01A2;uc:(#$01A3,#0,#0)),
    (fl:CFT_COMMON;lc:#$01A4;uc:(#$01A5,#0,#0)),
    (fl:CFT_COMMON;lc:#$01A6;uc:(#$0280,#0,#0)),
    (fl:CFT_COMMON;lc:#$01A7;uc:(#$01A8,#0,#0)),
    (fl:CFT_COMMON;lc:#$01A9;uc:(#$0283,#0,#0)),
    (fl:CFT_COMMON;lc:#$01AC;uc:(#$01AD,#0,#0)),
    (fl:CFT_COMMON;lc:#$01AE;uc:(#$0288,#0,#0)),
    (fl:CFT_COMMON;lc:#$01AF;uc:(#$01B0,#0,#0)),
    (fl:CFT_COMMON;lc:#$01B1;uc:(#$028A,#0,#0)),
    (fl:CFT_COMMON;lc:#$01B2;uc:(#$028B,#0,#0)),
    (fl:CFT_COMMON;lc:#$01B3;uc:(#$01B4,#0,#0)),
    (fl:CFT_COMMON;lc:#$01B5;uc:(#$01B6,#0,#0)),
    (fl:CFT_COMMON;lc:#$01B7;uc:(#$0292,#0,#0)),
    (fl:CFT_COMMON;lc:#$01B8;uc:(#$01B9,#0,#0)),
    (fl:CFT_COMMON;lc:#$01BC;uc:(#$01BD,#0,#0)),
    (fl:CFT_COMMON;lc:#$01C4;uc:(#$01C6,#0,#0)),
    (fl:CFT_COMMON;lc:#$01C5;uc:(#$01C6,#0,#0)),
    (fl:CFT_COMMON;lc:#$01C7;uc:(#$01C9,#0,#0)),
    (fl:CFT_COMMON;lc:#$01C8;uc:(#$01C9,#0,#0)),
    (fl:CFT_COMMON;lc:#$01CA;uc:(#$01CC,#0,#0)),
    (fl:CFT_COMMON;lc:#$01CB;uc:(#$01CC,#0,#0)),
    (fl:CFT_COMMON;lc:#$01CD;uc:(#$01CE,#0,#0)),
    (fl:CFT_COMMON;lc:#$01CF;uc:(#$01D0,#0,#0)),
    (fl:CFT_COMMON;lc:#$01D1;uc:(#$01D2,#0,#0)),
    (fl:CFT_COMMON;lc:#$01D3;uc:(#$01D4,#0,#0)),
    (fl:CFT_COMMON;lc:#$01D5;uc:(#$01D6,#0,#0)),
    (fl:CFT_COMMON;lc:#$01D7;uc:(#$01D8,#0,#0)),
    (fl:CFT_COMMON;lc:#$01D9;uc:(#$01DA,#0,#0)),
    (fl:CFT_COMMON;lc:#$01DB;uc:(#$01DC,#0,#0)),
    (fl:CFT_COMMON;lc:#$01DE;uc:(#$01DF,#0,#0)),
    (fl:CFT_COMMON;lc:#$01E0;uc:(#$01E1,#0,#0)),
    (fl:CFT_COMMON;lc:#$01E2;uc:(#$01E3,#0,#0)),
    (fl:CFT_COMMON;lc:#$01E4;uc:(#$01E5,#0,#0)),
    (fl:CFT_COMMON;lc:#$01E6;uc:(#$01E7,#0,#0)),
    (fl:CFT_COMMON;lc:#$01E8;uc:(#$01E9,#0,#0)),
    (fl:CFT_COMMON;lc:#$01EA;uc:(#$01EB,#0,#0)),
    (fl:CFT_COMMON;lc:#$01EC;uc:(#$01ED,#0,#0)),
    (fl:CFT_COMMON;lc:#$01EE;uc:(#$01EF,#0,#0)),
    (fl:CFT_FULL;lc:#$01F0;uc:(#$006A,#$030C,#0)),
    (fl:CFT_COMMON;lc:#$01F1;uc:(#$01F3,#0,#0)),
    (fl:CFT_COMMON;lc:#$01F2;uc:(#$01F3,#0,#0)),
    (fl:CFT_COMMON;lc:#$01F4;uc:(#$01F5,#0,#0)),
    (fl:CFT_COMMON;lc:#$01F6;uc:(#$0195,#0,#0)),
    (fl:CFT_COMMON;lc:#$01F7;uc:(#$01BF,#0,#0)),
    (fl:CFT_COMMON;lc:#$01F8;uc:(#$01F9,#0,#0)),
    (fl:CFT_COMMON;lc:#$01FA;uc:(#$01FB,#0,#0)),
    (fl:CFT_COMMON;lc:#$01FC;uc:(#$01FD,#0,#0)),
    (fl:CFT_COMMON;lc:#$01FE;uc:(#$01FF,#0,#0)),
    (fl:CFT_COMMON;lc:#$0200;uc:(#$0201,#0,#0)),
    (fl:CFT_COMMON;lc:#$0202;uc:(#$0203,#0,#0)),
    (fl:CFT_COMMON;lc:#$0204;uc:(#$0205,#0,#0)),
    (fl:CFT_COMMON;lc:#$0206;uc:(#$0207,#0,#0)),
    (fl:CFT_COMMON;lc:#$0208;uc:(#$0209,#0,#0)),
    (fl:CFT_COMMON;lc:#$020A;uc:(#$020B,#0,#0)),
    (fl:CFT_COMMON;lc:#$020C;uc:(#$020D,#0,#0)),
    (fl:CFT_COMMON;lc:#$020E;uc:(#$020F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0210;uc:(#$0211,#0,#0)),
    (fl:CFT_COMMON;lc:#$0212;uc:(#$0213,#0,#0)),
    (fl:CFT_COMMON;lc:#$0214;uc:(#$0215,#0,#0)),
    (fl:CFT_COMMON;lc:#$0216;uc:(#$0217,#0,#0)),
    (fl:CFT_COMMON;lc:#$0218;uc:(#$0219,#0,#0)),
    (fl:CFT_COMMON;lc:#$021A;uc:(#$021B,#0,#0)),
    (fl:CFT_COMMON;lc:#$021C;uc:(#$021D,#0,#0)),
    (fl:CFT_COMMON;lc:#$021E;uc:(#$021F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0222;uc:(#$0223,#0,#0)),
    (fl:CFT_COMMON;lc:#$0224;uc:(#$0225,#0,#0)),
    (fl:CFT_COMMON;lc:#$0226;uc:(#$0227,#0,#0)),
    (fl:CFT_COMMON;lc:#$0228;uc:(#$0229,#0,#0)),
    (fl:CFT_COMMON;lc:#$022A;uc:(#$022B,#0,#0)),
    (fl:CFT_COMMON;lc:#$022C;uc:(#$022D,#0,#0)),
    (fl:CFT_COMMON;lc:#$022E;uc:(#$022F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0230;uc:(#$0231,#0,#0)),
    (fl:CFT_COMMON;lc:#$0232;uc:(#$0233,#0,#0)),
    (fl:CFT_COMMON;lc:#$0345;uc:(#$03B9,#0,#0)),
    (fl:CFT_COMMON;lc:#$0386;uc:(#$03AC,#0,#0)),
    (fl:CFT_COMMON;lc:#$0388;uc:(#$03AD,#0,#0)),
    (fl:CFT_COMMON;lc:#$0389;uc:(#$03AE,#0,#0)),
    (fl:CFT_COMMON;lc:#$038A;uc:(#$03AF,#0,#0)),
    (fl:CFT_COMMON;lc:#$038C;uc:(#$03CC,#0,#0)),
    (fl:CFT_COMMON;lc:#$038E;uc:(#$03CD,#0,#0)),
    (fl:CFT_COMMON;lc:#$038F;uc:(#$03CE,#0,#0)),
    (fl:CFT_FULL;lc:#$0390;uc:(#$03B9,#$0308,#$0301)),
    (fl:CFT_COMMON;lc:#$0391;uc:(#$03B1,#0,#0)),
    (fl:CFT_COMMON;lc:#$0392;uc:(#$03B2,#0,#0)),
    (fl:CFT_COMMON;lc:#$0393;uc:(#$03B3,#0,#0)),
    (fl:CFT_COMMON;lc:#$0394;uc:(#$03B4,#0,#0)),
    (fl:CFT_COMMON;lc:#$0395;uc:(#$03B5,#0,#0)),
    (fl:CFT_COMMON;lc:#$0396;uc:(#$03B6,#0,#0)),
    (fl:CFT_COMMON;lc:#$0397;uc:(#$03B7,#0,#0)),
    (fl:CFT_COMMON;lc:#$0398;uc:(#$03B8,#0,#0)),
    (fl:CFT_COMMON;lc:#$0399;uc:(#$03B9,#0,#0)),
    (fl:CFT_COMMON;lc:#$039A;uc:(#$03BA,#0,#0)),
    (fl:CFT_COMMON;lc:#$039B;uc:(#$03BB,#0,#0)),
    (fl:CFT_COMMON;lc:#$039C;uc:(#$03BC,#0,#0)),
    (fl:CFT_COMMON;lc:#$039D;uc:(#$03BD,#0,#0)),
    (fl:CFT_COMMON;lc:#$039E;uc:(#$03BE,#0,#0)),
    (fl:CFT_COMMON;lc:#$039F;uc:(#$03BF,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A0;uc:(#$03C0,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A1;uc:(#$03C1,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A3;uc:(#$03C3,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A4;uc:(#$03C4,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A5;uc:(#$03C5,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A6;uc:(#$03C6,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A7;uc:(#$03C7,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A8;uc:(#$03C8,#0,#0)),
    (fl:CFT_COMMON;lc:#$03A9;uc:(#$03C9,#0,#0)),
    (fl:CFT_COMMON;lc:#$03AA;uc:(#$03CA,#0,#0)),
    (fl:CFT_COMMON;lc:#$03AB;uc:(#$03CB,#0,#0)),
    (fl:CFT_FULL;lc:#$03B0;uc:(#$03C5,#$0308,#$0301)),
    (fl:CFT_COMMON;lc:#$03C2;uc:(#$03C3,#0,#0)),
    (fl:CFT_COMMON;lc:#$03D0;uc:(#$03B2,#0,#0)),
    (fl:CFT_COMMON;lc:#$03D1;uc:(#$03B8,#0,#0)),
    (fl:CFT_COMMON;lc:#$03D5;uc:(#$03C6,#0,#0)),
    (fl:CFT_COMMON;lc:#$03D6;uc:(#$03C0,#0,#0)),
    (fl:CFT_COMMON;lc:#$03DA;uc:(#$03DB,#0,#0)),
    (fl:CFT_COMMON;lc:#$03DC;uc:(#$03DD,#0,#0)),
    (fl:CFT_COMMON;lc:#$03DE;uc:(#$03DF,#0,#0)),
    (fl:CFT_COMMON;lc:#$03E0;uc:(#$03E1,#0,#0)),
    (fl:CFT_COMMON;lc:#$03E2;uc:(#$03E3,#0,#0)),
    (fl:CFT_COMMON;lc:#$03E4;uc:(#$03E5,#0,#0)),
    (fl:CFT_COMMON;lc:#$03E6;uc:(#$03E7,#0,#0)),
    (fl:CFT_COMMON;lc:#$03E8;uc:(#$03E9,#0,#0)),
    (fl:CFT_COMMON;lc:#$03EA;uc:(#$03EB,#0,#0)),
    (fl:CFT_COMMON;lc:#$03EC;uc:(#$03ED,#0,#0)),
    (fl:CFT_COMMON;lc:#$03EE;uc:(#$03EF,#0,#0)),
    (fl:CFT_COMMON;lc:#$03F0;uc:(#$03BA,#0,#0)),
    (fl:CFT_COMMON;lc:#$03F1;uc:(#$03C1,#0,#0)),
    (fl:CFT_COMMON;lc:#$03F2;uc:(#$03C3,#0,#0)),
    (fl:CFT_COMMON;lc:#$03F4;uc:(#$03B8,#0,#0)),
    (fl:CFT_COMMON;lc:#$03F5;uc:(#$03B5,#0,#0)),
    (fl:CFT_COMMON;lc:#$0400;uc:(#$0450,#0,#0)),
    (fl:CFT_COMMON;lc:#$0401;uc:(#$0451,#0,#0)),
    (fl:CFT_COMMON;lc:#$0402;uc:(#$0452,#0,#0)),
    (fl:CFT_COMMON;lc:#$0403;uc:(#$0453,#0,#0)),
    (fl:CFT_COMMON;lc:#$0404;uc:(#$0454,#0,#0)),
    (fl:CFT_COMMON;lc:#$0405;uc:(#$0455,#0,#0)),
    (fl:CFT_COMMON;lc:#$0406;uc:(#$0456,#0,#0)),
    (fl:CFT_COMMON;lc:#$0407;uc:(#$0457,#0,#0)),
    (fl:CFT_COMMON;lc:#$0408;uc:(#$0458,#0,#0)),
    (fl:CFT_COMMON;lc:#$0409;uc:(#$0459,#0,#0)),
    (fl:CFT_COMMON;lc:#$040A;uc:(#$045A,#0,#0)),
    (fl:CFT_COMMON;lc:#$040B;uc:(#$045B,#0,#0)),
    (fl:CFT_COMMON;lc:#$040C;uc:(#$045C,#0,#0)),
    (fl:CFT_COMMON;lc:#$040D;uc:(#$045D,#0,#0)),
    (fl:CFT_COMMON;lc:#$040E;uc:(#$045E,#0,#0)),
    (fl:CFT_COMMON;lc:#$040F;uc:(#$045F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0410;uc:(#$0430,#0,#0)),
    (fl:CFT_COMMON;lc:#$0411;uc:(#$0431,#0,#0)),
    (fl:CFT_COMMON;lc:#$0412;uc:(#$0432,#0,#0)),
    (fl:CFT_COMMON;lc:#$0413;uc:(#$0433,#0,#0)),
    (fl:CFT_COMMON;lc:#$0414;uc:(#$0434,#0,#0)),
    (fl:CFT_COMMON;lc:#$0415;uc:(#$0435,#0,#0)),
    (fl:CFT_COMMON;lc:#$0416;uc:(#$0436,#0,#0)),
    (fl:CFT_COMMON;lc:#$0417;uc:(#$0437,#0,#0)),
    (fl:CFT_COMMON;lc:#$0418;uc:(#$0438,#0,#0)),
    (fl:CFT_COMMON;lc:#$0419;uc:(#$0439,#0,#0)),
    (fl:CFT_COMMON;lc:#$041A;uc:(#$043A,#0,#0)),
    (fl:CFT_COMMON;lc:#$041B;uc:(#$043B,#0,#0)),
    (fl:CFT_COMMON;lc:#$041C;uc:(#$043C,#0,#0)),
    (fl:CFT_COMMON;lc:#$041D;uc:(#$043D,#0,#0)),
    (fl:CFT_COMMON;lc:#$041E;uc:(#$043E,#0,#0)),
    (fl:CFT_COMMON;lc:#$041F;uc:(#$043F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0420;uc:(#$0440,#0,#0)),
    (fl:CFT_COMMON;lc:#$0421;uc:(#$0441,#0,#0)),
    (fl:CFT_COMMON;lc:#$0422;uc:(#$0442,#0,#0)),
    (fl:CFT_COMMON;lc:#$0423;uc:(#$0443,#0,#0)),
    (fl:CFT_COMMON;lc:#$0424;uc:(#$0444,#0,#0)),
    (fl:CFT_COMMON;lc:#$0425;uc:(#$0445,#0,#0)),
    (fl:CFT_COMMON;lc:#$0426;uc:(#$0446,#0,#0)),
    (fl:CFT_COMMON;lc:#$0427;uc:(#$0447,#0,#0)),
    (fl:CFT_COMMON;lc:#$0428;uc:(#$0448,#0,#0)),
    (fl:CFT_COMMON;lc:#$0429;uc:(#$0449,#0,#0)),
    (fl:CFT_COMMON;lc:#$042A;uc:(#$044A,#0,#0)),
    (fl:CFT_COMMON;lc:#$042B;uc:(#$044B,#0,#0)),
    (fl:CFT_COMMON;lc:#$042C;uc:(#$044C,#0,#0)),
    (fl:CFT_COMMON;lc:#$042D;uc:(#$044D,#0,#0)),
    (fl:CFT_COMMON;lc:#$042E;uc:(#$044E,#0,#0)),
    (fl:CFT_COMMON;lc:#$042F;uc:(#$044F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0460;uc:(#$0461,#0,#0)),
    (fl:CFT_COMMON;lc:#$0462;uc:(#$0463,#0,#0)),
    (fl:CFT_COMMON;lc:#$0464;uc:(#$0465,#0,#0)),
    (fl:CFT_COMMON;lc:#$0466;uc:(#$0467,#0,#0)),
    (fl:CFT_COMMON;lc:#$0468;uc:(#$0469,#0,#0)),
    (fl:CFT_COMMON;lc:#$046A;uc:(#$046B,#0,#0)),
    (fl:CFT_COMMON;lc:#$046C;uc:(#$046D,#0,#0)),
    (fl:CFT_COMMON;lc:#$046E;uc:(#$046F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0470;uc:(#$0471,#0,#0)),
    (fl:CFT_COMMON;lc:#$0472;uc:(#$0473,#0,#0)),
    (fl:CFT_COMMON;lc:#$0474;uc:(#$0475,#0,#0)),
    (fl:CFT_COMMON;lc:#$0476;uc:(#$0477,#0,#0)),
    (fl:CFT_COMMON;lc:#$0478;uc:(#$0479,#0,#0)),
    (fl:CFT_COMMON;lc:#$047A;uc:(#$047B,#0,#0)),
    (fl:CFT_COMMON;lc:#$047C;uc:(#$047D,#0,#0)),
    (fl:CFT_COMMON;lc:#$047E;uc:(#$047F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0480;uc:(#$0481,#0,#0)),
    (fl:CFT_COMMON;lc:#$048C;uc:(#$048D,#0,#0)),
    (fl:CFT_COMMON;lc:#$048E;uc:(#$048F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0490;uc:(#$0491,#0,#0)),
    (fl:CFT_COMMON;lc:#$0492;uc:(#$0493,#0,#0)),
    (fl:CFT_COMMON;lc:#$0494;uc:(#$0495,#0,#0)),
    (fl:CFT_COMMON;lc:#$0496;uc:(#$0497,#0,#0)),
    (fl:CFT_COMMON;lc:#$0498;uc:(#$0499,#0,#0)),
    (fl:CFT_COMMON;lc:#$049A;uc:(#$049B,#0,#0)),
    (fl:CFT_COMMON;lc:#$049C;uc:(#$049D,#0,#0)),
    (fl:CFT_COMMON;lc:#$049E;uc:(#$049F,#0,#0)),
    (fl:CFT_COMMON;lc:#$04A0;uc:(#$04A1,#0,#0)),
    (fl:CFT_COMMON;lc:#$04A2;uc:(#$04A3,#0,#0)),
    (fl:CFT_COMMON;lc:#$04A4;uc:(#$04A5,#0,#0)),
    (fl:CFT_COMMON;lc:#$04A6;uc:(#$04A7,#0,#0)),
    (fl:CFT_COMMON;lc:#$04A8;uc:(#$04A9,#0,#0)),
    (fl:CFT_COMMON;lc:#$04AA;uc:(#$04AB,#0,#0)),
    (fl:CFT_COMMON;lc:#$04AC;uc:(#$04AD,#0,#0)),
    (fl:CFT_COMMON;lc:#$04AE;uc:(#$04AF,#0,#0)),
    (fl:CFT_COMMON;lc:#$04B0;uc:(#$04B1,#0,#0)),
    (fl:CFT_COMMON;lc:#$04B2;uc:(#$04B3,#0,#0)),
    (fl:CFT_COMMON;lc:#$04B4;uc:(#$04B5,#0,#0)),
    (fl:CFT_COMMON;lc:#$04B6;uc:(#$04B7,#0,#0)),
    (fl:CFT_COMMON;lc:#$04B8;uc:(#$04B9,#0,#0)),
    (fl:CFT_COMMON;lc:#$04BA;uc:(#$04BB,#0,#0)),
    (fl:CFT_COMMON;lc:#$04BC;uc:(#$04BD,#0,#0)),
    (fl:CFT_COMMON;lc:#$04BE;uc:(#$04BF,#0,#0)),
    (fl:CFT_COMMON;lc:#$04C1;uc:(#$04C2,#0,#0)),
    (fl:CFT_COMMON;lc:#$04C3;uc:(#$04C4,#0,#0)),
    (fl:CFT_COMMON;lc:#$04C7;uc:(#$04C8,#0,#0)),
    (fl:CFT_COMMON;lc:#$04CB;uc:(#$04CC,#0,#0)),
    (fl:CFT_COMMON;lc:#$04D0;uc:(#$04D1,#0,#0)),
    (fl:CFT_COMMON;lc:#$04D2;uc:(#$04D3,#0,#0)),
    (fl:CFT_COMMON;lc:#$04D4;uc:(#$04D5,#0,#0)),
    (fl:CFT_COMMON;lc:#$04D6;uc:(#$04D7,#0,#0)),
    (fl:CFT_COMMON;lc:#$04D8;uc:(#$04D9,#0,#0)),
    (fl:CFT_COMMON;lc:#$04DA;uc:(#$04DB,#0,#0)),
    (fl:CFT_COMMON;lc:#$04DC;uc:(#$04DD,#0,#0)),
    (fl:CFT_COMMON;lc:#$04DE;uc:(#$04DF,#0,#0)),
    (fl:CFT_COMMON;lc:#$04E0;uc:(#$04E1,#0,#0)),
    (fl:CFT_COMMON;lc:#$04E2;uc:(#$04E3,#0,#0)),
    (fl:CFT_COMMON;lc:#$04E4;uc:(#$04E5,#0,#0)),
    (fl:CFT_COMMON;lc:#$04E6;uc:(#$04E7,#0,#0)),
    (fl:CFT_COMMON;lc:#$04E8;uc:(#$04E9,#0,#0)),
    (fl:CFT_COMMON;lc:#$04EA;uc:(#$04EB,#0,#0)),
    (fl:CFT_COMMON;lc:#$04EC;uc:(#$04ED,#0,#0)),
    (fl:CFT_COMMON;lc:#$04EE;uc:(#$04EF,#0,#0)),
    (fl:CFT_COMMON;lc:#$04F0;uc:(#$04F1,#0,#0)),
    (fl:CFT_COMMON;lc:#$04F2;uc:(#$04F3,#0,#0)),
    (fl:CFT_COMMON;lc:#$04F4;uc:(#$04F5,#0,#0)),
    (fl:CFT_COMMON;lc:#$04F8;uc:(#$04F9,#0,#0)),
    (fl:CFT_COMMON;lc:#$0531;uc:(#$0561,#0,#0)),
    (fl:CFT_COMMON;lc:#$0532;uc:(#$0562,#0,#0)),
    (fl:CFT_COMMON;lc:#$0533;uc:(#$0563,#0,#0)),
    (fl:CFT_COMMON;lc:#$0534;uc:(#$0564,#0,#0)),
    (fl:CFT_COMMON;lc:#$0535;uc:(#$0565,#0,#0)),
    (fl:CFT_COMMON;lc:#$0536;uc:(#$0566,#0,#0)),
    (fl:CFT_COMMON;lc:#$0537;uc:(#$0567,#0,#0)),
    (fl:CFT_COMMON;lc:#$0538;uc:(#$0568,#0,#0)),
    (fl:CFT_COMMON;lc:#$0539;uc:(#$0569,#0,#0)),
    (fl:CFT_COMMON;lc:#$053A;uc:(#$056A,#0,#0)),
    (fl:CFT_COMMON;lc:#$053B;uc:(#$056B,#0,#0)),
    (fl:CFT_COMMON;lc:#$053C;uc:(#$056C,#0,#0)),
    (fl:CFT_COMMON;lc:#$053D;uc:(#$056D,#0,#0)),
    (fl:CFT_COMMON;lc:#$053E;uc:(#$056E,#0,#0)),
    (fl:CFT_COMMON;lc:#$053F;uc:(#$056F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0540;uc:(#$0570,#0,#0)),
    (fl:CFT_COMMON;lc:#$0541;uc:(#$0571,#0,#0)),
    (fl:CFT_COMMON;lc:#$0542;uc:(#$0572,#0,#0)),
    (fl:CFT_COMMON;lc:#$0543;uc:(#$0573,#0,#0)),
    (fl:CFT_COMMON;lc:#$0544;uc:(#$0574,#0,#0)),
    (fl:CFT_COMMON;lc:#$0545;uc:(#$0575,#0,#0)),
    (fl:CFT_COMMON;lc:#$0546;uc:(#$0576,#0,#0)),
    (fl:CFT_COMMON;lc:#$0547;uc:(#$0577,#0,#0)),
    (fl:CFT_COMMON;lc:#$0548;uc:(#$0578,#0,#0)),
    (fl:CFT_COMMON;lc:#$0549;uc:(#$0579,#0,#0)),
    (fl:CFT_COMMON;lc:#$054A;uc:(#$057A,#0,#0)),
    (fl:CFT_COMMON;lc:#$054B;uc:(#$057B,#0,#0)),
    (fl:CFT_COMMON;lc:#$054C;uc:(#$057C,#0,#0)),
    (fl:CFT_COMMON;lc:#$054D;uc:(#$057D,#0,#0)),
    (fl:CFT_COMMON;lc:#$054E;uc:(#$057E,#0,#0)),
    (fl:CFT_COMMON;lc:#$054F;uc:(#$057F,#0,#0)),
    (fl:CFT_COMMON;lc:#$0550;uc:(#$0580,#0,#0)),
    (fl:CFT_COMMON;lc:#$0551;uc:(#$0581,#0,#0)),
    (fl:CFT_COMMON;lc:#$0552;uc:(#$0582,#0,#0)),
    (fl:CFT_COMMON;lc:#$0553;uc:(#$0583,#0,#0)),
    (fl:CFT_COMMON;lc:#$0554;uc:(#$0584,#0,#0)),
    (fl:CFT_COMMON;lc:#$0555;uc:(#$0585,#0,#0)),
    (fl:CFT_COMMON;lc:#$0556;uc:(#$0586,#0,#0)),
    (fl:CFT_FULL;lc:#$0587;uc:(#$0565,#$0582,#0)),
    (fl:CFT_COMMON;lc:#$1E00;uc:(#$1E01,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E02;uc:(#$1E03,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E04;uc:(#$1E05,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E06;uc:(#$1E07,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E08;uc:(#$1E09,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E0A;uc:(#$1E0B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E0C;uc:(#$1E0D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E0E;uc:(#$1E0F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E10;uc:(#$1E11,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E12;uc:(#$1E13,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E14;uc:(#$1E15,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E16;uc:(#$1E17,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E18;uc:(#$1E19,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E1A;uc:(#$1E1B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E1C;uc:(#$1E1D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E1E;uc:(#$1E1F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E20;uc:(#$1E21,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E22;uc:(#$1E23,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E24;uc:(#$1E25,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E26;uc:(#$1E27,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E28;uc:(#$1E29,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E2A;uc:(#$1E2B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E2C;uc:(#$1E2D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E2E;uc:(#$1E2F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E30;uc:(#$1E31,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E32;uc:(#$1E33,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E34;uc:(#$1E35,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E36;uc:(#$1E37,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E38;uc:(#$1E39,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E3A;uc:(#$1E3B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E3C;uc:(#$1E3D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E3E;uc:(#$1E3F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E40;uc:(#$1E41,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E42;uc:(#$1E43,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E44;uc:(#$1E45,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E46;uc:(#$1E47,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E48;uc:(#$1E49,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E4A;uc:(#$1E4B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E4C;uc:(#$1E4D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E4E;uc:(#$1E4F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E50;uc:(#$1E51,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E52;uc:(#$1E53,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E54;uc:(#$1E55,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E56;uc:(#$1E57,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E58;uc:(#$1E59,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E5A;uc:(#$1E5B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E5C;uc:(#$1E5D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E5E;uc:(#$1E5F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E60;uc:(#$1E61,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E62;uc:(#$1E63,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E64;uc:(#$1E65,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E66;uc:(#$1E67,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E68;uc:(#$1E69,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E6A;uc:(#$1E6B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E6C;uc:(#$1E6D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E6E;uc:(#$1E6F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E70;uc:(#$1E71,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E72;uc:(#$1E73,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E74;uc:(#$1E75,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E76;uc:(#$1E77,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E78;uc:(#$1E79,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E7A;uc:(#$1E7B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E7C;uc:(#$1E7D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E7E;uc:(#$1E7F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E80;uc:(#$1E81,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E82;uc:(#$1E83,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E84;uc:(#$1E85,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E86;uc:(#$1E87,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E88;uc:(#$1E89,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E8A;uc:(#$1E8B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E8C;uc:(#$1E8D,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E8E;uc:(#$1E8F,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E90;uc:(#$1E91,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E92;uc:(#$1E93,#0,#0)),
    (fl:CFT_COMMON;lc:#$1E94;uc:(#$1E95,#0,#0)),
    (fl:CFT_FULL;lc:#$1E96;uc:(#$0068,#$0331,#0)),
    (fl:CFT_FULL;lc:#$1E97;uc:(#$0074,#$0308,#0)),
    (fl:CFT_FULL;lc:#$1E98;uc:(#$0077,#$030A,#0)),
    (fl:CFT_FULL;lc:#$1E99;uc:(#$0079,#$030A,#0)),
    (fl:CFT_FULL;lc:#$1E9A;uc:(#$0061,#$02BE,#0)),
    (fl:CFT_COMMON;lc:#$1E9B;uc:(#$1E61,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EA0;uc:(#$1EA1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EA2;uc:(#$1EA3,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EA4;uc:(#$1EA5,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EA6;uc:(#$1EA7,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EA8;uc:(#$1EA9,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EAA;uc:(#$1EAB,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EAC;uc:(#$1EAD,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EAE;uc:(#$1EAF,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EB0;uc:(#$1EB1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EB2;uc:(#$1EB3,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EB4;uc:(#$1EB5,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EB6;uc:(#$1EB7,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EB8;uc:(#$1EB9,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EBA;uc:(#$1EBB,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EBC;uc:(#$1EBD,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EBE;uc:(#$1EBF,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EC0;uc:(#$1EC1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EC2;uc:(#$1EC3,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EC4;uc:(#$1EC5,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EC6;uc:(#$1EC7,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EC8;uc:(#$1EC9,#0,#0)),
    (fl:CFT_COMMON;lc:#$1ECA;uc:(#$1ECB,#0,#0)),
    (fl:CFT_COMMON;lc:#$1ECC;uc:(#$1ECD,#0,#0)),
    (fl:CFT_COMMON;lc:#$1ECE;uc:(#$1ECF,#0,#0)),
    (fl:CFT_COMMON;lc:#$1ED0;uc:(#$1ED1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1ED2;uc:(#$1ED3,#0,#0)),
    (fl:CFT_COMMON;lc:#$1ED4;uc:(#$1ED5,#0,#0)),
    (fl:CFT_COMMON;lc:#$1ED6;uc:(#$1ED7,#0,#0)),
    (fl:CFT_COMMON;lc:#$1ED8;uc:(#$1ED9,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EDA;uc:(#$1EDB,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EDC;uc:(#$1EDD,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EDE;uc:(#$1EDF,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EE0;uc:(#$1EE1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EE2;uc:(#$1EE3,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EE4;uc:(#$1EE5,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EE6;uc:(#$1EE7,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EE8;uc:(#$1EE9,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EEA;uc:(#$1EEB,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EEC;uc:(#$1EED,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EEE;uc:(#$1EEF,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EF0;uc:(#$1EF1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EF2;uc:(#$1EF3,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EF4;uc:(#$1EF5,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EF6;uc:(#$1EF7,#0,#0)),
    (fl:CFT_COMMON;lc:#$1EF8;uc:(#$1EF9,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F08;uc:(#$1F00,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F09;uc:(#$1F01,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F0A;uc:(#$1F02,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F0B;uc:(#$1F03,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F0C;uc:(#$1F04,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F0D;uc:(#$1F05,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F0E;uc:(#$1F06,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F0F;uc:(#$1F07,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F18;uc:(#$1F10,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F19;uc:(#$1F11,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F1A;uc:(#$1F12,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F1B;uc:(#$1F13,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F1C;uc:(#$1F14,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F1D;uc:(#$1F15,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F28;uc:(#$1F20,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F29;uc:(#$1F21,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F2A;uc:(#$1F22,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F2B;uc:(#$1F23,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F2C;uc:(#$1F24,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F2D;uc:(#$1F25,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F2E;uc:(#$1F26,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F2F;uc:(#$1F27,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F38;uc:(#$1F30,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F39;uc:(#$1F31,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F3A;uc:(#$1F32,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F3B;uc:(#$1F33,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F3C;uc:(#$1F34,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F3D;uc:(#$1F35,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F3E;uc:(#$1F36,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F3F;uc:(#$1F37,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F48;uc:(#$1F40,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F49;uc:(#$1F41,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F4A;uc:(#$1F42,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F4B;uc:(#$1F43,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F4C;uc:(#$1F44,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F4D;uc:(#$1F45,#0,#0)),
    (fl:CFT_FULL;lc:#$1F50;uc:(#$03C5,#$0313,#0)),
    (fl:CFT_FULL;lc:#$1F52;uc:(#$03C5,#$0313,#$0300)),
    (fl:CFT_FULL;lc:#$1F54;uc:(#$03C5,#$0313,#$0301)),
    (fl:CFT_FULL;lc:#$1F56;uc:(#$03C5,#$0313,#$0342)),
    (fl:CFT_COMMON;lc:#$1F59;uc:(#$1F51,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F5B;uc:(#$1F53,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F5D;uc:(#$1F55,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F5F;uc:(#$1F57,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F68;uc:(#$1F60,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F69;uc:(#$1F61,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F6A;uc:(#$1F62,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F6B;uc:(#$1F63,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F6C;uc:(#$1F64,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F6D;uc:(#$1F65,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F6E;uc:(#$1F66,#0,#0)),
    (fl:CFT_COMMON;lc:#$1F6F;uc:(#$1F67,#0,#0)),
    (fl:CFT_FULL;lc:#$1F80;uc:(#$1F00,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F81;uc:(#$1F01,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F82;uc:(#$1F02,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F83;uc:(#$1F03,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F84;uc:(#$1F04,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F85;uc:(#$1F05,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F86;uc:(#$1F06,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F87;uc:(#$1F07,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F88;uc:(#$1F00,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F88;uc:(#$1F80,#0,#0)),
    (fl:CFT_FULL;lc:#$1F89;uc:(#$1F01,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F89;uc:(#$1F81,#0,#0)),
    (fl:CFT_FULL;lc:#$1F8A;uc:(#$1F02,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F8A;uc:(#$1F82,#0,#0)),
    (fl:CFT_FULL;lc:#$1F8B;uc:(#$1F03,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F8B;uc:(#$1F83,#0,#0)),
    (fl:CFT_FULL;lc:#$1F8C;uc:(#$1F04,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F8C;uc:(#$1F84,#0,#0)),
    (fl:CFT_FULL;lc:#$1F8D;uc:(#$1F05,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F8D;uc:(#$1F85,#0,#0)),
    (fl:CFT_FULL;lc:#$1F8E;uc:(#$1F06,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F8E;uc:(#$1F86,#0,#0)),
    (fl:CFT_FULL;lc:#$1F8F;uc:(#$1F07,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F8F;uc:(#$1F87,#0,#0)),
    (fl:CFT_FULL;lc:#$1F90;uc:(#$1F20,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F91;uc:(#$1F21,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F92;uc:(#$1F22,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F93;uc:(#$1F23,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F94;uc:(#$1F24,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F95;uc:(#$1F25,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F96;uc:(#$1F26,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F97;uc:(#$1F27,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1F98;uc:(#$1F20,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F98;uc:(#$1F90,#0,#0)),
    (fl:CFT_FULL;lc:#$1F99;uc:(#$1F21,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F99;uc:(#$1F91,#0,#0)),
    (fl:CFT_FULL;lc:#$1F9A;uc:(#$1F22,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F9A;uc:(#$1F92,#0,#0)),
    (fl:CFT_FULL;lc:#$1F9B;uc:(#$1F23,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F9B;uc:(#$1F93,#0,#0)),
    (fl:CFT_FULL;lc:#$1F9C;uc:(#$1F24,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F9C;uc:(#$1F94,#0,#0)),
    (fl:CFT_FULL;lc:#$1F9D;uc:(#$1F25,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F9D;uc:(#$1F95,#0,#0)),
    (fl:CFT_FULL;lc:#$1F9E;uc:(#$1F26,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F9E;uc:(#$1F96,#0,#0)),
    (fl:CFT_FULL;lc:#$1F9F;uc:(#$1F27,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1F9F;uc:(#$1F97,#0,#0)),
    (fl:CFT_FULL;lc:#$1FA0;uc:(#$1F60,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FA1;uc:(#$1F61,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FA2;uc:(#$1F62,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FA3;uc:(#$1F63,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FA4;uc:(#$1F64,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FA5;uc:(#$1F65,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FA6;uc:(#$1F66,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FA7;uc:(#$1F67,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FA8;uc:(#$1F60,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FA8;uc:(#$1FA0,#0,#0)),
    (fl:CFT_FULL;lc:#$1FA9;uc:(#$1F61,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FA9;uc:(#$1FA1,#0,#0)),
    (fl:CFT_FULL;lc:#$1FAA;uc:(#$1F62,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FAA;uc:(#$1FA2,#0,#0)),
    (fl:CFT_FULL;lc:#$1FAB;uc:(#$1F63,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FAB;uc:(#$1FA3,#0,#0)),
    (fl:CFT_FULL;lc:#$1FAC;uc:(#$1F64,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FAC;uc:(#$1FA4,#0,#0)),
    (fl:CFT_FULL;lc:#$1FAD;uc:(#$1F65,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FAD;uc:(#$1FA5,#0,#0)),
    (fl:CFT_FULL;lc:#$1FAE;uc:(#$1F66,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FAE;uc:(#$1FA6,#0,#0)),
    (fl:CFT_FULL;lc:#$1FAF;uc:(#$1F67,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FAF;uc:(#$1FA7,#0,#0)),
    (fl:CFT_FULL;lc:#$1FB2;uc:(#$1F70,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FB3;uc:(#$03B1,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FB4;uc:(#$03AC,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FB6;uc:(#$03B1,#$0342,#0)),
    (fl:CFT_FULL;lc:#$1FB7;uc:(#$03B1,#$0342,#$03B9)),
    (fl:CFT_COMMON;lc:#$1FB8;uc:(#$1FB0,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FB9;uc:(#$1FB1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FBA;uc:(#$1F70,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FBB;uc:(#$1F71,#0,#0)),
    (fl:CFT_FULL;lc:#$1FBC;uc:(#$03B1,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FBC;uc:(#$1FB3,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FBE;uc:(#$03B9,#0,#0)),
    (fl:CFT_FULL;lc:#$1FC2;uc:(#$1F74,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FC3;uc:(#$03B7,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FC4;uc:(#$03AE,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FC6;uc:(#$03B7,#$0342,#0)),
    (fl:CFT_FULL;lc:#$1FC7;uc:(#$03B7,#$0342,#$03B9)),
    (fl:CFT_COMMON;lc:#$1FC8;uc:(#$1F72,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FC9;uc:(#$1F73,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FCA;uc:(#$1F74,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FCB;uc:(#$1F75,#0,#0)),
    (fl:CFT_FULL;lc:#$1FCC;uc:(#$03B7,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FCC;uc:(#$1FC3,#0,#0)),
    (fl:CFT_FULL;lc:#$1FD2;uc:(#$03B9,#$0308,#$0300)),
    (fl:CFT_FULL;lc:#$1FD3;uc:(#$03B9,#$0308,#$0301)),
    (fl:CFT_FULL;lc:#$1FD6;uc:(#$03B9,#$0342,#0)),
    (fl:CFT_FULL;lc:#$1FD7;uc:(#$03B9,#$0308,#$0342)),
    (fl:CFT_COMMON;lc:#$1FD8;uc:(#$1FD0,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FD9;uc:(#$1FD1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FDA;uc:(#$1F76,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FDB;uc:(#$1F77,#0,#0)),
    (fl:CFT_FULL;lc:#$1FE2;uc:(#$03C5,#$0308,#$0300)),
    (fl:CFT_FULL;lc:#$1FE3;uc:(#$03C5,#$0308,#$0301)),
    (fl:CFT_FULL;lc:#$1FE4;uc:(#$03C1,#$0313,#0)),
    (fl:CFT_FULL;lc:#$1FE6;uc:(#$03C5,#$0342,#0)),
    (fl:CFT_FULL;lc:#$1FE7;uc:(#$03C5,#$0308,#$0342)),
    (fl:CFT_COMMON;lc:#$1FE8;uc:(#$1FE0,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FE9;uc:(#$1FE1,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FEA;uc:(#$1F7A,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FEB;uc:(#$1F7B,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FEC;uc:(#$1FE5,#0,#0)),
    (fl:CFT_FULL;lc:#$1FF2;uc:(#$1F7C,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FF3;uc:(#$03C9,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FF4;uc:(#$03CE,#$03B9,#0)),
    (fl:CFT_FULL;lc:#$1FF6;uc:(#$03C9,#$0342,#0)),
    (fl:CFT_FULL;lc:#$1FF7;uc:(#$03C9,#$0342,#$03B9)),
    (fl:CFT_COMMON;lc:#$1FF8;uc:(#$1F78,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FF9;uc:(#$1F79,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FFA;uc:(#$1F7C,#0,#0)),
    (fl:CFT_COMMON;lc:#$1FFB;uc:(#$1F7D,#0,#0)),
    (fl:CFT_FULL;lc:#$1FFC;uc:(#$03C9,#$03B9,#0)),
    (fl:CFT_SIMPLE;lc:#$1FFC;uc:(#$1FF3,#0,#0)),
    (fl:CFT_COMMON;lc:#$2126;uc:(#$03C9,#0,#0)),
    (fl:CFT_COMMON;lc:#$212A;uc:(#$006B,#0,#0)),
    (fl:CFT_COMMON;lc:#$212B;uc:(#$00E5,#0,#0)),
    (fl:CFT_COMMON;lc:#$2160;uc:(#$2170,#0,#0)),
    (fl:CFT_COMMON;lc:#$2161;uc:(#$2171,#0,#0)),
    (fl:CFT_COMMON;lc:#$2162;uc:(#$2172,#0,#0)),
    (fl:CFT_COMMON;lc:#$2163;uc:(#$2173,#0,#0)),
    (fl:CFT_COMMON;lc:#$2164;uc:(#$2174,#0,#0)),
    (fl:CFT_COMMON;lc:#$2165;uc:(#$2175,#0,#0)),
    (fl:CFT_COMMON;lc:#$2166;uc:(#$2176,#0,#0)),
    (fl:CFT_COMMON;lc:#$2167;uc:(#$2177,#0,#0)),
    (fl:CFT_COMMON;lc:#$2168;uc:(#$2178,#0,#0)),
    (fl:CFT_COMMON;lc:#$2169;uc:(#$2179,#0,#0)),
    (fl:CFT_COMMON;lc:#$216A;uc:(#$217A,#0,#0)),
    (fl:CFT_COMMON;lc:#$216B;uc:(#$217B,#0,#0)),
    (fl:CFT_COMMON;lc:#$216C;uc:(#$217C,#0,#0)),
    (fl:CFT_COMMON;lc:#$216D;uc:(#$217D,#0,#0)),
    (fl:CFT_COMMON;lc:#$216E;uc:(#$217E,#0,#0)),
    (fl:CFT_COMMON;lc:#$216F;uc:(#$217F,#0,#0)),
    (fl:CFT_COMMON;lc:#$24B6;uc:(#$24D0,#0,#0)),
    (fl:CFT_COMMON;lc:#$24B7;uc:(#$24D1,#0,#0)),
    (fl:CFT_COMMON;lc:#$24B8;uc:(#$24D2,#0,#0)),
    (fl:CFT_COMMON;lc:#$24B9;uc:(#$24D3,#0,#0)),
    (fl:CFT_COMMON;lc:#$24BA;uc:(#$24D4,#0,#0)),
    (fl:CFT_COMMON;lc:#$24BB;uc:(#$24D5,#0,#0)),
    (fl:CFT_COMMON;lc:#$24BC;uc:(#$24D6,#0,#0)),
    (fl:CFT_COMMON;lc:#$24BD;uc:(#$24D7,#0,#0)),
    (fl:CFT_COMMON;lc:#$24BE;uc:(#$24D8,#0,#0)),
    (fl:CFT_COMMON;lc:#$24BF;uc:(#$24D9,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C0;uc:(#$24DA,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C1;uc:(#$24DB,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C2;uc:(#$24DC,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C3;uc:(#$24DD,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C4;uc:(#$24DE,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C5;uc:(#$24DF,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C6;uc:(#$24E0,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C7;uc:(#$24E1,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C8;uc:(#$24E2,#0,#0)),
    (fl:CFT_COMMON;lc:#$24C9;uc:(#$24E3,#0,#0)),
    (fl:CFT_COMMON;lc:#$24CA;uc:(#$24E4,#0,#0)),
    (fl:CFT_COMMON;lc:#$24CB;uc:(#$24E5,#0,#0)),
    (fl:CFT_COMMON;lc:#$24CC;uc:(#$24E6,#0,#0)),
    (fl:CFT_COMMON;lc:#$24CD;uc:(#$24E7,#0,#0)),
    (fl:CFT_COMMON;lc:#$24CE;uc:(#$24E8,#0,#0)),
    (fl:CFT_COMMON;lc:#$24CF;uc:(#$24E9,#0,#0)),
    (fl:CFT_FULL;lc:#$FB00;uc:(#$0066,#$0066,#0)),
    (fl:CFT_FULL;lc:#$FB01;uc:(#$0066,#$0069,#0)),
    (fl:CFT_FULL;lc:#$FB02;uc:(#$0066,#$006C,#0)),
    (fl:CFT_FULL;lc:#$FB03;uc:(#$0066,#$0066,#$0069)),
    (fl:CFT_FULL;lc:#$FB04;uc:(#$0066,#$0066,#$006C)),
    (fl:CFT_FULL;lc:#$FB05;uc:(#$0073,#$0074,#0)),
    (fl:CFT_FULL;lc:#$FB06;uc:(#$0073,#$0074,#0)),
    (fl:CFT_FULL;lc:#$FB13;uc:(#$0574,#$0576,#0)),
    (fl:CFT_FULL;lc:#$FB14;uc:(#$0574,#$0565,#0)),
    (fl:CFT_FULL;lc:#$FB15;uc:(#$0574,#$056B,#0)),
    (fl:CFT_FULL;lc:#$FB16;uc:(#$057E,#$0576,#0)),
    (fl:CFT_FULL;lc:#$FB17;uc:(#$0574,#$056D,#0)),
    (fl:CFT_COMMON;lc:#$FF21;uc:(#$FF41,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF22;uc:(#$FF42,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF23;uc:(#$FF43,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF24;uc:(#$FF44,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF25;uc:(#$FF45,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF26;uc:(#$FF46,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF27;uc:(#$FF47,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF28;uc:(#$FF48,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF29;uc:(#$FF49,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF2A;uc:(#$FF4A,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF2B;uc:(#$FF4B,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF2C;uc:(#$FF4C,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF2D;uc:(#$FF4D,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF2E;uc:(#$FF4E,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF2F;uc:(#$FF4F,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF30;uc:(#$FF50,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF31;uc:(#$FF51,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF32;uc:(#$FF52,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF33;uc:(#$FF53,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF34;uc:(#$FF54,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF35;uc:(#$FF55,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF36;uc:(#$FF56,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF37;uc:(#$FF57,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF38;uc:(#$FF58,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF39;uc:(#$FF59,#0,#0)),
    (fl:CFT_COMMON;lc:#$FF3A;uc:(#$FF5A,#0,#0))
  );

const
  max_wtr=673;
  wtr:array[1..max_wtr*2] of widechar=(
    #$0041,#$0061,
    #$0042,#$0062,
    #$0043,#$0063,
    #$0044,#$0064,
    #$0045,#$0065,
    #$0046,#$0066,
    #$0047,#$0067,
    #$0048,#$0068,
    #$0049,#$0069,
    #$004A,#$006A,
    #$004B,#$006B,
    #$004C,#$006C,
    #$004D,#$006D,
    #$004E,#$006E,
    #$004F,#$006F,
    #$0050,#$0070,
    #$0051,#$0071,
    #$0052,#$0072,
    #$0053,#$0073,
    #$0054,#$0074,
    #$0055,#$0075,
    #$0056,#$0076,
    #$0057,#$0077,
    #$0058,#$0078,
    #$0059,#$0079,
    #$005A,#$007A,
    #$00C0,#$00E0,
    #$00C1,#$00E1,
    #$00C2,#$00E2,
    #$00C3,#$00E3,
    #$00C4,#$00E4,
    #$00C5,#$00E5,
    #$00C6,#$00E6,
    #$00C7,#$00E7,
    #$00C8,#$00E8,
    #$00C9,#$00E9,
    #$00CA,#$00EA,
    #$00CB,#$00EB,
    #$00CC,#$00EC,
    #$00CD,#$00ED,
    #$00CE,#$00EE,
    #$00CF,#$00EF,
    #$00D0,#$00F0,
    #$00D1,#$00F1,
    #$00D2,#$00F2,
    #$00D3,#$00F3,
    #$00D4,#$00F4,
    #$00D5,#$00F5,
    #$00D6,#$00F6,
    #$00D8,#$00F8,
    #$00D9,#$00F9,
    #$00DA,#$00FA,
    #$00DB,#$00FB,
    #$00DC,#$00FC,
    #$00DD,#$00FD,
    #$00DE,#$00FE,
    #$0100,#$0101,
    #$0102,#$0103,
    #$0104,#$0105,
    #$0106,#$0107,
    #$0108,#$0109,
    #$010A,#$010B,
    #$010C,#$010D,
    #$010E,#$010F,
    #$0110,#$0111,
    #$0112,#$0113,
    #$0114,#$0115,
    #$0116,#$0117,
    #$0118,#$0119,
    #$011A,#$011B,
    #$011C,#$011D,
    #$011E,#$011F,
    #$0120,#$0121,
    #$0122,#$0123,
    #$0124,#$0125,
    #$0126,#$0127,
    #$0128,#$0129,
    #$012A,#$012B,
    #$012C,#$012D,
    #$012E,#$012F,
    #$0132,#$0133,
    #$0134,#$0135,
    #$0136,#$0137,
    #$0139,#$013A,
    #$013B,#$013C,
    #$013D,#$013E,
    #$013F,#$0140,
    #$0141,#$0142,
    #$0143,#$0144,
    #$0145,#$0146,
    #$0147,#$0148,
    #$014A,#$014B,
    #$014C,#$014D,
    #$014E,#$014F,
    #$0150,#$0151,
    #$0152,#$0153,
    #$0154,#$0155,
    #$0156,#$0157,
    #$0158,#$0159,
    #$015A,#$015B,
    #$015C,#$015D,
    #$015E,#$015F,
    #$0160,#$0161,
    #$0162,#$0163,
    #$0164,#$0165,
    #$0166,#$0167,
    #$0168,#$0169,
    #$016A,#$016B,
    #$016C,#$016D,
    #$016E,#$016F,
    #$0170,#$0171,
    #$0172,#$0173,
    #$0174,#$0175,
    #$0176,#$0177,
    #$0178,#$00FF,
    #$0179,#$017A,
    #$017B,#$017C,
    #$017D,#$017E,
    #$0181,#$0253,
    #$0182,#$0183,
    #$0184,#$0185,
    #$0186,#$0254,
    #$0187,#$0188,
    #$0189,#$0256,
    #$018A,#$0257,
    #$018B,#$018C,
    #$018E,#$01DD,
    #$018F,#$0259,
    #$0190,#$025B,
    #$0191,#$0192,
    #$0193,#$0260,
    #$0194,#$0263,
    #$0196,#$0269,
    #$0197,#$0268,
    #$0198,#$0199,
    #$019C,#$026F,
    #$019D,#$0272,
    #$019F,#$0275,
    #$01A0,#$01A1,
    #$01A2,#$01A3,
    #$01A4,#$01A5,
    #$01A7,#$01A8,
    #$01A9,#$0283,
    #$01AC,#$01AD,
    #$01AE,#$0288,
    #$01AF,#$01B0,
    #$01B1,#$028A,
    #$01B2,#$028B,
    #$01B3,#$01B4,
    #$01B5,#$01B6,
    #$01B7,#$0292,
    #$01B8,#$01B9,
    #$01BC,#$01BD,
    #$01C4,#$01C6,
    #$01C7,#$01C9,
    #$01CA,#$01CC,
    #$01CD,#$01CE,
    #$01CF,#$01D0,
    #$01D1,#$01D2,
    #$01D3,#$01D4,
    #$01D5,#$01D6,
    #$01D7,#$01D8,
    #$01D9,#$01DA,
    #$01DB,#$01DC,
    #$01DE,#$01DF,
    #$01E0,#$01E1,
    #$01E2,#$01E3,
    #$01E4,#$01E5,
    #$01E6,#$01E7,
    #$01E8,#$01E9,
    #$01EA,#$01EB,
    #$01EC,#$01ED,
    #$01EE,#$01EF,
    #$01F1,#$01F3,
    #$01F4,#$01F5,
    #$01FA,#$01FB,
    #$01FC,#$01FD,
    #$01FE,#$01FF,
    #$0200,#$0201,
    #$0202,#$0203,
    #$0204,#$0205,
    #$0206,#$0207,
    #$0208,#$0209,
    #$020A,#$020B,
    #$020C,#$020D,
    #$020E,#$020F,
    #$0210,#$0211,
    #$0212,#$0213,
    #$0214,#$0215,
    #$0216,#$0217,
    #$0386,#$03AC,
    #$0388,#$03AD,
    #$0389,#$03AE,
    #$038A,#$03AF,
    #$038C,#$03CC,
    #$038E,#$03CD,
    #$038F,#$03CE,
    #$0391,#$03B1,
    #$0392,#$03B2,
    #$0393,#$03B3,
    #$0394,#$03B4,
    #$0395,#$03B5,
    #$0396,#$03B6,
    #$0397,#$03B7,
    #$0398,#$03B8,
    #$0399,#$03B9,
    #$039A,#$03BA,
    #$039B,#$03BB,
    #$039C,#$03BC,
    #$039D,#$03BD,
    #$039E,#$03BE,
    #$039F,#$03BF,
    #$03A0,#$03C0,
    #$03A1,#$03C1,
    #$03A3,#$03C3,
    #$03A4,#$03C4,
    #$03A5,#$03C5,
    #$03A6,#$03C6,
    #$03A7,#$03C7,
    #$03A8,#$03C8,
    #$03A9,#$03C9,
    #$03AA,#$03CA,
    #$03AB,#$03CB,
    #$03E2,#$03E3,
    #$03E4,#$03E5,
    #$03E6,#$03E7,
    #$03E8,#$03E9,
    #$03EA,#$03EB,
    #$03EC,#$03ED,
    #$03EE,#$03EF,
    #$0401,#$0451,
    #$0402,#$0452,
    #$0403,#$0453,
    #$0404,#$0454,
    #$0405,#$0455,
    #$0406,#$0456,
    #$0407,#$0457,
    #$0408,#$0458,
    #$0409,#$0459,
    #$040A,#$045A,
    #$040B,#$045B,
    #$040C,#$045C,
    #$040E,#$045E,
    #$040F,#$045F,
    #$0410,#$0430,
    #$0411,#$0431,
    #$0412,#$0432,
    #$0413,#$0433,
    #$0414,#$0434,
    #$0415,#$0435,
    #$0416,#$0436,
    #$0417,#$0437,
    #$0418,#$0438,
    #$0419,#$0439,
    #$041A,#$043A,
    #$041B,#$043B,
    #$041C,#$043C,
    #$041D,#$043D,
    #$041E,#$043E,
    #$041F,#$043F,
    #$0420,#$0440,
    #$0421,#$0441,
    #$0422,#$0442,
    #$0423,#$0443,
    #$0424,#$0444,
    #$0425,#$0445,
    #$0426,#$0446,
    #$0427,#$0447,
    #$0428,#$0448,
    #$0429,#$0449,
    #$042A,#$044A,
    #$042B,#$044B,
    #$042C,#$044C,
    #$042D,#$044D,
    #$042E,#$044E,
    #$042F,#$044F,
    #$0460,#$0461,
    #$0462,#$0463,
    #$0464,#$0465,
    #$0466,#$0467,
    #$0468,#$0469,
    #$046A,#$046B,
    #$046C,#$046D,
    #$046E,#$046F,
    #$0470,#$0471,
    #$0472,#$0473,
    #$0474,#$0475,
    #$0476,#$0477,
    #$0478,#$0479,
    #$047A,#$047B,
    #$047C,#$047D,
    #$047E,#$047F,
    #$0480,#$0481,
    #$0490,#$0491,
    #$0492,#$0493,
    #$0494,#$0495,
    #$0496,#$0497,
    #$0498,#$0499,
    #$049A,#$049B,
    #$049C,#$049D,
    #$049E,#$049F,
    #$04A0,#$04A1,
    #$04A2,#$04A3,
    #$04A4,#$04A5,
    #$04A6,#$04A7,
    #$04A8,#$04A9,
    #$04AA,#$04AB,
    #$04AC,#$04AD,
    #$04AE,#$04AF,
    #$04B0,#$04B1,
    #$04B2,#$04B3,
    #$04B4,#$04B5,
    #$04B6,#$04B7,
    #$04B8,#$04B9,
    #$04BA,#$04BB,
    #$04BC,#$04BD,
    #$04BE,#$04BF,
    #$04C1,#$04C2,
    #$04C3,#$04C4,
    #$04C7,#$04C8,
    #$04CB,#$04CC,
    #$04D0,#$04D1,
    #$04D2,#$04D3,
    #$04D4,#$04D5,
    #$04D6,#$04D7,
    #$04D8,#$04D9,
    #$04DA,#$04DB,
    #$04DC,#$04DD,
    #$04DE,#$04DF,
    #$04E0,#$04E1,
    #$04E2,#$04E3,
    #$04E4,#$04E5,
    #$04E6,#$04E7,
    #$04E8,#$04E9,
    #$04EA,#$04EB,
    #$04EE,#$04EF,
    #$04F0,#$04F1,
    #$04F2,#$04F3,
    #$04F4,#$04F5,
    #$04F8,#$04F9,
    #$0531,#$0561,
    #$0532,#$0562,
    #$0533,#$0563,
    #$0534,#$0564,
    #$0535,#$0565,
    #$0536,#$0566,
    #$0537,#$0567,
    #$0538,#$0568,
    #$0539,#$0569,
    #$053A,#$056A,
    #$053B,#$056B,
    #$053C,#$056C,
    #$053D,#$056D,
    #$053E,#$056E,
    #$053F,#$056F,
    #$0540,#$0570,
    #$0541,#$0571,
    #$0542,#$0572,
    #$0543,#$0573,
    #$0544,#$0574,
    #$0545,#$0575,
    #$0546,#$0576,
    #$0547,#$0577,
    #$0548,#$0578,
    #$0549,#$0579,
    #$054A,#$057A,
    #$054B,#$057B,
    #$054C,#$057C,
    #$054D,#$057D,
    #$054E,#$057E,
    #$054F,#$057F,
    #$0550,#$0580,
    #$0551,#$0581,
    #$0552,#$0582,
    #$0553,#$0583,
    #$0554,#$0584,
    #$0555,#$0585,
    #$0556,#$0586,
    #$10A0,#$10D0,
    #$10A1,#$10D1,
    #$10A2,#$10D2,
    #$10A3,#$10D3,
    #$10A4,#$10D4,
    #$10A5,#$10D5,
    #$10A6,#$10D6,
    #$10A7,#$10D7,
    #$10A8,#$10D8,
    #$10A9,#$10D9,
    #$10AA,#$10DA,
    #$10AB,#$10DB,
    #$10AC,#$10DC,
    #$10AD,#$10DD,
    #$10AE,#$10DE,
    #$10AF,#$10DF,
    #$10B0,#$10E0,
    #$10B1,#$10E1,
    #$10B2,#$10E2,
    #$10B3,#$10E3,
    #$10B4,#$10E4,
    #$10B5,#$10E5,
    #$10B6,#$10E6,
    #$10B7,#$10E7,
    #$10B8,#$10E8,
    #$10B9,#$10E9,
    #$10BA,#$10EA,
    #$10BB,#$10EB,
    #$10BC,#$10EC,
    #$10BD,#$10ED,
    #$10BE,#$10EE,
    #$10BF,#$10EF,
    #$10C0,#$10F0,
    #$10C1,#$10F1,
    #$10C2,#$10F2,
    #$10C3,#$10F3,
    #$10C4,#$10F4,
    #$10C5,#$10F5,
    #$1E00,#$1E01,
    #$1E02,#$1E03,
    #$1E04,#$1E05,
    #$1E06,#$1E07,
    #$1E08,#$1E09,
    #$1E0A,#$1E0B,
    #$1E0C,#$1E0D,
    #$1E0E,#$1E0F,
    #$1E10,#$1E11,
    #$1E12,#$1E13,
    #$1E14,#$1E15,
    #$1E16,#$1E17,
    #$1E18,#$1E19,
    #$1E1A,#$1E1B,
    #$1E1C,#$1E1D,
    #$1E1E,#$1E1F,
    #$1E20,#$1E21,
    #$1E22,#$1E23,
    #$1E24,#$1E25,
    #$1E26,#$1E27,
    #$1E28,#$1E29,
    #$1E2A,#$1E2B,
    #$1E2C,#$1E2D,
    #$1E2E,#$1E2F,
    #$1E30,#$1E31,
    #$1E32,#$1E33,
    #$1E34,#$1E35,
    #$1E36,#$1E37,
    #$1E38,#$1E39,
    #$1E3A,#$1E3B,
    #$1E3C,#$1E3D,
    #$1E3E,#$1E3F,
    #$1E40,#$1E41,
    #$1E42,#$1E43,
    #$1E44,#$1E45,
    #$1E46,#$1E47,
    #$1E48,#$1E49,
    #$1E4A,#$1E4B,
    #$1E4C,#$1E4D,
    #$1E4E,#$1E4F,
    #$1E50,#$1E51,
    #$1E52,#$1E53,
    #$1E54,#$1E55,
    #$1E56,#$1E57,
    #$1E58,#$1E59,
    #$1E5A,#$1E5B,
    #$1E5C,#$1E5D,
    #$1E5E,#$1E5F,
    #$1E60,#$1E61,
    #$1E62,#$1E63,
    #$1E64,#$1E65,
    #$1E66,#$1E67,
    #$1E68,#$1E69,
    #$1E6A,#$1E6B,
    #$1E6C,#$1E6D,
    #$1E6E,#$1E6F,
    #$1E70,#$1E71,
    #$1E72,#$1E73,
    #$1E74,#$1E75,
    #$1E76,#$1E77,
    #$1E78,#$1E79,
    #$1E7A,#$1E7B,
    #$1E7C,#$1E7D,
    #$1E7E,#$1E7F,
    #$1E80,#$1E81,
    #$1E82,#$1E83,
    #$1E84,#$1E85,
    #$1E86,#$1E87,
    #$1E88,#$1E89,
    #$1E8A,#$1E8B,
    #$1E8C,#$1E8D,
    #$1E8E,#$1E8F,
    #$1E90,#$1E91,
    #$1E92,#$1E93,
    #$1E94,#$1E95,
    #$1EA0,#$1EA1,
    #$1EA2,#$1EA3,
    #$1EA4,#$1EA5,
    #$1EA6,#$1EA7,
    #$1EA8,#$1EA9,
    #$1EAA,#$1EAB,
    #$1EAC,#$1EAD,
    #$1EAE,#$1EAF,
    #$1EB0,#$1EB1,
    #$1EB2,#$1EB3,
    #$1EB4,#$1EB5,
    #$1EB6,#$1EB7,
    #$1EB8,#$1EB9,
    #$1EBA,#$1EBB,
    #$1EBC,#$1EBD,
    #$1EBE,#$1EBF,
    #$1EC0,#$1EC1,
    #$1EC2,#$1EC3,
    #$1EC4,#$1EC5,
    #$1EC6,#$1EC7,
    #$1EC8,#$1EC9,
    #$1ECA,#$1ECB,
    #$1ECC,#$1ECD,
    #$1ECE,#$1ECF,
    #$1ED0,#$1ED1,
    #$1ED2,#$1ED3,
    #$1ED4,#$1ED5,
    #$1ED6,#$1ED7,
    #$1ED8,#$1ED9,
    #$1EDA,#$1EDB,
    #$1EDC,#$1EDD,
    #$1EDE,#$1EDF,
    #$1EE0,#$1EE1,
    #$1EE2,#$1EE3,
    #$1EE4,#$1EE5,
    #$1EE6,#$1EE7,
    #$1EE8,#$1EE9,
    #$1EEA,#$1EEB,
    #$1EEC,#$1EED,
    #$1EEE,#$1EEF,
    #$1EF0,#$1EF1,
    #$1EF2,#$1EF3,
    #$1EF4,#$1EF5,
    #$1EF6,#$1EF7,
    #$1EF8,#$1EF9,
    #$1F08,#$1F00,
    #$1F09,#$1F01,
    #$1F0A,#$1F02,
    #$1F0B,#$1F03,
    #$1F0C,#$1F04,
    #$1F0D,#$1F05,
    #$1F0E,#$1F06,
    #$1F0F,#$1F07,
    #$1F18,#$1F10,
    #$1F19,#$1F11,
    #$1F1A,#$1F12,
    #$1F1B,#$1F13,
    #$1F1C,#$1F14,
    #$1F1D,#$1F15,
    #$1F28,#$1F20,
    #$1F29,#$1F21,
    #$1F2A,#$1F22,
    #$1F2B,#$1F23,
    #$1F2C,#$1F24,
    #$1F2D,#$1F25,
    #$1F2E,#$1F26,
    #$1F2F,#$1F27,
    #$1F38,#$1F30,
    #$1F39,#$1F31,
    #$1F3A,#$1F32,
    #$1F3B,#$1F33,
    #$1F3C,#$1F34,
    #$1F3D,#$1F35,
    #$1F3E,#$1F36,
    #$1F3F,#$1F37,
    #$1F48,#$1F40,
    #$1F49,#$1F41,
    #$1F4A,#$1F42,
    #$1F4B,#$1F43,
    #$1F4C,#$1F44,
    #$1F4D,#$1F45,
    #$1F59,#$1F51,
    #$1F5B,#$1F53,
    #$1F5D,#$1F55,
    #$1F5F,#$1F57,
    #$1F68,#$1F60,
    #$1F69,#$1F61,
    #$1F6A,#$1F62,
    #$1F6B,#$1F63,
    #$1F6C,#$1F64,
    #$1F6D,#$1F65,
    #$1F6E,#$1F66,
    #$1F6F,#$1F67,
    #$1FB8,#$1FB0,
    #$1FB9,#$1FB1,
    #$1FBA,#$1F70,
    #$1FBB,#$1F71,
    #$1FC8,#$1F72,
    #$1FC9,#$1F73,
    #$1FCA,#$1F74,
    #$1FCB,#$1F75,
    #$1FD8,#$1FD0,
    #$1FD9,#$1FD1,
    #$1FDA,#$1F76,
    #$1FDB,#$1F77,
    #$1FE8,#$1FE0,
    #$1FE9,#$1FE1,
    #$1FEA,#$1F7A,
    #$1FEB,#$1F7B,
    #$1FEC,#$1FE5,
    #$1FF8,#$1F78,
    #$1FF9,#$1F79,
    #$1FFA,#$1F7C,
    #$1FFB,#$1F7D,
    #$2160,#$2170,
    #$2161,#$2171,
    #$2162,#$2172,
    #$2163,#$2173,
    #$2164,#$2174,
    #$2165,#$2175,
    #$2166,#$2176,
    #$2167,#$2177,
    #$2168,#$2178,
    #$2169,#$2179,
    #$216A,#$217A,
    #$216B,#$217B,
    #$216C,#$217C,
    #$216D,#$217D,
    #$216E,#$217E,
    #$216F,#$217F,
    #$24B6,#$24D0,
    #$24B7,#$24D1,
    #$24B8,#$24D2,
    #$24B9,#$24D3,
    #$24BA,#$24D4,
    #$24BB,#$24D5,
    #$24BC,#$24D6,
    #$24BD,#$24D7,
    #$24BE,#$24D8,
    #$24BF,#$24D9,
    #$24C0,#$24DA,
    #$24C1,#$24DB,
    #$24C2,#$24DC,
    #$24C3,#$24DD,
    #$24C4,#$24DE,
    #$24C5,#$24DF,
    #$24C6,#$24E0,
    #$24C7,#$24E1,
    #$24C8,#$24E2,
    #$24C9,#$24E3,
    #$24CA,#$24E4,
    #$24CB,#$24E5,
    #$24CC,#$24E6,
    #$24CD,#$24E7,
    #$24CE,#$24E8,
    #$24CF,#$24E9,
    #$FF21,#$FF41,
    #$FF22,#$FF42,
    #$FF23,#$FF43,
    #$FF24,#$FF44,
    #$FF25,#$FF45,
    #$FF26,#$FF46,
    #$FF27,#$FF47,
    #$FF28,#$FF48,
    #$FF29,#$FF49,
    #$FF2A,#$FF4A,
    #$FF2B,#$FF4B,
    #$FF2C,#$FF4C,
    #$FF2D,#$FF4D,
    #$FF2E,#$FF4E,
    #$FF2F,#$FF4F,
    #$FF30,#$FF50,
    #$FF31,#$FF51,
    #$FF32,#$FF52,
    #$FF33,#$FF53,
    #$FF34,#$FF54,
    #$FF35,#$FF55,
    #$FF36,#$FF56,
    #$FF37,#$FF57,
    #$FF38,#$FF58,
    #$FF39,#$FF59,
    #$FF3A,#$FF5A
  );

function checkstr(s:widestring;x:integer;l:w3arr):integer;
begin
  result:=0;
  if x>length(s) then exit;
  if s[x]<>l[1] then exit;
  inc(x);
  if l[2]=#0 then begin result:=1; exit; end;
  if x>length(s) then exit;
  if s[x]<>l[2] then exit;
  inc(x);
  if l[3]=#0 then begin result:=2; exit; end;
  if x>length(s) then exit;
  if s[x]<>l[3] then exit;
  result:=3;
end;

function uni_trans(s:widestring; d:boolean; b:byte):widestring;
var
  i,x,z:integer;
begin
  x:=1;
  while x<=length(s) do
  begin
if not ms_unicode then
begin
    i:=1;
    while i<=max_trans do
    begin
      if (b and rcc[i].fl) <> 0 then
      begin
        if not d then
        begin
          if s[x]=rcc[i].lc then
          begin
            s[x]:=rcc[i].uc[1];
            if rcc[i].uc[2]<>#0 then
            begin
              insert(rcc[i].uc[2],s,x+1);
	      inc(x);
            end;
            if rcc[i].uc[3]<>#0 then
            begin
              insert(rcc[i].uc[3],s,x+1);
	      inc(x);
            end;
            break;
          end;
        end
        else
        begin
          z:=checkstr(s,x,rcc[i].uc);
          if z>0 then
          begin
            delete(s,x,z-1);
            s[x]:=rcc[i].lc;
            break;
          end;
        end;
      end;
      inc(i);
    end;
end
else
begin
    i:=0;
    while i<max_wtr do
    begin
      if d then
      begin
        if wtr[i*2+1]=s[x] then s[x]:=wtr[i*2+2];
      end
      else
      begin
        if wtr[i*2+2]=s[x] then s[x]:=wtr[i*2+1];
      end;
      inc(i);
    end;
end;
    inc(x);
  end;
  result:=s;
end;

function uni2uppers(s:widestring):widestring;
var
  b:byte;
begin
  b:= CFT_COMMON + CFT_SIMPLE;
  if doti then
    b := b + CFT_SPECIAL;
  result := uni_trans(s, true, b);
end;

function uni2lowers(s:widestring):widestring;
var
  b:byte;
begin
  b:=CFT_COMMON+CFT_SIMPLE;
  if doti then b:=b+CFT_SPECIAL;
  result:=uni_trans(s, false, b);
end;

function uni2upperf(s:widestring):widestring;
var
  b:byte;
begin
  b:=CFT_COMMON+CFT_FULL;
  if doti then b:=b+CFT_SPECIAL;
  result:=uni_trans(s, true, b);
end;

function uni2lowerf(s:widestring):widestring;
var
  b:byte;
begin
  b:=CFT_COMMON+CFT_FULL;
  if doti then b:=b+CFT_SPECIAL;
  result:=uni_trans(s, false, b);
end;

function CreateUnicodeHintString(Value : WideString) : string;
var S : String;
    l : integer;
    T : WideChar;
begin
  result := '';
  if Length(Value) > 0 then
  begin
    S := Value;
    l := Length(Value) * 2;
    SetLength(S, l + 4);

    Move(Value[1], S[1], Length(Value) * 2);
    T := #0;
    Move(T, S[l + 1], sizeof(T));
    T := #$FFFF;
    Move(T, S[l + 3], sizeof(T));
    result := S;
  end;
end;

function WideStringDup(S : WideString) : PWideChar;
var i : integer;
begin
  i := (Length(S) + 1) * sizeof(WideChar);
  GetMem(Result, i);
  Move(S[1], Result[1], i);
end;

function WidePos(const Substr, S: WideString): Integer;
var
  P: PWideChar;
begin
  Result := 0;
  P := WideStrPos(PWideChar(S), PWideChar(SubStr));
  if P <> nil then
    Result := P - PWideChar(S) + 1;
end;

(*
function WidePos(Substr, S : WideString) : integer;
var i : integer;
    slen : integer;
begin
  slen := length(Substr);
  if (slen = 0) or (Length(S) = 0) then
    result := 0
  else
  begin
    result := 0;
    i := 1;
    while i <= Length(S) do
    begin
      if Length(S) - i < slen then
      begin
        result := 0;
        exit;
      end;
      if CompareMem(@Substr[1], @S[i], slen * sizeof(WideChar)) then
      begin
        result := i;
        exit;
      end;
      inc(i);
    end;
  end;
end;
*)

function WideStrScan(const Str: PWideChar; Chr: WideChar): PWideChar; assembler;
asm
        PUSH    EDI
        PUSH    EAX
        MOV     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     EAX,EAX
        REPNE   SCASW
        NOT     ECX
        POP     EDI
        MOV     AX, Chr
        REPNE   SCASW
        MOV     EAX,0
        JNE     @@1
        MOV     EAX, EDI
        DEC     EAX
        DEC     EAX
@@1:    POP     EDI
end;

function WideStrRScan(const Str: PWideChar; Chr: WideChar): PWideChar; assembler;
asm
        PUSH    EDI
        MOV     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        STD
        DEC     EDI
        DEC     EDI
        MOV     AX,Chr
        REPNE   SCASW
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        INC     EAX
        INC     EAX
@@1:    CLD
        POP     EDI
end;

{$ifndef BROKEN_UNICODE}
function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
var
  P, Src, Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := WideStrScan(PWideChar(S), Quote);
  while Assigned(P) do
  begin
    Inc(P);
    Inc(AddCount);
    P := WideStrScan(P, Quote);
  end;

  if AddCount = 0 then
    Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := WideStrScan(Src, Quote);
    repeat
      Inc(P);
      WideMove(Src^, Dest^, P - Src);
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := WideStrScan(Src, Quote);
    until not Assigned(P);
    P := WideStrEnd(Src);
    WideMove(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;
end;

function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then
    Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := WideStrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then
      Break;
    Inc(Src);
    Inc(DropCount);
    Src := WideStrScan(Src, Quote);
  end;
  if Src = nil then
    Src := WideStrEnd(P);
  if ((Src - P) <= 1) then
    Exit;
  if DropCount = 1 then
  begin
    SetWideString(Result, P, Src - P - 1);
  end
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := WideStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, Src - P + sizeof(WideChar) * 2);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := WideStrScan(Src, Quote);
    end;
    if Src = nil then
      Src := WideStrEnd(P);
    Move(P^, Dest^, Src - P + sizeof(WideChar));
  end;
end;
{$endif}

function WideStrEnd(const Str: PWideChar): PWideChar; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        LEA     EAX,[EDI - 2]
        MOV     EDI,EDX
end;

function WideSameText(const S1, S2: WideString): boolean;
begin
  result := WideCompareText(WideUppercase(S1), WideUppercase(S2)) = 0;
end;

function WideCompareText(const S1, S2: WideString): Integer;
var P1,
    P2 : PWideChar;
begin
  result := 0;
  P1 := @S1[1];
  P2 := @S2[1];
  while (P1^ <> #0) and (P2^ <> #0) and (P2^ = P1^) do
  begin
    inc(P1);
    Inc(P2);
  end;
  if P1^ = P2^ then
    result := 0
  else
  if P1^ < P2^ then
    result := -1
  else
  if P1^ > P2^ then
    result := 1;
end;

(*
function WideCopy(S : WideString; SPos, SLen : integer) : WideString;
var pl : integer;
begin
  if (SPos <= 0) or (Length(S) < SPos) or (SLen < 1) then
    result := ''
  else
  begin
    if Length(s) <= SPos + SLen then
      pl := Length(s) - SPos + 1
    else
      pl := SLen;
    SetLength(result, pl);
    System.Move(S[SPos], Result[1], pl * sizeof(WideChar));
  end;
end;
*)

(*
procedure WideInsert(var S : WideString; Text : WideString; SPos : integer);
begin
  if SPos > Length(S) then
    S := S + Text
  else
    S := WideCopy(S, 1, SPos - 1) + Text + WideCopy(S, SPos, Length(S));
end;

procedure WideDelete(var S : WideString; SPos, SLen : integer);
var pl : integer;
    result :  WideString;
begin
  if (SPos <= 0) or (Length(S) < SPos) or (SLen < 1) then
    result := S
  else
  begin
    if Length(s) <= SPos + SLen then
      pl := SPos - 1
    else
      pl := Length(s) - SLen;
    SetLength(Result, pl);
    pl := SPos - 1;
    if pl > 0 then
      System.Move(S[1], Result[1], pl * sizeof(WideChar));
    if Length(s) > SPos + SLen then
      System.Move(S[SPos + SLen], Result[pl + 1], (Length(S) - (SPos + SLen) + 1) * sizeof(WideChar));
  end;
  S := result;
end;
*)
function WideExtractStr(var S: WideString; SPos, SLen: integer): WideString;
begin
  result := WideCopy(S, SPos, SLen);
  WideDelete(S, SPos, SLen);
end;

function WideStrCopy(Target : PWideChar; Source: PWideChar): PWideChar;
assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,1
        REP     MOVSW
        POP     ESI
        POP     EDI
end;

function WideStrPCopy(Target : PWideChar; const Source: WideString): PWideChar;
begin
  Result := WideStrLCopy(Target, PWideChar(Source), Length(Source));
end;

function WideStrComp(const S1, S2: PWideChar): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     EAX,EAX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI,EDX
        XOR     EDX,EDX
        REPE    CMPSW
        MOV     AX,[ESI-2]
        MOV     DX,[EDI-2]
        SUB     EAX,EDX
        POP     ESI
        POP     EDI
end;

function WideStrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     EBX,ECX
        XOR     EAX,EAX
        OR      ECX,ECX
        JE      @@1
        REPNE   SCASW
        SUB     EBX,ECX
        MOV     ECX,EBX
        MOV     EDI,EDX
        XOR     EDX,EDX
        REPE    CMPSW
        MOV     AX,[ESI-2]
        MOV     DX,[EDI-2]
        SUB     EAX,EDX
@@1:    POP     EBX
        POP     ESI
        POP     EDI
end;

function WideStrLen(const Str: PWideChar): Cardinal; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;

function WideStrPas(const Source: PWideChar): WideString;
var l : integer;
begin
  if Source = nil then
    result := WideString('')
  else
  begin
    l := WideStrLen(Source);
    SetLength(Result, l);
    WideMove(Source[0], Result[1], l);
  end;
end;

procedure WideMove(const Source; var Dest; Count : Integer); assembler;
asm
{     ->EAX     Pointer to source       }
{       EDX     Pointer to destination  }
{       ECX     Count In Words          }

        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EDX,ECX
        CMP     EDI,ESI
        JA      @@1
        JE      @@2
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,1
        REP     MOVSW
        JMP     @@2
@@1:    LEA     ESI,[ESI+ECX*2-2]
        LEA     EDI,[EDI+ECX*2-2]
        AND     ECX,1
        STD
        REP     MOVSW
        DEC     ESI
        DEC     EDI
        MOV     ECX,EDX
        DEC     ESI
        DEC     EDI
        SHR     ECX,1
        REP     MOVSD
        CLD
@@2:    POP     EDI
        POP     ESI
end;

procedure FillWord(var X; Count: Integer; Value: Word); assembler;
asm
{     ->EAX     Pointer to destination  }
{       EDX     count   }
{       CX      value   }

        PUSH    EDI

        MOV     EDI,EAX { Point EDI to destination              }

        MOV     EAX,ECX { Fill EAX with value repeated 2 times  }
        SHL     EAX,16
        MOV     AX,CX

        MOV     ECX,EDX
        SAR     ECX,1
        JS      @@exit

        REP     STOSD   { Fill count DIV 2 dwords       }

        MOV     ECX,EDX
        AND     ECX,1
        REP     STOSW   { Fill count MOD 2 bytes        }

@@exit:
        POP     EDI
end;

procedure FillWideChar(var X; Count: Integer; Value: WideChar); assembler;
asm
{     ->EAX     Pointer to destination  }
{       EDX     count   }
{       CX      value   }

        PUSH    EDI

        MOV     EDI,EAX { Point EDI to destination              }

        MOV     EAX,ECX { Fill EAX with value repeated 2 times  }
        SHL     EAX,16
        MOV     AX,CX

        MOV     ECX,EDX
        SAR     ECX,1
        JS      @@exit

        REP     STOSD   { Fill count DIV 2 dwords       }

        MOV     ECX,EDX
        AND     ECX,1
        REP     STOSW   { Fill count MOD 2 bytes        }

@@exit:
        POP     EDI
end;


function WideStrMove(Dest: PWideChar; const Source: PWideChar; Count: Cardinal): PWideChar; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX
        MOV     EDI,EAX
        MOV     EDX,ECX
        CMP     EDI,ESI
        JA      @@1
        JE      @@2
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,1
        REP     MOVSW
        JMP     @@2
@@1:    LEA     ESI,[ESI+ECX*2-2]
        LEA     EDI,[EDI+ECX*2-2]
        AND     ECX,1
        STD
        REP     MOVSW
        DEC     ESI
        DEC     EDI
        MOV     ECX,EDX
        DEC     ESI
        DEC     EDI
        SHR     ECX,1
        REP     MOVSD
        CLD
@@2:    POP     EDI
        POP     ESI
end;

function WideStrECopy(Dest: PWideChar; const Source: PWideChar): PWideChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,1
        REP     MOVSW
        LEA     EAX,[EDI-2]
        POP     ESI
        POP     EDI
end;

function WideStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        XOR     AX,AX
        TEST    ECX,ECX
        JZ      @@1
        REPNE   SCASW
        JNE     @@1
        INC     ECX
@@1:    SUB     EBX,ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,EDI
        MOV     ECX,EBX
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EBX
        AND     ECX,1
        REP     MOVSW
        STOSW
        MOV     EAX,EDX
        POP     EBX
        POP     ESI
        POP     EDI
end;

function WideStrLCat(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,Dest
        MOV     EBX,MaxLen
        MOV     ESI,Source
        SHL     EBX, 1
        CALL    WideStrEnd
        MOV     ECX,EDI
        ADD     ECX,EBX
        SUB     ECX,EAX
        JBE     @@1
        SHR     ECX, 1
        MOV     EDX,ESI
        CALL    WideStrLCopy
@@1:    MOV     EAX,EDI
        POP     EBX
        POP     ESI
        POP     EDI
end;

function WideStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  WideStrCopy(WideStrEnd(Dest), Source);
  Result := Dest;
end;

procedure SetWideString(var S: WideString; Buffer: PWideChar; Len: Integer);
begin
  SetLength(S, Len);
  if Len > 0 then
  begin
    if Assigned(Buffer) then
      WideMove(Buffer^, S[1], Len)
    else
      FillWideChar(S[1], Len, #0);
  end;
end;

function CompareWideStr(const S1, S2: WideString): Integer;
var
  P1, P2: PWideChar;
  P: PWideChar;
  S: WideString;
begin
  S := GetWideStringOf(#0, 1);
  P := PWideChar(S);
  P1 := PWideChar(S1);
  if not Assigned(P1) then P1 := P;
  P2 := PWideChar(S2);
  if not Assigned(P2) then P2 := P;
  Result := WideStrComp(P1, P2);
end;

function SameWideStr(const S1, S2: WideString): Boolean;
begin
  Result := (CompareWideStr(S1, S2) = 0);
end;

function WideLastChar(const S: WideString): PWideChar;
var
  LastWord: Integer;
begin
  LastWord := Length(S);
  if LastWord > 0 then
  begin
    Result := @S[LastWord];
  end
  else
    Result := nil;
end;

{$ifndef KYLIX_USED}
function WideStrAlloc(Size: Cardinal): PWideChar;
asm
        TEST    EAX,EAX
        JE      @@1
        PUSH    EAX
        PUSH    0
        CALL    SysAllocStringLen
        TEST    EAX,EAX
        JE      WStrError
@@1:
end;

function WideStrBufSize(const Str: PWideChar): Cardinal;
begin
  Result := SysStringLen(Str);
end;

function WideStrNew(const Str: PWideChar): PWideChar;
var
  Size: Cardinal;
begin
  if Str = nil then Result := nil else
  begin
    Size := WideStrLen(Str) + 1;
    Result := WideStrMove(WideStrAlloc(Size), Str, Size);
  end;
end;

procedure WideStrDispose(Str: PWideChar);
begin
  if not Assigned(Str) then
  begin
    SysFreeString(Str);
  end;
end;
{$endif}

function WideUpperCase(const S: WideString): WideString;
begin
  Result := S;
  if Result <> '' then
    result := uni2uppers(result);
end;

function WideLowerCase(const S: WideString): WideString;
begin
  Result := S;
  if Result <> '' then
    result := uni2lowers(result);
end;

function IsWideDelimiter(const Delimiters, S: WideString; Index: Integer): Boolean;
begin
  Result := False;
  if (Index <= 0) or (Index > Length(S)) then exit;
  Result := WideStrScan(PWideChar(Delimiters), S[Index]) <> nil;
end;

function IsWidePathDelimiter(const S: WideString; Index: Integer): Boolean;
begin
  Result := IsWideDelimiter(SWidePathDelimiters, S, Index);
end;

function IncludeWideTrailingDelimiter(const S: WideString): WideString;
begin
  Result := S;
  if Result <> '' then
  begin
    if not IsWidePathDelimiter(Result, Length(Result)) then
    begin
      if Assigned(WideStrScan(PWideChar(Result), '/')) then
      begin
        Result := Result + '/';
      end
      else
      begin
        Result := Result + '\';
      end;
    end;
  end;
end;

function ExcludeWideTrailingDelimiter(const S: WideString): WideString;
begin
  Result := S;
  if IsWidePathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function GetWideCharRangeString(FirstChar, LastChar: WideChar): WideString;
var
  I: Integer;
  T: WideChar;
begin
  if LastChar < FirstChar then
  begin
    T := FirstChar;
    FirstChar := LastChar;
    LastChar := T;
  end;
  I := 1;
  SetLength(Result, Ord(LastChar) - Ord(FirstChar) + 1);
  for T := FirstChar to LastChar do
  begin
    Result[I] := T;
    Inc(I);
  end;
end;

function GetWideStringOf(Char: WideChar; Len: Cardinal): WideString;
begin
  SetLength(Result, Len);
  if Len > 0 then
  begin
    FillWideChar(Result[1], Len, Char);
  end;
end;

function WideStringReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TWideReplaceFlags): WideString;
var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
begin
  if wrfIgnoreCase in Flags then
  begin
    SearchStr := WideUpperCase(S);
    Patt := WideUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := WidePos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := WideCopy(NewStr, Offset + Length(OldPattern), Length(NewStr));
    if not (wrfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := WideCopy(SearchStr, Offset + Length(Patt), Length(SearchStr));
  end;
end;

{$O-}

(*
function WideStrComp(const S1, S2: PWideChar): Integer;
var P1,
    P2 : PWideChar;
begin
  result := 0;
  P1 := S1;
  P2 := S2;
  while (P1^ <> #0) and (P2^ <> #0) and (P1^ = P2^) do
  begin
    inc(P1);
    Inc(P2);
  end;
  if (P1^ = P2^) then
    result := 0
  else
  if P1^ < P2^ then
    result := -1
  else
  if P1^ > P2^ then
    result := 1;
end;
*)

function WideReplace;
var
  i: integer;
begin
  i := WidePos(SourceString, Str);
  if i = 0 then
  begin
    result := false;
    exit;
  end;
  WideDelete(Str, i, Length(SourceString));
  WideInsert(DestString, Str, i);
  result := true;
end;

function WideStrPos(const Str1, Str2: PWideChar): PWideChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX,EAX
        JE      @@2
        OR      EDX,EDX
        JE      @@2
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     AX,AX
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASW
        NOT     ECX
        DEC     ECX
        JE      @@2
        MOV     ESI,ECX
        MOV     EDI,EBX
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASW
        NOT     ECX
        SUB     ECX,ESI
        JBE     @@2
        MOV     EDI,EBX
        LEA     EBX,[ESI-1]
@@1:    MOV     ESI,EDX
        LODSW
        REPNE   SCASW
        JNE     @@2
        MOV     EAX,ECX
        PUSH    EDI
        MOV     ECX,EBX
        REPE    CMPSW
        POP     EDI
        MOV     ECX,EAX
        JNE     @@1
        LEA     EAX,[EDI-2]
        JMP     @@3
@@2:    XOR     EAX,EAX
@@3:    POP     EBX
        POP     ESI
        POP     EDI
end;


function WideCopy(S : WideString; SPos, SLen : integer) : WideString;
begin
  Result := Copy(S, SPos, SLen);
end;

procedure WideInsert(Text : WideString; var S : WideString; SPos : integer);
begin
  System.Insert(Text, S, SPos);
end;

procedure WideDelete(var S : WideString; SPos, SLen : integer);
begin
  System.Delete(S, SPos, SLen);
end;

function WideMakeString(FLen: Integer; Seq : WideString): WideString;
var i : integer;
begin
  SetLength(Result, FLen * Length(Seq));
  for i := 0 to FLen - 1 do
    Move(Seq[1], Result[Length(Seq) * i + 1], Length(Seq) * sizeof(WideChar));
end;

function WideLastPos(SubStr, Strn: WideString): integer;
var
  i,
    j: integer;
  ls, // total length of substring
    ld // length of string
    : integer;

begin
  result := 0;
  ls := Length(SubStr);
  ld := Length(Strn);
  if (ls > ld) or (ls = 0) or (ld = 0) then exit;
  for i := ld downto ls do
  begin
    j := ls;
    while j >= 1 do
    begin
      if Strn[i - ls + j] <> SubStr[j] then break;
      dec(j);
    end;
    if j = 0 then
    begin
      result := i - ls + 1;
      exit;
    end;
  end;
end;

{$endif}

procedure TStrDelete(var S : TElFString; SPos, SLen : integer);
{$ifdef ELPACK_UNICODE}
var S1 : WideString;
{$else}
var S1 : String;
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  S1 := S;
  WideDelete(S1, SPos, SLen);
  S := S1;
{$else}
  S1 := S;
  Delete(S1, SPos, SLen);
  S := S1;
{$endif}
end;

function TStrExtractStr(var S: TElFString; SPos, SLen: integer): TElFString;
{$ifdef ELPACK_UNICODE}
var S1 : WideString;
{$else}
var S1 : String;
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  S1 := S;
  result := WideExtractStr(S1, SPos, SLen);
  S := S1;
{$else}
  S1 := S;
  result := ExtractStr(S1, SPos, SLen);
  S := S1;
{$endif}
end;

procedure SetTStr(var S: TElFString; Buffer: PElFChar; Len: Integer);
{$ifdef ELPACK_UNICODE}
var S1 : WideString;
{$else}
var S1 : String;
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  S1 := S;
  SetWideString(S1, PWideChar(Buffer), Len);
  S := S1;
{$else}
  S1 := S;
  SetString(S1, Buffer, Len);
  S := S1;
{$endif}
end;

function GetCharRangeString(FirstChar, LastChar: Char): String;
var
  I: Integer;
  T: Char;
begin
  if LastChar < FirstChar then
  begin
    T := FirstChar;
    FirstChar := LastChar;
    LastChar := T;
  end;
  I := 1;
  SetLength(Result, Ord(LastChar) - Ord(FirstChar) + 1);
  for T := FirstChar to LastChar do
  begin
    Result[I] := T;
    Inc(I);
  end;
end;

{$ifdef ELPACK_UNICODE}
function ConvertUTF16toUTF8 ( var source: widestring; sourcelen: cardinal;
                              var target: string; targetlen: cardinal;
                              flags: ConversionFlags): ConversionResult;
const
  byteMask: UTF32 = $0BF;
  byteMark: UTF32 = $80;
var
  ch, ch2: UTF32;
  bytesToWrite: word;
  i, j: cardinal;
begin
  result := conversionOK;
  i := 1;
  j := 1;
  while i < sourcelen do
  begin
    // bytesToWrite := 0;
    ch := UTF32(source[i]);
    inc(i);
    { If we have a surrogate pair, convert to UTF32 first }
    if ((ch >= UNI_SUR_HIGH_START) and (ch <= UNI_SUR_HIGH_END) and (i < sourcelen)) then
    begin
      ch2 := UTF32(source[i]);
      if ((ch2 >= UNI_SUR_LOW_START) and (ch2 <= UNI_SUR_LOW_END)) then
      begin
        ch := ((ch - UNI_SUR_HIGH_START) shl halfShift) + (ch2 - UNI_SUR_LOW_START) + halfBase;
        inc(i);
      end
      else
        if (flags = strictConversion) then  { it's an unpaired high surrogate }
        begin
          result := sourceIllegal;
          break;
        end;
    end
    else
      if ((flags = strictConversion) and ((ch >= UNI_SUR_LOW_START) and (ch <= UNI_SUR_LOW_END))) then
      begin
        result := sourceIllegal;
        break;
      end;
    { Figure out how many bytes the result will require }
    if ch < UTF32($80) then
      bytesToWrite := 1
    else
    if ch < UTF32($800) then
      bytesToWrite := 2
    else
    if ch < UTF32($10000) then
      bytesToWrite := 3
    else
    if ch < UTF32($200000) then
      bytesToWrite := 4
    else
    begin
      bytesToWrite := 2;
      ch := UNI_REPLACEMENT_CHAR;
    end;
    j := j + bytesToWrite;
    if j > targetlen then
    begin
      result := targetExhausted;
      break;
    end;

    { note: everything falls through. }
    if bytesToWrite = 4 then
    begin
      dec(j);
      target[j] := char((ch or byteMark) and byteMask);
      ch := ch shr 6
    end;
    if bytesToWrite >= 3 then
    begin
      dec(j);
      target[j] := char((ch or byteMark) and byteMask);
      ch := ch shr 6
    end;
    if bytesToWrite >= 2 then
    begin
      dec(j);
      target[j] := char((ch or byteMark) and byteMask);
      ch := ch shr 6
    end;
    if bytesToWrite >= 1 then
    begin
      dec(j);
      target[j] := char(ch or firstByteMark[bytesToWrite]);
    end;
    j := j + bytesToWrite;
  end;
end;

{
  Utility routine to tell whether a sequence of bytes is legal UTF-8.
  This must be called with the length pre-determined by the first byte.
  If not calling this from ConvertUTF8to, then the length can be set by:
        length = trailingBytesForUTF8[source]+1;
  and the sequence is illegal right away if there aren't that many bytes
  available.
  If presented with a length > 4, this returns false.  The Unicode
  definition of UTF-8 goes up to 4-byte sequences.
}
function isLegalUTF8(source: string; length: cardinal): boolean;
var
  a: UTF8;
  src: string;
    i: cardinal;
begin
  src := source;
  i := 1 + length;
  result := false;
  if (length < 1) or (length > 4) then exit;
  { Everything else falls through when "true"... }
  if length = 4 then
  begin
    dec(i);
    a:= UTF8(src[i]);
    if (a < $80) or (a > $bf) then exit;
  end;
  if length >= 3 then
  begin
    dec(i);
    a:= UTF8(src[i]);
    if (a < $80) or (a > $bf) then exit;
  end;
  if length >= 2 then
  begin
    dec(i);
    a:= UTF8(src[i]);
    if (a > $bf) or
       ( (UTF8(source[1]) = $0e0) and (a < $0a0) ) or
       ( (UTF8(source[1]) = $0f0) and (a < $90) ) or
       ( (UTF8(source[1]) = $0f4) and (a > $8f) ) or
       (a < $80) then exit;
  end;
  if length >= 1 then
    if ((UTF8(source[1]) >= $80) and (UTF8(source[1]) < $0c2)) or (UTF8(source[1]) > $0f4) then exit;
  result := true;
end;

function ConvertUTF8toUTF16 ( var source: string; sourcelen: cardinal;
                              var target: widestring; targetlen: cardinal;
                              flags: ConversionFlags): ConversionResult;
var
  ch: UTF32;
  extraBytesToRead: word;
  i, j: cardinal;
begin
  result := conversionOK;
  i := 1;
  j := 1;
  while i < sourcelen do
  begin
    ch := 0;
    extraBytesToRead := trailingBytesForUTF8[UTF8(source[i])];
    if (i + extraBytesToRead) >= sourcelen then
    begin
      result := sourceExhausted;
      break;
    end;
    { Do this check whether lenient or strict }
    if (not isLegalUTF8(copy(source,i,extraBytesToRead+1), extraBytesToRead+1)) then
    begin
      result := sourceIllegal;
      break;
    end;
    { The cases all fall through.}
    if extraBytesToRead = 3 then
    begin
      ch := ch + UTF32(source[i]);
      inc(i);
      ch := ch shl 6;
    end;
    if (extraBytesToRead >= 2) and (extraBytesToRead < 4) then
    begin
      ch := ch + UTF32(source[i]);
      inc(i);
      ch := ch shl 6;
    end;
    if (extraBytesToRead >= 1) and (extraBytesToRead < 4) then
    begin
      ch := ch + UTF32(source[i]);
      inc(i);
      ch := ch shl 6;
    end;
    if (* (extraBytesToRead >= 0) and *) (extraBytesToRead < 4) then
    begin
      ch := ch + UTF32(source[i]);
      inc(i);
    end;
    ch := ch - offsetsFromUTF8[extraBytesToRead];
    if j >= targetlen then
    begin
      result := targetExhausted;
      break;
    end;
    if (ch <= UNI_MAX_BMP) then         { Target is a character <= 0xFFFF }
    begin
      if ((flags = strictConversion) and ((ch >= UNI_SUR_HIGH_START) and (ch <= UNI_SUR_LOW_END))) then
      begin
        result := sourceIllegal;
        break;
      end else
      begin
        target[j] := widechar(ch);  { normal case }
        inc(j);
      end;
    end
    else
      if (ch > UNI_MAX_UTF16) then
      begin
        if (flags = strictConversion) then
        begin
          result := sourceIllegal;
          i := i - extraBytesToRead;
        end else
        begin
          target[j] := widechar(UNI_REPLACEMENT_CHAR);
          inc(j);
        end;
      end
      else
      { target is a character in range 0xFFFF - 0x10FFFF. }
      begin
        if (j + 1) > targetlen then
        begin
          result := targetExhausted;
          break;
        end;
        ch := ch - halfBase;
        target[j] := widechar((ch shr halfShift) + UNI_SUR_HIGH_START);
        inc(j);
        target[j] := widechar((ch and halfMask) + UNI_SUR_LOW_START);
        inc(j);
      end;
  end;
end;

function isLegalUTF8Sequence(source: string; sourcelen: cardinal): boolean;
var
  length: cardinal;
begin
  length := trailingBytesForUTF8[byte(source[1])]+1;
  if length > sourcelen then
    result := false
  else
    result := isLegalUTF8(source, length);
end;
{$endif}

end.
