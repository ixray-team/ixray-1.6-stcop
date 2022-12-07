{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS, Alex Ionov       }
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

Version History

04/23/2002

Fixed the case when version info resource was not present in the file

*)

unit ElVerInfo;

interface

uses
  Windows, Classes, ShellAPI;

type
  TVersionAttribute = (vaDebug, vaPatched, vaPreRelease, vaPrivateBuild,
    vaSpecialBuild);
  TVersionAttributes = set of TVersionAttribute;

  TElVersionInfo = class(TComponent)
  private
    FBuffer: string;
    FFileName: string;
    FFixedFileInfo: PVSFixedFileInfo;
    FLanguage: string;
    function GetAttributes: TVersionAttributes;
    function GetBuild: integer;
    function GetLanguage: string;
    function GetMajorVersion: integer;
    function GetMinorVersion: integer;
    function GetPredefined(Index: integer): string;
    function GetRelease: integer;
    function GetValue(AName: string): string;
    procedure SetAttributes(const Value: TVersionAttributes);
    procedure SetDummy(const Value: string);
    procedure SetDummyEx(Index: integer; Value: string);
    procedure SetFileName(const Value: string);
    procedure SetDummyInt(const Value: integer);
    function StoreFileName: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Refresh;
    property Values[Name: string]: string read GetValue; default;
  published
    property Attributes: TVersionAttributes read GetAttributes write SetAttributes stored False;
    property Build: integer read GetBuild write SetDummyInt stored False;
    property Comments: string index 9 read GetPredefined write SetDummyEx stored False;
    property CompanyName: string index 0 read GetPredefined write SetDummyEx stored False;
    property FileDescription: string index 1 read GetPredefined write SetDummyEx stored False;
    property FileName: string read FFileName write SetFileName stored StoreFileName;
    property FileVersion: string index 2 read GetPredefined write SetDummyEx stored False;
    property InternalName: string index 3 read GetPredefined write SetDummyEx stored False;
    property Language: string read GetLanguage write SetDummy stored False;
    property LegalCopyright: string index 4 read GetPredefined write SetDummyEx stored False;
    property LegalTrademarks: string index 5 read GetPredefined write SetDummyEx stored False;
    property MajorVersion: integer read GetMajorVersion write SetDummyInt stored False;
    property MinorVersion: integer read GetMinorVersion write SetDummyInt stored False;
    property OriginalFilename: string index 6 read GetPredefined write SetDummyEx stored False;
    property ProductName: string index 7 read GetPredefined write SetDummyEx stored False;
    property ProductVersion: string index 8 read GetPredefined write SetDummyEx stored False;
    property Release: integer read GetRelease write SetDummyInt stored False;
  end;

implementation

uses
  SysUtils;

{ TVmVersionInfo }

const
  PredefinedFields: array [0..9] of string = (
    'CompanyName',
    'FileDescription',
    'FileVersion',
    'InternalName',
    'LegalCopyright',
    'LegalTrademarks',
    'OriginalFilename',
    'ProductName',
    'ProductVersion',
    'Comments'
  );

constructor TElVersionInfo.Create(AOwner: TComponent);
begin
  inherited;
  FileName := '';
end;

function TElVersionInfo.GetAttributes: TVersionAttributes;
begin
  Result := [];
  if FFixedFileInfo = nil then
    exit;
  if (VS_FF_DEBUG and FFixedFileInfo.dwFileFlags) <> 0 then Include(Result, vaDebug);
  if (VS_FF_PATCHED and FFixedFileInfo.dwFileFlags) <> 0 then Include(Result, vaPatched);
  if (VS_FF_PRERELEASE and FFixedFileInfo.dwFileFlags) <> 0 then Include(Result, vaPreRelease);
  if (VS_FF_PRIVATEBUILD and FFixedFileInfo.dwFileFlags) <> 0 then Include(Result, vaPrivateBuild);
  if (VS_FF_SPECIALBUILD and FFixedFileInfo.dwFileFlags) <> 0 then Include(Result, vaSpecialBuild);
end;

function TElVersionInfo.GetBuild: Integer;
begin
  if FFixedFileInfo = nil then
  begin
    result := 0;
    exit;
  end;
  Result := LongRec(FFixedFileInfo.dwFileVersionLS).Lo;
end;

function TElVersionInfo.GetLanguage: string;
begin
  SetLength(Result, 64);
  SetLength(Result, VerLanguageName(StrToIntDef('$' + Copy(FLanguage, 1, 4), $0409),
    PChar(Result), 64));
end;

function TElVersionInfo.GetMajorVersion: Integer;
begin
  if FFixedFileInfo = nil then
  begin
    result := 0;
    exit;
  end;
  Result := LongRec(FFixedFileInfo.dwFileVersionMS).Hi;
end;

function TElVersionInfo.GetMinorVersion: Integer;
begin
  if FFixedFileInfo = nil then
  begin
    result := 0;
    exit;
  end;
  Result := LongRec(FFixedFileInfo.dwFileVersionMS).Lo;
end;

function TElVersionInfo.GetPredefined(Index: integer): string;
begin
  if FFixedFileInfo = nil then
  begin
    result := '';
    exit;
  end;
  Result := GetValue(PredefinedFields[Index]);
end;

function TElVersionInfo.GetRelease: Integer;
begin
  if FFixedFileInfo = nil then
  begin
    result := 0;
    exit;
  end;
  Result := LongRec(FFixedFileInfo.dwFileVersionLS).Hi;
end;

function TElVersionInfo.GetValue(AName: string): string;
var
  Size: DWORD;
  Value: PChar;
begin
  Result := '';
  if Length(FBuffer) > 0 then
  begin
    Value := nil;
    VerQueryValue(PChar(FBuffer), PChar('\StringFileInfo\' + FLanguage + '\' + AName),
      Pointer(Value), Size);
    Result := Value;  
  end
end;

procedure TElVersionInfo.Refresh;
begin
  SetFileName(FFileName);
end;

procedure TElVersionInfo.SetAttributes(const Value: TVersionAttributes);
begin
end;

procedure TElVersionInfo.SetDummy(const Value: string);
begin
end;

procedure TElVersionInfo.SetDummyEx(Index: integer; Value: string);
begin
end;

procedure TElVersionInfo.SetDummyInt(const Value: integer);
begin
end;

procedure TElVersionInfo.SetFileName(const Value: string);
var
  Size, Dummy: DWORD;
  P: pointer;
  FileName: string;
begin
  if Value = '' then
  begin
    if csDesigning in ComponentState then
    begin
      SetLength(FileName, 256);
      SetLength(FileName, GetModuleFileName(hInstance, PChar(FileName), 256));
    end
    else
      FileName := ParamStr(0);
  end
  else
    FileName := Value;
  FFileName := Value;
  try
    SetLength(FBuffer, 0);
    Size := GetFileVersionInfoSize(PChar(FileName), Dummy);
    if Size > 0 then
    begin
      SetLength(FBuffer, Size);
      FLanguage := '040904E4';
      Win32Check(GetFileVersionInfo(PChar(FileName), Dummy, Size, PChar(FBuffer)));
      if VerQueryValue(PChar(FBuffer), '\VarFileInfo\Translation', P, Size) then
      begin
        FLanguage := IntToHex(PLongint(P)^, 8);
        FLanguage := Copy(FLanguage, 5, 4) + Copy(FLanguage, 1, 4);
      end;
      VerQueryValue(Pointer(FBuffer), '\', Pointer(FFixedFileInfo), Size);
    end
    else
      FFixedFileInfo := nil;
  finally
  end;
end;

function TElVersionInfo.StoreFileName: Boolean;
begin
  Result := (FFileName <> ParamStr(0)) and (FFileName <> '');
end;

end.
