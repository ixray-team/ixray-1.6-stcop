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

Version History
  
03/06/2002

  Added unicode hint

12/02/2001

  Fixed painting in Windows XP with styles disabled

*)

unit ElDriveCombo;

interface

uses

  Controls,
  Messages,
  Windows,
  Graphics,
  ShellApi,
  StdCtrls,
  Forms,
  
  SysUtils,
  Classes,
{$ifdef VCL_6_USED}
Types,
{$endif}

  ElTmSchema,
  ElUxTheme,
  ElTools,
  ElVCLUtils,
  ElACtrls;

type

  TElDriveComboOption = (dcoDisplayFloppy,
                         dcoDisplayNetwork,
                         dcoDisplayHard,
                         dcoDisplayCD,
                         dcoDisplayRAM);

  TElDriveComboOptions = set of TElDriveComboOption;

  TElDriveComboBox = class(TElAdvancedComboBox)
  private
    FDummyInt   : integer;
    FDummyChar  : char;
  protected
    FDrive: Char;
    FTextCase: TElTextCase;
    FOptions: TElDriveComboOptions;

    {$ifndef CLX_USED}
    procedure DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;
    {$else}
    function DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState): Boolean; override;
    {$endif}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure FillItems;
    procedure SetDrive(Value: Char);
    procedure CreateWnd; override;
    procedure SetTextCase(Value: TElTextCase);
    procedure SetOptions(Value: TElDriveComboOptions);
    procedure Change; override;
  public
    constructor Create(AOwner :TComponent); override;
    property Drive: Char read FDrive write SetDrive;
    property ItemIndex : char read FDummyChar;
  published
    property Items : integer read FDummyInt;
    property Style: Integer read FDummyInt;
    property ItemHeight : integer read FDummyInt;

    property TextCase: TElTextCase read FTextCase write SetTextCase default etcLowercase;
    property Options: TElDriveComboOptions read FOptions write SetOptions;
  end;

implementation

{$ifndef CLX_USED}
procedure TElDriveComboBox.DrawItem(Index: Integer; Rect: TRect; State:
    {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
{$else}
function TElDriveComboBox.DrawItem(Index: Integer; Rect: TRect; State:
    TOwnerDrawState): Boolean;
{$endif}
var S : string;
    fi : TSHFileInfo;
    iw,
    ih : integer;
    sid: integer;
begin
  Canvas.Brush.Style := bsSolid;
  if odSelected in State then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := Color;

  Canvas.FillRect(Rect);

  if ItemHeight < GetSystemMetrics(SM_CXICON) then
  begin
    SHGetFileInfo(PChar(inherited Items[Index][1] + ':\'), 0, fi, sizeof(fi), SHGFI_ICON or SHGFI_SMALLICON);
    iw := GetSystemMetrics(SM_CXSMICON);
    ih := GetSystemMetrics(SM_CYSMICON);
  end
  else
  begin
    SHGetFileInfo(PChar(inherited Items[Index][1] + ':\'), 0, fi, sizeof(fi), SHGFI_ICON or SHGFI_LARGEICON);
    iw := GetSystemMetrics(SM_CXICON);
    ih := GetSystemMetrics(SM_CYICON);
  end;

  DrawIconEx(Canvas.Handle, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - ih) div 2, fi.hIcon, Min(ItemHeight, ih), Min(ItemHeight, iw), 0, 0, DI_NORMAL);

  Inc(Rect.Left, iw + iw div 4);

  S := inherited Items[Index];
  case TextCase of
    etcLowercase: S := Lowercase(S);
    etcUppercase: S := Uppercase(S);
  end;
  if (ThemesAvailable and IsThemeActive) then
  begin
    if not Enabled then
      sid := ETS_DISABLED
    else
    if odSelected in State then
      sid := ETS_SELECTED
    else
    if FMouseOver then
      sid := ETS_HOT
    else
      sid := ETS_NORMAL;
    DrawThemeTextTo('EDIT', Canvas.Handle, EP_EDITTEXT, sid, PWideChar(WideString(s)), Length(WideString(s)), DT_VCENTER or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE, 0, Rect);
  end
  else
    DrawText(Canvas.Handle, PChar(S), Length(S), Rect, DT_VCENTER or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE);

  {$ifdef CLX_USED}
  result := true;
  {$endif}
end;

procedure TElDriveComboBox.FillItems;
var
    i : Cardinal;
    c : char;
    sv: integer;
    Drives: set of 0..25;
    Name  : string;
    dt : DWORD;

    function GetVolumeName(Volume : string) : string;
    var NameBuf: array[0..255] of char;
    begin
      GetVolumeInformation(PChar(Volume), NameBuf, 255, @i, i, i, nil, 0);
      SetString(Result, NameBuf, StrLen(NameBuf));
    end;

    function GetNetworkVolumeName(Volume : string) : string;
    var
      Buf : array [0..MAX_PATH] of Char;
      Drv : array [0..3] of Char;
      BufSize: Cardinal;
    begin
      BufSize := sizeof(buf);
      StrPCopy(Drv, Volume);

      if WNetGetConnection(Drv, Buf, BufSize) = WN_SUCCESS then
        result := StrPas(Buf)
      else
        result := '';
    end;

begin
  inherited Items.Clear;
  sv := Windows.SetErrorMode(SEM_FAILCRITICALERRORS);
  Integer(Drives) := GetLogicalDrives;
  for c := 'A' to 'Z' do
  begin
    if Ord(C) - Ord('A') in Drives then
    begin
      dt := GetDriveType(PChar(C + ':\'));
      if (dt = DRIVE_REMOVABLE) and not (dcoDisplayFloppy in Options) then
        Continue;
      if (dt = DRIVE_REMOTE) and not (dcoDisplayNetwork in Options) then
        Continue;
      if (dt = DRIVE_CDROM) and not (dcoDisplayCD in Options) then
        Continue;
      if (dt = DRIVE_FIXED) and not (dcoDisplayHard in Options) then
        Continue;
      if (dt = DRIVE_RAMDISK) and not (dcoDisplayRAM in Options) then
        Continue;
      if dt = DRIVE_REMOVABLE then
        Name := ''
      else
      if dt = DRIVE_REMOTE then
        Name := GetNetworkVolumeName(C + ':')
      else
        Name := GetVolumeName(C + ':\');

      inherited Items.Add(Format('%s: [%s]', [C, Name]));
    end;
  end;
  Windows.SetErrorMode(sv);
 
end;

procedure TElDriveComboBox.SetDrive(Value: Char);
var i, j : integer;
begin
  if FDrive <> Value then
  begin
    Value := Upcase(Value);
    if Value in ['A'..'Z'] then
    begin
      j := inherited Items.Count - 1;
      for i := 0 to j do
      begin
        if inherited Items[i][1] = Value then
        begin
          FDrive := Value;
          inherited ItemIndex := i;
          exit;
        end;
      end;
    end;
  end;
end;

constructor TElDriveComboBox.Create(AOwner :TComponent);
begin
  inherited;
  FDrive := 'C';
  inherited Style := csOwnerDrawFixed; 
  FTextCase  := etcLowercase;
  FOptions := [dcoDisplayFloppy, dcoDisplayNetwork, dcoDisplayHard, dcoDisplayCD, dcoDisplayRAM];
  inherited ItemHeight := Max(Abs(Font.Height) + 2 + GetSystemMetrics(SM_CXEDGE) * 2, Max(ItemHeight, GetSystemMetrics(SM_CYSMICON)));
end;

procedure TElDriveComboBox.CreateWnd;
var D : Char;
    b : array[0..MAX_PATH] of char;
begin
  inherited;
  FillItems;
  D := FDrive;
  GetCurrentDirectory(MAX_PATH, b);
  FDrive := B[0];
  SetDrive(D);
end;

procedure TElDriveComboBox.SetTextCase(Value: TElTextCase);
begin
  if FTextCase <> Value then
  begin
    FTextCase := Value;
    FillItems;
  end;
end;

procedure TElDriveComboBox.SetOptions(Value: TElDriveComboOptions);
var D : Char;
    b : array[0..MAX_PATH] of char;
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    D := FDrive;
    FillItems;
    GetCurrentDirectory(MAX_PATH, b);
    FDrive := B[0];
    SetDrive(D);
  end;
end;

procedure TElDriveComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  inherited ItemHeight := Max(Abs(Font.Height) + 2 + GetSystemMetrics(SM_CXEDGE) * 2, Max(ItemHeight, GetSystemMetrics(SM_CYSMICON)));
end;

procedure TElDriveComboBox.Change;
begin
  inherited;
  if inherited ItemIndex = -1 then
    FDrive := #0
  else
    FDrive := inherited Items[inherited ItemIndex][1];
end;

end.
