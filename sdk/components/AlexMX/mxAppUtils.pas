{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit mxAppUtils;

interface

{$I mx.INC}

uses Windows, Registry,
  Classes, Controls, Forms, IniFiles, Grids, mxVCLUtils;

function GetDefaultSection(Component: TComponent): string;
procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string; UseRegistry: Boolean);
function GetDefaultIniName: string;

type
  TOnGetDefaultIniName = function: string;

const
  OnGetDefaultIniName: TOnGetDefaultIniName = nil;

var
  DefCompanyName: string = '';
  RegUseAppTitle: Boolean = False;

function GetDefaultIniRegKey: string;

function FindForm(FormClass: TFormClass): TForm;
function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
function ShowDialog(FormClass: TFormClass): Boolean;
function InstantiateForm(FormClass: TFormClass; var Reference): TForm;

procedure SaveFormPlacement(Form: TForm; const IniFileName: string;
  UseRegistry: Boolean);
procedure RestoreFormPlacement(Form: TForm; const IniFileName: string;
  UseRegistry: Boolean);
procedure WriteFormPlacementReg(Form: TForm; IniFile: TRegIniFile;
  const Section: string);
procedure ReadFormPlacementReg(Form: TForm; IniFile: TRegIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);
procedure SaveMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
procedure RestoreMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
procedure RestoreGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
procedure SaveGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);

procedure WriteFormPlacement(Form: TForm; IniFile: TIniFile;
  const Section: string);
procedure ReadFormPlacement(Form: TForm; IniFile: TIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);
procedure SaveMDIChildren(MainForm: TForm; IniFile: TIniFile);
procedure RestoreMDIChildren(MainForm: TForm; IniFile: TIniFile);
procedure RestoreGridLayout(Grid: TCustomGrid; IniFile: TIniFile);
procedure SaveGridLayout(Grid: TCustomGrid; IniFile: TIniFile);

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;

function StrToIniStr(const Str: string): string;
function IniStrToStr(const Str: string): string;

function IniReadString(IniFile: TObject; const Section, Ident,
  Default: string): string;
procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
function IniReadInteger(IniFile: TObject; const Section, Ident: string;
  Default: Longint): Longint;
procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  Value: Longint);
function IniReadBool(IniFile: TObject; const Section, Ident: string;
  Default: Boolean): Boolean;
procedure IniWriteBool(IniFile: TObject; const Section, Ident: string;
  Value: Boolean);
procedure IniReadSections(IniFile: TObject; Strings: TStrings);
procedure IniEraseSection(IniFile: TObject; const Section: string);
procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);

procedure AppBroadcast(Msg, wParam: Longint; lParam: Longint);

procedure AppTaskbarIcons(AppOnly: Boolean);

{ Internal using utilities }

procedure InternalSaveGridLayout(Grid: TCustomGrid; IniFile: TObject;
  const Section: string);
procedure InternalRestoreGridLayout(Grid: TCustomGrid; IniFile: TObject;
  const Section: string);
procedure InternalSaveMDIChildren(MainForm: TForm; IniFile: TObject);
procedure InternalRestoreMDIChildren(MainForm: TForm; IniFile: TObject);

implementation

uses SysUtils, Messages, Consts, mxStrUtils, mxFileUtil, mxPlacemnt, RTLConsts;

function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then begin
    if Component is TCustomForm then Result := Component.ClassName
    else begin
      Result := Component.Name;
      if Component is TControl then begin
        F := GetParentForm(TControl(Component));
        if F <> nil then Result := F.ClassName + Result
        else begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end
      else begin
        Owner := Component.Owner;
        if Owner is TForm then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end
  else Result := '';
end;

function GetDefaultIniName: string;
begin
  if Assigned(OnGetDefaultIniName) then
    Result:= OnGetDefaultIniName
  else
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.INI'));
end;

function GetDefaultIniRegKey: string;
begin
  if RegUseAppTitle and (Application.Title <> '') then
    Result := Application.Title
  else Result := ExtractFileName(ChangeFileExt(Application.ExeName, ''));
  if DefCompanyName <> '' then
    Result := DefCompanyName + '\' + Result;
  Result := 'Software\' + Result;
end;

procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string; UseRegistry: Boolean);
var
  I: Integer;
begin
  IniFileName := EmptyStr;
  with Control do
    if Owner is TCustomForm then
      for I := 0 to Owner.ComponentCount - 1 do
        if (Owner.Components[I] is TFormPlacement) then begin
          IniFileName := TFormPlacement(Owner.Components[I]).IniFileName;
          Break;
        end;
  Section := GetDefaultSection(Control);
  if IniFileName = EmptyStr then
    if UseRegistry then IniFileName := GetDefaultIniRegKey
    else
    IniFileName := GetDefaultIniName;
end;

function FindForm(FormClass: TFormClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is FormClass then begin
      Result := Screen.Forms[I];
      Break;
    end;
  end;
end;

function InternalFindShowForm(FormClass: TFormClass;
  const Caption: string; Restore: Boolean): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is FormClass then
      if (Caption = '') or (Caption = Screen.Forms[I].Caption) then begin
        Result := Screen.Forms[I];
        Break;
      end;
  end;
  if Result = nil then begin
    Application.CreateForm(FormClass, Result);
    if Caption <> '' then Result.Caption := Caption;
  end;
  with Result do begin
    if Restore and (WindowState = wsMinimized) then WindowState := wsNormal;
    Show;
  end;
end;

function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
begin
  Result := InternalFindShowForm(FormClass, Caption, True);
end;

function ShowDialog(FormClass: TFormClass): Boolean;
var
  Dlg: TForm;
begin
  Application.CreateForm(FormClass, Dlg);
  try
    Result := Dlg.ShowModal in [mrOk, mrYes];
  finally
    Dlg.Free;
  end;
end;

function InstantiateForm(FormClass: TFormClass; var Reference): TForm;
begin
  if TForm(Reference) = nil then
    Application.CreateForm(FormClass, Reference);
  Result := TForm(Reference);
end;

function StrToIniStr(const Str: string): string;
var
  Buffer: array[0..4095] of Char;
  B, S: PChar;
begin
  if Length(Str) > SizeOf(Buffer) then
    raise Exception.Create(ResStr(SLineTooLong));
  S := PChar(Str);
  try
    B := Buffer;
    while S^ <> #0 do
      case S^ of
        #13, #10:
          begin
            if (S^ = #13) and (S[1] = #10) then Inc(S)
            else if (S^ = #10) and (S[1] = #13) then Inc(S);
            B^ := '\';
            Inc(B);
            B^ := 'n';
            Inc(B);
            Inc(S);
          end;
      else
        B^ := S^;
        Inc(B);
        Inc(S);
      end;
  finally
  end;
  B^ := #0;
  Result := StrPas(Buffer);
end;

function IniStrToStr(const Str: string): string;
var
  Buffer: array[0..4095] of Char;
  B, S: PChar;
begin
  if Length(Str) > SizeOf(Buffer) then
    raise Exception.Create(ResStr(SLineTooLong));
  S := PChar(Str);
  try
    B := Buffer;
    while S^ <> #0 do
      if (S[0] = '\') and (S[1] = 'n') then
      begin
        B^ := #13;
        Inc(B);
        B^ := #10;
        Inc(B);
        Inc(S);
        Inc(S);
      end
      else
      begin
        B^ := S^;
        Inc(B);
        Inc(S);
      end;
  finally
  end;
  B^ := #0;
  Result := StrPas(Buffer);
end;

const
{ The following strings should not be localized }
  siFlags     = 'Flags';
  siShowCmd   = 'ShowCmd';
  siMinMaxPos = 'MinMaxPos';
  siNormPos   = 'NormPos';
  siPixels    = 'PixelsPerInch';
  siMDIChild  = 'MDI Children';
  siListCount = 'Count';
  siItem      = 'Item%d';

function IniReadString(IniFile: TObject; const Section, Ident,
  Default: string): string;
begin
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadString(Section, Ident, Default)
  else
  if IniFile is TIniFile then
    Result := TIniFile(IniFile).ReadString(Section, Ident, Default)
  else Result := Default;
end;

procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
var
  S: string;
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteString(Section, Ident, Value)
  else begin
    S := Value;
    if S <> '' then begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteString(Section, Ident, S);
  end;
end;

function IniReadInteger(IniFile: TObject; const Section, Ident: string;
  Default: Longint): Longint;
begin
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else
  if IniFile is TIniFile then
    Result := TIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else Result := Default;
end;

procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  Value: Longint);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteInteger(Section, Ident, Value)
  else
  if IniFile is TIniFile then
    TIniFile(IniFile).WriteInteger(Section, Ident, Value);
end;

function IniReadBool(IniFile: TObject; const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadBool(Section, Ident, Default)
  else
  if IniFile is TIniFile then
    Result := TIniFile(IniFile).ReadBool(Section, Ident, Default)
  else Result := Default;
end;

procedure IniWriteBool(IniFile: TObject; const Section, Ident: string;
  Value: Boolean);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteBool(Section, Ident, Value)
  else
  if IniFile is TIniFile then
    TIniFile(IniFile).WriteBool(Section, Ident, Value);
end;

procedure IniEraseSection(IniFile: TObject; const Section: string);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).EraseSection(Section)
  else
  if IniFile is TIniFile then
    TIniFile(IniFile).EraseSection(Section);
end;

procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).DeleteKey(Section, Ident)
  else if IniFile is TIniFile then
    TIniFile(IniFile).DeleteKey(Section, Ident);
end;

procedure IniReadSections(IniFile: TObject; Strings: TStrings);
begin
  if IniFile is TIniFile then
    TIniFile(IniFile).ReadSections(Strings)
  else if IniFile is TRegIniFile then
    TRegIniFile(IniFile).ReadSections(Strings);
end;

procedure InternalSaveMDIChildren(MainForm: TForm; IniFile: TObject);
var
  I: Integer;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(ResStr(SNoMDIForm));
  IniEraseSection(IniFile, siMDIChild);
  if MainForm.MDIChildCount > 0 then begin
    IniWriteInteger(IniFile, siMDIChild, siListCount,
      MainForm.MDIChildCount);
    for I := 0 to MainForm.MDIChildCount - 1 do
      IniWriteString(IniFile, siMDIChild, Format(siItem, [I]),
        MainForm.MDIChildren[I].ClassName);
  end;
end;

procedure InternalRestoreMDIChildren(MainForm: TForm; IniFile: TObject);
var
  I: Integer;
  Count: Integer;
  FormClass: TFormClass;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(ResStr(SNoMDIForm));
  StartWait;
  try
    Count := IniReadInteger(IniFile, siMDIChild, siListCount, 0);
    if Count > 0 then begin
      for I := 0 to Count - 1 do begin
        FormClass := TFormClass(GetClass(IniReadString(IniFile, siMDIChild,
          Format(siItem, [Count - I - 1]), '')));
        if FormClass <> nil then
          InternalFindShowForm(FormClass, '', False);
      end;
    end;
  finally
    StopWait;
  end;
end;

procedure SaveMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
begin
  InternalSaveMDIChildren(MainForm, IniFile);
end;

procedure RestoreMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
begin
  InternalRestoreMDIChildren(MainForm, IniFile);
end;

procedure SaveMDIChildren(MainForm: TForm; IniFile: TIniFile);
begin
  InternalSaveMDIChildren(MainForm, IniFile);
end;

procedure RestoreMDIChildren(MainForm: TForm; IniFile: TIniFile);
begin
  InternalRestoreMDIChildren(MainForm, IniFile);
end;

procedure InternalSaveGridLayout(Grid: TCustomGrid; IniFile: TObject;
  const Section: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    IniWriteInteger(IniFile, Section, Format(siItem, [I]),
      TDrawGrid(Grid).ColWidths[I]);
end;

procedure InternalRestoreGridLayout(Grid: TCustomGrid; IniFile: TObject;
  const Section: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    TDrawGrid(Grid).ColWidths[I] := IniReadInteger(IniFile, Section,
      Format(siItem, [I]), TDrawGrid(Grid).ColWidths[I]);
end;

procedure RestoreGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
begin
  InternalRestoreGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

procedure SaveGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
begin
  InternalSaveGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

procedure RestoreGridLayout(Grid: TCustomGrid; IniFile: TIniFile);
begin
  InternalRestoreGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

procedure SaveGridLayout(Grid: TCustomGrid; IniFile: TIniFile);
begin
  InternalSaveGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

function CrtResString: string;
begin
  Result := Format('(%dx%d)', [GetSystemMetrics(SM_CXSCREEN),
    GetSystemMetrics(SM_CYSCREEN)]);
end;

function ReadPosStr(IniFile: TObject; const Section, Ident: string): string;
begin
  Result := IniReadString(IniFile, Section, Ident + CrtResString, '');
  if Result = '' then Result := IniReadString(IniFile, Section, Ident, '');
end;

procedure WritePosStr(IniFile: TObject; const Section, Ident, Value: string);
begin
  IniWriteString(IniFile, Section, Ident + CrtResString, Value);
  IniWriteString(IniFile, Section, Ident, Value);
end;

procedure InternalWriteFormPlacement(Form: TForm; IniFile: TObject;
  const Section: string);
var
  Placement: TWindowPlacement;
begin
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do begin
    if (Form = Application.MainForm) and IsIconic(Application.Handle) then
      ShowCmd := SW_SHOWMINIMIZED;
    if (FormStyle = fsMDIChild) and (WindowState = wsMinimized) then
      Flags := Flags or WPF_SETMINPOSITION;
    IniWriteInteger(IniFile, Section, siFlags, Flags);
    IniWriteInteger(IniFile, Section, siShowCmd, ShowCmd);
    IniWriteInteger(IniFile, Section, siPixels, Screen.PixelsPerInch);
    WritePosStr(IniFile, Section, siMinMaxPos, Format('%d,%d,%d,%d',
      [ptMinPosition.X, ptMinPosition.Y, ptMaxPosition.X, ptMaxPosition.Y]));
    WritePosStr(IniFile, Section, siNormPos, Format('%d,%d,%d,%d',
      [rcNormalPosition.Left, rcNormalPosition.Top, rcNormalPosition.Right,
      rcNormalPosition.Bottom]));
  end;
end;

procedure WriteFormPlacementReg(Form: TForm; IniFile: TRegIniFile;
  const Section: string);
begin
  InternalWriteFormPlacement(Form, IniFile, Section);
end;

procedure WriteFormPlacement(Form: TForm; IniFile: TIniFile;
  const Section: string);
begin
  InternalWriteFormPlacement(Form, IniFile, Section);
end;

procedure SaveFormPlacement(Form: TForm; const IniFileName: string;
  UseRegistry: Boolean);
var
  IniFile: TObject;
begin
  if UseRegistry then IniFile := TRegIniFile.Create(IniFileName)
  else IniFile := TIniFile.Create(IniFileName);
  try
    InternalWriteFormPlacement(Form, IniFile, Form.ClassName);
  finally
    IniFile.Free;
  end;
end;

  {$HINTS OFF}

type

{*******************************************************}
{ !! ATTENTION Nasty implementation                     }
{*******************************************************}
{                                                       }
{ This class definition was copied from FORMS.PAS.      }
{ It is needed to access some private fields of TForm.  }
{                                                       }
{ Any changes in the underlying classes may cause       }
{ errors in this implementation!                        }
{                                                       }
{*******************************************************}

  TNastyForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
{$IFDEF RX_D4}
    FSizeChanging: Boolean;
{$ENDIF}
    FWindowState: TWindowState; { !! }
  end;

  THackComponent = class(TComponent);
  {$HINTS ON}

procedure InternalReadFormPlacement(Form: TForm; IniFile: TObject;
  const Section: string; LoadState, LoadPosition: Boolean);
const
  Delims = [',',' '];
var
  PosStr: string;
  Placement: TWindowPlacement;
  WinState: TWindowState;
  DataFound: Boolean;
begin
  if not (LoadState or LoadPosition) then Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do begin
    if not IsWindowVisible(Form.Handle) then
      ShowCmd := SW_HIDE;
    if LoadPosition then begin
      DataFound := False;
      Flags := IniReadInteger(IniFile, Section, siFlags, Flags);
      PosStr := ReadPosStr(IniFile, Section, siMinMaxPos);
      if PosStr <> '' then begin
        DataFound := True;
        ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
        ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
        ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
        ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
      end;
      PosStr := ReadPosStr(IniFile, Section, siNormPos);
      if PosStr <> '' then begin
        DataFound := True;
        rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims), Left);
        rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims), Top);
        rcNormalPosition.Right := StrToIntDef(ExtractWord(3, PosStr, Delims), Left + Width);
        rcNormalPosition.Bottom := StrToIntDef(ExtractWord(4, PosStr, Delims), Top + Height);
      end;
      if Screen.PixelsPerInch <> IniReadInteger(IniFile, Section, siPixels,
        Screen.PixelsPerInch) then DataFound := False;
      if DataFound then begin
        if not (BorderStyle in [bsSizeable, bsSizeToolWin]) then
          rcNormalPosition := Rect(rcNormalPosition.Left, rcNormalPosition.Top,
            rcNormalPosition.Left + Width, rcNormalPosition.Top + Height);
        if rcNormalPosition.Right > rcNormalPosition.Left then begin
          if (Position in [poScreenCenter {$IFDEF RX_D4}, poDesktopCenter {$ENDIF}]) and
            not (csDesigning in ComponentState) then
          begin
            THackComponent(Form).SetDesigning(True);
            try
              Position := poDesigned;
            finally
              THackComponent(Form).SetDesigning(False);
            end;
          end;
          SetWindowPlacement(Handle, @Placement);
        end;
      end;
    end;
    if LoadState then begin
      WinState := wsNormal;
      { default maximize MDI main form }
      if ((Application.MainForm = Form) {$IFDEF RX_D4} or
        (Application.MainForm = nil) {$ENDIF}) and ((FormStyle = fsMDIForm) or
        ((FormStyle = fsNormal) and (Position = poDefault))) then
        WinState := wsMaximized;
      ShowCmd := IniReadInteger(IniFile, Section, siShowCmd, SW_HIDE);
      case ShowCmd of
        SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
          WinState := wsNormal;
        SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
          WinState := wsMinimized;
        SW_MAXIMIZE: WinState := wsMaximized;
      end;
      if (WinState = wsMinimized) and ((Form = Application.MainForm)
        {$IFDEF RX_D4} or (Application.MainForm = nil) {$ENDIF}) then
      begin
        TNastyForm(Form).FWindowState := wsNormal;
        PostMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        Exit;
      end;
      if FormStyle in [fsMDIChild, fsMDIForm] then
        TNastyForm(Form).FWindowState := WinState
      else WindowState := WinState;
    end;
    Update;
  end;
end;

procedure ReadFormPlacementReg(Form: TForm; IniFile: TRegIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);
begin
  InternalReadFormPlacement(Form, IniFile, Section, LoadState, LoadPosition);
end;

procedure ReadFormPlacement(Form: TForm; IniFile: TIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);
begin
  InternalReadFormPlacement(Form, IniFile, Section, LoadState, LoadPosition);
end;

procedure RestoreFormPlacement(Form: TForm; const IniFileName: string;
  UseRegistry: Boolean);
var
  IniFile: TObject;
begin
  if UseRegistry then begin
    IniFile := TRegIniFile.Create(IniFileName);
  {$IFDEF RX_D5} 
    TRegIniFile(IniFile).Access := KEY_READ;
  {$ENDIF}
  end
  else 
    IniFile := TIniFile.Create(IniFileName);
  try
    InternalReadFormPlacement(Form, IniFile, Form.ClassName, True, True);
  finally
    IniFile.Free;
  end;
end;

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;
var
  CurrentName: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to MaxInt do begin
    CurrentName := Format(FileNameMask, [I]);
    if not FileExists(NormalDir(Path) + CurrentName) then begin
      Result := CurrentName;
      Exit;
    end;
  end;
end;

procedure AppBroadcast(Msg, wParam: Longint; lParam: Longint);
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    SendMessage(Screen.Forms[I].Handle, Msg, wParam, lParam);
end;

procedure AppTaskbarIcons(AppOnly: Boolean);
var
  Style: Longint;
begin
  Style := GetWindowLong(Application.Handle, GWL_STYLE);
  if AppOnly then Style := Style or WS_CAPTION
  else Style := Style and not WS_CAPTION;
  SetWindowLong(Application.Handle, GWL_STYLE, Style);
  if AppOnly then SwitchToWindow(Application.Handle, False);
end;

end.