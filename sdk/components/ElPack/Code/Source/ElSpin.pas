{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

(*

Version History

07/05/2002

  OnChange is called when the value is changed with keyboard in ElFloatSpinEdit

05/27/2002

  Finally fixed button border behaviour

03/27/2002

  Mouse wheel didn't cause OnChange event. Fixed.

01/26/2002

  Changed BtnFlat behavior

01/22/2002

  ReadOnly flag was ignored when pressing arrow buttons. Fixed.

01/06/2002

  Fixed Alignment property, spoiled in 3.02 release

01/01/2002

  Fixed some problems with painting borders when focus is moved

07/17/2001

  Fixed the problem with cursor position when typing the values over MaxLength

03/29/2001

  Fixed the problem with input of negative numbers


02/28/2001

  ButtonDirection and ButtonType properties added
  ButtonWidth property added

12/16/2000

  Setting a value for the first time programmatically in run-time didn't work.
  Fixed.

*)
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
unit ElSpin;

interface

uses
  SysUtils,
  Classes,
{$ifdef MSWINDOWS}
  Controls,
  Messages,
  Windows,
  Graphics,
  Forms,
  StdCtrls,
  Menus,
{$ifdef VCL_6_USED}
Types,
{$endif}
{$else}
  QForms,
  QStdCtrls,
  QControls,
  QGraphics,
  Qt,
  Types,
  QMenus,
{$endif}
  ElACtrls,
  ElVCLUtils,
  ElTools,
  ElEdits,
  ElSpinBtn;

type

  TElSpinEditError = class(Exception)
  end;

  TElSpinEdit = class(TCustomElEdit)
  protected
    FModified : Boolean;
    FBtnWidth : Integer;
    FMouseOver : boolean;
    FLargeIncrement : Integer;
    FIncrement : Integer;
    FSaveValue : integer;
    FSavePos : integer;
    FSaveLen : integer;
    FChanging : boolean;
    FValue : Integer;
    FMaxValue : Integer;
    FMinValue : Integer;
    FAllowEdit : Boolean;
    FButton : TElSpinButton;
    FOnUpClick,
    FOnDownClick : TNotifyEvent;
    FButtonWidth: Integer;
    FUseButtonWidth: Boolean;
    FValueUndefined: Boolean;
    FReadOnly: Boolean;
    FButtonThinFrame: Boolean;
    FButtonFlat: Boolean;
    procedure SetValue(newValue : Integer);
    procedure SetMaxValue(newValue : Integer);
    procedure SetMinValue(newValue : Integer);
    procedure SetAllowEdit(newValue : Boolean);
    procedure SetIncrement(newValue : integer);
    procedure SetEditRect(Value : TRect); override;

    {$ifdef MSWINDOWS}
    procedure WMKeyDown(var Msg : TWMKeyDown); message WM_KEYDOWN;
    procedure CMEnter(var Msg : TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg : TCMExit); message CM_EXIT;
    procedure WMCreate(var Msg : TWMCreate); message WM_CREATE;
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure CMEnabledChanged(var Msg : TMessage); message CM_ENABLEDCHANGED;
    procedure WMCut(var Msg : TMessage); message WM_CUT;
    procedure WMPaste(var Msg : TMessage); message WM_PASTE;
    procedure WMMButtonDown(var Msg : TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure CMFontChanged(var Msg : TMessage); message CM_FONTCHANGED;
    procedure WMContextMenu(var Msg : TMessage); message WM_CONTEXTMENU;
    {$endif}

    procedure SpinUpClick(Sender : TObject; Increment : Double); virtual;
    procedure SpinDownClick(Sender : TObject; Increment : Double); virtual;
    procedure SpinDrag(Sender : TObject; NewValue : Double); virtual;
    procedure SpinStart(Sender : TObject; var StartValue : Double);
    procedure SetReadOnly(Value: Boolean);
    
    procedure SetFlat(const Value : Boolean); override;

    {$ifdef MSWINDOWS}
    procedure CreateParams(var Params : TCreateParams); override;
    {$endif}
    procedure KeyPress(var Key : char); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure Click; override;
    function GetPopupMenu: TPopupMenu; override;

    procedure SetModified(newValue : Boolean); virtual;
    procedure SetButtonWidth(const Value: Integer);
    procedure SetUseButtonWidth(Value: Boolean);
    function GetButtonType: TElSpinBtnType;
    procedure SetButtonType(Value: TElSpinBtnType);
    function GetButtonDirection: TElSpinBtnDir;
    procedure SetButtonDirection(Value: TElSpinBtnDir);
    procedure SetValueUndefined(Value: Boolean);
    function GetUseXPThemes: Boolean;
    procedure SetUseXPThemes(const Value: Boolean); override;
    procedure SetButtonThinFrame(Value: Boolean);
    procedure SetLineBorderActiveColor(Value: TColor); override;
    procedure SetLineBorderInactiveColor(Value: TColor); override;
    procedure SetButtonFlat(Value: Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    {$ifdef MSWINDOWS}
    procedure CreateWnd; override;
    {$endif}
    procedure Change; override;

    procedure Loaded; override;

    property MouseOver : boolean read FMouseOver;

    property Modified : Boolean read FModified write SetModified;  { Published }
  published
    property Value : Integer read FValue write SetValue default 0;
    property MaxValue : Integer read FMaxValue write SetMaxValue default 100;
    property MinValue : Integer read FMinValue write SetMinValue default 0;
    property AllowEdit : Boolean read FAllowEdit write SetAllowEdit default True;
    property Increment : Integer read FIncrement write SetIncrement default 1;
    property LargeIncrement : Integer read FLargeIncrement write FLargeIncrement default 10;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth stored
        FUseButtonWidth nodefault;
    property UseButtonWidth: Boolean read FUseButtonWidth write SetUseButtonWidth
        default false;
    property ButtonType: TElSpinBtnType read GetButtonType write SetButtonType
        default sbtUpDown;
    property ButtonDirection: TElSpinBtnDir read GetButtonDirection write
        SetButtonDirection default sbdUpDown;
    property ValueUndefined: Boolean read FValueUndefined write SetValueUndefined
        default false;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property UseXPThemes: Boolean read GetUseXPThemes write SetUseXPThemes default true;
    property ButtonThinFrame: Boolean read FButtonThinFrame write SetButtonThinFrame default true;

    property OnUpClick   : TNotifyEvent read FOnUpClick write FOnUpClick;
    property OnDownClick : TNotifyEvent read FOnDownClick write FOnDownClick;

    property Alignment;
    property TopMargin;
    property LeftMargin;
    property RightMargin;
    property BorderSides;
    property MaxLength;
    property Transparent;
    property HandleDialogKeys default false;
    property HideSelection;
    {$ifdef ELPACK_COMPLETE}
    property ImageForm;
    {$endif}
    property ChangeDisabledText;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnChange;
    property OnSelectionChange;

    {$IFDEF VCL_4_USED}
    property Anchors;
    {$ENDIF}
    {$IFDEF VCL_4_USED}
    property Constraints;
    {$ENDIF}
    {$IFDEF VCL_4_USED}
    {$ifdef MSWINDOWS}
    property DragKind;
    {$endif}
    {$ENDIF}
    {$IFDEF VCL_4_USED}
    {$ifdef MSWINDOWS}
    property BiDiMode;
    {$endif}
    {$ENDIF}
    property ActiveBorderType;
    property Align;
    property AutoSelect;
    property AutoSize;
    property Background;
    property BorderStyle;
    property Color;
    {$ifdef MSWINDOWS}
    property Ctl3D;
    property DragCursor;
    {$endif}
    property DragMode;
    property Enabled;
    property Font;
    property Flat;
    property InactiveBorderType;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;

    property ParentColor;
    {$ifdef MSWINDOWS}
    property ParentCtl3D;
    {$endif}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabStop default True;
    property TabOrder;
    property ShowHint;
    property UseBackground;
    property Visible;

    {$ifdef MSWINDOWS}
    {$IFDEF VCL_4_USED}
    property OnStartDock;
    property OnEndDock;
    {$ENDIF}
    {$IFDEF VCL_5_USED}
    property OnContextPopup;
    {$ENDIF}
    {$endif}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property OnStartDrag;
    property ButtonFlat: Boolean read FButtonFlat write SetButtonFlat default false;
  end;

  TElFloatSpinEdit = class(TCustomElEdit)
  protected
    FAllowEdit: Boolean;
    FBtnWidth: Integer;
    FButton: TElSpinButton;
    FButtonWidth: Integer;
    FChanging: Boolean;
    FIncrement: Double;
    FLargeIncrement: Double;
    FMaxValue: Double;
    FMinValue: Double;
    FModified: Boolean;
    FMouseOver: Boolean;
    FOnUpClick, FOnDownClick: TNotifyEvent;
    FReadOnly: Boolean;
    FSaveLen: Integer;
    FSavePos: Integer;
    FSaveValue: Double;
    FUseButtonWidth: Boolean;
    FValue: Double;
    FValueUndefined: Boolean;
    FButtonThinFrame: Boolean;
    procedure SetAllowEdit(newValue : Boolean);
    procedure SetIncrement(newValue: Double);
    procedure SetMaxValue(newValue: Double);
    procedure SetMinValue(newValue: Double);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValue(newValue: Double);
    procedure SpinDownClick(Sender : TObject; Increment : Double); virtual;
    procedure SpinDrag(Sender : TObject; NewValue : Double); virtual;
    procedure SpinStart(Sender : TObject; var StartValue : Double);
    procedure SpinUpClick(Sender : TObject; Increment : Double); virtual;

    {$ifdef MSWINDOWS}
    procedure WMKeyDown(var Msg : TWMKeyDown); message WM_KEYDOWN;
    procedure CMEnter(var Msg : TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg : TCMExit); message CM_EXIT;
    procedure WMCreate(var Msg : TWMCreate); message WM_CREATE;
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure CMEnabledChanged(var Msg : TMessage); message CM_ENABLEDCHANGED;
    procedure WMCut(var Msg : TMessage); message WM_CUT;
    procedure WMPaste(var Msg : TMessage); message WM_PASTE;
    procedure WMMButtonDown(var Msg : TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure CMFontChanged(var Msg : TMessage); message CM_FONTCHANGED;
    procedure WMContextMenu(var Msg : TMessage); message WM_CONTEXTMENU;
    {$endif}

    procedure Click; override;
    {$ifdef MSWINDOWS}
    procedure CreateParams(var Params : TCreateParams); override;
    {$endif}
    function GetButtonDirection: TElSpinBtnDir;
    function GetButtonType: TElSpinBtnType;
    function GetPopupMenu: TPopupMenu; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure KeyPress(var Key : char); override;
    procedure SetButtonDirection(Value: TElSpinBtnDir);
    procedure SetButtonType(Value: TElSpinBtnType);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetFlat(const Value : Boolean); override;
    procedure SetModified(newValue : Boolean); virtual;
    procedure SetUseButtonWidth(Value: Boolean);
    procedure SetValueUndefined(Value: Boolean);
    function GetUseXPThemes: Boolean;
    procedure SetUseXPThemes(const Value: Boolean); override;
    procedure SetButtonThinFrame(Value: Boolean);
    procedure SetLineBorderActiveColor(Value: TColor); override;
    procedure SetLineBorderInactiveColor(Value: TColor); override;
    procedure SetEditRect(Value : TRect); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Change; override;
    {$ifdef MSWINDOWS}
    procedure CreateWnd; override;
    {$endif}
    procedure Loaded; override;
    property Modified: Boolean read FModified write SetModified;
    property MouseOver: Boolean read FMouseOver;
  published
    property AllowEdit: Boolean read FAllowEdit write SetAllowEdit default True;
    property ButtonDirection: TElSpinBtnDir read GetButtonDirection write
        SetButtonDirection default sbdUpDown;
    property ButtonType: TElSpinBtnType read GetButtonType write SetButtonType
        default sbtUpDown;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth stored
        FUseButtonWidth nodefault;
    property Increment: Double read FIncrement write SetIncrement;
    property LargeIncrement: Double read FLargeIncrement write FLargeIncrement;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property UseButtonWidth: Boolean read FUseButtonWidth write SetUseButtonWidth
        default false;
    property Value: Double read FValue write SetValue;
    property ValueUndefined: Boolean read FValueUndefined write SetValueUndefined
        default false;

    property Alignment;
    property TopMargin;
    property LeftMargin;
    property RightMargin;
    property BorderSides;
    property MaxLength;
    property Transparent;
    property HandleDialogKeys default false;
    property HideSelection;
    {$ifdef ELPACK_COMPLETE}
    property ImageForm;
    {$endif}
    property ChangeDisabledText;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnChange;
    property OnSelectionChange;

    {$IFDEF VCL_4_USED}
    property Anchors;
    {$ENDIF}
    {$IFDEF VCL_4_USED}
    property Constraints;
    {$ENDIF}
    {$IFDEF VCL_4_USED}
    {$ifdef MSWINDOWS}
    property DragKind;
    {$endif}
    {$ENDIF}
    {$IFDEF VCL_4_USED}
    {$ifdef MSWINDOWS}
    property BiDiMode;
    {$endif}
    {$ENDIF}
    property ActiveBorderType;
    property Align;
    property AutoSelect;
    property AutoSize;
    property Background;
    property BorderStyle;
    property Color;
    {$ifdef MSWINDOWS}
    property Ctl3D;
    property DragCursor;
    {$endif}
    property DragMode;
    property Enabled;
    property Font;
    property Flat;
    property InactiveBorderType;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;

    property ParentColor;
    {$ifdef MSWINDOWS}
    property ParentCtl3D;
    {$endif}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabStop default True;
    property TabOrder;
    property ShowHint;
    property UseBackground;
    property Visible;

    {$ifdef MSWINDOWS}
    {$IFDEF VCL_4_USED}
    property OnStartDock;
    property OnEndDock;
    {$ENDIF}
    {$IFDEF VCL_5_USED}
    property OnContextPopup;
    {$ENDIF}
    {$endif}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property OnStartDrag;
    property UseXPThemes: Boolean read GetUseXPThemes write SetUseXPThemes default 
        true;
    property ButtonThinFrame: Boolean read FButtonThinFrame write 
        SetButtonThinFrame default true;
  end;

implementation

(*const
  FBtnWidth = 12;*)

function TElSpinEdit.GetPopupMenu: TPopupMenu;
var p : TPoint;
begin
  if FButton.SpinDragging then
  begin
    GetCursorPos(p);
    p := FButton.ScreenToClient(p);
    FButton.StopDragging;
  end;
  result := inherited GetPopupMenu;
end;

procedure TElSpinEdit.SetIncrement(newValue : integer);
begin
  FIncrement := newValue;
  FButton.Increment := newValue;
end;

procedure TElSpinEdit.SetValue(newValue : Integer);
begin
  if (FValue <> newValue) then
  begin
    if not (csLoading in ComponentState) then
    begin
      FValueUndefined := false;
      if newValue < FMinValue then
         FValue := FMinValue
      else
      if newValue > FMaxValue then
         FValue := FMaxValue
      else
         FValue := newValue;
    end else
       FValue := newValue;
    FChanging := true;
    Modified := true;
    FSaveValue := FValue;
    if not (csLoading in ComponentState) then
      Text := IntToStr(FValue);
    FChanging := false;
  end; {if}
end; {SetValue}

procedure TElSpinEdit.SetMaxValue(newValue : Integer);
begin
  if (FMaxValue <> newValue) then
  begin
    if (csLoading in ComponentState) or (newValue >= FMinValue) then
    begin
      FMaxValue := newValue;
      if FValue > FMaxValue then SetValue(FMaxValue);
    end;
  end; {if}
end; {SetMaxValue}

procedure TElSpinEdit.SetMinValue(newValue : Integer);
begin
  if (FMinValue <> newValue) and (newValue <= FMaxValue) then
  begin
    FMinValue := newValue;
    if FValue < FMinValue then SetValue(FMinValue);
  end; {if}
end; {SetMinValue}

procedure TElSpinEdit.SetAllowEdit(newValue : Boolean);
begin
  if (FAllowEdit <> newValue) then
  begin
    FAllowEdit := newValue;
    inherited ReadOnly := not FAllowEdit;
  end; {if}
end; {SetAllowEdit}

{$ifdef MSWINDOWS}
procedure TElSpinEdit.WMKeyDown(var Msg : TWMKeyDown);
begin
  if Msg.CharCode <> 0 then
  begin
    FSavePos := SelStart;
    FSaveLen := SelLength;
  end;
  inherited;
end; {WMKeyDown}

procedure TElSpinEdit.CreateParams(var Params : TCreateParams);
const
  Alignments : array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited;
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or Alignments[Alignment];
end; {CreateParams}
{$endif}

procedure TElSpinEdit.CreateWnd;
begin
  inherited;
end; {CreateWnd}

procedure TElSpinEdit.SetEditRect;
var
  R : TRect;
begin
  Dec(Value.Right, FBtnWidth);
  inherited SetEditRect(Value);
  if not HandleAllocated then exit; 
  R := Value;
  R.Left := R.Right;
  R.Right := ClientWidth;
  FButton.BoundsRect := R;
  FButton.Visible := true;
end; {SetEditRect}

procedure TElSpinEdit.CMEnter(var Msg : TCMEnter);
begin
  inherited;
end; {CMEnter}

procedure TElSpinEdit.CMExit(var Msg : TCMExit);
var
  i : integer;
begin
  inherited;
  try
    if (not FValueUndefined) then
    begin
      i := StrToInt(Text);
      if i > FMaxValue then
        Value := FMaxValue
      else if i < FMinValue then
        Value := FMinValue
      else
         Value := i;
    end
    else
    begin
      FChanging := true;
      Text := '';
      FChanging := false;
    end;
  except
    Value := FMinValue;
    FChanging := true;
    Text := IntToStr(FValue);
    FChanging := false;
  end;
  if Flat and (not FMouseOver) then
  begin
    FButton.OldStyled := false;
    FButton.Invalidate;
  end;
end; {CMExit}

procedure TElSpinEdit.Change;
var i: integer;
    a: integer;
begin
  if (Parent = nil) or (csLoading in ComponentState) then exit;

  if ((FChanging) or (Text = '')) then
  begin
    inherited;
    exit;
  end;
  if Text <> '-' then
  try
    i := StrToInt(Text);
    if not InRange(FMinValue, FMaxValue, i) then
       raise TElSpinEditError.Create('Value out of range');
    FSaveValue := i;
    FSavePos := SelStart;

    Value := i;
  except
    a := FSavePos;
    text := IntToStr(FSaveValue);
    FSavePos := a;
    SelStart := FSavePos;
  end;
  inherited;
  SelStart := FSavePos;
end; {Change}

procedure TElSpinEdit.KeyPress(var Key : char);
const
  AllowedKeys = ['0'..'9', '-', #8];
begin
  if (not (Key in AllowedKeys)) or ((Key = '-') and (SelStart > 0)) then Key := #0;
  inherited KeyPress(Key);
end; {KeyPress}

procedure TElSpinEdit.WMCreate(var Msg : TWMCreate);
begin
  inherited;
  if ValueUndefined then
    Text := ''
  else
    Text := IntToStr(Value);
end;

procedure TElSpinEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  if not ReadOnly then
  begin
    if (Key = VK_UP) and (Shift = []) then
    begin
      Value := Value + Increment;
      if Assigned(FOnUpClick) then FOnUpClick(Self);
      Change;
      SelStart := Length(Text);
    end
    else if (Key = VK_DOWN) and (Shift = []) then
    begin
      Value := Value - Increment;
      if Assigned(FOnDownClick) then FOnDownClick(Self);
      Change;
      SelStart := Length(Text);
    end
    else if (Key = VK_PRIOR) and (Shift = []) then
    begin
      Value := Value + FLargeIncrement;
      if Assigned(FOnUpClick) then FOnUpClick(Self);
      Change;
      SelStart := Length(Text);
    end
    else if (Key = VK_NEXT) and (Shift = []) then
    begin
      Value := Value - FLargeIncrement;
      if Assigned(FOnDownClick) then FOnDownClick(Self);
      Change;
      SelStart := Length(Text);
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TElSpinEdit.Click;
begin
  FSavePos := SelStart;
  inherited;
end;

procedure TElSpinEdit.SetFlat(const Value : Boolean);
{ Sets data member FFlat to newValue. }
begin
  inherited;
  if Value then
     FButton.OldStyled := (FMouseOver or Focused)
  else
     FButton.OldStyled := true;
  FButton.MoneyFlat := Flat and (InactiveBorderType = fbtColorLineBorder) and (ActiveBorderType = fbtColorLineBorder);
  FButton.Invalidate;
end; { SetFlat }

procedure TElSpinEdit.CMMouseEnter(var Msg : TMessage); { private }
begin
  inherited;
  FMouseOver := true;
  FButton.OldStyled := (not Flat) or (not ButtonThinFrame);
end; { CMMouseEnter }

procedure TElSpinEdit.CMMouseLeave(var Msg : TMessage); { private }
begin
  inherited;
  FMouseOver := false;
  FButton.OldStyled := (not Flat) or ((not ButtonThinFrame) and Focused);
end; { CMMouseLeave }

procedure TElSpinEdit.WMSetFocus(var Msg : TWMSetFocus); { private }
begin
  inherited;
  FButton.OldStyled := (not Flat) or (not ButtonThinFrame);
end; { WMSetFocus }

procedure TElSpinEdit.WMKillFocus(var Msg : TWMKillFocus); { private }
begin
  inherited;
  FButton.OldStyled := (not Flat) or ((not ButtonThinFrame) and MouseOver);
end; { WMKillFocus }

procedure TElSpinEdit.WMMouseWheel(var Msg: TWMMouseWheel);
var
  Dy : integer;
  sl : integer;
begin
  if IsWinNT or IsWin98 then SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @sl, SPIF_SENDCHANGE) else sl := 3;
  if sl = 0 then sl := 1;
  Dy := Msg.WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  if Dy <> 0 then
  begin
    SetValue(FValue + Dy * FIncrement);
    Change;
  end;
end; { WMMouseWheel }

procedure TElSpinEdit.CMEnabledChanged(var Msg : TMessage);
begin
  inherited;
  invalidate;
  FButton.Enabled := Enabled;
end;

procedure TElSpinEdit.WMCut(var Msg : TMessage);
begin
  if not FAllowEdit then exit;
  inherited;
end;

procedure TElSpinEdit.WMPaste(var Msg : TMessage);
begin
  if not FAllowEdit then exit;
  inherited;
end;

procedure TElSpinEdit.Loaded;  { public }
begin
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  if (FValue < FMinValue) then
    FValue := FMinValue;
  if (FValue > FMaxValue) then
    FValue := FMaxValue;
  if (not ValueUndefined) then
  begin
    FSaveValue := FValue;
    FChanging := true;
    Text := IntToStr(FValue);
    FChanging := false;
  end;
  inherited;
end;  { Loaded }

procedure TElSpinEdit.WMSize(var Msg : TWMSize);
begin
  inherited;
end;

procedure TElSpinEdit.WMMButtonDown(var Msg : TWMMButtonDown);  { private }
begin
  inherited;
  if CanFocus then SetFocus;
end;  { WMMButtonDown }

procedure TElSpinEdit.CMFontChanged(var Msg : TMessage);  { private }
begin
  inherited;
end;  { CMFontChanged }

procedure TElSpinEdit.WMContextMenu(var Msg : TMessage);  { private }
var p : TPoint;
begin
  if FButton.SpinDragging then
  begin
    GetCursorPos(p);
    p := FButton.ScreenToClient(p);
    FButton.StopDragging;
  end;
  inherited;
end;  { WMContextMenu }

procedure TElSpinEdit.SpinUpClick(Sender : TObject; Increment : Double);
begin
  if not ReadOnly then
  begin
    Value := Value + Trunc(Increment);
    if CanFocus and (Windows.GetFocus <> Handle) then SetFocus;
    SelStart := 0;
    SelLength := Length(Text);
    if Assigned(FOnUpClick) then FOnUpClick(Self);
    Change;
  end;
end;

procedure TElSpinEdit.SpinDownClick(Sender : TObject; Increment : Double);
begin
  if not ReadOnly then
  begin
    Value := Value - Trunc(Increment);
    if CanFocus and (Windows.GetFocus <> Handle) then SetFocus;
    SelStart := 0;
    SelLength := Length(Text);
    if Assigned(FOnDownClick) then FOnDownClick(Self);
    Change;
  end;
end;

procedure TElSpinEdit.SpinDrag(Sender : TObject; NewValue : Double);
begin
  if not ReadOnly then
  begin
    Value := Trunc(NewValue);
    Change;
  end;
end;

procedure TElSpinEdit.SpinStart(Sender : TObject; var StartValue : Double);
begin
  StartValue := Value;
end;

procedure TElSpinEdit.SetModified(newValue : Boolean);
begin
  if (FModified <> newValue) then
  begin
    FModified := newValue;
    inherited Modified := newValue;
    Change;
  end;  { if }
end;  { SetModified }

destructor TElSpinEdit.Destroy;
begin
  FButton.Free;
  FButton := nil;
  inherited Destroy;
end; {Destroy}

constructor TElSpinEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FBtnWidth := GetSystemMetrics(SM_CXVSCROLL);
  FButtonWidth := FBtnWidth;
   FButtonThinFrame := true;

   HandleDialogKeys := false;
   FValue := 0;
   FMaxValue := 100;
   FMinValue := 0;
  FSaveValue := 0;
   FAllowEdit := true;
   FIncrement := 1;
   FLargeIncrement := 10;
  FButton := TElSpinButton.Create(Self);
  FButton.Parent := Self;
  FButton.Increment := FIncrement;
  FButton.OnUpClick := SpinUpClick;
  FButton.OnDownClick := SpinDownClick;
  FButton.OnSpinDrag := SpinDrag;
  FButton.OnSpinStart := SpinStart;
  FButton.Increment := 1;
  FButton.OldStyled := true;
   TabStop := true;
  AlignBottom := true;
end; {Create}

procedure TElSpinEdit.SetButtonWidth(const Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    if FUSeButtonWidth then
    begin
      FBtnWidth := FButtonWidth;
      SetEditRect(ClientRect);
    end;
  end;
end;

procedure TElSpinEdit.SetUseButtonWidth(Value: Boolean);
begin
  if FUseButtonWidth <> Value then
  begin
    FUseButtonWidth := Value;
    if Value then
      FBtnWidth := FButtonWidth
    else
      FBtnWidth := GetSystemMetrics(SM_CXVSCROLL);
    SetEditRect(ClientRect);
  end;
end;

function TElSpinEdit.GetButtonType: TElSpinBtnType;
begin
  Result := FButton.ButtonType;
end;

procedure TElSpinEdit.SetButtonType(Value: TElSpinBtnType);
begin
  FButton.ButtonType := Value;
end;

function TElSpinEdit.GetButtonDirection: TElSpinBtnDir;
begin
  Result := FButton.ButtonDirection;
end;

procedure TElSpinEdit.SetButtonDirection(Value: TElSpinBtnDir);
begin
  FButton.ButtonDirection := Value;
end;

procedure TElSpinEdit.SetValueUndefined(Value: Boolean);
begin
  if FValueUndefined <> Value then
  begin
    FValueUndefined := Value;
    if csLoading in cOmponentState then
      exit;
    if FValueUndefined then
    begin
      FChanging := true;
      Text := '';
      FChanging := false;
    end
    else
    begin
      FChanging := true;
      Text := IntToStr(FValue);
      FChanging := false;
    end;
  end;
end;

procedure TElSpinEdit.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if FReadOnly then
    begin
      inherited ReadOnly := true;
      // FButton.Enabled := false;
    end
    else
    begin
      // FButton.Enabled := false;
      inherited ReadOnly := not AllowEdit;
    end;
  end;
end;

function TElSpinEdit.GetUseXPThemes: Boolean;
begin
  Result := FButton.UseXPThemes;
end;

procedure TElSpinEdit.SetUseXPThemes(const Value: Boolean);
begin
  inherited;
  FButton.UseXPThemes := Value;
end;

procedure TElSpinEdit.SetButtonThinFrame(Value: Boolean);
begin
  if FButtonThinFrame <> Value then
  begin
    FButtonThinFrame := Value;
    FButton.OldStyled := (not Flat) or not (ButtonThinFrame or not (MouseOver or Focused));
  end;
end;

procedure TElSpinEdit.SetLineBorderActiveColor(Value: TColor);
begin
  if LineBorderActiveColor <> Value then
  begin
    inherited;
    FButton.MoneyFlatActiveColor := Value;
    FButton.MoneyFlat := Flat and (InactiveBorderType = fbtColorLineBorder) and (ActiveBorderType = fbtColorLineBorder);
  end;            
end;

procedure TElSpinEdit.SetLineBorderInactiveColor(Value: TColor);
begin
  if LineBorderInactiveColor <> Value then
  begin
    inherited;
    FButton.MoneyFlatInactiveColor := Value;
    FButton.MoneyFlat := Flat and (InactiveBorderType = fbtColorLineBorder) and (ActiveBorderType = fbtColorLineBorder);
  end;
end;

procedure TElSpinEdit.SetButtonFlat(Value: Boolean);
begin
  if FButtonFlat <> Value then
  begin
    FButtonFlat := Value;
    FButton.Flat := Value;
  end;
end;

constructor TElFloatSpinEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FBtnWidth := GetSystemMetrics(SM_CXVSCROLL);
  FButtonWidth := FBtnWidth;
  FButtonThinFrame := true;

  HandleDialogKeys := false;
  FValue := 0;
  FMaxValue := 100;
  FMinValue := 0;
  FSaveValue := 0;
  FAllowEdit := true;
  FIncrement := 1;
  FLargeIncrement := 10;
  FButton := TElSpinButton.Create(Self);
  FButton.Parent := Self;
  FButton.Increment := FIncrement;
  FButton.OnUpClick := SpinUpClick;
  FButton.OnDownClick := SpinDownClick;
  FButton.OnSpinDrag := SpinDrag;
  FButton.OnSpinStart := SpinStart;
  TabStop := true;
  AlignBottom := true;
end; {Create}

destructor TElFloatSpinEdit.Destroy;
begin
  FButton.Free;
  FButton := nil;
  inherited Destroy;
end; {Destroy}

procedure TElFloatSpinEdit.Change;
var i: Double;
    a: integer;
begin
  if (Parent = nil) or (csLoading in ComponentState) then exit;

  if ((FChanging) or (Text = '')) then
  begin
    inherited;
    exit;
  end;
  if Text <> '-' then
  try
    i := StrToFloat(Text);
    if not InRangeF(FMinValue, FMaxValue, i) then
       raise TElSpinEditError.Create('Value out of range');
    FSaveValue := i;
    FSavePos := SelStart;

    Value := i;
  except
    a := FSavePos;
    text := FloatToStr(FSaveValue);
    FSavePos := a;
    SelStart := FSavePos;
  end;
  inherited;
end; {Change}

procedure TElFloatSpinEdit.Click;
begin
  FSavePos := SelStart;
  inherited;
end;

procedure TElFloatSpinEdit.CMEnabledChanged(var Msg : TMessage);
begin
  inherited;
  Invalidate;
  FButton.Enabled := Enabled;
end;

procedure TElFloatSpinEdit.CMEnter(var Msg : TCMEnter);
begin
  inherited;
end; {CMEnter}

procedure TElFloatSpinEdit.CMExit(var Msg : TCMExit);
var
  i : Double;
begin
  inherited;
  try
    if (not FValueUndefined) then
    begin
      i := StrToFloat(Text);
      if i > FMaxValue then
        Value := FMaxValue
      else if i < FMinValue then
        Value := FMinValue
      else
        Value := i;
    end
    else
    begin
      FChanging := true;
      Text := '';
      FChanging := false;
    end;
  except
    Value := FMinValue;
    FChanging := true;
    Text := FloatToStr(FValue);
    FChanging := false;
  end;
  if Flat and (not FMouseOver) then
  begin
    FButton.OldStyled := false;
    FButton.Invalidate;
  end;
end; {CMExit}

procedure TElFloatSpinEdit.CMFontChanged(var Msg : TMessage);
begin
  inherited;
end;  { CMFontChanged }

procedure TElFloatSpinEdit.CMMouseEnter(var Msg : TMessage);
begin
  inherited;
  FMouseOver := true;
  FButton.OldStyled := (not Flat) or (not ButtonThinFrame);
end; { CMMouseEnter }

procedure TElFloatSpinEdit.CMMouseLeave(var Msg : TMessage);
begin
  inherited;
  FMouseOver := false;
  FButton.OldStyled := (not Flat) or ((not ButtonThinFrame) and Focused);
end; { CMMouseLeave }

{$ifdef MSWINDOWS}
procedure TElFloatSpinEdit.CreateParams(var Params : TCreateParams);
const
  Alignments : array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited;
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or Alignments[Alignment];
end; {CreateParams}
{$endif}

procedure TElFloatSpinEdit.CreateWnd;
begin
  inherited;
end; {CreateWnd}

function TElFloatSpinEdit.GetButtonDirection: TElSpinBtnDir;
begin
  Result := FButton.ButtonDirection;
end;

function TElFloatSpinEdit.GetButtonType: TElSpinBtnType;
begin
  Result := FButton.ButtonType;
end;

function TElFloatSpinEdit.GetPopupMenu: TPopupMenu;
var p : TPoint;
begin
  if FButton.SpinDragging then
  begin
    GetCursorPos(p);
    p := FButton.ScreenToClient(p);
    FButton.StopDragging;
  end;
  result := inherited GetPopupMenu;
end;

procedure TElFloatSpinEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  if not ReadOnly then
  begin
    if (Key = VK_UP) and (Shift = []) then
    begin
      Value := Value + Increment;
      if Assigned(FOnUpClick) then FOnUpClick(Self);
      Change;
    end
    else
    if (Key = VK_DOWN) and (Shift = []) then
    begin
      Value := Value - Increment;
      if Assigned(FOnDownClick) then FOnDownClick(Self);
      Change;
    end
    else
    if (Key = VK_PRIOR) and (Shift = []) then
    begin
      Value := Value + FLargeIncrement;
      if Assigned(FOnUpClick) then FOnUpClick(Self);
      Change;
    end
    else
    if (Key = VK_NEXT) and (Shift = []) then
    begin
      Value := Value - FLargeIncrement;
      if Assigned(FOnDownClick) then FOnDownClick(Self);
      Change;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TElFloatSpinEdit.KeyPress(var Key : char);
const
  AllowedKeys = ['0'..'9', '-', #8, '.', ','];
begin
  if (not (Key in AllowedKeys)) or ((Key = '-') and (SelStart > 0)) then Key := #0;
  inherited KeyPress(Key);
end; {KeyPress}

procedure TElFloatSpinEdit.Loaded;
begin
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  if (FValue < FMinValue) then
    FValue := FMinValue;
  if (FValue > FMaxValue) then
    FValue := FMaxValue;
  if (not ValueUndefined) then
  begin
    FSaveValue := FValue;
    FChanging := true;
    Text := FloatToStr(FValue);
    FChanging := false;
  end;
  inherited;
end;  { Loaded }

procedure TElFloatSpinEdit.SetAllowEdit(newValue : Boolean);
begin
  if (FAllowEdit <> newValue) then
  begin
    FAllowEdit := newValue;
    inherited ReadOnly := not FAllowEdit;
  end; {if}
end; {SetAllowEdit}

procedure TElFloatSpinEdit.SetButtonDirection(Value: TElSpinBtnDir);
begin
  FButton.ButtonDirection := Value;
end;

procedure TElFloatSpinEdit.SetButtonType(Value: TElSpinBtnType);
begin
  FButton.ButtonType := Value;
end;

procedure TElFloatSpinEdit.SetButtonWidth(const Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    if FUSeButtonWidth then
    begin
      FBtnWidth := FButtonWidth;
      SetEditRect(ClientRect);
    end;
  end;
end;

procedure TElFloatSpinEdit.SetFlat(const Value : Boolean);
{ Sets data member FFlat to newValue. }
begin
  inherited;
  if Value then
     FButton.OldStyled := (FMouseOver or Focused)
  else
     FButton.OldStyled := true;
  FButton.MoneyFlat := Flat and (InactiveBorderType = fbtColorLineBorder) and (ActiveBorderType = fbtColorLineBorder);
  FButton.Invalidate;
end; { SetFlat }

procedure TElFloatSpinEdit.SetIncrement(newValue: Double);
begin
  FIncrement := newValue;
  FButton.Increment := newValue;
end;

procedure TElFloatSpinEdit.SetMaxValue(newValue: Double);
begin
  if (FMaxValue <> newValue) then
  begin
    if (csLoading in ComponentState) or (newValue >= FMinValue) then
    begin
      FMaxValue := newValue;
      if FValue > FMaxValue then SetValue(FMaxValue);
    end;
  end; {if}
end; {SetMaxValue}

procedure TElFloatSpinEdit.SetMinValue(newValue: Double);
begin
  if (FMinValue <> newValue) and (newValue <= FMaxValue) then
  begin
    FMinValue := newValue;
    if FValue < FMinValue then SetValue(FMinValue);
  end; {if}
end; {SetMinValue}

procedure TElFloatSpinEdit.SetModified(newValue : Boolean);
begin
  if (FModified <> newValue) then
  begin
    FModified := newValue;
    inherited Modified := newValue;
    Change;
  end;  { if }
end;  { SetModified }

procedure TElFloatSpinEdit.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if FReadOnly then
    begin
      inherited ReadOnly := true;
      FButton.Enabled := false;
    end
    else
    begin
      FButton.Enabled := false;
      inherited ReadOnly := not AllowEdit;
    end;
  end;
end;

procedure TElFloatSpinEdit.SetUseButtonWidth(Value: Boolean);
begin
  if FUseButtonWidth <> Value then
  begin
    FUseButtonWidth := Value;
    if Value then
      FBtnWidth := FButtonWidth
    else
      FBtnWidth := GetSystemMetrics(SM_CXVSCROLL);
    SetEditRect(ClientRect);
  end;
end;

procedure TElFloatSpinEdit.SetValue(newValue: Double);
begin
  if (FValue <> newValue) then
  begin
    if not (csLoading in ComponentState) then
    begin
      FValueUndefined := false;
      if newValue < FMinValue then
         FValue := FMinValue
      else
      if newValue > FMaxValue then
         FValue := FMaxValue
      else
         FValue := newValue;
    end else
       FValue := newValue;
    FChanging := true;
    Modified := true;
    FSaveValue := FValue;
    if not (csLoading in ComponentState) then
      Text := FloatToStr(FValue);
    FChanging := false;
  end; {if}
end; {SetValue}

procedure TElFloatSpinEdit.SetValueUndefined(Value: Boolean);
begin
  if FValueUndefined <> Value then
  begin
    FValueUndefined := Value;
    if csLoading in cOmponentState then
      exit;
    if FValueUndefined then
    begin
      FChanging := true;
      Text := '';
      FChanging := false;
    end
    else
    begin
      FChanging := true;
      Text := FloatToStr(FValue);
      FChanging := false;
    end;
  end;
end;

procedure TElFloatSpinEdit.SpinDownClick(Sender : TObject; Increment : Double);
begin
  if not ReadOnly then
  begin
    Value := Value - Increment;
    if CanFocus and (Windows.GetFocus <> Handle) then SetFocus;
    SelStart := 0;
    SelLength := Length(Text);
    if Assigned(FOnDownClick) then FOnDownClick(Self);
  end;
end;

procedure TElFloatSpinEdit.SpinDrag(Sender : TObject; NewValue : Double);
begin
  if not ReadOnly then
  begin
    Value := NewValue;
    Change;
  end;
end;

procedure TElFloatSpinEdit.SpinStart(Sender : TObject; var StartValue : 
    Double);
begin
  StartValue := Value;
end;

procedure TElFloatSpinEdit.SpinUpClick(Sender : TObject; Increment : Double);
begin
  if not ReadOnly then
  begin
    Value := Value + Increment;
    if CanFocus and (Windows.GetFocus <> Handle) then SetFocus;
    SelStart := 0;
    SelLength := Length(Text);
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end;
end;

procedure TElFloatSpinEdit.WMContextMenu(var Msg : TMessage);
var p : TPoint;
begin
  if FButton.SpinDragging then
  begin
    GetCursorPos(p);
    p := FButton.ScreenToClient(p);
    FButton.StopDragging;
  end;
  inherited;
end;  { WMContextMenu }

procedure TElFloatSpinEdit.WMCreate(var Msg : TWMCreate);
begin
  inherited;
  if ValueUndefined then
    Text := ''
  else
    Text := FloatToStr(Value);
end;

procedure TElFloatSpinEdit.WMCut(var Msg : TMessage);
begin
  if not FAllowEdit then exit;
  inherited;
end;

procedure TElFloatSpinEdit.WMKeyDown(var Msg : TWMKeyDown);
begin
  FSavePos := SelStart;
  FSaveLen := SelLength;
  inherited;
end; {WMKeyDown}

procedure TElFloatSpinEdit.WMKillFocus(var Msg : TWMKillFocus);
begin
  inherited;
  FButton.OldStyled := (not Flat) or ((not ButtonThinFrame) and MouseOver);
end; { WMKillFocus }

procedure TElFloatSpinEdit.WMMButtonDown(var Msg : TWMMButtonDown);
begin
  inherited;
  if CanFocus then SetFocus;
end;  { WMMButtonDown }

procedure TElFloatSpinEdit.WMMouseWheel(var Msg: TWMMouseWheel);
var
  Dy : integer;
  sl : integer;
begin
  if IsWinNT or IsWin98 then SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @sl, SPIF_SENDCHANGE) else sl := 3;
  if sl = 0 then sl := 1;
  Dy := Msg.WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  if Dy <> 0 then 
  begin
    SetValue(FValue + Dy * FIncrement);
    Change;
  end;
end; { WMMouseWheel }

procedure TElFloatSpinEdit.WMPaste(var Msg : TMessage);
begin
  if not FAllowEdit then exit;
  inherited;
end;

procedure TElFloatSpinEdit.WMSetFocus(var Msg : TWMSetFocus);
begin
  inherited;
  FButton.OldStyled := (not Flat) or (not ButtonThinFrame);
end; { WMSetFocus }

procedure TElFloatSpinEdit.WMSize(var Msg : TWMSize);
begin
  inherited;
end;

function TElFloatSpinEdit.GetUseXPThemes: Boolean;
begin
  Result := FButton.UseXPThemes;
end;

procedure TElFloatSpinEdit.SetUseXPThemes(const Value: Boolean);
begin
  inherited;
  FButton.UseXPThemes := Value;
end;

procedure TElFloatSpinEdit.SetButtonThinFrame(Value: Boolean);
begin
  if FButtonThinFrame <> Value then
  begin
    FButtonThinFrame := Value;
    FButton.OldStyled := (not Flat) or not (ButtonThinFrame or not (MouseOver or Focused));
  end;
end;

procedure TElFloatSpinEdit.SetLineBorderActiveColor(Value: TColor);
begin
  if LineBorderActiveColor <> Value then
  begin
    inherited;
    FButton.MoneyFlatActiveColor := Value;
    FButton.MoneyFlat := Flat and (InactiveBorderType = fbtColorLineBorder) and (ActiveBorderType = fbtColorLineBorder);
  end;
end;

procedure TElFloatSpinEdit.SetLineBorderInactiveColor(Value: TColor);
begin
  if LineBorderInactiveColor <> Value then
  begin
    inherited;
    FButton.MoneyFlatInactiveColor := Value;
    FButton.MoneyFlat := Flat and (InactiveBorderType = fbtColorLineBorder) and (ActiveBorderType = fbtColorLineBorder);
  end;
end;

procedure TElFloatSpinEdit.SetEditRect;
var
  R : TRect;
begin
  Dec(Value.Right, FBtnWidth);
  inherited SetEditRect(Value);
  if not HandleAllocated then exit;
  R := Value;
  R.Left := R.Right;
  R.Right := ClientWidth;
  FButton.BoundsRect := R;
  FButton.Visible := true;
end; {SetEditRect}

end.

