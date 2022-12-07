
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

unit ElMemoCombo;  { TElMemoCombo component. }

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElBtnEdit,
  ElACtrls,
  ElVCLUtils,
  ElPopBtn,
  ElTools,
  ElTmSchema,
  ElUxTheme,
  ElEdits,
  ExtCtrls;

type
  TElMemoComboDropEvent = procedure(Sender : TObject; var Dropped : boolean; var Text : string) of object;

  TElIntMemo = TElAdvancedMemo;
  // TElIntMemo = TElEdit;

  TElMemoCombo = class(TCustomElButtonEdit)
  private
    FOnMemoChange : TNotifyEvent;
    FOnMemoClick : TNotifyEvent;
    FOnMemoDblClick : TNotifyEvent;
    FOnMemoKeyDown : TKeyEvent;
    FOnMemoKeyPress : TKeyPressEvent;
    FOnMemoKeyUp : TKeyEvent;
    FOnMemoMouseDown : TMouseEvent;
    FOnMemoMouseMove : TMouseMoveEvent;
    FOnMemoMouseUp : TMouseEvent;
    { Private declarations }
    FMemo : TElIntMemo;
    FMemoAlignment: TAlignment;
    { Owned component }
    FDropAutoWidth: boolean;
    FDropHeight : Integer;
    FDropWidth : Integer;
    FDropped : Boolean;
    FOnDrop : TElMemoComboDropEvent;
    FIgnoreBtn : boolean;
    procedure SetDropHeight(newValue : Integer);
    procedure SetDropWidth(newValue : Integer);
    procedure SetDropped(newValue : Boolean);
    { Inherited properties' Read/Write methods: }
    procedure SetMemoAlignment(newValue : TAlignment);
    procedure SetMemoColor(newValue : TColor);
    function GetMemoColor : TColor;
    procedure SetMemoFont(newValue : TFont);
    function GetMemoFont : TFont;
    procedure SetMemoMaxLength(newValue : Integer);
    function GetMemoMaxLength : Integer;
    procedure SetMemoPopupMenu(newValue : TPopupMenu);
    function GetMemoPopupMenu : TPopupMenu;
    procedure SetMemoScrollBars(newValue : TScrollStyle);
    function GetMemoScrollBars : TScrollStyle;
    procedure SetMemoWordWrap(newValue : Boolean);
    function GetMemoWordWrap : Boolean;
    procedure MemoDeactivate(Sender : TObject);
    procedure ButtonClick(Sender : TObject);
    procedure SetMemoFlatScrollBars(newValue : Boolean);
    function GetMemoFlatScrollBars : Boolean;
    procedure SetMemoActiveBorderType(newValue : TElFlatBorderType);
    function GetMemoActiveBorderType : TElFlatBorderType;
    procedure MemoChangeTransfer(Sender : TObject);  { TNotifyEvent }
    procedure MemoClickTransfer(Sender : TObject);  { TNotifyEvent }
    procedure MemoDblClickTransfer(Sender : TObject);  { TNotifyEvent }
    procedure MemoKeyDownTransfer(Sender : TObject; var Key : Word; Shift : TShiftState);  { TKeyEvent }
    procedure MemoKeyPressTransfer(Sender : TObject; var Key : Char);  { TKeyPressEvent }
    procedure MemoKeyUpTransfer(Sender : TObject; var Key : Word; Shift : TShiftState);  { TKeyEvent }
    procedure MemoMouseDownTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);  { TMouseEvent }
    procedure MemoMouseMoveTransfer(Sender : TObject; Shift : TShiftState; X : Integer; Y : Integer);  { TMouseMoveEvent }
    procedure MemoMouseUpTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);  { TMouseEvent }
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure SetDropAutoWidth(const Value: boolean);
  protected
    procedure TriggerDropEvent(var Dropped : boolean; var Text : string); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Drop(Dropped : boolean);
    function GetMemo : TElIntMemo;
  published
    property DropAutoWidth: boolean read FDropAutoWidth write SetDropAutoWidth default True;
    property DropHeight : Integer read FDropHeight write SetDropHeight default 80;  { Published }
    property DropWidth : Integer read FDropWidth write SetDropWidth;  { Published }
    property Dropped : Boolean read FDropped write SetDropped;  { Published }
    property OnDrop : TElMemoComboDropEvent read FOnDrop write FOnDrop;

    property MemoAlignment: TAlignment read FMemoAlignment write SetMemoAlignment;
    property MemoColor : TColor read GetMemoColor write SetMemoColor;
    property MemoFont : TFont read GetMemoFont write SetMemoFont;
    property MemoMaxLength : Integer read GetMemoMaxLength write SetMemoMaxLength;
    property MemoPopupMenu : TPopupMenu read GetMemoPopupMenu write SetMemoPopupMenu;
    property MemoScrollBars : TScrollStyle read GetMemoScrollBars write SetMemoScrollBars;
    property MemoWordWrap : Boolean read GetMemoWordWrap write SetMemoWordWrap;
    property MemoActiveBorderType : TElFlatBorderType read GetMemoActiveBorderType write SetMemoActiveBorderType;
    property MemoFlatScrollBars : Boolean read GetMemoFlatScrollBars write SetMemoFlatScrollBars;

    property TopMargin;
    property LeftMargin;
    property RightMargin;
    property AutoSize;
    property RTLContent;
    property BorderSides;
    property PasswordChar;
    property MaxLength;
    property Transparent;
    property FlatFocusedScrollBars;
    property WantTabs;
    property HandleDialogKeys;
    property HideSelection;
    property TabSpaces;
    property Lines  stored false;
    {$ifdef ELPACK_COMPLETE}
    property ImageForm;
    {$endif}
    property WordWrap;
    property ScrollBars;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnChange;
    property OnSelectionChange;

    property Text;
    property Multiline default true;

    property Align;
    property ActiveBorderType;
    property AutoSelect;
    property BorderStyle;
    property ButtonClickSound;
    property ButtonDownSound;
    property ButtonUpSound;
{$IFDEF USE_SOUND_MAP}
    property ButtonSoundMap;
{$ENDIF}
    property ButtonColor;
    property ButtonHint;
    property ButtonWidth default 15;

    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property InactiveBorderType;
    // property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Visible;
    { Inherited events: }
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
    property OnMemoChange : TNotifyEvent read FOnMemoChange write FOnMemoChange;
    property OnMemoClick : TNotifyEvent read FOnMemoClick write FOnMemoClick;
    property OnMemoDblClick : TNotifyEvent read FOnMemoDblClick write FOnMemoDblClick;
    property OnMemoKeyDown : TKeyEvent read FOnMemoKeyDown write FOnMemoKeyDown;
    property OnMemoKeyPress : TKeyPressEvent read FOnMemoKeyPress write FOnMemoKeyPress;
    property OnMemoKeyUp : TKeyEvent read FOnMemoKeyUp write FOnMemoKeyUp;
    property OnMemoMouseDown : TMouseEvent read FOnMemoMouseDown write FOnMemoMouseDown;
    property OnMemoMouseMove : TMouseMoveEvent read FOnMemoMouseMove write FOnMemoMouseMove;
    property OnMemoMouseUp : TMouseEvent read FOnMemoMouseUp write FOnMemoMouseUp;

{$IFDEF VCL_4_USED}
    property Anchors;
    property Constraints;
    property DockOrientation;
    property Floating;
    property DoubleBuffered;
    property DragKind;
{$ENDIF}

  end;  { TElMemoCombo }

type

  TElHackMemo = class(TElIntMemo)
  private
    Owner : TComponent;
    FOnDeactivate : TNotifyEvent;
    procedure WMNCHitTest(var Msg : TMessage); message WM_NCHITTEST;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSysKeyDown(var Msg : TWMKeyDown); message WM_SYSKEYDOWN;
    procedure WMKeyDown(var Msg : TWMKeyDown); message WM_KEYDOWN;
    procedure WMActivate(var Msg : TWMActivate); message WM_ACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property OnDeactivate : TNotifyEvent read FOnDeactivate write FOnDeactivate;
  public
    constructor Create(Owner : TComponent); override;
  end;

implementation

type TElMemoComboButton = class(TCustomElGraphicButton)
     protected
       function GetThemedClassName: WideString; override;
       function GetThemePartID: Integer; override;
       function GetThemeStateID: Integer; override;
     public
       constructor Create(AOwner : TComponent); override;
     end;

constructor TElHackMemo.Create(Owner : TComponent);
begin
  inherited;
  Align := alClient;
  Parent  := nil;
  Ctl3D   := true;
  Visible := False;
  ParentCtl3D := False;
  TabStop := true;
  ReadOnly := false;
//  Multiline := true;
end;

procedure TElHackMemo.WMNCHitTest(var Msg : TMessage); { private }
var
  R : TRect;
  P : TPoint;
begin
  inherited;
  with Msg do
  begin
    P := Point(LOWORD(LParam), HIWORD(LPARAM));
    P := ScreenToClient(P);
    R := ClientRect;
    OffsetRect(R, -R.Left, -R.Top);
    InflateRect(R, GetSystemMetrics(SM_CXFIXEDFRAME), GetSystemMetrics(SM_CYFIXEDFRAME));
    result := HitTest(R, P, R.Bottom div 6, GetSystemMetrics(SM_CXFIXEDFRAME));
    if (Result = HTBOTTOMLEFT) or
       (Result = HTLEFT) or
       (Result = HTTOPLEFT) or
       (Result = HTTOPRIGHT) or
       (Result = HTTOP) then
      Result := HTNOWHERE;
  end; { with }
end; { WMNCHitTest }

procedure TElHackMemo.CreateParams(var Params: TCreateParams);  { protected }
const HScrolls : array[boolean] of DWORD = (ES_AUTOHSCROLL, 0);
begin
  inherited;
  with Params do
  begin
    Style := WS_CHILD or WS_CLIPSIBLINGS or WS_TABSTOP or WS_VISIBLE or ES_AUTOVSCROLL or ES_MULTILINE or HSCrolls[(Owner as TElMemoCombo).MemoWordWrap];
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_STATICEDGE;
  end;
end;  { CreateParams }

procedure TElHackMemo.WMKillFocus(var Msg : TWMKillFocus);  { private }
begin
  inherited;
  if assigned(FOnDeactivate) then FOnDeactivate(Self);
end;  { WMKillFocus }

procedure TElHackMemo.WMSysKeyDown(var Msg : TWMKeyDown);  { private }
begin
  if (Msg.CharCode = VK_UP) and (ssAlt in GetShiftState)
  then
  begin
    Msg.Result := 1;
    Msg.CharCode := 0;
    if assigned(FOnDeactivate) then FOnDeactivate(Self);
    exit;
  end;
  if (Msg.CharCode = VK_F4) and (ssAlt in GetShiftState) then
  begin
    Msg.Result := 1;
    Msg.CharCode := 0;
    exit;
  end;
  inherited;
end;  { WMSysKeyDown }

procedure TElHackMemo.WMKeyDown(var Msg : TWMKeyDown);  { private }
begin
  if (Msg.CharCode = VK_ESCAPE) and (GetShiftState = []) then
  begin
    Msg.Result := 1;
    Msg.CharCode := 0;
    if assigned(FOnDeactivate) then FOnDeactivate(Self);
  end else inherited;
end;  { WMKeyDown }

procedure TElHackMemo.WMActivate(var Msg : TWMActivate);  { private }
begin
  inherited;
end;  { WMActivate }

procedure TElMemoCombo.MemoDeactivate;
begin
  SetDropped(false);
  //if Parent <> nil then
  SetFocus;
end;

procedure TElMemoCombo.SetDropped(newValue : Boolean);
var
  P : TPoint;
  S : String;
begin
  if newValue <> FDropped then
  begin
    FDropped := newValue;
    if FDropped then
    begin
      S := Text;
      TriggerDropEvent(FDropped, S);
      FMemo.Parent := GetParentForm(Self);
      P := Parent.ClientToScreen(Point(Left, Top + Height));
      FIgnoreBtn := true;
      FMemo.Parent := nil;
      FMemo.ParentWindow := GetDesktopWindow;
      FMemo.Height := FDropHeight;
      if FDropAutoWidth then
        FMemo.Width := Width
      else
        FMemo.Width := FDropWidth;

      FMemo.Lines.Text := S;
      //FMemo.Show;
      FMemo.ReadOnly := true;
      with P do
        SetWindowPos(FMemo.Handle, HWND_TOPMOST, x, y, FMemo.Width, FDropHeight, SWP_SHOWWINDOW);

      //Windows.SetFocus(FMemo.Handle);
      SetForegroundWindow(GetDesktopWindow);
      SetForegroundWindow(FMemo.Handle);
      SetWindowPos(FMemo.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    end
    else
    begin
      S := FMemo.Lines.Text;
      TriggerDropEvent(FDropped, S);
      Text := S;
      FIgnoreBtn  := false;
      FDropHeight := FMemo.Height;
      FDropWidth  := FMemo.Width;
      FMemo.Hide;
      SetWindowPos(FMemo.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_HIDEWINDOW);
      //FMemo.ParentWindow := 0;
      //FMemo.Parent := Self;
      if Parent <> nil then
        SetFocus;
    end;
  end;
end;

procedure TElMemoCombo.ButtonClick(Sender : TObject);
begin
  if (not FIgnoreBtn) then
     Drop(not Dropped);
  //FIgnoreBtn := false;
  //if not Dropped then
  //  SetFocus;
end;

procedure TElMemoCombo.Drop(Dropped : boolean);
begin
  SetDropped(Dropped);
end; { SetDropped }

procedure TElMemoCombo.SetDropHeight(newValue : Integer);
{ Sets data member FDropHeight to newValue. }
begin
  if (FDropHeight <> newValue) then
  begin
    FDropHeight := newValue;
    if Dropped then FMemo.Height := FDropHeight;
  end;  { if }
end;  { SetDropHeight }

procedure TElMemoCombo.SetDropWidth(newValue : Integer);
{ Sets data member FDropWidth to newValue. }
begin
  if (FDropWidth <> newValue) then
  begin
    FDropWidth := newValue;
    if Dropped then FMemo.Width := FDropWidth;
  end;  { if }
end;  { SetDropWidth }

{ Inherited properties' Read/Write methods: }
procedure TElMemoCombo.SetMemoAlignment(newValue : TAlignment);
{ Sets the FMemo subcomponent's Alignment property to newValue. }
begin
  if FMemoAlignment <> NewValue then
  begin
    FMemoAlignment := NewValue;
    if Dropped then FMemo.Alignment := newValue;
  end;  
end;  { SetMemoAlignment }

procedure TElMemoCombo.SetMemoColor(newValue : TColor);
{ Sets the FMemo subcomponent's Color property to newValue. }
begin
  FMemo.Color := newValue;
end;  { SetMemoColor }

function TElMemoCombo.GetMemoColor : TColor;
{ Returns the Color property from the FMemo subcomponent. }
begin
  GetMemoColor := FMemo.Color;
end;  { GetMemoColor }

procedure TElMemoCombo.SetMemoFont(newValue : TFont);
{ Sets the FMemo subcomponent's Font property to newValue. }
begin
  FMemo.Font := newValue;
end;  { SetMemoFont }

function TElMemoCombo.GetMemoFont : TFont;
{ Returns the Font property from the FMemo subcomponent. }
begin
  GetMemoFont := FMemo.Font;
end;  { GetMemoFont }

procedure TElMemoCombo.SetMemoMaxLength(newValue : Integer);
{ Sets the FMemo subcomponent's MaxLength property to newValue. }
begin
  FMemo.MaxLength := newValue;
end;  { SetMemoMaxLength }

function TElMemoCombo.GetMemoMaxLength : Integer;
{ Returns the MaxLength property from the FMemo subcomponent. }
begin
  GetMemoMaxLength := FMemo.MaxLength;
end;  { GetMemoMaxLength }

procedure TElMemoCombo.SetMemoPopupMenu(newValue : TPopupMenu);
{ Sets the FMemo subcomponent's PopupMenu property to newValue. }
begin
  FMemo.PopupMenu := newValue;
end;  { SetMemoPopupMenu }

function TElMemoCombo.GetMemoPopupMenu : TPopupMenu;
{ Returns the PopupMenu property from the FMemo subcomponent. }
begin
  GetMemoPopupMenu := FMemo.PopupMenu;
end;  { GetMemoPopupMenu }

procedure TElMemoCombo.SetMemoScrollBars(newValue : TScrollStyle);
{ Sets the FMemo subcomponent's ScrollBars property to newValue. }
begin
  FMemo.ScrollBars := newValue;
end;  { SetMemoScrollBars }

function TElMemoCombo.GetMemoScrollBars : TScrollStyle;
{ Returns the ScrollBars property from the FMemo subcomponent. }
begin
  GetMemoScrollBars := FMemo.ScrollBars;
end;  { GetMemoScrollBars }

procedure TElMemoCombo.SetMemoWordWrap(newValue : Boolean);
{ Sets the FMemo subcomponent's WordWrap property to newValue. }
begin
  FMemo.WordWrap := newValue;
end;  { SetMemoWordWrap }

function TElMemoCombo.GetMemoWordWrap : Boolean;
{ Returns the WordWrap property from the FMemo subcomponent. }
begin
  GetMemoWordWrap := FMemo.WordWrap;
end;  { GetMemoWordWrap }

{ Event triggers: }
procedure TElMemoCombo.TriggerDropEvent(var Dropped : boolean; var Text : string);
begin
  if (assigned(FOnDrop)) then FOnDrop(Self, Dropped , Text );
end;  { TriggerDropEvent }

procedure TElMemoCombo.SetMemoFlatScrollBars(newValue : Boolean);
{ Sets the FMemo subcomponent's FlatFocusedScrollBars property to newValue. }
begin
  FMemo.FlatFocusedScrollBars := newValue;
end;  { SetMemoFlatScrollBars }

function TElMemoCombo.GetMemoFlatScrollBars : Boolean;
{ Returns the FlatFocusedScrollBars property from the FMemo subcomponent. }
begin
  GetMemoFlatScrollBars := FMemo.FlatFocusedScrollBars;
end;  { GetMemoFlatScrollBars }

{ Exposed properties' Read/Write methods: }
procedure TElMemoCombo.SetMemoActiveBorderType(newValue : TElFlatBorderType);
{ Sets the FMemo subcomponent's ActiveBorderType property to newValue. }
begin
  FMemo.ActiveBorderType := newValue;
end;  { SetMemoActiveBorderType }

function TElMemoCombo.GetMemoActiveBorderType : TElFlatBorderType;
{ Returns the ActiveBorderType property from the FMemo subcomponent. }
begin
  GetMemoActiveBorderType := FMemo.ActiveBorderType;
end;  { GetMemoActiveBorderType }

procedure TElMemoCombo.MemoChangeTransfer(Sender : TObject);
{ Transfers FMemo OnChange event to the outside world. }
begin
  if (assigned(FOnMemoChange)) then
    FOnMemoChange(Self);  { Substitute Self for subcomponent's Sender. }
end;  { MemoChangeTransfer }

procedure TElMemoCombo.MemoClickTransfer(Sender : TObject);
{ Transfers FMemo OnClick event to the outside world. }
begin
  if (assigned(FOnMemoClick)) then
    FOnMemoClick(Self);  { Substitute Self for subcomponent's Sender. }
end;  { MemoClickTransfer }

procedure TElMemoCombo.MemoDblClickTransfer(Sender : TObject);
{ Transfers FMemo OnDblClick event to the outside world. }
begin
  if (assigned(FOnMemoDblClick)) then
    FOnMemoDblClick(Self);  { Substitute Self for subcomponent's Sender. }
end;  { MemoDblClickTransfer }

procedure TElMemoCombo.MemoKeyDownTransfer(Sender : TObject; var Key : Word; Shift : TShiftState);
{ Transfers FMemo OnKeyDown event to the outside world. }
begin
  if (assigned(FOnMemoKeyDown)) then
    FOnMemoKeyDown(Self, Key , Shift );  { Substitute Self for subcomponent's Sender. }
end;  { MemoKeyDownTransfer }

procedure TElMemoCombo.MemoKeyPressTransfer(Sender : TObject; var Key : Char);
{ Transfers FMemo OnKeyPress event to the outside world. }
begin
  if (assigned(FOnMemoKeyPress)) then
    FOnMemoKeyPress(Self, Key );  { Substitute Self for subcomponent's Sender. }
end;  { MemoKeyPressTransfer }

procedure TElMemoCombo.MemoKeyUpTransfer(Sender : TObject; var Key : Word; Shift : TShiftState);
{ Transfers FMemo OnKeyUp event to the outside world. }
begin
  if (assigned(FOnMemoKeyUp)) then
    FOnMemoKeyUp(Self, Key , Shift );  { Substitute Self for subcomponent's Sender. }
end;  { MemoKeyUpTransfer }

procedure TElMemoCombo.MemoMouseDownTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);
{ Transfers FMemo OnMouseDown event to the outside world. }
begin
  if (assigned(FOnMemoMouseDown)) then
    FOnMemoMouseDown(Self, Button , Shift , X , Y );  { Substitute Self for subcomponent's Sender. }
end;  { MemoMouseDownTransfer }

procedure TElMemoCombo.MemoMouseMoveTransfer(Sender : TObject; Shift : TShiftState; X : Integer; Y : Integer);
{ Transfers FMemo OnMouseMove event to the outside world. }
begin
  if (assigned(FOnMemoMouseMove)) then
    FOnMemoMouseMove(Self, Shift , X , Y );  { Substitute Self for subcomponent's Sender. }
end;  { MemoMouseMoveTransfer }

procedure TElMemoCombo.MemoMouseUpTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);
{ Transfers FMemo OnMouseUp event to the outside world. }
begin
  if (assigned(FOnMemoMouseUp)) then
    FOnMemoMouseUp(Self, Button , Shift , X , Y );  { Substitute Self for subcomponent's Sender. }
end;  { MemoMouseUpTransfer }

procedure TElMemoCombo.WMSetFocus(var Msg : TWMSetFocus);  { private }
begin
  FIgnoreBtn := false;
  inherited;
end;  { WMSetFocus }

procedure TElMemoCombo.CreateParams(var Params: TCreateParams);  { protected }
begin
  inherited;
end;  { CreateParams }

destructor TElMemoCombo.Destroy;
begin
  FMemo.Free;
  inherited Destroy;
//   FMemo.Free;
end;  { Destroy }

constructor TElMemoCombo.Create(AOwner : TComponent);
begin
  ButtonClass := TElMemoComboButton;
  inherited Create(AOwner);
  FMemo := TElHackMemo.Create(nil);
  TElHackMemo(FMemo).Owner := Self;
  FMemo.Visible := false;

  // FMemo.Parent := Self;

  //FMemo.WantReturns := True;
  FMemo.WantTabs := true;
  FMemo.OnChange := MemoChangeTransfer;
  FMemo.OnClick := MemoClickTransfer;
  FMemo.OnDblClick := MemoDblClickTransfer;
  FMemo.OnKeyDown := MemoKeyDownTransfer;
  FMemo.OnKeyPress := MemoKeyPressTransfer;
  FMemo.OnKeyUp := MemoKeyUpTransfer;
  FMemo.OnMouseDown := MemoMouseDownTransfer;
  FMemo.OnMouseMove := MemoMouseMoveTransfer;
  FMemo.OnMouseUp := MemoMouseUpTransfer;

  TElHackMemo(FMemo).OnDeactivate := MemoDeactivate;
  FDropHeight := 80;
  OnButtonClick := ButtonClick;
  ButtonGlyph.LoadFromResourceName(HInstance, 'DROPDOWNGLYPH');
  AltButtonVisible := false;
  ButtonFlat := false;
  ButtonWidth := 15;
  ButtonShortcut := Shortcut(VK_DOWN, [ssAlt]);
  FDropAutoWidth := True;
  FDropWidth := Width;
  Multiline := true;
end;  { Create }

function TElMemoCombo.GetMemo : TElIntMemo;
begin
  result := FMemo;
end;

procedure TElMemoCombo.SetDropAutoWidth(const Value: boolean);
begin
  if FDropAutoWidth <> Value then
  begin
    FDropAutoWidth := Value;
    if Dropped then
      if Value then
        FMemo.Width := Width
      else
        FMemo.Width := DropWidth;  
  end;  
end;

procedure TElMemoCombo.CreateWnd;
begin
  inherited;
  FMultiline := false;
  AdjustHeight;
  FMultiline := true;
end;

function TElMemoComboButton.GetThemedClassName: WideString;
begin
  Result := 'COMBOBOX';
end;

function TElMemoComboButton.GetThemePartID: Integer;
begin
  result := CP_DROPDOWNBUTTON;
  ShowGlyph := not IsThemeApplied;
end;

function TElMemoComboButton.GetThemeStateID: Integer;
begin
  if not Enabled then
    result := CBXS_DISABLED
  else
  if FState in [ebsDown, ebsExclusive] then
    result := CBXS_PRESSED
  else
  if MouseInControl then
    result := CBXS_HOT
  else
    result := CBXS_NORMAL;
end;

constructor TElMemoComboButton.Create(AOwner : TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csDoubleClicks];
end;


end.

