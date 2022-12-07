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

03/22/2002

  Fixed style painting for disabled button with XP themes enabled

03/06/2002

  Added unicode hint

01/06/2002

  Added Qt styles in CLX version 

11/30/2001

  Added moneyFlat and accompanying properties

09/17/2001

  Added Windows XP Themes Support 

08/03/2001

  Fixed the problem with spin buttons, that continue to spin when mouse cursor
  is moved over drag are and mouse button is pressed  

03/29/2001

  Fixed the problem with spin buttons, that continued to spin when context menu
  is invoked

*)

unit ElSpinBtn;

interface

uses
  {$ifndef CLX_USED}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Menus,
  StdCtrls,
  ExtCtrls,
  ElACtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  {$else}
  Qt,
  QTypes,
  Types,
  QMenus,
  QGraphics,
  QControls,
  QForms,
  QStdCtrls,
  QExtCtrls,
  ElCLXUtils,
  {$endif}
  SysUtils,
  Classes,
  ElVCLUtils,
  ElXPThemedControl,
  ElUxTheme,
  ElTmSchema,
  ElTools;

type

  TElSpinEvent = procedure(Sender : TObject; Distance : Double) of object;
  TElSpinStartEvent = procedure(Sender : TObject; var InitialDistance : Double) of object;

  TElSpinBtnDir  = (sbdUpDown, sbdLeftRight);
  TElSpinBtnType = (sbtUpDown, sbtLeftRight);

  TElSpinButton = class(TElXPThemedControl)
  private
    FFlat : Boolean;
    FUseDrag : Boolean;
    FOnUpClick,
    FOnDownClick,
    FOnSpinDrag : TElSpinEvent;
    FOnSpinStart: TElSpinStartEvent;
    FMouseInUpPart: Boolean;
    FMouseInDownPart: Boolean;

    {$ifndef CLX_USED}
    procedure WMMouseMove(var Msg : TWMMouseMove); message WM_MOUSEMOVE;
    procedure CMEnabledChanged(var Msg : TMessage); message CM_ENABLEDCHANGED;
    procedure WMContextMenu(var Msg : TMessage); message WM_CONTEXTMENU;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    {$endif}
  protected
    FTopBtnDown,
      FBottomBtnDown : boolean;
    FTimer : TTimer;
    SaveCursor  : TCursor;
    FCaptured,
      FDragging : boolean;
    FStartValue,
    FValue      : Double;
    FIncrement: Double;
    FButtonDirection: TElSpinBtnDir;
    FButtonType: TElSpinBtnType;
    FUpArrowEnabled: Boolean;
    FDownArrowEnabled: Boolean;

    FMoneyFlat: Boolean;
    FMoneyFlatActiveColor: TColor;
    FMoneyFlatDownColor: TColor;
    FMoneyFlatInactiveColor: TColor;
    FOldStyled: Boolean;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    procedure IntMouseMove(XPos, YPos : SmallInt);
    {$ifdef CLX_USED}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    procedure EnabledChanged; override;
    {$endif}
    procedure Paint; override;
    procedure OnTimer(Sender : TObject);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;

    {$ifndef CLX_USED}
    procedure CMExit(var Msg : TCMExit); message CM_EXIT;
    {$else}
    procedure DoExit; override;
    {$endif}
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    {$ifndef CLX_USED}
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    {$endif}

    function GetPopupMenu: TPopupMenu; override;
    procedure SetUseDrag(newValue : Boolean); virtual;
    procedure SetFlat(newValue : Boolean); virtual;
    procedure SetButtonDirection(Value: TElSpinBtnDir);
    procedure SetButtonType(Value: TElSpinBtnType);
    function GetThemedClassName: WideString; override;
    procedure SetUpArrowEnabled(Value: Boolean);
    procedure SetDownArrowEnabled(Value: Boolean);
    procedure SetMoneyFlat(Value: Boolean);
    procedure SetMoneyFlatActiveColor(Value: TColor);
    procedure SetMoneyFlatDownColor(Value: TColor);
    procedure SetMoneyFlatInactiveColor(Value: TColor);
    function GetMoneyFlat: Boolean;
    procedure SetOldStyled(Value: Boolean);

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure StopDragging;
    property SpinDragging : boolean read FDragging;

  published
    property Increment: Double read FIncrement write FIncrement;
    property UseDrag : Boolean read FUseDrag write SetUseDrag default true;  { Published }
    property Flat : Boolean read FFlat write SetFlat;  { Published }
    property ButtonDirection: TElSpinBtnDir read FButtonDirection write
        SetButtonDirection default sbdUpDown;
    property ButtonType: TElSpinBtnType read FButtonType write SetButtonType;
    property UpArrowEnabled: Boolean read FUpArrowEnabled write SetUpArrowEnabled default true;
    property DownArrowEnabled: Boolean read FDownArrowEnabled write SetDownArrowEnabled default true;
    property MoneyFlat: Boolean read GetMoneyFlat write SetMoneyFlat default false;
    property MoneyFlatActiveColor: TColor read FMoneyFlatActiveColor write
        SetMoneyFlatActiveColor stored GetMoneyFlat;
    property MoneyFlatDownColor: TColor read FMoneyFlatDownColor write
        SetMoneyFlatDownColor stored GetMoneyFlat;
    property MoneyFlatInactiveColor: TColor read FMoneyFlatInactiveColor write
        SetMoneyFlatInactiveColor stored GetMoneyFlat;
    property OldStyled: Boolean read FOldStyled write SetOldStyled default false;

    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}

    property OnUpClick  : TElSpinEvent read FOnUpClick write FOnUpClick;
    property OnDownClick: TElSpinEvent read FOnDownClick write FOnDownClick;
    property OnSpinDrag : TElSpinEvent read FOnSpinDrag write FOnSpinDrag;
    property OnSpinStart: TElSpinStartEvent read FOnSpinStart write FOnSpinStart;

    property Caption;
    property Enabled;
    property TabStop default True;
    property TabOrder;
    property PopupMenu;
    property Color;
    property ParentColor;
    property Align;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property UseXPThemes;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;

{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    {$ifndef CLX_USED}
    property DockOrientation;
    property Floating;
    property BevelKind;
    property DoubleBuffered;
    property DragKind;

    property OnStartDock;
    property OnEndDock;
    {$endif}
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

implementation

{$R ElSpin.res}

constructor TElSpinButton.Create(AOwner : TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csCaptureMouse];
  if AOwner <> nil then
    SetDesigning(csDesigning in AOwner.ComponentState);
  FUseDrag := true;
  Width := 16;
  Height := 16;
  FUpArrowEnabled := true;
  FDownArrowEnabled := true;
end;

destructor TElSpinButton.Destroy;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := false;
    FTimer.Free;
    FTimer := nil;
  end;
  inherited;
end;

procedure TElSpinButton.Paint;
var
{$IFNDEF MANUAL_BUTTONS}
  R,
{$ENDIF}
  R1,
  R2
  {$ifdef MSWINDOWS}
  ,RClip
  {$endif}
   : TRect;
  Clr   : TColor;
  {$ifndef CLX_USED}
  AColor: TColor;
  {$endif}
  {$ifdef CLX_USED}
  AQColorGroup : QColorGroupH;
  {$endif}
  {$ifdef MSWINDOWS}
  pid,
  sid   : integer;
  {$endif}
begin
  inherited;
  if FButtonType = sbtUpDown then
  begin
    R1 := Rect(0, 0, Width, Height div 2);
    R2 := Rect(0, Height div 2 + 1, Width, Height);
  end
  else
  begin
    R1 := Rect(0, 0, Width div 2, Height);
    R2 := Rect(Width div 2 + 1, 0, Width, Height);
  end;

  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(R1);
  Canvas.FillRect(R2);

  {$ifdef MSWINDOWS}
  if not IsThemeApplied then
  {$endif}
  begin
    {$ifndef CLX_USED}
    if MoneyFlat then
    begin
      if FTopBtnDown then
        AColor := MoneyFlatDownColor
      else
      if FMouseInUpPart then
        AColor := MoneyFlatActiveColor
      else
        AColor := MoneyFlatInactiveColor;
      DrawFlatFrameEx2(Canvas.Handle, R1, AColor, clBtnFace, not Flat, Enabled, AllBorderSides, fbtColorLineBorder);
      if FBottomBtnDown then
        AColor := MoneyFlatDownColor
      else
      if FMouseInDownPart then
        AColor := MoneyFlatActiveColor
      else
        AColor := MoneyFlatInactiveColor;
      DrawFlatFrameEx2(Canvas.Handle, R2, AColor, clBtnFace, not Flat, Enabled, AllBorderSides, fbtColorLineBorder);
    end
    else
    {$endif}
    begin
      {$ifndef CLX_USED}
      if not Flat or FTopBtnDown then
        DrawButtonFrame(Canvas.Handle, R1, OldStyled, FTopBtnDown);
      if not Flat or FBottomBtnDown then
        DrawButtonFrame(Canvas.Handle, R2, OldStyled, FBottomBtnDown);
      {$else}
      Canvas.Start;
      AQColorGroup := Palette.ColorGroup(GetColorGroup(Self));
      if not Flat or FTopBtnDown then
      with R1 do
        QStyle_drawButton(Application.Style.Handle,
                           Canvas.Handle,
                           Left,
                           Top,
                           Right - Left,
                           Bottom - Top,
                           AQColorGroup,
                           FTopBtnDown,
                           Canvas.Brush.Handle);
      if not Flat or FBottomBtnDown then
      with R2 do
        QStyle_drawButton(Application.Style.Handle,
                           Canvas.Handle,
                           Left,
                           Top,
                           Right - Left,
                           Bottom - Top,
                           AQColorGroup,
                           FBottomBtnDown,
                           Canvas.Brush.Handle);
      //AQColorGroup.Free;
      Canvas.Stop;
      {$endif}
    end;

    Dec(R1.Right);
    Dec(R2.Right);

  {$IFNDEF MANUAL_BUTTONS}
    Canvas.Brush.Color := clBtnFace;
    if FButtonDirection = sbdUpDown then
    begin
      DrawArrow(Canvas, eadUp, R1, clBtnText, Enabled and UpArrowEnabled);
      DrawArrow(Canvas, eadDown, R2, clBtnText, Enabled and DownArrowEnabled);
    end
    else
    begin
      DrawArrow(Canvas, eadRight, R1, clBtnText, Enabled and UpArrowEnabled);
      DrawArrow(Canvas, eadLeft, R2, clBtnText,  Enabled and DownArrowEnabled);
    end;
  {$ELSE}
    if Enabled then
       Clr := clBtnText
    else
       Clr := clBtnShadow;
    {$ifdef CLX_USED}
    Canvas.Pen.Color := Clr;
    {$endif}
    with Canvas do
    begin
      if FButtonDirection = sbdUpDown then
      begin
        {$ifndef CLX_USED}
        Pixels[R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1 - 1] := Clr;
        Pixels[R1.Left + (R1.Right - R1.Left) shr 1 -1, R1.Top + (R1.Bottom - r1.Top) shr 1] := Clr;
        Pixels[R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1] := Clr;
        Pixels[R1.Left + (R1.Right - R1.Left) shr 1 + 1, R1.Top + (R1.Bottom - r1.Top) shr 1] := Clr;

        Pixels[R2.Left + (R2.Right - R2.Left) shr 1, R2.Top + (R2.Bottom - R2.Top) shr 1] := Clr;
        Pixels[R2.Left + (R2.Right - R2.Left) shr 1 -1, R2.Top + (R2.Bottom - R2.Top) shr 1 - 1] := Clr;
        Pixels[R2.Left + (R2.Right - R2.Left) shr 1, R2.Top + (R2.Bottom - R2.Top) shr 1 - 1] := Clr;
        Pixels[R2.Left + (R2.Right - R2.Left) shr 1 + 1, R2.Top + (R2.Bottom - R2.Top) shr 1 - 1] := Clr;
        {$else}
        Canvas.DrawPoint(R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1 - 1);
        Canvas.DrawPoint(R1.Left + (R1.Right - R1.Left) shr 1 -1, R1.Top + (R1.Bottom - r1.Top) shr 1);
        Canvas.DrawPoint(R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1);
        Canvas.DrawPoint(R1.Left + (R1.Right - R1.Left) shr 1 + 1, R1.Top + (R1.Bottom - r1.Top) shr 1);

        Canvas.DrawPoint(R2.Left + (R2.Right - R2.Left) shr 1, R2.Top + (R2.Bottom - R2.Top) shr 1);
        Canvas.DrawPoint(R2.Left + (R2.Right - R2.Left) shr 1 -1, R2.Top + (R2.Bottom - R2.Top) shr 1 - 1);
        Canvas.DrawPoint(R2.Left + (R2.Right - R2.Left) shr 1, R2.Top + (R2.Bottom - R2.Top) shr 1 - 1);
        Canvas.DrawPoint(R2.Left + (R2.Right - R2.Left) shr 1 + 1, R2.Top + (R2.Bottom - R2.Top) shr 1 - 1);
        {$endif}
      end
      else
      begin
        {$ifndef CLX_USED}
        Pixels[R1.Left + (R1.Right - R1.Left) shr 1 - 1, R1.Top + (R1.Bottom - r1.Top) shr 1] := Clr;
        Pixels[R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1 - 1] := Clr;
        Pixels[R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1] := Clr;
        Pixels[R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1 + 1] := Clr;

        Pixels[R2.Left + (R2.Right - R2.Left) shr 1, R2.Top + (R2.Bottom - R2.Top) shr 1] := Clr;
        Pixels[R2.Left + (R2.Right - R2.Left) shr 1 - 1, R2.Top + (R2.Bottom - R2.Top) shr 1 - 1] := Clr;
        Pixels[R2.Left + (R2.Right - R2.Left) shr 1 - 1, R2.Top + (R2.Bottom - R2.Top) shr 1] := Clr;
        Pixels[R2.Left + (R2.Right - R2.Left) shr 1 - 1, R2.Top + (R2.Bottom - R2.Top) shr 1 + 1] := Clr;
        {$else}
        Canvas.DrawPoint(R1.Left + (R1.Right - R1.Left) shr 1 - 1, R1.Top + (R1.Bottom - r1.Top) shr 1);
        Canvas.DrawPoint(R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1 - 1);
        Canvas.DrawPoint(R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1);
        Canvas.DrawPoint(R1.Left + (R1.Right - R1.Left) shr 1, R1.Top + (R1.Bottom - r1.Top) shr 1 + 1);

        Canvas.DrawPoint(R2.Left + (R2.Right - R2.Left) shr 1, R2.Top + (R2.Bottom - R2.Top) shr 1);
        Canvas.DrawPoint(R2.Left + (R2.Right - R2.Left) shr 1 - 1, R2.Top + (R2.Bottom - R2.Top) shr 1 - 1);
        Canvas.DrawPoint(R2.Left + (R2.Right - R2.Left) shr 1 - 1, R2.Top + (R2.Bottom - R2.Top) shr 1);
        Canvas.DrawPoint(R2.Left + (R2.Right - R2.Left) shr 1 - 1, R2.Top + (R2.Bottom - R2.Top) shr 1 + 1);
        {$endif}
      end;
    end;
  {$endif}
  {$ifdef MSWINDOWS}
  end
  else
  begin
    if FButtonDirection = sbdUpDown then
    begin
      pid := SPNP_UP;
      if not (Enabled and UpArrowEnabled) then
        sid := UPS_DISABLED
      else
      if FTopBtnDown then
        sid := UPS_PRESSED
      else
      if FMouseInUpPart then
        sid := UPS_HOT
      else
        sid := UPS_NORMAL;
      RClip := Canvas.ClipRect;
      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, pid, sid, R1, @RClip);
      {$else}
      DrawThemeBackground(Theme, QPainter_handle(Canvas.Handle), pid, sid, R1, @RClip);
      {$endif}
      pid := SPNP_DOWN;
      if not (Enabled and DownArrowEnabled) then
        sid := DNS_DISABLED
      else
      if FBottomBtnDown then
        sid := DNS_PRESSED
      else
      if FMouseInDownPart then
        sid := DNS_HOT
      else
        sid := DNS_NORMAL;
      RClip := Canvas.ClipRect;
      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, pid, sid, R2, @RClip);
      {$else}
      DrawThemeBackground(Theme, QPainter_handle(Canvas.Handle), pid, sid, R2, @RClip);
      {$endif}
    end
    else
    begin
      pid := SPNP_DOWNHORZ;
      if not (Enabled and UpArrowEnabled) then
        sid := DNHZS_DISABLED
      else
      if FTopBtnDown then
        sid := DNHZS_PRESSED
      else
      if FMouseInUpPart then
        sid := DNHZS_HOT
      else
        sid := DNHZS_NORMAL;
      RClip := Canvas.ClipRect;
      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, pid, sid, R1, @RClip);
      {$else}
      DrawThemeBackground(Theme, QPainter_handle(Canvas.Handle), pid, sid, R1, @RClip);
      {$endif}
      pid := SPNP_UPHORZ;
      if not (Enabled and DownArrowEnabled) then
        sid := UPHZS_DISABLED
      else
      if FBottomBtnDown then
        sid := UPHZS_PRESSED
      else
      if FMouseInDownPart then
        sid := UPHZS_HOT
      else
        sid := UPHZS_NORMAL;
      RClip := Canvas.ClipRect;
      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, pid, sid, R2, @RClip);
      {$else}
      DrawThemeBackground(Theme, QPainter_handle(Canvas.Handle), pid, sid, R2, @RClip);
      {$endif}
    end;
  {$endif}
  end;
end;

procedure TElSpinButton.StopDragging;
begin
  MouseUp(mbLeft, [], 0, 0);
end;

procedure TElSpinButton.OnTimer(Sender : TObject);
begin
  FTimer.Interval := 100;
  if FTopBtnDown then
  begin
    if FButtonType = sbtLeftRight then
    begin
      if Assigned(FOnDownClick) then FOnDownClick(Self, FIncrement);
    end
    else
    begin
      if Assigned(FOnUpClick) then FOnUpClick(Self, FIncrement);
    end;
  end
  else
  if FBottomBtnDown then
  begin
    if FButtonType = sbtUpDown then
    begin
      if Assigned(FOnDownClick) then FOnDownClick(Self, FIncrement);
    end
    else
    begin
      if Assigned(FOnUpClick) then FOnUpClick(Self, FIncrement);
    end;
  end;
end;

procedure TElSpinButton.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited;
  if (csDesigning in ComponentState) or (Button <> mbLeft) then exit;

  if ((FButtonType = sbtUpDown) and
      (Y < ClientHeight div 2)) or
     ((FButtonType = sbtLeftRight) and
      (X < ClientWidth div 2)) then
  begin
    FTopBtnDown := true;
    FValue := 0;
    if FButtonType = sbtLeftRight then
    begin
      if Assigned(FOnDownClick) then FOnDownClick(Self, FIncrement);
    end
    else
    begin
      if Assigned(FOnUpClick) then FOnUpClick(Self, FIncrement);
    end;
  end
  else
  if ((FButtonType = sbtUpDown) and
      (Y > ClientHeight div 2 + 1)) or
     ((FButtonType = sbtLeftRight) and
      (X > ClientWidth div 2 + 1)) then
  begin
    FBottomBtnDown := true;
    FValue := 0;
    if FButtonType = sbtUpDown then
    begin
      if Assigned(FOnDownClick) then FOnDownClick(Self, FIncrement);
    end
    else
    begin
      if Assigned(FOnUpClick) then FOnUpClick(Self, FIncrement);
    end;
  end
  else
  begin
    if not UseDrag then exit;
    if (not (csDesigning in ComponentState)) and
       (((FButtonType = sbtUpDown) and
         (Screen.Cursor <> crSizeNS)) or
        ((FButtonType = sbtLeftRight) and
         (Screen.Cursor <> crSizeWE))) then
    begin
      SaveCursor := Screen.Cursor;
      if FButtonType = sbtLeftRight then
        Screen.Cursor := crSizeWE
      else
        Screen.Cursor := crSizeNS;
    end;
    FDragging := true;
    FStartValue := 0;
    if Assigned(FOnSpinStart) then
      FOnSpinStart(Self, FStartValue);
    {$ifndef CLX_USED}
    SetCapture(Handle);
    {$else}
    SetMouseGrabControl(Self);
    {$endif}
    exit;
  end;
  {$ifndef CLX_USED}
  SetCapture(Handle);
  {$else}
  SetMouseGrabControl(Self);
  {$endif}
  FCaptured := true;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := OnTimer;
  FTimer.Interval := 400;
  Invalidate;
end;

procedure TElSpinButton.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited;
  if (csDesigning in ComponentState) or (Button <> mbLeft) then exit;
  if (Screen.Cursor = crSizeNS) or (Screen.Cursor = crSizeWE) then
     Screen.Cursor := SaveCursor;
  if FDragging then
  begin
    {$ifndef CLX_USED}
    ReleaseCapture;
    {$else}
    SetMouseGrabControl(nil);
    {$endif} 
    FDragging := false;
    exit;
  end;
  if FTimer <> nil then
  begin
    FTimer.Enabled := false;
    FTimer.Free;
    FTimer := nil;
  end;
  {$ifndef CLX_USED}
  ReleaseCapture;
  {$else}
  SetMouseGrabControl(nil);
  {$endif}
  FTopBtnDown := false;
  FBottomBtnDown := false;
  Invalidate;
end;

function TElSpinButton.GetPopupMenu: TPopupMenu;
var p : TPoint;
begin
  if FDragging then
  begin
    GetCursorPos(p);
    p := ScreenToClient(p);
    MouseUp(mbLeft, [], p.x, p.y);
  end;
  result := inherited GetPopupMenu;
end;

{$ifndef CLX_USED}
procedure TElSpinButton.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  with TMessage(Msg) do
    SendMessage(Parent.Handle, Msg, wParam, lParam);
end;
{$endif}

procedure TElSpinButton.MouseMove(Shift : TShiftState; X, Y : Integer);
var b : boolean;
begin
  inherited;
  if FDragging then
  begin
    if Assigned(FOnSpinDrag) then
    begin
      if FButtonType = sbtUpDown then
        FOnSpinDrag(self, FStartValue + (ClientHeight div 2 - Y) * FIncrement)
      else
        FOnSpinDrag(self, FStartValue + (X - ClientWidth div 2) * FIncrement);
    end;
  end
  else
  begin
    if ((FButtonType = sbtUpDown) and
      (Y < ClientHeight div 2)) or
     ((FButtonType = sbtLeftRight) and
      (X < ClientWidth div 2)) then
    begin
      b := FMouseInUpPart;
      FMouseInUpPart := true;
      FMouseInDownPart := false;
      if not b then Invalidate;
    end
    else
    if ((FButtonType = sbtUpDown) and
      (Y > ClientHeight div 2 + 1)) or
     ((FButtonType = sbtLeftRight) and
      (X > ClientWidth div 2 + 1)) then
    begin
      b := FMouseInDownPart;
      FMouseInUpPart := false;
      FMouseInDownPart := true;
      if not b then Invalidate;
    end
    else
    begin
      b := FMouseInUpPart or FMouseInDownPart;
      FMouseInUpPart := false;
      FMouseInDownPart := false;
      if b then Invalidate;
    end;
  end;
end;

{$ifndef CLX_USED}
procedure TElSpinButton.CMExit(var Msg : TCMExit);
{$else}
procedure TElSpinButton.DoExit;
{$endif}
begin
  inherited;
  Invalidate;
end; {CMExit}

procedure TElSpinButton.IntMouseMove(XPos, YPos : SmallInt);
begin
  if ((FButtonType = sbtUpDown) and
     (((YPos = ClientHeight div 2) or (YPos = ClientHeight div 2 + 1))
      and InRange(0, Width, XPos))) or
     ((FButtonType = sbtLeftRight) and
     (((XPos = ClientWidth div 2) or (XPos = ClientWidth div 2 + 1))
      and InRange(0, Height, YPos))) then
  begin
    if (not FCaptured) and (not FDragging) and UseDrag then
    begin
      {$ifndef CLX_USED}
      SetCapture(Handle);
      {$else}
      SetMouseGrabControl(Self);
      {$endif}
      FCaptured := true;

      if (not (csDesigning in ComponentState)) and
       (((FButtonType = sbtUpDown) and
         (Screen.Cursor <> crSizeNS)) or
        ((FButtonType = sbtLeftRight) and
        (Screen.Cursor <> crSizeWE)))
       then
      begin
        SaveCursor := Screen.Cursor;
        if FButtonType = sbtUpDown then
          Screen.Cursor := crSizeNS
        else
          Screen.Cursor := crSizeWE;
      end;
    end;
  end
  else
  if (not FDragging) and (FCaptured) then
  begin
    if (FTimer = nil) then
    begin
      {$ifndef CLX_USED}
      ReleaseCapture;
      {$else}
      SetMouseGrabControl(nil);
      {$endif}
      FCaptured := false;
    end;
    if (Screen.Cursor = crSizeNS) or (Screen.Cursor = crSizeWE) then
       Screen.Cursor := SaveCursor;
  end;
end;

{$ifdef CLX_USED}
function TElSpinButton.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  result := inherited EventFilter(Sender, Event);
  if QEvent_type(Event) = QEventType_MouseMove then
    IntMouseMove(QMouseEvent_x(QMouseEventH(Event)), QMouseEvent_y(QMouseEventH(Event)));
end;
{$endif}

{$ifndef CLX_USED}
procedure TElSpinButton.WMMouseMove(var Msg : TWMMouseMove);
begin
  IntMouseMove(Msg.XPos, Msg.YPos);
  inherited;
end; {WMMouseMove}
{$endif}

procedure TElSpinButton.SetUseDrag(newValue : Boolean);
{ Sets data member FUseDrag to newValue. }
begin
  if (FUseDrag <> newValue) then
    FUseDrag := newValue;
end;  { SetUseDrag }

procedure TElSpinButton.SetFlat(newValue : Boolean);
{ Sets data member FFlat to newValue. }
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    Invalidate;
  end;  { if }
end;  { SetFlat }

{$ifndef CLX_USED}
procedure TElSpinButton.CMEnabledChanged(var Msg : TMessage);  { private }
{$else}
procedure TElSpinButton.EnabledChanged;
{$endif}
begin
  inherited;
  Invalidate;
end;  { CMEnabledChanged }

procedure TElSpinButton.SetButtonDirection(Value: TElSpinBtnDir);
begin
  if FButtonDirection <> Value then
  begin
    FButtonDirection := Value;
    if (not (csLoading in ComponentState)) then Invalidate;
  end;
end;

procedure TElSpinButton.SetButtonType(Value: TElSpinBtnType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;
    if (not (csLoading in ComponentState)) then Invalidate;
  end;
end;

{$ifndef CLX_USED}
procedure TElSpinButton.WMContextMenu(var Msg : TMessage);
begin
  if FTimer <> nil then
    MouseUp(mbLeft, [], Msg.lParam and $FFFF, Msg.lParam shr 16);
  inherited;
end;  { WMContextMenu }
{$endif}

{$ifndef CLX_USED}
procedure TElSpinButton.CMMouseLeave(var Msg : TMessage);
begin
  inherited;
  FMouseInUpPart := false;
  FMouseInDownPart := false;
  if IsThemeApplied or MoneyFlat then
    Invalidate;
end; { CMMouseLeave }
{$endif}

function TElSpinButton.GetThemedClassName: WideString;
begin
  Result := 'SPIN';
end;

procedure TElSpinButton.SetUpArrowEnabled(Value: Boolean);
begin
  if FUpArrowEnabled <> Value then
  begin
    FUpArrowEnabled := Value;
    if Enabled then Invalidate;
  end;
end;

procedure TElSpinButton.SetDownArrowEnabled(Value: Boolean);
begin
  if FDownArrowEnabled <> Value then
  begin
    FDownArrowEnabled := Value;
    if Enabled then Invalidate;
  end;
end;

procedure TElSpinButton.SetMoneyFlat(Value: Boolean);
begin
  if FMoneyFlat <> Value then
  begin
    FMoneyFlat := Value;
    Invalidate;
  end;
end;

procedure TElSpinButton.SetMoneyFlatActiveColor(Value: TColor);
begin
  if FMoneyFlatActiveColor <> Value then
  begin
    FMoneyFlatActiveColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

procedure TElSpinButton.SetMoneyFlatDownColor(Value: TColor);
begin
  if FMoneyFlatDownColor <> Value then
  begin
    FMoneyFlatDownColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

procedure TElSpinButton.SetMoneyFlatInactiveColor(Value: TColor);
begin
  if FMoneyFlatInactiveColor <> Value then
  begin
    FMoneyFlatInactiveColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

function TElSpinButton.GetMoneyFlat: Boolean;
begin
  Result := FMoneyFlat;
end;

procedure TElSpinButton.SetOldStyled(Value: Boolean);
begin
  if FOldStyled <> Value then
  begin
    FOldStyled := Value;
    Invalidate;
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElSpinButton.CMHintShow(var Message: TMessage);
{$else}
function TElSpinButton.HintShow(var HintInfo : THintInfo): Boolean; 
{$endif}
var T: WideChar;
{$ifndef CLX_USED}
  HintInfo : PHintInfo;
{$endif}
  l : integer;
  S : String;
  WS: WideString;
begin
{$ifndef CLX_USED}
  inherited;
  HintInfo := PHintInfo(Message.lParam);
{$else}
  inherited HintShow(HintInfo);
  result := true;
{$endif}
  if Length(FHint) = 0 then
  begin
    HintInfo.hintStr := '';
    exit;
  end;

  S := GetShortHint(inherited Hint);
  if HintInfo.HintStr = S then
  begin
    WS := GetShortHintW(FHint);
  end
  else
  begin
    S := FHint;
    WS := FHint;
  end;

  l := Length(S) + 1 + Length(WS) * 2;
  SetLength(HintInfo.HintStr, l + 4);
  Move(PChar(S)^, HintInfo.HintStr[1], Length(S) + 1);

  Move(WS[1], HintInfo.HintStr[Length(S) + 2], Length(WS) * 2);
  T := #0;
  Move(T, HintInfo.HintStr[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, HintInfo.HintStr[l + 3], sizeof(T));
end;

procedure TElSpinButton.SetHint(Value: WideString);
var S : String;
    i,
    l : integer;
    T : WideChar;
begin
  FHint := Value;

  S := FHint;
  i := Length(S);
  l := Length(S) + 1 + Length(FHint) * 2;
  SetLength(S, l + 4);

  Move(FHint[1], S[i + 2], Length(FHint) * 2);
  T := #0;
  Move(T, S[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, S[l + 3], sizeof(T));
  inherited Hint := S;
end;

{$endif}

end.

