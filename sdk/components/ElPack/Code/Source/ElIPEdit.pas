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

02/21/2002

  Improved positioning of the text
  Changed editor ancestor to TElEdit
  
01/27/2002

  Text was not with some fonts set. Fixed

01/01/2002

  Fixed some problems with painting borders when focus is moved

12/21/2001

  Points are now drawn correctly for all fonts

10/25/2001

  Fixed painting of the borders with XP styles enabled
  
07/23/2001

  OnChange event was not fired. Fixed.

07/12/2001

  BorderSides property added.

1/8/2001

 Original version created.

*)

unit ElIPEdit;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  WinSock,
  Menus,
  ElUxTheme,
  ElXPThemedControl,
  ElTools,
  ElEdits,
  ElVCLUtils,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElImgFrm;

type

  TElIPPartEdit = class(TElEdit)
  private
    OnPoint : TNotifyEvent;
    OnLeftPoint: TNotifyEvent;
    procedure WMChar(var Message: TMessage); message WM_CHAR;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  protected
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
  end;

  IntEditClass  = TElIPPartEdit;

  TElIPEdit = class (TElXPThemedControl)
  private
    FBorderStyle: TBorderStyle;
    FFlat : boolean;
    FActiveBorderType: TElFlatBorderType;
    FInactiveBorderType: TElFlatBorderType;
    FMouseOver : boolean;
    FModified  : boolean;

    FOnChange: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;

    FPartCanEdit: array [1..4] of boolean;
    FPartEditors: array[1..4] of IntEditClass;
    FParts: array[1..4] of byte;
    FBorderSides: TElBorderSides;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    function FindPart(Editor : IntEditClass) : integer;
    function GetPart(Index: Integer): Byte;
    function GetPartCanEdit(Index: Integer): Boolean;
    procedure SetPart(Index: Integer; Value: Byte);
    procedure SetPartCanEdit(Index: Integer; Value: Boolean);
    function GetIPAddress : DWORD;
    procedure SetIPAddress(Value: DWORD);
    procedure SetFlat(const Value: boolean);
    procedure SetActiveBorderType(const Value: TElFlatBorderType);
    procedure SetInactiveBorderType(const Value: TElFlatBorderType);
    procedure SetBorderStyle(Value : TBorderStyle);
    procedure UpdateFrame;
    procedure SetModified(Value : boolean);

    procedure SetIPString(value : string);
    function  GetIPString : string;
    procedure SetBorderSides(Value: TElBorderSides);
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;

    procedure ClickHandler(Sender: TObject);
    {$ifdef VCL_5_USED}
    procedure ContextPopupHandler(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    {$endif}
    procedure DblClickHandler(Sender: TObject);
    procedure DragDropHandler(Sender: TObject; Source: TObject; X: Integer; Y: Integer);
    procedure DragOverHandler(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);
    procedure EndDragHandler(Sender: TObject; Target: TObject; X: Integer; Y: Integer);
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPressHandler(Sender: TObject; var Key: Char);
    procedure KeyUpHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure MouseEnterHandler(Sender: TObject);
    procedure MouseLeaveHandler(Sender: TObject);
    procedure MouseMoveHandler(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
    procedure MouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure StartDragHandler(Sender: TObject; var DragObject: TDragObject);
  protected
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    FChangeDisabledText: Boolean;
    procedure DrawFlatBorder(DC: HDC);

    procedure AdjustEditorPositions; virtual;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;

    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;

    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure OnEditorChange(Sender : TObject);
    procedure OnEditorEnter(Sender :TObject);
    procedure OnEditorExit(Sender :TObject);
    procedure OnEditorPoint(Sender : TObject);
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnEditorLeftPoint(Sender : TObject);
    procedure TriggerChangeEvent; virtual;
    function GetThemedClassName: WideString; override;
    procedure Loaded; override;
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
    procedure SetChangeDisabledText(Value: Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    property IPAddress : DWORD read GetIPAddress write SetIPAddress;
    property MouseOver : boolean read FMouseOver;
    property Modified  : boolean read FModified write SetModified;
  published
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
    property BorderStyle : TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Flat : boolean read FFlat write SetFlat default false;
    property ActiveBorderType: TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;
    property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;

    property Part1: Byte index 1 read GetPart write SetPart default 0;
    property Part2: Byte index 2 read GetPart write SetPart default 0;
    property Part3: Byte index 3 read GetPart write SetPart default 0;
    property Part4: Byte index 4 read GetPart write SetPart default 0;

    property Part1CanEdit: Boolean index 1 read GetPartCanEdit write SetPartCanEdit default true;
    property Part2CanEdit: Boolean index 2 read GetPartCanEdit write SetPartCanEdit default true;
    property Part3CanEdit: Boolean index 3 read GetPartCanEdit write SetPartCanEdit default true;
    property Part4CanEdit: Boolean index 4 read GetPartCanEdit write SetPartCanEdit default true;

    property IPString  : string read GetIPString write SetIPString;
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
        SetLineBorderInactiveColor;
    property ChangeDisabledText: Boolean read FChangeDisabledText write 
        SetChangeDisabledText default false;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    {$IFDEF VCL_4_USED}
    property Anchors;
    {$ENDIF}
    property Color;
    {$IFDEF VCL_4_USED}
    property Constraints;
    {$ENDIF}
    property Ctl3D default true;
    property DragCursor;
    {$IFDEF VCL_4_USED}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default false;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default true;
    property Visible;
    property UseXPThemes;

    property OnClick;
    {$IFDEF VCL_5_USED}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF VCL_4_USED}
    property OnEndDock;
    property OnEndDrag;
    {$ENDIF}
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF VCL_4_USED}
    property OnStartDock;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

{
********************************** TElIPPartEdit ***********************************
}

procedure TElIPPartEdit.WMChar(var Message: TMessage);
begin
  if TWMChar(Message).CharCode = Ord('.') then
    if Assigned(OnPoint) then
      OnPoint(Self)
    else
  else
    inherited;
end;

procedure TElIPPartEdit.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  Message.Result := {DLGC_WANTCHARS or }DLGC_WANTARROWS;
end;

procedure TElIPPartEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  if (Message.CharCode = VK_LEFT) and (KeyDataToShiftState(Message.KeyData) = []) and (Self.SelStart = 0) then
    if Assigned(OnLeftPoint) then
      OnLeftPoint(Self)
    else

  else
  if (Message.CharCode = VK_RIGHT) and (KeyDataToShiftState(Message.KeyData) = []) and (Self.SelStart = Length(Text)) then
    if Assigned(OnPoint) then
      OnPoint(Self)
    else
    
  else
    inherited;
end;

procedure TElIPPartEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  with Parent as TElIPEdit do
    if FFlat or IsThemeApplied then
      DrawFlatBorder(0);
end;

procedure TElIPPartEdit.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  with Parent as TElIPEdit do
    if FFlat or IsThemeApplied then
      DrawFlatBorder(0);
end;

{
********************************** TElIPEdit ***********************************
}
constructor TElIPEdit.Create(AOwner : TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  {$ifdef MSWINDOWS}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}
  Ctl3D := true;
  for i :=  1 to 4 do
  begin
    FPartEditors[i] := IntEditClass.Create(Self);
    with FPartEditors[i] do
    begin
      Parent := Self;
      BorderStyle := bsNone;
      OnChange := OnEditorChange;
      OnExit   := OnEditorExit;
      OnPoint  := OnEditorPoint;
      OnLeftPoint  := OnEditorLeftPoint;
      OnClick := ClickHandler;
      {$ifdef VCL_5_USED}
      OnContextPopup := ContextPopupHandler;
      {$endif}
      OnDblClick := DblClickHandler;
      OnDragDrop := DragDropHandler;
      OnDragOver := DragOverHandler;
      OnEndDrag := EndDragHandler;
      OnKeyDown := KeyDownHandler;
      OnKeyPress := KeyPressHandler;
      OnKeyUp := KeyUpHandler;
      OnMouseDown := MouseDownHandler;
      OnMouseEnter := MouseEnterHandler;
      OnMouseLeave := MouseLeaveHandler;
      OnMouseMove := MouseMoveHandler;
      OnMouseUp := MouseUpHandler;
      OnStartDrag := StartDragHandler;

      AutoSelect := true;
      AutoSize := false;
      MaxLength := 3;
      Text := '0';
      Alignment := taCenter;
      ParentColor := True;
      TabStop := false;
      Font := Self.Font;
      LeftMargin := 0;
      RightMargin := 0;
    end;
    FPartCanEdit[i] := true;
  end;
  TabStop := true;
  Width := 100;
  Height := 21;
  ParentColor := false;
  Color := clWindow;
  FInactiveBorderType := fbtSunkenOuter;
  FActiveBorderType := fbtSunken;
end;

destructor TElIPEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TElIPEdit.DrawFlatBorder(DC: HDC);
var
  BorderType: TElFlatBorderType;
  MustRelease: boolean;
  R,
  R1 : TRect;
  b  : boolean;
  ax,
  ay : integer;
  AColor : TColor;
  
begin
  if ((not FFlat) and (not IsThemeApplied)) or (BorderStyle = bsNone) then
    exit;

  if IsThemeApplied then
  begin
    DC := GetWindowDC(Handle);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    (*
    R1 := ClientRect;
    OffsetRect(R1, ClientOrigin.X - Left, ClientOrigin.Y - Top);
    *)
    R1 := ClientRect;
    R1.TopLeft := Parent.ScreenToClient(ClientToScreen(R1.TopLeft));

    ax := Left - R1.Left;
    ay := Top  - R1.Top;

    R1 := ClientRect;
    OffsetRect(R1, -ax, -ay);

    with R1 do
      ExcludeClipRect(DC, Left, Top, Right, Bottom);

    DrawThemeBackground(Theme, DC, 0, 0, R, nil);
    ReleaseDC(Handle, DC);
  end
  else
  begin
    MustRelease := (DC = 0);
    if MustRelease then
      DC := GetWindowDC(Handle);
    try
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      b := Focused or (GetParent(Getfocus) = Handle) or FMouseOver;
      if b then
        BorderType := FActiveBorderType
      else
        BorderType := FInactiveBorderType;
      if b then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;

      DrawFlatFrameEx2(DC, R, Acolor, Color, b, Enabled, FBorderSides, BorderType);
    finally
      if MustRelease then
        ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TElIPEdit.Paint;
var i,
    cx,
    w : integer;
    s : string;
begin
  if FFlat and (not IsThemeApplied) then
    DrawFlatBorder(Canvas.Handle);

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);

  w := (ClientWidth - 15) div 4;
  cx := w + 2;
  S := '.';
  SetTextAlign(Canvas.Handle, TA_BASELINE);

  for i := 1 to 3 do
  begin
    TextOut(Canvas.Handle, cx, (ClientHeight + Abs(Font.Height)) div 2 - 1, PChar(S), 1);
    inc(cx, w + 5);
  end;
end;

procedure TElIPEdit.AdjustEditorPositions;
var cx, w,
    i  : integer;
    tm : TTextMetric;
begin
  cx := 0;
  w := (ClientWidth - 15) div 4;
  GetTextMetrics(Canvas.Handle, tm);
  for i := 1 to 4 do
  begin
    FPartEditors[i].SetBounds(cx, 0, w, ClientHeight);
    FPartEditors[i].AlignBottom := false;
    FPartEditors[i].TopMargin := (ClientHeight - Abs(Font.Height)) div 2;
    inc(cx, w + 5);
  end;
end;

procedure TElIPEdit.CMColorChanged(var Message: TMessage);
var i : integer;
begin
  inherited;
  for i := 1 to 4 do
    FPartEditors[i].Color := Color;
end;


procedure TElIPEdit.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if FFlat and (not Focused) and (not IsThemeApplied) then
    DrawFlatBorder(0);
  DoMouseEnter;
end;

procedure TElIPEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if FFlat and (not Focused) and (not IsThemeApplied) then
    DrawFlatBorder(0);
  DoMouseLeave;
end;

procedure TElIPEdit.CMEnabledChanged(var Message: TMessage);
var i : integer;
begin
  inherited;
  for i := 1 to 4 do
    FPartEditors[i].Enabled := Enabled;
  UpdateFrame;
end;

procedure TElIPEdit.CMFontChanged(var Message: TMessage);
var i : integer;
begin
  inherited;
  for i := 1 to 4 do
    FPartEditors[i].Font.Assign(Font);
  AdjustEditorPositions;
  UpdateFrame;
end;

procedure TElIPEdit.CMSysColorChange(var Message: TMessage);
var i : integer;
begin
  inherited;
  for i := 1 to 4 do
    FPartEditors[i].Color := Color;
  Invalidate;
  UpdateFrame;
end;

procedure TElIPEdit.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.result := 1;
end;

procedure TElIPEdit.WMNCPaint(var Msg : TMessage);
var DC : HDC;
begin
  if not Flat and (BorderStyle = bsSingle) then
    inherited;
  if Flat or IsThemeApplied then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC <> 0 then
       DrawFlatBorder(DC)
    else
    begin
      DC := GetWindowDC(Handle);
      DrawFlatBorder(DC);
    end;
    ReleaseDC(Handle, DC);
    Msg.Result := 0;
  end;
end;

type THackWinControl = class(TWinControl)
     end;

procedure TElIPEdit.WMSetFocus(var Msg: TMessage);
var R, C : TWinControl;
begin
  inherited;
  R := FindControl(Msg.WParam);
  if (R <> nil) and (FindPart(TElIPPartEdit(R)) <> -1) then
  begin
   C := THackWinControl(Parent).FindNextControl(Self, false, true, true);
   if C <> nil then
     C.SetFocus;
  end
  else
    FPartEditors[1].SetFocus;
  if FFlat or IsThemeApplied then
    DrawFlatBorder(0);
end;

procedure TElIPEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  if not HandleAllocated then exit;
  if FFlat or IsThemeApplied then
    DrawFlatBorder(0);
end;

procedure TElIPEdit.WMSize(var Message: TMessage);
begin
  inherited;
  AdjustEditorPositions;
end;

procedure TElIPEdit.CreateWindowHandle(const Params: TCreateParams);
var i : integer;
begin
  inherited;
  for i :=  1 to 4 do
    SetWindowLong(FPartEditors[i].Handle, GWL_STYLE, GetWindowLong(
      FPartEditors[i].Handle, GWL_STYLE) or ES_NUMBER);
end;

function TElIPEdit.GetPart(Index: Integer): Byte;
begin
  result := FParts[Index];
end;

function TElIPEdit.GetPartCanEdit(Index: Integer): Boolean;
begin
  result := FPartCanEdit[Index];
end;

function TElIPEdit.FindPart(Editor : IntEditClass) : integer;
var i : integer;
begin
  result := -1;
  for i := 1 to 4 do
  begin
    if Editor = FPartEditors[i] then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TElIPEdit.OnEditorChange(Sender : TObject);
var i, j : integer;
    p    : integer;
    s    : string;
begin
  p := FindPart(IntEditClass(Sender));
  S := IntEditClass(Sender).Text;

  val(S, i, j);
  if (j = 0) then
  begin
    if (i <= 255) then
    begin
      FParts[p] := i;
      SetModified(true);
      TriggerChangeEvent;
      exit;
    end
    else
    if Length(S) >= 2 then
    begin
      i := StrToIntDef(Copy(S, Length(S) - 2, 3), 256);
      if i < 255 then
        FParts[p] := i
      else
      begin
        i := StrToIntDef(Copy(S, Length(S) - 1, 2), 256);
        if i < 255 then
          FParts[p] := i;
      end;
      SetModified(true);
      TriggerChangeEvent;
    end;
  end;
  if Length(s) > 0 then
  begin
    if IntEditClass(Sender).HandleAllocated then
      i := IntEditClass(Sender).SelStart
    else
      i := 0;
    IntEditClass(Sender).Text := IntToStr(FParts[p]);
    if IntEditClass(Sender).HandleAllocated then
      IntEditClass(Sender).SelStart := Min(i, Length(IntEditClass(Sender).Text));
  end;
end;

procedure TElIPEdit.OnEditorEnter(Sender :TObject);
begin
  IntEditClass(Sender).SelLength := 0;
  IntEditClass(Sender).SelStart := 0;
end;

procedure TElIPEdit.OnEditorExit(Sender :TObject);
begin
  IntEditClass(Sender).SelLength := 0;
  if IntEditClass(Sender).Text = '' then
  begin
    IntEditClass(Sender).Text := IntToStr(FParts[FindPart(IntEditClass(Sender))]);
    OnEditorChange(Sender);
  end;
end;

procedure TElIPEdit.OnEditorPoint(Sender : TObject);
var p : integer;
begin
  p := FindPart(IntEditClass(Sender));
  if (p < 4) then
    FPartEditors[p + 1].SetFocus;
end;

procedure TElIPEdit.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TElIPEdit.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    if not Focused and not FMouseOver then UpdateFrame;
  end;
end;

procedure TElIPEdit.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    if (Focused or FMouseOver) then UpdateFrame;
  end;
end;

procedure TElIPEdit.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if HandleAllocated then
      if Flat then
        Invalidate
      else
        RecreateWnd;
  end;
end;

procedure TElIPEdit.SetBorderStyle(Value : TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

function TElIPEdit.GetIPAddress : DWORD;
begin
  result := PInteger(@FParts[1])^;
end;

procedure TElIPEdit.SetIPAddress(Value: DWORD);
begin
  Value := SwapInt32(Value);
  Part1 := Value shr 24;
  Part2 := Value shr 16 and $FF;
  Part3 := Value shr 8 and $FF;
  Part4 := Value and $FF;
  SetModified(false);
end;

procedure TElIPEdit.SetIPString(value : string);
var i : DWORD;
begin
  i := inet_addr(PAnsiChar(value));
  if Value = '255.255.255.255' then
     SetIPAddress($FFFFFFFF)
  else
  if i = DWORD(INADDR_NONE) then
    Raise Exception.Create('Invalid IP address string')
  else
    SetIPAddress(i);
end;

function  TElIPEdit.GetIPString : string;
var ina : TInAddr;
begin
  ina.S_addr := IPAddress;
  result := StrPas(inet_ntoa(ina));
end;

procedure TElIPEdit.SetPart(Index: Integer; Value: Byte);
begin
  with FPartEditors[Index] do
  begin
    FParts[index] := Value;
    Text := IntToStr(Value);
    SelStart := 0;
  end;
end;

procedure TElIPEdit.SetPartCanEdit(Index: Integer; Value: Boolean);
begin
  FPartCanEdit[Index] := Value;
  FPartEditors[Index].ReadOnly := not Value;
end;

procedure TElIPEdit.SetModified(Value : boolean);
begin
  FModified := Value; 
end;

procedure TElIPEdit.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TElIPEdit.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TElIPEdit.UpdateFrame;
var R : TRect;
begin
  if Flat then
  begin
    R := Rect(0, 0, Width, Height);
    if (BorderStyle = bsSingle) and (not (csDestroying in ComponentState)) and (HandleAllocated) then
       RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame );
  end;
end;

procedure TElIPEdit.OnEditorLeftPoint(Sender : TObject);
var p : integer;
begin
  p := FindPart(IntEditClass(Sender));
  if (p > 1) then
    FPartEditors[p - 1].SetFocus;
end;
procedure TElIPEdit.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TElIPEdit.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) and (not (ThemesAvailable and UseXPThemes)) then
  begin
    if not (ebsLeft in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if not (ebsTop in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if not (ebsRight in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if not (ebsBottom in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end;
    // Message.Result := WVR_REDRAW;
end;

procedure TElIPEdit.TriggerChangeEvent;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TElIPEdit.ClickHandler(Sender: TObject);
begin
  Click;
end;  { ClickHandler }

{$ifdef VCL_5_USED}
procedure TElIPEdit.ContextPopupHandler(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  DoContextPopup(MousePos, Handled);
end;  { ContextPopupHandler }
{$endif}

procedure TElIPEdit.DblClickHandler(Sender: TObject);
begin
  DblClick;
end;  { DblClickHandler }

procedure TElIPEdit.DragDropHandler(Sender: TObject; Source: TObject; X: Integer; Y: Integer);
begin
  DragDrop(Source, X, Y);
end;  { DragDropHandler }

procedure TElIPEdit.DragOverHandler(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);
begin
  DragOver(Source, X, Y, State, Accept);
end;  { DragOverHandler }

procedure TElIPEdit.EndDragHandler(Sender: TObject; Target: TObject; X: Integer; Y: Integer);
begin
  DoEndDrag(Target, X, Y);
end;  { EndDragHandler }

procedure TElIPEdit.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;  { KeyDownHandler }

procedure TElIPEdit.KeyPressHandler(Sender: TObject; var Key: Char);
begin
  KeyPress(Key);
end;  { KeyPressHandler }

procedure TElIPEdit.KeyUpHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyUp(Key, Shift);
end;  { KeyUpHandler }

procedure TElIPEdit.MouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  MouseDown(Button, Shift, X, Y);
end;  { MouseDownHandler }

procedure TElIPEdit.MouseEnterHandler(Sender: TObject);
begin
  DoMouseEnter;
end;  { MouseEnterHandler }

procedure TElIPEdit.MouseLeaveHandler(Sender: TObject);
begin
  DoMouseLeave;
end;  { MouseLeaveHandler }

procedure TElIPEdit.MouseMoveHandler(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
begin
  MouseMove(Shift, X, Y);
end;  { MouseMoveHandler }

procedure TElIPEdit.MouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  MouseUp(Button, Shift, X, Y);
end;  { MouseUpHandler }

procedure TElIPEdit.StartDragHandler(Sender: TObject; var DragObject: TDragObject);
begin
  DoStartDrag(DragObject);
end;  { StartDragHandler }

function TElIPEdit.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

procedure TElIPEdit.Loaded;
begin
  inherited;
  AdjustEditorPositions;
end;

procedure TElIPEdit.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TElIPEdit.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then
    if HandleAllocated then
      Invalidate;
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElIPEdit.CMHintShow(var Message: TMessage);
{$else}
function TElIPEdit.HintShow(var HintInfo : THintInfo): Boolean; 
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

procedure TElIPEdit.SetHint(Value: WideString);
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

procedure TElIPEdit.SetChangeDisabledText(Value: Boolean);
var i : integer;
begin
  if FChangeDisabledText <> Value then
  begin
    FChangeDisabledText := Value;
    for I := 1 to 4 do
    begin
      FPartEditors[I].ChangeDisabledText := Value;
    end;
  end;
end;

end.
