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

09/26/2001

  Added LinkStyle, LinkColor and LinkPopupMenu properties
  Fixed link clicking
  Fixed image painting

*)

unit ElHTMLPanel;

interface

uses

  SysUtils,
  Classes,

  ElPanel,
  ElVCLUtils,
  HTMLRender,
  ElTools,

  {$ifdef VCL_4_USED}
  ImgList,
  {$endif}
  Menus,
  Windows,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElStrUtils,
  ExtCtrls,
  Messages;

type

  TCustomElHTMLPanel = class(TCustomElPanel)
  private
    FOnImageNeeded: TElHTMLImageNeededEvent;
    FOnLinkClick: TElHTMLLinkClickEvent;
    FCursor: TCursor;
    FLinkColor: TColor;
    FLinkPopupMenu: TPopupMenu;
    FLinkStyle: TFontStyles;
    FTextRect: TRect;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
  protected
    FRender : TElHTMLRender;
    procedure TriggerPaintEvent; override;
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image
        : TBitmap);
    procedure TriggerLinkClickEvent(HRef : TElFString); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    procedure SetCursor(newValue : TCursor); virtual;
    procedure SetLinkPopupMenu(newValue : TPopupMenu); virtual;
    procedure SetLinkColor(newValue : TColor); virtual;
    procedure SetLinkStyle(newValue : TFontStyles); virtual;
    procedure SetCaption(newValue: TElFString); override;
    {$ifdef VCL_5_USED}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$endif}
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure DoLinkPopup(MousePos : TPoint);

    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        FOnImageNeeded;
    property OnLinkClick: TElHTMLLinkClickEvent read FOnLinkClick write
        FOnLinkClick;
    property Cursor: TCursor read FCursor write SetCursor;
    property LinkColor: TColor read FLinkColor write SetLinkColor;
    property LinkPopupMenu: TPopupMenu read FLinkPopupMenu write SetLinkPopupMenu;
    property LinkStyle: TFontStyles read FLinkStyle write SetLinkStyle;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TElHTMLPanel = class(TCustomElHTMLPanel)
  published
    property OnImageNeeded;
    property OnLinkClick;
    property Cursor;
    property LinkColor;
    property LinkPopupMenu;
    property LinkStyle;

    property Background;
    property BackgroundType;
    property GradientEndColor;
    property GradientStartColor;
    property GradientSteps;
    property Alignment;
    property Layout;
    property ImageForm;
    property TopGrabHandle;
    property RightGrabHandle;
    property LeftGrabHandle;
    property BottomGrabHandle;

    property Resizable;
    property Movable;
    property OnMove;

    property Align nodefault;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Canvas;
    property Color;
{$ifdef MSWINDOWS}
    property DragCursor;
{$endif}
    property DragMode;
    property Enabled;
    property Font;
{$ifdef MSWINDOWS}
    property Locked;
    property MouseCapture;
{$endif}
    property ParentColor;
{$ifdef MSWINDOWS}
    property ParentCtl3D;
{$endif}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Transparent;
    property TransparentXPThemes;
    property UseXPThemes;
    property Visible;
    property SizeGrip;

    property Caption;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
{$ifdef MSWINDOWS}
    property DockOrientation;
    property Floating;
    property BevelKind;
    property DoubleBuffered;
    property DragKind;
{$endif}
{$ENDIF}
  end;

implementation

const Margin = 1;

constructor TCustomElHTMLPanel.Create(AOwner : TComponent);
begin
  inherited;
  FRender := TElHTMLRender.Create;
  FRender.OnImageNeeded := TriggerImageNeededEvent;
  OwnerDraw := true;
  FIntPaint := true;
  AlwaysPaintBackground := true;
  FLinkColor := clBlue;
  FLinkStyle := [fsUnderline];
end;

destructor TCustomElHTMLPanel.Destroy;
begin
  FRender.Free;
  inherited;
end;

procedure TCustomElHTMLPanel.TriggerPaintEvent;
var Rect : TRect;
    R1   : TRect;
begin
  Rect := ClientRect;
  AdjustClientRect(Rect);
  // Canvas.Brush.Color := Color;
  // Canvas.FillRect(Rect);
  InflateRect(Rect, - Margin * 2, 0);

  FRender.Data.DefaultBgColor := clNone;
  FRender.Data.DefaultColor  := Font.Color;
  FRender.Data.DefaultHeight := Font.Height;
  FRender.Data.DefaultStyle := Font.Style;
  FRender.Data.DefaultFont := Font.Name;
  FRender.Data.Charset := Font.Charset;
  FRender.Data.LinkColor := LinkColor;
  FRender.Data.LinkStyle := LinkStyle;

  FRender.PrepareText(Caption, Rect.Right -Rect.Left, false);
  if Caption = '' then exit;

  case Alignment of
    taRightJustify:
      Rect.Left := Rect.Right - FRender.Data.TextSize.cx;
    taCenter:
      begin
        CenterRects(FRender.Data.TextSize.cx, Rect.Right- Rect.Left,
                    FRender.Data.TextSize.cy, Rect.Bottom - Rect.Top, R1);
        OffsetRect(R1, Rect.Left, Rect.Top);
        Rect.Left := R1.Left;
        Rect.Right := R1.Right;
      end;
  end;
  case Layout of
    tlBottom:
      Rect.Top := Rect.Bottom - FRender.Data.TextSize.cy;
    tlCenter:
      begin
        CenterRects(FRender.Data.TextSize.cx, Rect.Right- Rect.Left,
                    FRender.Data.TextSize.cy, Rect.Bottom - Rect.Top, R1);
        OffsetRect(R1, Rect.Left, Rect.Top);
        Rect.Top := R1.Top;
        Rect.Bottom := R1.Bottom;
      end;
  end;
  FTextRect := Rect;
  with Rect do
    if (Top < bottom) and (Left < Right) then
    begin
      FRender.DrawText(Canvas, Point(0, 0), Rect, clNone);
      if (ShowFocus) and (GetFocus = Self.Handle) then
      begin
        InflateRect(Rect, 1, 1);
        OffsetRect(Rect, 0, 1);
        Canvas.DrawFocusRect(Rect);
      end;
    end;
end;

procedure TCustomElHTMLPanel.TriggerImageNeededEvent(Sender : TObject; Src : 
    TElFString; var Image : TBitmap);
begin
  Image := nil;
  if (assigned(FOnImageNeeded)) then
      FOnImageNeeded(Self, Src, Image);
end;  { TriggerImageNeededEvent }

procedure TCustomElHTMLPanel.TriggerLinkClickEvent(HRef : TElFString);
begin
  if (assigned(FOnLinkClick)) then
    FOnLinkClick(Self, HRef );
end;  { TriggerLinkClickEvent }

procedure TCustomElHTMLPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var href : TElFString;
begin
  if FRender.IsCursorOverLink(Point(X - FTextRect.Left, Y - FTextRect.Top), Point(0, 0), FTextRect, href) then
     inherited Cursor := crHandPoint
  else
     inherited Cursor := FCursor;
  inherited MouseMove(Shift, X, Y);
end;  { MouseMove }

procedure TCustomElHTMLPanel.Click;
var P : TPoint;
    href : TElFString;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if FRender.IsCursorOverLink(Point(P.X - FTextRect.Left, P.Y - FTextRect.Top), Point(0, 0), FTextRect, href) then
    TriggerLinkClickEvent(href)
  else
    inherited;
end;  { Click }

procedure TCustomElHTMLPanel.SetCursor(newValue : TCursor);
var P : TPoint;
    href : TElFString;
begin
  if (FCursor <> newValue) then
  begin
    FCursor := newValue;
    GetCursorPos(P);
    P := ScreenToClient(P);
    if FRender.IsCursorOverLink(Point(P.X - FTextRect.Left, P.Y - FTextRect.Top), Point(0, 0), FTextRect, href) then
      inherited Cursor := crHandPoint
    else
      inherited Cursor := FCursor;
  end;  { if }
end;  { SetCursor }

procedure TCustomElHTMLPanel.SetLinkColor(newValue : TColor);
{ Sets data member FLinkColor to newValue. }
begin
  if (FLinkColor <> newValue) then
  begin
    FLinkColor := newValue;
    Invalidate;
  end;  { if }
end;  { SetLinkColor }

procedure TCustomElHTMLPanel.SetLinkPopupMenu(newValue : TPopupMenu);
begin
  if (FLinkPopupMenu <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FLinkPopupMenu <> nil then
      FLinkPopupMenu.RemoveFreeNotification(Self);
    {$endif}
    FLinkPopupMenu := newValue;
    if (newValue <> nil) then
       newValue.FreeNotification(Self);
  end;  { if }
end;  { SetLinkPopupMenu }

procedure TCustomElHTMLPanel.SetLinkStyle(newValue : TFontStyles);
{ Sets data member FLinkStyle to newValue. }
begin
  if (FLinkStyle <> newValue) then
  begin
    FLinkStyle := newValue;
    Invalidate;
  end;  { if }
end;  { SetLinkStyle }

procedure TCustomElHTMLPanel.SetCaption(newValue: TElFString);
{ Sets data member FCaption to newValue. }
begin
  inherited;
  Invalidate;
end; { SetCaption }

{$ifdef VCL_5_USED}
procedure TCustomElHTMLPanel.WMContextMenu(var Message: TWMContextMenu);
var
  Pt, Temp: TPoint;
  Handled: Boolean;
  href : TElFString;
begin
  if Message.Result <> 0 then Exit;
  if csDesigning in ComponentState then Exit;

  Pt := ScreenToClient(SmallPointToPoint(Message.Pos));

  if (LinkPopupMenu <> nil) and FRender.IsCursorOverLink(Point(Pt.X - FTextRect.Left, Pt.Y - FTextRect.Top), Point(0, 0), FTextRect, href) then
  begin
    Handled := False;
    Temp := ClientToScreen(Pt);                        
    DoContextPopup(Temp, Handled);
    Message.Result := Ord(Handled);
    if Handled then Exit;

    DoLinkPopup(ClientToScreen(Pt));
    Message.Result := 1;
  end
  else
    inherited;
end;
{$endif}

procedure TCustomElHTMLPanel.WMRButtonUp(var Message: TWMRButtonUp);
{$ifndef VCL_5_USED}
var P : TPoint;
    href : TElFString;
{$endif}
begin
  {$ifndef VCL_5_USED}
  P := SmallPointToPoint(Message.Pos);
  if (LinkPopupMenu <> nil) and FRender.IsCursorOverLink(Point(P.X - FTextRect.Left, P.Y - FTextRect.Top), Point(0, 0), ClientRect, href) then
  begin
    if not (csNoStdEvents in ControlStyle) then
      with Message do
        MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos);
    if Message.Result = 0 then
    begin
      DoLinkPopup(P);
    end;
  end
  else
  {$endif}
    inherited;
end;

procedure TCustomElHTMLPanel.DoLinkPopup(MousePos : TPoint);
begin
  if (FLinkPopupMenu <> nil) and FLinkPopupMenu.AutoPopup then
  begin
    SendCancelMode(nil);
    FLinkPopupMenu.PopupComponent := Self;
    if MousePos.X < 0 then
      MousePos := ClientToScreen(Point(0,0));
    FLinkPopupMenu.Popup(MousePos.X, MousePos.Y);
  end;
end;

procedure TCustomElHTMLPanel.CMMouseLeave(var Msg : TMessage);
begin
  inherited Cursor := FCursor;
  inherited;
end;  { CMMouseLeave }


end.
 
