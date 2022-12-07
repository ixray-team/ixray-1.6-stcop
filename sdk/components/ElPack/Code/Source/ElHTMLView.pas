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
  Added margins between control sides and text rectangle
  Improved vertical scrolling

10/16/2001

  When the tree is disabled, scrollbars (only when UseCustomScrollbars is true)
  are shown as disabled in Windows XP with styles enabled.
  Borders were drawn in XP style even when BorderStyle = bsNone. Fixed.
  
09/26/2001

  Fixed LinkPopupMenu that didn't work
  Fixed calculation of non-client area for control_size < size_of_scrollbar
  with no real scrollbar cases (for exmaple, when control height is 15 and no
  scrollbars are visible, non-client area was calculated as if there was a
  scrollbar)

09/10/2001

  Fixed updating after control is resized and WordWrap is set to TRUE.

08/20/2001

  Caption property made published, void property Text removed from design-time
  property list

07/12/2001

  BorderSides property added.

11/29/2000

  Fixed the problem with horizontal scrollbar, when WordWrap is on. 

11/24/2000

  Transparent property was not working. Fixed.

*)

unit ElHTMLView;  { TElHTMLView component. }

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Menus,
  StdCtrls,
  ExtCtrls,
  HTMLRender,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElScrollBar,
  ElImgFrm,
  ElXPThemedControl,
  ElUxTheme,
  ElTmSchema,
  ElStrUtils,
  ElVCLUtils;

type

  { event handlers support }
  TElFString = ElStrUtils.TElFString;
{$ifdef VCL_4_USED}
  {$NODEFINE TElFString}
{$endif}

  TElHTMLView = class(TElXPThemedControl)
  private
    FFlatFocusedScrollBars : Boolean;
    FUseCustomScrollBars : Boolean;
    FLinkPopupMenu : TPopupMenu;
    FCursor : TCursor;
    FHighlightColor : TColor;
    FHighlightBkColor : TColor;
    FOnLinkClick : TElHTMLLinkClickEvent;
    FOnImageNeeded : TElHTMLImageNeededEvent;
    FLinkColor : TColor;
    FLinkStyle : TFontStyles;
    FImgForm : TElImageForm;
    FActiveBorderType : TElFlatBorderType;
    FInactiveBorderType : TElFlatBorderType;
    FFlat : Boolean;
    FWordWrap : Boolean;
    FMouseOver: Boolean;
    FBorderStyle: TBorderStyle;
    FRender : TElHTMLRender;
    TmpDC   : HDC;
    FGradientSteps : Integer;
    FGradientStartColor : TColor;
    FGradientEndColor : TColor;
    FTmpBmp : TBitmap;
    FTransparent : Boolean;
    FBackground : TBitmap;
    FBackgroundType : TElBkGndType;
    FImgFormChLink  : TImgFormChangeLink;

    FDummyString : string;
    FViewPos  : TPoint;
    FTextRect : TRect;
    FScrollStep : integer;
    FVertScrollBar,
    FHorzScrollBar  : TElScrollBar;
    FVScrollVisible,
    FHScrollVisible : boolean;
    FVertScrollBarStyles : TElScrollBarStyles;
    FHorzScrollBarStyles : TElScrollBarStyles;
    FBorderSides: TElBorderSides;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    procedure WMSysColorChange(var Msg: TMessage); message WM_SYSCOLORCHANGE;
    procedure CMFontChanged(var Msg : TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Msg : TMessage); message CM_SYSCOLORCHANGE;
    procedure SBChanged(Sender: TObject);
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetImageForm(newValue : TElImageForm);
    procedure SetTransparent(newValue : boolean);
    procedure SetBackground(newValue : TBitmap);
    procedure SetBackgroundType(newValue : TElBkGndType);
    procedure ImageChange(Sender : TObject);
    procedure ImageFormChange(Sender : TObject);
    procedure SetGradientStartColor(newValue : TColor);
    procedure SetGradientEndColor(newValue : TColor);
    procedure SetGradientSteps(newValue : Integer);
    procedure RedoTmpBmp;
    procedure DrawFlatBorder(HorzTracking, VertTracking : boolean);
    procedure DrawFlatBorderEx(DC : HDC; HorzTracking, VertTracking : boolean);

    procedure OnHScroll(Sender: TObject; ScrollCode: TElScrollCode;
                        var ScrollPos: Integer; var DoChange : boolean);
    procedure OnVScroll(Sender: TObject; ScrollCode: TElScrollCode;
                        var ScrollPos: Integer; var DoChange : boolean);
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure CMTextChanged(var Msg : TMessage); message CM_TEXTCHANGED;
    procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure SetLinkPopupMenu(newValue : TPopupMenu);
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMNCCalcSize(var Message : TWMNcCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Msg : TMessage); message WM_NCHITTEST;
    procedure WMVScroll(var Msg : TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg : TWMHScroll); message WM_HSCROLL;

    {$ifndef CLX_USED}
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    {$else}
    procedure EnabledChanged; override;
    {$endif}

    {$IFDEF VCL_4_USED}
    procedure CMMouseWheel(var Msg : TCMMouseWheel); message CM_MOUSEWHEEL;
    {$ENDIF}
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure SetBorderSides(Value: TElBorderSides);
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    {$ifdef VCL_5_USED}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$endif}
  protected
    { Protected declarations }
    FCaption: TElFString;
    FUseXPThemes: Boolean;
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    FViewRect: TRect;
    FMargin: Integer;

    procedure SetVertScrollBarStyles(newValue : TElScrollBarStyles); virtual;
    procedure SetHorzScrollBarStyles(newValue : TElScrollBarStyles); virtual;
    procedure PrepareText; virtual;
    procedure SetViewPos(newValue : TPoint); virtual;
    procedure SetWordWrap(newValue : Boolean); virtual;
    procedure AdjustScrollBars;
    procedure Paint; override;
    procedure UpdateFrame;
    procedure SetActiveBorderType(newValue : TElFlatBorderType); virtual;
    procedure SetInactiveBorderType(newValue : TElFlatBorderType); virtual;
    procedure SetFlat(newValue : Boolean); virtual;
    procedure Notification(AComponent : TComponent; operation : TOperation); override;
    procedure CreateParams(var Params : TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure SetLinkColor(newValue : TColor); virtual;
    procedure SetLinkStyle(newValue : TFontStyles); virtual;
    procedure SetHighlightColor(newValue : TColor); virtual;
    procedure SetHighlightBkColor(newValue : TColor); virtual;
    procedure TriggerLinkClickEvent(HRef : string); virtual;
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image 
        : TBitmap); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetCursor(newValue : TCursor); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetUseCustomScrollBars(newValue : Boolean); virtual;
    procedure SetFlatFocusedScrollBars(newValue : Boolean); virtual;
    procedure Loaded; override;
    procedure SetCaption(newValue: TElFString); virtual;
    procedure IFMRepaintChildren(var Message: TMessage); message 
        IFM_REPAINTCHILDREN;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message 
        WM_WINDOWPOSCHANGED;
    procedure SetUseXPThemes(const Value: Boolean); override;
    function GetThemedClassName: WideString; override;
    procedure DoLinkPopup(MousePos : TPoint);
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);
    procedure SetViewRect(Value: TRect);
    procedure SetMargin(Value: Integer);
    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}

    property ViewRect: TRect read FViewRect write SetViewRect;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Click; override;

    property VertScrollBarVisible : boolean read FVScrollVisible;
    property HorzScrollBarVisible : boolean read FHScrollVisible;
    property Position : TPoint read FViewPos write SetViewPos;
  published
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
    property Caption: TElFString read FCaption write SetCaption;
    property VertScrollBarStyles : TElScrollBarStyles read FVertScrollBarStyles write SetVertScrollBarStyles;
    property HorzScrollBarStyles : TElScrollBarStyles read FHorzScrollBarStyles write SetHorzScrollBarStyles;
    property WordWrap   : Boolean read FWordWrap write SetWordWrap;
    property Transparent : Boolean read FTransparent write SetTransparent; { Protected }
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ActiveBorderType : TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;
    property InactiveBorderType : TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;
    property Flat : Boolean read FFlat write SetFlat;
    property ImageForm : TElImageForm read FImgForm write SetImageForm;
    property GradientStartColor : TColor read FGradientStartColor write SetGradientStartColor;  { Protected }
    property GradientEndColor : TColor read FGradientEndColor write SetGradientEndColor;  { Protected }
    property GradientSteps : Integer read FGradientSteps write SetGradientSteps default 16;  { Protected }
    property Background : TBitmap read FBackground write SetBackground;
    property BackgroundType : TElBkGndType read FBackgroundType write SetBackgroundType default bgtColorFill;
    property Cursor : TCursor read FCursor write SetCursor;  { Published }
    property LinkColor : TColor read FLinkColor write SetLinkColor default clBlue;  { Published }
    property LinkStyle : TFontStyles read FLinkStyle write SetLinkStyle;  { Published }
    property LinkPopupMenu : TPopupMenu read FLinkPopupMenu write SetLinkPopupMenu;
    property HighlightColor : TColor read FHighlightColor write SetHighlightColor default clHighlightText;  { Published }
    property HighlightBkColor : TColor read FHighlightBkColor write SetHighlightBkColor default clHighlight;  { Published }
    property OnLinkClick : TElHTMLLinkClickEvent read FOnLinkClick write FOnLinkClick;
    property OnImageNeeded : TElHTMLImageNeededEvent read FOnImageNeeded write FOnImageNeeded;
    property UseCustomScrollBars : Boolean read FUseCustomScrollBars write SetUseCustomScrollBars default true;  { Published }
    property FlatFocusedScrollBars : Boolean read FFlatFocusedScrollBars write SetFlatFocusedScrollBars;  { Published }
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
        SetLineBorderInactiveColor;
    property Text : string read FDummyString;
    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
        true;
    property Margin: Integer read FMargin write SetMargin default 4;

{$IFDEF VCL_4_USED}
    property Anchors;
{$ENDIF}
    property Color;
{$IFDEF VCL_4_USED}
    property Constraints;
{$ENDIF}
    property Ctl3D;
    property DragCursor;
{$IFDEF VCL_4_USED}
    property DragKind;
{$ENDIF}
    property Align;
    property DragMode;
    property Enabled;
    property Font;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

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
{$IFDEF VCL_4_USED}
    property OnStartDock;
    property OnEndDock;
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;  { TElHTMLView }

implementation

uses ElTools, ElExtBkgnd, ElHandPt;

procedure TElHTMLView.OnHScroll(Sender: TObject; ScrollCode: TElScrollCode;
                                var ScrollPos: Integer; var DoChange : boolean);
begin
  //if ScrollCode <> escTrack then
  begin
    if (FViewPos.X <> ScrollPos) then
    begin
      FViewPos.X := ScrollPos;
      DoChange := true;
      AdjustScrollBars;
      InvalidateRect(Handle, @FViewRect, true);
      DrawFlatBorder(false, false);
    end;
  end;
end;

procedure TElHTMLView.OnVScroll(Sender: TObject; ScrollCode: TElScrollCode;
                                var ScrollPos: Integer; var DoChange : boolean);
{
var xoffs,
    yoffs : integer;
    R     : TRect;
}
begin
  //if ScrollCode <> escTrack then
  begin
    if (FViewPos.Y <> ScrollPos) then
    begin
      //yoffs := FViewPos.Y - ScrollPos;
      //ScrollWindowEx(Handle, 0, yoffs, @FViewRect, @FViewRect, 0, @R, SW_INVALIDATE);
      FViewPos.Y := ScrollPos;
      DoChange := true;
      AdjustScrollBars;
      InvalidateRect(Handle, @FViewRect, true);
      DrawFlatBorder(false, false);
    end;
  end;
end;

procedure TElHTMLView.SBChanged(Sender: TObject);
begin
  AdjustScrollBars;
  Invalidate;
  UpdateFrame;
end;

procedure TElHTMLView.SetVertScrollBarStyles(newValue : TElScrollBarStyles);
begin
  FVertScrollBarStyles.Assign(newValue);
end;  { SetVertScrollBarStyles }

procedure TElHTMLView.SetHorzScrollBarStyles(newValue : TElScrollBarStyles);
begin
  FHorzScrollBarStyles.Assign(newValue);
end;  { SetHorzScrollBarStyles }

procedure TElHTMLView.SetViewPos(newValue : TPoint);
var R : TRect;
begin
  if (newValue.x <> FViewPos.x) or (newValue.y <> FViewPos.y) then
  begin
    FViewPos := newValue;
    AdjustScrollBars;
    R := FViewRect;
    if HandleAllocated then
       InvalidateRect(Handle, @R, false);
    //UpdateFrame;
  end;
end;

procedure TElHTMLView.PrepareText;
var ww : boolean;
begin
  FViewPos := Point(0, 0);
  FRender.Data.DefaultStyle := Font.Style;
  FRender.Data.DefaultFont  := Font.Name;
  FRender.Data.DefaultColor := Font.Color;
  FRender.Data.DefaultHeight:= Font.Height;
  FRender.Data.Charset      := Font.Charset;
  FRender.Data.LinkColor    := LinkColor;
  FRender.Data.LinkStyle    := LinkStyle;
  FRender.Data.HighlightColor := FHighlightColor;
  FRender.Data.HighlightBgColor := FHighlightBkColor;
  ww := WordWrap;
  if (csLoading in ComponentState) then
    ww := false;
  if HandleAllocated then
  begin
    FRender.PrepareText(Caption, FTextRect.Right - FTextRect.Left, ww);
    Invalidate;
  end;
end;

procedure TElHTMLView.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TElHTMLView.UpdateFrame;
var R : TRect;
begin
  if HandleAllocated then
  begin
    R := Rect(0, 0, Width, Height);
    if (BorderStyle = bsSingle) and (not (csDestroying in ComponentState)) and (HandleAllocated) then
       RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame );
  end;
end;

procedure TElHTMLView.CMFontChanged(var Msg : TMessage);  { private }
begin
  inherited;
  FScrollStep := Abs(Font.Height) div 2;
  PrepareText;
  AdjustScrollBars;
  Invalidate;
  UpdateFrame;
end;  { CMFontChanged }

procedure TElHTMLView.CMSysColorChange(var Msg : TMessage);  { private }
begin
  inherited;
  Invalidate;
  UpdateFrame;
end;  { CMSysColorChange }

procedure TElHTMLView.SetWordWrap(newValue : Boolean);
{ Sets data member FWordWrap to newValue. }
begin
  if (FWordWrap <> newValue) then
  begin
    FWordWrap := newValue;
    AdjustScrollBars;
    PrepareText;
    AdjustScrollBars;
    Invalidate;
    UpdateFrame;
  end;  { if }
end;  { SetWordWrap }

procedure TElHTMLView.AdjustScrollBars;  { protected }
var
    HScrollRect,
    VScrollRect : TRect;

    function UpdateVScroll : boolean;
    var b : boolean;
        i : integer;
        ScrollInfo : TScrollInfo;
    begin
      if FHScrollVisible then
         i := FHorzScrollBarStyles.Width
      else
         i := 0;
      FVertScrollBar.Max := FRender.Data.TextSize.cy;
      FVertScrollBar.Position := FViewPos.y;
      FVertScrollBar.Page := (ClientHeight - i);
      FVertScrollbar.Step := Abs(Font.Height);

      ScrollInfo.cbSize := sizeof(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL;
      ScrollInfo.nMin  := 0;
      ScrollInfo.nMax  := FVertScrollBar.Max;
      ScrollInfo.nPage := FVertScrollBar.Page;
      ScrollInfo.nPos  := FViewPos.y;
      ScrollInfo.nTrackPos := FViewPos.y;

      if FUseCustomScrollBars then
         ScrollInfo.fMask := ScrollInfo.fMask + SIF_DISABLENOSCROLL;

      SetScrollInfo(Handle, SB_VERT, ScrollInfo, true);

      b := FVertScrollBar.Max > FVertScrollBar.Page;
      result := b <> FVScrollVisible;
      FVScrollVisible := b;
    end;

    function UpdateHScroll : boolean;
    var b : boolean;
        i : integer;
        ScrollInfo : TScrollInfo;
    begin
      if FVScrollVisible then
         i := FVertScrollBarStyles.Width
      else
         i := 0;

      FHorzScrollBar.Max := FRender.Data.TextSize.cx;
      if WordWrap then
        FHorzScrollBar.Max := 0;
      FHorzScrollBar.Position := FViewPos.x;
      FHorzScrollBar.Page := (ClientWidth - i) div 2;

      ScrollInfo.cbSize := sizeof(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL;
      ScrollInfo.nMin  := 0;
      ScrollInfo.nMax  := FHorzScrollBar.Max;
      ScrollInfo.nPage := FHorzScrollBar.Page;
      ScrollInfo.nPos  := FViewPos.x;
      ScrollInfo.nTrackPos := FViewPos.x;

      if FUseCustomScrollBars then
         ScrollInfo.fMask := ScrollInfo.fMask + SIF_DISABLENOSCROLL;

      SetScrollInfo(Handle, SB_HORZ, ScrollInfo, true);

      b := FHorzScrollBar.Max > (ClientWidth - i);
      result := b <> FHScrollVisible;
      FHScrollVisible := b;
    end;

var b : boolean;
    FViewRect : TRect;
begin
  if not HandleAllocated then exit;

  FHScrollVisible := (not WordWrap) and (ClientWidth < FRender.Data.TextSize.cx);
  //FVScrollVisible := FRender.Data.TextSize.cy > ClientHeight ;

  b := false;
  while UpdateHScroll or UpdateVSCroll do
  begin
    if WordWrap then
       PrepareText;
    b := true;
  end;

  if b and FUseCustomScrollBars then
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_FRAMECHANGED);

  FViewRect := ClientRect;
  if FHScrollVisible and FUseCustomScrollBars then
  begin
    dec(FViewRect.Bottom, FHorzScrollBarStyles.Width - 1);
    if FVScrollVisible then
      HScrollRect := Rect(0, ClientHeight - FHorzScrollBarStyles.Width, ClientWidth - FVertScrollBarStyles.Width, FHorzScrollBarStyles.Width)
    else
      HScrollRect := Rect(0, ClientHeight - FHorzScrollBarStyles.Width, ClientWidth, FHorzScrollBarStyles.Width);
    with HScrollRect do
      FHorzScrollBar.SetBounds(Left, Top, Right, Bottom);
    FHorzScrollBar.Visible := true;
  end
    else FHorzScrollBar.Visible := false;

  if FVScrollVisible and FUseCustomScrollBars then
  begin
    dec(FViewRect.Right, FVertScrollBarStyles.Width - 1);
    if FHScrollVisible then
      VScrollRect := Rect(ClientWidth - FVertScrollBarStyles.Width, 0, FVertScrollBarStyles.Width, ClientHeight - FHorzScrollBarStyles.Width)
    else
      VScrollRect := Rect(ClientWidth - FVertScrollBarStyles.Width, 0, FVertScrollBarStyles.Width, ClientHeight);
    with VScrollRect do
      FVertScrollBar.SetBounds(Left,Top, Right, Bottom);
    FVertScrollBar.Visible := true;
  end
    else FVertScrollBar.Visible := false;
  ViewRect := FViewRect;
end;  { AdjustScrollBars }

procedure TElHTMLView.Paint;  { protected }
var R, Rect,
    R1     : TRect;
    ACtl   : TWinControl;
    //ax, ay : integer;
    P      : TPoint;
    BgRect : TRect;

begin
  if Flat then DrawFlatBorder(false, false);

  R := ClientRect;

  if not Transparent then
  begin
    if (FImgForm <> nil) and (not (csDesigning in FImgForm.ComponentState)) then
    begin
      if (FImgForm.Control <> Self) then
      begin
        ACtl := FImgForm.GetRealControl;
        BgRect := R;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        P := Parent.ClientToScreen(Point(Left, Top));
        {ax := BgRect.Left - P.x;
        ay := BgRect.Top - P.y;
        }
        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);

        FImgForm.PaintBkgnd(Canvas.Handle, R, BgRect.TopLeft, false);
      end;
      //FImgForm.PaintBkgnd(Canvas.Handle, R, Point(BgRect.Left - ax, BgRect.Top - ay), false);
    end
    else
    begin
      with Canvas do
      case BackgroundType of //
        bgtColorFill :
          begin
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := Color;
            Canvas.FillRect(R);
          end;
        bgtHorzGradient,
        bgtVertGradient:
          GradientFill(Canvas.Handle, R, GradientStartColor, GradientEndColor, GradientSteps, BackgroundType = bgtVertGradient);
        bgtStretchBitmap,
        bgtTileBitmap:
          begin
            CopyRect(Rect, FTmpBmp.Canvas, Classes.Rect(0, 0, FTmpBmp.Width, FTmpBmp.Height));
          end;
        bgtCenterBitmap :
          begin
            Brush.Color := Color;
            Rect := R;
            FillRect(Rect);
            R := Classes.Rect(0, 0, FBackground.Width, FBackground.Height);
            CenterRects(FBackground.Width, Rect.Right - Rect.Left, FBackground.Height, Rect.Bottom - Rect.Top, R1);
            OffsetRect(R1, Rect.Left, Rect.Top);
            CopyRect(R1, FBackground.Canvas, Classes.Rect(0, 0, FBackground.Width, FBackground.Height));
          end;
      end; // case

      if (FVScrollVisible and FHScrollVisible) and (FUseCustomScrollBars) then
      begin
        R := Classes.Rect(FVertScrollbar.Left, FHorzScrollBar.Top, FVertScrollbar.Left + FVertScrollbar.Width, FHorzScrollBar.Top + FHorzScrollBar.Height);
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := FVertScrollBar.Color;
        Canvas.FillRect(R);
      end;
    end;
  end else
  begin
    GetClipBox(Canvas.Handle, R);
    R1 := R;
    P := Parent.ScreenToClient(ClientToScreen(Point(0, 0)));
    with P do
      OffsetRect(R1, X, Y);
    RedrawWindow(Parent.Handle, @R1, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN or RDW_UPDATENOW);

    with R do
        BitBlt(Canvas.Handle, Left, Top, Right, Bottom, TmpDC, R.Left, R.Top, SRCCOPY);
  end;
  FRender.DrawText(Canvas, FViewPos, FTextRect, clNone);
end;  { Paint }

procedure TElHTMLView.CMMouseEnter(var Msg : TMessage);  { private }
begin
  inherited;
  FMouseOver := true;
  if (Flat and (not Focused)) or UseCustomScrollBars then DrawFlatBorder(false, false);
end;  { CMMouseEnter }

procedure TElHTMLView.CMMouseLeave(var Msg : TMessage);  { private }
begin
  FMouseOver := false;
  if (Flat and (not Focused)) or UseCustomScrollBars then DrawFlatBorder(false, false);
  inherited Cursor := FCursor;
  inherited;
end;  { CMMouseLeave }

procedure TElHTMLView.SetActiveBorderType(newValue : TElFlatBorderType);
begin
  if (FActiveBorderType <> newValue) then
  begin
    FActiveBorderType := newValue;
    if (Focused or FMouseOver) then UpdateFrame;
  end;  { if }
end;  { SetActiveBorderType }

procedure TElHTMLView.SetInactiveBorderType(newValue : TElFlatBorderType);
{ Sets data member FInactiveBorderType to newValue. }
begin
  if (FInactiveBorderType <> newValue) then
  begin
    FInactiveBorderType := newValue;
    if (not (Focused or FMouseOver)) then UpdateFrame;
  end;  { if }
end;  { SetInactiveBorderType }

procedure TElHTMLView.SetFlat(newValue : Boolean);
{ Sets data member FFlat to newValue. }
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    UpdateFrame;
  end;  { if }
end;  { SetFlat }

procedure TElHTMLView.SetImageForm(newValue : TElImageForm);
begin
  if (FImgForm <> newValue) then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnregisterChanges(FImgFormChLink);
    end;
    FImgForm := newValue;
    if (newValue <> nil) then
    begin
      newValue.FreeNotification(Self);
      FImgForm.RegisterChanges(FImgFormChLink);
    end;
    if not (csDesigning in ComponentState) then
    begin
      Invalidate;
      UpdateFrame;
    end;
  end;  { if }
end;  { SetImageForm }

procedure TElHTMLView.SetGradientStartColor(newValue : TColor);
{ Sets data member FGradientStartColor to newValue. }
begin
  if (FGradientStartColor <> newValue) then
  begin
    FGradientStartColor := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then
    begin
      Invalidate;
      UpdateFrame;
    end;
  end;  { if }
end;  { SetGradientStartColor }

procedure TElHTMLView.SetGradientEndColor(newValue : TColor);
{ Sets data member FGradientEndColor to newValue. }
begin
  if (FGradientEndColor <> newValue) then
  begin
    FGradientEndColor := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then
    begin
      Invalidate;
      UpdateFrame;
    end;
  end;  { if }
end;  { SetGradientEndColor }

procedure TElHTMLView.SetGradientSteps(newValue : Integer);
{ Sets data member FGradientSteps to newValue. }
begin
  if (FGradientSteps <> newValue) and (newValue > 0) then
  begin
    FGradientSteps := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then
    begin
      Invalidate;
      UpdateFrame;
    end;
  end;  { if }
end;  { SetGradientSteps }

procedure TElHTMLView.SetBackground(newValue : TBitmap);
begin
  FBackground.Assign(newValue);
  if (FBackground.Empty) and (FBackGroundType in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap])
     then BackgroundType := bgtColorFill;
end; {SetBackground}

procedure TElHTMLView.SetBackgroundType(newValue : TElBkGndType);
begin
  if (FBackgroundType <> newValue) then
  begin
    FBackgroundType := newValue;
    if (FBackground.Empty) and (FBackGroundType in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap])
       then FBackgroundType := bgtColorFill;
    RedoTmpBmp;
    begin
      Invalidate;
      UpdateFrame;
    end;
  end; {if}
end; {SetBackgroundType}

procedure TElHTMLView.ImageFormChange(Sender : TObject);
begin
  Invalidate;
  UpdateFrame;
end;

procedure TElHTMLView.ImageChange(Sender : TObject);
begin
  if ((FBackground.Height = 0) or (FBackground.Width = 0)) then
    BackgroundType := bgtColorFill
  else
  begin
    if not (BackgroundType in [bgtColorFill, bgtCenterBitmap]) then RedoTmpBmp;
    UpdateFrame;
  end;
end;

procedure TElHTMLView.CreateParams;
const
{$IFNDEF VCL_4_USED}
  BorderStyles: array[TBorderStyle] of Longint = (0, WS_BORDER);
{$ELSE}
  BorderStyles: array[TBorderStyle] of DWord = (0, WS_BORDER);
{$ENDIF}
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] {or (WS_HSCROLL or WS_VSCROLL)};
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    if Transparent then
       ExStyle := ExStyle or WS_EX_TRANSPARENT
    else
       ExStyle := ExStyle and not WS_EX_TRANSPARENT;
    with Params.WindowClass do
      style := style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TElHTMLView.SetTransparent(newValue : Boolean);
{ Sets data member FTransparent to newValue. }
begin
  if (FTransparent <> newValue) then
  begin
    FTransparent := newValue;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    RecreateWnd;
    UpdateFrame;
  end; { if }
end; { SetTransparent }

procedure TElHTMLView.DrawFlatBorder(HorzTracking, VertTracking : boolean);
var
  DC : HDC;
  RC,
  RW : TRect;
  SavedDC : HDC;
  b  : boolean;
  BS : TElFlatBorderType;
  Theme : HTheme;
  AColor: TColor;
   
const ScrollBars: array [boolean, boolean] of TScrollStyle = ((ssNone, ssVertical), (ssHorizontal, ssBoth));

begin
  Windows.GetClientRect(Handle, RC);
  if not UseCustomScrollBars then
  begin
    if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
      inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
    if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
      inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
  end;
  GetWindowRect(Handle, RW);
  MapWindowPoints(0, Handle, RW, 2);
  OffsetRect(RC, -RW.Left, -RW.Top);
  OffsetRect(RW, -RW.Left, -RW.Top);

  DC := GetWindowDC(Handle);
  try
    if (BorderStyle = bsSingle) and IsThemeApplied then
    begin
      Theme := OpenThemeData(0, 'EDIT');
      if Theme <> 0 then
      begin
        SavedDC := SaveDC(DC);
        ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
        DrawThemeBackground(Theme, DC, 0, 0, RW, nil);

        CloseThemeData(Theme);

        RestoreDC(DC, SavedDC);
      end;
    end;
    begin
      if (BorderStyle = bsSingle) and not IsThemeApplied then
      begin
        b := Focused or FMouseOver;
        if b then BS := FActiveBorderType else BS := FInactiveBorderType;
        if ((not FFlat) and FUseCustomScrollBars) then
           BS := fbtSunken;
        if bs = fbtRaised then bs := fbtRaisedOuter;
        if Focused or FMouseOver then
          AColor := LineBorderActiveColor
        else
          AColor := LineBorderInactiveColor;
        DrawFlatFrameEx2(DC, RW, AColor, Color, b, Enabled, BorderSides, BS);
      end;
      if (not FUseCustomScrollBars) and (not IsThemeApplied) then
         DrawFlatScrollBars(Handle, DC, RW, (Focused or FMouseOver) and (not FlatFocusedScrollBars),
                            ScrollBars[FHScrollVisible, FVScrollVisible], HorzTracking, VertTracking, false,
                            GetWindowLong(Handle, GWL_STYLE), GetWindowLong(Handle, GWL_EXSTYLE));
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TElHTMLView.DrawFlatBorderEx(DC : HDC; HorzTracking, VertTracking : boolean);
var
  RC,
  RW : TRect;
  SavedDC : HDC;
  b  : boolean;
  BS : TElFlatBorderType;
  Theme : HTheme;
  AColor: TColor;

const ScrollBars: array [boolean, boolean] of TScrollStyle = ((ssNone, ssVertical), (ssHorizontal, ssBoth));
begin
  Windows.GetClientRect(Handle, RC);
  if not UseCustomScrollBars then
  begin
    if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
      inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
    if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
      inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
  end;
  GetWindowRect(Handle, RW);
  MapWindowPoints(0, Handle, RW, 2);
  OffsetRect(RC, -RW.Left, -RW.Top);
  OffsetRect(RW, -RW.Left, -RW.Top);

  if (BorderStyle = bsSingle) and IsThemeApplied then
  begin
    Theme := OpenThemeData(0, 'EDIT');
    if Theme <> 0 then
    begin
      SavedDC := SaveDC(DC);
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
      DrawThemeBackground(Theme, DC, 0, 0, RW, nil);

      CloseThemeData(Theme);

      RestoreDC(DC, SavedDC);
    end;
  end;
  if (BorderStyle = bsSingle) and not IsThemeApplied then
  begin
    b := Focused or FMouseOver;
    if b then BS := FActiveBorderType else BS := FInactiveBorderType;
    if ((not FFlat) and FUseCustomScrollBars) then
       BS := fbtSunken;
    if bs = fbtRaised then bs := fbtRaisedOuter;
    if Focused or FMouseOver then
      AColor := LineBorderActiveColor
    else
      AColor := LineBorderInactiveColor;

    DrawFlatFrameEx2(DC, RW, AColor, Color, b, Enabled, FBorderSides, BS);
  end;

  if (not FUseCustomScrollBars) and (not IsThemeApplied) then
     DrawFlatScrollBars(Handle, DC, RW, (Focused or FMouseOver) and (not FlatFocusedScrollBars),
     ScrollBars[FHScrollVisible, FVScrollVisible], HorzTracking, VertTracking, false,
     GetWindowLong(Handle, GWL_STYLE), GetWindowLong(Handle, GWL_EXSTYLE));
end;

procedure TElHTMLView.RedoTmpBmp;
var BgRect,
    BgRect2 : TRect;
begin
  if BackgroundType in [bgtHorzGradient, bgtVertGradient, bgtColorFill, bgtCenterBitmap] then
  begin
    FTmpBmp.FreeImage;
  end else
  begin
    if HandleAllocated and (ClientWidth <> 0) and (ClientHeight <> 0) then
    begin
      FTmpBmp.Height := ClientHeight - 1;
      FTmpBmp.Width := ClientWidth - 1;
      BgRect := ClientRect;
      BgRect2 := BgRect;
      OffsetRect(BgRect2, BgRect2.Left, BgRect2.Top);
      ExtDrawBkgnd(FTmpBmp.Canvas.Handle, Handle, BgRect, BgRect, BgRect, BgRect2, false, Color, Color, false, Background, BackgroundType);
    end;
  end;
end;

procedure TElHTMLView.Notification(AComponent : TComponent; operation : TOperation);
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) then
  begin
    if (AComponent = FImgForm) then
      ImageForm := nil;
    if (AComponent = FLinkPopupMenu) then
      LinkPopupMenu := nil;
  end;  { if }
end;  { Notification }

procedure TElHTMLView.WMSetFocus(var Msg : TWMSetFocus);  { private }
begin
  inherited;
  if Flat or FUseCustomScrollBars then UpdateFrame;
end;  { WMSetFocus }

procedure TElHTMLView.WMKillFocus(var Msg : TWMKillFocus);  { private }
begin
  inherited;
  if not HandleAllocated then exit;
  if Flat or FUseCustomScrollBars then UpdateFrame;
end;  { WMKillFocus }

procedure TElHTMLView.CMTextChanged(var Msg : TMessage);  { private }
var R : TRect;
begin
  inherited;
  FViewPos := Point(0, 0);
  if WordWrap then
  begin
    PrepareText;
    AdjustScrollBars;
  end;
  PrepareText;
  AdjustScrollBars;
  R := FViewRect;
  if HandleAllocated then
     InvalidateRect(Handle, @R, false);
  //UpdateFrame;
end;  { CMTextChanged }

procedure TElHTMLView.CreateWindowHandle(const Params: TCreateParams);  { protected }
begin
  inherited CreateWindowHandle(Params);
  PrepareText;
  AdjustScrollBars;
  if WordWrap then
    PrepareText;
end;  { CreateWindowHandle }

procedure TElHTMLView.SetLinkColor(newValue : TColor);
{ Sets data member FLinkColor to newValue. }
begin
  if (FLinkColor <> newValue) then
  begin
    FLinkColor := newValue;
    PrepareText;
  end;  { if }
end;  { SetLinkColor }

procedure TElHTMLView.SetLinkStyle(newValue : TFontStyles);
{ Sets data member FLinkStyle to newValue. }
begin
  if (FLinkStyle <> newValue) then
  begin
    FLinkStyle := newValue;
    PrepareText;
  end;  { if }
end;  { SetLinkStyle }

procedure TElHTMLView.SetHighlightColor(newValue : TColor);
{ Sets data member FHighlightColor to newValue. }
begin
  if (FHighlightColor <> newValue) then
  begin
    FHighlightColor := newValue;
    Invalidate;
    UpdateFrame;
  end;  { if }
end;  { SetHighlightColor }

procedure TElHTMLView.SetHighlightBkColor(newValue : TColor);
{ Sets data member FHighlightBkColor to newValue. }
begin
  if (FHighlightBkColor <> newValue) then
  begin
    FHighlightBkColor := newValue;
    Invalidate;
    UpdateFrame;
  end;  { if }
end;  { SetHighlightBkColor }

procedure TElHTMLView.TriggerLinkClickEvent(HRef : string);
begin
  if (assigned(FOnLinkClick)) then
    FOnLinkClick(Self, HRef );
end;  { TriggerLinkClickEvent }

procedure TElHTMLView.TriggerImageNeededEvent(Sender : TObject; Src : 
    TElFString; var Image : TBitmap);
begin
  if (assigned(FOnImageNeeded)) then
    FOnImageNeeded(Self, Src , Image );
end;  { TriggerImageNeededEvent }

procedure TElHTMLView.Click;  { public }
var P : TPoint;
    href : TElFString;
    P1: TPoint;
begin
  P1 := FViewPos;
  dec(P1.x, FTextRect.Left);
  dec(P1.y, FTextRect.Top);

  GetCursorPos(P);
  P := ScreenToClient(P);
  if FRender.IsCursorOverLink(P, P1, FTextRect, href) then
    TriggerLinkClickEvent(href)
  else
    inherited;
end;  { Click }

procedure TElHTMLView.MouseMove(Shift: TShiftState; X, Y: Integer);  { protected }
var href : TElFString;
    P1   : TPoint;
begin
  P1 := FViewPos;
  dec(P1.x, FTextRect.Left);
  dec(P1.y, FTextRect.Top);

  if FRender.IsCursorOverLink(Point(X, Y), P1, FTextRect, href) then
     inherited Cursor := crURLCursor
  else
     inherited Cursor := FCursor;
  inherited MouseMove(Shift, X, Y);
end;  { MouseMove }

procedure TElHTMLView.SetCursor(newValue : TCursor);
var P : TPoint;
    href : TElFString;
    P1    : TPoint;
begin
  if (FCursor <> newValue) then
  begin
    P1 := FViewPos;
    dec(P1.x, FTextRect.Left);
    dec(P1.y, FTextRect.Top);

    FCursor := newValue;
    GetCursorPos(P);
    P := ScreenToClient(P);
    if FRender.IsCursorOverLink(P, P1, FTextRect, href) then
       inherited Cursor := crURLCursor
    else
       inherited Cursor := FCursor;
  end;  { if }
end;  { SetCursor }

type THackElScrollBar = class(TElScrollBar)
       procedure KeyDown(var Key: Word; Shift: TShiftState); override;
     end;

procedure THackElScrollBar.KeyDown;
begin
  inherited;
end;

procedure TElHTMLView.KeyDown(var Key: Word; Shift: TShiftState);  { protected }
begin
  inherited KeyDown(Key, Shift);
  if (Key <> 0) and (Shift = []) then
  begin
    case Key of
      VK_RETURN:
        begin
          if (FRender.Data.SelectedItem <> nil) and
             (FRender.Data.SelectedItem.IsLink) then
          TriggerLinkClickEvent(FRender.Data.SelectedItem.LinkRef);
        end;
      VK_TAB:
        begin
          FRender.SelectNextLink;
          InvalidateRect(Handle, @FViewRect, false);
        end;
      VK_UP,
      VK_DOWN,
      VK_PRIOR,
      VK_NEXT:
        begin
          if FVertScrollBar.Visible then
             THackElScrollBar(FVertScrollBar).KeyDown(Key, Shift);
        end;
      VK_SPACE:
        begin
          Key := VK_NEXT;
          if FVertScrollBar.Visible then
             THackElScrollBar(FVertScrollBar).KeyDown(Key, Shift);
        end;
      VK_LEFT,
      VK_RIGHT:
        begin
          if FHorzScrollBar.Visible then
             THackElScrollBar(FHorzScrollBar).KeyDown(Key, Shift);
        end;
    end;
  end else
  if (Key <> 0) and (Shift = [ssShift]) then
  begin
    if (Key = VK_TAB) then
    begin
      FRender.SelectPrevLink;
      InvalidateRect(Handle, @FViewRect, false);
    end;
  end else
  if (Key <> 0) and (Shift = [ssCtrl]) then
  begin
    if (Key = VK_LEFT) or (Key = VK_RIGHT) then
    begin
      if (Key = VK_LEFT) then Key := VK_PRIOR else
      if (Key = VK_RIGHT) then Key := VK_NEXT;
      THackElScrollBar(FHorzScrollBar).KeyDown(Key, []);
    end;
  end;
end;  { KeyDown }

procedure TElHTMLView.WMGetDlgCode(var Msg : TWMGetDlgCode);  { private }
begin
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTTAB;
end;  { WMGetDlgCode }

procedure TElHTMLView.WMPaint(var Msg : TWMPaint);  { private }
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  R : TRect;
  ARgn : HRGN;

begin
  if (Msg.DC <> 0) then PaintHandler(Msg)
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      GetClipBox(DC, R);
      if IsRectEmpty(R) then
         R := FViewRect
      else
        InflateRect(R, 1, 1); 
      with R do
        ARgn := CreateRectRgn(Left, Top, right, Bottom);
      SelectClipRgn(MemDC, ARgn);

      TmpDC := DC;

      Msg.DC := MemDC;
      WMPaint(Msg);
      SelectClipRgn(MemDC, 0);
      DeleteObject(ARgn);
      Msg.DC := 0;
      with R do
        BitBlt(DC, Left, Top, Right, Bottom, MemDC, Left, Top, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;  { WMPaint }

procedure TElHTMLView.WMEraseBkgnd(var Msg : TWMEraseBkgnd);  { private }
begin
  Msg.Result := 1;
end;  { WMEraseBkgnd }

procedure TElHTMLView.WMMouseWheel(var Msg: TWMMouseWheel); { private }
var
  Dy : integer;
  sl : integer;
begin
  if IsWinNT or IsWin98 then
     SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @sl, SPIF_SENDCHANGE)
  else
     sl := 3;
  if sl = 0 then sl := 1;
  Dy := Msg.WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  if Dy <> 0 then
  begin
    FVertScrollBar.Position := FViewPos.Y - Dy * FScrollStep;
    FViewPos.Y := FVertScrollBar.Position;
    InvalidateRect(Handle, @FViewRect, false);
  end;
end; { WMMouseWheel }

procedure TElHTMLView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  { protected }
var P : TPoint;
    href : TElFString;
    P1: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if CanFocus then SetFocus;
  P := Point(X, Y);
  P1 := FViewPos;
  dec(P1.x, FTextRect.Left);
  dec(P1.y, FTextRect.Top);

  if FRender.IsCursorOverLink(P, P1, FTextRect, href) then
     FRender.SelectLinkAt(P, FViewPos, FTextRect)
  else
     FRender.SelectLinkAt(Point(-1, -1), Point(0, 0), FTextRect);
  InvalidateRect(Handle, @FViewRect, false);
end;

procedure TElHTMLView.SetLinkPopupMenu(newValue : TPopupMenu);
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

procedure TElHTMLView.SetUseCustomScrollBars(newValue : Boolean);
{ Sets data member FUseCustomScrollBars to newValue. }
begin
  if (FUseCustomScrollBars <> newValue) then
  begin
    FUseCustomScrollBars := newValue;
    RecreateWnd;
    AdjustScrollBars;
  end;  { if }
end;  { SetUseCustomScrollBars }

{$ifndef CLX_USED}
procedure TElHTMLView.CMEnabledChanged(var Message: TMessage);
{$else}
procedure TElHTMLView.EnabledChanged;
{$endif}
begin
  inherited;
  FVertScrollBar.Enabled := Enabled;
  FHorzScrollBar.Enabled := Enabled;
end;

procedure TElHTMLView.WMNCPaint(var Msg : TMessage);  { private }
var DC : HDC;
begin
  if not FUseCustomScrollBars then
    inherited;
  if (Flat or FUseCustomScrollBars or IsThemeApplied) and (BorderStyle = bsSingle) then
  begin
    DC := GetWindowDC(Handle);
    (*
    if IsThemeApplied then
    begin
      R1 := BoundsRect;
      OffsetRect(R1, -R1.Left, - R1.Top);

      R.TopLeft := Parent.ClientToScreen(BoundsRect.TopLeft);
      R.BottomRight := Parent.ClientToScreen(BoundsRect.BottomRight);
      R2 := ClientRect;
      OffsetRect(R2, ClientOrigin.X - R.Left, ClientOrigin.Y - R.Top);

      with R2 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      DrawThemeBackgroundTo('EDIT', DC, 0, 0, R1, nil)
    end
    else
    *)
    DrawFlatBorderEx(DC, false, false);

    ReleaseDC(Handle, DC);
    Msg.Result := 0;
  end;
end;  { WMNCPaint }

procedure TElHTMLView.WMNCCalcSize(var Message : TWMNcCalcSize);  { private }
begin
  if not FUseCustomScrollBars then
    inherited
  else
  begin
    if BorderStyle = bsSingle then
    begin
      inc(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(smYEdge[Ctl3D]));
      inc(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(smXEdge[Ctl3D]));
      dec(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(smYEdge[Ctl3D]));
      dec(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(smXEdge[Ctl3D]));
    end;
  end;

  if BorderStyle = bsSingle then
  begin
    if not (ebsLeft in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(smYEdge[Ctl3D]));
    if not (ebsTop in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(smXEdge[Ctl3D]));
    if not (ebsRight in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(smYEdge[Ctl3D]));
    if not (ebsBottom in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(smXEdge[Ctl3D]));
  end;
  // Message.Result := WVR_REDRAW;
end;  { WMNCCalcSize }

procedure TElHTMLView.WMNCHitTest(var Msg : TMessage);  { private }
begin
  inherited;
  if FUseCustomScrollBars then
  begin
    if (Msg.Result = HTHSCROLL) or (Msg.Result = HTVSCROLL) then
        Msg.Result := HTBORDER;
  end;
  if Msg.result = HTBORDER then Msg.result := HTSIZE;
end;  { WMNCHitTest }
{$WARNINGS OFF}
procedure TElHTMLView.WMVScroll(var Msg : TWMVScroll);  { private }
var b : boolean;
    sc: TElscrollCode;
    sp: integer;
begin
  b := false;
  case Msg.ScrollCode of
    SB_TOP:
      begin
        sc := escTop;
        sp := 0;
      end;
    SB_BOTTOM:
      begin
        sc := escTop;
        sp := FRender.Data.TextSize.cy;
      end;
    SB_ENDSCROLL:
      begin
        sc := escEndScroll;
        sp := FViewPos.y;
      end;
    SB_LINEDOWN:
      begin
        sc := escLineDown;
        sp := FViewPos.y + Abs(Font.Height);
      end;
    SB_LINEUP:
      begin
        sc := escLineUp;
        sp := FViewPos.y - Abs(Font.Height);
      end;
    SB_PAGEDOWN:
      begin
        sc := escPageDown;
        sp := FViewPos.y + FVertScrollBar.Page;
      end;
    SB_PAGEUP:
      begin
        sc := escPageUp;
        sp := FViewPos.y - FVertScrollBar.Page;
      end;
    SB_THUMBPOSITION:
      begin
        sc := escPosition;
        sp := Msg.Pos;
      end;
    SB_THUMBTRACK:
      begin
        sc := escTrack;
        sp := Msg.Pos;
      end;
  end;
  if (sp >= 0) and (sp < FVertScrollBar.Max) then
     OnVScroll(FVertScrollBar, SC, sp, b);
end;  { WMVScroll }

procedure TElHTMLView.WMHScroll(var Msg : TWMHScroll);  { private }
var b : boolean;
    sc: TElscrollCode;
    sp: integer;
begin
  b := false;
  case Msg.ScrollCode of
    SB_TOP:
      begin
        sc := escTop;
        sp := 0;
      end;
    SB_BOTTOM:
      begin
        sc := escTop;
        sp := FRender.Data.TextSize.cx;
      end;
    SB_ENDSCROLL:
      begin
        sc := escEndScroll;
        sp := FViewPos.x;
      end;
    SB_LINEDOWN:
      begin
        sc := escLineDown;
        sp := FViewPos.x + 1;
      end;
    SB_LINEUP:
      begin
        sc := escLineUp;
        sp := FViewPos.x - 1;
      end;
    SB_PAGEDOWN:
      begin
        sc := escPageDown;
        sp := FViewPos.x + FHorzScrollBar.Page;
      end;
    SB_PAGEUP:
      begin
        sc := escPageUp;
        sp := FViewPos.x - FHorzScrollBar.Page;
      end;
    SB_THUMBPOSITION:
      begin
        sc := escPosition;
        sp := Msg.Pos;
      end;
    SB_THUMBTRACK:
      begin
        sc := escTrack;
        sp := Msg.Pos;
      end;
  end;
  if (sp >= 0) and (sp < FHorzScrollBar.Max) then
     OnHScroll(FHorzScrollBar, SC, sp, b);
end;  { WMHScroll }
{$WARNINGS ON}

{$IFDEF VCL_4_USED}
procedure TElHTMLView.CMMouseWheel(var Msg : TCMMouseWheel);  { private }
var
  Dy : integer;
  sl : integer;
begin
  if IsWinNT or IsWin98 then
     SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @sl, SPIF_SENDCHANGE)
  else
     sl := 3;
  if sl = 0 then sl := 1;
  Dy := Msg.WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  if Dy <> 0 then
  begin
    FVertScrollBar.Position := FViewPos.Y - Dy * FScrollStep;
    FViewPos.Y := FVertScrollBar.Position;
    InvalidateRect(Handle, @FViewRect, false);
  end;
end;  { CMMouseWheel }
{$ENDIF}

procedure TElHTMLView.SetFlatFocusedScrollBars(newValue : Boolean);
{ Sets data member FFlatFocusedScrollBars to newValue. }
begin
  if (FFlatFocusedScrollBars <> newValue) then
  begin
    FFlatFocusedScrollBars := newValue;
    if Focused and (not FUseCustomScrollBars) then DrawFlatBorder(false, false);
  end;  { if }
end;  { SetFlatFocusedScrollBars }

procedure TElHTMLView.Loaded;  { protected }
begin
  inherited;
  FVertScrollBar.Loaded;
  FHorzScrollBar.Loaded;
  AdjustScrollBars;
  if HandleAllocated then
  begin
    FRender.PrepareText(Caption, FTextRect.Right - FTextRect.Left, WordWrap);
    Invalidate;
  end;
end;  { Loaded }

procedure TElHTMLView.WMSize(var Msg : TWMSize);  { private }
begin
  inherited;
  AdjustScrollBars;
  if WordWrap then
     PrepareText;
  AdjustScrollBars;
  Invalidate;
end;  { WMSize }

destructor TElHTMLView.Destroy;
begin
  Destroying;
  ImageForm := nil;
  FRender.Free;
  FVertScrollBarStyles.Free;
  FHorzScrollBarStyles.Free;
  FVertScrollBar.Free;
  FHorzScrollBar.Free;
  FBackground.Free;
  FTmpBmp.Free;
  FImgFormChLink.Free;
  inherited Destroy;
end;  { Destroy }

constructor TElHTMLView.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  FUseCustomScrollBars := true;
  FUseXPThemes := true;
  FRender := TElHTMLRender.Create;
  FImgFormChLink  := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  FBackground := TBitmap.Create;
  FBackground.OnChange := ImageChange;
  FBackgroundType := bgtColorFill;
  FGradientSteps := 16;
  FMargin := 4;
  FTmpBmp := TBitmap.Create;
  FRender.OnImageNeeded := TriggerImageNeededEvent;
  FLinkColor := clBlue;
  FLinkStyle := [fsUnderline];
  FBorderStyle := bsSingle;
  Parentcolor := false;
  Color := clWindow;
  FActiveBorderType := fbtSunken;
  FInactiveBorderType := fbtSunkenOuter;
  FHighlightColor := clHighlightText;
  FHighlightBkColor := clHighlight;
  FScrollStep := Abs(Font.Height) div 2;
  FHorzScrollBar := THackElScrollBar.Create(nil);
  FHorzScrollBar.Parent := Self;
  FHorzScrollBar.OnScroll := OnHScroll;
  FHorzScrollBar.Ctl3D := false;
  FHorzScrollBarStyles := TElScrollBarStyles.Create(FHorzScrollBar, Self);
  FHorzScrollBar.TabStop := false;
  FHorzScrollBar.Height := GetSystemMetrics(SM_CYVTHUMB);
  FHorzScrollBarStyles.OnChange := SBChanged;

  FVertScrollBar := THackElScrollBar.Create(nil);
  FVertScrollBar.Parent := Self;
  FVertScrollBar.OnScroll := OnVScroll;
  FVertScrollBar.Kind := sbVertical;
  FVertScrollBarStyles := TElScrollBarStyles.Create(FVertScrollBar, Self);
  FVertScrollBarStyles.ThumbMode := etmAuto;
  FVertScrollBarStyles.ThumbMode := etmAuto;
  FVertScrollBarStyles.OnChange := SBChanged;
  FVertScrollBar.TabStop := false;
  FVertScrollBar.Ctl3D := false;
  FVertScrollBar.Width := GetSystemMetrics(SM_CXHTHUMB);

  Width := 136;
  Height := 81;
end;  { Create }

procedure TElHTMLView.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TElHTMLView.SetCaption(newValue: TElFString);
{ Sets data member FCaption to newValue. }
begin
  FCaption := newValue;
  inherited Caption := newValue;
end; { SetCaption }

procedure TElHTMLView.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;

procedure TElHTMLView.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;

procedure TElHTMLView.SetUseXPThemes(const Value: Boolean);
begin
  inherited;
  FHorzScrollBar.UseXPThemes := Value;
  FVertScrollBar.UseXPThemes := Value;
end;

function TElHTMLView.GetThemedClassName: WideString;
begin
  Result := 'WINDOW';
end;

procedure TElHTMLView.WMRButtonUp(var Message: TWMRButtonUp);
{$ifndef VCL_5_USED}
var P,P1 : TPoint;
    href : TElFString;

{$endif}
begin
  {$ifndef VCL_5_USED}
  P := SmallPointToPoint(Message.Pos);
  P1 := FViewPos;
  dec(P1.x, FTextRect.Left);
  dec(P1.y, FTextRect.Top);

  if FRender.IsCursorOverLink(P, P1, FTextRect, href) then
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

procedure TElHTMLView.DoLinkPopup(MousePos : TPoint);
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

{$ifdef VCL_5_USED}
procedure TElHTMLView.WMContextMenu(var Message: TWMContextMenu);
var
  Pt,
  P1, Temp: TPoint;
  Handled : Boolean;
  href    : TElFString;
begin
  if Message.Result <> 0 then Exit;
  if csDesigning in ComponentState then Exit;

  Pt := ScreenToClient(SmallPointToPoint(Message.Pos));
  P1 := FViewPos;
  dec(P1.x, FTextRect.Left);
  dec(P1.y, FTextRect.Top);

  if FRender.IsCursorOverLink(Pt, P1, FTextRect, href) then
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


procedure TElHTMLView.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then
      if HandleAllocated then
        Invalidate;
  end;
end;

procedure TElHTMLView.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then
      if HandleAllocated then
        Invalidate;
  end;
end;

{$ifndef CLX_USED}
procedure TElHTMLView.WMSysColorChange(var Msg: TMessage);
begin
  inherited;
  PostMessage(FVertScrollBar.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
  PostMessage(FHorzScrollBar.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end; { WMSysColorChange }

{$endif}

procedure TElHTMLView.SetViewRect(Value: TRect);
begin
  FViewRect := Value;
  FTextRect := FViewRect;
  InflateRect(FTextRect, -FMargin, 0);
  if HandleAllocated then
  begin
    FRender.PrepareText(Caption, FTextRect.Right - FTextRect.Left, HandleAllocated);
    Invalidate;
  end;
end;

procedure TElHTMLView.SetMargin(Value: Integer);
var R : TRect;
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    R := FViewRect;
    FViewRect := Rect(0, 0, 0, 0);
    ViewRect := R;
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElHTMLView.CMHintShow(var Message: TMessage);
{$else}
function TElHTMLView.HintShow(var HintInfo : THintInfo): Boolean; 
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

procedure TElHTMLView.SetHint(Value: WideString);
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

