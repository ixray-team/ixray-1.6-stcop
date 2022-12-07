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

05/12/2002

  Focus rectangle was not shown when the focus was set to control. Fixed. 

03/06/2002

  Added unicode hint

02/26/2002

  Fixed trackbar auto-scrolling behaviour when the thumb goes under the cursor
while scrolling

01/25/2002

  Fixed tracking that was spoiled on 12/21/2001 

12/21/2001

  Improved handling of track operations

12/12/2001

  Added Focus rectangle when the trackbar is focused

12/11/2001

  Fixed OnChanging event
  Fixed keyboard operations
  Added ActiveBorderType and InactiveBorderType properties

11/29/2001

  Added Flat property

11/22/2001

  Fixed repainting when Enabled state changes

11/21/2001

  Fixed some artefacts with XP styles enabled

11/17/2001

  Removed flicker with XP styles enabled

11/09/2001

  Transparency fixed.

10/26/2001

  Fixed painting of background with XP styles enabled

10/19/2001

  When Min value is > 0, trackbar worked incorrectly. Fixed.

10/12/2001

  UseXPThemes made published

09/19/2001

  Added Windows XP Themes Support

08/30/2001

  OnTrack event added

*)

unit ElTrackBar;

interface

uses
  SysUtils,
{$ifdef MSWINDOWS}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
{$else}
  Qt,
  QTypes,
  Types,
  QGraphics,
  QControls,
  QForms,
  QMenus,
  QExtCtrls,
  QStdCtrls,
{$endif}
  Classes,
  ElTools,
  ElVCLUtils,
{$IFDEF ELPACK_COMPLETE}
  ElImgFrm,
{$ENDIF}
{$IFDEF HAS_HTML_RENDER}
  HTMLRender,
{$ENDIF}
  ElHintWnd,
  ElTmSchema,
  ElUxTheme,
  ElXPThemedControl
  ;

type

    TElTrackBarOrientation = (toHorizontal, toVertical);

    TElTrackTickPosition = (tpNone, tpAbove, tpBelow, tpBoth);

    TElTrackBarPart = (tbpNowhere, tbSelMarkStart, tbSelMarkEnd, tbpTicksAbove, tbpTicksBelow, tbpTrack, tbpThumb);

    TElTrackThumbType = (tstPointerTop, tstPointerBottom, tstBox);

    TTickMarkMode = (tmmAuto, tmmManual);

    TElTrackHitTestEvent     = procedure(Sender : TObject; X, Y : integer; var Part : TElTrackBarPart; var DefaultTest : boolean) of object;
    TElTrackDrawPartEvent    = procedure(Sender : TObject; Canvas : TCanvas; R : TRect; Part : TElTrackBarPart; Enabled, Focused, Pressed : boolean; var DefaultDraw : boolean) of object;
    TElTrackChangeEvent      = procedure(Sender : TObject; NewPos : integer) of object;
    TElTrackChangingEvent    = procedure(Sender : TObject; var NewPos : integer; var AllowChange : boolean) of object;
    TElTickMarkPositionEvent = procedure(Sender : TObject; PrevPos : integer; var TickPos : integer) of object;

    TElTrackBar = class(TElXPThemedControl)
    private
      FMouseOverPart: TElTrackBarPart;
      FActiveBorderType: TElFlatBorderType;
      FInactiveBorderType: TElFlatBorderType;
      procedure SetActiveBorderType(const Value: TElFlatBorderType);
      procedure SetInactiveBorderType(const Value: TElFlatBorderType);
      procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    protected
      FTransparent: Boolean;
      FOrientation: TElTrackBarOrientation;
      FMax: Integer;
      FMin: Integer;
      FPage: Integer;
      FShowTrackHint: Boolean;
      FThumbPos : integer;
      FMouseDown : boolean;
      FImgFormChLink: TImgFormChangeLink;
      FImgForm: TElImageForm;
      FTickPosition: TElTrackTickPosition;
      FOffsetLeft: Integer;
      FOffsetRight: Integer;
      FOnDrawPart: TElTrackDrawPartEvent;
      FOwnerDraw: Boolean;
      FOnChange: TElTrackChangeEvent;
      FOnChanging: TElTrackChangingEvent;
      FThumbType: TElTrackThumbType;
      FTickWidth: Integer;
      FSelStart: Integer;
      FSelEnd: Integer;
      FOnTickMark: TElTickMarkPositionEvent;
      FFrequency: Integer;
      FTickMarkMode: TTickMarkMode;
      FTickColor: TColor;
      FTrackColor: TColor;
      FTrackFrameColor: TColor;
      FTrackWidth: Integer;
      FTrackSelColor: TColor;
      FShowSelection: Boolean;
      FSelectionMarkSize: Integer;
      FThumbWidth: Integer;
      FThumbVisible: Boolean;
      FDown,
      FDragging : boolean;
      FDragPos  : TPoint;
      FSavePosition : integer;
      FSaveThumb : TRect;
      FSaveCapture  : HWND;
      FPosition: Integer;
      FOnHitTest: TElTrackHitTestEvent;
      FDownTimer : TTimer;
      FThumbColor: TColor;
      FOnTrack: TElTrackChangeEvent;
      FFlat: Boolean;
      {$ifdef ELPACK_UNICODE}
      FHint: WideString;
      {$endif}

      function GetPosition: Integer;
      procedure SetOrientation(newValue: TElTrackBarOrientation);
      procedure SetMax(newValue: Integer);
      procedure SetMin(newValue: Integer);
      procedure SetPage(newValue: Integer);
      procedure SetPosition(newValue: Integer);
      procedure DoSetMax(newValue : Integer; Redraw : boolean);
      procedure DoSetMin(newValue : Integer; Redraw : boolean);
      procedure DoSetPage(newValue : Integer; Redraw : boolean);
      procedure DoSetPosition(newValue : Integer; Redraw : boolean);
      function AdjustThumbPos: Integer;
      procedure SetImageForm(newValue: TElImageForm); virtual;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure SetTickPosition(Value: TElTrackTickPosition);
      procedure SetOffsetLeft(Value: Integer);
      procedure SetOffsetRight(Value: Integer);
      procedure TriggerDrawPartEvent(Canvas : TCanvas; R : TRect; Part : 
          TElTrackBarPart; Enabled, Focused, Pressed : boolean; var DefaultDraw : 
          boolean); virtual;
      procedure SetOwnerDraw(Value: Boolean);
      procedure TriggerChangeEvent(NewPos : integer); virtual;
      procedure TriggerChangingEvent(var NewPos : integer; var AllowChange : boolean);
          virtual;
      function GetAutoThumbSize: Integer; virtual;
      procedure CreateParams(var Params : TCreateParams); override;
      procedure SetTransparent(newValue : Boolean); virtual;
      procedure SetThumbType(Value: TElTrackThumbType);
      procedure WMEraseBkgnd(var Msg : TWMEraseBkGnd); message WM_ERASEBKGND;
      procedure SetTickWidth(Value: Integer);
      procedure SetSelStart(Value: Integer);
      procedure SetSelEnd(Value: Integer);
      procedure TriggerTickMarkPositionEvent(PrevPos : integer; var TickPos :
          integer); virtual;
      procedure SetFrequency(Value: Integer);
      procedure SetTickMarkMode(Value: TTickMarkMode);
      procedure SetTickColor(Value: TColor);
      procedure SetTrackColor(Value: TColor);
      procedure SetTrackFrameColor(Value: TColor);
      procedure SetTrackWidth(Value: Integer);
      procedure SetTrackSelColor(Value: TColor);
      procedure SetShowSelection(Value: Boolean);
      procedure SetSelectionMarkSize(Value: Integer);
      function CalcThumbRect: TRect;
      procedure SetThumbWidth(Value: Integer);
      procedure SetThumbVisible(Value: Boolean);
      procedure DrawThumb(Canvas : TCanvas; Rect : TRect);
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure TriggerHitTestEvent(X, Y : integer; var Part : TElTrackBarPart; var
          DefaultTest : boolean); virtual;
      procedure OnDownTimer(Sender :TObject);
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
          override;
      procedure IFMRepaintChildren(var Message: TMessage); message
          IFM_REPAINTCHILDREN;
      procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
          WM_WINDOWPOSCHANGED;
      procedure ImageFormChange(Sender : TObject);
      procedure SetThumbColor(Value: TColor);
      procedure TriggerTrackEvent(NewPos : integer); virtual;
      procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
      function GetThemedClassName: WideString; override;
      procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
      procedure CMExit(var Msg: TCMExit); message CM_EXIT;
      procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
      procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
      procedure SetFlat(Value: Boolean);

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
      procedure Paint; override;
      destructor Destroy; override;
      function GetHitTest(X, Y : integer): TElTrackBarPart;

      property Tracking : boolean read FDragging;
    published
      property Orientation: TElTrackBarOrientation read FOrientation write
          SetOrientation default toHorizontal;
      property ImageForm: TElImageForm read FImgForm write SetImageForm;
      property Max: Integer read FMax write SetMax default 20;
      property Min: Integer read FMin write SetMin default 0;
      property Page: Integer read FPage write SetPage default 5;
      property Position: Integer read GetPosition write SetPosition default 0;
      property ShowTrackHint: Boolean read FShowTrackHint write FShowTrackHint
          default false;
      property TickPosition: TElTrackTickPosition read FTickPosition write
          SetTickPosition default tpAbove;
      property OffsetLeft: Integer read FOffsetLeft write SetOffsetLeft default 5;
      property OffsetRight: Integer read FOffsetRight write SetOffsetRight default 5;
      property Transparent: Boolean read FTransparent write SetTransparent default false;
      property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default false;

      property ThumbType: TElTrackThumbType read FThumbType write SetThumbType
          default tstPointerTop;
      property TickWidth: Integer read FTickWidth write SetTickWidth default 3;
      property SelStart: Integer read FSelStart write SetSelStart default 0;
      property SelEnd: Integer read FSelEnd write SetSelEnd;
      property Frequency: Integer read FFrequency write SetFrequency default 2;
      property TickMarkMode: TTickMarkMode read FTickMarkMode write SetTickMarkMode
          default tmmAuto;
      property TickColor: TColor read FTickColor write SetTickColor default clBtnText;
      property TrackColor: TColor read FTrackColor write SetTrackColor default
          clWindow;
      property TrackFrameColor: TColor read FTrackFrameColor write SetTrackFrameColor
          default clBtnShadow;
      property TrackWidth: Integer read FTrackWidth write SetTrackWidth default 10;
      property TrackSelColor: TColor read FTrackSelColor write SetTrackSelColor default clHighlight;
      property ShowSelection: Boolean read FShowSelection write SetShowSelection default false;
      property SelectionMarkSize: Integer read FSelectionMarkSize write
          SetSelectionMarkSize default 3;
      property ThumbWidth: Integer read FThumbWidth write SetThumbWidth default 9;
      property ThumbVisible: Boolean read FThumbVisible write SetThumbVisible default
          true;

      property OnDrawPart: TElTrackDrawPartEvent read FOnDrawPart write FOnDrawPart;
      property OnChange: TElTrackChangeEvent read FOnChange write FOnChange;
      property OnChanging: TElTrackChangingEvent read FOnChanging write FOnChanging;
      property OnTickMark: TElTickMarkPositionEvent read FOnTickMark write
          FOnTickMark;
      property OnHitTest: TElTrackHitTestEvent read FOnHitTest write FOnHitTest;
      property ThumbColor: TColor read FThumbColor write SetThumbColor default
          clBtnFace;
      property OnTrack: TElTrackChangeEvent read FOnTrack write FOnTrack;

      {$ifdef ELPACK_UNICODE}
      property Hint: WideString read FHint write SetHint;
      {$endif}

      {$IFDEF VCL_4_USED}
      property Anchors;
      {$ENDIF}
      {$IFDEF VCL_4_USED}
      property Constraints;
      {$ENDIF}
      {$IFDEF VCL_4_USED}
      property DragKind;
      {$ENDIF}


      property Align;
      property Color;
  {$ifndef CLX_USED}
      property Ctl3D;
  {$endif}
      property Enabled;
      property ParentColor;
      property ParentShowHint;
      property ShowHint;

      property TabOrder;
      property TabStop;
      property Visible;
      property UseXPThemes;

  {$IFDEF VCL_4_USED}
  {$ifndef CLX_USED}
      property OnStartDock;
      property OnEndDock;
  {$endif}
  {$ENDIF}
  {$IFDEF VCL_5_USED}
      property OnContextPopup;
  {$ENDIF}
      property OnDragDrop;
      property OnDragOver;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnMouseDown;
      property OnMouseUp;
      property OnMouseMove;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnStartDrag;
      property Flat: Boolean read FFlat write SetFlat default false;
      property ActiveBorderType: TElFlatBorderType read FActiveBorderType write 
          SetActiveBorderType default fbtSunken;
      property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write 
          SetInactiveBorderType default fbtSunkenOuter;
    end;

implementation

procedure TElTrackBar.SetImageForm(newValue: TElImageForm);
{ Sets data member FImageForm to newValue. }
begin
  if FImgForm <> newValue then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnregisterChanges(FImgFormChLink);
    end;
    FImgForm := newValue;
    if FImgForm <> nil then
    begin
      FImgForm.RegisterChanges(FImgFormChLink);
      FImgForm.FreeNotification(Self);
    end;
    Invalidate;
  end;
end; { SetImageForm }

procedure TElTrackBar.SetOrientation(newValue: TElTrackBarOrientation);
begin
  if (FOrientation <> newValue) then
  begin
    FOrientation := newValue;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Height, Width);
    Invalidate;
  end; { if }
end; { SetKind }

procedure TElTrackBar.SetMax(newValue: Integer);
begin
  DoSetMax(newValue, not (csLoading in ComponentState) and HandleAllocated);
end; { SetMax }

procedure TElTrackBar.SetMin(newValue: Integer);
{ Sets data member FMin to newValue. }
begin
  DoSetMin(newValue, not (csLoading in ComponentState) and HandleAllocated);
end; { SetMin }

procedure TElTrackBar.SetPage(newValue: Integer);
{ Sets data member FPage to newValue. }
begin
  DoSetPage(newValue, not (csLoading in ComponentState) and HandleAllocated);
end; { SetPage }

procedure TElTrackBar.SetPosition(newValue: Integer);
begin
  DoSetPosition(newValue, not (csLoading in ComponentState) and HandleAllocated);
end; { SetPosition }

procedure TElTrackBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewPos: Integer;
  DoChange: boolean;
begin
  if (Shift = []) then
  begin
    case Key of
      {$ifdef MSWINDOWS}
      VK_PRIOR:
      {$else}
      KEY_PRIOR:
      {$endif}
        begin
          NewPos := ElTools.Max(FMin, FPosition - FPage);
        end;
      {$ifdef MSWINDOWS}
      VK_NEXT:
      {$else}
      KEY_NEXT:
      {$endif}
        begin
          NewPos := ElTools.Min(FMax, FPosition + FPage);
        end;
      {$ifdef MSWINDOWS}
      VK_LEFT, VK_UP:
      {$else}
      KEY_LEFT, KEY_UP:
      {$endif}
        begin
          NewPos := ElTools.Max(FMin, FPosition - 1);
        end;
      {$ifdef MSWINDOWS}
      VK_RIGHT, VK_DOWN:
      {$else}
      KEY_RIGHT, KEY_DOWN:
      {$endif}
        begin
          NewPos := ElTools.Min(FMax, FPosition + 1);
        end;
      {$ifdef MSWINDOWS}
      VK_HOME:
      {$else}
      KEY_HOME:
      {$endif}
        begin
          NewPos := FMin;
        end;
      {$ifdef MSWINDOWS}
      VK_END:
      {$else}
      KEY_END:
      {$endif}
        begin
          NewPos := FMax;
        end;
    end;
    DoChange := true;
    TriggerChangingEvent(NewPos, DoChange);
    if DoChange then
    begin
      Position := NewPos;
      TriggerChangeEvent(NewPos);
    end;
    Key := 0;
  end
  else
    inherited;
end; { KeyDown }

procedure TElTrackBar.SetTickPosition(Value: TElTrackTickPosition);
begin
  if FTickPosition <> Value then
  begin
    FTickPosition := Value;
    Invalidate;
  end;
end;

constructor TElTrackBar.Create(AOwner : TComponent);
begin
  inherited;
  Color := clBtnFace;
  FTickPosition := tpAbove;
  FTickColor  := clBtnText;
  FTrackColor := clWindow;
  FTrackFrameColor := clBtnShadow;
  FTrackSelColor := clhighlight;
  FThumbColor := clBtnFace;
  FMin := 0;
  FMax := 20;
  FPage := 5;
  FFrequency := 2;
  FTickWidth := 3;
  FTrackWidth:= 10;
  FOffsetLeft := 5;
  FOffsetRight := 5;
  FShowSelection := false;
  FSelectionMarkSize := 3;
  FThumbVisible := true;
  FThumbWidth := 9;
  Width := 100;
  Height := 20;
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
end;

procedure TElTrackBar.SetOffsetLeft(Value: Integer);
begin
  if FOffsetLeft <> Value then
  begin
    FOffsetLeft := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetOffsetRight(Value: Integer);
begin
  if FOffsetRight <> Value then
  begin
    FOffsetRight := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.TriggerDrawPartEvent(Canvas : TCanvas; R : TRect; Part : 
    TElTrackBarPart; Enabled, Focused, Pressed : boolean; var DefaultDraw : 
    boolean);
begin
  DefaultDraw := true;
  if FOwnerDraw then
    exit;
  if Assigned(FOnDrawPart) then
    FOnDrawPart(Self, Canvas, R, Part, Enabled, Focused, Pressed, DefaultDraw);
end;

procedure TElTrackBar.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.DoSetMax(newValue: Integer; Redraw: boolean);
begin
  if (FMax <> newValue) then
  begin
    FMax := newValue;
    if not (csLoading in ComponentState) then
    begin
      if FMin >= FMax then FMax := FMin + 1;
      FPage := ElTools.Min(FPage, FMax - FMin + 1);
      FThumbPos := AdjustThumbPos;
      if HandleAllocated then Invalidate;
    end;
    if FPosition > FMax then
      SetPosition(Max)
    else
      if Redraw then Invalidate;
  end; { if }
end;

procedure TElTrackBar.DoSetMin(newValue: Integer; Redraw: boolean);
begin
  if (FMin <> newValue) then
  begin
    FMin := newValue;
    if not (csLoading in ComponentState) then
    begin
      if FMin >= FMax then FMax := FMin + 1;
      FPage := ElTools.Min(FPage, FMax - FMin + 1);
      FThumbPos := AdjustThumbPos;
      if HandleAllocated then Invalidate;
    end;
    if FPosition < FMin then
      SetPosition(Min)
    else
      if Redraw then Invalidate;
  end; { if }
end;

procedure TElTrackBar.DoSetPage(newValue: Integer; Redraw: boolean);
begin
  if (FPage <> newValue) and (newValue >= 0) then
  begin
    FPage := newValue;
    if (not (csLoading in ComponentState)) and HandleAllocated then Invalidate;
  end; { if }
end;

procedure TElTrackBar.DoSetPosition(newValue: Integer; Redraw: boolean);
var op : integer;
begin
  if (FPosition <> newValue) then
  begin
    op := FPosition;
    if not (csLoading in ComponentState) then
    begin
      if newValue < FMin then
        FPosition := FMin
      else
        if newValue > FMax then
          FPosition := FMax
        else
          FPosition := newValue;
      FThumbPos := AdjustThumbPos;
    end
    else
      FPosition := newValue;
    if Redraw and (OP <> FPosition) then
      Invalidate;
  end; { if }
end;

procedure TElTrackBar.TriggerChangeEvent(NewPos : integer);
begin
  if assigned(FOnChange) then
    FOnChange(Self, NewPos);
end;

procedure TElTrackBar.TriggerChangingEvent(var NewPos : integer; var 
    AllowChange : boolean);
begin
  if assigned(FOnChanging) then
    FOnChanging(Self, NewPos, AllowChange);
end;

function TElTrackBar.AdjustThumbPos: Integer;
var
  CS  : integer;
  ATS : integer;
  XX  : integer;
  TB,
  BB  : integer;
begin
  if Orientation = toHorizontal then
    CS := ClientWidth
  else
    CS := ClientHeight;
  ATS := GetAutoThumbSize;
  TB := OffsetLeft;
  BB := OffsetRight;
  if (CS > (TB + BB + ATS)) and (Max > Min - 1) then
  begin
    XX := ElTools.Min(Max + 1 - Page, Position - Min);
    result := TB + ElTools.Max(0, MulDiv((CS - (TB + BB) - ATS),
      XX, (Max - Min + 1) - ElTools.Min((Page), (Max - Min + 1))));
  end
  else
    result := TB;
end;

function TElTrackBar.GetAutoThumbSize: Integer;
var pid,
    sid  : integer;
    PS   : TSize;
begin
  if IsThemeApplied then
  begin
    if not Enabled then
      sid := TUS_DISABLED
    else
    if FDown then
      sid := TUS_PRESSED
    else
    if FMOuseOverPart = tbpThumb then
      sid := TUS_HOT
    else
    if Focused then
      sid := TUS_FOCUSED
    else
      sid := TUS_NORMAL;
    if Orientation = toHorizontal then
    begin
      if ThumbType = tstBox then
        pid := TKP_THUMB
      else
      if ThumbType = tstPointerTop then
        pid := TKP_THUMBTOP
      else
        pid := TKP_THUMBBOTTOM;
    end
    else
    begin
      if ThumbType = tstBox then
        pid := TKP_THUMBVERT
      else
      if ThumbType = tstPointerTop then
        pid := TKP_THUMBLEFT
      else
        pid := TKP_THUMBRIGHT;
    end;
    GetThemePartSize(Theme, Canvas.Handle, pid, sid, nil, TS_DRAW, PS);
    if Orientation = toHorizontal then
      result := PS.cy
    else
      result := PS.cx;
  end
  else
    Result := 12;
end;

procedure TElTrackBar.CreateParams(var Params : TCreateParams);
begin
  inherited;
  if Transparent then
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT
  else
    Params.ExStyle := Params.ExStyle and not WS_EX_TRANSPARENT;
end; { CreateParams }

procedure TElTrackBar.SetTransparent(newValue : Boolean);
{ Sets data member FTransparent to newValue. }
begin
  if (FTransparent <> newValue) then
  begin
    FTransparent := newValue;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    {$ifndef CLX_USED}
    RecreateWnd;
    {$else}
    RecreateWidget;
    {$endif}
  end; { if }
end; { SetTransparent }

procedure TElTrackBar.DrawThumb(Canvas : TCanvas; Rect : TRect);
var R : TRect;
    Points : array[0..4] of TPoint;
    pid,
    sid    : integer;
    P      : TPoint;
    T      : TElTrackBarPart;
begin
  Canvas.Brush.Color := ThumbColor;
  Canvas.Pen.Color := Color;
  if IsThemeApplied then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    T := GetHitTest(P.X, P.Y);

    R := Rect;
    if not Enabled then
      sid := TUS_DISABLED
    else
    if FDown then
      sid := TUS_PRESSED
    else
    if T = tbpThumb then
      sid := TUS_HOT
    else
    if Focused then
      sid := TUS_FOCUSED
    else
      sid := TUS_NORMAL;
    if Orientation = toHorizontal then
    begin
      if ThumbType = tstBox then
        pid := TKP_THUMB
      else
      if ThumbType = tstPointerTop then
        pid := TKP_THUMBTOP
      else
        pid := TKP_THUMBBOTTOM;
    end
    else
    begin
      if ThumbType = tstBox then
        pid := TKP_THUMBVERT
      else
      if ThumbType = tstPointerTop then
        pid := TKP_THUMBLEFT
      else
        pid := TKP_THUMBRIGHT;
    end;
    DrawThemeBackground(Theme, Canvas.Handle, pid, sid, Rect, nil);
  end
  else
  if Orientation = toHorizontal then
  begin
    if ThumbType = tstBox then
    begin
      Canvas.FillRect(Rect);
      ELVCLUtils.DrawButtonFrameEx(Canvas.Handle, Rect, (not Flat) or Focused or (FMouseOverPart <> tbpNowhere), false, clBtnFace, false);
    end
    else
    if ThumbType = tstPointerTop then
    begin
      R := Rect;
      Points[0] := Point(R.Left, R.Top + ThumbWidth div 2 + 1);
      Points[1] := Point(R.Left + ThumbWidth div 2, R.Top);
      Points[2] := Point(R.Right - 1, R.Top + ThumbWidth div 2 + 1);
      Points[3] := Point(R.Right - 1, R.Bottom - 1);
      Points[4] := Point(R.Left, R.Bottom - 1);
      Polygon(Canvas.Handle, Points, 5);
      R := Rect;
      R.Top := R.Top + ThumbWidth div 2 + 1;
      DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_LEFT);
      inc(R.Left);
      DrawEdge(Canvas.Handle, R, BDR_RAISED, BF_BOTTOMRIGHT);
      R.Right := R.Left;
      dec(R.Left);
      DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_BOTTOM);
      R := Rect;
      R.Bottom := R.Top + ThumbWidth div 2 + 1;
      R.Right := R.Left + ThumbWidth div 2 + 2;
      DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_DIAGONAL_ENDBOTTOMLEFT);
      R.Left := R.Right - 2;
      R.Right := Rect.Right;
      DrawEdge(Canvas.Handle, R, BDR_RAISED, BF_DIAGONAL_ENDBOTTOMRIGHT);
    end
    else
    begin
      R := Rect;
      Points[0] := Point(R.Left, R.Top);
      Points[1] := Point(R.Right - 1, R.Top);
      Points[2] := Point(R.Right - 1, R.Bottom - (ThumbWidth div 2 + 1));
      Points[3] := Point(R.Right - (ThumbWidth div 2 + 1), R.Bottom);
      Points[4] := Point(R.Left, R.Bottom - (ThumbWidth div 2 + 1));
      Polygon(Canvas.Handle, Points, 5);
      R := Rect;
      R.Bottom := R.Bottom - ThumbWidth div 2 + 1;

      DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_TOPLEFT);
      inc(R.Top);
      DrawEdge(Canvas.Handle, R, BDR_RAISED, BF_RIGHT);
      R.Bottom := R.Top;
      dec(R.Top);
      DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_RIGHT);
      R := Rect;
      R.Top := R.Bottom - ThumbWidth div 2;
      Inc(R.Bottom);
      R.Right := R.Left + ThumbWidth div 2 + 1;
      DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_DIAGONAL_ENDBOTTOMRIGHT);
      R.Left := R.Left + ThumbWidth div 2;
      R.Right := Rect.Right - 1;
      DrawEdge(Canvas.Handle, R, BDR_RAISED, BF_DIAGONAL_ENDBOTTOMLEFT);
    end;
  end
  else
  begin
    if ThumbType = tstBox then
    begin
      Canvas.FillRect(Rect);
      ELVCLUtils.DrawButtonFrameEx(Canvas.Handle, Rect, (not Flat) or Focused or (FMouseOverPart <> tbpNowhere), false, clBtnFace, false);
    end
    else
    if ThumbType = tstPointerTop then
    begin
      R := Rect;
      Points[0] := Point(R.Right - 1, R.Top);
      Points[1] := Point(R.Right - 1, R.Bottom - 1);
      Points[2] := Point(R.Left + ThumbWidth div 2 - 1, R.Bottom - 1);
      Points[3] := Point(R.Left, R.Top + ThumbWidth div 2 + 1);
      Points[4] := Point(R.Left + ThumbWidth div 2 - 1, R.Top);
      Polygon(Canvas.Handle, Points, 5);

      R := Rect;
      R.Left := R.Left + ThumbWidth div 2 - 1;
      DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_TOP);
      inc(R.Top);
      DrawEdge(Canvas.Handle, R, BDR_RAISED, BF_BOTTOMRIGHT);
      R.Bottom := R.Top;
      Dec(R.Top);
      DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_RIGHT);
      R := Rect;
      R.Right := R.Left + ThumbWidth div 2 + 1;
      R.Bottom := R.Top + ThumbWidth div 2;
      DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_DIAGONAL_ENDBOTTOMLEFT);

      R := Classes.Rect(Rect.Left, Rect.Top + ThumbWidth div 2, Rect.Left + ThumbWidth div 2 + 1, Rect.Bottom - 1);
      DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_DIAGONAL_ENDTOPLEFT);
      OffsetRect(R, -1, 0);
      DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_DIAGONAL_ENDBOTTOMRIGHT);
    end
    else
    begin
      R := Rect;
      Points[0] := Point(R.Left, R.Top);
      Points[1] := Point(R.Left, R.Bottom - 1);
      Points[2] := Point(R.Right - ThumbWidth div 2 - 1, R.Bottom - 1);
      Points[3] := Point(R.Right - 1, R.Bottom - ThumbWidth div 2 - 1);
      Points[4] := Point(R.Right - ThumbWidth div 2 - 1, R.Top);
      Polygon(Canvas.Handle, Points, 5);

      R := Rect;
      R.Right := R.Right - ThumbWidth div 2;
      DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_TOPLEFT);
      inc(R.Left);
      DrawEdge(Canvas.Handle, R, BDR_RAISED, BF_BOTTOM);
      dec(R.Left);
      R.Right := R.Left + 1;
      DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_BOTTOM);


      R := Rect;
      R.Left := Rect.Right - ThumbWidth div 2 - 1;
      R.Right := Rect.Right;
      R.Top := Rect.Bottom - ThumbWidth div 2 - 1;
      R.Bottom := Rect.Bottom;
      DrawEdge(Canvas.Handle, R, BDR_RAISED, BF_DIAGONAL_ENDBOTTOMLEFT);

      R.Bottom := R.Top;
      R.Top := Rect.Top;
      DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_DIAGONAL_ENDBOTTOMRIGHT);
    end;
  end;
end;

procedure TElTrackBar.Paint;
var ARect,
    BgRect,
    R1    : TRect;
    OldP,
    P     : TPoint;
    ACtl  : TWinControl;
    ax, ay: integer;
    DefaultDraw : boolean;
    ss,
    se    : integer;
    Bitmap: TBitmap;
    // DC    : HDC;

    procedure DrawTickMarks(R : TRect);
    var i,
        p,
        j : integer;
        R1: TRect;
    begin
      Bitmap.Canvas.Pen.Color := TickColor;
      if Orientation = toHorizontal then
      begin
        if TickMarkMode = tmmAuto then
        begin
          i := R.Left + OffsetLeft;
          j := 1;
          while i < R.Right - OffsetRight do
          begin
            if IsThemeApplied then
            begin
              R1 := Rect(i, R.Top, i + 1, R.Top + TickWidth);
              DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICS, TSS_NORMAL, R1, nil);
            end
            else
            begin
              Bitmap.Canvas.MoveTo(i, R.Top);
              Bitmap.Canvas.LineTo(i, R.Top + TickWidth);
            end;
            if FMax > FMin then
              i := R.Left + OffsetLeft + MulDiv(Frequency * j, R.Right - R.Left - OffsetLeft - OffsetRight, FMax - FMin)
            else
              i := R.Left + OffsetLeft + 1;
            inc(j);
          end;
          i := R.Right - OffsetRight - 1;
          if IsThemeApplied then
          begin
            R1 := Rect(i, R.Top, i + 1, R.Top + TickWidth);
            DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICS, TSVS_NORMAL, R1, nil);
          end
          else
          begin
            Bitmap.Canvas.MoveTo(i, R.Top);
            Bitmap.Canvas.LineTo(i, R.Top + TickWidth);
          end;
        end
        else
        begin
          i := R.Left + OffsetLeft;
          if IsThemeApplied then
          begin
            R1 := Rect(i, R.Top, i + 1, R.Top + TickWidth);
            DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICS, TSS_NORMAL, R1, nil);
          end
          else
          begin
            Bitmap.Canvas.MoveTo(i, R.Top);
            Bitmap.Canvas.LineTo(i, R.Top + TickWidth);
          end;
          i := R.Right - OffsetRight - 1;
          if IsThemeApplied then
          begin
            R1 := Rect(i, R.Top, i + 1, R.Top + TickWidth);
            DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICS, TSS_NORMAL, R1, nil);
          end
          else
          begin
            Bitmap.Canvas.MoveTo(i, R.Top);
            Bitmap.Canvas.LineTo(i, R.Top + TickWidth);
          end;
          TriggerTickMarkPositionEvent(-1, i);
          while i <> -1 do
          begin
            p := R.Left + OffsetLeft + MulDiv(i, R.Right - R.Left - OffsetLeft - OffsetRight, FMax - FMin);
            if IsThemeApplied then
            begin
              R1 := Rect(i, R.Top, i + 1, R.Top + TickWidth);
              DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICS, TSS_NORMAL, R1, nil);
            end
            else
            begin
              Bitmap.Canvas.MoveTo(p, R.Top);
              Bitmap.Canvas.LineTo(p, R.Top + TickWidth);
            end;
            TriggerTickMarkPositionEvent(i, i);
          end;
        end;
      end
      else
      begin
        if TickMarkMode = tmmAuto then
        begin
          i := R.Top + OffsetLeft;
          j := 1;
          while i < R.Bottom - OffsetRight do
          begin
            if IsThemeApplied then
            begin
              R1 := Rect(R.Left, i, R.Left + TickWidth, i + 1);
              DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICSVERT, TSVS_NORMAL, R1, nil);
            end
            else
            begin
              Bitmap.Canvas.MoveTo(R.Left, i);
              Bitmap.Canvas.LineTo(R.Left + TickWidth, i);
            end;
            i := R.Top + OffsetLeft + MulDiv(Frequency * j, R.Bottom - R.Top - OffsetLeft - OffsetRight, FMax - FMin);
            inc(j);
          end;
          i := R.Bottom - OffsetRight - 1;
          if IsThemeApplied then
          begin
            R1 := Rect(R.Left, i, R.Left + TickWidth, i + 1);
            DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICSVERT, TSVS_NORMAL, R1, nil);
          end
          else
          begin
            Bitmap.Canvas.MoveTo(R.Left, i);
            Bitmap.Canvas.LineTo(R.Left + TickWidth, i);
          end;
        end
        else
        begin
          i := R.Top + OffsetLeft;
          if IsThemeApplied then
          begin
            R1 := Rect(R.Left, i, R.Left + TickWidth, i + 1);
            DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICSVERT, TSVS_NORMAL, R1, nil);
          end
          else
          begin
            Bitmap.Canvas.MoveTo(R.Left, i);
            Bitmap.Canvas.LineTo(R.Left + TickWidth, i);
          end;
          i := R.Bottom - OffsetRight - 1;
          if IsThemeApplied then
          begin
            R1 := Rect(R.Left, i, R.Left + TickWidth, i + 1);
            DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICSVERT, TSVS_NORMAL, R1, nil);
          end
          else
          begin
            Bitmap.Canvas.MoveTo(R.Left, i);
            Bitmap.Canvas.LineTo(R.Left + TickWidth, i);
          end;
          TriggerTickMarkPositionEvent(-1, i);
          while i <> -1 do
          begin
            p := R.Top + OffsetLeft + MulDiv(i, R.Bottom - R.Top - OffsetLeft - OffsetRight, FMax - FMin);
            if IsThemeApplied then
            begin
              R1 := Rect(R.Left, i, R.Left + TickWidth, i + 1);
              DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TICSVERT, TSVS_NORMAL, R1, nil);
            end
            else
            begin
              Bitmap.Canvas.MoveTo(R.Left, p);
              Bitmap.Canvas.LineTo(R.Left + TickWidth, p);
            end;
            TriggerTickMarkPositionEvent(i, i);
          end;
        end;
      end;
    end;

begin
  ARect := ClientRect;
  Bitmap := TBitmap.Create;
  Bitmap.Width := ARect.Right;
  Bitmap.Height := ARect.Bottom;
  
  if IsThemeApplied() then
  begin
    P := Point(Left, Top);
    SetMapMode(Bitmap.Canvas.Handle, MM_ANISOTROPIC);
    SetViewPortOrgEx(Bitmap.Canvas.Handle, -P.x, -P.y, @OldP);
    SendMessage(Parent.Handle, WM_ERASEBKGND, Bitmap.Canvas.Handle, 0);
    SetViewPortOrgEx(Bitmap.Canvas.Handle, OldP.x, OldP.y, nil);
    SetMapMode(Bitmap.Canvas.Handle, MM_TEXT);
  end
  else
  if not Transparent then
  begin
    if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
    begin
      if FImgForm.Control <> Self then
      begin
        ACtl := FImgForm.GetRealControl;
        BgRect := ClientRect;
        R1 := BgRect;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        P := Parent.ClientToScreen(Point(Left, Top));
        ax := BgRect.Left - P.x;
        ay := BgRect.Top - P.y;

        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);

        FImgForm.PaintBkgnd(Bitmap.Canvas.Handle, R1, Point(BgRect.Left - ax, BgRect.Top - ay), false);
      end;
    end
    else
    begin
      Bitmap.Canvas.Brush.Color := Color;
      Bitmap.Canvas.FillRect(ClientRect);
    end;
  end
  else
  begin
    {$ifdef MSWINDOWS}
    GetClipBox(Canvas.Handle, ARect);
    // ARect := ClientRect;
    OffsetRect(ARect, Left, Top);
    RedrawWindow(Parent.Handle, @ARect, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN or RDW_UPDATENOW);
    OffsetRect(ARect, -Left, -Top);

    // DC := GetDC(Handle);
    bitblt(Bitmap.Canvas.Handle, ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, Canvas.Handle, ARect.Left, ARect.Top, SRCCOPY);
    // ReleaseDC(Handle, DC);
//     Bitmap.Canvas.CopyRect(ARect, Canvas, ARect);
    {$endif}
    ARect := ClientRect;
  end;

  if Orientation = toHorizontal then
  begin
    R1 := ARect;
    if FTickPosition in [tpAbove, tpBoth] then
    begin
      R1.Bottom := R1.Top + TickWidth;
      if IsThemeApplied then
      begin
        OffsetRect(R1, 0, 1);
        DrawTickMarks(R1);
        OffsetRect(R1, 0, -1);
      end
      else
      begin
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbpTicksAbove, Enabled, Focused, FMouseDown, DefaultDraw);
        OffsetRect(R1, 0, 1);
        if DefaultDraw then
          DrawTickMarks(R1);
        OffsetRect(R1, 0, -1);
      end;
      ARect.Top := R1.Bottom;
    end;
    R1 := ARect;
    if FTickPosition in [tpBelow, tpBoth] then
    begin
      R1.Top := R1.Bottom - TickWidth - 1;
      if IsThemeApplied then
      begin
        OffsetRect(R1, 0, -1);
        DrawTickMarks(R1);
        OffsetRect(R1, 0, 1);
      end
      else
      begin
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbpTicksBelow, Enabled, Focused, FMouseDown, DefaultDraw);
        OffsetRect(R1, 0, -1);
        if DefaultDraw then
          DrawTickMarks(R1);
        OffsetRect(R1, 0, 1);
      end;
      ARect.Bottom := R1.Top;
    end;
    ARect := Rect(
              ARect.Left,
              ARect.Top + (ARect.Bottom - ARect.Top - FTrackWidth) div 2,
              ARect.Right,
              ARect.Top + (ARect.Bottom - ARect.Top - FTrackWidth) div 2 + FTrackWidth
             );

    // draw selection marks
    if ShowSelection and (SelStart >= FMin) and (SelStart < FMax) then
    begin
      if FTickPosition in [tpAbove, tpBoth] then
      begin
        R1 := Rect(ARect.Left + OffsetLeft + MulDiv(SelStart, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin) - SelectionMarkSize,
                   ARect.Top - SelectionMarkSize - 2,
                   ARect.Left + OffsetLeft + MulDiv(SelStart, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Top - 2);
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbSelMarkStart, Enabled, Focused, FMouseDown, DefaultDraw);
        if DefaultDraw then
        begin
          Bitmap.Canvas.Brush.Color := TickColor;
          Bitmap.Canvas.Pen.Color := TickColor;
          Bitmap.Canvas.Polygon([Point(R1.Left, R1.Top),
                          Point(R1.Right, R1.Top),
                          Point(R1.Right, R1.Bottom),
                          Point(R1.Left, R1.Top)]);
        end;
      end;
      if FTickPosition in [tpBelow, tpBoth] then
      begin
        R1 := Rect(ARect.Left + OffsetLeft + MulDiv(SelStart, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin) - SelectionMarkSize,
                   ARect.Bottom + 2,
                   ARect.Left + OffsetLeft + MulDiv(SelStart, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Bottom + SelectionMarkSize + 2);
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbSelMarkStart, Enabled, Focused, FMouseDown, DefaultDraw);
        if DefaultDraw then
        begin
          Bitmap.Canvas.Brush.Color := TickColor;
          Bitmap.Canvas.Pen.Color := TickColor;
          Bitmap.Canvas.Polygon([Point(R1.Left, R1.Bottom),
                          Point(R1.Right, R1.Bottom),
                          Point(R1.Right, R1.Top),
                          Point(R1.Left, R1.Bottom)]);
        end;
      end;
    end;

    // draw selection marks
    if ShowSelection and (SelEnd > FMin) and (SelEnd <= FMax) then
    begin
      if FTickPosition in [tpAbove, tpBoth] then
      begin
        R1 := Rect(ARect.Left + OffsetLeft + MulDiv(SelEnd, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Top - TickWidth - 2,
                   ARect.Left + OffsetLeft + MulDiv(SelEnd, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin) + SelectionMarkSize,
                   ARect.Top - 2);
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbSelMarkEnd, Enabled, Focused, FMouseDown, DefaultDraw);
        if DefaultDraw then
        begin
          Bitmap.Canvas.Brush.Color := TickColor;
          Bitmap.Canvas.Pen.Color := TickColor;
          Bitmap.Canvas.Polygon([Point(R1.Left, R1.Top),
                          Point(R1.Right, R1.Top),
                          Point(R1.Left, R1.Bottom),
                          Point(R1.Left, R1.Top)]);
        end;
      end;
      if FTickPosition in [tpBelow, tpBoth] then
      begin
        R1 := Rect(ARect.Left + OffsetLeft + MulDiv(SelEnd, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Bottom + 2,
                   ARect.Left + OffsetLeft + MulDiv(SelEnd, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin) + SelectionMarkSize,
                   ARect.Bottom + TickWidth + 2);
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbSelMarkEnd, Enabled, Focused, FMouseDown, DefaultDraw);
        if DefaultDraw then
        begin
          Bitmap.Canvas.Brush.Color := TickColor;
          Bitmap.Canvas.Pen.Color := TickColor;
          Bitmap.Canvas.Polygon([Point(R1.Left, R1.Bottom),
                          Point(R1.Right, R1.Bottom),
                          Point(R1.Left, R1.Top),
                          Point(R1.Left, R1.Bottom)]);
        end;
      end;
    end;

    // now draw the main things
    R1 := ARect;
    TriggerDrawPartEvent(Bitmap.Canvas, R1, tbpTrack, Enabled, Focused, FMouseDown, DefaultDraw);
    if DefaultDraw then
    begin
      Bitmap.Canvas.Brush.Color := TrackColor;

      R1.Left := OffsetLeft;
      dec(R1.Right, OffsetRight);

      if IsThemeApplied then
      begin
        DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TRACK, TRS_NORMAL, R1, nil);
      end
      else
        Bitmap.Canvas.FillRect(R1);

      if ShowSelection then
      begin
        if (SelStart >= FMin) and (SelStart < FMax) then
          ss := SelStart
        else
          ss := FMin;
        if (SelEnd > FMin) and (SelEnd <= FMax) then
          se := SelEnd
        else
          se := FMin - 1;

        if ss < se then
        begin
          ss := ARect.Left + OffsetLeft + MulDiv(ss, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin);
          se := ARect.Left + OffsetLeft + MulDiv(se, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin);
          Bitmap.Canvas.Brush.Color := TrackSelColor;
          Bitmap.Canvas.FillRect(Rect(ss, ARect.Top, se, ARect.Bottom));
        end;
      end;

      if not IsThemeApplied then
      begin
        if TrackFrameColor = clBtnShadow then
        begin
          InflateRect(R1, 1, 1);
          if Flat then
            if (Focused or (FMouseOverPart <> tbpNowhere)) then
              DrawFlatFrameEx2(Bitmap.Canvas.Handle, R1, TrackColor, TrackColor, false, Enabled, AllBorderSides, ActiveBorderType)
            else
              DrawFlatFrameEx2(Bitmap.Canvas.Handle, R1, TrackColor, TrackColor, false, Enabled, AllBorderSides, InactiveBorderType)
          else
            DrawFlatFrameEx2(Bitmap.Canvas.Handle, R1, TrackColor, TrackColor, false, Enabled, AllBorderSides, fbtSunken);
          InflateRect(R1, -1, -1);
        end
        else
        begin
          Bitmap.Canvas.Brush.Color := TrackFrameColor;
          Bitmap.Canvas.FrameRect(R1);
        end;
      end;
    end;
    if ThumbVisible then
    begin
      R1 := CalcThumbRect;
      TriggerDrawPartEvent(Bitmap.Canvas, R1, tbpThumb, Enabled, Focused, FMouseDown, DefaultDraw);
      if DefaultDraw then
        DrawThumb(Bitmap.Canvas, R1);
    end;
  end
  else  // Vertical 
  begin
    R1 := ARect;
    if FTickPosition in [tpAbove, tpBoth] then
    begin
      R1.Right := R1.Left + TickWidth;
      TriggerDrawPartEvent(Bitmap.Canvas, R1, tbpTicksAbove, Enabled, Focused, FMouseDown, DefaultDraw);
      OffsetRect(R1, 1, 0);
      if DefaultDraw then
        DrawTickMarks(R1);
      OffsetRect(R1, -1, 0);
      ARect.Left := R1.Right;
    end;
    R1 := ARect;
    if FTickPosition in [tpBelow, tpBoth] then
    begin
      R1.Left := R1.Right - TickWidth - 1;
      TriggerDrawPartEvent(Bitmap.Canvas, R1, tbpTicksBelow, Enabled, Focused, FMouseDown, DefaultDraw);
      OffsetRect(R1, 1, 0);
      if DefaultDraw then
        DrawTickMarks(R1);
      OffsetRect(R1, -1, 0);
      ARect.Right := R1.Left;
    end;
    ARect := Rect(
              ARect.Left + (ARect.Right - ARect.Left - FTrackWidth) div 2,
              ARect.Top,
              ARect.Left + (ARect.Right - ARect.Left - FTrackWidth) div 2 + FTrackWidth, 
              ARect.Bottom
             );

    // draw selection marks
    if ShowSelection and (SelStart >= FMin) and (SelStart < FMax) then
    begin
      if FTickPosition in [tpAbove, tpBoth] then
      begin
        R1 := Rect(ARect.Left - SelectionMarkSize - 2,
                   ARect.Top + OffsetLeft + MulDiv(SelStart, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin) - SelectionMarkSize,
                   ARect.Left - 2,
                   ARect.Top + OffsetLeft + MulDiv(SelStart, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin)
                  );
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbSelMarkStart, Enabled, Focused, FMouseDown, DefaultDraw);
        if DefaultDraw then
        begin
          Bitmap.Canvas.Brush.Color := TickColor;
          Bitmap.Canvas.Pen.Color := TickColor;
          Bitmap.Canvas.Polygon([Point(R1.Left, R1.Top),
                          Point(R1.Right, R1.Bottom),
                          Point(R1.Left, R1.Bottom)]);
        end;
      end;
      if FTickPosition in [tpBelow, tpBoth] then
      begin
        R1 := Rect(ARect.Right + 2,
                   ARect.Top + OffsetLeft + MulDiv(SelStart, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin) - SelectionMarkSize,
                   ARect.Right + SelectionMarkSize + 2,
                   ARect.Top + OffsetLeft + MulDiv(SelStart, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin)
                  );
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbSelMarkStart, Enabled, Focused, FMouseDown, DefaultDraw);
        if DefaultDraw then
        begin
          Bitmap.Canvas.Brush.Color := TickColor;
          Bitmap.Canvas.Pen.Color := TickColor;
          Bitmap.Canvas.Polygon([Point(R1.Left, R1.Bottom),
                          Point(R1.Right, R1.Bottom),
                          Point(R1.Right, R1.Top),
                          Point(R1.Left, R1.Bottom)]);
        end;
      end;
    end;

    // draw selection marks
    if ShowSelection and (SelEnd > FMin) and (SelEnd <= FMax) then
    begin
      if FTickPosition in [tpAbove, tpBoth] then
      begin
        R1 := Rect(ARect.Left - TickWidth - 2,
                   ARect.Top + OffsetLeft + MulDiv(SelEnd, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Left - 2,
                   ARect.Top + OffsetLeft + MulDiv(SelEnd, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin) + SelectionMarkSize
                   );
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbSelMarkEnd, Enabled, Focused, FMouseDown, DefaultDraw);
        if DefaultDraw then
        begin
          Bitmap.Canvas.Brush.Color := TickColor;
          Bitmap.Canvas.Pen.Color := TickColor;
          Bitmap.Canvas.Polygon([Point(R1.Left, R1.Top),
                          Point(R1.Right, R1.Top),
                          Point(R1.Left, R1.Bottom)]);
        end;
      end;
      if FTickPosition in [tpBelow, tpBoth] then
      begin
        R1 := Rect(ARect.Right + 2,
                   ARect.Top + OffsetLeft + MulDiv(SelEnd, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Right + TickWidth + 2,
                   ARect.Top + OffsetLeft + MulDiv(SelEnd, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin) + SelectionMarkSize
                   );
        TriggerDrawPartEvent(Bitmap.Canvas, R1, tbSelMarkEnd, Enabled, Focused, FMouseDown, DefaultDraw);
        if DefaultDraw then
        begin
          Bitmap.Canvas.Brush.Color := TickColor;
          Bitmap.Canvas.Pen.Color := TickColor;
          Bitmap.Canvas.Polygon([Point(R1.Left, R1.Top),
                          Point(R1.Right, R1.Top),
                          Point(R1.Right, R1.Bottom)]);
        end;
      end;
    end;

    // now draw the main things
    R1 := ARect;
    TriggerDrawPartEvent(Bitmap.Canvas, R1, tbpTrack, Enabled, Focused, FMouseDown, DefaultDraw);
    if DefaultDraw then
    begin
      Bitmap.Canvas.Brush.Color := TrackColor;
      R1.Top := OffsetLeft;
      dec(R1.Bottom, OffsetRight);

      if IsThemeApplied then
      begin
        DrawThemeBackground(Theme, Bitmap.Canvas.Handle, TKP_TRACK, TRS_NORMAL, R1, nil);
      end
      else
        Bitmap.Canvas.FillRect(R1);

      if ShowSelection then
      begin
        if (SelStart >= FMin) and (SelStart < FMax) then
          ss := SelStart
        else
          ss := FMin;
        if (SelEnd > FMin) and (SelEnd <= FMax) then
          se := SelEnd
        else
          se := FMin - 1;

        if ss < se then
        begin
          ss := ARect.Top + OffsetLeft + MulDiv(ss, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin);
          se := ARect.Top + OffsetLeft + MulDiv(se, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin);
          Bitmap.Canvas.Brush.Color := TrackSelColor;
          Bitmap.Canvas.FillRect(Rect(ARect.Left, ss, ARect.Right, se));
        end;
      end;

      if not IsThemeApplied then
      begin
        if TrackFrameColor = clBtnShadow then
        begin
          InflateRect(R1, 1, 1);
          if Flat then
            if (Focused or (FMouseOverPart <> tbpNowhere)) then
              DrawFlatFrameEx2(Bitmap.Canvas.Handle, R1, TrackColor, TrackColor, false, Enabled, AllBorderSides, ActiveBorderType)
            else
              DrawFlatFrameEx2(Bitmap.Canvas.Handle, R1, TrackColor, TrackColor, false, Enabled, AllBorderSides, InactiveBorderType)
          else
            DrawFlatFrameEx2(Bitmap.Canvas.Handle, R1, TrackColor, TrackColor, false, Enabled, AllBorderSides, fbtSunken);
          InflateRect(R1, -1, -1);
        end
        else
        begin
          Bitmap.Canvas.Brush.Color := TrackFrameColor;
          Bitmap.Canvas.FrameRect(R1);
        end;
      end;
    end;
    if ThumbVisible then
    begin
      R1 := CalcThumbRect;
      TriggerDrawPartEvent(Bitmap.Canvas, R1, tbpThumb, Enabled, Focused, FMouseDown, DefaultDraw);
      if DefaultDraw then
        DrawThumb(Bitmap.Canvas, R1);
    end;
  end;

  ARect := ClientRect;
  if Focused then
  begin
    //InflateRect(ARect, -1, -1);
    Bitmap.Canvas.DrawFocusRect(ARect);
    //InflateRect(ARect, 1, 1);
  end;
  Canvas.CopyRect(ARect, Bitmap.Canvas, ARect);
  Bitmap.Free;
end;

procedure TElTrackBar.SetThumbType(Value: TElTrackThumbType);
begin
  if FThumbType <> Value then
  begin
    FThumbType := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.WMEraseBkgnd(var Msg : TWMEraseBkGnd);
begin
  Msg.Result := 1;
end; { WMEraseBkGnd }

procedure TElTrackBar.SetTickWidth(Value: Integer);
begin
  if FTickWidth <> Value then
  begin
    FTickWidth := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetSelStart(Value: Integer);
begin
  if FSelStart <> Value then
  begin
    FSelStart := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetSelEnd(Value: Integer);
begin
  if FSelEnd <> Value then
  begin
    FSelEnd := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.TriggerTickMarkPositionEvent(PrevPos : integer; var 
    TickPos : integer);
begin
  TickPos := -1;
  if assigned(FOnTickMark) then
    FOnTickMark(Self, PrevPos, TickPos);
end;

procedure TElTrackBar.SetFrequency(Value: Integer);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    if FTickMarkMode = tmmAuto then
      Invalidate;
  end;
end;

procedure TElTrackBar.SetTickMarkMode(Value: TTickMarkMode);
begin
  if FTickMarkMode <> Value then
  begin
    FTickMarkMode := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetTickColor(Value: TColor);
begin
  if FTickColor <> Value then
  begin
    FTickColor := Value;
    if TickPosition <> tpNone then
      Invalidate;
  end;
end;

procedure TElTrackBar.SetTrackColor(Value: TColor);
begin
  if FTrackColor <> Value then
  begin
    FTrackColor := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetTrackFrameColor(Value: TColor);
begin
  if FTrackFrameColor <> Value then
  begin
    FTrackFrameColor := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetTrackWidth(Value: Integer);
begin
  if FTrackWidth <> Value then
  begin
    if Value mod 2 = 0 then
      inc(Value);
    FTrackWidth := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetTrackSelColor(Value: TColor);
begin
  if FTrackSelColor <> Value then
  begin
    FTrackSelColor := Value;
    if ShowSelection then
      Invalidate;
  end;
end;

procedure TElTrackBar.SetShowSelection(Value: Boolean);
begin
  if FShowSelection <> Value then
  begin
    FShowSelection := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetSelectionMarkSize(Value: Integer);
begin
  if FSelectionMarkSize <> Value then
  begin
    FSelectionMarkSize := Value;
    Invalidate;
  end;
end;

destructor TElTrackBar.Destroy;
begin
  FImgFormChLink.Free;
  inherited;
end;

function TElTrackBar.CalcThumbRect: TRect;
begin
  if Orientation = toHorizontal then
  begin
    if FMax > FMin then
      Result.Left := OffsetLeft + MulDiv(FPosition - Min, ClientWidth - OffsetLeft - OffsetRight, FMax - FMin) - (ThumbWidth div 2)
    else
      Result.Left := OffsetLeft - (ThumbWidth div 2);
    Result.Right  := Result.Left + ThumbWidth;
    Result.Top    := ClientHeight div 2 - ThumbWidth;
    Result.Bottom := Result.Top + ThumbWidth * 2;
  end
  else
  begin
    Result.Left   := ClientWidth div 2 - ThumbWidth;
    Result.Right  := Result.Left + ThumbWidth * 2;
    if FMax > FMin then
      Result.Top  := OffsetLeft + MulDiv(FPosition - Min, ClientHeight - OffsetLeft - OffsetRight, FMax - FMin) - (ThumbWidth div 2)
    else
      Result.Top := OffsetLeft - (ThumbWidth div 2);

    Result.Bottom := Result.Top + ThumbWidth;
  end;
end;

procedure TElTrackBar.SetThumbWidth(Value: Integer);
begin
  if FThumbWidth <> Value then
  begin
    FThumbWidth := Value;
    if ThumbVisible then
      Invalidate;
  end;
end;

procedure TElTrackBar.SetThumbVisible(Value: Boolean);
begin
  if FThumbVisible <> Value then
  begin
    FThumbVisible := Value;
    Invalidate;
  end;
end;

function TElTrackBar.GetHitTest(X, Y : integer): TElTrackBarPart;
var R,
    ARect : TRect;
    mx,
    my    : integer;
    Default : boolean;
begin
  TriggerHitTestEvent(X, Y, Result, Default);
  if not Default then exit;

  Result := tbpNowhere;
  ARect := ClientRect;
  if Orientation = toHorizontal then
  begin
    if TickPosition in [tpAbove, tpBoth] then
    begin
      R.Bottom := ARect.Top + TickWidth;
      if Y < TickWidth then
      begin
        result := tbpTicksAbove;
        exit;
      end;
      ARect.Top := R.Bottom;
    end;
    R := ARect;
    if TickPosition in [tpBelow, tpBoth] then
    begin
      R.Top := R.Bottom - TickWidth - 1;
      if ClientHeight - Y < TickWidth then
      begin
        result := tbpTicksBelow;
        exit;
      end;
      ARect.Bottom := R.Top;
    end;
    my := (ClientHeight - TrackWidth) div 2;
    R := CalcThumbRect;
    if ((Y < my) or (Y > my + TrackWidth)) and (not PtInRect(R, Point(X, Y))) then
    begin
      ARect := Rect(
          ARect.Left,
          ARect.Top + (ARect.Bottom - ARect.Top - FTrackWidth) div 2,
          ARect.Right,
          ARect.Top + (ARect.Bottom - ARect.Top - FTrackWidth) div 2 + FTrackWidth
        );

      if ShowSelection and (SelStart >= FMin) and (SelStart < FMax) then
      begin

        if FTickPosition in [tpAbove, tpBoth] then
        begin
          R := Rect(ARect.Left + OffsetLeft + MulDiv(SelStart, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin) - SelectionMarkSize,
                    ARect.Top - SelectionMarkSize - 2,
                    ARect.Left + OffsetLeft + MulDiv(SelStart, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin),
                    ARect.Top - 2);
          if PtInRect(R, Point(X, Y)) then
          begin
            result := tbSelMarkStart;
            exit;
          end;
        end;
        if FTickPosition in [tpBelow, tpBoth] then
        begin
          R := Rect(ARect.Left + OffsetLeft + MulDiv(SelStart, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin) - SelectionMarkSize,
                   ARect.Bottom + 2,
                   ARect.Left + OffsetLeft + MulDiv(SelStart, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Bottom + SelectionMarkSize + 2);
          if PtInRect(R, Point(X, Y)) then
          begin
            result := tbSelMarkStart;
            exit;
          end;
        end;
      end;
      if ShowSelection and (SelEnd > FMin) and (SelEnd <= FMax) then
      begin
        if FTickPosition in [tpAbove, tpBoth] then
        begin
          R := Rect(ARect.Left + OffsetLeft + MulDiv(SelEnd, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Top - TickWidth - 2,
                   ARect.Left + OffsetLeft + MulDiv(SelEnd, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin) + SelectionMarkSize,
                   ARect.Top - 2);
          if PtInRect(R, Point(X, Y)) then
          begin
            result := tbSelMarkEnd;
            exit;
          end;
        end;
        if FTickPosition in [tpBelow, tpBoth] then
        begin
          R := Rect(ARect.Left + OffsetLeft + MulDiv(SelEnd, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin),
                   ARect.Bottom + 2,
                   ARect.Left + OffsetLeft + MulDiv(SelEnd, ARect.Right - ARect.Left - OffsetLeft - OffsetRight, FMax - FMin) + SelectionMarkSize,
                   ARect.Bottom + TickWidth + 2);
          if PtInRect(R, Point(X, Y)) then
          begin
            result := tbSelMarkEnd;
            exit;
          end;
        end;
      end;
    end
    else
    begin
      if (Y >= my) and (Y <= my + TrackWidth) then
        result := tbpTrack;

      if PtInRect(R, Point(X, Y)) then
      begin
        result := tbpThumb;
      end;
    end;
  end
  else
  begin
    if TickPosition in [tpAbove, tpBoth] then
    begin
      R.Right := ARect.Left + TickWidth;
      if X < TickWidth then
      begin
        result := tbpTicksAbove;
        exit;
      end;
      ARect.Left := R.Right;
    end;
    R := ARect;
    if TickPosition in [tpBelow, tpBoth] then
    begin
      R.Left := R.Right - TickWidth - 1;
      if ClientWidth - X < TickWidth then
      begin
        result := tbpTicksBelow;
        exit;
      end;
      ARect.Right := R.Left;
    end;
    mx := (ClientWidth - TrackWidth) div 2;
    R := CalcThumbRect;
    if ((x < mx) or (X > mx + TrackWidth)) and (not PtInRect(R, Point(X, Y))) then
    begin
      ARect := Rect(
          ARect.Left + (ARect.Right - ARect.Left - FTrackWidth) div 2,
          ARect.Top,
          ARect.Left + (ARect.Right - ARect.Left - FTrackWidth) div 2 + FTrackWidth,
          ARect.Bottom
        );

      if ShowSelection and (SelStart >= FMin) and (SelStart < FMax) then
      begin

        if FTickPosition in [tpAbove, tpBoth] then
        begin
          R := Rect(ARect.Left - SelectionMarkSize - 2,
                    ARect.Top + OffsetLeft + MulDiv(SelStart, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin) - SelectionMarkSize,
                    ARect.Left - 2,
                    ARect.Top + OffsetLeft + MulDiv(SelStart, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin)
                    );
          if PtInRect(R, Point(X, Y)) then
          begin
            result := tbSelMarkStart;
            exit;
          end;
        end;
        if FTickPosition in [tpBelow, tpBoth] then
        begin
          R := Rect(ARect.Right + 2,
                    ARect.Top + OffsetLeft + MulDiv(SelStart, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin) - SelectionMarkSize,
                    ARect.Right + SelectionMarkSize + 2,
                    ARect.Top + OffsetLeft + MulDiv(SelStart, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin)
                    );
          if PtInRect(R, Point(X, Y)) then
          begin
            result := tbSelMarkStart;
            exit;
          end;
        end;
      end;
      if ShowSelection and (SelEnd > FMin) and (SelEnd <= FMax) then
      begin
        if FTickPosition in [tpAbove, tpBoth] then
        begin
          R := Rect(ARect.Left - TickWidth - 2,
                    ARect.Top + OffsetLeft + MulDiv(SelEnd, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin),
                    ARect.Left - 2,
                    ARect.Top + OffsetLeft + MulDiv(SelEnd, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin) + SelectionMarkSize
                   );
          if PtInRect(R, Point(X, Y)) then
          begin
            result := tbSelMarkEnd;
            exit;
          end;
        end;
        if FTickPosition in [tpBelow, tpBoth] then
        begin
          R := Rect(ARect.Right + 2,
                    ARect.Top + OffsetLeft + MulDiv(SelEnd, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin),
                    ARect.Bottom + TickWidth + 2,
                    ARect.Top + OffsetLeft + MulDiv(SelEnd, ARect.Bottom - ARect.Top - OffsetLeft - OffsetRight, FMax - FMin) + SelectionMarkSize
                    );
          if PtInRect(R, Point(X, Y)) then
          begin
            result := tbSelMarkEnd;
            exit;
          end;
        end;
      end;
    end
    else
    begin
      if (X >= mx) and (X <= mX + TrackWidth) then
        result := tbpTrack;

      if PtInRect(R, Point(X, Y)) then
      begin
        result := tbpThumb;
      end;
    end;
  end;
end;

function TElTrackBar.GetPosition: Integer;
begin
  {if FDragging then
    Result := FSavePosition
  else  }
    Result := FPosition;
end;

procedure TElTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var Pos    : TPoint;
    T      : TElTrackBarPart;
    NewPos : integer;
    DoChange: boolean; 
begin
  GetCursorPos(Pos);
  Pos := ScreenToClient(Pos);
  if FDown then
  begin
    if WindowFromPoint(ClientToScreen(Point(X, Y))) <> Handle then
    begin
      if (FSaveCapture <> 0) and (FSaveCapture <> Handle) then
        SetCapture(FSaveCapture)
      else
        ReleaseCapture;
      Position := FSavePosition;
      FDown := false;
      FDownTimer.Free;
      FDownTimer := nil;
    end;
  end
  else
  if FDragging then
  begin
    DoChange := true;
    if Orientation = toHorizontal then
      NewPos := Round((Pos.X - OffsetLeft) * (Max - Min) / (ClientWidth - OffsetLeft - OffsetRight)) + Min
    else
      NewPos := Round((Pos.Y - OffsetLeft) * (Max - Min) / (ClientHeight - OffsetLeft - OffsetRight)) + Min;

    NewPos := ElTools. Min(FMax, ElTools. Max(FMin, NewPos));

    if NewPos <> Position then
    begin
      TriggerChangingEvent(NewPos, DoChange);
      if DoChange then
        Position := NewPos;
      TriggerTrackEvent(FPosition);
    end;
  end
  else
  if IsThemeApplied or Flat then 
  begin
    T := GetHitTest(Pos.X, Pos.Y);
    if t <> FMouseOverPart then
      Invalidate;
    FMouseOverPart := T;
  end;
  inherited;
end;

procedure TElTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TElTrackBarPart;
    R : TRect;
begin
  inherited;
  if CanFocus then
    SetFocus;
  P := GetHitTest(X, Y);
  R := CalcThumbRect;
  if (P = tbpThumb) or (P = tbpTrack) then
  begin
    if P = tbpThumb then
    begin
      FDragging := true;
    end
    else
    if P = tbpTrack then
    begin
      FDown := true;
    end;
    FDragPos.X := X;
    FDragPos.Y := Y;
    FSaveThumb := R;
    FSavePosition := FPosition;
    FSaveCapture := GetCapture;
    SetCapture(Handle);
    FDownTimer := TTimer.Create(Self);
    FDownTimer.Enabled := false;
    FDownTimer.OnTimer := OnDownTimer;
    OnDownTimer(FDownTimer);
    FDownTimer.Enabled := true;
    FDownTimer.Interval := GetKeybTimes(SPI_GETKEYBOARDDELAY);
  end;
end;

procedure TElTrackBar.TriggerHitTestEvent(X, Y : integer; var Part :
    TElTrackBarPart; var DefaultTest : boolean);
begin
  DefaultTest := true;
  if assigned(FOnHitTest) then
    FOnHitTest(Self, X, Y, Part, DefaultTest);
end;

procedure TElTrackBar.OnDownTimer(Sender :TObject);
var R: TRect;
    Pos  : TPoint;
    DP   : integer;
    DoChange: boolean;
begin
  GetCursorPos(Pos);
  Pos := ScreenToClient(Pos);
  if FDown then
  begin
    FDownTimer.Interval := GetKeybTimes(SPI_GETKEYBOARDSPEED);
    R := CalcThumbRect;
    if (Orientation = toHorizontal) then
    begin
      if ((Pos.X < R.Left) and (FDragPos.X < FSaveThumb.Left)) then
      begin
        DP := {Position }- 1;
      end
      else
      if ((Pos.X > R.Right) and (FDragPos.X > FSaveThumb.Right)) then
      begin
        DP := {Position }+ 1;
      end
      else
        DP := 0;
    end
    else
    begin
      if ((Pos.Y < R.Top) and (FDragPos.Y < FSaveThumb.Top)) then
      begin
        DP := {Position }- 1;
      end
      else
      if ((Pos.Y > R.Bottom) and (FDragPos.Y > FSaveThumb.Bottom)) then
      begin
        DP := {Position }+ 1;
      end
      else
        DP := 0;
    end;
    DP := Position + DP;

    if DP < FMin then
      DP := FMin
    else
    if DP > FMax then
      DP := FMax;

    if Position <> DP  then
    begin
      DoChange := true;
      TriggerChangingEvent(DP, DoChange);
      if DoChange then
      begin
        Position := DP;
        TriggerChangeEvent(Position);
      end;
    end;
  end;
end;

procedure TElTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: 
    Integer);
begin
  inherited;
  if FDown or FDragging then
  begin
    if (FSaveCapture <> 0) and (FSaveCapture <> Handle) then
      SetCapture(FSaveCapture)
    else
      ReleaseCapture;
    
    FDownTimer.Free;
    FDownTimer := nil;

    FDown := false;
    if FDragging then
    begin
      FDragging := false;
      TriggerChangeEvent(FPosition);
    end;
  end;
end;

procedure TElTrackBar.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;

procedure TElTrackBar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;

procedure TElTrackBar.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TElTrackBar.SetThumbColor(Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.TriggerTrackEvent(NewPos : integer);
begin
  if assigned(FOnTrack) then
    FOnTrack(Self, NewPos);
end;

procedure TElTrackBar.WMMouseWheel(var Msg: TMessage);
var
  Dy, sl  : integer;
  NewPos  : integer;
begin
  inherited;
  if IsWinNTUp or IsWin98Up then
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @sl, SPIF_SENDCHANGE)
  else
    sl := 3;

  if sl = 0 then sl := 1;
  Dy := TWMMouseWheel(Msg).WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  if Dy <> 0 then
  begin
    NewPos := Position - Dy;
    Position := NewPos;
    TriggerChangeEvent(NewPos);
  end;
end; { WMMouseWheel }

function TElTrackBar.GetThemedClassName: WideString;
begin
  Result := 'TRACKBAR';
end;

procedure TElTrackBar.CMEnter(var Msg: TCMEnter);
begin
  inherited;
  Invalidate;
end;

procedure TElTrackBar.CMExit(var Msg: TCMExit);
begin
  inherited;
  Invalidate;
end;

procedure TElTrackBar.CMMouseLeave(var Message : TMessage);
begin
  inherited;
  FMouseOverPart := tbpNowhere;
  if IsThemeApplied or Flat then
    Invalidate;
end;

procedure TElTrackBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if IsThemeApplied then
    Invalidate;
end;

procedure TElTrackBar.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TElTrackBar.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    {$ifdef MSWINDOWS}
    if Focused or (FMouseOverPart <> tbpNowhere) then
      if HandleAllocated then
        Invalidate;
    {$endif}
  end;
end;

procedure TElTrackBar.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    {$ifdef MSWINDOWS}
    if Focused or (FMouseOverPart <> tbpNowhere) then
      if HandleAllocated then
        Invalidate;
    {$endif}
  end;  
end;

procedure TElTrackBar.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := dlgc_WantArrows; { so we get the arrow keys. }
end; { WMGetDlgCode }

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElTrackBar.CMHintShow(var Message: TMessage);
{$else}
function TElTrackBar.HintShow(var HintInfo : THintInfo): Boolean; 
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

procedure TElTrackBar.SetHint(Value: WideString);
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
