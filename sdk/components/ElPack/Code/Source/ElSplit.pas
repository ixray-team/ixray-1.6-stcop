{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

(*

Version History

03/22/2002

  Fixed positioning of the splitter in AutoSnap operations

03/05/2002

  IsSnappedLEft, IsSnappedRight properties added

12/07/2001

  Second snap button added

09/21/2001

  Windows XP support added

06/01/2001

  preview line (the one that appears when you drag the splitter) was too long 

05/28/2001 (c) Akzhan Abdulin

  Splitter dragging rewritten to be more accurate

  WM_CANCELMODE message now stops dragging.
  Test: try to press Windows key while splitter dragged.

  Controls was accepted by splitter. Now it is not.

  New Snap button can be used when one of SnapTopLeft
  or SnapBottomRight properties set to true but not both
  Inspired by Netscape Navigator 6.0.

  ESC Key isn't handled now

12/16/2000

  Focus was grabbed by splitter. Now it is not.

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

unit ElSplit;  { TElSplitter component. }

interface

uses
  ElHook,
  ExtCtrls,  
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  ElUxTheme,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElPanel;

type

  TElSplitterEvent = procedure(Sender : TObject; var NewPos : integer; var Accept : boolean) of object;

  TElSplitter = class(TCustomElPanel)
  private
    FSnapTopLeft,
    FSnapBottomRight: boolean;
    FAutoHide : Boolean;
    FOldFocused : HWND;
    FDragging   : boolean;
    FOffset     : integer;
    FControlTopLeft : TControl;
    FControlBottomRight : TControl;
    FMinSizeTopLeft : Integer;
    FMinSizeBottomRight : Integer;
    FAutoSnap : Boolean;
    FLineVisible : boolean;
    FLineDC : HDC;
    FCurPos : integer;
    FSizeBeforeSnap: Integer;
    FLeftSnapButtonPushed: Boolean;
    FLeftSnapButtonPushing: Boolean;

    FRightSnapButtonPushed: Boolean;
    FRightSnapButtonPushing: Boolean;

    FShowSnapButton: Boolean;
    FSnapButtonCursor: TCursor;
    FSnapButtonColor: TColor;
    FSnapButtonDotColor: TColor;
    FSnapButtonArrowColor: TColor;
    FLeftHook,
    FRightHook : TElHook;
    FOnPositionChanged : TNotifyEvent;  { Defined in Classes unit. }
    FOnPositionChanging : TElSplitterEvent;
    FArrowTheme: HTheme;
    procedure AfterMessage(Sender: TObject; var Msg: TMessage; var Handled: boolean);
    procedure SetMinSizeTopLeft(newValue : Integer);
    procedure SetControlTopLeft(newValue : TControl);
    procedure SetControlBottomRight(newValue : TControl);
    procedure DrawLine;
    procedure AllocateLineDC;
    procedure ReleaseLineDC;
    procedure SetAutoHide(newValue : Boolean);
    procedure UpdateAutoVis;
    function GetLeftSnapButtonRect: TRect;
    procedure RecalcCurPos(X, Y: Integer);
    procedure StopMode;
    procedure UpdateShowSnapButton;
    procedure SetSnapBottomRight(const Value: boolean);
    procedure SetSnapTopLeft(const Value: boolean);
    procedure SetShowSnapButton(const Value: Boolean);
    procedure DoSizing(L: Integer);
    procedure SetSnapButtonArrowColor(const Value: TColor);
    procedure SetSnapButtonColor(const Value: TColor);
    procedure SetSnapButtonDotColor(const Value: TColor);
    function GetRightSnapButtonRect: TRect;
  protected
    FSnappedLeft: Boolean;
    FSnappedRight: Boolean;
    FInvertSnapButtons: Boolean;
    procedure TriggerPositionChangedEvent; virtual;
    procedure TriggerPositionChangingEvent(var NewPos : integer; var Accept : boolean); virtual;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure Notification(AComponent : TComponent; operation : TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
//    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure StopSizing(X, Y : integer; Accept : boolean); virtual;
    procedure Loaded; override;
    function GetThemedClassName: WideString; override;
    {$ifdef VCL_4_USED}
    procedure RequestAlign; override;
    {$endif}
    procedure SetUseXPThemes(const Value: Boolean); override;
    procedure CreateArrowThemeHandle; virtual;
    procedure FreeArrowThemeHandle; virtual;
    procedure SetInvertSnapButtons(Value: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Snap(SnapLeft : boolean);
    property ArrowTheme: HTheme read FArrowTheme;

    property SnappedLeft: Boolean read FSnappedLeft;
    property SnappedRight: Boolean read FSnappedRight;
  published
    { Published properties and events }
    property MinSizeTopLeft : Integer read FMinSizeTopLeft write SetMinSizeTopLeft default 20;  { Published }
    property MinSizeBottomRight : Integer read FMinSizeBottomRight write FMinSizeBottomRight default 20;  { Published }
    property AutoSnap : Boolean read FAutoSnap write FAutoSnap default False;  { Published }
    property SnapTopLeft : boolean read FSnapTopLeft write SetSnapTopLeft default true;
    property SnapBottomRight : boolean read FSnapBottomRight write SetSnapBottomRight default true;
    property ControlTopLeft : TControl read FControlTopLeft write SetControlTopLeft;
    property ControlBottomRight : TControl read FControlBottomRight write SetControlBottomRight;

    property AutoHide : Boolean read FAutoHide write SetAutoHide;  { Published }

    property ShowSnapButton: Boolean read FShowSnapButton write SetShowSnapButton default false;
    property SnapButtonCursor: TCursor read FSnapButtonCursor write FSnapButtonCursor default crDefault;
    property SnapButtonColor: TColor read FSnapButtonColor write SetSnapButtonColor default clBtnHighlight;
    property SnapButtonDotColor: TColor read FSnapButtonDotColor write SetSnapButtonDotColor default clBtnShadow;
    property SnapButtonArrowColor: TColor read FSnapButtonArrowColor write SetSnapButtonArrowColor default clBtnShadow;

    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$ifndef CLX_USED}
    property UseXPThemes;
    {$endif}

    property OnPositionChanged : TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
    property OnPositionChanging : TElSplitterEvent read FOnPositionChanging write FOnPositionChanging;
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
    property InvertSnapButtons: Boolean read FInvertSnapButtons write 
        SetInvertSnapButtons default false;
  end;  { TElSplitter }

implementation

uses
  ElVCLUtils,
  ElTmSchema,
  ElTools;

procedure TElSplitter.SetMinSizeTopLeft(newValue : Integer);
{ Sets data member FMinSizeTopLeft to newValue. }
begin
  if (FMinSizeTopLeft <> newValue) then
  begin
    FMinSizeTopLeft := newValue;
  end;  { if }
end;  { SetMinSizeTopLeft }

{ Event triggers: }
procedure TElSplitter.TriggerPositionChangedEvent;
{ Triggers the OnPositionChanged event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnPositionChanged)) then
    FOnPositionChanged(Self);
end;  { TriggerPositionChangedEvent }

procedure TElSplitter.TriggerPositionChangingEvent(var NewPos : integer; var Accept : boolean);
{ Triggers the OnPositionChanging event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnPositionChanging)) then
    FOnPositionChanging(Self, NewPos , Accept );
end;  { TriggerPositionChangingEvent }

procedure TElSplitter.SetControlTopLeft(newValue : TControl);
begin
  if (FControlTopLeft <> newValue) then
  begin
    if (FControlTopLeft = Self) or (FControlTopLeft is TCustomForm) then
      newValue := nil;
    {$ifdef VCL_5_USED}
    if FControlTopLeft <> nil then
      FControlTopLeft.RemoveFreeNotification(Self);
    {$endif}
    FControlTopLeft := newValue;
    if FControlTopLeft <> nil then
       FControlTopLeft.FreeNotification(Self);
    FLeftHook.Control := newValue;
    FLeftHook.Active := (NewValue <> nil);
  end;  { if }
end;  { SetControlTopLeft }       

procedure TElSplitter.SetControlBottomRight(newValue : TControl);
begin
  if (FControlBottomRight <> newValue) then
  begin
    if (FControlBottomRight = Self) or (FControlBottomRight is TCustomForm) then newValue := nil;
    {$ifdef VCL_5_USED}
    if FControlBottomRight <> nil then
      FControlBottomRight.RemoveFreeNotification(Self);
    {$endif}
    FControlBottomRight := newValue;
    if FControlBottomRight <> nil then
       FControlBottomRight.FreeNotification(Self);
    FRightHook.Control := newValue;
    FRightHook.Active := (NewValue <> nil);
  end;  { if }
end;  { SetControlBottomRight }

procedure TElSplitter.Notification(AComponent : TComponent; operation : TOperation);
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) then
  begin
    if (AComponent = FControlTopLeft) then
       ControlTopLeft := nil;
    if (AComponent = FControlBottomRight) then
       ControlBottomRight := nil;
  end;  { if }
end;  { Notification }

procedure TElSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  { protected }
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not (csDesigning in ComponentState) and (Button = mbLeft) then
  begin
    FOldFocused := GetFocus;
    SetCapture(Handle);
    if (not FShowSnapButton) or
       ((not PtInRect(GetLeftSnapButtonRect(), Point(X, Y))) and
        (not PtInRect(GetRightSnapButtonRect(), Point(X, Y)))) then
    begin
      FDragging := true;
      if Width > Height then
      begin
        FOffset := Y;
      end
      else
      begin
        FOffset := X;
      end;
      AllocateLineDC;
      RecalcCurPos(X, Y);
      DrawLine;
    end
    else
    begin
      if PtInRect(GetLeftSnapButtonRect(), Point(X, Y)) then
      begin
        FLeftSnapButtonPushing := True;
        FLeftSnapButtonPushed := True;
      end
      else
      begin
        FRightSnapButtonPushing := True;
        FRightSnapButtonPushed := True;
      end;
      Invalidate;
    end;
  end;
end;  { MouseDown }

procedure TElSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);  { protected }
var
  NewPushed: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    DrawLine;
    RecalcCurPos(X, Y);
    DrawLine;
  end;
  if FLeftSnapButtonPushing then
  begin
    NewPushed := PtInRect(GetLeftSnapButtonRect(), Point(X, Y));
    if NewPushed <> FLeftSnapButtonPushed then
    begin
      FLeftSnapButtonPushed := NewPushed;
      Invalidate;
    end;
  end
  else
  if FRightSnapButtonPushing then
  begin
    NewPushed := PtInRect(GetRightSnapButtonRect(), Point(X, Y));
    if NewPushed <> FRightSnapButtonPushed then
    begin
      FRightSnapButtonPushed := NewPushed;
      Invalidate;
    end;
  end;
end;  { MouseMove }

procedure TElSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  { protected }
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    ReleaseCapture;
    StopSizing(X, Y, true);
  end
  else
  if FLeftSnapButtonPushing then
  begin
    ReleaseCapture;
    if FLeftSnapButtonPushed then Snap(True);
    FLeftSnapButtonPushed := false;
    Invalidate;
  end
  else
  if FRightSnapButtonPushing then
  begin
    ReleaseCapture;
    if FRightSnapButtonPushed then Snap(false);
    FRightSnapButtonPushed := false;
    Invalidate;
  end;
  FDragging := false;
  FLeftSnapButtonPushing := false;
  FLeftSnapButtonPushed := false;
  FRightSnapButtonPushing := false;
  FRightSnapButtonPushed := false;
end;  { MouseUp }

procedure TElSplitter.StopSizing(X, Y : integer; Accept : boolean);
var l : integer;
    IsVertical : boolean;
begin
  if FDragging then
  begin
    DrawLine;
    ReleaseLineDC;
    if Accept then
    begin
      FSnappedLeft := False;
      FSnappedRight := False;

      isVertical := Width > Height;
      if IsVertical then l := Y - FOffset + Top else l := X - FOffset + Left;
      if (l < MinSizeTopLeft) then
      begin
        if FAutoSnap and FSnapTopLeft then
        begin
          l := 0;
          FSnappedLeft := True;
        end
        else
          l := MinSizeTopLeft;
      end
      else
      if  ((l > Parent.ClientWidth - FMinSizeBottomRight - Width) and (not IsVertical)) or
           ((l > Parent.ClientHeight - FMinSizeBottomRight - Height) and (IsVertical)) then
      begin
        if FAutoSnap and FSnapBottomRight then
        begin
          if IsVertical then
            l := Parent.ClientHeight - Height
          else
            l := Parent.ClientWidth - Width;
          FSnappedRight := True;
        end else
        begin
          if IsVertical then
             L := Parent.ClientHeight - FMinSizeBottomRight - Height
          else
             L := Parent.ClientWidth - FMinSizeBottomRight - Width;
        end;
      end;
      DoSizing(l);
    end;
    FDragging := false;
  end;
end;

procedure TElSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Handle, 0, DCX_CACHE {or DCX_CLIPSIBLINGS }or DCX_PARENTCLIP
    or DCX_LOCKWINDOWUPDATE);
end;

procedure TElSplitter.ReleaseLineDC;
begin
  ReleaseDC(Handle, FLineDC);
end;

procedure TElSplitter.DrawLine;
begin
  FLineVisible := not FLineVisible;
  if Width > Height then
     PatBlt(FLineDC, 0, FCurPos + FOffset, Width, 1, PATINVERT)
  else
     PatBlt(FLineDC, FCurPos + FOffset, 0, 1, Height, PATINVERT);
end;

procedure TElSplitter.UpdateAutoVis;
begin
  if (FControlTopLeft <> nil) and (not FControlTopLeft.Visible) then Visible := false;
  if (FControlBottomRight <> nil) and (not FControlBottomRight.Visible) then Visible := false;
  if (FControlTopLeft <> nil) and (FControlBottomRight <> nil) and
     FControlTopLeft.Visible and FControlBottomRight.Visible then Visible := true;
end;

procedure TElSplitter.SetAutoHide(newValue : Boolean);
{ Sets data member FAutoHide to newValue. }
begin
  if (FAutoHide <> newValue) then
  begin
    FAutoHide := newValue;
    if FAutoHide then
    begin
      if not (csLoading in ComponentState) then
        UpdateAutoVis;
    end;
  end;  { if }
end;  { SetAutoHide }

procedure TElSplitter.AfterMessage(Sender: TObject; var Msg: TMessage; var Handled: boolean);
begin
  if (Msg.Msg = CM_VISIBLECHANGED) and AutoHide then
     UpdateAutoVis;
end;

procedure TElSplitter.Loaded;
begin
  inherited;
  if AutoHide then
  begin
    FAutoHide := false;
    AutoHide  := true;
  end;
  FSizeBeforeSnap := Max(MinSizeTopLeft, MinSizeBottomRight);
end;

destructor TElSplitter.Destroy;
begin
  FLeftHook.Free;
  FRighthook.Free;
  FLeftHook  := nil;
  FRightHook := nil;
  inherited Destroy;
end;  { Destroy }

constructor TElSplitter.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FSizeBeforeSnap := 20;
  FMinSizeTopLeft := 20;
  FMinSizeBottomRight := 20;
  FAutoSnap := False;
  FSnapTopLeft := true;
  FSnapBottomRight := true;
  FShowSnapButton := false;
  ControlStyle := ControlStyle - [csSetCaption, csCaptureMouse, csAcceptsControls];
  TabStop := false;
  Align := alLeft;
  Width := 5;
  Cursor := crHSplit;
  FSnapButtonCursor := crDefault;
  FSnapButtonColor := clBtnHighlight;
  FSnapButtonDotColor := clBtnShadow;
  FSnapButtonArrowColor := clBtnShadow;
  FLeftHook := TElHook.Create(nil);
  FLeftHook.OnAfterProcess := AfterMessage;
  FRightHook := TElHook.Create(nil);
  FRightHook.OnAfterProcess := AfterMessage;
end;  { Create }

procedure TElSplitter.WMCancelMode(var Msg: TMessage);
begin
  StopMode;
  Inherited;
  Msg.Result := 0;
end;

procedure TElSplitter.Paint;
var
  R1, R2: TRect;
  I, J: Integer;
  DC: HDC;
  Dir: TElArrowDir;
  ext: integer;
  RClip: TRect;

  procedure IntDrawArrow(const R: TRect);
  const
    States: array[TElArrowDir] of Integer = (
      // eadLeft, eadUp, eadRight, eadDown
      ABS_LEFTNORMAL, ABS_UPNORMAL, ABS_RIGHTNORMAL, ABS_DOWNNORMAL
    );
  begin
    if FArrowTheme <> 0 then
    begin
      DrawThemeBackground(ArrowTheme, DC, SBP_ARROWBTN, States[Dir], R, @RClip);
    end
    else
    begin
      DrawArrow(Canvas, Dir, R, SnapButtonArrowColor, True);
    end;
  end;

  procedure DrawSnapButton(R1 : TRect; LeftButton : boolean);
  var b : boolean;
  begin
    if Width > Height then
    begin
      if (LeftButton and not FSnappedLeft) or
         ((not LeftButton) and FSnappedRight) then
      begin
        Dir := eadUp;
      end
      else
      begin
        Dir := eadDown;
      end;
    end
    else
    begin
      if (LeftButton and not FSnappedLeft) or
         ((not LeftButton) and FSnappedRight) then
      begin
        Dir := eadLeft;
      end
      else
      begin
        Dir := eadRight;
      end;
    end;
    if IsThemeApplied then
    begin
      DrawThemeEdge(Theme, DC, WP_FRAME, FS_INACTIVE, R1, BDR_RAISEDOUTER,
                    BF_ADJUST or BF_FLAT or BF_MONO or BF_RECT or BF_SOFT, @R1);
    end
    else
    begin
      b := (LeftButton and FLeftSnapButtonPushed) or
           ((not LeftButton) and FRightSnapButtonPushed);
           
      DrawButtonFrameEx(DC, R1, False, b, SnapButtonColor, False);
      InflateRect(R1, -1, -1);

      FillSolidRect2(DC, R1, ColorToRGB(SnapButtonColor));
      InflateRect(R1, -1, -1);
    end;

    if IsThemeApplied then ext := 14 else ext := 7;

    R2 := R1;
    if Width > Height then
    begin
      if Width > (ext + 6) * 2 then
        InflateRect(R1, -12, 0);
    end
    else
    begin
      if Height > (ext + 6) * 2 then
        InflateRect(R1, 0, -12);
    end;

    if IsThemeApplied then
    begin
      I := R1.Left;
      while I < R1.Right do
      begin
        J := R1.Top;
        while J < R1.Bottom do
        begin
          DrawThemeBackground(Theme, DC, WP_FRAME, FS_ACTIVE, Bounds(I, J, 1, 1), @RClip);
          Inc(J, 2);
        end;
        Inc(I, 3);
      end;
    end
    else
    begin
    Canvas.Pen.Color := SnapButtonDotColor;
    DC := Canvas.Handle;
    I := R1.Left;
    while I < R1.Right do
    begin
      J := R1.Top;
      while J < R1.Bottom do
      begin
        MoveToEx(DC, I, J, nil);
        LineTo(DC, I, Succ(J));
        Inc(J, 2);
      end;
      Inc(I, 2);
    end;
    end;

    R1 := R2;
    InflateRect(R1, 1, 1);
    R2 := R1;
    if Width > Height then
    begin
      if Width > (ext + 6) * 2 then
      begin
        if (R1.Bottom - R1.Top) shr 1 = 1 then
          dec(R1.Bottom);
        R1.Left := R2.Left + 3;
        R1.Right := R1.Left + ext;
        IntDrawArrow(R1);
        R1.Left := R2.Right - ext - 3;
        R1.Right := R2.Right - 3;
        IntDrawArrow(R1);
      end;
    end
    else
    begin
      if Height > (ext + 6) * 2 then
      begin
        if (R1.Right - R1.Left) shr 1 = 1 then
          dec(R1.Right);

        R1.Top := R2.Top + 3;
        R1.Bottom := R1.Top + ext;
        IntDrawArrow(R1);
        R1.Top := R2.Bottom - ext - 3;
        R1.Bottom := R2.Bottom - 3;
        IntDrawArrow(R1);
      end;
    end;
  end;

begin
  DC := Canvas.Handle;
  RClip := Canvas.ClipRect;
  if IsThemeApplied then
  begin
    DrawThemeBackground(Theme, DC, WP_FRAME, FS_INACTIVE, ClientRect, @RClip);
  end
  else
  begin
    inherited;
  end;
  if FShowSnapButton then
  begin
    R1 := GetLeftSnapButtonRect();
    if not IsRectEmpty(R1) then
      DrawSnapButton(R1, true);

    R1 := GetRightSnapButtonRect();
    if not IsRectEmpty(R1) then
      DrawSnapButton(R1, false);
  end;
end;

function TElSplitter.GetLeftSnapButtonRect: TRect;
var
  aBW: Integer;
begin
  SetRectEmpty(Result);
  if FShowSnapButton and SnapTopLeft then
  begin
    if (BevelInner = bvNone) and (BevelOuter = bvNone) then
      aBW := 0
    else
    if (BevelInner = bvNone) xor (BevelOuter = bvNone) then
      aBW := 2 * BevelWidth
    else
      aBW := 4 * BevelWidth; 

    if Width > Height then
    begin
      if (Width shr 1 >= aBW) and (Height - aBW > 1) then
      begin
        if SnapBottomRight then
        begin
          if InvertSnapButtons then
            Result := Bounds(Width shr 1, aBW shr 1, Width shr 2, Height - aBW)
          else
            Result := Bounds(Width shr 2, aBW shr 1, Width shr 2, Height - aBW)
        end
        else
          Result := Bounds(Width shr 2, aBW shr 1, Width shr 1, Height - aBW);
      end;
    end
    else
    begin
      if (Height shr 1 >= aBW) and (Width - aBW > 1) then
      begin
        if SnapBottomRight then
        begin
          if InvertSnapButtons then
            Result := Bounds(aBW shr 1, Height shr 2, Width - aBW, Height shr 2)
          else
            Result := Bounds(aBW shr 1, Height shr 2, Width - aBW, Height shr 2);
        end
        else
          Result := Bounds(aBW shr 1, Height shr 2, Width - aBW, Height shr 1);
      end;
    end;
  end;
end;

procedure TElSplitter.StopMode;
begin
  if MouseCapture then
  begin
    ReleaseCapture;
  end;
  if FDragging then
    StopSizing(0, 0, False);

  if FLeftSnapButtonPushing then
  begin
    FLeftSnapButtonPushed := False;
    FLeftSnapButtonPushing := False;
    Invalidate;
  end;
  if FRightSnapButtonPushing then
  begin
    FRightSnapButtonPushed := False;
    FRightSnapButtonPushing := False;
    Invalidate;
  end;
end;

procedure TElSplitter.SetSnapBottomRight(const Value: boolean);
begin
  FSnapBottomRight := Value;
  UpdateShowSnapButton;
end;

procedure TElSplitter.SetSnapTopLeft(const Value: boolean);
begin
  FSnapTopLeft := Value;
  UpdateShowSnapButton;
end;

procedure TElSplitter.UpdateShowSnapButton;
begin
  if not (FSnapTopLeft or FSnapBottomRight) then
    FShowSnapButton := false;
  Invalidate;
end;

procedure TElSplitter.SetShowSnapButton(const Value: Boolean);
begin
  FShowSnapButton := Value;
  UpdateShowSnapButton;
end;

procedure TElSplitter.Snap(SnapLeft : boolean);
var
  l : integer;
begin
  if FShowSnapButton then
  begin
    if ((not FSnappedLeft) and
       (not FSnappedRight)) or
       ((FSnappedLeft and (not SnapLeft)) or
       (FSnappedRight and (SnapLeft))) then
    begin
      if FSnappedLeft or FSnappedRight then
      begin
        FSnappedLeft := false;
        FSnappedRight:= false;
        DoSizing(FSizeBeforeSnap);
      end;
      if SnapLeft then
        FSnappedLeft := true
      else
        FSnappedRight := true;

      if Width > Height then
      begin
        if SnapLeft then
        begin
          l := 0;
        end
        else
        begin
          l := Parent.ClientHeight - Height;
        end;
      end
      else
      begin
        if SnapLeft then
        begin
          l := 0;
        end
        else
        begin
          l := Parent.ClientWidth - Width;
        end;
      end;
    end
    else
    begin
      FSnappedLeft := false;
      FSnappedRight:= false;
      l := FSizeBeforeSnap;
    end;
    DoSizing(l);
  end;
end;

procedure TElSplitter.DoSizing(L: Integer);
var
  Accept: Boolean;
begin
  Accept := True;
  TriggerPositionChangingEvent(L, Accept);
  if Accept then
  begin
    Parent.DisableAlign;
    if Width > Height then
    begin
      if FSnappedLeft or FSnappedRight then
        FSizeBeforeSnap := Top;
      if (FControlTopLeft <> nil) then
        FControlTopLeft.Height := L - FControlTopLeft.Top;
      Top := L;
      if (FControlBottomRight <> nil) then
      if FSnappedLeft or FSnappedRight then
        FControlBottomRight.SetBounds(FControlBottomRight.Left, L + Height, FControlBottomRight.Width, FControlBottomRight.Top + FControlBottomRight.Height - (L + Height))
      else
        FControlBottomRight.SetBounds(FControlBottomRight.Left, L + Height, FControlBottomRight.Width, FControlBottomRight.Top + FControlBottomRight.Height - L);
    end
    else
    begin
      if FSnappedLeft or FSnappedRight then
        FSizeBeforeSnap := Left;
      if (FControlTopLeft <> nil) then
        FControlTopLeft.Width := L - FControlTopLeft.Left;
      Left := L;
      if (FControlBottomRight <> nil) then
      if FSnappedLeft or FSnappedRight then
        FControlBottomRight.SetBounds(L + Width, FControlBottomRight.Top, FControlBottomRight.Left + FControlBottomRight.Width - (L + Width), FControlBottomRight.Height)
      else
        FControlBottomRight.SetBounds(L + Width, FControlBottomRight.Top, FControlBottomRight.Left + FControlBottomRight.Width - L, FControlBottomRight.Height)
    end;
    Parent.EnableAlign;
    TriggerPositionChangedEvent;
  end;
end;

procedure TElSplitter.WMSetCursor(var Msg: TWMSetCursor);
var
  C: TCursor;
  CP: TPoint;
begin
  Msg.Result := 1;
  if Msg.CursorWnd = Handle then
  begin
    C := Screen.Cursor;
    if (C = crDefault) and not (csDesigning in ComponentState) then
    begin
      GetCursorPos(CP);
      if FShowSnapButton and
       (PtInRect(GetLeftSnapButtonRect(), ScreenToClient(CP)) or
        PtInRect(GetRightSnapButtonRect(), ScreenToClient(CP))) then
      begin
        C := SnapButtonCursor;
      end
      else
      begin
        C := Cursor;
      end;
    end;
    SetCursor(Screen.Cursors[C]);
  end
  else
    Inherited;
end;

procedure TElSplitter.SetSnapButtonArrowColor(const Value: TColor);
begin
  if FSnapButtonArrowColor <> Value then
  begin
    FSnapButtonArrowColor := Value;
    if FShowSnapButton then
    begin
      Invalidate;
    end;
  end;
end;

procedure TElSplitter.SetSnapButtonColor(const Value: TColor);
begin
  if FSnapButtonColor <> Value then
  begin
    FSnapButtonColor := Value;
    if FShowSnapButton then
    begin
      Invalidate;
    end;
  end;
end;

procedure TElSplitter.SetSnapButtonDotColor(const Value: TColor);
begin
  if FSnapButtonDotColor <> Value then
  begin
    FSnapButtonDotColor := Value;
    if FShowSnapButton then
    begin
      Invalidate;
    end;
  end;
end;

procedure TElSplitter.RecalcCurPos(X, Y: Integer);
var Mrg1,
    Mrg2  : integer;
begin
  if AutoSnap then
  begin
    Mrg1 := 0;
    Mrg2 := 0;
  end else
  begin
    Mrg1 := MinSizeTopLeft;
    Mrg2 := MinSizeBottomRight;
  end;
  if Width <= Height then
  begin
    FCurPos := Min(Max(Mrg1 -Left, X - FOffset), Parent.ClientWidth - Left - Width - Mrg2);
  end
  else
  begin
    FCurPos := Min(Max(Mrg1 -Top, Y - FOffset), Parent.ClientHeight - Top  - Height - Mrg2);
  end;
end;

{$ifdef VCL_4_USED}
procedure TElSplitter.RequestAlign;
begin
  inherited;
  if (Cursor = crHSplit) or (Cursor = crVSplit)then
  begin
    if Align in [alBottom, alTop] then
      Cursor := crVSplit
    else
      Cursor := crHSplit;
  end;
end;

{$endif}

procedure TElSplitter.SetUseXPThemes(const Value: Boolean);
begin
  if UseXPThemes <> Value then
  begin
    inherited;
    if ThemesAvailable and HandleAllocated then
    begin
      if UseXPThemes then
      begin
        CreateArrowThemeHandle;
      end
      else
      begin
        FreeArrowThemeHandle;
      end;
    end;
  end;
end;

procedure TElSplitter.CreateArrowThemeHandle;
begin
  if ThemesAvailable then
  begin
    FArrowTheme := OpenThemeData(Handle, 'SCROlLBAR');
  end
  else
  begin
    FArrowTheme := 0;
  end;
end;

procedure TElSplitter.FreeArrowThemeHandle;
begin
  if ThemesAvailable then
  begin
    CloseThemeData(FArrowTheme);
  end;
  FArrowTheme := 0;
end;

function TElSplitter.GetThemedClassName: WideString;
begin
  Result := 'TOOLBAR';
end;

function TElSplitter.GetRightSnapButtonRect: TRect;
var
  aBW: Integer;
begin
  SetRectEmpty(Result);
  if FShowSnapButton  and SnapBottomRight then
  begin
    if (BevelInner = bvNone) and (BevelOuter = bvNone) then
      aBW := 0
    else
    if (BevelInner = bvNone) xor (BevelOuter = bvNone) then
      aBW := 2 * BevelWidth
    else
      aBW := 4 * BevelWidth;

    if Width > Height then
    begin
      if (Width shr 1 >= aBW) and (Height - aBW > 1) then
      begin
        if SnapTopLeft then
        begin
          if InvertSnapButtons then
            Result := Bounds(Width shr 2, aBW shr 1, Width shr 2, Height - aBW)
          else
            Result := Bounds(Width shr 1, aBW shr 1, Width shr 2, Height - aBW)
        end
        else
          Result := Bounds(Width shr 2, aBW shr 1, Width shr 1, Height - aBW);
      end;
    end
    else
    begin
      if (Height shr 1 >= aBW) and (Width - aBW > 1) then
      begin
        if SnapTopLeft then
        begin
          if InvertSnapButtons then
            Result := Bounds(aBW shr 1, Height shr 2, Width - aBW, Height shr 2)
          else
            Result := Bounds(aBW shr 1, Height shr 1, Width - aBW, Height shr 2);
        end
        else
          Result := Bounds(aBW shr 1, Height shr 2, Width - aBW, Height shr 1);
      end;
    end;
  end;
end;

procedure TElSplitter.SetInvertSnapButtons(Value: Boolean);
begin
  if FInvertSnapButtons <> Value then
  begin
    FInvertSnapButtons := Value;
    Invalidate;
  end;
end;


end.

