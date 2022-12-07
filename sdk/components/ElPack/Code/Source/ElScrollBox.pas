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

  Fixed the problem with the border in XP with themes disabled

03/10/2002

  Removed terrible flicker that happened on repainting

*)

unit ElScrollBox;

interface

uses ElVCLUtils,
     ElUxTheme,
     ElTmSchema,
     ElImgFrm,

     Classes,
     StdCtrls,

     Windows,
     Controls,
     Messages,
     Graphics,
{$ifdef VCL_6_USED}
Types,
{$endif}
     Forms;

type

    TElScrollBox = class(TScrollBox)
    private
      FActiveBorderType: TElFlatBorderType;
      FBackground: TBitmap;
      FBorderSides: TElBorderSides;
      FFlat: Boolean;
      FFlatFocusedScrollBars: Boolean;
      FInactiveBorderType: TElFlatBorderType;
      FTheme: HTheme;
      FUseBackground: Boolean;
      FImgForm: TElImageForm;
      FImgFormChLink: TImgFormChangeLink;
      FMouseOver: Boolean;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseLeave: TNotifyEvent;
      FPainting: Boolean;
      FPaintingTo: Boolean;
      FTransparent: Boolean;
      procedure SetActiveBorderType(const Value: TElFlatBorderType);
      procedure SetBackground(const Value: TBitmap);
      procedure SetBorderSides(Value: TElBorderSides);
      procedure SetFlat(const Value: boolean);
      procedure SetFlatFocusedScrollBars(const Value: boolean);
      procedure SetImageForm(newValue : TElImageForm);
      procedure SetInactiveBorderType(const Value: TElFlatBorderType);
      procedure SetUseBackground(const Value: boolean);
      procedure BackgroundChanged(Sender: TObject);
      procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      procedure DrawBackground(DC: HDC; R: TRect);
      procedure DrawFlatBorder(DC: HDC);
      procedure DrawParentControl(DC: HDC);
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
      procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
      procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
      procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
      procedure ImageFormChange(Sender : TObject);
      procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
          WM_WINDOWPOSCHANGED;
    protected
      FLineBorderActiveColor: TColor;
      FLineBorderInactiveColor: TColor;
      FUseXPThemes: Boolean;
      {$ifdef ELPACK_UNICODE}
      FHint: WideString;
      {$endif}

      procedure SetLineBorderActiveColor(Value: TColor);
      procedure SetLineBorderInactiveColor(Value: TColor);
      procedure SetUseXPThemes(Value: Boolean);
      procedure CreateThemeHandle; virtual;
      procedure CreateWnd; override;
      procedure DestroyWnd; override;
      procedure FreeThemeHandle; virtual;
      function GetThemedClassName: WideString; virtual;
      function IsThemeApplied: Boolean;
      procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
      procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
      procedure DoMouseEnter; dynamic;
      procedure DoMouseLeave; dynamic;
      procedure DoPaint(DC : HDC); dynamic;
      procedure IFMRepaintChildren(var Message: TMessage); message
          IFM_REPAINTCHILDREN;
      procedure CreateParams(var Params: TCreateParams); override;

      {$ifdef ELPACK_UNICODE}
      {$ifndef CLX_USED}
      procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
      {$else}
      function HintShow(var HintInfo : THintInfo): Boolean; override;
      {$endif}
      {$endif}
      procedure DrawThemedBackground(DC : HDC); virtual;
      {$ifdef ELPACK_UNICODE}
      procedure SetHint(Value: WideString);
      {$endif}

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property Theme: HTheme read FTheme;
    published
      property ActiveBorderType: TElFlatBorderType read FActiveBorderType write 
          SetActiveBorderType default fbtSunken;
      property Background: TBitmap read FBackground write SetBackground;
      property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
      property Flat: Boolean read FFlat write SetFlat default False;
      property FlatFocusedScrollBars: Boolean read FFlatFocusedScrollBars write
          SetFlatFocusedScrollBars default False;
      property ImageForm: TElImageForm read FImgForm write SetImageForm;
      property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write
          SetInactiveBorderType default fbtSunkenOuter;
      property LineBorderActiveColor: TColor read FLineBorderActiveColor write 
          SetLineBorderActiveColor;
      property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write 
          SetLineBorderInactiveColor;
      property UseBackground: Boolean read FUseBackground write SetUseBackground
          default False;
      property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
          true;
      property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
      property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
      {$ifdef ELPACK_UNICODE}
      property Hint: WideString read FHint write SetHint;
      {$endif}
    end;

implementation

{$ifdef VCL_5_USED}
uses FlatSB;
{$endif}

type

  THackWinControl = class(TWinControl);

procedure TElScrollBox.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    {$ifndef CLX_USED}
    if Focused or FMouseOver then DrawFlatBorder(0);
    {$endif}
  end;
end;

procedure TElScrollBox.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
end;

procedure TElScrollBox.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TElScrollBox.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    {$ifndef CLX_USED}
    if HandleAllocated then
      Invalidate;
    {$endif}
  end;
end;

procedure TElScrollBox.SetFlatFocusedScrollBars(const Value: boolean);
begin
  if FFlatFocusedScrollBars <> Value then
  begin
    FFlatFocusedScrollBars := Value;
    {$ifndef CLX_USED}
    if Focused then DrawFlatBorder(0);
    {$endif}
  end;  
end;

procedure TElScrollBox.SetImageForm(newValue : TElImageForm);
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
    if newValue <> nil then
       newValue.FreeNotification(Self);
    FImgForm := newValue;
    if FImgForm <> nil then FImgForm.RegisterChanges(FImgFormChLink);
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$else}
    ColorChanged;
    {$endif}
  end;
end;

procedure TElScrollBox.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    {$ifndef CLX_USED}
    if not Focused and not FMouseOver then DrawFlatBorder(0);
    {$endif}
  end;
end;

procedure TElScrollBox.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then
      if HandleAllocated then
        Invalidate;
  end;
end;

procedure TElScrollBox.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then
      if HandleAllocated then
        Invalidate;
  end;
end;

procedure TElScrollBox.SetUseBackground(const Value: boolean);
begin
  if FUseBackground <> Value then
  begin
    FUseBackground := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

procedure TElScrollBox.SetUseXPThemes(Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    {$ifdef MSWINDOWS}
    {$ifndef CLX_USED}
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$else}
    RedrawWindow(QWidget_winID(Handle), nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$endif}
    {$endif}
  end;
end;

procedure TElScrollBox.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
  {$ifndef CLX_USED}
  Perform(CM_COLORCHANGED, 0, 0);
  {$else}
  ColorChanged;
  {$endif}
end;

procedure TElScrollBox.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if IsThemeApplied or ((Flat and (FInactiveBorderType <> FActiveBorderType)) and not Focused) then DrawFlatBorder(0);
  DoMouseEnter;
end;

procedure TElScrollBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if IsThemeApplied or ((Flat and (FInactiveBorderType <> FActiveBorderType)) and not Focused) then DrawFlatBorder(0);
  DoMouseLeave;
end;

procedure TElScrollBox.CreateThemeHandle;
begin
  if (ThemesAvailable and IsThemeActive) then
    {$ifndef CLX_USED}
    FTheme := OpenThemeData(Handle, PWideChar(GetThemedClassName()))
    {$else}
    {$ifdef MSWINDOWS}
    FTheme := OpenThemeData(QWidget_winID(Handle), PWideChar(GetThemedClassName()))
    {$endif}
    {$endif}
  else
    FTheme := 0;
end;

procedure TElScrollBox.CreateWnd;
begin
  inherited;
  if UseXPThemes and not IsThemeApplied then
  begin
    {$ifdef VCL_5_USED}
    UninitializeFlatSB(Handle);
    {$endif}
    CreateThemeHandle;
  end;
end;

procedure TElScrollBox.DestroyWnd;
begin
  inherited;
  if UseXPThemes and IsThemeApplied then
  begin
    FreeThemeHandle;
  end;
end;

procedure TElScrollBox.DrawBackground(DC: HDC; R: TRect);
var
  X, Y: integer;
begin
  if FUseBackground and not FBackground.Empty then
  begin
    X := R.Left; Y := R.Top;
    while Y < R.Bottom do
    begin
      while X < R.Right do
      begin
        BitBlt(DC, X, Y, R.Right - X, R.Bottom - Y,
          FBackground.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(X, FBackground.Width);
      end;
      X := R.Left;
      Inc(Y, FBackground.Height);
    end;
  end;
end;

procedure TElScrollBox.DrawFlatBorder(DC: HDC);
var
  R : TRect;
  BS: TElFlatBorderType;
  MustRelease: boolean;
  AColor : TColor;
  ARgn,
  CRgn   : HRGN;

const BordersFlat : array[boolean] of Integer = (0, WS_BORDER);
      Borders3D : array[boolean] of Integer = (0, WS_EX_CLIENTEDGE);
      
begin
  if not HandleAllocated then exit;
  R := Rect(0, 0, Width, Height);
  if IsThemeApplied then
  begin
    if BorderStyle = bsSingle then
    begin
      ARgn := CreateRectRgnIndirect(R);
      R := ClientRect;
      CRgn := CreateRectRgnIndirect(R);
      CombineRgn(ARgn, ARgn, CRgn, RGN_DIFF);
      RedrawWindow(Handle, nil, ARgn, RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);
      DeleteObject(ARgn);
      DeleteObject(CRgn);
    end;
    exit;
  end;
  MustRelease := (DC = 0);
  if MustRelease then
    DC := GetWindowDC(Handle);
  try
    if (BorderStyle = bsSingle) then
    begin
      if Focused or FMouseOver then
        BS := FActiveBorderType
      else
        BS := FInactiveBorderType;
      if Focused or FMouseOver then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;
      DrawFlatFrameEx2(DC, R, AColor, Color, Focused or FMouseOver, Enabled, FBorderSides, BS);
    end;
    if FFlatFocusedScrollBars or not (Focused or FMouseOver) then
      DrawFlatScrollbars(Handle, DC, R,
        (Focused or FMouseOver) and not FFlatFocusedScrollBars,
        ssBoth, False, False, False,
        GetWindowLong(Handle, GWL_STYLE) or BordersFlat[(not Ctl3D) and (BorderStyle = bsSingle)],
        GetWindowLong(Handle, GWL_EXSTYLE) or Borders3D[Ctl3D and (BorderStyle = bsSingle)]);
  finally
    if MustRelease then ReleaseDC(Handle, DC);
  end;
end;

procedure TElScrollBox.DrawParentControl(DC: HDC);
var
  SavedDC: integer;
  P: TPoint;
begin
  if Assigned(Parent) then
  begin
    SavedDC := SaveDC(DC);
    try
      P := Parent.ScreenToClient(ClientOrigin);
      MoveWindowOrg(DC, -P.X, -P.Y);
      Parent.Perform(WM_ERASEBKGND, DC, 0);
      Parent.Perform(WM_PAINT, DC, 0);
      THackWinControl(Parent).PaintControls(DC, nil);
    finally
      RestoreDC(DC, SavedDC);
    end;
  end;
end;

procedure TElScrollBox.FreeThemeHandle;
begin
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
    CloseThemeData(FTheme);
  {$endif}
  FTheme := 0;
end;

function TElScrollBox.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

function TElScrollBox.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TElScrollBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  R1,
  BgRect : TRect;
  ACtl   : TWinControl;
begin
  if (FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in FImgForm.GetRealControl.Componentstate)) then
  begin
    if (FImgForm.Control <> Self) then
    begin
      ACtl := FImgForm.GetRealControl;
      R1 := ClientRect;
      BgRect := ClientRect;
      BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
      BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
      BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
      BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);

      FImgForm.PaintBkgnd(Msg.DC, R1, Point(BgRect.Left, BgRect.Top), false);
    end;
  end
  else
  if FTransparent then
    DrawParentControl(Msg.DC)
  else
  if FUseBackground and not FBackground.Empty then
    DrawBackground(Msg.DC, ClientRect)
  else
  begin
    //if not IsthemeApplied then
    inherited;
    //Msg.Result := 1;
  end;
end;

procedure TElScrollBox.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  if not HandleAllocated then exit;
  if (Flat and (FInactiveBorderType <> FActiveBorderType))  and not FMouseOver then DrawFlatBorder(0);
end;

procedure TElScrollBox.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    if (ebsLeft in BorderSides) then
      inc(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if (ebsTop in BorderSides) then
      inc(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if (ebsRight in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if (ebsBottom in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end
  else
  if (BorderStyle = bsSingle) then
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

procedure TElScrollBox.WMNCPaint(var Msg : TMessage);
var DC : HDC;
    RW,
    RC : TRect;

const BordersFlat : array[boolean] of Integer = (0, WS_BORDER);
      Borders3D : array[boolean] of Integer = (0, WS_EX_CLIENTEDGE);
      ScrollBars : array[boolean, boolean] of Integer = ((0, WS_VSCROLL), (WS_HSCROLL, WS_HSCROLL or WS_VSCROLL));

begin
  if IsThemeApplied then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC = 0 then
    begin
      DC := GetWindowDC(Handle);
    end;
    DrawFlatScrollbars(Handle, DC, Rect(0, 0, Width, Height),
      false,
      ssNone, False, False, False,
      GetWindowLong(Handle, GWL_STYLE) or BordersFlat[(not Ctl3D) and (BorderStyle = bsSingle)] or ScrollBars[HorzScrollbar.Visible, VertScrollBar.Visible],
      GetWindowLong(Handle, GWL_EXSTYLE) or Borders3D[Ctl3D and (BorderStyle = bsSingle)]);

    if BorderStyle = bsSingle then
    begin
      Windows.GetClientRect(Handle, RC);
      if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
        inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
      if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
        inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
      GetWindowRect(Handle, RW);
      MapWindowPoints(0, Handle, RW, 2);
      OffsetRect(RC, -RW.Left, -RW.Top);
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
      OffsetRect(RW, -RW.Left, -RW.Top);

      DrawThemeBackgroundTo('EDIT', DC, 0, 0, RW, nil);
    end;

    ReleaseDC(Handle, DC);
  end
  else
    inherited;
end;

procedure TElScrollBox.WMPaint(var Msg: TWMPaint);
//var PS : TPaintStruct;
begin
  if Msg.DC = 0 then
  begin
    inherited;
    if FFlat and (not IsThemeApplied) then
    DrawFlatBorder(0);
  end
  else
  if not FPainting and not FPaintingTo then
  begin
    //BeginPaint(Handle, PS);
    // inherited;
    (*if FTransparent or (FUseBackground and not FBackground.Empty) or
       ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
    *)
    DoPaint(Msg.DC);
    //EndPaint(Handle, PS);
  end
  else
    inherited;
end;

procedure TElScrollBox.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  if (Flat and (FInactiveBorderType <> FActiveBorderType)) and (not FMouseOver) and (not IsThemeApplied) then DrawFlatBorder(0);
end;

procedure TElScrollBox.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    FreeThemeHandle;
    CreateThemeHandle;
    SetWindowPos(
      Handle,
      0,
      0, 0, 0, 0,
      SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER
      );
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end;
  Message.Result := 1;
end;

constructor TElScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csOpaque];
  FActiveBorderType := fbtSunken;
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChanged;
  {$ifndef CLX_USED}
  FBorderSides := AllBorderSides;
  {$endif}
  FFlat := False;
  FFlatFocusedScrollBars := False;
  FInactiveBorderType := fbtSunkenOuter;
  FMouseOver := False;
  FPainting := False;
  FPaintingTo := False;
  FTransparent := False;
  FUseBackground := False;
  {$ifndef CLX_USED}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  FUseXPThemes := true;
end;

destructor TElScrollBox.Destroy;
begin
  {$ifndef CLX_USED}
  ImageForm := nil;
  FImgFormChLink.Free;
  {$endif}
  FBackground.Free;
  inherited;
end;

procedure TElScrollBox.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TElScrollBox.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TElScrollBox.DoPaint(DC : HDC);
const
  BorderOffsets: array [TBorderStyle] of integer = (1, -1);
var
  CtlDC, TempDC: HDC;
  OldBmp, TempBmp: HBITMAP;
begin
  FPainting := True;
  if FTransparent or (FUseBackground and not FBackground.Empty)   or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
  begin
    CtlDC := DC;//GetDC(Handle);
    try
      TempDC := CreateCompatibleDC(CtlDC);
      try
        TempBmp := CreateCompatibleBitmap(CtlDC, ClientWidth + 1, ClientHeight + 1);
        try
          OldBmp := SelectObject(TempDC, TempBmp);
          FPaintingTo := True;
          try
            PaintTo(TempDC, 0, 0);
          finally
            FPaintingTo := False;
          end;
          if IsThemeApplied or FFlat then DrawFlatBorder(TempDC);
          BitBlt(CtlDC, BorderOffsets[BorderStyle], BorderOffsets[BorderStyle], ClientWidth, ClientHeight, TempDC, 1, 1, SRCCOPY);
          SelectObject(TempDC, OldBmp);
        finally
          DeleteObject(TempBmp);
        end;
      finally
        DeleteDC(TempDC);
      end;
    finally
      // ReleaseDC(Handle, CtlDC);
    end;
  end
  else
  begin
    DrawThemedBackground(DC);
    // CtlDC := GetDC(Handle);
    //ReleaseDC(Handle, CtlDC);
  end;
  FPainting := False;
end;

procedure TElScrollBox.IFMRepaintChildren(var Message: TMessage);
var i : integer;
begin
  inherited;
  Invalidate;
  for i := 0 to ControlCount -1 do
  begin
    if Controls[i] is TWinControl then
      PostMessage(TWinControl(Controls[i]).Handle, Message.Msg, Message.wParam, Message.lParam);
  end;
end;

procedure TElScrollBox.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TElScrollBox.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if Assigned(ImageForm) then
    Perform(IFM_REPAINTCHILDREN, 0, 0);
end;

procedure TElScrollBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPCHILDREN;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    Params.Style := Params.Style and (not WS_BORDER);
    Params.ExStyle := Params.ExStyle and (not WS_EX_CLIENTEDGE);
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElScrollBox.CMHintShow(var Message: TMessage);
{$else}
function TElScrollBox.HintShow(var HintInfo : THintInfo): Boolean; 
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

{$endif}

procedure TElScrollBox.DrawThemedBackground(DC : HDC);
var
  hBr : HBRUSH;
  R : TRect;
begin
  GetClipBox(DC, R);
  if IsRectEmpty(R) then
    R := ClientRect;
  hBr := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, R, hBr);
  DeleteObject(hBr);
end;

{$ifdef ELPACK_UNICODE}
procedure TElScrollBox.SetHint(Value: WideString);
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

