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

unit ElMaskEdit;

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  StdCtrls,
  Messages,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  {$else}
  Qt,
  QForms,
  Types,
  QGraphics,
  QControls,
  QStdCtrls,
  QExtCtrls,
  {$endif}
{$ifdef VCL_6_USED}
Types,
{$endif}

  ElUxTheme,
  ElTmSchema,
  ElXPThemedControl,
  SysUtils,
  Classes,
  Clipbrd,
  ElVCLUtils,
  ElImgFrm,
  Mask;

type
     TCustomElMaskEdit = class(TCustomMaskEdit)
     private
       FActiveBorderType: TElFlatBorderType;
       FAlignment: TAlignment;
       FBackground: TBitmap;
       FBorderSides: TElBorderSides;
       FFlat: Boolean;
       FHandleDialogKeys: Boolean;
       FImgForm: TElImageForm;
       FImgFormChLink: TImgFormChangeLink;
       FInactiveBorderType: TElFlatBorderType;
       FMouseOver: Boolean;
       FOnMouseEnter: TNotifyEvent;
       FOnMouseLeave: TNotifyEvent;
       FPainting: Boolean;
       FPaintingTo: Boolean;
       FReturnPressed: Boolean;
       FTransparent: Boolean;
       FUseBackground: Boolean;
       FTheme: HTheme;
       {$ifdef ELPACK_UNICODE}
       FHint: WideString;
       {$endif}
       
       procedure BackgroundChanged(Sender: TObject);
       {$ifdef MSWINDOWS}
       procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
       procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
       procedure CMParentColorChanged(var Msg: TMessage); message
           CM_PARENTCOLORCHANGED;
       procedure CNCtlColorEdit(var Msg: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
       procedure CNCtlColorStatic(var Msg: TWMCtlColorStatic); message
           CN_CTLCOLORSTATIC;
       procedure DrawBackground(DC: HDC; R: TRect);
       procedure DrawFlatBorder(DC: HDC);
       procedure DrawParentControl(DC: HDC);
       procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
       procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
       procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
       procedure WMMove(var Msg: TMessage); message WM_MOVE;
       procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
       procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
       procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
       {$endif}

       procedure ImageFormChange(Sender : TObject);
       procedure SetActiveBorderType(const Value: TElFlatBorderType);
       procedure SetAlignment(const Value: TAlignment);
       procedure SetBackground(const Value: TBitmap);
       procedure SetBorderSides(Value: TElBorderSides);
       procedure SetInactiveBorderType(const Value: TElFlatBorderType);
       procedure SetTransparent(const Value: boolean);
       procedure SetUseBackground(const Value: boolean);
     protected
       FNoHandleEnter: Boolean;
       FUseXPThemes: Boolean;
       FLineBorderActiveColor: TColor;
       FLineBorderInactiveColor: TColor;

       {$ifdef MSWINDOWS}
       procedure Change; override;
       procedure CreateParams(var Params: TCreateParams); override;
       {$endif}
       procedure DoMouseEnter; dynamic;
       procedure DoMouseLeave; dynamic;
       {$ifdef MSWINDOWS}
       procedure DoPaint; dynamic;
       {$endif}
       procedure KeyDown(var Key: Word; Shift: TShiftState); override;
       procedure KeyPress(var Key: Char); override;
       {$ifndef MSWINDOWS}
       function NeedKey(Key: Integer; Shift: TShiftState; const KeyText: WideString):
           Boolean; override;
       {$endif}
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure SetFlat(const Value: boolean); virtual;
       procedure SetImageForm(newValue : TElImageForm); virtual;
       procedure IFMRepaintChildren(var Message: TMessage); message
           IFM_REPAINTCHILDREN;
       procedure CreateThemeHandle; virtual;
       procedure CreateWnd; override;
       procedure DestroyWnd; override;
       procedure FreeThemeHandle; virtual;
       function IsThemeApplied: Boolean;
       procedure SetUseXPThemes(Value: Boolean); virtual;
       procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
       procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
       function GetThemedClassName: WideString;
       procedure SetLineBorderActiveColor(Value: TColor);
       procedure SetLineBorderInactiveColor(Value: TColor);
       procedure WMPaste(var Message: TMessage); message WM_PASTE;

      {$ifdef ELPACK_UNICODE}
      procedure SetHint(Value: WideString);

      {$ifndef CLX_USED}
      procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
      {$else}
      function HintShow(var HintInfo : THintInfo): Boolean; override;
      {$endif}
      {$endif}

       property ActiveBorderType: TElFlatBorderType read FActiveBorderType write
           SetActiveBorderType default fbtSunken;
       property Alignment: TAlignment read FAlignment write SetAlignment default
           taLeftJustify;
       property Background: TBitmap read FBackground write SetBackground;
       property Flat: Boolean read FFlat write SetFlat default False;
       property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write
           SetInactiveBorderType default fbtSunkenOuter;
       property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
       property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
       property Transparent: Boolean read FTransparent write SetTransparent default
           False;
       property UseBackground: Boolean read FUseBackground write SetUseBackground
           default False;
       property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
       property HandleDialogKeys: Boolean read FHandleDialogKeys write
           FHandleDialogKeys default true;
       property ImageForm: TElImageForm read FImgForm write SetImageForm;
       property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
           true;
       property LineBorderActiveColor: TColor read FLineBorderActiveColor write
           SetLineBorderActiveColor;
       property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
           SetLineBorderInactiveColor;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       property MouseOver: Boolean read FMouseOver default false;
       property Theme: HTheme read FTheme;
     published
      {$ifdef ELPACK_UNICODE}
      property Hint: WideString read FHint write SetHint;
      {$endif}
     end;

     TElMaskEdit = class(TCustomElMaskEdit)
     published
       property ActiveBorderType;
       property Alignment;
       property Background;
       property Flat;
       property InactiveBorderType;
       property OnMouseEnter;
       property OnMouseLeave;
       property Transparent;
       property UseBackground;
       property BorderSides;
       property HandleDialogKeys;
       property ImageForm;
       property UseXPThemes;
       property LineBorderActiveColor;
       property LineBorderInactiveColor;

       property Align;
       {$IFDEF VCL_4_USED}
       property Anchors;
       {$ENDIF}
       property AutoSelect;
       property AutoSize;
       {$IFDEF VCL_4_USED}
       {$ifdef MSWINDOWS}
       property BiDiMode;
       {$endif}
       {$ENDIF}
       property BorderStyle;
       property CharCase;
       property Color;
       {$IFDEF VCL_4_USED}
       property Constraints;
       {$ENDIF}
       property Ctl3D;
       property Cursor;
       {$ifdef MSWINDOWS}
       property DragCursor;
       {$endif}
       {$IFDEF VCL_4_USED}
       {$ifdef MSWINDOWS}
       property DragKind;
       {$endif}
       {$ENDIF}
       property DragMode;
       property Enabled;
       property EditMask;
       property Font;
       property HideSelection;
       {$ifdef MSWINDOWS}
       property ImeMode;
       property ImeName;
       {$endif}
       property MaxLength;
       {$ifdef MSWINDOWS}
       property OEMConvert;
       {$endif}
       {$IFDEF VCL_4_USED}
       {$ifdef MSWINDOWS}
       property ParentBiDiMode;
       {$endif}
       {$ENDIF}
       property ParentColor;
       property ParentCtl3D;
       property ParentFont;
       property ParentShowHint;
       {$ifdef MSWINDOWS}
       property PasswordChar;
       {$endif}
       property PopupMenu;
       property ReadOnly;
       property ShowHint;
       property TabOrder;
       property Text;

       property Visible;
       property OnChange;
       property OnClick;
       {$IFDEF VCL_5_USED}
       property OnContextPopup;
       {$ENDIF}
       property OnDblClick;
       property OnDragDrop;
       property OnDragOver;
       {$IFDEF VCL_4_USED}
       {$ifdef MSWINDOWS}
       property OnEndDock;
       {$endif}
       {$ENDIF}
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
       {$ifdef MSWINDOWS}
       property OnStartDock;
       {$endif}
       {$ENDIF}
       property OnStartDrag;
     end;

implementation

type
  THackWinControl = class(TWinControl);

constructor TCustomElMaskEdit.Create(AOwner: TComponent);
begin
  inherited;
  FActiveBorderType := fbtSunken;
  FAlignment := taLeftJustify;
  FHandleDialogKeys := true;
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChanged;
  FFlat := False;
  FInactiveBorderType := fbtSunkenOuter;
  FMouseOver := False;
  FPainting := False;
  FPaintingTo := False;
  FTransparent := False;
  FUseBackground := False;
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$ifdef MSWINDOWS}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}
  {$ifndef MSWINDOWS}
  InputKeys := [ikChars, ikNav];
  {$endif}
  UseXPThemes := true;
end;

destructor TCustomElMaskEdit.Destroy;
begin
  ImageForm := nil;
  FImgFormChLink.Free;
  FBackground.Free;
  inherited;
end;

procedure TCustomElMaskEdit.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
  {$ifdef MSWINDOWS}
  Perform(CM_COLORCHANGED, 0, 0);
  {$else}
  ColorChanged;
  {$endif} 
end;

procedure TCustomElMaskEdit.Change;
begin
  DoPaint;
  inherited;
end;

procedure TCustomElMaskEdit.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if ThemesAvailable or
     (Flat and (not Focused)) then DrawFlatBorder(0);
  DoMouseEnter;
end;

procedure TCustomElMaskEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if ThemesAvailable or (Flat and (not Focused)) then
    DrawFlatBorder(0);
  DoMouseLeave;
end;

procedure TCustomElMaskEdit.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty) or
     ((FImgForm <> nil) and (not (csDesigning in Componentstate))) then
    Invalidate;
end;

procedure TCustomElMaskEdit.CNCtlColorEdit(var Msg: TWMCtlColorEdit);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty) or
     ((FImgForm <> nil) and (not (csDesigning in Componentstate))) then
    SetBkMode(Msg.ChildDC, Windows.TRANSPARENT);
end;

procedure TCustomElMaskEdit.CNCtlColorStatic(var Msg: TWMCtlColorStatic);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty)  or
     ((FImgForm <> nil) and (not (csDesigning in Componentstate))) then
    SetBkMode(Msg.ChildDC, Windows.TRANSPARENT);
end;

procedure TCustomElMaskEdit.CreateParams(var Params: TCreateParams);
const
   Alignments: array [TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited;
  Params.Style := Params.Style or ES_MULTILINE or Alignments[FAlignment];
end;

procedure TCustomElMaskEdit.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomElMaskEdit.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TCustomElMaskEdit.DoPaint;
const
  BorderOffsets: array [TBorderStyle] of integer = (1, -1);
var
  CtlDC, TempDC: HDC;
  OldBmp, TempBmp: HBITMAP;
begin
  FPainting := True;
  try
    if FTransparent or (FUseBackground and not FBackground.Empty) or
       ((FImgForm <> nil) and (not (csDesigning in Componentstate))) then
    begin
      HideCaret(Handle);
      CtlDC := GetDC(Handle);
      try
        TempDC := CreateCompatibleDC(CtlDC);
        try
          TempBmp := CreateCompatibleBitmap(CtlDC, ClientWidth +1, ClientHeight +1);
          try
            OldBmp := SelectObject(TempDC, TempBmp);
            FPaintingTo := True;
            try
              PaintTo(TempDC, 0, 0);
            finally
              FPainting := False;
            end;
            if FFlat  then
               DrawFlatBorder(TempDC);
            BitBlt(CtlDC, BorderOffsets[BorderStyle], BorderOffsets[BorderStyle], ClientWidth, ClientHeight, TempDC, 1, 1, SRCCOPY);
            SelectObject(TempDC, OldBmp);
          finally
            DeleteObject(TempBmp);
          end;
        finally
          DeleteDC(TempDC);
        end;
      finally
        ReleaseDC(Handle, CtlDC);
      end;
      ShowCaret(Handle);
    end;
  finally
    FPainting := False;
  end;
end;

procedure TCustomElMaskEdit.DrawBackground(DC: HDC; R: TRect);
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

procedure TCustomElMaskEdit.DrawFlatBorder(DC: HDC);
var
  BorderType: TElFlatBorderType;
  MustRelease: boolean;
  R: TRect;
  AColor : TColor;
begin
  if (not FFlat) or (BorderStyle = bsNone) or (not HandleAllocated) then exit;
  if IsThemeApplied then
  begin
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_UPDATENOW);
    exit;
  end;
  
  MustRelease := (DC = 0);
  if MustRelease then DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);

    if Focused or FMouseOver then
      BorderType := FActiveBorderType
    else
      BorderType := FInactiveBorderType;

    if Focused or FMouseOver then
      AColor := LineBorderActiveColor
    else
      AColor := LineBorderInactiveColor;

    DrawFlatFrameEx2(DC, R, AColor, Color, Focused or FMouseOver, Enabled, FBorderSides, BorderType);
  finally
    if MustRelease then ReleaseDC(Handle, DC);
  end;
end;

procedure TCustomElMaskEdit.DrawParentControl(DC: HDC);
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

procedure TCustomElMaskEdit.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TCustomElMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  {$ifdef MSWINDOWS}
  FReturnPressed := ((Key = VK_RETURN) and (Shift = [ssCtrl])) or (Key = VK_UP)
                    or (Key = VK_DOWN) or (Key = VK_PRIOR) or (Key = VK_NEXT);
  {$else}
  FReturnPressed := (((Key = KEY_RETURN) or (Key = KEY_RETURN)) and (Shift = [ssCtrl]))
                    or (Key = Key_UP) or (Key = Key_DOWN)
                    or (Key = Key_PRIOR) or (Key = Key_NEXT);

  {$endif}
end;

procedure TCustomElMaskEdit.KeyPress(var Key: Char);
begin
  inherited;
  {$ifdef MSWINDOWS}
  if ((Key = Char(VK_RETURN)) or FReturnPressed) and (not FNoHandleEnter) then
  {$else}
  if (((Key = Char(Key_Return)) or ((Key = Char(Key_Enter)))) or FReturnPressed) and (not FNoHandleEnter) then
  {$endif}
  begin
    Key := #0;
    {$ifdef MSWINDOWS}
    MessageBeep(0);
    {$endif}
  end;  
end;

{$ifndef MSWINDOWS}
function TCustomElMaskEdit.NeedKey(Key: Integer; Shift: TShiftState; const KeyText:
    WideString): Boolean;
begin
  result := inherited NeedKey(Key, Shift, KeyText);
  if (Key = Key_Escape) or (Key = Key_Enter) or (Key = Key_Return) then
    result := HandleDialogKeys;
end;
{$endif}

procedure TCustomElMaskEdit.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FImgForm then
    begin
      FImgForm := nil;
      Invalidate;
    end;
  end;
end;

procedure TCustomElMaskEdit.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    {$ifdef MSWINDOWS}
    if Focused or FMouseOver then DrawFlatBorder(0);
    {$endif}
  end;
end;

procedure TCustomElMaskEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    {$ifdef MSWINDOWS}
    RecreateWnd;
    {$endif}
  end;  
end;

procedure TCustomElMaskEdit.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
end;

procedure TCustomElMaskEdit.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TCustomElMaskEdit.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    {$ifdef MSWINDOWS}
    if HandleAllocated then
      if Flat then
        Invalidate
      else
        RecreateWnd;
    {$endif}
  end;
end;

procedure TCustomElMaskEdit.SetImageForm(newValue : TElImageForm);
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
    {$ifdef MSWINDOWS}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$else}
    RecreateWidget;
    colorChanged;
    {$endif}
  end;
end;

procedure TCustomElMaskEdit.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    {$ifdef MSWINDOWS}
    if not Focused and not FMouseOver then DrawFlatBorder(0);
    {$endif}
  end;  
end;

procedure TCustomElMaskEdit.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    {$ifdef MSWINDOWS}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;  
end;

procedure TCustomElMaskEdit.SetUseBackground(const Value: boolean);
begin
  if FUseBackground <> Value then
  begin
    FUseBackground := Value;
    {$ifdef MSWINDOWS}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

procedure TCustomElMaskEdit.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  R1, BgRect : TRect;
  ACtl       : TWinControl;
begin
  if (FImgForm <> nil) and (not (csDesigning in FImgForm.Componentstate)) then
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
    end
  end
  else
  if FTransparent then
    DrawParentControl(Msg.DC)
  else
  if FUseBackground and not FBackground.Empty then
    DrawBackground(Msg.DC, ClientRect)
  else inherited;
end;

procedure TCustomElMaskEdit.WMGetDlgCode(var Msg : TMessage);
begin
  Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.wParam, Msg.lParam);
  Msg.Result := (Msg.Result and (not DLGC_WANTALLKEYS)) or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if HandleDialogKeys then Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;

procedure TCustomElMaskEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  if not HandleAllocated then exit;
  if Flat and not FMouseOver then DrawFlatBorder(0);
end;

procedure TCustomElMaskEdit.WMMove(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomElMaskEdit.WMNCCalcSize(var Message : TWMNCCalcSize);
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

procedure TCustomElMaskEdit.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty) or
     ((FImgForm <> nil) and (not (csDesigning in Componentstate)))
  then
    if not FPainting and not FPaintingTo then DoPaint;
  if Flat then
    DrawFlatBorder(0);
end;

procedure TCustomElMaskEdit.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  if Flat and not FMouseOver then DrawFlatBorder(0);
end;

procedure TCustomElMaskEdit.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;

procedure TCustomElMaskEdit.CreateThemeHandle;
begin
  if ThemesAvailable then
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

{$ifndef CLX_USED}
procedure TCustomElMaskEdit.CreateWnd;
{$else}
procedure TCustomElMaskEdit.CreateWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and not IsThemeApplied then
  begin
    CreateThemeHandle;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElMaskEdit.DestroyWnd;
{$else}
procedure TCustomElMaskEdit.DestroyWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and IsThemeApplied then
  begin
    FreeThemeHandle;
  end;
end;

procedure TCustomElMaskEdit.FreeThemeHandle;
begin
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
    CloseThemeData(FTheme);
  {$endif}
  FTheme := 0;
end;

function TCustomElMaskEdit.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TCustomElMaskEdit.SetUseXPThemes(Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    RecreateWnd;
  end;
end;

procedure TCustomElMaskEdit.WMNCPaint(var Msg : TMessage);
var DC : HDC;
    RW,
    RC : TRect;
begin
  if IsThemeApplied and (BorderStyle = bsSingle) then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC = 0 then
    begin
      DC := GetWindowDC(Handle);
    end;

    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    DrawThemeBackground(FTheme, DC, 0, 0, RW, nil);
    ReleaseDC(Handle, DC);
  end
  else
  if not Flat and (BorderStyle = bsSingle) then
    inherited;
end;

procedure TCustomElMaskEdit.WMThemeChanged(var Message: TMessage);
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

function TCustomElMaskEdit.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

procedure TCustomElMaskEdit.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then
      if HandleAllocated then
        Invalidate;
  end;
end;

procedure TCustomElMaskEdit.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then
      if HandleAllocated then
        Invalidate;
  end;
end;

procedure TCustomElMaskEdit.WMPaste(var Message: TMessage);
var Clip  : TClipboard;
    Piece : String;
begin
  if ReadOnly then exit;
  Clip := Clipboard;
  Clip.Open;
  Piece := Clip.AsText;
  while (Pos(#10, Piece) > 0) do
    Delete(Piece, Pos(#10, Piece), 1);
  while (Pos(#13, Piece) > 0) do
    Delete(Piece, Pos(#13, Piece), 1);
  Clip.AsText := Piece;
  Clip.Close;
  inherited;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TCustomElMaskEdit.CMHintShow(var Message: TMessage);
{$else}
function TCustomElMaskEdit.HintShow(var HintInfo : THintInfo): Boolean; 
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

procedure TCustomElMaskEdit.SetHint(Value: WideString);
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
