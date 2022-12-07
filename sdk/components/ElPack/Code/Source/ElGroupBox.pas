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

03/11/2002

  In HTML mode font name is set correctly now
  Flicker reduced with XP themes enabled

11/28/2001

  Added MoneyFlat and accompanying properties

11/09/2001

  Transparency fixed

11/01/2001

  AutoDisableChildren property added. If it is true, children get disabled
  automatically if checkbox is unchecked, and enabled if checkbox is checked.  

10/28/2001

  Changing Enabled state didn't repaint the group box. Fixed.

10/22/2001

  Fixed painting of background with XP styles enabled 

09/17/2001

  Added Windows XP Themes Support 

09/08/2001

  Added missing Unicode support

*)

unit ElGroupBox;

interface

uses
  SysUtils,
  Classes,
  {$ifndef CLX_USED}
{$ifdef VCL_6_USED}
  Types,
{$endif}
  Stdctrls,
  Messages,
  Controls,
  Windows,
  Graphics,
  ElImgFrm,
  ExtCtrls,
  Forms,
  {$ifdef VCL_4_USED}
  ImgList,
  {$endif}
  {$else}
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  QForms,
  QStdCtrls,
  QControls,
  QGraphics,
  QTypes,
  Qt,
  Types,
  QImgList,
  ElCLXUtils,
  {$endif}
  ElList,
  ElPanel,
  ElVCLUtils,
  HTMLRender,
  ElTools,
  ElTmSchema,
  ElUxTheme,
  {$ifdef USE_SOUND_MAP}
  ElSndMap,
  {$endif}
  ElCheckCtl;

type

  TCustomElGroupBox = class(TCustomElPanel)
  protected
    FReading  : Boolean;
  {$ifdef HAS_HTML_RENDER}
    FRender   : TElHTMLRender;
    FIsHTML: Boolean;
  {$endif}
    FBorderSides: TElBorderSides;
    FTextRect,
    FCheckRect  : TRect;
    {$ifdef USE_SOUND_MAP}
    FCheckSound: TElSoundName;
    {$endif}
    FGlyph: TBitmap;
    FImages: TImageList;
    {$ifdef USE_SOUND_MAP}
    FSoundMap: TElSoundMap;
    {$endif}
    FUseCustomGlyphs: Boolean;
    FUseImageList: Boolean;
    FChLink: TChangeLink;
    FShowCheckBox: Boolean;
    FCheckBoxChecked: Boolean;
    FShowFocus: Boolean;
    FCaptionColor: TColor;
    FMouseInText,
    FMouseInCheckBox : boolean;
    FFlat: Boolean;
    FFlatAlways: Boolean;
    FAutoDisableChildren: Boolean;
    {$ifndef CLX_USED}
    FMoneyFlat: Boolean;
    FMoneyFlatInactiveColor: TColor;
    FMoneyFlatActiveColor: TColor;
    FMoneyFlatDownColor: TColor;
    {$endif}
    procedure SetBorderSides(Value: TElBorderSides);
    procedure ImagesChanged(Sender: TObject);
    {$ifdef HAS_HTML_RENDER}
    procedure SetIsHTML(Value: Boolean); virtual;
    {$endif}
    procedure ReadState(Reader: TReader); override;
    function  CanModify: Boolean; virtual;
    procedure Paint; override;
    {$ifdef MSWINDOWS}
    function GetThemedClassName: WideString; override;
    {$endif}
    procedure SetShowCheckBox(Value: Boolean);
    procedure SetCheckBoxChecked(Value: Boolean); virtual;
    procedure SetShowFocus(Value: Boolean);
    procedure SetCaptionColor(Value: TColor);
    {$ifdef USE_SOUND_MAP}
    procedure SetCheckSound(Value: TElSoundName); virtual;
    {$endif}
    procedure SetGlyph(Value: TBitmap); virtual;
    {$ifdef USE_SOUND_MAP}
    procedure SetSoundMap(Value: TElSoundMap); virtual;
    {$endif}
    procedure SetUseCustomGlyphs(Value: Boolean); virtual;
    procedure SetImages(Value: TImageList); virtual;
    procedure SetUseImageList(Value: Boolean); virtual;
    procedure Notification(AComponent : TComponent; operation : TOperation); 
        override;
    procedure GlyphChange(Sender : TObject); virtual;
    function GetCheckBoxSize: TSize;
    procedure DrawGlyph(Canvas : TCanvas; DestRect: TRect; SrcRect: TRect); virtual;
    procedure SetFlat(newValue : boolean); virtual;
    {$ifndef CLX_USED}
    procedure DrawFlatFrame(Canvas : TCanvas; R : TRect);
    {$endif}
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
        override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); 
        override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function GetTopOffset: Integer;
    {$ifdef CLX_USED}
    function WidgetFlags: Integer; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$endif}
    {$ifndef CLX_USED}
    procedure WMEraseBkGnd(var Msg : TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var Msg : TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg : TCMExit); message CM_EXIT;
    {$endif}
    procedure SetFlatAlways(Value: Boolean);
    function GetCaptionRect: TRect;
    function GetCheckRect: TRect;
    function GetLineTopOffset: Integer;
    {$ifndef CLX_USED}
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    {$else}
    procedure EnabledChanged; override;
    {$endif}
    procedure SetAutoDisableChildren(Value: Boolean);
    {$ifndef CLX_USED}
    procedure SetMoneyFlat(Value: Boolean); virtual;
    procedure SetMoneyFlatInactiveColor(Value: TColor); virtual;
    procedure SetMoneyFlatActiveColor(Value: TColor); virtual;
    procedure SetMoneyFlatDownColor(Value: TColor); virtual;
    function GetMoneyFlat: Boolean;
    {$endif}

    {$ifdef HAS_HTML_RENDER}
    property IsHTML: Boolean read FIsHTML write SetIsHTML default false;
    {$endif}
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox
        default false;
    property CheckBoxChecked: Boolean read FCheckBoxChecked write
        SetCheckBoxChecked default true;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default
        clNone;
    {$ifdef USE_SOUND_MAP}
    property CheckSound: TElSoundName read FCheckSound write SetCheckSound;
    {$endif}
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Images: TImageList read FImages write SetImages;
    {$ifdef USE_SOUND_MAP}
    property SoundMap: TElSoundMap read FSoundMap write SetSoundMap;
    {$endif}
    property UseCustomGlyphs: Boolean read FUseCustomGlyphs write
        SetUseCustomGlyphs default false;
    property UseImageList: Boolean read FUseImageList write SetUseImageList default
        false;
    property Flat: Boolean read FFlat write SetFlat;
    property FlatAlways: Boolean read FFlatAlways write SetFlatAlways;
    property AutoDisableChildren: Boolean read FAutoDisableChildren write SetAutoDisableChildren;
    {$ifndef CLX_USED}
    property MoneyFlat: Boolean read GetMoneyFlat write SetMoneyFlat default false;
    property MoneyFlatInactiveColor: TColor read FMoneyFlatInactiveColor write SetMoneyFlatInactiveColor stored GetMoneyFlat;
    property MoneyFlatActiveColor: TColor read FMoneyFlatActiveColor write
        SetMoneyFlatActiveColor stored GetMoneyFlat;
    property MoneyFlatDownColor: TColor read FMoneyFlatDownColor write
        SetMoneyFlatDownColor stored GetMoneyFlat;
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

{$IFDEF VCL_4_USED}
    procedure FlipChildren(AllLevels: Boolean); override;
{$ENDIF}

  published
  end;

  TElGroupBox = class(TCustomElGroupBox)
    property Align;
{$IFDEF VCL_4_USED}
    property Anchors;
    {$ifndef CLX_USED}
    property AutoDisableChildren;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$endif}
{$ENDIF}
    {$ifndef CLX_USED}
    property BorderSides;
    {$endif}
    property Caption;
    property CaptionColor;
    property CheckBoxChecked;
    {$ifdef USE_SOUND_MAP}
    property CheckSound;
    {$endif}
    property Color;
    {$ifndef CLX_USED}
    property Ctl3D;
    property DragCursor;
    {$endif}
    property DragMode;
    property Enabled;
    property Flat;
    property FlatAlways;
    property Font;
    property Glyph;
    {$ifndef CLX_USED}
    property ImageForm;
    {$endif}
    property Images;
    {$ifndef CLX_USED}
    property MoneyFlat;
    property MoneyFlatInactiveColor;
    property MoneyFlatActiveColor;
    property MoneyFlatDownColor;
    {$endif}
  {$ifdef HAS_HTML_RENDER}
    property IsHTML;
  {$endif}
    {$ifndef CLX_USED}
    property ParentCtl3D;
    {$endif}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCheckBox;
    property ShowFocus;
    property ShowHint;
    {$ifdef USE_SOUND_MAP}
    property SoundMap;
    {$endif}
    property TabOrder;
    property TabStop;
    property Transparent;
    property UseCustomGlyphs;
    property UseImageList;
    {$ifdef MSWINDOWS}
    property UseXPThemes;
    {$endif}
    property Visible;

    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnStartDrag;
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

implementation

const CheckBoxSize = 13;
      CheckMargin = 5;



constructor TCustomElGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csAcceptsControls, csDoubleClicks];
  FBorderSides := AllBorderSides;
  FCaptionColor := clNone;
  FCheckBoxChecked := true;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChange;
  FChLink := TChangeLink.Create;
  FChLink.OnChange := ImagesChanged;
end;

destructor TCustomElGroupBox.Destroy;
begin
{$ifdef HAS_HTML_RENDER}
  if IsHTML then
    FRender.Free;
{$endif}
  FGlyph.Free;
  FGlyph := nil;
  FChLink.Free;
  FChLink := nil;
  inherited Destroy;
end;

{$IFDEF VCL_4_USED}
procedure TCustomElGroupBox.FlipChildren(AllLevels: Boolean);
begin
end;
{$ENDIF}

procedure TCustomElGroupBox.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
end;

function TCustomElGroupBox.CanModify: Boolean;
begin
  Result := True;
end;

procedure TCustomElGroupBox.Paint;
var
  H: Integer;
  {$ifdef MSWINDOWS}
  Flags : Longint;
  {$endif}
  SrcRect,
  CheckRect : TRect;
  cbh,
  cbw   : integer;
  i,
  {$ifdef MSWINDOWS}
  pid,
  sid,
  {$endif}
  ImgIdx: integer;

  R1,
  R,
  R2
  {$ifndef CLX_USED}
  ,Rect
  {$endif}
   : TRect;
  {$ifndef CLX_USED}
//  DC   : HDC;
  ACtl : TWinControl;
  P    : TPoint;

  ax,
  ay   : integer;
  {$endif}
  {$ifdef MSWINDOWS}
  RClip: TRect;
  {$endif}
  {$ifdef CLX_USED}
  PX   : TSize;
  {$endif}

  {$ifndef CLX_USED}
  c    : ColorRef;
  {$else}
  {$ifdef MSWINDOWS}
  C    : COLORREF;
  {$else}
  c    : TColor;
  {$endif}
  {$endif}
  {$ifndef _CLX_USED}
  Canvas: TCanvas;
  Bitmap: TBitmap;
  {$endif}

  procedure DrawGroupBevel(Canvas : TCanvas; R, R1 : TRect; Color1, Color2 : TColor; BorderSides : TElBorderSides);
  begin
    {$ifdef CLX_USED}
    Canvas.Start;
    {$endif}
    DrawBevel(Canvas.Handle, R, Color1, Color2, BorderSides * [ebsLeft, ebsRight, ebsBottom]);
    if ebsTop in BorderSides then
    begin
      if R1.Left > 0 then
        DrawBevel(Canvas.Handle, Classes.Rect(R.Left, R.Top, R1.Left, R.Bottom), Color1, Color2, [ebsTop]);
      if R1.Right < R.Right then
        DrawBevel(Canvas.Handle, Classes.Rect(R1.Right, R.Top, R.Right, R.Bottom), Color1, Color2, [ebsTop]);
    end;
    {$ifdef CLX_USED}
    Canvas.Stop;
    {$endif}
  end;

begin

  {$ifndef _CLX_USED}
  Bitmap := Tbitmap.Create;
  Bitmap.Width := ClientWidth;
  Bitmap.Height := ClientHeight;
  // Bitmap.Canvas.Handle := CreateCompatibleBitmap(Self.Canvas.Handle, ClientWidth, ClientHeight);
  if true {not IsThemeApplied} then
  begin
    Canvas := Bitmap.Canvas;
    {$ifndef CLX_USED}
    bitblt(Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Self.Canvas.Handle, 0, 0, SRCCOPY);
    {$else}
    Canvas.CopyRect(ClientRect, Self.Canvas, ClientRect);
    {$endif}
  end
  else
    Canvas := Self.Canvas;
  {$else}
  Bitmap := nil;
  Canvas := Self.Canvas;
  {$endif}

  {$ifdef MSWINDOWS}
  if not IsThemeApplied then
  {$endif}
  begin
    {$ifndef CLX_USED}
    if (ImageForm <> nil) and (not (csDesigning in ImageForm.ComponentState)) then
    begin
      if (ImageForm.Control <> Self) then
      begin
        ACtl := ImageForm.GetRealControl;
        Rect  := ClientRect;
        R1 := Rect;
        Rect.TopLeft := ClientToScreen(Rect.TopLeft);
        P := Parent.ClientToScreen(Point(Left, Top));
        ax := Rect.Left - P.x;
        ay := Rect.Top - P.y;

        Rect.BottomRight := ClientToScreen(Rect.BottomRight);
        Rect.TopLeft := ACtl.ScreenToClient(Rect.TopLeft);
        Rect.BottomRight := ACtl.ScreenToClient(Rect.BottomRight);

        ImageForm.PaintBkgnd(Canvas.Handle, R1, Point(Rect.Left - ax, Rect.Top - ay), false);
      end;
    end
    else
    if Transparent then
    begin
      GetClipBox(Self.Canvas.Handle, R2);
      OffsetRect(R2, Left, Top);
      RedrawWindow(Parent.Handle, @R2, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN or RDW_UPDATENOW);
      OffsetRect(R2, -Left, -Top);
      // DC := GetDC(Handle);
      bitblt(Canvas.Handle, R2.Left, R2.Top, R2.Right - R2.Left, R2.Bottom - R2.Top, Self.Canvas.Handle, R2.Left, R2.Top, SRCCOPY);
      // ReleaseDC(Handle, DC);

      // bitblt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Self.Canvas.Handle, 0, 0, SRCCOPY);

    end
    else
    {$endif}
    begin
      R2 := Canvas.ClipRect;
      // GetClipBox(Canvas.Handle, R2);
      Canvas.Brush.Color := Color;
      Canvas.FillRect(R2);
    end;
  end;

  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    {$ifdef MSWINDOWS}
    if IsThemeApplied then
    begin
      if not Enabled then
        sid := GBS_DISABLED
      else
        sid := GBS_NORMAL;
      GetThemeColor(Theme, BP_GROUPBOX, sid, TMT_TEXTCOLOR, C);
    end
    else
    {$endif}
      C := Font.Color;

    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.DefaultColor := C;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.Charset := Font.Charset;
    FRender.Data.DefaultBgColor := clNone;

    FRender.PrepareText(Caption, 0, false);

    H := FRender.Data.TextSize.cy;
  end
  else
{$endif}
  begin
    with Canvas do
    begin
      Font := Self.Font;
      H := TextHeight('0');
    end;
  end;

  if ShowCheckBox then
  begin
    {$ifdef MSWINDOWS}
    if not IsThemeApplied then
    {$endif}
    begin
      i := 0;
      ImgIdx := 0;
      if FUseCustomGlyphs and (not FGlyph.Empty) then
      begin
        cbh := FGlyph.Height;
        cbw := FGlyph.Width div 6;
        if not CheckBoxChecked then
        begin
          if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText))) then
            SrcRect := Classes.Rect(0, 0, cbw, cbh)
          else
            SrcRect := Classes.Rect(cbw, 0, cbw * 2, cbh);
        end
        else
        begin
          if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText))) then
            SrcRect := Classes.Rect(cbw * 2, 0, cbw * 3, cbh)
          else
            SrcRect := Classes.Rect(cbw * 3, 0, cbw * 4, cbh);
        end;
      end
      else
      begin
        {$ifndef CLX_USED}
        if FUseImageList then
        begin
          cbw := FImages.Width;
          cbh := FImages.Height;
          if not CheckBoxChecked then
          begin
            if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
            then
              ImgIdx := 0
            else
              ImgIdx := 1;
          end
          else
          begin
            if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
            then
              ImgIdx := 2
            else
              ImgIdx := 3;
          end;
        end
        else
        begin
          cbw := GetCheckBoxSize.cx;
          cbh := GetCheckBoxSize.cy;
        end;
        if not CheckBoxChecked then
          i := DFCS_BUTTONCHECK
        else
          i := DFCS_BUTTONCHECK or DFCS_CHECKED;

        if (not Enabled) or ((FMouseInCheckBox or FMouseInText) and FPressed) then
          i := i or DFCS_INACTIVE;
        {$else}
        if FUseImageList then
        begin
          cbw := FImages.Width;
          cbh := FImages.Height;
          if not CheckBoxChecked then
          begin
            if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
            then
              ImgIdx := 0
            else
              ImgIdx := 1;
          end
          else
          begin
            if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
            then
              ImgIdx := 2
            else
              ImgIdx := 3;
          end;
        end
        else
        begin
          QStyle_indicatorSize(Application.Style.Handle, @PX);
          cbw := PX.cx;
          cbh := PX.cy;
        end;
        if CheckBoxChecked then
          i := integer(QButtonToggleState_On)
        else
          i := integer(QButtonToggleState_Off);
        {$endif}
      end;
      CheckRect := Classes.Rect(8, (H div 2 - cbh div 2), 8 + cbw, 0);
      CheckRect.Bottom := CheckRect.Top + cbh;

      if UseCustomGlyphs and (not FGlyph.Empty) then
        DrawGlyph(Canvas, CheckRect, SrcRect)
      else
      begin
        if FUseImageList then
        begin
          FImages.Draw(Canvas, CheckRect.Left, CheckRect.Top, ImgIdx);
        end
        else
        {$ifndef CLX_USED}
        begin
          DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, i);
          if MoneyFlat or ((Flat and (not (Focused or (FMouseInCheckBox or FMouseInText)))) or FlatAlways) and Enabled then
            DrawFlatFrame(Canvas, CheckRect);
        end;
        {$else}
        Canvas.Brush.Style := bsClear;
        Canvas.Brush.Color := clBtnFace;
        // this is a dirty trick
        Canvas.FillRect(checkRect);
        {$ifdef CLX_USED}
        Canvas.Start;
        {$endif}
        QStyle_DrawIndicator(Application.Style.Handle,
                             Canvas.Handle,
                             CheckRect.Left,
                             CheckRect.Top,
                             CheckRect.Right - CheckRect.Left,
                             CheckRect.Bottom - CheckRect.Top,
                             QWidget_colorGroup(Handle),
                             i,
                             ((FMouseInCheckBox or FMouseInText) and FPressed),
                             Enabled);
        {$ifdef CLX_USED}
        Canvas.Stop;
        {$endif}
        Canvas.Brush.Color := clBtnFace;
        Canvas.Brush.Style := bsSolid;
        {$endif}
      end;
    {$ifdef MSWINDOWS}
    end
    else // XP-style checkmarks
    begin
      pid := BP_CHECKBOX;
      if not CheckBoxChecked then
      begin
        if not Enabled then
          sid := CBS_UNCHECKEDDISABLED
        else
        if FPressed and (FMouseInCheckBox or FMouseInText) then
          sid := CBS_UNCHECKEDPRESSED
        else
        if FMouseInCheckBox then
          sid := CBS_UNCHECKEDHOT
        else
          sid := CBS_UNCHECKEDNORMAL;
      end
      else
      begin
        if not Enabled then
          sid := CBS_CHECKEDDISABLED
        else
        if FPressed and (FMouseInCheckBox or FMouseInText) then
          sid := CBS_CHECKEDPRESSED
        else
        if (FMouseInCheckBox or FMouseInText) then
          sid := CBS_CHECKEDHOT
        else
          sid := CBS_CHECKEDNORMAL;
      end;

      cbw := GetCheckBoxSize.cx;
      cbh := GetCheckBoxSize.cy;

      CheckRect := Classes.Rect(8, (H div 2 - cbh div 2), 8 + cbw, 0);
      CheckRect.Bottom := CheckRect.Top + cbh;
      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, pid, sid, CheckRect, nil);
      {$else}
      Canvas.Start;
      DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)),pid, sid, CheckRect, nil);
      Canvas.Stop;
      {$endif}
    {$endif}
    end;

    FCheckRect := CheckRect;
  end
  else
    SetRectEmpty(FCheckRect);

  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    R := Classes.Rect(0, H div 2, Width, Height);
    R1 := R;
    if (FCaptionColor <> clNone) {$ifdef MSWINDOWS}and (not IsThemeApplied){$endif} then
    begin
      R := Classes.Rect(0, 0, Width, H);
      Canvas.Brush.Color := FCaptionColor;
      Canvas.FillRect(R);
    end;

    if Text <> '' then
    begin
      if FCheckRect.Right <> 0 then
        R := Classes.Rect(FCheckRect.Right + CheckMargin, 0, FCheckRect.Right + CheckMargin + FRender.Data.TextSize.cx, H)
      else
        R := Classes.Rect(8, 0, 8 + FRender.Data.TextSize.cx, H);
      {$ifdef CLX_USED}
      Canvas.Start;
      {$endif}
      FRender.DrawText(Canvas, Point(0, 0), R, clNone);
      {$ifdef CLX_USED}
      Canvas.Stop;
      {$endif}
      InflateRect(R, 1, 0);
      FTextRect := R;

      if ShowFocus and Focused then
      begin
        InflateRect(R, 1, 0);
        Canvas.DrawFocusRect(R);
        InflateRect(R, -1, 0);
      end;

      if FCheckRect.Left <> 0 then
        R.Left := FCheckRect.Left;
    end
    else
      R := FCheckRect;

    {$ifdef MSWINDOWS}
      {$ifndef CLX_USED}
    with R do
       ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
      {$else}
    begin
      Canvas.Start;
      with R do
        ExcludeClipRect(QPaintDevice_handle(QPainter_device(Canvas.Handle)), Left, Top, Right, Bottom);
      Canvas.Stop;
    end;
      {$endif}
    {$endif}

    {$ifdef MSWINDOWS}
    if IsThemeApplied then
    begin
      if not Enabled then
        sid := GBS_DISABLED
      else
        sid := GBS_NORMAL;

      {$ifndef CLX_USED}
      RClip := Canvas.ClipRect;
      {$else}
      RClip := ClientRect;
      {$endif}


      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, BP_GROUPBOX, sid, R1, nil);
      {$else}
      Canvas.Start;
      DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_GROUPBOX, sid, R1, @RClip);
      Canvas.Stop;
      {$endif}
    end
    else
    {$endif}
    begin
      {$ifndef CLX_USED}
      if Ctl3D and (not MoneyFlat) then
      {$endif}
      begin
        Inc(R1.Left);
        Inc(R1.Top);
        {$ifndef CLX_USED}
        DrawBevel(Canvas.Handle, R1, clBtnHighlight, clBtnHighlight, BorderSides);
        {$else}
        DrawGroupBevel(Canvas, R1, R, Palette.GetColor(GetColorGroup(Self), crLight), Palette.GetColor(GetColorGroup(Self), crLight), BorderSides);
        {$endif}
        OffsetRect(R1, -1, -1);
        {$ifdef MSWINDOWS}
        Brush.Color := clBtnShadow;
        {$else}
        Brush.Color := Palette.GetColor(GetColorGroup(Self), crShadow);
        {$endif}
      end
      {$ifndef CLX_USED}
      else
        if MoneyFlat then
          Brush.Color := MoneyFlatInactiveColor
        else
          Brush.Color := clWindowFrame
      {$endif}
      ;

      {$ifndef CLX_USED}
      DrawBevel(Canvas.Handle, R1, Brush.Color, Brush.Color, BorderSides);
      {$else}
      DrawGroupBevel(Canvas, R1, R, Brush.Color, Brush.Color, BorderSides);
      {$endif}
    end;
  end
  else
  {$endif}

  with Canvas do
  begin
    Font := Self.Font;
    R := Classes.Rect(0, H div 2, Width, Height);
    R1 := R;

    if (FCaptionColor <> clNone) {$ifdef MSWINDOWS}and (not IsThemeApplied){$endif} then
    begin
      R := Classes.Rect(0, 0, Width, H);
      Canvas.Brush.Color := FCaptionColor;
      Canvas.FillRect(R);
    end;

    if Caption <> '' then
    begin
      {$ifdef MSWINDOWS}
      if IsThemeApplied then
      begin
        if not Enabled then
          sid := GBS_DISABLED
        else
          sid := GBS_NORMAL;

        Flags := DT_LEFT;
        RClip := ClientRect;
        SetRectEmpty(R);
        {$ifndef CLX_USED}
        GetThemeTextExtent(Theme, Canvas.Handle, BP_GROUPBOX, sid,
        {$else}
        Canvas.Start;
        GetThemeTextExtent(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_GROUPBOX, sid,
        {$endif}
          PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, @RClip, R);

        Canvas.Brush.Style := bsClear;
        //R := R1;
        if FCheckRect.Left <> 0 then
          OffsetRect(R, FCheckRect.Right + CheckMargin, 0)
        else
          OffsetRect(R, 8, 0);

        {$ifndef CLX_USED}
        DrawThemeText(Theme, Canvas.Handle, BP_GROUPBOX, sid,
          PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, 0, R);
        {$else}
        // DrawThemeText(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_GROUPBOX, 0{sid},
        Canvas.TextRect(R, R.Left, R.Top, Caption);
        {$endif}

        {$ifdef CLX_USED}
        Canvas.Stop;
        {$endif}
        InflateRect(R, 1, 0);

        if ShowFocus and Focused then
        begin
          InflateRect(R, 1, 0);
          Canvas.DrawFocusRect(R);
          InflateRect(R, -1, 0);
        end;
      end
      else
      {$endif}
      begin
        R := Classes.Rect(0, 0, 0, H);

        {$ifndef CLX_USED}
        Flags := DT_LEFT;
        {$IFDEF ELPACK_UNICODE}
        ElVCLUtils.DrawTextW(Handle, PWideChar(Caption), Length(Caption), R, Flags or DT_CALCRECT);
        {$else}
        DrawText(Handle, PChar(Caption), Length(Caption), R, Flags or DT_CALCRECT);
        {$endif}
        {$else}
        Canvas.TextExtent(Caption, R);
        {$endif}
           // Brush.Color := Color;

        if FCheckRect.Left <> 0 then
          OffsetRect(R, FCheckRect.Right + CheckMargin, 0)
        else
          OffsetRect(R, 8, 0);

        Canvas.Brush.Style := bsClear;
        {$ifndef CLX_USED}
        {$IFDEF ELPACK_UNICODE}
        ElVCLUtils.DrawTextW(Handle, PWideChar(Caption), Length(Caption), R, Flags);
        {$else}
        DrawText(Handle, PChar(Caption), Length(Caption), R, Flags);
        {$endif}
        {$else}
        Canvas.TextRect(R, R.Left, R.Top, Caption);
        {$endif}
        InflateRect(R, 1, 0);
        FTextRect := R;

        if ShowFocus and Focused then
        begin
          InflateRect(R, 1, 0);
          DrawFocusRect(R);
          InflateRect(R, -1, 0);
        end;
      end;
      if FCheckRect.Left <> 0 then
        R.Left := FCheckRect.Left;
    end
    else
      R := FCheckRect;
    {$ifdef MSWINDOWS}
    {$ifndef CLX_USED}
    with R do
      ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    {$else}
      Canvas.Start;
      with R do
        ExcludeClipRect(QPaintDevice_handle(QPainter_device(Canvas.Handle)), Left, Top, Right, Bottom);
      Canvas.Stop;
    {$endif}
    if IsThemeApplied then
    begin
      if not Enabled then
        sid := GBS_DISABLED
      else
        sid := GBS_NORMAL;
      {$ifndef CLX_USED}
      RClip := Canvas.ClipRect;
      {$else}
      RClip := ClientRect;
      {$endif}
      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, BP_GROUPBOX, sid, R1, @RClip);
      {$else}
      Canvas.Start;
      DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_GROUPBOX, sid, R1, @RClip);
      Canvas.Stop;
      {$endif}
    end
    else
    {$endif}
    begin
      {$ifndef CLX_USED}
      if Ctl3D and not MoneyFlat then
      {$endif}
      begin
        Inc(R1.Left);
        Inc(R1.Top);
        {$ifndef CLX_USED}
        DrawBevel(Canvas.Handle, R1, clBtnHighlight, clBtnHighlight, BorderSides);
        {$else}
        DrawGroupBevel(Canvas, R1, R, Palette.GetColor(GetColorGroup(Self), crLight), Palette.GetColor(GetColorGroup(Self), crLight), BorderSides);
        {$endif}
        OffsetRect(R1, -1, -1);
        {$ifndef CLX_USED}
        Brush.Color := clBtnShadow;
        {$else}
        Brush.Color := Palette.GetColor(GetColorGroup(Self), crShadow);
        {$endif}
      end
      {$ifndef CLX_USED}
      else
        if MoneyFlat then
          Brush.Color := MoneyFlatInactiveColor
        else
          Brush.Color := clWindowFrame
      {$endif}
      ;

      {$ifndef CLX_USED}
      DrawBevel(Canvas.Handle, R1, Brush.Color, Brush.Color, BorderSides);
      {$else}
      DrawGroupBevel(Canvas, R1, R, Brush.Color, Brush.Color, BorderSides);
      {$endif}
    end;
  end;

  R1 := Self.Canvas.ClipRect;
  if IsRectEmpty(R1) then
    R1 := ClientRect;
  {$ifndef _CLX_USED}
  {$ifndef CLX_USED}
  if true {not IsThemeApplied} then
    bitblt(Self.Canvas.Handle, R1.Left, R1.Top, R1.right - R1.Left, R1.Bottom - R1.Top, Bitmap.Canvas.Handle, R1.Left, R1.Top, SRCCOPY);
  {$else}
  if true{not IsThemeApplied} then
    Self.Canvas.CopyRect(R1, Bitmap.Canvas, R1);
  {$endif}
  Bitmap.Free;
  {$endif}
end;

{$ifndef CLX_USED}
procedure TCustomElGroupBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if IsThemeApplied then
  begin
    DrawThemeParentBackground(Handle, Msg.DC, ClientRect);

    (*
    R1 := GetCaptionRect;

    R := ClientRect;
    H := GetLineTopOffset;

    R.Top := H div 2;
    DrawThemeBackground(Theme, Msg.DC, BP_GROUPBOX, 0, R, nil);
    DrawThemeParentBackground(Handle, Msg.DC, R1);
    *)
  end;
  Msg.Result := 1;
end;
{$endif}

{$ifdef HAS_HTML_RENDER}
procedure TCustomElGroupBox.SetIsHTML(Value: Boolean);
begin
  if FIsHTML <> Value then
  begin
    FIsHTML := Value;
    if FIsHTML then
      FRender := TElHTMLRender.Create
    else
      FRender.Free;
    if HandleAllocated then
      Invalidate;
  end;
end;

{$endif}

procedure TCustomElGroupBox.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      Invalidate;
  end;
end;

{$ifdef MSWINDOWS}
function TCustomElGroupBox.GetThemedClassName: WideString;
begin
  Result := 'BUTTON';
end;
{$endif}

procedure TCustomElGroupBox.SetShowCheckBox(Value: Boolean);
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    Invalidate;
  end;
end;

procedure TCustomElGroupBox.SetCheckBoxChecked(Value: Boolean);
var i : integer;
begin
  if FCheckBoxChecked <> Value then
  begin
    FCheckBoxChecked := Value;
    if FAutoDisableChildren then
      for I := 0 to ControlCount - 1 do
        Controls[i].Enabled := Enabled and ((not ShowCheckBox) or CheckboxChecked);

    {$ifndef CLX_USED}
    if ShowCheckBox then
      InvalidateRect(Handle, @FCheckRect, true);
    {$else}
    Inc(FCheckRect.Bottom); Inc(FCheckRect.Right);
    QWidget_update(Handle, @FCheckRect);
    Dec(FCheckRect.Bottom); Dec(FCheckRect.Right);
    {$endif}
  end;
end;

procedure TCustomElGroupBox.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TCustomElGroupBox.SetCaptionColor(Value: TColor);
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Invalidate;
  end;
end;

{$ifdef USE_SOUND_MAP}
procedure TCustomElGroupBox.SetCheckSound(Value: TElSoundName);
begin
  if FCheckSound <> Value then
  begin
    FCheckSound := Value;
  end;
end;
{$endif}

procedure TCustomElGroupBox.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TCustomElGroupBox.SetImages(Value: TImageList);
var b : boolean;
begin
  if FImages <> Value then
  begin
    b := FImages = nil;
    {$ifdef VCL_5_USED}
    if not b then
    begin
      FImages.RemoveFreeNotification(Self);
      FImages.UnRegisterChanges(FChLink);
    end;
    {$endif}
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(Self);
      FImages.RegisterChanges(FChLink);
    end;
    
    if FImages = nil then
      UseImageList := false
    else
    if b and (not (csLoading in ComponentState)) then
      UseImageList := true;
  end;
end;

{$ifdef USE_SOUND_MAP}
procedure TCustomElGroupBox.SetSoundMap(Value: TElSoundMap);
begin
  if FSoundMap <> Value then
  begin
    {$ifdef VCL_5_USED}
    if FSoundMap <> nil then
      FSoundmap.RemoveFreeNotification(Self);
    {$endif}
    FSoundMap := Value;
    if FSoundMap <> nil then
      FSoundmap.FreeNotification(Self);
  end;
end;
{$endif}

procedure TCustomElGroupBox.SetUseCustomGlyphs(Value: Boolean);
begin
  if FUseCustomGlyphs <> Value then
  begin
    FUseCustomGlyphs := Value;
    if ShowCheckBox then Invalidate;
  end;
end;

procedure TCustomElGroupBox.SetUseImageList(Value: Boolean);
begin
  if FUseImageList <> Value then
  begin
    FUseImageList := Value;
    if ShowCheckBox then Invalidate;
  end;
end;

procedure TCustomElGroupBox.ImagesChanged(Sender: TObject);
begin
  if FUseImageList and ShowCheckBox then
    Invalidate;
end;

procedure TCustomElGroupBox.Notification(AComponent : TComponent; operation : 
    TOperation);
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) then
  begin
{$IFDEF USE_SOUND_MAP}
    if (AComponent = FSoundMap) then SoundMap := nil;
{$ENDIF}
    if AComponent = Images then
      Images := nil;
  end; { if }
end; { Notification }

procedure TCustomElGroupBox.GlyphChange(Sender : TObject);
begin
  if UseCustomGlyphs then Invalidate;
end;

function TCustomElGroupBox.GetCheckBoxSize: TSize;
begin
  Result.cx := CheckBoxSize;
  Result.cy := CheckBoxSize;
  {$ifdef MSWINDOWS}
  if IsThemeApplied() then
  begin
    {$ifndef CLX_USED}
    GetThemePartSizeTo('BUTTON', Canvas.Handle, BP_CHECKBOX, 1, nil, TS_TRUE, Result);
    {$else}
    Canvas.Start;
    GetThemePartSizeTo('BUTTON', QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_CHECKBOX, 1, nil, TS_TRUE, Result);
    Canvas.Stop;
    {$endif}
  end;
  {$else}
  QStyle_indicatorSize(Application.Style.Handle, @result);
  {$endif}
end;

procedure TCustomElGroupBox.DrawGlyph(Canvas : TCanvas; DestRect: TRect;
    SrcRect: TRect);
begin
  {$ifndef CLX_USED}
  DrawTransparentBitmapEx(Canvas.Handle, FGlyph, DestRect.Left, DestRect.Top, SrcRect, FGlyph.Canvas.Pixels[SrcRect.Left, SrcRect.Bottom - 1]);
  {$else}
  DrawTransparentBitmapEx(Canvas.Handle, FGlyph, DestRect.Left, DestRect.Top, SrcRect, GetPixel(FGlyph.Canvas, SrcRect.Left, SrcRect.Bottom - 1));
  {$endif}
end;

procedure TCustomElGroupBox.SetFlat(newValue : boolean);
begin
  if (newValue <> FFlat) then
  begin
    FFlat := newValue;
    if ShowCheckBox then Invalidate;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElGroupBox.DrawFlatFrame(Canvas : TCanvas; R : TRect);
var AColor, Color : TColor;
begin
  if MoneyFlat then
  begin
    if Enabled and not FPressed then
      Color := clWindow
    else
      Color := clBtnFace;
    if FPressed and (FMouseInCheckBox or FMouseInText) then
      AColor := MoneyFlatDownColor
    else
    if Focused or (FMouseInCheckBox or FMouseInText) then
      AColor := MoneyFlatActiveColor
    else
      AColor := MoneyFlatInactiveColor;
    ELVCLUtils.DrawFlatFrameEx2(Canvas.Handle, R, AColor, Color, false, true, AllBorderSides, fbtColorLineBorder);
  end
  else
  if Enabled and not FPressed then
    ElVCLUtils.DrawFlatFrame(Canvas.Handle, R, clWindow, false)
  else
    ElVCLUtils.DrawFlatFrame(Canvas.Handle, R, clBtnFace, false);
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElGroupBox.CMMouseLeave(var Msg : TMessage);
begin
  inherited;
  if ShowCheckBox and (FMouseInCheckBox or FMouseInText) then
  begin
    FMouseInCheckBox := false;
    FmouseInText := false;
    InvalidateRect(Handle, @FCheckRect, true);
  end;
end; { CMMouseLeave }
{$endif}

procedure TCustomElGroupBox.MouseDown(Button : TMouseButton; Shift :
    TShiftState; X, Y : Integer);
begin
  if CanFocus then
    SetFocus;
  if FMouseInCheckBox or FMouseInText then
  {$ifndef CLX_USED}
    InvalidateRect(Handle, @FCheckRect, true);
  {$else}
    Inc(FCheckRect.Bottom); Inc(FCheckRect.Right);
    QWidget_update(Handle, @FCheckRect);
    Dec(FCheckRect.Bottom); Dec(FCheckRect.Right);
  {$endif}
  inherited;
end; { MouseDown }

procedure TCustomElGroupBox.MouseUp(Button : TMouseButton; Shift : TShiftState;
    X, Y : Integer);
begin
  if PtInRect(FCheckRect, Point(X, Y)) or PtInRect(FTextRect, Point(X, Y)) then
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then
      SoundMap.Play(FCheckSound);
{$ENDIF}
    CheckBoxChecked := not CheckBoxChecked;
    Click;
  end;
  if FMouseInCheckBox or FMouseInText then
  {$ifndef CLX_USED}
    InvalidateRect(Handle, @FCheckRect, true);
  {$else}
    Inc(FCheckRect.Bottom); Inc(FCheckRect.Right);
    QWidget_update(Handle, @FCheckRect);
    Dec(FCheckRect.Bottom); Dec(FCheckRect.Right);
  {$endif}
end; { MouseUp }

{$ifndef CLX_USED}
procedure TCustomElGroupBox.CMEnter(var Msg : TCMEnter);
{$else}
procedure TCustomElGroupBox.DoEnter;
{$endif}
begin
  inherited;
  if ShowFocus then
    Invalidate;
end; { CMEnter }

{$ifndef CLX_USED}
procedure TCustomElGroupBox.CMExit(var Msg : TCMExit);
{$else}
procedure TCustomElGroupBox.DoExit;
{$endif}
begin
  inherited;
  if ShowFocus or (ShowCheckBox and Flat and not FlatAlways) then
    Invalidate;
end; { CMExit }

procedure TCustomElGroupBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var b : boolean;
begin
  b := FMouseInCheckBox or FMouseInText;
  FMouseInCheckBox := ShowCheckBox and PtInRect(FCheckRect, Point(X, Y));
  FMouseInText := {ShowCheckBox and} PtInRect(FTextRect, Point(X, Y));
  inherited;
  if (FMouseInCheckBox or FMouseInText) <> b then
    Invalidate;
end; { MouseMove }

procedure TCustomElGroupBox.KeyDown(var Key : Word; Shift : TShiftState);
begin
  inherited;
  if not ShowCheckBox then exit;
  {$ifndef CLX_USED}
  if (Key = VK_SPACE) and (Shift = []) then
  {$else}
  if (Key = KEY_SPACE) and (Shift = []) then
  {$endif}
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(FCheckSound);
{$ENDIF}
    CheckBoxChecked := not CheckBoxChecked;
    Click;
  end;
end; { KeyDown }

procedure TCustomElGroupBox.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Canvas.Font := Font;
  Inc(Rect.Top, GetTopOffset);
  InflateRect(Rect, -1, -1);
  {$ifndef CLX_USED}
  if Ctl3d then
  {$endif}
    InflateRect(Rect, -1, -1);
end;

function TCustomElGroupBox.GetTopOffset: Integer;
var R     : TRect;
{$ifdef MSWINDOWS}
    RClip : TRect;
    sid   : integer;
{$endif}
begin
  SetRectEmpty(R);
{$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.DefaultColor := Color;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.Charset := Font.Charset;
    FRender.Data.DefaultBgColor := clNone;

    FRender.PrepareText(Caption, 0, false);

    Result := Max(FRender.Data.TextSize.cy, GetCheckBoxSize().cy);
  end
  else
{$endif}
  begin
    {$ifdef MSWINDOWS}
    if IsThemeApplied then
    begin
      if not Enabled then
        sid := GBS_DISABLED
      else
        sid := GBS_NORMAL;
      RClip := ClientRect;
      {$ifndef CLX_USED}
      GetThemeTextExtent(Theme, Canvas.Handle, BP_GROUPBOX, sid,
      {$else}
      Canvas.Start;
      GetThemeTextExtent(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_GROUPBOX, sid,
      {$endif}
        PWideChar(WideString(Caption)), Length(WideString(Caption)), DT_LEFT, @RClip, R);
      {$ifdef CLX_USED}
      Canvas.Stop;
      {$endif}
    end
    else
    {$endif}
    begin
      Canvas.Font := Font;
      SetRectEmpty(R);
      {$ifndef CLX_USED}
      {$IFDEF ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Caption), Length(Caption), R, DT_LEFT or DT_CALCRECT);
      {$else}
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, DT_LEFT or DT_CALCRECT);
      {$endif}
      {$else}
      Canvas.TextExtent(Caption, R);
      {$endif}
    end;
    result := Max(R.Bottom - R.Top, GetCheckBoxSize().cy);
  end;
end;

procedure TCustomElGroupBox.SetFlatAlways(Value: Boolean);
begin
  if FFlatAlways <> Value then
  begin
    FFlatAlways := Value;
  end;
end;

{$ifdef CLX_USED}
function TCustomElGroupBox.WidgetFlags: Integer;
begin
  result := Integer(WidgetFlags_WRepaintNoErase);
end;

{$endif}

function TCustomElGroupBox.GetCaptionRect: TRect;
var CheckRect,
    {$ifdef MSWINDOWS}
    RClip,
    {$endif}
    R, R1     : TRect;
    H
    {$ifdef MSWINDOWS}
    , sid
    {$endif}
              : integer;
    {$ifdef MSWINDOWS}
    Flags     : integer;
    {$endif}

begin
  CheckRect := GetCheckRect;
  H := GetLineTopOffset;

  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    if Text <> '' then
    begin
      if CheckRect.Right <> 0 then
        R := Classes.Rect(CheckRect.Right + CheckMargin, 0, CheckRect.Right + CheckMargin + FRender.Data.TextSize.cx, H)
      else
        R := Classes.Rect(8, 0, 8 + FRender.Data.TextSize.cx, H);

      FRender.DrawText(Canvas, Point(0, 0), R, clNone);
      InflateRect(R, 1, 0);

      if ShowFocus and Focused then
      begin
        InflateRect(R, 1, 0);
        Canvas.DrawFocusRect(R);
        InflateRect(R, -1, 0);
      end;

      if CheckRect.Left <> 0 then
        R.Left := CheckRect.Left;
    end
    else
      Result := CheckRect;
  end
  else
  {$endif}
  begin
    with Canvas do
    begin
      Font := Self.Font;
      R := Classes.Rect(0, H div 2, Width, Height);
      R1 := R;

      if (FCaptionColor <> clNone) {$ifdef MSWINDOWS}and (not IsThemeApplied){$endif} then
      begin
        R := Classes.Rect(0, 0, Width, H);
      end;

      if Caption <> '' then
      begin
        {$ifdef MSWINDOWS}
        if IsThemeApplied then
        begin
          if not Enabled then
            sid := GBS_DISABLED
          else
            sid := GBS_NORMAL;

          Flags := DT_LEFT;
          RClip := ClientRect;
          SetRectEmpty(R);
          {$ifndef CLX_USED}
          GetThemeTextExtent(Theme, Canvas.Handle, BP_GROUPBOX, sid,
          {$else}
          Canvas.Start;
          GetThemeTextExtent(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_GROUPBOX, sid,
          {$endif}
            PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, @RClip, R);
          {$ifdef CLX_USED}
          Canvas.Stop;
          {$endif}
          if CheckRect.Left <> 0 then
            OffsetRect(R, CheckRect.Right + CheckMargin, 0)
          else
            OffsetRect(R, 8, 0);

          InflateRect(R, 1, 0);
        end
        else
        {$endif}
        begin
          R := Classes.Rect(0, 0, 0, H);

          {$ifndef CLX_USED}
          Flags := DT_LEFT;
          {$IFDEF ELPACK_UNICODE}
          ElVCLUtils.DrawTextW(Handle, PWideChar(Caption), Length(Caption), R, Flags or DT_CALCRECT);
          {$else}
          DrawText(Handle, PChar(Caption), Length(Caption), R, Flags or DT_CALCRECT);
          {$endif}
          {$else}
          Canvas.TextExtent(Caption, R);
          {$endif}

          if CheckRect.Left <> 0 then
            OffsetRect(R, CheckRect.Right + CheckMargin, 0)
          else
            OffsetRect(R, 8, 0);
        end;
        if CheckRect.Left <> 0 then
          R.Left := CheckRect.Left;
        Result := R;
      end
      else
        Result := CheckRect;
    end;
  end;
end;

function TCustomElGroupBox.GetCheckRect: TRect;
var cbw,
    cbh  : integer;
    ImgIdx : integer;
    i : integer;
    SrcRect : TRect;
    CheckRect: TRect;
    H        : integer;
    {$ifdef MSWINDOWS}
    pid,
    sid      : integer;
    {$endif}
    {$ifdef CLX_USED}
    PX       : TSize;
    {$endif}
begin
  H := GetLineTopOffset;

  if ShowCheckBox then
  begin
    {$ifdef MSWINDOWS}
    if not IsThemeApplied then
    {$endif}
    begin
      i := 0;
      ImgIdx := 0;
      if FUseCustomGlyphs and (not FGlyph.Empty) then
      begin
        cbh := FGlyph.Height;
        cbw := FGlyph.Width div 6;
        if not CheckBoxChecked then
        begin
          if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
          then
            SrcRect := Classes.Rect(0, 0, cbw, cbh)
          else
            SrcRect := Classes.Rect(cbw, 0, cbw * 2, cbh);
        end
        else
        begin
          if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText))) then
            SrcRect := Classes.Rect(cbw * 2, 0, cbw * 3, cbh)
          else
            SrcRect := Classes.Rect(cbw * 3, 0, cbw * 4, cbh);
        end;
      end
      else
      begin
        {$ifndef CLX_USED}
        if FUseImageList then
        begin
          cbw := FImages.Width;
          cbh := FImages.Height;
          if not CheckBoxChecked then
          begin
            if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
            then
              ImgIdx := 0
            else
              ImgIdx := 1;
          end
          else
          begin
            if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
            then
              ImgIdx := 2
            else
              ImgIdx := 3;
          end;
        end
        else
        begin
          cbw := GetCheckBoxSize.cx;
          cbh := GetCheckBoxSize.cy;
        end;
        if not CheckBoxChecked then
          i := DFCS_BUTTONCHECK
        else
          i := DFCS_BUTTONCHECK or DFCS_CHECKED;

        if (not Enabled) or ((FMouseInCheckBox or FMouseInText) and FPressed) then
          i := i or DFCS_INACTIVE;
        {$else}
        if FUseImageList then
        begin
          cbw := FImages.Width;
          cbh := FImages.Height;
          if not CheckBoxChecked then
          begin
            if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
            then
              ImgIdx := 0
            else
              ImgIdx := 1;
          end
          else
          begin
            if Enabled and (not (FPressed and (FMouseInCheckBox or FMouseInText)))
            then
              ImgIdx := 2
            else
              ImgIdx := 3;
          end;
        end
        else
        begin
          QStyle_indicatorSize(Application.Style.Handle, @PX);
          cbw := PX.cx;
          cbh := PX.cy;
        end;
        if CheckBoxChecked then
          i := integer(QButtonToggleState_On)
        else
          i := integer(QButtonToggleState_Off);
        {$endif}
      end;
      CheckRect := Classes.Rect(8, (H div 2 - cbh div 2), 8 + cbw, 0);
      CheckRect.Bottom := CheckRect.Top + cbh;

      if UseCustomGlyphs and (not FGlyph.Empty) then
        DrawGlyph(Canvas, CheckRect, SrcRect)
      else
      begin
        if FUseImageList then
        begin
          FImages.Draw(Canvas, CheckRect.Left, CheckRect.Top, ImgIdx);
        end
        else
        {$ifndef CLX_USED}
        begin
          DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, i);
          if ((Flat and (not (Focused or (FMouseInCheckBox or FMouseInText)))) or FlatAlways) and Enabled then
            DrawFlatFrame(Canvas, CheckRect);
        end;
        {$else}
        Canvas.Start;
        QStyle_DrawIndicator(Application.Style.Handle,
                             Canvas.Handle,
                             CheckRect.Left,
                             CheckRect.Top,
                             CheckRect.Right - CheckRect.Left,
                             CheckRect.Bottom - CheckRect.Top,
                             QWidget_colorGroup(Handle),
                             i,
                             ((FMouseInCheckBox or FMouseInText) and FPressed),
                             Enabled);
        Canvas.stop;
        {$endif}
      end;
    {$ifdef MSWINDOWS}
    end
    else // XP-style checkmarks
    begin
      pid := BP_CHECKBOX;
      if not CheckBoxChecked then
      begin
        if not Enabled then
          sid := CBS_UNCHECKEDDISABLED
        else
        if FPressed and (FMouseInCheckBox or FMouseInText) then
          sid := CBS_UNCHECKEDPRESSED
        else
        if FMouseInCheckBox then
          sid := CBS_UNCHECKEDHOT
        else
          sid := CBS_UNCHECKEDNORMAL;
      end
      else
      begin
        if not Enabled then
          sid := CBS_CHECKEDDISABLED
        else
        if FPressed and (FMouseInCheckBox or FMouseInText) then
          sid := CBS_CHECKEDPRESSED
        else
        if (FMouseInCheckBox or FMouseInText) then
          sid := CBS_CHECKEDHOT
        else
          sid := CBS_CHECKEDNORMAL;
      end;

      cbw := GetCheckBoxSize.cx;
      cbh := GetCheckBoxSize.cy;

      CheckRect := Classes.Rect(8, (H div 2 - cbh div 2), 8 + cbw, 0);
      CheckRect.Bottom := CheckRect.Top + cbh;
      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, pid, sid, CheckRect, nil);
      {$else}
      Canvas.Start;
      DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)),pid, sid, CheckRect, nil);
      Canvas.Stop;
      {$endif}
    {$endif}
    end;

    Result := CheckRect;
  end
  else
    SetRectEmpty(Result);
end;

function TCustomElGroupBox.GetLineTopOffset: Integer;
begin
  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.DefaultColor := Font.Color;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.Charset := Font.Charset;
    FRender.Data.DefaultBgColor := clNone;

    FRender.PrepareText(Caption, 0, false);

    result := FRender.Data.TextSize.cy;
  end
  else
  {$endif}
  begin
    with Canvas do
    begin
      Font := Self.Font;
      result := TextHeight('0');
    end;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElGroupBox.CMEnabledChanged(var Message: TMessage);
{$else}
procedure TCustomElGroupBox.EnabledChanged;
{$endif}
begin
  inherited;
  Invalidate;
end;

procedure TCustomElGroupBox.SetAutoDisableChildren(Value: Boolean);
var i : integer;
begin
  if FAutoDisableChildren <> Value then
  begin
    FAutoDisableChildren := Value;
    if FAutoDisableChildren then
      for I := 0 to ControlCount - 1 do
        Controls[i].Enabled := Enabled and ((not ShowCheckBox) or CheckboxChecked);
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElGroupBox.SetMoneyFlat(Value: Boolean);
begin
  if FMoneyFlat <> Value then
  begin
    FMoneyFlat := Value;
    Invalidate;
  end;
end;

procedure TCustomElGroupBox.SetMoneyFlatInactiveColor(Value: TColor);
begin
  if FMoneyFlatInactiveColor <> Value then
  begin
    FMoneyFlatInactiveColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

procedure TCustomElGroupBox.SetMoneyFlatActiveColor(Value: TColor);
begin
  if FMoneyFlatActiveColor <> Value then
  begin
    FMoneyFlatActiveColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

procedure TCustomElGroupBox.SetMoneyFlatDownColor(Value: TColor);
begin
  if FMoneyFlatDownColor <> Value then
  begin
    FMoneyFlatDownColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

function TCustomElGroupBox.GetMoneyFlat: Boolean;
begin
  Result := FMoneyFlat;
end;

{$endif}

end.





