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

02/16/2002

  Optimized resource usage on Win9x

08/20/2001

  Improved AutoSize behaviour

07/26/2001

  Added Unicode support

05/27/2001 (c) Akzhan Abdulin

    Fixed Color property design-time storing issue (clWindow not stored)

*)

unit ElLabel;

interface

uses
  Windows,
  Classes,
  Graphics,
  StdCtrls,
  Messages,
  Controls,
  Forms,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElCLabel,
  ElVCLUtils,
  ElStrUtils;

type
  TElLabelTextStyle = (ltsNormal, ltsEmbossed, ltsRecessed, ltsOutlined);
  TElLabelEffectStyle = (lesNone, lesShadow, lesExtrude);
  TElLabelExtrudePosition = (lepLeft, lepLeftTop, lepTop, lepRightTop, lepRight,
    lepRightBottom, lepBottom, lepLeftBottom);

  TElLabel = class(TElCustomLabel)
  private
    FAngle: integer;
    FBuffer: TBitmap;
    FDarkColor: TColor;
    FDepth: integer;
    FDisabledDarkColor: TColor;
    FDisabledLightColor: TColor;
    FEffect: TElLabelEffectStyle;
    FFarColor: TColor;
    FLightColor: TColor;
    FNearColor: TColor;
    FOutlineColor: TColor;
    FPosition: TElLabelExtrudePosition;
    FShadowColor: TColor;
    FStriated: boolean;
    FStyle: TElLabelTextStyle;
    FXOffset: integer;
    FYOffset: integer;
    procedure SetAngle(Value: integer);
    procedure SetDarkColor(const Value: TColor);
    procedure SetDepth(const Value: integer);
    procedure SetDisabledDarkColor(const Value: TColor);
    procedure SetDisabledLightColor(const Value: TColor);
    procedure SetEffect(const Value: TElLabelEffectStyle);
    procedure SetExtrudePosition(const Value: TElLabelExtrudePosition);
    procedure SetFarColor(const Value: TColor);
    procedure SetLightColor(const Value: TColor);
    procedure SetNearColor(const Value: TColor);
    procedure SetOutlineColor(const Value: TColor);
    procedure SetShadowColor(const Value: TColor);
    procedure SetStriated(const Value: boolean);
    procedure SetStyle(const Value: TElLabelTextStyle);
    procedure SetXOffset(const Value: integer);
    procedure SetYOffset(const Value: integer);
  protected
    FCaption: TElFString;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

{$IFDEF VER100}
    procedure AdjustBounds;
{$else}
    procedure AdjustBounds; override;
{$ENDIF}
    procedure DoDrawText(var Rect: TRect; {$IFDEF B_3}Flags : word{$ELSE}Flags: longint{$ENDIF});
{$IFDEF VCL_4_USED}
    override;
{$ELSE}
{$IFDEF BUILDER_USED}
   override;
{$ENDIF}
{$ENDIF}
    procedure DrawDisabledText(Canvas: TCanvas; var Rect: TRect; Text: TElFSTring; 
        Flags: longint); virtual;
    procedure DrawEffect(Canvas: TCanvas; var Rect: TRect; Flags: longint); dynamic;
    procedure DrawExtrusion(Canvas: TCanvas; var Rect: TRect; Text: TElFSTring; 
        Flags: longint; NearColor, FarColor: TColor);
    procedure DrawNormalText(Canvas: TCanvas; var Rect: TRect; Text: TElFSTring; 
        Flags: longint);
    procedure DrawOutlinedText(Canvas: TCanvas; var Rect: TRect; Text: TElFSTring; 
        Flags: longint; OutlineColor: TColor);
    procedure DrawRaisedText(Canvas: TCanvas; var Rect: TRect; Text: TElFSTring; 
        Flags: longint; LeftTop, RightBottom: TColor);
    procedure DrawShadow(Canvas: TCanvas; var Rect: TRect; Text: TElFSTring; Flags:
        longint; X, Y: integer; ShadowColor: TColor);
    procedure DrawText(Canvas: TCanvas; var Rect: TRect; Flags: longint); dynamic;
    procedure Paint; override;
    procedure SetCaption(newValue: TElFString); virtual;
    procedure SetAutoSize(newValue : Boolean); override;

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
    procedure SetName(const Value: TComponentName); override;

    property Angle: integer read FAngle write SetAngle default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption: TElFString read FCaption write SetCaption;
    property DarkColor: TColor read FDarkColor write SetDarkColor default clBtnShadow;
    property Depth: integer read FDepth write SetDepth default 10;
    property DisabledDarkColor: TColor read FDisabledDarkColor write SetDisabledDarkColor default clBtnShadow;
    property DisabledLightColor: TColor read FDisabledLightColor write SetDisabledLightColor default clBtnHighlight;
    property Effect: TElLabelEffectStyle read FEffect write SetEffect default lesNone;
    property ExtrudePosition: TElLabelExtrudePosition read FPosition write SetExtrudePosition default lepRightBottom;
    property FarColor: TColor read FFarColor write SetFarColor default clBlack;
    property LightColor: TColor read FLightColor write SetLightColor default clBtnHighlight;
    property NearColor: TColor read FNearColor write SetNearColor default clBlack;
    property OutlineColor: TColor read FOutlineColor write SetOutlineColor default clWhite;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property Striated: boolean read FStriated write SetStriated default False;
    property Style: TElLabelTextStyle read FStyle write SetStyle default ltsNormal;
    property XOffset: integer read FXOffset write SetXOffset default 2;
    property YOffset: integer read FYOffset write SetYOffset default 2;
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
    property Transparent default true;
  end;

implementation

{ TElLabel }

constructor TElLabel.Create(AOwner: TComponent);
begin
  inherited;
  FAngle := 0;
  FDarkColor := clBtnShadow;
  FDepth := 10;
  FDisabledDarkColor := clBtnShadow;
  FDisabledLightColor := clBtnHighlight;
  FEffect := lesNone;
  FFarColor := clBlack;
  FLightColor := clBtnHighlight;
  FNearColor := clBlack;
  FOutlineColor := clWhite;
  FPosition := lepRightBottom;
  FShadowColor := clBtnShadow;
  FStriated := False;
  FStyle := ltsNormal;
  FXOffset := 2;
  FYOffset := 2;
  Transparent := true;
end;

destructor TElLabel.Destroy;
begin
  inherited;
end;

procedure TElLabel.DoDrawText(var Rect: TRect; {$IFDEF B_3}Flags : word{$ELSE}Flags: longint{$ENDIF});
var
  EffectRect, TextRect: TRect;
begin
  if FEffect <> lesNone then
  begin
    EffectRect := Rect;
    if DT_CALCRECT and Flags = 0 then
      DrawEffect(FBuffer.Canvas, EffectRect, Flags)
    else
      DrawEffect(Canvas, EffectRect, Flags);
  end;
  TextRect := Rect;
  if DT_CALCRECT and Flags = 0 then
    DrawText(FBuffer.Canvas, TextRect, Flags)
  else
    DrawText(Canvas, TextRect, Flags);
  if FEffect <> lesNone then
    UnionRect(Rect, TextRect, EffectRect)
  else
    Rect := TextRect; 
end;

procedure TElLabel.DrawDisabledText(Canvas: TCanvas; var Rect: TRect; Text: 
    TElFSTring; Flags: longint);
begin
  if DT_CALCRECT and Flags <> 0 then
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags)
    {$else}
    Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags)
    {$endif}
  else
  begin
    Canvas.Font.Color := clBtnHighlight;
    OffsetRect(Rect, 1, 1);
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
    {$else}
    Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    {$endif}
    Canvas.Font.Color := clBtnShadow;
    OffsetRect(Rect, -1, -1);
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags)
    {$else}
    Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags)
    {$endif}
  end;
end;

procedure TElLabel.DrawEffect(Canvas: TCanvas; var Rect: TRect; Flags: longint);
var
  Text: TElFString;
begin
  Text := Caption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
{$IFDEF VCL_4_USED}
  Flags := DrawTextBiDiModeFlags(Flags);
{$ENDIF}
  Canvas.Font := Font;
  case FEffect of
    lesShadow : DrawShadow(Canvas, Rect, Text, Flags, FXOffset, FYOffset, FShadowColor);
    lesExtrude: DrawExtrusion(Canvas, Rect, Text, Flags, FNearColor, FFarColor);
  end;
end;

procedure TElLabel.DrawExtrusion(Canvas: TCanvas; var Rect: TRect; Text: 
    TElFSTring; Flags: longint; NearColor, FarColor: TColor);
const
  XOffsets: array [TElLabelExtrudePosition] of shortint =
    (-1, -1, 0, 1, 1, 1, 0, -1);
  YOffsets: array [TElLabelExtrudePosition] of shortint =
    (0, -1, -1, -1, 0, 1, 1, 1);
var
  CalcRect, R: TRect;
  NearR, NearG, NearB, FarR, FarG, FarB: byte;
  I: integer;
begin
  if DT_CALCRECT and Flags <> 0 then
  begin
    CalcRect := Rect;
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), CalcRect, Flags);
    {$else}
    Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), CalcRect, Flags);
    {$endif}
    R := CalcRect;
    OffsetRect(R, XOffsets[FPosition] * FDepth, YOffsets[FPosition] * FDepth);
    UnionRect(Rect, CalcRect, R);
  end
  else begin
    NearColor := ColorToRGB(NearColor);
    FarColor := ColorToRGB(FarColor);
    NearR := GetRValue(NearColor); NearG := GetGValue(NearColor); NearB := GetBValue(NearColor);
    FarR := GetRValue(FarColor); FarG := GetGValue(FarColor); FarB := GetBValue(FarColor);
    for I := FDepth downto 1 do
    begin
      R := Rect;
      OffsetRect(R, XOffsets[FPosition] * I, YOffsets[FPosition] * I);
      if not FStriated then
        Canvas.Font.Color := RGB(
          Round(NearR + ((FarR - NearR) / (FDepth / I))),
          Round(NearG + ((FarG - NearG) / (FDepth / I))),
          Round(NearB + ((FarB - NearB) / (FDepth / I))))
      else
        Canvas.Font.Color := RGB(
          I * (NearR + ((FarR - NearR) div FDepth)) div (Ord(I mod 2 = 0) * 3 + 1),
          I * (NearG + ((FarG - NearG) div FDepth)) div (Ord(I mod 2 = 0) * 3 + 1),
          I * (NearB + ((FarB - NearB) div FDepth)) div (Ord(I mod 2 = 0) * 3 + 1));
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, Flags);
      {$else}
      Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, Flags);
      {$endif}
    end;
  end;
end;

procedure TElLabel.DrawNormalText(Canvas: TCanvas; var Rect: TRect; Text: 
    TElFSTring; Flags: longint);
begin
  {$ifdef ELPACK_UNICODE}
  ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags)
  {$else}
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags)
  {$endif}
end;

procedure TElLabel.DrawOutlinedText(Canvas: TCanvas; var Rect: TRect; Text: 
    TElFSTring; Flags: longint; OutlineColor: TColor);
const
  XOffsets: array [0..3] of integer = (-1, -1, 1, 1);
  YOffsets: array [0..3] of integer = (-1, 1, -1, 1);
var
  R: TRect;
  I: integer;
begin
  if DT_CALCRECT and Flags <> 0 then
  begin
    R := Rect;
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, Flags)
    {$else}
    Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, Flags)
    {$endif}
  end;
  Canvas.Font.Color := OutlineColor;
  for I := Low(XOffsets) to High(XOffsets) do
  begin
    R := Rect;
    OffsetRect(R, XOffsets[I], YOffsets[I]);
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, Flags);
    {$else}
    Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, Flags);
    {$endif}
  end;
  Canvas.Font.Color := Font.Color;
  {$ifdef ELPACK_UNICODE}
  Windows.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
  {$else}
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  {$endif}
  if DT_CALCRECT and Flags <> 0 then InflateRect(Rect, 1, 1);
end;

procedure TElLabel.DrawRaisedText(Canvas: TCanvas; var Rect: TRect; Text: 
    TElFSTring; Flags: longint; LeftTop, RightBottom: TColor);
var
  R: TRect;
begin
  if DT_CALCRECT and Flags <> 0 then
  begin
    R := Rect;
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, Flags);
    {$else}
    Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, Flags);
    {$endif}
  end;
  R := Rect;
  Canvas.Font.Color := LeftTop;
  OffsetRect(R, -1, -1);
  {$ifdef ELPACK_UNICODE}
  ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, Flags);
  {$else}
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, Flags);
  {$endif}
  Canvas.Font.Color := RightBottom;
  OffsetRect(R, 2, 2);
  {$ifdef ELPACK_UNICODE}
  ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, Flags);
  {$else}
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, Flags);
  {$endif}
  Canvas.Font.Color := Font.Color;
  {$ifdef ELPACK_UNICODE}
  ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
  {$else}
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  {$endif}
  if DT_CALCRECT and Flags <> 0 then InflateRect(Rect, 1, 1);
end;

procedure TElLabel.DrawShadow(Canvas: TCanvas; var Rect: TRect; Text: 
    TElFSTring; Flags: longint; X, Y: integer; ShadowColor: TColor);
begin
  OffsetRect(Rect, X, Y);
  Canvas.Font.Color := ShadowColor;
  {$ifdef ELPACK_UNICODE}
  ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags)
  {$else}
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags)
  {$endif}
end;

procedure TElLabel.DrawText(Canvas: TCanvas; var Rect: TRect; Flags: longint);
var
  Text: TElFString;
begin
  Text := Caption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
{$IFDEF VCL_4_USED}
  Flags := DrawTextBiDiModeFlags(Flags);
{$ENDIF}
  Canvas.Font := Font;
  if Enabled then
    case FStyle of
      ltsNormal  : DrawNormalText(Canvas, Rect, Text, Flags);
      ltsEmbossed: DrawRaisedText(Canvas, Rect, Text, Flags, FLightColor, FDarkColor);
      ltsRecessed: DrawRaisedText(Canvas, Rect, Text, Flags, FDarkColor, FLightColor);
      ltsOutlined: DrawOutlinedText(Canvas, Rect, Text, Flags, FOutlineColor);
    end
  else
    DrawDisabledText(Canvas, Rect, Text, Flags);
end;

procedure TElLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Rect, CalcRect: TRect;
  DrawStyle: longint;
begin
  FBuffer := TBitmap.Create;

  FBuffer.Height := Height; FBuffer.Width := Width;
  BitBlt(FBuffer.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
  with FBuffer.Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    { DoDrawText takes care of BiDi alignments }
    DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment];
    { Calculate vertical layout }
    if Layout <> tlTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if Layout = tlBottom then OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
  end;
  BitBlt(Canvas.Handle, 0, 0, Width, Height, FBuffer.Canvas.Handle, 0, 0, SRCCOPY);
  FBuffer.Free;
end;

procedure TElLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    if Alignment = taRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TElLabel.SetAngle(Value: integer);
begin
  if (Value < 0) or (Value > 359) then
  begin
    Value := Value mod 360;
    if Value < 0 then
      Value := 360 + Value;
  end;
  if FAngle <> Value then
  begin
    FAngle := Value;
    AdjustBounds;
    Invalidate;
  end;  
end;

procedure TElLabel.SetDarkColor(const Value: TColor);
begin
  if FDarkColor <> Value then
  begin
    FDarkColor := Value;
    if (FStyle = ltsEmbossed) or (FStyle = ltsRecessed) then Invalidate;
  end;    
end;

procedure TElLabel.SetDepth(const Value: integer);
begin
  if (FDepth <> Value) and (Value >= 0) then
  begin
    FDepth := Value;
    if FEffect = lesExtrude then
    begin
      AdjustBounds;
      Invalidate;
    end;
  end;
end;

procedure TElLabel.SetDisabledDarkColor(const Value: TColor);
begin
  if FDisabledDarkColor <> Value then
  begin
    FDisabledDarkColor := Value;
    if not Enabled then Invalidate;
  end;  
end;

procedure TElLabel.SetDisabledLightColor(const Value: TColor);
begin
  if FDisabledLightColor <> Value then
  begin
    FDisabledLightColor := Value;
    if not Enabled then Invalidate;
  end;
end;

procedure TElLabel.SetEffect(const Value: TElLabelEffectStyle);
begin
  if FEffect <> Value then
  begin
    FEffect := Value;
    AdjustBounds;
    Invalidate;
  end;  
end;

procedure TElLabel.SetExtrudePosition(const Value: TElLabelExtrudePosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    if FEffect = lesExtrude then
    begin
      AdjustBounds;
      Invalidate;
    end;
  end;  
end;

procedure TElLabel.SetFarColor(const Value: TColor);
begin
  if FFarColor <> Value then
  begin
    FFarColor := Value;
    if FEffect = lesExtrude then Invalidate;
  end;  
end;

procedure TElLabel.SetLightColor(const Value: TColor);
begin
  if FLightColor <> Value then
  begin
    FLightColor := Value;
    if (FStyle = ltsEmbossed) or (FStyle = ltsRecessed) then Invalidate;
  end;  
end;

procedure TElLabel.SetNearColor(const Value: TColor);
begin
  if FNearColor <> Value then
  begin
    FNearColor := Value;
    if FEffect = lesExtrude then Invalidate;
  end;  
end;

procedure TElLabel.SetOutlineColor(const Value: TColor);
begin
  if FOutlineColor <> Value then
  begin
    FOutlineColor := Value;
    if FStyle = ltsOutlined then Invalidate;
  end;  
end;

procedure TElLabel.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    if FEffect = lesShadow then Invalidate;
  end;  
end;

procedure TElLabel.SetStriated(const Value: boolean);
begin
  if FStriated <> Value then
  begin
    FStriated := Value;
    if FEffect = lesExtrude then Invalidate;
  end;  
end;

procedure TElLabel.SetStyle(const Value: TElLabelTextStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    AdjustBounds;
    Invalidate;
  end;  
end;

procedure TElLabel.SetXOffset(const Value: integer);
begin
  if FXOffset <> Value then
  begin
    FXOffset := Value;
    if FEffect = lesShadow then Invalidate;
  end;   
end;

procedure TElLabel.SetYOffset(const Value: integer);
begin
  if FYOffset <> Value then
  begin
    FYOffset := Value;
    if FEffect = lesShadow then Invalidate;
  end;
end;

procedure TElLabel.SetCaption(newValue: TElFString);
{ Sets data member FCaption to newValue. }
begin
  FCaption := newValue;
  inherited Caption := newValue;
  AdjustBounds;
end; { SetCaption }

procedure TElLabel.SetAutoSize(newValue : Boolean);
{ Sets the value for the AutoSize property. }
begin
  if AutoSize <> newValue then
  begin
    inherited;
    {$ifdef VER100}
    AdjustBounds;
    {$endif}
  end;
end;  { SetAutoSize }

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElLabel.CMHintShow(var Message: TMessage);
{$else}
function TElLabel.HintShow(var HintInfo : THintInfo): Boolean; 
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

procedure TElLabel.SetHint(Value: WideString);
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


procedure TElLabel.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (csSetCaption in ControlStyle) and
    not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then
    Caption := Value;
end;

end.

