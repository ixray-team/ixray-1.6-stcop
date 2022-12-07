
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998 Alex Shovkoplyas              }
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

unit ElBiProgr;

interface

uses
  {$ifndef CLX_USED}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  extctrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  {$else}
  QGraphics,
  QControls,
  QForms,
  QExtCtrls,
  Qt,
  Types,
  QTypes,
  {$endif}
  Classes,
  SysUtils;

type
  TProgrShowMode = (psmAllFull, psmLightHalf, psmDarkHalf, psmAllHalf);
  TElBevelStyle = (ebsNone, ebsLowered, ebsRaised);

type
  TElBiProgressBar = class(TGraphicControl)
  private
    FLightTextFullLine: Boolean;
    FDarkTextFullLine: Boolean;
    FLightButtonStyle : Boolean;
    FDarkButtonStyle : Boolean;
    FMinValue : Integer;
    FBorderStyle : TElBevelStyle;
    FDarkText  : string;
    FLightText : string;
    FDarkTextColor : TColor;
    FLightTextColor : TColor;
    FLightColor : TColor;
    FDarkColor : TColor;
    FScale : integer;
    FLightValue : integer;
    FDarkValue : integer;
    Bitmap : TBitmap;
    FAdditive : boolean;
    FProgrShowMode : TProgrShowMode;
    FCaption       : string;

    procedure SetLightColor(aValue : TColor);
    procedure SetDarkColor(aValue : TColor);
    procedure SetScale(aValue : integer);
    procedure SetLightValue(aValue : integer);
    procedure SetDarkValue(aValue : integer);
    procedure SetAdditive(aValue : boolean);
    procedure SetProgrShowMode(aValue : TProgrShowMode);
    procedure SetDarkText(newValue : string);
    procedure SetLightText(newValue : string);
    procedure SetCaption(newValue : string);
    procedure SetDarkTextColor(newValue : TColor);
    procedure SetLightTextColor(newValue : TColor);
    procedure SetBorderStyle(newValue : TElBevelStyle);
    procedure SetMinValue(newValue : Integer);
    procedure SetLightButtonStyle(newValue : Boolean);
    procedure SetDarkButtonStyle(newValue : Boolean);
    procedure SetTransparent(newValue : Boolean);
    procedure SetLightTextFullLine(newValue : boolean);
    procedure SetDarkTextFullLine(newValue : boolean);
    function GetTransparent : boolean;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetValues(ALightValue, ADarkValue, AScale : integer; AAdditive : boolean);
  published
    property LightColor : TColor read FLightColor write SetLightColor default clRed;
    property DarkColor : TColor read FDarkColor write SetDarkColor default clMaroon;
    property Scale : integer read FScale write SetScale default 100;
    property LightValue : integer read FLightValue write SetLightValue;
    property DarkValue : integer read FDarkValue write SetDarkValue;
    property Additive : boolean read FAdditive write SetAdditive;
    property Caption : string read FCaption write SetCaption;
    property ProgressShowMode : TProgrShowMode read FProgrShowMode write SetProgrShowMode default psmAllFull;
    property LightTextFullLine: Boolean read FLightTextFullLine write SetLightTextFullLine;
    property DarkTextFullLine: Boolean read FDarkTextFullLine write SetDarkTextFullLine;
    property DarkText : string read FDarkText write SetDarkText; { Published }
    property LightText : string read FLightText write SetLightText; { Published }
    property DarkTextColor : TColor read FDarkTextColor write SetDarkTextColor; { Published }
    property LightTextColor : TColor read FLightTextColor write SetLightTextColor; { Published }
    property BorderStyle : TElBevelStyle read FBorderStyle write SetBorderStyle; { Published }
    property MinValue : Integer read FMinValue write SetMinValue default 0;
    property LightButtonStyle : Boolean read FLightButtonStyle write SetLightButtonStyle;
    property DarkButtonStyle : Boolean read FDarkButtonStyle write SetDarkButtonStyle;
    property Transparent : Boolean read GetTransparent write SetTransparent;  { Published }
    property Align;
    property Color default clWindow;
    {$ifndef CLX_USED}
    property DragCursor;
    {$endif}
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default false;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    {$ifndef CLX_USED}
    property DockOrientation;
    property Floating;
    property DragKind;
    {$endif}
{$ENDIF}
  end;

implementation

uses ElTools;

{TELBiProgressBar}

//---------------------------------------------------------------
//                        create/destroy
//---------------------------------------------------------------

constructor TELBiProgressBar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FLightColor := clRed;
  FDarkColor := clMaroon;
  ParentColor := false;
  Color := clWindow;
  Scale := 100;
  FProgrShowMode := psmAllFull;
  Bitmap := TBitmap.Create;
  FMinValue := 0;
  Height := 21;
  Width := 100;
end;

destructor TELBiProgressBar.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

//---------------------------------------------------------------
//                     get/set properties
//---------------------------------------------------------------

procedure TELBiProgressBar.SetCaption(newValue : string);
{ Sets data member FDarkText to newValue. }
begin
  if (FCaption <> newValue) then
  begin
    FCaption := newValue;
    Invalidate;
  end; { if }
end; { SetCaption }

procedure TELBiProgressBar.SetDarkText(newValue : string);
{ Sets data member FDarkText to newValue. }
begin
  if (FDarkText <> newValue) then
  begin
    FDarkText := newValue;
    Invalidate;
  end; { if }
end; { SetDarkText }

procedure TELBiProgressBar.SetLightText(newValue : string);
{ Sets data member FLightText to newValue. }
begin
  if (FLightText <> newValue) then
  begin
    FLightText := newValue;
    Invalidate;
  end; { if }
end; { SetLightText }

procedure TELBiProgressBar.SetDarkTextColor(newValue : TColor);
{ Sets data member FDarkTextColor to newValue. }
begin
  if (FDarkTextColor <> newValue) then
  begin
    FDarkTextColor := newValue;
    Invalidate;
  end; { if }
end; { SetDarkTextColor }

procedure TELBiProgressBar.SetLightTextColor(newValue : TColor);
{ Sets data member FLightTextColor to newValue. }
begin
  if (FLightTextColor <> newValue) then
  begin
    FLightTextColor := newValue;
    Invalidate;
  end; { if }
end; { SetLightTextColor }

procedure TELBiProgressBar.SetBorderStyle(newValue : TElBevelStyle);
{ Sets data member FBorderStyle to newValue. }
begin
  if (FBorderStyle <> newValue) then
  begin
    FBorderStyle := newValue;
    Invalidate;
  end; { if }
end; { SetBorderStyle }

procedure TELBiProgressBar.SetLightColor(aValue : TColor);
begin
  if FLightColor = aValue then Exit;
  FLightColor := aValue;
  Invalidate;
end;

procedure TELBiProgressBar.SetDarkColor(aValue : TColor);
begin
  if FDarkColor = aValue then Exit;
  FDarkColor := aValue;
  Invalidate;
end;

procedure TELBiProgressBar.SetScale(aValue : integer);
begin
  if FScale = aValue then Exit;
  FScale := aValue;
  Invalidate;
end;

procedure TELBiProgressBar.SetLightValue(aValue : integer);
begin
  if FLightValue = aValue then Exit;
  FLightValue := aValue;
  Invalidate;
end;

procedure TELBiProgressBar.SetDarkValue(aValue : integer);
begin
  if FDarkValue = aValue then Exit;
  FDarkValue := aValue;
  Invalidate;
end;

procedure TELBiProgressBar.SetAdditive(aValue : boolean);
begin
  if FAdditive = aValue then Exit;
  FAdditive := aValue;
  Invalidate;
end;

procedure TELBiProgressBar.SetValues(ALightValue, ADarkValue, AScale : integer; AAdditive : boolean);
begin
  FScale := AScale;
  FLightValue := ALightValue;
  FDarkValue := ADarkValue;
  FAdditive := AAdditive;
  Invalidate;
end;

procedure TELBiProgressBar.SetProgrShowMode(aValue : TProgrShowMode);
begin
  if FProgrShowMode <> aValue then
  begin
    FProgrShowMode := aValue;
    Invalidate;
  end;
end;

procedure TELBiProgressBar.SetLightTextFullLine(newValue : boolean);
begin
  if FLightTextFullLine <> newValue then
  begin
    FLightTextFullLine := newValue;
    Invalidate;
  end;
end;

procedure TELBiProgressBar.SetDarkTextFullLine(newValue : boolean);
begin
  if FDarkTextFullLine <> newValue then
  begin
    FDarkTextFullLine := newValue;
    Invalidate;
  end;
end;

//---------------------------------------------------------------
//                            paint
//---------------------------------------------------------------

procedure TELBiProgressBar.Paint;
var
  TR, R : TRect;
  L, D : integer;
  Color1, Color2 : TColor;
  DarkRect,
  LightRect : TRect;

  procedure BevelRect(const R : TRect);
  begin
    with Bitmap.Canvas do
    begin
      Pen.Color := Color1;
      PolyLine([Point(R.Left, R.Bottom), Point(R.Left, R.Top),
        Point(R.Right, R.Top)]);
      Pen.Color := Color2;
      PolyLine([Point(R.Right, R.Top), Point(R.Right, R.Bottom),
        Point(R.Left, R.Bottom)]);
    end;
  end;

begin
  {$ifdef CLX_USED}
  Canvas.Brush.Style := bsSolid;
  {$endif}
  Bitmap.Width := ClientWidth;
  Bitmap.Height := ClientHeight;
  if Transparent then
  {$ifndef CLX_USED}
    BitBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height,
        Canvas.Handle, 0, 0, SRCCOPY);
  {$else}
    BitBlt(Bitmap.Handle, 0, 0, QPainter_device(Canvas.Handle), 0, 0, Bitmap.Width, Bitmap.Height,
    RasterOp_CopyROP, true);
  {$endif}


  R := ClientRect;

  {$ifndef CLX_USED}
  with Bitmap.Canvas do
  {$else}
  with Canvas do
  {$endif}
  begin
    //BG
    if FBorderStyle <> ebsNone then
    begin
      Pen.Width := 1;
      if FBorderStyle = ebsLowered then
      begin
        Color1 := clBtnShadow;
        Color2 := clBtnHighlight;
      end
      else
      begin
        Color1 := clBtnHighlight;
        Color2 := clBtnShadow;
      end;
      BevelRect(Rect(0, 0, Width - 1, Height - 1));
      InflateRect(R, -1, -1);
    end;

    if not Transparent then
    begin
      Brush.Color := Self.Color;
      FillRect(R);
    end;

    if FScale > 0 then
    try
     //dark
      D := MulDiv(DarkValue - MinValue, R.Right, Scale - MinValue);
      if D < 0 then D := 0;
      if D < R.Right then
        R.Right := D
      else
        D := R.Right;
      Brush.Color := FDarkColor;
      if FProgrShowMode in [psmDarkHalf, psmAllHalf] then R.Top := R.Bottom div 2;
      if FDarkButtonStyle then
      begin
        if FDarkValue > 0 then
        begin
          {$ifndef CLX_USED}
          DrawFrameControl(Handle, R, DFC_BUTTON, DFCS_BUTTONPUSH);
          {$else}
          Start;
          QStyle_drawButton(Application.Style.Handle, Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                            QWidget_colorGroup(Parent.Handle), false, QBrushH(nil));
		  Stop;
          {$endif}
        end;
      end
      else if FDarkValue > 0 then
        FillRect(R);
      DarkRect := R;
      R.Left := 1;
      R.Right := Width - 1;

      //light
      R := ClientRect;
      if FBorderStyle <> ebsNone then InflateRect(R, -1, -1);
      L := MulDiv(LightValue - MinValue, R.Right, Scale - MinValue);
      if FProgrShowMode in [psmLightHalf, psmAllHalf] then R.Bottom := R.Bottom div 2;
      if FProgrShowMode = psmAllFull then
        if FAdditive then R.Left := D;
      if (R.Left + L) < R.Right then R.Right := R.Left + L;
      Brush.Color := FLightColor;
      if FLightButtonStyle then
      begin
        if FLightValue > 0 then
          {$ifndef CLX_USED}
          DrawFrameControl(Handle, R, DFC_BUTTON, DFCS_BUTTONPUSH);
          {$else}
          Start;
          QStyle_drawButton(Application.Style.Handle, Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                            QWidget_colorGroup(Parent.Handle), false, QBrushH(nil));
          Stop;
          {$endif}
      end
      else
      if FLightValue > 0 then
        FillRect(R);
      LightRect := R;
      R.Left := 1;
      R.Right := Width - 1;

      // dark text

      Font.Assign(Self.Font);
      Font.Color := FDarkTextColor;
      Brush.Color := clNone;
      Brush.Style := bsClear;
      if Length(FDarkText) > 0 then
      begin
        if not FDarkTextFullLine then TR := DarkRect else
        begin
          TR := Rect(0, 0, 0, 0);
          {$ifndef CLX_USED}
          DrawText(Handle, PChar(FDarkText), -1, TR, DT_CALCRECT);
          {$else}
          Tr.BottomRight := TPoint(Canvas.TextExtent(FDarkText));
          {$endif}
          CenterRects((TR.Right - TR.Left), (R.Right - R.Left), (TR.Bottom - TR.Top), (R.Bottom - R.Top), TR);
          OffsetRect(TR, 0, R.Top);
        end;
        {$ifndef CLX_USED}
        DrawText(Handle, PChar(FDarkText), -1, TR, DT_NOPREFIX or DT_CENTER or DT_SINGLELINE or DT_VCENTER);
        {$else}
        Canvas.TextRect(TR, TR.Left, TR.Top, FDarkText, Integer(AlignmentFlags_AlignCenter) or Integer(AlignmentFlags_SingleLine));
        {$endif}
      end;

      // light text
      Font.Assign(Self.Font);
      Font.Color := FLightTextColor;
      Brush.Color := clNone;
      Brush.Style := bsClear;
      if Length(FLightText) > 0 then
      begin
        if not FLightTextFullLine then TR := LightRect else
        begin
          TR := Rect(0, 0, 0, 0);
          {$ifndef CLX_USED}
          DrawText(Handle, PChar(FLightText), -1, TR, DT_CALCRECT);
          {$else}
          Tr.BottomRight := TPoint(Canvas.TextExtent(FLightText));
          {$endif}
          CenterRects((TR.Right - TR.Left), (R.Right - R.Left), (TR.Bottom - TR.Top), (R.Bottom - R.Top), TR);
          OffsetRect(TR, 0, R.Top);
        end;
        {$ifndef CLX_USED}
        DrawText(Handle, PChar(FLightText), -1, TR, DT_NOPREFIX or DT_CENTER or DT_SINGLELINE or DT_VCENTER);
        {$else}
        Canvas.TextRect(TR, TR.Left, TR.Top, FLightText, Integer(AlignmentFlags_AlignCenter) or Integer(AlignmentFlags_SingleLine));
        {$endif}
      end;

      if FCaption <> '' then
      begin
        Brush.Style := bsClear;
        Font.Assign(Self.Font);
        TR := Rect(0, 0, 0, 0);
        R := ClientRect;
        R.Left := 1;
        R.Right := Width - 1;
        {$ifndef CLX_USED}
        DrawText(Handle, PChar(FCaption), -1, TR, DT_CALCRECT);
        {$else}
        Tr.BottomRight := TPoint(Canvas.TextExtent(FCaption));
        {$endif}
        CenterRects((TR.Right - TR.Left), (R.Right - R.Left), (TR.Bottom - TR.Top), (R.Bottom - R.Top), TR);
        OffsetRect(TR, 0, R.Top);
        {$ifndef CLX_USED}
        DrawText(Handle, PChar(FCaption), -1, TR, DT_NOPREFIX or DT_CENTER or DT_VCENTER);
        {$else}
        Canvas.TextRect(TR, TR.Left, TR.Top, FCaption, Integer(AlignmentFlags_AlignCenter));
        {$endif}
      end;
    except
    end;
  end;
  {$ifndef CLX_USED}
  BitBlt(Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  {$else}
  //bitblt(QPainter_device(Canvas.Handle), 0, 0, QPainter_device(Bitmap.Canvas.Handle), 0, 0, Bitmap.Width, Bitmap.Height, RasterOp_CopyROP, true);
  {$endif}
end;

procedure TELBiProgressBar.SetMinValue(newValue : Integer);
begin
  if (FMinValue <> newValue) then
  begin
    FMinValue := newValue;
    Invalidate;
  end; {if}
end; {SetMinValue}

procedure TELBiProgressBar.SetLightButtonStyle(newValue : Boolean);
begin
  if (FLightButtonStyle <> newValue) then
  begin
    FLightButtonStyle := newValue;
    Invalidate;
  end; {if}
end; {SetLightButtonStyle}

procedure TELBiProgressBar.SetDarkButtonStyle(newValue : Boolean);
begin
  if (FDarkButtonStyle <> newValue) then
  begin
    FDarkButtonStyle := newValue;
    Invalidate;
  end; {if}
end; {SetDarkButtonStyle}

function TElBiProgressBar.GetTransparent;
begin
  result := not (csOpaque in ControlStyle);
end;

procedure TElBiProgressBar.SetTransparent(newValue : Boolean);
{ Sets data member FTransparent to newValue. }
begin
  if Transparent <> newValue then
  begin
    if newValue then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;  { SetTransparent }

end.
