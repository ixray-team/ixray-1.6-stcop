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

unit ElHstgrm;

interface

uses
{$ifndef CLX_USED}
{$ifdef VCL_6_USED}
  Types,
{$endif}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  extctrls,
  ElImgFrm,
  {$else}
  QGraphics,
  QControls,
  QForms,
  QExtCtrls,
  Qt,
  Types,
  QTypes,
  {$endif}
  ElTools,
  SysUtils,
  Classes;

const
  HISTOBUFFERSIZE = 16384;

type
  THistoBuf = array[0..HISTOBUFFERSIZE - 1] of Integer;
  PHistoBuf = ^THistoBuf;

  THistoDoubleMode = (hdmCumulative, hdmHSplitOppositeIn, hdmHSplitOppositeOut, hdmHSplitSingle,
                      hdmVSplitOpposite, hdmVSplitSingle);

type
  TElHistogram = class;

  {:
  }
  THistoBuffer = class (TObject)
  private
    FBuf: PHistoBuf;
    FBufSize : Integer;
    FElements: Integer;
    FOwner: TElHistogram;
    procedure SetBufSize(newValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aValue : Integer);
    function Average(aSamples : integer): Integer;
    procedure Clear;
    procedure Push(aValue : Integer);
    property BufSize: Integer read FBufSize write SetBufSize;
  end;

  {:
  }
  TElHistogram = class (TGraphicControl)
  private
    FBorderStyle : TBorderStyle;
    Bitmap: TBitmap;
    FBgColor: TColor;
    FBuffer: THistoBuffer;
    FBuffer2: THistoBuffer;
    FColor2: TColor;
    FDoubleMode: THistoDoubleMode;
    FExtBuffer: THistoBuffer;
    FExtBuffer2: THistoBuffer;
    FGrColor: TColor;
    FHGrid: Boolean;
    FScale: Integer;
    FShowZeroValues: Boolean;
    FSmooth: Boolean;
    FUseBuffer2: Boolean;
    FVGrid     : Boolean;
    {$ifndef CLX_USED}
    FImgForm   : TElImageForm;
    FImgFormChLink  : TImgFormChangeLink;

    procedure ImageFormChange(Sender: TObject);
    procedure SetImageForm(newValue : TElImageForm);
    {$endif}
    function GetBufferSize: Integer;
    function GetTransparent: Boolean;
    procedure SetBgColor(aValue: TColor);
    procedure SetBufferSize(newValue: Integer);
    procedure SetColor2(Value: TColor);
    procedure SetDoubleMode(Value: THistoDoubleMode);
    procedure SetGrColor(aValue: TColor);
    procedure SetHGrid(aValue: Boolean);
    procedure SetScale(aValue: Integer);
    procedure SetShowZeroValues(newValue: Boolean);
    procedure SetSmooth(aValue: Boolean);
    procedure SetTransparent(newValue: Boolean);
    procedure SetUseBuffer2(Value: Boolean);
    procedure SetVGrid(aValue: Boolean);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetBorderStyle(newValue : TBorderStyle); virtual;
    {$ifndef CLX_USED}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$endif}
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Buffer: THistoBuffer read FBuffer;
    property Buffer2: THistoBuffer read FBuffer2;
    property ExternalBuffer: THistoBuffer read FExtBuffer write FExtBuffer;
    property ExternalBuffer2: THistoBuffer read FExtBuffer2 write FExtBuffer2;
  published
{$IFDEF VCL_4_USED}
    property Action;
{$ENDIF}
    property Align;
{$IFDEF VCL_4_USED}
    property Anchors;
{$ENDIF}
    property BgColor: TColor read FBgColor write SetBgColor default clBlack;
    property BufferSize: Integer read GetBufferSize write SetBufferSize default
        HISTOBUFFERSIZE;
    property Color default clAqua;
    property Color2: TColor read FColor2 write SetColor2 default clLime;
{$IFDEF VCL_4_USED}
    property Constraints;
{$ifndef CLX_USED}
    property DockOrientation;
{$endif}
{$ENDIF}
    property DoubleMode: THistoDoubleMode read FDoubleMode write SetDoubleMode;
{$ifndef CLX_USED}
    property DragCursor;
{$endif}
{$IFDEF VCL_4_USED}
{$ifndef CLX_USED}
    property DragKind;
{$endif}
{$ENDIF}
    property DragMode;
    property Enabled;
{$IFDEF VCL_4_USED}
{$ifndef CLX_USED}
    property Floating;
{$endif}
{$ENDIF}
    property GridColor: TColor read FGrColor write SetGrColor default clSilver;
    property Height default 30;
    property HGrid: Boolean read FHGrid write SetHGrid default false;
    {$ifndef CLX_USED}
    property ImageForm   : TElImageForm read FImgForm write SetImageForm;
    {$endif}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property ParentColor default false;
    property ParentShowHint;
    property PopupMenu;
    property Scale: Integer read FScale write SetScale default 4096;
    property ShowHint;
    property ShowZeroValues: Boolean read FShowZeroValues write
        SetShowZeroValues;
    property Smooth: Boolean read FSmooth write SetSmooth default false;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property UseBuffer2: Boolean read FUseBuffer2 write SetUseBuffer2;
    property VGrid: Boolean read FVGrid write SetVGrid default false;
    property Visible;
    property Width default 100;
    property BorderStyle : TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;  { Published }
  end;

implementation

{THistoBuffer}

{:
}
{
********************************* THistoBuffer *********************************
}
constructor THistoBuffer.Create;
begin
  inherited;
  SetBufSize(HistoBufferSize);
end;

destructor THistoBuffer.Destroy;
begin
  FreeMem(FBuf);
  FElements := 0;
  inherited;
end;

procedure THistoBuffer.Add(aValue : Integer);
begin
  Inc(FBuf^[0], aValue);
end;

function THistoBuffer.Average(aSamples : integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  if aSamples < 1 then Exit;
  if aSamples > FElements then aSamples := FElements;
  for i := 0 to aSamples -1 do
    Inc(Result, FBuf^[i]);
  Result := Result div aSamples;
end;

procedure THistoBuffer.Clear;
begin
  FillChar(FBuf^, FBufSize * sizeof(Integer), 0);
  FElements := 0;
end;

procedure THistoBuffer.Push(aValue : Integer);
begin
  Move(FBuf^[0], FBuf^[1], (FBufSize - 1) * sizeof(Integer));
  FBuf^[0] := aValue;
  if FElements < FBufSize then Inc(FElements);
  FOwner.Invalidate;
end;

procedure THistoBuffer.SetBufSize(newValue: Integer);
begin
  if (FBufSize <> newValue) and InRange(1, HISTOBUFFERSIZE, newValue) then
  begin
    ReallocMem(FBuf, newValue * sizeof(Integer));
    FBufSize := newValue;
    if FElements > FBufsize then FElements := FBufsize;  
  end;
end;

{TElHistogram}

{:
}
{
********************************* TElHistogram *********************************
}
constructor TElHistogram.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 100;
  Height := 30;
  Color := clAqua;
  Color2 := clLime;
  BgColor := clBlack;
  GridColor := clSilver;
  VGrid := false;
  HGrid := false;
  Scale := 4096;
  Smooth := false;
  FBuffer := THistoBuffer.Create;
  FBuffer.FOwner := self;
  FBuffer.Clear;
  FBuffer2 := THistoBuffer.Create;
  FBuffer2.FOwner := self;
  FBuffer2.Clear;
  {$ifndef CLX_USED}
  FImgFormChLink  := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  Bitmap := TBitmap.Create;
  FBorderStyle := bsSingle;
end;

destructor TElHistogram.Destroy;
begin
  {$ifndef CLX_USED}
  ImageForm := nil;
  FImgFormChLink.Free;
  {$endif}
  Bitmap.Free;
  FBuffer2.Free;
  FBuffer.Free;
  inherited Destroy;
end;

function TElHistogram.GetBufferSize: Integer;
  
  { Returns the value of data member FBufferSize. }
  
begin
  result := FBuffer.BufSize;
end;

function TElHistogram.GetTransparent: Boolean;
  
  { Returns the value of data member FTransparent. }
  
begin
  result := not (csOpaque in ControlStyle);
end;

procedure TElHistogram.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$ifndef CLX_USED}
    if AComponent = FImgForm then
    begin
      ImageForm := nil;
      Invalidate;
    end;
    {$endif}
  end;
end;

procedure TElHistogram.Paint;
var
  R: TRect;
  R1
  {$ifndef CLX_USED}
  ,BgRect
  {$endif}
  : TRect;
  {$ifndef CLX_USED}
  ACtl : TWinControl;
  {$endif}
  i: Integer;
  W: Integer;

  function GetY(i : integer; Ht : integer; Buffer : THistoBuffer) : integer;
  var aVal : integer;
  begin
    try
      if FUseBuffer2 and (FDoubleMode = hdmCumulative)  then
        aVal := FBuffer.FBuf[i] + FBuffer2.FBuf[i]
      else
        aVal := Buffer.FBuf[i];
      Result := MulDiv(Ht, aVal, FScale);
      if (aVal > 0) and (Result = 0) then
         inc(Result);
    except
      Result := 0;
      Exit;
    end;
    if Result < 0 then Result := 0;
    if Result > (Ht - 2) then Result := (Ht - 2);
  end;

  procedure PaintBuffer(R : TRect; const Buffer : THistoBuffer; IsSecondary : boolean);
  var i, hoffs, ht : integer;
  begin
    if FShowZeroValues then
       hoffs := 1
    else
       hoffs := 0;

    ht := R.Bottom - R.Top - hoffs + 1;

    with Bitmap.Canvas do
    begin

      if IsSecondary then
         Pen.Color := Self.Color2
      else
         Pen.Color := self.Color;

      if ((IsSecondary and (FDoubleMode in [hdmVSplitSingle, hdmHSplitOppositeOut, hdmHSplitSingle])) or ((not IsSecondary) and ((not FUseBuffer2) or (FDoubleMode <> hdmHSplitOppositeOut)))) then
      begin
        if R.Right < HISTOBUFFERSIZE then
          W := R.Right
        else
          W := HISTOBUFFERSIZE;

        MoveTo(R.Right - 1, R.Bottom - hoffs - GetY(1, ht, Buffer));
        if FSmooth then
          for i := 1 to W - 2 do
            LineTo(R.Right - i, R.Bottom - hoffs - GetY(i, ht, Buffer))
        else
          for i := 1 to W - 2 do
          begin
            MoveTo(R.Right - i, R.Bottom);
            LineTo(R.Right - i, R.Bottom - hoffs - GetY(i, ht, Buffer));
          end;
      end else
      if IsSecondary and (FDoubleMode = hdmVSplitOpposite) then
      begin
        if R.Right < HISTOBUFFERSIZE then
          W := R.Right
        else
          W := HISTOBUFFERSIZE;

        MoveTo(R.Left + 1, R.Bottom - 1 - GetY(1, ht, Buffer));
        if FSmooth then
          for i := 1 to W - 1 do
            LineTo(R.Left + i, R.Bottom - hoffs - GetY(i, ht, Buffer))
        else
          for i := 1 to W - 1 do
          begin
            MoveTo(R.Left + i, R.Bottom);
            LineTo(R.Left + i, R.Bottom - hoffs - GetY(i, ht, Buffer));
          end;
      end else
      if (IsSecondary and (FDoubleMode = hdmHSplitOppositeIn)) or
         ((not IsSecondary) and FUseBuffer2 and (FDoubleMode = hdmHSplitOppositeOut)) then
      begin
        if R.Right < HISTOBUFFERSIZE then
          W := R.Right
        else
          W := HISTOBUFFERSIZE;

        MoveTo(R.Right - 1, R.Top + hoffs + GetY(1, ht, Buffer));
        if FSmooth then
          for i := 1 to W - 1 do
            LineTo(R.Right - i, R.Top + hoffs + GetY(i, ht, Buffer))
        else
          for i := 1 to W - 1 do
          begin
            MoveTo(R.Right - i, R.Top);
            LineTo(R.Right - i, R.Top + hoffs + GetY(i, ht, Buffer));
          end;
      end;
    end;
  end;

begin
  Bitmap.Width := Width;
  Bitmap.Height := Height;
  if Transparent then
  {$ifndef CLX_USED}
    BitBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height,
        Canvas.Handle, 0, 0, SRCCOPY);
  {$else}
    BitBlt(Bitmap.Handle, 0, 0, QPainter_device(Canvas.Handle), 0, 0, Bitmap.Width, Bitmap.Height,
    RasterOp_CopyROP, true);
  {$endif}
  with Bitmap.Canvas do
  begin
    R := ClientRect;
    //BG
    if not Transparent then
    begin
      {$ifndef CLX_USED}
      if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
      begin
        ACtl := FImgForm.GetRealControl;
        R1 := R;
        BgRect := R;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);

        FImgForm.PaintBkgnd(Handle, R1, Point(BgRect.Left, BgRect.Top), false);
      end else
      {$endif}
      begin
        Brush.Color := FBgColor;
        FillRect(R);
      end;
    end;
    //border
    if BorderStyle = bsSingle then
    begin
      {$ifndef CLX_USED}
      DrawEdge(Handle, R, BDR_SUNKENOUTER, BF_RECT);
      {$else}
      DrawEdge(Canvas, R, esNone, esLowered, [ebLeft, ebTop, ebRight, ebBottom]);
      {$endif}
      InflateRect(R, -1, -1);
    end;

    //grid
    Pen.Color := FGrColor;
    if FVGrid then
      for i := 1 to 3 do
      begin
        MoveTo(MulDiv(R.Right, i, 4), R.Top);
        LineTo(MulDiv(R.Right, i, 4), R.Bottom);
      end;
    if FHGrid then
      for i := 1 to 3 do
      begin
        MoveTo(R.Left, MulDiv(R.Bottom - 2, i, 4));
        LineTo(R.Right, MulDiv(R.Bottom - 2, i, 4));
      end;
    //histo
    InflateRect(R, -1, -1);
    if UseBuffer2 and (FDoubleMode <> hdmCumulative) then
    begin
      R1 := R;
      case FDoubleMode of
        hdmHSplitOppositeIn,
        hdmHSplitOppositeOut,
        hdmHSplitSingle:
          begin
            R1.Top := (R1.Bottom - R1.Top) div 2 + (R1.Bottom - R1.Top) mod 2 + R1.Top;
            R.Bottom := R1.Top - 1;
          end;
        hdmVSplitOpposite,
        hdmVSplitSingle:
          begin
            R1.Left := (R1.Right - R1.Left) div 2 + (R1.Right - R1.Left) mod 2 + R1.Left;
            R.Right := R1.Left - 1;
          end;
      end;
      if FExtBuffer <> nil then
         PaintBuffer(R1, FExtBuffer, false)
      else
         PaintBuffer(R1, FBuffer, false);
      if FExtBuffer2 <> nil then
         PaintBuffer(R, FExtBuffer2, true)
      else
         PaintBuffer(R, FBuffer2, true);
    end else
    begin
      if (FExtBuffer <> nil) then
         PaintBuffer(R, FExtBuffer, false)
      else
         PaintBuffer(R, Buffer, false);
    end;
  end;
  Canvas.Draw(0, 0, Bitmap);
end;

{$ifndef CLX_USED}
procedure TElHistogram.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TElHistogram.SetImageForm(newValue : TElImageForm);
begin
  if FImgForm <> newValue then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnRegisterChanges(FImgFormChLink);
    end;
    if newValue <> nil then
       newValue.FreeNotification(Self);
    FImgForm := newValue;
    if FImgForm <> nil then FImgForm.RegisterChanges(FImgFormChLink);
    Invalidate;
  end;
end;
{$endif}

procedure TElHistogram.SetBgColor(aValue: TColor);
begin
  if FBgColor = aValue then Exit;
  FBgColor := aValue;
  Repaint;
end;

procedure TElHistogram.SetBufferSize(newValue: Integer);
  
  { Sets data member FBufferSize to newValue. }
  
begin
  FBuffer.BufSize := newValue;
  FBuffer2.BufSize := newValue;
end;

procedure TElHistogram.SetColor2(Value: TColor);
begin
  if FColor2 <> Value then
  begin
    FColor2 := Value;
  end;
end;

procedure TElHistogram.SetDoubleMode(Value: THistoDoubleMode);
begin
  if FDoubleMode <> Value then
  begin
    FDoubleMode := Value;
    Repaint;
  end;
end;

procedure TElHistogram.SetGrColor(aValue: TColor);
begin
  if FGrColor = aValue then Exit;
  FGrColor := aValue;
  Repaint;
end;

procedure TElHistogram.SetHGrid(aValue: Boolean);
begin
  if FHGrid = aValue then Exit;
  FHGrid := aValue;
  Repaint;
end;

procedure TElHistogram.SetScale(aValue: Integer);
begin
  if FScale = aValue then Exit;
  FScale := aValue;
  Repaint;
end;

procedure TElHistogram.SetShowZeroValues(newValue: Boolean);
  
  { Sets data member FShowZeroValues to newValue. }
  
begin
  if (FShowZeroValues <> newValue) then
  begin
    FShowZeroValues := newValue;
    Repaint;
  end;  { if }
end;

procedure TElHistogram.SetSmooth(aValue: Boolean);
begin
  if FSmooth = aValue then Exit;
  FSmooth := aValue;
  Repaint;
end;

procedure TElHistogram.SetTransparent(newValue: Boolean);
  
  { Sets data member FTransparent to newValue. }
  
begin
  if Transparent <> newValue then
  begin
    if newValue then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TElHistogram.SetUseBuffer2(Value: Boolean);
begin
  if FUseBuffer2 <> Value then
  begin
    FUseBuffer2 := Value;
    Repaint;
  end;
end;

procedure TElHistogram.SetVGrid(aValue: Boolean);
begin
  if FVGrid = aValue then Exit;
  FVGrid := aValue;
  Repaint;
end;

procedure TElHistogram.SetBorderStyle(newValue : TBorderStyle);
{ Sets data member FBorderStyle to newValue. }
begin
  if (FBorderStyle <> newValue) then
  begin
    FBorderStyle := newValue;
    Invalidate;
  end;  { if }
end;  { SetBorderStyle }

{$ifndef CLX_USED}
procedure TElHistogram.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
{$endif}

end.
