
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

unit ElHint;

{Cool hint window}

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
{$ifdef VCL_6_USED}
Types,
{$endif}
  Controls;

type
  TElHintWindow = class(THintWindow)
  private
    procedure WMNCPAINT(var Msg : TMessage); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    XLoc,
      YLoc : integer;
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(Rect : TRect; const AHint : string); override;
    function CalcHintRect(MaxWidth : Integer; const AHint : string; AData : Pointer) : TRect; override;
  end;

procedure SetHintWindow;

implementation

uses Graphics, Forms;

procedure SetHintWindow;
begin
  Application.ShowHint := false;
  HintWindowClass := TElHintWindow;
  Application.ShowHint := true;
end;

const
  XExt = 10;
  YExt = 10;
  HLW = 25;

function Min (P1, P2: integer): integer;
begin
  if P1 < P2
    then Result := P1
    else Result := P2;
end;

procedure TElHintWindow.WMNCPAINT(var Msg : TMessage);
begin
end;

procedure TElHintWindow.Paint;
var
  R : TRect;
  FSaveStyle : TBrushStyle;
  P : array[0..2] of TPoint;
  lt, rt : integer;
begin
  R := ClientRect;
  lt := (R.Right - XExt) div 4 + XExt div 2;
  rt := lt + min(((R.Right - XExt) div 2), HLW);
  P[0] := Point(lt, 15);
  P[1] := Point(XLoc, YLoc);
  P[2] := Point(rt, 15);
  Canvas.Pen.Color := clBtnText;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Application.HintColor;
  Windows.Polygon(Canvas.Handle, P, 3);
  Dec(R.Bottom, 15);
  OffsetRect(R, 0, 15);
  // draw the shadow
  Dec(R.Bottom, 2);
  Dec(R.Right, 2);
  OffsetRect(R, 2, 2);
  Canvas.Pen.Color := clBtnShadow;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBtnShadow;
  Windows.RoundRect(Canvas.Handle, R.Left, R.Top, R.Right - 2, R.Bottom - 2, XExt, YExt);
  OffsetRect(R, -2, -2);
  // now draw the hint
  FSaveStyle := Brush.Style;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Application.HintColor;
  Canvas.Pen.Color := clBtnText;
  Windows.RoundRect(Canvas.Handle, R.Left, R.Top, R.Right - 2, R.Bottom - 2, XExt, YExt);
  Canvas.Brush.Style := FSaveStyle;
  Canvas.Font.Color := clInfoText;
  InflateRect(R, -2, -2);
  DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  Canvas.Pen.Color := Application.HintColor;
  Canvas.MoveTo(P[0].X, P[0].Y);
  Canvas.LineTo(P[2].X, P[2].Y);
end;

procedure TElHintWindow.ActivateHint(Rect : TRect; const AHint : string);
var
  R : TRect;
  FRgn,
    FRgn1,
    FRgn2,
    FRgn3,
    FRgn4 : HRGN;
var
  P : array[0..2] of TPoint;
  lt, rt : integer;

  function GetCursorHeightMargin : Integer;
  begin
    Result := GetSystemMetrics(SM_CYICON);
  end;

var
  p1 : TPoint;

begin
  OffsetRect(Rect, 0, -GetCursorHeightMargin);
  inherited;
  GetCursorPos(P1);
  R := ClientRect;
  lt := (R.Right - XExt) div 4 + XExt div 2;
  rt := lt + min((R.Right - XExt div 2), HLW);
  P[0] := Point(lt, 15);
  XLoc := P1.X - Rect.Left;
  if Rect.Right > GetSystemMetrics(SM_CXSCREEN) then Dec(XLoc, GetSystemMetrics(SM_CXSCREEN) - Rect.Right);
  YLoc := 0;
  YLoc := P1.Y - Rect.Top;
  if Rect.Bottom > GetSystemMetrics(SM_CYSCREEN) then Dec(YLoc, GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom);
  P[1] := Point(XLoc, YLoc);
  P[2] := Point(rt, 15);
  OffsetRect(R, 0, 15);
  Dec(R.Bottom, 2);
  Dec(R.Right, 2);
  FRgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, XExt, YExt);
  if FRgn = 0 then exit;
  OffsetRect(R, 2, 2);
  FRgn1 := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, XExt, YExt);
  if FRgn1 = 0 then DeleteObject(FRgn);
  FRgn2 := 0;
  CombineRgn(FRgn2, FRgn, FRgn1, RGN_OR);
  if FRgn2 = 0 then
  begin
    DeleteObject(FRgn);
    DeleteObject(FRgn1);
    exit;
  end;
  FRgn3 := CreatePolygonRgn(P, 3, WINDING);
  if FRgn3 = 0 then
  begin
    DeleteObject(FRgn);
    DeleteObject(FRgn1);
    DeleteObject(FRgn2);
    exit;
  end;
  FRgn4 := 0;
  CombineRgn(FRgn4, FRgn2, FRgn3, RGN_OR);
  if FRgn4 = 0 then
  begin
    DeleteObject(FRgn);
    DeleteObject(FRgn1);
    DeleteObject(FRgn2);
    DeleteObject(FRgn3);
    exit;
  end
  else
    SetWindowRgn(Handle, FRgn4, true);
end; {CreateWindowHandle}

function TElHintWindow.CalcHintRect(MaxWidth : Integer; const AHint : string;
  AData : Pointer) : TRect;
begin
  result := inherited CalcHintRect(MaxWidth, AHint, AData);
  Inc(Result.Right, 6);
  if Result.Right - Result.Left < 34 then Result.Right := Result.Left + 34;
  Inc(Result.Bottom, 6 + 15);
end;

procedure TElHintWindow.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

destructor TElHintWindow.Destroy;
begin
  inherited Destroy;
end; {Destroy}

constructor TElHintWindow.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end; {Create}

end.
