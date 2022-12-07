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

11/28/2001

  New border style, fbtColorLineBorder, added

11/01/2001

  8K limitation in DrawTextW removed.

10/11/2001

  Overhang was not taken into account in NextLineW. Fixed.
  DrawFlatScrollbars incorrectly calculated the position of scrollbar in control
  that had Ctl3D = false. Fixed.

10/05/2001

  Fixed measurement of single-line DT_WORDWRAP'ed text in DrawTextW 

09/12/2001

  Fixed the problem with fonts synthesized by GDI in DrawTextExW

09/11/2001

  DrawTextExW fixed to support lines with up to 8192 characters

09/08/2001

  Fixed some problems with incorrect rectangle in DrawTextExW

08/22/2001

  DrawTextW added to provide Unicode support in Win 9x. 

07/27/2001

  Added AlphaFill method
  
05/27/2001 (c) Akzhan Abdulin

    DrawButtonFrameEx2 rewritten to support clNone color

*)

unit ElVCLUtils;

interface

uses
{$ifndef CLX_USED}
     Messages,
     Windows,
     Forms,
     Graphics,
     StdCtrls,
     Controls,
     Registry,
{$endif}
     Classes,
     SysUtils,
     ElStrUtils,
{$ifdef ELPACK_UNICODE}
     ElUnicodeStrings,
{$endif}
     ElTools;

const
  MOUSE_WHEEL_DELTA = 120;
  {$ifndef B_5_UP}
  DT_HIDEPREFIX     = $00100000;
  WM_CHANGEUISTATE  = $0127;
  WM_UPDATEUISTATE  = $0128;
  WM_QUERYUISTATE   = $0129;
  {$endif}
  {$ifdef B_4_UP}
  {$EXTERNALSYM SPI_GETKEYBOARDCUES}
  SPI_GETKEYBOARDCUES = $100A;  
  {$else}
  SPI_GETKEYBOARDCUES = $100A;  
  {$endif}

  SC_DRAGMOVE = $F012;
var
  ParentControlRepaintedMessage : DWORD;

{$IFNDEF VCL_4_USED}
type
  TWMMouseWheel = record
    Msg : Cardinal;
    Keys : SmallInt;
    WheelDelta : SmallInt;
    case Integer of
      0 : (
        XPos : Smallint;
        YPos : Smallint);
      1 : (
        Pos : TSmallPoint;
        Result : Longint);
  end;

{$ENDIF}

{$ifndef VCL_4_USED}
type
  TImageIndex = type Integer;
{$else}
{$ifndef B_5_UP}
type
  TImageIndex = type Integer;
{$endif}
{$endif}

type
  TBlendFunction = record
    BlendOp: Byte;
    BlendFlags: Byte;
    SourceConstantAlpha: Byte;
    AlphaFormat: Byte;
  end;

{$ifndef CLX_USED}
type
  TAlphaBlend =
    function(
      hdcDest: HDC;                     // handle to destination DC
      nXOriginDest: Integer;            // x-coord of upper-left corner
      nYOriginDest: Integer;            // y-coord of upper-left corner
      nWidthDest: Integer;              // destination width
      nHeightDest: Integer;             // destination height
      hdcSrc: HDC;                      // handle to source DC
      nXOriginSrc: Integer;             // x-coord of upper-left corner
      nYOriginSrc: Integer;             // y-coord of upper-left corner
      nWidthSrc: Integer;               // source width
      nHeightSrc: Integer;              // source height
      blendFunction: Integer            // alpha-blending function
      ): BOOL; stdcall;
{$endif}

type
  TElBkGndType = (bgtTileBitmap, bgtStretchBitmap, bgtColorFill, bgtCenterBitmap, bgtHorzGradient, bgtVertGradient, bgtTopLeftBitmap);
  TElBorderSide = (ebsLeft, ebsRight, ebsTop, ebsBottom);
  TElBorderSides = set of TElBorderSide;
  TElFlatBorderType = (fbtFlat, fbtSunken, fbtSunkenInner,
                       fbtSunkenOuter, fbtRaised, fbtRaisedInner, fbtRaisedOuter,
                       fbtBump, fbtBigBump, fbtEtched, fbtFramed,
                       fbtLine, fbtLineBorder, fbtNone, fbtColorLineBorder);

  TElTextDrawType = (tdtNormal, tdtShadowed, tdtRaised);
  TElArrowDir = (eadLeft, eadUp, eadRight, eadDown);

  TTaskbarEdge = (tbeBottom, tbeLeft, tbeTop, tbeRight);

  TElTextCase = (etcNoChange, etcUppercase, etcLowercase);


const

  AllBorderSides : TElBorderSides = [ebsLeft, ebsRight, ebsTop, ebsBottom];

{$ifndef CLX_USED}

const
  smXEdge : array[boolean] of DWORD = (SM_CXBORDER, SM_CXEDGE);
  smYEdge : array[boolean] of DWORD = (SM_CYBORDER, SM_CYEDGE);


function GetSysTrayRect : TRect;
function GetTaskbarEdge : TTaskbarEdge;
function GetTaskbarRect : TRect;

procedure MinimizeToTray (Wnd : HWND);
{$endif}

procedure GradientFill(DC : HDC; R : TRect; StartColor, EndColor : TColor; Steps : integer; Vertical : boolean);

procedure GradientFillEx(DC : HDC; DCRect, R : TRect; Origin : TPoint;
                         StartColor, EndColor : TColor; Steps : integer;
                         Vertical : boolean);
// DCRect - the rectangle of the "real DC", i.e. the DC, on which the painting is
//          usually performed
// R      - the real rectangle to be painted
// Origin - The original point, i.e. the point, which, when addded to R, would have
//          been copied from the "real DC"

function GetTopOwnerControl(Component : TComponent) : TControl;
function GetOwnerForm(Component : TComponent) : TForm;

function GetKeybTimes(TimeKind : integer) : integer;
// correct parameter values are SPI_GETKEYBOARDDELAY and SPI_GETKEYBOARDSPEED

{$ifndef CLX_USED}
function FindVCLChild(Control : TWinControl; ChildHandle : HWND) : TWinControl;

function HitTest(R : TRect; Pt : TPoint; CornerSize, BorderSize : integer) : integer;

//procedure FillSolidRect2(DC : QPainterH; Rect : TRect; Color : TColor);
//procedure FillSolidRect(DC : QPainterH; x, y, cx, cy : integer; Color : TColor);
{$endif}


procedure FillSolidRect2(DC : HDC; Rect : TRect; Color : TColor);
procedure FillSolidRect(DC : HDC; x, y, cx, cy : integer; Color : TColor);

procedure TiledPaint(Canvas : TCanvas; Bitmap : TBitmap; Rect : TRect);
// destination rectangle is meant.

procedure DrawButtonFrame(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean);
procedure DrawButtonFrameEx(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean;
                            ButtonColor : TColor; Thin : boolean);
procedure DrawButtonFrameEx2(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean;
                            ButtonColor : TColor; Thin : boolean; clrHighlight, clrDkShadow, clrFace, clrShadow : TColor);
procedure DrawButtonFrameEx3(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean;
                            ButtonColor : TColor; Thin : boolean; BorderSides : TElBorderSides);
procedure Draw3dBorder(DC : HDC; rc : TRect; nColor1, nColor2, nColor3, nColor4 : TColor);

procedure Draw3dRectEx(DC : HDC; x, y, cx, cy : integer; clrTopLeft,
{$ifndef CLX_USED}
  clrBottomRight : COLORREF;
{$endif}
  BorderSides : TElBorderSides);

{$ifndef CLX_USED}
function DrawBevel(DC : HDC; R : TRect; Color1, Color2 : TColor; Sides : TElBorderSides) : TRect;
{$endif}

{$ifndef CLX_USED}
function DrawFlatFrame(DC : HDC; R : TRect; BkColor : TColor; Focused : boolean) : TRect;
function DrawFlatFrame2(DC : HDC; R : TRect; BkColor : TColor; Focused : boolean; BorderSides : TElBorderSides) : TRect;
procedure DrawFlatFrameEx(DC : HDC; R : TRect; BkColor : TColor; Focused, Enabled : boolean);
procedure DrawFlatFrameEx2(DC : HDC; R : TRect; Color, BkColor : TColor;
                           Focused, Enabled : boolean;
                           BorderSides : TElBorderSides;
                           BorderType : TElFlatBorderType);
{$endif}

{$ifndef CLX_USED}
function DrawFlatScrollbars(
        {$ifndef CLX_USED}
        Wnd : HWND; DC : HDC;
        {$else}
        Wnd : QWidgetH; DC : QPainterH;
        {$endif}
        Rect : TRect; Focused : boolean;
        ScrollBars : TScrollStyle; DragHorz, DragVert : boolean;
        IsControl : boolean; Style, ExStyle : integer): TRect;
{$endif}

procedure DrawTypedText(Canvas : TCanvas; Bounds : TRect; Text : string; Flags : integer; DrawType : TElTextDrawType);
procedure DrawTypedTextW(Canvas : TCanvas; Bounds : TRect; Text : WideString; Flags : integer; DrawType : TElTextDrawType);

procedure DrawFlatScrollbarThumb(DC : HDC; rc : TRect; Focused : boolean);

{$ifndef CLX_USED}
procedure DrawTransparentBitmapEx(DC : HDC; Bitmap : TBitmap; X, Y : integer; Src : TRect; Transparent : TColor);
{$endif}

procedure DrawArrow(Canvas : TCanvas; Dir : TElArrowDir; R : TRect; Color : TColor; Enabled : boolean);

function RectsIntersect(R1, R2 : TRect) : boolean;

{$ifndef CLX_USED}

function GetDesktopTop    : integer;
function GetDesktopLeft   : integer;
function GetDesktopBottom : integer;
function GetDesktopRight  : integer;
function GetDesktopRect   : TRect;

function GetWorkSpaceTop    : integer;
function GetWorkSpaceLeft   : integer;
function GetWorkSpaceBottom : integer;
function GetWorkSpaceRight  : integer;
function GetWorkSpaceRect   : TRect;
{$endif}

const
  WaitCursor: TCursor = crHourGlass;

procedure StartWait;
procedure StopWait;

function InvertColor(aColor: TColor): TColor;

function RGBtoHLS(rgbc: integer): integer;
function HLStoRGB(hlsc: integer): integer;

{$ifndef CLX_USED}
procedure DrawFlatScrollBarsEx(
        {$ifndef CLX_USED}
        Wnd : HWND; DC : HDC;
        {$endif}
        Rect : TRect; Focused : boolean; ScrollBars : TScrollStyle;
        DragHorz, DragVert : boolean; IsControl : boolean;
        BkColor, DitherColor,  ButtonColor, ArrowColor, HotButtonColor : TColor;
        DrawFrames, DitherBack : boolean);

procedure DrawFlatScrollBarEx(
        {$ifndef CLX_USED}
        Wnd : HWND; DC : HDC;
        {$endif}
        Rect : TRect; nType : integer; bScrollbarCtrl : boolean;
        Dragging : boolean; Focused : boolean;
        BkColor, DitherColor, ButtonColor, ArrowColor, HotButtonColor : TColor;
        DrawFrames, DitherBack : boolean);

function AlphaBlend(
  hdcDest: HDC;                     // handle to destination DC
  nXOriginDest: Integer;            // x-coord of upper-left corner
  nYOriginDest: Integer;            // y-coord of upper-left corner
  nWidthDest: Integer;              // destination width
  nHeightDest: Integer;             // destination height
  hdcSrc: HDC;                      // handle to source DC
  nXOriginSrc: Integer;             // x-coord of upper-left corner
  nYOriginSrc: Integer;             // y-coord of upper-left corner
  nWidthSrc: Integer;               // source width
  nHeightSrc: Integer;              // source height
  SourceConstantAlpha: Byte;        // Specifies an alpha transparency value to be used on the entire source bitmap
  srcAlpha : byte
  ): Boolean;
{$endif}

{$ifndef CLX_USED}
procedure AlphaCopyRect(DestCanvas : TCanvas; Dest: TRect; SourceCanvas: TCanvas; Source: TRect; AlphaLevel : byte; UseAlphaLevel : boolean);
procedure AlphaFillRect(Canvas : TCanvas; Rect : TRect; Color : TColor; AlphaLevel : byte);
{$endif}

{$ifndef CLX_USED}
function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: UINT): Integer;
function GetSysColorPen(Color : COLORREF) : HPEN;
{$endif}

function IncColor(const Color : TColor; RInc, GInc, BInc : integer) : integer;

procedure DrawFocus(Canvas : TCanvas; R : TRect);

{$ifndef CLX_USED}
function Win2KHideUIState : boolean;
{$endif}

function ModalFormVisible : boolean;

{$ifndef CLX_USED}
function ShiftStateToKeyData(Shift : TShiftState) : integer;
{$endif}

function GetTimeAMChar : char;
function GetTimePMChar : char;

{$ifdef BUILDER_USED}
{$EXTERNALSYM GetGValue}
function GetGValue(aColor : TColor) : byte;
{$EXTERNALSYM GetBValue}
function GetBValue(aColor : TColor) : byte;
{$EXTERNALSYM GetRValue}
function GetRValue(aColor : TColor) : byte;
{$EXTERNALSYM RGB}
function RGB(R, G, B : byte) : TColor;
{$else}
function GetGValue(aColor : TColor) : byte;
function GetBValue(aColor : TColor) : byte;
function GetRValue(aColor : TColor) : byte;
function RGB(R, G, B : byte) : TColor;
{$endif}

{$ifdef ELPACK_UNICODE}
function GetShortHintW(Hint : WideString) : WideString;
{$endif}

implementation

function GetTimeAMChar : char;
begin
  if Length(TimeAMString) > 0 then
    result := TimeAMString[1]
  else
    result := #0;
end;

function GetTimePMChar : char;
begin
  if Length(TimePMString) > 0 then
    result := TimePMString[1]
  else
    result := #0;
end;

function IncColor(const Color : TColor; RInc, GInc, BInc : integer) : integer;
var r, g, b : integer;
begin
  r := (ColorToRgb(Color) and $0000ff) + RInc;
  g := (ColorToRgb(Color) and $00ff00) shr 8 + GInc;
  b := (ColorToRgb(Color) and $ff0000) shr 16 + BInc;

  if r < 0 then r := 0 else if r > 255 then r := 255;
  if g < 0 then g := 0 else if g > 255 then g := 255;
  if b < 0 then b := 0 else if b > 255 then b := 255;
  result := (b shl 16) or (g shl 8) or r;
end;

function GetBValue(aColor : TColor) : Byte;
begin
  result := (ColorToRgb(aColor) and $ff0000) shr 16;
end;

function GetGValue(aColor : TColor) : Byte;
begin
  result := (ColorToRgb(aColor) and $00ff00) shr 8;
end;

function GetRValue(aColor : TColor) : Byte;
begin
  result := (ColorToRgb(aColor) and $0000ff);
end;

function RGB(R, G, B : Byte) : TColor;
begin
  result := (b shl 16) or (g shl 8) or r;
end;

function InvertColor(aColor: TColor): TColor;
var
  r,g,b: integer;
begin
  r := GetRValue(ColorToRgb(aColor));
  g := GetGValue(ColorToRgb(aColor));
  b := GetBValue(ColorToRgb(aColor));

  r := (not r) and $000000FF;
  g := ((not g) and $000000FF) shl 8;
  b := ((not b) and $000000FF) shl 16;

  Result := b or g or r;
end;

{$ifndef CLX_USED}
function GetWorkSpaceRect : TRect;

const SM_XVIRTUALSCREEN  = 76;
      SM_YVIRTUALSCREEN  = 77;
      SM_CXVIRTUALSCREEN = 78;
      SM_CYVIRTUALSCREEN = 79;


begin
  if IsWin98Up or IsWin2000Up then
  begin
    result.Left  := 0;
    result.Top   := 0;
    result.Right := GetSystemMetrics(SM_CXVIRTUALSCREEN);
    result.Bottom := GetSystemMetrics(SM_CYVIRTUALSCREEN);
    OffsetRect(Result, Getsystemmetrics(SM_XVIRTUALSCREEN), GetSystemMetrics(SM_YVIRTUALSCREEN));
  end
  else
  begin
    result.Left  := 0;
    result.Top   := 0;
    result.Right := GetSystemMetrics(SM_CXSCREEN);
    result.Bottom := GetSystemMetrics(SM_CYSCREEN);
  end;
end;

function GetWorkSpaceTop  : integer;
begin
  result := GetWorkSpaceRect.Top;
end;

function GetWorkSpaceLeft   : integer;
begin
  result := GetWorkSpaceRect.Left;
end;

function GetWorkSpaceBottom : integer;
begin
  result := GetWorkSpaceRect.Bottom;
end;

function GetWorkSpaceRight  : integer;
begin
  result := GetWorkSpaceRect.Right;
end;

function GetDesktopRect : TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @result, 0);
end;

function GetDesktopTop  : integer;
begin
  result := GetDesktopRect.Top;
end;

function GetDesktopLeft   : integer;
begin
  result := GetDesktopRect.Left;
end;

function GetDesktopBottom : integer;
begin
  result := GetDesktopRect.Bottom;
end;

function GetDesktopRight  : integer;
begin
  result := GetDesktopRect.Right;
end;

procedure MinimizeToTray (Wnd : HWND);
var
  WinPlace   : TWindowPlacement;
  R          : TRect;
begin
  WinPlace.length   := SizeOf(TWindowPlacement);
  GetWindowPlacement(Wnd, @WinPlace);
  WinPlace.flags    := WPF_SETMINPOSITION;
  R := GetSysTrayRect;
  WinPlace.ptMinPosition.x := R.Left;
  WinPlace.ptMinPosition.y := R.Top;
  SetWindowPlacement(Wnd, @WinPlace);
  SendMessage(Wnd, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

function GetSysTrayRect : TRect;
var H1,
    H2 : HWND;
begin
  SetRectEmpty(result);
  H1 := FindWindow('Shell_TrayWnd', nil);
  if H1 <> 0 then
  begin
    H2 := FindWindowEx(H1, 0, 'TrayNotifyWnd', nil);
    if H2 <> 0 then
       GetWindowRect(H2, result);
  end;
end;

function GetTaskbarRect : TRect;
var H1 : HWND;
begin
  H1 := FindWindow('Shell_TrayWnd', nil);
  if H1 <> 0 then
     GetWindowRect(H1, Result)
  else
     SetRectEmpty(result);
end;

function GetTaskbarEdge : TTaskbarEdge;
var H1 : HWND;
    R  : TRect;
    DesktopWidth,
    DesktopHeight  : integer;
begin
  result := tbeBottom;
  H1 := FindWindow('Shell_TrayWnd', nil);
  if H1 <> 0 then
  begin
    R := GetWorkSpaceRect;
    DesktopWidth := R.Right - R.Left;
    DesktopHeight:= R.Bottom - R.Top;
    GetWindowRect(H1, R);
    if R.Top > DesktopHeight div 2 then result := tbeBottom else
    if R.Right < DesktopWidth div 2 then result := tbeLeft else
    if R.Bottom < DesktopHEight div 2 then result := tbeTop else
    if R.Left > DesktopWidth div 2 then result := tbeRight;
  end;
end;

{$endif}

{$warnings off}
function GetKeybTimes(TimeKind : integer) : integer;
begin
  {$ifndef CLX_USED}
  SystemParametersInfo(TimeKind, 0, @result, 0);
  if TimeKind = SPI_GETKEYBOARDSPEED then
  begin
    result := 1000 div (result + 1);
  end else
  if TimeKind = SPI_GETKEYBOARDDELAY then
  begin
    result := 250 * (result + 1);
  end;
  {$endif}
end;
{$warnings on}

procedure GradientFillEx(DC : HDC; DCRect, R : TRect; Origin : TPoint; StartColor, EndColor : TColor; Steps : integer; Vertical : boolean);
var
  i : integer;
  RBeg, RDif, Rc,
    GBeg, GDif, Gc,
    BBeg, BDif, Bc : integer;
  {$ifndef CLX_USED}
  Brush,
  OldBrush    : HBrush;
  {$endif}
  R1, R2, R3  : TRect;
begin
  if StartColor = EndColor then
  begin
    {$ifndef CLX_USED}
    Brush := CreateSolidBrush(ColorToRGB(StartColor));
    FillRect(DC, R, Brush);
    DeleteObject(Brush);
    {$endif}
  end
  else
  begin
    RBeg := GetRValue(ColorToRGB(StartColor));
    GBeg := GetGValue(ColorToRGB(StartColor));
    BBeg := GetBValue(ColorToRGB(StartColor));
    RDif := GetRValue(ColorToRGB(EndColor)) - RBeg;
    GDif := GetGValue(ColorToRGB(EndColor)) - GBeg;
    BDif := GetBValue(ColorToRGB(EndColor)) - BBeg;
    R1 := R;
    for i := 0 to Steps - 1 do // Iterate
    begin
      if Vertical then
      begin
        R1.Top := R.Top + MulDiv(i, R.Bottom - R.Top, Steps);
        R1.Bottom := R.Top + MulDiv(i + 1, R.Bottom - R.Top, Steps);
      end else
      begin
        R1.Left := R.Left + MulDiv(i, R.Right - R.Left, Steps);
        R1.Right := R.Left + MulDiv(i + 1, R.Right - R.Left, Steps);
      end;
      R3 := DCRect;
      OffsetRect(R3, Origin.X, Origin.Y);
      IntersectRect(R2, R3, R1);
      if not IsRectEmpty(R2) then
      begin
        //R2 := R1;
        OffsetRect(R2, -Origin.x, -Origin.y);
        Rc := RBeg + MulDiv(i, RDif, Steps - 1);
        Gc := GBeg + MulDiv(i, GDif, Steps - 1);
        Bc := BBeg + MulDiv(i, BDif, Steps - 1);

        {$ifndef CLX_USED}
        Brush := CreateSolidBrush(RGB(Rc, Gc, Bc));
        OldBrush := SelectObject(DC, Brush);
        PatBlt(DC, R2.Left, R2.Top, R2.Right - R2.Left, R2.Bottom - R2.Top, PATCOPY);
        SelectObject(DC, OldBrush);
        DeleteObject(Brush);
        {$endif}
      end;
    end; // for
  end;
end;

procedure GradientFill(DC : HDC; R : TRect; StartColor, EndColor : TColor; Steps : integer; Vertical : boolean);
var
  i : integer;
  RBeg, RDif, Rc,
    GBeg, GDif, Gc,
    BBeg, BDif, Bc : integer;

  {$ifndef CLX_USED}
  Brush,
  OldBrush    : HBrush;
  {$endif}

  R1 : TRect;
begin
  if StartColor = EndColor then
  begin
    {$ifndef CLX_USED}
    Brush := CreateSolidBrush(ColorToRGB(StartColor));
    FillRect(DC, R, Brush);
    DeleteObject(Brush);
    {$endif}
  end
  else
  begin
    RBeg := GetRValue(ColorToRGB(StartColor));
    GBeg := GetGValue(ColorToRGB(StartColor));
    BBeg := GetBValue(ColorToRGB(StartColor));
    RDif := GetRValue(ColorToRGB(EndColor)) - RBeg;
    GDif := GetGValue(ColorToRGB(EndColor)) - GBeg;
    BDif := GetBValue(ColorToRGB(EndColor)) - BBeg;
    R1 := R;
    for i := 0 to Steps - 1 do // Iterate
    begin
      if Vertical then
      begin
        R1.Top := R.Top + MulDiv(i, R.Bottom - R.Top, Steps);
        R1.Bottom := R.Top + MulDiv(i + 1, R.Bottom - R.Top, Steps);
      end else
      begin
        R1.Left := R.Left + MulDiv(i, R.Right - R.Left, Steps);
        R1.Right := R.Left + MulDiv(i + 1, R.Right - R.Left, Steps);
      end;

      Rc := RBeg + MulDiv(i, RDif, Steps - 1);
      Gc := GBeg + MulDiv(i, GDif, Steps - 1);
      Bc := BBeg + MulDiv(i, BDif, Steps - 1);

      {$ifndef CLX_USED}
      Brush := CreateSolidBrush(RGB(Rc, Gc, Bc));
      OldBrush := SelectObject(DC, Brush);
      PatBlt(DC, R1.Left, R1.Top, R1.Right - R1.Left, R1.Bottom - R1.Top, PATCOPY);
      SelectObject(DC, OldBrush);
      DeleteObject(Brush);
      {$endif}
    end; // for
  end;
end;

procedure DrawArrow(Canvas : TCanvas; Dir : TElArrowDir; R : TRect; Color : TColor; Enabled : boolean);
var FSavePenColor : TColor;
    DestRect : TRect;
begin
  FSavePenColor := Canvas.Pen.Color;
  if Enabled then
    Canvas.Pen.Color := Color
  else
    Canvas.Pen.Color := clBtnHighlight;
  case Dir of
    eadRight:
      begin
        if (R.Right - R.Left < 3) or (R.Bottom - R.Top < 6) then exit;
        CenterRects(4, R.Right - R.Left, 7, R.Bottom - R.Top, DestRect);
        if not Enabled then
           OffsetRect(DestRect, R.Left + 1, R.Top + 1)
        else
           OffsetRect(DestRect, R.Left, R.Top);

        with Canvas do
        begin
          MoveTo(DestRect.Left, DestRect.Top);
          LineTo(DestRect.Left, DestRect.Top + 7);
          MoveTo(DestRect.Left + 1, DestRect.Top + 1);
          LineTo(DestRect.Left + 1, DestRect.Top + 6);
          MoveTo(DestRect.Left + 2, DestRect.Top + 2);
          LineTo(DestRect.Left + 2, DestRect.Top + 5);
          MoveTo(DestRect.Left + 3, DestRect.Top + 3);
          LineTo(DestRect.Left + 3, DestRect.Top + 4);
          (*
          {$ifndef CLX_USED}
          Pixels[DestRect.Left + 3, DestRect.Top + 3] := Canvas.Pen.Color;
          {$else}
          DrawPoint(DestRect.Left + 3, DestRect.Top + 3);
          {$endif}
          *)
          if not Enabled then
          begin
            Canvas.Pen.Color := clBtnShadow;
            OffsetRect(DestRect, -1, -1);
            MoveTo(DestRect.Left, DestRect.Top);
            LineTo(DestRect.Left, DestRect.Top + 7);
            MoveTo(DestRect.Left + 1, DestRect.Top + 1);
            LineTo(DestRect.Left + 1, DestRect.Top + 6);
            MoveTo(DestRect.Left + 2, DestRect.Top + 2);
            LineTo(DestRect.Left + 2, DestRect.Top + 5);
            MoveTo(DestRect.Left + 3, DestRect.Top + 3);
            LineTo(DestRect.Left + 3, DestRect.Top + 4);

            (*
            {$ifndef CLX_USED}
            Pixels[DestRect.Left + 3, DestRect.Top + 3] := Canvas.Pen.Color;
            {$else}
            DrawPoint(DestRect.Left + 3, DestRect.Top + 3);
            {$endif}
            *)
          end;
        end;
      end;
    eadUp:
      begin
        if (R.Right - R.Left < 6) or (R.Bottom - R.Top < 3) then exit;
        CenterRects(7, R.Right - R.Left, 4, R.Bottom - R.Top, DestRect);
        if not Enabled then
           OffsetRect(DestRect, R.Left + 1, R.Top + 1)
        else
           OffsetRect(DestRect, R.Left, R.Top);
        with Canvas do
        begin
          //mdm - Decremented all "LineTo(DestRect.Left" by one to draw properly
          MoveTo(DestRect.Left, DestRect.Top + 3);
          LineTo(DestRect.Left + 7, DestRect.Top + 3);

          MoveTo(DestRect.Left + 1, DestRect.Top + 2);
          LineTo(DestRect.Left + 6, DestRect.Top + 2);

          MoveTo(DestRect.Left + 2, DestRect.Top + 1);
          LineTo(DestRect.Left + 5, DestRect.Top + 1);

          MoveTo(DestRect.Left + 3, DestRect.Top);
          LineTo(DestRect.Left + 4, DestRect.Top);

          (*
          {$ifndef CLX_USED}
          Pixels[DestRect.Left + 3, DestRect.Top] := Canvas.Pen.Color;
          {$else}
          DrawPoint(DestRect.Left + 3, DestRect.Top);
          {$endif}
          *)

          if not Enabled then
          begin
            Canvas.Pen.Color := clBtnShadow;
            OffsetRect(DestRect, -1, -1);
            MoveTo(DestRect.Left, DestRect.Top + 3);
            LineTo(DestRect.Left + 7, DestRect.Top + 3);

            MoveTo(DestRect.Left + 1, DestRect.Top + 2);
            LineTo(DestRect.Left + 6, DestRect.Top + 2);

            MoveTo(DestRect.Left + 2, DestRect.Top + 1);
            LineTo(DestRect.Left + 5, DestRect.Top + 1);

            MoveTo(DestRect.Left + 3, DestRect.Top);
            LineTo(DestRect.Left + 4, DestRect.Top);

            (*
            {$ifndef CLX_USED}
            Pixels[DestRect.Left + 3, DestRect.Top] := Canvas.Pen.Color;
            {$else}
            DrawPoint(DestRect.Left + 3, DestRect.Top);
            {$endif}
            *)
          end;
        end;
      end;
    eadLeft:
      begin
        if (R.Right - R.Left < 3) or (R.Bottom - R.Top < 6) then exit;
        CenterRects(4, R.Right - R.Left, 7, R.Bottom - R.Top, DestRect);
        if not Enabled then
           OffsetRect(DestRect, R.Left + 1, R.Top + 1)
        else
           OffsetRect(DestRect, R.Left, R.Top);
        with Canvas do
        begin
          MoveTo(DestRect.Left + 3, DestRect.Top);
          LineTo(DestRect.Left + 3, DestRect.Top + 7);
          MoveTo(DestRect.Left + 2, DestRect.Top + 1);
          LineTo(DestRect.Left + 2, DestRect.Top + 6);
          MoveTo(DestRect.Left + 1, DestRect.Top + 2);
          LineTo(DestRect.Left + 1, DestRect.Top + 5);

          MoveTo(DestRect.Left, DestRect.Top + 3);
          LineTo(DestRect.Left, DestRect.Top + 4);

          (*
          {$ifndef CLX_USED}
          Pixels[DestRect.Left, DestRect.Top + 3] := Canvas.Pen.Color;
          {$else}
          DrawPoint(DestRect.Left, DestRect.Top + 3);
          {$endif}
          *)
          if not Enabled then
          begin
            Canvas.Pen.Color := clBtnShadow;
            OffsetRect(DestRect, -1, -1);
            MoveTo(DestRect.Left + 3, DestRect.Top);
            LineTo(DestRect.Left + 3, DestRect.Top + 7);
            MoveTo(DestRect.Left + 2, DestRect.Top + 1);
            LineTo(DestRect.Left + 2, DestRect.Top + 6);
            MoveTo(DestRect.Left + 1, DestRect.Top + 2);
            LineTo(DestRect.Left + 1, DestRect.Top + 5);

            MoveTo(DestRect.Left, DestRect.Top + 3);
            LineTo(DestRect.Left, DestRect.Top + 4);

            (*
            {$ifndef CLX_USED}
            Pixels[DestRect.Left, DestRect.Top + 3] := Canvas.Pen.Color;
            {$else}
            DrawPoint(DestRect.Left, DestRect.Top + 3);
            {$endif}
            *)
          end;
        end;
      end;
    eadDown:
      begin
        if (R.Right - R.Left < 6) or (R.Bottom - R.Top < 3) then exit;
        CenterRects(7, R.Right - R.Left, 4, R.Bottom - R.Top, DestRect);
        if not Enabled then
           OffsetRect(DestRect, R.Left + 1, R.Top + 1)
        else
           OffsetRect(DestRect, R.Left, R.Top);
        with Canvas do
        begin
          MoveTo(DestRect.Left, DestRect.Top);
          LineTo(DestRect.Left + 7, DestRect.Top);

          MoveTo(DestRect.Left + 1, DestRect.Top + 1);
          LineTo(DestRect.Left + 6, DestRect.Top + 1);

          MoveTo(DestRect.Left + 2, DestRect.Top + 2);
          LineTo(DestRect.Left + 5, DestRect.Top + 2);

          MoveTo(DestRect.Left + 3, DestRect.Top + 3);
          LineTo(DestRect.Left + 4, DestRect.Top + 3);
          (*
          {$ifndef CLX_USED}
          Pixels[DestRect.Left + 3, DestRect.Top + 3] := Canvas.Pen.Color;
          {$else}
          DrawPoint(DestRect.Left + 3, DestRect.Top + 3);
          {$endif}
          *)
          if not Enabled then
          begin
            Canvas.Pen.Color := clBtnShadow;
            OffsetRect(DestRect, -1, -1);

            MoveTo(DestRect.Left, DestRect.Top);
            LineTo(DestRect.Left + 7, DestRect.Top);

            MoveTo(DestRect.Left + 1, DestRect.Top + 1);
            LineTo(DestRect.Left + 6, DestRect.Top + 1);

            MoveTo(DestRect.Left + 2, DestRect.Top + 2);
            LineTo(DestRect.Left + 5, DestRect.Top + 2);

            MoveTo(DestRect.Left + 3, DestRect.Top + 3);
            LineTo(DestRect.Left + 4, DestRect.Top + 3);

            (*
            {$ifndef CLX_USED}
            Pixels[DestRect.Left + 3, DestRect.Top + 3] := Canvas.Pen.Color;
            {$else}
            DrawPoint(DestRect.Left + 3, DestRect.Top + 3);
            {$endif}
            *)
          end;
        end;
      end;
  end;
  Canvas.Pen.Color := FSavePenColor;
end;

function RectsIntersect(R1, R2 : TRect) : boolean;
var
  R : TRect;
begin
  IntersectRect(R, R1, R2);
  result := IsRectEmpty(R);
end;

{$ifndef CLX_USED}
function FindVCLChild(Control : TWinControl; ChildHandle : HWND) : TWinControl;
var
  i : integer;
  C : TWinControl;
begin
  for i := 0 to Control.ControlCount - 1 do // Iterate
  begin
    if Control.Controls[i] is TWinControl then
    begin
      C := TWinControl(Control.Controls[i]);
      if C.HandleAllocated and (C.Handle = ChildHandle) then
      begin
        result := C;
        exit;
      end;
    end;
  end; // for
  result := nil;
end;
{$endif}

{$ifndef CLX_USED}
procedure DrawTransparentBitmapEx(DC : HDC; Bitmap : TBitmap; X, Y : integer; Src : TRect; Transparent : TColor);
{$else}
procedure DrawTransparentBitmapEx(DC : QPainterH; Bitmap : TBitmap; X, Y : integer; Src : TRect; Transparent : TColor);
{$endif}
var
  {$ifndef CLX_USED}
  cColor : TColorRef;
  bmAndBack,
    bmAndObject,
    bmAndMem,
    bmSave,
    bmBackOld,
    bmObjectOld,
    bmMemOld,
    bmSaveOld : HBitmap;
  hdcMem,
    hdcBack,
    hdcObject,
    hdcTemp,
    hdcSave : HDC;
  bmWidth, bmHeight : integer;
  {$endif}
begin
  {$ifndef CLX_USED}
  hdcTemp := CreateCompatibleDC(DC);
  SelectObject(hdcTemp, Bitmap.Handle); { select the bitmap }

  bmWidth := Src.Right - Src.Left;
  bmHeight := Src.Bottom - Src.Top;

   { create some DCs to hold temporary data }
  hdcBack := CreateCompatibleDC(DC);
  hdcObject := CreateCompatibleDC(DC);
  hdcMem := CreateCompatibleDC(DC);
  hdcSave := CreateCompatibleDC(DC);

   { create a bitmap for each DC }

   { monochrome DC }
  bmAndBack := CreateBitmap(bmWidth, bmHeight, 1, 1, nil);
  bmAndObject := CreateBitmap(bmWidth, bmHeight, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(DC, bmWidth, bmHeight);
  bmSave := CreateCompatibleBitmap(DC, bmWidth, bmHeight);

   { each DC must select a bitmap object to store pixel data }
  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

   { set proper mapping mode }
  SetMapMode(hdcTemp, GetMapMode(DC));

   { save the bitmap sent here, because it will be overwritten }
  BitBlt(hdcSave, 0, 0, bmWidth, bmHeight, hdcTemp, Src.Left, Src.Top, SRCCOPY);

   { set the background color of the source DC to the color.
    contained in the parts of the bitmap that should be transparent }
  cColor := SetBkColor(hdcTemp, ColorToRGB(Transparent));

   { create the object mask for the bitmap by performing a BitBlt()
     from the source bitmap to a monochrome bitmap }
  BitBlt(hdcObject, 0, 0, bmWidth, bmHeight, hdcTemp, Src.Left, Src.Top, SRCCOPY);

   { set the background color of the source DC back to the original
color }
  SetBkColor(hdcTemp, cColor);

   { create the inverse of the object mask }
  BitBlt(hdcBack, 0, 0, bmWidth, bmHeight, hdcObject, 0, 0, NOTSRCCOPY);

   { copy the background of the main DC to the destination }
  BitBlt(hdcMem, 0, 0, bmWidth, bmHeight, DC, X, Y, SRCCOPY);

   { mask out the places where the bitmap will be placed }
  BitBlt(hdcMem, 0, 0, bmWidth, bmHeight, hdcObject, 0, 0, SRCAND);

   { mask out the transparent colored pixels on the bitmap }
  BitBlt(hdcTemp, Src.Left, Src.Top, bmWidth, bmHeight, hdcBack, 0, 0, SRCAND);

   { XOR the bitmap with the background on the destination DC }
  BitBlt(hdcMem, 0, 0, bmWidth, bmHeight, hdcTemp, Src.Left, Src.Top, SRCPAINT);

   { copy the destination to the screen }
  BitBlt(DC, X, Y, bmWidth, bmHeight, hdcMem, 0, 0, SRCCOPY);

   { place the original bitmap back into the bitmap sent here }
  BitBlt(hdcTemp, Src.Left, Src.Top, bmWidth, bmHeight, hdcSave, 0, 0, SRCCOPY);

   { delete the memory bitmaps }
  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

   { delete the memory DCs }
  DeleteDC(hdcMem);
  DeleteDC(hdcBack);
  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
  {$endif}
end;

procedure DrawTypedTextW(Canvas : TCanvas; Bounds : TRect; Text : WideString; Flags : integer; DrawType : TElTextDrawType);
var
  Col : TColor;
begin
  case DrawType of
    tdtNormal :
      {$ifndef CLX_USED}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Bounds, Flags);
      {$endif}
    tdtShadowed :
      begin
        OffsetRect(Bounds, 2, 2);
        Col := Canvas.Font.Color;
        Canvas.Font.Color := clBtnShadow;
        {$ifndef CLX_USED}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Bounds, Flags);
        {$endif}
        OffsetRect(Bounds, -2, -2);
        Canvas.Font.Color := col;
        {$ifndef CLX_USED}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Bounds, Flags);
        {$endif}
      end;
    tdtRaised :
      begin
        OffsetRect(Bounds, -1, -1);
        Col := Canvas.Font.Color;
        Canvas.Font.Color := clBtnHighlight;
        {$ifndef CLX_USED}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Bounds, Flags);
        {$endif}
        OffsetRect(Bounds, 1, 1);
        Canvas.Font.Color := col;
        {$ifndef CLX_USED}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Bounds, Flags);
        {$endif}
      end;
  end;
end;

procedure DrawTypedText(Canvas : TCanvas; Bounds : TRect; Text : string; Flags : integer; DrawType : TElTextDrawType);
var
  Col : TColor;
begin
  case DrawType of
    tdtNormal :
      {$ifndef CLX_USED}
      DrawText(Canvas.Handle, PChar(Text), Length(Text), Bounds, Flags);
      {$endif}
    tdtShadowed :
      begin
        OffsetRect(Bounds, 2, 2);
        Col := Canvas.Font.Color;
        Canvas.Font.Color := clBtnShadow;
        {$ifndef CLX_USED}
        DrawText(Canvas.Handle, PChar(Text), Length(Text), Bounds, Flags);
        {$endif}
        OffsetRect(Bounds, -2, -2);
        Canvas.Font.Color := col;
        {$ifndef CLX_USED}
        DrawText(Canvas.Handle, PChar(Text), Length(Text), Bounds, Flags);
        {$endif}
      end;
    tdtRaised :
      begin
        OffsetRect(Bounds, -1, -1);
        Col := Canvas.Font.Color;
        Canvas.Font.Color := clBtnHighlight;
        {$ifndef CLX_USED}
        DrawText(Canvas.Handle, PChar(Text), Length(Text), Bounds, Flags);
        {$endif}
        OffsetRect(Bounds, 1, 1);
        Canvas.Font.Color := col;
        {$ifndef CLX_USED}
        DrawText(Canvas.Handle, PChar(Text), Length(Text), Bounds, Flags);
        {$endif}
      end;
  end;
end;

procedure FillSolidRect2(DC : HDC; Rect : TRect; Color : TColor);
{$ifndef CLX_USED}
var
  SaveC : COLORREF;
begin
  SaveC := GetBkColor(DC);
  SetBkColor(DC, Color);
  ExtTextOut(DC, 0, 0, ETO_OPAQUE, @Rect, nil, 0, nil);
  SetBkColor(DC, SaveC);
{$endif}
end;

procedure FillSolidRect(DC : HDC; x, y, cx, cy : integer; Color : TColor);
{$ifndef CLX_USED}
var
  Rect : TRect;
  SaveC : COLORREF;
begin
  SaveC := GetBkColor(DC);
  SetBkColor(DC, Color);
  Rect := Classes.Rect(x, y, x + cx, y + cy);
  ExtTextOut(DC, 0, 0, ETO_OPAQUE, @Rect, nil, 0, nil);
  SetBkColor(DC, SaveC);
{$endif}
end;

procedure Draw3dRect(DC : HDC; x, y, cx, cy : integer; clrTopLeft,
{$ifndef CLX_USED}
clrBottomRight : COLORREF);
{$endif}
begin
{$ifndef CLX_USED}
  FillSolidRect(DC, x, y, cx - 1, 1, clrTopLeft);
  FillSolidRect(DC, x, y, 1, cy - 1, clrTopLeft);
  FillSolidRect(DC, x + cx, y, -1, cy, clrBottomRight);
  FillSolidRect(DC, x, y + cy, cx, -1, clrBottomRight);
{$endif}
end;

procedure Draw3dRectEx(DC : HDC; x, y, cx, cy : integer; clrTopLeft,
{$ifndef CLX_USED}
  clrBottomRight : COLORREF;
{$endif}
  BorderSides : TElBorderSides);
  
begin
{$ifndef CLX_USED}
  if ebsTop in BorderSides then
    FillSolidRect(DC, x, y, cx - 1, 1, clrTopLeft);
  if ebsLeft in BorderSides then
    FillSolidRect(DC, x, y, 1, cy - 1, clrTopLeft);
  if ebsRight in BorderSides then
    FillSolidRect(DC, x + cx, y, -1, cy, clrBottomRight);
  if ebsBottom in BorderSides then
    FillSolidRect(DC, x, y + cy, cx, -1, clrBottomRight);
{$endif}
end;

procedure Draw3dBorder(DC : HDC; rc : TRect; nColor1, nColor2, nColor3, nColor4 : TColor);
begin
  nColor1 := ColorToRGB(GetSysColor(nColor1));
  nColor2 := ColorToRGB(GetSysColor(nColor2));
  nColor3 := ColorToRGB(GetSysColor(nColor3));
  nColor4 := ColorToRGB(GetSysColor(nColor4));

  {$ifndef CLX_USED}
  Draw3dRect(DC, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, nColor1, nColor2);
  Draw3dRect(DC, rc.left + 1, rc.top + 1, rc.right - rc.left - 2, rc.bottom - rc.top - 2, nColor3, nColor4);
  {$endif}
end;

const
  hlsMax=252; // H, L, S vary over 0 - hlsMax; best if divisible by 6
  rgbMax=255; // r, g, b vary over 0-rgbMax; must each fit in a byte

  hlsMaxD2   = hlsMax div 2;
  hlsMaxD3   = hlsMax div 3;
  hlsMaxD6   = hlsMax div 6;
  hlsMaxD12  = hlsMax div 12;
  hlsMaxM2D3 = hlsMax*2 div 3;
  rgbMaxM2   = rgbMax*2;
  undefined  = (hlsMaxM2D3);

procedure checkRange(var hls: integer);
begin
  if hls<0 then inc(hls, hlsMax); if hls>hlsMax then dec(hls, hlsMax)
end;

function RGBtoHLS(rgbc: integer): integer;
var
  r, g, b, h, l, s, cMax, cMin, rD, gD, bD,
  cDiff, cDiffD2, cSum, cSumD2: integer;
begin
  r:=getRValue(rgbc);
  g:=getGValue(rgbc);
  b:=getBValue(rgbc);
  cMax:=max(max(r, g), b);
  cMin:=min(min(r, g), b);

  l:=(((cMax+cMin)*hlsMax)+rgbMax) div (rgbMaxM2);

  if cMax=cMin then
  begin
    s:=0;
    h := undefined
  end
  else
  begin
    cDiff:=cMax-cMin;
    cDiffD2:=cDiff div 2;
    cSum:=cMax+cMin;
    cSumD2:=cSum div 2;
    if l<=hlsMaxD2 then
       s:=(cDiff*hlsMax + cSumD2) div cSum
    else
       s:=(cDiff*hlsMax + (rgbMaxM2-cDiff) div 2) div (rgbMaxM2-cDiff);
    rD:=((cMax-r)*hlsMaxD6 + cDiffD2) div cDiff;
    gD:=((cMax-g)*hlsMaxD6 + cDiffD2) div cDiff;
    bD:=((cMax-b)*hlsMaxD6 + cDiffD2) div cDiff;
    if r=cMax then
       h:= bD - gD
    else
    if g=cMax then
       h:=hlsMaxD3 + rD - bD
    else
       h:=hlsMaxM2D3 + gD - rD;
    checkRange(h);
  end;
  {$ifndef CLX_USED}
  result:=rgb(h, l, s);
  {$endif}
end;

function HLStoRGB(hlsc: integer): integer;
var
  h, l, s, r, g, b, magic1, magic2: integer;

  function hueToRGB(n1, n2, hue: integer): integer;
  begin
    checkRange(hue);
    if hue<hlsMaxD6 then
      result:=n1+((n2-n1)*hue+hlsMaxD12) div hlsMaxD6
    else if hue<hlsMaxD2 then result:=n2
    else if hue<hlsMaxM2D3 then
      result:=n1+((n2-n1)*(hlsMaxM2D3-hue)+hlsMaxD12) div hlsMaxD6
    else result:=n1
  end;

begin
  h:=getRValue(hlsc); l:=getGValue(hlsc); s:=getBValue(hlsc);
  if s=0 then
  begin
    r:=l * rgbMax div hlsMax; g:=r; b:=r;
    if h<>undefined then begin {ERROR} end
  end
  else
  begin
    if l<=hlsMaxD2 then magic2:=(l*(hlsMax+s) + hlsMaxD2) div hlsMax
    else magic2:=l+s-(l*s+hlsMaxD2) div hlsMax;
    magic1:=2*l-magic2;
    r:=Max(0, Min(255, (hueToRGB(magic1, magic2, h+hlsMaxD3)*rgbMax+hlsMaxD2) div hlsMax));
    g:=Max(0, Min(255, (hueToRGB(magic1, magic2, h)*rgbMax+hlsMaxD2) div hlsMax));
    b:=Max(0, Min(255, (hueToRGB(magic1, magic2, h-hlsMaxD3)*rgbMax+hlsMaxD2) div hlsMax));
  end;
  {$ifndef CLX_USED}
  result:=rgb(r,g,b)
  {$endif}
end;

procedure DrawButtonFrameEx3(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean;
                            ButtonColor : TColor; Thin : boolean; BorderSides : TElBorderSides);
var Color1,
    Color2,
    Color3,
    Color4 : TColor;
    hls1, hls2, hls3, hls4 : integer;
    lum : integer;

    procedure DrawBorder(DC : HDC; rc : TRect; nColor1, nColor2, nColor3, nColor4 : TColor; Thin : boolean; BorderSides : TElBorderSides);
    begin
      {$ifndef CLX_USED}
      Draw3dRectEx(DC, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, nColor1, nColor2, BorderSides);
      if not Thin then
        Draw3dRectEx(DC, rc.left + 1, rc.top + 1, rc.right - rc.left - 2, rc.bottom - rc.top - 2, nColor3, nColor4, BorderSides);
      {$endif}
    end;

begin
  if ButtonColor = clBtnFace then
  begin
    Color1 := ColorToRGB(clBtnHighlight);
    Color2 := ColorToRGB(cl3DDkShadow);
    Color3 := ColorToRGB(clBtnFace);
    Color4 := ColorToRGB(clBtnShadow);
  end
  else
  begin
    ButtonColor := ColorToRGB(ButtonColor);

    hls1 := RGBtoHLS(ButtonColor);
    hls2 := hls1;
    hls3 := hls1;
    hls4 := hls1;

    lum := Hi(hls3 and $0000FFFF);
    if lum <> 0 then
    begin
      hls1 := (Min(239, (Hi(hls3 and $0000FFFF)  + lum div 3)) shl 8) or (hls1 and $FF00FF);
      hls2 := (Min(239, (Hi(hls3 and $0000FFFF)  - lum div 2)) shl 8) or (hls2 and $FF00FF);
      hls4 := (Min(239, (Hi(hls3 and $0000FFFF)  - lum div 3)) shl 8) or (hls4 and $FF00FF);

      Color1 := HLStoRGB(hls1);
      Color2 := HLStoRGB(hls2);
      Color3 := ButtonColor;
      Color4 := HLStoRGB(hls4);
    end
    else
    begin
      Color1 := ColorToRGB(clBtnHighlight);
      Color2 := ColorToRGB(cl3DDkShadow);
      Color3 := ButtonColor;
      Color4 := ColorToRGB(clBtnShadow);
    end;
  end;

  if not Pushed then
  begin
    if Focused then
    {$ifndef CLX_USED}
      DrawBorder(DC, rc, Color1, Color2, Color3, Color4, Thin, BorderSides)
    {$endif}
    else
      DrawBorder(DC, rc, Color1, Color4, Color3, Color3, Thin, BorderSides);
  end
  else
  begin
    if Focused then
      DrawBorder(DC, rc, Color2, Color1, Color4, Color3, Thin, BorderSides)
    else
      DrawBorder(DC, rc, Color4, Color1, Color3, Color3, Thin, BorderSides);
  end;
end;

procedure DrawButtonFrameEx(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean; ButtonColor : TColor; Thin : boolean);
var Color1,
    Color2,
    Color3,
    Color4 : TColor;
    hls1, hls2, hls3, hls4 : integer;
    lum : integer;

    procedure DrawBorder(DC : HDC; rc : TRect; nColor1, nColor2, nColor3, nColor4 : TColor; Thin : boolean);
    begin
      {$ifndef CLX_USED}
      Draw3dRect(DC, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, nColor1, nColor2);
      if not Thin then
        Draw3dRect(DC, rc.left + 1, rc.top + 1, rc.right - rc.left - 2, rc.bottom - rc.top - 2, nColor3, nColor4);
      {$endif}
    end;

begin
  if ButtonColor = clBtnFace then
  begin
    Color1 := ColorToRGB(clBtnHighlight);
    Color2 := ColorToRGB(cl3DDkShadow);
    Color3 := ColorToRGB(clBtnFace);
    Color4 := ColorToRGB(clBtnShadow);
  end
  else
  begin
    ButtonColor := ColorToRGB(ButtonColor);

    hls1 := RGBtoHLS(ButtonColor);
    hls2 := hls1;
    hls3 := hls1;
    hls4 := hls1;

    lum := Hi(hls3 and $0000FFFF);
    if lum <> 0 then
    begin
      hls1 := (Min(239, (Hi(hls3 and $0000FFFF)  + lum div 3)) shl 8) or (hls1 and $FF00FF);
      hls2 := (Min(239, (Hi(hls3 and $0000FFFF)  - lum div 2)) shl 8) or (hls2 and $FF00FF);
      hls4 := (Min(239, (Hi(hls3 and $0000FFFF)  - lum div 3)) shl 8) or (hls4 and $FF00FF);

      Color1 := HLStoRGB(hls1);
      Color2 := HLStoRGB(hls2);
      Color3 := ButtonColor;
      Color4 := HLStoRGB(hls4);
    end
    else
    begin
      Color1 := ColorToRGB(clBtnHighlight);
      Color2 := ColorToRGB(cl3DDkShadow);
      Color3 := ButtonColor;
      Color4 := ColorToRGB(clBtnShadow);
    end;
  end;

  if not Pushed then
  begin
    if Focused then
    {$ifndef CLX_USED}
      DrawBorder(DC, rc, Color1, Color2, Color3, Color4, Thin)
    {$endif}
    else
      DrawBorder(DC, rc, Color1, Color4, Color3, Color3, Thin);
  end
  else
  begin
    if Focused then
      DrawBorder(DC, rc, Color2, Color1, Color4, Color3, Thin)
    else
      DrawBorder(DC, rc, Color4, Color1, Color3, Color3, Thin);
  end;
end;

procedure DrawButtonFrame(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean);
begin
  {$ifndef CLX_USED}
  if not Pushed then
  begin
    if Focused then
      Draw3dBorder(DC, rc, COLOR_3DHIGHLIGHT, COLOR_3DDKSHADOW, COLOR_3DFACE, COLOR_3DSHADOW)
    else
      Draw3dBorder(DC, rc, COLOR_3DHIGHLIGHT, COLOR_3DSHADOW, COLOR_3DFACE, COLOR_3DFACE);
  end
  else
    Draw3dBorder(DC, rc, COLOR_3DDKSHADOW , COLOR_3DHIGHLIGHT, COLOR_3DSHADOW, COLOR_3DFACE);
  {$endif}
end;

procedure DrawButtonFrameEx2(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean; ButtonColor : TColor; Thin : boolean; clrHighlight, clrDkShadow, clrFace, clrShadow : TColor);
var Color1,
    Color2,
    Color3,
    Color4 : TColor;

    procedure DrawBorder(DC : HDC; rc : TRect; nColor1, nColor2, nColor3, nColor4 : TColor; Thin : boolean);

      (*
      procedure Draw3dRect(DC : HDC; x, y, cx, cy : integer; clrTopLeft, clrBottomRight : TColor);
      var
        Clr: TColor;
      begin
        if clrTopLeft <> clNone then
        begin
          Clr := ColorToRGB(clrTopLeft);
          FillSolidRect(DC, x, y, cx - 1, 1, Clr);
          FillSolidRect(DC, x, y, 1, cy - 1, Clr);
        end;
        if clrBottomRight <> clNone then
        begin
          Clr := ColorToRGB(clrBottomRight);
          FillSolidRect(DC, x + cx, y, -1, cy, Clr);
          FillSolidRect(DC, x, y + cy, cx, -1, Clr);
        end;
      end;
      *)
    begin
      Draw3dRect(DC, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, nColor1, nColor2);
      if not Thin then
        Draw3dRect(DC, rc.left + 1, rc.top + 1, rc.right - rc.left - 2, rc.bottom - rc.top - 2, nColor3, nColor4);
    end;

begin
  Color1 := clrHighlight;
  Color2 := clrDkShadow;
  Color3 := clrFace;
  Color4 := clrShadow;
  if not Pushed then
  begin
    if Focused then
      DrawBorder(DC, rc, Color1, Color2, Color3, Color4, Thin)
    else
      DrawBorder(DC, rc, Color1, Color4, Color3, Color3, Thin);
  end
  else
    DrawBorder(DC, rc, Color2, Color1, Color4, Color3, Thin);
end;

{$WARNINGS off}

{$ifndef CLX_USED}
procedure DrawFlatFrameEx2(DC : HDC; R : TRect; Color, BkColor : TColor;
                           Focused, Enabled : boolean;
                           BorderSides : TElBorderSides;
                           BorderType : TElFlatBorderType);
{$ifndef CLX_USED}
var BtnFaceBrush,
    WindowBrush,
    ShadowBrush: HBRUSH;
{$endif}
    R1   : TRect;
    Edge : DWORD;

const EdgeTypes : array [TElFlatBorderType] of DWORD = (EDGE_RAISED, BDR_SUNKEN, BDR_SUNKENINNER, BDR_SUNKENOUTER, EDGE_RAISED, BDR_RAISEDINNER, BDR_RAISEDOUTER, EDGE_BUMP, EDGE_BUMP, EDGE_ETCHED, EDGE_BUMP, 0, 0, 0, 0);
      EdgeFlat  : array [boolean] of DWORD = (BF_RECT or BF_ADJUST, BF_FLAT or BF_ADJUST);

begin

  if BorderType = fbtFlat then
  begin
    DrawFlatFrame2(DC, R, BkColor, false, BorderSides);
    exit;
  end;

  BtnFaceBrush := GetSysColorBrush(COLOR_BTNFACE); //get the Windows brush
  WindowBrush := GetSysColorBrush(COLOR_WINDOW); //get the Windows brush
  ShadowBrush := GetSysColorBrush(COLOR_BTNSHADOW);

  if BorderType = fbtFlat then
    Edge := BF_FLAT or BF_ADJUST
  else
  begin
    Edge := BF_ADJUST;
    if ebsLeft in BorderSides then
      Edge := Edge or BF_LEFT;
    if ebsTop in BorderSides then
      Edge := Edge or BF_TOP;
    if ebsRight in BorderSides then
      Edge := Edge or BF_RIGHT;
    if ebsBottom in BorderSides then
      Edge := Edge or BF_BOTTOM;
  end;
  if BorderType < fbtLine then
    DrawEdge(DC, R, EdgeTypes[BorderType], Edge);

  case BorderType of
    fbtFramed:
      with R do
      begin
        if ebsLeft in BorderSides then
          FrameRect (DC, Rect(Left-1, Top, Left, Bottom), BtnFaceBrush);
        if ebsTop in BorderSides then
          FrameRect (DC, Rect(Left-2, Top-1, Right+1, Top), BtnFaceBrush);
        if ebsRight in BorderSides then
          FrameRect (DC, Rect(Right+1, Top-2, Right+2, Bottom+2), ShadowBrush);
        if ebsBottom in BorderSides then
          FrameRect (DC, Rect(Left-2, Bottom+1, Right+2, Bottom+2), shadoWbrush);
      end;
    fbtRaised:
      begin
        R1 := R;
        (*
        R1 := DrawBevel(DC, R1, BkColor, BkColor, [ebsLeft, ebsRight, ebsTop, ebsBottom]);
        R1 := DrawBevel(DC, R1, BkColor, BkColor, [ebsLeft, ebsRight, ebsTop, ebsBottom]);
        R1 := DrawBevel(DC, R, clBtnShadow, clBtnHighlight, [ebsLeft, ebsRight, ebsTop, ebsBottom]);
        *)
        R1 := DrawBevel(DC, R1, BkColor, BkColor, BorderSides);
        R1 := DrawBevel(DC, R1, BkColor, BkColor, BorderSides);
        R1 := DrawBevel(DC, R, clBtnShadow, clBtnHighlight, BorderSides);
      end;
    fbtRaisedInner:
      with R do
      begin
        if ebsLeft in BorderSides then
          FrameRect(DC, Rect(Left, Top, Left+1, Bottom), BtnFaceBrush);
        if ebsTop in BorderSides then
          FrameRect(DC, Rect(Left, Top, Right, Top+1), BtnFaceBrush);

        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right-1, Top, Right, Bottom), BtnFaceBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left, Bottom - 1, Right, Bottom), BtnFaceBrush);
      end;
    fbtRaisedOuter:
      with R do
      begin
        if ebsLeft in BorderSides then
          FillRect(DC, Rect(Left, Top, Left+1, Bottom), BtnFaceBrush);
        if ebsTop in BorderSides then
          FillRect(DC, Rect(Left, Top, Right, Top+1), BtnFaceBrush);
        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right-1, Top, Right, Bottom), BtnFaceBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left, Bottom - 1, Right, Bottom), BtnFaceBrush);

        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right, Top-1, Right+1, Bottom+1), ShadowBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left-1, Bottom, Right+1, Bottom+1), ShadowBrush);
      end;
    {fbtSunken:
      begin
        if not Focused then
        begin
          FrameRect (DC, R, BtnFaceBrush);
          InflateRect (R, -1, -1);
          if (not Enabled)
             then FrameRect(DC, R, WindowBrush)
             else FrameRect(DC, R, BtnFaceBrush);
        end;
      end;
    }fbtSunkenInner:
      with R do
      begin
        if ebsLeft in BorderSides then
          FrameRect(DC, Rect(Left, Top, Left+1, Bottom), BtnFaceBrush);
        if ebsTop in BorderSides then
          FrameRect(DC, Rect(Left, Top, Right, Top+1), BtnFaceBrush);

        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right-1, Top, Right, Bottom), BtnFaceBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left, Bottom - 1, Right, Bottom), BtnFaceBrush);

        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right, Top-1, Right+1, Bottom+1), BtnFaceBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left-1, Bottom, Right+1, Bottom+1), BtnFaceBrush);
      end;
    fbtSunkenOuter:
      with R do
      begin
        if ebsLeft in BorderSides then
          FrameRect(DC, Rect(Left, Top, Left+1, Bottom-1), BtnFaceBrush);
        if ebsTop in BorderSides then
          FrameRect(DC, Rect(Left, Top, Right-1, Top+1), BtnFaceBrush);
        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right-1, Top, Right, Bottom), BtnFaceBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left, Bottom - 1, Right, Bottom), BtnFaceBrush);

        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right, Top-1, Right+1, Bottom+1), WindowBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left-1, Bottom, Right+1, Bottom+1), WindowBrush);
      end;
    fbtBump:
      with R do
      begin
        if ebsLeft in BorderSides then
          FillRect(DC, Rect(Left-1, Top-1, Left, Bottom), ShadowBrush);
        if ebsTop in BorderSides then
          FillRect(DC, Rect(Left, Top-1, Right, Top), ShadowBrush);
        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right+2, Top-2, Right+1, Bottom+2), ShadowBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left-2, Bottom+1, Right+2, Bottom+2), ShadowBrush);
      end;
    fbtBigBump:
      with R do
      begin
        if ebsLeft in BorderSides then
          FillRect(DC, Rect(Left-1, Top-1, Left, Bottom), ShadowBrush);
        if ebsTop in BorderSides then
          FillRect(DC, Rect(Left, Top-1, Right, Top), ShadowBrush);
        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right+2, Top-2, Right+1, Bottom+2), ShadowBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left-2, Bottom+1, Right+2, Bottom+2), ShadowBrush);
        if ebsLeft in BorderSides then
          FillRect(DC, Rect(Left-2, Top-2, Left-1, Bottom+1), WindowBrush);
        if ebsTop in BorderSides then
          FillRect(DC, Rect(Left-2, Top-2, Right+1, Top-1), WindowBrush);
        if ebsRight in BorderSides then
          FillRect(DC, Rect(Right+1, Top-2, Right, Bottom+1), WindowBrush);
        if ebsBottom in BorderSides then
          FillRect(DC, Rect(Left-2, Bottom, Right+1, Bottom+1), WindowBrush);
      end;
    fbtFlat:
      with R do
      begin
        if (ebsLeft in BorderSides) and
           (ebsTop in BorderSides) then
          FillRect (DC, Rect(Left-1, Top-1, Right, Top), BtnFaceBrush)
        else
        if (ebsLeft in BorderSides) then
          FillRect (DC, Rect(Left-1, Top, Right, Top), BtnFaceBrush)
        else
        if (ebsTop in BorderSides) then
          FillRect (DC, Rect(Left, Top-1, Right, Top), BtnFaceBrush);
      end;
    fbtEtched:
      with R do
      begin
        if (ebsLeft in BorderSides) and
           (ebsTop in BorderSides) then
          FillRect (DC, Rect(Left-1, Top-1, Right, Top), BtnFaceBrush)
        else
        if (ebsLeft in BorderSides) then
          FillRect (DC, Rect(Left-1, Top, Right, Top), BtnFaceBrush)
        else
        if (ebsTop in BorderSides) then
          FillRect (DC, Rect(Left, Top-1, Right, Top), BtnFaceBrush);
        //FillRect (DC, Rect(Left-1, Top-1, Right, Top), BtnFaceBrush);
      end;
    fbtLine,
    fbtLineBorder,
    fbtNone: //mdm - Support for new styles
      begin
        R1 := DrawBevel(DC, R, BkColor, BkColor, BorderSides);
        DrawBevel(DC, R1, BkColor, BkColor, BorderSides);
        case BorderType of
          fbtLine:       DrawBevel(DC, R, clWindowFrame, clWindowFrame, [ebsBottom]);
          fbtLineBorder: DrawBevel(DC, R, clWindowFrame, clWindowFrame, bordersidEs);
        end;
      end;
    fbtColorLineBorder:
      begin
        R1 := DrawBevel(DC, R, Color, Color, BorderSides);
        DrawBevel(DC, R1, BkColor, BkColor, BorderSides);
      end;
  end;
end;

procedure DrawFlatFrameEx(DC : HDC; R : TRect; BkColor : TColor; Focused, Enabled : boolean);
begin
  if Focused then
  begin
    Draw3dBorder(DC, R, COLOR_3DSHADOW, COLOR_3DHIGHLIGHT, COLOR_3DDKSHADOW, COLOR_3DLIGHT);
  end
  else
  begin
    if not Enabled then
      Draw3dBorder(DC, R, COLOR_3DSHADOW, COLOR_3DHIGHLIGHT, COLOR_3DFACE, COLOR_3DFACE)
    else
      Draw3dBorder(DC, R, COLOR_3DSHADOW, COLOR_3DHIGHLIGHT, COLOR_3DLIGHT, COLOR_3DLIGHT);
  end;
end;

function DrawFlatFrame(DC : HDC; R : TRect; BkColor : TColor; Focused : boolean) : TRect;
var
  R1 : TRect;
begin
  R1 := R;
  R1 := DrawBevel(DC, R1, BkColor, BkColor, [ebsLeft, ebsRight, ebsTop, ebsBottom]);
  R1 := DrawBevel(DC, R1, BkColor, BkColor, [ebsLeft, ebsRight, ebsTop, ebsBottom]);

  if Focused then
  begin
    R := DrawBevel(DC, R, clBtnShadow, clBtnHighlight, [ebsLeft, ebsRight, ebsTop, ebsBottom]);
    R := DrawBevel(DC, R, clBtnText, cl3DLight, [ebsLeft, ebsRight, ebsTop, ebsBottom]);
  end
  else
  begin
    DrawBevel(DC, R, clBtnShadow, clBtnHighlight, [ebsLeft, ebsRight, ebsTop, ebsBottom]);
  end;
end;

function DrawFlatFrame2(DC : HDC; R : TRect; BkColor : TColor; Focused : boolean; BorderSides : TElBorderSides) : TRect;
var
  R1 : TRect;
begin
  R1 := R;
  R1 := DrawBevel(DC, R1, BkColor, BkColor, BorderSides);
  R1 := DrawBevel(DC, R1, BkColor, BkColor, BorderSides);

  if Focused then
  begin
    R := DrawBevel(DC, R, clBtnShadow, clBtnHighlight, BorderSides);
    R := DrawBevel(DC, R, clBtnText, cl3DLight, BorderSides);
  end
  else
  begin
    DrawBevel(DC, R, clBtnShadow, clBtnHighlight, BorderSides);
  end;
end;

{$endif}

procedure TiledPaint(Canvas : TCanvas; Bitmap : TBitmap; Rect : TRect);
var
  CurRect : TRect;
  CurLeft,
    CurTop : integer;
begin
  if not IsWinNT then
  begin
    CurTop := Rect.Top;
    while CurTop < Rect.Bottom do
    begin
      CurLeft := Rect.Left;
      while CurLeft < Rect.Right do
      begin
        CurRect := Classes.Rect(CurLeft, CurTop, Min(CurLeft + Bitmap.Width, Rect.Right), Min(CurTop + Bitmap.Height, Rect.Bottom));
        {$ifndef CLX_USED}
        BitBlt(Canvas.Handle, CurLeft, CurTop, CurRect.Right - CurRect.Left, CurRect.Bottom - CurRect.Top, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
        {$endif}
        Inc(CurLeft, Bitmap.Width);
      end;
      Inc(CurTop, Bitmap.Height);
    end;
  end
  else
  begin
    Canvas.Brush.Bitmap := TBitmap.Create;
    Canvas.Brush.Bitmap.Assign(Bitmap);
    Canvas.FillRect(Rect);
    Canvas.Brush.Bitmap.Free;
    Canvas.Brush.Bitmap := nil;
  end;
end;

{$ifndef CLX_USED}
function HitTest(R : TRect; Pt : TPoint; CornerSize, BorderSize : integer) : integer;
begin
  if not PtInRect(R, Pt) then
  begin
    result := HTNOWHERE;
    exit;
  end;
  if InRange(R.Left, R.Left + BorderSize - 1, Pt.X) then
  begin
    if Pt.Y <= (R.Top + CornerSize) then
    begin
      result := HTTOPLEFT;
      exit;
    end;
    if Pt.Y >= (R.Bottom - CornerSize - 1) then
    begin
      result := HTBOTTOMLEFT;
      exit;
    end;
    result := HTLEFT;
    exit;
  end
  else if InRange(R.Top, R.Top + BorderSize - 1, Pt.Y) then
  begin
    if PT.X <= (R.Left + CornerSize) then
    begin
      result := HTTOPLEFT;
      exit;
    end;
    if PT.x >= (R.Right - CornerSize - 1) then
    begin
      result := HTTOPRIGHT;
      exit;
    end;
    result := HTTOP;
    exit;
  end
  else if InRange(R.Bottom - BorderSize - 1, R.Bottom - 1, Pt.Y) then
  begin
    if PT.X <= (R.Left + CornerSize) then
    begin
      result := HTBOTTOMLEFT;
      exit;
    end;
    if PT.x >= (R.Right - CornerSize - 1) then
    begin
      result := HTBOTTOMRIGHT;
      exit;
    end;
    result := HTBOTTOM;
    exit;
  end
  else if InRange(R.Right - BorderSize - 1, R.Right - 1, Pt.X) then
  begin
    if Pt.Y <= (R.Top + CornerSize) then
    begin
      result := HTTOPRIGHT;
      exit;
    end;
    if Pt.Y >= (R.Bottom - CornerSize - 1) then
    begin
      result := HTBOTTOMRIGHT;
      exit;
    end;
    result := HTRIGHT;
    exit;
  end;
  result := HTCLIENT;
end;
{$endif}

function GetTopOwnerControl(Component : TComponent) : TControl;
begin
  while (not (Component is TControl)) and (Component.Owner <> nil) do
    Component := Component.Owner;
  result := Component as TControl;
end;

function GetOwnerForm(Component : TComponent) : TForm;
begin
  result := nil;
  while (not (Component is TForm)) and (Component.Owner <> nil) do
    Component := Component.Owner;
  if Component is TForm then result := (Component as TForm);
end;

{$ifdef VCL_6_USED}
var
{$else}
const
{$endif}
  WaitCount: Integer = 0;
  SaveCursor: TCursor = crDefault;

procedure StartWait;
begin
  if WaitCount = 0 then
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := WaitCursor;
  end;
  Inc(WaitCount);
end;

procedure StopWait;
begin
  if WaitCount > 0 then
  begin
    Dec(WaitCount);
    if WaitCount <= 0 then
       Screen.Cursor := SaveCursor;
  end;
end;

procedure DrawFlatScrollbarThumb(DC : HDC; rc : TRect; Focused : boolean);
begin
{$ifndef CLX_USED}
  if Focused then
    Draw3dBorder(DC, rc, COLOR_3DFACE, COLOR_3DDKSHADOW, COLOR_3DHIGHLIGHT, COLOR_3DSHADOW)
  else
    Draw3dBorder(DC, rc, COLOR_3DHIGHLIGHT, COLOR_3DSHADOW, COLOR_3DFACE, COLOR_3DFACE);
{$endif}
end;

{$ifndef CLX_USED}
function AlphaBlend(
  hdcDest: HDC;                     // handle to destination DC
  nXOriginDest: Integer;            // x-coord of upper-left corner
  nYOriginDest: Integer;            // y-coord of upper-left corner
  nWidthDest: Integer;              // destination width
  nHeightDest: Integer;             // destination height
  hdcSrc: HDC;                      // handle to source DC
  nXOriginSrc: Integer;             // x-coord of upper-left corner
  nYOriginSrc: Integer;             // y-coord of upper-left corner
  nWidthSrc: Integer;               // source width
  nHeightSrc: Integer;              // source height
  SourceConstantAlpha: Byte;        // Specifies an alpha transparency value to be used on the entire source bitmap
  SrcAlpha : byte
  ): Boolean;
var
  hLib: HINST;
  Func: TAlphaBlend;
  blendFunction: TBlendFunction;
  iBlendFunction: Integer absolute blendFunction;
begin
{$OPTIMIZATION OFF}
  Result := False;
  if IsWin2000Up or IsWin98Up then
  begin
    hLib := LoadLibrary(PChar('msimg32.dll'));
    if hLib <> 0 then
    begin
      Func := TAlphaBlend(GetProcAddress(hLib, PChar('AlphaBlend')));
      if Assigned(Func) then
      begin
        with blendFunction do
        begin
          BlendOp := 0;
          BlendFlags := 0;
          AlphaFormat := SrcAlpha;//AC_SRC_ALPHA;
        end;
        blendFunction.SourceConstantAlpha := SourceConstantAlpha;
        Result := Func(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, Iblendfunction);
      end;
      FreeLibrary(hLib);
    end;
  end;
{$OPTIMIZATION ON}
end;
{$endif}

{$ifndef CLX_USED}
procedure DrawFlatScrollBar(Wnd : HWND; DC : HDC; rect : TRect; nType : integer; bScrollbarCtrl : boolean; Dragging : boolean; Focused : boolean);var
  nScrollSize : integer;
  nMinThumbSize : integer;
  RC1, RC2 : TRect;
  si : TScrollInfo;
  nRange : integer;
  nThumbPos,
    nThumbSize : integer;
  nScrollArea : integer;

begin
  nScrollSize := GetSystemMetrics(SM_CXHSCROLL);

   // The minimal thumb size depends on the system version
   // For Windows 98 minimal thumb size is a half of scrollbar size
   // and for Windows NT is always 8 pixels regardless of system metrics.
   // I really don't know why.
  if IsWin98 then // Windows 98 code
    nMinThumbSize := nScrollSize div 2
  else
    nMinThumbSize := 8;

   // Calculate the arrow rectangles
  rc1 := rect;
  rc2 := rect;

  if (nType = SB_HORZ) then
  begin
    if ((rect.right - rect.left) < 2 * nScrollSize) then //nScrollSize := ( rect.right - rect.left ) div 2;
      nScrollSize := Round((rect.right - rect.left) / 2);
    rc1.right := rect.left + nScrollSize;
    rc2.left := rect.right - nScrollSize;
  end
  else // SB_VERT
  begin
    if ((rect.bottom - rect.top) < 2 * nScrollSize) then nScrollSize := (rect.bottom - rect.top) div 2;
    rc1.bottom := rect.top + nScrollSize;
    rc2.top := rect.bottom - nScrollSize;
  end;
  if not Dragging then
  begin
     // Draw the scrollbar arrows
    DrawFlatScrollbarThumb(DC, rc1, Focused);
    DrawFlatScrollbarThumb(DC, rc2, Focused);
  end;

   // Disabled scrollbar never have a thumb
  if (bScrollbarCtrl and not (IsWindowEnabled(Wnd))) then exit;

  si.cbSize := sizeof(TSCROLLINFO);
  si.fMask := SIF_ALL;
  if bScrollbarCtrl
    then
    GetScrollInfo(Wnd, SB_CTL, si)
  else
    GetScrollInfo(Wnd, nType, si);

  if si.nPage = 0 then exit;
   // Calculate the size and position of the thumb
  nRange := si.nMax - si.nMin + 1;

  if (nRange <> 0) then
  begin
    if nType = SB_VERT
      then
      nScrollArea := (rect.bottom - rect.top) - 2 * nScrollSize
    else
      nScrollArea := (rect.right - rect.left) - 2 * nScrollSize;
    if (si.nPage = 0) then // If nPage is not set then thumb has default size
      nThumbSize := GetSystemMetrics(SM_CXHTHUMB)
    else
      nThumbSize := max(MulDiv(si.nPage, nScrollArea, nRange), nMinThumbSize);

    if (nThumbSize >= nScrollArea) then
    begin
      nThumbSize := nScrollArea;
      if (bScrollbarCtrl = FALSE) then exit;
    end;

    if (DWORD(nRange) = si.nPage) then
    begin
      nThumbPos := 0;
      dec(nThumbSize);
    end
    else
    begin
      if Dragging
        then
        nThumbPos := MulDiv(si.nTrackPos - si.nMin, nScrollArea - nThumbSize, nRange - si.nPage)
      else
        nThumbPos := MulDiv(si.nPos - si.nMin, nScrollArea - nThumbSize, nRange - si.nPage);
    end;

    if (nType = SB_VERT) then
    begin
      rc1.top := rc1.top + nScrollSize + nThumbPos;
      rc1.bottom := rc1.top + nThumbSize;
    end
    else // SB_HORZ
    begin
      rc1.left := rc1.left + nScrollSize + nThumbPos;
      rc1.right := rc1.left + nThumbSize;
    end;

    if (nThumbSize <= nScrollArea) then // Don't draw the thumb when it's larger than the scroll area
      DrawFlatScrollbarThumb(DC, rc1, Focused);
  end;
end;
{$WARNINGS on}

function DrawFlatScrollbars(Wnd : HWND; DC : HDC; Rect : TRect; Focused : boolean; ScrollBars : TScrollStyle; DragHorz, DragVert : boolean; IsControl : boolean; Style, ExStyle : integer): TRect;
var
  nFrameSize,
  hScrollSize,
  vScrollSize : integer;
  RC, RC1 : TRect;
  dwStyle : DWORD;
  st: integer;
begin
  dwStyle := Style;
  st := ExStyle;
  if ((dwStyle and WS_BORDER) = WS_BORDER) then
     nFrameSize := GetSystemMetrics(SM_CXBORDER)
  else
  if ((st and WS_EX_CLIENTEDGE) = WS_EX_CLIENTEDGE) then
     nFrameSize := GetSystemMetrics(SM_CXEDGE)
  else
     nFrameSize := 0;
  hScrollSize := GetSystemMetrics(SM_CXHSCROLL);
  vScrollSize := GetSystemMetrics(SM_CYVSCROLL);
  GetClientRect(Wnd, Rc);
  GetWindowRect(Wnd, Rc1);
  OffsetRect(Rc1, - Rc1.Left, - Rc1.Top);

  if ((dwStyle and WS_HSCROLL) <> 0) and ((dwStyle and WS_VSCROLL) <> 0)
     and ((Rc1.Right - Rc1.Left) - (Rc.Right - Rc.Left) >= nFrameSize + hScrollSize)
     and ((Rc1.Bottom - Rc1.Top) - (Rc.Bottom - Rc.Top) >= nFrameSize + vScrollSize) then
  begin
    rc.left := rect.right - nFrameSize - hScrollSize;
    rc.right := rect.right - nFrameSize;
    rc.top := rect.bottom - nFrameSize - vScrollSize;
    rc.bottom := rect.bottom - nFrameSize;
    FillRect(DC, rc, HBRUSH(COLOR_BTNFACE + 1));
  end;
  if ScrollBars = ssNone then exit;
  if ScrollBars <> ssBoth then
  begin
    if ScrollBars = ssVertical then dwStyle := dwStyle and not WS_HSCROLL;
    if ScrollBars = ssHorizontal then dwStyle := dwStyle and not WS_VSCROLL;
  end;
  if ((dwStyle and WS_HSCROLL) <> 0) and ((dwStyle and WS_VSCROLL) <> 0)
     and ((Rc1.Right - Rc1.Left) - (Rc.Right - Rc.Left) >= nFrameSize + hScrollSize)
     and ((Rc1.Bottom - Rc1.Top) - (Rc.Bottom - Rc.Top) >= nFrameSize + vScrollSize) then
  begin
    rc.left := rect.left + nFrameSize;
    rc.top := rect.bottom - nFrameSize - vScrollSize;
    rc.right := rect.right - nFrameSize - hScrollSize;
    rc.bottom := rect.bottom - nFrameSize;
    DrawFlatScrollBar(Wnd, DC, rc, SB_HORZ, IsControl, DragHorz, Focused);

    rc.left := rect.right - nFrameSize - hScrollSize;
    rc.top := rect.top + nFrameSize;
    rc.right := rect.right - nFrameSize;
    rc.bottom := rect.bottom - nFrameSize - vScrollSize;
    DrawFlatScrollBar(Wnd, DC, rc, SB_VERT, IsControl, DragVert, Focused);
  end
  else
  if ((dwStyle and WS_VSCROLL) <> 0)
     and ((Rc1.Right - Rc1.Left) - (Rc.Right - Rc.Left) >= nFrameSize + hScrollSize) then
  begin
    rc.left := rect.right - nFrameSize - hScrollSize;
    rc.top := rect.top + nFrameSize;
    rc.right := rect.right - nFrameSize;
    rc.bottom := rect.bottom - nFrameSize;
    DrawFlatScrollBar(Wnd, DC, rc, SB_VERT, IsControl, DragVert, Focused);
  end
  else
  if ((dwStyle and WS_HSCROLL) <> 0)
     and ((Rc1.Bottom - Rc1.Top) - (Rc.Bottom - Rc.Top) >= nFrameSize + vScrollSize) then
  begin
    rc.left := rect.left + nFrameSize;
    rc.top := rect.bottom - nFrameSize - vScrollSize;
    rc.right := rect.right - nFrameSize;
    rc.bottom := rect.bottom - nFrameSize;
    DrawFlatScrollBar(Wnd, DC, rc, SB_HORZ, IsControl, DragHorz, Focused);
  end;
end;

{$warnings off}

procedure DrawFlatScrollBarEx(Wnd : HWND; DC : HDC; rect : TRect; nType :
    integer; bScrollbarCtrl : boolean; Dragging : boolean; Focused : boolean;
    BkColor, DitherColor, ButtonColor, ArrowColor, hotbuTtoncolor : TColor;
    DrawFrames, DitherBack : boolean);
var
  nScrollSize : integer;
  nMinThumbSize : integer;
  RC1, RC2 : TRect;
  si : TScrollInfo;
  nRange : integer;
  nThumbPos,
    nThumbSize : integer;
  nScrollArea : integer;

begin
  nScrollSize := GetSystemMetrics(SM_CXHSCROLL);

   // The minimal thumb size depends on the system version
   // For Windows 98 minimal thumb size is a half of scrollbar size
   // and for Windows NT is always 8 pixels regardless of system metrics.
   // I really don't know why.
  if IsWin98 then // Windows 98 code
    nMinThumbSize := nScrollSize div 2
  else
    nMinThumbSize := 8;

   // Calculate the arrow rectangles
  rc1 := rect;
  rc2 := rect;

  if (nType = SB_HORZ) then
  begin
    if ((rect.right - rect.left) < 2 * nScrollSize) then //nScrollSize := ( rect.right - rect.left ) div 2;
      nScrollSize := Round((rect.right - rect.left) / 2);
    rc1.right := rect.left + nScrollSize;
    rc2.left := rect.right - nScrollSize;
  end
  else // SB_VERT
  begin
    if ((rect.bottom - rect.top) < 2 * nScrollSize) then nScrollSize := (rect.bottom - rect.top) div 2;
    rc1.bottom := rect.top + nScrollSize;
    rc2.top := rect.bottom - nScrollSize;
  end;
  if not Dragging then
  begin
     // Draw the scrollbar arrows
    DrawFlatScrollbarThumb(DC, rc1, Focused);
    DrawFlatScrollbarThumb(DC, rc2, Focused);
  end;

   // Disabled scrollbar never have a thumb
  if (bScrollbarCtrl and not (IsWindowEnabled(Wnd))) then exit;

  si.cbSize := sizeof(TSCROLLINFO);
  si.fMask := SIF_ALL;
  if bScrollbarCtrl
    then
    GetScrollInfo(Wnd, SB_CTL, si)
  else
    GetScrollInfo(Wnd, nType, si);

   // Calculate the size and position of the thumb
  nRange := si.nMax - si.nMin + 1;

  if (nRange <> 0) then
  begin
    if nType = SB_VERT
      then
      nScrollArea := (rect.bottom - rect.top) - 2 * nScrollSize
    else
      nScrollArea := (rect.right - rect.left) - 2 * nScrollSize;
    if (si.nPage = 0) then // If nPage is not set then thumb has default size
      nThumbSize := GetSystemMetrics(SM_CXHTHUMB)
    else
      nThumbSize := max(MulDiv(si.nPage, nScrollArea, nRange), nMinThumbSize);

    if (nThumbSize >= nScrollArea) then
    begin
      nThumbSize := nScrollArea;
      if (bScrollbarCtrl = FALSE) then exit;
    end;

    if (DWORD(nRange) = si.nPage) then
    begin
      nThumbPos := 0;
      dec(nThumbSize);
    end
    else
    begin
      if Dragging
        then
        nThumbPos := MulDiv(si.nTrackPos - si.nMin, nScrollArea - nThumbSize, nRange - si.nPage)
      else
        nThumbPos := MulDiv(si.nPos - si.nMin, nScrollArea - nThumbSize, nRange - si.nPage);
    end;

    if (nType = SB_VERT) then
    begin
      rc1.top := rc1.top + nScrollSize + nThumbPos;
      rc1.bottom := rc1.top + nThumbSize;
    end
    else // SB_HORZ
    begin
      rc1.left := rc1.left + nScrollSize + nThumbPos;
      rc1.right := rc1.left + nThumbSize;
    end;

    if (nThumbSize <= nScrollArea) then // Don't draw the thumb when it's larger than the scroll area
      DrawFlatScrollbarThumb(DC, rc1, Focused);
  end;
end;

procedure DrawFlatScrollBarsEx(Wnd : HWND; DC : HDC; Rect : TRect; Focused :
    boolean; ScrollBars : TScrollStyle; DragHorz, DragVert : boolean; IsControl
    : boolean; BkColor, DitherColor,  ButtonColor, ArrowColor, HotButtonColor :
    TColor; DrawFrames, DitherBack : boolean);
var
  nFrameSize,
  hScrollSize,
  vScrollSize : integer;
  RC, RC1 : TRect;
  dwStyle : DWORD;
  st: integer;
begin
  if ScrollBars = ssNone then exit;
  dwStyle := GetWindowLong(Wnd, GWL_STYLE);
  st := GetWindowLong(Wnd, GWL_EXSTYLE);
  if ((dwStyle and WS_BORDER) = WS_BORDER) then
     nFrameSize := GetSystemMetrics(SM_CXBORDER)
  else
  if ((st and WS_EX_CLIENTEDGE) = WS_EX_CLIENTEDGE) then
     nFrameSize := GetSystemMetrics(SM_CXEDGE)
  else
     nFrameSize := 0;
  hScrollSize := GetSystemMetrics(SM_CXHSCROLL);
  vScrollSize := GetSystemMetrics(SM_CYVSCROLL);
  GetClientRect(Wnd, Rc);
  GetWindowRect(Wnd, Rc1);
  OffsetRect(Rc1, - Rc1.Left, - Rc1.Top);

  if ScrollBars <> ssBoth then
  begin
    if ScrollBars = ssVertical then dwStyle := dwStyle and not WS_HSCROLL;
    if ScrollBars = ssHorizontal then dwStyle := dwStyle and not WS_VSCROLL;
  end;
  if ((dwStyle and WS_HSCROLL) <> 0) and ((dwStyle and WS_VSCROLL) <> 0)
     and ((Rc1.Right - Rc1.Left) - (Rc.Right - Rc.Left) >= nFrameSize + hScrollSize)
     and ((Rc1.Bottom - Rc1.Top) - (Rc.Bottom - Rc.Top) >= nFrameSize + vScrollSize) then
  begin
    rc.left := rect.left + nFrameSize;
    rc.top := rect.bottom - nFrameSize - vScrollSize;
    rc.right := rect.right - nFrameSize - hScrollSize;
    rc.bottom := rect.bottom - nFrameSize;
    DrawFlatScrollBarEx(Wnd, DC, rc, SB_HORZ, IsControl, DragHorz, Focused,
                        BkColor, DitherColor, ButtonColor, ArrowColor,
                        HotButtonColor, DrawFrames, DitherBack);

    rc.left := rect.right - nFrameSize - hScrollSize;
    rc.top := rect.top + nFrameSize;
    rc.right := rect.right - nFrameSize;
    rc.bottom := rect.bottom - nFrameSize - vScrollSize;
    DrawFlatScrollBarEx(Wnd, DC, rc, SB_VERT, IsControl, DragVert, Focused,
                        BkColor, DitherColor, ButtonColor, ArrowColor,
                        HotButtonColor, DrawFrames, DitherBack);

    rc.left := rect.right - nFrameSize - hScrollSize;
    rc.right := rect.right - nFrameSize;
    rc.top := rect.bottom - nFrameSize - vScrollSize;
    rc.bottom := rect.bottom - nFrameSize;
    FillRect(DC, rc, HBRUSH(COLOR_BTNFACE + 1));
  end
  else
  if ((dwStyle and WS_VSCROLL) <> 0)
     and ((Rc1.Bottom - Rc1.Top) - (Rc.Bottom - Rc.Top) >= nFrameSize + vScrollSize) then
  begin
    rc.left := rect.right - nFrameSize - hScrollSize;
    rc.top := rect.top + nFrameSize;
    rc.right := rect.right - nFrameSize;
    rc.bottom := rect.bottom - nFrameSize;
    DrawFlatScrollBarEx(Wnd, DC, rc, SB_VERT, IsControl, DragVert, Focused,
                        BkColor, DitherColor, ButtonColor, ArrowColor,
                        HotButtonColor, DrawFrames, DitherBack);
  end
  else
  if ((dwStyle and WS_HSCROLL) <> 0)
     and ((Rc1.Right - Rc1.Left) - (Rc.Right - Rc.Left) >= nFrameSize + hScrollSize) then
  begin
    rc.left := rect.left + nFrameSize;
    rc.top := rect.bottom - nFrameSize - vScrollSize;
    rc.right := rect.right - nFrameSize;
    rc.bottom := rect.bottom - nFrameSize;
    DrawFlatScrollBarEx(Wnd, DC, rc, SB_HORZ, IsControl, DragHorz, Focused,
                        BkColor, DitherColor, ButtonColor, ArrowColor,
                        HotButtonColor, DrawFrames, DitherBack);
  end;
end;
{$endif}

{$warnings on}

{$ifndef CLX_USED}
function DrawBevel(DC : HDC; R : TRect; Color1, Color2 : TColor; Sides : TElBorderSides) : TRect;
var
  APN,
  OPN : HPEN;
begin
  APN := CreatePen(PS_SOLID, 1, ColorToRGB(Color1));
  OPN := SelectObject(DC, APN);
  if ebsLeft in Sides then
  begin
    MoveToEx(DC, R.Left, R.Top, nil);
    LineTo(DC, R.Left, R.Bottom);
  end;
  if ebsTop in Sides then
  begin
    MoveToEx(DC, R.Left, R.Top, nil);
    LineTo(DC, R.Right, R.Top);
  end;

  SelectObject(DC, OPN);
  DeleteObject(APN);

  APN := CreatePen(PS_SOLID, 1, ColorToRGB(Color2));
  SelectObject(DC, APN);
  if ebsRight in Sides then
  begin
    MoveToEx(DC, R.Right - 1, R.Top, nil);
    LineTo(DC, R.Right - 1, R.Bottom);
  end;
  if ebsBottom in Sides then
  begin
    MoveToEx(DC, R.Left, R.Bottom - 1, nil);
    LineTo(DC, R.Right, R.Bottom - 1);
  end;
  SelectObject(DC, OPN);
  DeleteObject(APN);

  if ebsLeft in Sides then inc(R.Left);
  if ebsTop in Sides then inc(R.Top);
  if ebsRight in Sides then dec(R.Right);
  if ebsBottom in Sides then dec(R.Bottom);
  Result := R;
end;
{$endif}

{$ifndef CLX_USED}
procedure AlphaCopyRect(DestCanvas : TCanvas; Dest: TRect; SourceCanvas: TCanvas; Source: TRect; AlphaLevel : byte; UseAlphaLevel : boolean);
var i, j, k   : integer;
    Color,
    DstColor  : TColor;
    DstBitmap,
    TmpBitmap : TBitmap;

    {h,} w : integer;
    Info : TBitmapInfo;
    pBits,
    dBits: pchar;
    p, dp: PInteger;
begin
  TmpBitmap := TBitmap.Create;
  TmpBitmap.Width  := Dest.Right - Dest.Left;
  TmpBitmap.Height := Dest.Bottom - Dest.Top;
  TmpBitmap.PixelFormat := pf32Bit;

  DstBitmap := TBitmap.Create;
  DstBitmap.Width  := Dest.Right - Dest.Left;
  DstBitmap.Height := Dest.Bottom - Dest.Top;
  DstBitmap.PixelFormat := pf32Bit;

  if (Dest.Right - Dest.Left = Source.Right - Source.Left) and
     (Dest.Bottom - Dest.Top = Source.Bottom - Source.Top) then
    bitblt(TmpBitmap.Canvas.Handle, 0, 0, Dest.Right - Dest.Left, Dest.Bottom - Dest.Top, SourceCanvas.Handle, Source.Left, Source.Top, srccopy)
  else
    StretchBlt(TmpBitmap.Canvas.Handle, 0,0, TmpBitmap.Width, TmpBitmap.Height,
               sourcecAnvas.Handle, Source.Left, Source.Top,
               Source.Right - Source.Left, Source.Bottom - Source.Top, SRCCOPY);

  bitblt(DstBitmap.Canvas.Handle, 0, 0, Dest.Right - Dest.Left, Dest.Bottom - Dest.Top, DestCanvas.Handle, Dest.Left, Dest.Top, srccopy);

  ZeroMemory(@Info.bmiHeader, sizeof(Info.bmiHeader));
  Info.bmiHeader.biSize := sizeof(Info.bmiHeader);
  Info.bmiHeader.biWidth := TmpBitmap.Width;
  Info.bmiHeader.biHeight := TmpBitmap.Height;
  Info.bmiHeader.biPlanes := 1;
  Info.bmiHeader.biBitCount := 32;
  Info.bmiHeader.biCompression := BI_RGB;

  i := TmpBitmap.Width * TmpBitmap.Height * sizeof(Integer);
  GetMem(pBits, i);
  GetDIBits(DestCanvas.Handle, TmpBitmap.Handle, 0, TmpBitmap.Height, pBits, Info, DIB_RGB_COLORS);

  GetMem(dBits, i);
  GetDIBits(DestCanvas.Handle, DstBitmap.Handle, 0, TmpBitmap.Height, dBits, Info, DIB_RGB_COLORS);

  //h := TmpBitmap.Height;
  w := TmpBitmap.Width;
  for j := 0 to Dest.Bottom - Dest.Top -1 do
  begin
    for i := 0 to Dest.Right - Dest.Left -1 do
    begin
      k := ({H - 1 - }j) * w + i;
      p := PInteger(pBits);
      inc(p, k);
      dp := PInteger(dBits);
      inc(dp, k);

      Color := P^;
      DstColor := DP^;

      if not UseAlphaLevel then
        AlphaLevel := Color shr 24;

      P^ := ((((Color and $FF0000 shr 16) * AlphaLevel + (DstColor and $FF0000 shr 16) * (256 - AlphaLevel)) shl 8) and $FF0000) or
            (((Color and $FF00 shr 8) * AlphaLevel + (DstColor and $FF00 shr 8) * (256 - AlphaLevel)) and $00FF00) or
            (((Color and $FF) * AlphaLevel + (DstColor and $FF) * (256 - AlphaLevel)) shr 8 and $FF);
    end;
  end;
  SetDIBits(TmpBitmap.Canvas.Handle, TmpBitmap.Handle, 0, TmpBitmap.Height, pBits, Info, DIB_RGB_COLORS);
  bitblt(DestCanvas.Handle, Dest.Left, Dest.Top, Dest.Right - Dest.Left, Dest.Bottom - Dest.Top, TmpBitmap.Canvas.Handle, 0, 0, srccopy);
  TmpBitmap.Free;
  DstBitmap.Free;
  FreeMem(dBits);
  FreeMem(pBits);
end;

procedure AlphaFillRect(Canvas : TCanvas; Rect : TRect; Color : TColor; AlphaLevel : byte);
var i, j: integer;
    DstColor : TColor;
    SrcRValue,
    SrcGValue,
    SrcBValue: integer;
    DstRValue,
    DstGValue,
    DstBValue: byte;
begin
  SrcRValue := GetRValue(Color);
  SrcGValue := GetGValue(Color);
  SrcBValue := GetBValue(Color);
  for i := Rect.Left to Rect.Right -1 do
  begin
    for j := Rect.Top to Rect.Bottom - 1 do
    begin
      DstColor := Canvas.Pixels[i, j];
      DstRValue := (SrcRValue * AlphaLevel + GetRValue(DstColor) * (256 - AlphaLevel)) shr 8;
      DstGValue := (SrcGValue * AlphaLevel + GetGValue(DstColor) * (256 - AlphaLevel)) shr 8;
      DstBValue := (SrcBValue * AlphaLevel + GetBValue(DstColor) * (256 - AlphaLevel)) shr 8;
      DstColor  := RGB(DstRValue, DstGValue, DstBValue);
      Canvas.Pixels[i, j] := DstColor;
    end;
  end;
end;

(*
function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: UINT): Integer;
var MaxW,
    TotalH: integer;
    ASize : TSize;
    S, S1 : WideString;
    l, i  : Cardinal;
    R     : TRect;
    Lines : TElWideStringList;
    TabSpc: integer;

    function AdjustText(const Text : WideString; MaxWidth : Integer; var RealWidth : integer) : WideString;
    var Size : TSize;
        S    : WideString;
        l    : integer;
    begin
      Result := Text;
      repeat
        Result := WideCopy(Result, 1, Length(Result) - 1);
        S := Result + '...';
        l := Length(Result) + 3;
        if l > 3 then
        begin
          if not GetTextExtentPoint32W(hDC, PWideChar(S), l, ASize) then
          begin
            RealWidth := MaxWidth;
            exit;
          end;
          if (ASize.cx <= MaxWidth) or (l = 4) then
          begin
            RealWidth := ASize.cx;
            Result := Result + '...';
            exit;
          end;
        end
        else
        begin
          RealWidth := 0;
          Result := '';
          exit;
        end;
      until Length(Result) = 1;
      if Length(Result) = 1 then
      begin
        Result := Result + '...';
        l := Length(Result);
        GetTextExtentPoint32W(hDC, PWideChar(Result), l, ASize);
        RealWidth := ASize.cx;
      end;
    end;

begin
  if false then // IsWinNTUp then
  begin
    Windows.DrawTextW(hDC, lpString, nCount, lpREct, uFormat)
  end
  else
  begin
    S := WideStrPas(lpString);
    if nCount <> -1 then
      l := Min(Length(S), nCount)
    else
      l := Length(S);

    if uFormat and DT_SINGLELINE = DT_SINGLELINE then
      while WideReplace(S, WideString(#13#10), '  ') do ;

    if uFormat and DT_TABSTOP = DT_TABSTOP then
    begin
      TabSpc := (HIBYTE(LOWORD(uFormat)));
      if uFormat and DT_CALCRECT = DT_CALCRECT then
        exit;
      uFormat := uFormat and not (DT_EXTERNALLEADING or DT_INTERNAL or DT_NOCLIP or DT_NOPREFIX);
    end
    else
      TabSpc := 8;

    if uFormat and DT_EXPANDTABS = DT_EXPANDTABS then
    begin
      S1 := GetWideStringOf(' ', TabSpc);
      while WideReplace(S, WideString(#9), S1) do inc(l, Length(S1) - 1);
      if uFormat and DT_CALCRECT = DT_CALCRECT then
        exit;
      uFormat := uFormat and not (DT_EXTERNALLEADING or DT_INTERNAL or DT_NOCLIP or DT_NOPREFIX);
    end;
    
    // hide prefix if needed
    if uFormat and DT_NOPREFIX <> DT_NOPREFIX then
    begin
      i := 1;
      while i < Length(S) do
      begin
        if (S[i] = WideChar('&')) and (S[i + 1] = WideChar('&')) then
        begin
          WideDelete(S, i, 1);
          dec(l);
          inc(i);
        end
        else
        if (S[i] = WideChar('&')) then
        begin
          WideDelete(S, i, 1);
          dec(l);
        end
        else
        begin
          inc(i);
        end;
      end;
    end;

    // calculate size of multiline text
    if uFormat and DT_SINGLELINE <> DT_SINGLELINE then
    begin
      Lines := TElWideStringList.Create;
      Lines.Text := S;
      MaxW := 0;
      TotalH := 0;
      for i := 0 to Lines.Count - 1 do
      begin
        if not GetTextExtentPoint32W(hDC, PWideChar(Lines[i]), Length(Lines[i]), ASize) then
        begin
          result := 0;
          exit;
        end;
        Lines.Objects[i] := TObject(Pointer(ASize.cy));
        inc(TotalH, ASize.cy);
        if (ASize.cx > lpRect.Right - lpRect.Left) and
           (uFormat and DT_CALCRECT <> DT_CALCRECT) and
           (uFormat and (DT_END_ELLIPSIS or DT_WORD_ELLIPSIS) <> 0) then
        begin
          Lines[i] := AdjustText(Lines[i], lpRect.Right - lpRect.Left, ASize.cx);
        end;
        MaxW := Max(MaxW, ASize.cx);
      end;
    end
    else
    begin
      if not GetTextExtentPoint32W(hDC, PWideChar(S), l, ASize) then
      begin
        result := 0;
        exit;
      end;
      TotalH := ASize.cy;
      MaxW := ASize;
      if uFormat and DT_CALCRECT = DT_CALCRECT then
      begin
        Rect.Right := Rect.Left + MaxW;
        Rect.Bottom := Rect.Top + TotalH;
        exit;
      end
      else
      begin
        
      end;

    end;

  end;
end;
*)

{$ifndef CLX_USED}
{$ifndef BROKEN_UNICODE}

function NextLineW(hdc : HDC; const str : PWideChar; count : PInteger;
                        dest : PWideChar; len : PInteger; width : integer; format : Word;
                        var tabwidth, spAcewidth, prefix_offset, overhang : integer) : PWideChar;
var
  i, j, k : integer;
  plen : integer;
  numspaces : integer;
  Size : TSize;
  lasttab : integer;
  wb_i,
  wb_j,
  wb_count : integer;
  maxl : integer;
  bb : boolean;

begin
  i := 0;
  j := 0;
  plen := 0;
  lasttab := 0;
  wb_i := 0; wb_j := 0;
  wb_count := 0;
  maxl := len^;

  while (count^ <> 0) and (j < maxl) do
  begin
    case (str[i]) of
      WideChar(#13),
      WideChar(#10):
         begin
  	   if (format and DT_SINGLELINE) <> DT_SINGLELINE then
  	   begin
  	     if ((count^ > 1) and (str[i] = WideChar(#13)) and (str[i+1] = WideChar(#10))) then
             begin
               dec(count^);
               inc(i);
             end;
  	     inc(i);
             len^ := j;
             dec(count^);
             result := (@str[i]);
             exit;
  	   end;

  	   dest[j] := str[i]; inc(j); inc(i);

  	   if ((Format and DT_NOCLIP)    <> DT_NOCLIP) or
              ((Format and DT_NOPREFIX)  <> DT_NOPREFIX) or
  	      ((Format and DT_WORDBREAK) = DT_WORDBREAK) then
  	   begin
  	     if (not GetTextExtentPoint32W(hdc, @dest[j-1], 1, size)) then
             begin
               result := nil;
               exit;
             end;
  	     inc(plen, size.cx - overhang);
  	   end;
        end;
  	WideChar('&'):
          begin
            bb := true;
  	    if ((format and DT_NOPREFIX) <> DT_NOPREFIX) and (count^ > 1) then
            begin
              inc(i);
              if (str[i] = WideChar('&')) then
                dec(count^)
              else
              begin
                prefix_offset := j;
                bb := false;
              end;
  	    end;
            if bb then
            begin
              dest[j] := str[i]; inc(j); inc(i);
              if ((Format and DT_NOCLIP)    <> DT_NOCLIP) or
                ((Format and DT_NOPREFIX)  <> DT_NOPREFIX) or
                ((Format and DT_WORDBREAK) = DT_WORDBREAK) then
              begin
                if (not GetTextExtentPoint32W(hdc, @dest[j-1], 1, size)) then
                begin
                  result := nil;
                  exit;
                end;
                inc(plen, size.cx - overhang);
              end;
            end;
          end;
  	WideChar(#9):
  	  if (format and DT_EXPANDTABS) = DT_EXPANDTABS then
          begin
            inc(i);
            wb_i := i;
  	    wb_j := j;
  	    wb_count := count^;

  	    if (not GetTextExtentPoint32W(hdc, @dest[lasttab], j - lasttab, size)) then
            begin
  	      result := nil;
              exit;
            end;

  	    numspaces := (tabwidth - (size.cx - overhang)) div spacewidth;
  	    for k := 0 to numspaces - 1 do
            begin
  	      dest[j] := WideChar(#32);
              inc(j);
            end;
  	    inc(plen, tabwidth - size.cx);
  	    lasttab := wb_j + numspaces;
  	  end
  	  else
  	  begin
            dest[j] := str[i]; inc(j); inc(i);

            if ((Format and DT_NOCLIP)    <> DT_NOCLIP) or
                ((Format and DT_NOPREFIX)  <> DT_NOPREFIX) or
                ((Format and DT_WORDBREAK) = DT_WORDBREAK) then
  	    begin
              if (not GetTextExtentPoint32W(hdc, @dest[j-1], 1, size)) then
              begin
                result := nil;
                exit;
              end;
              inc(plen, size.cx - overhang);
  	    end;
  	  end;
  	WideChar(#32):
          begin
  	    dest[j] := str[i]; inc(j); inc(i);

  	    if ((Format and DT_NOCLIP)    <> DT_NOCLIP) or
                ((Format and DT_NOPREFIX)  <> DT_NOPREFIX) or
                ((Format and DT_WORDBREAK) = DT_WORDBREAK) then
  	    begin
  	      wb_i := i;
  	      wb_j := j - 1;
  	      wb_count := count^;
  	      if (not GetTextExtentPoint32W(hdc, @dest[j-1], 1, size)) then
              begin
                result := nil;
                exit;
              end;
              inc(plen, size.cx - overhang);
  	    end;
          end;
  	else
          begin                      
  	    dest[j] := str[i]; inc(j); inc(i);

  	    if ((Format and DT_NOCLIP)    <> DT_NOCLIP) or
                ((Format and DT_NOPREFIX)  <> DT_NOPREFIX) or
                ((Format and DT_WORDBREAK) = DT_WORDBREAK) then
  	    begin
              if (not GetTextExtentPoint32W(hdc, @dest[j-1], 1, size)) then
              begin
                result := nil;
                exit;
              end;
              inc(plen, size.cx - overhang);
  	    end;
          end;
    end;

    dec(count^);
    if ((format and DT_NOCLIP) <> DT_NOCLIP) or
       ((format and DT_WORDBREAK) = DT_WORDBREAK) then
    begin
      if (plen > width) {and (((format and DT_CALCRECT) <> DT_CALCRECT) or (width <> 0))} then
      begin
    	if (format and DT_WORDBREAK) = DT_WORDBREAK then
    	begin
    	  if (wb_j <> 0) then
    	  begin
    	    len^ := wb_j;
    	    count^ := wb_count - 1;
    	    result := (@str[wb_i]);
            exit;
    	  end
    	end
    	else
    	begin
    	  len^ := j;
    	  result := (@str[i]);
          exit;
    	end;
      end;
    end;
  end;

  len^ := j;
  result := nil;
end;

function DrawTextExW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: UINT; dtp : PDRAWTEXTPARAMS): Integer;

const MAX_STATIC_BUFFER = 8192;
const STATIC_BUFFER_LEN : integer = 8192;
const DT_PREFIXONLY = $00200000;

var size : TSize;
    strPtr : PWideChar;
    line   : PWideChar;
    swapStr: PWideChar;
    linestat: array[0..MAX_STATIC_BUFFER - 1] of WideChar;
    swapstat: array[0..MAX_STATIC_BUFFER - 1] of WideChar;
    len,
    lh,
    count  : integer;
    prefix_x: integer;
    prefix_end : integer;
    tm : TTextMetricA;
    lmargin : integer;
    rmargin : integer;
    x, y,
    width,
    max_width : integer;
    tabstop : integer;
    tabwidth,
    spacewidth,
    prefix_offset : integer;
    fnameDelim : PWideChar;
    totalLen   : integer;
    fnameLen   : integer;
    lastBkSlash: PWideChar;
    lastFwdSlash: PWideChar;
    apen,
    oldPen     : HPEN;
    etoFlags : integer;
    statBufUsed: boolean;
    strCopyUsed: boolean;
begin
  len := nCount;
  if len = -1 then
    len := WideStrLen(lpString);
  if len < MAX_STATIC_BUFFER - 4 then
  begin
    strCopyUsed := false;
    statBufUsed := true;
    line := @LineStat;
    swapStr := @swapStat;
    STATIC_BUFFER_LEN := MAX_STATIC_BUFFER;
  end
  else
  begin
    strCopyUsed := false;
    if not IsWinNTUp then
    begin
      strCopyUsed := true;
      GetMem(Line, (len + 1) * sizeof(WideChar));
      WideStrLCopy(Line, lpString, len);
      lpString := Line;
    end;
    statBufUsed := false;
    STATIC_BUFFER_LEN := len;
    GetMem(Line, (len + 4) * sizeof(WideChar));
    GetMem(swapStr, (len + 4) * sizeof(WideChar));
  end;
  try
    tabStop := 8;
    count := nCount;
    prefix_x := 0;
    prefix_end := 0;
    lmargin := 0;
    rmargin := 0;
    x := lpRect.left;
    y := lpRect.top;
    max_width := 0;

    if (lpString = nil) then
    begin
      result := 0;
      exit;
    end;

    if (count = -1) then
      count := WideStrLen(lpString);
    if (count = 0) then
    begin
      lpRect.Right := lpRect.Left;
      lpRect.Bottom := lpRect.Top;
      result := 0;
      exit;
    end;

    if (uFormat and DT_CALCRECT) = DT_CALCRECT then
    begin
      if (lpRect.Right = 0) then
        lpRect.Right := 32767;
      if (lpRect.Bottom = 0) then
        lpRect.Bottom := 32767;
    end;

    width := lpRect.right - lpRect.left;

    strPtr := lpString;

    GetTextMetrics(hdc, tm);
    if (uFormat and DT_EXTERNALLEADING) = DT_EXTERNALLEADING then
      lh := tm.tmHeight + tm.tmExternalLeading
    else
      lh := tm.tmHeight;

    if dtp <> nil then
    begin
      lmargin := dtp.iLeftMargin * tm.tmAveCharWidth;
      rmargin := dtp.iRightMargin * tm.tmAveCharWidth;
      if (uFormat and (DT_CENTER or DT_RIGHT)) = 0 then
          inc(x, lmargin);
      dtp.uiLengthDrawn := 0;
    end;

    if (uFormat and DT_TABSTOP) = DT_TABSTOP then
      if dtp <> nil then
        tabStop := dtp.iTabLength
      else
        tabStop := uFormat shr 8;

    if (uFormat and DT_EXPANDTABS) = DT_EXPANDTABS then
    begin
      GetTextExtentPoint32W(hdc, WideChar(' '), 1, size);
      spacewidth := size.cx - tm.tmOverhang;
      GetTextExtentPoint32W(hdc, WideChar('o'), 1, size);
      tabwidth := (size.cx - tm.tmOverhang) * tabstop;
    end;

    if (uFormat and DT_CALCRECT) = DT_CALCRECT then
      uFormat := uFormat or DT_NOCLIP;

    repeat
      prefix_offset := -1;
      len := STATIC_BUFFER_LEN;
      strPtr := NextLineW(hdc, strPtr, @count, line, @len, width, uFormat, tabwidth, spacewidth, prefix_offset, tm.tmOverhang);

      if (prefix_offset <> -1) then
      begin
        GetTextExtentPoint32W(hdc, line, prefix_offset, size);
        prefix_x := size.cx;
        GetTextExtentPoint32W(hdc, line, prefix_offset + 1, size);
        prefix_end := size.cx - 1;
      end;

      if (not GetTextExtentPoint32W(hdc, line, len, size)) then
      begin
        result := 0;
        exit;
      end
      else
        inc(size.cx, tm.tmOverhang);

      if (uFormat and DT_CALCRECT) <> DT_CALCRECT then
      begin
        if (uFormat and DT_CENTER) = DT_CENTER then
          x := max(lpRect.Left, (lpRect.left + lpRect.right - size.cx) shr 1)
        else
        if (uFormat and DT_RIGHT) = DT_RIGHT then
          x := max(lpRect.Left, lpRect.right - size.cx);
      end;

      if (uFormat and DT_SINGLELINE) = DT_SINGLELINE then
      begin
        if (uFormat and DT_CALCRECT) <> DT_CALCRECT then
        begin
          if (uFormat and DT_VCENTER) = DT_VCENTER then
            y := lpRect.top + (lpRect.bottom - lpRect.top) shr 1 - size.cy shr 1
          else
          if (uFormat and DT_BOTTOM) = DT_BOTTOM then
            y := lpRect.bottom - size.cy;
        end;

        if ((uFormat and (DT_PATH_ELLIPSIS or DT_END_ELLIPSIS or DT_WORD_ELLIPSIS)) <> 0) then
        begin
          // fnameDelim := nil;
          if nCount >= 0 then
            totalLen := nCount
          else
            totalLen := WideStrLen(lpString);

          if (size.cx > width) then
          begin
            fnameLen := totalLen;

            count := min(totalLen + 3, STATIC_BUFFER_LEN - 3);

            if (uFormat and DT_WORD_ELLIPSIS) = DT_WORD_ELLIPSIS then
              uFormat := uFormat or DT_WORDBREAK;

            if (uFormat and DT_PATH_ELLIPSIS) = DT_PATH_ELLIPSIS then
            begin
              lastBkSlash := nil;
              lastFwdSlash := nil;

              WideStrLCopy(line, lpString, totalLen);
              line[totalLen] := WideChar('0');

              lastBkSlash := WideStrRScan(line, WideChar('\'));
              lastFwdSlash := wideStrrscan(line, WideChar('/'));
              if Integer(lastBkSlash) > integer(lastFwdSlash) then
                fnameDelim := lastBkSlash
              else
                fnameDelim := lastFwdSlash;

              if (fnameDelim <> nil) then
                fnameLen := @line[totalLen] - fnameDelim
              else
                fnameDelim := lpString;

              WideStrCopy(swapStr, '...'#0);
              WideStrLCopy(swapStr + WideStrLen(swapStr), fnameDelim, fnameLen);
              swapStr[fnameLen + 3] := WideChar(#0);
              WideStrLCopy(swapStr + WideStrLen(swapStr), lpString, totalLen - fnameLen);
              swapStr[totalLen + 3] := WideChar(#0);
            end
            else  (* DT_END_ELLIPSIS | DT_WORD_ELLIPSIS *)
            begin
              WideStrCopy(swapStr, '...'#0);
              WideStrLCopy(swapStr + WideStrLen(swapStr), lpString, totalLen);
            end;

            len := STATIC_BUFFER_LEN;
            NextLineW(hdc, swapStr, @count, line, @len, width, uFormat, tabWidth, spacewidth, prefix_offset, tm.tmOverhang);

            (* if only the ELLIPSIS will fit, just let it be clipped *)
            len := max(4, len);
            GetTextExtentPoint32W(hdc, line, len, size);
            inc(size.cx, tm.tmOverhang);
            (* FIXME:
             * NextLine uses GetTextExtentPoint for each character,
             * rather than GetCharABCWidth...  So the whitespace between
             * characters is ignored in the width measurement, and the
             * reported len is too great.  To compensate, we must get
             * the width of the entire line and adjust len accordingly.
            *)
            while ((size.cx > width) and (len > 3)) do
            begin
              dec(len);
              line[len] := WideChar(#0);
              GetTextExtentPoint32W(hdc, line, len, size);
              inc(size.cx, tm.tmOverhang);
            end;

            if (fnameLen < len - 3) then
            begin
              (* put the ELLIPSIS between the path and filename *)
              WideStrLCopy(swapStr, @line[fnameLen+3], len - 3 - fnameLen);
              swapStr[len - 3 - fnameLen] := WideChar(#0);
              WideStrCat(swapStr, '...'#0);
              WideStrLCopy(swapStr + WideStrLen(swapStr), @line[3], fnameLen);
            end
            else
            begin
              (* move the ELLIPSIS to the end *)
              WideStrLCopy(swapStr, @line[3], len - 3);
              swapStr[len - 3] := WideChar(#0);
              WideStrCopy(swapStr + WideStrLen(swapStr), '...'#0);
            end;

            WideStrLCopy(line, swapStr, len);
            line[len] := WideChar(#0);
            strPtr := nil;
          end;
        end;
        if (uFormat and DT_MODIFYSTRING) = DT_MODIFYSTRING then
          WideStrCopy(lpString, swapStr);
      end;
      if (uFormat and DT_CALCRECT) <> DT_CALCRECT then
      begin
        if (uFormat and DT_PREFIXONLY) <> DT_PREFIXONLY then
        begin
          etoFlags := 0;
          if uFormat and DT_NOCLIP <> DT_NOCLIP then
            etoFlags := etoFlags or ETO_CLIPPED;
          if uFormat and DT_RTLREADING = DT_RTLREADING then
            etoFlags := etoFlags or DT_RTLREADING;

          if (etoFlags and ETO_CLIPPED) = ETO_CLIPPED then
            InflateRect(lpRect, 1, 1);
          if (not ExtTextOutW( hdc, x, y, etoFlags, @lpRect, line, len, nil )) then
          begin
            result := 0;
            exit;
          end;
          if (etoFlags and ETO_CLIPPED) = ETO_CLIPPED then
            InflateRect(lpRect, -1, -1);
        end;
        if (prefix_offset <> -1) and ((uFormat and DT_HIDEPREFIX) = 0) then
        begin
          apen := CreatePen( PS_SOLID, 1, gEttextcolor(hdc) );
          oldPen := SelectObject( hdc, apen );
          MoveToEx(hdc, x + prefix_x, y + tm.tmAscent + 1, nil );
          LineTo(hdc, x + prefix_end + 1, y + tm.tmAscent + 1 );
          SelectObject( hdc, oldPen );
          DeleteObject( apen );
        end
      end
      else
        if (size.cx > max_width) then
          max_width := size.cx;

      inc(y, lh);
      if (strPtr <> nil) then
      begin
        if (uFormat and DT_CALCRECT) <> DT_CALCRECT then
        begin
          if (uFormat and DT_NOCLIP <> DT_NOCLIP) then
          begin
            if (y > lpRect.bottom - lh) then
              break;
          end;
        end;
      end;
      if dtp <> nil then
        inc(dtp.uiLengthDrawn, len);
    until (StrPtr = nil);

    if (uFormat and DT_CALCRECT) = DT_CALCRECT then
    begin
      lpRect.right := lpRect.left + max_width;
      lpRect.bottom := y;
      if (dtp <> nil) then
        inc(lpRect.right, lmargin + rmargin);
    end;
    result := y - lpRect.top;
  finally
    if strCopyUsed then
      FreeMem(lpString);
    if not statBufUsed then
    begin
      FreeMem(Line);
      FreeMem(swapStr);
    end;
  end
end;

{$endif}

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: UINT): Integer;
begin
(*  if IsWinNTUp then
    result := Windows.DrawTextW(hDC, lpString, nCount, lpRect, uFormat)
  else
*)   result := DrawTextExW(hDC, lpString, nCount, lpRect, uFormat, nil);
end;

{$endif}

function GetSysColorPen(Color : COLORREF) : HPEN;
begin
  result := CreatePen(PS_SOLID, 1, Color);
end;

{$endif}

procedure DrawFocus(Canvas : TCanvas; R : TRect);

  procedure DrawLine(Canvas : TCanvas; StartX, StartY, EndX, EndY : integer);
  var
    Coord: Integer;
    // CRef: COLORREF;
    // DC: HDC;
  begin
    // CRef := ColorToRGB(Canvas.Pen.Color);
    // skip a pixel if not in grid
    Coord := (StartX and 1) xor (StartY and 1);
    if StartX = EndX then
    begin
      // draw vertical line
      Inc(Coord, StartY);
      // DC := Canvas.Handle;
      while Coord < EndY do
      begin
        Canvas.MoveTo(StartX, Coord);
        Inc(Coord, 1);
        Canvas.LineTo(StartX, Coord);
        Inc(Coord, 1);
      end;
    end
    else
    begin
      // draw horizontal line
      Inc(Coord, StartX);
      //DC := Canvas.Handle;
      while Coord < EndX do
      begin
        Canvas.MoveTo(Coord, StartY);
        Inc(Coord, 1);
        Canvas.LineTo(Coord, StartY);
        Inc(Coord, 1);
      end;
    end;
  end;

begin
  DrawLine(Canvas, R.Left, R.Top, R.Left, R.Bottom - 1);
  DrawLine(Canvas, R.Left, R.Top, R.Right - 1, R.Top);
  DrawLine(Canvas, R.Right - 1, R.Top, R.Right - 1, R.Bottom);
  DrawLine(Canvas, R.Left, R.Bottom - 1, R.Right - 1, R.Bottom - 1);
end;

{$ifndef CLX_USED}
function Win2KHideUIState : boolean;
var b : boolean;
begin
  SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @b, 0);
  result := not b;
  // R := TRegistry.Create;
// R.OpenKey('Software\Microsoft\Windows\CurrentVersion\Policies\Explorer', );
end;
{$endif}

function ModalFormVisible : boolean;
var i : integer;
{$ifndef VCL_4_USED}
    PFS: ^TFormState;
{$endif}
begin
  result := false;
  for i := 0 to Screen.FormCount - 1 do
  begin
    {$ifdef VCL_4_USED}
    if fsModal in TCustomForm(Screen.Forms[i]).FormState then
    {$else}
    PFS := @TCustomForm(Screen.Forms[i]).DropTarget;
    dec(PFS);
    if fsModal in PFS^ then
    {$endif}
    begin
      result := true;
      exit;
    end;
  end;
end;

{$ifndef CLX_USED}
function ShiftStateToKeyData(Shift : TShiftState) : integer;
begin
  if ssAlt in Shift then
    result := $20000000
  else
    result := 0;
end;
{$endif}

{$ifdef ELPACK_UNICODE}
function GetShortHintW(Hint : WideString) : WideString;
var
  I: Integer;
begin
  I := WidePos('|', Hint);
  if I = 0 then
    Result := Hint
  else
    Result := WideCopy(Hint, 1, I - 1);
end;
{$endif}

initialization

  //ParentControlRepaintedMessage := RegisterWindowMessage('Transparent controls notification -- parent repainted');

end.
