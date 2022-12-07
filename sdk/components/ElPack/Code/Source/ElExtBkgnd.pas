
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

unit ElExtBkgnd;

interface

uses
  {$ifndef CLX_USED}
  Windows,
  Graphics,
{$ifdef VCL_6_USED}
Types,
{$endif}
  {$else}
  Qt,
  Types,
  QTypes,
  QGraphics,
  {$endif}
  Classes,
  SysUtils,
  ElTools,
  ElVCLUtils;

{$ifndef CLX_USED}

procedure ExtDrawBkgnd2(DC : HDC; Wnd : HWND; R, DocRect : TRect; Origin : TPoint; FillColor : TColor;
          SourceBitmap : Graphics.TBitmap; DrawMode : TElBkGndType);

procedure ExtDrawBkgnd(DC : HDC; Wnd : HWND; RectDoc, RectWindow, RectDC, RectOnDC : TRect;
  InvertedMode : boolean; FillColor, OverColor : TColor; DoBlend : boolean;
  SourceBitmap : Graphics.TBitmap; DrawMode : TElBkGndType);

{$else}

procedure ExtDrawBkgnd2(DC : QPainterH; Wnd : QWidgetH; R, DocRect : TRect; Origin : TPoint; FillColor : TColor;
          SourceBitmap : TBitmap; DrawMode : TElBkGndType);

procedure ExtDrawBkgnd(DC : QPainterH; Wnd : QWidgetH; RectDoc, RectWindow, RectDC, RectOnDC : TRect;
  InvertedMode : boolean; FillColor, OverColor : TColor; DoBlend : boolean;
  SourceBitmap : TBitmap; DrawMode : TElBkGndType);

{$endif}

// RectWindow - the Window Rectangle. Just to calculate window's width and height
// RectDC - the DC full rectangle relation to RectWindow
// RectOnDC - the rectangle relative to DC coordinates to which the bitmap is drawn
// DrawMode = (bgtTileBitmap, bgtStretchBitmap, bgtColorFill, bgtCenterBitmap);

implementation

var
  BlendBitmap : TBitmap;


{$ifndef CLX_USED}
procedure ExtDrawBkgnd2(DC : HDC; Wnd : HWND; R, DocRect : TRect; Origin : TPoint; FillColor : TColor;
          SourceBitmap : Graphics.TBitmap; DrawMode : TElBkGndType);
{$else}
procedure ExtDrawBkgnd2(DC : QPainterH; Wnd : QWidgetH; R, DocRect : TRect; Origin : TPoint; FillColor : TColor;
          SourceBitmap : TBitmap; DrawMode : TElBkGndType);
{$endif}
var CurRect,
    DstRect,
    SrcRect,
    RealRect : TRect;
    i, j, k,
    l, n,
    sw, sh   : integer;
    HelpBmp  : TBitmap;
begin
  if DrawMode = bgtColorFill then
  begin
    FillSolidRect2(DC, R, ColorToRGB(FillColor));
    exit;
  end;
  RealRect := R;
  OffsetRect(RealRect, Origin.X, Origin.Y);
  if (RealRect.Bottom > DocRect.Bottom) or
     (RealRect.Top < DocRect.Top) or
     (RealRect.Right > DocRect.Right) or
     (RealRect.Left < DocRect.Left) then
  begin
    FillSolidRect2(DC, R, ColorToRGB(FillColor));
  end;
  case DrawMode of
    bgtTileBitmap:
      begin
        sw := SourceBitmap.Width;
        sh := SourceBitmap.Height;
        i := 0;
        while i + sh < RealRect.Top do
          inc(i, sh);
        j := 0;
        while j + sw < RealRect.Left do
          inc(j, sw);
        k := j;
        while i < RealRect.Bottom do
        begin
          while j < RealRect.Right do
          begin
            // get visible area of the tiled bitmap
            SrcRect := Rect(j, i, j + sw, i + sh);
            IntersectRect(DstRect, SrcRect, RealRect);
            // get the portion of the bitmap to be copied
            l := DstRect.Left mod sw;
            n := DstRect.Top mod sh;
            //SrcRect := Rect(l, n, l + DstRect.Right - DstRect.Left, n + DstRect.Bottom - DstRect.Top);
            {$ifndef CLX_USED}
            bitblt(DC, DstRect.Left - Origin.X,
                       DstRect.Top - Origin.Y,
                       DstRect.Right - DstRect.Left,
                       DstRect.Bottom - DstRect.Top,
                       SourceBitmap.Canvas.Handle,
                       l, n, SRCCOPY);
            {$else}
            bitblt(QPainter_device(DC), DstRect.Left - Origin.X,
                   DstRect.Top - Origin.Y, SourceBitmap.Handle,
                   l, n, DstRect.Right - DstRect.Left,
                         DstRect.Bottom - DstRect.Top,
                         RasterOp_CopyROP, true);
            {$endif}
            inc(j, sw);
          end;
          j := k;
          inc(i, sh);
        end;
      end;
    bgtTopLeftBitmap,
    bgtCenterBitmap:
      begin
        DstRect := RealRect;
        FillSolidRect2(DC, DstRect, ColorToRGB(FillColor));
        // Center the bitmap
        if DrawMode = bgtCenterBitmap then
        begin
          CenterRects(SourceBitmap.Width, DocRect.Right - DocRect.Left, SourceBitmap.Height, DocRect.Bottom - DocRect.Top, CurRect);
        end
        else
          CurRect := Rect(0, 0, SourceBitmap.Width, SourceBitmap.Height);

        OffsetRect(CurRect, DocRect.Left, DocRect.Top);
        // now get the target rectangle
        IntersectRect(DstRect, CurRect, RealRect);
        // now get the source rectangle
        SrcRect := DstRect;
        OffsetRect(SrcRect, - CurRect.Left, -CurRect.Top);
        l := SrcRect.Left;
        n := SrcRect.Top;
        {$ifndef CLX_USED}
        bitblt(DC, DstRect.Left - Origin.X,
                   DstRect.Top - Origin.Y,
                   DstRect.Right - DstRect.Left,
                   DstRect.Bottom - DstRect.Top,
                   SourceBitmap.Canvas.Handle,
                   l, n, SRCCOPY);
        {$else}
        bitblt(QPainter_device(DC), DstRect.Left - Origin.X,
                                    DstRect.Top - Origin.Y,
                                    SourceBitmap.Handle, l, n,
                                    DstRect.Right - DstRect.Left,
                                    DstRect.Bottom - DstRect.Top,
                                    RasterOp_CopyROP, true);
        {$endif}
      end;
    bgtStretchBitmap:
      begin
        HelpBmp := TBitmap.Create;
        try
          HelpBmp.Width := DocRect.Right - DocRect.Left;
          HelpBmp.Height := DocRect.Bottom - DocRect.Top;

          {$ifndef CLX_USED}
          stretchblt(HelpBmp.Canvas.Handle, 0, 0, HelpBmp.Width, HelpBmp.Height,
                     SourceBitmap.Canvas.Handle, 0, 0, SourceBitmap.Width, SourceBitmap.Height, SRCCOPY);
          {$else}
          HelpBmp.Canvas.StretchDraw(Rect(0, 0, HelpBmp.Width, HelpBmp.Height), SourceBitmap);
          {$endif}

          DstRect := RealRect;
          CurRect := Rect(0, 0, HelpBmp.Width, HelpBmp.Height);

          OffsetRect(CurRect, DocRect.Left, DocRect.Top);
          // now get the target rectangle
          IntersectRect(DstRect, CurRect, RealRect);
          // now get the source rectangle
          SrcRect := DstRect;
          OffsetRect(SrcRect, - CurRect.Left, -CurRect.Top);
          l := SrcRect.Left;
          n := SrcRect.Top;
          {$ifndef CLX_USED}
          bitblt(DC, DstRect.Left - Origin.X,
                     DstRect.Top - Origin.Y,
                     DstRect.Right - DstRect.Left,
                     DstRect.Bottom - DstRect.Top,
                     HelpBmp.Canvas.Handle,
                     l, n, SRCCOPY);
          {$else}
          bitblt(QPainter_device(DC), DstRect.Left - Origin.X,
                                      DstRect.Top - Origin.Y,
                                      HelpBmp.Handle, l, n,
                                      DstRect.Right - DstRect.Left,
                                      DstRect.Bottom - DstRect.Top,
                                      RasterOp_CopyROP, true);
          {$endif}
        finally
          HelpBmp.Free;
        end;
      end;
  end;
end;

{$ifndef CLX_USED}
procedure ExtDrawBkgnd(DC : HDC; Wnd : HWND; RectDoc, RectWindow, RectDC, RectOnDC : TRect;
  InvertedMode : boolean; FillColor, OverColor : TColor; DoBlend : boolean;
  SourceBitmap : Graphics.TBitmap; DrawMode : TElBkGndType);
{$else}
procedure ExtDrawBkgnd(DC : QPainterH; Wnd : QWidgetH; RectDoc, RectWindow, RectDC, RectOnDC : TRect;
  InvertedMode : boolean; FillColor, OverColor : TColor; DoBlend : boolean;
  SourceBitmap : TBitmap; DrawMode : TElBkGndType);
{$endif}
var
  DestRect : TRect;
  SrcRect : TRect;
  TmpRect : TRect;
  {$ifndef CLX_USED}
  BltMode : Integer;
  {$else}
  BltMode : RasterOp;
  {$endif}
  {$ifndef CLX_USED}
  Bmp,
  {$endif}
    HelpBmp : TBitmap;
  {$ifndef CLX_USED}
  OC : TColor;
  {$endif}
  {$ifndef CLX_USED}
  b : boolean;
  i,
  j,
  {$endif}
    sw, sh : integer;
  CurRect : TRect;
  CurLeft,
    CurTop : integer;
  //TempDC : HDC;

begin
  TmpRect := RectOnDC;
  if (DrawMode = bgtColorFill) or (SourceBitmap.Empty and (DrawMode in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap, bgtTopLeftBitmap])) then
  begin
    FillSolidRect2(DC, TmpRect, ColorToRGB(FillColor));
    exit;
  end;
  case DrawMode of
    bgtTileBitmap :
      begin
        OffsetRect(TmpRect, -RectDC.Left, -RectDC.Top);
        CurLeft := RectWindow.Left + RectDoc.Left;
        CurTop := RectWindow.Top + RectDoc.Top;
        sw := SourceBitmap.Width;
        sh := SourceBitmap.Height;
        CurRect := Rect(CurLeft - RectDoc.Left, CurTop - RectDoc.Top, CurLeft + sw - RectDoc.Left, CurTop + sh - RectDoc.Top);
        while CurRect.Bottom <= TmpRect.Top do
        begin
          inc(CurTop, sh);
          CurRect := Rect(CurLeft - RectDoc.Left, CurTop - RectDoc.Top, CurLeft + sw - RectDoc.Left, CurTop + sh - RectDoc.Top);
        end;
        while CurTop < TmpRect.Bottom do
        begin
          while CurLeft < TmpRect.Right do
          begin
            SrcRect := Rect(CurLeft, CurTop, CurLeft + sw, CurTop + sh);
            IntersectRect(DestRect, TmpRect, SrcRect);
            if not IsRectEmpty(DestRect) then
            begin
              SrcRect := DestRect;
              OffsetRect(SrcRect, -CurLeft, -CurTop);

              OffsetRect(DestRect, -RectDC.Left, -RectDC.Top);

              {$ifndef CLX_USED}
              if InvertedMode then
                BltMode := SRCINVERT
              else
                BltMode := SRCCOPY;
              {$else}
              if InvertedMode then
                BltMode := RasterOp_NotCopyROP
              else
                BltMode := RasterOp_CopyROP;
              {$endif}
              {$ifndef CLX_USED}
              if DoBlend then
              begin
                Bmp := TBitmap.Create;
                {$ifndef CLX_USED}
                Bmp.Handle := CreateCompatibleBitmap(DC, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top);
                bitblt(Bmp.Canvas.Handle,
                  0, 0,
                  DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
                  SourceBitmap.Canvas.Handle,
                  SrcRect.Left, SrcRect.Top, SRCCOPY);
                {$else}
                Bmp.Width  := DestRect.Right - DestRect.Left;
                Bmp.Height := DestRect.Bottom - DestRect.Top;
                bitblt(QPainter_device(DC), 0, 0,
                       SourceBitmap.Handle, SrcRect.Left, SrcRect.Top,
                       DestRect.Right - DestRect.Left,
                       DestRect.Bottom - DestRect.Top, RasterOp_CopyROP, true);
                {$endif}

                OC := ColorToRgb(OverColor);
                for i := 0 to Bmp.Height - 1 do // Iterate
                begin
                  b := (DestRect.Left mod 2 = 1) xor ((DestRect.Top + i) mod 2 = 1);
                  for j := 0 to Bmp.Width - 1 do // Iterate
                  begin
                    if b then
                      Bmp.Canvas.Pixels[j, i] := OC;
                    b := not b;
                  end; // for
                end; // for
                {$ifndef CLX_USED}
                bitblt(DC,
                  DestRect.Left, DestRect.Top,
                  DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
                  Bmp.Canvas.Handle,
                  0, 0, BltMode);
                {$else}
                bitblt(QPainter_device(DC), 0, 0,
                       SourceBitmap.Handle, SrcRect.Left, SrcRect.Top,
                       DstRect.Right - DstRect.Left,
                       DstRect.Bottom - DstRect.Top, BltMode, true);
                {$endif}

                Bmp.Free;
              end
              else
              {$endif}
              begin
                {$ifndef CLX_USED}
                bitblt(DC,
                  DestRect.Left, DestRect.Top,
                  DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
                  SourceBitmap.Canvas.Handle,
                  SrcRect.Left, SrcRect.Top, BltMode);
                {$else}
                bitblt(QPainter_device(DC), 0, 0,
                       SourceBitmap.Handle, SrcRect.Left, SrcRect.Top,
                       DestRect.Right - DestRect.Left,
                       DestRect.Bottom - DestRect.Top, BltMode, true);
                {$endif}
              end;
            end;
            CurLeft := CurLeft + SourceBitmap.Width;
          end;
          CurLeft := RectWindow.Left + RectDoc.Left;
          CurTop := CurTop + SourceBitmap.Height;
        end;
      end;
    bgtTopLeftBitmap,
    bgtCenterBitmap :
      begin
        FillSolidRect2(DC, TmpRect, ColorToRGB(FillColor));
        OffsetRect(TmpRect, -RectDC.Left, -RectDC.Top);

        if DrawMode = bgtCenterBitmap then
        begin
          CenterRects(SourceBitmap.Width, RectWindow.Right - RectWindow.Left,
            SourceBitmap.Height, RectWindow.Bottom - RectWindow.Top, SrcRect);
        end
        else
          SrcRect := Rect(0, 0, SourceBitmap.Width, SourceBitmap.Height);

        OffsetRect(SrcRect, RectWindow.Left, RectWindow.Top);

        IntersectRect(TmpRect, TmpRect, SrcRect);
        if not IsRectEmpty(TmpRect) then
        begin
          DestRect := TmpRect;
          OffsetRect(DestRect, -SrcRect.Left, -SrcRect.Top);
          SrcRect := DestRect;

          DestRect := TmpRect;
          OffsetRect(DestRect, -RectDC.Left, -RectDC.Top);


          {$ifndef CLX_USED}
          if InvertedMode then
            BltMode := SRCINVERT
          else
            BltMode := SRCCOPY;
          {$else}
          if InvertedMode then
            BltMode := RasterOp_NotCopyROP
          else
            BltMode := RasterOp_CopyROP;
          {$endif}
          {$ifndef CLX_USED}
          if DoBlend then
          begin
            Bmp := Graphics.TBitmap.Create;
            Bmp.Handle := CreateCompatibleBitmap(DC, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top);
            bitblt(Bmp.Canvas.Handle,
              0, 0,
              DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
              SourceBitmap.Canvas.Handle,
              SrcRect.Left, SrcRect.Top, SRCCOPY);
            OC := ColorToRgb(OverColor);

            for i := 0 to Bmp.Height - 1 do // Iterate
            begin
              b := (DestRect.Left mod 2 = 1) and ((DestRect.Top + i) mod 2 = 1);
              for j := 0 to Bmp.Width - 1 do // Iterate
              begin
                if b then Bmp.Canvas.Pixels[j, i] := OC;
                b := ((DestRect.Top + i) mod 2 = 1) and ((DestRect.Left + j) mod 2 = 1);
              end; // for
            end; // for
            bitblt(DC,
              DestRect.Left, DestRect.Top,
              DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
              Bmp.Canvas.Handle,
              0, 0, BltMode);
            Bmp.Free;
          end
          else
          {$endif}
          begin
            {$ifndef CLX_USED}
            bitblt(DC,
              DestRect.Left, DestRect.Top,
              DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
              SourceBitmap.Canvas.Handle,
              SrcRect.Left, SrcRect.Top, BltMode);
            {$else}
            bitblt(QPainter_device(DC), DestRect.Left, DestRect.Top,
                   SourceBitmap.Handle, SrcRect.Left, SrcRect.Top,
                   DestRect.Right - DestRect.Left,
                   DestRect.Bottom - DestRect.Top, BltMode, true);
            {$endif}
          end;
        end;
      end;
    bgtStretchBitmap :
      begin
        OffsetRect(TmpRect, -RectDC.Left, -RectDC.Top);
        HelpBmp := TBitmap.Create;
        HelpBmp.Width := RectWindow.Right - RectWindow.Left;
        HelpBmp.Height := RectWindow.Bottom - rectWindow.Top;
        {$ifndef CLX_USED}
        stretchblt(HelpBmp.Canvas.Handle, 0, 0, HelpBmp.Width, HelpBmp.Height,
          SourceBitmap.Canvas.Handle, 0, 0, SourceBitmap.Width, SourceBitmap.Height, SRCCOPY);
        {$else}
        HelpBmp.Canvas.StretchDraw(Rect(0, 0, HelpBmp.Width, HelpBmp.Height), SourceBitmap);
        {$endif}
        SrcRect := RectWindow;
        IntersectRect(TmpRect, TmpRect, SrcRect);
        if not IsRectEmpty(TmpRect) then
        begin
          SrcRect := TmpRect;
          OffsetRect(SrcRect, -RectWindow.Left, -RectWindow.Top);
          DestRect := TmpRect;
          OffsetRect(DestRect, -RectDC.Left, -RectDC.Top);
          {$ifndef CLX_USED}
          if InvertedMode then
            BltMode := SRCINVERT
          else
            BltMode := SRCCOPY;
          {$else}
          if InvertedMode then
            BltMode := RasterOp_NotCopyROP
          else
            BltMode := RasterOp_CopyROP;
          {$endif}
          {$ifndef CLX_USED}
          if DoBlend then
          begin
            OC := ColorToRgb(OverColor);
            BlendBitmap.Canvas.Pixels[0, 0] := OverColor;
            Bmp := Graphics.TBitmap.Create;
            Bmp.Handle := CreateCompatibleBitmap(DC, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top);
            bitblt(Bmp.Canvas.Handle,
              0, 0,
              DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
              HelpBmp.Canvas.Handle,
              SrcRect.Left, SrcRect.Top, SRCCOPY);
            for i := 0 to Bmp.Height - 1 do // Iterate
            begin
              b := ((DestRect.Top + i) mod 2 = 1) and (DestRect.Left mod 2 = 1);
              for j := 0 to Bmp.Width - 1 do // Iterate
              begin
                if b then SetPixel(BMP.Canvas.Handle, j, i, OC); // Bmp.Canvas.Pixels[j, i] := OC;
                b := ((DestRect.Top + i) mod 2 = 1) and ((DestRect.Left + j) mod 2 = 1);
              end; // for
            end; // for*)

            bitblt(DC,
              DestRect.Left, DestRect.Top,
              DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
              Bmp.Canvas.Handle,
              0, 0, BltMode);
            Bmp.Free;
          end
          else
          {$endif}
          begin
            {$ifndef CLX_USED}
            bitblt(DC,
              DestRect.Left, DestRect.Top,
              DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
              HelpBmp.Canvas.Handle,
              SrcRect.Left, SrcRect.Top, BltMode);
            {$else}
            bitblt(QPainter_device(DC), DestRect.Left, DestRect.Top,
                   HelpBmp.Handle, SrcRect.Left, SrcRect.Top,
                   DestRect.Right - DestRect.Left,
                   DestRect.Bottom - DestRect.Top,
                   BltMode, true);
            {$endif}

          end;
          HelpBmp.Free;
        end;

      end;
  end;
end;

initialization
  BlendBitmap := TBitmap.Create;
  BlendBitmap.Width := 2;
  BlendBitmap.Height := 2;
finalization
  BlendBitmap.Free;

end.
