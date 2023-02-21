// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElVCLUtils.pas' rev: 35.00 (Windows)

#ifndef ElvclutilsHPP
#define ElvclutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <System.Win.Registry.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElTools.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elvclutils
{
//-- forward type declarations -----------------------------------------------
struct TBlendFunction;
//-- type declarations -------------------------------------------------------
typedef int TImageIndex;

struct DECLSPEC_DRECORD TBlendFunction
{
public:
	System::Byte BlendOp;
	System::Byte BlendFlags;
	System::Byte SourceConstantAlpha;
	System::Byte AlphaFormat;
};


typedef System::LongBool __stdcall (*TAlphaBlend)(HDC hdcDest, int nXOriginDest, int nYOriginDest, int nWidthDest, int nHeightDest, HDC hdcSrc, int nXOriginSrc, int nYOriginSrc, int nWidthSrc, int nHeightSrc, int blendFunction);

enum DECLSPEC_DENUM TElBkGndType : unsigned char { bgtTileBitmap, bgtStretchBitmap, bgtColorFill, bgtCenterBitmap, bgtHorzGradient, bgtVertGradient, bgtTopLeftBitmap };

enum DECLSPEC_DENUM TElBorderSide : unsigned char { ebsLeft, ebsRight, ebsTop, ebsBottom };

typedef System::Set<TElBorderSide, TElBorderSide::ebsLeft, TElBorderSide::ebsBottom> TElBorderSides;

enum DECLSPEC_DENUM TElFlatBorderType : unsigned char { fbtFlat, fbtSunken, fbtSunkenInner, fbtSunkenOuter, fbtRaised, fbtRaisedInner, fbtRaisedOuter, fbtBump, fbtBigBump, fbtEtched, fbtFramed, fbtLine, fbtLineBorder, fbtNone, fbtColorLineBorder };

enum DECLSPEC_DENUM TElTextDrawType : unsigned char { tdtNormal, tdtShadowed, tdtRaised };

enum DECLSPEC_DENUM TElArrowDir : unsigned char { eadLeft, eadUp, eadRight, eadDown };

enum DECLSPEC_DENUM TTaskbarEdge : unsigned char { tbeBottom, tbeLeft, tbeTop, tbeRight };

enum DECLSPEC_DENUM TElTextCase : unsigned char { etcNoChange, etcUppercase, etcLowercase };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE unsigned ParentControlRepaintedMessage;
extern DELPHI_PACKAGE TElBorderSides AllBorderSides;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 2> smXEdge;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 2> smYEdge;
extern DELPHI_PACKAGE System::Uitypes::TCursor WaitCursor;
extern DELPHI_PACKAGE System::WideChar __fastcall GetTimeAMChar(void);
extern DELPHI_PACKAGE System::WideChar __fastcall GetTimePMChar(void);
extern DELPHI_PACKAGE int __fastcall IncColor(const System::Uitypes::TColor Color, int RInc, int GInc, int BInc);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall InvertColor(System::Uitypes::TColor aColor);
extern DELPHI_PACKAGE System::Types::TRect __fastcall GetWorkSpaceRect(void);
extern DELPHI_PACKAGE int __fastcall GetWorkSpaceTop(void);
extern DELPHI_PACKAGE int __fastcall GetWorkSpaceLeft(void);
extern DELPHI_PACKAGE int __fastcall GetWorkSpaceBottom(void);
extern DELPHI_PACKAGE int __fastcall GetWorkSpaceRight(void);
extern DELPHI_PACKAGE System::Types::TRect __fastcall GetDesktopRect(void);
extern DELPHI_PACKAGE int __fastcall GetDesktopTop(void);
extern DELPHI_PACKAGE int __fastcall GetDesktopLeft(void);
extern DELPHI_PACKAGE int __fastcall GetDesktopBottom(void);
extern DELPHI_PACKAGE int __fastcall GetDesktopRight(void);
extern DELPHI_PACKAGE void __fastcall MinimizeToTray(HWND Wnd);
extern DELPHI_PACKAGE System::Types::TRect __fastcall GetSysTrayRect(void);
extern DELPHI_PACKAGE System::Types::TRect __fastcall GetTaskbarRect(void);
extern DELPHI_PACKAGE TTaskbarEdge __fastcall GetTaskbarEdge(void);
extern DELPHI_PACKAGE int __fastcall GetKeybTimes(int TimeKind);
extern DELPHI_PACKAGE void __fastcall GradientFillEx(HDC DC, const System::Types::TRect &DCRect, const System::Types::TRect &R, const System::Types::TPoint &Origin, System::Uitypes::TColor StartColor, System::Uitypes::TColor EndColor, int Steps, bool Vertical);
extern DELPHI_PACKAGE void __fastcall GradientFill(HDC DC, const System::Types::TRect &R, System::Uitypes::TColor StartColor, System::Uitypes::TColor EndColor, int Steps, bool Vertical);
extern DELPHI_PACKAGE void __fastcall DrawArrow(Vcl::Graphics::TCanvas* Canvas, TElArrowDir Dir, const System::Types::TRect &R, System::Uitypes::TColor Color, bool Enabled);
extern DELPHI_PACKAGE bool __fastcall RectsIntersect(const System::Types::TRect &R1, const System::Types::TRect &R2);
extern DELPHI_PACKAGE Vcl::Controls::TWinControl* __fastcall FindVCLChild(Vcl::Controls::TWinControl* Control, HWND ChildHandle);
extern DELPHI_PACKAGE void __fastcall DrawTransparentBitmapEx(HDC DC, Vcl::Graphics::TBitmap* Bitmap, int X, int Y, const System::Types::TRect &Src, System::Uitypes::TColor Transparent);
extern DELPHI_PACKAGE void __fastcall DrawTypedTextW(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Bounds, System::WideString Text, int Flags, TElTextDrawType DrawType);
extern DELPHI_PACKAGE void __fastcall DrawTypedText(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Bounds, System::UnicodeString Text, int Flags, TElTextDrawType DrawType);
extern DELPHI_PACKAGE void __fastcall FillSolidRect2(HDC DC, const System::Types::TRect &Rect, System::Uitypes::TColor Color);
extern DELPHI_PACKAGE void __fastcall FillSolidRect(HDC DC, int x, int y, int cx, int cy, System::Uitypes::TColor Color);
extern DELPHI_PACKAGE void __fastcall Draw3dRectEx(HDC DC, int x, int y, int cx, int cy, unsigned clrTopLeft, unsigned clrBottomRight, TElBorderSides BorderSides);
extern DELPHI_PACKAGE void __fastcall Draw3dBorder(HDC DC, const System::Types::TRect &rc, System::Uitypes::TColor nColor1, System::Uitypes::TColor nColor2, System::Uitypes::TColor nColor3, System::Uitypes::TColor nColor4);
extern DELPHI_PACKAGE int __fastcall RGBtoHLS(int rgbc);
extern DELPHI_PACKAGE int __fastcall HLStoRGB(int hlsc);
extern DELPHI_PACKAGE void __fastcall DrawButtonFrameEx3(HDC DC, const System::Types::TRect &rc, bool Focused, bool Pushed, System::Uitypes::TColor ButtonColor, bool Thin, TElBorderSides BorderSides);
extern DELPHI_PACKAGE void __fastcall DrawButtonFrameEx(HDC DC, const System::Types::TRect &rc, bool Focused, bool Pushed, System::Uitypes::TColor ButtonColor, bool Thin);
extern DELPHI_PACKAGE void __fastcall DrawButtonFrame(HDC DC, const System::Types::TRect &rc, bool Focused, bool Pushed);
extern DELPHI_PACKAGE void __fastcall DrawButtonFrameEx2(HDC DC, const System::Types::TRect &rc, bool Focused, bool Pushed, System::Uitypes::TColor ButtonColor, bool Thin, System::Uitypes::TColor clrHighlight, System::Uitypes::TColor clrDkShadow, System::Uitypes::TColor clrFace, System::Uitypes::TColor clrShadow);
extern DELPHI_PACKAGE void __fastcall DrawFlatFrameEx2(HDC DC, const System::Types::TRect &R, System::Uitypes::TColor Color, System::Uitypes::TColor BkColor, bool Focused, bool Enabled, TElBorderSides BorderSides, TElFlatBorderType BorderType);
extern DELPHI_PACKAGE void __fastcall DrawFlatFrameEx(HDC DC, const System::Types::TRect &R, System::Uitypes::TColor BkColor, bool Focused, bool Enabled);
extern DELPHI_PACKAGE System::Types::TRect __fastcall DrawFlatFrame(HDC DC, const System::Types::TRect &R, System::Uitypes::TColor BkColor, bool Focused);
extern DELPHI_PACKAGE System::Types::TRect __fastcall DrawFlatFrame2(HDC DC, const System::Types::TRect &R, System::Uitypes::TColor BkColor, bool Focused, TElBorderSides BorderSides);
extern DELPHI_PACKAGE void __fastcall TiledPaint(Vcl::Graphics::TCanvas* Canvas, Vcl::Graphics::TBitmap* Bitmap, const System::Types::TRect &Rect);
extern DELPHI_PACKAGE int __fastcall HitTest(const System::Types::TRect &R, const System::Types::TPoint &Pt, int CornerSize, int BorderSize);
extern DELPHI_PACKAGE Vcl::Controls::TControl* __fastcall GetTopOwnerControl(System::Classes::TComponent* Component);
extern DELPHI_PACKAGE Vcl::Forms::TForm* __fastcall GetOwnerForm(System::Classes::TComponent* Component);
extern DELPHI_PACKAGE void __fastcall StartWait(void);
extern DELPHI_PACKAGE void __fastcall StopWait(void);
extern DELPHI_PACKAGE void __fastcall DrawFlatScrollbarThumb(HDC DC, const System::Types::TRect &rc, bool Focused);
extern DELPHI_PACKAGE bool __fastcall AlphaBlend(HDC hdcDest, int nXOriginDest, int nYOriginDest, int nWidthDest, int nHeightDest, HDC hdcSrc, int nXOriginSrc, int nYOriginSrc, int nWidthSrc, int nHeightSrc, System::Byte SourceConstantAlpha, System::Byte srcAlpha);
extern DELPHI_PACKAGE System::Types::TRect __fastcall DrawFlatScrollbars(HWND Wnd, HDC DC, const System::Types::TRect &Rect, bool Focused, System::Uitypes::TScrollStyle ScrollBars, bool DragHorz, bool DragVert, bool IsControl, int Style, int ExStyle);
extern DELPHI_PACKAGE void __fastcall DrawFlatScrollBarEx(HWND Wnd, HDC DC, const System::Types::TRect &Rect, int nType, bool bScrollbarCtrl, bool Dragging, bool Focused, System::Uitypes::TColor BkColor, System::Uitypes::TColor DitherColor, System::Uitypes::TColor ButtonColor, System::Uitypes::TColor ArrowColor, System::Uitypes::TColor HotButtonColor, bool DrawFrames, bool DitherBack);
extern DELPHI_PACKAGE void __fastcall DrawFlatScrollBarsEx(HWND Wnd, HDC DC, const System::Types::TRect &Rect, bool Focused, System::Uitypes::TScrollStyle ScrollBars, bool DragHorz, bool DragVert, bool IsControl, System::Uitypes::TColor BkColor, System::Uitypes::TColor DitherColor, System::Uitypes::TColor ButtonColor, System::Uitypes::TColor ArrowColor, System::Uitypes::TColor HotButtonColor, bool DrawFrames, bool DitherBack);
extern DELPHI_PACKAGE System::Types::TRect __fastcall DrawBevel(HDC DC, const System::Types::TRect &R, System::Uitypes::TColor Color1, System::Uitypes::TColor Color2, TElBorderSides Sides);
extern DELPHI_PACKAGE void __fastcall AlphaCopyRect(Vcl::Graphics::TCanvas* DestCanvas, const System::Types::TRect &Dest, Vcl::Graphics::TCanvas* SourceCanvas, const System::Types::TRect &Source, System::Byte AlphaLevel, bool UseAlphaLevel);
extern DELPHI_PACKAGE void __fastcall AlphaFillRect(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect, System::Uitypes::TColor Color, System::Byte AlphaLevel);
extern DELPHI_PACKAGE int __fastcall DrawTextW(HDC hDC, System::WideChar * lpString, int nCount, System::Types::TRect &lpRect, unsigned uFormat);
extern DELPHI_PACKAGE HPEN __fastcall GetSysColorPen(unsigned Color);
extern DELPHI_PACKAGE void __fastcall DrawFocus(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
extern DELPHI_PACKAGE bool __fastcall Win2KHideUIState(void);
extern DELPHI_PACKAGE bool __fastcall ModalFormVisible(void);
extern DELPHI_PACKAGE int __fastcall ShiftStateToKeyData(System::Classes::TShiftState Shift);
extern DELPHI_PACKAGE System::WideString __fastcall GetShortHintW(System::WideString Hint);
}	/* namespace Elvclutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELVCLUTILS)
using namespace Elvclutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElvclutilsHPP
