// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxVCLUtils.pas' rev: 35.00 (Windows)

#ifndef MxvclutilsHPP
#define MxvclutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Dialogs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxvclutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TScreenCanvas;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TFillDirection : unsigned char { fdTopToBottom, fdBottomToTop, fdLeftToRight, fdRightToLeft };

enum DECLSPEC_DENUM TVertAlignment : unsigned char { vaTopJustify, vaCenter, vaBottomJustify };

class PASCALIMPLEMENTATION TScreenCanvas : public Vcl::Graphics::TCanvas
{
	typedef Vcl::Graphics::TCanvas inherited;
	
private:
	HDC FDeviceContext;
	
protected:
	virtual void __fastcall CreateHandle();
	
public:
	__fastcall virtual ~TScreenCanvas();
	void __fastcall SetOrigin(int X, int Y);
	void __fastcall FreeHandle();
public:
	/* TCanvas.Create */ inline __fastcall TScreenCanvas() : Vcl::Graphics::TCanvas() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Uitypes::TColor clCream = System::Uitypes::TColor(10930928);
static const System::Uitypes::TColor clMoneyGreen = System::Uitypes::TColor(12639424);
static const System::Uitypes::TColor clSkyBlue = System::Uitypes::TColor(16776176);
extern DELPHI_PACKAGE System::Uitypes::TCursor WaitCursor;
extern DELPHI_PACKAGE void __fastcall ResourceNotFound(System::WideChar * ResID);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall MakeModuleBitmap(NativeUInt Module, System::WideChar * ResID);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall MakeBitmap(System::WideChar * ResID);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall MakeBitmapID(System::Word ResID);
extern DELPHI_PACKAGE void __fastcall AssignBitmapCell(Vcl::Graphics::TGraphic* Source, Vcl::Graphics::TBitmap* Dest, int Cols, int Rows, int Index);
extern DELPHI_PACKAGE void __fastcall CopyParentImage(Vcl::Controls::TControl* Control, Vcl::Graphics::TCanvas* Dest);
extern DELPHI_PACKAGE void __fastcall StretchBltTransparent(HDC DstDC, int DstX, int DstY, int DstW, int DstH, HDC SrcDC, int SrcX, int SrcY, int SrcW, int SrcH, HPALETTE Palette, unsigned TransparentColor);
extern DELPHI_PACKAGE void __fastcall DrawTransparentBitmap(HDC DC, HBITMAP Bitmap, int DstX, int DstY, unsigned TransparentColor);
extern DELPHI_PACKAGE void __fastcall StretchBitmapRectTransparent(Vcl::Graphics::TCanvas* Dest, int DstX, int DstY, int DstW, int DstH, const System::Types::TRect &SrcRect, Vcl::Graphics::TBitmap* Bitmap, System::Uitypes::TColor TransparentColor);
extern DELPHI_PACKAGE void __fastcall DrawBitmapRectTransparent(Vcl::Graphics::TCanvas* Dest, int DstX, int DstY, const System::Types::TRect &SrcRect, Vcl::Graphics::TBitmap* Bitmap, System::Uitypes::TColor TransparentColor);
extern DELPHI_PACKAGE void __fastcall DrawBitmapTransparent(Vcl::Graphics::TCanvas* Dest, int DstX, int DstY, Vcl::Graphics::TBitmap* Bitmap, System::Uitypes::TColor TransparentColor);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall ChangeBitmapColor(Vcl::Graphics::TBitmap* Bitmap, System::Uitypes::TColor Color, System::Uitypes::TColor NewColor);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateDisabledBitmapEx(Vcl::Graphics::TBitmap* FOriginal, System::Uitypes::TColor OutlineColor, System::Uitypes::TColor BackColor, System::Uitypes::TColor HighlightColor, System::Uitypes::TColor ShadowColor, bool DrawHighlight);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateDisabledBitmap(Vcl::Graphics::TBitmap* FOriginal, System::Uitypes::TColor OutlineColor);
extern DELPHI_PACKAGE void __fastcall ImageListDrawDisabled(Vcl::Controls::TImageList* Images, Vcl::Graphics::TCanvas* Canvas, int X, int Y, int Index, System::Uitypes::TColor HighlightColor, System::Uitypes::TColor GrayColor, bool DrawHighlight);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateTwoColorsBrushPattern(System::Uitypes::TColor Color1, System::Uitypes::TColor Color2);
extern DELPHI_PACKAGE Vcl::Graphics::TIcon* __fastcall MakeIcon(System::WideChar * ResID);
extern DELPHI_PACKAGE Vcl::Graphics::TIcon* __fastcall MakeIconID(System::Word ResID);
extern DELPHI_PACKAGE Vcl::Graphics::TIcon* __fastcall MakeModuleIcon(NativeUInt Module, System::WideChar * ResID);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateBitmapFromIcon(Vcl::Graphics::TIcon* Icon, System::Uitypes::TColor BackColor);
extern DELPHI_PACKAGE Vcl::Graphics::TIcon* __fastcall CreateIconFromBitmap(Vcl::Graphics::TBitmap* Bitmap, System::Uitypes::TColor TransparentColor);
extern DELPHI_PACKAGE System::Word __fastcall DialogUnitsToPixelsX(System::Word DlgUnits);
extern DELPHI_PACKAGE System::Word __fastcall DialogUnitsToPixelsY(System::Word DlgUnits);
extern DELPHI_PACKAGE System::Word __fastcall PixelsToDialogUnitsX(System::Word PixUnits);
extern DELPHI_PACKAGE System::Word __fastcall PixelsToDialogUnitsY(System::Word PixUnits);
extern DELPHI_PACKAGE NativeUInt __fastcall LoadDLL(const System::UnicodeString LibName);
extern DELPHI_PACKAGE bool __fastcall RegisterServer(const System::UnicodeString ModuleName);
extern DELPHI_PACKAGE void __fastcall Beep(void);
extern DELPHI_PACKAGE void __fastcall FreeUnusedOle(void);
extern DELPHI_PACKAGE void __fastcall NotImplemented(void);
extern DELPHI_PACKAGE void __fastcall PaintInverseRect(const System::Types::TPoint &RectOrg, const System::Types::TPoint &RectEnd);
extern DELPHI_PACKAGE void __fastcall DrawInvertFrame(const System::Types::TRect &ScreenRect, int Width);
extern DELPHI_PACKAGE int __fastcall WidthOf(const System::Types::TRect &R);
extern DELPHI_PACKAGE int __fastcall HeightOf(const System::Types::TRect &R);
extern DELPHI_PACKAGE bool __fastcall PointInRect(const System::Types::TPoint &P, const System::Types::TRect &R);
extern DELPHI_PACKAGE bool __fastcall PointInPolyRgn(const System::Types::TPoint &P, const System::Types::TPoint *Points, const int Points_High);
extern DELPHI_PACKAGE int __fastcall PaletteColor(System::Uitypes::TColor Color);
extern DELPHI_PACKAGE void __fastcall KillMessage(HWND Wnd, unsigned Msg);
extern DELPHI_PACKAGE HFONT __fastcall CreateRotatedFont(Vcl::Graphics::TFont* Font, int Angle);
extern DELPHI_PACKAGE void __fastcall Delay(int MSecs);
extern DELPHI_PACKAGE int __fastcall PaletteEntries(HPALETTE Palette);
extern DELPHI_PACKAGE void __fastcall CenterControl(Vcl::Controls::TControl* Control);
extern DELPHI_PACKAGE void __fastcall CenterWindow(HWND Wnd);
extern DELPHI_PACKAGE void __fastcall MergeForm(Vcl::Controls::TWinControl* AControl, Vcl::Forms::TForm* AForm, Vcl::Controls::TAlign Align, bool Show);
extern DELPHI_PACKAGE void __fastcall ShowMDIClientEdge(NativeUInt ClientHandle, bool ShowEdge);
extern DELPHI_PACKAGE System::Variant __fastcall MakeVariant(const System::Variant *Values, const int Values_High);
extern DELPHI_PACKAGE void __fastcall ShadeRect(HDC DC, const System::Types::TRect &Rect);
extern DELPHI_PACKAGE System::Types::TRect __fastcall ScreenWorkArea(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WindowClassName(HWND Wnd);
extern DELPHI_PACKAGE void __fastcall ShowWinNoAnimate(HWND Handle, int CmdShow);
extern DELPHI_PACKAGE void __fastcall SwitchToWindow(HWND Wnd, bool Restore);
extern DELPHI_PACKAGE void __fastcall ActivateWindow(HWND Wnd);
extern DELPHI_PACKAGE HWND __fastcall FindPrevInstance(const System::UnicodeString MainFormClass, const System::UnicodeString ATitle);
extern DELPHI_PACKAGE bool __fastcall ActivatePrevInstance(const System::UnicodeString MainFormClass, const System::UnicodeString ATitle);
extern DELPHI_PACKAGE int __fastcall MsgBox(const System::UnicodeString Caption, const System::UnicodeString Text, int Flags);
extern DELPHI_PACKAGE System::Word __fastcall MsgDlg(const System::UnicodeString Msg, System::Uitypes::TMsgDlgType AType, System::Uitypes::TMsgDlgButtons AButtons, int HelpCtx);
extern DELPHI_PACKAGE void __fastcall GradientFillRect(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &ARect, System::Uitypes::TColor StartColor, System::Uitypes::TColor EndColor, TFillDirection Direction, System::Byte Colors);
extern DELPHI_PACKAGE System::UnicodeString __fastcall MinimizeText(const System::UnicodeString Text, Vcl::Graphics::TCanvas* Canvas, int MaxWidth);
extern DELPHI_PACKAGE System::Types::TPoint __fastcall GetAveCharSize(Vcl::Graphics::TCanvas* Canvas);
extern DELPHI_PACKAGE void * __fastcall AllocMemo(int Size);
extern DELPHI_PACKAGE void * __fastcall ReallocMemo(void * fpBlock, int Size);
extern DELPHI_PACKAGE void __fastcall FreeMemo(void * &fpBlock);
extern DELPHI_PACKAGE int __fastcall GetMemoSize(void * fpBlock);
extern DELPHI_PACKAGE bool __fastcall CompareMem(void * fpBlock1, void * fpBlock2, unsigned Size);
extern DELPHI_PACKAGE void __fastcall HugeInc(void * &HugePtr, int Amount);
extern DELPHI_PACKAGE void __fastcall HugeDec(void * &HugePtr, int Amount);
extern DELPHI_PACKAGE void * __fastcall HugeOffset(void * HugePtr, int Amount);
extern DELPHI_PACKAGE void __fastcall HMemCpy(void * DstPtr, void * SrcPtr, int Amount);
extern DELPHI_PACKAGE void __fastcall HugeMove(void * Base, int Dst, int Src, int Size);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetEnvVar(const System::UnicodeString VarName);
extern DELPHI_PACKAGE void __fastcall SplitCommandLine(const System::UnicodeString CmdLine, System::UnicodeString &ExeName, System::UnicodeString &Params);
extern DELPHI_PACKAGE System::UnicodeString __fastcall AnsiUpperFirstChar(const System::UnicodeString S);
extern DELPHI_PACKAGE System::WideChar * __fastcall StrPAlloc(const System::UnicodeString S);
extern DELPHI_PACKAGE System::WideChar * __fastcall StringToPChar(System::UnicodeString &S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DropT(const System::UnicodeString S);
extern DELPHI_PACKAGE HICON __fastcall LoadAniCursor(NativeUInt Instance, System::WideChar * ResID);
extern DELPHI_PACKAGE System::Uitypes::TCursor __fastcall DefineCursor(NativeUInt Instance, System::WideChar * ResID);
extern DELPHI_PACKAGE void __fastcall StartWait(void);
extern DELPHI_PACKAGE void __fastcall StopWait(void);
extern DELPHI_PACKAGE void __fastcall WriteText(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect, int DX, int DY, const System::UnicodeString Text, System::Classes::TAlignment Alignment, bool WordWrap, bool ARightToLeft = false);
extern DELPHI_PACKAGE void __fastcall DrawCellTextEx(Vcl::Controls::TCustomControl* Control, int ACol, int ARow, const System::UnicodeString S, const System::Types::TRect &ARect, System::Classes::TAlignment Align, TVertAlignment VertAlign, bool WordWrap, bool ARightToLeft)/* overload */;
extern DELPHI_PACKAGE void __fastcall DrawCellText(Vcl::Controls::TCustomControl* Control, int ACol, int ARow, const System::UnicodeString S, const System::Types::TRect &ARect, System::Classes::TAlignment Align, TVertAlignment VertAlign, bool ARightToLeft)/* overload */;
extern DELPHI_PACKAGE void __fastcall DrawCellTextEx(Vcl::Controls::TCustomControl* Control, int ACol, int ARow, const System::UnicodeString S, const System::Types::TRect &ARect, System::Classes::TAlignment Align, TVertAlignment VertAlign, bool WordWrap)/* overload */;
extern DELPHI_PACKAGE void __fastcall DrawCellText(Vcl::Controls::TCustomControl* Control, int ACol, int ARow, const System::UnicodeString S, const System::Types::TRect &ARect, System::Classes::TAlignment Align, TVertAlignment VertAlign)/* overload */;
extern DELPHI_PACKAGE void __fastcall DrawCellBitmap(Vcl::Controls::TCustomControl* Control, int ACol, int ARow, Vcl::Graphics::TGraphic* Bmp, const System::Types::TRect &Rect);
extern DELPHI_PACKAGE void __fastcall RaiseWin32Error(unsigned ErrorCode);
extern DELPHI_PACKAGE bool __fastcall CheckWin32(bool OK);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ResStr(const System::UnicodeString Ident);
extern DELPHI_PACKAGE bool __fastcall IsForegroundTask(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetWindowsVersion(void);
}	/* namespace Mxvclutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXVCLUTILS)
using namespace Mxvclutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxvclutilsHPP
