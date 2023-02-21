// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElUxTheme.pas' rev: 35.00 (Windows)

#ifndef EluxthemeHPP
#define EluxthemeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <ElTmSchema.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eluxtheme
{
//-- forward type declarations -----------------------------------------------
struct TMargins;
struct TIntList;
struct TThemeErrorContext;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TMargins
{
public:
	int cxLeftWidth;
	int cxRightWidth;
	int cyTopHeight;
	int cyBottomHeight;
};


typedef TMargins *PMargins;

struct DECLSPEC_DRECORD TIntList
{
public:
	int iValueCount;
	System::StaticArray<int, 10> iValues;
};


typedef TIntList *PIntList;

struct DECLSPEC_DRECORD TThemeErrorContext
{
public:
	unsigned dwSize;
	HRESULT hr;
	System::StaticArray<System::WideChar, 260> szMsgParam1;
	System::StaticArray<System::WideChar, 260> szMsgParam2;
	System::StaticArray<System::WideChar, 260> szFileName;
	System::StaticArray<System::WideChar, 260> szSourceLine;
	int iLineNum;
};


typedef TThemeErrorContext *PThemeErrorContext;

typedef NativeUInt HTheme;

typedef NativeUInt HIMAGELIST;

typedef bool __stdcall (*IsThemeActiveProc)(void);

typedef HRESULT __stdcall (*EnableThemingProc)(bool fEnable);

typedef NativeUInt __stdcall (*OpenThemeDataProc)(HWND hwnd, System::WideChar * pszClassList);

typedef HRESULT __stdcall (*CloseThemeDataProc)(NativeUInt Theme);

typedef HRESULT __stdcall (*DrawThemeParentBackgroundProc)(HWND hwnd, HDC hdc, const System::Types::TRect &Rect);

typedef HRESULT __stdcall (*DrawThemeBackgroundProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, const System::Types::TRect &Rect, System::Types::PRect pClipRect);

typedef HRESULT __stdcall (*DrawThemeTextProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, System::WideChar * pszText, int iCharCount, unsigned dwTextFlags, unsigned dwTextFlags2, System::Types::TRect &Rect);

typedef HRESULT __stdcall (*GetThemeBackgroundContentRectProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, const System::Types::TRect &pBoundingRect, System::Types::TRect &pContentRect);

typedef HRESULT __stdcall (*GetThemeBackgroundExtentProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, System::Types::PRect pBoundingRect, System::Types::TRect &pContentRect);

typedef HRESULT __stdcall (*GetThemePartSizeProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, System::Types::PRect rect, int eSize, System::Types::TSize &psz);

typedef HRESULT __stdcall (*GetThemeTextExtentProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, System::WideChar * pszText, int iCharCount, unsigned dwTextFlags, System::Types::PRect pBoundingRect, System::Types::TRect &pExtentRect);

typedef HRESULT __stdcall (*GetThemeTextMetricsProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, tagTEXTMETRICW &ptm);

typedef HRESULT __stdcall (*GetThemeBackgroundRegionProc)(NativeUInt Theme, int iPartId, int iStateId, System::Types::PRect pRect, HRGN &pRegion);

typedef HRESULT __stdcall (*HitTestThemeBackgroundProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, unsigned dwOptions, System::Types::PRect pRect, HRGN hrgn, System::Types::TPoint ptTest, System::Word &pwHitTestCode);

typedef HRESULT __stdcall (*DrawThemeEdgeProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, const System::Types::TRect &pDestRect, unsigned uEdge, unsigned uFlags, System::Types::PRect pContentRect);

typedef HRESULT __stdcall (*DrawThemeIconProc)(NativeUInt Theme, HDC hdc, int iPartId, int iStateId, System::Types::PRect pRect, NativeUInt himl, int iImageIndex);

typedef bool __stdcall (*IsThemePartDefinedProc)(NativeUInt Theme, int iPartId, int iStateId);

typedef bool __stdcall (*IsThemeBackgroundPartiallyTransparentProc)(NativeUInt Theme, int iPartId, int iStateId);

typedef HRESULT __stdcall (*GetThemeColorProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, unsigned &Color);

typedef HRESULT __stdcall (*GetThemeMetricProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, int &piVal);

typedef HRESULT __stdcall (*GetThemeStringProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, System::WideChar * pszBuff, int cchMaxBuffChars);

typedef HRESULT __stdcall (*GetThemeBoolProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, System::LongBool &pfVal);

typedef HRESULT __stdcall (*GetThemeIntProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, int &pfVal);

typedef HRESULT __stdcall (*GetThemeEnumValueProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, int &pfVal);

typedef HRESULT __stdcall (*GetThemePositionProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, System::Types::TPoint &pPoint);

typedef HRESULT __stdcall (*GetThemeFontProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, tagLOGFONTW &pPoint);

typedef HRESULT __stdcall (*GetThemeRectProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, System::Types::TRect &pRect);

typedef HRESULT __stdcall (*GetThemeMarginsProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, TMargins &Margins);

typedef HRESULT __stdcall (*GetThemeIntListProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, TIntList &pIntList);

typedef HRESULT __stdcall (*GetThemePropertyOriginProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, int &pOrigin);

typedef HRESULT __stdcall (*SetWindowThemeProc)(HWND hwnd, System::WideChar * pszSubAppName, System::WideChar * pszSubIdList);

typedef HRESULT __stdcall (*GetThemeFilenameProc)(NativeUInt Theme, int iPartId, int iStateId, int iPropId, System::WideChar * pszThemeFileName, int cchMaxBuffChars);

typedef unsigned __stdcall (*GetThemeSysColorProc)(NativeUInt Theme, int iColorId);

typedef HBRUSH __stdcall (*GetThemeSysColorBrushProc)(NativeUInt Theme, int iColorId);

typedef int __stdcall (*GetThemeSysSizeProc)(NativeUInt Theme, int iSizeId);

typedef int __stdcall (*GetThemeSysBoolProc)(NativeUInt Theme, int iBoolId);

typedef HRESULT __stdcall (*GetThemeSysFontProc)(NativeUInt Theme, int iFontId, tagLOGFONTW &plf);

typedef HRESULT __stdcall (*GetThemeSysStringProc)(NativeUInt Theme, int iStringId, System::WideChar * pszStringBuff, int cchMaxStringChars);

typedef HRESULT __stdcall (*GetThemeSysIntProc)(NativeUInt Theme, int iIntId, int &piValue);

typedef bool __stdcall (*IsAppThemedProc)(void);

typedef NativeUInt __stdcall (*GetWindowThemeProc)(HWND hwnd);

typedef HRESULT __stdcall (*EnableThemeDialogTextureProc)(HWND hwnd, System::LongBool fEnable);

typedef bool __stdcall (*IsThemeDialogTextureEnabledProc)(HWND hwnd);

typedef unsigned __stdcall (*GetThemeAppPropertiesProc)(void);

typedef void __stdcall (*SetThemeAppPropertiesProc)(unsigned dwFlags);

typedef HRESULT __stdcall (*GetCurrentThemeNameProc)(System::WideChar * pszThemeFileName, int cchMaxNameChars, System::WideChar * pszColorBuff, int cchMaxColorChars, System::WideChar * pszSizeBuff, int cchMaxSizeChars);

typedef HRESULT __stdcall (*GetThemeDocumentationPropertyProc)(System::WideChar * pszThemeName, System::WideChar * pszPropertyName, System::WideChar * pszValueBuff, int cchMaxValChars);

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 DTT_GRAYED = System::Int8(0x1);
static const System::Int8 HTTB_BACKGROUNDSEG = System::Int8(0x0);
static const System::Int8 HTTB_FIXEDBORDER = System::Int8(0x2);
static const System::Int8 HTTB_CAPTION = System::Int8(0x4);
static const System::Int8 HTTB_RESIZINGBORDER_LEFT = System::Int8(0x10);
static const System::Int8 HTTB_RESIZINGBORDER_TOP = System::Int8(0x20);
static const System::Int8 HTTB_RESIZINGBORDER_RIGHT = System::Int8(0x40);
static const System::Byte HTTB_RESIZINGBORDER_BOTTOM = System::Byte(0x80);
static const System::Byte HTTB_RESIZINGBORDER = System::Byte(0xf0);
static const System::Word HTTB_USESIZINGTEMPLATE = System::Word(0x100);
static const System::Int8 DTL_LEFT = System::Int8(0x1);
static const System::Int8 DTL_TOP = System::Int8(0x2);
static const System::Int8 DTL_RIGHT = System::Int8(0x4);
static const System::Int8 DTL_BOTTOM = System::Int8(0x8);
static const System::Int8 MAX_INTLIST_COUNT = System::Int8(0xa);
static const System::Int8 PO_STATE = System::Int8(0x0);
static const System::Int8 PO_PART = System::Int8(0x1);
static const System::Int8 PO_CLASS = System::Int8(0x2);
static const System::Int8 PO_GLOBAL = System::Int8(0x3);
static const System::Int8 PO_NOTFOUND = System::Int8(0x4);
static const System::Int8 STAP_ALLOW_NONCLIENT = System::Int8(0x1);
static const System::Int8 STAP_ALLOW_CONTROLS = System::Int8(0x2);
static const System::Int8 STAP_ALLOW_WEBCONTENT = System::Int8(0x4);
#define SZ_THDOCPROP_DISPLAYNAME L"DisplayName"
#define SZ_THDOCPROP_CANONICALNAME L"ThemeName"
#define SZ_THDOCPROP_TOOLTIP L"ToolTip"
#define SZ_THDOCPROP_AUTHOR L"author"
static const System::Word WM_THEMECHANGED = System::Word(0x31a);
static const System::Int8 TS_MIN = System::Int8(0x0);
static const System::Int8 TS_TRUE = System::Int8(0x1);
static const System::Int8 TS_DRAW = System::Int8(0x2);
extern DELPHI_PACKAGE IsThemeActiveProc IsThemeActive;
extern DELPHI_PACKAGE EnableThemingProc EnableTheming;
extern DELPHI_PACKAGE OpenThemeDataProc OpenThemeData;
extern DELPHI_PACKAGE CloseThemeDataProc CloseThemeData;
extern DELPHI_PACKAGE DrawThemeParentBackgroundProc DrawThemeParentBackground;
extern DELPHI_PACKAGE DrawThemeBackgroundProc DrawThemeBackground;
extern DELPHI_PACKAGE DrawThemeTextProc DrawThemeText;
extern DELPHI_PACKAGE GetThemeBackgroundContentRectProc GetThemeBackgroundContentRect;
extern DELPHI_PACKAGE GetThemeBackgroundExtentProc GetThemeBackgroundExtent;
extern DELPHI_PACKAGE GetThemePartSizeProc GetThemePartSize;
extern DELPHI_PACKAGE GetThemeTextExtentProc GetThemeTextExtent;
extern DELPHI_PACKAGE GetThemeTextMetricsProc GetThemeTextMetrics;
extern DELPHI_PACKAGE GetThemeBackgroundRegionProc GetThemeBackgroundRegion;
extern DELPHI_PACKAGE HitTestThemeBackgroundProc HitTestThemeBackground;
extern DELPHI_PACKAGE DrawThemeEdgeProc DrawThemeEdge;
extern DELPHI_PACKAGE DrawThemeIconProc DrawThemeIcon;
extern DELPHI_PACKAGE IsThemePartDefinedProc IsThemePartDefined;
extern DELPHI_PACKAGE IsThemeBackgroundPartiallyTransparentProc IsThemeBackgroundPartiallyTransparent;
extern DELPHI_PACKAGE GetThemeColorProc GetThemeColor;
extern DELPHI_PACKAGE GetThemeMetricProc GetThemeMetric;
extern DELPHI_PACKAGE GetThemeStringProc GetThemeString;
extern DELPHI_PACKAGE GetThemeBoolProc GetThemeBool;
extern DELPHI_PACKAGE GetThemeIntProc GetThemeInt;
extern DELPHI_PACKAGE GetThemeEnumValueProc GetThemeEnumValue;
extern DELPHI_PACKAGE GetThemePositionProc GetThemePosition;
extern DELPHI_PACKAGE GetThemeFontProc GetThemeFont;
extern DELPHI_PACKAGE GetThemeRectProc GetThemeRect;
extern DELPHI_PACKAGE GetThemeMarginsProc GetThemeMargins;
extern DELPHI_PACKAGE GetThemeIntListProc GetThemeIntList;
extern DELPHI_PACKAGE GetThemePropertyOriginProc GetThemePropertyOrigin;
extern DELPHI_PACKAGE SetWindowThemeProc SetWindowTheme;
extern DELPHI_PACKAGE GetThemeFilenameProc GetThemeFilename;
extern DELPHI_PACKAGE GetThemeSysColorProc GetThemeSysColor;
extern DELPHI_PACKAGE GetThemeSysColorBrushProc GetThemeSysColorBrush;
extern DELPHI_PACKAGE GetThemeSysSizeProc GetThemeSysSize;
extern DELPHI_PACKAGE GetThemeSysBoolProc GetThemeSysBool;
extern DELPHI_PACKAGE GetThemeSysFontProc GetThemeSysFont;
extern DELPHI_PACKAGE GetThemeSysStringProc GetThemeSysString;
extern DELPHI_PACKAGE GetThemeSysIntProc GetThemeSysInt;
extern DELPHI_PACKAGE IsAppThemedProc IsAppThemed;
extern DELPHI_PACKAGE GetWindowThemeProc GetWindowTheme;
extern DELPHI_PACKAGE EnableThemeDialogTextureProc EnableThemeDialogTexture;
extern DELPHI_PACKAGE IsThemeDialogTextureEnabledProc IsThemeDialogTextureEnabled;
extern DELPHI_PACKAGE GetThemeAppPropertiesProc GetThemeAppProperties;
extern DELPHI_PACKAGE SetThemeAppPropertiesProc SetThemeAppProperties;
extern DELPHI_PACKAGE GetCurrentThemeNameProc GetCurrentThemeName;
extern DELPHI_PACKAGE GetThemeDocumentationPropertyProc GetThemeDocumentationProperty;
extern DELPHI_PACKAGE bool ThemesAvailable;
extern DELPHI_PACKAGE HRESULT __fastcall GetThemePartSizeTo(System::WideChar * pszClassList, HDC hdc, int iPartId, int iStateId, System::Types::PRect rect, int eSize, System::Types::TSize &psz);
extern DELPHI_PACKAGE HRESULT __fastcall DrawThemeTextTo(System::WideChar * pszClassList, HDC hdc, int iPartId, int iStateId, System::WideChar * pszText, int iCharCount, unsigned dwTextFlags, unsigned dwTextFlags2, System::Types::TRect &Rect);
extern DELPHI_PACKAGE HRESULT __fastcall DrawThemeBackgroundTo(System::WideChar * pszClassList, HDC hdc, int iPartId, int iStateId, const System::Types::TRect &pRect, System::Types::PRect pClipRect);
}	/* namespace Eluxtheme */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELUXTHEME)
using namespace Eluxtheme;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EluxthemeHPP
