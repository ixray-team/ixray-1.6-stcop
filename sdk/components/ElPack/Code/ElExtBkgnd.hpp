// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElExtBkgnd.pas' rev: 35.00 (Windows)

#ifndef ElextbkgndHPP
#define ElextbkgndHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <ElTools.hpp>
#include <ElVCLUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elextbkgnd
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall ExtDrawBkgnd2(HDC DC, HWND Wnd, const System::Types::TRect &R, const System::Types::TRect &DocRect, const System::Types::TPoint &Origin, System::Uitypes::TColor FillColor, Vcl::Graphics::TBitmap* SourceBitmap, Elvclutils::TElBkGndType DrawMode);
extern DELPHI_PACKAGE void __fastcall ExtDrawBkgnd(HDC DC, HWND Wnd, const System::Types::TRect &RectDoc, const System::Types::TRect &RectWindow, const System::Types::TRect &RectDC, const System::Types::TRect &RectOnDC, bool InvertedMode, System::Uitypes::TColor FillColor, System::Uitypes::TColor OverColor, bool DoBlend, Vcl::Graphics::TBitmap* SourceBitmap, Elvclutils::TElBkGndType DrawMode);
}	/* namespace Elextbkgnd */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELEXTBKGND)
using namespace Elextbkgnd;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElextbkgndHPP
