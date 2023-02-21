// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElColor.pas' rev: 35.00 (Windows)

#ifndef ElcolorHPP
#define ElcolorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <ElTools.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcolor
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int __fastcall ColorToRGB(const System::Uitypes::TColor Color);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall BrightColor(const System::Uitypes::TColor Color, const System::Byte Percent);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall DarkColor(const System::Uitypes::TColor Color, const System::Byte Percent);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall ColorToGray(const System::Uitypes::TColor Color);
extern DELPHI_PACKAGE int __fastcall ConvertColorToHTML(System::Uitypes::TColor Color);
}	/* namespace Elcolor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCOLOR)
using namespace Elcolor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcolorHPP
