// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCBFmts.pas' rev: 34.00 (Windows)

#ifndef ElcbfmtsHPP
#define ElcbfmtsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcbfmts
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall HasFormat(NativeUInt Handle, int Index);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetFormatName(int AFormat);
extern DELPHI_PACKAGE int __fastcall GetFormatIndex(System::UnicodeString FormatName);
}	/* namespace Elcbfmts */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCBFMTS)
using namespace Elcbfmts;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcbfmtsHPP
