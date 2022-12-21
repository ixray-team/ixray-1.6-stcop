// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxMaxMin.pas' rev: 34.00 (Windows)

#ifndef MxmaxminHPP
#define MxmaxminHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxmaxmin
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SwapInt(int &Int1, int &Int2);
extern DELPHI_PACKAGE void __fastcall SwapInt64(__int64 &Int1, __int64 &Int2);
extern DELPHI_PACKAGE void __fastcall SwapLong(int &Int1, int &Int2);
extern DELPHI_PACKAGE int __fastcall Max(int A, int B);
extern DELPHI_PACKAGE int __fastcall Min(int A, int B);
extern DELPHI_PACKAGE int __fastcall MaxInteger(const int *Values, const int Values_High);
extern DELPHI_PACKAGE int __fastcall MinInteger(const int *Values, const int Values_High);
extern DELPHI_PACKAGE __int64 __fastcall MaxInt64(const __int64 *Values, const int Values_High);
extern DELPHI_PACKAGE __int64 __fastcall MinInt64(const __int64 *Values, const int Values_High);
extern DELPHI_PACKAGE System::Extended __fastcall MaxFloat(const System::Extended *Values, const int Values_High);
extern DELPHI_PACKAGE System::Extended __fastcall MinFloat(const System::Extended *Values, const int Values_High);
extern DELPHI_PACKAGE System::TDateTime __fastcall MaxDateTime(const System::TDateTime *Values, const int Values_High);
extern DELPHI_PACKAGE System::TDateTime __fastcall MinDateTime(const System::TDateTime *Values, const int Values_High);
extern DELPHI_PACKAGE System::Variant __fastcall MaxOf(const System::Variant *Values, const int Values_High);
extern DELPHI_PACKAGE System::Variant __fastcall MinOf(const System::Variant *Values, const int Values_High);
}	/* namespace Mxmaxmin */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXMAXMIN)
using namespace Mxmaxmin;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxmaxminHPP
