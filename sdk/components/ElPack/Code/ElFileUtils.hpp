// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElFileUtils.pas' rev: 35.00 (Windows)

#ifndef ElfileutilsHPP
#define ElfileutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elfileutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::WideString __fastcall GetWideCurrentDir(void);
extern DELPHI_PACKAGE bool __fastcall SetWideCurrentDir(const System::WideString Dir);
extern DELPHI_PACKAGE bool __fastcall CreateWideDir(const System::WideString Dir);
extern DELPHI_PACKAGE bool __fastcall RemoveWideDir(const System::WideString Dir);
extern DELPHI_PACKAGE System::WideString __fastcall GetWideModuleName(NativeUInt Module);
}	/* namespace Elfileutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELFILEUTILS)
using namespace Elfileutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElfileutilsHPP
