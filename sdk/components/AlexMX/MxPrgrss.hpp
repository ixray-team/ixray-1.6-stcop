// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MxPrgrss.pas' rev: 35.00 (Windows)

#ifndef MxprgrssHPP
#define MxprgrssHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxprgrss
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall SupportsProgressControl(Vcl::Controls::TControl* Control);
extern DELPHI_PACKAGE void __fastcall RegisterProgressControl(Vcl::Controls::TControlClass AClass, const System::UnicodeString MaxPropName, const System::UnicodeString MinPropName, const System::UnicodeString ProgressPropName);
extern DELPHI_PACKAGE void __fastcall UnRegisterProgressControl(Vcl::Controls::TControlClass AClass);
extern DELPHI_PACKAGE void __fastcall SetProgressMax(Vcl::Controls::TControl* Control, int MaxValue);
extern DELPHI_PACKAGE void __fastcall SetProgressMin(Vcl::Controls::TControl* Control, int MinValue);
extern DELPHI_PACKAGE void __fastcall SetProgressValue(Vcl::Controls::TControl* Control, int ProgressValue);
}	/* namespace Mxprgrss */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXPRGRSS)
using namespace Mxprgrss;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxprgrssHPP
