// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCGControl.pas' rev: 35.00 (Windows)

#ifndef ElcgcontrolHPP
#define ElcgcontrolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Controls.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcgcontrol
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElCustomGraphicControl;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElCustomGraphicControl : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
public:
	virtual void __fastcall Loaded();
	
__published:
	__property Color;
public:
	/* TGraphicControl.Create */ inline __fastcall virtual TElCustomGraphicControl(System::Classes::TComponent* AOwner) : Vcl::Controls::TGraphicControl(AOwner) { }
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TElCustomGraphicControl() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elcgcontrol */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCGCONTROL)
using namespace Elcgcontrol;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcgcontrolHPP
