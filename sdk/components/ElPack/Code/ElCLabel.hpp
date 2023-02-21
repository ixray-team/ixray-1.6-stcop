// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCLabel.pas' rev: 35.00 (Windows)

#ifndef ElclabelHPP
#define ElclabelHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elclabel
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElCustomLabel;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElCustomLabel : public Vcl::Stdctrls::TLabel
{
	typedef Vcl::Stdctrls::TLabel inherited;
	
public:
	virtual void __fastcall Loaded();
	
__published:
	__property Color;
public:
	/* TCustomLabel.Create */ inline __fastcall virtual TElCustomLabel(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TLabel(AOwner) { }
	
public:
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TElCustomLabel() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elclabel */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCLABEL)
using namespace Elclabel;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElclabelHPP
