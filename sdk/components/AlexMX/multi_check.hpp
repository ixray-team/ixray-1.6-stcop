// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'multi_check.pas' rev: 34.00 (Windows)

#ifndef Multi_checkHPP
#define Multi_checkHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Winapi.CommCtrl.hpp>

//-- user supplied -----------------------------------------------------------

namespace Multi_check
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMultiObjCheck;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TMultiObjCheck : public Vcl::Stdctrls::TCheckBox
{
	typedef Vcl::Stdctrls::TCheckBox inherited;
	
__published:
	void __fastcall ObjFirstInit(Vcl::Stdctrls::TCheckBoxState chk);
	void __fastcall ObjNextInit(Vcl::Stdctrls::TCheckBoxState chk);
	void __fastcall ObjApply(int &_to);
public:
	/* TCustomCheckBox.Create */ inline __fastcall virtual TMultiObjCheck(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TCheckBox(AOwner) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TMultiObjCheck(HWND ParentWindow) : Vcl::Stdctrls::TCheckBox(ParentWindow) { }
	/* TWinControl.Destroy */ inline __fastcall virtual ~TMultiObjCheck() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Multi_check */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MULTI_CHECK)
using namespace Multi_check;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Multi_checkHPP
