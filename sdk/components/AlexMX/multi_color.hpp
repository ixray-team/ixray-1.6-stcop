// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'multi_color.pas' rev: 34.00 (Windows)

#ifndef Multi_colorHPP
#define Multi_colorHPP

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

namespace Multi_color
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMultiObjColor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TMultiObjColor : public Vcl::Extctrls::TShape
{
	typedef Vcl::Extctrls::TShape inherited;
	
private:
	int m_BeforeDialog;
	int m_AfterDialog;
	bool m_Diffs;
	bool m_Changed;
	
public:
	__fastcall virtual TMultiObjColor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMultiObjColor();
	void __fastcall ObjFirstInit(int value);
	void __fastcall ObjNextInit(int value);
	bool __fastcall ObjApply(int &_to);
	int __fastcall Get();
	void __fastcall _Set(int value);
	bool __fastcall diffs();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Multi_color */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MULTI_COLOR)
using namespace Multi_color;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Multi_colorHPP
