// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElXPThemedControl.pas' rev: 35.00 (Windows)

#ifndef ElxpthemedcontrolHPP
#define ElxpthemedcontrolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Messages.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elxpthemedcontrol
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElXPThemedControl;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElXPThemedControl : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	bool FUseXPThemes;
	NativeUInt FTheme;
	
protected:
	virtual void __fastcall SetUseXPThemes(const bool Value);
	virtual System::WideString __fastcall GetThemedClassName() = 0 ;
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Message);
	virtual void __fastcall FreeThemeHandle();
	virtual void __fastcall CreateThemeHandle();
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	
public:
	__fastcall virtual TElXPThemedControl(System::Classes::TComponent* AOwner);
	bool __fastcall IsThemeApplied();
	__property NativeUInt Theme = {read=FTheme, nodefault};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TElXPThemedControl() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElXPThemedControl(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elxpthemedcontrol */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELXPTHEMEDCONTROL)
using namespace Elxpthemedcontrol;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElxpthemedcontrolHPP
