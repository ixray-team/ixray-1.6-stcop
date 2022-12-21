// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHint.pas' rev: 34.00 (Windows)

#ifndef ElhintHPP
#define ElhintHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhint
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHintWindow;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElHintWindow : public Vcl::Controls::THintWindow
{
	typedef Vcl::Controls::THintWindow inherited;
	
private:
	HIDESBASE MESSAGE void __fastcall WMNCPAINT(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	
protected:
	int XLoc;
	int YLoc;
	virtual void __fastcall Paint();
	
public:
	__fastcall virtual TElHintWindow(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElHintWindow();
	virtual void __fastcall ActivateHint(const System::Types::TRect &Rect, const System::UnicodeString AHint);
	virtual System::Types::TRect __fastcall CalcHintRect(int MaxWidth, const System::UnicodeString AHint, void * AData);
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHintWindow(HWND ParentWindow) : Vcl::Controls::THintWindow(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SetHintWindow(void);
}	/* namespace Elhint */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHINT)
using namespace Elhint;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhintHPP
