// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElClipMon.pas' rev: 35.00 (Windows)

#ifndef ElclipmonHPP
#define ElclipmonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <ElCBFmts.hpp>
#include <Winapi.Windows.hpp>
#include <ElBaseComp.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elclipmon
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElClipboardMonitor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElClipboardMonitor : public Elbasecomp::TElBaseComponent
{
	typedef Elbasecomp::TElBaseComponent inherited;
	
protected:
	HWND FPrevHandle;
	System::Classes::TStrings* FDataFormats;
	System::Classes::TNotifyEvent FOnChange;
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall DoSetEnabled(bool AEnabled);
	virtual void __fastcall TriggerChangeEvent();
	System::Classes::TStrings* __fastcall GetDataFormats();
	
public:
	__fastcall virtual ~TElClipboardMonitor();
	System::UnicodeString __fastcall GetDataString(System::UnicodeString Format);
	__property System::Classes::TStrings* DataFormats = {read=GetDataFormats};
	
__published:
	__property Enabled = {default=0};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TElBaseComponent.Create */ inline __fastcall virtual TElClipboardMonitor(System::Classes::TComponent* AOwner) : Elbasecomp::TElBaseComponent(AOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elclipmon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCLIPMON)
using namespace Elclipmon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElclipmonHPP
