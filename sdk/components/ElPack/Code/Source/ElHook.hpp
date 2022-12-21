// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHook.pas' rev: 34.00 (Windows)

#ifndef ElhookHPP
#define ElhookHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhook
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHook;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TElHookEvent)(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);

class PASCALIMPLEMENTATION TElHook : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FDesignActive;
	TElHookEvent FOnBeforeProcess;
	TElHookEvent FOnAfterProcess;
	bool FActive;
	Vcl::Controls::TControl* FControl;
	void __fastcall SetControl(Vcl::Controls::TControl* newValue);
	void __fastcall SetActive(bool newValue);
	void __fastcall SetDesignActive(bool newValue);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall TriggerBeforeProcessEvent(Winapi::Messages::TMessage &Msg, bool &Handled);
	virtual void __fastcall TriggerAfterProcessEvent(Winapi::Messages::TMessage &Msg, bool &Handled);
	
public:
	void __fastcall HookControl(Vcl::Controls::TWinControl* AControl);
	__fastcall virtual ~TElHook();
	
__published:
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property bool DesignActive = {read=FDesignActive, write=SetDesignActive, nodefault};
	__property Vcl::Controls::TControl* Control = {read=FControl, write=SetControl};
	__property TElHookEvent OnBeforeProcess = {read=FOnBeforeProcess, write=FOnBeforeProcess};
	__property TElHookEvent OnAfterProcess = {read=FOnAfterProcess, write=FOnAfterProcess};
public:
	/* TComponent.Create */ inline __fastcall virtual TElHook(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elhook */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHOOK)
using namespace Elhook;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhookHPP
