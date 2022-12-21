// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxHook.pas' rev: 34.00 (Windows)

#ifndef MxhookHPP
#define MxhookHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <mxConst.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxhook
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TRxWindowHook;
//-- type declarations -------------------------------------------------------
typedef System::TClass *PClass;

typedef void __fastcall (__closure *THookMessageEvent)(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);

class PASCALIMPLEMENTATION TRxWindowHook : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FActive;
	Vcl::Controls::TWinControl* FControl;
	System::TObject* FControlHook;
	THookMessageEvent FBeforeMessage;
	THookMessageEvent FAfterMessage;
	Vcl::Controls::TWinControl* __fastcall GetWinControl();
	HWND __fastcall GetHookHandle();
	void __fastcall SetActive(bool Value);
	void __fastcall SetWinControl(Vcl::Controls::TWinControl* Value);
	bool __fastcall IsForm();
	bool __fastcall NotIsForm();
	void * __fastcall DoUnhookControl();
	void __fastcall ReadForm(System::Classes::TReader* Reader);
	void __fastcall WriteForm(System::Classes::TWriter* Writer);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	DYNAMIC void __fastcall DoAfterMessage(Winapi::Messages::TMessage &Msg, bool &Handled);
	DYNAMIC void __fastcall DoBeforeMessage(Winapi::Messages::TMessage &Msg, bool &Handled);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TRxWindowHook(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TRxWindowHook();
	void __fastcall HookControl();
	void __fastcall UnhookControl();
	__property HWND HookWindow = {read=GetHookHandle, nodefault};
	
__published:
	__property bool Active = {read=FActive, write=SetActive, default=1};
	__property Vcl::Controls::TWinControl* WinControl = {read=GetWinControl, write=SetWinControl, stored=NotIsForm};
	__property THookMessageEvent BeforeMessage = {read=FBeforeMessage, write=FBeforeMessage};
	__property THookMessageEvent AfterMessage = {read=FAfterMessage, write=FAfterMessage};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void * __fastcall GetVirtualMethodAddress(System::TClass AClass, int AIndex);
extern DELPHI_PACKAGE void * __fastcall SetVirtualMethodAddress(System::TClass AClass, int AIndex, void * NewAddress);
extern DELPHI_PACKAGE int __fastcall FindVirtualMethodIndex(System::TClass AClass, void * MethodAddr);
}	/* namespace Mxhook */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXHOOK)
using namespace Mxhook;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxhookHPP
