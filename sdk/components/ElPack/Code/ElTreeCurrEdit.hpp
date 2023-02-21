// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeCurrEdit.pas' rev: 35.00 (Windows)

#ifndef EltreecurreditHPP
#define EltreecurreditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElTree.hpp>
#include <ElHeader.hpp>
#include <ElTools.hpp>
#include <ElStrUtils.hpp>
#include <ElCurrEdit.hpp>
#include <ElXPThemedControl.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreecurredit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS THackInplaceCurrencyEdit;
class DELPHICLASS TElTreeInplaceCurrencyEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION THackInplaceCurrencyEdit : public Elcurredit::TElCurrencyEdit
{
	typedef Elcurredit::TElCurrencyEdit inherited;
	
__published:
	DYNAMIC void __fastcall DoExit();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState ShiftState);
public:
	/* TElCurrencyEdit.Create */ inline __fastcall virtual THackInplaceCurrencyEdit(System::Classes::TComponent* AOwner) : Elcurredit::TElCurrencyEdit(AOwner) { }
	/* TElCurrencyEdit.Destroy */ inline __fastcall virtual ~THackInplaceCurrencyEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall THackInplaceCurrencyEdit(HWND ParentWindow) : Elcurredit::TElCurrencyEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElTreeInplaceCurrencyEdit : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::StaticArray<System::Classes::TWndMethod, 2> SaveIntWndProc;
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	void __fastcall IntEditorWndProc2(Winapi::Messages::TMessage &Message);
	void __fastcall IntEditorWndProc1(Winapi::Messages::TMessage &Message);
	
protected:
	Elcurredit::TElCurrencyEdit* FEditor;
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall SetEditorParent();
	void __fastcall RealWndProc(Winapi::Messages::TMessage &Message);
	
public:
	__fastcall virtual TElTreeInplaceCurrencyEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceCurrencyEdit();
	__property Elcurredit::TElCurrencyEdit* Editor = {read=FEditor};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreecurredit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREECURREDIT)
using namespace Eltreecurredit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreecurreditHPP
