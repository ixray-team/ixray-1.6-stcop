// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeComboBox.pas' rev: 34.00 (Windows)

#ifndef EltreecomboboxHPP
#define EltreecomboboxHPP

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
#include <HTMLLbx.hpp>
#include <ElACtrls.hpp>
#include <Vcl.StdCtrls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreecombobox
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS THackInplaceComboBox;
class DELPHICLASS TElTreeInplaceComboBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION THackInplaceComboBox : public Htmllbx::TElHTMLComboBox
{
	typedef Htmllbx::TElHTMLComboBox inherited;
	
__published:
	virtual void __fastcall ComboWndProc(Winapi::Messages::TMessage &Message, HWND ComboWnd, void * ComboProc);
public:
	/* TCustomElHTMLComboBox.Create */ inline __fastcall virtual THackInplaceComboBox(System::Classes::TComponent* AOwner) : Htmllbx::TElHTMLComboBox(AOwner) { }
	/* TCustomElHTMLComboBox.Destroy */ inline __fastcall virtual ~THackInplaceComboBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall THackInplaceComboBox(HWND ParentWindow) : Htmllbx::TElHTMLComboBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElTreeInplaceComboBox : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Htmllbx::TElHTMLComboBox* FEditor;
	bool FInitiallyDropped;
	virtual void __fastcall SetEditorParent();
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	
public:
	__fastcall virtual TElTreeInplaceComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceComboBox();
	__property Htmllbx::TElHTMLComboBox* Editor = {read=FEditor};
	
__published:
	__property bool InitiallyDropped = {read=FInitiallyDropped, write=FInitiallyDropped, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreecombobox */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREECOMBOBOX)
using namespace Eltreecombobox;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreecomboboxHPP
