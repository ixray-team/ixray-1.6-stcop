// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeStdEditors.pas' rev: 34.00 (Windows)

#ifndef EltreestdeditorsHPP
#define EltreestdeditorsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElEdits.hpp>
#include <ElTree.hpp>
#include <ElHeader.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreestdeditors
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElTreeInplaceEdit;
class DELPHICLASS TElTreeInplaceMemo;
class DELPHICLASS TElTreeInplaceDateTimePicker;
class DELPHICLASS TElTreeInplaceCheckBox;
class DELPHICLASS THackInplaceComboBox;
class DELPHICLASS TElTreeInplaceComboBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElTreeInplaceEdit : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Eledits::TElEdit* FEditor;
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceEdit();
	__property Eledits::TElEdit* Editor = {read=FEditor};
};


class PASCALIMPLEMENTATION TElTreeInplaceMemo : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Vcl::Stdctrls::TMemo* FEditor;
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceMemo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceMemo();
	__property Vcl::Stdctrls::TMemo* Editor = {read=FEditor};
};


class PASCALIMPLEMENTATION TElTreeInplaceDateTimePicker : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Vcl::Comctrls::TDateTimePicker* FEditor;
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceDateTimePicker(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceDateTimePicker();
	__property Vcl::Comctrls::TDateTimePicker* Editor = {read=FEditor};
};


class PASCALIMPLEMENTATION TElTreeInplaceCheckBox : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Vcl::Stdctrls::TCheckBox* FEditor;
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceCheckBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceCheckBox();
	__property Vcl::Stdctrls::TCheckBox* Editor = {read=FEditor};
};


class PASCALIMPLEMENTATION THackInplaceComboBox : public Vcl::Stdctrls::TComboBox
{
	typedef Vcl::Stdctrls::TComboBox inherited;
	
__published:
	virtual void __fastcall ComboWndProc(Winapi::Messages::TMessage &Message, HWND ComboWnd, void * ComboProc);
	
protected:
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
public:
	/* TCustomComboBox.Create */ inline __fastcall virtual THackInplaceComboBox(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TComboBox(AOwner) { }
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~THackInplaceComboBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall THackInplaceComboBox(HWND ParentWindow) : Vcl::Stdctrls::TComboBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElTreeInplaceComboBox : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	bool FInitiallyDropped;
	Vcl::Stdctrls::TComboBox* FEditor;
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceComboBox();
	__property Vcl::Stdctrls::TComboBox* Editor = {read=FEditor};
	
__published:
	__property bool InitiallyDropped = {read=FInitiallyDropped, write=FInitiallyDropped, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreestdeditors */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREESTDEDITORS)
using namespace Eltreestdeditors;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreestdeditorsHPP
