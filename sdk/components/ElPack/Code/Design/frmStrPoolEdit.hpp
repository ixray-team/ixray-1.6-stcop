// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frmStrPoolEdit.pas' rev: 35.00 (Windows)

#ifndef FrmstrpooleditHPP
#define FrmstrpooleditHPP

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
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <ElTree.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElACtrls.hpp>
#include <ElSplit.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElPanel.hpp>
#include <ElBtnCtl.hpp>
#include <ElPopBtn.hpp>
#include <Vcl.Menus.hpp>
#include <ElStrArray.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <DesignWindows.hpp>
#include <DsnConst.hpp>
#include <ElStrPool.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <System.TypInfo.hpp>
#include <ElFrmPers.hpp>
#include <ElIni.hpp>
#include <ElXPThemedControl.hpp>
#include <ElEdits.hpp>
#include <ElUnicodeStrings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmstrpooledit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStrPoolEditForm;
class DELPHICLASS TStrPoolItemsProperty;
class DELPHICLASS TStrPoolItemsEditor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStrPoolEditForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Elpanel::TElPanel* ElPanel1;
	Elpanel::TElPanel* ElPanel2;
	Elpanel::TElPanel* ElPanel3;
	Elsplit::TElSplitter* ElSplitter1;
	Elpanel::TElPanel* ElPanel4;
	Eltree::TElTree* List;
	Elpopbtn::TElPopupButton* OkBtn;
	Elpopbtn::TElPopupButton* CancelBtn;
	Elpopbtn::TElPopupButton* AddBtn;
	Elpopbtn::TElPopupButton* InsertBtn;
	Elpopbtn::TElPopupButton* DeleteBtn;
	Vcl::Menus::TPopupMenu* PopupMenu;
	Vcl::Menus::TMenuItem* AddItem;
	Vcl::Menus::TMenuItem* InsertItem;
	Vcl::Menus::TMenuItem* DeleteItem;
	Vcl::Menus::TMainMenu* MainMenu;
	Vcl::Menus::TMenuItem* Pool1;
	Vcl::Menus::TMenuItem* Clear1;
	Vcl::Menus::TMenuItem* Open1;
	Vcl::Menus::TMenuItem* Save1;
	Vcl::Menus::TMenuItem* Text1;
	Vcl::Menus::TMenuItem* Open2;
	Vcl::Menus::TMenuItem* Save2;
	Elfrmpers::TElFormPersist* ElFormPersist1;
	Elini::TElIniFile* ElIniFile1;
	Elpopbtn::TElPopupButton* UpBtn;
	Elpopbtn::TElPopupButton* DownBtn;
	Elpopbtn::TElPopupButton* CopyBtn;
	Eledits::TElEdit* Memo;
	void __fastcall ListItemFocused(System::TObject* Sender);
	void __fastcall AddBtnClick(System::TObject* Sender);
	void __fastcall InsertBtnClick(System::TObject* Sender);
	void __fastcall DeleteBtnClick(System::TObject* Sender);
	void __fastcall ListItemDeletion(System::TObject* Sender, Eltree::TElTreeItem* Item);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall ListVirtualTextNeeded(System::TObject* Sender, Eltree::TElTreeItem* Item, int SectionIndex, Elstrutils::TElFString &Text);
	void __fastcall ListVirtualHintNeeded(System::TObject* Sender, Eltree::TElTreeItem* Item, Elstrutils::TElFString &Hint);
	void __fastcall UpBtnClick(System::TObject* Sender);
	void __fastcall DownBtnClick(System::TObject* Sender);
	void __fastcall CopyBtnClick(System::TObject* Sender);
	void __fastcall OkBtnClick(System::TObject* Sender);
	
private:
	int CurIndex;
	Elunicodestrings::TElWideStringArray* StrArray;
	
public:
	void __fastcall GetData(Elunicodestrings::TElWideStringArray* anArray);
	void __fastcall SetData(Elunicodestrings::TElWideStringArray* anArray);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TStrPoolEditForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TStrPoolEditForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TStrPoolEditForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TStrPoolEditForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TStrPoolItemsProperty : public Designeditors::TPropertyEditor
{
	typedef Designeditors::TPropertyEditor inherited;
	
public:
	virtual void __fastcall Edit();
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TStrPoolItemsProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TStrPoolItemsProperty() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStrPoolItemsEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TStrPoolItemsEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStrPoolItemsEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TStrPoolEditForm* StrPoolEditForm;
}	/* namespace Frmstrpooledit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMSTRPOOLEDIT)
using namespace Frmstrpooledit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmstrpooleditHPP
