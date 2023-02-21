// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElMenuDsgn.pas' rev: 35.00 (Windows)

#ifndef ElmenudsgnHPP
#define ElmenudsgnHPP

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
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElMenus.hpp>
#include <ElVCLUtils.hpp>
#include <ElXPThemedControl.hpp>
#include <ElStrUtils.hpp>
#include <ElTree.hpp>
#include <ElUnicodeStrings.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <DesignEditors.hpp>
#include <DesignConst.hpp>
#include <DesignIntf.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elmenudsgn
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElDesignMenu;
class DELPHICLASS TElMenuEditor;
class DELPHICLASS TElMenuItemsProperty;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElDesignMenu : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TGroupBox* GroupBox1;
	Eltree::TElTree* MenuEdit;
	Vcl::Extctrls::TPanel* Panel2;
	Vcl::Stdctrls::TButton* NewSubItemBtn;
	Vcl::Stdctrls::TButton* NewItemBtn;
	Vcl::Stdctrls::TButton* DeleteItemBtn;
	Vcl::Stdctrls::TButton* Load;
	Vcl::Stdctrls::TButton* Save;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Dialogs::TOpenDialog* OpenMenuDlg;
	Vcl::Dialogs::TSaveDialog* SaveMenuDlg;
	Vcl::Extctrls::TBevel* Bevel2;
	Vcl::Stdctrls::TButton* MoveUp;
	Vcl::Stdctrls::TButton* MoveDown;
	Vcl::Stdctrls::TButton* LevelUp;
	Vcl::Stdctrls::TButton* LevelDown;
	void __fastcall NewItemBtnClick(System::TObject* Sender);
	void __fastcall NewSubItemBtnClick(System::TObject* Sender);
	void __fastcall DeleteItemBtnClick(System::TObject* Sender);
	void __fastcall FormClose(System::TObject* Sender, System::Uitypes::TCloseAction &Action);
	void __fastcall MenuEditItemFocused(System::TObject* Sender);
	void __fastcall SaveClick(System::TObject* Sender);
	void __fastcall MoveUpClick(System::TObject* Sender);
	void __fastcall MoveDownClick(System::TObject* Sender);
	void __fastcall LevelUpClick(System::TObject* Sender);
	void __fastcall LevelDownClick(System::TObject* Sender);
	void __fastcall LoadClick(System::TObject* Sender);
	void __fastcall MenuEditStartDrag(System::TObject* Sender, Vcl::Controls::TDragObject* &DragObject);
	void __fastcall MenuEditDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	void __fastcall MenuEditDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	
private:
	Vcl::Menus::TMenu* FMenu;
	Eltree::TElTreeItem* FDragItem;
	Elmenus::TElMenuItem* FElMenuItem;
	void __fastcall SetElMenu(Vcl::Menus::TMenu* const Value);
	void __fastcall MenuChanged(System::TObject* Sender, Elmenus::TElMenuItem* Source, bool Rebuild);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	System::Classes::TComponent* AComp;
	Designintf::_di_IDesigner ADesigner;
	__fastcall virtual ~TElDesignMenu();
	__property Vcl::Menus::TMenu* Menu = {read=FMenu, write=SetElMenu};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TElDesignMenu(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TElDesignMenu(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDesignMenu(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElMenuEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TElMenuEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElMenuEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElMenuItemsProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual void __fastcall Edit();
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TElMenuItemsProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TElMenuItemsProperty() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TElDesignMenu* ElDesignMenu;
}	/* namespace Elmenudsgn */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELMENUDSGN)
using namespace Elmenudsgn;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElmenudsgnHPP
