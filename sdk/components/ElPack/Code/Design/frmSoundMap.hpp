// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frmSoundMap.pas' rev: 34.00 (Windows)

#ifndef FrmsoundmapHPP
#define FrmsoundmapHPP

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
#include <ElPopBtn.hpp>
#include <Vcl.Menus.hpp>
#include <ElHeader.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <DesignWindows.hpp>
#include <DsnConst.hpp>
#include <System.TypInfo.hpp>
#include <ElSndMap.hpp>
#include <ElBtnCtl.hpp>
#include <ElTreeAdvEdit.hpp>
#include <ElTreeModalEdit.hpp>
#include <ElXPThemedControl.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmsoundmap
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSoundMapForm;
class DELPHICLASS TSoundNameProperty;
class DELPHICLASS TSoundMapEditor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSoundMapForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Elpopbtn::TElPopupButton* OkBtn;
	Eltree::TElTree* List;
	Elpopbtn::TElPopupButton* AddBtn;
	Elpopbtn::TElPopupButton* RemoveBtn;
	Vcl::Menus::TPopupMenu* PopupMenu;
	Vcl::Menus::TMenuItem* AddItem;
	Vcl::Menus::TMenuItem* RemoveItem;
	Elpopbtn::TElPopupButton* CancelBtn;
	Vcl::Dialogs::TOpenDialog* SoundDialog;
	Vcl::Menus::TMenuItem* PlayItem;
	void __fastcall AddItemClick(System::TObject* Sender);
	void __fastcall RemoveItemClick(System::TObject* Sender);
	void __fastcall ListItemFocused(System::TObject* Sender);
	void __fastcall ListEditRequest(System::TObject* Sender, Eltree::TElTreeItem* Item, Elheader::TElHeaderSection* Section);
	void __fastcall PlayItemClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	
private:
	Elsndmap::TElSoundMap* FMap;
	Eltreeadvedit::TElTreeInplaceAdvancedEdit* AdvEditor;
	Eltreemodaledit::TElTreeInplaceModalEdit* AnEditor;
	void __fastcall ModalEditorExecute(System::TObject* Sender, bool &Accepted);
	
public:
	void __fastcall SetData(Elsndmap::TElSoundMap* AMap);
	void __fastcall GetData(Elsndmap::TElSoundMap* AMap);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TSoundMapForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TSoundMapForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TSoundMapForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSoundMapForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSoundNameProperty : public Designeditors::TPropertyEditor
{
	typedef Designeditors::TPropertyEditor inherited;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
	virtual System::UnicodeString __fastcall GetValue();
	virtual void __fastcall SetValue(const System::UnicodeString Value)/* overload */;
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TSoundNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TSoundNameProperty() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSoundMapEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TSoundMapEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSoundMapEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TSoundMapForm* SoundMapForm;
}	/* namespace Frmsoundmap */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMSOUNDMAP)
using namespace Frmsoundmap;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmsoundmapHPP
