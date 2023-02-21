// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxPropsEd.pas' rev: 35.00 (Windows)

#ifndef MxpropsedHPP
#define MxpropsedHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <mxPlacemnt.hpp>
#include <Vcl.Consts.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <mxVCLUtils.hpp>
#include <MXCtrls.hpp>
#include <MXProps.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxpropsed
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TFormPropsDlg;
class DELPHICLASS TFormStorageEditor;
class DELPHICLASS TStoredPropsProperty;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TFormPropsDlg : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Stdctrls::TLabel* Label30;
	Vcl::Stdctrls::TLabel* Label31;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Buttons::TSpeedButton* UpBtn;
	Vcl::Buttons::TSpeedButton* DownBtn;
	Mxctrls::TTextListBox* StoredList;
	Mxctrls::TTextListBox* PropertiesList;
	Mxctrls::TTextListBox* ComponentsList;
	Vcl::Stdctrls::TGroupBox* FormBox;
	Vcl::Stdctrls::TCheckBox* ActiveCtrlBox;
	Vcl::Stdctrls::TCheckBox* PositionBox;
	Vcl::Stdctrls::TCheckBox* StateBox;
	Vcl::Stdctrls::TButton* AddButton;
	Vcl::Stdctrls::TButton* DeleteButton;
	Vcl::Stdctrls::TButton* ClearButton;
	Vcl::Stdctrls::TButton* OkBtn;
	Vcl::Stdctrls::TButton* CancelBtn;
	void __fastcall AddButtonClick(System::TObject* Sender);
	void __fastcall ClearButtonClick(System::TObject* Sender);
	void __fastcall ListClick(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall DeleteButtonClick(System::TObject* Sender);
	void __fastcall StoredListClick(System::TObject* Sender);
	void __fastcall UpBtnClick(System::TObject* Sender);
	void __fastcall DownBtnClick(System::TObject* Sender);
	void __fastcall StoredListDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	void __fastcall StoredListDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall PropertiesListDblClick(System::TObject* Sender);
	
private:
	System::Classes::TComponent* FCompOwner;
	Designintf::_di_IDesigner FDesigner;
	void __fastcall ListToIndex(Vcl::Stdctrls::TCustomListBox* List, int Idx);
	void __fastcall UpdateCurrent();
	void __fastcall DeleteProp(int I);
	bool __fastcall FindProp(const System::UnicodeString CompName, const System::UnicodeString PropName, int &IdxComp, int &IdxProp);
	void __fastcall ClearLists();
	void __fastcall CheckAddItem(const System::UnicodeString CompName, const System::UnicodeString PropName);
	void __fastcall AddItem(int IdxComp, int IdxProp, bool AUpdate);
	void __fastcall BuildLists(System::Classes::TStrings* StoredProps);
	void __fastcall CheckButtons();
	void __fastcall SetStoredList(System::Classes::TStrings* AList);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TFormPropsDlg(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TFormPropsDlg(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TFormPropsDlg() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TFormPropsDlg(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TFormStorageEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TFormStorageEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFormStorageEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStoredPropsProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
	virtual void __fastcall Edit();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TStoredPropsProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TStoredPropsProperty() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall ShowStorageDesigner(System::Classes::TComponent* ACompOwner, Designintf::_di_IDesigner ADesigner, System::Classes::TStrings* AStoredList, Mxplacemnt::TPlacementOptions &Options);
}	/* namespace Mxpropsed */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXPROPSED)
using namespace Mxpropsed;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxpropsedHPP
