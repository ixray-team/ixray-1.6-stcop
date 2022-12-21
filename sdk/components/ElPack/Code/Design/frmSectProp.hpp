// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frmSectProp.pas' rev: 34.00 (Windows)

#ifndef FrmsectpropHPP
#define FrmsectpropHPP

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
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <DesignWindows.hpp>
#include <DsnConst.hpp>
#include <ElTools.hpp>
#include <ElHeader.hpp>
#include <Vcl.StdCtrls.hpp>
#include <frmSectEdit.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElXPThemedControl.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmsectprop
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElSectionsPropDlg;
class DELPHICLASS TElSectionsProperty;
class DELPHICLASS TElHeaderEditor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElSectionsPropDlg : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Dialogs::TOpenDialog* OpenDlg;
	Vcl::Dialogs::TSaveDialog* SaveDlg;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Stdctrls::TButton* Button1;
	Vcl::Stdctrls::TButton* Button2;
	Vcl::Stdctrls::TButton* Button3;
	Vcl::Stdctrls::TGroupBox* GroupBox1;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Stdctrls::TListBox* SecList;
	Vcl::Extctrls::TPanel* Panel2;
	Vcl::Stdctrls::TButton* AddBtn;
	Vcl::Stdctrls::TButton* DeleteBtn;
	Vcl::Stdctrls::TButton* EditBtn;
	Vcl::Stdctrls::TButton* UpBtn;
	Vcl::Stdctrls::TButton* DownBtn;
	Vcl::Stdctrls::TButton* LoadBtn;
	Vcl::Stdctrls::TButton* SaveBtn;
	Elheader::TElHeader* TestHeader;
	Vcl::Stdctrls::TButton* DuplicateBtn;
	Vcl::Stdctrls::TButton* ReindexBtn;
	void __fastcall LoadBtnClick(System::TObject* Sender);
	void __fastcall SaveBtnClick(System::TObject* Sender);
	void __fastcall EditBtnClick(System::TObject* Sender);
	void __fastcall AddBtnClick(System::TObject* Sender);
	void __fastcall DeleteBtnClick(System::TObject* Sender);
	void __fastcall UpBtnClick(System::TObject* Sender);
	void __fastcall DownBtnClick(System::TObject* Sender);
	void __fastcall SecListClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall SecListKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall SecListDblClick(System::TObject* Sender);
	void __fastcall DuplicateBtnClick(System::TObject* Sender);
	void __fastcall Button3Click(System::TObject* Sender);
	void __fastcall ReindexBtnClick(System::TObject* Sender);
	
public:
	Elheader::TElHeaderSections* ASect;
	void __fastcall FillSecList();
	void __fastcall SetData();
	void __fastcall GetData();
	__fastcall virtual TElSectionsPropDlg(System::Classes::TComponent* AOwner);
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TElSectionsPropDlg(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TElSectionsPropDlg() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElSectionsPropDlg(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElSectionsProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual void __fastcall Edit();
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TElSectionsProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TElSectionsProperty() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElHeaderEditor : public Designeditors::TDefaultEditor
{
	typedef Designeditors::TDefaultEditor inherited;
	
public:
	virtual void __fastcall EditProperty(const Designintf::_di_IProperty Prop, bool &Continue);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TElHeaderEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TDefaultEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElHeaderEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TElSectionsPropDlg* ElSectionsPropDlg;
}	/* namespace Frmsectprop */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMSECTPROP)
using namespace Frmsectprop;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmsectpropHPP
