// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frmFormPers.pas' rev: 35.00 (Windows)

#ifndef FrmformpersHPP
#define FrmformpersHPP

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
#include <ElTree.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElPopBtn.hpp>
#include <ElImgLst.hpp>
#include <DesignEditors.hpp>
#include <DesignWindows.hpp>
#include <DsnConst.hpp>
#include <DesignIntf.hpp>
#include <ElFrmPers.hpp>
#include <ElMTree.hpp>
#include <Vcl.Menus.hpp>
#include <ElIni.hpp>
#include <System.TypInfo.hpp>
#include <ElVCLUtils.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElHeader.hpp>
#include <ElBtnCtl.hpp>
#include <Vcl.Graphics.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmformpers
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPersPropsForm;
class DELPHICLASS TPropListProperty;
class DELPHICLASS TElPropListEditor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TPersPropsForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* BtnPanel;
	Elpopbtn::TElPopupButton* OkBtn;
	Elpopbtn::TElPopupButton* CancelBtn;
	Elimglst::TElImageList* ImageList;
	Eltree::TElTree* Tree;
	Elfrmpers::TElFormPersist* ElFormPersist1;
	Elini::TElIniFile* ElIniFile1;
	void __fastcall TreeItemChange(System::TObject* Sender, Eltree::TElTreeItem* Item, Eltree::TItemChangeMode ItemChangeMode);
	void __fastcall TreeHeaderSectionCollapse(System::TObject* Sender, Elheader::TElHeaderSection* Section);
	void __fastcall TreeHeaderSectionExpand(System::TObject* Sender, Elheader::TElHeaderSection* Section);
	void __fastcall ElFormPersist1Restore(System::TObject* Sender);
	void __fastcall ElFormPersist1Save(System::TObject* Sender);
	
protected:
	int FOldWidth;
	
public:
	Elmtree::TElMTree* Props;
	Elfrmpers::TElFormPersist* FPers;
	void __fastcall GetData();
	void __fastcall SetData();
	__fastcall virtual TPersPropsForm(System::Classes::TComponent* AOwner);
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TPersPropsForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TPersPropsForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TPersPropsForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TPropListProperty : public Designeditors::TPropertyEditor
{
	typedef Designeditors::TPropertyEditor inherited;
	
public:
	virtual void __fastcall Edit();
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TPropListProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TPropListProperty() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElPropListEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TElPropListEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElPropListEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TPersPropsForm* PersPropsForm;
}	/* namespace Frmformpers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMFORMPERS)
using namespace Frmformpers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmformpersHPP
