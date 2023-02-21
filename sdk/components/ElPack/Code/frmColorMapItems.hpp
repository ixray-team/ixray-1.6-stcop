// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frmColorMapItems.pas' rev: 35.00 (Windows)

#ifndef FrmcolormapitemsHPP
#define FrmcolormapitemsHPP

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
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElColorMap.hpp>
#include <ElBtnCtl.hpp>
#include <ElPopBtn.hpp>
#include <ElACtrls.hpp>
#include <ElCheckCtl.hpp>
#include <ElClrCmb.hpp>
#include <ElXPThemedControl.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmcolormapitems
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TColorMapItemsForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TColorMapItemsForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Elclrcmb::TElColorCombo* FgColor;
	Elclrcmb::TElColorCombo* BkColor;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* IDLbl;
	Elpopbtn::TElPopupButton* OkBtn;
	Elpopbtn::TElPopupButton* CancelBtn;
	Elpopbtn::TElPopupButton* AddBtn;
	Elpopbtn::TElPopupButton* DelBtn;
	Elpopbtn::TElPopupButton* AddGroupBtn;
	Elpopbtn::TElPopupButton* DelGroupBtn;
	Elactrls::TElAdvancedListBox* EntryLB;
	Elactrls::TElAdvancedListBox* GroupLB;
	Elcheckctl::TElCheckBox* UseBkCB;
	Elcheckctl::TElCheckBox* UseFgCB;
	void __fastcall UseFgCBClick(System::TObject* Sender);
	void __fastcall CustomFgCBClick(System::TObject* Sender);
	void __fastcall AddGroupBtnClick(System::TObject* Sender);
	void __fastcall GroupLBClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall UseBkCBClick(System::TObject* Sender);
	void __fastcall DelGroupBtnClick(System::TObject* Sender);
	void __fastcall AddBtnClick(System::TObject* Sender);
	void __fastcall DelBtnClick(System::TObject* Sender);
	void __fastcall EntryLBClick(System::TObject* Sender);
	void __fastcall FgColorChange(System::TObject* Sender);
	void __fastcall BkColorChange(System::TObject* Sender);
	
protected:
	int GrSel;
	int MapSel;
	int EntSel;
	System::UnicodeString SaveVal;
	void __fastcall RefreshEntriesList();
	
public:
	bool Runtime;
	Elcolormap::TElColorMap* Map;
	void __fastcall RefreshColors();
public:
	/* TCustomForm.Create */ inline __fastcall virtual TColorMapItemsForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TColorMapItemsForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TColorMapItemsForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TColorMapItemsForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TColorMapItemsForm* ColorMapItemsForm;
}	/* namespace Frmcolormapitems */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMCOLORMAPITEMS)
using namespace Frmcolormapitems;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmcolormapitemsHPP
