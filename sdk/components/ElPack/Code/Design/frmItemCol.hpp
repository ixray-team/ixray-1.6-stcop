// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frmItemCol.pas' rev: 34.00 (Windows)

#ifndef FrmitemcolHPP
#define FrmitemcolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElTree.hpp>
#include <System.TypInfo.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <DesignWindows.hpp>
#include <DsnConst.hpp>
#include <ElTools.hpp>
#include <Vcl.ComCtrls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmitemcol
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TItemColDlg;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TItemColDlg : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TButton* OKBtn;
	Vcl::Stdctrls::TButton* CancelBtn;
	Vcl::Comctrls::TPageControl* PageControl1;
	Vcl::Comctrls::TTabSheet* TabSheet1;
	Vcl::Comctrls::TTabSheet* TabSheet2;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TMemo* TextEdit;
	Vcl::Stdctrls::TLabel* Label13;
	Vcl::Stdctrls::TMemo* HintEdit;
	Vcl::Stdctrls::TMemo* ColTextMemo;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TCheckBox* StylesCB;
	Vcl::Stdctrls::TGroupBox* StylesGB;
	Vcl::Stdctrls::TCheckBox* BoldCB;
	Vcl::Stdctrls::TCheckBox* ItCB;
	Vcl::Stdctrls::TCheckBox* ULCB;
	Vcl::Stdctrls::TCheckBox* StrikeCB;
	Vcl::Stdctrls::TCheckBox* ColorsCB;
	Vcl::Stdctrls::TGroupBox* ColorsGB;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* Label12;
	Vcl::Stdctrls::TComboBox* ColorCombo;
	Vcl::Stdctrls::TComboBox* BkColorCombo;
	Vcl::Stdctrls::TComboBox* RowBkColorCombo;
	Vcl::Stdctrls::TCheckBox* UseBkColorCB;
	Vcl::Stdctrls::TCheckBox* ShowChecksCB;
	Vcl::Stdctrls::TGroupBox* CBGroup;
	Vcl::Stdctrls::TLabel* Label8;
	Vcl::Stdctrls::TLabel* Label9;
	Vcl::Stdctrls::TComboBox* CBTypeCombo;
	Vcl::Stdctrls::TComboBox* CBStateCombo;
	Vcl::Stdctrls::TCheckBox* CBEnabledCB;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TLabel* Label7;
	Vcl::Stdctrls::TEdit* StIndexEdit;
	Vcl::Stdctrls::TEdit* IndexEdit;
	Vcl::Stdctrls::TLabel* Label10;
	Vcl::Stdctrls::TLabel* Label11;
	Vcl::Stdctrls::TEdit* Index2Edit;
	Vcl::Stdctrls::TEdit* StIndex2Edit;
	Vcl::Comctrls::TTabSheet* TabSheet3;
	Vcl::Stdctrls::TCheckBox* ForcedBtnsCB;
	Vcl::Stdctrls::TCheckBox* EnabledCB;
	Vcl::Stdctrls::TCheckBox* HiddenCB;
	Vcl::Stdctrls::TCheckBox* HtmlCB;
	Vcl::Stdctrls::TLabel* Label14;
	Vcl::Stdctrls::TEdit* TagEdit;
	Vcl::Stdctrls::TCheckBox* StrikeOutCB;
	Vcl::Stdctrls::TComboBox* StrikeLineColorCB;
	Vcl::Stdctrls::TCheckBox* HorZlineCB;
	Vcl::Stdctrls::TCheckBox* AllowEditCB;
	Vcl::Stdctrls::TCheckBox* SuppressButtonsCB;
	Vcl::Stdctrls::TCheckBox* MultilineCB;
	Vcl::Stdctrls::TCheckBox* OwnHeightCB;
	Vcl::Stdctrls::TEdit* HeightEdit;
	Vcl::Stdctrls::TEdit* IndentEdit;
	Vcl::Stdctrls::TCheckBox* IndentAdjustCB;
	Vcl::Stdctrls::TComboBox* BorderStyleCombo;
	Vcl::Stdctrls::TLabel* Label15;
	Vcl::Stdctrls::TCheckBox* SuppressLinesCB;
	Vcl::Stdctrls::TLabel* Label16;
	Vcl::Stdctrls::TEdit* OvIndexEdit;
	Vcl::Stdctrls::TEdit* OvIndex2Edit;
	void __fastcall ColorsCBClick(System::TObject* Sender);
	void __fastcall StylesCBClick(System::TObject* Sender);
	void __fastcall FormCloseQuery(System::TObject* Sender, bool &CanClose);
	void __fastcall OKBtnClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall CancelBtnClick(System::TObject* Sender);
	void __fastcall ShowChecksCBClick(System::TObject* Sender);
	void __fastcall CBTypeComboChange(System::TObject* Sender);
	void __fastcall StrikeOutCBClick(System::TObject* Sender);
	void __fastcall OwnHeightCBClick(System::TObject* Sender);
	void __fastcall IndentAdjustCBClick(System::TObject* Sender);
	
public:
	Eltree::TElTreeItem* Item;
	bool ByCancel;
	void __fastcall SetData();
	void __fastcall GetData();
public:
	/* TCustomForm.Create */ inline __fastcall virtual TItemColDlg(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TItemColDlg(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TItemColDlg() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TItemColDlg(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TItemColDlg* ItemColDlg;
}	/* namespace Frmitemcol */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMITEMCOL)
using namespace Frmitemcol;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmitemcolHPP
