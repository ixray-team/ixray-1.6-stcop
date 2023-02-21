// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frmSectEdit.pas' rev: 35.00 (Windows)

#ifndef FrmsecteditHPP
#define FrmsecteditHPP

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
#include <ElHeader.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmsectedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSectEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSectEdit : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TEdit* ImIndexEdit;
	Vcl::Stdctrls::TGroupBox* GroupBox1;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TEdit* TextEB;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TEdit* FieldEdit;
	Vcl::Stdctrls::TLabel* Label11;
	Vcl::Stdctrls::TEdit* HintEdit;
	Vcl::Stdctrls::TLabel* Label8;
	Vcl::Stdctrls::TGroupBox* GroupBox2;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TEdit* WidthEB;
	Vcl::Stdctrls::TEdit* MinWidthEB;
	Vcl::Stdctrls::TEdit* MaxWidthEB;
	Vcl::Stdctrls::TLabel* Label7;
	Vcl::Stdctrls::TLabel* Label9;
	Vcl::Stdctrls::TLabel* Label10;
	Vcl::Stdctrls::TGroupBox* GroupBox3;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Stdctrls::TComboBox* StyleCombo;
	Vcl::Stdctrls::TComboBox* ColTypeCB;
	Vcl::Stdctrls::TComboBox* PopupCombo;
	Vcl::Stdctrls::TComboBox* ParentCombo;
	Vcl::Stdctrls::TCheckBox* ExpandableCB;
	Vcl::Stdctrls::TCheckBox* ExpandedCB;
	Vcl::Stdctrls::TCheckBox* FilterCB;
	Vcl::Stdctrls::TCheckBox* LookupCB;
	Vcl::Stdctrls::TCheckBox* ClickCB;
	Vcl::Stdctrls::TCheckBox* ClickSelCB;
	Vcl::Stdctrls::TCheckBox* ResizeCB;
	Vcl::Stdctrls::TCheckBox* PswCB;
	Vcl::Stdctrls::TCheckBox* EditCB;
	Vcl::Extctrls::TRadioGroup* AlignRG;
	Vcl::Extctrls::TRadioGroup* LayoutRG;
	Vcl::Extctrls::TRadioGroup* ImAlignRG;
	Vcl::Extctrls::TRadioGroup* SortRG;
	Vcl::Stdctrls::TButton* ElPopupButton1;
	Vcl::Stdctrls::TButton* ElPopupButton2;
	Vcl::Stdctrls::TCheckBox* VisCB;
	Vcl::Stdctrls::TCheckBox* AutosizeCB;
	Vcl::Stdctrls::TCheckBox* ShowSortMarkCB;
	void __fastcall ExpandableCBClick(System::TObject* Sender);
	void __fastcall FilterCBClick(System::TObject* Sender);
	void __fastcall LookupCBClick(System::TObject* Sender);
	
public:
	Elheader::TElHeaderSection* Item;
	Elheader::TElHeaderSections* Items;
	Vcl::Forms::TCustomForm* Form;
	void __fastcall SetData();
	void __fastcall GetData();
public:
	/* TCustomForm.Create */ inline __fastcall virtual TSectEdit(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TSectEdit(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TSectEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSectEdit(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TSectEdit* SectEdit;
}	/* namespace Frmsectedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMSECTEDIT)
using namespace Frmsectedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmsecteditHPP
