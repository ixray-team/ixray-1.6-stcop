// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElPrinterPreview.pas' rev: 34.00 (Windows)

#ifndef ElprinterpreviewHPP
#define ElprinterpreviewHPP

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
#include <Vcl.ExtCtrls.hpp>
#include <ElPanel.hpp>
#include <ElToolbar.hpp>
#include <ElStatBar.hpp>
#include <ElPopBtn.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElACtrls.hpp>
#include <ElTools.hpp>
#include <Vcl.Printers.hpp>
#include <ElPrinter.hpp>
#include <ElSpin.hpp>
#include <ElList.hpp>
#include <ElHook.hpp>
#include <ElCombos.hpp>
#include <ElXPThemedControl.hpp>
#include <ElEdits.hpp>
#include <ElScrollBox.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elprinterpreview
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElPrinterPreviewDlg;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElPrinterPreviewDlg : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Eltoolbar::TElToolBar* Toolbar;
	Vcl::Forms::TScrollBox* ScrollBox;
	Elstatbar::TElStatusBar* StatusBar;
	Eltoolbar::TElToolButton* PrintBtn;
	Eltoolbar::TElToolButton* ElToolButton2;
	Eltoolbar::TElToolButton* OnePageBtn;
	Eltoolbar::TElToolButton* MultipageBtn;
	Eltoolbar::TElToolButton* ElToolButton1;
	Eltoolbar::TElToolButton* SaveBtn;
	Eltoolbar::TElToolButton* ElToolButton3;
	Vcl::Dialogs::TPrintDialog* PrintDialog;
	Eltoolbar::TElToolButton* PrintSetupBtn;
	Eltoolbar::TElToolButton* PrevPageBtn;
	Eltoolbar::TElToolButton* ElToolButton5;
	Eltoolbar::TElToolButton* NextPageBtn;
	Elspin::TElSpinEdit* PageSpin;
	Vcl::Dialogs::TPrinterSetupDialog* PrinterSetupDialog;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	Elpopbtn::TElGraphicButton* CloseBtn;
	Elpanel::TElPanel* PagesPanel;
	Elpanel::TElPanel* MainPagePanel;
	Elhook::TElHook* ElHook1;
	Elcombos::TElComboBox* ScaleCombo;
	Elhook::TElHook* ElHook2;
	void __fastcall PrintBtnClick(System::TObject* Sender);
	void __fastcall ScrollBoxResize(System::TObject* Sender);
	void __fastcall MainPagePanelPaint(System::TObject* Sender);
	void __fastcall PageSpinChange(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall ScaleComboExit(System::TObject* Sender);
	void __fastcall NextPageBtnClick(System::TObject* Sender);
	void __fastcall PrintSetupBtnClick(System::TObject* Sender);
	void __fastcall CloseBtnClick(System::TObject* Sender);
	void __fastcall SaveBtnClick(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall PrevPageBtnClick(System::TObject* Sender);
	void __fastcall ScComboKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ElHook1AfterProcess(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall FormResize(System::TObject* Sender);
	void __fastcall ScaleComboChange(System::TObject* Sender);
	void __fastcall MultipageBtnClick(System::TObject* Sender);
	void __fastcall OnePageBtnClick(System::TObject* Sender);
	void __fastcall ScrollBoxKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ElHook2AfterProcess(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	
private:
	int FScale;
	int FCurrentPage;
	int FTotalPages;
	Ellist::TElList* Panels;
	int PagePanels;
	int HorzPages;
	int VertPages;
	Elprinter::TElPrinter* FPrinter;
	int FScaleIdx;
	int FRealIdx;
	void __fastcall SetCurrentPage(int Value);
	void __fastcall SetTotalPages(int Value);
	void __fastcall SetScale(int Value);
	
protected:
	void __fastcall UpdatePageNumbers();
	void __fastcall UpdatePanels();
	void __fastcall UpdateMultiPage();
	
public:
	void __fastcall SetData(Elprinter::TElPrinter* Printer);
	__property int CurrentPage = {read=FCurrentPage, write=SetCurrentPage, nodefault};
	__property int TotalPages = {read=FTotalPages, write=SetTotalPages, nodefault};
	__property int Scale = {read=FScale, write=SetScale, nodefault};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TElPrinterPreviewDlg(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TElPrinterPreviewDlg(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TElPrinterPreviewDlg() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElPrinterPreviewDlg(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TElPrinterPreviewDlg* ElPrinterPreviewDlg;
}	/* namespace Elprinterpreview */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELPRINTERPREVIEW)
using namespace Elprinterpreview;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElprinterpreviewHPP
