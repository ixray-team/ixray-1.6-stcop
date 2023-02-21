// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDailyTip.pas' rev: 35.00 (Windows)

#ifndef EldailytipHPP
#define EldailytipHPP

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
#include <Vcl.ExtCtrls.hpp>
#include <ElPopBtn.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElTools.hpp>
#include <Vcl.Graphics.hpp>
#include <ElBtnCtl.hpp>
#include <ElStrUtils.hpp>
#include <ElStrPool.hpp>
#include <ElXPThemedControl.hpp>
#include <ElHTMLLbl.hpp>
#include <HTMLRender.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldailytip
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElDailyTipForm;
class DELPHICLASS TElDailyTipDialog;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElDailyTipForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Elpopbtn::TElPopupButton* OkBtn;
	Vcl::Stdctrls::TCheckBox* NextTimeCB;
	Elpopbtn::TElPopupButton* NextBtn;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Extctrls::TPanel* Panel2;
	Vcl::Extctrls::TImage* Image1;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Extctrls::TPanel* Panel4;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Extctrls::TPanel* Panel5;
	Vcl::Stdctrls::TLabel* TipNumLabel;
	Elhtmllbl::TElHTMLLabel* TipText;
	void __fastcall NextBtnClick(System::TObject* Sender);
	
private:
	int MinNum;
	int CurNum;
	int MaxNum;
	Elstrpool::TElStringPool* FStringPool;
public:
	/* TCustomForm.Create */ inline __fastcall virtual TElDailyTipForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TElDailyTipForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TElDailyTipForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDailyTipForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElDailyTipDialog : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FShowNextTime;
	int FStartID;
	int FEndID;
	bool FShowTipNumber;
	Elstrpool::TElStringPool* FStringPool;
	bool FIsHTML;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	System::Uitypes::TColor FLinkColor;
	System::Uitypes::TFontStyles FLinkStyle;
	void __fastcall SetStringPool(Elstrpool::TElStringPool* newValue);
	void __fastcall SetStartID(int newValue);
	void __fastcall SetEndID(int newValue);
	void __fastcall SetIsHTML(bool newValue);
	
protected:
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	
public:
	void __fastcall Execute();
	__fastcall virtual TElDailyTipDialog(System::Classes::TComponent* AOwner);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	
__published:
	__property bool ShowNextTime = {read=FShowNextTime, write=FShowNextTime, nodefault};
	__property int StartID = {read=FStartID, write=SetStartID, default=10001};
	__property int EndID = {read=FEndID, write=SetEndID, default=10001};
	__property bool ShowTipNumber = {read=FShowTipNumber, write=FShowTipNumber, default=1};
	__property Elstrpool::TElStringPool* StringPool = {read=FStringPool, write=SetStringPool};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, nodefault};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=SetLinkColor, nodefault};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=SetLinkStyle, nodefault};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TElDailyTipDialog() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TElDailyTipForm* ElDailyTipForm;
}	/* namespace Eldailytip */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDAILYTIP)
using namespace Eldailytip;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldailytipHPP
