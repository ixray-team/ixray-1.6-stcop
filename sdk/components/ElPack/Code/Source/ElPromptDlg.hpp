// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElPromptDlg.pas' rev: 34.00 (Windows)

#ifndef ElpromptdlgHPP
#define ElpromptdlgHPP

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
#include <ElBtnCtl.hpp>
#include <ElCheckCtl.hpp>
#include <ElPopBtn.hpp>
#include <ElStrPool.hpp>
#include <ElStrUtils.hpp>
#include <ElStrArray.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElImgLst.hpp>
#include <ElHTMLLbl.hpp>
#include <HTMLRender.hpp>
#include <ElFrmPers.hpp>
#include <ElVCLUtils.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElXPThemedControl.hpp>
#include <ElPanel.hpp>
#include <Vcl.Consts.hpp>
#include <ElCaption.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elpromptdlg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElPromptForm;
class DELPHICLASS TElPromptDialog;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TElPromptFormClass;

typedef void __fastcall (__closure *TPromptCloseEvent)(System::TObject* Sender, int Result);

class PASCALIMPLEMENTATION TElPromptForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TTimer* Timer;
	Elpanel::TElPanel* ElPanel1;
	Vcl::Stdctrls::TLabel* TimeLabel;
	Vcl::Extctrls::TImage* Image;
	Elhtmllbl::TElHTMLLabel* MessageLabel;
	Elcheckctl::TElCheckBox* ShowAgainCB;
	Elpopbtn::TElPopupButton* HelpBtn;
	Elpopbtn::TElPopupButton* OkBtn;
	Elpopbtn::TElPopupButton* IgnoreBtn;
	Elpopbtn::TElPopupButton* YesBtn;
	Elpopbtn::TElPopupButton* CancelBtn;
	Elpopbtn::TElPopupButton* NoBtn;
	Elpopbtn::TElPopupButton* NoToAllBtn;
	Elpopbtn::TElPopupButton* AbortBtn;
	Elpopbtn::TElPopupButton* RetryBtn;
	Elpopbtn::TElPopupButton* YesToAllBtn;
	Elfrmpers::TElFormPersist* ElFormPersist1;
	Elimglst::TElImageList* DisabledImages;
	Elimglst::TElImageList* EnabledImages;
	Elcaption::TElFormCaption* Captions;
	Elpopbtn::TElPopupButton* ElPopupButton1;
	void __fastcall TimerTimer(System::TObject* Sender);
	void __fastcall HelpBtnClick(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall FormClose(System::TObject* Sender, System::Uitypes::TCloseAction &Action);
	void __fastcall BtnClick(System::TObject* Sender);
	void __fastcall MessageLabelLinkClick(System::TObject* Sender, Elstrutils::TElFString HRef);
	void __fastcall MessageLabelImageNeeded(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	
private:
	bool FDisableOk;
	int FLeft;
	bool FShowTime;
	Elstrutils::TElFString FSaveDefText;
	Elstrutils::TElFString SecondsCaption;
	Elpopbtn::TElPopupButton* DefaultButton;
	bool Modal;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeed;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	System::Classes::TNotifyEvent FOnTimer;
	TPromptCloseEvent FOnClose;
	HIDESBASE MESSAGE void __fastcall WMSysCommand(Winapi::Messages::TMessage &Message);
	
public:
	void *CustomData;
public:
	/* TCustomForm.Create */ inline __fastcall virtual TElPromptForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TElPromptForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TElPromptForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElPromptForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElPromptDialog : public Vcl::Dialogs::TCommonDialog
{
	typedef Vcl::Dialogs::TCommonDialog inherited;
	
protected:
	bool FTopmost;
	Elunicodestrings::TElWideStringArray* FControlTexts;
	Elstrutils::TElFString FMessage;
	int FCaptionIdx;
	int FMessageIdx;
	System::Uitypes::TMsgDlgType FDlgType;
	Elunicodestrings::TElWideStringArray* FCaptions;
	Elunicodestrings::TElWideStringArray* FTexts;
	System::Uitypes::TMsgDlgButtons FButtons;
	System::Uitypes::TMsgDlgBtn FDefBtn;
	System::Uitypes::TMsgDlgBtn FCancelBtn;
	bool FShowGlyphs;
	int FTimeLimit;
	bool FShowOnceMore;
	bool FShowAgainChecked;
	bool FTimedShow;
	int FHelpCtx;
	Elstrutils::TElFString FShowAgainText;
	Elstrutils::TElFString FDlgCaption;
	bool FIsHTML;
	Vcl::Forms::TPosition FPosition;
	System::Classes::TNotifyEvent FOnBeforeShow;
	System::Classes::TNotifyEvent FOnTimer;
	TPromptCloseEvent FOnClose;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	TElPromptFormClass FClass;
	bool FParentFont;
	Vcl::Graphics::TFont* FFont;
	TElPromptForm* FForm;
	void __fastcall SetTexts(Elunicodestrings::TElWideStringArray* anArray);
	void __fastcall SetCaptions(Elunicodestrings::TElWideStringArray* anArray);
	void __fastcall SetControlTexts(Elunicodestrings::TElWideStringArray* newValue);
	TElPromptForm* __fastcall CreateWndx();
	void __fastcall SetParentFont(bool Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* Value);
	void __fastcall CloseTransfer(System::TObject* Sender, int Result);
	void __fastcall FontChange(System::TObject* Sender);
	
public:
	void *CustomData;
	__fastcall virtual TElPromptDialog(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElPromptDialog();
	int __fastcall ShowModal();
	void __fastcall Show();
	virtual bool __fastcall Execute()/* overload */;
	__property TElPromptFormClass FormClass = {read=FClass, write=FClass};
	
__published:
	__property bool DisableDefault = {read=FTimedShow, write=FTimedShow, nodefault};
	__property System::Uitypes::TMsgDlgBtn DefaultButton = {read=FDefBtn, write=FDefBtn, nodefault};
	__property System::Uitypes::TMsgDlgBtn CancelButton = {read=FCancelBtn, write=FCancelBtn, nodefault};
	__property int TimeDelay = {read=FTimeLimit, write=FTimeLimit, nodefault};
	__property bool ShowGlyphs = {read=FShowGlyphs, write=FShowGlyphs, nodefault};
	__property Elunicodestrings::TElWideStringArray* Texts = {read=FTexts, write=SetTexts};
	__property Elunicodestrings::TElWideStringArray* ControlTexts = {read=FControlTexts, write=SetControlTexts};
	__property Elstrutils::TElFString DialogCaption = {read=FDlgCaption, write=FDlgCaption};
	__property Elstrutils::TElFString Message = {read=FMessage, write=FMessage};
	__property int MessageIdx = {read=FMessageIdx, write=FMessageIdx, default=-1};
	__property System::Uitypes::TMsgDlgType DlgType = {read=FDlgType, write=FDlgType, nodefault};
	__property System::Uitypes::TMsgDlgButtons Buttons = {read=FButtons, write=FButtons, nodefault};
	__property bool ShowAgainCheck = {read=FShowOnceMore, write=FShowOnceMore, nodefault};
	__property bool ShowAgainChecked = {read=FShowAgainChecked, write=FShowAgainChecked, nodefault};
	__property Elstrutils::TElFString ShowAgainText = {read=FShowAgainText, write=FShowAgainText};
	__property Elunicodestrings::TElWideStringArray* Captions = {read=FCaptions, write=SetCaptions};
	__property int CaptionIdx = {read=FCaptionIdx, write=FCaptionIdx, nodefault};
	__property int HelpContext = {read=FHelpCtx, write=FHelpCtx, nodefault};
	__property bool IsHTML = {read=FIsHTML, write=FIsHTML, nodefault};
	__property bool TopMost = {read=FTopmost, write=FTopmost, nodefault};
	__property Vcl::Forms::TPosition Position = {read=FPosition, write=FPosition, nodefault};
	__property System::Classes::TNotifyEvent OnTimer = {read=FOnTimer, write=FOnTimer};
	__property TPromptCloseEvent OnClose = {read=FOnClose, write=FOnClose};
	__property System::Classes::TNotifyEvent OnBeforeShow = {read=FOnBeforeShow, write=FOnBeforeShow};
	__property Htmlrender::TElHTMLImageNeededEvent OnHTMLImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property bool ParentFont = {read=FParentFont, write=SetParentFont, default=1};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	/* Hoisted overloads: */
	
public:
	inline bool __fastcall  Execute(HWND ParentWnd){ return Vcl::Dialogs::TCommonDialog::Execute(ParentWnd); }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TElPromptForm* ElPromptForm;
extern DELPHI_PACKAGE System::ResourceString _SShowNextTime;
#define Elpromptdlg_SShowNextTime System::LoadResourceString(&Elpromptdlg::_SShowNextTime)
extern DELPHI_PACKAGE System::ResourceString _SDSecondsLeft;
#define Elpromptdlg_SDSecondsLeft System::LoadResourceString(&Elpromptdlg::_SDSecondsLeft)
extern DELPHI_PACKAGE System::Word __fastcall ElMessageDlg(const Elstrutils::TElFString Msg, System::Uitypes::TMsgDlgType DlgType, System::Uitypes::TMsgDlgButtons Buttons, int HelpCtx);
extern DELPHI_PACKAGE System::Word __fastcall ElMessageDlgEx2(const Elstrutils::TElFString Msg, System::Uitypes::TMsgDlgType DlgType, System::Uitypes::TMsgDlgButtons Buttons, int HelpCtx, bool IsHTML, Htmlrender::TElHTMLLinkClickEvent OnLinkClick);
extern DELPHI_PACKAGE System::Word __fastcall ElMessageDlgEx(const Elstrutils::TElFString Msg, System::Uitypes::TMsgDlgType DlgType, System::Uitypes::TMsgDlgButtons Buttons, int HelpCtx, TElPromptFormClass FormClass);
}	/* namespace Elpromptdlg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELPROMPTDLG)
using namespace Elpromptdlg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElpromptdlgHPP
