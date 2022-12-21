// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTrayInfo.pas' rev: 34.00 (Windows)

#ifndef EltrayinfoHPP
#define EltrayinfoHPP

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
#include <ElTools.hpp>
#include <ElVCLUtils.hpp>
#include <ElHTMLLbl.hpp>
#include <ElFrmPers.hpp>
#include <ElStrUtils.hpp>
#include <HTMLRender.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltrayinfo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTrayInfoForm;
class DELPHICLASS TElTrayInfo;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElTrayInfoType : unsigned char { titInformation, titWarning, titError };

class PASCALIMPLEMENTATION TTrayInfoForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Extctrls::TImage* Image1;
	Vcl::Extctrls::TTimer* Timer;
	Elhtmllbl::TElHTMLLabel* InfoLabel;
	Elfrmpers::TElFormPersist* ElFormPersist1;
	void __fastcall TimerTimer(System::TObject* Sender);
	void __fastcall ClickHandler(System::TObject* Sender);
	void __fastcall DblClickHandler(System::TObject* Sender);
	void __fastcall ShowHandler(System::TObject* Sender);
	void __fastcall HideHandler(System::TObject* Sender);
	
private:
	HIDESBASE MESSAGE void __fastcall WMMouseActivate(Winapi::Messages::TMessage &Msg);
	
protected:
	System::Classes::TNotifyEvent FOnShow;
	System::Classes::TNotifyEvent FOnHide;
	System::Classes::TNotifyEvent FOnClick;
	System::Classes::TNotifyEvent FOnDblClick;
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTrayInfoForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTrayInfoForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTrayInfoForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTrayInfoForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElTrayInfo : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FIsHTML;
	TTrayInfoForm* FInfoForm;
	unsigned FShowTime;
	TElTrayInfoType FInfoType;
	Elstrutils::TElFString FMessage;
	System::Uitypes::TColor FColor;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	System::Classes::TNotifyEvent FOnShow;
	System::Classes::TNotifyEvent FOnHide;
	System::Classes::TNotifyEvent FOnClick;
	System::Classes::TNotifyEvent FOnDblClick;
	
protected:
	virtual void __fastcall SetShowTime(unsigned newValue);
	virtual void __fastcall SetInfoType(TElTrayInfoType newValue);
	virtual void __fastcall SetMessage(Elstrutils::TElFString newValue);
	void __fastcall AdjustFormIcon();
	void __fastcall AdjustFormSize(int X, int Y);
	int __fastcall SuggestedHeight();
	int __fastcall SuggestedWidth();
	virtual void __fastcall SetIsHTML(bool newValue);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ClickHandler(System::TObject* Sender);
	void __fastcall DblClickHandler(System::TObject* Sender);
	void __fastcall ShowHandler(System::TObject* Sender);
	void __fastcall HideHandler(System::TObject* Sender);
	void __fastcall SetColor(System::Uitypes::TColor Value);
	
public:
	void __fastcall Show();
	void __fastcall Hide();
	__fastcall virtual TElTrayInfo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTrayInfo();
	__property TTrayInfoForm* InfoForm = {read=FInfoForm};
	
__published:
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, default=-16777192};
	__property unsigned ShowTime = {read=FShowTime, write=SetShowTime, nodefault};
	__property TElTrayInfoType InfoType = {read=FInfoType, write=SetInfoType, nodefault};
	__property Elstrutils::TElFString Message = {read=FMessage, write=SetMessage};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, nodefault};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property System::Classes::TNotifyEvent OnShow = {read=FOnShow, write=FOnShow};
	__property System::Classes::TNotifyEvent OnHide = {read=FOnHide, write=FOnHide};
	__property System::Classes::TNotifyEvent OnClick = {read=FOnClick, write=FOnClick};
	__property System::Classes::TNotifyEvent OnDblClick = {read=FOnDblClick, write=FOnDblClick};
};


typedef System::TMetaClass* TTrayInfoFormClass;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TTrayInfoFormClass TrayInfoFormClass;
}	/* namespace Eltrayinfo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTRAYINFO)
using namespace Eltrayinfo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltrayinfoHPP
