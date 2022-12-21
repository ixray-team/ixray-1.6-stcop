// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHintWnd.pas' rev: 34.00 (Windows)

#ifndef ElhintwndHPP
#define ElhintwndHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <HTMLRender.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhintwnd
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHintWindow;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElHintWindow : public Vcl::Controls::THintWindow
{
	typedef Vcl::Controls::THintWindow inherited;
	
protected:
	Vcl::Graphics::TFont* FFont;
	bool FActivating;
	System::WideString FWideCaption;
	Htmlrender::TElHTMLRender* FRender;
	bool FIsHTML;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	HIDESBASE void __fastcall SetFont(Vcl::Graphics::TFont* newValue);
	bool FWordWrap;
	virtual void __fastcall TriggerLinkClickEvent(Elstrutils::TElFString HRef);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	void __fastcall SetWordWrap(bool Value);
	
public:
	__fastcall virtual TElHintWindow(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElHintWindow();
	virtual void __fastcall Paint();
	virtual System::Types::TRect __fastcall CalcHintRect(int MaxWidth, const System::UnicodeString AHint, void * AData);
	System::Types::TRect __fastcall CalcHintRectW(int MaxWidth, const System::WideString AHint, void * AData);
	virtual void __fastcall ActivateHintW(const System::Types::TRect &Rect, const System::WideString AHint);
	__property Canvas;
	
__published:
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property bool WordWrap = {read=FWordWrap, write=SetWordWrap, default=0};
	__property System::WideString WideCaption = {read=FWideCaption, write=FWideCaption};
	__property bool IsHTML = {read=FIsHTML, write=FIsHTML, nodefault};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHintWindow(HWND ParentWindow) : Vcl::Controls::THintWindow(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::WideString __fastcall GetUnicodeHint(System::UnicodeString Hint);
}	/* namespace Elhintwnd */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHINTWND)
using namespace Elhintwnd;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhintwndHPP
