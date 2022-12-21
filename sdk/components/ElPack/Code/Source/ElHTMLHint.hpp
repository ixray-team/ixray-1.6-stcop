// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHTMLHint.pas' rev: 34.00 (Windows)

#ifndef ElhtmlhintHPP
#define ElhtmlhintHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElHintWnd.hpp>
#include <HTMLRender.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhtmlhint
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHTMLHint;
class DELPHICLASS TElHTMLHintWindow;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElHTMLHint : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FEnabled;
	Vcl::Controls::THintWindowClass FHintClass;
	System::Classes::TNotifyEvent FOnShow;
	System::Classes::TNotifyEvent FOnHide;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	
protected:
	System::Uitypes::TFontName FFontName;
	virtual void __fastcall SetEnabled(bool Value);
	void __fastcall SetOnHide(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnShow(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnImageNeeded(Htmlrender::TElHTMLImageNeededEvent Value);
	void __fastcall SetFontName(const System::Uitypes::TFontName Value);
	
public:
	__fastcall virtual ~TElHTMLHint();
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property System::Classes::TNotifyEvent OnShow = {read=FOnShow, write=SetOnShow};
	__property System::Classes::TNotifyEvent OnHide = {read=FOnHide, write=SetOnHide};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=SetOnImageNeeded};
	__property System::Uitypes::TFontName FontName = {read=FFontName, write=SetFontName};
public:
	/* TComponent.Create */ inline __fastcall virtual TElHTMLHint(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	
};


class PASCALIMPLEMENTATION TElHTMLHintWindow : public Elhintwnd::TElHintWindow
{
	typedef Elhintwnd::TElHintWindow inherited;
	
protected:
	void __fastcall OnShow();
	void __fastcall OnHide();
	MESSAGE void __fastcall WMShowWindow(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	void __fastcall SetupRightCaption(System::UnicodeString Caption);
	
public:
	__fastcall virtual TElHTMLHintWindow(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElHTMLHintWindow();
	virtual System::Types::TRect __fastcall CalcHintRect(int MaxWidth, const System::UnicodeString AHint, void * AData);
	virtual void __fastcall ActivateHint(const System::Types::TRect &Rect, const System::UnicodeString AHint);
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHTMLHintWindow(HWND ParentWindow) : Elhintwnd::TElHintWindow(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elhtmlhint */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHTMLHINT)
using namespace Elhtmlhint;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhtmlhintHPP
