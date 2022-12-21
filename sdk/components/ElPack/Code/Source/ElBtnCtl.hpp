// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElBtnCtl.pas' rev: 34.00 (Windows)

#ifndef ElbtnctlHPP
#define ElbtnctlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElXPThemedControl.hpp>
#include <ElTools.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elbtnctl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElButtonControl;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElButtonControl : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
private:
	HIDESBASE bool __fastcall IsCaptionStored();
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Winapi::Messages::TWMKey &Message);
	
protected:
	bool FTransparent;
	Elvclutils::TElTextDrawType FTextDrawType;
	bool F2000DrawFocus;
	bool F2000DrawAccel;
	HIDESBASE MESSAGE void __fastcall WMMove(Winapi::Messages::TWMMove &Msg);
	void __fastcall SetTextDrawType(Elvclutils::TElTextDrawType newValue);
	bool ClicksDisabled;
	Elstrutils::TElFString FCaption;
	System::WideString FHint;
	bool FMoneyFlat;
	System::Uitypes::TColor FMoneyFlatActiveColor;
	System::Uitypes::TColor FMoneyFlatInactiveColor;
	System::Uitypes::TColor FMoneyFlatDownColor;
	virtual bool __fastcall GetChecked() = 0 ;
	virtual void __fastcall SetChecked(bool newValue) = 0 ;
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall SetTransparent(bool newValue);
	__property bool Checked = {read=GetChecked, write=SetChecked, default=0};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property Elvclutils::TElTextDrawType TextDrawType = {read=FTextDrawType, write=SetTextDrawType, default=0};
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	void __fastcall SetCaption(Elstrutils::TElFString Value);
	virtual System::WideString __fastcall GetThemedClassName();
	bool __fastcall GetUIStateDrawFocus();
	bool __fastcall GetUIStateDrawAccel();
	MESSAGE void __fastcall WMUpdateUIState(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMChangeUIState(Winapi::Messages::TMessage &Message);
	void __fastcall SetMoneyFlat(bool Value);
	void __fastcall SetMoneyFlatActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetMoneyFlatInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetMoneyFlatDownColor(System::Uitypes::TColor Value);
	bool __fastcall GetMoneyFlat();
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	__property Color;
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption, stored=IsCaptionStored};
	__property bool MoneyFlat = {read=GetMoneyFlat, write=SetMoneyFlat, default=0};
	__property System::Uitypes::TColor MoneyFlatActiveColor = {read=FMoneyFlatActiveColor, write=SetMoneyFlatActiveColor, stored=GetMoneyFlat, nodefault};
	__property System::Uitypes::TColor MoneyFlatInactiveColor = {read=FMoneyFlatInactiveColor, write=SetMoneyFlatInactiveColor, stored=GetMoneyFlat, nodefault};
	__property System::Uitypes::TColor MoneyFlatDownColor = {read=FMoneyFlatDownColor, write=SetMoneyFlatDownColor, stored=GetMoneyFlat, nodefault};
	
public:
	__fastcall virtual TElButtonControl(System::Classes::TComponent* Owner);
	__property bool UIStateDrawFocus = {read=GetUIStateDrawFocus, nodefault};
	__property bool UIStateDrawAccel = {read=GetUIStateDrawAccel, nodefault};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TElButtonControl() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElButtonControl(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elbtnctl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELBTNCTL)
using namespace Elbtnctl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElbtnctlHPP
