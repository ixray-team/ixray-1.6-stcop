// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElSpinBtn.pas' rev: 34.00 (Windows)

#ifndef ElspinbtnHPP
#define ElspinbtnHPP

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
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElACtrls.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElVCLUtils.hpp>
#include <ElXPThemedControl.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElTools.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elspinbtn
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElSpinButton;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TElSpinEvent)(System::TObject* Sender, double Distance);

typedef void __fastcall (__closure *TElSpinStartEvent)(System::TObject* Sender, double &InitialDistance);

enum DECLSPEC_DENUM TElSpinBtnDir : unsigned char { sbdUpDown, sbdLeftRight };

enum DECLSPEC_DENUM TElSpinBtnType : unsigned char { sbtUpDown, sbtLeftRight };

class PASCALIMPLEMENTATION TElSpinButton : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
private:
	bool FFlat;
	bool FUseDrag;
	TElSpinEvent FOnUpClick;
	TElSpinEvent FOnDownClick;
	TElSpinEvent FOnSpinDrag;
	TElSpinStartEvent FOnSpinStart;
	bool FMouseInUpPart;
	bool FMouseInDownPart;
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMContextMenu(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	
protected:
	bool FTopBtnDown;
	bool FBottomBtnDown;
	Vcl::Extctrls::TTimer* FTimer;
	System::Uitypes::TCursor SaveCursor;
	bool FCaptured;
	bool FDragging;
	double FStartValue;
	double FValue;
	double FIncrement;
	TElSpinBtnDir FButtonDirection;
	TElSpinBtnType FButtonType;
	bool FUpArrowEnabled;
	bool FDownArrowEnabled;
	bool FMoneyFlat;
	System::Uitypes::TColor FMoneyFlatActiveColor;
	System::Uitypes::TColor FMoneyFlatDownColor;
	System::Uitypes::TColor FMoneyFlatInactiveColor;
	bool FOldStyled;
	System::WideString FHint;
	void __fastcall IntMouseMove(short XPos, short YPos);
	virtual void __fastcall Paint();
	void __fastcall OnTimer(System::TObject* Sender);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Msg);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Msg);
	DYNAMIC Vcl::Menus::TPopupMenu* __fastcall GetPopupMenu();
	virtual void __fastcall SetUseDrag(bool newValue);
	virtual void __fastcall SetFlat(bool newValue);
	void __fastcall SetButtonDirection(TElSpinBtnDir Value);
	void __fastcall SetButtonType(TElSpinBtnType Value);
	virtual System::WideString __fastcall GetThemedClassName();
	void __fastcall SetUpArrowEnabled(bool Value);
	void __fastcall SetDownArrowEnabled(bool Value);
	void __fastcall SetMoneyFlat(bool Value);
	void __fastcall SetMoneyFlatActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetMoneyFlatDownColor(System::Uitypes::TColor Value);
	void __fastcall SetMoneyFlatInactiveColor(System::Uitypes::TColor Value);
	bool __fastcall GetMoneyFlat();
	void __fastcall SetOldStyled(bool Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	
public:
	__fastcall virtual TElSpinButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElSpinButton();
	void __fastcall StopDragging();
	__property bool SpinDragging = {read=FDragging, nodefault};
	
__published:
	__property double Increment = {read=FIncrement, write=FIncrement};
	__property bool UseDrag = {read=FUseDrag, write=SetUseDrag, default=1};
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property TElSpinBtnDir ButtonDirection = {read=FButtonDirection, write=SetButtonDirection, default=0};
	__property TElSpinBtnType ButtonType = {read=FButtonType, write=SetButtonType, nodefault};
	__property bool UpArrowEnabled = {read=FUpArrowEnabled, write=SetUpArrowEnabled, default=1};
	__property bool DownArrowEnabled = {read=FDownArrowEnabled, write=SetDownArrowEnabled, default=1};
	__property bool MoneyFlat = {read=GetMoneyFlat, write=SetMoneyFlat, default=0};
	__property System::Uitypes::TColor MoneyFlatActiveColor = {read=FMoneyFlatActiveColor, write=SetMoneyFlatActiveColor, stored=GetMoneyFlat, nodefault};
	__property System::Uitypes::TColor MoneyFlatDownColor = {read=FMoneyFlatDownColor, write=SetMoneyFlatDownColor, stored=GetMoneyFlat, nodefault};
	__property System::Uitypes::TColor MoneyFlatInactiveColor = {read=FMoneyFlatInactiveColor, write=SetMoneyFlatInactiveColor, stored=GetMoneyFlat, nodefault};
	__property bool OldStyled = {read=FOldStyled, write=SetOldStyled, default=0};
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property TElSpinEvent OnUpClick = {read=FOnUpClick, write=FOnUpClick};
	__property TElSpinEvent OnDownClick = {read=FOnDownClick, write=FOnDownClick};
	__property TElSpinEvent OnSpinDrag = {read=FOnSpinDrag, write=FOnSpinDrag};
	__property TElSpinStartEvent OnSpinStart = {read=FOnSpinStart, write=FOnSpinStart};
	__property Caption = {default=0};
	__property Enabled = {default=1};
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property PopupMenu;
	__property Color = {default=-16777211};
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnKeyPress;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnStartDrag;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElSpinButton(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elspinbtn */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSPINBTN)
using namespace Elspinbtn;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElspinbtnHPP
