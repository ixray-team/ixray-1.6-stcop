// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElGauge.pas' rev: 35.00 (Windows)

#ifndef ElgaugeHPP
#define ElgaugeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <ElImgFrm.hpp>
#include <ElCGControl.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elgauge
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElGauge;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElGaugeKind : unsigned char { egkNeedle, egkPie };

class PASCALIMPLEMENTATION TElGauge : public Elcgcontrol::TElCustomGraphicControl
{
	typedef Elcgcontrol::TElCustomGraphicControl inherited;
	
private:
	bool FTransparent;
	TElGaugeKind FGaugeKind;
	System::UnicodeString FText;
	int FValue;
	int FMinValue;
	int FMaxValue;
	bool FShowPoints;
	int FPoints;
	System::Uitypes::TColor FBackColor;
	System::Uitypes::TColor FForeColor;
	int FCriticalValue;
	System::Uitypes::TColor FCriticalColor;
	Vcl::Graphics::TBitmap* Bitmap;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall SetValue(int newValue);
	void __fastcall SetMinValue(int newValue);
	void __fastcall SetMaxValue(int newValue);
	void __fastcall SetShowPoints(bool newValue);
	void __fastcall SetPoints(int newValue);
	void __fastcall SetBackColor(System::Uitypes::TColor newValue);
	void __fastcall SetForeColor(System::Uitypes::TColor newValue);
	void __fastcall SetCriticalValue(int newValue);
	void __fastcall SetCriticalColor(System::Uitypes::TColor newValue);
	void __fastcall SetGaugeKind(TElGaugeKind newValue);
	void __fastcall SetTransparent(bool newValue);
	
protected:
	HIDESBASE void __fastcall SetText(System::UnicodeString Value);
	virtual void __fastcall Paint();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	
public:
	__fastcall virtual TElGauge(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElGauge();
	
__published:
	__property int Value = {read=FValue, write=SetValue, default=0};
	__property int MinValue = {read=FMinValue, write=SetMinValue, default=0};
	__property int MaxValue = {read=FMaxValue, write=SetMaxValue, default=100};
	__property bool ShowPoints = {read=FShowPoints, write=SetShowPoints, default=1};
	__property int Points = {read=FPoints, write=SetPoints, default=11};
	__property System::Uitypes::TColor BackColor = {read=FBackColor, write=SetBackColor, default=-16777196};
	__property System::Uitypes::TColor ForeColor = {read=FForeColor, write=SetForeColor, default=-16777198};
	__property int CriticalValue = {read=FCriticalValue, write=SetCriticalValue, nodefault};
	__property System::Uitypes::TColor CriticalColor = {read=FCriticalColor, write=SetCriticalColor, nodefault};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property Align = {default=0};
	__property Color;
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Visible = {default=1};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property TElGaugeKind GaugeKind = {read=FGaugeKind, write=SetGaugeKind, nodefault};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elgauge */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELGAUGE)
using namespace Elgauge;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElgaugeHPP
