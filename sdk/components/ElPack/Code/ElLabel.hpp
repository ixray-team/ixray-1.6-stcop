// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElLabel.pas' rev: 35.00 (Windows)

#ifndef EllabelHPP
#define EllabelHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <ElCLabel.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ellabel
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElLabel;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElLabelTextStyle : unsigned char { ltsNormal, ltsEmbossed, ltsRecessed, ltsOutlined };

enum DECLSPEC_DENUM TElLabelEffectStyle : unsigned char { lesNone, lesShadow, lesExtrude };

enum DECLSPEC_DENUM TElLabelExtrudePosition : unsigned char { lepLeft, lepLeftTop, lepTop, lepRightTop, lepRight, lepRightBottom, lepBottom, lepLeftBottom };

class PASCALIMPLEMENTATION TElLabel : public Elclabel::TElCustomLabel
{
	typedef Elclabel::TElCustomLabel inherited;
	
private:
	int FAngle;
	Vcl::Graphics::TBitmap* FBuffer;
	System::Uitypes::TColor FDarkColor;
	int FDepth;
	System::Uitypes::TColor FDisabledDarkColor;
	System::Uitypes::TColor FDisabledLightColor;
	TElLabelEffectStyle FEffect;
	System::Uitypes::TColor FFarColor;
	System::Uitypes::TColor FLightColor;
	System::Uitypes::TColor FNearColor;
	System::Uitypes::TColor FOutlineColor;
	TElLabelExtrudePosition FPosition;
	System::Uitypes::TColor FShadowColor;
	bool FStriated;
	TElLabelTextStyle FStyle;
	int FXOffset;
	int FYOffset;
	void __fastcall SetAngle(int Value);
	void __fastcall SetDarkColor(const System::Uitypes::TColor Value);
	void __fastcall SetDepth(const int Value);
	void __fastcall SetDisabledDarkColor(const System::Uitypes::TColor Value);
	void __fastcall SetDisabledLightColor(const System::Uitypes::TColor Value);
	void __fastcall SetEffect(const TElLabelEffectStyle Value);
	void __fastcall SetExtrudePosition(const TElLabelExtrudePosition Value);
	void __fastcall SetFarColor(const System::Uitypes::TColor Value);
	void __fastcall SetLightColor(const System::Uitypes::TColor Value);
	void __fastcall SetNearColor(const System::Uitypes::TColor Value);
	void __fastcall SetOutlineColor(const System::Uitypes::TColor Value);
	void __fastcall SetShadowColor(const System::Uitypes::TColor Value);
	void __fastcall SetStriated(const bool Value);
	void __fastcall SetStyle(const TElLabelTextStyle Value);
	void __fastcall SetXOffset(const int Value);
	void __fastcall SetYOffset(const int Value);
	
protected:
	Elstrutils::TElFString FCaption;
	System::WideString FHint;
	DYNAMIC void __fastcall AdjustBounds();
	HIDESBASE void __fastcall DoDrawText(System::Types::TRect &Rect, int Flags);
	virtual void __fastcall DrawDisabledText(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect, Elstrutils::TElFString Text, int Flags);
	DYNAMIC void __fastcall DrawEffect(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect, int Flags);
	void __fastcall DrawExtrusion(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect, Elstrutils::TElFString Text, int Flags, System::Uitypes::TColor NearColor, System::Uitypes::TColor FarColor);
	void __fastcall DrawNormalText(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect, Elstrutils::TElFString Text, int Flags);
	void __fastcall DrawOutlinedText(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect, Elstrutils::TElFString Text, int Flags, System::Uitypes::TColor OutlineColor);
	void __fastcall DrawRaisedText(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect, Elstrutils::TElFString Text, int Flags, System::Uitypes::TColor LeftTop, System::Uitypes::TColor RightBottom);
	void __fastcall DrawShadow(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect, Elstrutils::TElFString Text, int Flags, int X, int Y, System::Uitypes::TColor ShadowColor);
	DYNAMIC void __fastcall DrawText(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect, int Flags);
	virtual void __fastcall Paint();
	virtual void __fastcall SetCaption(Elstrutils::TElFString newValue);
	virtual void __fastcall SetAutoSize(bool newValue);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	virtual void __fastcall SetName(const System::Classes::TComponentName Value);
	__property int Angle = {read=FAngle, write=SetAngle, default=0};
	
public:
	__fastcall virtual TElLabel(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElLabel();
	
__published:
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property System::Uitypes::TColor DarkColor = {read=FDarkColor, write=SetDarkColor, default=-16777200};
	__property int Depth = {read=FDepth, write=SetDepth, default=10};
	__property System::Uitypes::TColor DisabledDarkColor = {read=FDisabledDarkColor, write=SetDisabledDarkColor, default=-16777200};
	__property System::Uitypes::TColor DisabledLightColor = {read=FDisabledLightColor, write=SetDisabledLightColor, default=-16777196};
	__property TElLabelEffectStyle Effect = {read=FEffect, write=SetEffect, default=0};
	__property TElLabelExtrudePosition ExtrudePosition = {read=FPosition, write=SetExtrudePosition, default=5};
	__property System::Uitypes::TColor FarColor = {read=FFarColor, write=SetFarColor, default=0};
	__property System::Uitypes::TColor LightColor = {read=FLightColor, write=SetLightColor, default=-16777196};
	__property System::Uitypes::TColor NearColor = {read=FNearColor, write=SetNearColor, default=0};
	__property System::Uitypes::TColor OutlineColor = {read=FOutlineColor, write=SetOutlineColor, default=16777215};
	__property System::Uitypes::TColor ShadowColor = {read=FShadowColor, write=SetShadowColor, default=-16777200};
	__property bool Striated = {read=FStriated, write=SetStriated, default=0};
	__property TElLabelTextStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property int XOffset = {read=FXOffset, write=SetXOffset, default=2};
	__property int YOffset = {read=FYOffset, write=SetYOffset, default=2};
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property Transparent = {default=1};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ellabel */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELLABEL)
using namespace Ellabel;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EllabelHPP
