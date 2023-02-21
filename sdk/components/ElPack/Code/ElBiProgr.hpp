// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElBiProgr.pas' rev: 35.00 (Windows)

#ifndef ElbiprogrHPP
#define ElbiprogrHPP

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
#include <Vcl.ExtCtrls.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elbiprogr
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElBiProgressBar;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TProgrShowMode : unsigned char { psmAllFull, psmLightHalf, psmDarkHalf, psmAllHalf };

enum DECLSPEC_DENUM TElBevelStyle : unsigned char { ebsNone, ebsLowered, ebsRaised };

class PASCALIMPLEMENTATION TElBiProgressBar : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
private:
	bool FLightTextFullLine;
	bool FDarkTextFullLine;
	bool FLightButtonStyle;
	bool FDarkButtonStyle;
	int FMinValue;
	TElBevelStyle FBorderStyle;
	System::UnicodeString FDarkText;
	System::UnicodeString FLightText;
	System::Uitypes::TColor FDarkTextColor;
	System::Uitypes::TColor FLightTextColor;
	System::Uitypes::TColor FLightColor;
	System::Uitypes::TColor FDarkColor;
	int FScale;
	int FLightValue;
	int FDarkValue;
	Vcl::Graphics::TBitmap* Bitmap;
	bool FAdditive;
	TProgrShowMode FProgrShowMode;
	System::UnicodeString FCaption;
	void __fastcall SetLightColor(System::Uitypes::TColor aValue);
	void __fastcall SetDarkColor(System::Uitypes::TColor aValue);
	void __fastcall SetScale(int aValue);
	void __fastcall SetLightValue(int aValue);
	void __fastcall SetDarkValue(int aValue);
	void __fastcall SetAdditive(bool aValue);
	void __fastcall SetProgrShowMode(TProgrShowMode aValue);
	void __fastcall SetDarkText(System::UnicodeString newValue);
	void __fastcall SetLightText(System::UnicodeString newValue);
	void __fastcall SetCaption(System::UnicodeString newValue);
	void __fastcall SetDarkTextColor(System::Uitypes::TColor newValue);
	void __fastcall SetLightTextColor(System::Uitypes::TColor newValue);
	void __fastcall SetBorderStyle(TElBevelStyle newValue);
	void __fastcall SetMinValue(int newValue);
	void __fastcall SetLightButtonStyle(bool newValue);
	void __fastcall SetDarkButtonStyle(bool newValue);
	void __fastcall SetTransparent(bool newValue);
	void __fastcall SetLightTextFullLine(bool newValue);
	void __fastcall SetDarkTextFullLine(bool newValue);
	bool __fastcall GetTransparent();
	
protected:
	virtual void __fastcall Paint();
	
public:
	__fastcall virtual TElBiProgressBar(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElBiProgressBar();
	void __fastcall SetValues(int ALightValue, int ADarkValue, int AScale, bool AAdditive);
	
__published:
	__property System::Uitypes::TColor LightColor = {read=FLightColor, write=SetLightColor, default=255};
	__property System::Uitypes::TColor DarkColor = {read=FDarkColor, write=SetDarkColor, default=128};
	__property int Scale = {read=FScale, write=SetScale, default=100};
	__property int LightValue = {read=FLightValue, write=SetLightValue, nodefault};
	__property int DarkValue = {read=FDarkValue, write=SetDarkValue, nodefault};
	__property bool Additive = {read=FAdditive, write=SetAdditive, nodefault};
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property TProgrShowMode ProgressShowMode = {read=FProgrShowMode, write=SetProgrShowMode, default=0};
	__property bool LightTextFullLine = {read=FLightTextFullLine, write=SetLightTextFullLine, nodefault};
	__property bool DarkTextFullLine = {read=FDarkTextFullLine, write=SetDarkTextFullLine, nodefault};
	__property System::UnicodeString DarkText = {read=FDarkText, write=SetDarkText};
	__property System::UnicodeString LightText = {read=FLightText, write=SetLightText};
	__property System::Uitypes::TColor DarkTextColor = {read=FDarkTextColor, write=SetDarkTextColor, nodefault};
	__property System::Uitypes::TColor LightTextColor = {read=FLightTextColor, write=SetLightTextColor, nodefault};
	__property TElBevelStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, nodefault};
	__property int MinValue = {read=FMinValue, write=SetMinValue, default=0};
	__property bool LightButtonStyle = {read=FLightButtonStyle, write=SetLightButtonStyle, nodefault};
	__property bool DarkButtonStyle = {read=FDarkButtonStyle, write=SetDarkButtonStyle, nodefault};
	__property bool Transparent = {read=GetTransparent, write=SetTransparent, nodefault};
	__property Align = {default=0};
	__property Color = {default=-16777211};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=0};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDrag;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elbiprogr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELBIPROGR)
using namespace Elbiprogr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElbiprogrHPP
