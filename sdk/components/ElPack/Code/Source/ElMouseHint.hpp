// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElMouseHint.pas' rev: 34.00 (Windows)

#ifndef ElmousehintHPP
#define ElmousehintHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Controls.hpp>
#include <System.Classes.hpp>
#include <ElStrUtils.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElVCLUtils.hpp>
#include <HTMLRender.hpp>
#include <ElHintWnd.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elmousehint
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElMouseHint;
//-- type declarations -------------------------------------------------------
typedef Elunicodestrings::TElWideStrings TElFStrings;

typedef Elunicodestrings::TElWideStringList TElFStringList;

enum DECLSPEC_DENUM TElMouseHintLocation : unsigned char { hlLeftTop, hlLeftCenter, hlLeftBottom, hlRightTop, hlRightCenter, hlRightBottom, hlCenterTop, hlCenterBottom };

typedef System::Set<TElMouseHintLocation, TElMouseHintLocation::hlLeftTop, TElMouseHintLocation::hlCenterBottom> TElMousehintLocations;

class PASCALIMPLEMENTATION TElMouseHint : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Elhintwnd::TElHintWindow* FHintWindow;
	
protected:
	bool FActive;
	bool FAutoSize;
	bool FIsHTML;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	bool FUseSystemHintSettings;
	Elunicodestrings::TElWideStrings* FLines;
	bool FWordWrap;
	System::Uitypes::TColor FColor;
	int FWidth;
	int FHeight;
	TElMouseHintLocation FLocation;
	Vcl::Graphics::TFont* FFont;
	Vcl::Controls::TControl* FBoundingControl;
	bool FVisible;
	int FDistanceToHint;
	bool FKeepWithinWindow;
	int FHideCount;
	TElMousehintLocations FLocations;
	bool FAutoAdjustLocation;
	void __fastcall SetActive(bool Value);
	void __fastcall SetAutoSize(bool Value);
	void __fastcall SetCaption(Elstrutils::TElFString Value);
	void __fastcall SetIsHTML(bool Value);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	void __fastcall SetLines(Elunicodestrings::TElWideStrings* Value);
	void __fastcall LinesChange(System::TObject* Sender);
	Elstrutils::TElFString __fastcall GetCaption();
	void __fastcall BuildHint();
	void __fastcall SetWordWrap(bool Value);
	void __fastcall SetColor(System::Uitypes::TColor Value);
	bool __fastcall GetVisible();
	void __fastcall SetWidth(int Value);
	void __fastcall SetHeight(int Value);
	void __fastcall SetLocation(TElMouseHintLocation Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* Value);
	void __fastcall FontChange(System::TObject* Sender);
	void __fastcall SetBoundingControl(Vcl::Controls::TControl* Value);
	Vcl::Controls::TControl* __fastcall GetBoundingControl();
	void __fastcall UpdateHintPos(const System::Types::TPoint &MousePos, Vcl::Controls::TControl* Control);
	System::Types::TRect __fastcall DoUpdatePos(const System::Types::TPoint &MousePos, TElMouseHintLocation Location);
	void __fastcall SetDistanceToHint(int Value);
	void __fastcall SetUseSystemHintSettings(bool Value);
	void __fastcall DoShowHintWindow();
	void __fastcall DoHideHintWindow();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	void __fastcall SetKeepWithinWindow(bool Value);
	void __fastcall SetAutoAdjustLocation(bool Value);
	__property TElMousehintLocations Locations = {read=FLocations, write=FLocations, nodefault};
	
public:
	__fastcall virtual TElMouseHint(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElMouseHint();
	void __fastcall ShowHint();
	void __fastcall HideHint();
	virtual void __fastcall Loaded();
	void __fastcall ShowLines(Elunicodestrings::TElWideStrings* Lines);
	void __fastcall ShowString(Elstrutils::TElFString Caption);
	__property bool Visible = {read=GetVisible, nodefault};
	
__published:
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=1};
	__property Elstrutils::TElFString Caption = {read=GetCaption, write=SetCaption};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, nodefault};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property bool UseSystemHintSettings = {read=FUseSystemHintSettings, write=SetUseSystemHintSettings, default=1};
	__property Elunicodestrings::TElWideStrings* Lines = {read=FLines, write=SetLines};
	__property bool WordWrap = {read=FWordWrap, write=SetWordWrap, default=0};
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property Vcl::Controls::TControl* BoundingControl = {read=FBoundingControl, write=SetBoundingControl};
	__property int DistanceToHint = {read=FDistanceToHint, write=SetDistanceToHint, default=2};
	__property bool KeepWithinWindow = {read=FKeepWithinWindow, write=SetKeepWithinWindow, default=0};
	__property bool AutoAdjustLocation = {read=FAutoAdjustLocation, write=SetAutoAdjustLocation, default=0};
	__property TElMouseHintLocation Location = {read=FLocation, write=SetLocation, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elmousehint */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELMOUSEHINT)
using namespace Elmousehint;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElmousehintHPP
