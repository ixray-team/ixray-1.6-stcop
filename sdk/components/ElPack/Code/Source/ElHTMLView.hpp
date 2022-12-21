// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHTMLView.pas' rev: 34.00 (Windows)

#ifndef ElhtmlviewHPP
#define ElhtmlviewHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <HTMLRender.hpp>
#include <ElScrollBar.hpp>
#include <ElImgFrm.hpp>
#include <ElXPThemedControl.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElStrUtils.hpp>
#include <ElVCLUtils.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhtmlview
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHTMLView;
//-- type declarations -------------------------------------------------------
using Elstrutils::TElFString;

class PASCALIMPLEMENTATION TElHTMLView : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
private:
	bool FFlatFocusedScrollBars;
	bool FUseCustomScrollBars;
	Vcl::Menus::TPopupMenu* FLinkPopupMenu;
	System::Uitypes::TCursor FCursor;
	System::Uitypes::TColor FHighlightColor;
	System::Uitypes::TColor FHighlightBkColor;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	System::Uitypes::TColor FLinkColor;
	System::Uitypes::TFontStyles FLinkStyle;
	Elimgfrm::TElImageForm* FImgForm;
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FFlat;
	bool FWordWrap;
	bool FMouseOver;
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	Htmlrender::TElHTMLRender* FRender;
	HDC TmpDC;
	int FGradientSteps;
	System::Uitypes::TColor FGradientStartColor;
	System::Uitypes::TColor FGradientEndColor;
	Vcl::Graphics::TBitmap* FTmpBmp;
	bool FTransparent;
	Vcl::Graphics::TBitmap* FBackground;
	Elvclutils::TElBkGndType FBackgroundType;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	System::UnicodeString FDummyString;
	System::Types::TPoint FViewPos;
	System::Types::TRect FTextRect;
	int FScrollStep;
	Elscrollbar::TElScrollBar* FVertScrollBar;
	Elscrollbar::TElScrollBar* FHorzScrollBar;
	bool FVScrollVisible;
	bool FHScrollVisible;
	Elscrollbar::TElScrollBarStyles* FVertScrollBarStyles;
	Elscrollbar::TElScrollBarStyles* FHorzScrollBarStyles;
	Elvclutils::TElBorderSides FBorderSides;
	System::WideString FHint;
	HIDESBASE MESSAGE void __fastcall WMSysColorChange(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Msg);
	void __fastcall SBChanged(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall SetTransparent(bool newValue);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetBackgroundType(Elvclutils::TElBkGndType newValue);
	void __fastcall ImageChange(System::TObject* Sender);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetGradientStartColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientEndColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientSteps(int newValue);
	void __fastcall RedoTmpBmp();
	void __fastcall DrawFlatBorder(bool HorzTracking, bool VertTracking);
	void __fastcall DrawFlatBorderEx(HDC DC, bool HorzTracking, bool VertTracking);
	void __fastcall OnHScroll(System::TObject* Sender, Elscrollbar::TElScrollCode ScrollCode, int &ScrollPos, bool &DoChange);
	void __fastcall OnVScroll(System::TObject* Sender, Elscrollbar::TElScrollCode ScrollCode, int &ScrollPos, bool &DoChange);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Msg);
	void __fastcall SetLinkPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	
protected:
	Elstrutils::TElFString FCaption;
	bool FUseXPThemes;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	System::Types::TRect FViewRect;
	int FMargin;
	virtual void __fastcall SetVertScrollBarStyles(Elscrollbar::TElScrollBarStyles* newValue);
	virtual void __fastcall SetHorzScrollBarStyles(Elscrollbar::TElScrollBarStyles* newValue);
	virtual void __fastcall PrepareText();
	virtual void __fastcall SetViewPos(const System::Types::TPoint &newValue);
	virtual void __fastcall SetWordWrap(bool newValue);
	void __fastcall AdjustScrollBars();
	virtual void __fastcall Paint();
	void __fastcall UpdateFrame();
	virtual void __fastcall SetActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	virtual void __fastcall SetInactiveBorderType(Elvclutils::TElFlatBorderType newValue);
	virtual void __fastcall SetFlat(bool newValue);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWindowHandle(const Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	virtual void __fastcall SetHighlightColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetHighlightBkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall TriggerLinkClickEvent(System::UnicodeString HRef);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	HIDESBASE virtual void __fastcall SetCursor(System::Uitypes::TCursor newValue);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall SetUseCustomScrollBars(bool newValue);
	virtual void __fastcall SetFlatFocusedScrollBars(bool newValue);
	virtual void __fastcall Loaded();
	virtual void __fastcall SetCaption(Elstrutils::TElFString newValue);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	virtual void __fastcall SetUseXPThemes(const bool Value);
	virtual System::WideString __fastcall GetThemedClassName();
	void __fastcall DoLinkPopup(const System::Types::TPoint &MousePos);
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetViewRect(const System::Types::TRect &Value);
	void __fastcall SetMargin(int Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	__property System::Types::TRect ViewRect = {read=FViewRect, write=SetViewRect};
	
public:
	__fastcall virtual TElHTMLView(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElHTMLView();
	DYNAMIC void __fastcall Click();
	__property bool VertScrollBarVisible = {read=FVScrollVisible, nodefault};
	__property bool HorzScrollBarVisible = {read=FHScrollVisible, nodefault};
	__property System::Types::TPoint Position = {read=FViewPos, write=SetViewPos};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property Elscrollbar::TElScrollBarStyles* VertScrollBarStyles = {read=FVertScrollBarStyles, write=SetVertScrollBarStyles};
	__property Elscrollbar::TElScrollBarStyles* HorzScrollBarStyles = {read=FHorzScrollBarStyles, write=SetHorzScrollBarStyles};
	__property bool WordWrap = {read=FWordWrap, write=SetWordWrap, nodefault};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, nodefault};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property System::Uitypes::TColor GradientStartColor = {read=FGradientStartColor, write=SetGradientStartColor, nodefault};
	__property System::Uitypes::TColor GradientEndColor = {read=FGradientEndColor, write=SetGradientEndColor, nodefault};
	__property int GradientSteps = {read=FGradientSteps, write=SetGradientSteps, default=16};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property Elvclutils::TElBkGndType BackgroundType = {read=FBackgroundType, write=SetBackgroundType, default=2};
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=SetLinkColor, default=16711680};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=SetLinkStyle, nodefault};
	__property Vcl::Menus::TPopupMenu* LinkPopupMenu = {read=FLinkPopupMenu, write=SetLinkPopupMenu};
	__property System::Uitypes::TColor HighlightColor = {read=FHighlightColor, write=SetHighlightColor, default=-16777202};
	__property System::Uitypes::TColor HighlightBkColor = {read=FHighlightBkColor, write=SetHighlightBkColor, default=-16777203};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property bool UseCustomScrollBars = {read=FUseCustomScrollBars, write=SetUseCustomScrollBars, default=1};
	__property bool FlatFocusedScrollBars = {read=FFlatFocusedScrollBars, write=SetFlatFocusedScrollBars, nodefault};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property System::UnicodeString Text = {read=FDummyString};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property int Margin = {read=FMargin, write=SetMargin, default=4};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property Align = {default=0};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHTMLView(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elhtmlview */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHTMLVIEW)
using namespace Elhtmlview;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhtmlviewHPP
