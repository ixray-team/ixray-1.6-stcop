// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElAdvPanel.pas' rev: 34.00 (Windows)

#ifndef EladvpanelHPP
#define EladvpanelHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElPanel.hpp>
#include <ElPopBtn.hpp>
#include <ElVCLUtils.hpp>
#include <ElHTMLPanel.hpp>
#include <ElTools.hpp>
#include <ElImgFrm.hpp>
#include <ElStrUtils.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <System.UITypes.hpp>
#include <HTMLRender.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eladvpanel
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElPanelCaptionSettings;
class DELPHICLASS TCustomElAdvancedPanel;
class DELPHICLASS TElAdvancedPanel;
class DELPHICLASS TElAdvCaptionPanel;
class DELPHICLASS TElAdvCaptionButton;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TElPanelCaptionSettings : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FVisible;
	int FHeight;
	bool FShowCloseButton;
	TCustomElAdvancedPanel* FOwner;
	bool FShowMinButton;
	bool FFlatButtons;
	int FButtonWidth;
	System::Classes::TAlignment FAlignment;
	Vcl::Stdctrls::TTextLayout FLayout;
	System::Uitypes::TColor FButtonColor;
	void __fastcall SetText(Elstrutils::TElFString Value);
	void __fastcall SetVisible(bool Value);
	void __fastcall SetHeight(int Value);
	void __fastcall SetShowCloseButton(bool Value);
	void __fastcall SetShowMinButton(bool Value);
	void __fastcall SetFlatButtons(bool Value);
	Elstrutils::TElFString __fastcall GetText();
	void __fastcall SetButtonWidth(int Value);
	void __fastcall SetAlignment(System::Classes::TAlignment Value);
	void __fastcall SetLayout(Vcl::Stdctrls::TTextLayout newValue);
	Vcl::Graphics::TFont* __fastcall GetFont();
	void __fastcall SetFont(Vcl::Graphics::TFont* Value);
	bool __fastcall GetParentFont();
	void __fastcall SetParentFont(bool Value);
	System::Uitypes::TColor __fastcall GetColor();
	void __fastcall SetColor(System::Uitypes::TColor Value);
	void __fastcall SetButtonColor(System::Uitypes::TColor Value);
	Elimgfrm::TElImageForm* __fastcall GetImageForm();
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* Value);
	
protected:
	bool FInvertMinButtonArrows;
	bool FAutoSize;
	Vcl::Graphics::TBitmap* __fastcall GetMinButtonGlyph();
	void __fastcall SetMinButtonGlyph(Vcl::Graphics::TBitmap* Value);
	Vcl::Graphics::TBitmap* __fastcall GetCloseButtonGlyph();
	void __fastcall SetCloseButtonGlyph(Vcl::Graphics::TBitmap* Value);
	void __fastcall SetInvertMinButtonArrows(bool Value);
	void __fastcall SetAutoSize(bool Value);
	void __fastcall FontChanged();
	void __fastcall AdjustHeight();
	
public:
	__fastcall TElPanelCaptionSettings(TCustomElAdvancedPanel* Owner);
	
__published:
	__property Elstrutils::TElFString Text = {read=GetText, write=SetText};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property int Height = {read=FHeight, write=SetHeight, default=19};
	__property bool ShowCloseButton = {read=FShowCloseButton, write=SetShowCloseButton, nodefault};
	__property bool ShowMinButton = {read=FShowMinButton, write=SetShowMinButton, default=1};
	__property bool FlatButtons = {read=FFlatButtons, write=SetFlatButtons, default=1};
	__property int ButtonWidth = {read=FButtonWidth, write=SetButtonWidth, default=15};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=2};
	__property Vcl::Stdctrls::TTextLayout Layout = {read=FLayout, write=SetLayout, default=1};
	__property Vcl::Graphics::TFont* Font = {read=GetFont, write=SetFont};
	__property bool ParentFont = {read=GetParentFont, write=SetParentFont, nodefault};
	__property System::Uitypes::TColor Color = {read=GetColor, write=SetColor, nodefault};
	__property System::Uitypes::TColor ButtonColor = {read=FButtonColor, write=SetButtonColor, default=-16777201};
	__property Elimgfrm::TElImageForm* ImageForm = {read=GetImageForm, write=SetImageForm};
	__property Vcl::Graphics::TBitmap* MinButtonGlyph = {read=GetMinButtonGlyph, write=SetMinButtonGlyph};
	__property Vcl::Graphics::TBitmap* CloseButtonGlyph = {read=GetCloseButtonGlyph, write=SetCloseButtonGlyph};
	__property bool InvertMinButtonArrows = {read=FInvertMinButtonArrows, write=SetInvertMinButtonArrows, default=0};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TElPanelCaptionSettings() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCustomElAdvancedPanel : public Elhtmlpanel::TCustomElHTMLPanel
{
	typedef Elhtmlpanel::TCustomElHTMLPanel inherited;
	
private:
	void __fastcall SetCaptionSettings(TElPanelCaptionSettings* Value);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TMessage &Msg);
	
protected:
	Vcl::Graphics::TBitmap* FMinButtonGlyph;
	Vcl::Controls::TImageList* FImages;
	int FSaveHeight;
	bool FMinimized;
	Elhtmlpanel::TElHTMLPanel* FCaptionPanel;
	TElPanelCaptionSettings* FCaptionSettings;
	Elpopbtn::TElGraphicButton* FMinButton;
	Elpopbtn::TElGraphicButton* FCloseButton;
	System::Classes::TNotifyEvent FOnMinimize;
	System::Classes::TNotifyEvent FOnRestore;
	System::Classes::TNotifyEvent FOnClose;
	virtual void __fastcall SetLinkPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TMessage &Message);
	virtual void __fastcall TriggerMinimizeEvent();
	virtual void __fastcall TriggerRestoreEvent();
	void __fastcall AdjustButtonSize();
	void __fastcall OnCaptionSize(System::TObject* Sender);
	void __fastcall OnMinButtonClick(System::TObject* Sender);
	void __fastcall OnCloseButtonClick(System::TObject* Sender);
	virtual void __fastcall TriggerCloseEvent();
	virtual void __fastcall AdjustClientRect(System::Types::TRect &Rect);
	virtual void __fastcall CreateWnd();
	void __fastcall LinkClickEventTransfer(System::TObject* Sender, Elstrutils::TElFString HRef);
	virtual void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	virtual void __fastcall SetUseXPThemes(const bool Value);
	virtual void __fastcall AdjustInnerSize(System::Types::TRect &R);
	int __fastcall GetBevelAdjustment();
	void __fastcall UpdateMinButtonImages();
	virtual void __fastcall Loaded();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadExpHeight(System::Classes::TReader* Reader);
	void __fastcall WriteExpHeight(System::Classes::TWriter* Writer);
	virtual void __fastcall SetTransparentXPThemes(bool Value);
	virtual TElAdvCaptionButton* __fastcall CreateButton();
	virtual void __fastcall SetMinimized(bool Value);
	virtual TElAdvCaptionPanel* __fastcall CreatePanel();
	virtual int __fastcall GetThemePartID();
	virtual void __fastcall UpdateInterior();
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Message);
	__property bool Minimized = {read=FMinimized, write=SetMinimized, default=0};
	__property TElPanelCaptionSettings* CaptionSettings = {read=FCaptionSettings, write=SetCaptionSettings};
	__property System::Classes::TNotifyEvent OnMinimize = {read=FOnMinimize, write=FOnMinimize};
	__property System::Classes::TNotifyEvent OnRestore = {read=FOnRestore, write=FOnRestore};
	__property System::Classes::TNotifyEvent OnClose = {read=FOnClose, write=FOnClose};
	
public:
	__fastcall virtual TCustomElAdvancedPanel(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElAdvancedPanel();
	virtual int __fastcall GetCaptionHeight();
	virtual int __fastcall GetButtonWidth();
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElAdvancedPanel(HWND ParentWindow) : Elhtmlpanel::TCustomElHTMLPanel(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElAdvancedPanel : public TCustomElAdvancedPanel
{
	typedef TCustomElAdvancedPanel inherited;
	
__published:
	__property OnImageNeeded;
	__property OnLinkClick;
	__property Cursor;
	__property LinkColor;
	__property LinkPopupMenu;
	__property LinkStyle;
	__property Background;
	__property BackgroundType = {default=2};
	__property GradientEndColor = {default=0};
	__property GradientStartColor = {default=0};
	__property GradientSteps = {default=16};
	__property Alignment = {default=2};
	__property Layout = {default=1};
	__property ImageForm;
	__property TopGrabHandle;
	__property RightGrabHandle;
	__property LeftGrabHandle;
	__property BottomGrabHandle;
	__property Resizable = {default=0};
	__property Movable = {default=0};
	__property OnMove;
	__property Align;
	__property BevelInner;
	__property BevelOuter;
	__property BevelSpaceColor;
	__property BevelWidth = {default=1};
	__property BorderStyle = {default=0};
	__property BorderWidth = {default=0};
	__property Canvas;
	__property Color = {default=-16777201};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property Locked = {default=0};
	__property MouseCapture;
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabStop = {default=1};
	__property Transparent = {default=0};
	__property TransparentXPThemes = {default=1};
	__property UseXPThemes = {default=1};
	__property Visible = {default=1};
	__property SizeGrip = {default=0};
	__property Caption;
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
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property Minimized = {default=0};
	__property CaptionSettings;
	__property OnMinimize;
	__property OnRestore;
	__property OnClose;
public:
	/* TCustomElAdvancedPanel.Create */ inline __fastcall virtual TElAdvancedPanel(System::Classes::TComponent* AOwner) : TCustomElAdvancedPanel(AOwner) { }
	/* TCustomElAdvancedPanel.Destroy */ inline __fastcall virtual ~TElAdvancedPanel() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElAdvancedPanel(HWND ParentWindow) : TCustomElAdvancedPanel(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElAdvCaptionPanel : public Elhtmlpanel::TElHTMLPanel
{
	typedef Elhtmlpanel::TElHTMLPanel inherited;
	
private:
	int FDummyInt;
	bool FDummyBool;
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual void __fastcall CreateWnd();
	virtual void __fastcall AdjustClientRect(System::Types::TRect &Rect);
	virtual void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Message);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TElAdvCaptionPanel(System::Classes::TComponent* AOwner);
	
__published:
	__property bool TabStop = {write=FDummyBool, nodefault};
	__property int TabOrder = {write=FDummyInt, nodefault};
public:
	/* TCustomElHTMLPanel.Destroy */ inline __fastcall virtual ~TElAdvCaptionPanel() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElAdvCaptionPanel(HWND ParentWindow) : Elhtmlpanel::TElHTMLPanel(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElAdvCaptionButton : public Elpopbtn::TElGraphicButton
{
	typedef Elpopbtn::TElGraphicButton inherited;
	
protected:
	virtual void __fastcall DrawThemedBackground(Vcl::Graphics::TCanvas* Canvas);
	
public:
	__fastcall virtual TElAdvCaptionButton(System::Classes::TComponent* AOwner);
public:
	/* TCustomElGraphicButton.Destroy */ inline __fastcall virtual ~TElAdvCaptionButton() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eladvpanel */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELADVPANEL)
using namespace Eladvpanel;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EladvpanelHPP
