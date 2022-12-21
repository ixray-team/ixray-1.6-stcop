// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElExpBar.pas' rev: 34.00 (Windows)

#ifndef ElexpbarHPP
#define ElexpbarHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <ElTools.hpp>
#include <ElList.hpp>
#include <ElPopBtn.hpp>
#include <ElScrollBox.hpp>
#include <ElStrToken.hpp>
#include <ElPanel.hpp>
#include <ElIni.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElAdvPanel.hpp>
#include <ElHTMLPanel.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <HTMLRender.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>
#include <ElVCLUtils.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElImgFrm.hpp>
#include <Vcl.Forms.hpp>
#include <ElStrUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elexpbar
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EExplorerBarError;
class DELPHICLASS TElExplorerBarGroupButton;
class DELPHICLASS TElExplorerBarGroup;
class DELPHICLASS TElExplorerBar;
class DELPHICLASS TElExplorerBarGroupCaption;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EExplorerBarError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EExplorerBarError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EExplorerBarError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EExplorerBarError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EExplorerBarError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EExplorerBarError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EExplorerBarError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EExplorerBarError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EExplorerBarError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EExplorerBarError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EExplorerBarError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EExplorerBarError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EExplorerBarError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EExplorerBarError() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElExplorerBarGroupButton : public Eladvpanel::TElAdvCaptionButton
{
	typedef Eladvpanel::TElAdvCaptionButton inherited;
	
protected:
	virtual void __fastcall DrawThemedBackground(Vcl::Graphics::TCanvas* Canvas);
	virtual System::WideString __fastcall GetThemedClassName();
	virtual int __fastcall GetThemePartID();
	virtual int __fastcall GetThemeStateID();
public:
	/* TElAdvCaptionButton.Create */ inline __fastcall virtual TElExplorerBarGroupButton(System::Classes::TComponent* AOwner) : Eladvpanel::TElAdvCaptionButton(AOwner) { }
	
public:
	/* TCustomElGraphicButton.Destroy */ inline __fastcall virtual ~TElExplorerBarGroupButton() { }
	
};


class PASCALIMPLEMENTATION TElExplorerBarGroup : public Eladvpanel::TCustomElAdvancedPanel
{
	typedef Eladvpanel::TCustomElAdvancedPanel inherited;
	
protected:
	Vcl::Controls::TAlign FAlign;
	virtual void __fastcall TriggerMinimizeEvent();
	virtual void __fastcall TriggerRestoreEvent();
	DYNAMIC void __fastcall Resize();
	HIDESBASE MESSAGE void __fastcall CMVisibleChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMMove(Winapi::Messages::TMessage &Message);
	virtual System::WideString __fastcall GetThemedClassName();
	virtual void __fastcall DrawThemedBackground();
	virtual Eladvpanel::TElAdvCaptionButton* __fastcall CreateButton();
	virtual Eladvpanel::TElAdvCaptionPanel* __fastcall CreatePanel();
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	virtual void __fastcall SetUseXPThemes(const bool Value);
	virtual void __fastcall CreateWnd();
	
public:
	__fastcall virtual TElExplorerBarGroup(System::Classes::TComponent* AOwner);
	virtual int __fastcall GetButtonWidth();
	virtual int __fastcall GetCaptionHeight();
	
__published:
	__property Vcl::Controls::TAlign Align = {read=FAlign, write=FAlign, stored=false, default=0};
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
	__property OnMove;
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
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabStop = {default=1};
	__property Transparent = {default=0};
	__property TransparentXPThemes = {default=1};
	__property UseXPThemes = {default=1};
	__property Visible = {default=1};
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
	/* TCustomElAdvancedPanel.Destroy */ inline __fastcall virtual ~TElExplorerBarGroup() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElExplorerBarGroup(HWND ParentWindow) : Eladvpanel::TCustomElAdvancedPanel(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElExplorerBar : public Elscrollbox::TElScrollBox
{
	typedef Elscrollbox::TElScrollBox inherited;
	
private:
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	
protected:
	int FInRealign;
	bool FUpdated;
	int FMargin;
	int FSpacing;
	int FGroupWidth;
	Elini::TElIniFile* FStorage;
	System::UnicodeString FStoragePath;
	virtual void __fastcall RealignGroups();
	MESSAGE void __fastcall CMControlChange(Vcl::Controls::TCMControlChange &Msg);
	HIDESBASE MESSAGE void __fastcall CMControlListChange(Winapi::Messages::TMessage &Msg);
	DYNAMIC void __fastcall Resize();
	void __fastcall SetMargin(int Value);
	void __fastcall SetSpacing(int Value);
	virtual void __fastcall CreateWnd();
	void __fastcall SetGroupWidth(int Value);
	virtual System::WideString __fastcall GetThemedClassName();
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TMessage &Message);
	virtual void __fastcall Loaded();
	HIDESBASE Vcl::Controls::TControl* __fastcall FindChildControl(const System::UnicodeString ControlName);
	virtual void __fastcall DrawThemedBackground(HDC DC);
	
public:
	__fastcall virtual TElExplorerBar(System::Classes::TComponent* AOwner);
	TElExplorerBarGroup* __fastcall AddPanel();
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	void __fastcall Restore();
	void __fastcall Save();
	
__published:
	__property int Margin = {read=FMargin, write=SetMargin, default=4};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=8};
	__property int GroupWidth = {read=FGroupWidth, write=SetGroupWidth, default=0};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=FStorage};
	__property System::UnicodeString StoragePath = {read=FStoragePath, write=FStoragePath};
public:
	/* TElScrollBox.Destroy */ inline __fastcall virtual ~TElExplorerBar() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElExplorerBar(HWND ParentWindow) : Elscrollbox::TElScrollBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElExplorerBarGroupCaption : public Eladvpanel::TElAdvCaptionPanel
{
	typedef Eladvpanel::TElAdvCaptionPanel inherited;
	
protected:
	virtual void __fastcall DrawThemedBackground();
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	virtual System::WideString __fastcall GetThemedClassName();
public:
	/* TElAdvCaptionPanel.Create */ inline __fastcall virtual TElExplorerBarGroupCaption(System::Classes::TComponent* AOwner) : Eladvpanel::TElAdvCaptionPanel(AOwner) { }
	
public:
	/* TCustomElHTMLPanel.Destroy */ inline __fastcall virtual ~TElExplorerBarGroupCaption() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElExplorerBarGroupCaption(HWND ParentWindow) : Eladvpanel::TElAdvCaptionPanel(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elexpbar */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELEXPBAR)
using namespace Elexpbar;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElexpbarHPP
