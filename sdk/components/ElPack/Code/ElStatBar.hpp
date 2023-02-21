// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElStatBar.pas' rev: 35.00 (Windows)

#ifndef ElstatbarHPP
#define ElstatbarHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <ElXPThemedControl.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <HTMLRender.hpp>
#include <ElTools.hpp>
#include <ElStrUtils.hpp>
#include <ElList.hpp>
#include <ElIni.hpp>
#include <ElVCLUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elstatbar
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElStatusPanel;
class DELPHICLASS TElStatusPanels;
class DELPHICLASS TElStatusBar;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElStatusPanelBevel : unsigned char { epbLowered, epbNone, epbRaised };

enum DECLSPEC_DENUM TElStatusPanelStyle : unsigned char { epsText, epsControl, epsOwnerDraw };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElStatusPanel : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Elstrutils::TElFString FHint;
	System::Classes::TAlignment FAlignment;
	TElStatusPanelBevel FBevel;
	TElStatusPanelStyle FStyle;
	Elstrutils::TElFString FText;
	int FWidth;
	Vcl::Controls::TControl* FControl;
	Vcl::Controls::TWinControl* FOldParent;
	System::Types::TRect FOldBounds;
	bool FCtlVisible;
	bool FOldVisible;
	bool FVisible;
	TElStatusBar* FOwner;
	int FSavedWidth;
	bool FResizable;
	bool FAutoSize;
	bool FIsHTML;
	void __fastcall SetAutoSize(bool newValue);
	void __fastcall SetVisible(bool newValue);
	void __fastcall SetAlignment(System::Classes::TAlignment newValue);
	void __fastcall SetBevel(TElStatusPanelBevel newValue);
	void __fastcall SetStyle(TElStatusPanelStyle newValue);
	void __fastcall SetText(Elstrutils::TElFString newValue);
	void __fastcall SetWidth(int newValue);
	void __fastcall SetControl(Vcl::Controls::TControl* newValue);
	int __fastcall GetLeft();
	int __fastcall GetRight();
	System::Types::TRect __fastcall GetPanelRect();
	void __fastcall SetIsHTML(bool Value);
	
protected:
	void __fastcall SaveWidth();
	void __fastcall RestoreWidth();
	virtual void __fastcall UpdateControl();
	int __fastcall CurWidth();
	int __fastcall CalcAutoSize();
	
public:
	__fastcall virtual TElStatusPanel(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElStatusPanel();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	int __fastcall GetRealWidth();
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, nodefault};
	__property int Left = {read=GetLeft, nodefault};
	__property int Right = {read=GetRight, nodefault};
	__property System::Types::TRect PanelRect = {read=GetPanelRect};
	
__published:
	__property bool Resizable = {read=FResizable, write=FResizable, default=1};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property TElStatusPanelBevel Bevel = {read=FBevel, write=SetBevel, default=0};
	__property TElStatusPanelStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property Elstrutils::TElFString Text = {read=FText, write=SetText};
	__property int Width = {read=FWidth, write=SetWidth, default=100};
	__property Vcl::Controls::TControl* Control = {read=FControl, write=SetControl};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property Elstrutils::TElFString Hint = {read=FHint, write=FHint};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, nodefault};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TElPanelEvent)(System::TObject* Sender, TElStatusPanel* Panel);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElStatusPanels : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElStatusPanel* operator[](int Index) { return this->Items[Index]; }
	
private:
	TElStatusBar* FStatusBar;
	TElStatusPanel* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TElStatusPanel* newValue);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TElStatusPanels(TElStatusBar* StatusBar);
	HIDESBASE TElStatusPanel* __fastcall Add();
	__property TElStatusPanel* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TElStatusPanels() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElStatusBar : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
private:
	TElStatusPanelBevel FBevel;
	Elini::TElIniFile* FStorage;
	System::UnicodeString FStoragePath;
	bool FResizablePanels;
	bool FPressed;
	TElStatusPanel* FHintPanel;
	TElStatusPanel* FPressedPanel;
	TElStatusPanel* FResizePanel;
	Elstrutils::TElFString FSimpleText;
	bool FSimplePanel;
	bool FSizeGrip;
	TElPanelEvent FOnPanelResize;
	TElPanelEvent FOnPanelDraw;
	TElPanelEvent FOnPanelClick;
	TElPanelEvent FOnPanelDblClick;
	System::Classes::TNotifyEvent FOnResize;
	TElStatusPanels* FPanels;
	System::Uitypes::TCursor FOldCursor;
	int FDelta;
	int FLine;
	bool FLineVis;
	bool FIgnoreSize;
	bool FSimpleTextIsHTML;
	Htmlrender::TElHTMLRender* FRender;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	System::WideString FHint;
	void __fastcall SetPanels(TElStatusPanels* Value);
	void __fastcall SetSimpleText(Elstrutils::TElFString newValue);
	void __fastcall SetSimplePanel(bool newValue);
	void __fastcall SetSizeGrip(bool newValue);
	HIDESBASE MESSAGE void __fastcall WMERASEBKGND(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Msg);
	void __fastcall IntHintShow(Vcl::Controls::THintInfo &HintInfo);
	void __fastcall DrawDragLine(bool Restore);
	void __fastcall SetBevel(TElStatusPanelBevel newValue);
	void __fastcall SetSimpleTextIsHTML(bool Value);
	
protected:
	virtual System::WideString __fastcall GetThemedClassName();
	virtual void __fastcall TriggerResizeEvent();
	virtual void __fastcall TriggerPanelResizeEvent(TElStatusPanel* Panel);
	virtual void __fastcall TriggerPanelDrawEvent(TElStatusPanel* Panel);
	virtual void __fastcall TriggerPanelDblClickEvent(TElStatusPanel* Panel);
	virtual void __fastcall TriggerPanelClickEvent(TElStatusPanel* Panel);
	virtual void __fastcall Paint();
	virtual void __fastcall DrawPanel(TElStatusPanel* Panel);
	virtual void __fastcall UpdatePanels();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	void __fastcall IntLButtonDown(int X, int Y);
	void __fastcall IntLButtonUp(int X, int Y);
	void __fastcall IntMouseMove(int X, int Y);
	void __fastcall InitDragLine();
	void __fastcall DeinitDragLine();
	HIDESBASE void __fastcall Resize();
	void __fastcall SetHint(System::WideString Value);
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TElStatusBar(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElStatusBar();
	virtual TElStatusPanel* __fastcall PanelAtPoint(const System::Types::TPoint &Pos);
	__property Canvas;
	void __fastcall Save();
	void __fastcall Restore();
	
__published:
	__property TElStatusPanels* Panels = {read=FPanels, write=SetPanels};
	__property Elstrutils::TElFString SimpleText = {read=FSimpleText, write=SetSimpleText};
	__property bool SimplePanel = {read=FSimplePanel, write=SetSimplePanel, default=1};
	__property bool SimpleTextIsHTML = {read=FSimpleTextIsHTML, write=SetSimpleTextIsHTML, nodefault};
	__property bool SizeGrip = {read=FSizeGrip, write=SetSizeGrip, default=1};
	__property bool ResizablePanels = {read=FResizablePanels, write=FResizablePanels, nodefault};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=FStorage};
	__property System::UnicodeString StoragePath = {read=FStoragePath, write=FStoragePath};
	__property TElStatusPanelBevel Bevel = {read=FBevel, write=SetBevel, nodefault};
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property UseXPThemes = {default=1};
	__property Align = {default=0};
	__property Color;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Visible = {default=1};
	__property TElPanelEvent OnPanelResize = {read=FOnPanelResize, write=FOnPanelResize};
	__property TElPanelEvent OnPanelDraw = {read=FOnPanelDraw, write=FOnPanelDraw};
	__property TElPanelEvent OnPanelClick = {read=FOnPanelClick, write=FOnPanelClick};
	__property TElPanelEvent OnPanelDblClick = {read=FOnPanelDblClick, write=FOnPanelDblClick};
	__property Htmlrender::TElHTMLImageNeededEvent OnHTMLImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property System::Classes::TNotifyEvent OnResize = {read=FOnResize, write=FOnResize};
	__property Anchors = {default=3};
	__property Action;
	__property Constraints;
	__property DockOrientation;
	__property Floating;
	__property BevelKind = {default=0};
	__property DoubleBuffered;
	__property DragKind = {default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElStatusBar(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::WideString __fastcall ConvertHintTextW(System::UnicodeString Hint);
}	/* namespace Elstatbar */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSTATBAR)
using namespace Elstatbar;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElstatbarHPP
