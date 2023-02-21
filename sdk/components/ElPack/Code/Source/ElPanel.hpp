// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElPanel.pas' rev: 35.00 (Windows)

#ifndef ElpanelHPP
#define ElpanelHPP

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
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElXPThemedControl.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElExtBkgnd.hpp>
#include <ElTools.hpp>
#include <ElImgFrm.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elpanel
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElPanelGrabHandle;
class DELPHICLASS TCustomElPanel;
class DELPHICLASS TElPanel;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElPanelGrabHandleKind : unsigned char { ghkNone, ghkMove, ghkResize, ghkMoveParent };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElPanelGrabHandle : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FSize;
	TElPanelGrabHandleKind FKind;
	Vcl::Controls::TBevelCut FBevelKind;
	TCustomElPanel* FOwner;
	bool FVisible;
	void __fastcall SetVisible(bool newValue);
	void __fastcall SetEnabled(bool newValue);
	void __fastcall SetSize(int newValue);
	void __fastcall SetBevelKind(Vcl::Controls::TBevelCut newValue);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
	__property int Size = {read=FSize, write=SetSize, default=0};
	__property TElPanelGrabHandleKind Kind = {read=FKind, write=FKind, default=0};
	__property Vcl::Controls::TBevelCut BevelKind = {read=FBevelKind, write=SetBevelKind, default=0};
	__property bool Visible = {read=FVisible, write=SetVisible, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TElPanelGrabHandle() { }
	
public:
	/* TObject.Create */ inline __fastcall TElPanelGrabHandle() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCustomElPanel : public Vcl::Extctrls::TCustomPanel
{
	typedef Vcl::Extctrls::TCustomPanel inherited;
	
private:
	Vcl::Controls::TBevelCut FBevelInner;
	Vcl::Controls::TBevelCut FBevelOuter;
	Vcl::Controls::TBevelWidth FBevelWidth;
	HIDESBASE void __fastcall SetBevelInner(Vcl::Controls::TBevelCut Value);
	HIDESBASE void __fastcall SetBevelOuter(Vcl::Controls::TBevelCut Value);
	HIDESBASE void __fastcall SetBevelWidth(Vcl::Controls::TBevelWidth Value);
	
protected:
	bool FOwnerDraw;
	bool FAlwaysPaintBackground;
	Vcl::Stdctrls::TTextLayout FLayout;
	System::Classes::TNotifyEvent FOnPaint;
	int FGradientSteps;
	System::Uitypes::TColor FGradientStartColor;
	System::Uitypes::TColor FGradientEndColor;
	Vcl::Graphics::TBitmap* FTmpBmp;
	bool FTransparent;
	Vcl::Graphics::TBitmap* FBackground;
	Elvclutils::TElBkGndType FBackgroundType;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	Elimgfrm::TElImageForm* FImgForm;
	HWND FSaveCapture;
	System::Classes::TNotifyEvent FOnMove;
	bool FResizable;
	bool FMovable;
	System::Types::TRect FSizeMoveRect;
	System::StaticArray<TElPanelGrabHandle*, 4> FGrabHandles;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	bool FMouseInControl;
	NativeUInt FTheme;
	bool FUseXPThemes;
	bool FPressed;
	bool FIntPaint;
	WideString FCaption;
	System::Byte FAlphaLevel;
	bool FTransparentXPThemes;
	bool FSizeGrip;
	System::Types::TRect SizeGripRect;
	System::Uitypes::TColor FBevelSpaceColor;
	bool FShowFocus;
	System::WideString FHint;
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* newValue);
	void __fastcall ImageChange(System::TObject* Sender);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetResizable(bool newValue);
	MESSAGE void __fastcall WMEnterSizeMove(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMExitSizeMove(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMMove(Winapi::Messages::TWMMove &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	void __fastcall RedoTmpBmp();
	void __fastcall SetGradientStartColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientEndColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientSteps(int newValue);
	void __fastcall SetLayout(Vcl::Stdctrls::TTextLayout newValue);
	void __fastcall SetOwnerDraw(bool newValue);
	TElPanelGrabHandle* __fastcall GetTopGrabHandle();
	void __fastcall SetTopGrabHandle(TElPanelGrabHandle* newValue);
	TElPanelGrabHandle* __fastcall GetRightGrabHandle();
	void __fastcall SetRightGrabHandle(TElPanelGrabHandle* newValue);
	TElPanelGrabHandle* __fastcall GetLeftGrabHandle();
	void __fastcall SetLeftGrabHandle(TElPanelGrabHandle* newValue);
	TElPanelGrabHandle* __fastcall GetBottomGrabHandle();
	void __fastcall SetBottomGrabHandle(TElPanelGrabHandle* newValue);
	void __fastcall SetAlwaysPaintBackground(bool Value);
	virtual void __fastcall Loaded();
	virtual void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	virtual void __fastcall SetBackgroundType(Elvclutils::TElBkGndType newValue);
	virtual void __fastcall SetTransparent(bool newValue);
	virtual void __fastcall SetMovable(bool newValue);
	virtual void __fastcall AlignControls(Vcl::Controls::TControl* AControl, System::Types::TRect &Rect);
	virtual void __fastcall Paint();
	virtual void __fastcall TriggerMoveEvent();
	virtual void __fastcall TriggerPaintEvent();
	virtual void __fastcall SetCaption(WideString newValue);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Message);
	HIDESBASE virtual void __fastcall AdjustClientRect(System::Types::TRect &Rect);
	TElPanelGrabHandle* __fastcall InGrabHandle(int X, int Y, const System::Types::TRect &Rect);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoMouseEnter();
	virtual void __fastcall DoMouseLeave();
	virtual WideString __fastcall GetCaption();
	void __fastcall SetAlphaLevel(System::Byte Value);
	virtual System::WideString __fastcall GetThemedClassName();
	virtual void __fastcall SetUseXPThemes(const bool Value);
	virtual void __fastcall CreateThemeHandle();
	virtual void __fastcall FreeThemeHandle();
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall DrawThemedBackground();
	virtual System::Types::TRect __fastcall GetBackgroundClientRect();
	virtual void __fastcall SetTransparentXPThemes(bool Value);
	void __fastcall SetSizeGrip(bool Value);
	virtual void __fastcall UpdateInterior();
	void __fastcall SetBevelSpaceColor(System::Uitypes::TColor Value);
	void __fastcall SetShowFocus(bool Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	__property bool AlwaysPaintBackground = {read=FAlwaysPaintBackground, write=SetAlwaysPaintBackground, default=0};
	__property Vcl::Stdctrls::TTextLayout Layout = {read=FLayout, write=SetLayout, default=1};
	__property bool OwnerDraw = {read=FOwnerDraw, write=SetOwnerDraw, default=0};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property Elvclutils::TElBkGndType BackgroundType = {read=FBackgroundType, write=SetBackgroundType, default=2};
	__property bool Resizable = {read=FResizable, write=SetResizable, default=0};
	__property bool Movable = {read=FMovable, write=SetMovable, default=0};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property System::Uitypes::TColor GradientStartColor = {read=FGradientStartColor, write=SetGradientStartColor, default=0};
	__property System::Uitypes::TColor GradientEndColor = {read=FGradientEndColor, write=SetGradientEndColor, default=0};
	__property int GradientSteps = {read=FGradientSteps, write=SetGradientSteps, default=16};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property TElPanelGrabHandle* TopGrabHandle = {read=GetTopGrabHandle, write=SetTopGrabHandle};
	__property TElPanelGrabHandle* RightGrabHandle = {read=GetRightGrabHandle, write=SetRightGrabHandle};
	__property TElPanelGrabHandle* LeftGrabHandle = {read=GetLeftGrabHandle, write=SetLeftGrabHandle};
	__property TElPanelGrabHandle* BottomGrabHandle = {read=GetBottomGrabHandle, write=SetBottomGrabHandle};
	__property System::Classes::TNotifyEvent OnMove = {read=FOnMove, write=FOnMove};
	__property System::Classes::TNotifyEvent OnPaint = {read=FOnPaint, write=FOnPaint};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property System::Byte AlphaLevel = {read=FAlphaLevel, write=SetAlphaLevel, default=0};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property bool TransparentXPThemes = {read=FTransparentXPThemes, write=SetTransparentXPThemes, default=1};
	__property TabStop = {default=1};
	__property bool SizeGrip = {read=FSizeGrip, write=SetSizeGrip, default=0};
	__property Vcl::Controls::TBevelCut BevelInner = {read=FBevelInner, write=SetBevelInner, default=0};
	__property Vcl::Controls::TBevelCut BevelOuter = {read=FBevelOuter, write=SetBevelOuter, default=2};
	__property Vcl::Controls::TBevelWidth BevelWidth = {read=FBevelWidth, write=SetBevelWidth, default=1};
	__property System::Uitypes::TColor BevelSpaceColor = {read=FBevelSpaceColor, write=SetBevelSpaceColor, default=-16777201};
	__property bool ShowFocus = {read=FShowFocus, write=SetShowFocus, default=0};
	
public:
	virtual void __fastcall Update();
	__fastcall virtual TCustomElPanel(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElPanel();
	bool __fastcall IsThemeApplied();
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
	__property Canvas;
	__property NativeUInt Theme = {read=FTheme, nodefault};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElPanel(HWND ParentWindow) : Vcl::Extctrls::TCustomPanel(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElPanel : public TCustomElPanel
{
	typedef TCustomElPanel inherited;
	
__published:
	__property AlphaLevel = {default=0};
	__property AlwaysPaintBackground = {default=0};
	__property Background;
	__property BackgroundType = {default=2};
	__property GradientEndColor = {default=0};
	__property GradientStartColor = {default=0};
	__property GradientSteps = {default=16};
	__property Alignment = {default=2};
	__property Layout = {default=1};
	__property OwnerDraw = {default=0};
	__property ImageForm;
	__property TopGrabHandle;
	__property RightGrabHandle;
	__property LeftGrabHandle;
	__property BottomGrabHandle;
	__property Resizable = {default=0};
	__property Movable = {default=0};
	__property OnMove;
	__property OnPaint;
	__property SizeGrip = {default=0};
	__property Align;
	__property BevelInner = {default=0};
	__property BevelOuter = {default=2};
	__property BevelSpaceColor = {default=-16777201};
	__property BevelWidth = {default=1};
	__property BorderStyle = {default=0};
	__property BorderWidth = {default=0};
	__property TransparentXPThemes = {default=1};
	__property UseXPThemes = {default=1};
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
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Transparent = {default=0};
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
	__property OnStartDrag;
public:
	/* TCustomElPanel.Create */ inline __fastcall virtual TElPanel(System::Classes::TComponent* AOwner) : TCustomElPanel(AOwner) { }
	/* TCustomElPanel.Destroy */ inline __fastcall virtual ~TElPanel() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElPanel(HWND ParentWindow) : TCustomElPanel(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elpanel */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELPANEL)
using namespace Elpanel;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElpanelHPP
