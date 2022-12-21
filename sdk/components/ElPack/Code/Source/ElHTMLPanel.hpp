// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHTMLPanel.pas' rev: 34.00 (Windows)

#ifndef ElhtmlpanelHPP
#define ElhtmlpanelHPP

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
#include <ElVCLUtils.hpp>
#include <HTMLRender.hpp>
#include <ElTools.hpp>
#include <Vcl.Menus.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <ElStrUtils.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <ElImgFrm.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhtmlpanel
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomElHTMLPanel;
class DELPHICLASS TElHTMLPanel;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCustomElHTMLPanel : public Elpanel::TCustomElPanel
{
	typedef Elpanel::TCustomElPanel inherited;
	
private:
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	System::Uitypes::TCursor FCursor;
	System::Uitypes::TColor FLinkColor;
	Vcl::Menus::TPopupMenu* FLinkPopupMenu;
	System::Uitypes::TFontStyles FLinkStyle;
	System::Types::TRect FTextRect;
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	
protected:
	Htmlrender::TElHTMLRender* FRender;
	virtual void __fastcall TriggerPaintEvent();
	void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	virtual void __fastcall TriggerLinkClickEvent(Elstrutils::TElFString HRef);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall Click();
	HIDESBASE virtual void __fastcall SetCursor(System::Uitypes::TCursor newValue);
	virtual void __fastcall SetLinkPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	virtual void __fastcall SetCaption(Elstrutils::TElFString newValue);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	void __fastcall DoLinkPopup(const System::Types::TPoint &MousePos);
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=SetLinkColor, nodefault};
	__property Vcl::Menus::TPopupMenu* LinkPopupMenu = {read=FLinkPopupMenu, write=SetLinkPopupMenu};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=SetLinkStyle, nodefault};
	
public:
	__fastcall virtual TCustomElHTMLPanel(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElHTMLPanel();
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElHTMLPanel(HWND ParentWindow) : Elpanel::TCustomElPanel(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElHTMLPanel : public TCustomElHTMLPanel
{
	typedef TCustomElHTMLPanel inherited;
	
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
	__property BevelInner = {default=0};
	__property BevelOuter = {default=2};
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
public:
	/* TCustomElHTMLPanel.Create */ inline __fastcall virtual TElHTMLPanel(System::Classes::TComponent* AOwner) : TCustomElHTMLPanel(AOwner) { }
	/* TCustomElHTMLPanel.Destroy */ inline __fastcall virtual ~TElHTMLPanel() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHTMLPanel(HWND ParentWindow) : TCustomElHTMLPanel(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elhtmlpanel */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHTMLPANEL)
using namespace Elhtmlpanel;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhtmlpanelHPP
