// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHTMLLbl.pas' rev: 34.00 (Windows)

#ifndef ElhtmllblHPP
#define ElhtmllblHPP

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
#include <Vcl.Menus.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <HTMLRender.hpp>
#include <ElTools.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElHandPt.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhtmllbl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHTMLLabel;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElHTMLLabel : public Vcl::Stdctrls::TCustomLabel
{
	typedef Vcl::Stdctrls::TCustomLabel inherited;
	
private:
	System::Uitypes::TCursor FCursor;
	Htmlrender::TElHTMLRender* FRender;
	bool FIsHTML;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	System::Uitypes::TColor FLinkColor;
	Vcl::Menus::TPopupMenu* FLinkPopupMenu;
	System::Uitypes::TFontStyles FLinkStyle;
	System::WideString FHint;
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	void __fastcall SetLinkPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	
protected:
	Elstrutils::TElFString FCaption;
	DYNAMIC void __fastcall AdjustBounds();
	virtual void __fastcall SetIsHTML(bool newValue);
	virtual void __fastcall TriggerLinkClickEvent(Elstrutils::TElFString HRef);
	void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	virtual void __fastcall Loaded();
	HIDESBASE virtual void __fastcall SetCursor(System::Uitypes::TCursor newValue);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall Click();
	virtual bool __fastcall GetWordWrap();
	HIDESBASE virtual void __fastcall SetWordWrap(bool newValue);
	virtual void __fastcall SetAutoSize(bool newValue);
	System::Types::TRect __fastcall GetTextRect();
	void __fastcall SetCaption(Elstrutils::TElFString Value);
	DYNAMIC void __fastcall DoDrawText(System::Types::TRect &Rect, int Flags);
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	void __fastcall DoLinkPopup(const System::Types::TPoint &MousePos);
	System::Types::TRect __fastcall CalcTextRect();
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	virtual void __fastcall SetName(const System::Classes::TComponentName Value);
	
public:
	__fastcall virtual TElHTMLLabel(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElHTMLLabel();
	virtual void __fastcall Paint();
	virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);
	__property System::Types::TRect TextRect = {read=GetTextRect};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, nodefault};
	__property bool WordWrap = {read=GetWordWrap, write=SetWordWrap, nodefault};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=SetLinkColor, default=16711680};
	__property Vcl::Menus::TPopupMenu* LinkPopupMenu = {read=FLinkPopupMenu, write=SetLinkPopupMenu};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=SetLinkStyle, nodefault};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Align = {default=0};
	__property Alignment = {default=0};
	__property AutoSize = {default=1};
	__property Color;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property FocusControl;
	__property Font;
	__property ParentColor = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Transparent;
	__property Layout = {default=0};
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
}	/* namespace Elhtmllbl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHTMLLBL)
using namespace Elhtmllbl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhtmllblHPP
