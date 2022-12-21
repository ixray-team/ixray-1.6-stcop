// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'HTMLLbx.pas' rev: 34.00 (Windows)

#ifndef HtmllbxHPP
#define HtmllbxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElACtrls.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <HTMLRender.hpp>
#include <ElHintWnd.hpp>
#include <ElStrUtils.hpp>
#include <ElImgFrm.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Htmllbx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomElHTMLListBox;
class DELPHICLASS TCustomElHTMLComboBox;
class DELPHICLASS TElHTMLListBox;
class DELPHICLASS TElHTMLComboBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCustomElHTMLListBox : public Elactrls::TElAdvancedListBox
{
	typedef Elactrls::TElAdvancedListBox inherited;
	
private:
	System::Classes::TNotifyEvent FDummyEvent;
	Htmlrender::TElHTMLRender* FRender;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	bool FIsHTML;
	System::Uitypes::TCursor FCursor;
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	
protected:
	Vcl::Stdctrls::TListBoxStyle FStyle;
	virtual int __fastcall GetItemWidth(int Index);
	virtual void __fastcall MeasureItem(int Index, int &Height);
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	virtual void __fastcall TriggerLinkClickEvent(System::TObject* Sender, Elstrutils::TElFString HRef);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	HIDESBASE virtual void __fastcall SetCursor(System::Uitypes::TCursor newValue);
	virtual Vcl::Controls::THintWindow* __fastcall CreateHintWindow();
	HIDESBASE void __fastcall SetStyle(Vcl::Stdctrls::TListBoxStyle Value);
	virtual void __fastcall SetIsHTML(bool Value);
	
public:
	__fastcall virtual TCustomElHTMLListBox(System::Classes::TComponent* AOwner);
	virtual void __fastcall Loaded();
	__fastcall virtual ~TCustomElHTMLListBox();
	
__published:
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property System::Classes::TNotifyEvent OnDrawItem = {read=FDummyEvent};
	__property System::Classes::TNotifyEvent OnMeasureItem = {read=FDummyEvent};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, nodefault};
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	__property Vcl::Stdctrls::TListBoxStyle Style = {read=FStyle, write=SetStyle, default=2};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElHTMLListBox(HWND ParentWindow) : Elactrls::TElAdvancedListBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TCustomElHTMLComboBox : public Elactrls::TElAdvancedComboBox
{
	typedef Elactrls::TElAdvancedComboBox inherited;
	
private:
	System::Classes::TNotifyEvent FDummyEvent;
	Htmlrender::TElHTMLRender* FRender;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	bool FIsHTML;
	
protected:
	Vcl::Stdctrls::TComboBoxStyle FStyle;
	virtual void __fastcall MeasureItem(int Index, int &Height);
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	virtual void __fastcall TriggerLinkClickEvent(System::TObject* Sender, Elstrutils::TElFString HRef);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	HIDESBASE void __fastcall SetStyle(Vcl::Stdctrls::TComboBoxStyle Value);
	virtual void __fastcall SetIsHTML(bool Value);
	
public:
	__fastcall virtual TCustomElHTMLComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElHTMLComboBox();
	virtual void __fastcall Loaded();
	
__published:
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property System::Classes::TNotifyEvent OnDrawItem = {read=FDummyEvent};
	__property System::Classes::TNotifyEvent OnMeasureItem = {read=FDummyEvent};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, nodefault};
	__property Vcl::Stdctrls::TComboBoxStyle Style = {read=FStyle, write=SetStyle, default=4};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElHTMLComboBox(HWND ParentWindow) : Elactrls::TElAdvancedComboBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElHTMLListBox : public TCustomElHTMLListBox
{
	typedef TCustomElHTMLListBox inherited;
	
public:
	/* TCustomElHTMLListBox.Create */ inline __fastcall virtual TElHTMLListBox(System::Classes::TComponent* AOwner) : TCustomElHTMLListBox(AOwner) { }
	/* TCustomElHTMLListBox.Destroy */ inline __fastcall virtual ~TElHTMLListBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHTMLListBox(HWND ParentWindow) : TCustomElHTMLListBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElHTMLComboBox : public TCustomElHTMLComboBox
{
	typedef TCustomElHTMLComboBox inherited;
	
public:
	/* TCustomElHTMLComboBox.Create */ inline __fastcall virtual TElHTMLComboBox(System::Classes::TComponent* AOwner) : TCustomElHTMLComboBox(AOwner) { }
	/* TCustomElHTMLComboBox.Destroy */ inline __fastcall virtual ~TElHTMLComboBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHTMLComboBox(HWND ParentWindow) : TCustomElHTMLComboBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Htmllbx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_HTMLLBX)
using namespace Htmllbx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// HtmllbxHPP
