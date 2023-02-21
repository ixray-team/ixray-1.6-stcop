// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElACtrls.pas' rev: 35.00 (Windows)

#ifndef ElactrlsHPP
#define ElactrlsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <ElImgFrm.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElTools.hpp>
#include <ElVCLUtils.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elactrls
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElAdvancedMemo;
class DELPHICLASS TCustomElAdvancedEdit;
class DELPHICLASS TElAdvancedEdit;
class DELPHICLASS TElAdvancedListBox;
class DELPHICLASS TElAdvancedComboBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElAdvancedMemo : public Vcl::Stdctrls::TMemo
{
	typedef Vcl::Stdctrls::TMemo inherited;
	
private:
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Vcl::Graphics::TBitmap* FBackground;
	bool FFlat;
	bool FFlatFocusedScrollBars;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FMouseOver;
	bool FPainting;
	bool FPaintingTo;
	bool FTransparent;
	bool FUseBackground;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	Elvclutils::TElBorderSides FBorderSides;
	bool FHandleDialogKeys;
	NativeUInt FTheme;
	System::WideString FHint;
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall BackgroundChanged(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMParentColorChanged(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall CNCtlColorEdit(Winapi::Messages::TWMCtlColor &Msg);
	MESSAGE void __fastcall CNCtlColorStatic(Winapi::Messages::TWMCtlColor &Msg);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMMove(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	void __fastcall DrawBackground(HDC DC, const System::Types::TRect &R);
	void __fastcall DrawFlatBorder(HDC DC);
	void __fastcall DrawParentControl(HDC DC);
	void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* const Value);
	void __fastcall SetFlat(const bool Value);
	void __fastcall SetFlatFocusedScrollBars(const bool Value);
	void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetTransparent(const bool Value);
	void __fastcall SetUseBackground(const bool Value);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	
protected:
	bool FUseXPThemes;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	DYNAMIC void __fastcall Change();
	DYNAMIC void __fastcall DoMouseEnter();
	DYNAMIC void __fastcall DoMouseLeave();
	DYNAMIC void __fastcall DoPaint();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	void __fastcall SetUseXPThemes(bool Value);
	bool __fastcall IsThemeApplied();
	virtual void __fastcall FreeThemeHandle();
	virtual void __fastcall CreateThemeHandle();
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	System::WideString __fastcall GetThemedClassName();
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	
public:
	__fastcall virtual TElAdvancedMemo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElAdvancedMemo();
	__property NativeUInt Theme = {read=FTheme, nodefault};
	
__published:
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Align = {default=0};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property bool FlatFocusedScrollBars = {read=FFlatFocusedScrollBars, write=SetFlatFocusedScrollBars, default=0};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property bool UseBackground = {read=FUseBackground, write=SetUseBackground, default=0};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property bool HandleDialogKeys = {read=FHandleDialogKeys, write=FHandleDialogKeys, default=0};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property System::WideString Hint = {read=FHint, write=SetHint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElAdvancedMemo(HWND ParentWindow) : Vcl::Stdctrls::TMemo(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TCustomElAdvancedEdit : public Vcl::Stdctrls::TCustomEdit
{
	typedef Vcl::Stdctrls::TCustomEdit inherited;
	
private:
	Elvclutils::TElFlatBorderType FActiveBorderType;
	System::Classes::TAlignment FAlignment;
	Vcl::Graphics::TBitmap* FBackground;
	bool FFlat;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FMouseOver;
	bool FPainting;
	bool FPaintingTo;
	bool FReturnPressed;
	bool FTransparent;
	bool FUseBackground;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	bool FHandleDialogKeys;
	Elvclutils::TElBorderSides FBorderSides;
	NativeUInt FTheme;
	System::WideString FHint;
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall BackgroundChanged(System::TObject* Sender);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMParentColorChanged(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall CNCtlColorEdit(Winapi::Messages::TWMCtlColor &Msg);
	MESSAGE void __fastcall CNCtlColorStatic(Winapi::Messages::TWMCtlColor &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMMove(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	void __fastcall DrawBackground(HDC DC, const System::Types::TRect &R);
	void __fastcall DrawFlatBorder(HDC DC);
	void __fastcall DrawParentControl(HDC DC);
	void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	HIDESBASE void __fastcall SetAlignment(const System::Classes::TAlignment Value);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* const Value);
	void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetTransparent(const bool Value);
	void __fastcall SetUseBackground(const bool Value);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	
protected:
	bool FNoHandleEnter;
	System::WideChar FPasswordChar;
	bool FUseXPThemes;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	virtual void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetFlat(const bool Value);
	DYNAMIC void __fastcall Change();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	DYNAMIC void __fastcall DoMouseEnter();
	DYNAMIC void __fastcall DoMouseLeave();
	DYNAMIC void __fastcall DoPaint();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property bool UseBackground = {read=FUseBackground, write=SetUseBackground, default=0};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE void __fastcall SetPasswordChar(System::WideChar Value);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	virtual void __fastcall SetUseXPThemes(bool Value);
	bool __fastcall IsThemeApplied();
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	virtual void __fastcall FreeThemeHandle();
	virtual void __fastcall CreateThemeHandle();
	System::WideString __fastcall GetThemedClassName();
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMPaste(Winapi::Messages::TMessage &Message);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	
public:
	__fastcall virtual TCustomElAdvancedEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElAdvancedEdit();
	__property bool MouseOver = {read=FMouseOver, nodefault};
	__property NativeUInt Theme = {read=FTheme, nodefault};
	
__published:
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property bool HandleDialogKeys = {read=FHandleDialogKeys, write=FHandleDialogKeys, default=0};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property System::WideChar PasswordChar = {read=FPasswordChar, write=SetPasswordChar, stored=false, default=0};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property System::WideString Hint = {read=FHint, write=SetHint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElAdvancedEdit(HWND ParentWindow) : Vcl::Stdctrls::TCustomEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElAdvancedEdit : public TCustomElAdvancedEdit
{
	typedef TCustomElAdvancedEdit inherited;
	
__published:
	__property ActiveBorderType = {default=1};
	__property Align = {default=0};
	__property Alignment = {default=0};
	__property AutoSelect = {default=1};
	__property AutoSize = {default=1};
	__property Background;
	__property BorderStyle = {default=1};
	__property CharCase = {default=0};
	__property Color = {default=-16777211};
	__property Cursor = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Flat = {default=0};
	__property Font;
	__property HideSelection = {default=1};
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property InactiveBorderType = {default=3};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property MaxLength = {default=0};
	__property OEMConvert = {default=0};
	__property ParentColor = {default=0};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PasswordChar = {default=0};
	__property PopupMenu;
	__property ReadOnly = {default=0};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property Text = {default=0};
	__property Transparent = {default=0};
	__property UseBackground = {default=0};
	__property Visible = {default=1};
	__property OnChange;
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
	__property OnMouseEnter;
	__property OnMouseMove;
	__property OnMouseLeave;
	__property OnMouseUp;
	__property OnStartDrag;
public:
	/* TCustomElAdvancedEdit.Create */ inline __fastcall virtual TElAdvancedEdit(System::Classes::TComponent* AOwner) : TCustomElAdvancedEdit(AOwner) { }
	/* TCustomElAdvancedEdit.Destroy */ inline __fastcall virtual ~TElAdvancedEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElAdvancedEdit(HWND ParentWindow) : TCustomElAdvancedEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElAdvancedListBox : public Vcl::Stdctrls::TListBox
{
	typedef Vcl::Stdctrls::TListBox inherited;
	
private:
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Vcl::Graphics::TBitmap* FBackground;
	bool FFlat;
	bool FFlatFocusedScrollBars;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FInvertSelection;
	int FLastTopIndex;
	bool FMouseOver;
	System::Uitypes::TColor FSelectedColor;
	Vcl::Graphics::TFont* FSelectedFont;
	bool FTransparent;
	bool FUseBackground;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	bool FInVScroll;
	bool FInHScroll;
	bool FTransparentSelection;
	Elvclutils::TElBorderSides FBorderSides;
	bool FShowLineHint;
	int FCurHintItem;
	Vcl::Stdctrls::TListBoxStyle FStyle;
	int FMaxWidth;
	bool FHorizontalScroll;
	Vcl::Extctrls::TTimer* FHintTimer;
	Vcl::Controls::THintWindow* FHintWnd;
	System::Classes::TWndMethod FHintWndProc;
	NativeUInt FTheme;
	System::WideString FHint;
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall BackgroundChanged(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMParentColorChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMParentFontChanged(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall LBGetTopIndex(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMNCMouseMove(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	void __fastcall DrawBackground(HDC DC, const System::Types::TRect &R);
	void __fastcall DrawBackgroundEx(HDC DC, const System::Types::TRect &R, const System::Types::TRect &SubR);
	void __fastcall DrawFlatBorder(HDC DC, bool HDragging, bool VDragging);
	void __fastcall DrawParentControl(HDC DC);
	void __fastcall DrawParentControlEx(HDC DC, const System::Types::TRect &R);
	void __fastcall IntMouseMove(short XPos, short YPos);
	void __fastcall SelectedFontChanged(System::TObject* Sender);
	void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* const Value);
	void __fastcall SetFlat(const bool Value);
	void __fastcall SetFlatFocusedScrollBars(const bool Value);
	void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetInvertSelection(const bool Value);
	void __fastcall SetSelectedColor(const System::Uitypes::TColor Value);
	void __fastcall SetSelectedFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetTransparent(const bool Value);
	void __fastcall SetUseBackground(const bool Value);
	void __fastcall SetTransparentSelection(bool Value);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	HIDESBASE void __fastcall SetStyle(Vcl::Stdctrls::TListBoxStyle Value);
	void __fastcall SetHorizontalScroll(bool Value);
	void __fastcall ResetHorizontalExtent();
	void __fastcall SetHorizontalExtent();
	void __fastcall CancelLineHint();
	void __fastcall OnLineHintTimer(System::TObject* Sender);
	void __fastcall HintWndProc(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	
protected:
	bool FUseXPThemes;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	virtual int __fastcall GetItemWidth(int Index);
	virtual int __fastcall GetParentCtlWidth();
	virtual int __fastcall GetParentCtlHeight();
	virtual System::Types::TPoint __fastcall RealScreenToClient(const System::Types::TPoint &APoint);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &R, Winapi::Windows::TOwnerDrawState State);
	virtual Vcl::Graphics::TBitmap* __fastcall GetBackground();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual Vcl::Controls::THintWindow* __fastcall CreateHintWindow();
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	void __fastcall SetUseXPThemes(bool Value);
	bool __fastcall IsThemeApplied();
	virtual void __fastcall FreeThemeHandle();
	virtual void __fastcall CreateThemeHandle();
	System::WideString __fastcall GetThemedClassName();
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	
public:
	__fastcall virtual TElAdvancedListBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElAdvancedListBox();
	__property NativeUInt Theme = {read=FTheme, nodefault};
	
__published:
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Vcl::Graphics::TBitmap* Background = {read=GetBackground, write=SetBackground};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property bool FlatFocusedScrollBars = {read=FFlatFocusedScrollBars, write=SetFlatFocusedScrollBars, default=0};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool InvertSelection = {read=FInvertSelection, write=SetInvertSelection, default=0};
	__property System::Uitypes::TColor SelectedColor = {read=FSelectedColor, write=SetSelectedColor, default=-16777203};
	__property Vcl::Graphics::TFont* SelectedFont = {read=FSelectedFont, write=SetSelectedFont};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property bool UseBackground = {read=FUseBackground, write=SetUseBackground, default=0};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property bool TransparentSelection = {read=FTransparentSelection, write=SetTransparentSelection, default=0};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property bool ShowLineHint = {read=FShowLineHint, write=FShowLineHint, default=0};
	__property Vcl::Stdctrls::TListBoxStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property bool HorizontalScroll = {read=FHorizontalScroll, write=SetHorizontalScroll, nodefault};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property System::WideString Hint = {read=FHint, write=SetHint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElAdvancedListBox(HWND ParentWindow) : Vcl::Stdctrls::TListBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElAdvancedComboBox : public Vcl::Stdctrls::TComboBox
{
	typedef Vcl::Stdctrls::TComboBox inherited;
	
private:
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FFlat;
	Vcl::Graphics::TCanvas* BtnCanvas;
	int FDropDownWidth;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	bool FAutoCompletion;
	void *FListInstance;
	void *FEditInstance;
	int FSaveEditWndProc;
	int FSaveListWndProc;
	System::Classes::TWndMethod FListWindowProc;
	System::Classes::TWndMethod FEditWindowProc;
	bool FHorizontalScroll;
	bool FInHScroll;
	bool FInVScroll;
	NativeUInt FTheme;
	System::WideString FHint;
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetFlat(bool newValue);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TMessage &Message);
	bool __fastcall IsFocused();
	void __fastcall SetActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetInactiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetHorizontalScroll(bool Value);
	void __fastcall SetHorizontalExtent();
	void __fastcall ResetHorizontalExtent();
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Msg);
	
protected:
	HWND FListHandle;
	HWND FEditHandle;
	bool FMouseOver;
	int FHorzPos;
	int FMaxWidth;
	bool FBtnFlat;
	bool FBtnTransparent;
	bool FUseXPThemes;
	bool FBtnThinFrame;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	bool FHandleDialogKeys;
	void __fastcall DrawFlatBorder(bool DrawButton);
	void __fastcall UpdateFrame();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	HIDESBASE virtual void __fastcall ListWndProc(Winapi::Messages::TMessage &Message);
	HIDESBASE virtual void __fastcall EditWndProc(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetBtnFlat(bool Value);
	void __fastcall SetBtnTransparent(bool Value);
	virtual int __fastcall GetItemWidth(int Index);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	void __fastcall SetUseXPThemes(bool Value);
	bool __fastcall IsThemeApplied();
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	virtual void __fastcall FreeThemeHandle();
	virtual void __fastcall CreateThemeHandle();
	System::WideString __fastcall GetThemedClassName();
	void __fastcall SetBtnThinFrame(bool Value);
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall DoAutoComplete();
	
public:
	__fastcall virtual TElAdvancedComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElAdvancedComboBox();
	__property HWND ListHandle = {read=FListHandle, nodefault};
	__property HWND EditHandle = {read=FEditHandle, nodefault};
	__property System::Classes::TWndMethod ListWindowProc = {read=FListWindowProc, write=FListWindowProc};
	__property System::Classes::TWndMethod EditWindowProc = {read=FEditWindowProc, write=FEditWindowProc};
	__property NativeUInt Theme = {read=FTheme, nodefault};
	
__published:
	__property Align = {default=0};
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool BtnFlat = {read=FBtnFlat, write=SetBtnFlat, default=0};
	__property bool BtnTransparent = {read=FBtnTransparent, write=SetBtnTransparent, default=0};
	__property bool HorizontalScroll = {read=FHorizontalScroll, write=SetHorizontalScroll, nodefault};
	__property ItemIndex = {default=-1};
	__property ItemHeight;
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property bool BtnThinFrame = {read=FBtnThinFrame, write=SetBtnThinFrame, default=1};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property int DropDownWidth = {read=FDropDownWidth, write=FDropDownWidth, default=0};
	__property bool AutoCompletion = {read=FAutoCompletion, write=FAutoCompletion, nodefault};
	__property bool HandleDialogKeys = {read=FHandleDialogKeys, write=FHandleDialogKeys, nodefault};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElAdvancedComboBox(HWND ParentWindow) : Vcl::Stdctrls::TComboBox(ParentWindow) { }
	
};


typedef TCustomElAdvancedEdit TCustomElFlatEdit;

typedef TElAdvancedEdit TElFlatEdit;

typedef TElAdvancedMemo TElFlatMemo;

typedef TElAdvancedListBox TElFlatListBox;

typedef TElAdvancedComboBox TElFlatComboBox;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elactrls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELACTRLS)
using namespace Elactrls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElactrlsHPP
