// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCombos.pas' rev: 34.00 (Windows)

#ifndef ElcombosHPP
#define ElcombosHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <ElACtrls.hpp>
#include <Vcl.Forms.hpp>
#include <ElImgFrm.hpp>
#include <Vcl.Menus.hpp>
#include <ElTools.hpp>
#include <ElVCLUtils.hpp>
#include <Vcl.Buttons.hpp>
#include <ElPopBtn.hpp>
#include <ElUxTheme.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElEdits.hpp>
#include <ElListBox.hpp>
#include <ElStrUtils.hpp>
#include <ElFrmPers.hpp>
#include <System.UITypes.hpp>
#include <ElXPThemedControl.hpp>
#include <ElScrollBar.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcombos
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElComboButton;
class DELPHICLASS TElComboListBox;
class DELPHICLASS TElComboBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElComboButton : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
private:
	bool FFlat;
	bool FFocused;
	bool FMouseOver;
	bool FTransparent;
	System::Uitypes::TColor FArrowColor;
	Elpopbtn::TElButtonGlyph* FGlyph;
	bool FDrawFrame;
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	void __fastcall SetFocused(const bool Value);
	void __fastcall SetDown(const bool Value);
	void __fastcall SetTransparent(const bool Value);
	void __fastcall SetDrawFrame(bool Value);
	
protected:
	bool ExtGlyph;
	bool FDown;
	bool KeepColor;
	virtual void __fastcall DrawArrow(const System::Types::TRect &R);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Paint();
	void __fastcall GlyphChanged(System::TObject* Sender);
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	
public:
	__fastcall virtual TElComboButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElComboButton();
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property bool Flat = {read=FFlat, write=FFlat, nodefault};
	__property bool Down = {read=FDown, write=SetDown, nodefault};
	__property bool Focused = {read=FFocused, write=SetFocused, nodefault};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, nodefault};
	__property bool DrawFrame = {read=FDrawFrame, write=SetDrawFrame, default=1};
};


class PASCALIMPLEMENTATION TElComboListBox : public Ellistbox::TElListBox
{
	typedef Ellistbox::TElListBox inherited;
	
private:
	TElComboBox* FCombo;
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseActivate(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CNCommand(Winapi::Messages::TMessage &Msg);
	
protected:
	DYNAMIC void __fastcall ResetContent();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual Vcl::Graphics::TBitmap* __fastcall GetBackground();
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &R, Winapi::Windows::TOwnerDrawState State);
	virtual void __fastcall MeasureItem(int Index, int &Height);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	
public:
	__fastcall virtual TElComboListBox(System::Classes::TComponent* AOwner);
public:
	/* TCustomElListBox.Destroy */ inline __fastcall virtual ~TElComboListBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElComboListBox(HWND ParentWindow) : Ellistbox::TElListBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElComboBox : public Eledits::TCustomElEdit
{
	typedef Eledits::TCustomElEdit inherited;
	
protected:
	bool ChangeAllowed;
	int FDropDownWidth;
	bool FAutoCompletion;
	System::Classes::TShortCut FAltButtonShortcut;
	System::Classes::TShortCut FButtonShortcut;
	System::Classes::TAlignment FAltBtnAlign;
	int FAltBtnWidth;
	bool FBtnFlat;
	bool FBtnTransparent;
	TElComboButton* FAltButton;
	TElComboButton* FButton;
	int FDropDownCount;
	Vcl::Forms::TForm* FForm;
	int FItemIndex;
	TElComboListBox* FListBox;
	System::Classes::TNotifyEvent FOnAltBtnClick;
	System::Classes::TNotifyEvent FOnDropDown;
	System::Uitypes::TColor FSaveColor;
	bool FSaveFlat;
	bool FForcedText;
	bool FIgnoreItemIdx;
	bool FCanDrop;
	bool FDroppedDown;
	bool FAdjustDropDownPos;
	Vcl::Stdctrls::TComboBoxStyle FStyle;
	Vcl::Stdctrls::TDrawItemEvent FOnDrawItem;
	Vcl::Stdctrls::TMeasureItemEvent FOnMeasureItem;
	Vcl::Graphics::TCanvas* FEditCanvas;
	Vcl::Graphics::TCanvas* FCanvas;
	System::Classes::TAlignment FDropDownAlignment;
	int __fastcall GetItemHeight();
	void __fastcall SetItemHeight(int Value);
	virtual void __fastcall ComboWndProc(Winapi::Messages::TMessage &Message, HWND ComboWnd, void * ComboProc);
	bool __fastcall GetListTransparentSelection();
	void __fastcall SetListTransparentSelection(bool Value);
	void __fastcall SetDropDownWidth(const int Value);
	bool __fastcall GetBtnDrawFrame();
	void __fastcall SetBtnDrawFrame(bool Value);
	bool __fastcall GetAltBtnDrawFrame();
	void __fastcall SetAltBtnDrawFrame(bool Value);
	MESSAGE void __fastcall CMCancelMode(Vcl::Controls::TCMCancelMode &Msg);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMMButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMContextMenu(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Msg);
	MESSAGE void __fastcall EMSetReadOnly(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Message);
	void __fastcall ButtonClick(System::TObject* Sender);
	void __fastcall AltButtonClick(System::TObject* Sender);
	System::Uitypes::TColor __fastcall GetBtnColor();
	void __fastcall GetDropDownValue();
	bool __fastcall GetDroppedDown();
	Elunicodestrings::TElWideStrings* __fastcall GetItems();
	System::Uitypes::TColor __fastcall GetListColor();
	bool __fastcall GetListInvertSelection();
	bool __fastcall GetSorted();
	bool __fastcall GetUseBackground();
	void __fastcall ListBoxClick(System::TObject* Sender);
	void __fastcall SetBtnColor(const System::Uitypes::TColor Value);
	void __fastcall SetBtnTransparent(const bool Value);
	void __fastcall SetDropDownCount(const int Value);
	void __fastcall SetDroppedDown(const bool Value);
	void __fastcall SetCanDrop(const bool Value);
	virtual void __fastcall SetEditRect(const System::Types::TRect &Value);
	void __fastcall SetItemIndex(const int Value);
	void __fastcall SetItems(Elunicodestrings::TElWideStrings* const Value);
	void __fastcall SetListColor(const System::Uitypes::TColor Value);
	void __fastcall SetListInvertSelection(const bool Value);
	void __fastcall SetSorted(const bool Value);
	HIDESBASE void __fastcall SetUseBackground(const bool Value);
	int __fastcall GetDroppedIndex();
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
	virtual void __fastcall Paint();
	virtual void __fastcall MeasureItem(int Index, int &Height);
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &R, Winapi::Windows::TOwnerDrawState State);
	Elimgfrm::TElImageForm* __fastcall GetListImageForm();
	void __fastcall SetListImageForm(Elimgfrm::TElImageForm* newValue);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	HIDESBASE MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall DoDropDown();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall Loaded();
	virtual void __fastcall SetBtnFlat(bool newValue);
	virtual System::Uitypes::TColor __fastcall GetBtnArrowColor();
	virtual void __fastcall SetBtnArrowColor(System::Uitypes::TColor newValue);
	System::Uitypes::TColor __fastcall GetListSelectedColor();
	void __fastcall SetListSelectedColor(System::Uitypes::TColor newValue);
	System::Uitypes::TColor __fastcall GetAltBtnColor();
	void __fastcall SetAltBtnColor(System::Uitypes::TColor Value);
	bool __fastcall GetAltBtnTransparent();
	void __fastcall SetAltBtnTransparent(bool Value);
	bool __fastcall GetAltBtnFlat();
	void __fastcall SetAltBtnFlat(bool Value);
	Vcl::Graphics::TBitmap* __fastcall GetAltBtnGlyph();
	void __fastcall SetAltBtnGlyph(Vcl::Graphics::TBitmap* Value);
	void __fastcall SetAltBtnWidth(int Value);
	bool __fastcall GetAltBtnVisible();
	void __fastcall SetAltBtnVisible(bool Value);
	void __fastcall SetAltBtnAlign(System::Classes::TLeftRight Value);
	void __fastcall DoAutoComplete();
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE void __fastcall SetReadOnly(bool Value);
	virtual void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	virtual void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	virtual void __fastcall SetFlat(const bool Value);
	HIDESBASE MESSAGE void __fastcall CMDialogKey(Winapi::Messages::TWMKey &Message);
	virtual void __fastcall DestroyWnd();
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Message);
	bool __fastcall GetShowLineHint();
	void __fastcall SetShowLineHint(bool Value);
	virtual void __fastcall SetStyle(Vcl::Stdctrls::TComboBoxStyle Value);
	HWND __fastcall GetEditHandle();
	void __fastcall SetDropDownAlignment(System::Classes::TAlignment Value);
	__property HWND EditHandle = {read=GetEditHandle, nodefault};
	
public:
	__fastcall virtual TElComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElComboBox();
	DYNAMIC void __fastcall Click();
	void __fastcall CloseUp(bool AcceptValue);
	void __fastcall DropDown();
	__property bool DroppedDown = {read=GetDroppedDown, write=SetDroppedDown, nodefault};
	__property int DroppedIndex = {read=GetDroppedIndex, nodefault};
	__property Ctl3D;
	__property ParentCtl3D = {default=1};
	__property Vcl::Graphics::TCanvas* Canvas = {read=FCanvas};
	
__published:
	__property ActiveBorderType = {default=1};
	__property Align = {default=0};
	__property Alignment;
	__property Background;
	__property BorderStyle;
	__property BorderSides;
	__property VertScrollBarStyles;
	__property HorzScrollBarStyles;
	__property UseCustomScrollBars;
	__property System::Uitypes::TColor BtnColor = {read=GetBtnColor, write=SetBtnColor, default=-16777201};
	__property bool BtnTransparent = {read=FBtnTransparent, write=SetBtnTransparent, default=0};
	__property bool BtnFlat = {read=FBtnFlat, write=SetBtnFlat, default=0};
	__property System::Uitypes::TColor BtnArrowColor = {read=GetBtnArrowColor, write=SetBtnArrowColor, nodefault};
	__property System::Uitypes::TColor AltBtnColor = {read=GetAltBtnColor, write=SetAltBtnColor, default=-16777201};
	__property bool AltBtnTransparent = {read=GetAltBtnTransparent, write=SetAltBtnTransparent, default=0};
	__property bool AltBtnFlat = {read=GetAltBtnFlat, write=SetAltBtnFlat, default=0};
	__property Vcl::Graphics::TBitmap* AltBtnGlyph = {read=GetAltBtnGlyph, write=SetAltBtnGlyph};
	__property bool AltBtnVisible = {read=GetAltBtnVisible, write=SetAltBtnVisible, default=0};
	__property int AltBtnWidth = {read=FAltBtnWidth, write=SetAltBtnWidth, nodefault};
	__property System::Classes::TLeftRight AltBtnPosition = {read=FAltBtnAlign, write=SetAltBtnAlign, default=1};
	__property System::Classes::TNotifyEvent OnAltButtonClick = {read=FOnAltBtnClick, write=FOnAltBtnClick};
	__property bool CanDrop = {read=FCanDrop, write=SetCanDrop, default=1};
	__property int DropDownCount = {read=FDropDownCount, write=SetDropDownCount, default=8};
	__property int DropDownWidth = {read=FDropDownWidth, write=SetDropDownWidth, default=-1};
	__property bool ListTransparentSelection = {read=GetListTransparentSelection, write=SetListTransparentSelection, default=0};
	__property bool BtnDrawFrame = {read=GetBtnDrawFrame, write=SetBtnDrawFrame, default=1};
	__property bool AutoCompletion = {read=FAutoCompletion, write=FAutoCompletion, nodefault};
	__property bool AltBtnDrawFrame = {read=GetAltBtnDrawFrame, write=SetAltBtnDrawFrame, default=1};
	__property Elunicodestrings::TElWideStrings* Items = {read=GetItems, write=SetItems};
	__property System::Uitypes::TColor ListColor = {read=GetListColor, write=SetListColor, default=-16777211};
	__property Elimgfrm::TElImageForm* ListImageForm = {read=GetListImageForm, write=SetListImageForm};
	__property bool ListInvertSelection = {read=GetListInvertSelection, write=SetListInvertSelection, default=0};
	__property System::Uitypes::TColor ListSelectedColor = {read=GetListSelectedColor, write=SetListSelectedColor, nodefault};
	__property bool Sorted = {read=GetSorted, write=SetSorted, default=0};
	__property bool UseBackground = {read=GetUseBackground, write=SetUseBackground, default=0};
	__property System::Classes::TNotifyEvent OnDropDown = {read=FOnDropDown, write=FOnDropDown};
	__property bool AdjustDropDownPos = {read=FAdjustDropDownPos, write=FAdjustDropDownPos, default=1};
	__property int ItemIndex = {read=FItemIndex, write=SetItemIndex, default=-1};
	__property AutoSelect = {default=0};
	__property AutoSize = {default=1};
	__property CharCase = {default=0};
	__property TopMargin = {default=1};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property RTLContent;
	__property PasswordChar = {default=0};
	__property Multiline = {default=0};
	__property WantTabs = {default=0};
	__property HandleDialogKeys = {default=0};
	__property HideSelection = {default=1};
	__property TabSpaces = {default=4};
	__property Lines = {stored=false};
	__property Color = {default=-16777211};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Flat = {default=0};
	__property Font;
	__property ImageForm;
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property InactiveBorderType = {default=3};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property MaxLength = {default=0};
	__property ParentColor = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ReadOnly = {write=SetReadOnly, default=0};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Text;
	__property Transparent;
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
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnStartDrag;
	__property System::Classes::TShortCut AltButtonShortcut = {read=FAltButtonShortcut, write=FAltButtonShortcut, nodefault};
	__property System::Classes::TShortCut ButtonShortcut = {read=FButtonShortcut, write=FButtonShortcut, nodefault};
	__property bool ShowLineHint = {read=GetShowLineHint, write=SetShowLineHint, default=0};
	__property Vcl::Stdctrls::TComboBoxStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property int ItemHeight = {read=GetItemHeight, write=SetItemHeight, nodefault};
	__property Vcl::Stdctrls::TDrawItemEvent OnDrawItem = {read=FOnDrawItem, write=FOnDrawItem};
	__property Vcl::Stdctrls::TMeasureItemEvent OnMeasureItem = {read=FOnMeasureItem, write=FOnMeasureItem};
	__property System::Classes::TAlignment DropDownAlignment = {read=FDropDownAlignment, write=SetDropDownAlignment, default=1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElComboBox(HWND ParentWindow) : Eledits::TCustomElEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elcombos */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCOMBOS)
using namespace Elcombos;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcombosHPP
