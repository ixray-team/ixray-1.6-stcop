// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCurrEdit.pas' rev: 34.00 (Windows)

#ifndef ElcurreditHPP
#define ElcurreditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <System.Math.hpp>
#include <Vcl.Menus.hpp>
#include <ElSndMap.hpp>
#include <ElTools.hpp>
#include <ElACtrls.hpp>
#include <ElPopBtn.hpp>
#include <ElEdits.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElXPThemedControl.hpp>
#include <ElStrUtils.hpp>
#include <ElVCLUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcurredit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElCurrPartEdit;
class DELPHICLASS TElCurrencyEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElCurrPartEdit : public Eledits::TElEdit
{
	typedef Eledits::TElEdit inherited;
	
private:
	bool FIsIntegerPart;
	System::Classes::TNotifyEvent OnPoint;
	System::Classes::TNotifyEvent OnLeftPoint;
	
protected:
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	virtual void __fastcall CreateWindowHandle(const Vcl::Controls::TCreateParams &Params);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
public:
	/* TCustomElEdit.Create */ inline __fastcall virtual TElCurrPartEdit(System::Classes::TComponent* AOwner) : Eledits::TElEdit(AOwner) { }
	/* TCustomElEdit.Destroy */ inline __fastcall virtual ~TElCurrPartEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElCurrPartEdit(HWND ParentWindow) : Eledits::TElEdit(ParentWindow) { }
	
};


typedef TElCurrPartEdit IntEditClass;

enum DECLSPEC_DENUM TElCurrencySymbolPosition : unsigned char { ecsPosLeft, ecsPosRight };

class PASCALIMPLEMENTATION TElCurrencyEdit : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
protected:
	System::Currency FAbsValue;
	System::Byte FDecimalPlaces;
	bool FUseSystemDecimalPlaces;
	Elstrutils::TElFString FCurrencySymbol;
	TElCurrencySymbolPosition FCurrencySymbolPosition;
	bool FUseSystemCurrencySymbol;
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	bool FFlat;
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FMouseOver;
	bool FModified;
	int FDWidth;
	int FSWidth;
	int FBWidth;
	int FSignWidth;
	int FDSWidth;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	Elvclutils::TElBorderSides FBorderSides;
	System::StaticArray<TElCurrPartEdit*, 2> FPartEditors;
	System::StaticArray<Elpopbtn::TElGraphicButton*, 1> FButtons;
	System::StaticArray<System::Classes::TNotifyEvent, 1> FButtonClicks;
	bool FEnableSign;
	bool FSign;
	System::Uitypes::TColor FNegativeValueTextColor;
	System::Uitypes::TColor FNegativeSignColor;
	bool FHandleDialogKeys;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	System::WideString FHint;
	bool FChangeDisabledText;
	System::UnicodeString __fastcall FracValue(System::Currency AValue);
	void __fastcall SetDecimalPlaces(const System::Byte Value);
	void __fastcall SetUseSystemDecimalPlaces(const bool Value);
	int __fastcall FindPart(TElCurrPartEdit* Editor);
	void __fastcall SetFlat(const bool Value);
	void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall UpdateFrame();
	void __fastcall SetModified(bool Value);
	void __fastcall UpdateParts();
	void __fastcall UpdatePartsWidth();
	void __fastcall SetCurrencySymbol(const Elstrutils::TElFString Value);
	void __fastcall SetUseSystemCurrencySymbol(const bool Value);
	void __fastcall SetCurrencySymbolPosition(const TElCurrencySymbolPosition Value);
	Elstrutils::TElFString __fastcall GetButtonCaption(int Index);
	System::Uitypes::TColor __fastcall GetButtonColor(int Index);
	bool __fastcall GetButtonDown(int Index);
	bool __fastcall GetButtonEnabled(int Index);
	bool __fastcall GetButtonFlat(int Index);
	bool __fastcall GetButtonVisible(int Index);
	int __fastcall GetButtonWidth(int Index);
	bool __fastcall GetButtonUseIcon(int Index);
	Vcl::Graphics::TBitmap* __fastcall GetButtonGlyph(int Index);
	System::UnicodeString __fastcall GetButtonHint(int Index);
	Vcl::Graphics::TIcon* __fastcall GetButtonIcon(int Index);
	int __fastcall GetButtonNumGlyphs(int Index);
	Elpopbtn::TPopupPlace __fastcall GetButtonPopupPlace(int Index);
	Vcl::Menus::TPopupMenu* __fastcall GetButtonPullDownMenu(int Index);
	bool __fastcall GetButtonUseImageList(int Index);
	System::Classes::TNotifyEvent __fastcall GetOnButtonClick(int Index);
	void __fastcall SetButtonCaption(int Index, Elstrutils::TElFString Value);
	void __fastcall SetButtonColor(const int Index, const System::Uitypes::TColor Value);
	void __fastcall SetButtonDown(const int Index, const bool Value);
	void __fastcall SetButtonEnabled(const int Index, const bool Value);
	void __fastcall SetButtonFlat(const int Index, const bool Value);
	void __fastcall SetButtonVisible(const int Index, const bool Value);
	void __fastcall SetButtonWidth(const int Index, const int Value);
	void __fastcall SetButtonUseIcon(const int Index, const bool Value);
	void __fastcall SetButtonGlyph(const int Index, Vcl::Graphics::TBitmap* const Value);
	void __fastcall SetButtonHint(const int Index, const System::UnicodeString Value);
	void __fastcall SetButtonIcon(const int Index, Vcl::Graphics::TIcon* const Value);
	void __fastcall SetButtonNumGlyphs(const int Index, const int Value);
	void __fastcall SetButtonPopupPlace(const int Index, const Elpopbtn::TPopupPlace Value);
	void __fastcall SetButtonPullDownMenu(const int Index, Vcl::Menus::TPopupMenu* const Value);
	void __fastcall SetButtonUseImageList(const int Index, const bool Value);
	void __fastcall SetOnButtonClick(const int Index, const System::Classes::TNotifyEvent Value);
	Vcl::Controls::TImageList* __fastcall GetButtonDisabledImages(int Index);
	Vcl::Controls::TImageList* __fastcall GetButtonDownImages(int Index);
	Vcl::Controls::TImageList* __fastcall GetButtonHotImages(int Index);
	Vcl::Controls::TImageList* __fastcall GetButtonImageList(int Index);
	Elvclutils::TImageIndex __fastcall GetButtonImageIndex(int Index);
	void __fastcall SetButtonDisabledImages(const int Index, Vcl::Controls::TImageList* const Value);
	void __fastcall SetButtonDownImages(const int Index, Vcl::Controls::TImageList* const Value);
	void __fastcall SetButtonHotImages(const int Index, Vcl::Controls::TImageList* const Value);
	void __fastcall SetButtonImageList(const int Index, Vcl::Controls::TImageList* const Value);
	void __fastcall SetButtonImageIndex(const int Index, const Elvclutils::TImageIndex Value);
	void __fastcall ButtonClickTransfer(System::TObject* Sender);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	void __fastcall SetEnableSign(const bool Value);
	System::Currency __fastcall GetSignValue();
	void __fastcall SetSign(const bool Value);
	void __fastcall SetSignValue(const System::Currency Value);
	void __fastcall SetAutoSelect(const bool Value);
	bool __fastcall GetAutoSelect();
	void __fastcall SetNegativeSignColor(const System::Uitypes::TColor Value);
	void __fastcall SetNegativeValueTextColor(const System::Uitypes::TColor Value);
	void __fastcall SetupPartsFont();
	bool __fastcall GetReadOnly();
	void __fastcall SetReadOnly(const bool Value);
	void __fastcall OnEditorClick(System::TObject* Sender);
	void __fastcall OnEditorDblClick(System::TObject* Sender);
	void __fastcall OnEditorEndDrag(System::TObject* Sender, System::TObject* Target, int X, int Y);
	void __fastcall OnEditorKeyUp(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall OnEditorMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall OnEditorMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall OnEditorMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall OnEditorStartDrag(System::TObject* Sender, Vcl::Controls::TDragObject* &DragObject);
	void __fastcall OnEditorKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall OnEditorDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall OnEditorDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	bool __fastcall StoreCurrencySymbol();
	bool __fastcall StoreDecimalPlaces();
	void __fastcall SetAbsValue(const System::Currency Value);
	void __fastcall DrawFlatBorder(HDC DC);
	virtual void __fastcall AdjustEditorPositions();
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMWinIniChange(Winapi::Messages::TMessage &Msg);
	DYNAMIC void __fastcall DoOnChange();
	DYNAMIC void __fastcall DoMouseEnter();
	DYNAMIC void __fastcall DoMouseLeave();
	virtual void __fastcall CreateWindowHandle(const Vcl::Controls::TCreateParams &Params);
	void __fastcall OnEditorChange(System::TObject* Sender);
	void __fastcall OnEditorEnter(System::TObject* Sender);
	void __fastcall OnEditorExit(System::TObject* Sender);
	void __fastcall OnEditorPoint(System::TObject* Sender);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	void __fastcall OnEditorLeftPoint(System::TObject* Sender);
	void __fastcall OnEditorKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual System::WideString __fastcall GetThemedClassName();
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	void __fastcall SetChangeDisabledText(bool Value);
	
public:
	__fastcall virtual TElCurrencyEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElCurrencyEdit();
	virtual void __fastcall Paint();
	virtual void __fastcall Loaded();
	__property bool MouseOver = {read=FMouseOver, nodefault};
	__property bool Modified = {read=FModified, write=SetModified, nodefault};
	__property System::Currency AbsoluteValue = {read=FAbsValue, write=SetAbsValue};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property bool AutoSelect = {read=GetAutoSelect, write=SetAutoSelect, default=1};
	__property System::Currency Value = {read=GetSignValue, write=SetSignValue};
	__property bool Sign = {read=FSign, write=SetSign, stored=false, nodefault};
	__property bool EnableSign = {read=FEnableSign, write=SetEnableSign, default=0};
	__property System::Uitypes::TColor NegativeSignColor = {read=FNegativeSignColor, write=SetNegativeSignColor, default=536870911};
	__property System::Uitypes::TColor NegativeValueTextColor = {read=FNegativeValueTextColor, write=SetNegativeValueTextColor, default=536870911};
	__property System::Byte DecimalPlaces = {read=FDecimalPlaces, write=SetDecimalPlaces, stored=StoreDecimalPlaces, nodefault};
	__property bool UseSystemDecimalPlaces = {read=FUseSystemDecimalPlaces, write=SetUseSystemDecimalPlaces, default=1};
	__property Elstrutils::TElFString CurrencySymbol = {read=FCurrencySymbol, write=SetCurrencySymbol, stored=StoreCurrencySymbol};
	__property TElCurrencySymbolPosition CurrencySymbolPosition = {read=FCurrencySymbolPosition, write=SetCurrencySymbolPosition, stored=StoreCurrencySymbol, default=1};
	__property bool UseSystemCurrencySymbol = {read=FUseSystemCurrencySymbol, write=SetUseSystemCurrencySymbol, default=0};
	__property Elstrutils::TElFString ButtonCaption = {read=GetButtonCaption, write=SetButtonCaption, index=0};
	__property System::Uitypes::TColor ButtonColor = {read=GetButtonColor, write=SetButtonColor, index=0, default=-16777201};
	__property bool ButtonDown = {read=GetButtonDown, write=SetButtonDown, index=0, default=0};
	__property bool ButtonEnabled = {read=GetButtonEnabled, write=SetButtonEnabled, index=0, default=1};
	__property bool ButtonFlat = {read=GetButtonFlat, write=SetButtonFlat, index=0, default=0};
	__property bool ButtonUseIcon = {read=GetButtonUseIcon, write=SetButtonUseIcon, index=0, default=0};
	__property bool ButtonVisible = {read=GetButtonVisible, write=SetButtonVisible, index=0, default=0};
	__property int ButtonWidth = {read=GetButtonWidth, write=SetButtonWidth, index=0, default=15};
	__property Vcl::Graphics::TBitmap* ButtonGlyph = {read=GetButtonGlyph, write=SetButtonGlyph, index=0};
	__property System::UnicodeString ButtonHint = {read=GetButtonHint, write=SetButtonHint, index=0};
	__property Vcl::Graphics::TIcon* ButtonIcon = {read=GetButtonIcon, write=SetButtonIcon, index=0};
	__property int ButtonNumGlyphs = {read=GetButtonNumGlyphs, write=SetButtonNumGlyphs, index=0, default=1};
	__property Elpopbtn::TPopupPlace ButtonPopupPlace = {read=GetButtonPopupPlace, write=SetButtonPopupPlace, index=0, nodefault};
	__property Vcl::Menus::TPopupMenu* ButtonPullDownMenu = {read=GetButtonPullDownMenu, write=SetButtonPullDownMenu, index=0};
	__property bool ButtonUseImageList = {read=GetButtonUseImageList, write=SetButtonUseImageList, index=0, default=0};
	__property Vcl::Controls::TImageList* ButtonImages = {read=GetButtonImageList, write=SetButtonImageList, index=0};
	__property Vcl::Controls::TImageList* ButtonDownImages = {read=GetButtonDownImages, write=SetButtonDownImages, index=0};
	__property Vcl::Controls::TImageList* ButtonHotImages = {read=GetButtonHotImages, write=SetButtonHotImages, index=0};
	__property Vcl::Controls::TImageList* ButtonDisabledImages = {read=GetButtonDisabledImages, write=SetButtonDisabledImages, index=0};
	__property Elvclutils::TImageIndex ButtonImageIndex = {read=GetButtonImageIndex, write=SetButtonImageIndex, index=0, nodefault};
	__property System::Classes::TNotifyEvent OnButtonClick = {read=GetOnButtonClick, write=SetOnButtonClick, index=0};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property bool HandleDialogKeys = {read=FHandleDialogKeys, write=FHandleDialogKeys, nodefault};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property bool ChangeDisabledText = {read=FChangeDisabledText, write=SetChangeDisabledText, default=0};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElCurrencyEdit(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 CURRBTNCOUNT = System::Int8(0x1);
static const System::Int8 CURRBTNMIN = System::Int8(0x0);
static const System::Int8 CURRBTNMAX = System::Int8(0x0);
}	/* namespace Elcurredit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCURREDIT)
using namespace Elcurredit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcurreditHPP
