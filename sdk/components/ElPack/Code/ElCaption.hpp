// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCaption.pas' rev: 35.00 (Windows)

#ifndef ElcaptionHPP
#define ElcaptionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Buttons.hpp>
#include <ElTools.hpp>
#include <ElPopBtn.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElHook.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcaption
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElCaptionText;
class DELPHICLASS TElCaptionTexts;
class DELPHICLASS TElCaptionButton;
class DELPHICLASS TElCaptionButtons;
class DELPHICLASS TElFormCaption;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TElCaptionText : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::Uitypes::TColor FActiveColor;
	System::Uitypes::TColor FInactiveColor;
	Vcl::Graphics::TFont* FFont;
	bool FVisible;
	Elstrutils::TElFString FCaption;
	bool FOwnerStyle;
	Vcl::Buttons::TButtonLayout FLayout;
	Elpopbtn::TElButtonGlyph* FGlyph;
	System::Classes::TAlignment FAlign;
	void __fastcall SetOwnerStyle(bool newValue);
	void __fastcall SetActiveColor(System::Uitypes::TColor newValue);
	void __fastcall SetInactiveColor(System::Uitypes::TColor newValue);
	void __fastcall SetFont(Vcl::Graphics::TFont* newValue);
	void __fastcall SetVisible(bool newValue);
	void __fastcall SetCaption(Elstrutils::TElFString newValue);
	void __fastcall FontChange(System::TObject* Sender);
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetLayout(Vcl::Buttons::TButtonLayout newValue);
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall SetAlign(System::Classes::TAlignment newValue);
	
protected:
	virtual void __fastcall Paint(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	int __fastcall GetWidth(Vcl::Graphics::TCanvas* Canvas, int Height);
	
public:
	__fastcall virtual TElCaptionText(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElCaptionText();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::Uitypes::TColor ActiveColor = {read=FActiveColor, write=SetActiveColor, default=-16777207};
	__property System::Uitypes::TColor InactiveColor = {read=FInactiveColor, write=SetInactiveColor, default=-16777197};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property bool OwnerStyle = {read=FOwnerStyle, write=SetOwnerStyle, default=1};
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property Vcl::Buttons::TButtonLayout Layout = {read=FLayout, write=SetLayout, nodefault};
	__property System::Classes::TAlignment Align = {read=FAlign, write=SetAlign, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElCaptionTexts : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElCaptionText* operator[](int index) { return this->Items[index]; }
	
private:
	TElFormCaption* FCaption;
	
protected:
	TElCaptionText* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TElCaptionText* newValue);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	HIDESBASE TElCaptionText* __fastcall Add();
	__property TElCaptionText* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TElCaptionTexts(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TElCaptionTexts() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TElPaintBkgndType : unsigned char { pbtActive, pbtInactive, pbtAlways };

class PASCALIMPLEMENTATION TElCaptionButton : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::Classes::TAlignment FAlign;
	Elstrutils::TElFString FCaption;
	Elpopbtn::TElButtonGlyph* FGlyph;
	TElCaptionButtons* FButtons;
	bool FEnabled;
	bool FFixClick;
	bool FDown;
	bool FVisible;
	bool FOwnerStyle;
	System::Uitypes::TColor FActiveColor;
	System::Uitypes::TColor FInactiveColor;
	Vcl::Graphics::TFont* FFont;
	Vcl::Buttons::TButtonLayout FLayout;
	System::Types::TRect FBtnRect;
	System::Classes::TNotifyEvent FOnClick;
	System::Classes::TNotifyEvent FOnDblClick;
	void __fastcall SetLayout(Vcl::Buttons::TButtonLayout newValue);
	void __fastcall SetFont(Vcl::Graphics::TFont* newValue);
	void __fastcall SetOwnerStyle(bool newValue);
	void __fastcall SetActiveColor(System::Uitypes::TColor newValue);
	void __fastcall SetInactiveColor(System::Uitypes::TColor newValue);
	void __fastcall SetVisible(bool newValue);
	void __fastcall SetEnabled(bool newValue);
	void __fastcall SetDown(bool newValue);
	void __fastcall SetAlign(System::Classes::TAlignment newValue);
	void __fastcall SetCaption(Elstrutils::TElFString newValue);
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* newValue);
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall FontChange(System::TObject* Sender);
	
protected:
	virtual void __fastcall Paint(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	int __fastcall GetWidth(Vcl::Graphics::TCanvas* Canvas, int Height);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TElCaptionButton(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElCaptionButton();
	
__published:
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property Vcl::Buttons::TButtonLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property System::Classes::TAlignment Align = {read=FAlign, write=SetAlign, default=1};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property bool FixClick = {read=FFixClick, write=FFixClick, nodefault};
	__property bool Down = {read=FDown, write=SetDown, default=0};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property bool OwnerStyle = {read=FOwnerStyle, write=SetOwnerStyle, default=1};
	__property System::Uitypes::TColor ActiveColor = {read=FActiveColor, write=SetActiveColor, default=-16777198};
	__property System::Uitypes::TColor InactiveColor = {read=FInactiveColor, write=SetInactiveColor, default=-16777198};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property System::Classes::TNotifyEvent OnClick = {read=FOnClick, write=FOnClick};
	__property System::Classes::TNotifyEvent OnDblClick = {read=FOnDblClick, write=FOnDblClick};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElCaptionButtons : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElCaptionButton* operator[](int index) { return this->Items[index]; }
	
private:
	TElFormCaption* FCaption;
	
protected:
	TElCaptionButton* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TElCaptionButton* newValue);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	HIDESBASE TElCaptionButton* __fastcall Add();
	__property TElCaptionButton* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TElCaptionButtons(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TElCaptionButtons() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TElCaptionDrawEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect);

typedef void __fastcall (__closure *TCaptionButtonEvent)(System::TObject* Sender, TElCaptionButton* Button);

class PASCALIMPLEMENTATION TElFormCaption : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TElCaptionTexts* FTexts;
	Vcl::Graphics::TBitmap* FInactiveBitmap;
	TElCaptionButtons* FButtons;
	TElPaintBkgndType FPaintBkgnd;
	Vcl::Menus::TPopupMenu* FPopupMenu;
	Vcl::Graphics::TBitmap* FBitmap;
	Elhook::TElHook* FHook;
	bool FActive;
	System::Uitypes::TColor FActiveLeftColor;
	System::Uitypes::TColor FActiveRightColor;
	System::Uitypes::TColor FInactiveLeftColor;
	System::Uitypes::TColor FInactiveRightColor;
	Elvclutils::TElBkGndType FBackgroundType;
	int FNumColors;
	System::Classes::TAlignment FAlignment;
	Vcl::Forms::TForm* FForm;
	bool FSystemFont;
	Vcl::Graphics::TFont* FFont;
	Vcl::Graphics::TFont* Font2;
	bool FWndActive;
	HRGN FRegion;
	HRGN FSaveRegion;
	bool FClicked;
	bool FInBtn;
	TElCaptionButton* FBtnPressed;
	System::Types::TRect FBtnsRect;
	TElCaptionDrawEvent FOnDrawCaption;
	TCaptionButtonEvent FOnButtonClick;
	TCaptionButtonEvent FOnButtonDblClick;
	bool FPreventUpdate;
	NativeUInt FTheme;
	void __fastcall SetActive(bool newValue);
	void __fastcall SetActiveLeftColor(System::Uitypes::TColor newValue);
	void __fastcall SetActiveRightColor(System::Uitypes::TColor newValue);
	void __fastcall SetInactiveLeftColor(System::Uitypes::TColor newValue);
	void __fastcall SetBackgroundType(Elvclutils::TElBkGndType newValue);
	void __fastcall SetPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	void __fastcall SetNumColors(int newValue);
	void __fastcall SetAlignment(System::Classes::TAlignment newValue);
	void __fastcall SetBitmap(Vcl::Graphics::TBitmap* newValue);
	void __fastcall BitmapChange(System::TObject* Sender);
	void __fastcall OnBeforeHook(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall OnAfterHook(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall SetPaintBkgnd(TElPaintBkgndType newValue);
	void __fastcall SetInactiveRightColor(System::Uitypes::TColor newValue);
	void __fastcall SetButtons(TElCaptionButtons* newValue);
	void __fastcall SetInactiveBitmap(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetSystemFont(bool newValue);
	void __fastcall SetFont(Vcl::Graphics::TFont* newValue);
	void __fastcall FontChange(System::TObject* Sender);
	void __fastcall GetSystemFont();
	void __fastcall SetTexts(TElCaptionTexts* newValue);
	
protected:
	bool FUseXPThemes;
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	void __fastcall Update();
	virtual void __fastcall PaintCaption(Winapi::Messages::TMessage &Msg, int Step);
	virtual void __fastcall TriggerDrawCaptionEvent(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &Rect);
	virtual void __fastcall TriggerButtonClickEvent(TElCaptionButton* Button);
	virtual void __fastcall TriggerButtonDblClickEvent(TElCaptionButton* Button);
	void __fastcall SetUseXPThemes(bool Value);
	virtual void __fastcall CreateThemeHandle();
	virtual void __fastcall FreeThemeHandle();
	void __fastcall AllocThemes();
	
public:
	__fastcall virtual TElFormCaption(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElFormCaption();
	TElCaptionButton* __fastcall ButtonAtPos(int X, int Y);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	bool __fastcall IsThemeApplied();
	
__published:
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property System::Uitypes::TColor ActiveLeftColor = {read=FActiveLeftColor, write=SetActiveLeftColor, default=0};
	__property System::Uitypes::TColor ActiveRightColor = {read=FActiveRightColor, write=SetActiveRightColor, default=-16777214};
	__property System::Uitypes::TColor InactiveLeftColor = {read=FInactiveLeftColor, write=SetInactiveLeftColor, default=0};
	__property System::Uitypes::TColor InactiveRightColor = {read=FInactiveRightColor, write=SetInactiveRightColor, default=-16777213};
	__property Elvclutils::TElBkGndType BackgroundType = {read=FBackgroundType, write=SetBackgroundType, default=2};
	__property Vcl::Menus::TPopupMenu* PopupMenu = {read=FPopupMenu, write=SetPopupMenu};
	__property int NumColors = {read=FNumColors, write=SetNumColors, default=64};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property Vcl::Graphics::TBitmap* ActiveBitmap = {read=FBitmap, write=SetBitmap};
	__property TElPaintBkgndType PaintBkgnd = {read=FPaintBkgnd, write=SetPaintBkgnd, default=2};
	__property TElCaptionButtons* Buttons = {read=FButtons, write=SetButtons};
	__property Vcl::Graphics::TBitmap* InactiveBitmap = {read=FInactiveBitmap, write=SetInactiveBitmap};
	__property TElCaptionDrawEvent OnDrawCaption = {read=FOnDrawCaption, write=FOnDrawCaption};
	__property TCaptionButtonEvent OnButtonClick = {read=FOnButtonClick, write=FOnButtonClick};
	__property TCaptionButtonEvent OnButtonDblClick = {read=FOnButtonDblClick, write=FOnButtonDblClick};
	__property bool SystemFont = {read=FSystemFont, write=SetSystemFont, default=1};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property TElCaptionTexts* Texts = {read=FTexts, write=SetTexts};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elcaption */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCAPTION)
using namespace Elcaption;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcaptionHPP
