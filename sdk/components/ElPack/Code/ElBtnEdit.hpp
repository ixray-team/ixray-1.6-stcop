// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElBtnEdit.pas' rev: 35.00 (Windows)

#ifndef ElbtneditHPP
#define ElbtneditHPP

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
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElPopBtn.hpp>
#include <ElVCLUtils.hpp>
#include <ElImgFrm.hpp>
#include <ElSndMap.hpp>
#include <ElTools.hpp>
#include <ElEdits.hpp>
#include <ElXPThemedControl.hpp>
#include <ElStrUtils.hpp>
#include <ElUnicodeStrings.hpp>
#include <System.UITypes.hpp>
#include <ElScrollBar.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elbtnedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomElButtonEdit;
class DELPHICLASS TElButtonEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCustomElButtonEdit : public Eledits::TCustomElEdit
{
	typedef Eledits::TCustomElEdit inherited;
	
private:
	System::Classes::TNotifyEvent FOnAltButtonClick;
	System::Classes::TShortCut FButtonShortcut;
	System::Classes::TShortCut FAltButtonShortcut;
	System::Classes::TNotifyEvent FOnButtonClick;
	void __fastcall SetButtonClickSound(Elsndmap::TElSoundName &newValue);
	Elsndmap::TElSoundName __fastcall GetButtonClickSound();
	void __fastcall SetButtonDownSound(Elsndmap::TElSoundName &newValue);
	Elsndmap::TElSoundName __fastcall GetButtonDownSound();
	void __fastcall SetButtonSoundMap(Elsndmap::TElSoundMap* newValue);
	Elsndmap::TElSoundMap* __fastcall GetButtonSoundMap();
	void __fastcall SetButtonUpSound(Elsndmap::TElSoundName &newValue);
	Elsndmap::TElSoundName __fastcall GetButtonUpSound();
	void __fastcall SetAltButtonClickSound(Elsndmap::TElSoundName &newValue);
	Elsndmap::TElSoundName __fastcall GetAltButtonClickSound();
	void __fastcall SetAltButtonUpSound(Elsndmap::TElSoundName &newValue);
	Elsndmap::TElSoundName __fastcall GetAltButtonUpSound();
	void __fastcall SetAltButtonSoundMap(Elsndmap::TElSoundMap* newValue);
	Elsndmap::TElSoundMap* __fastcall GetAltButtonSoundMap();
	void __fastcall SetButtonColor(System::Uitypes::TColor newValue);
	System::Uitypes::TColor __fastcall GetButtonColor();
	void __fastcall SetButtonDown(bool newValue);
	bool __fastcall GetButtonDown();
	void __fastcall SetButtonGlyph(Vcl::Graphics::TBitmap* newValue);
	Vcl::Graphics::TBitmap* __fastcall GetButtonGlyph();
	void __fastcall SetButtonHint(System::UnicodeString newValue);
	System::UnicodeString __fastcall GetButtonHint();
	void __fastcall SetButtonIcon(Vcl::Graphics::TIcon* newValue);
	Vcl::Graphics::TIcon* __fastcall GetButtonIcon();
	void __fastcall SetButtonNumGlyphs(int newValue);
	int __fastcall GetButtonNumGlyphs();
	void __fastcall SetButtonUseIcon(bool newValue);
	bool __fastcall GetButtonUseIcon();
	void __fastcall SetButtonWidth(int newValue);
	int __fastcall GetButtonWidth();
	void __fastcall ButtonClickTransfer(System::TObject* Sender);
	void __fastcall SetButtonVisible(bool newValue);
	bool __fastcall GetButtonVisible();
	void __fastcall SetAltButtonDown(bool newValue);
	bool __fastcall GetAltButtonDown();
	void __fastcall SetAltButtonDownSound(Elsndmap::TElSoundName &newValue);
	Elsndmap::TElSoundName __fastcall GetAltButtonDownSound();
	void __fastcall SetAltButtonFlat(bool newValue);
	bool __fastcall GetAltButtonFlat();
	void __fastcall SetAltButtonGlyph(Vcl::Graphics::TBitmap* newValue);
	Vcl::Graphics::TBitmap* __fastcall GetAltButtonGlyph();
	void __fastcall SetAltButtonIcon(Vcl::Graphics::TIcon* newValue);
	Vcl::Graphics::TIcon* __fastcall GetAltButtonIcon();
	void __fastcall SetAltButtonNumGlyphs(int newValue);
	int __fastcall GetAltButtonNumGlyphs();
	void __fastcall SetAltButtonUseIcon(bool newValue);
	bool __fastcall GetAltButtonUseIcon();
	void __fastcall SetAltButtonVisible(bool newValue);
	bool __fastcall GetAltButtonVisible();
	void __fastcall SetAltButtonWidth(int newValue);
	int __fastcall GetAltButtonWidth();
	void __fastcall AltButtonClickTransfer(System::TObject* Sender);
	void __fastcall SetButtonFlat(bool newValue);
	bool __fastcall GetButtonFlat();
	void __fastcall SetAltButtonEnabled(bool newValue);
	bool __fastcall GetAltButtonEnabled();
	void __fastcall SetButtonEnabled(bool newValue);
	bool __fastcall GetButtonEnabled();
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Msg);
	void __fastcall SetAltButtonHint(System::UnicodeString newValue);
	System::UnicodeString __fastcall GetAltButtonHint();
	void __fastcall SetAltButtonPopupPlace(Elpopbtn::TPopupPlace newValue);
	Elpopbtn::TPopupPlace __fastcall GetAltButtonPopupPlace();
	void __fastcall SetAltButtonPullDownMenu(Vcl::Menus::TPopupMenu* newValue);
	Vcl::Menus::TPopupMenu* __fastcall GetAltButtonPullDownMenu();
	void __fastcall SetButtonPopupPlace(Elpopbtn::TPopupPlace newValue);
	Elpopbtn::TPopupPlace __fastcall GetButtonPopupPlace();
	void __fastcall SetButtonPullDownMenu(Vcl::Menus::TPopupMenu* newValue);
	Vcl::Menus::TPopupMenu* __fastcall GetButtonPullDownMenu();
	void __fastcall SetAltButtonCaption(Vcl::Controls::TCaption newValue);
	Vcl::Controls::TCaption __fastcall GetAltButtonCaption();
	void __fastcall SetButtonCaption(Vcl::Controls::TCaption newValue);
	Vcl::Controls::TCaption __fastcall GetButtonCaption();
	void __fastcall SetAltBtnAlign(System::Classes::TLeftRight newValue);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Msg);
	
protected:
	Elpopbtn::TCustomElGraphicButton* FAltButton;
	Elpopbtn::TCustomElGraphicButton* FButton;
	System::Classes::TAlignment FAltBtnAlign;
	Elpopbtn::TCustomElGraphicButtonClass ButtonClass;
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall SetEditRect(const System::Types::TRect &Value);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	System::Uitypes::TColor __fastcall GetAltButtonColor();
	void __fastcall SetAltButtonColor(System::Uitypes::TColor Value);
	bool __fastcall GetButtonThinFrame();
	void __fastcall SetButtonThinFrame(bool Value);
	bool __fastcall GetAltButtonThinFrame();
	void __fastcall SetAltButtonThinFrame(bool Value);
	virtual void __fastcall SetUseXPThemes(const bool Value);
	void __fastcall UpdateButtonStyles();
	virtual void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	virtual void __fastcall SetFlat(const bool Value);
	virtual void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	virtual void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	bool __fastcall GetButtonTransparent();
	void __fastcall SetButtonTransparent(bool Value);
	bool __fastcall GetAltButtonTransparent();
	void __fastcall SetAltButtonTransparent(bool Value);
	__property Vcl::Controls::TCaption ButtonCaption = {read=GetButtonCaption, write=SetButtonCaption};
	__property Elsndmap::TElSoundName ButtonClickSound = {read=GetButtonClickSound, write=SetButtonClickSound};
	__property System::Uitypes::TColor ButtonColor = {read=GetButtonColor, write=SetButtonColor, nodefault};
	__property bool ButtonDown = {read=GetButtonDown, write=SetButtonDown, nodefault};
	__property Elsndmap::TElSoundName ButtonDownSound = {read=GetButtonDownSound, write=SetButtonDownSound};
	__property bool ButtonFlat = {read=GetButtonFlat, write=SetButtonFlat, nodefault};
	__property Vcl::Graphics::TBitmap* ButtonGlyph = {read=GetButtonGlyph, write=SetButtonGlyph};
	__property System::UnicodeString ButtonHint = {read=GetButtonHint, write=SetButtonHint};
	__property Vcl::Graphics::TIcon* ButtonIcon = {read=GetButtonIcon, write=SetButtonIcon};
	__property int ButtonNumGlyphs = {read=GetButtonNumGlyphs, write=SetButtonNumGlyphs, nodefault};
	__property Elpopbtn::TPopupPlace ButtonPopupPlace = {read=GetButtonPopupPlace, write=SetButtonPopupPlace, nodefault};
	__property Vcl::Menus::TPopupMenu* ButtonPullDownMenu = {read=GetButtonPullDownMenu, write=SetButtonPullDownMenu};
	__property Elsndmap::TElSoundMap* ButtonSoundMap = {read=GetButtonSoundMap, write=SetButtonSoundMap};
	__property Elsndmap::TElSoundName ButtonUpSound = {read=GetButtonUpSound, write=SetButtonUpSound};
	__property bool ButtonUseIcon = {read=GetButtonUseIcon, write=SetButtonUseIcon, nodefault};
	__property int ButtonWidth = {read=GetButtonWidth, write=SetButtonWidth, nodefault};
	__property bool ButtonEnabled = {read=GetButtonEnabled, write=SetButtonEnabled, nodefault};
	__property System::Classes::TNotifyEvent OnButtonClick = {read=FOnButtonClick, write=FOnButtonClick};
	__property System::Classes::TShortCut ButtonShortcut = {read=FButtonShortcut, write=FButtonShortcut, nodefault};
	__property System::Classes::TShortCut AltButtonShortcut = {read=FAltButtonShortcut, write=FAltButtonShortcut, nodefault};
	__property bool ButtonVisible = {read=GetButtonVisible, write=SetButtonVisible, nodefault};
	__property Vcl::Controls::TCaption AltButtonCaption = {read=GetAltButtonCaption, write=SetAltButtonCaption};
	__property Elsndmap::TElSoundName AltButtonClickSound = {read=GetAltButtonClickSound, write=SetAltButtonClickSound};
	__property Elsndmap::TElSoundName AltButtonDownSound = {read=GetAltButtonDownSound, write=SetAltButtonDownSound};
	__property Elsndmap::TElSoundMap* AltButtonSoundMap = {read=GetAltButtonSoundMap, write=SetAltButtonSoundMap};
	__property Elsndmap::TElSoundName AltButtonUpSound = {read=GetAltButtonUpSound, write=SetAltButtonUpSound};
	__property System::Uitypes::TColor AltButtonColor = {read=GetAltButtonColor, write=SetAltButtonColor, nodefault};
	__property bool AltButtonDown = {read=GetAltButtonDown, write=SetAltButtonDown, nodefault};
	__property bool AltButtonFlat = {read=GetAltButtonFlat, write=SetAltButtonFlat, nodefault};
	__property Vcl::Graphics::TBitmap* AltButtonGlyph = {read=GetAltButtonGlyph, write=SetAltButtonGlyph};
	__property System::UnicodeString AltButtonHint = {read=GetAltButtonHint, write=SetAltButtonHint};
	__property Vcl::Graphics::TIcon* AltButtonIcon = {read=GetAltButtonIcon, write=SetAltButtonIcon};
	__property int AltButtonNumGlyphs = {read=GetAltButtonNumGlyphs, write=SetAltButtonNumGlyphs, nodefault};
	__property Elpopbtn::TPopupPlace AltButtonPopupPlace = {read=GetAltButtonPopupPlace, write=SetAltButtonPopupPlace, nodefault};
	__property System::Classes::TLeftRight AltButtonPosition = {read=FAltBtnAlign, write=SetAltBtnAlign, default=1};
	__property Vcl::Menus::TPopupMenu* AltButtonPullDownMenu = {read=GetAltButtonPullDownMenu, write=SetAltButtonPullDownMenu};
	__property bool AltButtonUseIcon = {read=GetAltButtonUseIcon, write=SetAltButtonUseIcon, nodefault};
	__property bool AltButtonVisible = {read=GetAltButtonVisible, write=SetAltButtonVisible, nodefault};
	__property int AltButtonWidth = {read=GetAltButtonWidth, write=SetAltButtonWidth, nodefault};
	__property bool AltButtonEnabled = {read=GetAltButtonEnabled, write=SetAltButtonEnabled, nodefault};
	__property System::Classes::TNotifyEvent OnAltButtonClick = {read=FOnAltButtonClick, write=FOnAltButtonClick};
	
public:
	__fastcall virtual TCustomElButtonEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElButtonEdit();
	
__published:
	__property bool ButtonThinFrame = {read=GetButtonThinFrame, write=SetButtonThinFrame, default=0};
	__property bool AltButtonThinFrame = {read=GetAltButtonThinFrame, write=SetAltButtonThinFrame, default=0};
	__property bool ButtonTransparent = {read=GetButtonTransparent, write=SetButtonTransparent, default=0};
	__property bool AltButtonTransparent = {read=GetAltButtonTransparent, write=SetAltButtonTransparent, default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElButtonEdit(HWND ParentWindow) : Eledits::TCustomElEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElButtonEdit : public TCustomElButtonEdit
{
	typedef TCustomElButtonEdit inherited;
	
__published:
	__property AlignBottom = {default=1};
	__property CharCase = {default=0};
	__property ChangeDisabledText = {default=0};
	__property TopMargin = {default=1};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property AutoSize = {default=1};
	__property RTLContent;
	__property BorderSides;
	__property PasswordChar = {default=0};
	__property MaxLength = {default=0};
	__property Transparent;
	__property FlatFocusedScrollBars = {default=0};
	__property WantTabs = {default=0};
	__property HandleDialogKeys = {default=0};
	__property HideSelection = {default=1};
	__property TabSpaces = {default=4};
	__property Lines = {stored=false};
	__property ImageForm;
	__property WordWrap = {default=0};
	__property ScrollBars = {default=0};
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property OnSelectionChange;
	__property Text;
	__property Multiline = {default=0};
	__property VertScrollBarStyles;
	__property HorzScrollBarStyles;
	__property UseCustomScrollBars;
	__property Flat = {default=0};
	__property ActiveBorderType = {default=1};
	__property InactiveBorderType = {default=3};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property UseBackground = {default=0};
	__property Alignment;
	__property AutoSelect = {default=0};
	__property Background;
	__property ButtonCaption = {default=0};
	__property ButtonClickSound = {default=0};
	__property ButtonDownSound = {default=0};
	__property ButtonUpSound = {default=0};
	__property ButtonSoundMap;
	__property ButtonColor;
	__property ButtonDown;
	__property ButtonEnabled;
	__property ButtonFlat;
	__property ButtonGlyph;
	__property ButtonHint = {default=0};
	__property ButtonIcon;
	__property ButtonNumGlyphs;
	__property ButtonPopupPlace;
	__property ButtonPullDownMenu;
	__property ButtonShortcut;
	__property ButtonUseIcon;
	__property ButtonVisible;
	__property ButtonWidth;
	__property OnButtonClick;
	__property AltButtonCaption = {default=0};
	__property AltButtonClickSound = {default=0};
	__property AltButtonDownSound = {default=0};
	__property AltButtonUpSound = {default=0};
	__property AltButtonSoundMap;
	__property AltButtonColor;
	__property AltButtonDown;
	__property AltButtonEnabled;
	__property AltButtonFlat;
	__property AltButtonGlyph;
	__property AltButtonHint = {default=0};
	__property AltButtonIcon;
	__property AltButtonNumGlyphs;
	__property AltButtonPopupPlace;
	__property AltButtonPosition = {default=1};
	__property AltButtonPullDownMenu;
	__property AltButtonShortcut;
	__property AltButtonUseIcon;
	__property AltButtonVisible;
	__property AltButtonWidth;
	__property OnAltButtonClick;
	__property BorderStyle;
	__property Ctl3D;
	__property ParentCtl3D = {default=1};
	__property Enabled = {default=1};
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property PopupMenu;
	__property Color = {default=-16777211};
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property ReadOnly = {default=0};
	__property OnEnter;
	__property OnExit;
	__property OnClick;
	__property OnDblClick;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnStartDrag;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TCustomElButtonEdit.Create */ inline __fastcall virtual TElButtonEdit(System::Classes::TComponent* AOwner) : TCustomElButtonEdit(AOwner) { }
	/* TCustomElButtonEdit.Destroy */ inline __fastcall virtual ~TElButtonEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElButtonEdit(HWND ParentWindow) : TCustomElButtonEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elbtnedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELBTNEDIT)
using namespace Elbtnedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElbtneditHPP
