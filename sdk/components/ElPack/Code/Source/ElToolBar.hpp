// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElToolbar.pas' rev: 34.00 (Windows)

#ifndef EltoolbarHPP
#define EltoolbarHPP

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
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <ElImgFrm.hpp>
#include <ElIni.hpp>
#include <ElStrToken.hpp>
#include <ElTmSchema.hpp>
#include <ElXPThemedControl.hpp>
#include <ElUxTheme.hpp>
#include <ElPanel.hpp>
#include <ElList.hpp>
#include <ElTools.hpp>
#include <ElPopBtn.hpp>
#include <ElVCLUtils.hpp>
#include <ElSndMap.hpp>
#include <System.UITypes.hpp>
#include <ElStrUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltoolbar
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomElToolButton;
class DELPHICLASS TElToolButton;
class DELPHICLASS TElToolBar;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElBarOrientation : unsigned char { eboHorz, eboVert };

enum DECLSPEC_DENUM TElToolButtonType : unsigned char { ebtButton, ebtSeparator, ebtDivider };

class PASCALIMPLEMENTATION TCustomElToolButton : public Elpopbtn::TCustomElGraphicButton
{
	typedef Elpopbtn::TCustomElGraphicButton inherited;
	
private:
	void *FLargeGlyph;
	void *FGlyph;
	TElToolButtonType FButtonType;
	bool FWrap;
	bool FSettingVisible;
	bool FRealVisible;
	bool FFakeBoolProp;
	int FFakeIntProp;
	System::Classes::TNotifyEvent FFakeNotifyEvent;
	int FButtonID;
	bool FOwnerSettings;
	void __fastcall SetWrap(bool newValue);
	void __fastcall SetButtonType(TElToolButtonType newValue);
	void __fastcall SetLargeGlyph(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetNumLargeGlyphs(int newValue);
	HIDESBASE void __fastcall SetGlyph(Vcl::Graphics::TBitmap* newValue);
	HIDESBASE void __fastcall SetNumGlyphs(int newValue);
	HIDESBASE int __fastcall GetNumGlyphs();
	int __fastcall GetNumLargeGlyphs();
	HIDESBASE Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	Vcl::Graphics::TBitmap* __fastcall GetLargeGlyph();
	HIDESBASE void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall LargeGlyphChanged(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual void __fastcall SetUseArrow(bool newValue);
	void __fastcall SwitchGlyphs(bool ToLarge);
	virtual void __fastcall SetFlat(bool Value);
	virtual void __fastcall SetParent(Vcl::Controls::TWinControl* AParent);
	virtual void __fastcall Paint();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	virtual int __fastcall GetThemePartID();
	virtual int __fastcall GetThemeStateID();
	virtual System::WideString __fastcall GetThemedClassName();
	virtual int __fastcall GetArrowThemePartID();
	virtual int __fastcall GetArrowThemeStateID();
	virtual System::WideString __fastcall GetArrowThemedClassName();
	virtual void __fastcall DrawThemedBackground(Vcl::Graphics::TCanvas* Canvas);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadButtonID(System::Classes::TReader* Reader);
	void __fastcall WriteButtonID(System::Classes::TWriter* Writer);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMVisibleChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall ImagesChanged(System::TObject* Sender);
	virtual void __fastcall SetLayout(Vcl::Buttons::TButtonLayout Value);
	virtual void __fastcall SetMargin(int Value);
	virtual void __fastcall SetShowGlyph(bool newValue);
	virtual void __fastcall SetShowText(bool newValue);
	virtual void __fastcall SetSpacing(int Value);
	virtual void __fastcall Loaded();
	void __fastcall SetOwnerSettings(bool Value);
	virtual void __fastcall SetImageIndex(int newValue);
	__property Transparent = {default=0};
	__property bool Wrap = {read=FWrap, write=SetWrap, nodefault};
	__property TElToolButtonType ButtonType = {read=FButtonType, write=SetButtonType, nodefault};
	__property Vcl::Graphics::TBitmap* LargeGlyph = {read=GetLargeGlyph, write=SetLargeGlyph};
	__property int NumLargeGlyphs = {read=GetNumLargeGlyphs, write=SetNumLargeGlyphs, nodefault};
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property int NumGlyphs = {read=GetNumGlyphs, write=SetNumGlyphs, nodefault};
	__property bool OwnerSettings = {read=FOwnerSettings, write=SetOwnerSettings, default=1};
	__property bool Default = {read=FFakeBoolProp, write=FFakeBoolProp, stored=false, nodefault};
	__property bool ShowFocus = {read=FFakeBoolProp, write=FFakeBoolProp, stored=false, nodefault};
	__property bool TabStop = {read=FFakeBoolProp, write=FFakeBoolProp, stored=false, nodefault};
	__property int TabOrder = {read=FFakeIntProp, write=FFakeIntProp, stored=false, nodefault};
	__property System::Classes::TNotifyEvent OnEnter = {read=FFakeNotifyEvent, write=FFakeNotifyEvent, stored=false};
	__property System::Classes::TNotifyEvent OnExit = {read=FFakeNotifyEvent, write=FFakeNotifyEvent, stored=false};
	
public:
	__fastcall virtual TCustomElToolButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElToolButton();
	__property bool RealVisible = {read=FRealVisible, default=1};
};


class PASCALIMPLEMENTATION TElToolButton : public TCustomElToolButton
{
	typedef TCustomElToolButton inherited;
	
__published:
	__property Wrap;
	__property ButtonType;
	__property LargeGlyph;
	__property NumLargeGlyphs;
	__property Glyph;
	__property NumGlyphs;
	__property OwnerSettings = {default=1};
	__property AdjustSpaceForGlyph = {default=1};
	__property PullDownMenu;
	__property PopupPlace = {default=0};
	__property DisableAutoPopup = {default=0};
	__property Cancel = {default=0};
	__property ModalResult = {default=0};
	__property AllowAllUp = {default=0};
	__property GroupIndex = {default=0};
	__property Down = {default=0};
	__property Flat = {default=0};
	__property Layout = {default=0};
	__property Margin = {default=-1};
	__property Spacing = {default=4};
	__property UseArrow = {default=0};
	__property ShadowFollowsColor = {default=1};
	__property ShowGlyph = {default=1};
	__property ShowText = {default=1};
	__property OnArrowClick;
	__property Icon;
	__property ImageIsAlphaBlended = {default=0};
	__property IsSwitch = {default=0};
	__property TextDrawType = {default=0};
	__property ThinFrame = {default=0};
	__property DownSound = {default=0};
	__property UpSound = {default=0};
	__property ClickSound = {default=0};
	__property ArrowClickSound = {default=0};
	__property SoundMap;
	__property UseIcon = {default=0};
	__property ImageIndex = {default=-1};
	__property UseImageList = {default=0};
	__property OldStyled = {default=0};
	__property Background;
	__property DownBackground;
	__property BackgroundDrawBorder = {default=0};
	__property Transparent = {default=0};
	__property UseXPThemes = {default=1};
	__property Caption;
	__property Enabled = {default=1};
	__property PopupMenu;
	__property Color;
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnStartDrag;
public:
	/* TCustomElToolButton.Create */ inline __fastcall virtual TElToolButton(System::Classes::TComponent* AOwner) : TCustomElToolButton(AOwner) { }
	/* TCustomElToolButton.Destroy */ inline __fastcall virtual ~TElToolButton() { }
	
};


typedef System::TMetaClass* TElToolButtonClass;

class PASCALIMPLEMENTATION TElToolBar : public Elpanel::TElPanel
{
	typedef Elpanel::TElPanel inherited;
	
private:
	bool FNoReAlign;
	bool FShowMoreMenu;
	bool FTransparentButtons;
	bool FUseImageList;
	Vcl::Controls::TImageList* FImages;
	Vcl::Controls::TImageList* FHotImages;
	Vcl::Controls::TImageList* FDisabledImages;
	int FUpdateCount;
	int FUpdatingButtons;
	bool FUseLargeGlyphs;
	bool FHidden;
	bool FHideable;
	TElBarOrientation FOrientation;
	System::Uitypes::TColor FButtonColor;
	int FMinSize;
	bool FAutoSize;
	bool FFlat;
	int FLargeBtnWidth;
	int FLargeBtnHeight;
	Vcl::Buttons::TButtonLayout FGlyphLayout;
	int FSpacing;
	int FMargin;
	bool FShowGlyph;
	bool FShowCaption;
	bool FLargeSize;
	int FBtnWidth;
	int FBtnHeight;
	int FBtnOffsHorz;
	int FBtnOffsVert;
	bool FAutoWrap;
	bool FCreating;
	Vcl::Controls::TAlign FSaveAlign;
	System::UnicodeString FDummy;
	Elimgfrm::TElImageForm* FButtonImageForm;
	bool FMouseInControl;
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCLButtonDown(Winapi::Messages::TWMNCHitMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	
protected:
	bool FThinButtons;
	Elini::TElIniFile* FStorage;
	System::UnicodeString FStoragePath;
	bool FAdjustButtonWidth;
	bool FAdjustButtonHeight;
	Ellist::TElList* FButtons;
	TElToolButton* FFocusedButton;
	bool FTransparent;
	bool FImageIsAlphaBlended;
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Msg);
	void __fastcall StartMoreMenu();
	void __fastcall PutMoreItemsToBar();
	void __fastcall OnMoreItemClick(System::TObject* Sender);
	virtual void __fastcall SetButtonImageForm(Elimgfrm::TElImageForm* newValue);
	virtual void __fastcall SetBtnWidth(int newValue);
	virtual void __fastcall SetBtnHeight(int newValue);
	virtual void __fastcall SetFlat(bool newValue);
	virtual void __fastcall SetLargeSize(bool newValue);
	virtual void __fastcall SetLargeBtnWidth(int newValue);
	virtual void __fastcall SetLargeBtnHeight(int newValue);
	virtual void __fastcall SetButtonColor(System::Uitypes::TColor newValue);
	HIDESBASE virtual void __fastcall SetAutoSize(bool newValue);
	virtual void __fastcall SetTransparentButtons(bool newValue);
	virtual void __fastcall SetBtnOffsHorz(int newValue);
	virtual void __fastcall SetBtnOffsVert(int newValue);
	void __fastcall SetAutoWrap(bool newValue);
	void __fastcall SetShowGlyph(bool newValue);
	HIDESBASE void __fastcall SetShowCaption(bool newValue);
	virtual void __fastcall SetGlyphLayout(Vcl::Buttons::TButtonLayout newValue);
	virtual void __fastcall SetSpacing(int newValue);
	virtual void __fastcall SetMargin(int newValue);
	TElToolButton* __fastcall GetToolButton(int index);
	void __fastcall SetToolButton(int index, TElToolButton* newValue);
	int __fastcall GetButtonCount();
	HIDESBASE MESSAGE void __fastcall CMControlListChange(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall CMControlChange(Vcl::Controls::TCMControlChange &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	void __fastcall SetMinSize(int newValue);
	void __fastcall SetOrientation(TElBarOrientation newValue);
	void __fastcall SetUseLargeGlyphs(bool newValue);
	void __fastcall SetImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetHotImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetDisabledImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetUseImageList(bool newValue);
	virtual void __fastcall SetShowMoreMenu(bool newValue);
	virtual void __fastcall SetMoreMenuActive(bool newValue);
	virtual void __fastcall AlignControls(Vcl::Controls::TControl* AControl, System::Types::TRect &Rect);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Loaded();
	virtual void __fastcall Paint();
	void __fastcall RedrawMoreBtn();
	int __fastcall GetRealClientWidth();
	int __fastcall GetRealClientHeight();
	System::Types::TRect __fastcall GetMoreBtnRect();
	void __fastcall SetThinButtons(bool Value);
	virtual void __fastcall DrawThemedBackground();
	virtual System::WideString __fastcall GetThemedClassName();
	int __fastcall GetFreeButtonID();
	TElToolButton* __fastcall GetButtonByID(int ID);
	void __fastcall SetAdjustButtonWidth(bool Value);
	int __fastcall GetEffectiveButtonWidth(TCustomElToolButton* Button, bool IncludeArrow);
	void __fastcall SetAdjustButtonHeight(bool Value);
	int __fastcall GetEffectiveButtonHeight(TCustomElToolButton* Button);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	void __fastcall StartLeaveTracking();
	virtual TElToolButtonClass __fastcall GetButtonClass();
	void __fastcall SetFocusedButton(TElToolButton* Value);
	virtual void __fastcall SetTransparent(bool newValue);
	virtual void __fastcall SetUseXPThemes(const bool Value);
	DYNAMIC void __fastcall DoEnter();
	virtual void __fastcall SetParent(Vcl::Controls::TWinControl* AParent);
	void __fastcall SetImageIsAlphaBlended(bool Value);
	__property TElToolButton* FocusedButton = {read=FFocusedButton, write=SetFocusedButton};
	
public:
	__fastcall virtual TElToolBar(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElToolBar();
	virtual TElToolButton* __fastcall AddButton(TElToolButtonType ButtonType);
	void __fastcall OrderedControls(Ellist::TElList* L);
	virtual void __fastcall AlignButtons();
	virtual void __fastcall UpdateButtons();
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	void __fastcall Save();
	void __fastcall Restore();
	bool __fastcall Setup(bool ShowTextOptions, bool ShowIconOptions);
	TCustomElToolButton* __fastcall GetNextButton(TCustomElToolButton* CurrentButton, bool Forward, bool IncludeDisabled);
	__property TElToolButton* ToolButton[int index] = {read=GetToolButton, write=SetToolButton};
	__property System::UnicodeString Caption = {read=FDummy, write=FDummy};
	__property int ButtonCount = {read=GetButtonCount, nodefault};
	
__published:
	__property int BtnWidth = {read=FBtnWidth, write=SetBtnWidth, default=24};
	__property int BtnHeight = {read=FBtnHeight, write=SetBtnHeight, default=24};
	__property int BtnOffsHorz = {read=FBtnOffsHorz, write=SetBtnOffsHorz, default=3};
	__property int BtnOffsVert = {read=FBtnOffsVert, write=SetBtnOffsVert, default=3};
	__property bool AutoWrap = {read=FAutoWrap, write=SetAutoWrap, nodefault};
	__property bool ShowGlyph = {read=FShowGlyph, write=SetShowGlyph, default=1};
	__property bool ShowCaption = {read=FShowCaption, write=SetShowCaption, default=0};
	__property bool LargeSize = {read=FLargeSize, write=SetLargeSize, default=0};
	__property int LargeBtnWidth = {read=FLargeBtnWidth, write=SetLargeBtnWidth, default=48};
	__property int LargeBtnHeight = {read=FLargeBtnHeight, write=SetLargeBtnHeight, default=48};
	__property Vcl::Buttons::TButtonLayout GlyphLayout = {read=FGlyphLayout, write=SetGlyphLayout, default=0};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=4};
	__property int Margin = {read=FMargin, write=SetMargin, default=-1};
	__property bool Flat = {read=FFlat, write=SetFlat, default=1};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=1};
	__property int MinSize = {read=FMinSize, write=SetMinSize, default=24};
	__property System::Uitypes::TColor ButtonColor = {read=FButtonColor, write=SetButtonColor, default=-16777201};
	__property Elimgfrm::TElImageForm* ButtonImageForm = {read=FButtonImageForm, write=SetButtonImageForm};
	__property TElBarOrientation Orientation = {read=FOrientation, write=SetOrientation, default=0};
	__property bool UseLargeGlyphs = {read=FUseLargeGlyphs, write=SetUseLargeGlyphs, nodefault};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property Vcl::Controls::TImageList* HotImages = {read=FHotImages, write=SetHotImages};
	__property Vcl::Controls::TImageList* DisabledImages = {read=FDisabledImages, write=SetDisabledImages};
	__property bool UseImageList = {read=FUseImageList, write=SetUseImageList, nodefault};
	__property bool TransparentButtons = {read=FTransparentButtons, write=SetTransparentButtons, nodefault};
	__property bool ThinButtons = {read=FThinButtons, write=SetThinButtons, nodefault};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=FStorage};
	__property System::UnicodeString StoragePath = {read=FStoragePath, write=FStoragePath};
	__property bool AdjustButtonWidth = {read=FAdjustButtonWidth, write=SetAdjustButtonWidth, default=1};
	__property bool AdjustButtonHeight = {read=FAdjustButtonHeight, write=SetAdjustButtonHeight, default=1};
	__property bool ImageIsAlphaBlended = {read=FImageIsAlphaBlended, write=SetImageIsAlphaBlended, default=0};
	__property bool ShowMoreMenu = {read=FShowMoreMenu, write=SetShowMoreMenu, nodefault};
	__property UseXPThemes = {default=1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElToolBar(HWND ParentWindow) : Elpanel::TElPanel(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int DEF_SepSize;
}	/* namespace Eltoolbar */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTOOLBAR)
using namespace Eltoolbar;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltoolbarHPP
