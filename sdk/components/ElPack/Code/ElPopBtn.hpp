// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElPopBtn.pas' rev: 35.00 (Windows)

#ifndef ElpopbtnHPP
#define ElpopbtnHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <ElBtnCtl.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Winapi.CommCtrl.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <ElVCLUtils.hpp>
#include <HTMLRender.hpp>
#include <ElHandPt.hpp>
#include <ElTools.hpp>
#include <ElList.hpp>
#include <ElSndMap.hpp>
#include <ElStrUtils.hpp>
#include <ElImgFrm.hpp>
#include <ElXPThemedControl.hpp>
#include <System.ImageList.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elpopbtn
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElSpeedButton;
class DELPHICLASS TCustomElPopupButton;
class DELPHICLASS TElPopupButton;
class DELPHICLASS TElGlyphList;
class DELPHICLASS TElGlyphCache;
class DELPHICLASS TElButtonGlyph;
class DELPHICLASS TCustomElGraphicButton;
class DELPHICLASS TElGraphicButton;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TPullDownEvent)(System::TObject* Sender);

enum DECLSPEC_DENUM TElButtonState : unsigned char { ebsUp, ebsDisabled, ebsDown, ebsExclusive, ebsArrDown };

class PASCALIMPLEMENTATION TElSpeedButton : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
private:
	System::Uitypes::TColor FTransparentColor;
	bool FAutoSize;
	Vcl::Graphics::TBitmap* FNormalImage;
	Vcl::Graphics::TBitmap* FDisabledImage;
	Vcl::Graphics::TBitmap* FMouseInImage;
	Vcl::Graphics::TBitmap* FPressedImage;
	bool FFlat;
	bool FDrawEdge;
	bool FPressed;
	bool FOver;
	Vcl::Extctrls::TTimer* FPullTimer;
	System::Uitypes::TMouseButton FPullDownBtn;
	int FPullDownInterval;
	bool FPullDownEnabled;
	Vcl::Menus::TPopupMenu* FPullDownMenu;
	bool FTransparent;
	TPullDownEvent FOnPullDown;
	void __fastcall SetPullDownMenu(Vcl::Menus::TPopupMenu* newValue);
	void __fastcall SetTransparent(bool newValue);
	void __fastcall SetDrawEdge(bool newValue);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	void __fastcall SetFlat(bool newValue);
	void __fastcall SetNormalImage(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetDisabledImage(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetMouseInImage(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetPressedImage(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetTransparentColor(System::Uitypes::TColor newValue);
	MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	
protected:
	HIDESBASE void __fastcall SetAutoSize(bool newValue);
	virtual void __fastcall TriggerPullDownEvent();
	virtual void __fastcall Paint();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall OnTimer(System::TObject* Sender);
	void __fastcall StartTimer();
	void __fastcall PullMenu();
	
public:
	__fastcall virtual TElSpeedButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElSpeedButton();
	virtual bool __fastcall InCircle(int X, int Y);
	
__published:
	__property System::Uitypes::TMouseButton PullDownBtn = {read=FPullDownBtn, write=FPullDownBtn, nodefault};
	__property int PullDownInterval = {read=FPullDownInterval, write=FPullDownInterval, default=1000};
	__property bool PullDownEnabled = {read=FPullDownEnabled, write=FPullDownEnabled, default=0};
	__property Vcl::Menus::TPopupMenu* PullDownMenu = {read=FPullDownMenu, write=SetPullDownMenu};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=1};
	__property TPullDownEvent OnPullDown = {read=FOnPullDown, write=FOnPullDown};
	__property bool DrawEdge = {read=FDrawEdge, write=SetDrawEdge, default=0};
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property Vcl::Graphics::TBitmap* NormalImage = {read=FNormalImage, write=SetNormalImage};
	__property Vcl::Graphics::TBitmap* DisabledImage = {read=FDisabledImage, write=SetDisabledImage};
	__property Vcl::Graphics::TBitmap* MouseInImage = {read=FMouseInImage, write=SetMouseInImage};
	__property Vcl::Graphics::TBitmap* PressedImage = {read=FPressedImage, write=SetPressedImage};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=1};
	__property System::Uitypes::TColor TransparentColor = {read=FTransparentColor, write=SetTransparentColor, nodefault};
	__property Align = {default=0};
	__property Color;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property ParentColor = {default=1};
	__property Enabled = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDrag;
	__property OnDragOver;
	__property OnDragDrop;
};


enum DECLSPEC_DENUM TPopupPlace : unsigned char { ppDown, ppRight, ppTop };

class PASCALIMPLEMENTATION TCustomElPopupButton : public Elbtnctl::TElButtonControl
{
	typedef Elbtnctl::TElButtonControl inherited;
	
protected:
	int FNumGlyphs;
	System::Uitypes::TCursor FCursor;
	bool FShadowsUseCustom;
	System::Uitypes::TColor FShadowBtnHighlight;
	System::Uitypes::TColor FShadowBtnShadow;
	System::Uitypes::TColor FShadowBtnDkShadow;
	Vcl::Graphics::TBitmap* FBackground;
	bool FShadowFollowsColor;
	Vcl::Graphics::TBitmap* FDownBackground;
	bool FBackgroundDrawBorder;
	bool FThinFrame;
	Vcl::Controls::TImageList* FHotImages;
	Vcl::Controls::TImageList* FDisabledImages;
	Vcl::Controls::TImageList* FDownImages;
	Vcl::Controls::TImageList* FImageList;
	bool FOldStyled;
	bool FUseImageList;
	bool FUseIcon;
	Elsndmap::TElSoundMap* FSoundMap;
	Elsndmap::TElSoundName FDownSound;
	Elsndmap::TElSoundName FUpSound;
	Elsndmap::TElSoundName FClickSound;
	Elsndmap::TElSoundName FArrowClickSound;
	bool FIsSwitch;
	bool FShowGlyph;
	bool FShowText;
	bool FUseArrow;
	bool FShowFocus;
	bool FMultiLine;
	int FGroupIndex;
	TElButtonGlyph* FGlyph;
	bool FDown;
	bool FArrDown;
	bool FInMenu;
	bool FIgnoreClick;
	bool FDragging;
	bool FAllowAllUp;
	Vcl::Buttons::TButtonLayout FLayout;
	int FSpacing;
	int FMargin;
	bool FFlat;
	bool FMouseInArrow;
	bool FMouseInControl;
	bool FDisableAp;
	TPopupPlace FPopupPlace;
	bool FDefault;
	bool FCancel;
	bool FActive;
	System::Uitypes::TModalResult FModalResult;
	bool FClicksDisabled;
	Vcl::Imglist::TChangeLink* FChLink;
	Vcl::Imglist::TChangeLink* FNChLink;
	Vcl::Imglist::TChangeLink* FDChLink;
	Vcl::Imglist::TChangeLink* FHChLink;
	Vcl::Menus::TPopupMenu* FPullDownMenu;
	System::Classes::TNotifyEvent FOnArrowClick;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	bool FShowBorder;
	bool FAdjustSpaceForGlyph;
	NativeUInt FArrTheme;
	bool FIsHTML;
	Htmlrender::TElHTMLRender* FRender;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	TElButtonState FOrigState;
	TElButtonState FState;
	bool FDrawDefaultFrame;
	bool FImageIsAlphaBlended;
	bool FDrawFocusFrame;
	System::Uitypes::TColor FLinkColor;
	Vcl::Menus::TPopupMenu* FLinkPopupMenu;
	System::Uitypes::TFontStyles FLinkStyle;
	System::Types::TRect FTextRect;
	bool FChangeDisabledText;
	System::Classes::TAlignment FAlignment;
	void __fastcall SetAlignment(System::Classes::TAlignment Value);
	void __fastcall SetIsHTML(bool Value);
	void __fastcall SetShowBorder(bool newValue);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall SetPullDownMenu(Vcl::Menus::TPopupMenu* newValue);
	void __fastcall SetDefault(bool Value);
	void __fastcall SetPopupPlace(TPopupPlace Value);
	void __fastcall SetDisableAp(bool Value);
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall UpdateExclusive();
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	Vcl::Buttons::TNumGlyphs __fastcall GetNumGlyphs();
	void __fastcall SetNumGlyphs(Vcl::Buttons::TNumGlyphs Value);
	void __fastcall SetDown(bool Value);
	void __fastcall SetAllowAllUp(bool Value);
	void __fastcall SetGroupIndex(int Value);
	void __fastcall SetLayout(Vcl::Buttons::TButtonLayout Value);
	void __fastcall SetSpacing(int Value);
	void __fastcall SetMargin(int Value);
	void __fastcall UpdateTracking();
	void __fastcall IntMouseEnter();
	void __fastcall IntMouseLeave();
	void __fastcall IntEnabledChanged();
	bool __fastcall IntKeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall IntKeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall IntTextChanged();
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyUp(Winapi::Messages::TWMKey &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMButtonPressed(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogKey(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMFocusChanged(Vcl::Controls::TCMFocusChanged &Message);
	MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Message);
	void __fastcall SetShowFocus(bool newValue);
	void __fastcall SetShowGlyph(bool newValue);
	void __fastcall SetShowText(bool newValue);
	Vcl::Graphics::TIcon* __fastcall GetIcon();
	void __fastcall SetIcon(Vcl::Graphics::TIcon* newValue);
	void __fastcall SetIsSwitch(bool newValue);
	void __fastcall SetSoundMap(Elsndmap::TElSoundMap* newValue);
	void __fastcall SetImageIndex(int newValue);
	int __fastcall GetImageIndex();
	void __fastcall SetUseIcon(bool newValue);
	void __fastcall SetImageList(Vcl::Controls::TImageList* newValue);
	void __fastcall SetUseImageList(bool newValue);
	bool __fastcall GetUseImageList();
	void __fastcall SetOldStyled(bool newValue);
	void __fastcall SetHotImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetDownImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetDisabledImages(Vcl::Controls::TImageList* newValue);
	void __fastcall ImagesChanged(System::TObject* Sender);
	void __fastcall SetThinFrame(bool newValue);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetDownBackground(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetBackgroundDrawBorder(bool Value);
	void __fastcall SetShadowFollowsColor(bool Value);
	void __fastcall SetShadowsUseCustom(bool Value);
	void __fastcall SetShadowBtnHighlight(System::Uitypes::TColor Value);
	void __fastcall SetShadowBtnShadow(System::Uitypes::TColor Value);
	void __fastcall SetShadowBtnDkShadow(System::Uitypes::TColor Value);
	void __fastcall SetAdjustSpaceForGlyph(bool Value);
	void __fastcall SetLinkPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	void __fastcall DoLinkPopup(const System::Types::TPoint &MousePos);
	void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	virtual void __fastcall TriggerLinkClickEvent(Elstrutils::TElFString HRef);
	virtual void __fastcall SetUseArrow(bool newValue);
	virtual void __fastcall CreateThemeHandle();
	virtual void __fastcall FreeThemeHandle();
	DYNAMIC HPALETTE __fastcall GetPalette();
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Paint();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall SetButtonStyle(bool ADefault);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall SetFlat(bool Value);
	virtual bool __fastcall GetChecked();
	virtual void __fastcall SetChecked(bool newValue);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	__property bool ClicksDisabled = {read=FClicksDisabled, write=FClicksDisabled, nodefault};
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	void __fastcall SetDrawDefaultFrame(bool Value);
	virtual int __fastcall GetArrowSize();
	bool __fastcall DoSaveShadows();
	void __fastcall SetImageIsAlphaBlended(bool Value);
	void __fastcall SetDrawFocusFrame(bool Value);
	HIDESBASE virtual void __fastcall SetCursor(System::Uitypes::TCursor Value);
	void __fastcall SetChangeDisabledText(bool Value);
	__property Vcl::Menus::TPopupMenu* PullDownMenu = {read=FPullDownMenu, write=SetPullDownMenu};
	__property TPopupPlace PopupPlace = {read=FPopupPlace, write=SetPopupPlace, default=0};
	__property bool DisableAutoPopup = {read=FDisableAp, write=SetDisableAp, default=0};
	__property bool Cancel = {read=FCancel, write=FCancel, default=0};
	__property bool Default = {read=FDefault, write=SetDefault, default=0};
	__property System::Uitypes::TModalResult ModalResult = {read=FModalResult, write=FModalResult, default=0};
	__property bool AllowAllUp = {read=FAllowAllUp, write=SetAllowAllUp, default=0};
	__property int GroupIndex = {read=FGroupIndex, write=SetGroupIndex, default=0};
	__property bool Down = {read=FDown, write=SetDown, default=0};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property Vcl::Buttons::TButtonLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property int Margin = {read=FMargin, write=SetMargin, default=-1};
	__property Vcl::Buttons::TNumGlyphs NumGlyphs = {read=GetNumGlyphs, write=SetNumGlyphs, nodefault};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=4};
	__property bool ShowFocus = {read=FShowFocus, write=SetShowFocus, default=1};
	__property bool UseArrow = {read=FUseArrow, write=SetUseArrow, default=0};
	__property bool ShadowFollowsColor = {read=FShadowFollowsColor, write=SetShadowFollowsColor, default=1};
	__property bool ShowGlyph = {read=FShowGlyph, write=SetShowGlyph, default=1};
	__property bool ShowText = {read=FShowText, write=SetShowText, default=1};
	__property System::Classes::TNotifyEvent OnArrowClick = {read=FOnArrowClick, write=FOnArrowClick};
	__property Vcl::Graphics::TIcon* Icon = {read=GetIcon, write=SetIcon};
	__property bool UseIcon = {read=FUseIcon, write=SetUseIcon, default=0};
	__property bool IsSwitch = {read=FIsSwitch, write=SetIsSwitch, default=0};
	__property Elsndmap::TElSoundName DownSound = {read=FDownSound, write=FDownSound};
	__property Elsndmap::TElSoundName UpSound = {read=FUpSound, write=FUpSound};
	__property Elsndmap::TElSoundName ClickSound = {read=FClickSound, write=FClickSound};
	__property Elsndmap::TElSoundName ArrowClickSound = {read=FArrowClickSound, write=FArrowClickSound};
	__property Elsndmap::TElSoundMap* SoundMap = {read=FSoundMap, write=SetSoundMap};
	__property int ImageIndex = {read=GetImageIndex, write=SetImageIndex, default=-1};
	__property Vcl::Controls::TImageList* Images = {read=FImageList, write=SetImageList};
	__property Vcl::Controls::TImageList* DownImages = {read=FDownImages, write=SetDownImages};
	__property Vcl::Controls::TImageList* HotImages = {read=FHotImages, write=SetHotImages};
	__property Vcl::Controls::TImageList* DisabledImages = {read=FDisabledImages, write=SetDisabledImages};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property bool ShowBorder = {read=FShowBorder, write=SetShowBorder, default=1};
	__property bool ShadowsUseCustom = {read=FShadowsUseCustom, write=SetShadowsUseCustom, default=0};
	__property System::Uitypes::TColor ShadowBtnHighlight = {read=FShadowBtnHighlight, write=SetShadowBtnHighlight, stored=DoSaveShadows, nodefault};
	__property System::Uitypes::TColor ShadowBtnShadow = {read=FShadowBtnShadow, write=SetShadowBtnShadow, stored=DoSaveShadows, nodefault};
	__property System::Uitypes::TColor ShadowBtnDkShadow = {read=FShadowBtnDkShadow, write=SetShadowBtnDkShadow, stored=DoSaveShadows, nodefault};
	__property bool UseImageList = {read=GetUseImageList, write=SetUseImageList, default=0};
	__property bool OldStyled = {read=FOldStyled, write=SetOldStyled, default=0};
	__property bool ThinFrame = {read=FThinFrame, write=SetThinFrame, default=0};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property Vcl::Graphics::TBitmap* DownBackground = {read=FDownBackground, write=SetDownBackground};
	__property bool BackgroundDrawBorder = {read=FBackgroundDrawBorder, write=SetBackgroundDrawBorder, default=0};
	__property bool AdjustSpaceForGlyph = {read=FAdjustSpaceForGlyph, write=SetAdjustSpaceForGlyph, default=1};
	__property bool DrawDefaultFrame = {read=FDrawDefaultFrame, write=SetDrawDefaultFrame, nodefault};
	__property bool DrawFocusFrame = {read=FDrawFocusFrame, write=SetDrawFocusFrame, default=0};
	__property bool ImageIsAlphaBlended = {read=FImageIsAlphaBlended, write=SetImageIsAlphaBlended, default=0};
	__property bool ChangeDisabledText = {read=FChangeDisabledText, write=SetChangeDisabledText, default=1};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, default=0};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=SetLinkColor, default=16711680};
	__property Vcl::Menus::TPopupMenu* LinkPopupMenu = {read=FLinkPopupMenu, write=SetLinkPopupMenu};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=SetLinkStyle, nodefault};
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=2};
	
public:
	__fastcall virtual TCustomElPopupButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElPopupButton();
	virtual void __fastcall AClick(bool Arrow);
	DYNAMIC void __fastcall Click();
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElPopupButton(HWND ParentWindow) : Elbtnctl::TElButtonControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElPopupButton : public TCustomElPopupButton
{
	typedef TCustomElPopupButton inherited;
	
protected:
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	
__published:
	__property Alignment = {default=2};
	__property Background;
	__property BackgroundDrawBorder = {default=0};
	__property DownBackground;
	__property ImageIndex = {default=-1};
	__property UseImageList = {default=0};
	__property ImageIsAlphaBlended = {default=0};
	__property Images;
	__property HotImages;
	__property DisabledImages;
	__property DrawDefaultFrame;
	__property DrawFocusFrame = {default=0};
	__property PullDownMenu;
	__property PopupPlace = {default=0};
	__property DisableAutoPopup = {default=0};
	__property Cancel = {default=0};
	__property Default = {default=0};
	__property ModalResult = {default=0};
	__property MoneyFlat = {default=0};
	__property MoneyFlatActiveColor;
	__property MoneyFlatInactiveColor;
	__property MoneyFlatDownColor;
	__property AdjustSpaceForGlyph = {default=1};
	__property AllowAllUp = {default=0};
	__property GroupIndex = {default=0};
	__property Down = {default=0};
	__property Flat = {default=0};
	__property Glyph;
	__property ImageForm;
	__property IsHTML = {default=0};
	__property LinkColor = {default=16711680};
	__property LinkPopupMenu;
	__property LinkStyle;
	__property Layout = {default=0};
	__property Margin = {default=-1};
	__property NumGlyphs;
	__property ShadowFollowsColor = {default=1};
	__property ShadowsUseCustom = {default=0};
	__property ShadowBtnHighlight;
	__property ShadowBtnShadow;
	__property ShadowBtnDkShadow;
	__property ShowFocus = {default=1};
	__property ShowGlyph = {default=1};
	__property ShowText = {default=1};
	__property Spacing = {default=4};
	__property UseArrow = {default=0};
	__property IsSwitch = {default=0};
	__property OnArrowClick;
	__property Icon;
	__property UseIcon = {default=0};
	__property ThinFrame = {default=0};
	__property TextDrawType = {default=0};
	__property Transparent = {default=0};
	__property DownSound = {default=0};
	__property UpSound = {default=0};
	__property ClickSound = {default=0};
	__property ArrowClickSound = {default=0};
	__property SoundMap;
	__property DownImages;
	__property ShowBorder = {default=1};
	__property OldStyled = {default=0};
	__property UseXPThemes = {default=1};
	__property Caption;
	__property Cursor;
	__property Enabled = {default=1};
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property PopupMenu;
	__property Color;
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property HelpContext = {default=0};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property OnImageNeeded;
	__property OnLinkClick;
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnKeyPress;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnStartDrag;
public:
	/* TCustomElPopupButton.Create */ inline __fastcall virtual TElPopupButton(System::Classes::TComponent* AOwner) : TCustomElPopupButton(AOwner) { }
	/* TCustomElPopupButton.Destroy */ inline __fastcall virtual ~TElPopupButton() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElPopupButton(HWND ParentWindow) : TCustomElPopupButton(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElGlyphList : public Vcl::Controls::TImageList
{
	typedef Vcl::Controls::TImageList inherited;
	
private:
	System::Classes::TBits* Used;
	int FCount;
	int __fastcall AllocateIndex();
	
public:
	__fastcall TElGlyphList(int AWidth, int AHeight);
	__fastcall virtual ~TElGlyphList();
	HIDESBASE int __fastcall AddMasked(Vcl::Graphics::TBitmap* Image, System::Uitypes::TColor MaskColor);
	HIDESBASE void __fastcall Delete(int Index);
	__property int Count = {read=FCount, nodefault};
public:
	/* TCustomImageList.Create */ inline __fastcall virtual TElGlyphList(System::Classes::TComponent* AOwner) : Vcl::Controls::TImageList(AOwner) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElGlyphCache : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Ellist::TElList* GlyphLists;
	
public:
	__fastcall TElGlyphCache();
	__fastcall virtual ~TElGlyphCache();
	TElGlyphList* __fastcall GetList(int AWidth, int AHeight);
	void __fastcall ReturnList(TElGlyphList* List);
	bool __fastcall Empty();
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElButtonGlyph : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Vcl::Controls::TImageList* FImageList;
	int FImageIndex;
	bool FUseImageList;
	Vcl::Graphics::TIcon* FIcon;
	bool FUseIcon;
	Vcl::Graphics::TBitmap* FOriginal;
	TElGlyphList* FGlyphList;
	System::StaticArray<int, 5> FIndexs;
	System::Uitypes::TColor FTransparentColor;
	Vcl::Buttons::TNumGlyphs FNumGlyphs;
	System::Classes::TNotifyEvent FOnChange;
	bool FStretched;
	int FStrW;
	int FStrH;
	void __fastcall SetImageList(Vcl::Controls::TImageList* NewValue);
	void __fastcall SetImageIndex(int NewValue);
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall IconChanged(System::TObject* Sender);
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	void __fastcall SetNumGlyphs(Vcl::Buttons::TNumGlyphs Value);
	void __fastcall Repaint();
	int __fastcall CreateButtonGlyph(TElButtonState State);
	void __fastcall DrawButtonGlyph(Vcl::Graphics::TCanvas* Canvas, const System::Types::TPoint &GlyphPos, TElButtonState State, bool Transparent, System::Uitypes::TColor Color, bool AlphaBlended);
	void __fastcall DrawButtonText(Vcl::Graphics::TCanvas* Canvas, const Elstrutils::TElFString Caption, const System::Types::TRect &TextBounds, TElButtonState State, bool Multiline, System::Classes::TAlignment Alignment, bool Active, bool Transparent, Elvclutils::TElTextDrawType TextDrawType, bool UseThemesForText, NativeUInt Theme, int ThemePart, int ThemeState, bool ShowAccelChar, bool IsHTML, Htmlrender::TElHTMLRender* HTMLRender, bool ChangeDisabledText);
	void __fastcall CalcButtonLayout(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Client, const System::Types::TPoint &_offset, const Elstrutils::TElFString Caption, Vcl::Buttons::TButtonLayout Layout, int Margin, int Spacing, System::Types::TPoint &GlyphPos, System::Types::TRect &TextBounds, bool ShowGlyph, bool ShowText, bool MultiLine, int ArrowWidth, bool UseThemesForText, NativeUInt Theme, int ThemePart, int ThemeState, bool IsHTML, Htmlrender::TElHTMLRender* HTMLRender);
	System::Types::TRect __fastcall GetGlyphSize();
	void __fastcall SetUseIcon(bool NewValue);
	
protected:
	__property Vcl::Controls::TImageList* ImageList = {read=FImageList, write=SetImageList};
	__property int ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property bool UseImageList = {read=FUseImageList, write=FUseImageList, nodefault};
	
public:
	__fastcall TElButtonGlyph();
	__fastcall virtual ~TElButtonGlyph();
	void __fastcall ResetNumGlyphs();
	System::Types::TRect __fastcall Draw(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Client, const System::Types::TPoint &_offset, const Elstrutils::TElFString Caption, Vcl::Buttons::TButtonLayout Layout, int Margin, int Spacing, TElButtonState State, TElButtonState GlyphState, System::Classes::TAlignment Alignment, bool Transparent, bool Multiline, bool Active, bool ShowGlyph, bool ShowText, int ArrowWidth, Elvclutils::TElTextDrawType TextDrawType, System::Uitypes::TColor Color, bool UseThemesForText, NativeUInt Theme, int ThemePart, int ThemeState, bool ShowAccelChar, bool ImageIsAlphaBlended, bool IsHTML, Htmlrender::TElHTMLRender* HTMLRender, bool ChangeDisabledText);
	void __fastcall GetPaintGlyphSize(const System::Types::TRect &R, System::Types::TPoint &Size);
	int __fastcall CalcButtonWidth(Vcl::Graphics::TCanvas* Canvas, int &MaxHeight, const System::Types::TPoint &_offset, const Elstrutils::TElFString Caption, Vcl::Buttons::TButtonLayout Layout, int Margin, int Spacing, bool ShowGlyph, bool ShowText, bool MultiLine, int ArrowWidth, bool UseThemesForText, NativeUInt Theme, int ThemePart, int ThemeState, bool IsHTML, Htmlrender::TElHTMLRender* HTMLRender);
	__property bool UseIcon = {read=FUseIcon, write=SetUseIcon, nodefault};
	__property Vcl::Graphics::TIcon* Icon = {read=FIcon};
	__property Vcl::Graphics::TBitmap* Glyph = {read=FOriginal, write=SetGlyph};
	__property Vcl::Buttons::TNumGlyphs NumGlyphs = {read=FNumGlyphs, write=SetNumGlyphs, default=1};
	__property System::Types::TRect GlyphSize = {read=GetGlyphSize};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
};


class PASCALIMPLEMENTATION TCustomElGraphicButton : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
protected:
	int FNumGlyphs;
	System::Classes::TWndMethod FMenuWindowProc;
	System::Uitypes::TCursor FCursor;
	bool FShadowsUseCustom;
	System::Uitypes::TColor FShadowBtnHighlight;
	System::Uitypes::TColor FShadowBtnShadow;
	System::Uitypes::TColor FShadowBtnDkShadow;
	Vcl::Graphics::TBitmap* FBackground;
	bool FShadowFollowsColor;
	Vcl::Graphics::TBitmap* FDownBackground;
	bool FBackgroundDrawBorder;
	bool FThinFrame;
	Vcl::Controls::TImageList* FHotImages;
	Vcl::Controls::TImageList* FDownImages;
	Vcl::Controls::TImageList* FDisabledImages;
	Vcl::Controls::TImageList* FImageList;
	bool FOldStyled;
	bool FUseImageList;
	bool FUseIcon;
	Elsndmap::TElSoundMap* FSoundMap;
	Elsndmap::TElSoundName FDownSound;
	Elsndmap::TElSoundName FUpSound;
	Elsndmap::TElSoundName FClickSound;
	Elsndmap::TElSoundName FArrowClickSound;
	bool FIsSwitch;
	bool FShowGlyph;
	bool FShowText;
	bool FUseArrow;
	bool FMultiLine;
	int FGroupIndex;
	TElButtonGlyph* FGlyph;
	bool FDown;
	bool FArrDown;
	bool FInMenu;
	bool FIgnoreClick;
	bool FDragging;
	bool FAllowAllUp;
	Vcl::Buttons::TButtonLayout FLayout;
	int FSpacing;
	int FMargin;
	bool FFlat;
	bool FMouseInArrow;
	bool FMouseInControl;
	bool FDisableAp;
	TPopupPlace FPopupPlace;
	bool FDefault;
	bool FCancel;
	System::Uitypes::TModalResult FModalResult;
	bool FClicksDisabled;
	Vcl::Imglist::TChangeLink* FChLink;
	Vcl::Imglist::TChangeLink* FNChLink;
	Vcl::Imglist::TChangeLink* FDChLink;
	Vcl::Imglist::TChangeLink* FHChLink;
	Vcl::Menus::TPopupMenu* FPullDownMenu;
	System::Classes::TNotifyEvent FOnArrowClick;
	bool FTransparent;
	Elvclutils::TElTextDrawType FTextDrawType;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	bool FShowBorder;
	bool FAdjustSpaceForGlyph;
	System::Uitypes::TColor FLinkColor;
	Vcl::Menus::TPopupMenu* FLinkPopupMenu;
	System::Uitypes::TFontStyles FLinkStyle;
	bool FIsHTML;
	Htmlrender::TElHTMLRender* FRender;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	bool FUseXPThemes;
	NativeUInt FTheme;
	NativeUInt FArrTheme;
	HWND FWnd;
	System::Classes::TAlignment FAlignment;
	void __fastcall SetAlignment(System::Classes::TAlignment Value);
	void __fastcall SetIsHTML(bool Value);
	HIDESBASE bool __fastcall IsColorStored();
	void __fastcall SetShowBorder(bool newValue);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	virtual void __fastcall SetTransparent(bool newValue);
	void __fastcall SetTextDrawType(Elvclutils::TElTextDrawType newValue);
	void __fastcall SetPullDownMenu(Vcl::Menus::TPopupMenu* newValue);
	void __fastcall SetPopupPlace(TPopupPlace Value);
	void __fastcall SetDisableAp(bool Value);
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall UpdateExclusive();
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	Vcl::Buttons::TNumGlyphs __fastcall GetNumGlyphs();
	void __fastcall SetNumGlyphs(Vcl::Buttons::TNumGlyphs Value);
	void __fastcall SetDown(bool Value);
	void __fastcall SetAllowAllUp(bool Value);
	void __fastcall SetGroupIndex(int Value);
	virtual void __fastcall SetLayout(Vcl::Buttons::TButtonLayout Value);
	virtual void __fastcall SetSpacing(int Value);
	virtual void __fastcall SetMargin(int Value);
	void __fastcall UpdateTracking();
	void __fastcall IntMouseEnter();
	void __fastcall IntMouseLeave();
	void __fastcall IntEnabledChanged();
	void __fastcall IntTextChanged();
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMButtonPressed(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMDialogKey(Winapi::Messages::TWMKey &Message);
	MESSAGE void __fastcall CMDialogChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall SetShowGlyph(bool newValue);
	virtual void __fastcall SetShowText(bool newValue);
	Vcl::Graphics::TIcon* __fastcall GetIcon();
	void __fastcall SetIcon(Vcl::Graphics::TIcon* newValue);
	void __fastcall SetIsSwitch(bool newValue);
	void __fastcall SetSoundMap(Elsndmap::TElSoundMap* newValue);
	virtual void __fastcall SetImageIndex(int newValue);
	int __fastcall GetImageIndex();
	virtual void __fastcall SetUseIcon(bool newValue);
	virtual void __fastcall SetImageList(Vcl::Controls::TImageList* newValue);
	virtual void __fastcall SetUseImageList(bool newValue);
	bool __fastcall GetUseImageList();
	void __fastcall SetOldStyled(bool newValue);
	void __fastcall SetDownImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetHotImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetDisabledImages(Vcl::Controls::TImageList* newValue);
	virtual void __fastcall ImagesChanged(System::TObject* Sender);
	void __fastcall SetThinFrame(bool newValue);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetDownBackground(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetBackgroundDrawBorder(bool Value);
	void __fastcall SetShadowFollowsColor(bool Value);
	void __fastcall SetShadowsUseCustom(bool Value);
	void __fastcall SetShadowBtnHighlight(System::Uitypes::TColor Value);
	void __fastcall SetShadowBtnShadow(System::Uitypes::TColor Value);
	void __fastcall SetShadowBtnDkShadow(System::Uitypes::TColor Value);
	void __fastcall SetAdjustSpaceForGlyph(bool Value);
	void __fastcall SetUseXPThemes(const bool Value);
	void __fastcall CreateThemeHandle();
	void __fastcall FreeThemeHandle();
	TElButtonState FOrigState;
	TElButtonState FState;
	Elstrutils::TElFString FCaption;
	System::WideString FHint;
	bool FMoneyFlat;
	System::Uitypes::TColor FMoneyFlatDownColor;
	System::Uitypes::TColor FMoneyFlatActiveColor;
	System::Uitypes::TColor FMoneyFlatInactiveColor;
	bool FShortcutsEnabled;
	bool FImageIsAlphaBlended;
	System::Types::TRect FTextRect;
	bool FChangeDisabledText;
	void __fastcall SetLinkPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	void __fastcall DoLinkPopup(const System::Types::TPoint &MousePos);
	void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	virtual void __fastcall TriggerLinkClickEvent(Elstrutils::TElFString HRef);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	virtual void __fastcall SetUseArrow(bool newValue);
	DYNAMIC HPALETTE __fastcall GetPalette();
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Paint();
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall SetFlat(bool Value);
	virtual bool __fastcall GetChecked();
	virtual void __fastcall SetChecked(bool newValue);
	bool __fastcall DoSaveShadows();
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	void __fastcall SetCaption(Elstrutils::TElFString Value);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	virtual int __fastcall GetThemePartID();
	virtual int __fastcall GetThemeStateID();
	virtual System::WideString __fastcall GetThemedClassName();
	virtual int __fastcall GetArrowThemePartID();
	virtual int __fastcall GetArrowThemeStateID();
	virtual System::WideString __fastcall GetArrowThemedClassName();
	virtual int __fastcall GetArrowSize();
	virtual void __fastcall DrawThemedBackground(Vcl::Graphics::TCanvas* Canvas);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TMessage &Message);
	System::Types::TPoint __fastcall MeasureButton(bool LockHeight);
	void __fastcall SetMoneyFlat(bool Value);
	void __fastcall SetMoneyFlatDownColor(System::Uitypes::TColor Value);
	void __fastcall SetMoneyFlatActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetMoneyFlatInactiveColor(System::Uitypes::TColor Value);
	bool __fastcall GetMoneyFlat();
	void __fastcall IntWndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall DoPullMenu();
	void __fastcall SetShortcutsEnabled(bool Value);
	virtual bool __fastcall Focused();
	void __fastcall SetImageIsAlphaBlended(bool Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE virtual void __fastcall SetCursor(System::Uitypes::TCursor Value);
	void __fastcall SetChangeDisabledText(bool Value);
	__property bool ClicksDisabled = {read=FClicksDisabled, write=FClicksDisabled, nodefault};
	__property Vcl::Menus::TPopupMenu* PullDownMenu = {read=FPullDownMenu, write=SetPullDownMenu};
	__property TPopupPlace PopupPlace = {read=FPopupPlace, write=SetPopupPlace, default=0};
	__property bool DisableAutoPopup = {read=FDisableAp, write=SetDisableAp, default=0};
	__property bool Cancel = {read=FCancel, write=FCancel, default=0};
	__property System::Uitypes::TModalResult ModalResult = {read=FModalResult, write=FModalResult, default=0};
	__property bool AllowAllUp = {read=FAllowAllUp, write=SetAllowAllUp, default=0};
	__property int GroupIndex = {read=FGroupIndex, write=SetGroupIndex, default=0};
	__property bool Down = {read=FDown, write=SetDown, default=0};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property Vcl::Buttons::TButtonLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property int Margin = {read=FMargin, write=SetMargin, default=-1};
	__property Vcl::Buttons::TNumGlyphs NumGlyphs = {read=GetNumGlyphs, write=SetNumGlyphs, nodefault};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=4};
	__property bool UseArrow = {read=FUseArrow, write=SetUseArrow, default=0};
	__property bool ShadowFollowsColor = {read=FShadowFollowsColor, write=SetShadowFollowsColor, default=1};
	__property bool ShowGlyph = {read=FShowGlyph, write=SetShowGlyph, default=1};
	__property bool ShowText = {read=FShowText, write=SetShowText, default=1};
	__property System::Classes::TNotifyEvent OnArrowClick = {read=FOnArrowClick, write=FOnArrowClick};
	__property Vcl::Graphics::TIcon* Icon = {read=GetIcon, write=SetIcon};
	__property bool UseIcon = {read=FUseIcon, write=SetUseIcon, default=0};
	__property bool IsSwitch = {read=FIsSwitch, write=SetIsSwitch, default=0};
	__property Elsndmap::TElSoundName DownSound = {read=FDownSound, write=FDownSound};
	__property Elsndmap::TElSoundName UpSound = {read=FUpSound, write=FUpSound};
	__property Elsndmap::TElSoundName ClickSound = {read=FClickSound, write=FClickSound};
	__property Elsndmap::TElSoundName ArrowClickSound = {read=FArrowClickSound, write=FArrowClickSound};
	__property Elsndmap::TElSoundMap* SoundMap = {read=FSoundMap, write=SetSoundMap};
	__property int ImageIndex = {read=GetImageIndex, write=SetImageIndex, default=-1};
	__property Vcl::Controls::TImageList* Images = {read=FImageList, write=SetImageList};
	__property Vcl::Controls::TImageList* HotImages = {read=FHotImages, write=SetHotImages};
	__property Vcl::Controls::TImageList* DisabledImages = {read=FDisabledImages, write=SetDisabledImages};
	__property Vcl::Controls::TImageList* DownImages = {read=FDownImages, write=SetDownImages};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property bool ShowBorder = {read=FShowBorder, write=SetShowBorder, default=1};
	__property bool ShadowsUseCustom = {read=FShadowsUseCustom, write=SetShadowsUseCustom, default=0};
	__property System::Uitypes::TColor ShadowBtnHighlight = {read=FShadowBtnHighlight, write=SetShadowBtnHighlight, stored=DoSaveShadows, default=16250869};
	__property System::Uitypes::TColor ShadowBtnShadow = {read=FShadowBtnShadow, write=SetShadowBtnShadow, stored=DoSaveShadows, default=7764576};
	__property System::Uitypes::TColor ShadowBtnDkShadow = {read=FShadowBtnDkShadow, write=SetShadowBtnDkShadow, stored=DoSaveShadows, default=5856328};
	__property bool UseImageList = {read=GetUseImageList, write=SetUseImageList, default=0};
	__property bool OldStyled = {read=FOldStyled, write=SetOldStyled, default=0};
	__property bool ThinFrame = {read=FThinFrame, write=SetThinFrame, default=0};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property Vcl::Graphics::TBitmap* DownBackground = {read=FDownBackground, write=SetDownBackground};
	__property bool BackgroundDrawBorder = {read=FBackgroundDrawBorder, write=SetBackgroundDrawBorder, default=0};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property Elvclutils::TElTextDrawType TextDrawType = {read=FTextDrawType, write=SetTextDrawType, default=0};
	__property bool Checked = {read=GetChecked, write=SetChecked, default=0};
	__property Color = {stored=IsColorStored};
	__property bool AdjustSpaceForGlyph = {read=FAdjustSpaceForGlyph, write=SetAdjustSpaceForGlyph, default=1};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property bool MoneyFlat = {read=GetMoneyFlat, write=SetMoneyFlat, default=0};
	__property System::Uitypes::TColor MoneyFlatDownColor = {read=FMoneyFlatDownColor, write=SetMoneyFlatDownColor, stored=GetMoneyFlat, nodefault};
	__property System::Uitypes::TColor MoneyFlatActiveColor = {read=FMoneyFlatActiveColor, write=SetMoneyFlatActiveColor, stored=GetMoneyFlat, nodefault};
	__property System::Uitypes::TColor MoneyFlatInactiveColor = {read=FMoneyFlatInactiveColor, write=SetMoneyFlatInactiveColor, stored=GetMoneyFlat, nodefault};
	__property bool ImageIsAlphaBlended = {read=FImageIsAlphaBlended, write=SetImageIsAlphaBlended, default=0};
	__property bool ChangeDisabledText = {read=FChangeDisabledText, write=SetChangeDisabledText, default=1};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=2};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, default=0};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=SetLinkColor, default=16711680};
	__property Vcl::Menus::TPopupMenu* LinkPopupMenu = {read=FLinkPopupMenu, write=SetLinkPopupMenu};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=SetLinkStyle, nodefault};
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	
public:
	__fastcall virtual TCustomElGraphicButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElGraphicButton();
	virtual void __fastcall AClick(bool Arrow);
	bool __fastcall IsThemeApplied();
	DYNAMIC void __fastcall Click();
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
	__property NativeUInt Theme = {read=FTheme, nodefault};
	__property System::Classes::TWndMethod MenuWindowProc = {read=FMenuWindowProc, write=FMenuWindowProc};
	__property bool ShortcutsEnabled = {read=FShortcutsEnabled, write=SetShortcutsEnabled, default=0};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
};


typedef System::TMetaClass* TCustomElGraphicButtonClass;

class PASCALIMPLEMENTATION TElGraphicButton : public TCustomElGraphicButton
{
	typedef TCustomElGraphicButton inherited;
	
__published:
	__property Alignment = {default=2};
	__property Background;
	__property BackgroundDrawBorder = {default=0};
	__property DownBackground;
	__property ImageIsAlphaBlended = {default=0};
	__property ImageIndex = {default=-1};
	__property UseImageList = {default=0};
	__property Images;
	__property HotImages;
	__property DisabledImages;
	__property PullDownMenu;
	__property PopupPlace = {default=0};
	__property DisableAutoPopup = {default=0};
	__property Cancel = {default=0};
	__property ChangeDisabledText = {default=1};
	__property ModalResult = {default=0};
	__property MoneyFlat = {default=0};
	__property MoneyFlatInactiveColor;
	__property MoneyFlatActiveColor;
	__property MoneyFlatDownColor;
	__property AdjustSpaceForGlyph = {default=1};
	__property AllowAllUp = {default=0};
	__property GroupIndex = {default=0};
	__property Down = {default=0};
	__property Flat = {default=0};
	__property Glyph;
	__property ImageForm;
	__property IsHTML = {default=0};
	__property Layout = {default=0};
	__property Margin = {default=-1};
	__property NumGlyphs;
	__property ShadowFollowsColor = {default=1};
	__property ShadowsUseCustom = {default=0};
	__property ShadowBtnHighlight = {default=16250869};
	__property ShadowBtnShadow = {default=7764576};
	__property ShadowBtnDkShadow = {default=5856328};
	__property ShowGlyph = {default=1};
	__property ShowText = {default=1};
	__property Spacing = {default=4};
	__property UseArrow = {default=0};
	__property IsSwitch = {default=0};
	__property OnArrowClick;
	__property Icon;
	__property LinkColor = {default=16711680};
	__property LinkPopupMenu;
	__property LinkStyle;
	__property UseIcon = {default=0};
	__property ThinFrame = {default=0};
	__property TextDrawType = {default=0};
	__property Transparent = {default=0};
	__property DownSound = {default=0};
	__property UpSound = {default=0};
	__property ClickSound = {default=0};
	__property ArrowClickSound = {default=0};
	__property SoundMap;
	__property DownImages;
	__property ShowBorder = {default=1};
	__property ShortcutsEnabled = {default=0};
	__property OldStyled = {default=0};
	__property UseXPThemes = {default=1};
	__property Caption;
	__property Cursor;
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
	__property OnImageNeeded;
	__property OnLinkClick;
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnStartDrag;
	__property Anchors = {default=3};
	__property Action;
	__property Constraints;
	__property DockOrientation;
	__property Floating;
	__property DragKind = {default=0};
	__property OnStartDock;
	__property OnEndDock;
public:
	/* TCustomElGraphicButton.Create */ inline __fastcall virtual TElGraphicButton(System::Classes::TComponent* AOwner) : TCustomElGraphicButton(AOwner) { }
	/* TCustomElGraphicButton.Destroy */ inline __fastcall virtual ~TElGraphicButton() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TElGlyphCache* GlyphCache;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* Pattern;
extern DELPHI_PACKAGE int ButtonCount;
extern DELPHI_PACKAGE int MenuCancelMsg;
extern DELPHI_PACKAGE HMENU __fastcall GetMenuHandle(Vcl::Menus::TMenu* AMenu);
extern DELPHI_PACKAGE void __fastcall CreateBrushPattern(void);
}	/* namespace Elpopbtn */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELPOPBTN)
using namespace Elpopbtn;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElpopbtnHPP
