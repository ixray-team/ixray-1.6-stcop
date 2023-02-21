// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElPgCtl.pas' rev: 35.00 (Windows)

#ifndef ElpgctlHPP
#define ElpgctlHPP

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
#include <Vcl.Consts.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <System.Win.ComObj.hpp>
#include <Vcl.ImgList.hpp>
#include <ElHintWnd.hpp>
#include <ElTimers.hpp>
#include <ElSndMap.hpp>
#include <ElImgFrm.hpp>
#include <ElList.hpp>
#include <ElVCLUtils.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElXPThemedControl.hpp>
#include <ElStrUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elpgctl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElTabSheet;
class DELPHICLASS TElPageControl;
class DELPHICLASS TElTabs;
class DELPHICLASS TElTab;
class DELPHICLASS TElStdTab;
class DELPHICLASS TElBtnTab;
class DELPHICLASS TElFlatBtnTab;
class DELPHICLASS TElNetTab;
class DELPHICLASS TEl2DFlatTab;
class DELPHICLASS TElAngledTab;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElTabStyle : unsigned char { etsTabs, etsButtons, etsFlatButtons, etsNetTabs, etsFlatTabs, etsAngledTabs };

enum DECLSPEC_DENUM TElTabPosition : unsigned char { etpTop, etpBottom, etpRight, etpLeft };

enum DECLSPEC_DENUM TElTabDrawState : unsigned char { edsBackground, edsEdges, edsContents };

typedef void __fastcall (__closure *TElMeasureTabEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* Canvas, TElTabSheet* Page, System::Types::TSize &Size);

typedef void __fastcall (__closure *TElPaintTabEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* Canvas, TElTabSheet* Page, const System::Types::TRect &Rect, TElTabDrawState DrawStep, bool &DefaultDrawing);

typedef void __fastcall (__closure *TElTabGetImageEvent)(System::TObject* Sender, int PageIndex, int &ImageIndex);

typedef void __fastcall (__closure *TElTabChangingEvent)(System::TObject* Sender, TElTabSheet* NewPage, bool &AllowChange);

class PASCALIMPLEMENTATION TElTabSheet : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	bool FTabShowing;
	System::Types::TRect ARect;
	bool AComplete;
	int ALine;
	bool AShown;
	HIDESBASE MESSAGE void __fastcall CMShowingChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	void __fastcall SetTabShowing(bool Value);
	void __fastcall UpdateTabShowing();
	int __fastcall GetPageIndex();
	void __fastcall SetPageIndex(int Value);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	NativeUInt __fastcall GetBtnTheme();
	NativeUInt __fastcall GetScrollTheme();
	NativeUInt __fastcall GetTabTheme();
	bool __fastcall GetUseXPThemes();
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Winapi::Messages::TWMKey &Message);
	
protected:
	System::Classes::TNotifyEvent FOnShow;
	System::Classes::TNotifyEvent FOnHide;
	System::Uitypes::TColor FTabColor;
	TElPageControl* FPageControl;
	int FImageIndex;
	bool FTabVisible;
	Elstrutils::TElFString FCaption;
	bool FTabEnabled;
	Vcl::Menus::TPopupMenu* FTabMenu;
	System::WideString FHint;
	bool FUseTabColor;
	virtual void __fastcall TriggerShowEvent();
	virtual void __fastcall TriggerHideEvent();
	void __fastcall SetTabColor(System::Uitypes::TColor Value);
	void __fastcall SetPageControl(TElPageControl* Value);
	void __fastcall SetImageIndex(int Value);
	void __fastcall SetTabVisible(bool Value);
	void __fastcall SetCaption(Elstrutils::TElFString Value);
	virtual void __fastcall Paint();
	int __fastcall GetTabIndex();
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	void __fastcall SetTabEnabled(bool Value);
	void __fastcall SetTabMenu(Vcl::Menus::TPopupMenu* Value);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall ReadState(System::Classes::TReader* Reader);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	void __fastcall SetUseTabColor(bool Value);
	__property bool TabShowing = {read=FTabShowing, nodefault};
	
public:
	__fastcall virtual TElTabSheet(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTabSheet();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	bool __fastcall IsThemeApplied();
	__property int TabIndex = {read=GetTabIndex, nodefault};
	__property NativeUInt TabTheme = {read=GetTabTheme, nodefault};
	__property NativeUInt BtnTheme = {read=GetBtnTheme, nodefault};
	__property NativeUInt ScrollTheme = {read=GetScrollTheme, nodefault};
	__property bool UseXPThemes = {read=GetUseXPThemes, nodefault};
	
__published:
	__property System::Uitypes::TColor TabColor = {read=FTabColor, write=SetTabColor, default=-16777201};
	__property System::Classes::TNotifyEvent OnShow = {read=FOnShow, write=FOnShow};
	__property System::Classes::TNotifyEvent OnHide = {read=FOnHide, write=FOnHide};
	__property TElPageControl* PageControl = {read=FPageControl, write=SetPageControl};
	__property int ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property bool TabVisible = {read=FTabVisible, write=SetTabVisible, default=1};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property int PageIndex = {read=GetPageIndex, write=SetPageIndex, stored=false, nodefault};
	__property bool TabEnabled = {read=FTabEnabled, write=SetTabEnabled, default=1};
	__property Vcl::Menus::TPopupMenu* TabMenu = {read=FTabMenu, write=SetTabMenu};
	__property bool UseTabColor = {read=FUseTabColor, write=SetUseTabColor, default=0};
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property Color = {default=-16777201};
	__property ParentColor = {default=0};
	__property Visible = {default=1};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentFont = {default=1};
	__property Height = {stored=false};
	__property Left = {stored=false};
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDrag;
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Top = {stored=false};
	__property Width = {stored=false};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElTabSheet(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TElPageScrollBtnState : unsigned char { pbsNone, pbsLeftBtnOver, pbsLeftBtnDown, pbsLeftBtnHeld, pbsRightBtnOver, pbsRightBtnDown, pbsRightBtnHeld };

class PASCALIMPLEMENTATION TElPageControl : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
protected:
	int ALines;
	TElTabs* FTabs;
	System::Types::TRect ScrollLeftRect;
	System::Types::TRect ScrollRightRect;
	System::Types::TPoint TabsPos;
	System::Types::TSize TabsSize;
	HDC MemDC;
	TElPageScrollBtnState FScrollBtnState;
	HWND FSaveCapture;
	Vcl::Extctrls::TTimer* FScrollTimer;
	int FTabIndex;
	NativeUInt FBtnTheme;
	NativeUInt FScrollTheme;
	System::Types::TPoint FHintCoords;
	Eltimers::TElTimer* FHintTimer;
	Vcl::Controls::THintWindow* FHintWnd;
	bool FNoDTAlert;
	TElTabSheet* FDragTab;
	int FDoStartDrag;
	bool FDraggablePages;
	Elsndmap::TElSoundName FActivateSound;
	TElTabSheet* FActivePage;
	System::Uitypes::TColor FActiveTabColor;
	Vcl::Graphics::TBitmap* FBackground;
	Elvclutils::TElBkGndType FBackgroundType;
	int FBorderWidth;
	TElTabSheet* FDownTab;
	bool FDrawFocus;
	System::UnicodeString FDummyCaption;
	TElTabSheet* FFirstTab;
	bool FFlat;
	System::Uitypes::TColor FGradientEndColor;
	System::Uitypes::TColor FGradientStartColor;
	int FGradientSteps;
	bool FHotTrack;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	Vcl::Controls::TImageList* FImages;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	System::Uitypes::TColor FInactiveTabColor;
	int FMinTabHeight;
	int FMinTabWidth;
	bool FMultiLine;
	TElTabSheet* FNewDockSheet;
	System::Classes::TNotifyEvent FOnChange;
	TElTabGetImageEvent FOnGetImageIndex;
	TElMeasureTabEvent FOnMeasureTab;
	TElTabChangingEvent FOnChanging;
	Ellist::TElList* FPages;
	bool FRaggedRight;
	bool FScrollOpposite;
	bool FShowBorder;
	bool FShowImages;
	bool FShowTabs;
	Elsndmap::TElSoundMap* FSoundMap;
	TElTabStyle FStyle;
	unsigned FTabHeight;
	TElTabPosition FTabPosition;
	unsigned FTabWidth;
	Vcl::Graphics::TBitmap* FTmpBmp;
	TElTabSheet* FTrackTab;
	TElTabSheet* FUndockingPage;
	System::Uitypes::TColor FTabBkColor;
	System::Classes::TNotifyEvent FOnResize;
	Vcl::Graphics::TFont* FHotTrackFont;
	bool FShowTabHints;
	bool FSavvyMode;
	System::Uitypes::TColor FFlatTabBorderColor;
	System::Uitypes::TColor FTabBkColorNetStyle;
	bool FVerticalSideCaptions;
	Vcl::Graphics::TFont* FActiveTabFont;
	bool FUseActiveTabFont;
	System::Uitypes::TCursor FTabCursor;
	System::WideString FHint;
	TElPaintTabEvent FOnDrawTab;
	TElTabSheet* FDefaultPage;
	HIDESBASEDYNAMIC void __fastcall Resize();
	virtual System::WideString __fastcall GetThemedClassName();
	virtual void __fastcall FreeThemeHandle();
	virtual void __fastcall CreateThemeHandle();
	virtual bool __fastcall CanChange(TElTabSheet* NewPage);
	bool __fastcall CanScrollLeft();
	virtual void __fastcall Change();
	void __fastcall ChangeActivePage(TElTabSheet* Page);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TWMNCPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TWMSetCursor &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMNCLButtonDown(Winapi::Messages::TWMNCHitMessage &Message);
	MESSAGE void __fastcall WMNCLButtonUp(Winapi::Messages::TWMNCHitMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMShowingChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMControlListChange(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDesignHitTest(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogKey(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMFocusChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall CreateHandle();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	int __fastcall GetActivePageIndex();
	DYNAMIC void __fastcall GetChildren(System::Classes::TGetChildProc Proc, System::Classes::TComponent* Root);
	int __fastcall GetPageCount();
	TElTabSheet* __fastcall GetPages(int index);
	int __fastcall GetTabIndex();
	void __fastcall ImageChange(System::TObject* Sender);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall ImageListChange(System::TObject* Sender);
	virtual void __fastcall InsertPage(TElTabSheet* TabSheet);
	void __fastcall MakeTabVisible(TElTabSheet* ATabSheet);
	void __fastcall RebuildTabs(bool ResetFirstItem);
	void __fastcall RedoTmpBmp();
	virtual void __fastcall RemovePage(TElTabSheet* TabSheet);
	void __fastcall SetActivePage(TElTabSheet* Value);
	void __fastcall SetActivePageIndex(const int Value);
	void __fastcall SetActiveTabColor(System::Uitypes::TColor Value);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetBackgroundType(Elvclutils::TElBkGndType newValue);
	HIDESBASE void __fastcall SetBorderWidth(int Value);
	DYNAMIC void __fastcall SetChildOrder(System::Classes::TComponent* Child, int Order);
	void __fastcall SetDrawFocus(bool Value);
	void __fastcall SetFirstTab(TElTabSheet* Value);
	void __fastcall SetFlat(bool newValue);
	void __fastcall SetGradientEndColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientStartColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientSteps(int newValue);
	void __fastcall SetHotTrack(bool newValue);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall SetImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetInactiveTabColor(System::Uitypes::TColor Value);
	void __fastcall SetMinTabHeight(int Value);
	void __fastcall SetMinTabWidth(int Value);
	void __fastcall SetMultiLine(bool newValue);
	void __fastcall SetRaggedRight(const bool Value);
	void __fastcall SetScrollOpposite(const bool Value);
	void __fastcall SetShowBorder(bool Value);
	void __fastcall SetShowImages(bool newValue);
	void __fastcall SetShowTabs(bool Value);
	void __fastcall SetStyle(TElTabStyle newValue);
	void __fastcall SetTabHeight(unsigned newValue);
	void __fastcall SetTabIndex(const int Value);
	void __fastcall SetTabPosition(TElTabPosition newValue);
	void __fastcall SetTabWidth(unsigned newValue);
	virtual void __fastcall ShowControl(Vcl::Controls::TControl* AControl);
	virtual void __fastcall UpdateActivePage();
	void __fastcall UpdateTab(TElTabSheet* TabSheet);
	bool __fastcall CanScrollRight();
	void __fastcall SetHotTrackFont(Vcl::Graphics::TFont* Value);
	void __fastcall HotTrackFontChange(System::TObject* Sender);
	virtual void __fastcall Paint();
	void __fastcall SetScrollBtnState(TElPageScrollBtnState Value);
	void __fastcall SetTrackTab(TElTabSheet* Value);
	void __fastcall OnScrollTimer(System::TObject* Sender);
	void __fastcall SetTabBkColor(System::Uitypes::TColor Value);
	bool __fastcall HasVisibleTabs();
	bool __fastcall DoHitTest(int X, int Y, int &Res);
	void __fastcall UpdateMultilineOrder();
	virtual void __fastcall TriggerGetImageEvent(int PageIndex, int &ImageIndex);
	virtual void __fastcall TriggerMeasureTabEvent(Vcl::Graphics::TCanvas* Canvas, TElTabSheet* Page, System::Types::TSize &Size);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall IFMEffectiveSize(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall IFMCanPaintBkgnd(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall PMRefreshActivePage(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMNCRButtonUp(Winapi::Messages::TWMNCHitMessage &Message);
	MESSAGE void __fastcall WMNCCreate(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCDestroy(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMNCMouseMove(Winapi::Messages::TMessage &Message);
	virtual void __fastcall OnHintTimer(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	void __fastcall SetSavvyMode(bool Value);
	void __fastcall SetFlatTabBorderColor(System::Uitypes::TColor Value);
	void __fastcall SetTabBkColorNetStyle(System::Uitypes::TColor Value);
	void __fastcall SetVerticalSideCaptions(bool Value);
	void __fastcall SetDraggablePages(bool Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	void __fastcall SetActiveTabFont(Vcl::Graphics::TFont* Value);
	void __fastcall SetUseActiveTabFont(bool Value);
	void __fastcall ActiveTabFontChange(System::TObject* Sender);
	virtual void __fastcall TriggerDrawTabEvent(Vcl::Graphics::TCanvas* Canvas, TElTabSheet* Page, const System::Types::TRect &Rect, TElTabDrawState DrawStep, bool &DefaultDrawing);
	__property TElPageScrollBtnState ScrollBtnState = {read=FScrollBtnState, write=SetScrollBtnState, nodefault};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property Elvclutils::TElBkGndType BackgroundType = {read=FBackgroundType, write=SetBackgroundType, default=2};
	__property System::Uitypes::TColor GradientEndColor = {read=FGradientEndColor, write=SetGradientEndColor, nodefault};
	__property System::Uitypes::TColor GradientStartColor = {read=FGradientStartColor, write=SetGradientStartColor, nodefault};
	__property int GradientSteps = {read=FGradientSteps, write=SetGradientSteps, default=16};
	
public:
	__fastcall virtual TElPageControl(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElPageControl();
	TElTabSheet* __fastcall FindNextPage(TElTabSheet* CurPage, bool GoForward, bool CheckTabVisible, bool CheckTabEnabled);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SelectNextPage(bool GoForward);
	virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);
	TElTabSheet* __fastcall TabFromPoint(const System::Types::TPoint &Point);
	void __fastcall UpdateTabs(bool Immediate);
	TElTabSheet* __fastcall NewPage();
	virtual void __fastcall Loaded();
	__property int ActivePageIndex = {read=GetActivePageIndex, write=SetActivePageIndex, nodefault};
	__property TElTabSheet* FirstTab = {read=FFirstTab, write=SetFirstTab};
	__property int PageCount = {read=GetPageCount, nodefault};
	__property TElTabSheet* Pages[int index] = {read=GetPages};
	__property TElTabSheet* TrackTab = {read=FTrackTab, write=SetTrackTab};
	__property NativeUInt BtnTheme = {read=FBtnTheme, nodefault};
	__property NativeUInt ScrollTheme = {read=FScrollTheme, nodefault};
	__property System::Uitypes::TColor TabBkColorNetStyle = {read=FTabBkColorNetStyle, write=SetTabBkColorNetStyle, nodefault};
	
__published:
	__property Elsndmap::TElSoundName ActivateSound = {read=FActivateSound, write=FActivateSound};
	__property System::Uitypes::TColor ActiveTabColor = {read=FActiveTabColor, write=SetActiveTabColor, default=-16777201};
	__property int BorderWidth = {read=FBorderWidth, write=SetBorderWidth, nodefault};
	__property System::UnicodeString Caption = {read=FDummyCaption};
	__property Color = {default=-16777201};
	__property bool DrawFocus = {read=FDrawFocus, write=SetDrawFocus, nodefault};
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property bool HotTrack = {read=FHotTrack, write=SetHotTrack, default=1};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property System::Uitypes::TColor InactiveTabColor = {read=FInactiveTabColor, write=SetInactiveTabColor, default=-16777201};
	__property int MinTabHeight = {read=FMinTabHeight, write=SetMinTabHeight, default=40};
	__property int MinTabWidth = {read=FMinTabWidth, write=SetMinTabWidth, default=40};
	__property bool Multiline = {read=FMultiLine, write=SetMultiLine, nodefault};
	__property TElTabChangingEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TElTabGetImageEvent OnGetImageIndex = {read=FOnGetImageIndex, write=FOnGetImageIndex};
	__property TElMeasureTabEvent OnMeasureTab = {read=FOnMeasureTab, write=FOnMeasureTab};
	__property bool RaggedRight = {read=FRaggedRight, write=SetRaggedRight, nodefault};
	__property bool ScrollOpposite = {read=FScrollOpposite, write=SetScrollOpposite, nodefault};
	__property bool ShowBorder = {read=FShowBorder, write=SetShowBorder, default=1};
	__property bool ShowImages = {read=FShowImages, write=SetShowImages, default=1};
	__property bool ShowTabs = {read=FShowTabs, write=SetShowTabs, default=1};
	__property Elsndmap::TElSoundMap* SoundMap = {read=FSoundMap, write=FSoundMap};
	__property TElTabStyle Style = {read=FStyle, write=SetStyle, nodefault};
	__property unsigned TabHeight = {read=FTabHeight, write=SetTabHeight, default=0};
	__property int TabIndex = {read=GetTabIndex, write=SetTabIndex, default=-1};
	__property TElTabPosition TabPosition = {read=FTabPosition, write=SetTabPosition, nodefault};
	__property unsigned TabWidth = {read=FTabWidth, write=SetTabWidth, default=0};
	__property Vcl::Graphics::TFont* HotTrackFont = {read=FHotTrackFont, write=SetHotTrackFont};
	__property System::Uitypes::TColor TabBkColor = {read=FTabBkColor, write=SetTabBkColor, default=-16777201};
	__property TElTabSheet* ActivePage = {read=FActivePage, write=SetActivePage};
	__property bool ShowTabHints = {read=FShowTabHints, write=FShowTabHints, default=1};
	__property bool SavvyMode = {read=FSavvyMode, write=SetSavvyMode, default=0};
	__property System::Uitypes::TColor FlatTabBorderColor = {read=FFlatTabBorderColor, write=SetFlatTabBorderColor, default=-16777200};
	__property bool VerticalSideCaptions = {read=FVerticalSideCaptions, write=SetVerticalSideCaptions, default=1};
	__property bool DraggablePages = {read=FDraggablePages, write=SetDraggablePages, default=0};
	__property Vcl::Graphics::TFont* ActiveTabFont = {read=FActiveTabFont, write=SetActiveTabFont};
	__property bool UseActiveTabFont = {read=FUseActiveTabFont, write=SetUseActiveTabFont, default=0};
	__property System::Uitypes::TCursor TabCursor = {read=FTabCursor, write=FTabCursor, default=0};
	__property TElPaintTabEvent OnDrawTab = {read=FOnDrawTab, write=FOnDrawTab};
	__property TElTabSheet* DefaultPage = {read=FDefaultPage, write=FDefaultPage};
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property Align = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property ShowHint;
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property OnDblClick;
	__property OnStartDrag;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnEnter;
	__property OnExit;
	__property OnStartDock;
	__property OnUnDock;
	__property OnDockDrop;
	__property OnDockOver;
	__property OnEndDock;
	__property OnResize;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElPageControl(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


typedef System::TMetaClass* TElTabClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElTabs : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TElPageControl* FPageControl;
	TElTab* FTab;
	System::Types::TSize __fastcall MeasureSheet(Vcl::Graphics::TCanvas* ACanvas, TElTabSheet* Sheet);
	void __fastcall DoDrawTabs(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &Rect, bool DoDraw, System::Types::TSize &Size);
	NativeUInt __fastcall GetBtnTheme();
	NativeUInt __fastcall GetScrollTheme();
	NativeUInt __fastcall GetTabTheme();
	
protected:
	TElTabClass FTabClass;
	virtual System::Types::TSize __fastcall CalcTabAreaSize();
	void __fastcall DrawTabs(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &R, bool DoDraw, System::Types::TSize &Size);
	void __fastcall SetTabClass(TElTabClass Value);
	HFONT __fastcall GetRotatedFont(Vcl::Graphics::TCanvas* Canvas, int RotationAngle);
	void __fastcall ReorderPages(int MaxRows);
	bool __fastcall IsThemeApplied();
	__property NativeUInt TabTheme = {read=GetTabTheme, nodefault};
	__property NativeUInt BtnTheme = {read=GetBtnTheme, nodefault};
	__property NativeUInt ScrollTheme = {read=GetScrollTheme, nodefault};
	
public:
	__fastcall TElTabs(TElPageControl* PageControl);
	__fastcall virtual ~TElTabs();
	__property TElTabClass TabClass = {read=FTabClass, write=SetTabClass};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElTab : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TElTabs* FOwner;
	void __fastcall Draw(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &R, TElTabSheet* TabSheet);
	NativeUInt __fastcall GetBtnTheme();
	NativeUInt __fastcall GetScrollTheme();
	NativeUInt __fastcall GetTabTheme();
	TElTabPosition __fastcall GetTabPosition();
	
protected:
	virtual int __fastcall GetOuterMargin();
	virtual int __fastcall GetInnerMargin();
	virtual void __fastcall DrawTabContents(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R, TElTabSheet* TabSheet);
	virtual void __fastcall DrawSpace(Vcl::Graphics::TCanvas* Canvas, int &cx, int &cy, int mx, int my);
	virtual bool __fastcall CanDrawTab(bool ActiveDraw);
	virtual int __fastcall GetAscend();
	virtual void __fastcall AdjustDrawingSize(bool Active, System::Types::TRect &R);
	virtual void __fastcall AdjustTabSize(System::Types::TSize &Size);
	virtual void __fastcall AdjustFillSize(bool After, System::Types::TRect &R, TElTabSheet* TabSheet);
	virtual void __fastcall DrawTabLine(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	virtual void __fastcall FillSpace(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect);
	virtual void __fastcall FillTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect, TElTabSheet* TabSheet);
	virtual void __fastcall DrawButtons(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &LeftRect, const System::Types::TRect &RightRect, bool CSL, bool CSR);
	virtual int __fastcall GetRowMargin();
	bool __fastcall IsThemeApplied();
	virtual void __fastcall DrawTabEdges(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &R, TElTabSheet* TabSheet) = 0 ;
	virtual int __fastcall GetContentMargin();
	virtual void __fastcall FixupTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R, TElTabSheet* TabSheet);
	__property NativeUInt TabTheme = {read=GetTabTheme, nodefault};
	__property NativeUInt BtnTheme = {read=GetBtnTheme, nodefault};
	__property NativeUInt ScrollTheme = {read=GetScrollTheme, nodefault};
	__property TElTabPosition TabPosition = {read=GetTabPosition, nodefault};
	
public:
	__fastcall TElTab(TElTabs* Owner);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElTab() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElStdTab : public TElTab
{
	typedef TElTab inherited;
	
protected:
	virtual int __fastcall GetOuterMargin();
	virtual bool __fastcall CanDrawTab(bool ActiveDraw);
	virtual int __fastcall GetAscend();
	virtual void __fastcall AdjustDrawingSize(bool Active, System::Types::TRect &R);
	virtual void __fastcall AdjustTabSize(System::Types::TSize &Size);
	virtual void __fastcall DrawTabLine(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	virtual void __fastcall AdjustFillSize(bool After, System::Types::TRect &R, TElTabSheet* TabSheet);
	virtual void __fastcall FillTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect, TElTabSheet* TabSheet);
	virtual void __fastcall DrawTabEdges(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &R, TElTabSheet* TabSheet);
public:
	/* TElTab.Create */ inline __fastcall TElStdTab(TElTabs* Owner) : TElTab(Owner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElStdTab() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElBtnTab : public TElTab
{
	typedef TElTab inherited;
	
protected:
	virtual int __fastcall GetInnerMargin();
	virtual void __fastcall DrawSpace(Vcl::Graphics::TCanvas* Canvas, int &cx, int &cy, int mx, int my);
	virtual void __fastcall AdjustTabSize(System::Types::TSize &Size);
	virtual int __fastcall GetRowMargin();
	virtual void __fastcall FillTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect, TElTabSheet* TabSheet);
	virtual void __fastcall DrawTabEdges(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &R, TElTabSheet* TabSheet);
public:
	/* TElTab.Create */ inline __fastcall TElBtnTab(TElTabs* Owner) : TElTab(Owner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElBtnTab() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElFlatBtnTab : public TElTab
{
	typedef TElTab inherited;
	
protected:
	virtual int __fastcall GetInnerMargin();
	virtual void __fastcall DrawSpace(Vcl::Graphics::TCanvas* Canvas, int &cx, int &cy, int mx, int my);
	virtual void __fastcall AdjustTabSize(System::Types::TSize &Size);
	virtual void __fastcall AdjustFillSize(bool After, System::Types::TRect &R, TElTabSheet* TabSheet);
	virtual int __fastcall GetRowMargin();
	virtual void __fastcall FillTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect, TElTabSheet* TabSheet);
	virtual void __fastcall DrawTabEdges(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &R, TElTabSheet* TabSheet);
public:
	/* TElTab.Create */ inline __fastcall TElFlatBtnTab(TElTabs* Owner) : TElTab(Owner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElFlatBtnTab() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElNetTab : public TElTab
{
	typedef TElTab inherited;
	
protected:
	virtual void __fastcall AdjustTabSize(System::Types::TSize &Size);
	virtual int __fastcall GetInnerMargin();
	virtual void __fastcall DrawSpace(Vcl::Graphics::TCanvas* Canvas, int &cx, int &cy, int mx, int my);
	virtual bool __fastcall CanDrawTab(bool ActiveDraw);
	virtual void __fastcall AdjustDrawingSize(bool Active, System::Types::TRect &R);
	virtual int __fastcall GetOuterMargin();
	virtual void __fastcall DrawTabLine(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	virtual int __fastcall GetAscend();
	virtual void __fastcall FillSpace(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect);
	virtual void __fastcall AdjustFillSize(bool After, System::Types::TRect &R, TElTabSheet* TabSheet);
	virtual void __fastcall FillTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect, TElTabSheet* TabSheet);
	virtual void __fastcall DrawButtons(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &LeftRect, const System::Types::TRect &RightRect, bool CSL, bool CSR);
	virtual void __fastcall DrawTabEdges(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &R, TElTabSheet* TabSheet);
public:
	/* TElTab.Create */ inline __fastcall TElNetTab(TElTabs* Owner) : TElTab(Owner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElNetTab() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TEl2DFlatTab : public TElTab
{
	typedef TElTab inherited;
	
protected:
	virtual void __fastcall AdjustDrawingSize(bool Active, System::Types::TRect &R);
	virtual void __fastcall AdjustFillSize(bool After, System::Types::TRect &R, TElTabSheet* TabSheet);
	virtual void __fastcall AdjustTabSize(System::Types::TSize &Size);
	virtual bool __fastcall CanDrawTab(bool ActiveDraw);
	virtual void __fastcall DrawTabLine(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	virtual int __fastcall GetAscend();
	virtual int __fastcall GetInnerMargin();
	virtual int __fastcall GetOuterMargin();
	virtual void __fastcall FillTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect, TElTabSheet* TabSheet);
	virtual void __fastcall DrawTabEdges(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &R, TElTabSheet* TabSheet);
public:
	/* TElTab.Create */ inline __fastcall TEl2DFlatTab(TElTabs* Owner) : TElTab(Owner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEl2DFlatTab() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElAngledTab : public TElTab
{
	typedef TElTab inherited;
	
private:
	int SaveDCState;
	
protected:
	virtual bool __fastcall CanDrawTab(bool ActiveDraw);
	virtual void __fastcall DrawTabEdges(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &R, TElTabSheet* TabSheet);
	virtual void __fastcall DrawTabLine(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	virtual void __fastcall AdjustFillSize(bool After, System::Types::TRect &R, TElTabSheet* TabSheet);
	virtual void __fastcall AdjustDrawingSize(bool Active, System::Types::TRect &R);
	virtual void __fastcall AdjustTabSize(System::Types::TSize &Size);
	virtual void __fastcall FillTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect, TElTabSheet* TabSheet);
	virtual int __fastcall GetAscend();
	virtual int __fastcall GetInnerMargin();
	virtual int __fastcall GetOuterMargin();
	virtual int __fastcall GetContentMargin();
	void __fastcall CreateTabPoints(const System::Types::TRect &R, System::Types::PPoint Points);
	virtual void __fastcall FixupTab(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R, TElTabSheet* TabSheet);
public:
	/* TElTab.Create */ inline __fastcall TElAngledTab(TElTabs* Owner) : TElTab(Owner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElAngledTab() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word PM_REFRESHACTIVEPAGE = System::Word(0x210a);
}	/* namespace Elpgctl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELPGCTL)
using namespace Elpgctl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElpgctlHPP
