// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElSideBar.pas' rev: 35.00 (Windows)

#ifndef ElsidebarHPP
#define ElsidebarHPP

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
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElPanel.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElTools.hpp>
#include <ElSndMap.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <Vcl.ImgList.hpp>
#include <ElExtBkgnd.hpp>
#include <ElIni.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elsidebar
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElSideBarContainerPanel;
class DELPHICLASS TElSideBarCItem;
class DELPHICLASS TElSideBarItem;
class DELPHICLASS TElSideBarItems;
class DELPHICLASS TElSideBarSection;
class DELPHICLASS TElSideBarSections;
class DELPHICLASS TElSideBar;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElSideBarPart : unsigned char { sbpSection, sbpItemImage, sbpItemText, sbpUpScroll, sbpDownScroll, sbpInside };

enum DECLSPEC_DENUM TElSideBarIconLocation : unsigned char { ilLeft, ilTop };

class PASCALIMPLEMENTATION TElSideBarContainerPanel : public Elpanel::TElPanel
{
	typedef Elpanel::TElPanel inherited;
	
protected:
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Message);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	
public:
	__fastcall virtual TElSideBarContainerPanel(System::Classes::TComponent* AOwner);
public:
	/* TCustomElPanel.Destroy */ inline __fastcall virtual ~TElSideBarContainerPanel() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElSideBarContainerPanel(HWND ParentWindow) : Elpanel::TElPanel(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElSideBarCItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TElSideBar* FBar;
	bool FVisible;
	bool FEnabled;
	int FImageIndex;
	Elstrutils::TElFString FCaption;
	int FTag;
	Elstrutils::TElFString FHint;
	System::Types::TRect FTextRect;
	System::Types::TRect FBoundRect;
	bool FPartial;
	void __fastcall SetVisible(bool newValue);
	void __fastcall SetEnabled(bool newValue);
	void __fastcall SetImageIndex(int newValue);
	void __fastcall SetCaption(Elstrutils::TElFString newValue);
	
protected:
	TElSideBar* __fastcall GetBar();
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	bool Active;
	bool Hidden;
	bool Disabled;
	void *Data;
	__fastcall virtual TElSideBarCItem(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Index;
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property Elstrutils::TElFString Hint = {read=FHint, write=FHint};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property int ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TElSideBarCItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElSideBarItem : public TElSideBarCItem
{
	typedef TElSideBarCItem inherited;
	
public:
	__fastcall virtual TElSideBarItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElSideBarItem();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElSideBarItems : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElSideBarItem* operator[](int index) { return this->Items[index]; }
	
private:
	TElSideBarSection* FSection;
	TElSideBarItem* __fastcall GetItems(int index);
	void __fastcall SetItems(int index, TElSideBarItem* newValue);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	HIDESBASE TElSideBarItem* __fastcall Add();
	__fastcall TElSideBarItems(TElSideBarSection* Section);
	__property TElSideBarItem* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TElSideBarItems() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElSideBarSection : public TElSideBarCItem
{
	typedef TElSideBarCItem inherited;
	
private:
	bool FContainsControls;
	TElSideBarItems* FItems;
	TElSideBarSections* FSections;
	TElSideBarContainerPanel* FPanel;
	bool FInactive;
	void __fastcall SetItems(TElSideBarItems* newValue);
	void __fastcall SetContainsControls(bool newValue);
	void __fastcall AllocatePanel();
	
public:
	__fastcall virtual TElSideBarSection(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElSideBarSection();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TElSideBarContainerPanel* Panel = {read=FPanel};
	
__published:
	__property bool Inactive = {read=FInactive, write=FInactive, nodefault};
	__property TElSideBarItems* Items = {read=FItems, write=SetItems};
	__property bool ContainsControls = {read=FContainsControls, write=SetContainsControls, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElSideBarSections : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElSideBarSection* operator[](int index) { return this->Items[index]; }
	
private:
	TElSideBar* FBar;
	TElSideBarSection* __fastcall GetItems(int index);
	void __fastcall SetItems(int index, TElSideBarSection* newValue);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall TElSideBarSections(TElSideBar* Bar);
	HIDESBASE TElSideBarSection* __fastcall Add();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	__property TElSideBarSection* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TElSideBarSections() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElSideBar : public Elpanel::TCustomElPanel
{
	typedef Elpanel::TCustomElPanel inherited;
	
private:
	bool FVisibleSections;
	Vcl::Graphics::TFont* FItemsFont;
	bool FFlatSections;
	bool FFlatItems;
	bool FFlatActiveItem;
	bool FFlat;
	int FUpdateCount;
	Elsndmap::TElSoundMap* FSoundMap;
	Vcl::Controls::TImageList* FSectionImages;
	Vcl::Controls::TImageList* FSectionHotImages;
	Vcl::Controls::TImageList* FSectionDisabledImages;
	Vcl::Controls::TImageList* FItemDisabledImages;
	Vcl::Controls::TImageList* FItemImages;
	Vcl::Controls::TImageList* FItemHotImages;
	bool FSectionTracking;
	bool FItemTracking;
	bool FUnderlineTracked;
	bool FMouseOver;
	TElSideBarSections* FSections;
	bool FVisUpBtn;
	bool FVisDownBtn;
	bool FUpBtnPressed;
	bool FDownBtnPressed;
	bool FSaveUpBtnPressed;
	bool FSaveDownBtnPressed;
	System::Uitypes::TFontStyles FActiveSectionStyle;
	System::Uitypes::TFontStyles FActiveItemStyle;
	System::Uitypes::TColor FTrackSectionFontColor;
	System::Uitypes::TColor FTrackSectionBkColor;
	System::Uitypes::TColor FTrackItemFontColor;
	System::Uitypes::TColor FTrackItemBkColor;
	HWND FSaveCapture;
	bool FPressed;
	bool FAutoScroll;
	bool Registered;
	TElSideBarItem* FTopItem;
	TElSideBarSection* FSection;
	TElSideBarItem* FItem;
	TElSideBarSection* FTrackSection;
	TElSideBarItem* FTrackItem;
	TElSideBarSection* FDownSection;
	TElSideBarItem* FDownItem;
	TElSideBarSection* FSaveDownSection;
	TElSideBarItem* FSaveDownItem;
	Vcl::Imglist::TChangeLink* FSImagesLink;
	Vcl::Imglist::TChangeLink* FIImagesLink;
	Vcl::Imglist::TChangeLink* FSDImagesLink;
	Vcl::Imglist::TChangeLink* FIDImagesLink;
	Vcl::Imglist::TChangeLink* FSHImagesLink;
	Vcl::Imglist::TChangeLink* FIHImagesLink;
	Vcl::Menus::TPopupMenu* FItemsPopup;
	Vcl::Menus::TPopupMenu* FSectionsPopup;
	int FSectionHeight;
	System::Uitypes::TColor FSectionsColor;
	bool FWordWrap;
	int FSpacing;
	int FTopSpacing;
	int FScrollDelay;
	Vcl::Extctrls::TTimer* FScrollTimer;
	Elsndmap::TElSoundName FItemChangeSound;
	Elsndmap::TElSoundName FSectionChangeSound;
	int FItemSize;
	bool FRightAlignedBar;
	System::Classes::TNotifyEvent FOnItemChange;
	System::Classes::TNotifyEvent FOnSectionChange;
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FAutoSelectItem;
	TElSideBarItem* FDelayItem;
	TElSideBarCItem* FHintItem;
	int FChangeDelay;
	Vcl::Extctrls::TTimer* FDelayTimer;
	bool FKeepSelection;
	Elvclutils::TElBorderSides FBorderSides;
	TElSideBarIconLocation FIconLocation;
	void __fastcall SetActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetInactiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetSectionHeight(int newValue);
	void __fastcall SetSections(TElSideBarSections* newValue);
	int __fastcall GetSectionIndex();
	void __fastcall SetSectionIndex(int newValue);
	int __fastcall GetItemIndex();
	void __fastcall SetItemIndex(int newValue);
	void __fastcall SetSectionsColor(System::Uitypes::TColor newValue);
	void __fastcall SetWordWrap(bool newValue);
	void __fastcall SetSpacing(int newValue);
	void __fastcall SetTopSpacing(int newValue);
	void __fastcall SetScrollDelay(int newValue);
	void __fastcall SetSectionTracking(bool newValue);
	void __fastcall SetItemTracking(bool newValue);
	void __fastcall SetUnderlineTracked(bool newValue);
	void __fastcall SetSoundMap(Elsndmap::TElSoundMap* newValue);
	void __fastcall SetSectionImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetSectionHotImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetSectionDisabledImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetItemDisabledImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetItemImages(Vcl::Controls::TImageList* newValue);
	void __fastcall SetItemHotImages(Vcl::Controls::TImageList* newValue);
	MESSAGE void __fastcall CMControlChange(Vcl::Controls::TCMControlChange &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	MESSAGE void __fastcall WMGetMinMaxInfo(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Winapi::Messages::TWMKey &Msg);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	void __fastcall SetFlat(bool newValue);
	void __fastcall DrawFlatBorder();
	void __fastcall ItemRemoved(System::TObject* Sender, System::Classes::TCollectionItem* Item);
	void __fastcall ItemAdded(System::TObject* Sender, System::Classes::TCollectionItem* Item);
	void __fastcall SectionRemoved(System::TObject* Sender, System::Classes::TCollectionItem* Item);
	void __fastcall SectionAdded(System::TObject* Sender, System::Classes::TCollectionItem* Item);
	void __fastcall SetFlatSections(bool newValue);
	void __fastcall SetFlatItems(bool newValue);
	void __fastcall SetItemsFont(Vcl::Graphics::TFont* newValue);
	Vcl::Graphics::TFont* __fastcall GetSectionsFont();
	void __fastcall SetSectionsFont(Vcl::Graphics::TFont* newValue);
	void __fastcall SectionsFontChanged(System::TObject* Sender);
	void __fastcall ItemsFontChanged(System::TObject* Sender);
	void __fastcall SetVisibleSections(bool newValue);
	void __fastcall ImagesChanged(System::TObject* Sender);
	void __fastcall SetItemSize(int newValue);
	void __fastcall SetRightAlignedBar(bool newValue);
	void __fastcall OnScrollTimer(System::TObject* Sender);
	void __fastcall SetSectionsPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	void __fastcall SetItemsPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	int __fastcall GetTopIndex();
	void __fastcall SetTopIndex(int newValue);
	void __fastcall UpdateTracks();
	void __fastcall SetActiveSectionStyle(System::Uitypes::TFontStyles newValue);
	void __fastcall SetActiveItemStyle(System::Uitypes::TFontStyles newValue);
	void __fastcall SetTrackSectionFontColor(System::Uitypes::TColor Value);
	void __fastcall SetTrackSectionBkColor(System::Uitypes::TColor Value);
	void __fastcall SetTrackItemFontColor(System::Uitypes::TColor Value);
	void __fastcall SetTrackItemBkColor(System::Uitypes::TColor Value);
	void __fastcall SetFlatActiveItem(bool Value);
	void __fastcall SetKeepSelection(bool Value);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	void __fastcall SetIconLocation(TElSideBarIconLocation Value);
	
protected:
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	void __fastcall IntKeyDown(System::Word &Key, System::Classes::TShiftState ShiftState);
	void __fastcall IntMouseDown(System::Uitypes::TMouseButton Button, short XPos, short YPos);
	void __fastcall IntMouseUp(System::Uitypes::TMouseButton Button, short XPos, short YPos);
	void __fastcall IntMouseMove(short XPos, short YPos);
	void __fastcall IntMouseWheel(int WheelDelta, const System::Types::TPoint &MousePos);
	bool __fastcall IntHintShow(Vcl::Controls::THintInfo &HintInfo);
	void __fastcall UpdateSectionPanels();
	virtual void __fastcall TriggerItemChangeEvent();
	virtual void __fastcall TriggerSectionChangeEvent();
	DYNAMIC Vcl::Menus::TPopupMenu* __fastcall GetPopupMenu();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	int __fastcall GetSectionHeight();
	int __fastcall GetItemHeight();
	System::Types::TRect __fastcall GetSectionRect(TElSideBarSection* Section);
	System::Types::TRect __fastcall GetItemsRect();
	System::Types::TRect __fastcall GetTopSectionsRect();
	System::Types::TRect __fastcall GetBottomSectionsRect();
	void __fastcall UpdateSection(TElSideBarSection* Section);
	void __fastcall UpdateTopSections();
	void __fastcall UpdateBottomSections();
	void __fastcall UpdateItems();
	void __fastcall UpdateAllSections();
	void __fastcall UpdateBar();
	void __fastcall UpdateFrame();
	virtual int __fastcall GetMinHeight();
	virtual void __fastcall CreateWnd();
	DYNAMIC void __fastcall GetChildren(System::Classes::TGetChildProc Proc, System::Classes::TComponent* Root);
	DYNAMIC System::Classes::TComponent* __fastcall GetChildOwner();
	virtual void __fastcall ShowControl(Vcl::Controls::TControl* AControl);
	virtual void __fastcall AlignControls(Vcl::Controls::TControl* AControl, System::Types::TRect &Rect);
	void __fastcall OnDelayTimer(System::TObject* Sender);
	void __fastcall StartDelayTimer(TElSideBarItem* Item);
	void __fastcall StopDelayTimer();
	virtual void __fastcall DestroyWnd();
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TMessage &Message);
	virtual System::WideString __fastcall GetThemedClassName();
	virtual System::Types::TRect __fastcall GetBackgroundClientRect();
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	
public:
	__fastcall virtual TElSideBar(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElSideBar();
	void __fastcall UpdateChildControl();
	void __fastcall ScrollUp();
	void __fastcall ScrollDown();
	virtual void __fastcall Paint();
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	virtual void __fastcall Loaded();
	void __fastcall GetHitTest(int X, int Y, TElSideBarPart &BarPart, TElSideBarSection* &Section, TElSideBarItem* &Item);
	
__published:
	__property bool AutoSelectItem = {read=FAutoSelectItem, write=FAutoSelectItem, default=1};
	__property TElSideBarSections* Sections = {read=FSections, write=SetSections};
	__property int SectionIndex = {read=GetSectionIndex, write=SetSectionIndex, nodefault};
	__property int ItemIndex = {read=GetItemIndex, write=SetItemIndex, nodefault};
	__property System::Uitypes::TColor SectionsColor = {read=FSectionsColor, write=SetSectionsColor, default=-16777201};
	__property bool WordWrap = {read=FWordWrap, write=SetWordWrap, nodefault};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=4};
	__property int TopSpacing = {read=FTopSpacing, write=SetTopSpacing, default=8};
	__property int ScrollDelay = {read=FScrollDelay, write=SetScrollDelay, default=100};
	__property Elsndmap::TElSoundName ItemChangeSound = {read=FItemChangeSound, write=FItemChangeSound};
	__property Elsndmap::TElSoundName SectionChangeSound = {read=FSectionChangeSound, write=FSectionChangeSound};
	__property bool SectionTracking = {read=FSectionTracking, write=SetSectionTracking, default=1};
	__property bool ItemTracking = {read=FItemTracking, write=SetItemTracking, default=1};
	__property bool UnderlineTracked = {read=FUnderlineTracked, write=SetUnderlineTracked, default=1};
	__property Elsndmap::TElSoundMap* SoundMap = {read=FSoundMap, write=SetSoundMap};
	__property int ChangeDelay = {read=FChangeDelay, write=FChangeDelay, default=500};
	__property Vcl::Controls::TImageList* SectionImages = {read=FSectionImages, write=SetSectionImages};
	__property Vcl::Controls::TImageList* SectionHotImages = {read=FSectionHotImages, write=SetSectionHotImages};
	__property Vcl::Controls::TImageList* SectionDisabledImages = {read=FSectionDisabledImages, write=SetSectionDisabledImages};
	__property Vcl::Controls::TImageList* ItemDisabledImages = {read=FItemDisabledImages, write=SetItemDisabledImages};
	__property Vcl::Controls::TImageList* ItemImages = {read=FItemImages, write=SetItemImages};
	__property Vcl::Controls::TImageList* ItemHotImages = {read=FItemHotImages, write=SetItemHotImages};
	__property bool FlatSections = {read=FFlatSections, write=SetFlatSections, default=1};
	__property bool FlatItems = {read=FFlatItems, write=SetFlatItems, default=1};
	__property bool FlatActiveItem = {read=FFlatActiveItem, write=SetFlatActiveItem, nodefault};
	__property int ItemSize = {read=FItemSize, write=SetItemSize, default=36};
	__property bool RightAlignedBar = {read=FRightAlignedBar, write=SetRightAlignedBar, nodefault};
	__property int SectionHeight = {read=FSectionHeight, write=SetSectionHeight, nodefault};
	__property Vcl::Menus::TPopupMenu* ItemsPopupMenu = {read=FItemsPopup, write=SetItemsPopupMenu};
	__property Vcl::Menus::TPopupMenu* SectionsPopupMenu = {read=FSectionsPopup, write=SetSectionsPopupMenu};
	__property int TopIndex = {read=GetTopIndex, write=SetTopIndex, nodefault};
	__property System::Uitypes::TFontStyles ActiveSectionStyle = {read=FActiveSectionStyle, write=SetActiveSectionStyle, nodefault};
	__property System::Uitypes::TFontStyles ActiveItemStyle = {read=FActiveItemStyle, write=SetActiveItemStyle, nodefault};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Vcl::Graphics::TFont* ItemsFont = {read=FItemsFont, write=SetItemsFont};
	__property Vcl::Graphics::TFont* SectionsFont = {read=GetSectionsFont, write=SetSectionsFont};
	__property bool VisibleSections = {read=FVisibleSections, write=SetVisibleSections, default=1};
	__property System::Uitypes::TColor TrackSectionFontColor = {read=FTrackSectionFontColor, write=SetTrackSectionFontColor, default=-16777203};
	__property System::Uitypes::TColor TrackSectionBkColor = {read=FTrackSectionBkColor, write=SetTrackSectionBkColor, default=-16777201};
	__property System::Uitypes::TColor TrackItemFontColor = {read=FTrackItemFontColor, write=SetTrackItemFontColor, default=-16777203};
	__property System::Uitypes::TColor TrackItemBkColor = {read=FTrackItemBkColor, write=SetTrackItemBkColor, default=-16777200};
	__property bool KeepSelection = {read=FKeepSelection, write=SetKeepSelection, default=1};
	__property TElSideBarIconLocation IconLocation = {read=FIconLocation, write=SetIconLocation, default=1};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property System::Classes::TNotifyEvent OnItemChange = {read=FOnItemChange, write=FOnItemChange};
	__property System::Classes::TNotifyEvent OnSectionChange = {read=FOnSectionChange, write=FOnSectionChange};
	__property Background;
	__property BackgroundType = {default=2};
	__property GradientStartColor = {default=0};
	__property GradientEndColor = {default=0};
	__property GradientSteps = {default=16};
	__property Align;
	__property BorderStyle;
	__property Color = {default=-16777201};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property ParentColor = {default=0};
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
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnResize;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElSideBar(HWND ParentWindow) : Elpanel::TCustomElPanel(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elsidebar */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSIDEBAR)
using namespace Elsidebar;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElsidebarHPP
