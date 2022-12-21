// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHeader.pas' rev: 34.00 (Windows)

#ifndef ElheaderHPP
#define ElheaderHPP

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
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <ElIni.hpp>
#include <ElImgFrm.hpp>
#include <ElACtrls.hpp>
#include <ElStrUtils.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElTools.hpp>
#include <ElList.hpp>
#include <ElXPThemedControl.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <HTMLRender.hpp>
#include <ElVCLUtils.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elheader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EElHeaderError;
class DELPHICLASS TElHeaderSection;
class DELPHICLASS TElHeaderSections;
class DELPHICLASS TCustomElHeader;
class DELPHICLASS TElHeader;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElSectionStyle : unsigned char { ElhsText, ElhsOwnerDraw, ElhsPictureOnly };

enum DECLSPEC_DENUM TElSSortMode : unsigned char { hsmNone, hsmAscend, hsmDescend };

enum DECLSPEC_DENUM TElSAlignment : unsigned char { hsaLeft, hsaCenter, hsaRight };

enum DECLSPEC_DENUM TElHResizingStates : unsigned char { trsBegin, trsMove, trsEnd };

enum DECLSPEC_DENUM TSectionChangeMode : unsigned char { scmCaption, scmFieldName, scmFieldType, scmAlign, scmStyle, scmEditable, scmPassword };

enum DECLSPEC_DENUM TElSectionPart : unsigned char { espResizeArea, espText, espExpandSign, espLookupSign, espFilterSign };

enum DECLSPEC_DENUM TAdjustCondition : unsigned char { acAll, acAutoSizedOnly };

enum DECLSPEC_DENUM TElFieldType : unsigned char { sftCustom, sftText, sftNumber, sftFloating, sftDateTime, sftDate, sftTime, sftPicture, sftEnum, sftBLOB, sftUndef, sftBool, sftCurrency, sftMemo };

typedef System::Set<TElFieldType, TElFieldType::sftCustom, TElFieldType::sftMemo> TElFieldTypes;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EElHeaderError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElHeaderError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElHeaderError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElHeaderError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElHeaderError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElHeaderError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElHeaderError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElHeaderError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElHeaderError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElHeaderError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElHeaderError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElHeaderError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElHeaderError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElHeaderError() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TElHeaderSectionEvent)(TCustomElHeader* Sender, TElHeaderSection* Section);

typedef void __fastcall (__closure *TElHeaderLookupEvent)(System::TObject* Sender, TElHeaderSection* Section, System::UnicodeString &Text);

typedef void __fastcall (__closure *TElHeaderLookupDoneEvent)(System::TObject* Sender, TElHeaderSection* Section, System::UnicodeString Text, bool Accepted);

class PASCALIMPLEMENTATION TElHeaderSection : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	bool FFilterIsActive;
	bool FFilterEnabled;
	Elstrutils::TElFString FHint;
	Vcl::Stdctrls::TTextLayout FTextLayout;
	bool FUseMainStyle;
	System::Uitypes::TColor FFontColor;
	System::Uitypes::TColor FColor;
	bool FParentColor;
	bool FLookupEnabled;
	bool FExpandable;
	bool FExpanded;
	TElHeaderSection* FParentSection;
	int FParentIdx;
	Vcl::Menus::TPopupMenu* FPopupMenu;
	System::UnicodeString FPopupName;
	bool FResizable;
	bool FClickSelect;
	bool FProtected;
	System::UnicodeString FFieldName;
	TElFieldType FFieldType;
	bool FEditable;
	int FImageIndex;
	TElSAlignment FPicAlign;
	int FIntTag;
	int FIndex;
	int FTag;
	bool FVisible;
	TElSectionStyle FStyle;
	int FWidth;
	int FMinWidth;
	int FMaxWidth;
	TElSSortMode FSortMode;
	bool FAllowClick;
	TElSAlignment FAlignment;
	System::UnicodeString FText;
	void *FData;
	TCustomElHeader* FOwner;
	System::Classes::TStringList* FLookupHist;
	bool FAutoSize;
	float FStickySize;
	int ASaveSize;
	System::Classes::TNotifyEvent FOnResize;
	bool FShowSortMark;
	void __fastcall SetWidth(int value);
	int __fastcall GetWidth();
	int __fastcall GetLeft();
	int __fastcall GetRight();
	void __fastcall SetMaxWidth(int value);
	void __fastcall SetMinWidth(int value);
	void __fastcall SetText(System::UnicodeString value);
	void __fastcall SetStyle(TElSectionStyle value);
	void __fastcall SetSortMode(TElSSortMode value);
	void __fastcall SetAlignment(TElSAlignment value);
	void __fastcall SetVisible(bool value);
	int __fastcall GetIndex();
	int __fastcall GetPosition();
	void __fastcall SetImageIndex(int newValue);
	void __fastcall SetProtected(bool newValue);
	void __fastcall SetExpandable(bool newValue);
	void __fastcall SetExpanded(bool newValue);
	void __fastcall SetParentSection(TElHeaderSection* newValue);
	void __fastcall SetPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	bool __fastcall GetVisible();
	void __fastcall SetLookupEnabled(bool newValue);
	void __fastcall SetParentColor(bool newValue);
	void __fastcall SetColor(System::Uitypes::TColor newValue);
	void __fastcall SetFontColor(System::Uitypes::TColor newValue);
	void __fastcall SetUseMainStyle(bool newValue);
	void __fastcall SetTextLayout(Vcl::Stdctrls::TTextLayout newValue);
	void __fastcall SetFilterEnabled(bool newValue);
	void __fastcall SetFilterIsActive(bool newValue);
	void __fastcall SetLookupList(System::Classes::TStringList* newValue);
	void __fastcall SetAutoSize(bool newValue);
	bool __fastcall GetLocked();
	void __fastcall SetShowSortMark(bool Value);
	virtual void __fastcall SetFieldName(System::UnicodeString newValue);
	virtual void __fastcall SetFieldType(TElFieldType newValue);
	virtual void __fastcall SetEditable(bool newValue);
	virtual void __fastcall SetResizable(bool newValue);
	void __fastcall SetSaveSize(int newValue);
	__property int FSaveSize = {read=ASaveSize, write=SetSaveSize, default=-1};
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	__property System::Classes::TNotifyEvent OnResize = {read=FOnResize, write=FOnResize};
	
public:
	__fastcall TElHeaderSection(TCustomElHeader* AOwner);
	__fastcall virtual ~TElHeaderSection();
	void __fastcall UpdateSection();
	virtual void __fastcall Assign(System::Classes::TPersistent* source);
	__property int Index = {read=GetIndex, nodefault};
	__property int Left = {read=GetLeft, nodefault};
	__property int Right = {read=GetRight, nodefault};
	__property int Position = {read=GetPosition, nodefault};
	__property void * Data = {read=FData, write=FData};
	__property bool Locked = {read=GetLocked, nodefault};
	__property TCustomElHeader* Owner = {read=FOwner};
	
__published:
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property TElSectionStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property int Width = {read=GetWidth, write=SetWidth, nodefault};
	__property int MaxWidth = {read=FMaxWidth, write=SetMaxWidth, default=10000};
	__property int MinWidth = {read=FMinWidth, write=SetMinWidth, default=0};
	__property TElSSortMode SortMode = {read=FSortMode, write=SetSortMode, nodefault};
	__property bool AllowClick = {read=FAllowClick, write=FAllowClick, default=1};
	__property TElSAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property TElSAlignment PictureAlign = {read=FPicAlign, write=FPicAlign, nodefault};
	__property bool Visible = {read=GetVisible, write=SetVisible, nodefault};
	__property int ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property System::UnicodeString FieldName = {read=FFieldName, write=SetFieldName};
	__property TElFieldType FieldType = {read=FFieldType, write=SetFieldType, default=1};
	__property bool Editable = {read=FEditable, write=SetEditable, nodefault};
	__property bool Password = {read=FProtected, write=SetProtected, default=0};
	__property bool Resizable = {read=FResizable, write=SetResizable, default=1};
	__property bool ClickSelect = {read=FClickSelect, write=FClickSelect, default=1};
	__property bool Expandable = {read=FExpandable, write=SetExpandable, nodefault};
	__property bool Expanded = {read=FExpanded, write=SetExpanded, nodefault};
	__property TElHeaderSection* ParentSection = {read=FParentSection, write=SetParentSection};
	__property Vcl::Menus::TPopupMenu* PopupMenu = {read=FPopupMenu, write=SetPopupMenu};
	__property bool LookupEnabled = {read=FLookupEnabled, write=SetLookupEnabled, nodefault};
	__property System::Classes::TStringList* LookupHistory = {read=FLookupHist, write=SetLookupList};
	__property bool ParentColor = {read=FParentColor, write=SetParentColor, default=1};
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, nodefault};
	__property System::Uitypes::TColor FontColor = {read=FFontColor, write=SetFontColor, nodefault};
	__property bool UseMainStyle = {read=FUseMainStyle, write=SetUseMainStyle, nodefault};
	__property Vcl::Stdctrls::TTextLayout TextLayout = {read=FTextLayout, write=SetTextLayout, default=1};
	__property bool FilterEnabled = {read=FFilterEnabled, write=SetFilterEnabled, nodefault};
	__property bool FilterIsActive = {read=FFilterIsActive, write=SetFilterIsActive, nodefault};
	__property Elstrutils::TElFString Hint = {read=FHint, write=FHint};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, nodefault};
	__property bool ShowSortMark = {read=FShowSortMark, write=SetShowSortMark, default=1};
	__property int Tag = {read=FTag, write=FTag, nodefault};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElHeaderSections : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	TElHeaderSection* operator[](int Index) { return this->Item[Index]; }
	
private:
	System::Classes::TList* FList;
	int __fastcall GetCount();
	System::UnicodeString __fastcall GetSectionsOrder();
	void __fastcall SetSectionsOrder(System::UnicodeString newValue);
	
protected:
	TCustomElHeader* FOwner;
	virtual TElHeaderSection* __fastcall GetSectionByIntTag(int IntTag);
	virtual TElHeaderSection* __fastcall GetSection(int index);
	virtual void __fastcall SetSection(int index, TElHeaderSection* Section);
	virtual TElHeaderSection* __fastcall GetSectionByPos(int index);
	virtual TElHeaderSection* __fastcall CreateSection();
	virtual void __fastcall WriteData(System::Classes::TStream* Stream);
	virtual void __fastcall IntReadData(System::Classes::TStream* Stream, bool ClearCurrent);
	virtual void __fastcall ReadData(System::Classes::TStream* Stream);
	virtual void __fastcall FrameReadData(System::Classes::TStream* Stream);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	TElHeaderSection* __fastcall LastVisibleSection();
	TElHeaderSection* __fastcall GetPrevVisibleSection(TElHeaderSection* Section);
	virtual TElHeaderSection* __fastcall FindSection(int tag);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall TElHeaderSections(TCustomElHeader* AOwner);
	__fastcall virtual ~TElHeaderSections();
	void __fastcall Clear();
	virtual void __fastcall Assign(System::Classes::TPersistent* source);
	TElHeaderSection* __fastcall AddSection();
	TElHeaderSection* __fastcall InsertSection(int index);
	void __fastcall DeleteSection(TElHeaderSection* Section);
	void __fastcall MoveSection(TElHeaderSection* Section, int NewPos);
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	void __fastcall SaveToFile(System::UnicodeString FileName);
	void __fastcall LoadFromFile(System::UnicodeString FileName);
	void __fastcall Reindex();
	__property TCustomElHeader* Owner = {read=FOwner};
	__property int Count = {read=GetCount, nodefault};
	__property TElHeaderSection* ItemByPos[int Index] = {read=GetSectionByPos};
	__property TElHeaderSection* Item[int Index] = {read=GetSection, write=SetSection/*, default*/};
	
__published:
	__property System::UnicodeString SectionsOrder = {read=GetSectionsOrder, write=SetSectionsOrder, stored=false};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TMeasureSectionEvent)(System::TObject* Sender, TElHeaderSection* Section, System::Types::TPoint &Size);

typedef void __fastcall (__closure *TElSectionRedrawEvent)(TCustomElHeader* Sender, Vcl::Graphics::TCanvas* Canvas, TElHeaderSection* Section, const System::Types::TRect &R, bool Pressed);

typedef void __fastcall (__closure *TElSectionResizingEvent)(TCustomElHeader* Sender, TElHeaderSection* Section, TElHResizingStates State, int Width);

typedef void __fastcall (__closure *TElSectionMoveEvent)(TCustomElHeader* Sender, TElHeaderSection* Section, int OldPos, int NewPos);

typedef void __fastcall (__closure *TPictureNeededEvent)(TCustomElHeader* Sender, TElHeaderSection* Section, int &ImageIndex);

typedef void __fastcall (__closure *TSectionChangeEvent)(TCustomElHeader* Sender, TElHeaderSection* Section, TSectionChangeMode Change);

class PASCALIMPLEMENTATION TCustomElHeader : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
private:
	bool FMouseInControl;
	
protected:
	bool FWrapCaptions;
	TElHeaderSection* FLockedSection;
	int FHPos;
	bool FInvertSortArrows;
	bool FFlat;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	Elimgfrm::TElImageForm* FImgForm;
	System::Uitypes::TColor FActiveFilterColor;
	TElHeaderSectionEvent FOnFilterCall;
	TElHeaderSectionEvent FOnSectionAutoSize;
	System::Uitypes::TColor FFilterColor;
	bool FLockHeight;
	bool FRightAlignedText;
	bool FRightAlignedOrder;
	bool FStickySections;
	bool FMoveOnDrag;
	Elini::TElIniFile* FStorage;
	System::UnicodeString FStoragePath;
	Vcl::Controls::TImageList* FImages;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	TElHeaderSections* FSections;
	bool FTracking;
	bool FAllowDrag;
	bool FPainting;
	Vcl::Graphics::TBitmap* DragBmp;
	Vcl::Graphics::TBitmap* SaveBmp;
	Vcl::Graphics::TBitmap* DragBmpMask;
	System::Types::TRect DragRect;
	System::Types::TPoint FDragCoord;
	Vcl::Stdctrls::TComboBox* FLookup;
	System::Types::TPoint FPressCoord;
	bool FPressed;
	TElHeaderSection* FPressedItem;
	TElHeaderSection* FHintSection;
	TElHeaderSection* FLookupSection;
	TElHeaderSection* FTrackSection;
	bool FResizing;
	TElHeaderSection* FResizeSection;
	TElHeaderSection* FDropSrc;
	TElHeaderSection* FDropTrg;
	int FHeaderLine;
	int FLineTab;
	bool FResizeOnDrag;
	bool FHeaderLineVis;
	bool FIgnoreLookupChange;
	bool FDoingLookup;
	HDC FLineDC;
	NativeUInt FFocusedCtl;
	int LoadingCount;
	bool DeletionHappened;
	bool AdditionHappened;
	bool FInStick;
	int FOldWidth;
	int FUpdateCount;
	Htmlrender::TElHTMLRender* FRender;
	TElHeaderSectionEvent FOnSectionClick;
	TElHeaderSectionEvent FOnSectionResize;
	TElSectionRedrawEvent FOnSectionDraw;
	System::Classes::TNotifyEvent FOnResize;
	TElSectionResizingEvent FOnSectionResizing;
	TElHeaderSectionEvent FOnSectionDelete;
	TElSectionMoveEvent FOnSectionMove;
	TElHeaderSectionEvent FOnVisibleChange;
	TPictureNeededEvent FOnPictureNeeded;
	TSectionChangeEvent FOnSectionChange;
	TElHeaderSectionEvent FOnSectionCreate;
	TElHeaderLookupEvent FOnHeaderLookup;
	TElHeaderLookupDoneEvent FOnHeaderLookupDone;
	TElHeaderSectionEvent FOnSectionExpand;
	TElHeaderSectionEvent FOnSectionCollapse;
	TMeasureSectionEvent FOnMeasureSection;
	int FDefaultWidth;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	System::WideString FHint;
	bool FMultiSort;
	void __fastcall DrawLine(bool Restore);
	void __fastcall AllocateLineDC();
	void __fastcall ReleaseLineDC();
	int __fastcall GetColumnsWidth();
	void __fastcall InvalidateRight(int value);
	void __fastcall SetTracking(bool newValue);
	void __fastcall IntMouseEnter();
	void __fastcall IntMouseLeave();
	void __fastcall IntSize();
	void __fastcall IntExit();
	void __fastcall IntLButtonDown(short XPos, short YPos);
	void __fastcall IntLButtonUp(short XPos, short YPos);
	bool __fastcall IntRButtonDown(short XPos, short YPos);
	bool __fastcall IntRButtonUp(short XPos, short YPos);
	void __fastcall IntMouseMove(short XPos, short YPos);
	void __fastcall IntLButtonDblClick(short XPos, short YPos);
	bool __fastcall IntHintShow(Vcl::Controls::THintInfo &HintInfo);
	HIDESBASE MESSAGE void __fastcall CMDrag(Vcl::Controls::TCMDrag &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Msg);
	void __fastcall SetSections(TElHeaderSections* value);
	void __fastcall SetImages(Vcl::Controls::TImageList* newValue);
	void __fastcall OnImageListChange(System::TObject* Sender);
	void __fastcall GetDragImage(int XPos);
	void __fastcall SetStorage(Elini::TElIniFile* newValue);
	void __fastcall EditExit(System::TObject* Sender);
	void __fastcall EditKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall EditKeyUp(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall EditChange(System::TObject* Sender);
	void __fastcall SetStickySections(bool newValue);
	void __fastcall AdjustHeaderHeight();
	void __fastcall AdjustStickySize(TElHeaderSection* Caller);
	bool __fastcall IsLoading();
	void __fastcall SetRightAlignedText(bool newValue);
	void __fastcall SetRightAlignedOrder(bool newValue);
	void __fastcall SetLockHeight(bool newValue);
	void __fastcall SetFilterColor(System::Uitypes::TColor newValue);
	void __fastcall SetActiveFilterColor(System::Uitypes::TColor newValue);
	void __fastcall SetFlat(bool newValue);
	bool __fastcall GetIsDesigning();
	void __fastcall SetIsDesigning(bool newValue);
	void __fastcall SetInvertSortArrows(bool newValue);
	void __fastcall SetLeftPos(int newValue);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall SetLockedSection(TElHeaderSection* newValue);
	virtual void __fastcall SetWrapCaptions(bool newValue);
	void __fastcall RedrawSection(Vcl::Graphics::TCanvas* Canvas, TElHeaderSection* Section, const System::Types::TRect &R, bool Dithered);
	void __fastcall RedrawSections();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual TElHeaderSections* __fastcall CreateSections();
	virtual bool __fastcall InResizeArea(int X, TElHeaderSection* &HitSection);
	virtual void __fastcall Paint();
	HIDESBASEDYNAMIC void __fastcall Resize();
	virtual int __fastcall DoGetPicture(TElHeaderSection* Section);
	virtual void __fastcall DoVisChanged(TElHeaderSection* Section);
	virtual void __fastcall DoSectionDelete(TElHeaderSection* Section);
	virtual void __fastcall DoSectionMove(TElHeaderSection* Section, int OldPos, int NewPos);
	virtual void __fastcall DoSectionResizing(TElHeaderSection* Section, TElHResizingStates State, int NewWidth);
	virtual void __fastcall DoSectionResize(TElHeaderSection* Section);
	virtual void __fastcall DoSectionClick(TElHeaderSection* Section);
	virtual void __fastcall DoSectionDraw(Vcl::Graphics::TCanvas* Canvas, TElHeaderSection* Section, const System::Types::TRect &R, bool Pressed);
	virtual void __fastcall DoNotifySectionChange(TElHeaderSection* Section, TSectionChangeMode Change);
	virtual void __fastcall DoSectionExpandEvent(TElHeaderSection* Section);
	virtual void __fastcall DoSectionCollapseEvent(TElHeaderSection* Section);
	virtual void __fastcall DoSectionCreate(TElHeaderSection* Section);
	virtual void __fastcall DoSectionLookupEvent(TElHeaderSection* Section, System::UnicodeString &Text);
	virtual void __fastcall DoSectionLookupDoneEvent(TElHeaderSection* Section, System::UnicodeString Text, bool Accepted);
	virtual void __fastcall TriggerSectionAutoSizeEvent(TElHeaderSection* Section);
	virtual void __fastcall TriggerFilterCallEvent(TElHeaderSection* Section);
	virtual void __fastcall TriggerMeasureSectionEvent(TElHeaderSection* Section, System::Types::TPoint &Size);
	void __fastcall OnFontChange(System::TObject* Sender);
	int __fastcall GetResizableWidth();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	virtual void __fastcall CreateHandle();
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	virtual System::WideString __fastcall GetThemedClassName();
	void __fastcall SetHint(System::WideString Value);
	__property int SectionsWidth = {read=GetColumnsWidth, nodefault};
	__property TElHeaderSections* Sections = {read=FSections, write=SetSections};
	__property bool ResizeOnDrag = {read=FResizeOnDrag, write=FResizeOnDrag, default=1};
	__property bool Tracking = {read=FTracking, write=SetTracking, default=1};
	__property bool AllowDrag = {read=FAllowDrag, write=FAllowDrag, default=1};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property bool MoveOnDrag = {read=FMoveOnDrag, write=FMoveOnDrag, nodefault};
	__property System::UnicodeString StoragePath = {read=FStoragePath, write=FStoragePath};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=SetStorage};
	__property bool StickySections = {read=FStickySections, write=SetStickySections, nodefault};
	__property bool RightAlignedText = {read=FRightAlignedText, write=SetRightAlignedText, nodefault};
	__property bool RightAlignedOrder = {read=FRightAlignedOrder, write=SetRightAlignedOrder, nodefault};
	__property bool LockHeight = {read=FLockHeight, write=SetLockHeight, nodefault};
	__property System::Uitypes::TColor FilterColor = {read=FFilterColor, write=SetFilterColor, default=-16777198};
	__property System::Uitypes::TColor ActiveFilterColor = {read=FActiveFilterColor, write=SetActiveFilterColor, nodefault};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property TSectionChangeEvent OnSectionChange = {read=FOnSectionChange, write=FOnSectionChange};
	__property System::Classes::TNotifyEvent OnResize = {read=FOnResize, write=FOnResize};
	__property TElHeaderSectionEvent OnSectionShowHide = {read=FOnVisibleChange, write=FOnVisibleChange};
	__property TElHeaderSectionEvent OnSectionClick = {read=FOnSectionClick, write=FOnSectionClick};
	__property TElHeaderSectionEvent OnSectionResize = {read=FOnSectionResize, write=FOnSectionResize};
	__property TElSectionRedrawEvent OnSectionDraw = {read=FOnSectionDraw, write=FOnSectionDraw};
	__property TElHeaderSectionEvent OnSectionDelete = {read=FOnSectionDelete, write=FOnSectionDelete};
	__property TElSectionResizingEvent OnSectionResizing = {read=FOnSectionResizing, write=FOnSectionResizing};
	__property TElSectionMoveEvent OnSectionMove = {read=FOnSectionMove, write=FOnSectionMove};
	__property TPictureNeededEvent OnPictureNeeded = {read=FOnPictureNeeded, write=FOnPictureNeeded};
	__property TElHeaderSectionEvent OnSectionCreate = {read=FOnSectionCreate, write=FOnSectionCreate};
	__property TElHeaderSectionEvent OnSectionExpand = {read=FOnSectionExpand, write=FOnSectionExpand};
	__property TElHeaderSectionEvent OnSectionCollapse = {read=FOnSectionCollapse, write=FOnSectionCollapse};
	__property TElHeaderLookupEvent OnHeaderLookup = {read=FOnHeaderLookup, write=FOnHeaderLookup};
	__property TElHeaderLookupDoneEvent OnHeaderLookupDone = {read=FOnHeaderLookupDone, write=FOnHeaderLookupDone};
	__property TMeasureSectionEvent OnMeasureSection = {read=FOnMeasureSection, write=FOnMeasureSection};
	__property TElHeaderSectionEvent OnSectionAutoSize = {read=FOnSectionAutoSize, write=FOnSectionAutoSize};
	__property TElHeaderSectionEvent OnFilterCall = {read=FOnFilterCall, write=FOnFilterCall};
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property bool IsDesigning = {read=GetIsDesigning, write=SetIsDesigning, nodefault};
	__property bool InvertSortArrows = {read=FInvertSortArrows, write=SetInvertSortArrows, default=0};
	__property bool WrapCaptions = {read=FWrapCaptions, write=SetWrapCaptions, nodefault};
	__property int DefaultWidth = {read=FDefaultWidth, write=FDefaultWidth, default=120};
	__property Htmlrender::TElHTMLImageNeededEvent OnHTMLImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	
public:
	__fastcall virtual TCustomElHeader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElHeader();
	TElHeaderSection* __fastcall GetSectionAt(int X, int Y);
	TElHeaderSection* __fastcall GetSectionAtEx(int X, int Y, TElSectionPart &SectionPart);
	System::Types::TRect __fastcall GetSectionRect(int SectionNum);
	int __fastcall MeasureSectionWidth(TElHeaderSection* Section, System::PInteger TextWidth, System::PInteger SectionHeight);
	int __fastcall CalcHeaderHeight();
	__property Canvas;
	virtual void __fastcall Loaded();
	bool __fastcall Setup();
	void __fastcall Save();
	void __fastcall Restore();
	virtual void __fastcall Update();
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	void __fastcall MarkStickySections();
	__property int LeftPos = {read=FHPos, write=SetLeftPos, nodefault};
	__property TElHeaderSection* LockedSection = {read=FLockedSection, write=SetLockedSection};
	__property bool MultiSort = {read=FMultiSort, write=FMultiSort, nodefault};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElHeader(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElHeader : public TCustomElHeader
{
	typedef TCustomElHeader inherited;
	
__published:
	__property ActiveFilterColor;
	__property AllowDrag = {default=1};
	__property Align = {default=0};
	__property Color = {default=-16777211};
	__property DefaultWidth = {default=120};
	__property Enabled = {default=1};
	__property Flat;
	__property Anchors = {default=3};
	__property Action;
	__property Constraints;
	__property DockOrientation;
	__property Floating;
	__property BevelKind = {default=0};
	__property DoubleBuffered;
	__property DragKind = {default=0};
	__property MoveOnDrag;
	__property Font;
	__property FilterColor = {default=-16777198};
	__property Images;
	__property ImageForm;
	__property InvertSortArrows = {default=0};
	__property LockHeight;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ResizeOnDrag = {default=1};
	__property RightAlignedText;
	__property RightAlignedOrder;
	__property SectionsWidth;
	__property Sections;
	__property ShowHint;
	__property StickySections;
	__property Tracking = {default=1};
	__property Storage;
	__property StoragePath = {default=0};
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property WrapCaptions;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnResize;
	__property OnSectionClick;
	__property OnSectionResize;
	__property OnSectionDraw;
	__property OnSectionResizing;
	__property OnSectionDelete;
	__property OnSectionMove;
	__property OnSectionShowHide;
	__property OnPictureNeeded;
	__property OnSectionChange;
	__property OnSectionCreate;
	__property OnSectionExpand;
	__property OnSectionCollapse;
	__property OnHeaderLookup;
	__property OnHeaderLookupDone;
	__property OnHTMLImageNeeded;
	__property OnSectionAutoSize;
	__property OnFilterCall;
public:
	/* TCustomElHeader.Create */ inline __fastcall virtual TElHeader(System::Classes::TComponent* AOwner) : TCustomElHeader(AOwner) { }
	/* TCustomElHeader.Destroy */ inline __fastcall virtual ~TElHeader() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHeader(HWND ParentWindow) : TCustomElHeader(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 ResizeWidth = System::Int8(0x5);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* ElHeaderAscBmp;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* ElHeaderDescBmp;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* ElHeaderLeftBmp;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* ElHeaderRightBmp;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* ElHeaderPointBmp;
}	/* namespace Elheader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHEADER)
using namespace Elheader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElheaderHPP
