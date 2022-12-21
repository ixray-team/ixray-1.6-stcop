// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MXCtrls.pas' rev: 34.00 (Windows)

#ifndef MxctrlsHPP
#define MxctrlsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Menus.hpp>
#include <System.IniFiles.hpp>
#include <mxConst.hpp>
#include <mxPlacemnt.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxctrls
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTextListBox;
class DELPHICLASS TMxCustomListBox;
class DELPHICLASS TMxCheckListBox;
class DELPHICLASS TMxCustomLabel;
class DELPHICLASS TMxLabel;
class DELPHICLASS TMxPanel;
class DELPHICLASS TMxSpeedButton;
class DELPHICLASS TButtonImage;
class DELPHICLASS TMxButtonGlyph;
//-- type declarations -------------------------------------------------------
typedef int TPositiveInt;

class PASCALIMPLEMENTATION TTextListBox : public Vcl::Stdctrls::TCustomListBox
{
	typedef Vcl::Stdctrls::TCustomListBox inherited;
	
private:
	int FMaxWidth;
	void __fastcall ResetHorizontalExtent();
	void __fastcall SetHorizontalExtent();
	int __fastcall GetItemWidth(int Index);
	
protected:
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	
__published:
	__property Align = {default=0};
	__property BorderStyle = {default=1};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property ExtendedSelect = {default=1};
	__property Font;
	__property IntegralHeight = {default=0};
	__property Anchors = {default=3};
	__property BiDiMode;
	__property Constraints;
	__property DragKind = {default=0};
	__property ParentBiDiMode = {default=1};
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property ItemHeight = {default=16};
	__property Items;
	__property MultiSelect = {default=0};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Sorted = {default=0};
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TabWidth = {default=0};
	__property Visible = {default=1};
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
	__property OnStartDrag;
	__property OnContextPopup;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnEndDock;
	__property OnStartDock;
public:
	/* TCustomListBox.Create */ inline __fastcall virtual TTextListBox(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TCustomListBox(AOwner) { }
	/* TCustomListBox.Destroy */ inline __fastcall virtual ~TTextListBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTextListBox(HWND ParentWindow) : Vcl::Stdctrls::TCustomListBox(ParentWindow) { }
	
};


typedef void __fastcall (__closure *TGetItemWidthEvent)(Vcl::Controls::TWinControl* Control, int Index, int &Width);

class PASCALIMPLEMENTATION TMxCustomListBox : public Vcl::Controls::TWinControl
{
	typedef Vcl::Controls::TWinControl inherited;
	
private:
	System::Classes::TStrings* FItems;
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	Vcl::Graphics::TCanvas* FCanvas;
	int FColumns;
	int FItemHeight;
	Vcl::Stdctrls::TListBoxStyle FStyle;
	bool FIntegralHeight;
	bool FMultiSelect;
	bool FSorted;
	bool FExtendedSelect;
	int FTabWidth;
	System::Classes::TStringList* FSaveItems;
	int FSaveTopIndex;
	int FSaveItemIndex;
	bool FAutoScroll;
	bool FGraySelection;
	int FMaxItemWidth;
	Vcl::Stdctrls::TDrawItemEvent FOnDrawItem;
	Vcl::Stdctrls::TMeasureItemEvent FOnMeasureItem;
	TGetItemWidthEvent FOnGetItemWidth;
	void __fastcall ResetHorizontalExtent();
	void __fastcall SetHorizontalExtent();
	bool __fastcall GetAutoScroll();
	virtual int __fastcall GetItemHeight();
	int __fastcall GetItemIndex();
	int __fastcall GetSelCount();
	bool __fastcall GetSelected(int Index);
	int __fastcall GetTopIndex();
	void __fastcall SetAutoScroll(bool Value);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetColumnWidth();
	void __fastcall SetColumns(int Value);
	void __fastcall SetExtendedSelect(bool Value);
	void __fastcall SetIntegralHeight(bool Value);
	void __fastcall SetItemHeight(int Value);
	void __fastcall SetItemIndex(int Value);
	void __fastcall SetMultiSelect(bool Value);
	void __fastcall SetSelected(int Index, bool Value);
	void __fastcall SetSorted(bool Value);
	void __fastcall SetStyle(Vcl::Stdctrls::TListBoxStyle Value);
	void __fastcall SetTabWidth(int Value);
	void __fastcall SetTopIndex(int Value);
	void __fastcall SetGraySelection(bool Value);
	void __fastcall SetOnDrawItem(Vcl::Stdctrls::TDrawItemEvent Value);
	void __fastcall SetOnGetItemWidth(TGetItemWidthEvent Value);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Message);
	MESSAGE void __fastcall CNDrawItem(Winapi::Messages::TWMDrawItem &Message);
	MESSAGE void __fastcall CNMeasureItem(Winapi::Messages::TWMMeasureItem &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Winapi::Messages::TMessage &Message);
	
protected:
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	virtual System::Classes::TStrings* __fastcall CreateItemList();
	virtual int __fastcall GetItemWidth(int Index);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	DYNAMIC void __fastcall DragCanceled();
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	virtual void __fastcall MeasureItem(int Index, int &Height);
	DYNAMIC int __fastcall GetItemData(int Index);
	DYNAMIC void __fastcall SetItemData(int Index, int AData);
	virtual void __fastcall SetItems(System::Classes::TStrings* Value);
	DYNAMIC void __fastcall ResetContent();
	DYNAMIC void __fastcall DeleteString(int Index);
	__property bool AutoScroll = {read=GetAutoScroll, write=SetAutoScroll, default=0};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property int Columns = {read=FColumns, write=SetColumns, default=0};
	__property bool ExtendedSelect = {read=FExtendedSelect, write=SetExtendedSelect, default=1};
	__property bool GraySelection = {read=FGraySelection, write=SetGraySelection, default=0};
	__property bool IntegralHeight = {read=FIntegralHeight, write=SetIntegralHeight, default=0};
	__property int ItemHeight = {read=GetItemHeight, write=SetItemHeight, nodefault};
	__property bool MultiSelect = {read=FMultiSelect, write=SetMultiSelect, default=0};
	__property ParentColor = {default=0};
	__property bool Sorted = {read=FSorted, write=SetSorted, default=0};
	__property Vcl::Stdctrls::TListBoxStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property int TabWidth = {read=FTabWidth, write=SetTabWidth, default=0};
	__property Vcl::Stdctrls::TDrawItemEvent OnDrawItem = {read=FOnDrawItem, write=SetOnDrawItem};
	__property Vcl::Stdctrls::TMeasureItemEvent OnMeasureItem = {read=FOnMeasureItem, write=FOnMeasureItem};
	__property TGetItemWidthEvent OnGetItemWidth = {read=FOnGetItemWidth, write=SetOnGetItemWidth};
	
public:
	__fastcall virtual TMxCustomListBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMxCustomListBox();
	void __fastcall Clear();
	void __fastcall DefaultDrawText(int X, int Y, const System::UnicodeString S);
	int __fastcall ItemAtPos(const System::Types::TPoint &Pos, bool Existing);
	System::Types::TRect __fastcall ItemRect(int Index);
	__property Vcl::Graphics::TCanvas* Canvas = {read=FCanvas};
	__property System::Classes::TStrings* Items = {read=FItems, write=SetItems};
	__property int ItemIndex = {read=GetItemIndex, write=SetItemIndex, nodefault};
	__property int SelCount = {read=GetSelCount, nodefault};
	__property bool Selected[int Index] = {read=GetSelected, write=SetSelected};
	__property int TopIndex = {read=GetTopIndex, write=SetTopIndex, nodefault};
	
__published:
	__property TabStop = {default=1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TMxCustomListBox(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TCheckKind : unsigned char { ckCheckBoxes, ckRadioButtons, ckCheckMarks };

typedef void __fastcall (__closure *TChangeStateEvent)(System::TObject* Sender, int Index);

class PASCALIMPLEMENTATION TMxCheckListBox : public TMxCustomListBox
{
	typedef TMxCustomListBox inherited;
	
private:
	bool FAllowGrayed;
	TCheckKind FCheckKind;
	System::Classes::TList* FSaveStates;
	Vcl::Graphics::TBitmap* FDrawBitmap;
	int FCheckWidth;
	int FCheckHeight;
	int FReserved;
	bool FInUpdateStates;
	Mxplacemnt::TIniLink* FIniLink;
	System::Classes::TNotifyEvent FOnClickCheck;
	TChangeStateEvent FOnStateChange;
	void __fastcall ResetItemHeight();
	virtual int __fastcall GetItemHeight();
	void __fastcall DrawCheck(const System::Types::TRect &R, Vcl::Stdctrls::TCheckBoxState AState, bool Enabled);
	void __fastcall SetCheckKind(TCheckKind Value);
	void __fastcall SetChecked(int Index, bool AChecked);
	bool __fastcall GetChecked(int Index);
	void __fastcall SetState(int Index, Vcl::Stdctrls::TCheckBoxState AState);
	Vcl::Stdctrls::TCheckBoxState __fastcall GetState(int Index);
	void __fastcall SetItemEnabled(int Index, bool Value);
	bool __fastcall GetItemEnabled(int Index);
	bool __fastcall GetAllowGrayed();
	void __fastcall ToggleClickCheck(int Index);
	void __fastcall InvalidateCheck(int Index);
	void __fastcall InvalidateItem(int Index);
	System::TObject* __fastcall CreateCheckObject(int Index);
	System::TObject* __fastcall FindCheckObject(int Index);
	System::TObject* __fastcall GetCheckObject(int Index);
	bool __fastcall IsCheckObject(int Index);
	void __fastcall ReadVersion(System::Classes::TReader* Reader);
	void __fastcall WriteVersion(System::Classes::TWriter* Writer);
	void __fastcall ReadCheckData(System::Classes::TReader* Reader);
	void __fastcall WriteCheckData(System::Classes::TWriter* Writer);
	void __fastcall InternalSaveStates(System::TObject* IniFile, const System::UnicodeString Section);
	void __fastcall InternalRestoreStates(System::TObject* IniFile, const System::UnicodeString Section);
	Mxplacemnt::TFormPlacement* __fastcall GetStorage();
	void __fastcall SetStorage(Mxplacemnt::TFormPlacement* Value);
	void __fastcall IniSave(System::TObject* Sender);
	void __fastcall IniLoad(System::TObject* Sender);
	void __fastcall UpdateCheckStates();
	int __fastcall GetCheckedIndex();
	void __fastcall SetCheckedIndex(int Value);
	HIDESBASE MESSAGE void __fastcall CNDrawItem(Winapi::Messages::TWMDrawItem &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	
protected:
	virtual System::Classes::TStrings* __fastcall CreateItemList();
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual int __fastcall GetItemWidth(int Index);
	DYNAMIC int __fastcall GetItemData(int Index);
	DYNAMIC void __fastcall SetItemData(int Index, int AData);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall ResetContent();
	DYNAMIC void __fastcall DeleteString(int Index);
	DYNAMIC void __fastcall ClickCheck();
	DYNAMIC void __fastcall ChangeItemState(int Index);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	HIDESBASE MESSAGE void __fastcall WMDestroy(Winapi::Messages::TWMNoParams &Msg);
	int __fastcall GetCheckWidth();
	virtual void __fastcall SetItems(System::Classes::TStrings* Value);
	
public:
	__fastcall virtual TMxCheckListBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMxCheckListBox();
	void __fastcall SaveStatesReg(System::Win::Registry::TRegIniFile* IniFile);
	void __fastcall RestoreStatesReg(System::Win::Registry::TRegIniFile* IniFile);
	void __fastcall SaveStates(System::Inifiles::TIniFile* IniFile);
	void __fastcall RestoreStates(System::Inifiles::TIniFile* IniFile);
	void __fastcall ApplyState(Vcl::Stdctrls::TCheckBoxState AState, bool EnabledOnly);
	__property bool Checked[int Index] = {read=GetChecked, write=SetChecked};
	__property Vcl::Stdctrls::TCheckBoxState State[int Index] = {read=GetState, write=SetState};
	__property bool EnabledItem[int Index] = {read=GetItemEnabled, write=SetItemEnabled};
	
__published:
	__property bool AllowGrayed = {read=GetAllowGrayed, write=FAllowGrayed, default=0};
	__property TCheckKind CheckKind = {read=FCheckKind, write=SetCheckKind, default=0};
	__property int CheckedIndex = {read=GetCheckedIndex, write=SetCheckedIndex, default=-1};
	__property Mxplacemnt::TFormPlacement* IniStorage = {read=GetStorage, write=SetStorage};
	__property Align = {default=0};
	__property AutoScroll = {default=1};
	__property BorderStyle = {default=1};
	__property Color = {default=-16777211};
	__property Columns = {default=0};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property ExtendedSelect = {default=1};
	__property Font;
	__property GraySelection = {default=0};
	__property Anchors = {default=3};
	__property BiDiMode;
	__property Constraints;
	__property DragKind = {default=0};
	__property ParentBiDiMode = {default=1};
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property IntegralHeight = {default=0};
	__property ItemHeight;
	__property Items = {stored=false};
	__property MultiSelect = {default=0};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Sorted = {default=0};
	__property Style = {default=0};
	__property TabOrder = {default=-1};
	__property TabWidth = {default=0};
	__property Visible = {default=1};
	__property TChangeStateEvent OnStateChange = {read=FOnStateChange, write=FOnStateChange};
	__property System::Classes::TNotifyEvent OnClickCheck = {read=FOnClickCheck, write=FOnClickCheck};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnDrawItem;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnGetItemWidth;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMeasureItem;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDrag;
	__property OnContextPopup;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnEndDock;
	__property OnStartDock;
public:
	/* TWinControl.CreateParented */ inline __fastcall TMxCheckListBox(HWND ParentWindow) : TMxCustomListBox(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TShadowPosition : unsigned char { spLeftTop, spLeftBottom, spRightBottom, spRightTop };

class PASCALIMPLEMENTATION TMxCustomLabel : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
private:
	Vcl::Controls::TWinControl* FFocusControl;
	System::Classes::TAlignment FAlignment;
	bool FAutoSize;
	Vcl::Stdctrls::TTextLayout FLayout;
	System::Uitypes::TColor FShadowColor;
	System::Byte FShadowSize;
	TShadowPosition FShadowPos;
	bool FWordWrap;
	bool FShowAccelChar;
	bool FShowFocus;
	bool FFocused;
	bool FMouseInControl;
	bool FDragging;
	int FLeftMargin;
	int FRightMargin;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	void __fastcall DoDrawText(System::Types::TRect &Rect, System::Word Flags);
	bool __fastcall GetTransparent();
	void __fastcall UpdateTracking();
	void __fastcall SetAlignment(System::Classes::TAlignment Value);
	HIDESBASE void __fastcall SetAutoSize(bool Value);
	void __fastcall SetFocusControl(Vcl::Controls::TWinControl* Value);
	void __fastcall SetLayout(Vcl::Stdctrls::TTextLayout Value);
	void __fastcall SetLeftMargin(int Value);
	void __fastcall SetRightMargin(int Value);
	void __fastcall SetShadowColor(System::Uitypes::TColor Value);
	void __fastcall SetShadowSize(System::Byte Value);
	void __fastcall SetShadowPos(TShadowPosition Value);
	void __fastcall SetShowAccelChar(bool Value);
	void __fastcall SetTransparent(bool Value);
	void __fastcall SetWordWrap(bool Value);
	void __fastcall SetShowFocus(bool Value);
	MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMFocusChanged(Vcl::Controls::TCMFocusChanged &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMDialogChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMVisibleChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	
protected:
	void __fastcall AdjustBounds();
	virtual System::Uitypes::TColor __fastcall GetDefaultFontColor();
	virtual System::UnicodeString __fastcall GetLabelCaption();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Paint();
	DYNAMIC void __fastcall MouseEnter();
	DYNAMIC void __fastcall MouseLeave();
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=1};
	__property Vcl::Controls::TWinControl* FocusControl = {read=FFocusControl, write=SetFocusControl};
	__property Vcl::Stdctrls::TTextLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property int LeftMargin = {read=FLeftMargin, write=SetLeftMargin, default=0};
	__property int RightMargin = {read=FRightMargin, write=SetRightMargin, default=0};
	__property System::Uitypes::TColor ShadowColor = {read=FShadowColor, write=SetShadowColor, default=-16777196};
	__property System::Byte ShadowSize = {read=FShadowSize, write=SetShadowSize, default=1};
	__property TShadowPosition ShadowPos = {read=FShadowPos, write=SetShadowPos, default=0};
	__property bool ShowAccelChar = {read=FShowAccelChar, write=SetShowAccelChar, default=1};
	__property bool ShowFocus = {read=FShowFocus, write=SetShowFocus, default=0};
	__property bool Transparent = {read=GetTransparent, write=SetTransparent, default=0};
	__property bool WordWrap = {read=FWordWrap, write=SetWordWrap, default=0};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	
public:
	__fastcall virtual TMxCustomLabel(System::Classes::TComponent* AOwner);
	__property Canvas;
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
public:
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TMxCustomLabel() { }
	
};


class PASCALIMPLEMENTATION TMxLabel : public TMxCustomLabel
{
	typedef TMxCustomLabel inherited;
	
__published:
	__property Align = {default=0};
	__property Alignment = {default=0};
	__property AutoSize = {default=1};
	__property Caption = {default=0};
	__property Color = {default=-16777211};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property FocusControl;
	__property Font;
	__property Anchors = {default=3};
	__property BiDiMode;
	__property Constraints;
	__property DragKind = {default=0};
	__property ParentBiDiMode = {default=1};
	__property Layout = {default=0};
	__property ParentColor = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShadowColor = {default=-16777196};
	__property ShadowSize = {default=1};
	__property ShadowPos = {default=0};
	__property ShowAccelChar = {default=1};
	__property ShowFocus = {default=0};
	__property ShowHint;
	__property Transparent = {default=0};
	__property Visible = {default=1};
	__property WordWrap = {default=0};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnStartDrag;
	__property OnContextPopup;
	__property OnEndDock;
	__property OnStartDock;
	
public:
	__fastcall virtual TMxLabel(System::Classes::TComponent* AOwner);
public:
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TMxLabel() { }
	
};


class PASCALIMPLEMENTATION TMxPanel : public Vcl::Extctrls::TCustomPanel
{
	typedef Vcl::Extctrls::TCustomPanel inherited;
	
public:
	__property Canvas;
	__property DockManager;
	
protected:
	System::Classes::TNotifyEvent FOnPaint;
	virtual void __fastcall Paint();
	
__published:
	__property Align = {default=0};
	__property Alignment = {default=2};
	__property Anchors = {default=3};
	__property AutoSize = {default=0};
	__property BevelInner = {default=0};
	__property BevelOuter = {default=2};
	__property BevelWidth = {default=1};
	__property BiDiMode;
	__property BorderWidth = {default=0};
	__property BorderStyle = {default=0};
	__property Caption = {default=0};
	__property Color = {default=-16777201};
	__property Constraints;
	__property Ctl3D;
	__property UseDockManager = {default=1};
	__property DockSite = {default=0};
	__property DragCursor = {default=-12};
	__property DragKind = {default=0};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property FullRepaint = {default=1};
	__property Font;
	__property Locked = {default=0};
	__property ParentBiDiMode = {default=1};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property Visible = {default=1};
	__property OnCanResize;
	__property OnClick;
	__property OnConstrainedResize;
	__property OnContextPopup;
	__property OnDockDrop;
	__property OnDockOver;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDock;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnGetSiteInfo;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnResize;
	__property OnStartDock;
	__property OnStartDrag;
	__property OnUnDock;
	__property System::Classes::TNotifyEvent OnPaint = {read=FOnPaint, write=FOnPaint};
public:
	/* TCustomPanel.Create */ inline __fastcall virtual TMxPanel(System::Classes::TComponent* AOwner) : Vcl::Extctrls::TCustomPanel(AOwner) { }
	
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TMxPanel() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TMxPanel(HWND ParentWindow) : Vcl::Extctrls::TCustomPanel(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TGlyphLayout : unsigned char { glGlyphLeft, glGlyphRight, glGlyphTop, glGlyphBottom };

enum DECLSPEC_DENUM TScrollDirection : unsigned char { sdVertical, sdHorizontal };

typedef void __fastcall (__closure *TPanelDrawEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect);

typedef System::Int8 TMxNumGlyphs;

enum DECLSPEC_DENUM TMxDropDownMenuPos : unsigned char { dmpBottom, dmpRight };

enum DECLSPEC_DENUM TMxButtonState : unsigned char { rbsUp, rbsDisabled, rbsDown, rbsExclusive, rbsInactive };

class PASCALIMPLEMENTATION TMxSpeedButton : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
private:
	int FGroupIndex;
	Vcl::Buttons::TButtonStyle FStyle;
	void *FGlyph;
	Vcl::Graphics::TBitmap* FDrawImage;
	bool FDown;
	bool FDragging;
	bool FFlat;
	bool FMouseInControl;
	bool FAllowAllUp;
	Vcl::Buttons::TButtonLayout FLayout;
	int FSpacing;
	int FMargin;
	System::Uitypes::TModalResult FModalResult;
	bool FTransparent;
	bool FMarkDropDown;
	Vcl::Menus::TPopupMenu* FDropDownMenu;
	TMxDropDownMenuPos FMenuPosition;
	bool FInactiveGrayed;
	bool FMenuTracking;
	Vcl::Extctrls::TTimer* FRepeatTimer;
	bool FAllowTimer;
	System::Word FInitRepeatPause;
	System::Word FRepeatPause;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall UpdateExclusive();
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	TMxNumGlyphs __fastcall GetNumGlyphs();
	void __fastcall SetNumGlyphs(TMxNumGlyphs Value);
	bool __fastcall GetWordWrap();
	void __fastcall SetWordWrap(bool Value);
	System::Classes::TAlignment __fastcall GetAlignment();
	void __fastcall SetAlignment(System::Classes::TAlignment Value);
	void __fastcall SetDown(bool Value);
	void __fastcall SetAllowAllUp(bool Value);
	void __fastcall SetGroupIndex(int Value);
	void __fastcall SetLayout(Vcl::Buttons::TButtonLayout Value);
	void __fastcall SetSpacing(int Value);
	void __fastcall SetMargin(int Value);
	void __fastcall SetDropDownMenu(Vcl::Menus::TPopupMenu* Value);
	void __fastcall SetFlat(bool Value);
	void __fastcall SetStyle(Vcl::Buttons::TButtonStyle Value);
	void __fastcall SetInactiveGrayed(bool Value);
	void __fastcall SetTransparent(bool Value);
	void __fastcall SetMarkDropDown(bool Value);
	void __fastcall TimerExpired(System::TObject* Sender);
	void __fastcall SetAllowTimer(bool Value);
	bool __fastcall CheckMenuDropDown(const System::Types::TSmallPoint Pos, bool Manual);
	HIDESBASE void __fastcall DoMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	MESSAGE void __fastcall CMButtonPressed(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMDialogChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMVisibleChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	
protected:
	TMxButtonState FState;
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	System::Types::TPoint __fastcall GetDropDownMenuPos();
	DYNAMIC HPALETTE __fastcall GetPalette();
	virtual void __fastcall Paint();
	virtual void __fastcall Loaded();
	virtual void __fastcall PaintGlyph(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &ARect, TMxButtonState AState, bool DrawMark);
	DYNAMIC void __fastcall MouseEnter();
	DYNAMIC void __fastcall MouseLeave();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property void * ButtonGlyph = {read=FGlyph};
	
public:
	__fastcall virtual TMxSpeedButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMxSpeedButton();
	void __fastcall ButtonClick();
	bool __fastcall CheckBtnMenuDropDown();
	DYNAMIC void __fastcall Click();
	void __fastcall UpdateTracking();
	
__published:
	__property Action;
	__property Anchors = {default=3};
	__property BiDiMode;
	__property Constraints;
	__property DragKind = {default=0};
	__property ParentBiDiMode = {default=1};
	__property System::Classes::TAlignment Alignment = {read=GetAlignment, write=SetAlignment, default=2};
	__property bool AllowAllUp = {read=FAllowAllUp, write=SetAllowAllUp, default=0};
	__property bool AllowTimer = {read=FAllowTimer, write=SetAllowTimer, default=0};
	__property int GroupIndex = {read=FGroupIndex, write=SetGroupIndex, default=0};
	__property bool Down = {read=FDown, write=SetDown, default=0};
	__property Vcl::Menus::TPopupMenu* DropDownMenu = {read=FDropDownMenu, write=SetDropDownMenu};
	__property TMxDropDownMenuPos MenuPosition = {read=FMenuPosition, write=FMenuPosition, default=0};
	__property Caption = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Font;
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property bool GrayedInactive = {read=FInactiveGrayed, write=SetInactiveGrayed, default=1};
	__property System::Word InitPause = {read=FInitRepeatPause, write=FInitRepeatPause, default=500};
	__property Vcl::Buttons::TButtonLayout Layout = {read=FLayout, write=SetLayout, default=2};
	__property int Margin = {read=FMargin, write=SetMargin, default=-1};
	__property bool MarkDropDown = {read=FMarkDropDown, write=SetMarkDropDown, default=1};
	__property System::Uitypes::TModalResult ModalResult = {read=FModalResult, write=FModalResult, default=0};
	__property TMxNumGlyphs NumGlyphs = {read=GetNumGlyphs, write=SetNumGlyphs, default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=0};
	__property System::Word RepeatInterval = {read=FRepeatPause, write=FRepeatPause, default=100};
	__property ShowHint = {default=1};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=1};
	__property Vcl::Buttons::TButtonStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property bool WordWrap = {read=GetWordWrap, write=SetWordWrap, default=0};
	__property Visible = {default=1};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDrag;
	__property OnEndDock;
	__property OnStartDock;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TButtonImage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::TObject* FGlyph;
	System::Types::TPoint FButtonSize;
	Vcl::Controls::TCaption FCaption;
	TMxNumGlyphs __fastcall GetNumGlyphs();
	void __fastcall SetNumGlyphs(TMxNumGlyphs Value);
	bool __fastcall GetWordWrap();
	void __fastcall SetWordWrap(bool Value);
	System::Classes::TAlignment __fastcall GetAlignment();
	void __fastcall SetAlignment(System::Classes::TAlignment Value);
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	
public:
	__fastcall TButtonImage();
	__fastcall virtual ~TButtonImage();
	void __fastcall Invalidate();
	void __fastcall DrawEx(Vcl::Graphics::TCanvas* Canvas, int X, int Y, int Margin, int Spacing, Vcl::Buttons::TButtonLayout Layout, Vcl::Graphics::TFont* AFont, Vcl::Controls::TImageList* Images, int ImageIndex, System::Word Flags);
	void __fastcall Draw(Vcl::Graphics::TCanvas* Canvas, int X, int Y, int Margin, int Spacing, Vcl::Buttons::TButtonLayout Layout, Vcl::Graphics::TFont* AFont, System::Word Flags);
	__property System::Classes::TAlignment Alignment = {read=GetAlignment, write=SetAlignment, nodefault};
	__property Vcl::Controls::TCaption Caption = {read=FCaption, write=FCaption};
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property TMxNumGlyphs NumGlyphs = {read=GetNumGlyphs, write=SetNumGlyphs, nodefault};
	__property System::Types::TPoint ButtonSize = {read=FButtonSize, write=FButtonSize};
	__property bool WordWrap = {read=GetWordWrap, write=SetWordWrap, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TMxButtonGlyph : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Vcl::Graphics::TBitmap* FOriginal;
	Vcl::Controls::TImageList* FGlyphList;
	System::StaticArray<int, 5> FIndexs;
	System::Uitypes::TColor FTransparentColor;
	TMxNumGlyphs FNumGlyphs;
	bool FWordWrap;
	System::Classes::TAlignment FAlignment;
	System::Classes::TNotifyEvent FOnChange;
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	void __fastcall SetNumGlyphs(TMxNumGlyphs Value);
	System::Uitypes::TColor __fastcall MapColor(System::Uitypes::TColor Color);
	
protected:
	void __fastcall MinimizeCaption(Vcl::Graphics::TCanvas* Canvas, const System::UnicodeString Caption, System::WideChar * Buffer, int MaxLen, int Width);
	int __fastcall CreateButtonGlyph(TMxButtonState State);
	int __fastcall CreateImageGlyph(TMxButtonState State, Vcl::Controls::TImageList* Images, int Index);
	void __fastcall CalcButtonLayout(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Client, System::UnicodeString &Caption, Vcl::Buttons::TButtonLayout Layout, int Margin, int Spacing, bool PopupMark, System::Types::TPoint &GlyphPos, System::Types::TRect &TextBounds, System::Word Flags, Vcl::Controls::TImageList* Images, int ImageIndex);
	
public:
	__fastcall TMxButtonGlyph();
	__fastcall virtual ~TMxButtonGlyph();
	void __fastcall Invalidate();
	System::Types::TPoint __fastcall DrawButtonGlyph(Vcl::Graphics::TCanvas* Canvas, int X, int Y, TMxButtonState State);
	System::Types::TPoint __fastcall DrawButtonImage(Vcl::Graphics::TCanvas* Canvas, int X, int Y, Vcl::Controls::TImageList* Images, int ImageIndex, TMxButtonState State);
	System::Types::TRect __fastcall DrawEx(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Client, const System::UnicodeString Caption, Vcl::Buttons::TButtonLayout Layout, int Margin, int Spacing, bool PopupMark, Vcl::Controls::TImageList* Images, int ImageIndex, TMxButtonState State, System::Word Flags);
	void __fastcall DrawButtonText(Vcl::Graphics::TCanvas* Canvas, const System::UnicodeString Caption, const System::Types::TRect &TextBounds, TMxButtonState State, System::Word Flags);
	void __fastcall DrawPopupMark(Vcl::Graphics::TCanvas* Canvas, int X, int Y, TMxButtonState State);
	System::Types::TRect __fastcall Draw(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Client, const System::UnicodeString Caption, Vcl::Buttons::TButtonLayout Layout, int Margin, int Spacing, bool PopupMark, TMxButtonState State, System::Word Flags);
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=FAlignment, nodefault};
	__property Vcl::Graphics::TBitmap* Glyph = {read=FOriginal, write=SetGlyph};
	__property TMxNumGlyphs NumGlyphs = {read=FNumGlyphs, write=SetNumGlyphs, nodefault};
	__property bool WordWrap = {read=FWordWrap, write=FWordWrap, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
};


//-- var, const, procedure ---------------------------------------------------
static const Vcl::Stdctrls::TCheckBoxState clbDefaultState = (Vcl::Stdctrls::TCheckBoxState)(0);
static const bool clbDefaultEnabled = true;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CheckBitmap(void);
extern DELPHI_PACKAGE int __fastcall DrawShadowText(HDC DC, System::WideChar * Str, int Count, System::Types::TRect &Rect, System::Word Format, System::Byte ShadowSize, unsigned ShadowColor, TShadowPosition ShadowPos);
}	/* namespace Mxctrls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXCTRLS)
using namespace Mxctrls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxctrlsHPP
