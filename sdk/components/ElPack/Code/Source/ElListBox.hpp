// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElListBox.pas' rev: 34.00 (Windows)

#ifndef EllistboxHPP
#define EllistboxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Consts.hpp>
#include <Vcl.Forms.hpp>
#include <ElUxTheme.hpp>
#include <Vcl.ImgList.hpp>
#include <System.RTLConsts.hpp>
#include <ElImgFrm.hpp>
#include <Vcl.Menus.hpp>
#include <ElTools.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElTmSchema.hpp>
#include <ElHintWnd.hpp>
#include <ElXPThemedControl.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElStrUtils.hpp>
#include <ElVCLUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ellistbox
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElListBoxStrings;
class DELPHICLASS TCustomElListBox;
class DELPHICLASS TElListBox;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TDrawTextEvent)(Vcl::Graphics::TCanvas* ACanvas, int Index, System::Types::TRect &Rect, int Flags);

typedef System::StaticArray<int, 536870911> TIntArray;

typedef TIntArray *PIntArray;

class PASCALIMPLEMENTATION TElListBoxStrings : public Elunicodestrings::TElWideStringList
{
	typedef Elunicodestrings::TElWideStringList inherited;
	
private:
	TCustomElListBox* ListBox;
	
protected:
	virtual System::WideString __fastcall Get(int Index);
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall Put(int Index, const System::WideString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetUpdateState(bool Updating);
	void __fastcall ResetBox();
	
public:
	virtual int __fastcall Add(const System::WideString S);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual int __fastcall IndexOf(const System::WideString S);
	virtual void __fastcall Insert(int Index, const System::WideString S);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
public:
	/* TElWideStringList.Destroy */ inline __fastcall virtual ~TElListBoxStrings() { }
	
public:
	/* TObject.Create */ inline __fastcall TElListBoxStrings() : Elunicodestrings::TElWideStringList() { }
	
};


class PASCALIMPLEMENTATION TCustomElListBox : public Vcl::Controls::TWinControl
{
	typedef Vcl::Controls::TWinControl inherited;
	
protected:
	TIntArray *FImageIndex;
	int FImagesSize;
	System::WideChar *FStates;
	int FStatesSize;
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	Vcl::Graphics::TCanvas* FCanvas;
	int FColumns;
	bool FExtendedSelect;
	bool FIntegralHeight;
	int FItemHeight;
	bool FMultiSelect;
	bool FSorted;
	int FTabWidth;
	int FCurHintItem;
	int FLastTopIndex;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	Elunicodestrings::TElWideStrings* FListBoxStrings;
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Vcl::Graphics::TBitmap* FBackground;
	Elvclutils::TElBorderSides FBorderSides;
	bool FFlat;
	bool FFlatFocusedScrollBars;
	bool FHorizontalScroll;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FInvertSelection;
	System::Uitypes::TColor FSelectedColor;
	Vcl::Graphics::TFont* FSelectedFont;
	bool FShowLineHint;
	NativeUInt FTheme;
	bool FTransparent;
	bool FTransparentSelection;
	bool FUseBackground;
	Elimgfrm::TElImageForm* FImgForm;
	bool FMouseOver;
	Vcl::Extctrls::TTimer* FHintTimer;
	Elhintwnd::TElHintWindow* FHintWnd;
	System::Classes::TWndMethod FHintWndProc;
	int FMaxWidth;
	bool FInVScroll;
	bool FInHScroll;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	bool FUseXPThemes;
	bool FMoving;
	bool FShowCheckBox;
	bool FAllowGrayed;
	Vcl::Controls::TImageList* FImages;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	int FSaveTopIndex;
	int FSaveItemIndex;
	System::WideString FHint;
	TDrawTextEvent FOnDrawText;
	Vcl::Stdctrls::TListBoxStyle FStyle;
	Vcl::Stdctrls::TDrawItemEvent FOnDrawItem;
	Vcl::Stdctrls::TMeasureItemEvent FOnMeasureItem;
	bool FUseSelectedFont;
	System::Uitypes::TColor FSelectedFontColor;
	void __fastcall SetStyle(Vcl::Stdctrls::TListBoxStyle Value);
	void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* const Value);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	void __fastcall SetFlat(const bool Value);
	void __fastcall SetFlatFocusedScrollBars(const bool Value);
	void __fastcall SetHorizontalScroll(bool Value);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetInvertSelection(const bool Value);
	void __fastcall SetSelectedColor(const System::Uitypes::TColor Value);
	void __fastcall SetSelectedFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetTransparent(const bool Value);
	void __fastcall SetTransparentSelection(bool Value);
	void __fastcall SetUseBackground(const bool Value);
	void __fastcall BackgroundChanged(System::TObject* Sender);
	void __fastcall CancelLineHint();
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMParentColorChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMParentFontChanged(Winapi::Messages::TMessage &Msg);
	void __fastcall DrawBackground(HDC DC, const System::Types::TRect &R);
	void __fastcall DrawBackgroundEx(HDC DC, const System::Types::TRect &R, const System::Types::TRect &SubR);
	void __fastcall DrawFlatBorder(HDC DC, bool HDragging, bool VDragging);
	void __fastcall DrawParentControl(HDC DC);
	void __fastcall DrawParentControlEx(HDC DC, const System::Types::TRect &R);
	void __fastcall HintWndProc(Winapi::Messages::TMessage &Message);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall IntMouseMove(short XPos, short YPos);
	MESSAGE void __fastcall LBGetTopIndex(Winapi::Messages::TMessage &Msg);
	void __fastcall OnLineHintTimer(System::TObject* Sender);
	virtual void __fastcall ResetHorizontalExtent();
	void __fastcall SelectedFontChanged(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	MESSAGE void __fastcall WMNCMouseMove(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	void __fastcall ResetHorizontalExtent1();
	virtual void __fastcall SetHorizontalExtent();
	void __fastcall SetColumnWidth();
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Message);
	MESSAGE void __fastcall CNDrawItem(Winapi::Messages::TWMDrawItem &Message);
	MESSAGE void __fastcall CNMeasureItem(Winapi::Messages::TWMMeasureItem &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	int __fastcall GetItemHeight();
	int __fastcall GetItemIndex();
	int __fastcall GetSelCount();
	bool __fastcall GetSelected(int Index);
	int __fastcall GetTopIndex();
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetColumns(int Value);
	void __fastcall SetExtendedSelect(bool Value);
	void __fastcall SetIntegralHeight(bool Value);
	void __fastcall SetItemHeight(int Value);
	void __fastcall SetItemIndex(int Value);
	void __fastcall SetItems(Elunicodestrings::TElWideStrings* Value);
	void __fastcall SetMultiSelect(bool Value);
	void __fastcall SetSelected(int Index, bool Value);
	void __fastcall SetSorted(bool Value);
	void __fastcall SetTabWidth(int Value);
	void __fastcall SetTopIndex(int Value);
	virtual Vcl::Graphics::TBitmap* __fastcall GetBackground();
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetUseXPThemes(bool Value);
	virtual Elhintwnd::TElHintWindow* __fastcall CreateHintWindow();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateThemeHandle();
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &R, Winapi::Windows::TOwnerDrawState State);
	virtual void __fastcall FreeThemeHandle();
	virtual int __fastcall GetItemWidth(int Index);
	virtual int __fastcall GetParentCtlHeight();
	virtual int __fastcall GetParentCtlWidth();
	System::WideString __fastcall GetThemedClassName();
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	bool __fastcall IsThemeApplied();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual System::Types::TPoint __fastcall RealScreenToClient(const System::Types::TPoint &APoint);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	DYNAMIC int __fastcall InternalGetItemData(int Index);
	DYNAMIC void __fastcall InternalSetItemData(int Index, int AData);
	DYNAMIC int __fastcall GetItemData(int Index);
	DYNAMIC void __fastcall SetItemData(int Index, int AData);
	DYNAMIC void __fastcall ResetContent();
	DYNAMIC void __fastcall DeleteString(int Index);
	void __fastcall SetShowCheckBox(bool Value);
	Vcl::Stdctrls::TCheckBoxState __fastcall GetState(int Index);
	void __fastcall SetState(int Index, Vcl::Stdctrls::TCheckBoxState Value);
	System::Types::TSize __fastcall GetCheckBoxSize();
	void __fastcall SetAllowGrayed(bool Value);
	void __fastcall DrawFlatFrame(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	void __fastcall OnImageListChange(System::TObject* Sender);
	void __fastcall SetImages(Vcl::Controls::TImageList* newValue);
	virtual void __fastcall AdjustItemHeight();
	int __fastcall GetImageIndex(int Index);
	void __fastcall SetImageIndex(int Index, int Value);
	void __fastcall SetStatesSize(int aSize);
	void __fastcall SetImagesSize(int aSize);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	void __fastcall SetHint(System::WideString Value);
	void __fastcall ItemsChange(System::TObject* Sender);
	virtual void __fastcall DoDrawText(Vcl::Graphics::TCanvas* ACanvas, const Elstrutils::TElFString ACaption, System::Types::TRect &Rect, int Flags);
	virtual void __fastcall MeasureItem(int Index, int &Height);
	void __fastcall SetUseSelectedFont(bool Value);
	void __fastcall SetSelectedFontColor(System::Uitypes::TColor Value);
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property int Columns = {read=FColumns, write=SetColumns, default=0};
	__property bool ExtendedSelect = {read=FExtendedSelect, write=SetExtendedSelect, default=1};
	__property bool IntegralHeight = {read=FIntegralHeight, write=SetIntegralHeight, default=0};
	__property int ItemHeight = {read=GetItemHeight, write=SetItemHeight, nodefault};
	__property bool MultiSelect = {read=FMultiSelect, write=SetMultiSelect, default=0};
	__property ParentColor = {default=0};
	__property bool Sorted = {read=FSorted, write=SetSorted, default=0};
	__property int TabWidth = {read=FTabWidth, write=SetTabWidth, default=0};
	__property int ItemIndex = {read=GetItemIndex, write=SetItemIndex, nodefault};
	__property Elunicodestrings::TElWideStrings* Items = {read=FListBoxStrings, write=SetItems};
	__property int SelCount = {read=GetSelCount, nodefault};
	__property int TopIndex = {read=GetTopIndex, write=SetTopIndex, nodefault};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Vcl::Graphics::TBitmap* Background = {read=GetBackground, write=SetBackground};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property bool FlatFocusedScrollBars = {read=FFlatFocusedScrollBars, write=SetFlatFocusedScrollBars, default=0};
	__property bool HorizontalScroll = {read=FHorizontalScroll, write=SetHorizontalScroll, nodefault};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool InvertSelection = {read=FInvertSelection, write=SetInvertSelection, default=0};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property System::Uitypes::TColor SelectedColor = {read=FSelectedColor, write=SetSelectedColor, default=-16777203};
	__property Vcl::Graphics::TFont* SelectedFont = {read=FSelectedFont, write=SetSelectedFont};
	__property bool ShowLineHint = {read=FShowLineHint, write=FShowLineHint, default=0};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property bool TransparentSelection = {read=FTransparentSelection, write=SetTransparentSelection, default=0};
	__property bool UseBackground = {read=FUseBackground, write=SetUseBackground, default=0};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property TabStop = {default=1};
	__property bool ShowCheckBox = {read=FShowCheckBox, write=SetShowCheckBox, default=0};
	__property bool AllowGrayed = {read=FAllowGrayed, write=SetAllowGrayed, default=1};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property Vcl::Stdctrls::TListBoxStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property bool UseSelectedFont = {read=FUseSelectedFont, write=SetUseSelectedFont, default=0};
	__property System::Uitypes::TColor SelectedFontColor = {read=FSelectedFontColor, write=SetSelectedFontColor, default=-16777202};
	
public:
	__fastcall virtual TCustomElListBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElListBox();
	int __fastcall ItemAtPos(const System::Types::TPoint &Pos, bool Existing);
	System::Types::TRect __fastcall ItemRect(int Index);
	__property NativeUInt Theme = {read=FTheme, nodefault};
	__property Vcl::Graphics::TCanvas* Canvas = {read=FCanvas};
	__property bool Selected[int Index] = {read=GetSelected, write=SetSelected};
	__property Vcl::Stdctrls::TCheckBoxState State[int Index] = {read=GetState, write=SetState};
	__property int ImageIndex[int Index] = {read=GetImageIndex, write=SetImageIndex};
	__property TDrawTextEvent OnDrawText = {read=FOnDrawText, write=FOnDrawText};
	__property Vcl::Stdctrls::TDrawItemEvent OnDrawItem = {read=FOnDrawItem, write=FOnDrawItem};
	__property Vcl::Stdctrls::TMeasureItemEvent OnMeasureItem = {read=FOnMeasureItem, write=FOnMeasureItem};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElListBox(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElListBox : public TCustomElListBox
{
	typedef TCustomElListBox inherited;
	
__published:
	__property AllowGrayed = {default=1};
	__property BorderStyle = {default=1};
	__property Columns = {default=0};
	__property ExtendedSelect = {default=1};
	__property IntegralHeight = {default=0};
	__property ItemHeight;
	__property MultiSelect = {default=0};
	__property ParentColor = {default=0};
	__property Sorted = {default=0};
	__property TabWidth = {default=0};
	__property ItemIndex;
	__property Items;
	__property SelCount;
	__property TopIndex;
	__property ShowCheckBox = {default=0};
	__property ActiveBorderType = {default=1};
	__property Background;
	__property BorderSides;
	__property Flat = {default=0};
	__property Ctl3D;
	__property ParentCtl3D = {default=1};
	__property Font;
	__property FlatFocusedScrollBars = {default=0};
	__property HorizontalScroll;
	__property Images;
	__property ImageForm;
	__property InactiveBorderType = {default=3};
	__property InvertSelection = {default=0};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property SelectedColor = {default=-16777203};
	__property SelectedFont;
	__property ShowLineHint = {default=0};
	__property Transparent = {default=0};
	__property TransparentSelection = {default=0};
	__property UseBackground = {default=0};
	__property UseSelectedFont = {default=0};
	__property UseXPThemes = {default=1};
	__property TabStop = {default=1};
	__property ParentFont = {default=1};
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
	__property Align = {default=0};
	__property Color = {default=-16777211};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property Visible = {default=1};
	__property Style = {default=0};
	__property OnDrawItem;
	__property OnMeasureItem;
public:
	/* TCustomElListBox.Create */ inline __fastcall virtual TElListBox(System::Classes::TComponent* AOwner) : TCustomElListBox(AOwner) { }
	/* TCustomElListBox.Destroy */ inline __fastcall virtual ~TElListBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElListBox(HWND ParentWindow) : TCustomElListBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ellistbox */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELLISTBOX)
using namespace Ellistbox;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EllistboxHPP
