// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeGrids.pas' rev: 34.00 (Windows)

#ifndef EltreegridsHPP
#define EltreegridsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.Classes.hpp>
#include <ElStrArray.hpp>
#include <ElTree.hpp>
#include <ElHeader.hpp>
#include <ElStrUtils.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElTreeBtnEdit.hpp>
#include <ElXPThemedControl.hpp>
#include <System.Types.hpp>
#include <ElVCLUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <System.UITypes.hpp>
#include <ElScrollBar.hpp>
#include <ElImgFrm.hpp>
#include <Vcl.Menus.hpp>
#include <ElIni.hpp>
#include <HTMLRender.hpp>
#include <ElDragDrop.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreegrids
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomElTreeGrid;
class DELPHICLASS TElTreeTrickyInplaceEdit;
class DELPHICLASS TElTreeStringGrid;
class DELPHICLASS TElTreeGridView;
class DELPHICLASS EElTreeGridError;
//-- type declarations -------------------------------------------------------
typedef Elunicodestrings::TElWideStringArray TElFStringArray;

class PASCALIMPLEMENTATION TCustomElTreeGrid : public Eltree::TCustomElTree
{
	typedef Eltree::TCustomElTree inherited;
	
private:
	bool FgoAlwaysShowEditor;
	bool FgoRowSelect;
	bool FgoColMoving;
	bool FgoTabs;
	void __fastcall SetgoRowSelect(bool Value);
	void __fastcall SetgoColMoving(bool Value);
	int __fastcall GetCol();
	void __fastcall SetCol(int Value);
	int __fastcall GetRow();
	void __fastcall SetRow(int Value);
	int __fastcall GetLeftCol();
	void __fastcall SetLeftCol(int Value);
	int __fastcall GetColCount();
	void __fastcall SetColCount(int Value);
	int __fastcall GetColWidths(int Index);
	void __fastcall SetColWidths(int Index, int Value);
	int __fastcall GetDefaultColWidth();
	void __fastcall SetDefaultColWidth(int Value);
	int __fastcall GetDefaultRowHeight();
	void __fastcall SetDefaultRowHeight(int Value);
	bool __fastcall GetEditorMode();
	void __fastcall SetEditorMode(bool Value);
	int __fastcall GetRowCount();
	void __fastcall SetRowCount(int Value);
	int __fastcall GetTopRow();
	void __fastcall SetTopRow(int Value);
	bool __fastcall GetgoEditing();
	void __fastcall SetgoEditing(bool Value);
	
protected:
	bool FgoTabSkipNonEditable;
	virtual void __fastcall KeyDownTransfer(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TMessage &Message);
	virtual Eltree::TElTreeView* __fastcall CreateView();
	Eltree::TElTreeItem* __fastcall GetAsCell(int ACol, int ARow);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
	virtual void __fastcall MouseDownTransfer(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall EnsureColumnVisible(int SectionNumber);
	__property bool goAlwaysShowEditor = {read=FgoAlwaysShowEditor, write=FgoAlwaysShowEditor, nodefault};
	__property bool goRowSelect = {read=FgoRowSelect, write=SetgoRowSelect, nodefault};
	__property bool goColMoving = {read=FgoColMoving, write=SetgoColMoving, default=1};
	__property bool goTabs = {read=FgoTabs, write=FgoTabs, nodefault};
	__property int ColCount = {read=GetColCount, write=SetColCount, default=5};
	__property int DefaultColWidth = {read=GetDefaultColWidth, write=SetDefaultColWidth, default=64};
	__property int DefaultRowHeight = {read=GetDefaultRowHeight, write=SetDefaultRowHeight, default=24};
	__property bool EditorMode = {read=GetEditorMode, write=SetEditorMode, nodefault};
	__property int RowCount = {read=GetRowCount, write=SetRowCount, default=5};
	__property bool goEditing = {read=GetgoEditing, write=SetgoEditing, default=1};
	__property bool goTabSkipNonEditable = {read=FgoTabSkipNonEditable, write=FgoTabSkipNonEditable, nodefault};
	
public:
	System::Types::TRect __fastcall CellRect(int ACol, int ARow);
	void __fastcall MouseToCell(int X, int Y, int &ACol, int &ARow);
	__fastcall virtual TCustomElTreeGrid(System::Classes::TComponent* Owner);
	Elheader::TElHeaderSection* __fastcall GetNextEditableSection(Elheader::TElHeaderSection* Section, bool GoForward);
	__property int Col = {read=GetCol, write=SetCol, nodefault};
	__property int ColWidths[int Index] = {read=GetColWidths, write=SetColWidths};
	__property int LeftCol = {read=GetLeftCol, write=SetLeftCol, nodefault};
	__property int Row = {read=GetRow, write=SetRow, nodefault};
	__property int TopRow = {read=GetTopRow, write=SetTopRow, nodefault};
	
__published:
	__property VerticalLines = {default=1};
	__property HorizontalLines = {default=1};
public:
	/* TCustomElTree.CreateClass */ inline __fastcall TCustomElTreeGrid(System::Classes::TComponent* AOwner, Eltree::TElTreeItemClass ItemClass) : Eltree::TCustomElTree(AOwner, ItemClass) { }
	/* TCustomElTree.Destroy */ inline __fastcall virtual ~TCustomElTreeGrid() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElTreeGrid(HWND ParentWindow) : Eltree::TCustomElTree(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElTreeTrickyInplaceEdit : public Eltreebtnedit::TElTreeInplaceButtonEdit
{
	typedef Eltreebtnedit::TElTreeInplaceButtonEdit inherited;
	
private:
	System::UnicodeString FDummyStr;
	
__published:
	__property System::UnicodeString Name = {read=FDummyStr};
	__property System::UnicodeString Tree = {read=FDummyStr};
public:
	/* TElTreeInplaceButtonEdit.Create */ inline __fastcall virtual TElTreeTrickyInplaceEdit(System::Classes::TComponent* AOwner) : Eltreebtnedit::TElTreeInplaceButtonEdit(AOwner) { }
	/* TElTreeInplaceButtonEdit.Destroy */ inline __fastcall virtual ~TElTreeTrickyInplaceEdit() { }
	
};


class PASCALIMPLEMENTATION TElTreeStringGrid : public TCustomElTreeGrid
{
	typedef TCustomElTreeGrid inherited;
	
private:
	Elunicodestrings::TElWideStringArray* FCols;
	Elunicodestrings::TElWideStringArray* FRows;
	Eltreebtnedit::TElTreeInplaceButtonEdit* FEditor;
	System::Classes::TWndMethod FSavedEditWndProc;
	Elunicodestrings::TElWideStrings* __fastcall GetCols(int Index);
	void __fastcall SetCols(int Index, Elunicodestrings::TElWideStrings* Value);
	Elunicodestrings::TElWideStrings* __fastcall GetRows(int Index);
	void __fastcall SetRows(int Index, Elunicodestrings::TElWideStrings* Value);
	Elstrutils::TElFString __fastcall GetCells(int ACol, int ARow);
	void __fastcall SetCells(int ACol, int ARow, Elstrutils::TElFString Value);
	System::TObject* __fastcall GetObjects(int ACol, int ARow);
	void __fastcall SetObjects(int ACol, int ARow, System::TObject* Value);
	void __fastcall SetEditor(Eltreebtnedit::TElTreeInplaceButtonEdit* Value);
	
protected:
	bool FUseDefaultEditor;
	virtual void __fastcall TriggerInplaceEditorNeeded(Eltree::TElTreeItem* Item, int SectionIndex, Elheader::TElFieldType SupposedFieldType, Eltree::TElTreeInplaceEditor* &Editor);
	virtual void __fastcall OnFontChange(System::TObject* Sender);
	virtual void __fastcall KeyDownTransfer(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall EditorKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall EditWndProc(Winapi::Messages::TMessage &Message);
	
public:
	__fastcall virtual TElTreeStringGrid(System::Classes::TComponent* Owner);
	__fastcall virtual ~TElTreeStringGrid();
	__property Elunicodestrings::TElWideStrings* Cols[int Index] = {read=GetCols, write=SetCols};
	__property Elunicodestrings::TElWideStrings* Rows[int Index] = {read=GetRows, write=SetRows};
	__property Elstrutils::TElFString Cells[int ACol][int ARow] = {read=GetCells, write=SetCells/*, default*/};
	__property System::TObject* Objects[int ACol][int ARow] = {read=GetObjects, write=SetObjects};
	__property Eltreebtnedit::TElTreeInplaceButtonEdit* Editor = {read=FEditor, write=SetEditor};
	
__published:
	__property bool UseDefaultEditor = {read=FUseDefaultEditor, write=FUseDefaultEditor, default=0};
	__property ColCount = {default=5};
	__property RowCount = {default=5};
	__property goAlwaysShowEditor = {default=0};
	__property goRowSelect = {default=0};
	__property goColMoving = {default=1};
	__property goEditing = {default=1};
	__property goTabs = {default=0};
	__property goTabSkipNonEditable = {default=0};
	__property DefaultColWidth = {default=64};
	__property DefaultRowHeight = {default=24};
	__property ActiveBorderType = {default=1};
	__property Align = {default=0};
	__property AutoLookup = {default=0};
	__property AutoResizeColumns = {default=1};
	__property AdjustMultilineHeight = {default=1};
	__property Background;
	__property BackgroundType = {default=2};
	__property BorderStyle = {default=1};
	__property ChangeDelay = {default=500};
	__property ChangeStateImage = {default=0};
	__property CheckBoxGlyph;
	__property Ctl3D;
	__property Color = {default=-16777211};
	__property Cursor = {default=-2};
	__property CustomCheckboxes = {default=0};
	__property CustomPlusMinus = {default=0};
	__property VertDivLinesColor = {default=-16777201};
	__property HorzDivLinesColor = {default=-16777201};
	__property DragCursor;
	__property DragAllowed = {default=0};
	__property DragTrgDrawMode = {default=2};
	__property DragType = {default=1};
	__property DragExpandDelay = {default=500};
	__property DragImageMode = {default=0};
	__property DrawFocusRect = {default=1};
	__property DragRectAcceptColor = {default=32768};
	__property DragRectDenyColor = {default=255};
	__property Enabled = {default=1};
	__property ExpandOnDragOver = {default=0};
	__property ExplorerEditMode;
	__property FilteredVisibility = {default=0};
	__property Flat = {default=0};
	__property FlatFocusedScrollbars = {default=1};
	__property FocusedSelectColor = {default=-16777203};
	__property FocusedSelectTextColor = {default=-16777202};
	__property ForcedScrollBars = {default=0};
	__property Font = {stored=true};
	__property GradientStartColor = {default=0};
	__property GradientEndColor = {default=0};
	__property GradientSteps = {default=64};
	__property HeaderActiveFilterColor = {default=0};
	__property HeaderColor = {default=-16777201};
	__property HeaderHeight = {default=0};
	__property HeaderHotTrack = {default=1};
	__property HeaderInvertSortArrows = {default=0};
	__property HeaderSections;
	__property HeaderFilterColor = {default=-16777198};
	__property HeaderFlat = {default=0};
	__property HeaderImages;
	__property HeaderWrapCaptions = {default=0};
	__property HideFocusRect = {default=0};
	__property HideHintOnTimer = {default=0};
	__property HideHintOnMove = {default=1};
	__property HideSelectColor = {default=-16777201};
	__property HideSelectTextColor = {default=-16777200};
	__property HideSelection = {default=0};
	__property HorizontalLines = {default=1};
	__property HideHorzScrollBar = {default=0};
	__property HideVertScrollBar = {default=0};
	__property Hint;
	__property HorzScrollBarStyles;
	__property HeaderImageForm;
	__property ImageForm;
	__property Images;
	__property Images2;
	__property InactiveBorderType = {default=3};
	__property InplaceEditorDelay = {default=500};
	__property ItemIndent = {default=17};
	__property Items;
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property LinesColor = {default=-16777201};
	__property LinesStyle = {default=2};
	__property LineHintColor = {default=-16777211};
	__property LineHintMode = {default=1};
	__property LineHintTimeout = {default=3000};
	__property LockHeaderHeight = {default=1};
	__property MainTreeColumn = {default=0};
	__property MinusPicture;
	__property MoveFocusOnCollapse = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PlusMinusTransparent = {default=0};
	__property PlusPicture;
	__property PopupMenu;
	__property RadioButtonGlyph;
	__property RightAlignedText = {default=0};
	__property RightAlignedTree = {default=0};
	__property RightClickSelect = {default=1};
	__property ScrollbarOpposite;
	__property ScrollTracking = {default=0};
	__property ShowButtons = {default=0};
	__property ShowCheckboxes = {default=0};
	__property ShowEmptyImages = {default=0};
	__property ShowEmptyImages2 = {default=0};
	__property ShowHint;
	__property ShowImages = {default=0};
	__property ShowLines = {default=0};
	__property ShowRoot = {default=0};
	__property ShowRootButtons = {default=1};
	__property SortDir = {default=0};
	__property SortMode = {default=0};
	__property SortSection = {default=0};
	__property SortType = {default=1};
	__property Storage;
	__property StoragePath = {default=0};
	__property StickyHeaderSections = {default=0};
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property UseCustomScrollBars = {default=1};
	__property VerticalLines = {default=1};
	__property VerticalLinesLong = {default=0};
	__property VertScrollBarStyles;
	__property Visible = {default=1};
	__property UseSystemHintColors = {default=0};
	__property TextColor = {default=-16777208};
	__property BkColor = {default=-16777211};
	__property OnScroll;
	__property OnHeaderColumnClick;
	__property OnHeaderColumnDraw;
	__property OnHeaderColumnResize;
	__property OnHeaderColumnMove;
	__property OnHeaderLookup;
	__property OnHeaderLookupDone;
	__property OnHeaderResize;
	__property OnHeaderSectionExpand;
	__property OnHeaderSectionCollapse;
	__property OnHeaderSectionFilterCall;
	__property OnHeaderSectionAutoSize;
	__property OnHeaderSectionMeasure;
	__property OnHorzScrollDrawPart;
	__property OnHorzScrollHintNeeded;
	__property OnAfterSelectionChange;
	__property OnChanging;
	__property OnDragTargetChange;
	__property OnItemChange;
	__property OnItemPreDraw;
	__property OnItemDraw;
	__property OnResize;
	__property OnInplaceEditorNeeded;
	__property OnItemChecked;
	__property OnItemExpand;
	__property OnItemCollapse;
	__property OnItemExpanding;
	__property OnItemCollapsing;
	__property OnItemDeletion;
	__property OnItemFocused;
	__property OnShowLineHint;
	__property OnCompareItems;
	__property OnItemPicDraw;
	__property OnItemPicDraw2;
	__property OnItemPostDraw;
	__property OnHotTrack;
	__property OnSortBegin;
	__property OnSortEnd;
	__property OnItemSave;
	__property OnItemLoad;
	__property OnItemSelectedChange;
	__property OnCellStyleSave;
	__property OnCellStyleLoad;
	__property OnVertScrollDrawPart;
	__property OnVertScrollHintNeeded;
	__property OnHTMLImageNeeded;
	__property OnHeaderMouseDown;
	__property OnClick;
	__property OnEnter;
	__property OnExit;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnStartDrag;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnDblClick;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnOleTargetDrag;
	__property OnOleTargetDrop;
	__property OnOleDragStart;
	__property OnOleDragFinish;
public:
	/* TCustomElTree.CreateClass */ inline __fastcall TElTreeStringGrid(System::Classes::TComponent* AOwner, Eltree::TElTreeItemClass ItemClass) : TCustomElTreeGrid(AOwner, ItemClass) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElTreeStringGrid(HWND ParentWindow) : TCustomElTreeGrid(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElTreeGridView : public Eltree::TElTreeView
{
	typedef Eltree::TElTreeView inherited;
	
protected:
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	MESSAGE void __fastcall WMLButtonDblClick(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
public:
	/* TElTreeView.Create */ inline __fastcall virtual TElTreeGridView(System::Classes::TComponent* Owner) : Eltree::TElTreeView(Owner) { }
	/* TElTreeView.Destroy */ inline __fastcall virtual ~TElTreeGridView() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElTreeGridView(HWND ParentWindow) : Eltree::TElTreeView(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION EElTreeGridError : public Eltree::EElTreeError
{
	typedef Eltree::EElTreeError inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElTreeGridError(const System::UnicodeString Msg) : Eltree::EElTreeError(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElTreeGridError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Eltree::EElTreeError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElTreeGridError(NativeUInt Ident)/* overload */ : Eltree::EElTreeError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElTreeGridError(System::PResStringRec ResStringRec)/* overload */ : Eltree::EElTreeError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElTreeGridError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Eltree::EElTreeError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElTreeGridError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Eltree::EElTreeError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElTreeGridError(const System::UnicodeString Msg, int AHelpContext) : Eltree::EElTreeError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElTreeGridError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Eltree::EElTreeError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElTreeGridError(NativeUInt Ident, int AHelpContext)/* overload */ : Eltree::EElTreeError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElTreeGridError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Eltree::EElTreeError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElTreeGridError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Eltree::EElTreeError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElTreeGridError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Eltree::EElTreeError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElTreeGridError() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreegrids */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREEGRIDS)
using namespace Eltreegrids;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreegridsHPP
