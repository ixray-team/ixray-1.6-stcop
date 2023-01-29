// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElShellCtl.pas' rev: 34.00 (Windows)

#ifndef ElshellctlHPP
#define ElshellctlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Menus.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.CommCtrl.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ShellAPI.hpp>
#include <Winapi.ShlObj.hpp>
#include <System.Win.ComObj.hpp>
#include <Winapi.ActiveX.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Variants.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElACtrls.hpp>
#include <ElTreeStdEditors.hpp>
#include <ElShellUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElTools.hpp>
#include <ElList.hpp>
#include <ElHeader.hpp>
#include <ElTree.hpp>
#include <ElXPThemedControl.hpp>
#include <ElVCLUtils.hpp>
#include <System.UITypes.hpp>
#include <ElScrollBar.hpp>
#include <ElImgFrm.hpp>
#include <ElIni.hpp>
#include <HTMLRender.hpp>
#include <ElDragDrop.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elshellctl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EElShellError;
class DELPHICLASS TElShellTreeItem;
class DELPHICLASS TElShellTree;
class DELPHICLASS TElShellComboBox;
class DELPHICLASS TElShellListItem;
class DELPHICLASS TElShellList;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElShellSortType : unsigned char { sstCustom, sstName, sstExt, sstSize, sstCreationDate, sstModifyDate, sstAccessDate };

enum DECLSPEC_DENUM TElShellSortModifier : unsigned char { ssmFoldersFirst, ssmExecutablesFirst };

typedef System::Set<TElShellSortModifier, TElShellSortModifier::ssmFoldersFirst, TElShellSortModifier::ssmExecutablesFirst> TElShellSortModifiers;

enum DECLSPEC_DENUM TElShellSizeFormat : unsigned char { ssfAsIs, ssfKb, ssfAuto };

#pragma pack(push,4)
class PASCALIMPLEMENTATION EElShellError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElShellError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElShellError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElShellError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElShellError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElShellError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElShellError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElShellError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElShellError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElShellError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElShellError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElShellError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElShellError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElShellError() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElShellTreeItem : public Eltree::TElTreeItem
{
	typedef Eltree::TElTreeItem inherited;
	
private:
	bool FIsValidFile;
	
protected:
	bool FValid;
	System::UnicodeString FAttrAsString;
	System::UnicodeString FComment;
	System::UnicodeString FSizeAsString;
	System::UnicodeString FDisplayName;
	System::UnicodeString FTypeName;
	System::UnicodeString FFileName;
	unsigned FAttr;
	_ITEMIDLIST *FPIDL;
	_WIN32_FIND_DATAW *Win32FindData;
	void __fastcall GetAttributes(_di_IShellFolder iParentFolder);
	System::UnicodeString __fastcall GetDisplayName();
	HIDESBASE System::UnicodeString __fastcall GetFullName();
	bool __fastcall GetHasSubFolders();
	bool __fastcall GetIsFolder();
	bool __fastcall GetIsRemovable();
	_di_IShellFolder __fastcall GetParentFolder();
	Winapi::Shlobj::PItemIDList __fastcall GetPIDL();
	unsigned __fastcall GetSize();
	void __fastcall SetDisplayName(const System::UnicodeString Value);
	System::TDateTime __fastcall GetCreationTime();
	System::TDateTime __fastcall GetModificationTime();
	System::TDateTime __fastcall GetLastAccessTime();
	void __fastcall GetWin32Data(_di_IShellFolder ParentFolder);
	void __fastcall CheckWin32FindData();
	System::UnicodeString __fastcall GetFileName();
	System::UnicodeString __fastcall GetSizeAsString();
	System::UnicodeString __fastcall GetTypeName();
	bool __fastcall GetIsFileObject();
	System::UnicodeString __fastcall GetAttrAsString();
	System::UnicodeString __fastcall GetComment();
	void __fastcall Invalidate();
	TElShellTreeItem* __fastcall FindItemByPIDL(Winapi::Shlobj::PItemIDList APIDL);
	bool __fastcall GetCanRename();
	int __fastcall GetPicture(_di_IShellFolder ParentFolder);
	
public:
	__fastcall virtual TElShellTreeItem(Eltree::TCustomElTree* AOwner);
	__fastcall virtual ~TElShellTreeItem();
	Winapi::Shlobj::PItemIDList __fastcall BuildFullPIDL();
	__property unsigned Attr = {read=FAttr, nodefault};
	__property System::UnicodeString DisplayName = {read=GetDisplayName, write=SetDisplayName};
	__property System::UnicodeString FullName = {read=GetFullName};
	__property bool HasSubFolders = {read=GetHasSubFolders, nodefault};
	__property bool IsFolder = {read=GetIsFolder, nodefault};
	__property bool IsRemovable = {read=GetIsRemovable, nodefault};
	__property _di_IShellFolder ParentFolder = {read=GetParentFolder};
	__property Winapi::Shlobj::PItemIDList PIDL = {read=GetPIDL};
	__property System::TDateTime ModificationTime = {read=GetModificationTime};
	__property System::TDateTime CreationTime = {read=GetCreationTime};
	__property System::TDateTime LastAccessTime = {read=GetLastAccessTime};
	__property System::UnicodeString FileName = {read=GetFileName};
	__property System::UnicodeString SizeAsString = {read=GetSizeAsString};
	__property bool IsFileObject = {read=GetIsFileObject, nodefault};
	__property System::UnicodeString Comment = {read=GetComment};
	__property bool CanRename = {read=GetCanRename, nodefault};
	
__published:
	__property unsigned Size = {read=GetSize, nodefault};
	__property System::UnicodeString TypeName = {read=GetTypeName};
	__property System::UnicodeString AttrAsString = {read=GetAttrAsString};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TShellTreeItemAddingEvent)(System::TObject* Sender, System::UnicodeString ItemName, _di_IShellFolder ShellFolder, Winapi::Shlobj::PItemIDList RelPIDL, bool &Allowed);

typedef void __fastcall (__closure *TShellTreeItemAddedEvent)(System::TObject* Sender, System::UnicodeString ItemName, _di_IShellFolder ShellFolder, Winapi::Shlobj::PItemIDList RelPIDL, TElShellTreeItem* Item);

class PASCALIMPLEMENTATION TElShellTree : public Eltree::TCustomElTree
{
	typedef Eltree::TCustomElTree inherited;
	
protected:
	Eltreestdeditors::TElTreeInplaceEdit* FEditor;
	Elshellutils::TShellFolders FRootFolder;
	_ITEMIDLIST *FRootPIDL;
	_ITEMIDLIST *FFocusedPIDL;
	_di_IShellFolder FIFolder;
	System::UnicodeString FCustomRootFolder;
	bool FUseSystemMenus;
	bool FClearOnCollapse;
	bool FCheckForChildren;
	bool FShowHidden;
	bool FShowFiles;
	bool FHighlightCompressed;
	System::Classes::TStringList* FFileFilters;
	TShellTreeItemAddingEvent FOnFilterItem;
	TShellTreeItemAddedEvent FOnItemAdded;
	bool FFileSystemOnly;
	TElShellSortType FSortType;
	TElShellSortModifiers FSortModifiers;
	TElShellSizeFormat FSizeFormat;
	bool FDefaultColumns;
	bool FDefaultEditor;
	int FMaxColumns;
	bool FExpandRoot;
	Winapi::Shlobj::PItemIDList __fastcall GetFocusedPIDL();
	System::UnicodeString __fastcall GetFocusedDisplayName();
	void __fastcall BuildTree();
	void __fastcall ReleaseFocusedPIDL();
	virtual void __fastcall DoItemFocused();
	void __fastcall SetCustomRootFolder(const System::UnicodeString Value);
	virtual Eltree::TElTreeView* __fastcall CreateView();
	virtual Eltree::TElTreeItems* __fastcall CreateItems();
	bool __fastcall CheckChildren(Eltree::TElTreeItem* Item, _di_IShellFolder AFolder);
	void __fastcall FillItemWithData(TElShellTreeItem* Item, _di_IShellFolder AFolder, int recursive);
	void __fastcall SetShowHidden(bool Value);
	void __fastcall SetShowFiles(bool Value);
	void __fastcall SetHighlightCompressed(bool Value);
	System::Classes::TStrings* __fastcall GetFileFilters();
	void __fastcall SetFileFilters(System::Classes::TStrings* const Value);
	virtual bool __fastcall NameFiltered(System::UnicodeString S, _di_IShellFolder ShellFolder, Winapi::Shlobj::PItemIDList RelPIDL);
	virtual void __fastcall CreateHandle();
	virtual void __fastcall DoItemCollapse(Eltree::TElTreeItem* Item);
	virtual void __fastcall DoItemExpand(Eltree::TElTreeItem* Item);
	virtual void __fastcall DoItemExpanding(Eltree::TElTreeItem* Item, bool &CanProcess);
	Elshellutils::TShellFolders __fastcall GetRootFolder();
	void __fastcall SetRootFolder(Elshellutils::TShellFolders Value);
	virtual void __fastcall DoItemAdded(System::UnicodeString S, _di_IShellFolder ShellFolder, Winapi::Shlobj::PItemIDList RelPIDL, TElShellTreeItem* Item);
	void __fastcall SetFileSystemOnly(bool Value);
	TElShellTreeItem* __fastcall GetItemFocused();
	void __fastcall SetItemFocused(TElShellTreeItem* Value);
	virtual void __fastcall DoCompareItems(Eltree::TElTreeItem* Item1, Eltree::TElTreeItem* Item2, int &res);
	void __fastcall SetSortType(TElShellSortType Value);
	void __fastcall SetSortModifiers(TElShellSortModifiers Value);
	void __fastcall SetSizeFormat(TElShellSizeFormat Value);
	void __fastcall SetDefaultColumns(bool Value);
	void __fastcall AddDefaultColumns();
	int __fastcall DeleteDefaultColumns();
	virtual void __fastcall TriggerVirtualTextNeeded(Eltree::TElTreeItem* Item, int SectionIndex, WideString &Text);
	virtual void __fastcall TriggerVirtualValueNeeded(Eltree::TElTreeItem* Item, int SectionIndex, int VarType, System::Variant &Value);
	virtual void __fastcall TriggerSortBegin();
	virtual void __fastcall TriggerTryEditEvent(Eltree::TElTreeItem* Item, int SectionIndex, Elheader::TElFieldType &CellType, bool &CanEdit);
	virtual void __fastcall TriggerInplaceEditorNeeded(Eltree::TElTreeItem* Item, int SectionIndex, Elheader::TElFieldType SupposedFieldType, Eltree::TElTreeInplaceEditor* &Editor);
	void __fastcall FileFiltersChange(System::TObject* Sender);
	void __fastcall OnValidateEdit(System::TObject* Sender, bool &InputValid);
	void __fastcall SetExpandRoot(bool Value);
	virtual int __fastcall DoGetPicture(Eltree::TElTreeItem* Item);
	__property Items = {stored=false};
	
public:
	void __fastcall SetRootPIDL(Winapi::Shlobj::PItemIDList PIDL);
	__fastcall virtual TElShellTree(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElShellTree();
	virtual void __fastcall Loaded();
	void __fastcall RefreshTree(Eltree::TElTreeItem* Item, int recursive);
	void __fastcall SetSelectionPIDL(Winapi::Shlobj::PItemIDList PIDL);
	Winapi::Shlobj::PItemIDList __fastcall BuildRootPIDL();
	__property Winapi::Shlobj::PItemIDList FocusedPIDL = {read=GetFocusedPIDL};
	__property System::UnicodeString FocusedDisplayName = {read=GetFocusedDisplayName};
	__property TElShellTreeItem* ItemFocused = {read=GetItemFocused, write=SetItemFocused};
	__property Images;
	__property Images2;
	
__published:
	__property Elshellutils::TShellFolders RootFolder = {read=GetRootFolder, write=SetRootFolder, nodefault};
	__property System::UnicodeString CustomRootFolder = {read=FCustomRootFolder, write=SetCustomRootFolder};
	__property bool UseSystemMenus = {read=FUseSystemMenus, write=FUseSystemMenus, nodefault};
	__property bool ClearOnCollapse = {read=FClearOnCollapse, write=FClearOnCollapse, default=1};
	__property bool CheckForChildren = {read=FCheckForChildren, write=FCheckForChildren, nodefault};
	__property bool ShowHidden = {read=FShowHidden, write=SetShowHidden, default=1};
	__property bool ShowFiles = {read=FShowFiles, write=SetShowFiles, default=0};
	__property bool HighlightCompressed = {read=FHighlightCompressed, write=SetHighlightCompressed, default=1};
	__property System::Classes::TStrings* FileFilters = {read=GetFileFilters, write=SetFileFilters};
	__property TShellTreeItemAddingEvent OnItemAdding = {read=FOnFilterItem, write=FOnFilterItem};
	__property TShellTreeItemAddedEvent OnItemAdded = {read=FOnItemAdded, write=FOnItemAdded};
	__property bool FileSystemOnly = {read=FFileSystemOnly, write=SetFileSystemOnly, nodefault};
	__property TElShellSortType SortType = {read=FSortType, write=SetSortType, default=1};
	__property TElShellSortModifiers SortModifiers = {read=FSortModifiers, write=SetSortModifiers, nodefault};
	__property TElShellSizeFormat SizeFormat = {read=FSizeFormat, write=SetSizeFormat, nodefault};
	__property bool DefaultColumns = {read=FDefaultColumns, write=SetDefaultColumns, nodefault};
	__property bool DefaultEditor = {read=FDefaultEditor, write=FDefaultEditor, default=1};
	__property bool ExpandRoot = {read=FExpandRoot, write=SetExpandRoot, default=0};
	__property ActiveBorderType = {default=1};
	__property Align = {default=0};
	__property AlwaysKeepFocus = {default=0};
	__property AlwaysKeepSelection = {default=1};
	__property AutoExpand = {default=0};
	__property AutoLineHeight = {default=1};
	__property AutoLookup = {default=0};
	__property AutoResizeColumns = {default=1};
	__property DefaultSectionWidth;
	__property AdjustMultilineHeight = {default=1};
	__property Background;
	__property BackgroundType = {default=2};
	__property BarStyle = {default=0};
	__property BarStyleVerticalLines = {default=0};
	__property BorderSides;
	__property ChangeDelay = {default=500};
	__property ChangeStateImage = {default=0};
	__property CheckBoxGlyph;
	__property CheckBoxSize = {default=15};
	__property CustomCheckboxes = {default=0};
	__property CustomPlusMinus = {default=0};
	__property DeselectChildrenOnCollapse = {default=0};
	__property DblClickMode = {default=1};
	__property DoInplaceEdit = {default=1};
	__property DragAllowed = {default=0};
	__property DragCursor;
	__property DragExpandDelay = {default=500};
	__property DraggableSections = {default=0};
	__property DrawFocusRect = {default=1};
	__property DragImageMode = {default=0};
	__property DragRectAcceptColor = {default=32768};
	__property DragRectDenyColor = {default=255};
	__property DragScrollInterval = {default=100};
	__property DragTrgDrawMode = {default=2};
	__property DragType = {default=1};
	__property ExpandOnDblClick = {default=1};
	__property ExpandOnDragOver = {default=0};
	__property ExplorerEditMode = {default=1};
	__property FilteredVisibility = {default=0};
	__property Flat = {default=0};
	__property FlatFocusedScrollbars = {default=1};
	__property FocusedSelectColor = {default=-16777203};
	__property FocusedSelectTextColor = {default=-16777202};
	__property ForcedScrollBars = {default=0};
	__property Font = {stored=true};
	__property FullRowSelect = {default=1};
	__property GradientStartColor = {default=0};
	__property GradientEndColor = {default=0};
	__property GradientSteps = {default=64};
	__property HeaderActiveFilterColor = {default=0};
	__property HeaderColor = {default=-16777201};
	__property HeaderHeight;
	__property HeaderHotTrack = {default=1};
	__property HeaderInvertSortArrows = {default=0};
	__property HeaderSections;
	__property HeaderFilterColor = {default=-16777198};
	__property HeaderFlat = {default=0};
	__property HeaderFont;
	__property HeaderUseTreeFont = {default=1};
	__property HeaderImages;
	__property HeaderWrapCaptions = {default=0};
	__property HideFocusRect = {default=0};
	__property HideHintOnTimer = {default=0};
	__property HideHintOnMove = {default=1};
	__property HideSelectColor = {default=-16777201};
	__property HideSelectTextColor = {default=-16777200};
	__property HideSelection = {default=0};
	__property HorizontalLines = {default=0};
	__property HideHorzScrollBar = {default=0};
	__property HideVertScrollBar = {default=0};
	__property HintType = {default=2};
	__property HorzDivLinesColor = {default=-16777201};
	__property HorzScrollBarStyles;
	__property HeaderImageForm;
	__property ImageForm;
	__property InactiveBorderType = {default=3};
	__property IncrementalSearch;
	__property InplaceEditorDelay = {default=500};
	__property ItemIndent = {default=17};
	__property LeafPicture;
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property LineHeight;
	__property LinesColor = {default=-16777201};
	__property LinesStyle = {default=2};
	__property LineHintColor = {default=-16777211};
	__property LineHintMode = {default=1};
	__property LineHintTimeout = {default=3000};
	__property LineHintType = {default=2};
	__property LockHeaderHeight = {default=0};
	__property MainTreeColumn = {default=0};
	__property MinusPicture;
	__property MoveColumnOnDrag = {default=0};
	__property MoveFocusOnCollapse = {default=0};
	__property MouseFrameSelect = {default=1};
	__property MultiSelect = {default=1};
	__property MultiSelectLevel = {default=-1};
	__property OwnerDrawByColumn = {default=1};
	__property OwnerDrawMask = {default=0};
	__property PathSeparator = {default=92};
	__property PlusMinusTransparent = {default=0};
	__property PlusPicture;
	__property QuickEditMode = {default=0};
	__property RadioButtonGlyph;
	__property RightAlignedText = {default=0};
	__property RightAlignedTree = {default=0};
	__property RightClickSelect = {default=1};
	__property RowHotTrack = {default=0};
	__property RowSelect = {default=1};
	__property ScrollbarOpposite;
	__property ScrollTracking = {default=0};
	__property SelectColumn = {default=-1};
	__property ShowButtons = {default=1};
	__property ShowColumns = {default=0};
	__property ShowCheckboxes = {default=0};
	__property ShowEmptyImages = {default=0};
	__property ShowEmptyImages2 = {default=0};
	__property ShowHint;
	__property ShowImages = {default=1};
	__property ShowLeafButton;
	__property ShowLines = {default=1};
	__property ShowRoot = {default=0};
	__property ShowRootButtons = {default=1};
	__property SelectionMode = {default=1};
	__property SortDir = {default=0};
	__property SortMode = {default=0};
	__property SortUseCase = {default=1};
	__property Storage;
	__property StoragePath = {default=0};
	__property StickyHeaderSections = {default=0};
	__property StripedOddColor;
	__property StripedEvenColor;
	__property StripedItems = {default=0};
	__property Tracking = {default=1};
	__property TrackColor = {default=-16777203};
	__property UnderlineTracked = {default=1};
	__property UseCustomScrollBars = {default=1};
	__property VertDivLinesColor = {default=-16777201};
	__property VerticalLines = {default=0};
	__property VerticalLinesLong = {default=1};
	__property VertScrollBarStyles;
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
	__property OnTryEdit;
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
	__property OnMeasureItemPart;
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
	__property OnVirtualTextNeeded;
	__property OnVirtualHintNeeded;
	__property OnVirtualValueNeeded;
	__property OnVirtualStyleNeeded;
	__property OnHeaderMouseDown;
	__property OnOleTargetDrag;
	__property OnOleTargetDrop;
	__property OnOleDragStart;
	__property OnOleDragFinish;
	__property BorderStyle = {default=1};
	__property Ctl3D;
	__property Cursor = {default=-2};
	__property Enabled = {default=1};
	__property Hint;
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property Visible = {default=1};
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
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
	__property OnResize;
public:
	/* TCustomElTree.CreateClass */ inline __fastcall TElShellTree(System::Classes::TComponent* AOwner, Eltree::TElTreeItemClass ItemClass) : Eltree::TCustomElTree(AOwner, ItemClass) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElShellTree(HWND ParentWindow) : Eltree::TCustomElTree(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElShellComboBox : public Elactrls::TElAdvancedComboBox
{
	typedef Elactrls::TElAdvancedComboBox inherited;
	
protected:
	bool FNoRebuild;
	_ITEMIDLIST *FSelectionPIDL;
	bool FExplorerStyle;
	bool FShowHidden;
	bool FFileSystemOnly;
	int FDummyInt;
	Vcl::Stdctrls::TCustomEdit* FEditor;
	System::Uitypes::TCursor FCursor;
	Vcl::Stdctrls::TComboBoxStyle FStyle;
	void __fastcall SetExplorerStyle(bool Value);
	void __fastcall FillCombo(_di_IShellFolder BaseFolder, Winapi::Shlobj::PItemIDList BasePIDL, int Level);
	virtual void __fastcall CreateWnd();
	void __fastcall SetShowHidden(bool Value);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual int __fastcall GetItemWidth(int Index);
	void __fastcall SetFileSystemOnly(bool Value);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Msg);
	HIDESBASE MESSAGE void __fastcall CNDrawItem(Winapi::Messages::TWMDrawItem &Msg);
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	Winapi::Shlobj::PItemIDList __fastcall GetSelection();
	void __fastcall ShowEdit();
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	DYNAMIC void __fastcall DropDown();
	HIDESBASE MESSAGE void __fastcall WMDeleteItem(Winapi::Messages::TMessage &Message);
	void __fastcall AcceptEdit();
	void __fastcall CancelEdit();
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE void __fastcall SetCursor(System::Uitypes::TCursor Value);
	void __fastcall FillItems();
	HIDESBASE void __fastcall SetStyle(Vcl::Stdctrls::TComboBoxStyle Value);
	virtual void __fastcall DestroyWnd();
	
public:
	__fastcall virtual ~TElShellComboBox();
	__fastcall virtual TElShellComboBox(System::Classes::TComponent* AOwner);
	void __fastcall SetSelection(Winapi::Shlobj::PItemIDList PIDL);
	__property Winapi::Shlobj::PItemIDList Selection = {read=GetSelection, write=SetSelection};
	
__published:
	__property int Items = {read=FDummyInt, nodefault};
	__property int ItemHeight = {read=FDummyInt, nodefault};
	__property bool ExplorerStyle = {read=FExplorerStyle, write=SetExplorerStyle, default=1};
	__property bool ShowHidden = {read=FShowHidden, write=SetShowHidden, default=1};
	__property bool FileSystemOnly = {read=FFileSystemOnly, write=SetFileSystemOnly, nodefault};
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	__property Vcl::Stdctrls::TComboBoxStyle Style = {read=FStyle, write=SetStyle, stored=false, default=4};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElShellComboBox(HWND ParentWindow) : Elactrls::TElAdvancedComboBox(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElShellListItem : public Eltree::TElTreeItem
{
	typedef Eltree::TElTreeItem inherited;
	
private:
	bool FIsValidFile;
	bool FValid;
	
protected:
	int FAttr;
	System::UnicodeString FAttrAsString;
	System::UnicodeString FComment;
	System::UnicodeString FDisplayName;
	System::UnicodeString FFileName;
	_ITEMIDLIST *FPIDL;
	System::UnicodeString FSizeAsString;
	System::UnicodeString FTypeName;
	_WIN32_FIND_DATAW *Win32FindData;
	System::UnicodeString __fastcall GetAttrAsString();
	bool __fastcall GetCanRename();
	System::UnicodeString __fastcall GetComment();
	System::TDateTime __fastcall GetCreationTime();
	System::UnicodeString __fastcall GetDisplayName();
	System::UnicodeString __fastcall GetFileName();
	HIDESBASE System::UnicodeString __fastcall GetFullName();
	bool __fastcall GetIsFileObject();
	bool __fastcall GetIsFolder();
	bool __fastcall GetIsRemovable();
	System::TDateTime __fastcall GetLastAccessTime();
	System::TDateTime __fastcall GetModificationTime();
	Winapi::Shlobj::PItemIDList __fastcall GetPIDL();
	unsigned __fastcall GetSize();
	System::UnicodeString __fastcall GetSizeAsString();
	System::UnicodeString __fastcall GetTypeName();
	void __fastcall SetDisplayName(const System::UnicodeString Value);
	void __fastcall CheckWin32FindData();
	void __fastcall GetWin32Data(_di_IShellFolder ParentFolder);
	void __fastcall Invalidate();
	void __fastcall GetAttributes(_di_IShellFolder iParentFolder);
	int __fastcall GetPicture(_di_IShellFolder ParentFolder);
	
public:
	__fastcall virtual TElShellListItem(Eltree::TCustomElTree* AOwner);
	__fastcall virtual ~TElShellListItem();
	Winapi::Shlobj::PItemIDList __fastcall BuildFullPIDL();
	__property int Attr = {read=FAttr, nodefault};
	__property bool CanRename = {read=GetCanRename, nodefault};
	__property System::UnicodeString Comment = {read=GetComment};
	__property System::TDateTime CreationTime = {read=GetCreationTime};
	__property System::UnicodeString DisplayName = {read=GetDisplayName, write=SetDisplayName};
	__property System::UnicodeString FileName = {read=GetFileName};
	__property System::UnicodeString FullName = {read=GetFullName};
	__property bool IsFileObject = {read=GetIsFileObject, nodefault};
	__property bool IsFolder = {read=GetIsFolder, nodefault};
	__property bool IsRemovable = {read=GetIsRemovable, nodefault};
	__property System::TDateTime LastAccessTime = {read=GetLastAccessTime};
	__property System::TDateTime ModificationTime = {read=GetModificationTime};
	__property Winapi::Shlobj::PItemIDList PIDL = {read=GetPIDL};
	__property System::UnicodeString SizeAsString = {read=GetSizeAsString};
	
__published:
	__property System::UnicodeString AttrAsString = {read=GetAttrAsString};
	__property unsigned Size = {read=GetSize, nodefault};
	__property System::UnicodeString TypeName = {read=GetTypeName};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TShellListItemAddingEvent)(System::TObject* Sender, System::UnicodeString ItemName, _di_IShellFolder ShellFolder, Winapi::Shlobj::PItemIDList RelPIDL, bool &Allowed);

typedef void __fastcall (__closure *TShellListItemAddedEvent)(System::TObject* Sender, System::UnicodeString ItemName, _di_IShellFolder ShellFolder, Winapi::Shlobj::PItemIDList RelPIDL, TElShellListItem* Item);

class PASCALIMPLEMENTATION TElShellList : public Eltree::TCustomElTree
{
	typedef Eltree::TCustomElTree inherited;
	
private:
	int FAttr;
	
protected:
	Eltreestdeditors::TElTreeInplaceEdit* FEditor;
	System::UnicodeString FCustomFolder;
	bool FDefaultColumns;
	_ITEMIDLIST *FFocusedPIDL;
	bool FHighlightCompressed;
	Elshellutils::TShellFolders FFolder;
	bool FShowHidden;
	TElShellSizeFormat FSizeFormat;
	TElShellSortModifiers FSortModifiers;
	TElShellSortType FSortType;
	bool FUseSystemMenus;
	_di_IShellFolder FIFolder;
	int FMaxColumns;
	bool FFileSystemOnly;
	System::Classes::TStringList* FFileFilters;
	TShellListItemAddingEvent FOnFilterItem;
	TShellListItemAddedEvent FOnItemAdded;
	bool FDefaultEditor;
	_ITEMIDLIST *FRootPIDL;
	void __fastcall SetCustomFolder(const System::UnicodeString Value);
	void __fastcall SetDefaultColumns(bool Value);
	System::UnicodeString __fastcall GetFocusedDisplayName();
	Winapi::Shlobj::PItemIDList __fastcall GetFocusedPIDL();
	TElShellListItem* __fastcall GetItemFocused();
	void __fastcall SetHighlightCompressed(bool Value);
	void __fastcall SetItemFocused(TElShellListItem* Value);
	void __fastcall SetFolder(Elshellutils::TShellFolders Value);
	void __fastcall SetShowHidden(bool Value);
	void __fastcall SetSizeFormat(TElShellSizeFormat Value);
	void __fastcall SetSortModifiers(TElShellSortModifiers Value);
	void __fastcall SetSortType(TElShellSortType Value);
	void __fastcall SetPIDL(Winapi::Shlobj::PItemIDList PIDL);
	void __fastcall AddDefaultColumns();
	int __fastcall DeleteDefaultColumns();
	virtual void __fastcall TriggerSortBegin();
	virtual void __fastcall TriggerTryEditEvent(Eltree::TElTreeItem* Item, int SectionIndex, Elheader::TElFieldType &CellType, bool &CanEdit);
	virtual void __fastcall TriggerVirtualTextNeeded(Eltree::TElTreeItem* Item, int SectionIndex, WideString &Text);
	virtual void __fastcall TriggerVirtualValueNeeded(Eltree::TElTreeItem* Item, int SectionIndex, int VarType, System::Variant &Value);
	void __fastcall SetFileSystemOnly(bool Value);
	TElShellListItem* __fastcall FindItemByPIDL(Winapi::Shlobj::PItemIDList APIDL);
	virtual bool __fastcall NameFiltered(System::UnicodeString S, _di_IShellFolder ShellFolder, Winapi::Shlobj::PItemIDList RelPIDL);
	System::Classes::TStrings* __fastcall GetFileFilters();
	void __fastcall SetFileFilters(System::Classes::TStrings* const Value);
	void __fastcall FileFiltersChange(System::TObject* Sender);
	virtual void __fastcall CreateHandle();
	virtual Eltree::TElTreeItems* __fastcall CreateItems();
	virtual Eltree::TElTreeView* __fastcall CreateView();
	virtual void __fastcall DoCompareItems(Eltree::TElTreeItem* Item1, Eltree::TElTreeItem* Item2, int &res);
	virtual int __fastcall DoGetPicture(Eltree::TElTreeItem* Item);
	virtual void __fastcall DoItemAdded(System::UnicodeString S, _di_IShellFolder ShellFolder, Winapi::Shlobj::PItemIDList RelPIDL, TElShellListItem* Item);
	__property Items = {stored=false};
	
public:
	void __fastcall RefreshList();
	__fastcall virtual TElShellList(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElShellList();
	void __fastcall SetRootPIDL(Winapi::Shlobj::PItemIDList PIDL);
	void __fastcall SetSelectionPIDL(Winapi::Shlobj::PItemIDList PIDL);
	Winapi::Shlobj::PItemIDList __fastcall BuildRootPIDL();
	virtual void __fastcall Loaded();
	__property System::UnicodeString FocusedDisplayName = {read=GetFocusedDisplayName};
	__property Winapi::Shlobj::PItemIDList FocusedPIDL = {read=GetFocusedPIDL};
	__property TElShellListItem* ItemFocused = {read=GetItemFocused, write=SetItemFocused};
	
__published:
	__property System::UnicodeString CustomFolder = {read=FCustomFolder, write=SetCustomFolder};
	__property bool DefaultColumns = {read=FDefaultColumns, write=SetDefaultColumns, nodefault};
	__property bool HighlightCompressed = {read=FHighlightCompressed, write=SetHighlightCompressed, default=1};
	__property Elshellutils::TShellFolders Folder = {read=FFolder, write=SetFolder, default=1};
	__property bool ShowHidden = {read=FShowHidden, write=SetShowHidden, default=1};
	__property TElShellSizeFormat SizeFormat = {read=FSizeFormat, write=SetSizeFormat, nodefault};
	__property TElShellSortModifiers SortModifiers = {read=FSortModifiers, write=SetSortModifiers, nodefault};
	__property TElShellSortType SortType = {read=FSortType, write=SetSortType, default=1};
	__property bool UseSystemMenus = {read=FUseSystemMenus, write=FUseSystemMenus, nodefault};
	__property bool FileSystemOnly = {read=FFileSystemOnly, write=SetFileSystemOnly, nodefault};
	__property System::Classes::TStrings* FileFilters = {read=GetFileFilters, write=SetFileFilters};
	__property TShellListItemAddedEvent OnItemAdded = {read=FOnItemAdded, write=FOnItemAdded};
	__property TShellListItemAddingEvent OnItemAdding = {read=FOnFilterItem, write=FOnFilterItem};
	__property bool DefaultEditor = {read=FDefaultEditor, write=FDefaultEditor, default=1};
	__property ActiveBorderType = {default=1};
	__property Align = {default=0};
	__property AlwaysKeepFocus = {default=0};
	__property AlwaysKeepSelection = {default=1};
	__property AutoExpand = {default=0};
	__property AutoLineHeight = {default=1};
	__property AutoLookup = {default=0};
	__property AutoResizeColumns = {default=1};
	__property DefaultSectionWidth;
	__property AdjustMultilineHeight = {default=1};
	__property Background;
	__property BackgroundType = {default=2};
	__property BarStyle = {default=0};
	__property BarStyleVerticalLines = {default=0};
	__property BorderSides;
	__property ChangeDelay = {default=500};
	__property ChangeStateImage = {default=0};
	__property CheckBoxGlyph;
	__property CheckBoxSize = {default=15};
	__property CustomCheckboxes = {default=0};
	__property CustomPlusMinus = {default=0};
	__property DeselectChildrenOnCollapse = {default=0};
	__property DblClickMode = {default=1};
	__property DoInplaceEdit = {default=1};
	__property DragAllowed = {default=0};
	__property DragCursor;
	__property DragExpandDelay = {default=500};
	__property DraggableSections = {default=0};
	__property DrawFocusRect = {default=1};
	__property DragImageMode = {default=0};
	__property DragRectAcceptColor = {default=32768};
	__property DragRectDenyColor = {default=255};
	__property DragScrollInterval = {default=100};
	__property DragTrgDrawMode = {default=2};
	__property DragType = {default=1};
	__property ExpandOnDblClick = {default=1};
	__property ExpandOnDragOver = {default=0};
	__property ExplorerEditMode = {default=1};
	__property FilteredVisibility = {default=0};
	__property Flat = {default=0};
	__property FlatFocusedScrollbars = {default=1};
	__property FocusedSelectColor = {default=-16777203};
	__property FocusedSelectTextColor = {default=-16777202};
	__property ForcedScrollBars = {default=0};
	__property Font = {stored=true};
	__property FullRowSelect = {default=1};
	__property GradientStartColor = {default=0};
	__property GradientEndColor = {default=0};
	__property GradientSteps = {default=64};
	__property HeaderActiveFilterColor = {default=0};
	__property HeaderColor = {default=-16777201};
	__property HeaderHeight;
	__property HeaderHotTrack = {default=1};
	__property HeaderInvertSortArrows = {default=0};
	__property HeaderSections;
	__property HeaderFilterColor = {default=-16777198};
	__property HeaderFlat = {default=0};
	__property HeaderFont;
	__property HeaderUseTreeFont = {default=1};
	__property HeaderImages;
	__property HeaderWrapCaptions = {default=0};
	__property HideFocusRect = {default=0};
	__property HideHintOnTimer = {default=0};
	__property HideHintOnMove = {default=1};
	__property HideSelectColor = {default=-16777201};
	__property HideSelectTextColor = {default=-16777200};
	__property HideSelection = {default=0};
	__property HorizontalLines = {default=0};
	__property HideHorzScrollBar = {default=0};
	__property HideVertScrollBar = {default=0};
	__property HintType = {default=2};
	__property HorzDivLinesColor = {default=-16777201};
	__property HorzScrollBarStyles;
	__property HeaderImageForm;
	__property ImageForm;
	__property InactiveBorderType = {default=3};
	__property IncrementalSearch;
	__property InplaceEditorDelay = {default=500};
	__property ItemIndent = {default=17};
	__property LeafPicture;
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property LineHeight;
	__property LinesColor = {default=-16777201};
	__property LinesStyle = {default=2};
	__property LineHintColor = {default=-16777211};
	__property LineHintMode = {default=1};
	__property LineHintTimeout = {default=3000};
	__property LineHintType = {default=2};
	__property LockHeaderHeight = {default=0};
	__property MainTreeColumn = {default=0};
	__property MinusPicture;
	__property MoveColumnOnDrag = {default=0};
	__property MoveFocusOnCollapse = {default=0};
	__property MouseFrameSelect = {default=1};
	__property MultiSelect = {default=1};
	__property MultiSelectLevel = {default=-1};
	__property OwnerDrawByColumn = {default=1};
	__property OwnerDrawMask = {default=0};
	__property PathSeparator = {default=92};
	__property PlusMinusTransparent = {default=0};
	__property PlusPicture;
	__property QuickEditMode = {default=0};
	__property RadioButtonGlyph;
	__property RightAlignedText = {default=0};
	__property RightAlignedTree = {default=0};
	__property RightClickSelect = {default=1};
	__property RowHotTrack = {default=0};
	__property RowSelect = {default=1};
	__property ScrollbarOpposite;
	__property ScrollTracking = {default=0};
	__property SelectColumn = {default=-1};
	__property ShowButtons = {default=1};
	__property ShowColumns = {default=0};
	__property ShowCheckboxes = {default=0};
	__property ShowEmptyImages = {default=1};
	__property ShowEmptyImages2 = {default=0};
	__property ShowHint;
	__property ShowImages = {default=1};
	__property ShowLeafButton;
	__property ShowLines = {default=0};
	__property ShowRoot = {default=0};
	__property ShowRootButtons = {default=0};
	__property SelectionMode = {default=1};
	__property SortDir = {default=0};
	__property SortMode = {default=0};
	__property Storage;
	__property StoragePath = {default=0};
	__property StickyHeaderSections = {default=0};
	__property StripedOddColor;
	__property StripedEvenColor;
	__property StripedItems = {default=0};
	__property Tracking = {default=1};
	__property TrackColor = {default=-16777203};
	__property UnderlineTracked = {default=1};
	__property UseCustomScrollBars = {default=1};
	__property VertDivLinesColor = {default=-16777201};
	__property VerticalLines = {default=0};
	__property VerticalLinesLong = {default=1};
	__property VertScrollBarStyles;
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
	__property OnMeasureItemPart;
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
	__property OnVirtualTextNeeded;
	__property OnVirtualHintNeeded;
	__property OnVirtualValueNeeded;
	__property OnVirtualStyleNeeded;
	__property OnHeaderMouseDown;
	__property OnOleTargetDrag;
	__property OnOleTargetDrop;
	__property OnOleDragStart;
	__property OnOleDragFinish;
	__property BorderStyle = {default=1};
	__property Ctl3D;
	__property Cursor = {default=-2};
	__property Enabled = {default=1};
	__property Hint;
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property Visible = {default=1};
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
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
	__property OnResize;
public:
	/* TCustomElTree.CreateClass */ inline __fastcall TElShellList(System::Classes::TComponent* AOwner, Eltree::TElTreeItemClass ItemClass) : Eltree::TCustomElTree(AOwner, ItemClass) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElShellList(HWND ParentWindow) : Eltree::TCustomElTree(ParentWindow) { }
	
};


typedef System::StaticArray<System::UnicodeString, 8> Elshellctl__7;

//-- var, const, procedure ---------------------------------------------------
static const System::Word siBase = System::Word(0x5b2);
static const System::Int8 siMin = System::Int8(0x0);
static const System::Int8 siName = System::Int8(0x0);
static const System::Int8 siSize = System::Int8(0x1);
static const System::Int8 siType = System::Int8(0x2);
static const System::Int8 siModified = System::Int8(0x3);
static const System::Int8 siAttr = System::Int8(0x4);
static const System::Int8 siComment = System::Int8(0x5);
static const System::Int8 siCreated = System::Int8(0x6);
static const System::Int8 siAccessed = System::Int8(0x7);
static const System::Int8 siMax = System::Int8(0x7);
extern DELPHI_PACKAGE Elshellctl__7 DefaultColumnNames;
extern DELPHI_PACKAGE System::StaticArray<int, 8> DefaultColumnAlignments;
extern DELPHI_PACKAGE System::StaticArray<TElShellSortType, 8> ColumnSortTypes;
}	/* namespace Elshellctl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSHELLCTL)
using namespace Elshellctl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElshellctlHPP
