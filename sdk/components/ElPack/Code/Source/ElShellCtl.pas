{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
{$include elpack2.inc}
{$ifdef ELPACK_SINGLECOMP}
{$I ElPack.inc}
{$else}
{$ifdef LINUX}
{$I ../ElPack.inc}
{$else}
{$I ..\ElPack.inc}
{$endif}
{$endif}

(*

Version History

05/02/2002

  Fixed rebuilding of the tree and list when the component is loaded
  OnItemAdded is now correctly fired for all items
  Columns in list are now built correctly 

04/04/2002

  Improved speed of list building
  Fixed the problem with images

02/14/2002

  Solved the problem with AV happening when the combo box is recreated
  (RecreateWnd is used). 

12/24/2001

  Added ElShellList.BuildRootPIDL to access current root pidl
  Made ElShellListItem.BuildFullPIDL and ElShellTreeItem.BuildFullPIDL methods public
  Fixed calls to SetRootPIDL with Empty PIDL (with size of 2) as parameter

11/21/2001

  SystemMenu is shown when you press VK_APPS (AKA Context Menu) key.
  Fixed the problem with 32-bit icons in Windows XP. 

11/20/2001

  Fixed GetFocusedDisplayName methods

11/15/2001

  Fixed system menus that worked incorrectly
  ExpandRoot property added. It defines whether the root item is expanded. 

11/14/2001

  OnTryEdit property made published

*)

unit ElShellCtl;


interface

uses

  Controls,
  Menus,
  Classes,
  SysUtils,
  Messages,
  CommCtrl,
  Windows,
  ShellAPI,
  ShlObj,
  ComObj,
  ActiveX,
  Forms,
  Graphics,
  {$ifdef VCL_4_USED}
  ImgList,
  {$endif}
  Variants,
  StdCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}

  ElACtrls,
  ElTreeStdEditors,
  ElShellUtils,
  ElStrUtils,
  ElTools,
  ElList,
  ElHeader,
{$ifdef ELPACK_NO_ADV_EDITS}
  ElEdits,
{$endif}
  ElTree;

type

  TElShellSortType = (sstCustom, sstName, sstExt, sstSize, sstCreationDate, sstModifyDate, sstAccessDate);

  TElShellSortModifier = (ssmFoldersFirst, ssmExecutablesFirst);
  TElShellSortModifiers = set of TElShellSortModifier;

  TElShellSizeFormat = (ssfAsIs, ssfKb, ssfAuto);

  EElShellError = class(Exception);

  TElShellTreeItem = class(TElTreeItem)
  private
    FIsValidFile: Boolean;
  protected
    FValid : boolean;
    FAttrAsString: string;
    FComment: string;
    FSizeAsString: string;
    FDisplayName: string;
    FTypeName : string;
    FFileName : string;
    FAttr: Cardinal;
    FPIDL: PItemIDList;
    Win32FindData: PWin32FindData;
    procedure GetAttributes(iParentFolder : IShellFolder);
    function GetDisplayName: string;
    function GetFullName: string;
    function GetHasSubFolders: Boolean;
    function GetIsFolder: Boolean;
    function GetIsRemovable: Boolean;
    function GetParentFolder: IShellFolder;
    function GetPIDL: PItemIDList;
    function GetSize: Cardinal;
    procedure SetDisplayName(const Value: string);
    function GetCreationTime: TDateTime;
    function GetModificationTime: TDateTime;
    function GetLastAccessTime: TDateTime;
    procedure GetWin32Data(ParentFolder : IShellFolder);
    procedure CheckWin32FindData;
    function GetFileName: string;
    function GetSizeAsString: string;
    function GetTypeName: string;
    function GetIsFileObject: Boolean;
    function GetAttrAsString: string;
    function GetComment: string;
    procedure Invalidate;
    function FindItemByPIDL(APIDL : PItemIDList): TElShellTreeItem;
    function GetCanRename: Boolean;
    {$ifdef VCL_4_USED}
    function GetHintText(ParentFolder: IShellFolder): TElFString;
    {$endif}
    function GetPicture(ParentFolder : IShellFolder): Integer;
  public
    constructor Create(AOwner : TCustomElTree); override;
    destructor Destroy; override;
    function BuildFullPIDL: PItemIDList;

    property Attr: Cardinal read FAttr;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property FullName: string read GetFullName;
    property HasSubFolders: Boolean read GetHasSubFolders;
    property IsFolder: Boolean read GetIsFolder;
    property IsRemovable: Boolean read GetIsRemovable;
    property ParentFolder: IShellFolder read GetParentFolder;
    property PIDL: PItemIDList read GetPIDL;
    property ModificationTime: TDateTime read GetModificationTime;
    property CreationTime: TDateTime read GetCreationTime;
    property LastAccessTime: TDateTime read GetLastAccessTime;
    property FileName: string read GetFileName;
    property SizeAsString: string read GetSizeAsString;
    property IsFileObject: Boolean read GetIsFileObject;
    property Comment: string read GetComment;
    property CanRename: Boolean read GetCanRename;
  published
    property Size: Cardinal read GetSize;
    property TypeName: string read GetTypeName;
    property AttrAsString: string read GetAttrAsString;
  end;

  TShellTreeItemAddingEvent = procedure(Sender : TObject; ItemName : String;
                              ShellFolder : IShellFolder; RelPIDL : PItemIDList;
                              var Allowed : boolean) of object;
  TShellTreeItemAddedEvent  = procedure(Sender : TObject; ItemName : String;
                              ShellFolder : IShellFolder; RelPIDL : PItemIDList;
                              Item : TElShellTreeItem) of object;

  TElShellTree = class(TCustomElTree)
  protected
    FEditor: TElTreeInplaceEdit;
    FRootFolder : TShellFolders;
    FRootPIDL   : PItemIDList;
    FFocusedPIDL: PItemIDList;
    FIFolder    : IShellFolder;
    FCustomRootFolder: string;
    FUseSystemMenus: Boolean;
    FClearOnCollapse: Boolean;
    FCheckForChildren: Boolean;
    FShowHidden: Boolean;
    FShowFiles: Boolean;
    FHighlightCompressed: Boolean;
    FFileFilters: TStringList;
    FOnFilterItem: TShellTreeItemAddingEvent;
    FOnItemAdded: TShellTreeItemAddedEvent;
    FFileSystemOnly: Boolean;
    FSortType: TElShellSortType;
    FSortModifiers: TElShellSortModifiers;
    FSizeFormat: TElShellSizeFormat;
    FDefaultColumns: Boolean;
    FDefaultEditor: Boolean;
    FMaxColumns   : integer;
    FExpandRoot: Boolean;
    function GetFocusedPIDL: PItemIDList;
    function GetFocusedDisplayName: string;
    procedure BuildTree;
    procedure ReleaseFocusedPIDL;
    procedure DoItemFocused; override;
    procedure SetCustomRootFolder(const Value: string);
    function CreateView: TElTreeView; override;
    function CreateItems: TElTreeItems; override;
    function CheckChildren(Item : TElTreeItem; AFolder : IShellFolder): Boolean;
    procedure FillItemWithData(Item : TElShellTreeItem; AFolder : IShellFolder;
        recursive : integer);
    procedure SetShowHidden(Value: Boolean);
    procedure SetShowFiles(Value: Boolean);
    procedure SetHighlightCompressed(Value: Boolean);
    function GetFileFilters: TStrings;
    procedure SetFileFilters(const Value: TStrings);
    function NameFiltered(S : String; ShellFolder : IShellFolder; RelPIDL : PItemIDList): Boolean; virtual;
    procedure CreateHandle; override;
    procedure DoItemCollapse(Item: TElTreeItem); override;
    procedure DoItemExpand(Item: TElTreeItem); override;
    procedure DoItemExpanding(Item: TElTreeItem; var CanProcess: boolean); override;
    function GetRootFolder: TShellFolders;
    procedure SetRootFolder(Value: TShellFolders);
    procedure DoItemAdded(S : String; ShellFolder : IShellFolder; RelPIDL :
        PItemIDList; Item : TElShellTreeItem); virtual;
    procedure SetFileSystemOnly(Value: Boolean);
    function GetItemFocused: TElShellTreeItem;
    procedure SetItemFocused(Value: TElShellTreeItem);
    procedure DoCompareItems(Item1, Item2: TElTreeItem; var res: integer); override;
    procedure SetSortType(Value: TElShellSortType);
    procedure SetSortModifiers(Value: TElShellSortModifiers);
    procedure SetSizeFormat(Value: TElShellSizeFormat);
    procedure SetDefaultColumns(Value: Boolean);
    procedure AddDefaultColumns;
    function DeleteDefaultColumns: Integer;
    procedure TriggerVirtualTextNeeded(Item : TElTreeItem; SectionIndex : Integer;
        var Text : TElFString); override;
    procedure TriggerVirtualValueNeeded(Item : TElTreeItem; SectionIndex : Integer;
        VarType : integer; var Value : Variant); override;
    procedure TriggerSortBegin; override;
    procedure TriggerTryEditEvent(Item: TElTreeItem; SectionIndex : integer;
      var CellType: TElFieldType; var CanEdit: boolean); override;
    procedure TriggerInplaceEditorNeeded(Item : TElTreeItem; SectionIndex : Integer;
        SupposedFieldType : TElFieldType; var Editor : TElTreeInplaceEditor); override;
    procedure FileFiltersChange(Sender : TObject);
    procedure OnValidateEdit(Sender : TOBject; var InputValid : boolean);
    procedure SetExpandRoot(Value: Boolean);
    {$ifdef VCL_4_USED}
    procedure TriggerVirtualHintNeeded(Item : TElTreeItem; var Hint :
      TElFString); override;
    {$endif}
    function DoGetPicture(Item: TElTreeItem): integer; override;

    property Items stored false; 
  public
    procedure SetRootPIDL(PIDL : PItemIDList);
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure RefreshTree(Item : TElTreeItem; recursive : Integer);
    procedure SetSelectionPIDL(PIDL : PItemIDList);
    function BuildRootPIDL: PItemIDList;

    property FocusedPIDL: PItemIDList read GetFocusedPIDL;
    property FocusedDisplayName: string read GetFocusedDisplayName;
    property ItemFocused: TElShellTreeItem read GetItemFocused write SetItemFocused;

    property Images;
    property Images2;
  published
    property RootFolder: TShellFolders read GetRootFolder write SetRootFolder;
    property CustomRootFolder: string read FCustomRootFolder write
        SetCustomRootFolder;
    property UseSystemMenus: Boolean read FUseSystemMenus write FUseSystemMenus;
    property ClearOnCollapse: Boolean read FClearOnCollapse write FClearOnCollapse default true;
    property CheckForChildren: Boolean read FCheckForChildren write
        FCheckForChildren;
    property ShowHidden: Boolean read FShowHidden write SetShowHidden default true;
    property ShowFiles: Boolean read FShowFiles write SetShowFiles default false;
    property HighlightCompressed: Boolean read FHighlightCompressed write
        SetHighlightCompressed default true;
    property FileFilters: TStrings read GetFileFilters write SetFileFilters;
    property OnItemAdding: TShellTreeItemAddingEvent read FOnFilterItem write
        FOnFilterItem;
    property OnItemAdded: TShellTreeItemAddedEvent read FOnItemAdded write
        FOnItemAdded;
    property FileSystemOnly: Boolean read FFileSystemOnly write SetFileSystemOnly;
    property SortType: TElShellSortType read FSortType write SetSortType default
        sstName;
    property SortModifiers: TElShellSortModifiers read FSortModifiers write
        SetSortModifiers;
    property SizeFormat: TElShellSizeFormat read FSizeFormat write SetSizeFormat;
    property DefaultColumns: Boolean read FDefaultColumns write SetDefaultColumns;
    property DefaultEditor: Boolean read FDefaultEditor write FDefaultEditor
        default True;
    property ExpandRoot: Boolean read FExpandRoot write SetExpandRoot default false;

    // inherited properties
    property ActiveBorderType;
    property Align;
    property AlwaysKeepFocus;
    property AlwaysKeepSelection;
    property AutoExpand;
    property AutoLineHeight;
    property AutoLookup;
    property AutoResizeColumns;

    property DefaultSectionWidth;
    property AdjustMultilineHeight;
    property Background;
    property BackgroundType;

    property BarStyle;
    property BarStyleVerticalLines;
    property BorderSides;
    property ChangeDelay;
    property ChangeStateImage;
    property CheckBoxGlyph;
    property CheckBoxSize;
    property CustomCheckboxes;
    property CustomPlusMinus;
    property DeselectChildrenOnCollapse;

    property DblClickMode;
    property DoInplaceEdit;
    property DragAllowed;
    property DragCursor;
    property DragExpandDelay;
    property DraggableSections;
    property DrawFocusRect;
    property DragImageMode;
    property DragRectAcceptColor;
    property DragRectDenyColor;
    property DragScrollInterval;
    property DragTrgDrawMode;
    property DragType;

    property ExpandOnDblClick;
    property ExpandOnDragOver;
    property ExplorerEditMode default true;
    property FilteredVisibility;
    property Flat;
    property FlatFocusedScrollbars;
    property FocusedSelectColor;
    property FocusedSelectTextColor;
    property ForcedScrollBars;
    property Font stored true;
    property FullRowSelect;
    property GradientStartColor;
    property GradientEndColor;
    property GradientSteps;

    property HeaderActiveFilterColor;
    property HeaderColor;
    property HeaderHeight;
    property HeaderHotTrack;
    property HeaderInvertSortArrows;
    property HeaderSections;
    property HeaderFilterColor;
    property HeaderFlat;
    property HeaderFont;
    property HeaderUseTreeFont;
    property HeaderImages;
    property HeaderWrapCaptions;
    property HideFocusRect;
    property HideHintOnTimer;
    property HideHintOnMove;
    property HideSelectColor;
    property HideSelectTextColor;
    property HideSelection;
    property HorizontalLines;
    property HideHorzScrollBar;
    property HideVertScrollBar;
    property HintType;
    property HorzDivLinesColor;
    property HorzScrollBarStyles;
    {$ifdef ELPACK_COMPLETE}
    property HeaderImageForm;
    property ImageForm;
    {$endif}
    property InactiveBorderType;
    property IncrementalSearch;
    property InplaceEditorDelay;
    property ItemIndent;
    property LeafPicture;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;

    property LineHeight;
    property LinesColor;
    property LinesStyle;
    property LineHintColor;
    property LineHintMode default shmLong;
    property LineHintTimeout;
    property LineHintType;
    property LockHeaderHeight;
    property MainTreeColumn;
    property MinusPicture;
    property MoveColumnOnDrag;
    property MoveFocusOnCollapse;
    property MouseFrameSelect;
    property MultiSelect;
    property MultiSelectLevel;
    property OwnerDrawByColumn default true;
    property OwnerDrawMask;
    property PathSeparator;
    property PlusMinusTransparent;
    property PlusPicture;
    property QuickEditMode;

    property RadioButtonGlyph;
    property RightAlignedText;
    property RightAlignedTree;
    property RightClickSelect;
    property RowHotTrack;
    property RowSelect;

    property ScrollbarOpposite;
    property ScrollTracking;
    property SelectColumn;
    property ShowButtons;
    property ShowColumns;
    property ShowCheckboxes;
    property ShowEmptyImages;
    property ShowEmptyImages2;
    property ShowHint;
    property ShowImages;
    property ShowLeafButton;
    property ShowLines;
    property ShowRoot;
    property ShowRootButtons;
    property SelectionMode;
    property SortDir;
    property SortMode;
    property SortUseCase;
    property Storage;
    property StoragePath;
    property StickyHeaderSections;
    property StripedOddColor;
    property StripedEvenColor;
    property StripedItems;

    property Tracking;
    property TrackColor;
    property UnderlineTracked;
    property UseCustomScrollBars;

    property VertDivLinesColor;
    property VerticalLines;
    property VerticalLinesLong;
    property VertScrollBarStyles;
    property UseSystemHintColors;
    property TextColor;
    property BkColor;

    property OnScroll;
    property OnHeaderColumnClick;
    property OnHeaderColumnDraw;
    property OnHeaderColumnResize;
    property OnHeaderColumnMove;
    property OnHeaderLookup;
    property OnHeaderLookupDone;
    property OnHeaderResize;
    property OnHeaderSectionExpand;
    property OnHeaderSectionCollapse;
    property OnHeaderSectionFilterCall;
    property OnHeaderSectionAutoSize;
    property OnHeaderSectionMeasure;
    property OnHorzScrollDrawPart;
    property OnHorzScrollHintNeeded;

    property OnAfterSelectionChange;
    property OnChanging;
    property OnDragTargetChange;
    property OnItemChange;
    property OnItemPreDraw;
    property OnItemDraw;
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    property OnTryEdit;
    property OnEditRequest;
    property OnComboEditShow;
    property OnValidateCombo;
    property OnValidateInplaceEdit;
    property OnTuneUpInplaceEdit;
    property OnEditKeyDown;
{$endif}
{$else}
    property OnTryEdit;
    property OnInplaceEditorNeeded;
{$endif}
    property OnItemChecked;
    property OnItemExpand;
    property OnItemCollapse;
    property OnItemExpanding;
    property OnItemCollapsing;
    property OnItemDeletion;
    property OnItemFocused;
    property OnShowLineHint;
    property OnCompareItems;
    property OnItemPicDraw;
    property OnItemPicDraw2;
    property OnItemPostDraw;
    property OnHotTrack;
    property OnMeasureItemPart;
    property OnSortBegin;
    property OnSortEnd;
    property OnItemSave;
    property OnItemLoad;
    property OnItemSelectedChange;
    property OnCellStyleSave;
    property OnCellStyleLoad;
    property OnVertScrollDrawPart;
    property OnVertScrollHintNeeded;
{$IFDEF HAS_HTML_RENDER}
    property OnHTMLImageNeeded;
{$ENDIF}
    property OnVirtualTextNeeded;
    property OnVirtualHintNeeded;
    property OnVirtualValueNeeded;
{$ifdef ELTREE_USE_STYLES}
    property OnVirtualStyleNeeded;
{$endif}
    property OnHeaderMouseDown;

{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
    property OnOleTargetDrag;
    property OnOleTargetDrop;
    property OnOleDragStart;
    property OnOleDragFinish;
{$ENDIF}
{$endif}

    // VCL properties
{$IFDEF VCL_4_USED}
    property Anchors;
    property Constraints;
    property DockOrientation;
    property Floating;
    property DoubleBuffered;
    property DragKind;
{$ENDIF}
    property BorderStyle;

    property Ctl3D;
    property Cursor;
    property Enabled;
    property Hint;

    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;

    property Visible;
    property TabOrder;
    property TabStop;

    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
{$IFDEF VCL_4_USED}
    property OnStartDock;
    property OnEndDock;
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

  TElShellComboBox = class(TElAdvancedComboBox)
  protected
    FNoRebuild     : boolean;
    FSelectionPIDL : PItemIDList;
    FExplorerStyle : Boolean;
    FShowHidden    : Boolean;
    FFileSystemOnly: Boolean;
    FDummyInt      : integer;
    {$ifdef ELPACK_NO_ADV_EDITS}
    FEditor        : TCustomElEdit;
    {$else}
    FEditor        : TCustomEdit;
    {$endif}
    FCursor: TCursor;
    FStyle: TComboBoxStyle;

    procedure SetExplorerStyle(Value: Boolean);
    procedure FillCombo(BaseFolder : IShellFolder; BasePIDL : PItemIDList; Level :
        integer);
    procedure CreateWnd; override;
    procedure SetShowHidden(Value: Boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    function GetItemWidth(Index: Integer): Integer; override;
    procedure SetFileSystemOnly(Value: Boolean);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); 
        override;
    function GetSelection: PItemIDList;
    procedure ShowEdit;
    procedure KeyPress(var Key: Char); override;
    procedure DropDown; override;
    procedure WMDeleteItem(var Message: TMessage); message WM_DELETEITEM;
    procedure AcceptEdit;
    procedure CancelEdit;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure SetCursor(Value: TCursor);
    procedure FillItems;
    procedure SetStyle(Value: TComboBoxStyle); {$ifdef VCL_5_USED}reintroduce;{$endif}
    procedure DestroyWnd; override;
  public
    destructor Destroy; override;
    constructor Create(AOwner : TComponent); override;
    procedure SetSelection(PIDL : PItemIDList);
    property Selection: PItemIDList read GetSelection write SetSelection;
  published
    property Items : integer read FDummyInt;
    property ItemHeight : integer read FDummyInt;

    property ExplorerStyle: Boolean read FExplorerStyle write SetExplorerStyle default true;
    property ShowHidden: Boolean read FShowHidden write SetShowHidden default true;
    property FileSystemOnly: Boolean read FFileSystemOnly write SetFileSystemOnly;
    property Cursor: TCursor read FCursor write SetCursor;
    property Style: TComboBoxStyle read FStyle write SetStyle stored false default csOwnerDrawVariable;
  end;

  TElShellListItem = class(TElTreeItem)
  private
    FIsValidFile: Boolean;
    FValid: Boolean;
  protected
    {$ifndef VCL_4_USED}
    FAttr        : integer;
    {$else}
    FAttr        : Cardinal;
    {$endif}
    FAttrAsString: string;
    FComment: string;
    FDisplayName: string;
    FFileName: string;
    FPIDL: PItemIDList;
    FSizeAsString: string;
    FTypeName: string;
    Win32FindData: PWin32FindData;
    function GetAttrAsString: string;
    function GetCanRename: Boolean;
    function GetComment: string;
    function GetCreationTime: TDateTime;
    function GetDisplayName: string;
    function GetFileName: string;
    function GetFullName: string;
    function GetIsFileObject: Boolean;
    function GetIsFolder: Boolean;
    function GetIsRemovable: Boolean;
    function GetLastAccessTime: TDateTime;
    function GetModificationTime: TDateTime;
    function GetPIDL: PItemIDList;
    function GetSize: Cardinal;
    function GetSizeAsString: string;
    function GetTypeName: string;
    procedure SetDisplayName(const Value: string);
    procedure CheckWin32FindData;
    procedure GetWin32Data(ParentFolder : IShellFolder);
    procedure Invalidate;
    procedure GetAttributes(iParentFolder : IShellFolder);
    {$ifdef VCL_4_USED}
    function GetHintText(ParentFolder: IShellFolder): TElFString;
    {$endif}
    function GetPicture(ParentFolder: IShellFolder): Integer;
  public
    constructor Create(AOwner : TCustomElTree); override;
    destructor Destroy; override;
    function BuildFullPIDL: PItemIDList;

    {$ifndef VCL_4_USED}
    property Attr: Integer read FAttr;
    {$else}
    property Attr: Cardinal read FAttr;
    {$endif}
    property CanRename: Boolean read GetCanRename;
    property Comment: string read GetComment;
    property CreationTime: TDateTime read GetCreationTime;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property FileName: string read GetFileName;
    property FullName: string read GetFullName;
    property IsFileObject: Boolean read GetIsFileObject;
    property IsFolder: Boolean read GetIsFolder;
    property IsRemovable: Boolean read GetIsRemovable;
    property LastAccessTime: TDateTime read GetLastAccessTime;
    property ModificationTime: TDateTime read GetModificationTime;
    property PIDL: PItemIDList read GetPIDL;
    property SizeAsString: string read GetSizeAsString;
  published
    property AttrAsString: string read GetAttrAsString;
    property Size: Cardinal read GetSize;
    property TypeName: string read GetTypeName;
  end;

  TShellListItemAddingEvent = procedure(Sender : TObject; ItemName : String;
                              ShellFolder : IShellFolder; RelPIDL : PItemIDList;
                              var Allowed : boolean) of object;
  TShellListItemAddedEvent  = procedure(Sender : TObject; ItemName : String;
                              ShellFolder : IShellFolder; RelPIDL : PItemIDList;
                              Item : TElShellListItem) of object;

  TElShellList = class(TCustomElTree)
  private
    {$ifndef VCL_4_USED}
    FAttr: integer;
    {$else}
    FAttr: Cardinal;
    {$endif}
  protected
    FEditor: TElTreeInplaceEdit;
    FCustomFolder: string;
    FDefaultColumns: Boolean;
    FFocusedPIDL: PItemIDList;
    FHighlightCompressed: Boolean;
    FFolder: TShellFolders;
    FShowHidden: Boolean;
    FSizeFormat: TElShellSizeFormat;
    FSortModifiers: TElShellSortModifiers;
    FSortType: TElShellSortType;
    FUseSystemMenus: Boolean;
    FIFolder: IShellFolder;
    FMaxColumns: integer;
    FFileSystemOnly: Boolean;
    FFileFilters: TStringList;
    FOnFilterItem: TShellListItemAddingEvent;
    FOnItemAdded: TShellListItemAddedEvent;
    FDefaultEditor: Boolean;
    FRootPIDL: PItemIDList;
    procedure SetCustomFolder(const Value: string);
    procedure SetDefaultColumns(Value: Boolean);
    function GetFocusedDisplayName: string;
    function GetFocusedPIDL: PItemIDList;
    function GetItemFocused: TElShellListItem;
    procedure SetHighlightCompressed(Value: Boolean);
    procedure SetItemFocused(Value: TElShellListItem);
    procedure SetFolder(Value: TShellFolders);
    procedure SetShowHidden(Value: Boolean);
    procedure SetSizeFormat(Value: TElShellSizeFormat);
    procedure SetSortModifiers(Value: TElShellSortModifiers);
    procedure SetSortType(Value: TElShellSortType);
    procedure SetPIDL(PIDL : PItemIDList);
    procedure AddDefaultColumns;
    function DeleteDefaultColumns: Integer;
    procedure TriggerSortBegin; override;
    procedure TriggerTryEditEvent(Item: TElTreeItem; SectionIndex : integer; var
        CellType: TElFieldType; var CanEdit: boolean); override;
    procedure TriggerVirtualTextNeeded(Item : TElTreeItem; SectionIndex : Integer;
        var Text : TElFString); override;
    procedure TriggerVirtualValueNeeded(Item : TElTreeItem; SectionIndex : Integer;
        VarType : integer; var Value : Variant); override;
    procedure SetFileSystemOnly(Value: Boolean);
    function FindItemByPIDL(APIDL : PItemIDList): TElShellListItem;
    function NameFiltered(S : String; ShellFolder : IShellFolder; RelPIDL :
        PItemIDList): Boolean; virtual;
    function GetFileFilters: TStrings;
    procedure SetFileFilters(const Value: TStrings);
    procedure FileFiltersChange(Sender : TObject);
    procedure CreateHandle; override;
    function CreateItems: TElTreeItems; override;
    function CreateView: TElTreeView; override;
    procedure DoCompareItems(Item1, Item2: TElTreeItem; var res: integer); override;
    {$ifdef VCL_4_USED}
    procedure TriggerVirtualHintNeeded(Item : TElTreeItem; var Hint :
      TElFString); override;
    {$endif}
    function DoGetPicture(Item: TElTreeItem): integer; override;
    procedure DoItemAdded(S : String; ShellFolder : IShellFolder; RelPIDL : 
        PItemIDList; Item : TElShellListItem); virtual;

    property Items stored false;        
  public
    procedure RefreshList;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetRootPIDL(PIDL : PItemIDList);
    procedure SetSelectionPIDL(PIDL : PItemIDList);
    function BuildRootPIDL: PItemIDList;
    procedure Loaded; override;
    property FocusedDisplayName: string read GetFocusedDisplayName;
    property FocusedPIDL: PItemIDList read GetFocusedPIDL;
    property ItemFocused: TElShellListItem read GetItemFocused write SetItemFocused;
  published
    property CustomFolder: string read FCustomFolder write SetCustomFolder;
    property DefaultColumns: Boolean read FDefaultColumns write SetDefaultColumns;
    property HighlightCompressed: Boolean read FHighlightCompressed write
        SetHighlightCompressed default true;
    property Folder: TShellFolders read FFolder write SetFolder default sfoDesktop;
    property ShowHidden: Boolean read FShowHidden write SetShowHidden default true;
    property SizeFormat: TElShellSizeFormat read FSizeFormat write SetSizeFormat;
    property SortModifiers: TElShellSortModifiers read FSortModifiers write
        SetSortModifiers;
    property SortType: TElShellSortType read FSortType write SetSortType default
        sstName;
    property UseSystemMenus: Boolean read FUseSystemMenus write FUseSystemMenus;
    property FileSystemOnly: Boolean read FFileSystemOnly write SetFileSystemOnly;
    property FileFilters: TStrings read GetFileFilters write SetFileFilters;
    property OnItemAdded: TShellListItemAddedEvent read FOnItemAdded write
        FOnItemAdded;
    property OnItemAdding: TShellListItemAddingEvent read FOnFilterItem write
        FOnFilterItem;
    property DefaultEditor: Boolean read FDefaultEditor write FDefaultEditor
        default True;

    // inherited properties
    property ActiveBorderType;
    property Align;
    property AlwaysKeepFocus;
    property AlwaysKeepSelection;
    property AutoExpand;
    property AutoLineHeight;
    property AutoLookup;
    property AutoResizeColumns;

    property DefaultSectionWidth;
    property AdjustMultilineHeight;
    property Background;
    property BackgroundType;

    property BarStyle;
    property BarStyleVerticalLines;
    property BorderSides;
    property ChangeDelay;
    property ChangeStateImage;
    property CheckBoxGlyph;
    property CheckBoxSize;
    property CustomCheckboxes;
    property CustomPlusMinus;
    property DeselectChildrenOnCollapse;
	property DblClickMode;
    property DoInplaceEdit;
    property DragAllowed;
    property DragCursor;
    property DragExpandDelay;
    property DraggableSections;
    property DrawFocusRect;
    property DragImageMode;
    property DragRectAcceptColor;
    property DragRectDenyColor;
    property DragScrollInterval;
    property DragTrgDrawMode;
    property DragType;

    property ExpandOnDblClick;
    property ExpandOnDragOver;
    property ExplorerEditMode default true;
    property FilteredVisibility;
    property Flat;
    property FlatFocusedScrollbars;
    property FocusedSelectColor;
    property FocusedSelectTextColor;
    property ForcedScrollBars;
    property Font stored true;
    property FullRowSelect;
    property GradientStartColor;
    property GradientEndColor;
    property GradientSteps;

    property HeaderActiveFilterColor;
    property HeaderColor;
    property HeaderHeight;
    property HeaderHotTrack;
    property HeaderInvertSortArrows;
    property HeaderSections;
    property HeaderFilterColor;
    property HeaderFlat;
    property HeaderFont;
    property HeaderUseTreeFont;
    property HeaderImages;
    property HeaderWrapCaptions;
    property HideFocusRect;
    property HideHintOnTimer;
    property HideHintOnMove;
    property HideSelectColor;
    property HideSelectTextColor;
    property HideSelection;
    property HorizontalLines;
    property HideHorzScrollBar;
    property HideVertScrollBar;
    property HintType;
    property HorzDivLinesColor;
    property HorzScrollBarStyles;
    property HeaderImageForm;
    property ImageForm;

    property InactiveBorderType;
    property IncrementalSearch;
    property InplaceEditorDelay;
    property ItemIndent;
    property LeafPicture;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;
    property LineHeight;
    property LinesColor;
    property LinesStyle;
    property LineHintColor;
    property LineHintMode default shmLong;
    property LineHintTimeout;
    property LineHintType;
    property LockHeaderHeight;
    property MainTreeColumn;
    property MinusPicture;
    property MoveColumnOnDrag;
    property MoveFocusOnCollapse;
    property MouseFrameSelect;
    property MultiSelect;
    property MultiSelectLevel;
    property OwnerDrawByColumn default true;
    property OwnerDrawMask;
    property PathSeparator;
    property PlusMinusTransparent;
    property PlusPicture;
    property QuickEditMode;

    property RadioButtonGlyph;
    property RightAlignedText;
    property RightAlignedTree;
    property RightClickSelect;
    property RowHotTrack;
    property RowSelect;

    property ScrollbarOpposite;
    property ScrollTracking;
    property SelectColumn;
    property ShowButtons default true;
    property ShowColumns;
    property ShowCheckboxes;
    property ShowEmptyImages default true;
    property ShowEmptyImages2;
    property ShowHint;
    property ShowImages;
    property ShowLeafButton;
    property ShowLines default false;
    property ShowRoot default false;
    property ShowRootButtons default false;
    property SelectionMode;
    property SortDir;
    property SortMode;
    property Storage;
    property StoragePath;
    property StickyHeaderSections;
    property StripedOddColor;
    property StripedEvenColor;
    property StripedItems;

    property Tracking;
    property TrackColor;
    property UnderlineTracked;
    property UseCustomScrollBars;

    property VertDivLinesColor;
    property VerticalLines;
    property VerticalLinesLong;
    property VertScrollBarStyles;
    property UseSystemHintColors;
    property TextColor;
    property BkColor;

    property OnScroll;
    property OnHeaderColumnClick;
    property OnHeaderColumnDraw;
    property OnHeaderColumnResize;
    property OnHeaderColumnMove;
    property OnHeaderLookup;
    property OnHeaderLookupDone;
    property OnHeaderResize;
    property OnHeaderSectionExpand;
    property OnHeaderSectionCollapse;
    property OnHeaderSectionFilterCall;
    property OnHeaderSectionAutoSize;
    property OnHeaderSectionMeasure;
    property OnHorzScrollDrawPart;
    property OnHorzScrollHintNeeded;

    property OnAfterSelectionChange;
    property OnChanging;
    property OnDragTargetChange;
    property OnItemChange;
    property OnItemPreDraw;
    property OnItemDraw;
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    property OnTryEdit;
    property OnEditRequest;
    property OnComboEditShow;
    property OnValidateCombo;
    property OnValidateInplaceEdit;
    property OnTuneUpInplaceEdit;
    property OnEditKeyDown;
{$endif}
{$else}
    property OnInplaceEditorNeeded;
{$endif}
    property OnItemChecked;
    property OnItemExpand;
    property OnItemCollapse;
    property OnItemExpanding;
    property OnItemCollapsing;
    property OnItemDeletion;
    property OnItemFocused;
    property OnShowLineHint;
    property OnCompareItems;
    property OnItemPicDraw;
    property OnItemPicDraw2;
    property OnItemPostDraw;
    property OnHotTrack;
    property OnMeasureItemPart;
    property OnSortBegin;
    property OnSortEnd;
    property OnItemSave;
    property OnItemLoad;
    property OnItemSelectedChange;
    property OnCellStyleSave;
    property OnCellStyleLoad;
    property OnVertScrollDrawPart;
    property OnVertScrollHintNeeded;
{$IFDEF HAS_HTML_RENDER}
    property OnHTMLImageNeeded;
{$ENDIF}
    property OnVirtualTextNeeded;
    property OnVirtualHintNeeded;
    property OnVirtualValueNeeded;
{$ifdef ELTREE_USE_STYLES}
    property OnVirtualStyleNeeded;
{$endif}

    property OnHeaderMouseDown;

{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
    property OnOleTargetDrag;
    property OnOleTargetDrop;
    property OnOleDragStart;
    property OnOleDragFinish;
{$ENDIF}
{$endif}

    // VCL properties
{$IFDEF VCL_4_USED}
    property Anchors;
    property Constraints;
    property DockOrientation;
    property Floating;
    property DoubleBuffered;
    property DragKind;
{$ENDIF}
    property BorderStyle;

    property Ctl3D;
    property Cursor;
    property Enabled;
    property Hint;

    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;

    property Visible;
    property TabOrder;
    property TabStop;

    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
{$IFDEF VCL_4_USED}
    property OnStartDock;
    property OnEndDock;
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

const
     siBase     = 1458;
     siMin      = 0;
     siName     = 0;
     siSize     = 1;
     siType     = 2;
     siModified = 3;
     siAttr     = 4;
     siComment  = 5;
     siCreated  = 6;
     siAccessed = 7;
     siMax      = 7;

const DefaultColumnNames : array[siMin..siMax] of string
                         = ('Name',
                            'Size',
                            'Type',
                            'Modified',
                            'Attributes',
                            'Comment',
                            'Created',
                            'Accessed');
DefaultColumnAlignments  : array[siMin..siMax] of integer
                         = (LVCFMT_LEFT,
                            LVCFMT_RIGHT,
                            LVCFMT_LEFT,
                            LVCFMT_LEFT,
                            LVCFMT_LEFT,
                            LVCFMT_LEFT,
                            LVCFMT_LEFT,
                            LVCFMT_LEFT);

ColumnSortTypes          : array[siMin..siMax] of TElShellSortType
                         = (sstName,
                            sstSize,
                            sstCustom,
                            sstModifyDate,
                            sstCustom,
                            sstCustom,
                            sstCreationDate,
                            sstAccessDate);


implementation

type

  TElShellDefaultColumns = class(TInterfacedObject, IShellDetails)
    function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;
      var pDetails: TShellDetails): HResult; stdcall;
    function ColumnClick(iColumn: UINT): HResult; stdcall;
  end;

  TElShellTreeView = class(TElTreeView)
  private
    iCtxMenu : iContextMenu;
    FBuiltinMenu : HMENU;
    FMenuWnd : HWND;
    procedure MenuWndProc(var Message: TMessage);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure ShowSystemMenu(X, Y : integer);
    function GetPopupMenu: TPopupMenu; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TElShellListView = class(TElTreeView)
  private
    iCtxMenu: iContextMenu;
    FBuiltinMenu : HMENU;
    FMenuWnd : HWND;
    procedure MenuWndProc(var Message: TMessage);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure ShowSystemMenu(X, Y : integer);
    function GetPopupMenu: TPopupMenu; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TElShellComboEdit = class(TElAdvancedMemo)
  protected
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  PElShellComboData = ^TElShellComboData;
  TElShellComboData = record
    Level       : integer;
    DisplayName : string;
    FileName    : string;
    PIDL        : PItemIDList;
    IconIdx     : integer;
    OpenIconIdx : integer;
    Attr        : integer;
  end;

{
function SUCCEEDED(res : HRESULT) : boolean;
begin
  result := true;
  if (res <> S_OK) and (res <> S_FALSE) then
    raise EElShellError.CreateFmt('Shell error %x (Facility %x, Error code %x)', [res, HIWORD(res) and $0FFF, LOWORD(res)]);
end;
}

procedure TElShellComboEdit.DoExit;
begin
  inherited;
  TElShellComboBox(Parent).CancelEdit;
end;

procedure TElShellComboEdit.KeyPress(var Key: Char);
begin
  if Key = Char(VK_ESCAPE) then
  begin
    TElShellComboBox(Parent).CancelEdit;
    Key:= #0;
    exit;
  end;
  if Key = Char(VK_RETURN) then
  begin
    TElShellComboBox(Parent).AcceptEdit;
    Key:= #0;
    exit;
  end;

  if Key in [Char(VK_UP), Char(VK_DOWN), Char(VK_PRIOR), Char(VK_NEXT), #10] then
  begin
    Key := #0;
    exit;
  end
  else
  if Key = Char(VK_ESCAPE) then
  begin
    Key := #0;
  end;
  inherited;
end;

procedure TElShellComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    TElShellComboBox(Parent).AcceptEdit;
    Key:= 0;
  end;
  if Key = VK_ESCAPE then
  begin
  end;
  inherited;
end;


function TElShellDefaultColumns.GetDetailsOf(pidl: PItemIDList; iColumn: UINT;
      var pDetails: TShellDetails): HResult;
begin
  if (PIDL <> nil) or (not InRange(siMin, siMax, iColumn)) then
    result := E_NOTIMPL
  else
  begin
    pDetails.fmt := DefaultColumnAlignments[iColumn];
    pDetails.str.uType := STRRET_CSTR;
    StrPCopy(pDetails.str.cStr, DefaultColumnNames[iColumn]);
    result := S_OK;
  end;
end;

function TElShellDefaultColumns.ColumnClick(iColumn: UINT): HResult;
begin
  result := E_NOTIMPL;
end;

function TElShellTree.GetRootFolder: TShellFolders;
begin
  Result:= FRootFolder;
end;

procedure TElShellTree.SetRootFolder(Value: TShellFolders);
var PIDL : PItemIDList;
begin
  if FRootFolder <> Value then
  begin
    FRootFolder := Value;
    if (FRootFolder = sfoCustom) and (CustomRootFolder = '') and (not (csLoading in ComponentState)) then
      FRootFolder := sfoDesktop;
    PIDL := GetFolderPIDL(Value, CustomRootFolder);
    SetRootPIDL(PIDL);
    FreeIDList(PIDL);
  end;
end;

function TElShellTree.GetFocusedPIDL: PItemIDList;
begin
  if ItemFocused <> nil then
    result := TElShellTreeItem(ItemFocused).BuildFullPIDL
  else
    result := nil;
end;

function TElShellTree.GetFocusedDisplayName: string;
var PIDL : PItemIDList;
begin
  PIDL := GetFocusedPIDL;
  if (PIDL <> nil) and (GetPathFromPIDL(PIDL, Result)) then
  begin

  end
  else
    result := '';
end;

procedure TElShellTree.BuildTree;
begin
  IsUpdating := true;
  try
    Items.Clear;
    RefreshTree(nil, 0);
  finally
    IsUpdating := false;
  end;
end;

procedure TElShellTree.SetRootPIDL(PIDL : PItemIDList);
var iFolder : IShellFolder;
    hRes : HRESULT;
begin
  if FRootPIDL <> nil then
    FreeIDList(FRootPIDL);
  FRootPIDL := ClonePIDL(PIDL);
  iFolder := nil;
  SHGetDesktopFolder(iFolder);
  if (RootFolder <> sfoDesktop) and (RootFolder <> sfoDesktopExpanded) and (CalcPIDLSize(PIDL) <> 2) then
  begin
    hRes := iFolder.BindToObject(FRootPIDL, nil, IID_IShellFolder, pointer(FIFolder));
    if not SUCCEEDED(hRes) then
      raise Exception.Create('Failed to get IShellFolder for specified root');
    iFolder := nil;
  end
  else
    FIFolder := iFolder;

  BuildTree;
end;

constructor TElShellTree.Create(AOwner : TComponent);
begin
  inherited;
  Images := ShellIconCache.SmallImages;
  VirtualityLevel := vlTextAndStyles;
  FHeader.InvertSortArrows := true;
  FRootFolder  := sfoDesktop;
  FShowHidden  := true;
  FFileFilters := TStringList.Create;
  FFileFilters.OnChange := FileFiltersChange;
  FShowFiles := false;
  FSortType := sstName;
  FSortMode := smAddClick;
  FSortModifiers := [ssmFoldersFirst, ssmExecutablesFirst];
  FHighlightCompressed := true;
  FClearOnCollapse := true;
  FDefaultEditor := true;
  FEditor := TElTreeInplaceEdit.Create(nil);
  FEditor.OnValidateResult := OnValidateEdit;
  ExplorerEditMode := true;
  inherited SortType := stCustom;
end;

procedure TElShellTree.DoItemFocused;
begin
  inherited;
end;

procedure TElShellTree.ReleaseFocusedPIDL;
begin
  if FFocusedPIDL <> nil then
    FreeIDList(FFocusedPIDL);
  FFocusedPIDL := nil;
end;

procedure TElShellTree.SetCustomRootFolder(const Value: string);
var PIDL : PItemIDList;
begin
  if FCustomRootFolder <> Value then
  begin
    FCustomRootFolder := Value;
    if FRootFolder = sfoCustom then
    begin
      if CustomRootFolder = '' then
        SetRootFolder(sfoDesktop)
      else
      begin
        PIDL := GetFolderPIDL(sfoCustom, CustomRootFolder);
        SetRootPIDL(PIDL);
        FreeIDList(PIDL);
      end;
    end;
  end;
end;

function TElShellTree.CreateView: TElTreeView;
begin
  result := TElShellTreeView.Create(Self);
end;  { CreateView }

function TElShellTree.CreateItems: TElTreeItems;
begin
  result := inherited CreateItems;
  result.ItemClass := TElShellTreeItem;
end;

destructor TElShellTree.Destroy;
begin
  FEditor.Free;
  FIFolder := nil;
  FFileFilters.Free;
  // Items.Clear;
  inherited;
end;

procedure TElShellTree.RefreshTree(Item : TElTreeItem; recursive : Integer);
var RootItem : TElShellTreeItem;
     AFolder : IShellFolder;
       APIDL : PItemIDList;
         chF : IShellFolder;
         str : TStrRet;
           S : String;
           b : boolean;
           
begin
  if not HandleAllocated then exit;
  IsUpdating := true;
  try
    if Item <> nil then
    begin
      // we are refreshing not from root, but from some intermediate item
      RootItem := TElShellTreeItem(Item);

      if IsDesktopPIDL(RootItem.PIDL) then
        SHGetDesktopFolder(AFolder)
      else
      begin
        SHGetDesktopFolder(AFolder);
        // AFolder := RootItem.ParentFolder;
        APIDL := RootItem.BuildFullPIDL;
        if not SUCCEEDED(AFolder.BindToObject(APIDL, nil, IID_IShellFolder, pointer(chF))) then
        begin
          Items.DeleteItem(Item);
          FreeIDList(APIDL);
          exit;
        end;
        FreeIDList(APIDL);
        AFolder := nil;
        AFolder := chF;
      end;

      APIDL := ClonePIDL(RootItem.FPIDL);
      RootItem.Invalidate;
      FreeIDList(RootItem.FPIDL);
      RootItem.FPIDL := APIDL;
      RootItem.GetAttributes(AFolder);
    end
    else
    begin
      b := Items.Count = 0;
      // we are refreshing from root
      if Items.Count = 0 then
      begin
        RootItem := TElShellTreeItem(Items.AddItem(nil));
      end
      else
      begin
        RootItem := TElShellTreeItem(Items[0]);
        RootItem.Invalidate;
      end;

      if (FRootFolder <> sfoDesktop) and (FRootFolder <> sfoDesktopExpanded) then
      begin
        FreeIDList(RootItem.FPIDL);
        RootItem.FPIDL := ClonePIDL(GetOwnPIDL(FRootPIDL));
      end;
      AFolder := RootItem.GetParentFolder;

      RootItem.GetAttributes(AFolder);
      if (FRootFolder = sfoDesktopExpanded) or ExpandRoot then
        RootItem.Expanded := true;

      if b then
      begin
        ZeroMemory(@Str, sizeof(Str));
        Str.uType := STRRET_CSTR;
        AFolder.GetDisplayNameOf(RootItem.PIDL, SHGDN_INFOLDER or SHGDN_FORADDRESSBAR, str);
        S := StrRetToPas(str, RootItem.PIDL);
        StrRetFree(str);

        DoItemAdded(S, AFolder, RootItem.FPIDL, TElShellTreeItem(RootItem));
      end;
    end;

    if RootItem.IsFolder then
    begin
      RootItem.ForceButtons := RootItem.HasSubFolders;
      if (CheckForChildren and (not RootItem.IsRemovable)) or RootItem.Expanded then
      begin
        if SUCCEEDED(AFolder.BindToObject(RootItem.FPIDL, nil, IID_IShellFolder, Pointer(chF))) then
          CheckChildren(RootItem, chF);
      end
      else
        RootItem.ForceButtons := true;

      if RootItem.IsFolder and
           ((recursive = 2) or ((Recursive = 1) and RootItem.Expanded)) then
        FillItemWithData(RootItem, AFolder, Max(Recursive, 1));
    end;
  finally
    IsUpdating := false;
  end;
end;

function TElShellTree.CheckChildren(Item : TElTreeItem; AFolder :
    IShellFolder): Boolean;
var //t     : integer;
    i,
    j,
    FAttr : cardinal;
    List  : IEnumIDList;
    Flags : Cardinal;
    PIDL  : PItemIDList;
    // CList : TElList;
begin
  Flags := SHCONTF_FOLDERS;
  if ShowHidden then
    Flags := Flags or SHCONTF_INCLUDEHIDDEN;
  if ShowFiles then
    Flags := Flags or SHCONTF_NONFOLDERS;
  List := nil;

  // CList := TElList.Create;
  if SUCCEEDED(AFolder.EnumObjects(0, Flags, List)) then
  begin
    j := 0;
    PIDL := nil;
    result := false;
    
    while SUCCEEDED(List.Next(1, PIDL, j)) and (j > 0) do
    begin
      if FileSystemOnly then
      begin
        i := SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR;
        if SUCCEEDED(AFolder.GetAttributesOf(1, PIDL, i)) then
          FAttr := i
        else
          FAttr := 0;
        if ((FAttr and SFGAO_FILESYSTEM) <> SFGAO_FILESYSTEM) and
           ((FAttr and SFGAO_FILESYSANCESTOR) <> SFGAO_FILESYSANCESTOR) then
        begin
          if PIDL <> nil then
            FreeIDList(PIDL);
          Continue;
        end;
      end;
      if PIDL <> nil then
        FreeIDList(PIDL);
      result := true;
      break;
    end;
    Item.ForceButtons := result;
  end
  else
    result := false;
  List := nil;
end;

procedure TElShellTree.FillItemWithData(Item : TElShellTreeItem; AFolder :
    IShellFolder; recursive : integer);
var
    i,
    j,
    FAttr: cardinal;
    List : IEnumIDList;
    Flags: Cardinal;
    PIDL : PItemIDList;
    Child: TElShellTreeItem;
    S    : String;
    str  : TStrRet;
    chF  : IShellFolder;

begin
  Flags := SHCONTF_FOLDERS;
  if ShowHidden then
    Flags := Flags or SHCONTF_INCLUDEHIDDEN;
  if ShowFiles then
    Flags := Flags or SHCONTF_NONFOLDERS;
  List := nil;
  // get enumerator to walk through object
  if SUCCEEDED(AFolder.EnumObjects(Handle, Flags, List)) then
  begin
    j := 0;
    chF := nil;
    PIDL := nil;

    // invalidate all items. We will validate them as the shell provides right PIDLs
    // and then we'll remove all items, that will remain invalid
    i := 0;
    while i < Cardinal(Item.Count) do
    begin
      TElShellTreeItem(Item.Children[i]).FValid := false;
      inc(i);
    end;

    // first grab all child PIDLs
    while SUCCEEDED(List.Next(1, PIDL, j)) and (j > 0) and (PIDL <> nil) do
    begin
      if FileSystemOnly then
      begin
        i := SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR;
        if SUCCEEDED(AFolder.GetAttributesOf(1, PIDL, i)) then
          FAttr := i
        else
          FAttr := 0;
        if ((FAttr and SFGAO_FILESYSTEM) <> SFGAO_FILESYSTEM) and
           ((FAttr and SFGAO_FILESYSANCESTOR) <> SFGAO_FILESYSANCESTOR) then
        begin
          FreeIDList(PIDL);
          PIDL := nil;
          Continue;
        end;
      end;

      ZeroMemory(@Str, sizeof(Str));
      Str.uType := STRRET_CSTR;
      AFolder.GetDisplayNameOf(PIDL, SHGDN_INFOLDER or SHGDN_FORADDRESSBAR, str);
      S := StrRetToPas(str, PIDL);
      StrRetFree(str);
      if NameFiltered(S, AFolder, PIDL) then
      begin
        Child := Item.FindItemByPIDL(PIDL);
        if Child = nil then
        begin
          Child := TElShellTreeItem(Items.AddChild(Item, ''));
          FreeIDList(Child.FPIDL);
        end
        else
        begin
          Child.Invalidate;
          if Child.FPIDL <> nil then
            FreeIDList(Child.FPIDL);
        end;
        Child.FPIDL := PIDL;

        Child.GetAttributes(AFolder);
        if Child.IsFolder and
           ((recursive = 2) or ((Recursive = 1) and Child.Expanded)) then
        begin
          if SUCCEEDED(AFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(chF))) then
            FillItemWithData(Child, chF, Max(Recursive, 1))
          else
            Child.Clear;
          chF := nil;
        end
        else
          Child.Clear;

        if Child.IsFolder then
        begin
          Child.ForceButtons := Child.HasSubFolders;
          if CheckForChildren and not TElShellTreeItem(Child).IsRemovable then
          begin
            if SUCCEEDED(AFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(chF))) then
              CheckChildren(Child, chF);
          end
          else
            Child.ForceButtons := true;
        end;
        Child.FValid := true;
        DoItemAdded(S, AFolder, PIDL, Child);
      end
      else
      begin
        Child := Item.FindItemByPIDL(PIDL);
        if Child <> nil then
          Items.DeleteItem(Child);
        FreeIDList(PIDL);
        PIDL := nil;
      end;
    end;

    // now check for invalid tree items and remove them
    i := 0;
    while i < Cardinal(Item.Count) do
    begin
      if TElShellTreeItem(Item.Children[i]).FValid then
        inc(i)
      else
        Items.DeleteItem(Item.Children[i]);
    end;
  end;
  Item.Sort(false);
  List := nil;
end;

procedure TElShellTree.Loaded;
var F : TShellFolders;
begin
  inherited;
  F := FRootFolder;
  FRootFolder := TShellFolders(Integer(F) - 1);
  SetRootFolder(F);
end;

procedure TElShellTree.SetShowHidden(Value: Boolean);
begin
  if FShowHidden <> Value then
  begin
    FShowHidden := Value;
    RefreshTree(nil, 1);
  end;
end;

procedure TElShellTree.SetShowFiles(Value: Boolean);
begin
  if FShowFiles <> Value then
  begin
    FShowFiles := Value;
    RefreshTree(nil, 1);
  end;
end;

procedure TElShellTree.SetHighlightCompressed(Value: Boolean);
begin
  if FHighlightCompressed <> Value then
  begin
    FHighlightCompressed := Value;
    RefreshTree(nil, 1);
  end;
end;

function TElShellTree.GetFileFilters: TStrings;
begin
  Result := FFileFilters;
end;

procedure TElShellTree.SetFileFilters(const Value: TStrings);
begin
  FFileFilters.Assign(Value);
end;

function TElShellTree.NameFiltered(S : String; ShellFolder : IShellFolder; RelPIDL : PItemIDList) : Boolean;
var i : integer;
begin
  result := true;
  for i := 0 to FFileFilters.Count - 1 do
  begin
    if not FileNameLike(S, FFileFilters[i]) then
    begin
      result := false;
      break;
    end
  end;
  if Assigned(FOnFilterItem) then
    FOnFilterItem(Self, S, ShellFolder, RelPIDL, result);
end;

procedure TElShellTree.CreateHandle;
var PIDL : PItemIDList;
begin
  inherited;
  PIDL := GetFolderPIDL(FRootFolder, CustomRootFolder);
  SetRootPIDL(PIDL);
  FreeIDList(PIDL);
end;

procedure TElShellTree.DoItemCollapse(Item: TElTreeItem);
begin
  inherited;
  if ClearOnCollapse then
  begin
    if Item.Count > 0 then
      Item.ForceButtons := true;
    Item.Clear;
    //if (not MoveFocusOnCollapse) and (OldFocused <> inherited ItemFocused) then
    //  DoItemFocused;
  end;
end; { DoItemCollapse }

procedure TElShellTree.DoItemExpand(Item: TElTreeItem);
var dFolder,
    iFolder : IShellFolder;
    PIDL    : PItemIDList;
    SaveCursor: TCursor;
    b : boolean;
begin
  inherited;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    SHGetDesktopFolder(dFolder);
    if Item.Count = 0 then
    begin
      PIDL := TElShellTreeItem(Item).PIDL;
      if IsDesktopPIDL(PIDL) then
      begin
        iFolder := dFolder;
        b := true;
      end
      else
      begin
        dFolder := TElShellTreeItem(Item).GetParentFolder;
        b := SUCCEEDED(dFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(IFolder)));
      end;
      if b then
        FillItemWithData(TElShellTreeItem(Item), iFolder, 0);
      iFolder := nil;
      dFolder := nil;
      Item.Sort(false);
    end;
  finally
    Screen.Cursor := SaveCursor;
  end;
end; { DoItemExpand }

procedure TElShellTree.DoItemExpanding(Item: TElTreeItem; var CanProcess:
    boolean);
var dFolder,
    iFolder : IShellFolder;
    PIDL    : PItemIDList;
    b       : boolean;
begin
  inherited;
  if Item.Count = 0 then
  begin
    PIDL := TElShellTreeItem(Item).PIDL;
    iFolder := nil;
    dFolder := nil;
    SHGetDesktopFolder(dFolder);
    b := true;
    if IsDesktopPIDL(PIDL) then
      iFolder := dFolder
    else
    begin
      dFolder := TElShellTreeItem(Item).GetParentFolder;
      b := SUCCEEDED(dFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(iFolder)));
    end;
    CanProcess := false;
    Item.ForceButtons := false;

    if b and (TElShellTreeItem(Item).HasSubFolders or CheckChildren(Item, iFolder)) then
      CanProcess := true;

    iFolder := nil;
    dFolder := nil;
  end;
end; { DoItemExpanding }

procedure TElShellTree.DoItemAdded(S : String; ShellFolder : IShellFolder;
    RelPIDL : PItemIDList; Item : TElShellTreeItem);
begin
  if assigned(FOnItemAdded) then
    FOnItemAdded(Self, S, ShellFolder, RelPIDL, Item);
end;

procedure TElShellTree.SetFileSystemOnly(Value: Boolean);
begin
  if FFileSystemOnly <> Value then
  begin
    FFileSystemOnly := Value;
    RefreshTree(nil, 1);
  end;
end;

function TElShellTree.GetItemFocused: TElShellTreeItem;
begin
  Result := TElShellTreeItem(inherited ItemFocused);
end;

procedure TElShellTree.SetItemFocused(Value: TElShellTreeItem);
begin
  inherited ItemFocused := Value;
end;

procedure TElShellTree.DoCompareItems(Item1, Item2: TElTreeItem; var res:
    integer);
var S1, S2 : string;
    D1, D2 : TDateTime;
    B1, B2 : boolean;
    I1, I2 : Cardinal;
begin
  if SortType = sstCustom then
    inherited
  else
  begin
    if ssmFoldersFirst in SortModifiers then
    begin
      B1 := TElShellTreeItem(Item1).IsFolder;
      B2 := TElShellTreeItem(Item2).IsFolder;
      if B1 <> b2 then
      begin
        if b2 then
          res := 1
        else
          res := -1;
        exit;
      end
      else
      begin
        B1 := TElShellTreeItem(Item1).IsFileObject;
        B2 := TElShellTreeItem(Item2).IsFileObject;
        if B1 <> B2 then
        begin
          if b2 then
            res := 1
          else
            res := -1;
          exit;
        end;
      end;
    end;

    if ssmExecutablesFirst in SortModifiers then
    begin
      if (TElShellTreeItem(Item1).IsFileObject and
          TElShellTreeItem(Item2).IsFileObject) then
      begin
        B1 := TElShellTreeItem(Item1).IsFolder;
        B2 := TElShellTreeItem(Item2).IsFolder;
        if not (b1 or b2) then
        begin
          S1 := Uppercase(ExtractFileExt(TElShellTreeItem(Item1).FileName));
          S2 := Uppercase(ExtractFileExt(TElShellTreeItem(Item2).FileName));
          b1 := (S1 = '.EXE') or (S1 = '.DLL');
          b2 := (S2 = '.EXE') or (S2 = '.DLL');
          if B1 <> b2 then
          begin
            if B2 then
              res := 1
            else
              res := -1;
            exit;
          end;
        end;
      end;
    end;

    if ((TElShellTreeItem(Item1).FAttr and SFGAO_FILESYSANCESTOR) <> SFGAO_FILESYSANCESTOR) <>
       ((TElShellTreeItem(Item2).FAttr and SFGAO_FILESYSANCESTOR) <> SFGAO_FILESYSANCESTOR) then
    begin
      if (TElShellTreeItem(Item1).FAttr and SFGAO_FILESYSANCESTOR) <> SFGAO_FILESYSANCESTOR then
        res := 1
      else
        res := -1;
      exit;
    end;

    case SortType of
      sstName:
        begin
          S1 := TElShellTreeItem(Item1).FileName;
          S2 := TElShellTreeItem(Item2).FileName;
          res := AnsiStrIComp(PChar(S1), PChar(S2));
        end;
      sstExt:
        begin
          S1 := ExtractFileExt(TElShellTreeItem(Item1).FileName);
          S2 := ExtractFileExt(TElShellTreeItem(Item2).FileName);
          res := AnsiStrIComp(PChar(S1), PChar(S2));
        end;
      sstSize:
        begin
          I1 := TElShellTreeItem(Item1).Size;
          I2 := TElShellTreeItem(Item2).Size;
          if I1 > I2 then
            res := 1
          else
          if I1 < I2 then
            res := -1
          else
          if I1 = I2 then
            res := 0;
        end;
      sstCreationDate,
      sstModifyDate,
      sstAccessDate:
        begin
          case SortType of
            sstModifyDate:
              begin
                D1 := TElShellTreeItem(Item1).ModificationTime;
                D2 := TElShellTreeItem(Item2).ModificationTime;
              end;
            sstAccessDate:
              begin
                D1 := TElShellTreeItem(Item1).LastAccessTime;
                D2 := TElShellTreeItem(Item2).LastAccessTime;
              end;
            else
              begin
                D1 := TElShellTreeItem(Item1).CreationTime;
                D2 := TElShellTreeItem(Item2).CreationTime;
              end;
          end;
          if D1 > D2 then
            res := 1
          else
          if D1 < D2 then
            res := -1
          else
          if D1 = D2 then
            res := 0;
        end;
    end;
  end;
end; { DoCompareItems }

procedure TElShellTree.SetSortType(Value: TElShellSortType);
begin
  if FSortType <> Value then
  begin
    FSortType := Value;
    if SortMode in [smAdd, smAddClick] then
      Sort(true);
  end;
end;

procedure TElShellTree.SetSortModifiers(Value: TElShellSortModifiers);
begin
  if FSortModifiers <> Value then
  begin
    FSortModifiers := Value;
    if SortMode in [smAdd, smAddClick] then
      Sort(true);
  end;
end;

procedure TElShellTree.TriggerVirtualTextNeeded(Item : TElTreeItem;
    SectionIndex : Integer; var Text : TElFString);
var Index : integer;
begin
  if (SectionIndex <> -1) and (HeaderSections.Count > SectionIndex) then
    Index := HeaderSections[SectionIndex].Tag - siBase
  else
    Index := siName;

  begin
    Text := '';
    case Index of
      siName: Text := TElShellTreeItem(Item).DisplayName;
      siSize:
        if TElShellTreeItem(Item).IsFileObject and (not TElShellTreeItem(Item).IsFolder) then
          Text := TElShellTreeItem(Item).SizeAsString;
      siType:
        if TElShellTreeItem(Item).IsFileObject then
          Text := TElShellTreeItem(Item).TypeName;
      siModified:
        if TElShellTreeItem(Item).IsFileObject and TElShellTreeItem(Item).FIsValidFile then
          Text := DateTimeToStr(TElShellTreeItem(Item).ModificationTime);
      siAttr:
        if TElShellTreeItem(Item).IsFileObject and TElShellTreeItem(Item).FIsValidFile then
          Text := TElShellTreeItem(Item).AttrAsString;
      siComment:
        if not TElShellTreeItem(Item).IsFileObject then
          Text := TElShellTreeItem(Item).Comment;
      siCreated:
        if TElShellTreeItem(Item).IsFileObject and TElShellTreeItem(Item).FIsValidFile then
          Text := DateTimeToStr(TElShellTreeItem(Item).CreationTime);
      siAccessed:
        if TElShellTreeItem(Item).IsFileObject and TElShellTreeItem(Item).FIsValidFile then
          Text := DateTimeToStr(TElShellTreeItem(Item).LastAccessTime);
      else
        inherited;
    end;
  end;
end;

procedure TElShellTree.SetSizeFormat(Value: TElShellSizeFormat);
var i : integer;
begin
  if FSizeFormat <> Value then
  begin
    FSizeFormat := Value;
    for i := 0 to FALlList.Count - 1 do
      TElShellTreeItem(FAllList[i]).FSizeAsString := '-';
    Invalidate;
  end;
end;

procedure TElShellTree.SetDefaultColumns(Value: Boolean);
var
  MainTreeCol: integer;
begin
  if FDefaultColumns <> Value then
  begin
    FDefaultColumns := Value;
    MainTreeCol := DeleteDefaultColumns;
    if FDefaultColumns then
    begin
      AddDefaultColumns;
      FMainTreeCol := MainTreeCol;
    end;
  end;
end;

procedure TElShellTree.AddDefaultColumns;
var i : integer;
    dFolder,
    IFolder  : IShellFolder;
    IFolder2 : IShellFolder2;
    IDetails : IShellDetails;
    sd       : TShellDetails;
    PIDL     : PItemIDList;
    hres     : HResult;
begin
  IsUpdating := true;

  SHGetDesktopFolder(dFolder);
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  dFolder.BindToObject(PIDL, nil, IID_ISHELLFOLDER, Pointer(iFolder));

  if not SUCCEEDED(iFolder.CreateViewObject(Handle, IShellDetails, Pointer(IDetails))) then
  begin
    if not SUCCEEDED(iFolder.QueryInterface(IID_IShellFolder2, IFolder2)) then
    begin
      IDetails := TElShellDefaultColumns.Create as IShellDetails;
    end;
  end;

  i := 0;
  repeat
    if IFolder2 <> nil then
      hRes := IFolder2.GetDetailsOf(nil, i, SD)
    else
      hRes := IDetails.GetDetailsOf(nil, i, SD);
    if hRes = S_OK then
    begin
      with HeaderSections.InsertSection(i) do
      begin
        Text := StrRetToPas(SD.str, nil); StrRetFree(SD.str);
        AutoSize := false;
        FieldType := sftCustom;
        Editable := i = siName;
        Visible := InRange(siMin, siMax, i);
        Tag := siBase + i;
      end;
    end;
    inc(i);
  until hRes <> S_OK;
  FMaxColumns := i - 1;

  AutoSizeAllColumns;
  IsUpdating := false;
end;

function TElShellTree.DeleteDefaultColumns: Integer;
var j, m : integer;
    ASection : TElHeaderSection;
begin
  j := 0;
  m := HeaderSections.Count;
  while j <= HeaderSections.Count -1 do
  begin
    ASection := HeaderSections[j];
    if InRange(siBase, siBase + {FMaxColumns}m, ASection.Tag) then
      HeaderSections.DeleteSection(ASection)
    else
      inc(j);
  end;
  Result := j;
end;

procedure TElShellTree.TriggerVirtualValueNeeded(Item : TElTreeItem;
    SectionIndex : Integer; VarType : integer; var Value : Variant);
begin
  Value := Variants.Unassigned;
end;

procedure TElShellTree.TriggerSortBegin;
begin
  if SortMode in [smClick, smAddClick] then
  begin
    if ShowColumns then
    begin
      if SortSection in [siMin..siMax] then
        SortType := ColumnSortTypes[SortSection]
      else
      if Sortsection = -1 then
        SortType := sstName
      else
        SortType := sstCustom;
    end
    else
    begin
      SortSection := 0;
      SortType := sstName;
    end;
  end;
end;

procedure TElShellTree.TriggerInplaceEditorNeeded(Item : TElTreeItem; SectionIndex : Integer;
  SupposedFieldType : TElFieldType; var Editor : TElTreeInplaceEditor);
begin
  if DefaultEditor then
  begin
    if (SectionIndex = -1) or (HeaderSections[SectionIndex].Tag = siName) then
    begin
      Editor := FEditor;
      exit;
    end;
  end;
  inherited;
end;

procedure TElShellTree.TriggerTryEditEvent(Item: TElTreeItem; SectionIndex : integer;
      var CellType: TElFieldType; var CanEdit: boolean);
begin
  CanEdit := (SectionIndex = -1) or (HeaderSections[SectionIndex].Tag = siName);
  if CanEdit then
    CanEdit := TElShellTreeItem(Item).CanRename;
end;


procedure TElShellTree.FileFiltersChange(Sender : TObject);
begin
  RefreshTree(nil, 1);
end;

procedure TElShellTree.OnValidateEdit(Sender : TOBject; var InputValid :
    boolean);
begin
  try
    TElShellTreeItem(FEditor.Item).SetDisplayName(FEditor.Editor.Text);
  except
    InputValid := false;
  end;
end;

procedure TElShellTree.SetSelectionPIDL(PIDL : PItemIDList);
var OwnPIDL : PItemIDList;
    APIDL   : PItemIDList;
    Item,
    Item1   : TElTreeItem;
    dFolder,
    iFolder : IShellFolder;
begin
  APIDL := PIDL;

  while (APIDL^.mkid.cb > 0) and
        (APIDL^.mkid.cb = FRootPIDL^.mkid.cb) and
        (CompareMem(@APIDL^.mkid.abID, @FRootPIDL^.mkid.abID, APIDL^.mkid.cb)) do
  begin
    APIDL := GetNextItemID(APIDL);
  end;
  // check whether PIDL is under root
  OwnPIDL := GetNextItemID(APIDL);
  if (OwnPIDL <> nil) and (OwnPIDL.mkid.cb = 0) then exit;
  Item := Items[0];
  if not Item.Expanded then
  begin
    if TElShellTreeItem(Item).IsFolder then
    begin
      OwnPIDL := TElShellTreeItem(Item).BuildFullPIDL;
      SHGetDesktopFolder(dFolder);
      if IsDesktopPIDL(OwnPIDL) then
        iFolder := dFolder
      else
        dFolder.BindToObject(OwnPIDL, nil, IID_IShellfolder, pointer(iFolder));
      FreeIDList(OwnPIDL);
      if iFolder <> nil then
        FillItemWithData(TElShellTreeItem(Item), iFolder, 0);
    end;
  end;
  while (APIDL^.mkid.cb > 0) do
  begin
    OwnPIDL := GetItemIDOnly(APIDL);
    Item1 := Item;

    Item := TElShellTreeItem(Item1).FindItemByPIDL(OwnPIDL);
    FreeIDList(OwnPIDL);
    if Item = nil then
    begin
      Item := Item1;
      break;
    end;
    if TElShellTreeItem(Item).IsFolder then
    begin
      OwnPIDL := TElShellTreeItem(Item).BuildFullPIDL;
      SHGetDesktopFolder(dFolder);
      if SUCCEEDED(dFolder.BindToObject(OwnPIDL, nil, IID_IShellfolder, pointer(iFolder))) then
      begin
        FreeIDList(OwnPIDL);
        FillItemWithData(TElShellTreeItem(Item), iFolder, 0);
        APIDL := GetNextItemID(APIDL);
        if APIDL = nil then
          break;
      end
      else
      begin
        FreeIDList(OwnPIDL);
        break;
      end;
    end
    else
      break;
  end;
  if Item <> nil then
  begin
    inherited ItemFocused := Item;
    EnsureVisible(Item);
  end;
end;

procedure TElShellTree.SetExpandRoot(Value: Boolean);
begin
  if FExpandRoot <> Value then
  begin
    FExpandRoot := Value;
    RefreshTree(nil, 1);
  end;
end;

function TElShellTree.BuildRootPIDL: PItemIDList;
begin
  Result := ClonePIDL(FRootPIDL);
end;

{$ifdef VCL_4_USED}
procedure TElShellTree.TriggerVirtualHintNeeded(Item: TElTreeItem;
  var Hint: TElFString);
begin
  if Hint = '' then
    Hint := (Item as TElShellTreeItem).GetHintText(FIFolder);
  if Assigned(FOnVirtualHintNeeded) then
    OnVirtualHintNeeded(Self, Item, Hint);
end;
{$endif}

function TElShellTree.DoGetPicture(Item: TElTreeItem): integer;
begin
  Result := (Item as TElShellTreeItem).GetPicture(FIFolder);
end;

function TElShellTreeItem.BuildFullPIDL : PItemIDList;
var PIDL : PItemIDList;
begin
  if Parent = nil then
    result := ClonePIDL(TElShellTree(FOwner).FRootPIDL)
  else
  begin
    PIDL := TElShellTreeItem(Parent).BuildFullPIDL;
    result := AppendPIDL(PIDL, FPIDL);
    FreeIDList(PIDL);
  end;
end;

function TElShellTreeItem.GetPicture(ParentFolder : IShellFolder): Integer;
var
    i      : Cardinal;
    // Icon   : IExtractIcon;
    APIDL  : PItemIDList;
begin
  result := -1;
  if FPIDL = nil then
  begin
    exit;
  end;
  i := SFGAO_FOLDER or SFGAO_COMPRESSED or SFGAO_HASSUBFOLDER or SFGAO_GHOSTED
                    or SFGAO_CANRENAME or SFGAO_FILESYSANCESTOR
                    or SFGAO_FILESYSTEM or SFGAO_REMOVABLE;

  if SUCCEEDED(ParentFolder.GetAttributesOf(1, FPIDL, i)) then
    FAttr := i
  else
    FAttr := 0;
  APIDL := BuildFullPIDL;
  if (FAttr and SFGAO_FOLDER) = SFGAO_FOLDER then
  begin
    Result := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, false);
    FImageIndex := Result;
    FStImageIndex := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, true);
    // ImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL);
    // StateImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL or GIL_OPENICON);
  end
  else
  begin
    Result := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, false);
    FImageIndex := Result;
    FStImageIndex := Result;
    //ImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL);
    //StateImageIndex := ImageIndex;
  end;
  FreeIDList(APIDL);
end;


procedure TElShellTreeItem.GetAttributes(iParentFolder : IShellFolder);
var
    i      : Cardinal;
//    APIDL  : PItemIDList;
begin
  if FPIDL = nil then
  begin
    exit;
  end;
  i := SFGAO_FOLDER or SFGAO_COMPRESSED or SFGAO_HASSUBFOLDER or SFGAO_GHOSTED
                    or SFGAO_CANRENAME or SFGAO_FILESYSANCESTOR
                    or SFGAO_FILESYSTEM or SFGAO_REMOVABLE;

  if SUCCEEDED(IParentFolder.GetAttributesOf(1, FPIDL, i)) then
    FAttr := i
  else
    FAttr := 0;
  (*
  if (FAttr and SFGAO_FILESYSANCESTOR) = SFGAO_FILESYSANCESTOR then
    GetWin32Data(iParentFolder);
  *)
  // iParentFolder.GetUIObjectOf(0, 1, FPIDL, IExtractIcon, nil, Pointer(Icon));
  // if Icon <> nil then
{
  begin
    APIDL := BuildFullPIDL;
    if (FAttr and SFGAO_FOLDER) = SFGAO_FOLDER then
    begin
      ImageIndex := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, false);
      StateImageIndex := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, true);
      // ImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL);
      // StateImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL or GIL_OPENICON);
    end
    else
    begin
      ImageIndex := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, false);
      StateImageIndex := ImageIndex;
      //ImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL);
      //StateImageIndex := ImageIndex;
    end;
    FreeIDList(APIDL);
  end;
}
  if TElShellTree(FOwner).FHighlightCompressed then
  begin
    if (FAttr and SFGAO_COMPRESSED) = SFGAO_COMPRESSED then
    begin
      Color := GetCompressedColor;
      ParentColors := false;
      UseBkColor := false;
    end;
  end;
  if (FAttr and SFGAO_GHOSTED) = SFGAO_GHOSTED then
    Cut := true;
end;

function TElShellTreeItem.GetDisplayName: string;
var iFolder : IShellFolder;
    str     : TStrRet;
begin
  if FDisplayName = '' then
  begin
    result := '';
    iFolder := ParentFolder;
    ZeroMemory(@Str, sizeof(Str));
    Str.uType := STRRET_CSTR;
    if SUCCEEDED(iFolder.GetDisplayNameOf(FPIDL, SHGDN_INFOLDER, str)) then
      result := StrRetToPas(Str, FPIDL); StrRetFree(str);
    iFolder := nil;
    FDisplayName := Result;
  end
  else
    result := FDisplayName;
end;

function TElShellTreeItem.GetFullName: string;
var StrRet : TStrRet;
begin
  ZeroMemory(@StrRet, sizeof(StrRet));
  StrRet.uType := STRRET_CSTR;
  result := '';
  if Parent = nil then
  begin
    if TElShellTree(FOwner).FRootFolder <> sfoCustom then
      GetPathFromPIDL(TElShellTree(FOwner).FRootPIDL, Result)
    else
      result := TElShellTree(FOwner).CustomRootFolder;
  end
  else
  if SUCCEEDED(ParentFolder.GetDisplayNameOf(FPIDL, SHGDN_NORMAL or SHGDN_FORPARSING {SHGDN_FORADDRESSBAR}, StrRet)) then
  begin
    Result := StrRetToPas(StrRet, FPIDL);
    StrRetFree(StrRet);
  end;
end;

function TElShellTreeItem.GetHasSubFolders: Boolean;
begin
  Result := (FAttr and SFGAO_HASSUBFOLDER) = SFGAO_HASSUBFOLDER;
end;

function TElShellTreeItem.GetIsFolder: Boolean;
begin
  Result := (FAttr and SFGAO_FOLDER) = SFGAO_FOLDER;
end;

function TElShellTreeItem.GetIsRemovable: Boolean;
begin
  Result := (FAttr and SFGAO_REMOVABLE) = SFGAO_REMOVABLE;
end;

function TElShellTreeItem.GetParentFolder: IShellFolder;
var PIDL    : PItemIDList;
    iFolder : IShellFolder;
begin
  if Parent <> nil then
    PIDL := TElShellTreeItem(Parent).BuildFullPIDL
  else
  begin
    PIDL := GetParentPIDL(TElShellTree(FOwner).FRootPIDL);
  end;
  result := nil;
  if IsDesktopPIDL(PIDL) then
  begin
    result := nil;
    SHGetDesktopFolder(result);
    exit;
  end;
  SHGetDesktopFolder(iFolder);
  if not SUCCEEDED(IFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(Result))) then
    result := nil;

  iFolder := nil;
  FreeIDList(PIDL);
end;

function TElShellTreeItem.GetPIDL: PItemIDList;
begin
  {if Parent = nil then
    Result := TElShellTree(FOwner).FRootPIDL
  else
  }  Result := FPIDL;
end;

function TElShellTreeItem.GetSize: Cardinal;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := 0
  else
  begin
    result := Win32FindData.nFileSizeLow;
  end;
end;

procedure TElShellTreeItem.SetDisplayName(const Value: string);
var iFolder : IShellFolder;
    NewPIDL : PItemIDList;
begin
  IFolder := GetParentFolder;
  if SUCCEEDED(IFolder.SetNameOf(FOwner.Handle, FPIDL, PWideChar(WideString(Value)), SHGDN_INFOLDER, NewPIDL)) then
  begin
    FDisplayName := Value;
    if NewPIDL <> nil then
    begin
      FreeIDList(FPIDL);
      FPIDL := ClonePIDL(GetOwnPIDL(NewPIDL));
      FreeIDList(NewPIDL);
    end;
  end
  else
    raise EElShellError.Create('Failed to rename ' + DisplayName);
end;

constructor TElShellTreeItem.Create(AOwner : TCustomElTree);
begin
  inherited;
  FPIDL := GetEmptyPIDL;
  FAttrAsString := '-';
  FComment :=  '-';
  FTypeName := '-';
  FSizeAsString := '-';
  FFileName := '-';
  Win32FindData := nil;
end;

destructor TElShellTreeItem.Destroy;
begin
  if Win32FindData <> nil then
    Dispose(Win32FindData);
  Win32FindData := nil;
  if FPIDL <> nil then
    FreeIDList(FPIDL);
  inherited;
end;

function TElShellTreeItem.GetCreationTime: TDateTime;
var ST : TSystemTime;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := 0
  else
  begin
    FileTimeToSystemTime(Win32FindData.ftCreationTime, ST);
    result := SystemTimeToDateTime(ST);
  end;
end;

function TElShellTreeItem.GetModificationTime: TDateTime;
var ST : TSystemTime;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := 0
  else
  begin
    FileTimeToSystemTime(Win32FindData.ftLastWriteTime, ST);
    result := SystemTimeToDateTime(ST);
  end;
end;

function TElShellTreeItem.GetLastAccessTime: TDateTime;
var ST : TSystemTime;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := 0
  else
  begin
    FileTimeToSystemTime(Win32FindData.ftLastAccessTime, ST);
    result := SystemTimeToDateTime(ST);
  end;
end;

procedure TElShellTreeItem.GetWin32Data(ParentFolder : IShellFolder);
var HSRec  : THandle;
    FN     : string;
    S      : string;
begin
  FIsValidFile := false;
  New(Win32FindData);
  FN := GetFullName;
  S  := ExtractFilePath(FN);
  System.Delete(S, 1, Length(ExtractFileDrive(S)));
  if S = '\' then
  begin
    S := ExtractFileDrive(FN);
    if Copy(S, 1, 2) = '\\' then
    begin
      ZeroMemory(Win32FindData, sizeof(Win32FindData^));
      Win32FindData^.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
      FFileName := S;
      exit;
    end
    else
    begin
      if GetDriveType(PChar(S)) in [1, DRIVE_REMOVABLE, DRIVE_REMOTE] then
      begin
        ZeroMemory(Win32FindData, sizeof(Win32FindData^));
        Win32FindData^.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
        FFileName := S;
        exit;
      end;
    end;
    FFileName := FN;
  end;
  HSRec := FindFirstFile(PChar(GetFullName), Win32FindData^);
  if HSRec = INVALID_HANDLE_VALUE then
  begin
    ZeroMemory(Win32FindData, sizeof(Win32FindData^));
  end
  else
  begin
    FIsValidFile := true;
    FindClose(HSRec);
  end;
end;

procedure TElShellTreeItem.CheckWin32FindData;
begin
  if Win32FindData = nil then
    if ((FAttr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM)
       // or ((FAttr and SFGAO_FILESYSANCESTOR) = SFGAO_FILESYSANCESTOR)
       then
      GetWin32Data(ParentFolder);
end;

function TElShellTreeItem.GetFileName: string;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := DisplayName
  else
  begin
    if FFileName <> '-' then
      result := FFileName
    else
    begin
      Result := StrPas(Win32FindData.cFileName);
      FFileName := Result;
    end;
  end;
end;

function TElShellTreeItem.GetSizeAsString: string;
var ASize : integer;
begin
  if FSizeAsString = '-' then
  begin
    case TElShellTree(Owner).SizeFormat of
      ssfAsIs :
        begin
          result := IntToStrFmt(Size) + ' b';
        end;
      ssfKb:
        begin
          ASize  := Round(Size / 1024);
          result := IntToStrFmt(ASize) + ' Kb';
        end;
      ssfAuto:
        begin
          ASize := Size;
          if ASize < 1024 then
            result := IntToStrFmt(ASize) + ' b'
          else
          if ASize < 1024*1024 * 10 then
            result := IntToStrFmt(Round(Size / 1024)) + ' Kb'
          else
            result := IntToStrFmt(Round(Size / (1024 * 1024))) + ' Mb';
        end;
    end;
    FSizeAsString := result;
  end
  else
    result := FSizeAsString;
end;

function TElShellTreeItem.GetTypeName: string;
var SFI : TSHFileInfo;
begin
  if FTypeName = '-' then
  begin
    SHGetFileInfo(PChar(FullName), 0, SFI, sizeof(SFI), SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES);
    FTypeName := StrPas(SFI.szTypeName);
  end;
  Result := FTypeName;
end;

function TElShellTreeItem.GetIsFileObject: Boolean;
begin
  Result := ((FAttr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM);
end;

function TElShellTreeItem.GetAttrAsString: string;
var IFolder  : IShellFolder;
    IFolder2 : IShellFolder2;
    IDetails : IShellDetails;
    sd       : TShellDetails;
begin
  result := '';
  if FAttrAsString <> '-' then
  begin
    result := FAttrAsString;
    exit;
  end;

  if IsDesktopPIDL(PIDL) then
  begin
    FAttrAsString := '';
    exit;
  end;
  if PIDL = nil then exit;

  IFolder := GetParentFolder;

  if not SUCCEEDED(IFolder.QueryInterface(IID_IShellFolder2, IFolder2)) then
  begin
    if SUCCEEDED(IFolder.QueryInterface(IID_IShellDetails, IDetails)) then
    begin
      if SUCCEEDED(IDetails.GetDetailsOf(PIDL, siAttr, sd)) then
        result := StrRetToPas(sd.str, PIDL); StrRetFree(sd.str);
    end;
  end
  else
  begin
    if SUCCEEDED(IFolder2.GetDetailsOf(PIDL, siAttr, sd)) then
    begin
      result := StrRetToPas(sd.str, PIDL); StrRetFree(sd.str);
    end;
  end;
  FAttrAsString := result;
  (*
  CheckWin32FindData;
  if Win32FindData <> nil then
  begin
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY = FILE_ATTRIBUTE_READONLY then
      result := result + 'R';
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN = FILE_ATTRIBUTE_HIDDEN then
      result := result + 'H';
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM = FILE_ATTRIBUTE_SYSTEM then
      result := result + 'S';
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE = FILE_ATTRIBUTE_ARCHIVE then
      result := result + 'A';
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED = FILE_ATTRIBUTE_COMPRESSED then
      result := result + 'A';
  end;
  *)
end;

function TElShellTreeItem.GetComment: string;
var IFolder  : IShellFolder;
    IFolder2 : IShellFolder2;
    IDetails : IShellDetails;
    sd       : TShellDetails;
    hres     : HResult;

begin
  if FComment <> '-' then
  begin
    result := FComment;
    exit;
  end;
  result := '';
  iFolder := ParentFolder;

  if not SUCCEEDED(iFolder.CreateViewObject(FOwner.Handle, IShellDetails, Pointer(IDetails))) then
  begin
    if not SUCCEEDED(iFolder.QueryInterface(IID_IShellFolder2, IFolder2)) then
    begin
      IDetails := TElShellDefaultColumns.Create as IShellDetails;
    end;
  end;

  if IFolder2 <> nil then
    hRes := IFolder2.GetDetailsOf(FPIDL, siComment, SD)
  else
    hRes := IDetails.GetDetailsOf(FPIDL, siComment, SD);
  if hRes = S_OK then
  begin
    result := StrRetToPas(SD.str, nil); StrRetFree(SD.str);
    FComment := Result;
  end;
end;

procedure TElShellTreeItem.Invalidate;
begin
  if FPIDL <> nil then
    FreeIDList(FPIDL);
  FPIDL := GetEmptyPIDL;
  FAttrAsString := '-';
  FComment :=  '-';
  FTypeName := '-';
  FSizeAsString := '-';
  FDisplayName:= '';
  FFileName := '-';

  if Win32FindData <> nil then
    Dispose(Win32FindData);
  Win32FindData := nil;
  FAttr := 0;
end;

function TElShellTreeItem.FindItemByPIDL(APIDL : PItemIDList): TElShellTreeItem;
var i : integer;
begin
  for i := 0 to Count - 1 do
  begin
    if CompareIDLists(APIDL, TElShellTreeItem(Children[i]).FPIDL) then
    begin
      Result := TElShellTreeItem(Children[i]);
      exit;
    end;
  end;
  Result := nil;
end;

function TElShellTreeItem.GetCanRename: Boolean;
begin
  Result := (FAttr and SFGAO_CANRENAME) = SFGAO_CANRENAME;
end;

{$ifdef VCL_4_USED}
function TElShellTreeItem.GetHintText(ParentFolder: IShellFolder): TElFString;
var
    QInfo  : IQueryInfo;
    aWP    : PWideChar;
    Malloc : IMalloc;
begin
  if SUCCEEDED(ParentFolder.GetUIObjectOf(Owner.Handle, 1, FPIDL, IQueryInfo, nil, Pointer(QInfo))) then
  begin
    QInfo.GetInfoTip(0, aWP);
    Result := aWP;
    SHGetMalloc(Malloc);
    if aWP <> nil then
      Malloc.Free(aWP);
  end
  else
    Result := '';
end;
{$endif}

procedure TElShellTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer);
begin
  inherited;
  if (Button = mbRight) and (not Dragging) and TElShellTree(Parent).UseSystemMenus then
  begin
    // HCol := 0;
    ShowSystemMenu(X, Y);
  end;
end;

procedure TElShellTreeView.MenuWndProc(var Message: TMessage);
var
  {$IFDEF VCL_4_USED}
  ICI : CMInvokeCommandInfo;
  {$ELSE}
  ICI : TCMInvokeCommandInfo;
  {$ENDIF}

begin
  inherited;
  if (Message.Msg = WM_EXITMENULOOP) and Assigned(iCtxMenu) then
  begin
    DestroyMenu(FBuiltinMenu);
    FBuiltinMenu := 0;
  end;
  if (Message.Msg = WM_COMMAND) and Assigned(iCtxMenu) then
  begin
    FillMemory(@ICI, sizeof(ICI), 0);
    ICI.cbSize := SizeOf(ICI);
    ICI.nShow  := SW_SHOWNORMAL;
    ICI.lpVerb := MakeIntResourceA(LOWORD(Message.wParam));
    if Integer(ICI.lpVerb) >= 0 then
      iCtxMenu.InvokeCommand(ICI);
  end;
end;

constructor TElShellTreeView.Create(AOwner : TComponent);
begin
  inherited;
  FMenuWnd := AllocateHWND(MenuWndProc);
end;

destructor TElShellTreeView.Destroy;
begin
  DeallocateHWND(FMenuWnd);
  iCtxMenu := nil;
  inherited;
end;

procedure TElShellTreeView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_APPS) and (Shift = []) and TElShellTree(FOwner).UseSystemMenus then
  begin
    ShowSystemMenu(0, 0);
    Key := 0;
  end;
  inherited;
end;

procedure TElShellTreeView.ShowSystemMenu(X, Y : integer);
var
  Item    : TElTreeItem;
  PIDL    : PItemIDList;
  iFolder : IShellFolder;
  P       : TPoint;

begin
  Item := FOwner.ItemFocused;//GetItemAt(X, Y, ItemPart, HCol);
  if (Item = nil) or (Item.Enabled) then
  begin
    if Item <> nil then
    begin
      PIDL := TElShellTreeItem(Item).PIDL;
      if PIDL <> nil then
      begin
        iCtxMenu := nil;
        iFolder  := TElShellTreeItem(Item).GetParentFolder;
        if SUCCEEDED(iFolder.GetUIObjectOf(Parent.Handle, 1, PIDL, IID_iContextMenu, nil, Pointer(iCtxMenu))) then
        begin
          FBuiltinMenu := CreatePopupMenu;
          try
            iCtxMenu.QueryContextMenu(FBuiltinMenu, 0, 0, $FFFFFFFF, CMF_NORMAL);
            P := ClientToScreen(Point(X, Y));
            TrackPopupMenu(FBuiltinMenu, TPM_LEFTALIGN, P.X, P.Y, 0, FMenuWnd, nil);
          finally
            // DestroyMenu(Menu);
            // iCtxMenu := nil;
          end;
        end;
      end;
    end;
  end;
end;

function TElShellTreeView.GetPopupMenu: TPopupMenu;
begin
  if TElShellTree(FOwner).UseSystemMenus then
    result := nil
  else
    Result := inherited GetPopupMenu;
end;


procedure TElShellComboBox.SetExplorerStyle(Value: Boolean);
begin
  if FExplorerStyle <> Value then
  begin
    FExplorerStyle := Value;
    RecreateWnd;
  end;
end;

procedure TElShellComboBox.FillItems;
var iFolder : IShellFolder;
    PIDL    : PItemIDList;
begin
  inherited Items.Clear;
  inherited Items.BeginUpdate;
  SHGetDesktopFolder(iFolder);
  PIDL := GetEmptyPIDL;
  FillCombo(iFolder, PIDL, 0);
  FreeIDList(PIDL);
  ItemIndex := 0;
  inherited Items.EndUpdate;
end;

destructor TElShellComboBox.Destroy;
begin
  if FEditor <> nil then
  begin
    FEditor.Free;
    FEditor := nil;
  end;
  if FSelectionPIDL <> nil then
    FreeIDList(FSelectionPIDL);
  inherited;
end;

procedure TElShellComboBox.FillCombo(BaseFolder : IShellFolder; BasePIDL :
    PItemIDList; Level : integer);

    function AddThisItem(iFolder : IShellFolder; PIDL : PItemIDList) : PElShellComboData;
    var ComboData : PElShellComboData;
        Str       : TStrRet;
        // Icon      : IExtractIcon;
        j         : Cardinal;
    begin
      ZeroMemory(@Str, sizeof(Str));
      Str.uType := STRRET_CSTR;

      New(ComboData);
      ComboData.PIDL := AppendPIDL(BasePIDL, PIDL);
      BaseFolder.GetDisplayNameOf(PIDL, SHGDN_INFOLDER or SHGDN_FORADDRESSBAR, str);
      ComboData.DisplayName := StrRetToPas(str, PIDL);
      StrRetFree(str);
      BaseFolder.GetDisplayNameOf(PIDL, SHGDN_NORMAL or SHGDN_FORPARSING, str);
      ComboData.FileName := StrRetToPas(str, PIDL);
      StrRetFree(str);
      j := SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR or SFGAO_FOLDER;
      if SUCCEEDED(BaseFolder.GetAttributesOf(1, PIDL, j)) then
        ComboData.Attr := j
      else
        ComboData.Attr := 0;

      // BaseFolder.GetUIObjectOf(0, 1, PIDL, IExtractIcon, nil, Pointer(Icon));
      // if Icon <> nil then
      begin
        ComboData.IconIdx := ShellIconCache.AddFromPIDL(ComboData.PIDL, GIL_FORSHELL, false);
        ComboData.OpenIconIdx := ShellIconCache.AddFromPIDL(ComboData.PIDL, GIL_FORSHELL, true);
      end;
      ComboData.Level := Level;
      result := ComboData;
    end;

    function CompareItems(Item1,
                          Item2: Pointer;
                          Cargo: Pointer): Integer;
    var ComboData1,
        ComboData2 : PElShellComboData;
        S1, S2     : string;
    begin
      ComboData1 := PElShellComboData(Item1);
      ComboData2 := PElShellComboData(Item2);
      if (ComboData1.Attr and SFGAO_FOLDER) <> (ComboData2.Attr and SFGAO_FOLDER) then
      begin
        if (ComboData2.Attr and SFGAO_FOLDER) = SFGAO_FOLDER then
          result := 1
        else
          result := -1
      end
      else
      if (ComboData1.Attr and (SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR)) <> (ComboData2.Attr and (SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR)) then
      begin
        if (ComboData2.Attr and (SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR)) <> 0 then
          result := 1
        else
          result := -1
      end
      else
      begin
        if (ComboData1.Attr and SFGAO_FILESYSTEM) <> 0 then
          S1 := ComboData1.FileName
        else
          S1 := ComboData1.DisplayName;
        if (ComboData2.Attr and SFGAO_FILESYSTEM) <> 0 then
          S2 := ComboData2.FileName
        else
          S2 := ComboData2.DisplayName;
        result := AnsiStrIComp(PChar(S1), PChar(S2));
      end;
    end;

    procedure SortPIDLList(PIDLList : TElList);
    begin
      PIDLList.Sort(@CompareItems, Self);
    end;

var PIDLList : TElList;
    List     : IEnumIDList;
    PIDL     : PItemIDList;
    Flags    : Cardinal;
    i        : integer;
    FAttr    : Cardinal;
    j        : Cardinal;
    k        : Cardinal;
    MyComPIDL: PItemIDList;
    dFolder,
    iFolder  : IShellFolder;
    ComboData: PElShellComboData;
begin
  PIDLList := TElList.Create;
  try
    if Level = 0 then
    begin
      // level 0 is "desktop"
      ComboData := AddThisItem(BaseFolder, BasePIDL);
      inherited Items.AddObject(ComboData.DisplayName, TObject(ComboData));
      FillCombo(BaseFolder, BasePIDL, Level + 1);
    end
    else
    begin
      Flags := SHCONTF_FOLDERS;
      if ShowHidden then
        Flags := Flags or SHCONTF_INCLUDEHIDDEN;

      if SUCCEEDED(BaseFolder.EnumObjects(Handle, Flags, List)) then
      begin
        j := 0;
        while SUCCEEDED(List.Next(1, PIDL, j)) and (j > 0) and (PIDL <> nil) do
        begin
          if FileSystemOnly then
          begin
            k := SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR;
            if SUCCEEDED(BaseFolder.GetAttributesOf(1, PIDL, k)) then
              FAttr := k
            else
              FAttr := 0;
            if ((FAttr and SFGAO_FILESYSTEM) <> SFGAO_FILESYSTEM) and
               ((FAttr and SFGAO_FILESYSANCESTOR) <> SFGAO_FILESYSANCESTOR) then
              Continue;
          end;
          PIDLList.Add(AddThisItem(BaseFolder, PIDL));
        end;
      end;
      SortPIDLList(PIDLList);
      if Level in [1, 2] then
        SHGetSpecialFolderLocation(0, CSIDL_DRIVES, MyComPIDL);

      for i := 0 to PIDLList.Count - 1 do
      begin
        if PIDLContainsAt(FSelectionPIDL, GetOwnPIDL(PElShellComboData(PIDLList[i]).PIDL), Level) then
        begin
          ComboData := PIDLList[i];
          inherited Items.AddObject(ComboData.DisplayName, TObject(ComboData));
          SHGetDesktopFolder(dFolder);
          dFolder.BindToObject(PElShellComboData(PIDLList[i]).PIDL, nil, IShellFolder, Pointer(iFolder));
          FillCombo(iFolder, PElShellComboData(PIDLList[i]).PIDL, Level + 1);
        end
        else
        if (Level = 2) and CompareIDLists(BasePIDL, MyComPIDL) then
        begin
          ComboData := PIDLList[i];
          inherited Items.AddObject(ComboData.DisplayName, TObject(ComboData));
        end
        else
        if (Level = 1) and CompareIDLists(PElShellComboData(PIDLList[i]).PIDL, MyComPIDL) then
        begin
          ComboData := PIDLList[i];
          inherited Items.AddObject(ComboData.DisplayName, TObject(ComboData));
          SHGetDesktopFolder(dFolder);
          dFolder.BindToObject(MyComPIDL, nil, IShellFolder, Pointer(iFolder));
          FillCombo(iFolder, MyComPIDL, Level + 1);
        end
        else
        if (Level = 0) or (Level = 1) then
        begin
          ComboData := PIDLList[i];
          inherited Items.AddObject(ComboData.DisplayName, TObject(ComboData));
        end;
      end;
    end;
  finally
    PIDLList.Free;
  end;
end;

procedure TElShellComboBox.CreateWnd;
begin
  inherited;
  FillItems;
end;

procedure TElShellComboBox.SetShowHidden(Value: Boolean);
begin
  if FShowHidden <> Value then
  begin
    FShowHidden := Value;
    FillItems;
  end;
end;

procedure TElShellComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style  and (not CBS_DROPDOWN) or CBS_OWNERDRAWFIXED or CBS_DROPDOWNLIST and (not CBS_HASSTRINGS);
  (*
  if ExplorerStyle then
    Params.Style := Params.Style or CBS_DROPDOWN and (not CBS_DROPDOWNLIST)
  else
    Params.Style := Params.Style or CBS_DROPDOWNLIST and (not CBS_DROPDOWN);
  *)
end;

function TElShellComboBox.GetItemWidth(Index: Integer): Integer;
var
  S: string;
begin
  S := PElShellComboData(inherited Items.Objects[Index]).DisplayName + 'W';
  Result := Canvas.TextWidth(S) + (PElShellComboData(inherited Items.Objects[Index]).Level + 1) * GetSystemMetrics(SM_CXSMICON);
end;

procedure TElShellComboBox.SetFileSystemOnly(Value: Boolean);
begin
  if FFileSystemOnly <> Value then
  begin
    FFileSystemOnly := Value;
    FillItems;
  end;
end;

constructor TElShellComboBox.Create(AOwner : TComponent);
begin
  inherited;
  FExplorerStyle := true;
  FSelectionPIDL := GetEmptyPIDL;
  Style:= csOwnerDrawFixed;     //ssn   default csOwnerDrawVariable     ???
  inherited ItemHeight := Max(Abs(Font.Height) + 2 + GetSystemMetrics(SM_CXEDGE) * 2, Max(ItemHeight, GetSystemMetrics(SM_CYSMICON)));
end;

procedure TElShellComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  inherited ItemHeight := Max(Abs(Font.Height) + 2 + GetSystemMetrics(SM_CXEDGE) * 2, Max(ItemHeight, GetSystemMetrics(SM_CYSMICON)));
end;

procedure TElShellComboBox.DrawItem(Index: Integer; Rect: TRect; State:
    {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
var S : string;
    iw,
    ih : integer;
    ComboData : PElShellComboData;
    Icon : HICON;
    Flags,
    Idx  : Cardinal;
    R    : TRect;
begin
  ComboData := PElShellComboData((inherited Items).Objects[Index]);
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect);

  if (WindowFromDC(Canvas.Handle) <> Handle) then
    inc(Rect.Left, ComboData.Level * GetSystemMetrics(SM_CXSMICON));

  iw := ShellIconCache.SmallImages.Width;
  ih := ShellIconCache.SmallImages.Height;

  if PIDLStartsWith(FSelectionPIDL, ComboData.PIDL) then
    Idx := ComboData.OpenIconIdx
  else
    Idx := ComboData.IconIdx;
  if odSelected in State then
  begin
    Flags := ILD_SELECTED;
    Canvas.Brush.Color := clHighlight;
    ShellIconCache.SmallImages.BlendColor := clHighlight;
  end
  else
  begin
    Flags := ILD_NORMAL;
  end;
  Icon := ImageList_GetIcon(ShellIconCache.SmallImages.Handle, Idx, Flags);
  DrawIconEx(Canvas.Handle, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - ih) div 2, Icon, Min(ItemHeight, ih), Min(ItemHeight, iw), 0, 0, DI_NORMAL);
  DestroyIcon(Icon);
  Inc(Rect.Left, iw + 4);

  S := ComboData.DisplayName;
  Canvas.Brush.Color := clHighlight;
  R := Rect;
  DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_VCENTER or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE or DT_CALCRECT);
  R.Top := Rect.Top;
  R.Bottom := Rect.Bottom;
  InflateRect(R, 2, 0);
  if ([odSelected, odFocused] * State <> []) then
    Canvas.FillRect(R)
  else
    Canvas.Brush.Style := bsClear;

  DrawText(Canvas.Handle, PChar(S), Length(S), Rect, DT_VCENTER or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE);

  if odFocused in State then
    Canvas.DrawFocusRect(R);
end;

procedure TElShellComboBox.CNCommand(var Msg: TWMCommand);
begin
  if Msg.NotifyCode = CBN_SELENDOK then
  begin
    inherited;
    if ItemIndex < inherited Items.Count then
      SetSelection(PElShellComboData(inherited Items.Objects[ItemIndex]).PIDL);
  end
  else
    inherited;
end;

procedure TElShellComboBox.SetSelection(PIDL : PItemIDList);
var i : integer;
    b : boolean;
    MyComPIDL : PItemIDList;
begin
  // if GetPathFromPIDL(PIDL, S) and (S <> '') then
  begin
    FreeIDList(FSelectionPIDL);
    FSelectionPIDL := ClonePIDL(PIDL);

    b := false;
    i := 0;
    SHGetSpecialFolderLocation(0, CSIDL_DRIVES, MyComPIDL);
    while i < inherited Items.Count do
    begin
      if not b then
      begin
        if CompareIDLists(PIDL, PElShellComboData(Inherited Items.Objects[i]).PIDL) then
        begin
          b := true;
          // l := PElShellComboData(Inherited Items.Objects[i]).Level;
          ItemIndex := i;
        end;
        inc(i);
      end
      else
      begin
        if (PElShellComboData(Inherited Items.Objects[i]).Level > 2) or
           (PElShellComboData(Inherited Items.Objects[i]).Level = 2) and
           (not PIDLContainsAt(PElShellComboData(Inherited Items.Objects[i]).PIDL, MyComPIDL, 1)) then
          inherited Items.Delete(i)
        else
          inc(i);
      end;
    end;
    FreeIDList(MyComPIDL);

    if b then exit;

    FillItems;
    for i := 0 to inherited Items.Count - 1 do
    begin
      if CompareIDLists(PIDL, PElShellComboData(Inherited Items.Objects[i]).PIDL) then
      begin
        ItemIndex := i;
        exit;
      end;
    end;
  end;
end;

procedure TElShellComboBox.CNDrawItem(var Msg: TWMDrawItem);
var State : {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText
    end;
    if (Integer(itemID) >= 0) and (Integer(itemID) < inherited Items.Count) then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);
    Canvas.Handle := 0;
  end;
end;

function TElShellComboBox.GetSelection: PItemIDList;
begin
  Result := FSelectionPIDL;
end;

procedure TElShellComboBox.ShowEdit;
var R : TRect;
begin
  FEditor := TElShellComboEdit.Create(nil);
  FEditor.SetBounds(GetSystemMetrics(SM_CXEDGE) + GetSystemMetrics(SM_CXSMICON) + 3,
                    GetSystemMetrics(SM_CYEDGE),
                    Width - GetSystemMetrics(SM_CXVSCROLL) -
                            GetSystemMetrics(SM_CXSMICON) - 4 -
                            GetSystemMetrics(SM_CXEDGE) * 2 + 1,
                    Height - GetSystemMetrics(SM_CYEDGE) * 2);

  TElShellComboEdit(FEditor).BorderStyle := bsNone;
  TElShellComboEdit(FEditor).Font := Font;
  FEditor.Text := '';
  if (ItemIndex >= 0) and (ItemIndex < inherited Items.Count) then
  begin
    if PElShellComboData(inherited Items.Objects[ItemIndex]).Attr and SFGAO_FILESYSTEM <> 0 then
      FEditor.Text := PElShellComboData(inherited Items.Objects[ItemIndex]).FileName
    else
      FEditor.Text := PElShellComboData(inherited Items.Objects[ItemIndex]).DisplayName;
  end;
  FEditor.Parent := Self;
  R.Left := GetSystemMetrics(SM_CXEDGE) + 1;
  R.Top := (FEditor.Height - Abs(Font.Height) + 2) div 2 - 2;
  R.Bottom := R.Top + Abs(Font.Height) + 2;
  R.Right := FEditor.Width;
  FEditor.Perform(EM_SETRECT, 0, Integer(@R));
  FEditor.SetFocus;
  FEditor.SelectAll;
end;

procedure TElShellComboBox.KeyPress(var Key: Char);
begin
  if ExplorerStyle and (Key >= #32) then
  begin
    ShowEdit;
    FEditor.Text := Key;
    FEditor.SelStart := 1;
    FEditor.SelLength := 0;
    Key := #0;
  end
  else
    inherited;
end;

procedure TElShellComboBox.DropDown;
begin
  CancelEdit;
  inherited;
end;

procedure TElShellComboBox.CancelEdit;
begin
  if FEditor <> nil then
  begin
    FEditor.Free;
    FEditor := nil;
    SetFocus;
  end;
end;

procedure TElShellComboBox.WMDeleteItem(var Message: TMessage);
var St : PDeleteItemStruct;
begin
  St := PDeleteItemStruct(Message.LParam);
  if (St.itemData <> 0) then
    Dispose(PElShellComboData(St.ItemData));
  St.ItemData := 0;
  inherited;
end;

procedure TElShellComboBox.AcceptEdit;
var PIDL : PItemIDList;
begin
  if FEditor <> nil then
  begin
    PIDL := GetPIDLFromPath(FEditor.Text);
    if PIDL <> nil then
    begin
      SetSelection(PIDL);
      FreeIDList(PIDL);
    end;
    FEditor.Free;
    FEditor := nil;
    SetFocus;
    Change;
  end;
end;

procedure TElShellComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ExplorerStyle and
   (X < (Width - GetSystemMetrics(SM_CXEDGE) - GetSystemMetrics(SM_CXVSCROLL))) and
   (X > GetSystemMetrics(SM_CXEDGE)  + 4 + GetSystemMetrics(SM_CXSMICON)) then
    inherited Cursor := crIBeam
  else
    inherited Cursor := Cursor;
  inherited;
end;

procedure TElShellComboBox.WMLButtonDown(var Message: TWMMouse);
begin
  if ExplorerStyle and
     (Message.XPos < (Width - GetSystemMetrics(SM_CXEDGE) - GetSystemMetrics(SM_CXVSCROLL))) and
     (Message.XPos > GetSystemMetrics(SM_CXEDGE)  + 4 + GetSystemMetrics(SM_CXSMICON)) then
  begin
    ShowEdit;
    Exit;
  end;
  if Assigned(FEditor) then
    CancelEdit;
  inherited;
end;

procedure TElShellComboBox.SetCursor(Value: TCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    inherited Cursor := Cursor;
  end;
end;

procedure TElShellComboBox.SetStyle(Value: TComboBoxStyle);
begin
  if (FStyle <> Value) and (Value in [Low(TComboBoxStyle)..High(TComboBoxStyle)]) then
  begin
    FStyle := Value;
  end;
end;

procedure TElShellComboBox.DestroyWnd;
begin
  inherited Items.Clear;
  inherited;
end;


procedure TElShellList.SetCustomFolder(const Value: string);
begin
  if FCustomFolder <> Value then
  begin
    FCustomFolder := Value;
    if FFolder = sfoCustom then
    begin
      if CustomFolder = '' then
        SetFolder(sfoDesktop)
      else
        SetPIDL(GetFolderPIDL(sfoCustom, CustomFolder));
    end;
  end;
end;

procedure TElShellList.SetDefaultColumns(Value: Boolean);
var
  MainTreeCol: integer;
begin
  if FDefaultColumns <> Value then
  begin
    FDefaultColumns := Value;
    MainTreeCol := DeleteDefaultColumns;
    if FDefaultColumns then
    begin
      AddDefaultColumns;
      FMainTreeCol := MainTreeCol;
    end;
  end;
end;

function TElShellList.GetFocusedDisplayName: string;
var PIDL : PItemIDList;
begin
  PIDL := GetFocusedPIDL;
  if (PIDL <> nil) and (GetPathFromPIDL(PIDL, Result)) then
  begin

  end
  else
    result := '';
end;

function TElShellList.GetFocusedPIDL: PItemIDList;
begin
  if ItemFocused <> nil then
    result := TElShellListItem(ItemFocused).BuildFullPIDL
  else
    result := nil;
end;

function TElShellList.GetItemFocused: TElShellListItem;
begin
  Result := TElShellListItem(inherited ItemFocused);
end;

procedure TElShellList.SetHighlightCompressed(Value: Boolean);
begin
  if FHighlightCompressed <> Value then
  begin
    FHighlightCompressed := Value;
    RefreshList;
  end;
end;

procedure TElShellList.SetItemFocused(Value: TElShellListItem);
begin
  inherited ItemFocused := Value;
end;

procedure TElShellList.SetFolder(Value: TShellFolders);
begin
  if FFolder <> Value then
  begin
    FFolder := Value;
    if (FFolder = sfoCustom) and (CustomFolder = '') and (not (csLoading in ComponentState)) then
      FFolder := sfoDesktop;
    SetPIDL(GetFolderPIDL(Value, CustomFolder));
  end;
end;

procedure TElShellList.SetShowHidden(Value: Boolean);
begin
  if FShowHidden <> Value then
  begin
    FShowHidden := Value;
    RefreshList;
  end;
end;

procedure TElShellList.SetSizeFormat(Value: TElShellSizeFormat);
var i : integer;
begin
  if FSizeFormat <> Value then
  begin
    FSizeFormat := Value;
    for i := 0 to FALlList.Count - 1 do
      TElShellTreeItem(FAllList[i]).FSizeAsString := '-';
    Invalidate;
  end;
end;

procedure TElShellList.SetSortModifiers(Value: TElShellSortModifiers);
begin
  if FSortModifiers <> Value then
  begin
    FSortModifiers := Value;
    if SortMode in [smAdd, smAddClick] then
      Sort(true);
  end;
end;

procedure TElShellList.SetSortType(Value: TElShellSortType);
begin
  if FSortType <> Value then
  begin
    FSortType := Value;
    if SortMode in [smAdd, smAddClick] then
      Sort(true);
  end;
end;

procedure TElShellList.SetPIDL(PIDL : PItemIDList);
var iFolder : IShellFolder;
    hRes : HRESULT;
begin
  if FRootPIDL <> nil then
    FreeIDList(FRootPIDL);
  FRootPIDL := PIDL;
  SHGetDesktopFolder(iFolder);
  if (Folder <> sfoDesktop) and (Folder <> sfoDesktopExpanded) then
  begin
    hRes := iFolder.BindToObject(FRootPIDL, nil, IID_IShellFolder, Pointer(FIFolder));
    if not SUCCEEDED(hRes) then
      raise Exception.Create('Failed to get IShellFolder for specified root');
    iFolder := nil;
  end
  else
    FIFolder := iFolder;

  Items.Clear;
  RefreshList;
end;

procedure TElShellList.AddDefaultColumns;
var i : integer;
    IFolder  : IShellFolder;
    IFolder2 : IShellFolder2;
    IDetails : IShellDetails;
    sd       : TShellDetails;
    hres     : HResult;
    ASection : TElHeaderSection;
begin
  IsUpdating := true;

  iFolder := FIFolder;
  if not SUCCEEDED(iFolder.CreateViewObject(Handle, IShellDetails, Pointer(IDetails))) then
  begin
    if not SUCCEEDED(iFolder.QueryInterface(IID_IShellFolder2, IFolder2)) then
    begin
      IDetails := TElShellDefaultColumns.Create as IShellDetails;
    end;
  end;

  i := 0;
  repeat
    if IFolder2 <> nil then
      hRes := IFolder2.GetDetailsOf(nil, i, SD)
    else
      hRes := IDetails.GetDetailsOf(nil, i, SD);

    if hRes = S_OK then
    begin
      ASection := HeaderSections.InsertSection(i);
      with ASection do
      begin
        Text := StrRetToPas(SD.str, nil); StrRetFree(SD.str);
        AutoSize := false;
        FieldType := sftCustom;
        Editable := i = siName;
        Visible := InRange(siMin, siMax, i);
      end;
      ASection.Tag := siBase + i;
    end;

    inc(i);
  until hRes <> S_OK;

  FMaxColumns := i - 1;

  AutoSizeAllColumns;
  IsUpdating := false;
end;

function TElShellList.DeleteDefaultColumns: Integer;
var j, m : integer;
    ASection : TElHeaderSection;
begin
  j := 0;
  m := HeaderSections.Count;
  while j <= HeaderSections.Count - 1 do
  begin
    ASection := HeaderSections[j];
    if InRange(siBase, siBase + {FMaxColumns}m, ASection.Tag) then
{    if siBase = ASection.Tag then}
      HeaderSections.DeleteSection(ASection)
    else
      inc(j);
  end;
  Result := j;
end;

procedure TElShellList.RefreshList;
var
    i        : Cardinal;
    j,
    FAttr: cardinal;
    List : IEnumIDList;
    Flags: Cardinal;
    PIDL : PItemIDList;
    Child: TElShellListItem;
    S    : String;
    str  : TStrRet;
    chF  : IShellFolder;
    b    : boolean;
    MainTreeCol: integer;
begin
  IsUpdating := true;
  try

    b := ShowColumns;
    if DefaultColumns then
    begin
      MainTreeCol := DeleteDefaultColumns;
      AddDefaultColumns;
      FMainTreeCol := MainTreeCol;
      ShowColumns := b;
    end;

    Flags := SHCONTF_FOLDERS or SHCONTF_NONFOLDERS;
    if ShowHidden then
      Flags := Flags or SHCONTF_INCLUDEHIDDEN;
    List := nil;
    // get enumerator to walk through object
    if SUCCEEDED(FIFolder.EnumObjects(Handle, Flags, List)) then
    begin
      j := 0;
      chF := nil;
      PIDL := nil;

      // invalidate all items. We will validate them as the shell provides right PIDLs
      // and then we'll remove all items, that will remain invalid
      i := 0;
      while i < Cardinal(Items.Count) do
      begin
        TElShellTreeItem(Items[i]).FValid := false;
        inc(i);
      end;
      // first grab all child PIDLs
      while SUCCEEDED(List.Next(1, PIDL, j)) and (j > 0) and (PIDL <> nil) do
      begin
        if FileSystemOnly then
        begin
          i := SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR;
          if SUCCEEDED(FIFolder.GetAttributesOf(1, PIDL, i)) then
            FAttr := i
          else
            FAttr := 0;
          if ((FAttr and SFGAO_FILESYSTEM) <> SFGAO_FILESYSTEM) and
             ((FAttr and SFGAO_FILESYSANCESTOR) <> SFGAO_FILESYSANCESTOR) then
          begin
            FreeIDList(PIDL);
            PIDL := nil;
            Continue;
          end;
        end;

        ZeroMemory(@Str, sizeof(Str));
        Str.uType := STRRET_CSTR;

        FIFolder.GetDisplayNameOf(PIDL, SHGDN_INFOLDER or SHGDN_FORADDRESSBAR, str);
        S := StrRetToPas(str, PIDL);
        StrRetFree(str);
        if NameFiltered(S, FIFolder, PIDL) then
        begin
          Child := FindItemByPIDL(PIDL);
          if Child = nil then
          begin
            Child := TElShellListItem(Items.AddItem(nil));
            FreeIDList(Child.FPIDL);
          end
          else
          begin
            Child.Invalidate;
            if Child.FPIDL <> nil then
              FreeIDList(Child.FPIDL);
          end;
          Child.FPIDL := PIDL;
          Child.GetAttributes(FIFolder);

          Child.FValid := true;
          DoItemAdded(S, FIFolder, PIDL, Child);
        end
        else
        begin
          Child := FindItemByPIDL(PIDL);
          if Child <> nil then
            Items.DeleteItem(Child);
          FreeIDList(PIDL);
          PIDL := nil;
        end;
      end;

      // now check for invalid tree items and remove them
      i := 0;
      while i < Cardinal(Items.Count) do
      begin
        if TElShellTreeItem(Items[i]).FValid then
          inc(i)
        else
          Items.DeleteItem(Items[i]);
      end;
    end;
    Sort(true);

    List := nil;
  finally
    IsUpdating := false;
  end;
end;

procedure TElShellList.TriggerSortBegin;
begin
  if SortMode in [smClick, smAddClick] then
  begin
    if ShowColumns then
    begin
      if SortSection in [siMin..siMax] then
        SortType := ColumnSortTypes[SortSection]
      else
      if Sortsection = -1 then
        SortType := sstName
      else
        SortType := sstCustom;
    end
    else
    begin
      SortSection := 0;
      SortType := sstName;
    end;
  end;
end;

procedure TElShellList.TriggerTryEditEvent(Item: TElTreeItem; SectionIndex :
    integer; var CellType: TElFieldType; var CanEdit: boolean);
begin
  if CanEdit then
  begin
    CanEdit := HeaderSections[SectionIndex].Tag = siName;
    if CanEdit then
      CanEdit := TElShellListItem(Item).CanRename;
  end
end;

procedure TElShellList.TriggerVirtualTextNeeded(Item : TElTreeItem;
    SectionIndex : Integer; var Text : TElFString);
var Index : integer;
    IFolder  : IShellFolder;
    IFolder2 : IShellFolder2;
    IDetails : IShellDetails;
    sd       : TShellDetails;
    hres     : HResult;
begin
  if (SectionIndex <> -1) and (HeaderSections.Count > SectionIndex) then
    Index := HeaderSections[SectionIndex].Tag
  else
    Index := siName;
  if (Index = siName) or InRange(siBase, siBase + FMaxColumns, Index) then
  begin
    if (Index = siName) or (Index = siName + siBase) then
      Text := TElShellListItem(Item).DisplayName
    else
    begin
      Text := '';
      iFolder := FIFolder;

      if not SUCCEEDED(iFolder.CreateViewObject(Handle, IShellDetails, Pointer(IDetails))) then
      begin
        if not SUCCEEDED(iFolder.QueryInterface(IID_IShellFolder2, IFolder2)) then
        begin
          IDetails := TElShellDefaultColumns.Create as IShellDetails;
        end;
      end;

      ZeroMemory(@SD, sizeof(SD));
      SD.fmt := STRRET_CSTR;
      if IFolder2 <> nil then
        hRes := IFolder2.GetDetailsOf(TElShellListItem(Item).PIDL, Index - siBase, SD)
      else
        hRes := IDetails.GetDetailsOf(TElShellListItem(Item).PIDL, Index - siBase, SD);
      if hRes = S_OK then
      begin
        Text := StrRetToPas(SD.str, nil);
        StrRetFree(SD.str);
      end;
    end;
  end
  else
    inherited;
end;

procedure TElShellList.TriggerVirtualValueNeeded(Item : TElTreeItem;
    SectionIndex : Integer; VarType : integer; var Value : Variant);
begin
  Value := Unassigned;
end;

procedure TElShellList.SetFileSystemOnly(Value: Boolean);
begin
  if FFileSystemOnly <> Value then
  begin
    FFileSystemOnly := Value;
    RefreshList;
  end;
end;

function TElShellList.FindItemByPIDL(APIDL : PItemIDList): TElShellListItem;
var i : integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    if CompareIDLists(APIDL, TElShellListItem(Items[i]).FPIDL) then
    begin
      Result := TElShellListItem(Items[i]);
      exit;
    end;
  end;
  Result := nil;
end;

function TElShellList.NameFiltered(S : String; ShellFolder : IShellFolder;
    RelPIDL : PItemIDList): Boolean;
var i : integer;
begin
  result := true;
  for i := 0 to FFileFilters.Count - 1 do
  begin
    if not FileNameLike(S, FFileFilters[i]) then
    begin
      result := false;
      break;
    end
  end;
  if Assigned(FOnFilterItem) then
    FOnFilterItem(Self, S, ShellFolder, RelPIDL, result);
end;

function TElShellList.GetFileFilters: TStrings;
begin
  Result := FFileFilters;
end;

procedure TElShellList.SetFileFilters(const Value: TStrings);
begin
  FFileFilters.Assign(Value);
end;

constructor TElShellList.Create(AOwner : TComponent);
begin
  inherited;
  Images := ShellIconCache.SmallImages;
  VirtualityLevel := vlTextAndStyles;
  FHeader.InvertSortArrows := true;
  FFolder  := sfoDesktop;
  FShowHidden  := true;
  FFileFilters := TStringList.Create;
  FFileFilters.OnChange := FileFiltersChange;
  FSortType := sstName;
  FSortMode := smAddClick;
  FSortModifiers := [ssmFoldersFirst, ssmExecutablesFirst];
  FHighlightCompressed := true;
  FDefaultEditor := true;
  ShowRootButtons := false;
  ShowLines := false;
  ShowButtons := true;
  ShowEmptyImages := true;
  FEditor := TElTreeInplaceEdit.Create(nil);
  inherited SortType := stCustom;
end;

destructor TElShellList.Destroy;
begin
  FEditor.Free;
  FIFolder := nil;
  // Items.Clear;
  inherited;
  FFileFilters.Free;
end;

procedure TElShellList.FileFiltersChange(Sender : TObject);
begin
  RefreshList;
end;

procedure TElShellList.CreateHandle;
var PIDL : PItemIDList;
begin
  inherited;
  PIDL := GetFolderPIDL(FFolder, CustomFolder);
  SetRootPIDL(PIDL);
  FreeIDList(PIDL);
end;

function TElShellList.CreateItems: TElTreeItems;
begin
  result := inherited CreateItems;
  result.ItemClass := TElShellListItem;
end;

function TElShellList.CreateView: TElTreeView;
begin
  result := TElShellListView.Create(Self);
end;  { CreateView }

procedure TElShellList.SetRootPIDL(PIDL : PItemIDList);
var iFolder : IShellFolder;
    hRes : HRESULT;
begin
  if FRootPIDL <> nil then
    FreeIDList(FRootPIDL);
  FRootPIDL := ClonePIDL(PIDL);
  iFolder := nil;
  SHGetDesktopFolder(iFolder);
  if (CalcPIDLSize(PIDL) <> 2) and (Folder = sfoDesktop) then
    FFolder := sfoCustom
  else
  if (CalcPIDLSize(PIDL) = 2) and (Folder <> sfoDesktop) and (Folder <> sfoDesktopExpanded) then
    FFolder := sfoDesktop;
  if (Folder <> sfoDesktop) and (Folder <> sfoDesktopExpanded) and (CalcPIDLSize(PIDL) <> 2) then
  begin
    hRes := iFolder.BindToObject(FRootPIDL, nil, IID_IShellFolder, pointer(FIFolder));
    if not SUCCEEDED(hRes) then
      raise Exception.Create('Failed to get IShellFolder for specified root');
    iFolder := nil;
  end
  else
    FIFolder := iFolder;
  FAttr := SFGAO_FOLDER or SFGAO_COMPRESSED or SFGAO_HASSUBFOLDER or SFGAO_GHOSTED
                          or SFGAO_CANRENAME or SFGAO_FILESYSANCESTOR
                          or SFGAO_FILESYSTEM or SFGAO_REMOVABLE;

  FIFolder.GetAttributesOf(1, FRootPIDL, Cardinal(FAttr));
  RefreshList;
end;

procedure TElShellList.DoCompareItems(Item1, Item2: TElTreeItem; var res:
    integer);
var S1, S2 : string;
    D1, D2 : TDateTime;
    B1, B2 : boolean;
    I1, I2 : Cardinal;
begin
  if SortType = sstCustom then
    inherited
  else
  begin
    if ssmFoldersFirst in SortModifiers then
    begin
      B1 := TElShellListItem(Item1).IsFolder;
      B2 := TElShellListItem(Item2).IsFolder;
      if B1 <> b2 then
      begin
        if b2 then
          res := 1
        else
          res := -1;
        exit;
      end
      else
      begin
        B1 := TElShellListItem(Item1).IsFileObject;
        B2 := TElShellListItem(Item2).IsFileObject;
        if B1 <> B2 then
        begin
          if b2 then
            res := 1
          else
            res := -1;
          exit;
        end;
      end;
    end;

    if ssmExecutablesFirst in SortModifiers then
    begin
      if (TElShellListItem(Item1).IsFileObject and
          TElShellListItem(Item2).IsFileObject) then
      begin
        B1 := TElShellListItem(Item1).IsFolder;
        B2 := TElShellListItem(Item2).IsFolder;
        if not (b1 or b2) then
        begin
          S1 := Uppercase(ExtractFileExt(TElShellListItem(Item1).FileName));
          S2 := Uppercase(ExtractFileExt(TElShellListItem(Item2).FileName));
          b1 := (S1 = '.EXE') or (S1 = '.DLL');
          b2 := (S2 = '.EXE') or (S2 = '.DLL');
          if B1 <> b2 then
          begin
            if B2 then
              res := 1
            else
              res := -1;
            exit;
          end;
        end;
      end;
    end;

    case SortType of
      sstName:
        begin
          S1 := TElShellListItem(Item1).FileName;
          S2 := TElShellListItem(Item2).FileName;
          res := AnsiStrIComp(PChar(S1), PChar(S2));
        end;
      sstExt:
        begin
          S1 := ExtractFileExt(TElShellListItem(Item1).FileName);
          S2 := ExtractFileExt(TElShellListItem(Item2).FileName);
          res := AnsiStrIComp(PChar(S1), PChar(S2));
        end;
      sstSize:
        begin
          I1 := TElShellListItem(Item1).Size;
          I2 := TElShellListItem(Item2).Size;
          if I1 > I2 then
            res := 1
          else
          if I1 < I2 then
            res := -1
          else
          if I1 = I2 then
            res := 0;
        end;
      sstCreationDate,
      sstModifyDate,
      sstAccessDate:
        begin
          case SortType of
            sstModifyDate:
              begin
                D1 := TElShellListItem(Item1).ModificationTime;
                D2 := TElShellListItem(Item2).ModificationTime;
              end;
            sstAccessDate:
              begin
                D1 := TElShellListItem(Item1).LastAccessTime;
                D2 := TElShellListItem(Item2).LastAccessTime;
              end;
            else
              begin
                D1 := TElShellListItem(Item1).CreationTime;
                D2 := TElShellListItem(Item2).CreationTime;
              end;
          end;
          if D1 > D2 then
            res := 1
          else
          if D1 < D2 then
            res := -1
          else
          if D1 = D2 then
            res := 0;
        end;
    end;
  end;
end; { DoCompareItems }

procedure TElShellList.SetSelectionPIDL(PIDL : PItemIDList);
begin
  ItemFocused := FindItemByPIDL(PIDL);
end;

function TElShellList.BuildRootPIDL: PItemIDList;
begin
  Result := ClonePIDL(FRootPIDL);
end;

{$ifdef VCL_4_USED}
procedure TElShellList.TriggerVirtualHintNeeded(Item: TElTreeItem;
  var Hint: TElFString);
begin
  if Hint = '' then
    Hint := (Item as TElShellListItem).GetHintText(FIFolder);
  if Assigned(FOnVirtualHintNeeded) then
    OnVirtualHintNeeded(Self, Item, Hint);
end;
{$endif}

function TElShellList.DoGetPicture(Item: TElTreeItem): integer;
begin
  Result := (Item as TElShellListItem).GetPicture(FIFolder);
end;

procedure TElShellList.Loaded;
var F : TShellFolders;
begin
  inherited;
  F := FFolder;
  FFolder := TShellFolders(Integer(F) - 1);
  SetFolder(F);
end;

procedure TElShellList.DoItemAdded(S : String; ShellFolder : IShellFolder; 
    RelPIDL : PItemIDList; Item : TElShellListItem);
begin
  if assigned(FOnItemAdded) then
    FOnItemAdded(Self, S, ShellFolder, RelPIDL, Item);
end;

function TElShellListItem.GetAttrAsString: string;
var IFolder  : IShellFolder;
    IFolder2 : IShellFolder2;
    IDetails : IShellDetails;
    sd       : TShellDetails;
begin
  result := '';
  if FAttrAsString <> '-' then
  begin
    result := FAttrAsString;
    exit;
  end;

  if IsDesktopPIDL(PIDL) then
  begin
    FAttrAsString := '';
    exit;
  end;
  if PIDL = nil then exit;

  IFolder := TElShellList(FOwner).FIFolder;

  if not SUCCEEDED(IFolder.QueryInterface(IID_IShellFolder2, IFolder2)) then
  begin
    if SUCCEEDED(IFolder.QueryInterface(IID_IShellDetails, IDetails)) then
    begin
      if SUCCEEDED(IDetails.GetDetailsOf(PIDL, siAttr, sd)) then
        result := StrRetToPas(sd.str, PIDL); StrRetFree(sd.str);
    end;
  end
  else
  begin
    if SUCCEEDED(IFolder2.GetDetailsOf(PIDL, siAttr, sd)) then
    begin
      result := StrRetToPas(sd.str, PIDL); StrRetFree(sd.str);
    end;
  end;
  FAttrAsString := result;
  (*
  CheckWin32FindData;
  if Win32FindData <> nil then
  begin
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY = FILE_ATTRIBUTE_READONLY then
      result := result + 'R';
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN = FILE_ATTRIBUTE_HIDDEN then
      result := result + 'H';
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM = FILE_ATTRIBUTE_SYSTEM then
      result := result + 'S';
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE = FILE_ATTRIBUTE_ARCHIVE then
      result := result + 'A';
    if Win32FindData.dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED = FILE_ATTRIBUTE_COMPRESSED then
      result := result + 'A';
  end;
  *)
end;

function TElShellListItem.GetCanRename: Boolean;
begin
  Result := (FAttr and SFGAO_CANRENAME) = SFGAO_CANRENAME;
end;

function TElShellListItem.GetComment: string;
var IFolder  : IShellFolder;
    IFolder2 : IShellFolder2;
    IDetails : IShellDetails;
    sd       : TShellDetails;
    hres     : HResult;

begin
  if FComment <> '-' then
  begin
    result := FComment;
    exit;
  end;
  result := '';
  iFolder := TElShellList(FOwner).FIFolder;

  if not SUCCEEDED(iFolder.CreateViewObject(FOwner.Handle, IShellDetails, pointer(IDetails))) then
  begin
    if not SUCCEEDED(iFolder.QueryInterface(IID_IShellFolder2, IFolder2)) then
    begin
      IDetails := TElShellDefaultColumns.Create as IShellDetails;
    end;
  end;

  if IFolder2 <> nil then
    hRes := IFolder2.GetDetailsOf(FPIDL, siComment, SD)
  else
    hRes := IDetails.GetDetailsOf(FPIDL, siComment, SD);
  if hRes = S_OK then
  begin
    result := StrRetToPas(SD.str, nil); StrRetFree(SD.str);
    FComment := Result;
  end;

  (*
  if SUCCEEDED(ParentFolder.GetUIObjectOf(Owner.Handle, 1, FPIDL, IQueryInfo, nil, QInfo)) then
  begin
    QInfo.GetInfoTip(0, aWP);
    result := aWP;
    SHGetMalloc(Malloc);
    if aWP <> nil then
      Malloc.Free(aWP);
    FComment := Result;
  end
  else
    result := '';
  *)
end;

function TElShellListItem.GetCreationTime: TDateTime;
var ST : TSystemTime;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := 0
  else
  begin
    FileTimeToSystemTime(Win32FindData.ftCreationTime, ST);
    result := SystemTimeToDateTime(ST);
  end;
end;

function TElShellListItem.GetDisplayName: string;
var iFolder : IShellFolder;
    str     : TStrRet;
begin
  if FDisplayName = '' then
  begin
    result  := '';
    iFolder := TElShellList(FOwner).FIFolder;
    ZeroMemory(@Str, sizeof(Str));
    Str.uType := STRRET_CSTR;

    if SUCCEEDED(iFolder.GetDisplayNameOf(FPIDL, SHGDN_INFOLDER, str)) then
      result := StrRetToPas(Str, FPIDL); StrRetFree(str);
    iFolder := nil;
    FDisplayName := Result;
  end
  else
    result := FDisplayName;
end;

function TElShellListItem.GetFileName: string;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := DisplayName
  else
  begin
    if FFileName <> '-' then
      result := FFileName
    else
    begin
      Result := StrPas(Win32FindData.cFileName);
      FFileName := Result;
    end;
  end;
end;

function TElShellListItem.GetFullName: string;
var StrRet : TStrRet;
begin
  result := '';
  ZeroMemory(@StrRet, sizeof(StrRet));
  StrRet.uType := STRRET_CSTR;

  if SUCCEEDED(TElShellList(FOwner).FIFolder.GetDisplayNameOf(FPIDL, SHGDN_NORMAL or SHGDN_FORPARSING {SHGDN_FORADDRESSBAR}, StrRet)) then
  begin
    Result := StrRetToPas(StrRet, FPIDL);
    StrRetFree(StrRet);
  end;
end;

function TElShellListItem.GetIsFileObject: Boolean;
begin
  Result := ((FAttr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM);
end;

function TElShellListItem.GetIsFolder: Boolean;
begin
  Result := (FAttr and SFGAO_FOLDER) = SFGAO_FOLDER;
end;

function TElShellListItem.GetIsRemovable: Boolean;
begin
  Result := (FAttr and SFGAO_REMOVABLE) = SFGAO_REMOVABLE;
end;

function TElShellListItem.GetLastAccessTime: TDateTime;
var ST : TSystemTime;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := 0
  else
  begin
    FileTimeToSystemTime(Win32FindData.ftLastAccessTime, ST);
    result := SystemTimeToDateTime(ST);
  end;
end;

function TElShellListItem.GetModificationTime: TDateTime;
var ST : TSystemTime;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := 0
  else
  begin
    FileTimeToSystemTime(Win32FindData.ftLastWriteTime, ST);
    result := SystemTimeToDateTime(ST);
  end;
end;

function TElShellListItem.GetPIDL: PItemIDList;
begin
  {if Parent = nil then
    Result := TElShellTree(FOwner).FRootPIDL
  else}
    Result := FPIDL;
end;

function TElShellListItem.GetSize: Cardinal;
begin
  CheckWin32FindData;
  if Win32FindData = nil then
    Result := 0
  else
  begin
    result := Win32FindData.nFileSizeLow;
  end;
end;

function TElShellListItem.GetSizeAsString: string;
var ASize : integer;
begin
  if FSizeAsString = '-' then
  begin
    case TElShellList(Owner).SizeFormat of
      ssfAsIs :
        begin
          result := IntToStrFmt(Size) + ' b';
        end;
      ssfKb:
        begin
          ASize  := Round(Size / 1024);
          result := IntToStrFmt(ASize) + ' Kb';
        end;
      ssfAuto:
        begin
          ASize := Size;
          if ASize < 1024 then
            result := IntToStrFmt(ASize) + ' b'
          else
          if ASize < 1024*1024 * 10 then
            result := IntToStrFmt(Round(Size / 1024)) + ' Kb'
          else
            result := IntToStrFmt(Round(Size / (1024 * 1024))) + ' Mb';
        end;
    end;
    FSizeAsString := result;
  end
  else
    result := FSizeAsString;
end;

function TElShellListItem.GetTypeName: string;
var SFI : TSHFileInfo;
begin
  if FTypeName = '-' then
  begin
    SHGetFileInfo(PChar(FullName), 0, SFI, sizeof(SFI), SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES);
    FTypeName := StrPas(SFI.szTypeName);
  end;
  Result := FTypeName;
end;

procedure TElShellListItem.SetDisplayName(const Value: string);
var NewPIDL : PItemIDList;
begin
  if SUCCEEDED(TElShellList(FOwner).FIFolder.SetNameOf(FOwner.Handle, FPIDL, PWideChar(WideString(Value)), SHGDN_INFOLDER, NewPIDL)) then
  begin
    FDisplayName := Value;
    if NewPIDL <> nil then
    begin
      FreeIDList(FPIDL);
      FPIDL := ClonePIDL(GetOwnPIDL(NewPIDL));
      FreeIDList(NewPIDL);
    end;
  end
  else
    raise EElShellError.Create('Failed to rename ' + DisplayName);
end;

constructor TElShellListItem.Create(AOwner : TCustomElTree);
begin
  inherited;
  FPIDL := GetEmptyPIDL;
  FAttrAsString := '-';
  FComment :=  '-';
  FTypeName := '-';
  FSizeAsString := '-';
  FFileName := '-';
  Win32FindData := nil;
end;

destructor TElShellListItem.Destroy;
begin
  if Win32FindData <> nil then
    Dispose(Win32FindData);
  Win32FindData := nil;
  if FPIDL <> nil then
    FreeIDList(FPIDL);
  inherited;
end;

function TElShellListItem.BuildFullPIDL: PItemIDList;
begin
  result := AppendPIDL(TElShellList(FOwner).FRootPIDL, FPIDL);
end;


procedure TElShellListItem.CheckWin32FindData;
begin
  if Win32FindData = nil then
    if ((FAttr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM)
       // or ((FAttr and SFGAO_FILESYSANCESTOR) = SFGAO_FILESYSANCESTOR)
       then
      GetWin32Data(TElShellList(FOwner).FIFolder);
end;

procedure TElShellListItem.GetWin32Data(ParentFolder : IShellFolder);
var HSRec  : THandle;
    FN     : string;
    S      : string;
begin
  FIsValidFile := false;
  New(Win32FindData);
  FN := GetFullName;
  S  := ExtractFilePath(FN);
  System.Delete(S, 1, Length(ExtractFileDrive(S)));
  if S = '\' then
  begin
    S := ExtractFileDrive(FN);
    if Copy(S, 1, 2) = '\\' then
    begin
      ZeroMemory(Win32FindData, sizeof(Win32FindData^));
      Win32FindData^.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
      FFileName := S;
      exit;
    end
    else
    begin
      if GetDriveType(PChar(S)) in [1, DRIVE_REMOVABLE, DRIVE_REMOTE] then
      begin
        ZeroMemory(Win32FindData, sizeof(Win32FindData^));
        Win32FindData^.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
        FFileName := S;
        exit;
      end;
    end;
    FFileName := FN;
  end;
  HSRec := FindFirstFile(PChar(GetFullName), Win32FindData^);
  if HSRec = INVALID_HANDLE_VALUE then
  begin
    ZeroMemory(Win32FindData, sizeof(Win32FindData^));
  end
  else
  begin
    FIsValidFile := true;
    FindClose(HSRec);
  end;
end;

procedure TElShellListItem.Invalidate;
begin
  if FPIDL <> nil then
    FreeIDList(FPIDL);
  FPIDL := GetEmptyPIDL;
  FAttrAsString := '-';
  FComment :=  '-';
  FTypeName := '-';
  FSizeAsString := '-';
  FDisplayName:= '';
  FFileName := '-';

  if Win32FindData <> nil then
    Dispose(Win32FindData);
  Win32FindData := nil;
  FAttr := 0;
end;

function TElShellListItem.GetPicture(ParentFolder: IShellFolder): Integer;
var
    i      : Cardinal;
    // Icon   : IExtractIcon;
    APIDL  : PItemIDList;
begin
  result := -1;
  if FPIDL = nil then
  begin
    exit;
  end;
  i := SFGAO_FOLDER or SFGAO_COMPRESSED or SFGAO_HASSUBFOLDER or SFGAO_GHOSTED
                    or SFGAO_CANRENAME or SFGAO_FILESYSANCESTOR
                    or SFGAO_FILESYSTEM or SFGAO_REMOVABLE;

  if SUCCEEDED(ParentFolder.GetAttributesOf(1, FPIDL, i)) then
    FAttr := i
  else
    FAttr := 0;
  APIDL := BuildFullPIDL;
  if (FAttr and SFGAO_FOLDER) = SFGAO_FOLDER then
  begin
    Result := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, false);
    FImageIndex := Result;
    FStImageIndex := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, true);
    // ImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL);
    // StateImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL or GIL_OPENICON);
  end
  else
  begin
    Result := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, false);
    FImageIndex := Result;
    FStImageIndex := Result;
    //ImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL);
    //StateImageIndex := ImageIndex;
  end;
  FreeIDList(APIDL);
end;

procedure TElShellListItem.GetAttributes(iParentFolder : IShellFolder);
var
    i      : Cardinal;
    // Icon   : IExtractIcon;
//    APIDL  : PItemIDList;
begin
  if FPIDL = nil then
  begin
    exit;
  end;
  i := SFGAO_FOLDER or SFGAO_COMPRESSED or SFGAO_HASSUBFOLDER or SFGAO_GHOSTED
                    or SFGAO_CANRENAME or SFGAO_FILESYSANCESTOR
                    or SFGAO_FILESYSTEM or SFGAO_REMOVABLE;

  if SUCCEEDED(IParentFolder.GetAttributesOf(1, FPIDL, i)) then
    FAttr := i
  else
    FAttr := 0;
  (*
  if (FAttr and SFGAO_FILESYSANCESTOR) = SFGAO_FILESYSANCESTOR then
    GetWin32Data(iParentFolder);
  *)
  //iParentFolder.GetUIObjectOf(0, 1, FPIDL, IExtractIcon, nil, Pointer(Icon));
  // if Icon <> nil then
{
  begin
    APIDL := BuildFullPIDL;
    if (FAttr and SFGAO_FOLDER) = SFGAO_FOLDER then
    begin
      ImageIndex := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, false);
      StateImageIndex := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, true);
      // ImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL);
      // StateImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL or GIL_OPENICON);
    end
    else
    begin
      ImageIndex := ShellIconCache.AddFromPIDL(APIDL, GIL_FORSHELL, false);
      StateImageIndex := ImageIndex;
      //ImageIndex := ShellIconCache.AddIcon(Icon, GIL_FORSHELL);
      //StateImageIndex := ImageIndex;
    end;
    FreeIDList(APIDL);
  end;
}
  if TElShellList(FOwner).FHighlightCompressed then
  begin
    if (FAttr and SFGAO_COMPRESSED) = SFGAO_COMPRESSED then
    begin
      Color := GetCompressedColor;
      ParentColors := false;
      UseBkColor := false;
    end;
  end;
  if (FAttr and SFGAO_GHOSTED) = SFGAO_GHOSTED then
    Cut := true;
end;

{$ifdef VCL_4_USED}
function TElShellListItem.GetHintText(ParentFolder: IShellFolder): TElFString;
var
    QInfo  : IQueryInfo;
    aWP    : PWideChar;
    Malloc : IMalloc;
begin
  if SUCCEEDED(ParentFolder.GetUIObjectOf(Owner.Handle, 1, FPIDL, IQueryInfo, nil, Pointer(QInfo))) then
  begin
    QInfo.GetInfoTip(0, aWP);
    Result := aWP;
    SHGetMalloc(Malloc);
    if aWP <> nil then
      Malloc.Free(aWP);
  end
  else
    Result := '';
end;
{$endif}

procedure TElShellListView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer);
begin
  inherited;
  if (Button = mbRight) and (not Dragging) and TElShellList(Parent).UseSystemMenus then
  begin
    ShowSystemMenu(X, Y);
    // HCol := 0;
  end;
end;

procedure TElShellListView.MenuWndProc(var Message: TMessage);
var
  {$IFDEF VCL_4_USED}
  ICI : CMInvokeCommandInfo;
  {$ELSE}
  ICI : TCMInvokeCommandInfo;
  {$ENDIF}

begin
  inherited;
  if (Message.Msg = WM_EXITMENULOOP) and Assigned(iCtxMenu) then
  begin
    DestroyMenu(FBuiltinMenu);
    FBuiltinMenu := 0;
  end;
  if (Message.Msg = WM_COMMAND) then
  begin
    FillMemory(@ICI, sizeof(ICI), 0);
    ICI.cbSize := SizeOf(ICI);
    ICI.nShow  := SW_SHOWNORMAL;
    ICI.lpVerb := MakeIntResourceA(LOWORD(Message.wParam));
    if Integer(ICI.lpVerb) >= 0 then
      iCtxMenu.InvokeCommand(ICI);
  end;
end;

constructor TElShellListView.Create(AOwner : TComponent);
begin
  inherited;
  FMenuWnd := AllocateHWND(MenuWndProc);
end;

destructor TElShellListView.Destroy;
begin
  DeallocateHWND(FMenuWnd);
  iCtxMenu := nil;
  inherited;
end;

procedure TElShellListView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_APPS) and (Shift = []) and TElShellList(FOwner).UseSystemMenus then
  begin
    ShowSystemMenu(0, 0);
    Key := 0;
  end;
  inherited;
end;

procedure TElShellListView.ShowSystemMenu(X, Y : integer);
type PPItemIDList = ^PItemIDList;
var
  ItemPart: TSTItemPart;
  HCol    : Integer;
  Item    : TElTreeItem;
  iFolder : IShellFolder;
  List    : TElList;
  i       : integer;
  P       : TPoint;
begin
  List := TElList.Create;
  try
    FOwner.AllSelected(List);
    if List.Count = 0 then
      List.Add(FOwner.ItemFocused);
    if List.Count = 0 then
    begin
      HCol := 0;
      Item := GetItemAt(X, Y, ItemPart, HCol);
      List.Add(Item);
    end;
    for i := 0 to List.Count - 1 do
      List[i] := TElShellListItem(List[i]).FPIDL;

    iCtxMenu := nil;
    iFolder  := TElShellList(FOwner).FIFolder;

    if SUCCEEDED(iFolder.GetUIObjectOf(Parent.Handle, List.Count, PPItemIDList(List.List)^, IID_iContextMenu, nil, Pointer(iCtxMenu))) then
    begin
      FBuiltinMenu := CreatePopupMenu;
      iCtxMenu.QueryContextMenu(FBuiltinMenu, 0, 0, $FFFFFFFF, CMF_NORMAL);

      (*
      if (false) and Assigned(PopupMenu) and PopupMenu.AutoPopup then
      begin
      end;
      *)
      P := ClientToScreen(Point(X, Y));
      TrackPopupMenu(FBuiltinMenu, TPM_LEFTALIGN, P.X, P.Y, 0, FMenuWnd, nil);
    end;
  finally
    List.Free;
  end;
end;

function TElShellListView.GetPopupMenu: TPopupMenu;
begin
  if TElShellList(FOwner).UseSystemMenus then
    result := nil
  else
    Result := inherited GetPopupMenu;
end;


end.

