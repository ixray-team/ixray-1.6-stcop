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
  
03/06/2002

  Added unicode hint

02/08/2002

  Changed drag'n'drop behaviour - now when the mouse is not over the header,
  the section is dropped to position that it would be drooped to if the mouse
  was over the header with the same horizontal position. 

11/20/2001

  Fixed drawing of the section being dragged in Windows 95/98 
  Fixed an AV introduced 11/17

11/17/2001

  Fixed some artefacts when drawing with XP styles enabled

09/19/2001

  Added Windows XP Themes Support 

08/22/2001

  Fixed transparency issues with images

07/26/2001

  Added Unicode support

07/17/2001

  Header sections can now contain HTML text. 

07/14/2001

  Improved dragging painting
  Changed OnItemDraw event to include canvas

06/28/2001

  Default FieldType for HeaderSection is sftText now
  Improved drawing of the section being dragged
  Added DefaultWidth property. This defines width of new sections. 

06/26/2001

  Added support for WM_CANCELMODE message

03/10/2001

  Fixed possible AVs that could happen when image list is removed.

  Minor optimizations and readness improvements.

01/11/2001

   Fixed the bug in UpdateSection, which incorrectly invalidated sections when
   parent tree was horizontally scrolled

12/02/2000

   WrapCaptions property added. Now header captions can be multiline and wrapped
   automatically, if they don't fit into width 

   ImageForm support added

10/06/2000

   Deletion of the section could cause AV. Fixed. 

09/22/2000

   Fixed Section hints showing to be changed when cursor moves to another section

09/20/2000

   ElHeaderSection.AutoSize property added.
   
08/09/2000

   Lookup focus returning caused an exception ("can't focus hidden window"). Fixed. 

07/25/2000

  Lookup improved. Now focus is returned to the previously focused control.

07/09/2000

  Fixed lookup, expandable and filter sections to react on sign clicks when the
  heaader is scrolled.
  Changed section drawing to make offset of caption for non-resizable sections.  

*)

unit ElHeader;

interface
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

{$R ElHeader.res}

{$B-}
{$ALIGN ON}
uses
{$ifndef CLX_USED}
{$ifdef VCL_6_USED}
  Types,
{$endif}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ExtCtrls,
  StdCtrls,
{$IFDEF VCL_4_USED}
  ImgList,
{$ENDIF}
  {$else}
  {$ifdef LINUX}
  Xlib,
  {$else}
  Windows,
  {$endif}
  Qt,
  QTypes,
  Types,
  QGraphics,
  QControls,
  QForms,
  QMenus,
  QExtCtrls,
  QStdCtrls,
{$IFDEF VCL_4_USED}
  QImgList,
{$ENDIF}
  {$endif}
{$IFDEF ELPACK_COMPLETE}
{$ifdef SUPPORT_STORAGE}
  ElIni,
{$endif}
  ElImgFrm,
{$ifndef CLX_USED}
  ElACtrls,
{$endif}
{$ENDIF}
  ElStrUtils,
  SysUtils,
  Classes,
  ElTools,
  ElList,
  ElXPThemedControl,
  ElUxTheme,
  ElTmSchema,
{$ifdef HAS_HTML_RENDER}
  HTMLRender,
{$endif}  
  ElVCLUtils;

type
  TElSectionStyle = (ElhsText, ElhsOwnerDraw, ElhsPictureOnly);

  TElSSortMode = (hsmNone, hsmAscend, hsmDescend);

  TElSAlignment = (hsaLeft, hsaCenter, hsaRight);

  TElHResizingStates = (trsBegin, trsMove, trsEnd);

  TSectionChangeMode = (scmCaption, scmFieldName, scmFieldType,
    scmAlign, scmStyle, scmEditable, scmPassword);

  TElSectionPart = (espResizeArea, espText, espExpandSign, espLookupSign, espFilterSign);

  TAdjustCondition = (acAll,acAutoSizedOnly);

  TElFieldType = (
    sftCustom, // custom field
    sftText, // text field
    sftNumber, // Integer numbers
    sftFloating, // Floating numbers
    sftDateTime, // DateTime
    sftDate, // Date
    sftTime, // Time
    sftPicture, // Picture
    sftEnum,  // enumeration
    sftBLOB,  // BLOB data
    sftUndef, // undefined field
    sftBool,   // boolean field
    sftCurrency, // currency
    sftMemo   // Memo, multiline text
    );

  TElFieldTypes = set of TElFieldType;

  EElHeaderError = class(Exception);

type
  TCustomElHeader = class;
  TElHeaderSection = class;

{$ifndef SUPPORT_STORAGE}
  TElIniFile = class end;
{$endif}

  TElHeaderSectionEvent = procedure(Sender : TCustomElHeader; Section : TElHeaderSection) of object;

  TElHeaderLookupEvent = procedure(Sender : TObject; Section : TElHeaderSection; var Text : string) of object;
  TElHeaderLookupDoneEvent = procedure(Sender : TObject;
    Section : TElHeaderSection;
    Text : string;
    Accepted : boolean) of object;

  TElHeaderSection = class(TPersistent)
  protected
    FFilterIsActive : Boolean;
    FFilterEnabled : Boolean;
    FHint: TElFString;

    FTextLayout : TTextLayout;
    FUseMainStyle : Boolean;
    FFontColor : TColor;
    FColor : TColor;
    FParentColor : Boolean;
    FLookupEnabled : boolean;
    FExpandable : Boolean;
    FExpanded : Boolean;
    FParentSection : TElHeaderSection;
    FParentIdx : integer;
    FPopupMenu : TPopupMenu;
    FPopupName : string;
    FResizable : Boolean;
    FClickSelect : Boolean;
    FProtected : Boolean;
    FFieldName : string;
    FFieldType : TElFieldType;
    FEditable : Boolean;
    FImageIndex : Integer;
    FPicAlign : TElSAlignment;
    FIntTag,
    FIndex    : integer;
    FTag: Integer;
    FVisible : boolean;
    FStyle : TElSectionStyle;
    FWidth : integer;
    FMinWidth,
      FMaxWidth : integer;
    FSortMode : TElSSortMode;
    FAllowClick : boolean;
    FAlignment : TElSAlignment;
    FText: TElFString;
    FData : pointer;
    FOwner : TCustomElHeader;
    FLookupHist : TStringList;
    FAutoSize   : boolean;

    FStickySize : single;
    ASaveSize : integer;
    FOnResize: TNotifyEvent;
    FShowSortMark: Boolean;

    procedure SetWidth(value : integer);
    function GetWidth : integer;
    function GetLeft  : integer;
    function GetRight : integer;
    procedure SetMaxWidth(value : integer);
    procedure SetMinWidth(value : integer);
    procedure SetText(value: TElFString);
    procedure SetStyle(value : TElSectionStyle);
    procedure SetSortMode(value : TElSSortMode);
    procedure SetAlignment(value : TElSAlignment);
    procedure SetVisible(value : boolean);

    function GetIndex : integer;
    function GetPosition : Integer;
    procedure SetImageIndex(newValue : Integer);
    procedure SetProtected(newValue : Boolean);
    procedure SetExpandable(newValue : Boolean);
    procedure SetExpanded(newValue : Boolean);
    procedure SetParentSection(newValue : TElHeaderSection);
    procedure SetPopupMenu(newValue : TPopupMenu);
    function GetVisible : boolean;
    procedure SetLookupEnabled(newValue : boolean);
    procedure SetParentColor(newValue : Boolean);
    procedure SetColor(newValue : TColor);
    procedure SetFontColor(newValue : TColor);
    procedure SetUseMainStyle(newValue : Boolean);
    procedure SetTextLayout(newValue : TTextLayout);
    procedure SetFilterEnabled(newValue : Boolean);
    procedure SetFilterIsActive(newValue : Boolean);
    procedure SetLookupList(newValue : TStringList);
    procedure SetAutoSize(newValue : boolean);
{$IFNDEF LITE}
    function GetLocked : boolean;
{$ENDIF}
    procedure SetShowSortMark(Value: Boolean);
  protected
    procedure SetFieldName(newValue : string); virtual;
    procedure SetFieldType(newValue : TElFieldType); virtual;
    procedure SetEditable(newValue : boolean); virtual;
    procedure SetResizable(newValue : boolean); virtual;
    procedure SetSaveSize(newValue : integer);
    property FSaveSize : integer read ASaveSize write SetSaveSize default -1;
    function GetOwner : TPersistent; override;
    property OnResize : TNotifyEvent read FOnResize write FOnResize;
  public
    constructor Create(AOwner : TCustomElHeader);
    destructor Destroy; override;
    procedure UpdateSection;
    procedure Assign(source : TPersistent); override;
    property Index : integer read GetIndex;
    property Left : integer read GetLeft;
    property Right : integer read GetRight;
    property Position : Integer read GetPosition; { Public }
    property Data : pointer read FData write FData;
{$IFNDEF LITE}
    property Locked : boolean read GetLocked;
{$ENDIF}
    property Owner: TCustomElHeader read FOwner;
  published
    property Text: TElFString read FText write SetText;
    property Style : TElSectionStyle read FStyle write SetStyle default ElhsText;
    property Width : integer read GetWidth write SetWidth;
    property MaxWidth : integer read FMaxWidth write SetMaxWidth default 10000;
    property MinWidth : integer read FMinWidth write SetMinWidth default 0;
    property SortMode : TElSSortMode read FSortMode write SetSortMode;
    property AllowClick : boolean read FAllowClick write FAllowClick default true;
    property Alignment : TElSAlignment read FAlignment write SetAlignment default hsaLeft;
    property PictureAlign : TElSAlignment read FPicAlign write FPicAlign;
    property Visible : boolean read GetVisible write SetVisible;
    property ImageIndex : Integer read FImageIndex write SetImageIndex default -1; { Published }
    property FieldName : string read FFieldName write SetFieldName; { Public }
    property FieldType : TElFieldType read FFieldType write SetFieldType default sftText; { Public }
    property Editable : Boolean read FEditable write SetEditable; { Public }
    property Password : Boolean read FProtected write SetProtected default False; { Public }
    property Resizable : Boolean read FResizable write SetResizable default true; { Public }
    property ClickSelect : Boolean read FClickSelect write FClickSelect default true; { Public }
    property Expandable : Boolean read FExpandable write SetExpandable;
    property Expanded : Boolean read FExpanded write SetExpanded;
    property ParentSection : TElHeaderSection read FParentSection write SetParentSection;
    property PopupMenu : TPopupMenu read FPopupMenu write SetPopupMenu;
    property LookupEnabled : boolean read FLookupEnabled write SetLookupEnabled;
    property LookupHistory : TStringList read FLookupHist write SetLookupList;
    property ParentColor : Boolean read FParentColor write SetParentColor default true;
    property Color : TColor read FColor write SetColor;
    property FontColor : TColor read FFontColor write SetFontColor;
    property UseMainStyle : Boolean read FUseMainStyle write SetUseMainStyle;
    property TextLayout : TTextLayout read FTextLayout write SetTextLayout default tlCenter;
    property FilterEnabled : Boolean read FFilterEnabled write SetFilterEnabled; { Public }
    property FilterIsActive : Boolean read FFilterIsActive write SetFilterIsActive; { Published }
    property Hint: TElFString read FHint write FHint;
    property AutoSize : boolean read FAutoSize write SetAutoSize;
    property ShowSortMark: Boolean read FShowSortMark write SetShowSortMark default true;
    property Tag: Integer read FTag write FTag;
  end;

  TElHeaderSections = class(TPersistent)
  private
    FList : TElList;
    function GetCount : integer;
    function GetSectionsOrder : string;
    procedure SetSectionsOrder(newValue : string);
  protected
    FOwner : TCustomElHeader;
    function GetSectionByIntTag(IntTag : integer) : TElHeaderSection; virtual;
    function GetSection(index : integer) : TElHeaderSection; virtual;
    procedure SetSection(index : integer; Section : TElHeaderSection); virtual;
    function GetSectionByPos(index : integer) : TElHeaderSection; virtual;
    function CreateSection : TElHeaderSection; virtual;
    procedure WriteData(Stream : TStream); virtual;
    procedure IntReadData(Stream : TStream; ClearCurrent : boolean); virtual;
    procedure ReadData(Stream : TStream); virtual;
    procedure FrameReadData(Stream : TStream); virtual;
    procedure DefineProperties(Filer : TFiler); override;
    function LastVisibleSection : TElHeaderSection;
    function GetPrevVisibleSection(Section : TElHeaderSection) : TElHeaderSection;
    function FindSection(tag : integer) : TElHeaderSection; virtual;
    function GetOwner : TPersistent; override;
  public
    constructor Create(AOwner : TCustomElHeader);
    destructor Destroy; override;
    procedure Clear;

    procedure Assign(source : TPersistent); override;
    function AddSection : TElHeaderSection;
    function InsertSection(index : integer) : TElHeaderSection;
    procedure DeleteSection(Section : TElHeaderSection);
    procedure MoveSection(Section : TElHeaderSection; NewPos : integer);
    procedure LoadFromStream(Stream : TStream);
    procedure SaveToStream(Stream : TStream);
    procedure SaveToFile(FileName : string);
    procedure LoadFromFile(FileName : string);
    procedure Reindex;

    property Owner : TCustomElHeader read FOwner;
    property Count : integer read GetCount;
    property ItemByPos[Index : integer] : TElHeaderSection read GetSectionByPos;
    property Item[Index : integer] : TElHeaderSection read GetSection write SetSection; default;
  published
    property SectionsOrder : string read GetSectionsOrder write SetSectionsOrder stored false; { Protected }
  end;

  TMeasureSectionEvent = procedure(Sender: TObject; Section : TElHeaderSection; var Size: TPoint) of object;

  TElSectionRedrawEvent = procedure(Sender : TCustomElHeader;
    Canvas : TCanvas; Section : TElHeaderSection; R : TRect; Pressed : boolean) of object;
  TElSectionResizingEvent = procedure(Sender : TCustomElHeader;
    Section : TElHeaderSection; State : TElHResizingStates; Width : integer) of object;
  TElSectionMoveEvent = procedure(Sender : TCustomElHeader;
    Section : TElHeaderSection; OldPos, NewPos : integer) of object;
  TPictureNeededEvent = procedure(Sender : TCustomElHeader; Section : TElHeaderSection; var ImageIndex : integer) of object;
  TSectionChangeEvent = procedure(Sender : TCustomElHeader; Section : TElHeaderSection; Change : TSectionChangeMode) of object;

  TCustomElHeader = class(TElXPThemedControl)
  private
    FMouseInControl: Boolean;
  protected
{$IFNDEF LITE}
    FWrapCaptions : Boolean;
    FLockedSection : TElHeaderSection;
{$ENDIF}
    FHPos : Integer;
    FInvertSortArrows : Boolean;
    FFlat : Boolean;
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
    FImgFormChLink : TImgFormChangeLink;
    FImgForm: TElImageForm;
{$endif}
{$ENDIF}
    FActiveFilterColor : TColor;
    FOnFilterCall : TElHeaderSectionEvent;
    FOnSectionAutoSize : TElHeaderSectionEvent;
    FFilterColor : TColor;
    FLockHeight : Boolean;
    FRightAlignedText : Boolean;
    FRightAlignedOrder : Boolean;
    FStickySections : Boolean;
    FMoveOnDrag : Boolean;
{$ifdef ELPACK_COMPLETE}
    FStorage : TElIniFile;
    FStoragePath: string;
{$ENDIF}
    FImages : TImageList;

    FImageChangeLink : TChangeLink;
    FSections : TElHeaderSections;
    FTracking : boolean;
    FAllowDrag : boolean;
    FPainting : boolean;
    DragBmp,
    SaveBmp,
    DragBmpMask : TBitmap;
    DragRect : TRect;
    FDragCoord : TPoint;
    FLookup : TComboBox;
    FPressCoord : TPoint;
    FPressed : boolean;
    FPressedItem : TElHeaderSection;
    FHintSection,
    FLookupSection,
      FTrackSection : TElHeaderSection;
    FResizing : boolean;
    FResizeSection : TElHeaderSection;
    FDropSrc,
      FDropTrg : TElHeaderSection;
    FHeaderLine : integer;
    FLineTab : integer;
    FResizeOnDrag : boolean;
    FHeaderLineVis : boolean;
    FIgnoreLookupChange : boolean;
    FDoingLookup : boolean;
    {$ifndef CLX_USED}
    FLineDC : HDC;
    FFocusedCtl : THandle;
    {$else}
    FLineDC : TBitmap;
    {$ifdef LINUX}
    FFocusedCtl : TWindow;
    {$else}
    FFocusedCtl : HWND;
    {$endif}
    {$endif}
    LoadingCount : integer;
    DeletionHappened,
    AdditionHappened : boolean;

    FInStick : boolean;
    FOldWidth : integer;
    FUpdateCount: integer;
{$ifdef HAS_HTML_RENDER}
    FRender : TElHTMLRender;
{$endif}
    FOnSectionClick : TElHeaderSectionEvent;
    FOnSectionResize : TElHeaderSectionEvent;
    FOnSectionDraw : TElSectionRedrawEvent;
{$IFNDEF VCL_4_USED}
    FOnResize : TNotifyEvent;
{$ENDIF}
    FOnSectionResizing : TElSectionResizingEvent;
    FOnSectionDelete : TElHeaderSectionEvent;
    FOnSectionMove : TElSectionMoveEvent;
    FOnVisibleChange : TElHeaderSectionEvent;
    FOnPictureNeeded : TPictureNeededEvent;
    FOnSectionChange : TSectionChangeEvent;
    FOnSectionCreate : TElHeaderSectionEvent;
    FOnHeaderLookup : TElHeaderLookupEvent;
    FOnHeaderLookupDone : TElHeaderLookupDoneEvent;
    FOnSectionExpand : TElHeaderSectionEvent;
    FOnSectionCollapse : TElHeaderSectionEvent;
    FOnMeasureSection  : TMeasureSectionEvent;
    FDefaultWidth: Integer;
{$ifdef HAS_HTML_RENDER}
    FOnImageNeeded: TElHTMLImageNeededEvent;
{$endif}
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}
    FMultiSort: Boolean;

    procedure DrawLine(Restore : boolean);

    procedure AllocateLineDC;
    procedure ReleaseLineDC;

    function GetColumnsWidth : integer;
    procedure InvalidateRight(value : integer);
    procedure SetTracking(newValue : boolean);

    procedure IntMouseEnter;
    procedure IntMouseLeave;
    procedure IntSize;
    procedure IntExit;

    procedure IntLButtonDown(XPos, YPos : SmallInt);
    procedure IntLButtonUp(XPos, YPos : SmallInt);
    function  IntRButtonDown(XPos, YPos : SmallInt) : boolean;
    function  IntRButtonUp(XPos, YPos : SmallInt) : boolean;
    procedure IntMouseMove(XPos, YPos : SmallInt);
    procedure IntLButtonDblClick(XPos, YPos : SmallInt);
    function  IntHintShow(var HintInfo : THintInfo): Boolean;
    {$ifndef CLX_USED}
    procedure CMDrag(var Message : TCMDrag); message CM_DRAG;
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMRButtonDown(var Msg : TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure WMRButtonUp(var Msg : TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMLButtonDblClk(var Msg : TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMHintShow(var Msg : TMessage); message CM_HINTSHOW;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMLButtonDown(var Message : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message : TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message : TWMMouseMove); message WM_MOUSEMOVE;
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure CMExit(var Msg : TCMExit); message CM_EXIT;
    {$endif}
    procedure SetSections(value : TElHeaderSections);
    procedure SetImages(newValue : TImageList);
    procedure OnImageListChange(Sender : TObject);
    procedure GetDragImage(XPos : Integer);

{$ifdef SUPPORT_STORAGE}
    procedure SetStorage(newValue : TElIniFile);
{$ENDIF}
    procedure EditExit(Sender : TObject);
    procedure EditKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure EditKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure EditChange(Sender : TObject);
    procedure SetStickySections(newValue : Boolean);
    procedure AdjustHeaderHeight;
    procedure AdjustStickySize(Caller : TElHeaderSection);
    function IsLoading : Boolean;
    procedure SetRightAlignedText(newValue : Boolean);
    procedure SetRightAlignedOrder(newValue : Boolean);
    procedure SetLockHeight(newValue : Boolean);
    procedure SetFilterColor(newValue : TColor);
    procedure SetActiveFilterColor(newValue : TColor);
    procedure SetFlat(newValue : Boolean);
    function GetIsDesigning : Boolean;
    procedure SetIsDesigning(newValue : Boolean);
    procedure SetInvertSortArrows(newValue : Boolean);
    procedure SetLeftPos(newValue : Integer);
{$IFDEF ELPACK_COMPLETE}
    {$ifndef CLX_USED}
    procedure ImageFormChange(Sender : TObject);
    procedure SetImageForm(newValue : TElImageForm);
    {$endif}
{$ENDIF}
{$IFNDEF LITE}
    procedure SetLockedSection(newValue : TElHeaderSection);
    procedure SetWrapCaptions(newValue : Boolean); virtual;
{$ENDIF}

    procedure RedrawSection(Canvas : TCanvas; Section : TElHeaderSection; R : TRect; Dithered : Boolean);
    procedure RedrawSections;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    function  CreateSections : TElHeaderSections; virtual;
    function  InResizeArea(X : integer; var HitSection : TElHeaderSection) : boolean; virtual;

    {$ifdef CLX_USED}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure DoExit; override;
    function WidgetFlags: Integer; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}

    procedure Paint; override;
{$IFDEF VCL_4_USED}
    procedure Resize; override;
{$ELSE}
    procedure Resize; dynamic;
{$ENDIF}
    function DoGetPicture(Section : TElHeaderSection) : integer; virtual;
    procedure DoVisChanged(Section : TElHeaderSection); virtual;
    procedure DoSectionDelete(Section : TElHeaderSection); virtual;
    procedure DoSectionMove(Section : TElHeaderSection; OldPos, NewPos : integer); virtual;
    procedure DoSectionResizing(Section : TElHeaderSection; State : TElHResizingStates; NewWidth : integer); virtual;
    procedure DoSectionResize(Section : TElHeaderSection); virtual;
    procedure DoSectionClick(Section : TElHeaderSection); virtual;
    procedure DoSectionDraw(Canvas : TCanvas; Section : TElHeaderSection; R : TRect; Pressed : boolean); virtual;
    procedure DoNotifySectionChange(Section : TElHeaderSection; Change : TSectionChangeMode); virtual;
    procedure DoSectionExpandEvent(Section : TElHeaderSection); virtual;
    procedure DoSectionCollapseEvent(Section : TElHeaderSection); virtual;
    procedure DoSectionCreate(Section : TElHeaderSection); virtual;
    procedure DoSectionLookupEvent(Section : TElHeaderSection; var Text : string); virtual;
    procedure DoSectionLookupDoneEvent(Section : TElHeaderSection; Text : string; Accepted : boolean); virtual;
    procedure TriggerSectionAutoSizeEvent(Section : TElHeaderSection); virtual;
    procedure TriggerFilterCallEvent(Section : TElHeaderSection); virtual;
    procedure TriggerMeasureSectionEvent(Section : TElHeaderSection; var Size: TPoint); virtual;

    procedure OnFontChange(Sender : TObject);
    function GetResizableWidth : integer;
    {$ifndef CLX_USED}
    procedure CreateParams(var Params : TCreateParams); override;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    {$endif}
{$ifdef HAS_HTML_RENDER}
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image :
        TBitmap); virtual;
{$endif}
    {$ifndef CLX_USED}
    procedure CreateHandle; override;
    {$else}
    procedure CreateWidget; override;
    {$endif}
    {$ifndef CLX_USED}
    {$ifdef ELPACK_COMPLETE}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$endif}
    {$endif}
    {$ifdef MSWINDOWS}
    function GetThemedClassName: WideString; override;
    {$endif}

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);
    {$endif}

    property SectionsWidth : integer read GetColumnsWidth;
    property Sections : TElHeaderSections read FSections write SetSections;
    property ResizeOnDrag : boolean read FResizeOnDrag write FResizeOnDrag default true;
    property Tracking : boolean read FTracking write SetTracking default true;
    property AllowDrag : boolean read FAllowDrag write FAllowDrag default true;

    property Images : TImageList read FImages write SetImages; { Published }

    property MoveOnDrag : Boolean read FMoveOnDrag write FMoveOnDrag; { Published }
{$ifdef SUPPORT_STORAGE}
    property StoragePath : string read FStoragePath write FStoragePath;
    property Storage : TElIniFile read FStorage write SetStorage;
{$ELSE}
{$IFDEF ELPACK_COMPLETE}
    property StoragePath: string read FStoragePath write FStoragePath;
    property Storage : TElIniFile read FStorage write FStorage;
{$ENDIF}
{$ENDIF}
    property StickySections : Boolean read FStickySections write SetStickySections;
    property RightAlignedText : Boolean read FRightAlignedText write SetRightAlignedText;
    property RightAlignedOrder : Boolean read FRightAlignedOrder write SetRightAlignedOrder;
    property LockHeight : Boolean read FLockHeight write SetLockHeight; { Protected }
    property FilterColor : TColor read FFilterColor write SetFilterColor default clBtnText; { Protected }
    property ActiveFilterColor : TColor read FActiveFilterColor write SetActiveFilterColor; { Published }
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
    property ImageForm   : TElImageForm read FImgForm write SetImageForm;
{$endif}
{$ENDIF}
    property OnSectionChange : TSectionChangeEvent read FOnSectionChange
      write FOnSectionChange;
{$IFNDEF VCL_4_USED}
    property OnResize : TNotifyEvent read FOnResize write FOnResize;
{$ENDIF}
    property OnSectionShowHide : TElHeaderSectionEvent read FOnVisibleChange
      write FOnVisibleChange;
    property OnSectionClick : TElHeaderSectionEvent read FOnSectionClick
      write FOnSectionClick;
    property OnSectionResize : TElHeaderSectionEvent read FOnSectionResize
      write FOnSectionResize;
    property OnSectionDraw : TElSectionRedrawEvent read FOnSectionDraw
      write FOnSectionDraw;
    property OnSectionDelete : TElHeaderSectionEvent read FOnSectionDelete
      write FOnSectionDelete;
    property OnSectionResizing : TElSectionResizingEvent read FOnSectionResizing
      write FOnSectionResizing;
    property OnSectionMove : TElSectionMoveEvent read FOnSectionMove
      write FOnSectionMove;
    property OnPictureNeeded : TPictureNeededEvent read FOnPictureNeeded
      write FOnPictureNeeded;
    property OnSectionCreate : TElHeaderSectionEvent read FOnSectionCreate
      write FOnSectionCreate;
    property OnSectionExpand : TElHeaderSectionEvent read FOnSectionExpand
      write FOnSectionExpand;
    property OnSectionCollapse : TElHeaderSectionEvent read FOnSectionCollapse
      write FOnSectionCollapse;
    property OnHeaderLookup : TElHeaderLookupEvent read FOnHeaderLookup
      write FOnHeaderLookup;
    property OnHeaderLookupDone : TElHeaderLookupDoneEvent read FOnHeaderLookupDone
      write FOnHeaderLookupDone;
    property OnMeasureSection  : TMeasureSectionEvent read FOnMeasureSection write FOnMeasureSection;

    property OnSectionAutoSize : TElHeaderSectionEvent read FOnSectionAutoSize write FOnSectionAutoSize;
    property OnFilterCall : TElHeaderSectionEvent read FOnFilterCall write FOnFilterCall;
    property Flat : Boolean read FFlat write SetFlat; { Protected }
    property IsDesigning : Boolean read GetIsDesigning write SetIsDesigning;  { Protected }
    property InvertSortArrows : Boolean read FInvertSortArrows write SetInvertSortArrows default False;  { Protected }
{$IFNDEF LITE}
    property WrapCaptions : Boolean read FWrapCaptions write SetWrapCaptions;  { Published }
    property DefaultWidth: Integer read FDefaultWidth write FDefaultWidth default
        120;
{$ifdef HAS_HTML_RENDER}
    property OnHTMLImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        FOnImageNeeded;
{$endif}
{$ENDIF}
  public
      { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetSectionAt(X, Y : integer) : TElHeaderSection;
    function GetSectionAtEx(X, Y : integer; var SectionPart : TElSectionPart) : TElHeaderSection;
    function GetSectionRect(SectionNum : integer) : TRect;
    function MeasureSectionWidth(Section : TElHeaderSection; TextWidth : PInteger; SectionHeight : PInteger) : integer;
    function CalcHeaderHeight : integer;

    property Canvas;
    procedure Loaded; override;
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
    function Setup: boolean;
{$endif}
{$ENDIF}
{$ifdef SUPPORT_STORAGE}
    procedure Save;
    procedure Restore;
{$ENDIF}
    procedure Update; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MarkStickySections;
    property LeftPos : Integer read FHPos write SetLeftPos;  { Public }
{$IFNDEF LITE}
    property LockedSection : TElHeaderSection read FLockedSection write SetLockedSection;  { Public }
    property MultiSort: Boolean read FMultiSort write FMultiSort;
{$ENDIF}
  published
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
  end;

  TElHeader = class(TCustomElHeader)
  published
    property ActiveFilterColor;
    property AllowDrag;
    property Align;
    property Color;
    property DefaultWidth;
    property Enabled;
    property Flat;
{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    {$ifndef CLX_USED}
    property DockOrientation;
    property Floating;
    property BevelKind;
    property DoubleBuffered;
    property DragKind;
    {$endif}
{$ENDIF}
//        property FixClick; // Left for future
    property MoveOnDrag;
    property Font;
    property FilterColor;
    property Images;
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
    property ImageForm;
{$endif}
{$ENDIF}
    property InvertSortArrows;

    property LockHeight;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ResizeOnDrag;
    property RightAlignedText;
    property RightAlignedOrder;
    property SectionsWidth;
    property Sections;
    property ShowHint;
    property StickySections;
    property Tracking;
{$IFDEF ELPACK_COMPLETE}
    property Storage;
    property StoragePath;
{$ENDIF}
    property Visible;
    property UseXPThemes;
    property WrapCaptions;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSectionClick;
    property OnSectionResize;
    property OnSectionDraw;
    property OnSectionResizing;
    property OnSectionDelete;
    property OnSectionMove;
    property OnSectionShowHide;
    property OnPictureNeeded;
    property OnSectionChange;
    property OnSectionCreate;
    property OnSectionExpand;
    property OnSectionCollapse;
    property OnHeaderLookup;
    property OnHeaderLookupDone;
{$ifdef HAS_HTML_RENDER}
    property OnHTMLImageNeeded;
{$endif}
    property OnSectionAutoSize;
    property OnFilterCall;
{$IFDEF VCL_4_USED}
    {$ifndef CLX_USED}
    property OnStartDock;
    property OnEndDock;
    {$endif}
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

const
  ResizeWidth = 5;

var ElHeaderAscBmp,
    ElHeaderDescBmp,
    ElHeaderLeftBmp,
    ElHeaderRightBmp,
    ElHeaderPointBmp : TBitmap;

implementation

{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
uses
  frmHdrStp;

{$endif}
{$ENDIF}

// =============================================================================

constructor TElHeaderSection.Create;
begin
  inherited Create;
  FStyle := ElhsText;
  FVisible := true;
  FOwner := AOwner;
  FWidth := AOwner.DefaultWidth;
  FMinWidth := 0;
  FMaxWidth := 10000;
  FAlignment := hsaLeft;
  FAllowClick := true;
  FResizable := true;
  FClickSelect := true;
  FLookupHist := TStringList.Create;
  FParentColor := True;
  FSaveSize := -1;
  FTextLayout := tlCenter;
  FShowSortMark := true;
  FieldType := sftText;
  FImageIndex := -1;
end;

procedure TElHeaderSection.UpdateSection;
var
  R : TRect;
begin
  if (not Visible) or (not FOwner.HandleAllocated) then exit;
  FOwner.BeginUpdate;
  try
  R := Rect(Left, 0, Left + FWidth, FOwner.Height + 1);
  OffsetRect(R, -FOwner.FHPos, 0);
  {$ifndef CLX_USED}
  InvalidateRect(FOwner.Handle, @R, false);
  {$else}
  Inc(R.Bottom); Inc(R.Right);
  QWidget_update(FOwner.Handle, @R);
  Dec(R.Bottom); Dec(R.Right);
  {$endif}
  finally
    FOwner.EndUpdate;
  end;
  //FOwner.Refresh;
end;

procedure TElHeaderSection.SetSortMode(value : TElSSortMode);
var
  i : integer;
begin
  if FSortMode = value then exit;
  if not FOwner.MultiSort then
    for i := 0 to FOwner.FSections.Count - 1 do
      FOwner.FSections[i].FSortMode := hsmNone;
  FSortMode := value;
  if AutoSize then
     FOwner.TriggerSectionAutoSizeEvent(Self);
  if (not FOwner.HandleAllocated) then exit;
  if FShowSortMark then
    FOwner.Invalidate;//Repaint;
end;

procedure TElHeaderSection.SetMaxWidth(value : integer);
begin
  if value = FMaxWidth then exit;
  FMaxWidth := value;
  if FWidth > FMaxWidth then Width := FMaxWidth;
end;

procedure TElHeaderSection.SetMinWidth(value : integer);
begin
  if value = FMinWidth then exit;
  FMinWidth := value;
  if FWidth < FMinWidth then Width := FMinWidth;
end;

procedure TElHeaderSection.SetWidth(value : integer);
var
  diff, i, wo, wn : integer;
  HS, LS : TElHeaderSection;

begin
  if (value > FMaxWidth) or (value < FMinWidth) then exit;
  if (not FOwner.FStickySections) or (not Visible) or (FOwner.FInStick) or (FOwner.IsLoading) then
  begin
    FWidth := value;
    if Visible and (not FOwner.FInStick) then
    begin
      FOwner.AdjustHeaderHeight;
      if (FOwner.HandleAllocated) then
        FOwner.Invalidate;
      //FOwner.InvalidateRight(Self.Index);
      //FOwner.Refresh;
    end;
  end
    else if Visible then
  begin
    if Position = FOwner.FSections.Count - 1 then
    begin
      // the last section, do nothing (we can't resize it)
    end
    else
    begin
      wo := FOwner.Width - (Left + FWidth); if wo = 0 then wo := 1;
      wn := FOwner.Width - (Left + Value);
      Ls := nil;
      for i := Position + 1 to FOwner.Sections.Count - 1 do // Iterate
      begin
        HS := FOwner.FSections.ItemByPos[i];
        if HS.Resizable and HS.Visible then
        begin
          HS.FWidth := Round((HS.Width / wo) * wn);
          LS := HS;
        end;
      end; // for
      FWidth := value;
      if (LS <> nil) then
      begin
        diff := FOwner.Width - FOwner.SectionsWidth;
        if diff > 0 then
          LS.FWidth := LS.FWidth + diff;
      end;
      FOwner.AdjustStickySize(Self);
    end;
    FOwner.MarkStickySections;
    if Visible and (not FOwner.FInStick) then
    begin
      FOwner.InvalidateRight(Index);
      //FOwner.Refresh;
    end;
  end;
end;

procedure TElHeaderSection.SetLookupEnabled(newValue : boolean);
begin
  if FLookupEnabled <> newValue then
  begin
    FLookupEnabled := newValue;
    if newValue then
    begin
      FFilterEnabled := false;
      with FOwner do
      begin
        if (Height > 0) and (Height < 17) then Height := 17;
      end;
    end;
    if AutoSize then
       FOwner.TriggerSectionAutoSizeEvent(Self);
    UpdateSection;
  end;
end;

function TElHeaderSection.GetVisible : boolean;
begin
  if FParentSection = nil then
    result := FVisible
  else
    result := FVisible and FParentSection.Visible and FParentSection.Expanded;
end;

procedure TElHeaderSection.SetVisible;
var
  ns, os, j, k, wo, wn, i : integer;
  HS : TElHeaderSection;
begin
  if FVisible <> Value then begin
    FOwner.BeginUpdate;
    try
  if (not FOwner.FStickySections) or ((Self.FParentSection <> nil) and (not FParentSection.Visible)) or (FOwner.FInStick) or (FOwner.IsLoading) then
  begin
    FVisible := value;
    FOwner.AdjustHeaderHeight;
    if (not FOwner.FInStick) and ((ParentSection = nil) or (ParentSection.Visible)) then
    begin
      FOwner.InvalidateRight(Index);
      //FOwner.Refresh;
    end;
  end
  else
  begin
    if Value then
    begin
      ns := Width;
      os := 0;
    end
    else
    begin
      ns := 0;
      os := Width;
    end;
    FVisible := value;
    wo := FOwner.Width - (Left + os);
    wn := FOwner.Width - (Left + ns);
    if Position = FOwner.FSections.Count - 1 then
    begin
      j := 0;
      k := FOwner.Sections.Count - 2;
    end
    else
    begin
      j := Position + 1;
      k := FOwner.Sections.Count - 1;
    end;
    for i := j to k do // Iterate
    begin
      HS := FOwner.FSections.ItemByPos[i];
      if HS.Resizable then
      begin
        HS.FWidth := Round((HS.Width / wo) * wn);
      end;
    end; // for
    FOwner.AdjustStickySize(Self);
    FOwner.MarkStickySections;
  end;
  if (FOwner.HandleAllocated) then
    FOwner.Invalidate;//Repaint;
  FOwner.DoVisChanged(self);
    finally
      FOwner.EndUpdate;
    end;
  end;
end;

function TElHeaderSection.GetWidth : integer;
begin
  result := FWidth;
  //if FOwner.FLockedSection <> self then result := FWidth else result := 0;
end;

function TElHeaderSection.GetLeft : integer;
var
  ind : integer;
begin
  ind := FOwner.Sections.FList.IndexOf(self);
  if ind = 0 then
    result := 0
  else
    result := TElHeaderSection(FOwner.FSections.FList[ind - 1]).Right;
end;

function TElHeaderSection.GetRight : integer;
var
  i : integer;
  ind : integer;
begin
  ind := FOwner.FSections.FList.IndexOf(self);
  if ind = 0 then
    i := 0
  else
    i := TElHeaderSection(FOwner.FSections.FList[ind - 1]).Right;
  if Visible then
    result := i + FWidth
  else
    result := i;
end;

procedure TElHeaderSection.SetText(value: TElFString);
begin
  if FText = value then exit;
  FText := value;
  if AutoSize then
     FOwner.TriggerSectionAutoSizeEvent(Self);
  FOwner.AdjustHeaderHeight;
  if FStyle = ElhsText then
     UpdateSection;
  FOwner.DoNotifySectionChange(self, scmCaption);
end;

procedure TElHeaderSection.SetStyle(value : TElSectionStyle);
begin
  if FStyle = value then exit;
  FStyle := value;
  FOwner.DoNotifySectionChange(self, scmStyle);
  UpdateSection;
end;

procedure TElHeaderSection.SetAlignment(value : TElSAlignment);
begin
  if FAlignment = value then exit;
  FAlignment := value;
  FOwner.DoNotifySectionChange(self, scmAlign);
  UpdateSection;
end;

function TElHeaderSection.GetIndex : integer;
begin
  result := FIndex;
end;

procedure TElHeaderSection.Assign(source : TPersistent);
var
  THS : TElHeaderSection;
begin
  if Source is TElHeaderSection then
  begin
    THS := TElHeaderSection(source);
    FFieldType := THS.FFieldType;
    FHint := THS.Hint;
    FVisible := THS.FVisible;
    FStyle := THS.FStyle;
    FText := THS.FText;
    FWidth := THS.FWidth;
    FTag := THS.FTag;
    FMinWidth := THS.FMinWidth;
    FMaxWidth := THS.FMaxWidth;
    FSortMode := THS.FSortMode;
    FAllowClick := THS.FAllowClick;
    FAlignment := THS.FAlignment;
    FExpandable := THS.FExpandable;
    FExpanded := THS.FExpanded;
    if THS.FParentSection = nil then
      FParentSection := nil
    else
      FParentSection := FOwner.Sections.Item[THS.FParentSection.Index];
    FPopupMenu := THS.PopupMenu;
    FPopupName := THS.FPopupName;
    FLookupEnabled := THS.FLookupEnabled;
    FUseMainStyle := THS.FUseMainStyle;
    FTextLayout := THS.FTextLayout;
    FFilterEnabled := THS.FFilterEnabled;
    FParentColor := THS.FParentColor;
    FFontColor := THS.FFontColor;
    FColor := THS.FColor;
    FAutoSize := THS.FAutoSize;
    FHint := THS.FHint;
    FPicAlign := THS.FPicAlign;
    FResizable := THS.FResizable;
    FClickSelect := THS.FClickSelect;
    FProtected := THS.FProtected;
    FImageIndex := THS.FImageIndex;
    FEditable := THS.FEditable;
    FShowSortMark := THS.FShowSortMark;
    FIndex := THS.FIndex;
  end
  else
    inherited;
end;

function TElHeaderSection.GetPosition : Integer;
{ Returns the value of data member FPosition. }
begin
  result := -1;
  if FOwner <> nil then result := FOwner.FSections.FList.IndexOf(self);
end; { GetPosition }

procedure TElHeaderSection.SetImageIndex(newValue : Integer);
{ Sets data member FImageIndex to newValue. }
begin
  if (FImageIndex <> newValue) then
  begin
    FImageIndex := newValue;
    if AutoSize then
       FOwner.TriggerSectionAutoSizeEvent(Self);
    UpdateSection;
  end; { if }
end; { SetImageIndex }

procedure TElHeaderSection.SetResizable(newValue : boolean);
begin
  FResizable := newValue;
  if Assigned(FOwner) then
    if FOwner.FStickySections then FOwner.SetStickySections(true);
end;

procedure TElHeaderSection.SetSaveSize(newValue : integer);
begin
  ASaveSize := newValue;
end;

procedure TElHeaderSection.SetEditable(newValue : boolean);
begin
  if FEditable <> newValue then
  begin
    FEditable := newValue;
    FOwner.DoNotifySectionChange(self, scmEditable);
  end;
end;

procedure TElHeaderSection.SetFieldName(newValue : string);
{ Sets data member FFieldName to newValue. }
begin
  if (FFieldName <> newValue) then
  begin
    FFieldName := newValue;
    FOwner.DoNotifySectionChange(self, scmFieldName);
  end; { if }
end; { SetFieldName }

procedure TElHeaderSection.SetFieldType(newValue : TElFieldType);
{ Sets data member FFieldType to newValue. }
begin
  if (FFieldType <> newValue) then
  begin
    FFieldType := newValue;
    FOwner.DoNotifySectionChange(self, scmFieldType);
  end; { if }
end; { SetFieldType }

procedure TElHeaderSection.SetProtected(newValue : Boolean);
{ Sets data member FProtected to newValue. }
begin
  if (FProtected <> newValue) then
  begin
    FProtected := newValue;
    FOwner.DoNotifySectionChange(self, scmPassword);
  end; { if }
end; { SetProtected }

procedure TElHeaderSection.SetExpandable(newValue : Boolean);
var
  i : integer;
  b : boolean;
begin
  if (FExpandable <> newValue) then
  begin
    b := false;
    FExpandable := newValue;
    if not FExpandable then
      for i := 0 to FOwner.FSections.Count - 1 do
      begin
        if TElHeaderSection(FOwner.FSections.FList[i]).FParentSection = Self then
        begin
          TElHeaderSection(FOwner.FSections.FList[i]).FParentSection := nil;
          b := true;
        end;
      end;
      if AutoSize then
         FOwner.TriggerSectionAutoSizeEvent(Self);
    if b and (not (csDestroying in FOwner.ComponentState)) and (FOwner.HandleAllocated) then
      FOwner.Invalidate;//Repaint;
  end; {if}
end;

procedure TElHeaderSection.SetExpanded(newValue : Boolean);
begin
  if (FExpanded <> newValue) and Expandable then
  begin
    if (FExpandable and newValue) or (not newValue) then
    begin
      FExpanded := newValue;
      if (FOwner.HandleAllocated) then
        FOwner.Invalidate;//Repaint;
    end;
  end; {if}
end;

procedure TElHeaderSection.SetParentSection(newValue : TElHeaderSection);
var
  S : TElHeaderSection;
begin
  if (FParentSection <> newValue) then
  begin
    S := newValue;
    while S <> nil do
    begin
      if S = Self then exit;
      S := S.FParentSection;
    end;
    FParentSection := newValue;
    FOwner.DoVisChanged(Self);
    FOwner.AdjustHeaderHeight;
    if (FOwner.HandleAllocated) then
      FOwner.Invalidate;//Repaint;
  end; {if}
end;

procedure TElHeaderSection.SetPopupMenu(newValue : TPopupMenu);
begin
  if (FPopupMenu <> newValue) then
  begin
    FPopupMenu := newValue;
    FPopupName := FPopupMenu.Name;
  end; {if}
end;

procedure TElHeaderSection.SetParentColor(newValue : Boolean);
begin
  if (FParentColor <> newValue) then
  begin
    FParentColor := newValue;
    UpdateSection;
  end; {if}
end;

procedure TElHeaderSection.SetColor(newValue : TColor);
begin
  if (FColor <> newValue) then
  begin
    FColor := newValue;
    ParentColor := false;
    UpdateSection;
  end; {if}
end;

procedure TElHeaderSection.SetFontColor(newValue : TColor);
begin
  if (FFontColor <> newValue) then
  begin
    FFontColor := newValue;
    ParentColor := false;
    UpdateSection;
  end; {if}
end;

procedure TElHeaderSection.SetUseMainStyle(newValue : Boolean);
begin
  if (FUseMainStyle <> newValue) then
  begin
    FUseMainStyle := newValue;
    if Assigned(FOwner.Parent) and (FOwner.Parent.HandleAllocated) then
      FOwner.Parent.Invalidate;//Repaint;
  end; {if}
end;

procedure TElHeaderSection.SetTextLayout(newValue : TTextLayout);
begin
  if (FTextLayout <> newValue) then
  begin
    FTextLayout := newValue;
    UpdateSection;
  end; {if}
end;

procedure TElHeaderSection.SetFilterEnabled(newValue : Boolean);
begin
  if (FFilterEnabled <> newValue) then
  begin
    FFilterEnabled := newValue;
    if newValue then
    begin
      FLookupEnabled := false;
      with FOwner do
      begin
        if (Height > 0) and (Height < 17) then Height := 17;
      end;
    end;
    if AutoSize then
       FOwner.TriggerSectionAutoSizeEvent(Self);
    UpdateSection;
  end; { if }
end; { SetFilterEnabled }

procedure TElHeaderSection.SetAutoSize;
begin
  if FAutoSize <> newValue then
  begin
    FAutoSize := newValue;
    if newValue then
       FOwner.TriggerSectionAutoSizeEvent(Self);
  end;
end;

procedure TElHeaderSection.SetLookupList(newValue : TStringList);
begin
  FLookupHist.Assign(newValue);
end;

procedure TElHeaderSection.SetFilterIsActive(newValue : Boolean);
{ Sets data member FFilterApplied to newValue. }
begin
  if (FFilterIsActive <> newValue) then
  begin
    FFilterIsActive := newValue;
    if FFilterEnabled then UpdateSection;
  end; { if }
end; { SetFilterApplied }
{$IFNDEF LITE}
function TElHeaderSection.GetLocked : boolean;
begin
  result := FOwner.FLockedSection = Self;
end;
{$ENDIF}
function TElHeaderSection.GetOwner : TPersistent; { protected }
begin
  result := FOwner.Sections;
end; { GetOwner }

destructor TElHeaderSection.Destroy;
begin
  Expandable := false;
  FLookupHist.Free;
  inherited;
end;

procedure TElHeaderSection.SetShowSortMark(Value: Boolean);
begin
  if FShowSortMark <> Value then
  begin
    FShowSortMark := Value;
    if Self.SortMode <> hsmNone then
      UpdateSection; 
  end;
end;

// =============================================================================

constructor TElHeaderSections.Create;
begin
  inherited Create;
  FOwner := AOwner;
  FList := TElList.Create;
end;

destructor TElHeaderSections.Destroy;
begin
  Clear;
  FList.Free;
  inherited destroy;
end;

procedure TElHeaderSections.Clear;
begin
  while FList.Count > 0 do
  begin
    TElHeaderSection(FList[0]).Free;
    FList.Delete(0);
  end;
end;

procedure TElHeaderSections.DefineProperties(Filer : TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', FrameReadData, WriteData, true);
end;

function TElHeaderSections.CreateSection : TElHeaderSection;
begin
  result := TElHeaderSection.Create(FOwner);
end;

function TElHeaderSections.AddSection : TElHeaderSection;
begin
  result := InsertSection(FList.Count);
end;

function TElHeaderSections.InsertSection(index : integer) : TElHeaderSection;
var
  i : integer;
begin
  FOwner.BeginUpdate;
  try
  result := CreateSection;
  i := 0;
  while FindSection(i) <> nil do
    inc(i);
  Result.FIndex := i;
  Result.FIntTag := Integer(Pointer(result));
  while Self.GetSectionByIntTag(Result.FIntTag) <> nil do
    inc(Result.FIntTag);
  Result.FOwner := FOwner;
  FList.Insert(index, result);
  FOwner.InvalidateRight(i);
  if FOwner.FUpdateCount = 0 then
    FOwner.DoSectionCreate(result)
  else
    FOwner.AdditionHappened := true; 
  finally
    FOwner.EndUpdate;
  end;
end;

procedure TElHeaderSections.DeleteSection(Section : TElHeaderSection);
var
  i : integer;
begin
  FOwner.BeginUpdate;
  try
  if Section = nil then raise EElHeaderError.Create('Invalid section');
{$IFNDEF LITE}
  if FOwner.FLockedSection = Section then FOwner.LockedSection := nil;
{$ENDIF}
  FList.Remove(Section);
  for i := 0 to FList.Count - 1 do
    TElHeaderSection(FList[i]).FIndex := i;
  if FOwner.FUpdateCount = 0 then
    FOwner.DoSectionDelete(Section)
  else
    FOwner.DeletionHappened := true;
  Section.Free;
  if (FOwner.HandleAllocated) then
    FOwner.Invalidate;
  finally
    FOwner.EndUpdate;
  end;
end;

procedure TElHeaderSections.MoveSection(Section : TElHeaderSection; NewPos : integer);
var
  i : integer;
begin
  FOwner.BeginUpdate;
  try
{$IFNDEF LITE}
  if ((Section = FOwner.FLockedSection) and (NewPos <> 0)) then exit;
  if ((FOwner.FLockedSection <> nil) and (Section <> FOwner.FLockedSection) and (NewPos = 0)) then NewPos := 1; 
{$ENDIF}
  i := FList.IndexOf(Section);
  FList.Move(i, NewPos);
  FOwner.DoSectionMove(TElHeaderSection(FList[i]), i, NewPos);
  if (FOwner.HandleAllocated) then
    FOwner.Invalidate;
  finally
    FOwner.EndUpdate;
  end;
end;

function TElHeaderSections.GetCount : integer;
begin
  result := FList.Count;
end;

procedure TElHeaderSections.SetSection(index : integer; Section : TElHeaderSection);
begin

end;

function TElHeaderSections.GetSectionByIntTag(IntTag : integer) : TElHeaderSection;
var
  i, j : integer;
begin
  j := FList.Count - 1;
  for i := 0 to j do
    if TElHeaderSection(FList[i]).FIntTag = IntTag then
    begin
      result := TElHeaderSection(FList[i]);
      exit;
    end;
  result := nil;
end;

function TElHeaderSections.GetSection;
begin
  result := FindSection(index);
end;

function TElHeaderSections.GetSectionByPos;
begin
  result := TElHeaderSection(FList[index]);
end;

type

  PSectionPersistInfo = ^TSectionPersistInfo;
  TSectionPersistInfo = record
    FFakeProperty1 : Integer;
    FLookupEnabled : boolean;
    FParentIdx : integer;
    FExpandable : Boolean;
    FExpanded : Boolean;
    FVisible : boolean;
    FClickSel : boolean;
    FResizable : boolean;
    FStyle : TElSectionStyle;
    FWidth : integer;
    FMinWidth,
      FMaxWidth : integer;
    FSortMode : TElSSortMode;
    FAllowClick : boolean;
    FAlignment : TElSAlignment;
    FIntTag,
    FIndex : integer;
    FFieldType : TElFieldType;
    FEditable : Boolean;
    FImageIndex : Integer;
    FPicAlign : TElSAlignment;
    FPassword : boolean;
    FParentColor : boolean;
    FColor, FFontColor : TColor;
    FFilterEnabled : boolean;
    FTextLayout : TTextLayout;
    FUseMainStyle : boolean;
    FAutoSize     : boolean;
    FTag          : integer;
    FOtherData    : array[0..16] of integer;
  end;

  P9SectionData = ^T9SectionData;
  T9SectionData = record
    FFakeProperty1 : Integer;
    FLookupEnabled : boolean;
    FParentIdx : integer;
    FExpandable : Boolean;
    FExpanded : Boolean;
    FVisible : boolean;
    FClickSel : boolean;
    FResizable : boolean;
    FStyle : TElSectionStyle;
    FWidth : integer;
    FMinWidth,
      FMaxWidth : integer;
    FSortMode : TElSSortMode;
    FAllowClick : boolean;
    FAlignment : TElSAlignment;
    FIntTag,
    FTag : integer;
    FFieldType : TElFieldType;
    FEditable : Boolean;
    FImageIndex : Integer;
    FPicAlign : TElSAlignment;
    FPassword : boolean;
    FParentColor : boolean;
    FColor, FFontColor : TColor;
    FFilterEnabled : boolean;
    FTextLayout : TTextLayout;
    FUseMainStyle : boolean;
    FAutoSize     : boolean;
  end;

  P8aSectionData = ^T8aSectionData;
  T8aSectionData = record
    FFakeProperty1 : Integer;
    FLookupEnabled : boolean;
    FParentIdx : integer;
    FExpandable : Boolean;
    FExpanded : Boolean;
    FVisible : boolean;
    FClickSel : boolean;
    FResizable : boolean;
    FStyle : TElSectionStyle;
    FWidth : integer;
    FMinWidth,
      FMaxWidth : integer;
    FSortMode : TElSSortMode;
    FAllowClick : boolean;
    FAlignment : TElSAlignment;
    FTag : integer;
    FFieldType : TElFieldType;
    FEditable : Boolean;
    FImageIndex : Integer;
    FPicAlign : TElSAlignment;
    FPassword : boolean;
    FParentColor : boolean;
    FColor, FFontColor : TColor;
    FFilterEnabled : boolean;
    FTextLayout : TTextLayout;
    FUseMainStyle : boolean;
    FAutoSize     : boolean;
  end;

  P8SectionData = ^T8SectionData;
  T8SectionData = record
    FFakeProperty1 : Integer;
    FLookupEnabled : boolean;
    FParentIdx : integer;
    FExpandable : Boolean;
    FExpanded : Boolean;
    FVisible : boolean;
    FClickSel : boolean;
    FResizable : boolean;
    FStyle : TElSectionStyle;
    FWidth : integer;
    FMinWidth,
      FMaxWidth : integer;
    FSortMode : TElSSortMode;
    FAllowClick : boolean;
    FAlignment : TElSAlignment;
    FTag : integer;
    FFieldType : TElFieldType;
    FEditable : Boolean;
    FImageIndex : Integer;
    FPicAlign : TElSAlignment;
    FPassword : boolean;
    FParentColor : boolean;
    FColor, FFontColor : TColor;
    FFilterEnabled : boolean;
    FTextLayout : TTextLayout;
    FUseMainStyle : boolean;
  end;

  P7SectionData = ^T7SectionData;
  T7SectionData = record
    FFakeProperty1 : Integer;
    FLookupEnabled : boolean;
    FParentIdx : integer;
    FExpandable : Boolean;
    FExpanded : Boolean;
    FVisible : boolean;
    FClickSel : boolean;
    FResiable : boolean;
    FStyle : TElSectionStyle;
    FWidth : integer;
    FMinWidth,
      FMaxWidth : integer;
    FSortMode : TElSSortMode;
    FAllowClick : boolean;
    FAlignment : TElSAlignment;
    FTag : integer;
    FFieldType : TElFieldType;
    FEditable : Boolean;
    FImageIndex : Integer;
    FPicAlign : TElSAlignment;
    FPassword : boolean;
  end;

type
  P5SectionData = ^T5SectionData;
  T5SectionData = record
    FVisible : boolean;
    FStyle : TElSectionStyle;
    FWidth : integer;
    FMinWidth,
      FMaxWidth : integer;
    FSortMode : TElSSortMode;
    FAllowClick : boolean;
    FAlignment : TElSAlignment;
    FTag : integer;
    FFieldType : TElFieldType;
    FEditable : Boolean;
    FImageIndex : Integer;
    FPicAlign : TElSAlignment;
    FPassword : boolean;
  end;

type
  P6SectionData = ^T6SectionData;
  T6SectionData = record
    FVisible : boolean;
    FClickSel : boolean;
    FResiable : boolean;
    FStyle : TElSectionStyle;
    FWidth : integer;
    FMinWidth,
      FMaxWidth : integer;
    FSortMode : TElSSortMode;
    FAllowClick : boolean;
    FAlignment : TElSAlignment;
    FTag : integer;
    FFieldType : TElFieldType;
    FEditable : Boolean;
    FImageIndex : Integer;
    FPicAlign : TElSAlignment;
    FPassword : boolean;
  end;

procedure TElHeaderSections.WriteData(Stream : TStream);
var
  i, j : integer;
  P : PSectionPersistInfo;
  TS : TElHeaderSection;
  P1 : PChar;
  S : string;
begin
  inherited;
  GetMem(P, SizeOf(TSectionPersistInfo));
  i := -12;
  Stream.WriteBuffer(i, SizeOf(integer));
  i := Count;
  Stream.WriteBuffer(i, SizeOf(integer));
  for i := 0 to count - 1 do
  begin
    TS := TElHeaderSection(FList[i]);
    P.FLookupEnabled := TS.FLookupEnabled;
    if TS.FParentSection <> nil then
      P.FParentIdx := TS.FParentSection.Index
    else
      P.FParentIdx := -1;
    P.FExpandable := TS.FExpandable;
    P.FExpanded := TS.FExpanded;
    P.FVisible := TS.FVisible;
    P.FStyle := TS.FStyle;
    P.FWidth := TS.FWidth;
    P.FResizable := TS.FResizable;
    P.FClickSel := TS.FClickSelect;
    P.FMinWidth := TS.FMinWidth;
    P.FMaxWidth := TS.FMaxWidth;
    P.FSortMode := TS.FSortMode;
    P.FAllowClick := TS.FAllowClick;
    P.FAlignment := TS.FAlignment;
    P.FIntTag := TS.FIntTag;
    P.FTag := TS.FTag;
    P.FFieldType := TS.FFieldType;
    P.FEditable := TS.FEditable;
    P.FImageIndex := TS.FImageIndex;
    P.FPicAlign := TS.FPicAlign;
    P.FPassword := TS.FProtected;
    P.FParentColor := TS.FParentColor;
    P.FColor := TS.FColor;
    P.FFontColor := TS.FFontColor;
    P.FFilterEnabled := TS.FFilterEnabled;
    P.FTextLayout := TS.FTextLayout;
    P.FUseMainStyle := TS.FUseMainStyle;
    P.FAutoSize := TS.FAutoSize;
    P.FIndex := TS.FIndex;

    Stream.WriteBuffer(p^, SizeOf(TSectionPersistInfo));

    S := TElHeaderSection(FList[i]).Text;
    j := Length(S) + 1;
    GetMem(P1, j);
    StrPCopy(P1, S);
    Stream.WriteBuffer(j, SizeOf(integer));
    Stream.WriteBuffer(P1^, j);
    FreeMem(P1, j);

    S := TElHeaderSection(FList[i]).FFieldName;
    j := Length(S) + 1;
    GetMem(P1, j);
    StrPCopy(P1, S);
    Stream.WriteBuffer(j, SizeOf(integer));
    Stream.WriteBuffer(P1^, j);
    FreeMem(P1, j);
    WriteStringToStream(Stream, TS.FPopupName);
    {$ifdef ELPACK_UNICODE}
    WriteWideStringToStream(Stream, TS.Hint);
    {$else}
    WriteStringToStream(Stream, TS.Hint);
    {$endif}
  end;
  FreeMem(P);
end;

procedure TElHeaderSections.ReadData(Stream : TStream);
begin
  IntReadData(Stream, true);
end;

procedure TElHeaderSections.FrameReadData(Stream : TStream);
begin
  IntReadData(Stream, false);
end;

procedure TElHeaderSections.IntReadData(Stream : TStream; ClearCurrent : boolean);
var
  i, j : integer;
  Q  : P6SectionData;
  T  : PSectionPersistInfo;
  T7 : P7SectionData;
  T9 : P9SectionData;
  T8a: P8aSectionData;
  T8 : P8SectionData;
  P1 : PChar;
  S  : string;
  THS : TElHeaderSection;
  Ver : integer;
  //failed killsection : boolean;
begin
  inherited;
  Stream.ReadBuffer(i, SizeOf(integer));
  ver := i;
  //failed if ClearCurrent or (ver > -10) then
    Clear;
  if ver <= -11 then
  begin
    Stream.ReadBuffer(i, SizeOf(integer));
    GetMem(T, SizeOf(TSectionPersistInfo));
    while i > 0 do
    begin
      Stream.ReadBuffer(T^, SizeOf(TSectionPersistInfo));
      //failed killsection := (GetSectionByIntTag(T^.FIntTag) <> nil) and (not ClearCurrent);
      THS := AddSection;
      THS.FLookupEnabled := T.FLookupEnabled;
      THS.FExpandable := T.FExpandable;
      THS.FExpanded := T.FExpanded;
      THS.FParentIdx := T.FParentIdx;
      THS.FResizable := T.FResizable;
      THS.FClickSelect := T.FClickSel;
      THS.FVisible := T.FVisible;
      THS.FStyle := T.FStyle;
      THS.FWidth := T.FWidth;
      THS.FMinWidth := T.FMinWidth;
      THS.FMaxWidth := T.FMaxWidth;
      THS.FSortMode := T.FSortMode;
      THS.FAllowClick := T.FAllowClick;
      THS.FAlignment := T.FAlignment;
      THS.FIntTag := T.FIntTag;
      THS.FIndex := T.FIndex;
      THS.FTag := T.FTag;
      THS.FFieldType := T.FFieldType;
      THS.FEditable := T.FEditable;
      THS.FImageIndex := T.FImageIndex;
      THS.FPicAlign := T.FPicAlign;
      THS.FProtected := T.FPassword;
      THS.FTextLayout := T.FTextLayout;
      THS.FUseMainStyle := T.FUseMainStyle;
      THS.FParentColor := T.FParentColor;
      THS.FColor := T.FColor;
      THS.FFontColor := T.FFontColor;
      THS.FFilterEnabled := T.FFilterEnabled;
      THS.FAutoSize := T.FAutoSize;

      // read caption
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FText := S;
      FreeMem(P1, j);
      // read field name
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FFieldName := S;
      FreeMem(P1, j);
      dec(i);
      ReadStringFromStream(Stream, THS.FPopupName);
      {$ifdef ELPACK_UNICODE}
      if ver <= -12 then
        ReadFStringFromStream(Stream, THS.FHint)
      else
      begin
        ReadStringFromStream(Stream, S);
        THS.FHint := S;
      end;
      {$else}
      ReadFStringFromStream(Stream, THS.FHint);
      {$endif}
    end;
    FreeMem(T);
    for i := 0 to Count - 1 do
    begin
      if Item[i].FParentIdx <> -1 then
      begin
        Item[i].FParentSection := Item[Item[i].FParentIdx];
      end;
    end;
  end
  else
  if ver = -10 then
  begin
    Stream.ReadBuffer(i, SizeOf(integer));
    GetMem(T9, SizeOf(T9SectionData));
    while i > 0 do
    begin
      Stream.ReadBuffer(T9^, SizeOf(T9SectionData));
      //failed killsection := (GetSectionByIntTag(T^.FIntTag) <> nil) and (not ClearCurrent);
      THS := AddSection;
      THS.FLookupEnabled := T9.FLookupEnabled;
      THS.FExpandable := T9.FExpandable;
      THS.FExpanded := T9.FExpanded;
      THS.FParentIdx := T9.FParentIdx;
      THS.FResizable := T9.FResizable;
      THS.FClickSelect := T9.FClickSel;
      THS.FVisible := T9.FVisible;
      THS.FStyle := T9.FStyle;
      THS.FWidth := T9.FWidth;
      THS.FMinWidth := T9.FMinWidth;
      THS.FMaxWidth := T9.FMaxWidth;
      THS.FSortMode := T9.FSortMode;
      THS.FAllowClick := T9.FAllowClick;
      THS.FAlignment := T9.FAlignment;
      THS.FIntTag := T9.FIntTag;
      THS.FIndex := T9.FTag;
      THS.FFieldType := T9.FFieldType;
      THS.FEditable := T9.FEditable;
      THS.FImageIndex := T9.FImageIndex;
      THS.FPicAlign := T9.FPicAlign;
      THS.FProtected := T9.FPassword;
      THS.FTextLayout := T9.FTextLayout;
      THS.FUseMainStyle := T9.FUseMainStyle;
      THS.FParentColor := T9.FParentColor;
      THS.FColor := T9.FColor;
      THS.FFontColor := T9.FFontColor;
      THS.FFilterEnabled := T9.FFilterEnabled;
      THS.FAutoSize := T9.FAutoSize;

      // read caption
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FText := S;
      FreeMem(P1, j);
      // read field name
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FFieldName := S;
      FreeMem(P1, j);
      dec(i);
      ReadStringFromStream(Stream, THS.FPopupName);
      {$ifdef ELPACK_UNICODE}
      ReadStringFromStream(Stream, S);
      THS.FHint := S;
      {$else}
      ReadFStringFromStream(Stream, THS.FHint);
      {$endif}
    end;
    FreeMem(T9);
    for i := 0 to Count - 1 do
    begin
      if Item[i].FParentIdx <> -1 then
      begin
        Item[i].FParentSection := Item[Item[i].FParentIdx];
      end;
    end;
  end
  else
  if (ver = -9) then
  begin
    Stream.ReadBuffer(i, SizeOf(integer));
    GetMem(T8a, SizeOf(T8aSectionData));
    while i > 0 do
    begin
      Stream.ReadBuffer(T8A^, SizeOf(T8ASectionData));
      THS := AddSection;
      THS.FLookupEnabled := T8A.FLookupEnabled;
      THS.FExpandable := T8A.FExpandable;
      THS.FExpanded := T8A.FExpanded;
      THS.FParentIdx := T8A.FParentIdx;
      THS.FResizable := T8A.FResizable;
      THS.FClickSelect := T8A.FClickSel;
      THS.FVisible := T8A.FVisible;
      THS.FStyle := T8A.FStyle;
      THS.FWidth := T8A.FWidth;
      THS.FMinWidth := T8A.FMinWidth;
      THS.FMaxWidth := T8A.FMaxWidth;
      THS.FSortMode := T8A.FSortMode;
      THS.FAllowClick := T8A.FAllowClick;
      THS.FAlignment := T8A.FAlignment;
      THS.FIndex := T8A.FTag;
      THS.FFieldType := T8A.FFieldType;
      THS.FEditable := T8A.FEditable;
      THS.FImageIndex := T8A.FImageIndex;
      THS.FPicAlign := T8A.FPicAlign;
      THS.FProtected := T8A.FPassword;
      THS.FTextLayout := T8A.FTextLayout;
      THS.FUseMainStyle := T8A.FUseMainStyle;
      THS.FParentColor := T8A.FParentColor;
      THS.FColor := T8A.FColor;
      THS.FFontColor := T8A.FFontColor;
      THS.FFilterEnabled := T8A.FFilterEnabled;
      THS.FAutoSize := T8A.FAutoSize;

      // read caption
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FText := S;
      FreeMem(P1, j);
      // read field name
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FFieldName := S;
      FreeMem(P1, j);
      dec(i);
      ReadStringFromStream(Stream, THS.FPopupName);
      {$ifdef ELPACK_UNICODE}
      ReadStringFromStream(Stream, S);
      THS.FHint := S;
      {$else}
      ReadFStringFromStream(Stream, THS.FHint);
      {$endif}
    end;
    FreeMem(T8a);
    for i := 0 to Count - 1 do
    begin
      if Item[i].FParentIdx <> -1 then
      begin
        Item[i].FParentSection := Item[Item[i].FParentIdx];
      end;
    end;
  end
  else
  if (ver = -8) then
  begin
    Stream.ReadBuffer(i, SizeOf(integer));
    GetMem(T8, SizeOf(T8SectionData));
    while i > 0 do
    begin
      Stream.ReadBuffer(T8^, SizeOf(T8SectionData));
      THS := AddSection;
      THS.FLookupEnabled := T8.FLookupEnabled;
      THS.FExpandable := T8.FExpandable;
      THS.FExpanded := T8.FExpanded;
      THS.FParentIdx := T8.FParentIdx;
      THS.FResizable := T8.FResizable;
      THS.FClickSelect := T8.FClickSel;
      THS.FVisible := T8.FVisible;
      THS.FStyle := T8.FStyle;
      THS.FWidth := T8.FWidth;
      THS.FMinWidth := T8.FMinWidth;
      THS.FMaxWidth := T8.FMaxWidth;
      THS.FSortMode := T8.FSortMode;
      THS.FAllowClick := T8.FAllowClick;
      THS.FAlignment := T8.FAlignment;
      THS.FIndex := T8.FTag;
      THS.FFieldType := T8.FFieldType;
      THS.FEditable := T8.FEditable;
      THS.FImageIndex := T8.FImageIndex;
      THS.FPicAlign := T8.FPicAlign;
      THS.FProtected := T8.FPassword;
      THS.FTextLayout := T8.FTextLayout;
      THS.FUseMainStyle := T8.FUseMainStyle;
      THS.FParentColor := T8.FParentColor;
      THS.FColor := T8.FColor;
      THS.FFontColor := T8.FFontColor;
      THS.FFilterEnabled := T8.FFilterEnabled;

      // read caption
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FText := S;
      FreeMem(P1, j);
      // read field name
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FFieldName := S;
      FreeMem(P1, j);
      dec(i);
      ReadStringFromStream(Stream, THS.FPopupName);
      {$ifdef ELPACK_UNICODE}
      ReadStringFromStream(Stream, S);
      THS.FHint := S;
      {$else}
      ReadFStringFromStream(Stream, THS.FHint);
      {$endif}
    end;
    FreeMem(T8);
    for i := 0 to Count - 1 do
    begin
      if Item[i].FParentIdx <> -1 then
      begin
        Item[i].FParentSection := Item[Item[i].FParentIdx];
      end;
    end;
  end
  else if (ver = -5) or (ver = -6) then
  begin
    Stream.ReadBuffer(i, SizeOf(integer));
    GetMem(T7, SizeOf(T7SectionData));
    while i > 0 do
    begin
      Stream.ReadBuffer(T7^, SizeOf(T7SectionData));
      THS := AddSection;
      THS.FLookupEnabled := T7.FLookupEnabled;
      THS.FExpandable := T7.FExpandable;
      THS.FExpanded := T7.FExpanded;
      THS.FParentIdx := T7.FParentIdx;
      THS.FResizable := T7.FResiable;
      THS.FClickSelect := T7.FClickSel;
      THS.FVisible := T7.FVisible;
      THS.FStyle := T7.FStyle;
      THS.FWidth := T7.FWidth;
      THS.FMinWidth := T7.FMinWidth;
      THS.FMaxWidth := T7.FMaxWidth;
      THS.FSortMode := T7.FSortMode;
      THS.FAllowClick := T7.FAllowClick;
      THS.FAlignment := T7.FAlignment;
      THS.FIndex := T7.FTag;
      THS.FFieldType := T7.FFieldType;
      THS.FEditable := T7.FEditable;
      THS.FImageIndex := T7.FImageIndex;
      THS.FPicAlign := T7.FPicAlign;
      THS.FProtected := T7.FPassword;
      // read caption
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FText := S;
      FreeMem(P1, j);
      // read field name
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FFieldName := S;
      FreeMem(P1, j);
      dec(i);
      if Ver = -6 then ReadStringFromStream(Stream, THS.FPopupName);
    end;
    FreeMem(T7);
    for i := 0 to Count - 1 do
    begin
      if Item[i].FParentIdx <> -1 then
      begin
        Item[i].FParentSection := Item[Item[i].FParentIdx];
      end;
    end;
  end
  else if i = -4 then
  begin
    Stream.ReadBuffer(i, SizeOf(integer));
    GetMem(q, SizeOf(T6SectionData));
    while i > 0 do
    begin
      Stream.ReadBuffer(q^, SizeOf(T6SectionData));
      THS := AddSection;
      THS.FResizable := Q^.FResiable;
      THS.FClickSelect := Q^.FClickSel;
      THS.FVisible := q^.FVisible;
      THS.FStyle := q^.FStyle;
      THS.FWidth := q^.FWidth;
      THS.FMinWidth := q^.FMinWidth;
      THS.FMaxWidth := q^.FMaxWidth;
      THS.FSortMode := q^.FSortMode;
      THS.FAllowClick := q^.FAllowClick;
      THS.FAlignment := q^.FAlignment;
      THS.FIndex := q^.FTag;
      THS.FFieldType := q^.FFieldType;
      THS.FEditable := q^.FEditable;
      THS.FImageIndex := q^.FImageIndex;
      THS.FPicAlign := q^.FPicAlign;
      THS.FProtected := q^.FPassword;
    // read caption
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FText := S;
      FreeMem(P1, j);
    // read field name
      Stream.ReadBuffer(J, SizeOf(integer));
      GetMem(P1, j);
      Stream.ReadBuffer(P1^, j);
      S := StrPas(P1);
      THS.FFieldName := S;
      FreeMem(P1, j);
      dec(i);
    end;
    FreeMem(q);
  end;
  FOwner.AdjustHeaderHeight;
  if (FOwner.HandleAllocated) then
    FOwner.Invalidate;//Repaint;
end;

procedure TElHeaderSections.LoadFromStream(Stream : TStream);
begin
  ReadData(Stream);
  if (FOwner <> nil) and (FOwner.HandleAllocated) then
    FOwner.Invalidate;
end;

procedure TElHeaderSections.SaveToStream(Stream : TStream);
begin
  WriteData(Stream);
end;

procedure TElHeaderSections.SaveToFile(FileName : string);
var
  T : TStream;
begin
  T := nil;
  try
    T := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    SaveToStream(T);
  finally
    T.Free;
  end;
end;

procedure TElHeaderSections.LoadFromFile(FileName : string);
var
  T : TStream;
begin
  T := nil;
  try
    T := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(T);
  finally
    T.Free;
  end;
end;

function TElHeaderSections.LastVisibleSection : TElHeaderSection;
begin
  result := TElHeaderSection(FList[Count - 1]);
  if not Result.Visible then result := GetPrevVisibleSection(Result);
end;

function TElHeaderSections.GetPrevVisibleSection;
var
  I : Integer;

begin
  result := nil;
  I := FList.IndexOf(Section) - 1;
  if i < 0 then exit;
  while i >= 0 do
  begin
    if TElHeaderSection(FList[i]).Visible then
    begin
      result := TElHeaderSection(FList[i]);
      break;
    end;
    dec(i);
  end; // while
end;

procedure TElHeaderSections.Assign;
var
  IT : TElHeaderSections;
  j : integer;
begin
  if source is TElHeaderSections then
  begin
    FOwner.BeginUpdate;
    try
      IT := TElHeaderSections(source);
      // delete old sections
      while FList.Count > 0 do
        DeleteSection(Item[0]);
      // add new sections
      for j := 0 to IT.Count - 1 do
        AddSection.Assign(IT.ItemByPos[j]);
      for j := 0 to IT.Count - 1 do
      begin
        TElHeaderSection(FList[j]).FIndex := IT.ItemByPos[j].FIndex;
        TElHeaderSection(FList[j]).FIntTag := IT.ItemByPos[j].FIntTag;
      end;
      FOwner.MarkStickySections;
      FOwner.Invalidate;
    finally
      FOwner.EndUpdate;
    end;
  end
  else
    inherited;
end;

function TElHeaderSections.GetOwner : TPersistent;
begin
  result := FOwner;
end;

function TElHeaderSections.FindSection(tag : integer) : TElHeaderSection; { protected }
var
  i, j : integer;
begin
  j := FList.Count - 1;
  for i := 0 to j do
    if TElHeaderSection(FList[i]).FIndex = tag then
    begin
      result := TElHeaderSection(FList[i]);
      exit;
    end;
  result := nil;
end; { FindSection }

// =============================================================================

constructor TCustomElHeader.Create(AOwner : TComponent);
begin
  inherited;
{$IFNDEF VER90}
  ControlStyle := [csClickEvents, csDisplayDragImage, csDoubleClicks, csCaptureMouse,
    {csDesignInteractive,} csReplicatable, csOpaque{$ifndef CLX_USED}, csReflector{$endif}];
{$ELSE}
  ControlStyle := [csClickEvents, csDisplayDragImage, csDoubleClicks, csCaptureMouse,
    {csDesignInteractive,} csReplicatable, csOpaque];
{$ENDIF}
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
{$endif}
{$ENDIF}
  FSections := CreateSections;
  TabStop := false;
  Font.OnChange := OnFontChange;
  {$ifndef CLX_USED}
  Height := abs(Font.Height) + 6 + GetSystemMetrics(SM_CYBORDER) * 2;
  {$else}
  Height := abs(Font.Height) + 6 + QStyle_defaultFrameWidth(Application.Style.Handle);
  {$endif}
  DragMode := dmManual;
  FPressed := false;
  {$ifndef CLX_USED}
  DragCursor := Cursor;
  {$endif}

  FPressed := false;
  FTracking := true;
  FAllowDrag := true;
  FResizeOnDrag := false;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := OnImageListChange;
  Color := clBtnFace;
  Align := alTop;
  FFilterColor := clBtnText;
  FInvertSortArrows := False;
  FDefaultWidth := 120;
{$ifdef HAS_HTML_RENDER}
  FRender := TElHTMLRender.Create;
  FRender.OnImageNeeded := TriggerImageNeededEvent;
{$endif}
end;

destructor TCustomElHeader.Destroy;
begin
  Destroying;
  FSections.Free;
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
  ImageForm := nil;
  FImgFormChLink.Free;
  FImgFormChLink := nil;
{$endif}
{$ENDIF}
  if FImages <> nil then
     FImages.UnregisterChanges(FImageChangeLink);
  FImageChangeLink.Free;
{$ifdef HAS_HTML_RENDER}
  FRender.Free;
{$endif}
  inherited Destroy;
end;

function TCustomElHeader.CreateSections;
begin
  result := TElHeaderSections.Create(self);
end;

function TCustomElHeader.GetSectionAtEx(X, Y : integer; var SectionPart : TElSectionPart) : TElHeaderSection;
var
  THS : TElHeaderSection;
  rm : integer;
begin
  if InResizeArea(X, THS) then
  begin
    result := THS;
    SectionPart := espResizeArea;
    exit;
  end
  else
  begin
    THS := GetSectionAt(X, Y);
    if THS <> nil then
    begin
      rm := THS.Right - 4;
      if THS.Expandable then
      begin
        if ((rm + 13 - THS.Width) + (ResizeWidth - 4) < RM)
          and InRange(rm - 6 - (ResizeWidth - 4), RM, X)
          and InRange(2, 9, Y) then
        begin
          result := THS;
          SectionPart := espExpandSign;
          exit;
        end;
      end;
      if THS.FLookupEnabled then
      begin
        if ((rm + 13 - THS.Width) + (ResizeWidth - 4) < RM)
          and InRange(rm - 6 - (ResizeWidth - 4), RM, X)
          and InRange(12, 16, Y) then
        begin
          result := THS;
          SectionPart := espLookupSign;
          exit;
        end;
      end
      else if THS.FFilterEnabled then
      begin
        if ((rm + 13 - THS.Width) + (ResizeWidth - 4) < RM)
          and InRange(rm - 6 - (ResizeWidth - 4), RM, X)
          and InRange(12, 16, Y) then
        begin
          result := THS;
          SectionPart := espFilterSign;
          exit;
        end;
      end;
    end;
  end;
  result := THS;
  SectionPart := espText;
end;

function TCustomElHeader.GetSectionAt(X, Y : integer) : TElHeaderSection;
var
  i : integer;
  j : integer;
  ASection : TElHeaderSection;
begin
  j := 0;
  result := nil;
{$IFNDEF LITE}
  if (FLockedSection <> nil) and (FLockedSection.Visible) then
  begin
    if (X >= 0) and (X < FLockedSection.Width) then
    begin
      result := FLockedSection;
      if (Y < 0) or (Y > Height) then
        result := nil;
      exit;
    end;
    //X := X + FLockedSection.Width;
  end;
{$ENDIF}
  X := X + FHPos;
  for i := 0 to FSections.FList.Count - 1 do
  begin
    ASection := TElHeaderSection(FSections.FList[i]);
    //if ASection = FLockedSection then Continue;
    if ASection.Visible then j := j + ASection.Width;
    if j > X then
    begin
      result := ASection;
      if (Y < 0) or (Y > Height) then
        result := nil;
      exit;
    end;
  end;
end;

procedure TCustomElHeader.InvalidateRight(value : integer);
var
  R : TRect;
begin
  if not inRange(0, Sections.Count - 1, value) then raise EElHeaderError.Create('Index out of bounds');
  if not HandleAllocated then exit;
  R := Rect(TElHeaderSection(Sections[value]).Left, 0, ClientWidth, ClientHeight);
  if (HandleAllocated) then
  {$ifndef CLX_USED}
    InvalidateRect(Handle, @R, false);
  {$else}
    Inc(R.Bottom); Inc(R.Right);
    QWidget_update(Handle, @R);
    Dec(R.Bottom); Dec(R.Right);
  {$endif}
  //Update;
end;

function TCustomElHeader.GetColumnsWidth : integer;
var
  i : integer;
begin
  result := 0;
  i := FSections.Count - 1;
  while i >= 0 do
  begin
    if (TElHeaderSection(FSections.FList[i]).Visible) and
       ((FLockedSection <> TElHeaderSection(FSections.FList[i])) or
       (FSections.Count = 1)) then
    begin
      result := TElHeaderSection(FSections.FList[i]).Right;
      exit;
    end;
    dec(i);
  end;
end;

function TCustomElHeader.InResizeArea(X : integer; var HitSection : TElHeaderSection) : boolean;
var
  THS : TElHeaderSection;
  l : integer;
begin
  THS := GetSectionAt(X, 0);
  HitSection := THS;
  if THS = nil then
  begin
    if Sections.Count > 0 then THS := Sections.LastVisibleSection;
    if THS <> nil then
    begin
      l := THS.Right;
{$IFNDEF LITE}
      if THS <> FLockedSection then
{$ENDIF}
      dec(l, FHPos);

      if L + ResizeWidth > X then
      begin
        result := true;
        HitSection := THS;
        exit;
      end;
    end;
    result := false;
    HitSection := nil;
    exit;
  end
  else
  begin
    if THS.Width < ResizeWidth * 3 then
      result := false
    else
    begin
      l := THS.Left;
{$IFNDEF LITE}
      if THS <> FLockedSection then
{$ENDIF}
      dec(l, FHPos);
      if L + ResizeWidth > X then
      begin
        if Sections.GetPrevVisibleSection(THS) = nil then
        begin
          result := false;
          HitSection := THS;
          exit;
        end;
        result := true;
        HitSection := Sections.GetPrevVisibleSection(THS);
        exit;
      end;
      if L + THS.FWidth - ResizeWidth <= X then
      begin
        result := true;
        exit;
      end;
      result := false;
    end;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElHeader.WMCancelMode(var Msg: TMessage);
var Msg2 : TWMLButtonUp;
begin
  if (FAllowDrag) and (not FPressed) and (FDropSrc <> nil) then
  begin
    Msg2.Keys := 0;
    Msg2.XPos := -1;
    Msg2.YPos := -1;
    WMLButtonUp(Msg2);
  end;
  if FResizing then
  begin
    Msg2.Keys := 0;
    Msg2.XPos := -1;
    Msg2.YPos := -1;
    WMLButtonUp(Msg2);
  end;
  inherited;
  Msg.result := 0;
end;

procedure TCustomElHeader.WMLButtonDown(var Message : TWMLButtonDown);
begin
  IntLButtonDown(Message.XPos, Message.YPos);
  inherited;
end;

procedure TCustomElHeader.WMMouseMove(var Message : TWMMouseMove);
begin
  IntMouseMove(Message.XPos, Message.YPos);
  inherited;
end;

procedure TCustomElHeader.WMLButtonUp(var Message : TWMLButtonUp);
begin
  IntLButtonUp(Message.XPos, Message.YPos);
  inherited;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElHeader.CMMouseLeave;
begin
  IntMouseLeave;
  inherited;
end;

procedure TCustomElHeader.CMMouseEnter;
begin
  inherited;
  IntMouseEnter;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElHeader.CMDrag(var Message : TCMDrag);
var
  TSI : TElHeaderSection;
begin
  inherited;
  with Message, DragRec^ do
    case DragMessage of
//      dmDragMove: with ScreenToClient(Pos) do DoDragOver(Source, X, Y, Message.Result<>0);
      dmDragLeave,
      dmDragDrop :
        begin
          TSI := FDropTrg;
          FDropTrg := nil;
          if TSI <> nil then
          begin
            TSI.UpdateSection;
            Update;
          end;
        end;
      dmDragCancel :
        begin
          TSI := FDropTrg;
          FDropTrg := nil;
          if TSI <> nil then
          begin
            TSI.UpdateSection;
            Update;
          end;
          FDropSrc := nil;
        end;
    end;
end;
{$endif}

function TCustomElHeader.GetSectionRect(SectionNum : integer) : TRect;
var
  THS : TElHeaderSection;
  i : integer;
begin
  THS := Sections.FindSection(SectionNum);
  i := THS.Left;
  result := Rect(i, 0, i + THS.Width, ClientHeight);
{$IFNDEF LITE}
  if (FLockedSection = THS) then OffsetRect(Result, FHPos, 0);
{$ENDIF}
end;

function TCustomElHeader.DoGetPicture;
begin
  result := -1;
  if Assigned(FOnPictureNeeded) then FOnPictureNeeded(self, Section, result);
end;

function TCustomElHeader.MeasureSectionWidth(Section : TElHeaderSection; TextWidth : PInteger; SectionHeight : PInteger) : integer;
var p  : TPoint;
    StImIndex : integer;
    R  : TRect;
    AL : integer;
    S  : TElFString;
begin
  result := 10; // minimal width for resize area and borders
  if TextWidth <> nil then
     TextWidth^ := 0;
  if SectionHeight <> nil then
     SectionHeight^ := 0;

  try
    if (Section.FExpandable or
        Section.FLookupEnabled or
        Section.FFilterEnabled) then
        inc(Result, 8);
    {if Section.FSortMode <> hsmNone then }inc(result, 12);
    inc(result, ResizeWidth);
    if Section.Style = elhsOwnerdraw then
    begin
      p.x := 0;
      Self.TriggerMeasureSectionEvent(Section, P);
      inc(result, p.x);
      if SectionHeight <> nil then
        SectionHeight^ := p.y;
    end
    else
    begin
      S := Section.Text;
      if (FImages <> nil) then
      begin
        StImIndex := Section.FImageIndex;
        if StImIndex = -1 then StImIndex := DoGetPicture(Section);
        if InRange(0, FImages.Count - 1, StImIndex) then
           inc(result, FImages.Width);
      end;
      if (Section.Style = elhsPictureOnly) then exit;
      SetRectEmpty(R);
{$ifdef HAS_HTML_RENDER}
      if Pos('<html>', S) = 1 then
      begin
        FRender.Data.DefaultBgColor := Color;
        FRender.Data.DefaultColor := Font.Color;
        FRender.Data.DefaultStyle := Font.Style;
        FRender.Data.DefaultHeight := Font.Height;
        FRender.Data.DefaultFont := Font.Name;
        FRender.Data.Charset := Font.Charset;
        FRender.PrepareText(S, 0, false);
        R.Right := FRender.Data.TextSize.cx;
        R.Bottom := FRender.Data.TextSize.cy;
      end
      else
{$endif}
      begin
{$ifndef CLX_USED}
{$IFNDEF LITE}
        if WrapCaptions then
          AL := 0
        else
{$ENDIF}
          AL := DT_SINGLELINE;
        {$ifdef ELPACK_UNICODE}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R, DT_NOPREFIX or AL or DT_CALCRECT);
        {$else}
        DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_NOPREFIX or AL or DT_CALCRECT);
        {$endif}
{$else}
{$IFNDEF LITE}
        if WrapCaptions then
           AL := Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignTop)
        else
{$ENDIF}
           AL := Integer(AlignmentFlags_AlignLeft) or
                 Integer(AlignmentFlags_AlignTop) or
                 Integer(AlignmentFlags_SingleLine);
        R.Left := 0; R.Top := 0;
        Canvas.TextExtent(S, R, Al);
{$endif}
      end;
      inc(result, R.Right);
      if TextWidth <> nil then
        TextWidth^ := R.Right - R.Left;
      if SectionHeight <> nil then
      begin
        if Length(S) = 0 then
          SectionHeight^ := Abs(Canvas.Font.Height)
        else
          SectionHeight^ := R.Bottom;
      end;
    end;
  finally
    if result < Section.MinWidth then
       result := Section.MinWidth;
  end;
end;

{$hints off}
procedure TCustomElHeader.RedrawSection(Canvas : TCanvas; Section : TElHeaderSection; R : TRect; Dithered : Boolean);
var
  TS          : TElHeaderSection;
  R1,
  R2,
  BgRect      : TRect;
  w           : integer;
  {$ifndef CLX_USED}
  DC          : THandle;
  {$endif}
  s           : TElFString;
  SaveCol,
    SaveColor : TColor;
  SaveStyle   : TFontStyles;
  StImIndex   : integer;
  BMP         : TBitmap;
  AL          : integer;
  ACtl        : TWinControl;
  imfd        : boolean;
  C           : TColor;
  hls2        : integer;
  lum         : integer;
  sid, sid1   : integer;
  PS          : TSize;
begin
  sid := 0;
  sid1 := 0;
  PS.cx := 0;
  TS := Section;

  if TS.FParentColor then
    C := Color
  else
    C := TS.FColor;

  if Dithered then
  begin
    hls2 := RGBtoHLS(ColorToRGB(C));
    lum := Hi(hls2 and $FFFF);
    Dec(lum, lum shr 2);
    if lum > 252 then
      lum := 252;
    hls2 := (lum shl 8) or (hls2 and $FF00FF);
    C := TColor(HLStoRGB(hls2))
  end;

  Canvas.Brush.Color := C;

  imfd := false;
  {$ifdef MSWINDOWS}
  if not IsThemeApplied then
  {$endif}
  begin
  {$IFDEF ELPACK_COMPLETE}
    {$ifndef CLX_USED}
    if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
    begin
      if (FImgForm.Control <> Self) then
      begin
        ACtl := FImgForm.GetRealControl;
        BgRect := R;
        OffsetRect(BgRect, -Section.Left + FHPos, 0);
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);
        FImgForm.PaintBkgnd(Canvas.Handle, R, BgRect.TopLeft, false);
      end;
      imfd := true;
    end
    else
    {$endif}
  {$ENDIF}
      Canvas.FillRect(R);

    {$ifndef CLX_USED}
    DC := Canvas.Handle;
    {$endif}
    if (TS = FPressedItem) and (FPressed = true) then
    begin
      if Flat then
      begin
        dec(R.Bottom); dec(R.Right);
        {$ifndef CLX_USED}
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT);
        {$else}
        DrawEdge(Canvas, R, esNone, esLowered, [ebLeft, ebTop, ebRight, ebBottom]);
        {$endif}
        inc(R.Bottom); inc(R.Right);
        InflateRect(R, -1, -1);
        OffsetRect(R, 1, 1);
      end
      else
      begin
        dec(R.Bottom); dec(R.Right);
        {$ifndef CLX_USED}
        DrawEdge(DC, R, BDR_SUNKENINNER, BF_TOPLEFT);
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_BOTTOMRIGHT);
        {$else}
        DrawEdge(Canvas, R, esLowered, esNone, [ebLeft, ebTop]);
        DrawEdge(Canvas, R, esNone, esLowered, [ebRight, ebBottom]);
        {$endif}
        inc(R.Bottom); inc(R.Right);

        InflateRect(R, -1, -1);
        OffsetRect(R, 1, 1);
      end;
    end
    else
    begin
      if Flat then
      begin
        {$ifndef CLX_USED}
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_RECT);
        {$else}
        dec(R.Bottom); dec(R.Right);
        DrawEdge(Canvas, R, esNone, esRaised, [ebLeft, ebTop, ebRight, ebBottom]);
        inc(R.Bottom); inc(R.Right);
        {$endif}
      end
      else
      begin
        {$ifndef CLX_USED}
        DrawEdge(DC, R, BDR_RAISEDOUTER, BF_BOTTOMRIGHT); { black }
        {$else}
        dec(R.Bottom); dec(R.Right);
        DrawEdge(Canvas, R, esNone, esRaised, [ebRight, ebBottom]);
        inc(R.Bottom); inc(R.Right);
        {$endif}
        Dec(R.Bottom);
        Dec(R.Right);
        {$ifndef CLX_USED}
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_TOPLEFT); { btnhilite }
        {$else}
        //dec(R.Bottom); dec(R.Right);
        DrawEdge(Canvas, R, esNone, esRaised, [ebLeft, ebTop]);
        inc(R.Bottom); inc(R.Right);
        {$endif}
        Inc(R.Top);
        Inc(R.Left);
        {$ifndef CLX_USED}
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_BOTTOMRIGHT); { btnshadow }
        {$else}
        dec(R.Bottom); dec(R.Right);
        DrawEdge(Canvas, R, esRaised, esNone, [ebRight, ebBottom]);
        inc(R.Bottom); inc(R.Right);
        {$endif}
        Dec(R.Bottom);
        Dec(R.Right);
      end;
    end;

    {$ifndef CLX_USED}
    inc(R.Top);
    {$else}
    dec(R.Top);
    {$endif}
    inc(R.Left);
    dec(R.Bottom);
  end
  {$ifdef MSWINDOWS}
  else
  begin
    imfd := true;
    if ((TS = FPressedItem) and (FPressed = true)) or
       ((TS = FDropSrc) and not Dithered) then
      sid := HIS_PRESSED
    else
    if (TS = FTrackSection) or ((TS = FDropSrc) and Dithered) then
      sid := HIS_HOT
    else
      sid := HIS_NORMAL;
    {$ifndef CLX_USED}
    DrawThemeBackground(Theme, Canvas.Handle, HP_HEADERITEM, sid, R, @R);
    {$else}
    Canvas.Start;
    DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), HP_HEADERITEM, sid, R, @R);
    Canvas.Stop;
    {$endif}
  end
  {$endif}
  ;

  dec(R.Right, 4);
  if (TS.FExpandable or TS.FLookupEnabled or TS.FFilterEnabled) and (R.Left + 6 + (ResizeWidth - 4) < R.Right) then
  begin
    if TS.Expandable then
    begin
      R2 := Rect(R.Right - 5, R.Top, R.Right, R.Top + 7);
      if TS.Expanded then
        DrawArrow(Canvas, eadLeft, R2, clBtnText, true)
      else
        DrawArrow(Canvas, eadRight, R2, clBtnText, true);
    end;
    if TS.FLookupEnabled then
    begin
      BMP := ElHeaderPointBmp;
      R2 := Rect(R.Right - 5, R.Top + 10, R.Right, Min(R.Top + 14, R.Bottom));
      if imfd then
      {$ifndef CLX_USED}
        DrawTransparentBitmapEx(Canvas.Handle, Bmp, R2.Left, R2.Top, Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R2.Top), BMP.TransparentColor)
      {$else}
        Canvas.CopyRect(R2, Bmp.Canvas, Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R2.Top))
      {$endif}
      else
      {$ifndef CLX_USED}
        Canvas.BrushCopy(R2, Bmp, Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R2.Top), BMP.TransparentColor)
      {$else}
        Canvas.CopyRect(R2, Bmp.Canvas, Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R2.Top));
        //DrawTransparentBitmapEx(Canvas.Handle, Bmp, R2.Left, R2.Top, Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R2.Top), BMP.TransparentColor);
      {$endif}
    end
    else if TS.FFilterEnabled then
    begin
      SaveColor := Canvas.Brush.Color;
      SaveCol := Canvas.Pen.Color;
      if TS.FFilterIsActive then
        Canvas.Brush.Color := FActiveFilterColor
      else
        Canvas.Brush.Color := FFilterColor;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Polygon([Point(R.Right - 4, R.Top + 10), Point(R.Right, R.Top + 10), Point(R.Right - 2, Min(R.Top + 14, R.Bottom))]);
      Canvas.Brush.Color := SaveColor;
      Canvas.Pen.Color := SaveCol;
    end;
    Dec(R.Right, 8);
  end;
  R1 := R;
  if (TS.FSortMode <> hsmNone) and (TS.FShowSortMark) then
  begin
    {$ifdef MSWINDOWS}
    if false {IsThemeApplied} then
    begin
      if (TS.FSortMode = hsmAscend) xor (InvertSortArrows) then
        sid1 := HSAS_SORTEDDOWN
      else
        sid1 := HSAS_SORTEDUP;
      {$ifndef CLX_USED}
      GetThemePartSize(Theme, Canvas.Handle, HP_HEADERSORTARROW, sid1, @R, TS_TRUE, PS);
      {$else}
      Canvas.Start;
      GetThemePartSize(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), HP_HEADERSORTARROW, sid1, @R, TS_TRUE, PS);
      Canvas.Stop;
      {$endif}
      PS.CX := 9;
      PS.cy := 9;
      R2 := R1;
      if R1.Right - PS.CX < R1.Left + ResizeWidth then
      begin
        // w := 0;
        R1.Right := R1.Left + ResizeWidth;
        R2.Left := R1.Left;
      end
      else
      begin
        w := PS.CX + 1;
        dec(R1.Right, PS.CX + 1);
        R2.Left := R1.Right - w;
      end;
      R2.Right := R1.Right;

      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, HP_HEADERSORTARROW, sid1, R2, @R1);
      {$else}
      Canvas.Start;
      DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)),HP_HEADERSORTARROW, sid1, R2, @R1);
      Canvas.Stop;
      {$endif}
    end
    else
    {$endif}
    begin
      if R1.Right - 8 < R1.Left + ResizeWidth then
      begin
        w := 0;
        R1.Right := R1.Left + ResizeWidth;
      end
      else
      begin
        w := 9;
        dec(R1.Right, 9);
      end;
      R2 := Rect(R1.Right, ((R1.Bottom - R.Top) div 2 + R.Top) - 3, R1.Right + W, ((R1.Bottom - R.Top) div 2 + R.Top) + 3);
      Canvas.Brush.Color := C;

      if w > 0 then
      begin
        if (TS.FSortMode = hsmAscend) xor (InvertSortArrows) then
          BMP := ElHeaderAscBmp
        else
          BMP := ElHeaderDescBmp;
        if imfd then
        {$ifndef CLX_USED}
          DrawTransparentBitmapEx(Canvas.Handle, Bmp, R2.Left, R2.Top, Rect(0, 0, 9, 6), Bmp.Canvas.Pixels[0, 5])
        {$else}
          Canvas.CopyRect(R2, Bmp.Canvas, Rect(0, 0, 9, 6))
        {$endif}
        else
        {$ifndef CLX_USED}
          Canvas.BrushCopy(R2, Bmp, Rect(0, 0, 9, 6), Bmp.Canvas.Pixels[0, 5]);
        {$else}
          Canvas.CopyRect(R2, Bmp.Canvas, Rect(0, 0, 9, 6));
        {$endif}
        dec(R1.Right, 3);
      end;
    end;
  end;
  {if TS.FResizable then }
  inc(R1.Left, ResizeWidth);
  if R1.Right < R1.Left then exit;
  if TS.FStyle = ElhsOwnerDraw then
  begin
    DoSectionDraw(Canvas, TS, R1, (TS = FPressedItem) and (FPressed = true));
  end
  else
  begin
    if TS.FStyle = elhsPictureOnly then
    begin
      if (FImages <> nil) then
      begin
        StImIndex := TS.FImageIndex;
        if StImIndex = -1 then StImIndex := DoGetPicture(TS);
        if InRange(0, FImages.Count - 1, StImIndex) then
        begin
          BMP := TBitmap.Create;
          BMP.Width := FImages.Width;
          BMP.Height := FImages.Height;

          R2 := Rect(Max(R1.Left + ((R1.Right - R1.Left + 1) div 2) - (FImages.Width div 2), R1.Left),
              R1.Top + ((R1.Bottom - R1.Top + 1) div 2) - (FImages.Height div 2),
              Min(R1.Right, R1.Left + ((R1.Right - R1.Left + 1) div 2) - (FImages.Width div 2) + FImages.Width),
              R1.Top + FImages.Height);
          Bmp.Canvas.CopyRect(Rect(0, 0, min(FImages.Width, Min(R1.Right, R1.Left + FImages.Width - 1) - R1.Left + 1), FImages.Height), Canvas, R2);

          if R.Left < R.Right then
          begin
            FImages.Draw(BMP.Canvas, 0, 0, StImIndex);
            Canvas.CopyRect(R2, BMP.Canvas, Rect(0, 0, min(FImages.Width, Min(R1.Right, R1.Left + FImages.Width - 1) - R1.Left + 1), FImages.Height));
          end;
          inc(R1.Left, FImages.Width);
          BMP.Free;
        end;
      end;
      exit;
    end;
    if (FImages <> nil) and (TS.FPicAlign = hsaLeft) then
    begin
      StImIndex := TS.FImageIndex;
      if StImIndex = -1 then StImIndex := DoGetPicture(TS);
      if InRange(0, FImages.Count - 1, StImIndex) then
      begin
        BMP := TBitmap.Create;
        BMP.Width := FImages.Width;
        BMP.Height := FImages.Height;

        R2 := Rect(R1.Left, (R1.Bottom + R1.Top + 1) div 2 - (FImages.Height div 2), Min(R1.Right, R1.Left + FImages.Width), 0);
        R2.Bottom := R2.Top + FImages.Height;

        Bmp.Canvas.CopyRect(Rect(0, 0, min(FImages.Width, Min(R1.Right, R1.Left + FImages.Width - 1) - R1.Left + 1), FImages.Height), Canvas, R2);

        if R.Left < R.Right then
        begin
          // BMP.Canvas.Brush.Color := Canvas.Brush.Color;
          // BMP.Canvas.FillRect(Rect(0, 0, FImages.Width, FImages.Height));
          FImages.Draw(BMP.Canvas, 0, 0, StImIndex);
{$IFNDEF VER90}
          BMP.TransparentMode := tmAuto;
{$ENDIF}
          Canvas.CopyRect(R2, BMP.Canvas, Rect(0, 0, min(FImages.Width, Min(R1.Right, R1.Left + FImages.Width - 1) - R1.Left + 1), FImages.Height));
        end;
        inc(R1.Left, FImages.Width);
        BMP.Free;
      end;
    end;
    if R1.Right < R1.Left then exit;
    if TS.FStyle = elhsText then
    begin
      S := TS.Text;
{$ifdef HAS_HTML_RENDER}
      if Pos('<html>', s) = 1 then
      begin
        FRender.Data.DefaultBgColor := C;
        FRender.Data.DefaultColor := Font.Color;
        FRender.Data.DefaultStyle := Font.Style;
        FRender.Data.DefaultHeight := Font.Height;
        FRender.Data.DefaultFont := Font.Name;
        FRender.Data.Charset := Font.Charset;
        FRender.PrepareText(S, 0, false);
        FRender.DrawText(Canvas, Point(0, 0), R1, clNone);
      end
      else
{$endif}
      begin
        if FRightAlignedText then
        begin
          {$ifndef CLX_USED}
          Al := DT_RIGHT;
          case TS.FAlignment of
            hsaCenter : Al := DT_CENTER;
            hsaRight : Al := DT_LEFT;
          end; // case
          {$else}
          Al := Integer(AlignmentFlags_AlignRight);
          case TS.FAlignment of
            hsaCenter : Al := Integer(AlignmentFlags_AlignHCenter);
            hsaRight : Al := Integer(AlignmentFlags_AlignLeft);
          end; // case
          {$endif}
        end
        else
        begin
          {$ifndef CLX_USED}
          Al := DT_LEFT;
          case TS.FAlignment of
            hsaCenter : Al := DT_CENTER;
            hsaRight : Al := DT_RIGHT;
          end; // case
          {$else}
          Al := Integer(AlignmentFlags_AlignLeft);
          case TS.FAlignment of
            hsaCenter : Al := Integer(AlignmentFlags_AlignHCenter);
            hsaRight : Al := Integer(AlignmentFlags_AlignRight);
          end; // case
          {$endif}
        end;
        {$ifndef CLX_USED}
        case TS.FTextLayout of
          tlTop : Al := AL or DT_TOP;
          tlCenter : Al := Al or DT_VCENTER;
          tlBottom : Al := Al or DT_BOTTOM;
        end;
        if RightAlignedText then
           AL := AL or DT_RTLREADING;
  {$IFNDEF LITE}
        if WrapCaptions then
        begin
          AL := AL or DT_WORDBREAK;
          if TS.FTextLayout = tlCenter then
             AL := AL or DT_TOP;
        end
        else
        if (Pos(#13#10, TS.Text) = 0) then
  {$ENDIF}
           AL := AL or DT_SINGLELINE or DT_END_ELLIPSIS;
        {$else}
        case TS.FTextLayout of
          tlTop : Al := AL or Integer(AlignmentFlags_AlignTop);
          tlCenter : Al := Al or Integer(AlignmentFlags_AlignVCenter);
          tlBottom : Al := Al or Integer(AlignmentFlags_AlignBottom);
        end;
  {$IFNDEF LITE}
        if WrapCaptions then
        begin
          AL := AL or Integer(AlignmentFlags_WordBreak);
          if TS.FTextLayout = tlCenter then
             AL := AL or Integer(AlignmentFlags_AlignTop);
        end
        else
        if (Pos(#13#10, TS.Text) = 0) then
  {$ENDIF}
          AL := AL or Integer(AlignmentFlags_SingleLine);
        {$endif}
        Canvas.Brush.Style := bsClear;
        SaveCol := 0;
        if (TS = FTrackSection) {$ifdef MSWINDOWS}and (not IsThemeApplied){$endif} then
        begin
          SaveCol := Canvas.Font.Color;
          SaveStyle := Canvas.Font.Style;
          Canvas.Font.Color := clHighlight;
          Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
        end
        else
          if TS.ParentColor then
            Canvas.Font.Color := Font.Color
          else
           Canvas.Font.Color := TS.FontColor;
        {$ifdef MSWINDOWS}
        if IsThemeApplied then
          {$ifndef CLX_USED}
          DrawThemeText(Theme, Canvas.Handle, HP_HEADERITEM, sid, PWideChar(WideString(S)), -1, DT_NOPREFIX or AL, 0, R1)
          {$else}
        begin
          Canvas.Start;
          DrawThemeText(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), HP_HEADERITEM, sid, PWideChar(WideString(S)), -1, DT_NOPREFIX or AL, 0, R1);
          Canvas.Stop;
        end
          {$endif}
        else
        {$endif}

        {$ifndef CLX_USED}
          {$ifdef ELPACK_UNICODE}
          ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(S), -1, R1, DT_NOPREFIX or AL);
          {$else}
          DrawText(Canvas.Handle, PChar(S), -1, R1, DT_NOPREFIX or AL);
          {$endif}
        {$else}
          Canvas.TextRect(R1, R1.Left, R1.Top, S, AL);
        {$endif}
        if (TS = FTrackSection) {$ifdef MSWINDOWS}and (not IsThemeApplied){$endif} then
        begin
          Canvas.Font.Color := SaveCol;
          Canvas.Font.Style := SaveStyle;
        end;
        Canvas.Brush.Style := bsSolid;
      end;
    end;
    inc(R1.Left, Canvas.TextWidth(S) + 3);
    if R1.Right < R1.Left then exit;
  end;
  if (FImages <> nil) and (TS.FPicAlign = hsaRight) then
  begin
    StImIndex := TS.FImageIndex;
    if StImIndex = -1 then StImIndex := DoGetPicture(TS);
    if InRange(0, FImages.Count - 1, StImIndex) then
    begin
      BMP := TBitmap.Create;
      BMP.Width := FImages.Width;
      BMP.Height := FImages.Height;

      R2 := Rect(R1.Left, (R1.Bottom + R1.Top + 1) div 2 - (FImages.Height div 2), Min(R1.Right, R1.Left + FImages.Width), 0);
      R2.Bottom := R2.Top + FImages.Height;

      Bmp.Canvas.CopyRect(Rect(0, 0, min(FImages.Width, Min(R1.Right, R1.Left + FImages.Width - 1) - R1.Left + 1), FImages.Height), Canvas, R2);

      if R.Left < R.Right then
      begin
        FImages.Draw(BMP.Canvas, 0, 0, StImIndex);
{$IFNDEF VER90}
        BMP.TransparentMode := tmAuto;
{$ENDIF}
        // R2 := Rect(R1.Left, R1.Top + ((R1.Bottom - R1.Top + 1) div 2) - (FImages.Height div 2), Min(R1.Right, R1.Left + FImages.Width), R1.Top + FImages.Height);
        Canvas.CopyRect(R2, BMP.Canvas, Rect(0, 0, min(FImages.Width, Min(R1.Right, R1.Left + FImages.Width - 1) - R1.Left + 1), FImages.Height));
      end;
      inc(R1.Left, FImages.Width);
      BMP.Free;
    end;
  end;
end;
{$hints on}

procedure TCustomElHeader.RedrawSections;
var
  R, R1
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
  , BgRect
{$endif}
{$ENDIF}
  	 : TRect;
  i      : integer;
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
  ACtl   : TWinControl;
{$endif}
{$ENDIF}
begin
  R := ClientRect;
  
  //if IsThemeApplied then
  //  DrawThemeBackground(Theme, Canvas.Handle, 0, 0, R, nil);

  for i := 0 to Sections.Count - 1 do
  begin
    if not Sections[i].Visible then Continue;
{$IFNDEF LITE}
    if Sections[i] <> FLockedSection then
{$ENDIF}
    begin
      R := GetSectionRect(i);
      OffsetRect(R, -FHPos, 0);
      if IntersectRect(R1, R, Canvas.ClipRect) and (Sections[i].Visible) then
         RedrawSection(Canvas, Sections[i], R, false);
    end;
  end;
{$IFNDEF LITE}
  if (FLockedSection <> nil) then
  begin
    R := GetSectionRect(FLockedSection.Index);
    OffsetRect(R, -FHPos, 0);
    if IntersectRect(R1, R, Canvas.ClipRect) and (FLockedSection.Visible) then
       RedrawSection(Canvas, FLockedSection, R, false);
  end;
{$ENDIF}
  R := ClientRect;
  R.Left := SectionsWidth - FHPos;
  {$ifdef MSWINDOWS}
  if not IsThemeApplied then
  {$endif}
  begin
  {$IFDEF ELPACK_COMPLETE}
  {$ifndef CLX_USED}
    if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
    begin
      if (FImgForm.Control <> Self) then
      begin
        ACtl := FImgForm.GetRealControl;
        BgRect := R;
        OffsetRect(BgRect, -SectionsWidth, 0);
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);
        FImgForm.PaintBkgnd(Canvas.Handle, R, BgRect.TopLeft, false);
      end;
    end
    else
  {$endif}
  {$ENDIF}
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(R);
    end;
    {$ifndef CLX_USED}
    DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
    {$else}
    dec(R.Bottom); dec(R.Right);
    DrawEdge(Canvas, R, esNone, esRaised, [ebLeft, ebTop{, ebRight, ebBottom}]);
    {$endif}
  end
  {$ifdef MSWINDOWS}
  else
  begin
    {$ifndef CLX_USED}
    DrawThemeBackground(Theme, Canvas.Handle, 0, 0, R, nil);
    {$else}
    Canvas.Start;
    DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), 0, 0, R, nil);
    Canvas.Stop;
    {$endif}
  end
  {$endif}
  ;
end;

function TCustomElHeader.CalcHeaderHeight : integer;
{$IFNDEF LITE}
var i : integer;
    R : TRect;
    ASection : TElHeaderSection;
    dw,
    cw,
    ch,
    mh : integer;
//  AL : integer;
{$ELSE}
var mh : integer;
{$ENDIF}
begin
  {$IFNDEF LITE}
  mh := 0;
  for i := 0 to FSections.Count - 1 do
  begin
    ASection := FSections[i];
    if ASection.Visible and (ASection.Style = ElhsText) then
    begin
      SetRectEmpty(R);

      cw := MeasureSectionWidth(ASection, @dw, @ch);
      R.Right := ASection.Width - (cw - dw);
      if ch > mh then
        mh := ch;
    end;
  end;
{$ELSE}
  mh := Abs(Font.Height) + 4;
{$ENDIF}
  if Assigned(Images) then
    Result := Max(Max(17, mh + 4), Images.Height + 4)
  else
    Result := Max(17, mh + 4);
  if not Flat then
    Inc(Result, 2);
end;

procedure TCustomElHeader.AdjustHeaderHeight;
begin
  if LockHeight then
    exit;
  if HandleAllocated then 
    ClientHeight := CalcHeaderHeight;
end;

procedure TCustomElHeader.AdjustStickySize(Caller : TElHeaderSection);
var
  i, cw, sw : integer;
  HS : TElHeaderSection;

begin
  i := 0;
  while (SectionsWidth >= ClientWidth) and (ClientWidth > 0) and (i > -1) do
  begin
    i := FSections.Count - 1;
    while i >= 0 do
    begin
      HS := FSections.ItemByPos[i];
      cw := ClientWidth;
      sw := SectionsWidth;
      if HS.Visible and (HS.Width > 0) and (HS.Resizable) and (HS <> Caller) then
      begin
        if HS.Width > (sw - cw - 1) then
          Hs.FWidth := hs.Width - (sw - cw) - 1
        else
          HS.FWidth := 0;
        //HS.FStickySize := HS.Width / cw;
        break;
      end;
      dec(i);
    end; // while
  end;
  i := 0;
  while (SectionsWidth >= ClientWidth) and (ClientWidth > 0) and (i > -1) do // we probably have non-resizable sections
  begin
    i := FSections.Count - 1;
    while i >= 0 do
    begin
      HS := FSections.ItemByPos[i];
      cw := ClientWidth;
      sw := SectionsWidth;
      if HS.Visible and (HS.Width > 0) and ((not HS.Resizable) or (HS = Caller)) then
      begin
        if HS.FSaveSize = -1 then HS.FSaveSize := HS.Width; // this is the size we have to save, cause later we'll have to restore it
        if HS.Width > (sw - cw - 1) then
          HS.FWidth := hs.Width - (sw - cw) - 1
        else
          HS.FWidth := 0;
        break;
      end;
      dec(i);
    end;
  end;

  // first restore "non-resizable" sections
  i := 0;
  while (SectionsWidth < ClientWidth - 1) and (i > -1) do
  begin
    i := FSections.Count - 1;
    while i >= 0 do
    begin
      HS := FSections.ItemByPos[i];
      cw := ClientWidth;
      sw := SectionsWidth;
      if HS.Visible and (not HS.Resizable) and (HS.Width < HS.FSaveSize) and (HS.FSaveSize <> -1) then
      begin
        HS.FWidth := min(hs.Width + (cw - sw) - 1, HS.FSaveSize);
        if HS.FWidth = HS.FSaveSize then
          HS.FSaveSize := -1
        else
          break;
      end;
      dec(i);
    end; // while
  end;

  i := FSections.Count - 1;
  while i >= 0 do
  begin
    HS := FSections.ItemByPos[i];
    cw := ClientWidth;
    sw := SectionsWidth;
    if HS.Visible and (HS.Resizable) and (HS <> Caller) then
    begin
      HS.FWidth := hs.Width + (cw - sw) - 1;
      //HS.FStickySize := HS.Width / cw;
      break;
    end;
    dec(i);
  end; // while
end;

function TCustomElHeader.GetResizableWidth : integer;
var
  i, w : integer;
  HS : TElHeaderSection;
begin
  w := Width;
  for i := 0 to FSections.Count - 1 do // Iterate
  begin
    HS := FSections[i];
    //if HS.Visible and (HS.Resizable) then w := w + HS.Width;
    if HS.Visible and (not HS.Resizable) then w := w - HS.Width;
  end; // for
  result := w;
end;

procedure TCustomElHeader.MarkStickySections;
var
  w, i : integer;
  HS : TElHeaderSection;
begin
  W := GetResizableWidth;
  for i := 0 to FSections.Count - 1 do
  begin
    HS := FSections.ItemByPos[i];
    if HS.Resizable and HS.Visible then
      if (W <> 0) then
        HS.FStickySize := HS.FWidth / w
      else
        HS.FStickySize := 0;
  end;
end;

procedure TCustomElHeader.Resize;
var
  i, oh, nh, w : integer;
  HS : TElHeaderSection;
begin
  IntSize;
  if not FLockHeight then
  begin
    oh := ClientHeight;
    if Images <> nil then
      nh := Max(Max(Images.Height, oh), Abs(Font.Height) + 6)
    else
      nh := Max(oh, Abs(Font.Height) + 6);
    if nh <> oh then
       AdjustHeaderHeight;
  end;
  if StickySections then
  begin
    FInStick := true;
    if FOldWidth < ClientWidth then
      for i := FSections.Count - 1 downto 0 do // Iterate
      begin
        HS := FSections.ItemByPos[i];
        if (not HS.Resizable) and (HS.Visible) and (HS.FSaveSize <> -1) then
        begin
          HS.Width := min(HS.FSaveSize, ClientWidth);
          if HS.Width = HS.FSaveSize then HS.FSaveSize := -1;
        end;
      end;
    W := GetResizableWidth;
    if w = 0 then w := 1;
    for i := 0 to FSections.Count - 1 do // Iterate
    begin
      HS := FSections.ItemByPos[i];
      if HS.Resizable and HS.Visible then HS.Width := Round(w * HS.FStickySize);
    end; // for
    AdjustStickySize(nil);
    FOldWidth := ClientWidth;
    FInStick := false;
  end;
{$IFNDEF VCL_4_USED}
  if Assigned(FOnResize) then FOnResize(Self);
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TCustomElHeader.AllocateLineDC;
begin
{$ifndef CLX_USED}
  FLineDC := GetDCEx(Handle, 0, DCX_CACHE {or DCX_CLIPSIBLINGS }or DCX_PARENTCLIP
    or DCX_LOCKWINDOWUPDATE);
{$else}
  FLineDC := TBitmap.Create;
  FLineDC.Width := 1;
  FLineDC.Height := ClientHeight;
  Canvas.Pen.Color := clBtntext;
  FLineDC.Canvas.FillRect(Rect(0, 0, 1, ClientHeight));
{$endif}
end;

procedure TCustomElHeader.ReleaseLineDC;
begin
{$ifndef CLX_USED}
  ReleaseDC(Handle, FLineDC);
{$else}
  Canvas.Pen.Color := clBtnFace;
  FLineDC.Free;
{$endif}
end;


procedure TCustomElHeader.DrawLine(Restore : boolean);
begin
  FHeaderLineVis := not FHeaderLineVis;
{$ifndef CLX_USED}
  PatBlt(FLineDC, FHeaderLine, 0, 1, Parent.Height, PATINVERT);
{$else}
  if Restore then
    Canvas.CopyRect(Rect(FHeaderLine, 0, FHeaderLine + 1, ClientHeight), FLineDC.Canvas, Rect(0, 0, 1, ClientHeight))
  else
  begin
    FLineDC.Canvas.CopyRect(Rect(0, 0, FHeaderLine + 1, ClientHeight), Canvas, Rect(FHeaderLine, 0, 1, ClientHeight));
    Canvas.MoveTo(FHeaderLine, 0);
    Canvas.LineTo(FHeaderLine, ClientHeight);
  end;
{$endif}
end;

procedure TCustomElHeader.Paint;
begin
  FPainting := true;
  RedrawSections;
  FPainting := false;
end;

procedure TCustomElHeader.SetSections;
begin
  FSections.Assign(value);
end;

procedure TCustomElHeader.SetLockHeight(newValue : Boolean);
{ Sets data member FLockHeight to newValue. }
begin
  if (FLockHeight <> newValue) then
  begin
    FLockHeight := newValue;
    if (not FLockHeight) and (Parent <> nil) and (not (csLoading in Parent.ComponentState)) then
       AdjustHeaderHeight;
  end; { if }
end; { SetLockHeight }

procedure TCustomElHeader.SetFilterColor(newValue : TColor);
var
  i : integer;
  S : TElHeaderSection;
begin
  if (FFilterColor <> newValue) then
  begin
    FFilterColor := newValue;
    for i := 0 to FSections.Count - 1 do // Iterate
    begin
      S := FSections[i];
      if (S.Visible) and (S.FFilterEnabled) and (not S.FFilterIsActive) then S.UpdateSection;
    end; // for
  end; { if }
end; { SetFilterColor }

procedure TCustomElHeader.TriggerSectionAutoSizeEvent;
begin
  if (assigned(FOnSectionAutoSize)) then
    FOnSectionAutoSize(Self, Section);
end; { TriggerSectionAutoSizeEvent }

procedure TCustomElHeader.TriggerMeasureSectionEvent(Section : TElHeaderSection; var Size: TPoint);
begin
  if assigned(FOnMeasureSection) then FOnMeasureSection(Self, Section, Size); 
end;

procedure TCustomElHeader.TriggerFilterCallEvent;
begin
  if (assigned(FOnFilterCall)) then FOnFilterCall(Self, Section);
end; { TriggerFilterCallEvent }

{$ifndef CLX_USED}
procedure TCustomElHeader.WMLButtonDblClk(var Msg : TWMLButtonDblClk); { private }
begin
  IntLButtonDblClick(Msg.XPos, Msg.YPos);
  inherited;
end; { WMLButtonDblClk }
{$endif}

procedure TCustomElHeader.SetActiveFilterColor(newValue : TColor);
var
  i : integer;
  S : TElHeaderSection;
begin
  if (FActiveFilterColor <> newValue) then
  begin
    FActiveFilterColor := newValue;
    for i := 0 to FSections.Count - 1 do // Iterate
    begin
      S := FSections[i];
      if (S.Visible) and (S.FFilterEnabled) and S.FFilterIsActive then S.UpdateSection;
    end; // for
  end; { if }
end; { SetFilterColor }

procedure TCustomElHeader.SetFlat(newValue : Boolean);
{ Sets data member FFlat to newValue. }
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    if (HandleAllocated) then
      AdjustHeaderHeight;
  end; { if }
end; { SetFlat }

{$ifndef CLX_USED}
procedure TCustomElHeader.CMHintShow(var Msg : TMessage); { private }
begin
  inherited;
  IntHintShow(PHintInfo(Msg.lParam)^);
end; { CMHintShow }
{$endif}

procedure TCustomElHeader.SetImages(newValue : TImageList);
{ Sets data member FImages to newValue. }
begin
  if (FImages <> newValue) then
  begin
    if FImages <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImages.RemoveFreeNotification(Self);
      {$endif}
      FImages.UnRegisterChanges(FImageChangeLink);
    end;
    FImages := newValue;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
      if not LockHeight then
         AdjustHeaderHeight;
    end
    else
    if not LockHeight then
      AdjustHeaderHeight;
    if FPainting then exit;
    if (HandleAllocated) then
      Invalidate;//Repaint;
  end; { if }
end; { SetImages }

procedure TCustomElHeader.OnImageListChange(Sender : TObject); { private }
begin
  if FPainting then exit;
  if not LockHeight then
    AdjustHeaderHeight;
  if (HandleAllocated) then
    Invalidate;//Repaint;
end; { OnImageListChange }

procedure TCustomElHeader.DoVisChanged(Section : TElHeaderSection); { protected }
begin
  if Assigned(FOnVisibleChange) then FOnVisibleChange(Self, Section);
end; { DoVisChanged }

procedure TCustomElHeader.DoSectionDelete(Section : TElHeaderSection); { protected }
begin
  if assigned(FOnSectionDelete) then FOnSectionDelete(Self, Section);
end; { DoSectionDelete }

procedure TCustomElHeader.DoSectionMove(Section : TElHeaderSection; OldPos, NewPos : integer); { protected }
begin
  if assigned(FOnSectionMove) then FOnSectionMove(Self, Section, OldPos, NewPos);
end; { DoSectionMove }

procedure TCustomElHeader.DoSectionResizing(Section : TElHeaderSection; State : TElHResizingStates; NewWidth : integer); { protected }
begin
  if Assigned(FOnSectionResizing) then FOnSectionResizing(Self, Section, State, NewWidth);
end; { DoSectionResizing }

procedure TCustomElHeader.DoSectionResize(Section : TElHeaderSection); { protected }
begin
  if Assigned(FOnSectionResize) then FOnSectionResize(Self, Section);
end; { DoSectionResize }

procedure TCustomElHeader.DoSectionClick(Section : TElHeaderSection); { protected }
begin
  if (Assigned(FOnSectionClick)) then FOnSectionClick(Self, Section);
end; { DoSectionClick }

procedure TCustomElHeader.DoSectionDraw(Canvas : TCanvas; Section : TElHeaderSection; R : TRect; Pressed : boolean); { protected }
begin
  if Assigned(FOnSectionDraw) then FOnSectionDraw(Self, Canvas, Section, R, Pressed);
end; { DoSectionDraw }

procedure TCustomElHeader.DoNotifySectionChange(Section : TElHeaderSection; Change : TSectionChangeMode);
begin
  if (assigned(FOnSectionChange)) then
    FOnSectionChange(Self, Section, Change);
end; { TriggerSectionChangeEvent }

procedure TCustomElHeader.OnFontChange(Sender : TObject); { protected }
begin
  if not LockHeight then
     AdjustHeaderHeight;

  Canvas.Font.Assign(Font);
  if (HandleAllocated) then
    Invalidate;//Repaint;
end; { OnFontChange }

procedure TCustomElHeader.DoSectionCreate(Section : TElHeaderSection);
begin
  if (assigned(FOnSectionCreate)) then FOnSectionCreate(Self, Section);
end; { TriggerSectionCreateEvent }

procedure TCustomElHeader.GetDragImage; { private }
var
  R : TRect;
  S : TElHeaderSection;
  C : TColor;
  hls2: integer;
  lum : integer;

begin
  S := GetSectionAt(XPos, 0);
  if S = nil then exit;
  R := GetSectionRect(S.Index);
  DragRect := R;
  DragBmp := TBitmap.Create;
  DragBmp.Height := R.Bottom - R.Top + 1;
  DragBmp.Width := R.Right - R.Left + 1;
  R := Rect(0, 0, DragBmp.Width - 1, DragBmp.Height - 1);
  DragBmp.Canvas.Brush.Color := Color;
  DragBmp.Canvas.FillRect(R);
  DragBmpMask := TBitmap.Create;
  DragBmp.Canvas.Font.Assign(Font);
  RedrawSection(DragBmp.Canvas, S, R, true);
  DragBmpMask.Width := DragBmp.Width;
  DragBmpMask.Height := DragBmp.Height;
  DragBmpMask.Canvas.CopyRect(R, DragBmp.Canvas, R);

  if S.FParentColor then
    C := Color
  else
    C := S.FColor;

  begin
    hls2 := RGBtoHLS(ColorToRGB(C));
    lum := Hi(hls2 and $FFFF);
    Dec(lum, lum shr 2);
    if lum > 239 then
      lum := 239;
    hls2 := (lum shl 8) or (hls2 and $FF00FF);
    C := TColor(HLStoRGB(hls2))
  end;

{$IFNDEF VER90}
  DragBmpMask.Mask(C);
  {$ifdef CLX_USED}
  DragBmp.Mask(C);
  {$endif}
{$ENDIF}
end; { GetDragImage }

(*
procedure TCustomElHeader.GetDragImage; { private }
var //i : integer;
  R : TRect;
  S : TElHeaderSection;
  i,
  j : integer;
begin
  S := GetSectionAt(XPos, 0);
  if S = nil then exit;
  R := GetSectionRect(S.Index);
  DragRect := R;
  DragBmp := TBitmap.Create;
  DragBmp.Height := R.Bottom - R.Top + 1;
  DragBmp.Width := R.Right - R.Left + 1;
  DragBmp.Canvas.Brush.Color := Color;
  R := Rect(0, 0, DragBmp.Width - 1, DragBmp.Height - 1);
  DragBmp.Canvas.FillRect(R);
  DragBmpMask := TBitmap.Create;
  DragBmp.Canvas.Font.Assign(Font);
  RedrawSection(DragBmp.Canvas, S, R);
  DragBmpMask.Width := DragBmp.Width;
  DragBmpMask.Height := DragBmp.Height;
  DragBmpMask.Canvas.CopyRect(R, DragBmp.Canvas, R);
{$IFNDEF VER90}
  DragBmpMask.Mask(Color);
{$ENDIF}
end; { GetDragImage }
*)
procedure TCustomElHeader.SetTracking(newValue : boolean);
begin
  if FTracking <> newValue then
  begin
    FTracking := newValue;
    if (not FTracking) {$ifdef MSWINDOWS}and (not IsThemeApplied) {$endif}then
    begin
      FTrackSection := nil;
      if (HandleAllocated) then
        Invalidate;//Repaint;
    end;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElHeader.WMEraseBkgnd(var Msg : TWMEraseBkgnd); { private }
begin
  Msg.Result := 1;
end; { WMEraseBkgnd }

procedure TCustomElHeader.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if (Message.CharCode = VK_ESCAPE) and (KeyDataToShiftState(Message.KeyData) = []) and
     (FDropSrc <> nil) then
  begin
    IntLButtonUp(FDropSrc.Left, 1);
  end;
end;
{$endif}


{$ifdef SUPPORT_STORAGE}
procedure TCustomElHeader.Save;
var
  SaveKey, CKey : string;
  i : integer;
begin
  if Assigned(FStorage) then
  begin
    SaveKey := FStorage.CurrentKey;
    if FStorage.OpenKey(StoragePath + FStorage.Delimiter + 'ElHeader', true) then
    begin
      FStorage.WriteInteger(CKey, 'Width', Width);
      for i := 0 to FSections.Count - 1 do // Iterate
      begin
        CKey := 'Section' + IntToStr(i);
        FStorage.WriteBool(CKey, 'Visible', FSections[i].FVisible);
        FStorage.WriteBool(CKey, 'Expanded', FSections[i].Expanded);
        FStorage.WriteInteger(CKey, 'Width', FSections[i].Width);
        FStorage.WriteInteger(CKey, 'Align', Integer(FSections[i].Alignment));
        FStorage.WriteInteger(CKey, 'SortOrder', Integer(FSections[i].SortMode));
      end; // for
      FStorage.OpenKey(SaveKey, false);
    end;
  end;
end; {Save}

procedure TCustomElHeader.Restore;
var
  SaveKey, CKey : string;
  i,
  j,
  k     : integer;
  b     : boolean;
  align : Integer;
begin
  inc(LoadingCount);
  if Assigned(FStorage) then
  begin
    SaveKey := FStorage.CurrentKey;
    if FStorage.OpenKey(StoragePath + FStorage.Delimiter + 'ElHeader', false) then
    begin
      FStorage.ReadInteger(CKey, 'Width', Width, k);
      for i := 0 to FSections.Count - 1 do // Iterate
      begin
        CKey := 'Section' + IntToStr(i);
        FStorage.ReadInteger(CKey, 'Align', Integer(hsaLeft), align);
        FSections[i].FAlignment := TElSAlignment(align);
        FStorage.ReadBool(CKey, 'Visible', FSections[i].FVisible, FSections[i].FVisible);
        if FStorage.ReadBool(CKey, 'Expanded', FSections[i].Expanded, b) then FSections[i].Expanded := b;
        if FStorage.ReadInteger(CKey, 'Width', FSections[i].Width, j) then
          if ((FSections[i].Resizable) and (StickySections)) then
            FSections[i].Width := Trunc(j * (Width / k))
          else
            FSections[i].Width := j;
        if FStorage.ReadInteger(CKey, 'SortOrder', Integer(FSections[i].SortMode), j) then FSections[i].SortMode := TElSSortMode(j);
      end; // for
      FStorage.OpenKey(SaveKey, false);
    end;
  end;
  dec(LoadingCount);
  b := FStickySections;
  FStickySections := false;
  SetStickySections(b);
end; {Restore}
{$ENDIF}

procedure TCustomElHeader.EditExit(Sender : TObject);
begin
  if FDoingLookup then
  begin
    FLookup.Visible := false;
    FDoingLookup := false;
    DoSectionLookupDoneEvent(FLookupSection, FLookup.Text, false);
    try
      {$ifndef CLX_USED}
      Windows.SetFocus(FFocusedCtl);
      {$else}
      {$ifdef LINUX}
      XSetInputFocus(QtDisplay, FFocusedCtl, RevertToParent, CurrentTime);
      {$else}
      Windows.SetFocus(FFocusedCtl);
      {$endif}
      {$endif}
    except
      on E : Exception do
      begin
      end;
    end;
  end;
end;

procedure TCustomElHeader.EditChange;
var
  S : string;
begin
  if not FIgnoreLookupChange then
  begin
    S := FLookup.Text;
    DoSectionLookupEvent(FLookupSection, S);
    if S <> FLookup.Text then
    begin
      FIgnoreLookupChange := true;
      FLookup.Text := S;
      FIgnoreLookupChange := false;
    end;
  end;
end;

procedure TCustomElHeader.EditKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  if (Shift = []) and
  {$ifndef CLX_USED}
     ((Key = VK_RETURN) or (Key = VK_ESCAPE))
  {$else}
     ((Key = KEY_ENTER) or (Key = KEY_RETURN) or (Key = Key_ESCAPE))
  {$endif}
  then Key := 0;
end;

procedure TCustomElHeader.EditKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
var
  i : integer;
begin
  if (Shift = []) then
  begin
  {$ifndef CLX_USED}
    if Key = VK_RETURN then
  {$else}
    if (Key = KEY_ENTER) or (Key = KEY_RETURN) then 
  {$endif}
    begin
      FDoingLookup := false;
      DoSectionLookupDoneEvent(FLookupSection, FLookup.Text, true);
      i := FLookupSection.FLookupHist.IndexOf(FLookup.Text);
      if i <> -1 then FLookupSection.FLookupHist.Delete(i);
      FLookupSection.FLookupHist.Insert(0, FLookup.Text);
      FLookup.Visible := false;
      try
        {$ifndef CLX_USED}
        Windows.SetFocus(FFocusedCtl);
        {$else}
        {$ifdef LINUX}
        XSetInputFocus(QtDisplay, FFocusedCtl, RevertToParent, CurrentTime);
        {$else}
        Windows.SetFocus(FFocusedCtl);
        {$endif}
        {$endif}
      except
        on E : Exception do
        begin
        end;
      end;
      Key := 0;
    end;
    {$ifndef CLX_USED}
    if Key = VK_ESCAPE then
    {$else}
    if Key = KEY_ESCAPE then
    {$endif}
    begin
      FDoingLookup := false;
      DoSectionLookupDoneEvent(FLookupSection, FLookup.Text, false);
      FLookup.Visible := false;
      try
        {$ifndef CLX_USED}
        Windows.SetFocus(FFocusedCtl);
        {$else}
        {$ifdef LINUX}
        XSetInputFocus(QtDisplay, FFocusedCtl, RevertToParent, CurrentTime);
        {$else}
        Windows.SetFocus(FFocusedCtl);
        {$endif}
        {$endif}
      except
        on E : Exception do
        begin
        end;
      end;
      Key := 0;
    end;
  end;
end;

{$ifdef SUPPORT_STORAGE}
procedure TCustomElHeader.SetStorage(newValue : TElIniFile);
begin
  if newValue <> FStorage then
  begin
    {$ifdef VCL_5_USED}
    if FStorage <> nil then
      FStorage.RemoveFreeNotification(Self);
    {$endif}
    FStorage := newValue;
    if FStorage <> nil then
      FStorage.FreeNotification(Self);
  end;
end;
{$ENDIF}

procedure TCustomElHeader.Notification(AComponent : TComponent; Operation : TOperation);
var
  i : integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
{$ifdef SUPPORT_STORAGE}
    if AComponent = FStorage then FStorage := nil;
{$ENDIF}
    if AComponent = Images then
       Images := nil;
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
    if AComponent = FImgForm then
    begin
      FImgForm := nil;
      if (HandleAllocated) then
        Invalidate;
    end;
{$endif}
{$ENDIF}
    if AComponent is TPopupMenu then
      for i := 0 to FSections.Count - 1 do
        if TElHeaderSection(FSections.FList[i]).FPopupMenu = AComponent then TElHeaderSection(FSections.Item[i]).FPopupMenu := nil;
  end;
end;

procedure TCustomElHeader.DoSectionExpandEvent(Section : TElHeaderSection);
begin
  if (assigned(FOnSectionExpand)) then FOnSectionExpand(Self, Section);
end;

procedure TCustomElHeader.DoSectionCollapseEvent(Section :
  TElHeaderSection);
begin
  if (assigned(FOnSectionCollapse)) then FOnSectionCollapse(Self, Section);
end;

procedure TCustomElHeader.DoSectionLookupEvent;
begin
  if (assigned(FOnHeaderLookup)) then FOnHeaderLookup(Self, Section, Text);
end; {TriggerHeaderLookupEvent}

procedure TCustomElHeader.DoSectionLookupDoneEvent;
begin
  if (assigned(FOnHeaderLookupDone)) then FOnHeaderLookupDone(Self, Section, Text, Accepted);
end; {TriggerHeaderLookupDoneEvent}

{$ifndef CLX_USED}
procedure TCustomElHeader.WMRButtonUp(var Msg : TWMRButtonUp);
begin
  if IntRButtonUp(Msg.XPos, Msg.YPos) then
    Msg.Result := 0
  else
    inherited;
end; {WMRButtonUp}

procedure TCustomElHeader.WMRButtonDown(var Msg : TWMRButtonDown);
begin
  if IntRButtonDown(Msg.XPos, Msg.YPos) then
    Msg.Result := 0
  else
    inherited;
end;
{$endif}

procedure TCustomElHeader.Loaded;
var
  i : integer;
  O : TComponent;
  Form : TForm;
  b : boolean;
begin
  inherited Loaded;
  Form := ElVCLUtils.GetOwnerForm(Self);
  if Form = nil then exit;
  for i := 0 to FSections.Count - 1 do
  begin
    if FSections[i].FPopupName <> '' then
    begin
      O := Form.FindComponent(FSections[i].FPopupName);
      if (O <> nil) and (O is TPopupMenu) then FSections[i].FPopupMenu := TPopupMenu(O);
    end;
  end;
  b := FStickySections;
  FStickySections := false;
  SetStickySections(b);
  if RightAlignedOrder then
  begin
    FRightAlignedOrder := false;
    RightAlignedOrder := true;
  end;
  AdjustHeaderHeight;
end;

{$ifndef CLX_USED}
procedure TCustomElHeader.WMSize(var Msg : TWMSize);
begin
  IntSize;
{$IFNDEF VCL_4_USED}
  Resize;
{$ELSE}
  inherited;
{$ENDIF}
  AdjustHeaderHeight;
end;
{$endif}

procedure TCustomElHeader.SetStickySections(newValue : Boolean);
var
  i, w : integer;
  HS : TElHeaderSection;
begin
  if IsLoading then
  begin
    FStickySections := newValue;
    exit;
  end;
  //if (FStickySections <> newValue) then
  begin
    FStickySections := newValue;
    if newValue then
    begin
      w := GetResizableWidth;
      if w = 0 then w := 1;        
      for i := 0 to FSections.Count - 1 do // Iterate
      begin
        HS := FSections[i];
        if HS.Visible and (HS.Resizable) and (not IsLoading) and (w <> 0) then
          HS.FStickySize := HS.Width / w
        else
          HS.FStickySize := 0;
      end; // for
    end;
  end; {if}
end; {SetStickySections}

function TCustomElHeader.IsLoading : Boolean;
begin
  if Owner <> nil then
  begin
    result := (csLoading in ComponentState) or (LoadingCount > 0);
    if not result then result := csLoading in Owner.ComponentState;
  end
  else
  begin
    result := csLoading in ComponentState;
  end;
end; {IsLoading}

procedure TCustomElHeader.SetRightAlignedText(newValue : Boolean);
begin
  if (FRightAlignedText <> newValue) then
  begin
    FRightAlignedText := newValue;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end; {if}
end;

{$ifndef CLX_USED}
procedure TCustomElHeader.CreateParams(var Params : TCreateParams);
begin
  inherited;
  if RightAlignedText then
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_RTLREADING or WS_EX_LEFTSCROLLBAR or WS_EX_RIGHT;
  end;
end;
{$endif}

procedure TCustomElHeader.SetRightAlignedOrder(newValue : Boolean);
var
  FList : TElList;
  i : integer;
begin
  if (FRightAlignedOrder <> newValue) then
  begin
    FRightAlignedOrder := newValue;
    if csLoading in ComponentState then exit;
    FList := TElList.Create;
    for i := 0 to FSections.FList.Count - 1 do
      FList.Add(FSections.FList[FSections.FList.Count - i - 1]);
    FSections.FList.Clear;
    for i := 0 to FList.Count - 1 do
      FSections.FList.Add(FList[i]);
    FList.Free;
  end; {if}
end;

{$ifndef CLX_USED}
procedure TCustomElHeader.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  Invalidate;//Repaint;
end;
{$endif}

function TCustomElHeader.GetIsDesigning : Boolean;
{ Returns the value of data member FIsDesigning. }
begin
  result := csDesigning in ComponentState;
end;  { GetIsDesigning }

procedure TCustomElHeader.SetIsDesigning(newValue : Boolean);
{ Sets data member FIsDesigning to newValue. }
begin
  if (IsDesigning <> newValue) then
  begin
    SetDesigning(newValue);
  end;  { if }
end;  { SetIsDesigning }

procedure TCustomElHeader.SetInvertSortArrows(newValue : Boolean);
{ Sets data member FInvertSortArrows to newValue. }
begin
  if (FInvertSortArrows <> newValue) then
  begin
    FInvertSortArrows := newValue;
    if (HandleAllocated) then
      Invalidate;//Repaint;
  end;  { if }
end;  { SetInvertSortArrows }

{$ifndef CLX_USED}
procedure TCustomElHeader.WMPaint(var Msg : TWMPaint);  { private }
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  R : TRect;
  ARgn : HRGN;

begin
  if (Msg.DC <> 0) then PaintHandler(Msg)
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      GetClipBox(DC, R);
      if IsRectEmpty(R) then
        R := ClientRect
      else
      begin
        InflateRect(R, 1, 1);
      end;

      with R do
        ARgn := CreateRectRgn(Left, Top, right, Bottom);
      SelectClipRgn(MemDC, ARgn);
      //InvalidateRect(MemDC, @R, false);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Msg.DC := MemDC;
      WMPaint(Msg);
      SelectClipRgn(MemDC, 0);
      DeleteObject(ARgn);
      Msg.DC := 0;
      with R do
        BitBlt(DC, Left, Top, Right, Bottom, MemDC, Left, Top, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;  { WMPaint }
{$endif}

{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
procedure TCustomElHeader.ImageFormChange(Sender : TObject);
begin
  if (HandleAllocated) then
    Invalidate;
end;

procedure TCustomElHeader.SetImageForm(newValue : TElImageForm);
begin
  if FImgForm <> newValue then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnregisterChanges(FImgFormChLink);
    end;
    if newValue <> nil then
       newValue.FreeNotification(Self);
    FImgForm := newValue;
    if FImgForm <> nil then
    begin
      FImgForm.RegisterChanges(FImgFormChLink);
      FImgForm.FreeNotification(Self);
    end;
    if HandleAllocated and (not (csDestroying in ComponentState)) then
    begin
    {$ifndef CLX_USED}
      RecreateWnd;
      Perform(CM_COLORCHANGED, 0, 0);
    {$else}
      ColorChanged;
    {$endif}
    end;
  end;
end;
{$endif}
{$ENDIF}
procedure TCustomElHeader.SetLeftPos(newValue : Integer);
{ Sets data member FLeftPos to newValue. }
begin
  if (FHPos <> newValue) then
  begin
    FHPos := newValue;
    if (HandleAllocated) then
      Invalidate;//Repaint;
  end;  { if }
end;  { SetLeftPos }
{$IFNDEF LITE}
procedure TCustomElHeader.SetLockedSection(newValue : TElHeaderSection);
{ Sets data member FLockedSection to newValue. }
begin
  if (FLockedSection <> newValue) then
  begin
    FLockedSection := newValue;
    if newValue <> nil then
      Sections.MoveSection(FLockedSection, 0);
    if (HandleAllocated) then
      Invalidate;//Repaint;
  end;  { if }
end;  { SetLockedSection }
{$ENDIF}

{$ifndef CLX_USED}
procedure TCustomElHeader.CMExit(var Msg : TCMExit);  { private }
begin
  inherited;
  intExit;
end;  { CMExit }
{$endif}

function TElHeaderSections.GetSectionsOrder : string;
var
  i : integer;
  Section : TElHeaderSection;

begin
  result := '';
  for i := 0 to FList.Count - 1 do
  begin
    Section := TElHeaderSection(FList[i]);
    result := result + 'i' + IntToStr(Section.Index) + ':w' + IntToStr(Section.Width) + ':v';
    if Section.Visible then
      result := result + 't,'
    else
      result := result + 'f,';
  end;
end; { GetSectionsOrder }

procedure TElHeaderSections.SetSectionsOrder(newValue : string);
var
  i : integer;
  Section : TElHeaderSection;
  s : string;
begin
  if (csDesigning in Owner.ComponentState) or (csLoading in Owner.ComponentState) or  
     ((Owner.Owner <> nil) and ((csDesigning in Owner.Owner.ComponentState) or
                               (csLoading in Owner.Owner.ComponentState))) then
    exit;

  i := 0;
  while newValue <> '' do
  begin
    s := copy(newValue, 2, pos(':', newValue) - 2);
    delete(newValue, 1, pos(':', newValue));
    Section := Item[StrToInt(s)];
    MoveSection(Section, i);
    s := copy(newValue, 2, pos(':', newValue) - 2);
    delete(newValue, 1, pos(':', newValue));
    Section.Width := StrToInt(s);
    if (pos('vf,', newValue) = 1) or (pos('vf;', newValue) = 1) then
      Section.Visible := false
    else
      Section.Visible := true;
    delete(newValue, 1, 3);
    inc(i);
  end;
end; { SetSectionsOrder }

procedure TElHeaderSections.Reindex;
var i : integer;
begin
  for i := 0 to FList.Count - 1 do
    TElHeaderSection(FList[i]).FIndex := i;
end;

procedure TCustomElHeader.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TCustomElHeader.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if DeletionHappened then
    begin
      DeletionHappened := false;
      DoSectionDelete(nil);
    end;
    if AdditionHappened then
    begin
      AdditionHappened := false;
      DoSectionCreate(nil);
    end;
    Update;
  end;
end;

procedure TCustomElHeader.Update;
begin
  if FUpdateCount = 0 then
    inherited;
end;

{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
function TCustomElHeader.Setup: boolean;
var
  Form: TfrmHeaderSetup;
begin
  Application.CreateForm(TfrmHeaderSetup, Form);
  Form.LoadHeaderSections(FSections);
  Result := (Form.ShowModal = mrOk);
  if Result then Form.SaveHeaderSections(FSections);
  Form.Free;
end;
{$endif}
{$ENDIF}

{$IFNDEF LITE}
procedure TCustomElHeader.SetWrapCaptions(newValue : Boolean);
{ Sets data member FWrapCaptions to newValue. }
begin
  if (FWrapCaptions <> newValue) then
  begin
    FWrapCaptions := newValue;
    AdjustHeaderHeight;
  end;  { if }
end;  { SetWrapCaptions }
{$ENDIF}

procedure TCustomElHeader.IntLButtonDown(XPos, YPos : SmallInt);
var
  THS, THS1 : TElHeaderSection;
  b : boolean;
  rm : integer;
  R : TRect;
  xoffs : integer;
  {$ifdef LINUX}
  FRevertTo : Integer;
  {$endif}
begin
  {$ifndef CLX_USED}
  SendCancelMode(Self);
  {$endif}
  if FDoingLookup then
  begin
    FDoingLookup := false;
    FLookup.Visible := false;
    DoSectionLookupDoneEvent(FLookupSection, FLookup.Text, false);
    try
      {$ifndef CLX_USED}
      Windows.SetFocus(FFocusedCtl);
      {$else}
      {$ifdef LINUX}
      XSetInputFocus(QtDisplay, FFocusedCtl, RevertToParent, CurrentTime);
      {$else}
      Windows.SetFocus(FFocusedCtl);
      {$endif}
      {$endif}
    except
      on E : Exception do
      begin
      end;
    end;
  end
  else
  if not (csNoStdEvents in ControlStyle) then
  begin
    if InResizeArea(XPos, THS) and (THS.FResizable) then
    begin
      FResizing := true;
      FResizeSection := THS;
      THS1 := FTrackSection;
      FTrackSection := nil;
      if THS1 <> nil then
        THS1.UpdateSection;
      Screen.Cursor := crHSplit;
      {$ifndef CLX_USED}
      SetCapture(Handle);
      {$else}
      SetMouseGrabControl(Self);
      {$endif}
      FHeaderLine := FResizeSection.Right;
{$IFNDEF LITE}
      if FResizeSection <> FLockedSection then
{$ENDIF}
         Dec(FHeaderLine, FHPos);
      FLineTab := XPos - FHeaderLine;
      if (not ResizeOnDrag) then
      begin
        {$ifndef CLX_USED}
        SendCancelMode(Self);
        {$endif}
        AllocateLineDC;
        DrawLine(false);
      end;
      DoSectionResizing(THS, trsBegin, THS.Width);
    end // Resizing
    else
    begin
      FPressCoord := Point(XPos, YPos);
      FPressed := true;
      if FPressedItem <> nil then
      begin
        THS := FPressedItem;
        FPressedItem := nil;
        THS.UpdateSection;
      end;
      FPressedItem := GetSectionAt(XPos, YPos);
      THS := FPressedItem;
      if FPressedItem <> nil then
      begin
        b := true;
        rm := FPressedItem.Right - 4;
{$IFNDEF LITE}
        if FPressedItem <> FLockedSection then
{$ENDIF}
        Dec(rm, FHPos);
        if FPressedItem.Expandable then
        begin
          if ((rm + 13 - FPressedItem.Width) + (ResizeWidth - 4) < RM)
            and InRange(rm - 6 - (ResizeWidth - 4), RM, XPos)
            and InRange(2, 9, YPos) then
          begin
            THS := FPressedItem;
            FPressedItem := nil;
            THS.Expanded := not THS.Expanded;
            if THS.Expanded then
              DoSectionExpandEvent(THS)
            else
              DoSectionCollapseEvent(THS);
            b := false;
          end;
        end;
        if THS.FFilterEnabled then
        begin
          if ((rm + 13 - THS.Width) + (ResizeWidth - 4) < RM) and
              InRange(rm - 5 - (ResizeWidth - 4), RM, XPos) and
              InRange(12, 16, YPos) then
          begin
            TriggerFilterCallEvent(THS);
            b := false;
          end;
        end;
        if THS.FLookupEnabled then
        begin
{$IFNDEF LITE}
          if THS <> FLockedSection then
{$ENDIF}
          if ((rm + 13 - THS.Width) + (ResizeWidth - 4) < RM)
            and InRange(rm - 6 - (ResizeWidth - 4), RM, XPos)
            and InRange(12, 16, YPos) then
          begin
            if Flookup <> nil then
              FLookup.Text := ''
            else
            begin
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
              Flookup := TElAdvancedComboBox.Create(nil);
              TElAdvancedComboBox(FLookup).Flat := Self.Flat;
              TElAdvancedComboBox(FLookup).ActiveBorderType := fbtSunkenOuter;
{$else}
              Flookup := TComboBox.Create(nil);
{$endif}
{$ELSE}
              Flookup := TComboBox.Create(nil);
{$ENDIF}
              FLookup.Visible := false;
              FLookup.Style := csDropDown;
            end;
            FLookup.Parent := Parent;//Self;
            FLookup.OnExit := EditExit;
            FLookup.OnKeyDown := EditKeyDown;
            FLookup.OnChange := EditChange;
            FLookup.OnKeyUp := EditKeyUp;
            FLookup.Items.Assign(THS.FLookupHist);
            FLookupSection := THS;
            R := GetSectionRect(THS.Index);
{$IFNDEF LITE}
            if THS <> FLockedSection then
              xoffs := FHPos
            else
{$ENDIF}
            xoffs := 0;
            OffsetRect(R, Left - xoffs, Top);
            FLookup.BoundsRect := R;
            FLookup.Font.Assign(Font);
            FLookup.Font.Height := Abs(Font.Height) - 3;
            FLookup.Height := Max(FLookup.Height, R.Bottom - R.Top - 2);
            FDoingLookup := true;
            FLookup.Visible := true;
            {$ifndef CLX_USED}
            FFocusedCtl := Windows.GetFocus;
            {$else}
            {$ifdef LINUX}
            XGetInputFocus(QtDisplay, @FFocusedCtl, @FRevertTo);
            {$else}
            FFocusedCtl := Windows.GetFocus;
            {$endif}
            {$endif}
            FLookup.SetFocus;

            b := false;
          end;
        end;
        if b and (FPressedItem.FAllowClick {or FPressed}) then
          FPressedItem.UpdateSection
        else
          FPressedItem := nil;
      end;
    end; // not Resizing
  end;
end;

procedure TCustomElHeader.IntLButtonUp(XPos, YPos : SmallInt);
var
  THS : TElHeaderSection;
  NewPos : integer;
  R      : TRect;
begin
  if not (csNoStdEvents in ControlStyle) then
  begin
    if FResizing then
    begin
      FResizing := false;
      Screen.Cursor := Cursor;
      {$ifndef CLX_USED}
      ReleaseCapture;
      {$else}
      SetMouseGrabControl(nil);
      {$endif}
      if (not FResizeOnDrag) then
      begin
        if (FHeaderLineVis) then
          DrawLine(true);
        ReleaseLineDC;
        Dec(FHeaderLine, FResizeSection.Left);
{$IFNDEF LITE}
        if FResizeSection <> FLockedSection then
{$ENDIF}
        inc(FHeaderLine, FHPos);
        FResizeSection.Width := FHeaderLine;
      end;
      DoSectionResizing(FResizeSection, trsEnd, FResizeSection.Width);
      DoSectionResize(FResizeSection);
      FResizeSection := nil;
      Invalidate;
      Update;
    end
    else
    if FDropSrc <> nil then
    begin
      FDropTrg := GetSectionAt(XPos, YPos);
      if (FDropSrc <> FDropTrg) then
      begin
        if FDropTrg = nil then
        begin
          FDropTrg := GetSectionAt(XPos, 1);
        end;

        if FDropTrg = nil then
          NewPos := Sections.Count - 1
        else
          NewPos := FSections.FList.IndexOf(FDropTrg);
        FSections.MoveSection(FDropSrc, NewPos);
      end;
      FPressedItem := nil;
      FTrackSection := nil;
      FDropSrc := nil;
      FDropTrg := nil;
      SaveBmp.Free;
      SaveBmp := nil;
      DragBmp.Free;
      DragBmp := nil;
      DragBmpMask.Free;
      {$ifndef CLX_USED}
      ReleaseCapture;
      {$else}
      SetMouseGrabControl(nil);
      {$endif}
      R := ClientRect;
      {$ifndef CLX_USED}
      InvalidateRect(Handle, @R, true);
      {$else}
      Inc(R.Bottom); Inc(R.Right);
      QWidget_update(Handle, @R);
      Dec(R.Bottom); Dec(R.Right);
      {$endif}
      Update;
    end
    else
    begin
      FPressed := false;
      THS := FPressedItem;
      FPressedItem := nil;
      if THS <> nil then
      begin
        THS.UpdateSection;
        if (GetSectionAt(FPressCoord.X, FPressCoord.Y) = GetSectionAt(XPos, YPos)) then
          DoSectionClick(THS);
        THS := GetSectionAt(XPos, YPos);
        if (THS <> FTrackSection) then
        begin
          THS := FTrackSection;
          FTrackSection := nil;
          if THS <> nil then
            THS.UpdateSection;
        end;
      end;
    end;
  end;
end;

function TCustomElHeader.IntRButtonDown(XPos, YPos : SmallInt) : boolean;
var
  S : TElHeaderSection;
  P : TPoint;
begin
  if FResizing then
    result := true
  else
  begin
    result := false;
    S := Self.GetSectionAt(XPos, YPos);
    P := ClientToScreen(Point(XPos, YPos));
    if (S = nil) or (S.PopupMenu = nil) then
    begin
      if Assigned(PopupMenu) and (PopupMenu.AutoPopup) then
        PopupMenu.Popup(P.X, P.Y);
    end
    else
    if S.FPopupMenu.AutoPopup then
      S.FPopupMenu.Popup(P.X, P.Y);
  end;
end;

function TCustomElHeader.IntRButtonUp(XPos, YPos : SmallInt) : boolean;
begin
  result := FResizing;
end;

procedure TCustomElHeader.IntLButtonDblClick(XPos, YPos : SmallInt);
var
  S : TElHeaderSection;
  SP : TElSectionPart;
begin
  S := GetSectionAtEx(XPos, YPos, SP);
  if (SP = espResizeArea) and (S <> nil) and (S.Resizable) then
    TriggerSectionAutoSizeEvent(S);
end;

procedure TCustomElHeader.IntMouseMove(XPos, YPos : SmallInt);
var
  THS      : TElHeaderSection;
  {$ifndef CLX_USED}
  AMsg     : TMessage;
  {$endif}
  NewPos,
  NewWidth,
  L        : integer;
  XOffs    : integer;
  TmpBmp   : TBitmap;
  {$ifdef CLX_USED}
  R        : TRect;
  {$endif}

  function FixedRighterSections(Section : TElHeaderSection) : integer;
  var i : integer;
      ASect: TElHeaderSection;

  begin
    result := 0;
    for i := Section.Position + 1 to FSections.FList.Count - 1 do
    begin
      ASect := FSections.ItemByPos[i];
      if not ASect.Resizable then inc(result, ASect.Width);
    end;
  end;

begin
  if (FPressed) and (FAllowDrag)
{$IFNDEF LITE}
     and (FPressedItem <> FLockedSection)
{$ENDIF}
  then
  begin
    if (FPressedItem <> nil) and (FDropSrc = nil)
      and (sqrt(sqr(FPressCoord.X - XPos) + sqr(FPressCoord.Y - YPos)) >= 5) then
    begin
      (*
      FDropSrc := FTrackSection;
      FDropSrc.UpdateSection;
      if FDropSrc <> FPressedItem then
      *)
      FTrackSection := nil;
      FDropSrc := FPressedItem;
      FPressed := false;
      FDropSrc.UpdateSection;

      GetDragImage(FPressCoord.X);
{$IFNDEF LITE}
      if THS <> FLockedSection then
{$ENDIF}
      inc(FPressCoord.X, FHPos);
      {$ifndef CLX_USED}
      SetCapture(Handle);
      {$else}
      SetMouseGrabControl(Self);
      {$endif}
      FDropTrg := nil;
      FDragCoord := FPressCoord;
      DragRect := GetSectionRect(FDropSrc.Index);
{$IFNDEF LITE}
      if THS <> FLockedSection then
{$ENDIF}
      OffsetRect(DragRect, - FHPos, 0);
    end;
  end;
  if (FAllowDrag) and (not FPressed) and (FDropSrc <> nil) then
  begin
    FDropTrg := GetSectionAt(XPos, YPos);
    if FMoveOnDrag then
    begin
      if (FDropSrc <> FDropTrg) then
      begin
        if FDropTrg = nil then
        begin
          NewPos := Sections.Count - 1;
          FSections.MoveSection(FDropSrc, NewPos);
        end
        else if ((FDropSrc.Position < FDropTrg.Position) and (FDropTrg.Right - FDropSrc.Width < XPos))
          or ((FDropSrc.Position > FDropTrg.Position) and (FDropTrg.Left + FDropSrc.Width > XPos)) then
        begin
          NewPos := FSections.FList.IndexOf(FDropTrg);
          FSections.MoveSection(FDropSrc, NewPos);
        end;
      end;
    end;

{$IFNDEF LITE}
    XOffs := 0;
    if FDropSrc <> FLockedSection then
{$ENDIF}
    XOffs := FHPos;

    TmpBmp := TBitmap.Create;
    TmpBmp.Width := DragRect.Right - DragRect.Left + Abs(FDragCoord.x - (XPos + xOffs));
    TmpBmp.Height := DragRect.Bottom - DragRect.Top;
    {$ifndef CLX_USED}
    bitblt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, Canvas.Handle, Min(DragRect.Left, DragRect.Left - (FDragCoord.x - (XPos + xOffs))),  DragRect.Top, SRCCOPY);
    {$else}
    R.Left := Min(DragRect.Left, DragRect.Left - (FDragCoord.x - (XPos + xOffs)));
    R.Right := R.Left + TmpBmp.Width;
    R.Top := DragRect.Top;
    R.Bottom := R.Top + TmpBmp.Height;
    TmpBmp.Canvas.CopyRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height), Canvas, R);
    {$endif}
    if SaveBmp <> nil then
    begin
    {$ifndef CLX_USED}
      bitblt(TmpBmp.Canvas.Handle,
             Max(0, (FDragCoord.x - (XPos + xOffs))), 0,
             SaveBmp.Width, TmpBmp.Height,
             SaveBmp.Canvas.Handle, 0, 0, SRCCOPY);
    {$else}
      R.Left := Min(DragRect.Left, DragRect.Left - (FDragCoord.x - (XPos + xOffs)));
      R.Right := R.Left + TmpBmp.Width;
      R.Top := DragRect.Top;
      R.Bottom := R.Top + TmpBmp.Height;
      TmpBmp.Canvas.CopyRect(Rect(Max(0, (FDragCoord.x - (XPos + xOffs))), 0,
               SaveBmp.Width, TmpBmp.Height), SaveBmp.Canvas, Rect(0, 0, SaveBmp.Width, TmpBmp.Height));
    {$endif}
    end;

    OffsetRect(DragRect, - (FDragCoord.x - (XPos + xOffs)), 0);

    if SaveBmp = nil then
    begin
      SaveBmp := TBitmap.Create;
      SaveBmp.Width  := DragRect.Right - DragRect.Left;
      SaveBmp.Height := DragRect.Bottom - DragRect.Top;
    end;
    {$ifndef CLX_USED}
    bitblt(SaveBmp.Canvas.Handle, 0, 0,
           SaveBmp.Width, SaveBmp.Height, TmpBmp.Canvas.Handle,
           Max(0, -(FDragCoord.x - (XPos + xOffs))), 0, SRCCOPY);
    {$else}
    R.Left := Max(0, -(FDragCoord.x - (XPos + xOffs)));
    R.Right := R.Left + SaveBmp.Width;
    R.Top := 0;
    R.Bottom := R.Top + SaveBmp.Height;

    SaveBmp.Canvas.CopyRect(Rect(0, 0, SaveBmp.Width, SaveBmp.Height), TmpBmp.Canvas, R);
    {$endif}

{$IFNDEF VER90}
{$ifndef CLX_USED}
    if IsWinNTUp then
      MaskBlt(TmpBmp.Canvas.Handle, -(FDragCoord.x - (XPos + xOffs)), DragRect.Top, DragBmp.Width - 1, DragBmp.Height - 1, DragBmp.Canvas.Handle,
        0, 0, DragBmpMask.MaskHandle, 0, 0, ((SRCCOPY shl 8) and $FF000000) or SRCAND)
    else
{$endif}
{$ENDIF}
{$ifndef CLX_USED}
    BitBlt(TmpBmp.Canvas.Handle, -(FDragCoord.x - (XPos + xOffs)), DragRect.Top, DragBmp.Width, DragBmp.Height, DragBmp.Canvas.Handle,
           0, 0, SRCCOPY);
{$else}
    TmpBmp.Canvas.CopyRect(Rect(-(FDragCoord.x - (XPos + xOffs)), DragRect.Top, DragBmp.Width, DragBmp.Height), DragBmp.Canvas, Rect(0, 0, DragBmp.Width, DragBmp.Height));
{$endif}

    {$ifndef CLX_USED}
    bitblt(Canvas.Handle,
           Min(DragRect.Left, DragRect.Left + (FDragCoord.x - (XPos + xOffs))),
           DragRect.Top,
           TmpBmp.Width, TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
    {$else}
    R.Left := Min(DragRect.Left, DragRect.Left + (FDragCoord.x - (XPos + xOffs)));
    R.Top := DragRect.Top;
    R.Right := R.Left + TmpBmp.Width;
    R.Bottom := R.Top + TmpBmp.Height;
    Canvas.CopyRect(R, TmpBmp.Canvas, Rect(0, 0, TmpBmp.Width, TmpBmp.Height));
    {$endif}
    FDragCoord.X := XPos;
{$IFNDEF LITE}
    if FDropSrc <> FLockedSection then
{$ENDIF}
    inc(FDragCoord.X, FHPos);

    TmpBmp.Free;
  end;

  if ((FTracking) {$ifdef MSWINDOWS}or IsThemeApplied{$endif}) and (not FResizing) and
     (FDropSrc = nil) and (not (csNoStdEvents in ControlStyle)) and
     (not (csDesigning in ComponentState)) and FMouseInControl then
  begin
    THS := FTrackSection;
    FTrackSection := GetSectionAt(XPos, YPos);
    if THS <> FTrackSection then
    begin
      if THS <> nil then THS.UpdateSection;
      if FTrackSection <> nil then FTrackSection.UpdateSection;
    end;
  end;
  if (not FPressed) and (not FResizing) and (FDropSrc = nil) and FMouseInControl then
  begin
    if InResizeArea(XPos, THS) and (not (csDesigning in ComponentState)) and (THS.FResizable) then
    begin
      if Screen.Cursor <> crHSplit then Screen.Cursor := crHSplit;
    end
    else if Screen.Cursor = crHSplit then
      Screen.Cursor := Cursor;

    if (FHintSection <> nil) and (FHintSection <> THS) and (THS <> nil) and ShowHint and (THS.Hint <> '') then
    begin
      // P := ClientToScreen(SmallPointToPoint(Message.Pos));
      // Application.ActivateHint(P);
      {$ifndef CLX_USED}
      ZeroMemory(@AMsg, sizeof(AMsg));
      TWMMouse(AMsg).XPos := -10000;
      TWMMouse(AMsg).YPos := -10000;
      Application.HintMouseMessage(Self, AMsg);
      TWMMouse(AMsg).XPos := XPos;
      TWMMouse(AMsg).YPos := YPos;
      Application.HintMouseMessage(Self, AMsg);
      {$else}
      Application.HintMouseMessage(Self,[], -10000, -10000);
      Application.HintMouseMessage(Self, [], XPos, YPos);
      {$endif}
    end;
  end;
  if FResizing then
  begin
    L := FResizeSection.Left;
{$IFNDEF LITE}
    if FResizeSection <> FLockedSection then
{$ENDIF}
    XOffs := - FHPos
{$IFNDEF LITE}
    else XOffs := 0
{$ENDIF LITE}
    ;
    if (XPos - xOffs - FLineTab >= L)  and ((not StickySections) or
       (XPos - xOffs - FLineTab < Width - FixedRighterSections(FResizeSection))) then
    begin
      NewWidth := XPos - L - FLineTab;
{$IFNDEF LITE}
      if FResizeSection <> FLockedSection then
{$ENDIF}
      inc(NewWidth, FHPos);
      with FResizeSection do
        if InRange(MinWidth, MaxWidth, NewWidth) then
        begin
          if FResizeOnDrag then
          begin
            Width := NewWidth;
            InvalidateRight(FResizeSection.Index);
            Update;
            DoSectionResizing(FResizeSection, trsMove, NewWidth);
          end
          else
          begin
            DrawLine(true);
            FHeaderLine := L + NewWidth;
{$IFNDEF LITE}
            if FResizeSection <> FLockedSection then
{$ENDIF}
            dec(FHeaderLine, FHPos);
            DrawLine(false);
            DoSectionResizing(FResizeSection, trsMove, NewWidth);
          end;
        end;
    end; // really in client width
  end;
end;

procedure TCustomElHeader.IntMouseEnter;
begin
  if (FPressedItem <> nil) and (FDropSrc = nil) then
  begin
    FPressed := true;
    FPressedItem.UpdateSection;
  end;
  FMouseInControl := true;
end;

procedure TCustomElHeader.IntMouseLeave;
var
  THS : TElHeaderSection;
begin
  FMouseInControl := false;
  FDropTrg := nil;
  FPressed := false;
  if FPressedItem <> nil then
  begin
    THS := FPressedItem;
    THS.UpdateSection;
  end;
  if FTrackSection <> nil then
  begin
    THS := FTrackSection;
    FTrackSection := nil;
    THS.UpdateSection;
  end;
  if (not FResizing) and (Screen.Cursor = crHSplit) then Screen.Cursor := Cursor;
  FHintSection := nil;
end;

procedure TCustomElHeader.IntExit;
var P : TPoint;
begin
  inherited;
  if FPressed or FResizing or (FDropSrc <> nil) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    MouseUp(mbLeft, [], P.X, P.Y);
  end;
end;

procedure TCustomElHeader.IntSize;
var
  i : integer;
begin

  for i := 0 to Sections.Count - 1 do // Iterate
  begin
    if (Sections[i].LookupEnabled) or (Sections[i].FilterEnabled) then
    begin
      if (Height > 0) and (Height < 17) then Height := 17;
      break;
    end;
  end; // for
end;

function TCustomElHeader.IntHintShow(var HintInfo : THintInfo): Boolean;
var
  Section : TElHeaderSection;

  {$ifdef ELPACK_UNICODE}
  T: WideChar;
  WS: WideString;
  l : integer;
  S : String;
  {$endif}

begin
  Section := GetSectionAt(HintInfo.CursorPos.X, HintInfo.CursorPos.Y);
  FHintSection := Section;
  result := true;
  if (Section <> nil) and (Length(Section.Hint) > 0) then
    HintInfo.HintStr := Section.Hint;
  {$ifdef ELPACK_UNICODE}

  if (Section <> nil) and (Length(Section.Hint) > 0) then
    WS := Section.Hint
  else
    WS := FHint;

  if Length(WS) = 0 then
  begin
    HintInfo.HintStr := '';
    exit;
  end;

  if HintInfo.HintStr = GetShortHint(inherited Hint) then
  begin
    WS := GetShortHintW(WS);
    S := WS;
  end
  else
    S := WS;

  l := Length(S) + 1 + Length(WS) * 2;
  SetLength(HintInfo.HintStr, l + 4);
  Move(PChar(S)^, HintInfo.HintStr[1], Length(S) + 1);

  Move(WS[1], HintInfo.HintStr[Length(S) + 2], Length(WS) * 2);
  T := #0;
  Move(T, HintInfo.HintStr[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, HintInfo.HintStr[l + 3], sizeof(T));
  {$endif}
end;

{$ifdef CLX_USED}

procedure TCustomElHeader.MouseEnter(AControl: TControl);
begin
  inherited;
  IntMouseEnter;
end;

procedure TCustomElHeader.MouseLeave(AControl: TControl);
begin
  IntMouseLeave;
  inherited;
end;

function TCustomElHeader.WidgetFlags: Integer;
begin
  result := Integer(WidgetFlags_WRepaintNoErase);
end;

function TCustomElHeader.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
var Button: QT.ButtonState;
    TSI : TElHeaderSection;
begin
  case QEvent_type(Event) of
    QEventType_MouseButtonPress:
      begin
        Button := QMouseEvent_button(QMouseEventH(Event));
        if Integer(ButtonState_LeftButton) and Integer(Button) <> 0 then
          IntLButtonDown(QMouseEvent_x(QMouseEventH(Event)),
                         QMouseEvent_y(QMouseEventH(Event)))
        else
        if Integer(ButtonState_RightButton) and Integer(Button) <> 0 then
        begin
          result := IntRButtonDown(QMouseEvent_x(QMouseEventH(Event)),
                         QMouseEvent_y(QMouseEventH(Event)));
          if result then
            exit;
        end;
      end;
    QEventType_MouseButtonRelease:
      begin
        Button := QMouseEvent_button(QMouseEventH(Event));
        if Integer(ButtonState_LeftButton) and Integer(Button) <> 0 then
          IntLButtonUp(QMouseEvent_x(QMouseEventH(Event)),
                         QMouseEvent_y(QMouseEventH(Event)))
        else
        if Integer(ButtonState_RightButton) and Integer(Button) <> 0 then
        begin
          result := IntRButtonUp(QMouseEvent_x(QMouseEventH(Event)),
                                QMouseEvent_y(QMouseEventH(Event)));
          if result then
            exit;
        end;
      end;
    QEventType_MouseMove:
      begin
        IntMouseMove(QMouseEvent_x(QMouseEventH(Event)), QMouseEvent_y(QMouseEventH(Event)));
      end;
    QEventType_MouseButtonDblClick:
      begin
        Button := QMouseEvent_button(QMouseEventH(Event));
        if Integer(ButtonState_LeftButton) and Integer(Button) <> 0 then
          IntLButtonDblClick(QMouseEvent_x(QMouseEventH(Event)),
                             QMouseEvent_y(QMouseEventH(Event)));
      end;
    QEventType_Drop,
    QEventType_DragLeave:
      begin
        TSI := FDropTrg;
        FDropTrg := nil;
        if TSI <> nil then
        begin
          TSI.UpdateSection;
          Update;
        end;
      end;
  end;
  result := inherited EventFilter(Sender, Event);
end;

function TCustomElHeader.HintShow(var HintInfo : THintInfo): Boolean;
begin
  inherited HintShow(HintInfo);
  result := IntHintShow(HintInfo);
end;

procedure TCustomElHeader.DoExit;
begin
  inherited;
  IntExit;
end;
{$endif}

{$ifdef HAS_HTML_RENDER}
procedure TCustomElHeader.TriggerImageNeededEvent(Sender : TObject; Src :
    TElFString; var Image : TBitmap);
begin
  if (assigned(FOnImageNeeded)) then
    FOnImageNeeded(Self, Src , Image );
end;  { TriggerImageNeededEvent }
{$endif}

{$ifndef CLX_USED}
procedure TCustomElHeader.CreateHandle;
{$else}
procedure TCustomElHeader.CreateWidget;
{$endif}
begin
  inherited;
  AdjustHeaderHeight;
end;

{$ifndef CLX_USED}
{$ifdef ELPACK_COMPLETE}
procedure TCustomElHeader.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;
{$endif}
{$endif}

{$ifdef MSWINDOWS}
function TCustomElHeader.GetThemedClassName: WideString;
begin
  Result := 'HEADER';
end;

{$endif}

{$ifdef ELPACK_UNICODE}
procedure TCustomElHeader.SetHint(Value: WideString);
var S : String;
    i,
    l : integer;
    T : WideChar;
begin
  FHint := Value;

  S := FHint;
  i := Length(S);
  l := Length(S) + 1 + Length(FHint) * 2;
  SetLength(S, l + 4);

  Move(FHint[1], S[i + 2], Length(FHint) * 2);
  T := #0;
  Move(T, S[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, S[l + 3], sizeof(T));
  inherited Hint := S;
end;
{$endif}

initialization

  ElHeaderAscBmp := TBitmap.Create;
  ElHeaderDescBmp := TBitmap.Create;
  ElHeaderLeftBmp := TBitmap.Create;
  ElHeaderRightBmp := TBitmap.Create;
  ElHeaderPointBmp := TBitmap.Create;

  ElHeaderAscBmp.LoadFromResourceName(HInstance, 'ELHEADERASCBMP');
  {$ifdef CLX_USED}
  ElHeaderAscBmp.Mask(ElHeaderAscBmp.TransparentColor);
  {$endif}
  ElHeaderDescBmp.LoadFromResourceName(HInstance, 'ELHEADERDESCBMP');
  {$ifdef CLX_USED}
  ElHeaderDescBmp.Mask(ElHeaderDescBmp.TransparentColor);
  {$endif}
  ElHeaderLeftBmp.LoadFromResourceName(HInstance, 'ELHEADERLEFTBMP');
  {$ifdef CLX_USED}
  ElHeaderLeftBmp.Mask(ElHeaderLeftBmp.TransparentColor);
  {$endif}
  ElHeaderRightBmp.LoadFromResourceName(HInstance, 'ELHEADERRIGHTBMP');
  {$ifdef CLX_USED}
  ElHeaderRightBmp.Mask(ElHeaderRightBmp.TransparentColor);
  {$endif}
  ElHeaderPointBmp.LoadFromResourceName(HInstance, 'ELHEADERPOINTBMP');
  {$ifdef CLX_USED}
  ElHeaderPointBmp.Mask(ElHeaderPointBmp.TransparentColor);
  {$endif}

finalization

  ElHeaderPointBmp.Free;
  ElHeaderAscBmp.Free;
  ElHeaderDescBmp.Free;
  ElHeaderLeftBmp.Free;
  ElHeaderRightBmp.Free;

end.


