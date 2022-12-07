{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   XP Themes (c) Akzhan Abdulin                     }
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

07/11/2002

  Default page becomes active only when it is visible now

06/27/2002

  DefaultPage property added. It can be set in design-time to force activation
  of specified page  

05/27/2002

  Added OnDrawTab and changed parameter list for OnMeasureTab

05/15/2002

  Added UseActiveTabFont and ActiveTabFont properties. They let you specify separate
  font for active tab.
  Added TabCursor property

05/05/2002

  Added TabSheet.UseTabColor property to enable use of TabColor (which was useless previously)

05/03/2002

  Added cursor for drag'n'drop operations

04/25/2002

  Added drag'n'drop of the pages.

04/18/2002

  When the page is removed, the next page was erroneously activated. Fixed.

03/21/2002

  Controls on tab sheets didn't get accelerator chars. Fixed.

03/19/2002

  Fixed hint showing - now one can track the hint given 

03/10/2002

  PopupComponent is set to tabsheet or pagecontrol when PopupMenu is shown.  

03/06/2002

  Added unicode hint

02/28/2002

  VerticalSideCaptions property added.

02/24/2002

  Improved enabling of ScrollRight button in case when the last tab fits
  into view, but is covered with the button

02/22/2002

  Added TabBkColorNetStyle property to control background color of the tabs in NetTabs style.
  Accessible only in run-time

02/03/2002

  Added AngledTabs type

01/14/2002

  Added Shortcuts support for Top/Bottom tab orientations.

01/04/2002

  Slightly improved size calculation and painting of tabs with XP styles enabled
  and images assigned to tabs

12/24/2001

  ShowImages is now True by default.
  ShowImages is now taken into account when calculating the size of the tabs

12/21/2001

  Fixed size calculation and painting of tabs with XP styles enabled

11/28/2001

  Added FlatTabs style

11/21/2001

  Fixed the problem with tab space transparent background with XP styles enabled 

10/24/2001

  Now it's possible to set ShowBorder to false with XP styles enabled.

10/21/2001

  Improved painting of tab line when XP styles are enabled.

10/16/2001

  Fixed painting of tab backgrounds when XP styles are enabled.

10/15/2001

  Fixed painting of shadows in TabSheets when XP styles are enabled.
  ActivePage is forcefully made visible when the page control is loaded from
  resource

10/10/2001

  Tab hints are  shown now (optionally). 
  TabEnabled behavior improved. Now disabled tab functions in design-time and
  is completely disabled in run-time.

10/09/2001 (c) Akzhan Abdulin

  Windows XP Themes Support Added.

10/03/2001

  Fixed drawing of the line when tab style is in [tabs, net tabs], ShowBorder = false
  and tab position is in [right, bottom]

09/16/2001

  Fixed repainting bug that happened when size of the control is less than some limit

08/30/2001

  Problems with docking fixed
  Problem with ActivePage initial setting fixed 
  Now the tab is activated on MouseDown in [Tabs, NetTabs] styles

*)

unit ElPgCtl;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Consts,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  Menus,
  ComObj,
  {$ifdef VCL_4_USED}
  ImgList,
  {$endif}
{$ifdef VCL_6_USED}
Types,
{$endif}

  ElHintWnd,
  ElTimers,
  ElSndMap,
  ElImgFrm,
  ElList,
  ElVCLUtils,
  ElUxTheme,
  ElTmSchema,
  ElXPThemedControl,
  ElStrUtils;

const PM_REFRESHACTIVEPAGE = WM_USER + 7434;

type

  TElTabStyle = (etsTabs, etsButtons, etsFlatButtons, etsNetTabs, etsFlatTabs, etsAngledTabs);
  TElTabPosition = (etpTop, etpBottom, etpRight, etpLeft);
  TElTabDrawState = (edsBackground, edsEdges, edsContents);

  TElPageControl = class;
  TElTabSheet = class;
  TElTabs = class;
  TElTab = class;

  TElMeasureTabEvent = procedure(Sender: TObject; Canvas : TCanvas; Page : TElTabSheet;
    var Size : TSize) of object;

  TElPaintTabEvent = procedure(Sender: TObject; Canvas : TCanvas; Page : TElTabSheet; Rect : TRect; DrawStep : TElTabDrawState; var DefaultDrawing : boolean) of object;

  TElTabGetImageEvent = procedure(Sender: TObject; PageIndex: integer;
    var ImageIndex: integer) of object;
  TElTabChangingEvent = procedure(Sender: TObject; NewPage : TElTabSheet;
    var AllowChange: Boolean) of object;

  TElTabSheet = class(TCustomControl)
  private
    FTabShowing: Boolean;
    ARect      : TRect;
    AComplete  : boolean;
    ALine      : integer;
    AShown     : boolean;
    
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure SetTabShowing(Value: Boolean);
    procedure UpdateTabShowing;
    function GetPageIndex: Integer;
    procedure SetPageIndex(Value: Integer);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function GetBtnTheme: HTheme;
    function GetScrollTheme: HTheme;
    function GetTabTheme: HTheme;
    function GetUseXPThemes: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FTabColor: TColor;             
    FPageControl: TElPageControl;
    FImageIndex: integer;
    FTabVisible: Boolean;
    FCaption: TElFString;
    FTabEnabled: Boolean;
    FTabMenu: TPopupMenu;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}
    FUseTabColor: Boolean;

    procedure TriggerShowEvent; virtual;
    procedure TriggerHideEvent; virtual;
    procedure SetTabColor(Value: TColor);
    procedure SetPageControl(Value: TElPageControl);
    procedure SetImageIndex(Value: integer);
    procedure SetTabVisible(Value: Boolean);
    procedure SetCaption(Value: TElFString);
    procedure Paint; override;
    function GetTabIndex: Integer;

    procedure WMEraseBkGnd(var Msg : TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure SetTabEnabled(Value: Boolean);
    procedure SetTabMenu(Value: TPopupMenu);
    procedure CreateWnd; override;
    procedure ReadState(Reader: TReader); override;

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
    procedure SetUseTabColor(Value: Boolean);

    property TabShowing: Boolean read FTabShowing;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IsThemeApplied: Boolean;

    property TabIndex: Integer read GetTabIndex;
    property TabTheme: HTheme read GetTabTheme;
    property BtnTheme: HTheme read GetBtnTheme;
    property ScrollTheme: HTheme read GetScrollTheme;
    property UseXPThemes: Boolean read GetUseXPThemes;
  published
    property TabColor: TColor read FTabColor write SetTabColor default clBtnFace;

    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;

    property PageControl: TElPageControl read FPageControl write SetPageControl;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default true;
    property Caption: TElFString read FCaption write SetCaption;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property TabEnabled: Boolean read FTabEnabled write SetTabEnabled default true;
    property TabMenu: TPopupMenu read FTabMenu write SetTabMenu;
    property UseTabColor: Boolean read FUseTabColor write SetUseTabColor default
        false;

    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}

    // inherited properties
    property Color default clBtnFace;
    property ParentColor default false;
    property Visible;
    {$ifdef VCL_4_USED}
    property BorderWidth;
    property Constraints;
    {$endif}
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property Height stored False;
    property Left stored False;
    {$ifdef VCL_5_USED}
    property OnContextPopup;
    {$endif}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$ifdef VCL_4_USED}
    property OnResize;
    {$endif}
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Top stored False;
    property Width stored False;
  end;

  TElPageScrollBtnState = (pbsNone, pbsLeftBtnOver, pbsLeftBtnDown, pbsLeftBtnHeld, pbsRightBtnOver, pbsRightBtnDown, pbsRightBtnHeld);

  TElPageControl = class(TElXPThemedControl)
  protected
    ALines: Integer;
    FTabs: TElTabs;
    ScrollLeftRect,
    ScrollRightRect: TRect;
    TabsPos: TPoint;
    TabsSize: TSize;
    MemDC   : HDC;
    FScrollBtnState: TElPageScrollBtnState;
    FSaveCapture: HWND;
    FScrollTimer: TTimer;
    FTabIndex : integer;
    FBtnTheme: HTheme;
    FScrollTheme: HTheme;
    FHintCoords: TPoint;
    FHintTimer: TElTimer;
    FHintWnd  : THintWindow;
    FNoDTAlert: Boolean;
    FDragTab: TElTabSheet;
    FDoStartDrag: integer;
    FDraggablePages: boolean;
    FActivateSound: TElSoundName;
    FActivePage: TElTabSheet;
    FActiveTabColor: TColor;
    FBackground: TBitmap;
    FBackgroundType: TElBkGndType;
    FBorderWidth: Integer;
    FDownTab: TElTabSheet;
    FDrawFocus: Boolean;
    FDummyCaption: string;
    FFirstTab: TElTabSheet;
    FFlat: Boolean;
    FGradientEndColor: TColor;
    FGradientStartColor: TColor;
    FGradientSteps: Integer;
    FHotTrack: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TImageList;
    FImgForm: TElImageForm;
    FImgFormChLink: TImgFormChangeLink;
    FInactiveTabColor: TColor;
    FMinTabHeight: Integer;
    FMinTabWidth: Integer;
    FMultiLine: Boolean;
    FNewDockSheet: TElTabSheet;
    FOnChange: TNotifyEvent;
    FOnGetImageIndex: TElTabGetImageEvent;
    FOnMeasureTab: TElMeasureTabEvent;
    FOnChanging: TElTabChangingEvent;
    FPages: TElList;
    FRaggedRight: Boolean;
    FScrollOpposite: Boolean;
    FShowBorder: Boolean;
    FShowImages: Boolean;
    FShowTabs: Boolean;
    FSoundMap: TElSoundMap;
    FStyle: TElTabStyle;
    FTabHeight: DWORD;
    FTabPosition: TElTabPosition;
    FTabWidth: DWORD;
    FTmpBmp: TBitmap;
    FTrackTab: TElTabSheet;
    FUndockingPage: TElTabSheet;
    FTabBkColor: TColor;
{$IFNDEF VCL_4_USED}
    FOnResize : TNotifyEvent;
{$ENDIF}
    FHotTrackFont: TFont;
    FShowTabHints: Boolean;
    FSavvyMode: Boolean;
    FFlatTabBorderColor: TColor;
    FTabBkColorNetStyle: TColor;
    FVerticalSideCaptions: Boolean;
    FActiveTabFont: TFont;
    FUseActiveTabFont: Boolean;
    FTabCursor: TCursor;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}
    FOnDrawTab: TElPaintTabEvent;
    FDefaultPage: TElTabSheet;

{$IFDEF VCL_4_USED}
    procedure Resize; override;
{$ELSE}
    procedure Resize; dynamic;
{$ENDIF}
    function GetThemedClassName: WideString; override;
    procedure FreeThemeHandle; override;
    procedure CreateThemeHandle; override;
    function CanChange(NewPage : TElTabSheet): Boolean; virtual;
    function CanScrollLeft: Boolean;
    procedure Change; virtual;
    procedure ChangeActivePage(Page: TElTabSheet);

    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
        WM_WINDOWPOSCHANGED;
    procedure WMEraseBkGnd(var Msg : TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;

    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;

    {$ifdef VCL_4_USED}
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    {$endif}
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Message: TWMNCLBUTTONDOWN); message
        WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCLBUTTONUP); message WM_NCLBUTTONUP;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMControlListChange(var Message: TMessage); message
        CM_CONTROLLISTCHANGE;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message
        CM_DESIGNHITTEST;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    {$ifdef VCL_4_USED}
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMDockNotification(var Message: TCMDockNotification); message
        CM_DOCKNOTIFICATION;
    {$endif}
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    {$ifdef VCL_4_USED}
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    {$endif}
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    {$ifdef VCL_4_USED}
    procedure DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
        var Accept: Boolean); override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    {$endif}
    function GetActivePageIndex: Integer;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    {$ifdef VCL_4_USED}
    function GetDockClientFromMousePos(MousePos: TPoint): TControl;
    {$endif}
    function GetPageCount: Integer;
    {$ifdef VCL_4_USED}
    function GetPageFromDockClient(Client: TControl): TElTabSheet;
    {$endif}
    function GetPages(index : integer): TelTabSheet;
    {$ifdef VCL_4_USED}
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect; MousePos:
        TPoint; var CanDock: Boolean); override;
    {$endif}
    function GetTabIndex: Integer;
    procedure ImageChange(Sender : TObject);
    procedure ImageFormChange(Sender : TObject);
    procedure ImageListChange(Sender: TObject);
    procedure InsertPage(TabSheet : TElTabSheet); virtual;
    procedure MakeTabVisible(ATabSheet : TElTabSheet);
    procedure RebuildTabs(ResetFirstItem : boolean);
    procedure RedoTmpBmp;
    procedure RemovePage(TabSheet : TElTabSheet); virtual;
    procedure SetActivePage(Value: TElTabSheet);
    procedure SetActivePageIndex(const Value: Integer);
    procedure SetActiveTabColor(Value: TColor);
    procedure SetBackground(newValue: TBitmap);
    procedure SetBackgroundType(newValue: TElBkGndType);
    procedure SetBorderWidth(Value: Integer);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetDrawFocus(Value: Boolean);
    procedure SetFirstTab(Value: TElTabSheet);
    procedure SetFlat(newValue : Boolean);
    procedure SetGradientEndColor(newValue : TColor);
    procedure SetGradientStartColor(newValue : TColor);
    procedure SetGradientSteps(newValue : integer);
    procedure SetHotTrack(newValue : Boolean);
    procedure SetImageForm(newValue: TElImageForm);
    procedure SetImages(newValue : TImageList);
    procedure SetInactiveTabColor(Value: TColor);
    procedure SetMinTabHeight(Value: Integer);
    procedure SetMinTabWidth(Value: Integer);
    procedure SetMultiLine(newValue : Boolean);
    procedure SetRaggedRight(const Value: Boolean);
    procedure SetScrollOpposite(const Value: Boolean);
    procedure SetShowBorder(Value: Boolean);
    procedure SetShowImages(newValue : Boolean);
    procedure SetShowTabs(Value: Boolean);
    procedure SetStyle(newValue: TElTabStyle);
    procedure SetTabHeight(newValue : DWORD);
    procedure SetTabIndex(const Value: integer);
    procedure SetTabPosition(newValue : TElTabPosition);
    procedure SetTabWidth(newValue : DWORD);
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateActivePage; virtual;
    procedure UpdateTab(TabSheet : TElTabSheet);
    function CanScrollRight: Boolean;
    procedure SetHotTrackFont(Value: TFont);
    procedure HotTrackFontChange(Sender : TObject);
    procedure Paint; override;
    procedure SetScrollBtnState(Value: TElPageScrollBtnState);
    procedure SetTrackTab(Value: TElTabSheet);
    procedure OnScrollTimer(Sender : TObject);
    procedure SetTabBkColor(Value: TColor);
    function HasVisibleTabs: Boolean;
    function DoHitTest(X, Y : integer; var Res : integer) : boolean;
    procedure UpdateMultilineOrder;
    procedure TriggerGetImageEvent(PageIndex: integer; var ImageIndex: integer);
        virtual;
    procedure TriggerMeasureTabEvent(Canvas : TCanvas; Page : TElTabSheet; var Size : TSize); virtual;
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    procedure IFMEffectiveSize(var Message: TMessage); message IFM_EFFECTIVESIZE;
    procedure IFMCanPaintBkgnd(var Message: TMessage); message IFM_CANPAINTBKGND;
    procedure PMRefreshActivePage(var Message: TMessage); message
        PM_REFRESHACTIVEPAGE;
    procedure WMNCRButtonUp(var Message: TWMNCRBUTTONUP); message WM_NCRBUTTONUP;
    procedure WMNCCreate(var Message: TMessage); message WM_NCCREATE;
    procedure WMNCDestroy(var Message: TMessage); message WM_NCDESTROY;
    procedure WMNCMouseMove(var Message: TMessage); message WM_NCMOUSEMOVE;
    procedure OnHintTimer(Sender : TObject); virtual;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure SetSavvyMode(Value: Boolean);
    procedure SetFlatTabBorderColor(Value: TColor);
    procedure SetTabBkColorNetStyle(Value: TColor);
    procedure SetVerticalSideCaptions(Value: Boolean);
    procedure SetDraggablePages(Value: boolean);
    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
    procedure SetActiveTabFont(Value: TFont);
    procedure SetUseActiveTabFont(Value: Boolean);
    procedure ActiveTabFontChange(Sender : TObject);
    procedure TriggerDrawTabEvent(Canvas : TCanvas; Page : TElTabSheet; Rect : TRect; DrawStep :
        TElTabDrawState; var DefaultDrawing : boolean); virtual;

    property ScrollBtnState: TElPageScrollBtnState read FScrollBtnState write
        SetScrollBtnState;
    property Background: TBitmap read FBackground write SetBackground;
    property BackgroundType: TElBkGndType read FBackgroundType write
        SetBackgroundType default bgtColorFill;
    property GradientEndColor: TColor read FGradientEndColor write
        SetGradientEndColor;
    property GradientStartColor: TColor read FGradientStartColor write
        SetGradientStartColor;
    property GradientSteps: Integer read FGradientSteps write SetGradientSteps default 16;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function FindNextPage(CurPage: TElTabSheet; GoForward, CheckTabVisible,
        CheckTabEnabled : Boolean): TElTabSheet;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SelectNextPage(GoForward: Boolean);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function TabFromPoint(Point : TPoint): TElTabSheet;
    procedure UpdateTabs(Immediate : boolean);
    function NewPage: TElTabSheet;
    procedure Loaded; override;
    property ActivePageIndex: Integer read GetActivePageIndex write
        SetActivePageIndex;
    property FirstTab: TElTabSheet read FFirstTab write SetFirstTab;
    property PageCount: Integer read GetPageCount;
    property Pages[index : integer]: TElTabSheet read GetPages;
    property TrackTab: TElTabSheet read FTrackTab write SetTrackTab;
    property BtnTheme: HTheme read FBtnTheme;
    property ScrollTheme: HTheme read FScrollTheme;
    property TabBkColorNetStyle: TColor read FTabBkColorNetStyle write
        SetTabBkColorNetStyle nodefault;
  published
    property ActivateSound: TElSoundName read FActivateSound write FActivateSound;
    property ActiveTabColor: TColor read FActiveTabColor write SetActiveTabColor
        default clBtnFace;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property Caption: string read FDummyCaption;
    property Color default clBtnFace;
    property DrawFocus: Boolean read FDrawFocus write SetDrawFocus;
    property Flat: Boolean read FFlat write SetFlat;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default true;
    property ImageForm: TElImageForm read FImgForm write SetImageForm;
    property Images: TImageList read FImages write SetImages;
    property InactiveTabColor: TColor read FInactiveTabColor write
        SetInactiveTabColor default clBtnFace;
    property MinTabHeight: Integer read FMinTabHeight write SetMinTabHeight default
        40;
    property MinTabWidth: Integer read FMinTabWidth write SetMinTabWidth default 40;
    property Multiline: Boolean read FMultiLine write SetMultiLine;
    property OnChanging: TElTabChangingEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetImageIndex: TElTabGetImageEvent read FOnGetImageIndex write
        FOnGetImageIndex;
    property OnMeasureTab: TElMeasureTabEvent read FOnMeasureTab write
        FOnMeasureTab;
    property RaggedRight: Boolean read FRaggedRight write SetRaggedRight;
    property ScrollOpposite: Boolean read FScrollOpposite write SetScrollOpposite;
    property ShowBorder: Boolean read FShowBorder write SetShowBorder default true;
    property ShowImages: Boolean read FShowImages write SetShowImages default true;
    property ShowTabs: Boolean read FShowTabs write SetShowTabs default true;
    property SoundMap: TElSoundMap read FSoundMap write FSoundMap;
    property Style: TElTabStyle read FStyle write SetStyle;
    property TabHeight: DWORD read FTabHeight write SetTabHeight default 0;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabPosition: TElTabPosition read FTabPosition write SetTabPosition;
    property TabWidth: DWORD read FTabWidth write SetTabWidth default 0;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property TabBkColor: TColor read FTabBkColor write SetTabBkColor default
        clBtnFace;
    property ActivePage: TElTabSheet read FActivePage write SetActivePage;
    property ShowTabHints: Boolean read FShowTabHints write FShowTabHints default
        true;
    property SavvyMode: Boolean read FSavvyMode write SetSavvyMode default false;
    property FlatTabBorderColor: TColor read FFlatTabBorderColor write SetFlatTabBorderColor default clBtnShadow;
    property VerticalSideCaptions: Boolean read FVerticalSideCaptions write
        SetVerticalSideCaptions default true;
    property DraggablePages: boolean read FDraggablePages write SetDraggablePages default false;
    property ActiveTabFont: TFont read FActiveTabFont write SetActiveTabFont;
    property UseActiveTabFont: Boolean read FUseActiveTabFont write
        SetUseActiveTabFont default false;
    property TabCursor: TCursor read FTabCursor write FTabCursor default crDefault;
    property OnDrawTab: TElPaintTabEvent read FOnDrawTab write FOnDrawTab;
    property DefaultPage: TElTabSheet read FDefaultPage write FDefaultPage;
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
{$IFNDEF VCL_4_USED}
    property OnResize : TNotifyEvent read FOnResize write FOnResize;
{$ELSE}

    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property ShowHint;
    property Visible;
    property UseXPThemes;

    property OnDblClick;
    property OnStartDrag;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnUnDock;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnResize;
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}

{$IFDEF VCL_4_USED}
    property Anchors;
    property Constraints;
    property DockSite;
    property Floating;
    property DragKind;
{$ENDIF}
  end;

  TElTabClass = class of TElTab;

  TElTabs = class
  private
    FPageControl : TElPageControl;
    FTab: TElTab;
    function MeasureSheet(ACanvas : TCanvas; Sheet : TElTabSheet): TSize;
    procedure DoDrawTabs(ACanvas : TCanvas; Rect : TRect; DoDraw : boolean; var 
        Size : TSize);
    function GetBtnTheme: HTheme;
    function GetScrollTheme: HTheme;
    function GetTabTheme: HTheme;
  protected
    FTabClass: TElTabClass;
    function CalcTabAreaSize: TSize; virtual;
    procedure DrawTabs(ACanvas : TCanvas; R : TRect; DoDraw : boolean; var Size : TSize);
    procedure SetTabClass(Value: TElTabClass);
    function GetRotatedFont(Canvas : TCanvas; RotationAngle : integer): HFont;
    procedure ReorderPages(MaxRows : integer);
    function IsThemeApplied: Boolean;
    property TabTheme: HTheme read GetTabTheme;
    property BtnTheme: HTheme read GetBtnTheme;
    property ScrollTheme: HTheme read GetScrollTheme;
  public
    constructor Create(PageControl : TElPageControl);
    destructor Destroy; override;
    property TabClass: TElTabClass read FTabClass write SetTabClass;
  end;

  TElTab = class(TObject)
  private
    FOwner : TElTabs;
    procedure Draw(ACanvas : TCanvas; R : TRect; TabSheet : TElTabSheet);
    function GetBtnTheme: HTheme;
    function GetScrollTheme: HTheme;
    function GetTabTheme: HTheme;
    function GetTabPosition: TElTabPosition;
  protected
    function GetOuterMargin: Integer; virtual;
    function GetInnerMargin: Integer; virtual;
    procedure DrawTabContents(Canvas : TCanvas; R : TRect; TabSheet : TElTabSheet); 
        virtual;
    procedure DrawSpace(Canvas : TCanvas; var cx, cy : integer; mx, my : integer);
        virtual;
    function CanDrawTab(ActiveDraw : boolean): Boolean; virtual;
    function GetAscend: Integer; virtual;
    procedure AdjustDrawingSize(Active : boolean; var R : TRect); virtual;
    procedure AdjustTabSize(var Size : TSize); virtual;
    procedure AdjustFillSize(After : boolean; var R : TRect; TabSheet :
        TElTabSheet); virtual;
    procedure DrawTabLine(Canvas : TCanvas; R : TRect); virtual;
    procedure FillSpace(Canvas : TCanvas; Rect : TRect); virtual;
    procedure FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : TElTabSheet); 
        virtual;
    procedure DrawButtons(Canvas : TCanvas; LeftRect, RightRect : TRect; CSL, CSR : 
        boolean); virtual;
    function GetRowMargin: Integer; virtual;

    function IsThemeApplied: Boolean;
    procedure DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : TElTabSheet);
        virtual; abstract;
    function GetContentMargin: Integer; virtual;
    procedure FixupTab(Canvas : TCanvas; R : TRect; TabSheet : TElTabSheet); 
        virtual;
    property TabTheme: HTheme read GetTabTheme;
    property BtnTheme: HTheme read GetBtnTheme;
    property ScrollTheme: HTheme read GetScrollTheme;
    property TabPosition: TElTabPosition read GetTabPosition;
  public
    constructor Create(Owner : TElTabs);
  end;

  TElStdTab = class(TElTab)
  private
  protected
    function GetOuterMargin: Integer; override;
    function CanDrawTab(ActiveDraw : boolean): Boolean; override;
    function GetAscend: Integer; override;
    procedure AdjustDrawingSize(Active : boolean; var R : TRect); override;
    procedure AdjustTabSize(var Size : TSize); override;
    procedure DrawTabLine(Canvas : TCanvas; R : TRect); override;
    procedure AdjustFillSize(After : boolean; var R : TRect; TabSheet : 
        TElTabSheet); override;
    procedure FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : TElTabSheet); 
        override;
    procedure DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : TElTabSheet);
        override;
  public
  end;

  TElBtnTab = class(TElTab)
  private
  protected
    function GetInnerMargin: Integer; override;
    procedure DrawSpace(Canvas : TCanvas; var cx, cy : integer; mx, my : integer); 
        override;
    procedure AdjustTabSize(var Size : TSize); override;
    function GetRowMargin: Integer; override;
    procedure FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : TElTabSheet); override;
    procedure DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : TElTabSheet);
        override;
  end;

  TElFlatBtnTab = class(TElTab)
  private
  protected
    function GetInnerMargin: Integer; override;
    procedure DrawSpace(Canvas : TCanvas; var cx, cy : integer; mx, my : integer);
        override;
    procedure AdjustTabSize(var Size : TSize); override;
    procedure AdjustFillSize(After : boolean; var R : TRect; TabSheet :
        TElTabSheet); override;
    function GetRowMargin: Integer; override;
    procedure FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : TElTabSheet); override;
    procedure DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : TElTabSheet);
        override;
  end;

  TElNetTab = class(TElTab)
  private
  protected
    procedure AdjustTabSize(var Size : TSize); override;
    function GetInnerMargin: Integer; override;
    procedure DrawSpace(Canvas : TCanvas; var cx, cy : integer; mx, my : integer); 
        override;
    function CanDrawTab(ActiveDraw : boolean): Boolean; override;
    procedure AdjustDrawingSize(Active : boolean; var R : TRect); override;
    function GetOuterMargin: Integer; override;
    procedure DrawTabLine(Canvas : TCanvas; R : TRect); override;
    function GetAscend: Integer; override;
    procedure FillSpace(Canvas : TCanvas; Rect : TRect); override;
    procedure AdjustFillSize(After : boolean; var R : TRect; TabSheet : 
        TElTabSheet); override;
    procedure FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : TElTabSheet); 
        override;
    procedure DrawButtons(Canvas : TCanvas; LeftRect, RightRect : TRect; CSL, CSR : 
        boolean); override;
    procedure DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : TElTabSheet);
        override;
  end;

  TEl2DFlatTab = class(TElTab)
  private
  protected
    procedure AdjustDrawingSize(Active : boolean; var R : TRect); override;
    procedure AdjustFillSize(After : boolean; var R : TRect; TabSheet : 
        TElTabSheet); override;
    procedure AdjustTabSize(var Size : TSize); override;
    function CanDrawTab(ActiveDraw : boolean): Boolean; override;
    procedure DrawTabLine(Canvas : TCanvas; R : TRect); override;
    function GetAscend: Integer; override;
    function GetInnerMargin: Integer; override;
    function GetOuterMargin: Integer; override;
    procedure FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : TElTabSheet); 
        override;
    procedure DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : TElTabSheet);
        override;
  end;

  TElAngledTab = class(TElTab)
  private
    SaveDCState: Integer;
  protected
    function CanDrawTab(ActiveDraw : boolean): Boolean; override;
    procedure DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : TElTabSheet);
        override;
    procedure DrawTabLine(Canvas : TCanvas; R : TRect); override;
    procedure AdjustFillSize(After : boolean; var R : TRect; TabSheet : 
        TElTabSheet); override;
    procedure AdjustDrawingSize(Active : boolean; var R : TRect); override;
    procedure AdjustTabSize(var Size : TSize); override;
    procedure FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : TElTabSheet); 
        override;
    function GetAscend: Integer; override;
    function GetInnerMargin: Integer; override;
    function GetOuterMargin: Integer; override;
    function GetContentMargin: Integer; override;
    procedure CreateTabPoints(R : TRect; Points : PPoint);
    procedure FixupTab(Canvas : TCanvas; R : TRect; TabSheet : TElTabSheet); 
        override;
  end;

implementation

uses ElTools, ElExtBkgnd;

const

  Margin = 4;
  ButtonWidth = 15;
  AngledOffset= 9;

procedure IntMapWindowPoints(SrcWnd, DstWnd : HWND; var Rect : TRect; Transform : integer);
begin
  ScreenToClient(DstWnd, Rect.TopLeft);
  ScreenToClient(DstWnd, Rect.BottomRight);
end;

procedure TElTabSheet.TriggerShowEvent;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TElTabSheet.TriggerHideEvent;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;

procedure TElTabSheet.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
    TriggerShowEvent
  else
  if not Showing then
    TriggerHideEvent;
  (*
  if TabVisible and (FPageControl <> nil) then
    FPageControl.RebuildTabs(false);
  *)
end;

constructor TElTabSheet.Create(AOwner : TComponent);
begin
  inherited;
  Color := clBtnFace;
  TabColor := clBtnFace;
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := false;
  FTabVisible := true;
  FTabEnabled := true;
  FImageIndex := -1;
end;

procedure TElTabSheet.SetTabColor(Value: TColor);
begin
  if FTabColor <> Value then
  begin
    FTabColor := Value;
    if Parent <> nil then
      TElPageControl(Parent).UpdateTab(Self);
  end;
end;

procedure TElTabSheet.SetPageControl(Value: TElPageControl);
begin
  if FPageControl <> Value then
  begin
    if FPageControl <> nil then
      FPageControl.RemovePage(Self);
    FPageControl := Value;
    Parent := FPageControl;
    if FPageControl <> nil then
    begin
      FPageControl.InsertPage(Self);
      if TabVisible and (FPageControl <> nil) then
        FPageControl.UpdateTab(Self);

      if FPageControl.ActivePage = nil then
        FPageControl.ActivePage := Self;
    end;
    if ThemesAvailable and HandleAllocated then
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_FRAMECHANGED);
  end;
end;

procedure TElTabSheet.SetImageIndex(Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if TabVisible and (FPageControl <> nil) then
      FPageControl.UpdateTab(Self);
  end;
end;

procedure TElTabSheet.SetTabVisible(Value: Boolean);
begin
  if FTabVisible <> Value then
  begin
    FTabVisible := Value;
    UpdateTabShowing;
  end;
end;

procedure TElTabSheet.SetCaption(Value: TElFString);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if TabVisible and (FPageControl <> nil) then
      FPageControl.RebuildTabs(false);
  end;
end;

procedure TElTabSheet.WMPaint(var Msg : TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  R : TRect;
  ARgn  : HRGN;

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
      ;
      {else
        InflateRect(R, 1, 1);
      }
      with R do
        ARgn := CreateRectRgn(Left, Top, right, Bottom);
      SelectClipRgn(MemDC, ARgn);

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

procedure TElTabSheet.SetTabShowing(Value: Boolean);
begin
  if FTabShowing <> Value then
  begin
    FTabShowing := Value;
    FPageControl.RebuildTabs(false);
  end;
end;

procedure TElTabSheet.UpdateTabShowing;
begin
  SetTabShowing((FPageControl <> nil) and FTabVisible);
end;

function TElTabSheet.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
    Result := FPageControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

procedure TElTabSheet.SetPageIndex(Value: Integer);
var
  i, MaxPageIndex: Integer;
begin
  if FPageControl <> nil then
  begin
    MaxPageIndex := FPageControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.CreateFmt('%d is an invalid PageIndex value.  PageIndex must be ' +
    'between 0 and %d', [Value, MaxPageIndex]);
    i := TabIndex;
    FPageControl.FPages.Move(PageIndex, Value);
    if i >= 0 then
      FPageControl.RebuildTabs(false);
  end;
end;

procedure TElTabSheet.Paint;
var R, Rect,
    R1     : TRect;
    R2: TRect;
    ACtl   : TWinControl;
    BgRect : TRect;

begin
  R := ClientRect;
  if IsThemeApplied then
  begin
    R2 := BoundsRect;
    OffsetRect(R2, -R2.Left, -R2.Top);
    GetThemeBackgroundContentRect(TabTheme, Canvas.Handle, TABP_PANE, 0, R2, R1);

    R2.Left := - R1.Left;
    R2.Top := - R1.Top;
    R2.Right := R.Right + R2.Right - R1.Right - R2.Left + R1.Left;
    R2.Bottom := R.Bottom + R2.Bottom - R1.Bottom - R2.Top + R1.Top;

    R1 := Canvas.ClipRect;

    DrawThemeBackground(TabTheme, Canvas.Handle, TABP_PANE, 0, R2, @R1);
    //RedrawWindow(PageControl.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
    exit;               
  end;
                                      
  
  if (FPageControl <> nil) and (FPageControl.FImgForm <> nil) and (not (csDesigning in FPageControl.FImgForm.ComponentState)) then
  begin
    if (FPageControl.FImgForm.Control <> Self) then
    begin
      if (FPageControl.FImgForm.Control <> FPageControl) then
      begin
        ACtl := FPageControl.FImgForm.GetRealControl;
        BgRect := R;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
      end
      else
      begin
        ACtl := FPageControl.FImgForm.GetRealControl;
        BgRect := R;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        BgRect.TopLeft := ACtl.Parent.ScreenToClient(BgRect.TopLeft);
        Dec(BgRect.Left, FPageControl.Left);
        Dec(BgRect.Top, FPageControl.Top);
      end;
      FPageControl.FImgForm.PaintBkgnd(Canvas.Handle, R, BgRect.TopLeft, false);
    end;
  end
  else
  begin
    with Canvas do
    case FPageControl.BackgroundType of //
      bgtColorFill :
        begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := Color;
          Canvas.FillRect(R);
        end;
      bgtHorzGradient,
      bgtVertGradient:
        GradientFill(Canvas.Handle, R, FPageControl.GradientStartColor, FPageControl.GradientEndColor, FPageControl.GradientSteps, FPageControl.BackgroundType = bgtVertGradient);
      bgtStretchBitmap,
      bgtTileBitmap:
        begin
          CopyRect(Rect, FPageControl.FTmpBmp.Canvas, Classes.Rect(0, 0, FPageControl.FTmpBmp.Width, FPageControl.FTmpBmp.Height));
        end;
      bgtCenterBitmap :
        begin
          Brush.Color := Color;
          Rect := R;
          FillRect(Rect);
          with FPageControl do
          begin
            R := Classes.Rect(0, 0, FBackground.Width, FBackground.Height);
            CenterRects(FBackground.Width, Rect.Right - Rect.Left, FBackground.Height, Rect.Bottom - Rect.Top, R1);
            OffsetRect(R1, Rect.Left, Rect.Top);
            CopyRect(R1, FBackground.Canvas, Classes.Rect(0, 0, FBackground.Width, FBackground.Height));
          end;
        end;
    end; // case
  end;
end;  { Paint }

procedure TElTabSheet.CMEnabledChanged(var Message: TMessage);
begin
  if TabVisible and (FPageControl <> nil) then
    FPageControl.RebuildTabs(false);
  Invalidate;
end;

destructor TElTabSheet.Destroy;
begin
  if FPageControl <> nil then
  begin
    FPageControl.RemovePage(Self);
  end;
  inherited;
end;

function TElTabSheet.GetTabIndex: Integer;
var i : integer;
begin
  result := 0;
  if not FTabShowing then
    result := -1
  else
    for i := 0 to PageIndex - 1 do
      if TElTabSheet(FPageControl.FPages[I]).FTabShowing then
        Inc(result);
end;

procedure TElTabSheet.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = PageControl) then
      PageControl := nil;
    if AComponent = FTabMenu then
      TabMenu := nil; 
  end;
end;

procedure TElTabSheet.WMEraseBkGnd(var Msg : TWMEraseBkGnd);
var RW,
    R1,
    R2 : TRect;
    s  : integer;
begin
  if IsThemeApplied then
  begin
    GetWindowRect(Handle, RW);
    IntMapWindowPoints(0, Handle, RW, 2);
    //DrawThemeParentBackground(Handle, Msg.DC, RW);
    R2 := RW;
    // hide borders
    if PageControl.ShowBorder then
      s := 1
    else
      s := 2;
    // this conversion has been made intentionally to hide borders that might be not drawn
    RW := ClientRect;
    GetThemeBackgroundContentRect(TabTheme, Msg.DC, TABP_PANE, 0, RW, R1);
    Inc(RW.Left, (RW.Left - R1.Left) * s);
    Inc(RW.Top, (RW.Top - R1.Top) * s);
    Inc(RW.Right, (RW.Right - R1.Right) * s);
    Inc(RW.Bottom, (RW.Bottom - R1.Bottom) * s);

    if PageControl.ShowTabs and (PageControl.Style in [etsTabs, etsNetTabs, etsFlatTabs, etsAngledTabs]) then
    begin
      case PageControl.TabPosition of
        etpTop: RW.Top := RW.Top + (R2.Top - R1.Top);
        etpBottom: RW.Bottom := RW.Bottom + (R2.Bottom - R1.Bottom);
        etpRight: RW.Right := RW.Right + (R2.Right - R1.Right);
        etpLeft: RW.Left := RW.Left + (R2.Left - R1.Left);
      end;
    end;

    DrawThemeBackground(TabTheme, Msg.DC, TABP_PANE, 0, RW, @RW);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    FillRect(Msg.DC, ClientRect, Canvas.Brush.Handle);
  end;
  Msg.result := 1;
end;

procedure TElTabSheet.SetTabEnabled(Value: Boolean);
begin
  if FTabEnabled <> Value then
  begin
    FTabEnabled := Value;
    if TabShowing then
      FPageControl.UpdateTabs(false);
  end;
end;

procedure TElTabSheet.SetTabMenu(Value: TPopupMenu);
begin
  if FTabMenu <> Value then
  begin
    {$ifdef D_5_UP}
    if FTabMenu <> nil then
      FTabMenu.RemoveFreeNotification(Self);
    {$endif}
    FTabMenu := Value;
    if FTabMenu <> nil then
      FTabMenu.FreeNotification(Self);
  end;
end;

(*
function TElPageControl.CanChange(NewPage : TElTabSheet): Boolean;
begin
  result := True;
  if Assigned(FOnChanging) then FOnChanging(Self, NewPage, result);
end;
*)
function TElPageControl.CanScrollLeft: Boolean;
var i : integer;
begin
  result := false;
  if Multiline or (FirstTab = nil) then
    exit
  else
  for i := FirstTab.PageIndex - 1 downto 0 do
  begin
    if TElTabSheet(FPages[i]).TabVisible then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TElPageControl.CanChange(NewPage : TElTabSheet): Boolean;
begin
  result := (NewPage = nil) or (NewPage.TabEnabled or (csDesigning in ComponentState));
  if Assigned(FOnChanging) then
    FOnChanging(Self, NewPage, result);
end;

procedure TElPageControl.Change;
begin
{$IFDEF USE_SOUND_MAP}
  if SoundMap <> nil then
    SoundMap.Play(FActivateSound);
{$ENDIF}
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TElPageControl.ChangeActivePage(Page: TElTabSheet);
var
  ParentForm: TCustomForm;
begin
  if FActivePage <> Page then
  begin
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and (FActivePage <> nil) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) then
    begin
      ParentForm.ActiveControl := FActivePage;
      if ParentForm.ActiveControl <> FActivePage then
      begin
        TabIndex := FActivePage.TabIndex;
        exit;
      end;
    end;
    if Page <> nil then
    begin
      Page.Visible := True;
      Page.BringToFront;
      if (ParentForm <> nil) and (FActivePage <> nil) and
        (ParentForm.ActiveControl = FActivePage) then
        if Page.CanFocus then
          ParentForm.ActiveControl := Page
        else
          ParentForm.ActiveControl := Self;
    end;
    if FActivePage <> nil then
    begin
      FActivePage.Visible := False;
      if FSavvyMode then
        FActivePage.DestroyHandle;
    end;
    FActivePage := Page;
    if FSavvyMode and (FActivePage <> nil) and HandleAllocated and (not FActivePage.HandleAllocated) then
      FActivePage.UpdateControlState;
    if (ParentForm <> nil) and (FActivePage <> nil) and
      (ParentForm.ActiveControl = FActivePage) then
      FActivePage.SelectFirst;
    Change;
  end;
end;

procedure TElPageControl.CMControlListChange(var Message: TMessage);
begin
  inherited;
  HandleNeeded;
  RebuildTabs(false);
end;

procedure TElPageControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  ATabSheet : TElTabSheet;
  P : TPoint;
  Px: TPoint;
  i : integer; 
begin
  Px := ClientToScreen(SmallPointToPoint(Message.Pos));
  i := HTNOWHERE;
  if DoHitTest(Px.x, Px.y, i) then Message.Result := 1;

  P := Parent.ScreenToClient(Px);
  dec(p.x, Left);
  dec(p.y, Top);
  //MessageBox(0, PChar(Format('x=%d, y=%d', [P.x, P.y])), nil, 0);
  ATabSheet := TabFromPoint(P);
  if (ATabSheet <> nil) and (ATabSheet <> ActivePage) then
  begin
    Message.result := 1;
  end;
end;

procedure TElPageControl.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Focused or Windows.IsChild(Handle, Windows.GetFocus)) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) and
    ShowTabs then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.result := 1;
  end else
    inherited;
end;

{$ifdef VCL_4_USED}
procedure TElPageControl.CMDockClient(var Message: TCMDockClient);
var
  IsVisible: Boolean;
  DockCtl: TControl;
begin
  Message.result := 0;
  FNewDockSheet := TElTabSheet.Create(Self);
  try
    try
      DockCtl := Message.DockSource.Control;
      if DockCtl is TCustomForm then
        FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;
      FNewDockSheet.PageControl := Self;
      DockCtl.Dock(Self, Message.DockSource.DockRect);
    except
      FNewDockSheet.Free;
      raise;
    end;
    IsVisible := DockCtl.Visible;
    FNewDockSheet.TabVisible := IsVisible;
    if IsVisible then ActivePage := FNewDockSheet;
    DockCtl.Align := alClient;
  finally
    FNewDockSheet := nil;
  end;
end;

procedure TElPageControl.CMDockNotification(var Message: TCMDockNotification);
var
  I: integer;
  S: string;
  Page: TElTabSheet;
begin
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
        begin
          S := PChar(Message.NotifyRec.MsgLParam);
          { Search for first CR/LF and end string there }
          for i := 1 to Length(S) do
            if S[i] in [#13, #10] then
            begin
              SetLength(S, i - 1);
              Break;
            end;
          Page.Caption := S;
        end;
      CM_VISIBLECHANGED:
        Page.TabVisible := Boolean(Message.NotifyRec.MsgWParam);
    end;
  inherited;
end;
{$endif}

procedure TElPageControl.CMFocusChanged(var Message: TMessage);
begin
  inherited;
  UpdateTabs(false);
end;

procedure TElPageControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if ParentFont then
  begin
    HotTrackFont.Name := Font.Name;
    HotTrackFont.Size := Font.Size;
  end;
  Canvas.Font.Assign(Font);
  UpdateTabs(false);
end;

procedure TElPageControl.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if not HasVisibleTabs then
    Invalidate;
  if (Style = etsNetTabs) and not (csLoading in ComponentState) then
    TabBkColorNetStyle := IncColor(Color, 20, 20, 20);
  if ShowBorder then
    UpdateTabs(false);
end;

procedure TElPageControl.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if not HasVisibleTabs then
    Invalidate;
  UpdateTabs(false);
end;

{$ifdef VCL_4_USED}
procedure TElPageControl.CMUnDockClient(var Message: TCMUnDockClient);
var
  Page: TElTabSheet;
begin
  inherited;
  Message.result := 0;
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
  begin
    FUndockingPage := Page;
    Message.Client.Align := alNone;
  end;
end;
{$endif}

procedure TElPageControl.CreateHandle;
var i : integer;
begin
  inherited CreateHandle;
  if FirstTab = nil then
  begin
    for i := 0 to FPages.Count - 1 do
    begin
      if TElTabSheet(FPages[i]).TabVisible then
      begin
        FirstTab := TElTabSheet(FPages[i]);
        break;
      end;
    end;
  end;
  {
  UpdateTabs;
  ASheet := ActivePage;
  if ASheet <> nil then
  begin
    FActivePage := nil;
    ActivePage := ASheet;
  end;
  }
end;

procedure TElPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

{$ifdef VCL_4_USED}
procedure TElPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TElPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewDockSheet <> nil then
    Client.Parent := FNewDockSheet;
end;

procedure TElPageControl.DoRemoveDockClient(Client: TControl);
var APage : TElTabSheet;
begin
  if (FUndockingPage <> nil) and not (csDestroying in ComponentState) then
  begin
    APage := FindNextPage(ActivePage, true, true, false); 
    FUndockingPage.Free;
    FUndockingPage := nil;
    ActivePage := APage;
  end;
end;
{$endif}

function TElPageControl.GetActivePageIndex: Integer;
begin
  if ActivePage <> nil then
    Result := ActivePage.GetPageIndex
  else
    Result := -1;
end;

procedure TElPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i : integer;
begin
  for i := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[i]));
end;

{$ifdef VCL_4_USED}
function TElPageControl.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  Page: TElTabSheet;
begin
  Result := nil;
  if DockSite then
  begin
    Page := TabFromPoint(MousePos);
    if (Page <> nil) and (Page.ControlCount > 0) then
    begin
      Result := Page.Controls[0];
      if Result.HostDockSite <> Self then
        Result := nil;
    end;
  end;
end;
{$endif}

function TElPageControl.GetPageCount: integer;
begin
  result := FPages.Count;
end; { GetPageCount }

{$ifdef VCL_4_USED}
function TElPageControl.GetPageFromDockClient(Client: TControl): TElTabSheet;
var
  i : integer;
begin
  result := nil;
  if Client = nil then
  begin
    result := nil;
    exit;
  end;
  for i := 0 to PageCount - 1 do
  begin
    if (Client.Parent = Pages[i]) and (Client.HostDockSite = Self) then
    begin
      result := Pages[i];
      exit;
    end;
  end;
end;
{$endif}

function TElPageControl.GetPages(index : integer): TelTabSheet;
{ Returns the value of data member FPages[index ]. }
begin
  result := FPages[index];
end; { GetPages }

{$ifdef VCL_4_USED}
procedure TElPageControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
    MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;
{$endif}

function TElPageControl.GetTabIndex: integer;
var
  i: integer;
begin
  result := -1;
  if ActivePage <> nil then
  begin
    if ActivePage.TabShowing then
      result := ActivePage.TabIndex
    else
    begin
      for i := ActivePage.PageIndex + 1 to PageCount - 1 do
        if Pages[i].TabShowing then
        begin
          result := Pages[i].TabIndex;
          break;
        end;
      if result = -1 then
      begin
        for i := ActivePage.PageIndex - 1 downto 0 do
          if Pages[i].TabShowing then
          begin
            result := Pages[i].TabIndex;
            break;
          end;
      end;    
    end;
  end;
end;

procedure TElPageControl.ImageChange(Sender : TObject);
begin
  if ((FBackground.Height = 0) or (FBackground.Width = 0)) then
    BackgroundType := bgtColorFill
  else
  begin
    RedoTmpBmp;
    Invalidate;
    if ActivePAge <> nil then
      ActivePage.Invalidate;
  end;
end;

procedure TElPageControl.ImageFormChange(Sender : TObject);
begin
  RedoTmpBmp;
  Invalidate;
  UpdateTabs(false);
  if ActivePage <> nil then
    ActivePage.Invalidate;
end;

procedure TElPageControl.ImageListChange(Sender: TObject);
begin
  RebuildTabs(false);
end;

procedure TElPageControl.InsertPage(TabSheet : TElTabSheet);
var b : boolean;
begin
  b := HasVisibleTabs;
  if FPages.IndexOf(TabSheet) = -1 then
  begin
    FPages.Add(TabSheet);
    TabSheet.FPageControl := Self;
    if IsThemeApplied then
      SetWindowPos(TabSheet.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOOWNERZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_FRAMECHANGED);
    TabSheet.UpdateTabShowing;
    if (b <> HasVisibleTabs) or Multiline then
      RebuildTabs(false);
  end;
end;

procedure TElPageControl.MakeTabVisible(ATabSheet : TElTabSheet);
begin
  if HandleAllocated and ShowTabs then
  begin
    if (not Multiline) then
    begin
      if (FirstTab <> nil) then
      begin
        if FirstTab.PageIndex < ATabSheet.PageIndex then
        begin
          if (not ATabSheet.AShown) or not ATabSheet.AComplete then
          begin
            if not ATabSheet.TabVisible then
              exit;
            while (not ATabSheet.AComplete) and (FirstTab <> ATabSheet) do
              FirstTab := FindNextPage(FirstTab, true, true, false);
          end;
        end
        else
        if FirstTab.PageIndex > ATabSheet.PageIndex then
          FirstTab := ATabSheet;
      end
      else
        FirstTab := ATabSheet;
    end;
  end;
end;
                      
procedure TElPageControl.RebuildTabs(ResetFirstItem : boolean);
begin
  if ResetFirstItem and (FPages.Count > 0) then
    FFirstTab := FindNextPage(nil, true, true, false);
  if HandleAllocated then
  begin
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_FRAMECHANGED);
    RequestAlign;
  end;
  if ResetFirstItem and (FPages.Count > 0) then
  begin
    UpdateTabs(false);
    UpdateMultilineOrder;
  end;
end;

procedure TElPageControl.RedoTmpBmp;
var BgRect,
    BgRect2 : TRect;
begin
  if BackgroundType in [bgtHorzGradient, bgtVertGradient, bgtColorFill, bgtCenterBitmap] then
  begin
    FTmpBmp.FreeImage;
  end else
  begin
    if (ClientWidth <> 0) and (ClientHeight <> 0) then
    begin
      FTmpBmp.Height := ClientHeight - 1;
      FTmpBmp.Width := ClientWidth - 1;
      BgRect := ClientRect;
      BgRect2 := BgRect;
      OffsetRect(BgRect2, BgRect2.Left, BgRect2.Top);
      ElExtBkgnd.ExtDrawBkgnd(FTmpBmp.Canvas.Handle, Handle, BgRect, BgRect, BgRect, BgRect2, false, Color, Color, false, Background, BackgroundType);
    end;
  end;
end;

procedure TElPageControl.RemovePage(TabSheet : TElTabSheet);
var
  NextTab : TElTabSheet;
  b, b1 : boolean;
begin
  b := hasVisibleTabs;
  if FFirstTab = TabSheet then
    FFirstTab := FindNextPage(FFirstTab, false, not (csDesigning in ComponentState), false);
  b1 := TabSheet = ActivePage;
  if b1 then
    NextTab := FindNextPage(TabSheet, True, not (csDesigning in ComponentState), false)
  else
    NextTab := nil;

  if DefaultPage = TabSheet then
    DefaultPage := nil;
  if NextTab = TabSheet then NextTab := nil;
  TabSheet.FPageControl := nil;
  FPages.Remove(TabSheet);
  {$ifdef VCL_4_USED}
  if FUndockingPage = Tabsheet then
    FUndockingPage := nil;
  {$endif}
  if FDownTab = TabSheet then
    FDownTab := nil;
  if FTrackTab = TabSheet then
    FTrackTab := nil;
  if FirstTab = TabSheet then
    FFirstTab := nil;

  if b1 then
    SetActivePage(NextTab);
  if (b <> HasVisibleTabs) or Multiline then
    RebuildTabs(false)
  else
    UpdateTabs(false);
end;

procedure TElPageControl.SetActivePage(Value: TElTabSheet);
var
{$IFNDEF VER90}
  Form: TCustomForm;
{$ELSE}
  Form: TForm;
{$ENDIF}
begin
  if FActivePage <> Value then
  begin
    if (csLoading in ComponentState) then
      FActivePage := Value
    else
    begin
      if ((Value <> nil) and (Value.PageControl <> Self)) or (not CanChange(Value)) then
        exit;
      ChangeActivePage(Value);
      if Value = nil then
        TabIndex := -1
      else
      begin
        if Value = FActivePage then
          if Value.TabIndex <> -1 then
            TabIndex := Value.TabIndex;
        MakeTabVisible(Value);
      end;
      UpdateMultilineOrder;
      UpdateTabs(false);
    end;
    if ComponentState * [csReading, csReading, csLoading, csDestroying] = [] then
    begin
      Form := GetParentForm(self);
      if not FNoDTAlert then
      {$ifndef CLX_USED}
      if (Form <> nil) and (Form.Designer <> nil) then
        Form.Designer.Modified;
      {$else}
      if (Form <> nil) and (Form.DesignerHook <> nil) then
        Form.DesignerHook.Modified;
      {$endif}
      FNoDTAlert := false;
    end;
  end;
end;

procedure TElPageControl.SetActivePageIndex(const Value: Integer);
begin
  if (Value > -1) and (Value < PageCount) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

procedure TElPageControl.SetActiveTabColor(Value: TColor);
begin
  if FActiveTabColor <> Value then
  begin
    FActiveTabColor := Value;
    if ShowTabs and (ActivePage <> nil) and (ActivePage.TabShowing) then
      UpdateTabs(false);
  end;
end;

procedure TElPageControl.SetBackground(newValue: TBitmap);
begin
  FBackground.Assign(newValue);
  if (FBackground.Empty) and (FBackGroundType in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap]) then BackgroundType := bgtColorFill;
end; {SetBackground}

procedure TElPageControl.SetBackgroundType(newValue: TElBkGndType);
begin
  if (FBackgroundType <> newValue) then
  begin
    FBackgroundType := newValue;
    if (FBackground.Empty) and (FBackGroundType in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap])
       then FBackgroundType := bgtColorFill;
    RedoTmpBmp;
    Invalidate;
    if ActivePage <> nil then
      ActivePage.Invalidate;
  end; {if}
end; {SetBackgroundType}

procedure TElPageControl.SetBorderWidth(Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    if Value < 0 then
      raise EInvalidOperation.CreateFmt('Border width must be between 0 and %d', [MaxInt]);
    FBorderWidth := Value;
    RebuildTabs(true);
  end;
end;

procedure TElPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TElTabSheet(Child).PageIndex := Order;
end;

procedure TElPageControl.SetDrawFocus(Value: Boolean);
begin
  if FDrawFocus <> Value then
  begin
    FDrawFocus := Value;
    UpdateTabs(false);
  end;
end;

procedure TElPageControl.SetFirstTab(Value: TElTabSheet);
begin
  if FFirstTab <> Value then
  begin
    FFirstTab := Value;
    if not Multiline then
      UpdateTabs(true);
  end;
end;

procedure TElPageControl.SetFlat(newValue : Boolean);
{ Sets data member FFlatButtons to newValue. }
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    UpdateTabs(false);
  end; { if }
end; { SetFlatButtons }

procedure TElPageControl.SetGradientEndColor(newValue : TColor);
{ Sets data member FGradientEndColor to newValue. }
begin
  if (FGradientEndColor <> newValue) then
  begin
    FGradientEndColor := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then
    begin
      if ActivePage <> nil then
        ActivePage.Invalidate;
    end;
  end;  { if }
end;  { SetGradientEndColor }

procedure TElPageControl.SetGradientStartColor(newValue : TColor);
{ Sets data member FGradientStartColor to newValue. }
begin
  if (FGradientStartColor <> newValue) then
  begin
    FGradientStartColor := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then
    begin
      if ActivePage <> nil then
        ActivePage.Invalidate;
    end;
  end;  { if }
end;  { SetGradientStartColor }

procedure TElPageControl.SetGradientSteps(newValue : integer);
{ Sets data member FGradientSteps to newValue. }
begin
  if (FGradientSteps <> newValue) and (newValue > 0) then
  begin
    FGradientSteps := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then
    begin
      if ActivePage <> nil then
        ActivePage.Invalidate;
    end;
  end;  { if }
end;  { SetGradientSteps }

procedure TElPageControl.SetHotTrack(newValue : Boolean);
{ Sets data member FHotTrack to newValue. }
begin
  if (FHotTrack <> newValue) then
  begin
    FHotTrack := newValue;
    UpdateTabs(false);
  end; { if }
end; { SetHotTrack }

procedure TElPageControl.SetImageForm(newValue: TElImageForm);
begin
  if (FImgForm <> newValue) then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnregisterChanges(FImgFormChLink);
    end;
    FImgForm := newValue;
    if (newValue <> nil) then
    begin
      newValue.FreeNotification(Self);
      FImgForm.RegisterChanges(FImgFormChLink);
    end;
    UpdateTabs(false);
    if not (csDesigning in ComponentState) then
    begin
      if ActivePage <> nil then
        ActivePage.Invalidate;
    end;
  end;  { if }
end;  { SetImageForm }

procedure TElPageControl.SetImages(newValue : TImageList);
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
    end;
    RebuildTabs(false);
  end; { if }
end; { SetImages }

procedure TElPageControl.SetInactiveTabColor(Value: TColor);
begin
  if FInactiveTabColor <> Value then
  begin
    FInactiveTabColor := Value;
    if ShowTabs then
      UpdateTabs(false);
  end;
end;

procedure TElPageControl.SetMinTabHeight(Value: Integer);
begin
  if FMinTabHeight <> Value then
  begin
    FMinTabHeight := Value;
    RebuildTabs(false);
  end;
end;

procedure TElPageControl.SetMinTabWidth(Value: Integer);
begin
  if FMinTabWidth <> Value then
  begin
    FMinTabWidth := Value;
    RebuildTabs(false);
  end;
end;

procedure TElPageControl.SetMultiLine(newValue : Boolean);
{ Sets data member FMultiLine to newValue. }
begin
  if (FMultiLine <> newValue) then
  begin
    if newValue and (Style = etsAngledTabs) then
      raise Exception.Create('Multiline style is not supported for angled tabs'); 
    FMultiLine := newValue;
    RebuildTabs(true);
  end; { if }
end; { SetMultiLine }

procedure TElPageControl.SetRaggedRight(const Value: Boolean);
begin
  if (FRaggedRight <> Value) then
  begin
    FRaggedRight := Value;
    if Multiline then
      RebuildTabs(true);
  end;
end;

procedure TElPageControl.SetScrollOpposite(const Value: Boolean);
begin
  if (FScrollOpposite <> Value) then
  begin
    FScrollOpposite := Value;
    if Multiline then
      UpdateTabs(false);
  end;
end;

procedure TElPageControl.SetShowBorder(Value: Boolean);
begin
  if FShowBorder <> Value then
  begin
    FShowBorder := Value;
    RebuildTabs(true);
    if IsThemeApplied and (ActivePage <> nil) and ActivePAge.HandleAllocated then
    begin
      ActivePage.Invalidate;
      SetWindowPos(ActivePage.Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_FRAMECHANGED);
      //RedrawWindow(ActivePage.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
    end;
  end;
end;

procedure TElPageControl.SetShowImages(newValue : Boolean);
{ Sets data member FShowImages to newValue. }
begin
  if (FShowImages <> newValue) then
  begin
    FShowImages := newValue;
    RebuildTabs(false);
  end; { if }
end; { SetShowImages }

procedure TElPageControl.SetShowTabs(Value: Boolean);
begin
  if FShowTabs <> Value then
  begin
    FShowTabs := Value;
    RebuildTabs(true);
  end;
end;

procedure TElPageControl.SetStyle(newValue: TElTabStyle);
{ Sets data member FButtonStyle to newValue. }
begin
  if (FStyle <> newValue) then
  begin
    FStyle := newValue;
    case FStyle of
      etsTabs: FTabs.TabClass := TElStdTab;
      etsButtons: FTabs.TabClass := TElBtnTab;
      etsFlatButtons: FTabs.TabClass := TElFlatBtnTab;
      etsNetTabs: FTabs.TabClass := TElNetTab;
      etsFlatTabs: FTabs.TabClass := TEl2DFlatTab;
      etsAngledTabs: if Multiline then
                       raise Exception.Create('Multiline style is not supported for angled tabs')
                     else
                       FTabs.TabClass := TElAngledTab;
    end;
    RebuildTabs(false);
  end; { if }
end; { SetButtonStyle }

procedure TElPageControl.SetTabHeight(newValue : DWORD);
{ Sets data member FTabHeight to newValue. }
begin
  if (FTabHeight <> newValue) then
  begin
    FTabHeight := newValue;
    RebuildTabs(false);
  end; { if }
end; { SetTabHeight }

procedure TElPageControl.SetTabIndex(const Value: integer);
var
  APage: TElTabSheet;
  i, r : integer;
begin
  if csLoading in ComponentState then
    FTabIndex := Value
  else
  if Value <> -1 then
  begin
    r := 0;
    for i := 0 to FPages.Count - 1 do
    begin
      if Pages[i].TabVisible then
      begin
        if (R = Value) then
        begin
          APage := Pages[i];
          if (APage <> nil) and (APage <> ActivePage) then
            ActivePage := APage;
          break;
        end;
        inc(r);
      end;
    end;
  end
  else
    ActivePage := nil;
end;

procedure TElPageControl.SetTabPosition(newValue : TElTabPosition);
begin
  if (FTabPosition <> newValue) then
  begin
    FTabPosition := newValue;
    RebuildTabs(true);
  end; { if }
end; { SetTabPosition }

procedure TElPageControl.SetTabWidth(newValue : DWORD);
{ Sets data member FTabWidth to newValue. }
begin
  if (FTabWidth <> newValue) then
  begin
    FTabWidth := newValue;
    RebuildTabs(false);
  end; { if }
end; { SetTabWidth }

procedure TElPageControl.ShowControl(AControl: TControl);
begin
  if (AControl is TElTabSheet) and (TElTabSheet(AControl).PageControl = Self) then
    SetActivePage(TElTabSheet(AControl));
  inherited ShowControl(AControl);
end;

procedure TElPageControl.TriggerMeasureTabEvent(Canvas : TCanvas; Page : TElTabSheet; var Size : TSize);
begin
  Size.cx := 0;
  Size.cy := 0;
  if Assigned(OnMeasureTab) then
    OnMeasureTab(Self, Canvas, Page, Size);
end;

procedure TElPageControl.UpdateActivePage;
begin
  if TabIndex >= 0 then
    SetActivePage(TElTabSheet(FPages[TabIndex]))
  else
    SetActivePage(nil);
end;

procedure TElPageControl.UpdateTab(TabSheet : TElTabSheet);
begin
  UpdateTabs(false);
end;

procedure TElPageControl.WMEraseBkGnd(var Msg : TWMEraseBkGnd);
begin
  Msg.result := 1;
end;

procedure TElPageControl.WMNCCalcSize(var Message: TWMNCCalcSize);
var ASize    : TSize;
    EdgeSize : integer;
    //R, R1    : TRect;
begin
  inherited;
  if ShowTabs then
  begin
    ASize := FTabs.CalcTabAreaSize;
    TabsSize := ASize;

    if (not IsThemeApplied) and ((ShowBorder and (Style in [etsTabs, etsNetTabs, etsFlatTabs, etsAngledTabs])) and HasVisibleTabs) then
    begin
      {
      EdgeSize := 0;
      if IsThemeApplied then
      begin
        R := Message.CalcSize_Params.rgrc[0];
        if Succeeded(GetThemeBackgroundContentRect(Theme, Canvas.Handle, TABP_PANE, 0, R, R1)) then
        begin
          case TabPosition of
            etpTop:
              EdgeSize := R1.Top - R.Top;
            etpBottom:
              EdgeSize := R.Bottom - R1.Bottom;
            etpLeft:
              EdgeSize := R1.Left - R.Left;
            etpRight:
              EdgeSize := R.Right - R1.Right;
          end;
          EdgeSize := 0;
          //EdgeSize := -EdgeSize;
        end
        else
        if TabPosition in [etpTop, etpBottom] then
          EdgeSize := GetSystemMetrics(SM_CXEDGE)
        else
          EdgeSize := GetSystemMetrics(SM_CYEDGE);
      end
      else
      }
      begin
        if TabPosition in [etpTop, etpBottom] then
          EdgeSize := GetSystemMetrics(SM_CXEDGE)
        else
          EdgeSize := GetSystemMetrics(SM_CYEDGE);
      end;
    end
    else
      EdgeSize := 0;
    case TabPosition of
      etpTop:
        begin
          ASize.cy := min(ASize.cy, Message.CalcSize_Params.rgrc[0].Bottom - Message.CalcSize_Params.rgrc[0].Top);
          inc(Message.CalcSize_Params.rgrc[0].Top, ASize.cy - EdgeSize);
          TabsPos.x := 0;
          TabsPos.y := 0;
        end;
      etpBottom:
        begin
          {if ASize.cy > 0 then
            inc(ASize.cy);
          }ASize.cy := min(ASize.cy, Message.CalcSize_Params.rgrc[0].Bottom - Message.CalcSize_Params.rgrc[0].Top);
          dec(Message.CalcSize_Params.rgrc[0].Bottom, ASize.cy - EdgeSize);
          TabsPos.x := 0;
          TabsPos.y := Height - ASize.cy;
        end;
      etpRight:
        begin
          {if ASize.cx > 0 then
            inc(ASize.cx);
          }
          ASize.cx := min(ASize.cx, Message.CalcSize_Params.rgrc[0].Right - Message.CalcSize_Params.rgrc[0].Left);
          dec(Message.CalcSize_Params.rgrc[0].Right, ASize.cx - EdgeSize);
          TabsPos.x := Width - ASize.cx;
          TabsPos.y := 0;
        end;
      etpLeft:
        begin
          ASize.cx := min(ASize.cx, Message.CalcSize_Params.rgrc[0].Right - Message.CalcSize_Params.rgrc[0].Left);
          inc(Message.CalcSize_Params.rgrc[0].Left, ASize.cx - EdgeSize);
          TabsPos.x := 0;
          TabsPos.y := 0;
        end;
    end;
  end
  else
  begin
    TabsSize.cx := 0;
    TabsSize.cy := 0;
  end;
  if (not IsThemeApplied) and ShowBorder and (Style in [etsTabs, etsNetTabs, etsFlatTabs, etsAngledTabs]) then
    InflateRect(Message.CalcSize_Params.rgrc[0], -GetSystemMetrics(SM_CXEDGE), -GetSystemMetrics(SM_CYEDGE));
  if Message.CalcSize_Params.rgrc[0].Right < Message.CalcSize_Params.rgrc[0].Left then
    Message.CalcSize_Params.rgrc[0].Right := Message.CalcSize_Params.rgrc[0].Left + 1;
  if Message.CalcSize_Params.rgrc[0].Bottom < Message.CalcSize_Params.rgrc[0].Top then
    Message.CalcSize_Params.rgrc[0].Bottom := Message.CalcSize_Params.rgrc[0].Top + 1;
  Message.Result := 0;
end;

procedure TElPageControl.WMNCPaint(var Message: TWMNCPaint);
var DC : HDC;
    R  : TRect;
    ASize : TSize;
    TmpBmp : TBitmap;
    Clp      : TRect;
    BorderSides : TElBorderSides;
begin
  R := BoundsRect;
  OffsetRect(R, -R.Left, -R.Top);
  TmpBmp := TBitmap.Create;
  TmpBmp.Width := R.Right - R.Left;
  TmpBmp.Height := R.Bottom - R.Top;

  DC := GetWindowDC(Handle);
  ASize := TabsSize;

  if ShowBorder and (Style in [etsTabs, etsNetTabs, etsFlatTabs, etsAngledTabs]) and not IsThemeApplied then
  begin
    if ShowTabs and HasVisibleTabs then
    begin
      case TabPosition of
        etpLeft: R := Rect(ASize.cx - GetSystemMetrics(SM_CXEDGE), 0, Width, ASize.cy);
        etpRight: R := Rect(0, 0, Width - ASize.cx + GetSystemMetrics(SM_CXEDGE), ASize.cy);
        etpTop: R := Rect(0, ASize.cy - GetSystemMetrics(SM_CYEDGE), Width, Height);
        etpBottom: R := Rect(0, 0, Width, Height - ASize.cy + GetSystemMetrics(SM_CYEDGE));
      end;
    end
    else
    begin
      R := BoundsRect;
      OffsetRect(R, -Left, -Top);
    end;
    BorderSides := AllBorderSides;
    if ShowTabs and HasVisibleTabs then
      case TabPosition of
        etpLeft: BorderSides := AllBorderSides - [ebsLeft];
        etpRight: BorderSides := AllBorderSides - [ebsRight];
        etpTop: BorderSides := AllBorderSides - [ebsTop];
        etpBottom: BorderSides := AllBorderSides - [ebsBottom];
      end;
    if Style = etsFlatTabs then
      DrawFlatFrameEx2(DC, R, FlatTabBorderColor, Color, false, true, BorderSides, fbtColorLineBorder)
      // DrawButtonFrameEx3(DC, R, not Flat, false, FlatTabBorderColor, Color, false, BorderSides)
    else
      DrawButtonFrameEx3(DC, R, not Flat, false, Color, false, BorderSides);
    InflateRect(R, -GetSystemMetrics(SM_CXEDGE), -GetSystemMetrics(SM_CYEDGE));
  end;

  if ShowTabs then
  begin

    case TabPosition of
      etpLeft: R := Rect(0, 0, ASize.cx, ASize.cy);
      etpRight: R := Rect(Width - ASize.cx, 0, Width, ASize.cy);
      etpTop: R := Rect(0, 0, ASize.cx, ASize.cy);
      etpBottom: R := Rect(0, Height - ASize.cy, ASize.cx, Height);
    end;
    FTabs.DrawTabs(TmpBmp.Canvas, R, true, ASize);
  end;
  (*
  if GetClipBox(DC, Clp) = NULLREGION then
  begin
    Clp := BoundsRect;
    OffsetRect(Clp, -Clp.Left, -Clp.Top);
  end
  else
  begin
    inc(Clp.Right);
    inc(Clp.Bottom);
  end;
  *)
  if ShowTabs then
  begin
    Clp := R;
    bitblt(DC, Clp.Left, Clp.Top, Clp.Right - Clp.Left, Clp.Bottom - Clp.Top, TmpBmp.Canvas.Handle, Clp.Left, Clp.Top, SRCCOPY);
  end;
  ReleaseDC(Handle, DC);
  TmpBmp.Free;
end;

procedure TElPageControl.WMSize(var Msg : TWMSize);
begin
  if not (BackgroundType in [bgtColorFill, bgtCenterBitmap]) then RedoTmpBmp;
  inherited;
{$IFNDEF VCL_4_USED}
  Resize;
{$ENDIF}
  RebuildTabs(true);
end;  { WMSize }

constructor TElPageControl.Create(AOwner : TComponent);
begin
  inherited;
  ControlStyle := [csDoubleClicks, csOpaque];
  FPages := TElList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImgFormChLink  := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  FBackground := TBitmap.Create;
  FBackground.OnChange := ImageChange;
  FBackgroundType := bgtColorFill;
  FGradientSteps := 16;
  FVerticalSideCaptions :=  true;
  FTmpBmp := TBitmap.Create;
  FActiveTabColor := clBtnFace;
  FInactiveTabColor := clBtnFace;
  Color := clBtnFace;
  FShowBorder := true;
  FTabIndex := -1;
  Width := 289;
  Height := 193;
  FMinTabWidth := 40;
  FMinTabHeight := 40;
  FShowImages := true;
  FShowTabs := true;
  FTabs := TElTabs.Create(Self);
  FTabs.TabClass := TElStdTab;
  FHotTrack := true;
  FHotTrackFont := TFont.Create;
  FHotTrackFont.OnChange := HotTrackFontChange;
  FHotTrackFont.Color := clBlue; // clHighlight;
  FActiveTabFont := TFont.Create;
  FActiveTabFont.OnChange := ActiveTabFontChange;
  FTabBkColor := clBtnFace;
  FShowTabHints := true;
  FHintTimer := TElTimer.Create;
  FHintTimer.OnTimer := OnHintTimer;
  FlatTabBorderColor := clBtnShadow;
  FDoStartDrag := -1;
  FDraggablePages := false;
  FTabCursor := crDefault;
end;

destructor TElPageControl.Destroy;
var i : integer;
begin
  FHintTimer.Free;
  if FHintWnd <> nil then
    ShowWindow(FHintWnd.Handle, SW_HIDE);
  FHintTimer := nil;
  if FScrollTimer <> nil then
    FScrollTimer.Free;
  FScrollTimer := nil;
  for i := 0 to FPages.Count -1 do
    TElTabSheet(FPages[i]).FPageControl := nil;
  FActiveTabFont.Free;
  FHotTrackFont.Free;
  FTabs.Free;
  FImageChangeLink.Free;
  FPages.Free;
  FImgFormChLink.Free;
  FBackground.Free;
  FTmpBmp.Free;
  inherited;
end;

function TElPageControl.FindNextPage(CurPage: TElTabSheet; GoForward, 
    CheckTabVisible, CheckTabEnabled : Boolean): TElTabSheet;
var
  i, StartIndex: integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then
        StartIndex := FPages.Count - 1
      else
        StartIndex := 0;
    i := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if i = FPages.Count then
          i := 0;
      end
      else
      begin
        if i = 0 then
          i := FPages.Count;
        Dec(I);
      end;
      result := FPages[i];
      if (not CheckTabVisible or result.TabVisible) and
         ((not CheckTabEnabled) or (Result.TabEnabled or (csDesigning in ComponentState))) then
        exit;
    until i = StartIndex;
  end;
  result := nil;
end;

procedure TElPageControl.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FImages) then
      Images := nil;
    if (AComponent = FImgForm) then
      ImageForm := nil;
  end;
end;

procedure TElPageControl.SelectNextPage(GoForward: Boolean);
var
  Page: TElTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, True, True);
  if (Page <> nil) and (Page <> ActivePage) and CanChange(Page) then
  begin
    TabIndex := Page.TabIndex;
  end;
end;

procedure TElPageControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  // UpdateTabs;
end;

function TElPageControl.TabFromPoint(Point : TPoint): TElTabSheet;
var i : integer;
    ASheet : TElTabSheet;
    R      : TRect;
begin
  if (ActivePage <> nil) and ActivePAge.AShown then
  begin
    R := ActivePage.ARect;
    Inc(R.Right);
    Inc(R.Bottom);
    if PtInRect(R, Point) then
    begin
      result := ActivePage;
      exit;
    end;
  end;
  for i := 0 to FPages.Count -1 do
  begin
    ASheet := TElTabSheet(FPages[i]);
    if (ASheet <> ActivePage) and (ASheet.AShown) then
    begin
      R := ASheet.ARect;
      Inc(R.Right);
      Inc(R.Bottom);
      if PtInRect(R, Point) then
      begin
        result := ASheet;
        exit;
      end;
    end;
  end;
  result := nil;
end;

procedure TElPageControl.UpdateTabs(Immediate : boolean);
var R : TRect;
    Flags : integer;
    P : PRect;
begin
  if (not HandleAllocated) or (not ShowTabs) then exit;
  R := BoundsRect; OffsetRect(R, -Left, -Top);
  Flags := rdw_Invalidate or rdw_Frame;
  if Immediate then
    Flags := Flags or RDW_ERASENOW;
  P := nil;
  case TabPosition of
    etpLeft: if Width >= TabsSize.cx then P := @R;
    etpRight: if Width >= TabsSize.cx then P := @R;
    etpTop: if Height >= TabsSize.cy then P := @R;
    etpBottom: if Height >= TabsSize.cy then P := @R;
  end;
  RedrawWindow(Handle, P, 0, Flags);
end;

function TElPageControl.CanScrollRight: Boolean;
var i : integer;
begin
  result := false;
  if Multiline or (FirstTab = nil) then
    exit
  else
  begin
    for i := FirstTab.PageIndex to FPages.Count - 1 do
    begin
      if TElTabSheet(FPages[i]).TabVisible and (not TElTabSheet(FPages[i]).AComplete) then
      begin
        result := true;
        exit;
      end;
    end;
  end;
end;

procedure TElPageControl.SetHotTrackFont(Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TElPageControl.HotTrackFontChange(Sender : TObject);
begin
  if HotTrack and (FTrackTab <> nil) then
    UpdateTabs(false);
end;

procedure TElPageControl.WMPaint(var Msg : TWMPaint);
var
  DC : HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  R : TRect;
  ARgn : HRGN;

begin
  if (Msg.DC <> 0) then
    PaintHandler(Msg)
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
        ARgn := CreateRectRgn(Left, Top, Right, Bottom);
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

procedure TElPageControl.Paint;
var
  Rect,
  R1    : TRect;
  ACtl  : TWinControl;
  ax,
  ay    : integer;
  P     : TPoint;

  function HasVisiblePages : boolean;
  var i : integer;
  begin
    result := true;
    for i := 0 to FPages.Count -1 do
    begin
      if Pages[i].Visible then
        exit;
    end;
    result := false;
  end;

begin
  if HasVisiblePages then exit;

  Rect := ClientRect;
  R1 := Canvas.ClipRect;

  if IsThemeApplied and
     Succeeded(DrawThemeBackground(Theme, Canvas.Handle, TABP_BODY, 0, Rect, @R1)) then
    Exit;

  if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
  begin
    if (FImgForm.Control <> Self) then
    begin
      ACtl := FImgForm.GetRealControl;
      R1 := Rect;
      Rect.TopLeft := ClientToScreen(Rect.TopLeft);
      P := Parent.ClientToScreen(Point(Left, Top));
      ax := Rect.Left - P.x;
      ay := Rect.Top - P.y;

      Rect.BottomRight := ClientToScreen(Rect.BottomRight);
      Rect.TopLeft := ACtl.ScreenToClient(Rect.TopLeft);
      Rect.BottomRight := ACtl.ScreenToClient(Rect.BottomRight);

      FImgForm.PaintBkgnd(Handle, R1, Point(Rect.Left - ax, Rect.Top - ay), false);
    end;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
  end;
end;

{$ifdef VCL_4_USED}
procedure TElPageControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  DockCtl: TControl;
begin
  inherited;
  DockCtl := GetDockClientFromMousePos(SmallPointToPoint(Message.Pos));
  if DockCtl <> nil then
    DockCtl.ManualDock(nil, nil, alNone);
end;
{$endif}

procedure TElPageControl.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if ShowTabs then 
  case Message.CharCode of
    VK_LEFT:
    begin
      if ActivePageIndex > 0 then
        ActivePageIndex := ActivePageIndex - 1;;
      Message.CharCode := 0;
    end;
    VK_RIGHT:
    begin
      if ActivePageIndex < PageCount - 1 then
        ActivePageIndex := ActivePageIndex + 1;
      Message.CharCode := 0;
    end;
  end;
end;

procedure TElPageControl.WMLButtonDown(var Message: TWMLButtonDown);
var
  p : TPoint;
  s : TSmallPoint;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    P := ClientToScreen(SmallPointToPoint(Message.Pos));
    S.x := P.x;
    S.y := P.y;
    Perform(WM_NCLBUTTONDOWN, HTBORDER, Integer(S));
  end;
end;

procedure TElPageControl.WMLButtonUp(var Message: TWMLButtonUp);
var
  p : TPoint;
  S : TSmallPoint;
begin
  inherited;
  if FSaveCapture <> 0 then
    SetCapture(FSaveCapture)
  else
    ReleaseCapture;
  FSaveCapture := 0;

  P := ClientToScreen(SmallPointToPoint(Message.Pos));
  S.x := P.x;
  S.y := P.y;
  Perform(WM_NCLBUTTONUP, HTBORDER, Integer(S));
end;

procedure TElPageControl.SetScrollBtnState(Value: TElPageScrollBtnState);
begin
  if FScrollBtnState <> Value then
  begin
    if (not (FScrollBtnState in [pbsLeftBtnDown, pbsRightBtnDown])) and
      (Value in [pbsLeftBtnDown, pbsRightBtnDown]) then
    begin
      FScrollTimer := TTimer.Create(nil);
      FScrollTimer.Enabled := false;
      FScrollTimer.OnTimer := OnScrollTimer;
      FScrollTimer.Interval := 250;
      FScrollTimer.Enabled := true;
    end;
    FScrollBtnState := Value;
    if not (FScrollBtnState in [pbsLeftBtnDown, pbsRightBtnDown]) then
    begin
      FScrollTimer.Free;
      FScrollTimer := nil;
    end;
    UpdateTabs(false);
  end;
end;

procedure TElPageControl.SetTrackTab(Value: TElTabSheet);
begin
  if FTrackTab <> Value then
  begin
    FTrackTab := Value;
    UpdateTabs(false);
  end;
end;

procedure TElPageControl.WMNCLButtonDown(var Message: TWMNCLBUTTONDOWN);
var
  p : TPoint;
  TabSheet : TElTabSheet;
{$IFDEF VCL_4_USED}
  DockCtl: TControl;
{$ENDIF}
begin
  if FDraggablePages then
  begin
    FDoStartDrag := 0;
    FDragTab := FActivePage;
    if FMultiLine then FDragTab := FActivePage;
  end;

  inherited;
  // FSaveCapture := SetCapture(Handle);

  p := Parent.ScreenToClient(Point(Message.XCursor, Message.YCursor));
  dec(p.x, Left);
  dec(p.y, Top);
  if PtInRect(ScrollLeftRect, p) then
  begin
    if CanScrollLeft then
    begin
      ScrollBtnState := pbsLeftBtnDown;
      FirstTab := FindNextPage(FirstTab, false, true, false);
    end;
    exit;
  end
  else
  if PtInRect(ScrollRightRect, p) then
  begin
    if CanScrollRight then
    begin
      ScrollBtnState := pbsRightBtnDown;
      FirstTab := FindNextPage(FirstTab, true, true, false);
    end;
    exit;
  end
  else
  begin
    TabSheet := TabFromPoint(P);
    if TabSheet <> nil then
    begin
      if (Style in [etsTabs, etsNetTabs, etsFlatTabs, etsAngledTabs]) then
      begin
        if ActivePage <> TabSheet then
          ActivePage := TabSheet
        else
        if CanFocus and not Focused then
          SetFocus;
      end
      else
      begin
        if (TabSheet.TabEnabled or (csDesigning in ComponentState)) then
        begin
          FDownTab := TabSheet;
          UpdateTabs(false);
        end;
      end;
    end;
  end;
  {$IFDEF VCL_4_USED}
  if (Style in [etsTabs, etsNetTabs, etsFlatTabs, etsAngledTabs]) then
  begin
    DockCtl := GetDockClientFromMousePos(P);
    if (DockCtl <> nil) then
      DockCtl.BeginDrag(False);
  end;
  {$ENDIF}

  if FDraggablePages and (not FMultiLine) then FDragTab := FActivePage;
end;


procedure TElPageControl.WMNCLButtonUp(var Message: TWMNCLBUTTONUP);
var
  p: TPoint;
  TabSheet, SwpPage: TElTabSheet;
  oldpos, newpos, i: integer;
begin
  inherited;
  if FSaveCapture <> 0 then
    SetCapture(FSaveCapture)
  else
    ReleaseCapture;
  FSaveCapture := 0;

  if (FTrackTab <> nil) and (FTrackTab = FDownTab) and (Style in [etsButtons, etsFlatButtons]) then
  begin
    if ActivePage <> FTrackTab then
      ActivePage := FTrackTab
    else
    if CanFocus and not Focused then
      SetFocus;
  end;
  FDownTab := nil;
  ScrollBtnState := pbsNone;

  if (FDraggablePages)and(FDoStartDrag = 1) then
  begin
    p.x := TWMNCMouseMove(Message).XCursor;
    p.y := TWMNCMouseMove(Message).YCursor;
    p := parent.ScreenToClient(p);
    dec(p.x, left);
    dec(p.y, top);
    TabSheet := TabFromPoint(p);
    if (TabSheet<>nil)and(TabSheet<>FActivePage) then
    begin
      oldpos := FDragTab.PageIndex;
(*
      if FMultiLine then
      begin
        FDragTab.PageIndex := TabSheet.PageIndex;
        TabSheet.PageIndex := oldpos;
        ActivePageIndex := {oldpos}FDragTab.PageIndex;
      end
      else
      begin
*)
        newpos := TabSheet.PageIndex;
        if oldpos < newpos then
        begin
          i := oldpos+1;
          while (i <= newpos) do
          begin
            SwpPage := GetPages(i);
            SwpPage.PageIndex := i-1;
            inc(i);
          end;
          dec(i);
        end else
        begin
          i := oldpos - 1;
          while (i >= newpos) do
          begin
            SwpPage := GetPages(i);
            SwpPage.PageIndex := i+1;
            dec(i);
          end;
          inc(i);
        end;

        FDragTab.PageIndex := i;
        ActivePageIndex := i;
//      end;
      RebuildTabs(true);
    end;
  end;
  FDoStartDrag := -1;
  perform(WM_SETCURSOR, 0, 0);
end;

function TElPageControl.DoHitTest(X, Y : integer; var Res : integer) : boolean;
var
  P : TPoint;
  R : TRect;
  ASheet : TElTabSheet;
begin
  R.TopLeft:= TabsPos;
  R.Right  := TabsPos.x + TabsSize.cx;
  R.Bottom := TabsPos.y + TabsSize.cy;

  p := Parent.ScreenToClient(Point(X, Y));
  dec(p.x, Left);
  dec(p.y, Top);

  result := false;

  if PtInRect(ScrollLeftRect, p) then
  begin
    if ScrollBtnState = pbsNone then
      ScrollBtnState := pbsLeftBtnOver
    else
    if ScrollBtnState = pbsLeftBtnHeld then
      ScrollBtnState := pbsLeftBtnDown
    else
    if ScrollBtnState = pbsRightBtnDown then
      ScrollBtnState := pbsRightBtnHeld
    else
    if ScrollBtnState = pbsRightBtnOver then
      ScrollBtnState := pbsNone;
    result := true;
  end
  else
  begin
    if ScrollBtnState = pbsLeftBtnDown then
      ScrollBtnState := pbsLeftBtnHeld
    else
    if ScrollBtnState = pbsLeftBtnOver then
      ScrollBtnState := pbsNone;

    if PtInRect(ScrollRightRect, p) then
    begin
      if ScrollBtnState = pbsNone then
        ScrollBtnState := pbsRightBtnOver
      else
      if ScrollBtnState = pbsRightBtnHeld then
        ScrollBtnState := pbsRightBtnDown;
      result := true;
    end
    else
    if ScrollBtnState = pbsRightBtnOver then
      ScrollBtnState := pbsNone
    else
    if ScrollBtnState = pbsRightBtnDown then
      ScrollBtnState := pbsRightBtnHeld;
  end;

  if PtInRect(R, P) then
  begin
    Res := HTBORDER;
    ASheet := TabFromPoint(p);
    if (ASheet <> nil) and ASheet.TabEnabled or (csDesigning in ComponentState) then
    begin
      TrackTab := ASheet;
      exit;
    end;
  end;
  TrackTab := nil;
  (*if (ScrollBtnState in [pbsLeftBtnOver, pbsRightBtnOver]) then
    ScrollBtnState := pbsNone;*)
end;

procedure TElPageControl.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    DoHitTest(Message.XPos, Message.YPos, Message.Result);
end;

procedure TElPageControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if TrackTab <> nil then
  begin
    TrackTab := nil;
    UpdateTabs(false);
  end;
  if (ScrollBtnState in [pbsLeftBtnOver, pbsRightBtnOver]) then
    ScrollBtnState := pbsNone;
  if FHintWnd <> nil then
    ShowWindow(FhintWnd.Handle, SW_HIDE);
  FHintTimer.Enabled := false;
end;

procedure TElPageControl.OnScrollTimer(Sender : TObject);
begin
  if (ScrollBtnState = pbsLeftBtnDown) and (CanScrollLeft) then
    FirstTab := FindNextPage(FirstTab, false, true, false)
  else
  if (ScrollBtnState = pbsRightBtnDown) and (CanScrollRight) then
    FirstTab := FindNextPage(FirstTab, true, true, false)
  {else
    FScrollTimer.Enabled := false
  }
  ;
end;

procedure TElPageControl.SetTabBkColor(Value: TColor);
begin
  if FTabBkColor <> Value then
  begin
    FTabBkColor := Value;
    UpdateTabs(false);
  end;
end;

function TElPageControl.HasVisibleTabs: Boolean;
var i : integer;
begin
  result := false;
  for i := 0 to FPages.Count -1 do
  begin
    if TElTabSheet(FPages[i]).TabVisible then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TElPageControl.NewPage: TElTabSheet;
begin
  Result := TElTabSheet.Create(Self);
  Result.PageControl := Self;
end;

procedure TElPageControl.Loaded;
begin
  inherited;
  FNoDTAlert := true;
  UpdateTabs(false);
  if ActivePage <> nil then
    ActivePage.Visible := true;
  TabBkColorNetStyle := IncColor(ColorToRGB(Color), 20, 20, 20);
  if (DefaultPage <> nil) and (DefaultPage.PageControl = Self)
     and (DefaultPage.Visible) then
    ActivePage := DefaultPage;
end;

procedure TElPageControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

procedure TElPageControl.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  PostMessage(Handle, PM_REFRESHACTIVEPAGE, 0, 0);
  // if Multiline then
end;

procedure TElPageControl.Resize;
begin
{$IFNDEF VCL_4_USED}
  if Assigned(FOnResize) then FOnResize(Self);
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TElPageControl.UpdateMultilineOrder;
var i : integer;
begin
  if MultiLine and (ActivePage <> nil) and (Style in [etsTabs, etsNetTabs, etsFlatTabs, etsAngledTabs]) then
  begin
    if (ActivePage.ALine < ALines - 1) then
    begin
      for i := 0 to FPages.Count - 1 do
      begin
        if TElTabSheet(FPages[i]).ALine = ActivePage.ALine + 1 then
        begin
          FirstTab := TElTabSheet(FPages[i]);
          break;
        end;
      end;
    end;
  end;
end;

procedure TElPageControl.TriggerGetImageEvent(PageIndex: integer; var 
    ImageIndex: integer);
begin
  if assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, PageIndex, ImageIndex);
end;

procedure TElPageControl.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  if ShowTabs then
    UpdateTabs(false);
  Broadcast(Message);
end;

procedure TElPageControl.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;

procedure TElPageControl.IFMEffectiveSize(var Message: TMessage);
begin
  inherited;
  if Message.lParam <> 0 then
  begin
    PRect(Message.lParam)^ := BoundsRect;
    OffsetRect(PRect(Message.lParam)^, -Left, -Top);
  end;
end;

procedure TElPageControl.IFMCanPaintBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TElPageControl.PMRefreshActivePage(var Message: TMessage);
var ASheet : TElTabSheet;
begin
  ASheet := ActivePage;
  if ASheet <> nil then
  begin
    //Msg.Msg := WM_NCPAINT;
    //WMNCPaint(Msg);
    FActivePage := nil;
    ActivePage := ASheet;
  end;
end;

procedure TElPageControl.WMNCRButtonUp(var Message: TWMNCRBUTTONUP);
var
  p : TPoint;
  TabSheet : TElTabSheet;
  Menu     : TPopupMenu;
begin
  inherited;
  if FSaveCapture <> 0 then
    SetCapture(FSaveCapture)
  else
    ReleaseCapture;
  FSaveCapture := 0;
  p := Parent.ScreenToClient(Point(Message.XCursor, Message.YCursor));
  dec(p.x, Left);
  dec(p.y, Top);
  if (not PtInRect(ScrollRightRect, p)) and (not PtInRect(ScrollLeftRect, p)) then
  begin
    Menu := nil;
    TabSheet := TabFromPoint(P);
    if (TabSheet <> nil) then
    begin
      Menu := TabSheet.TabMenu;
      if Menu = nil then
        Menu := TabSheet.PopupMenu;
      if Menu <> nil then
        Menu.PopupComponent := TabSheet;
    end;
    if Menu = nil then
    begin
      Menu := PopupMenu;
      if Menu <> nil then
        Menu.PopupComponent := Self;
    end;
    if Menu <> nil then
    begin
      Inc(P.x, Left);
      inc(p.y, Top);
      P := Parent.ClientToScreen(p);
      Menu.Popup(p.X, p.Y);
    end;
  end;
end;

procedure TElPageControl.SetDraggablePages(Value: boolean);
begin
  FDraggablePages := Value;
  FDoStartDrag := -1;
end;

constructor TElTabs.Create(PageControl : TElPageControl);
begin
  inherited Create;
  FPageControl := PageControl;
end;

function TElTabs.CalcTabAreaSize: TSize;
var ADC   : HDC;
    R     : TRect;
    Canvas: TCanvas;
begin
  if FPageControl.ShowTabs then
  begin
    ADC := GetWindowDC(FPageControl.Handle);
    R := FPageControl.BoundsRect;
    OffsetRect(R, -FPageControl.Left, -FPageControl.Top);
    Result.cx := 0;
    Result.cy := 0;
    Canvas := TCanvas.Create;
    Canvas.Handle := ADC;
    DrawTabs(Canvas, R, false, Result);
    Canvas.Handle := 0;
    Canvas.Free;
    ReleaseDC(FPageControl.Handle, ADC);
  end
  else
  begin
    Result.cx := 0;
    Result.cy := 0;
  end;
end;

procedure TElTabs.DrawTabs(ACanvas : TCanvas; R : TRect; DoDraw : boolean; var Size : 
    TSize);
var i : integer;
    ASheet : TElTabSheet;
    BgRect : TRect;
    ACtl : TWinControl;

begin
  if DoDraw then
    if not IsThemeApplied and (FPageControl.FImgForm <> nil) and (not (csDesigning in FPageControl.ComponentState)) then
    begin
      if (FPageControl.FImgForm.Control <> FPageControl) then
      begin
        ACtl := FPageControl.FImgForm.GetRealControl;

        BgRect.Left := FPageControl.Left;
        BgRect.Top :=  FPageControl.Top;

        BgRect.TopLeft := FPageControl.Parent.ClientToScreen(BgRect.TopLeft);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
      end
      else
        BgRect.TopLeft := R.TopLeft;

      FPageControl.FImgForm.PaintBkgnd(ACanvas.Handle, R, BgRect.TopLeft, false);
    end
    else
      FTab.FillSpace(ACanvas, R);

  FPageControl.ALines := 0;
  for i := 0 to FPageControl.FPages.Count - 1 do
  begin
    ASheet := TElTabSheet(FPageControl.FPages[i]);
    SetRectEmpty(ASheet.ARect);
    ASheet.AComplete := false;
    ASheet.ALine := -1;
    ASheet.AShown := false;
  end;
  DoDrawTabs(ACanvas, R, DoDraw, Size);
end;

function TElTabs.MeasureSheet(ACanvas : TCanvas; Sheet : TElTabSheet): TSize;
var AFont : HFont;
    R,
    R1    : TRect;
    t, i  : integer;
    {$ifdef MSWINDOWS}
    {$IFDEF VCL_4_USED}
    TM    : tagTextMetricA;
    {$ELSE}
    TM    : TTextMetricA;
    {$ENDIF}
    {$else}
    Metrics : QFontMetricsH;
    {$endif}
    pid,
    sid     : integer;
    ATheme  : HTheme;
begin
  with FPageControl do
  begin
    TriggerMeasureTabEvent(ACanvas, Sheet, result);
    if (Result.cx = 0) and (Result.cy = 0) then
    begin
      if (FPageControl.FActivePage = Sheet) and (UseActiveTabFont) then
        ACanvas.Font.Assign(FPageControl.ActiveTabFont)
      else
      if FPageControl.HotTrack and (FPageControl.FTrackTab = Sheet) then
        ACanvas.Font.Assign(FPageControl.HotTrackFont)
      else
        ACanvas.Font.Assign(FPageControl.Font);
        
      case TabPosition of
        etpLeft:
          AFont := GetRotatedFont(ACanvas, 900);
        etpRight:
          AFont := GetRotatedFont(ACanvas, -900);
        else
          AFont := GetRotatedFont(ACanvas, 0);
      end;
      ACanvas.Font.Handle := AFont;
      R.Left := 0;
      R.Top := 0;
      R.Right := 0;
      R.Bottom := 0;
      if Length(Sheet.Caption) = 0 then
      begin
        {$ifdef MSWINDOWS}
        {$ifdef ELPACK_UNICODE}
        GetTextMetrics(ACanvas.Handle, TM);
        {$else}
        GetTextMetrics(ACanvas.Handle, TM);
        {$endif}
        t := Abs(TM.tmHeight);
        {$else}
        Metrics := QFontMetrics_create(ACanvas.Font.Handle);
        t := QFontMetrics_height(Metrics);
        QFontMetrics_destroy(Metrics);
        {$endif}
        if (TabPosition in [etpLeft, etpRight]) and VerticalSideCaptions then
        begin
          Result.cx := t;
          Result.cy := Max(10, MinTabHeight);
        end
        else
        begin
          Result.cy := t;
          Result.cx := Max(10, MinTabWidth);
        end;
        if (FImages <> nil) and ShowImages then
          case TabPosition of
            etpTop,
            etpBottom:
              begin
                Inc(Result.cx, FImages.Width + Margin);
                if (Result.cy < FImages.Height) then
                   Result.cy := FImages.Height;
              end;
            etpLeft,
            etpRight:
              begin
                if VerticalSideCaptions then
                begin
                  Inc(Result.cy, FImages.Height + Margin);
                  if (Result.cx < FImages.Width) then
                     Result.cx := FImages.Width;
                end
                else
                begin
                  Inc(Result.cx, FImages.Width + Margin);
                  if (Result.cy < FImages.Height) then
                     Result.cy := FImages.Height;
                end;
              end;
          end;
      end
      else
      with ACanvas do
      begin
        if IsThemeApplied then
        begin
          if (FTab is TElStdTab) or (FTab is TElNetTab) or (FTab is TEl2DFlatTab) then
          begin
            pid := TABP_TABITEM;
            if not Sheet.Enabled then
              sid := TIS_DISABLED
            else
            if Sheet = FPageControl.FActivePage then
            begin
              pid := TABP_TOPTABITEM;
              sid := TTIS_SELECTED;
            end
            else
            begin
              pid := TABP_TABITEM;
              if Sheet = FPageControl.FTrackTab then
              begin
                sid := TIS_HOT;
              end
              else
              begin
                if Sheet = FPageControl.FDownTab then
                begin
                  sid := TIS_FOCUSED;
                end
                else
                begin
                  sid := TIS_NORMAL;
                end;
              end;
            end;
            //pid := TABP_TOPTABITEM;
            //sid := TTIS_SELECTED;
          end
          else
          begin
            pid := BP_PUSHBUTTON;
            if Sheet.Enabled then
            begin
              if Sheet = FPageControl.FActivePage then
              begin
                sid := PBS_PRESSED;
              end
              else
              begin
                if Sheet = FPageControl.FTrackTab then
                begin
                  sid := PBS_HOT;
                end
                else
                begin
                  if Sheet = FPageControl.FDownTab then
                  begin
                    sid := PBS_PRESSED;
                  end
                  else
                  begin
                    sid := PBS_NORMAL;
                  end;
                end;
              end;
            end
            else
            begin
              sid := PBS_DISABLED;
            end;
          end;
          if (FTab is TElStdTab) or (FTab is TElNetTab) or (FTab is TEl2DFlatTab) then
            ATheme := TabTheme
          else
            ATheme := BtnTheme;
          GetThemeTextExtent(ATheme, Handle, pid, sid, PWideChar(WideString(Sheet.Caption)), Length(WideString(Sheet.Caption)), DT_LEFT, nil, R);
          Result.cx := R.Right - R.Left;
          Result.cy := R.Bottom - R.Top;
          if (TabPosition in [etpLeft, etpRight]) and VerticalSideCaptions then
          begin
            i := Result.cx;
            Result.cx := Result.cy;
            Result.cy := i;
          end;
          if (FImages <> nil) and ShowImages then
          case TabPosition of
            etpTop,
            etpBottom:
              begin
                Inc(Result.cx, FImages.Width + Margin);
                if (Result.cy < FImages.Height) then
                   Result.cy := FImages.Height;
              end;
            etpLeft,
            etpRight:
              begin
                if VerticalSideCaptions then
                begin
                  Inc(Result.cy, FImages.Height + Margin);
                  if (Result.cx < FImages.Width) then
                     Result.cx := FImages.Width;
                end
                else
                begin
                  Inc(Result.cx, FImages.Width + Margin);
                  if (Result.cy < FImages.Height) then
                     Result.cy := FImages.Height;
                end;
              end;
          end;
          if (not (FTab is TElFlatBtnTab)) or (sid <> PBS_NORMAL) then
          begin
            // GetThemeBackgroundExtent(ATheme, Handle, pid, sid, @R, R1);
            GetThemeBackgroundContentRect(ATheme, Handle, pid, sid, R, R1);
            if (TabPosition in [etpLeft, etpRight]) and VerticalSideCaptions then
            begin
              inc(Result.cy, (R1.Left - R.Left) + (R.Right - R1.Right));
              inc(Result.cx, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
            end
            else
            begin
              inc(Result.cx, (R1.Left - R.Left) + (R.Right - R1.Right));
              inc(Result.cy, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
            end;
          end;
        end
        else
        begin
          {$ifdef MSWINDOWS}
          {$ifdef ELPACK_UNICODE}
          GetTextExtentPoint32W(Handle, PWideChar(Sheet.Caption), Length(Sheet.Caption), Result);
          {$else}
          GetTextExtentPoint32(Handle, PChar(Sheet.Caption), Length(Sheet.Caption), Result);
          {$endif}
          if (TabPosition in [etpLeft, etpRight]) and VerticalSideCaptions then
          begin
            i := Result.cx;
            Result.cx := Result.cy;
            Result.cy := i;
          end;
          {$else}
          R2.Left := 0; R2.Right := MaxInt;
          R2.Top  := 0; R2.Bottom := MaxInt;
          QPainter_boundingRect(TmpCanvas.Handle, @R, @R2, Flags, PWideString(@Sheet.Caption), Length(Sheet.Caption), nil);
          Result.cx := R.Right - R.Left;
          Result.cy := R.Bottom - R.Top;
          {$endif}
          if (FImages <> nil) and ShowImages then
          case TabPosition of
            etpTop,
            etpBottom:
              begin
                Inc(Result.cx, FImages.Width + Margin);
                if (Result.cy < FImages.Height) then
                   Result.cy := FImages.Height;
              end;
            etpLeft,
            etpRight:
              begin
                if VerticalSideCaptions then
                begin
                  Inc(Result.cy, FImages.Height + Margin);
                  if (Result.cx < FImages.Width) then
                     Result.cx := FImages.Width;
                end
                else
                begin
                  Inc(Result.cx, FImages.Width + Margin);
                  if (Result.cy < FImages.Height) then
                     Result.cy := FImages.Height;
                end;
              end;
          end;
        end;
      end;
      case TabPosition of
        etpTop,
        etpBottom:
          begin
            Inc(Result.cx, Margin * 2 + Margin);
            Inc(Result.cy, Margin);
          end;
        etpLeft,
        etpRight:
          begin
            if FPageControl.VerticalSideCaptions then
            begin
              Inc(Result.cy, Margin * 2 + Margin);
              Inc(Result.cx, Margin);
            end
            else
            begin
              Inc(Result.cx, Margin * 2 + Margin);
              Inc(Result.cy, Margin);
            end;
          end;
      end;

      FTab.AdjustTabSize(Result);

      if TabWidth > 0 then
        Result.cx := TabWidth;
      if TabHeight > 0 then
        Result.cy := TabHeight;
    end;
  end;
end;

procedure TElTabs.SetTabClass(Value: TElTabClass);
begin
  if FTabClass <> Value then
  begin
    if FTab <> nil then
      FTab.Free;
    FTabClass := Value;
    FTab := FTabClass.Create(Self);
  end;
end;

function TElTabs.GetRotatedFont(Canvas : TCanvas; RotationAngle : integer):
    HFont;
var
  LogFont: TLogFont;
begin
  GetObject(Canvas.Font.Handle, SizeOf(LogFont), @LogFont);
  with LogFont do
  begin
    if (RotationAngle <> lfEscapement) and (FPageControl.FVerticalSideCaptions) then
    begin
      if RotationAngle = 0 then
        lfOutPrecision := OUT_DEFAULT_PRECIS
      else
        lfOutPrecision := OUT_TT_ONLY_PRECIS;
      lfEscapement := RotationAngle;
      lfOrientation := lfEscapement;
    end;
  end;
  Result := CreateFontIndirect(LogFont);
end;

destructor TElTabs.Destroy;
begin
  FTab.Free;
  inherited;
end;

procedure TElTabs.DoDrawTabs(ACanvas : TCanvas; Rect : TRect; DoDraw : boolean; 
    var Size : TSize);
var i, j : integer;
    SheetSize : TSize;
    sx, sy,
    cx, cy : integer;
    mx, my : integer;
    tx, ty : integer;
    TR     : TRect;
    First,
    sf     : boolean;
    curMargin  : integer;
    ASheet,
    ASheet2: TElTabSheet;
    PageControl : TElPageControl;
    csl,
    csr        : boolean;
    ScrollRect : TRect;
    bmp        : TBitmap;
    brfl,
    dap        : boolean;
    curRow     : integer;
    b          : boolean;
begin
  PageControl := FPageControl;
  with PageControl do
    if FirstTab <> nil then
      j := FPages.IndexOf(FirstTab)
    else
      j := 0;
  sx := 0;
  sy := 0;

  if PageControl.Multiline then
  begin
    // Multiline drawing
    SetRectEmpty(FPageControl.ScrollLeftRect);
    SetRectEmpty(FPageControl.ScrollRightRect);

    mx := 0;
    my := 0;
    tx := 0;
    ty := 0;
    curRow := 0;

    // first determine where the tabs will be located
    i := j;
    sf := true;
    First := true;
    brfl := false;
    if (PageControl.FPages.Count > 0) then
    repeat
      ASheet := PageControl.FPages[i];
      if (not sf) and (i = j) then break;
      sf := false;
      if ASheet.TabVisible then
      begin
        SheetSize := MeasureSheet(ACanvas, ASheet);
        if DoDraw then
          ASheet.ARect.BottomRight := TPoint(SheetSize);

        case PageControl.TabPosition of
          etpTop,
          etpBottom:
            begin
              if not First then
                CurMargin := FTab.GetInnerMargin
              else
                CurMargin := 0;
              if brfl or (tx + SheetSize.cx + CurMargin > PageControl.Width) then
              begin
                // go to new row
                inc(ty, my + FTab.GetRowMargin);
                mx := 0;
                inc(CurRow);
                if (tx + SheetSize.cx + CurMargin > PageControl.Width) then
                begin
                  if First then
                  begin
                    SheetSize.cx := PageControl.Width - FTab.GetOuterMargin  * 2;
                    ASheet.ARect.Right := PageControl.Width - FTab.GetOuterMargin * 2;
                  end;
                end;
                tx := FTab.GetOuterMargin;
                First := true;
              end
              else
              begin
                First := false;
              end;
              tx := tx + SheetSize.cx + CurMargin;
              my := Max(my, SheetSize.cy);
              if DoDraw then
                ASheet.ALine := CurRow;
            end;
          etpLeft,
          etpRight:
            begin
              if not First then
                CurMargin := FTab.GetInnerMargin
              else
                CurMargin := 0;
              if brfl or (ty + SheetSize.cy + CurMargin > PageControl.Height) then
              begin
                // go to new row
                inc(tx, mx + FTab.GetRowMargin);
                my := 0;
                inc(CurRow);
                if (ty + SheetSize.cy + CurMargin > PageControl.Height) then
                begin
                  if First then
                  begin
                    SheetSize.cy := PageControl.Height - FTab.GetOuterMargin * 2;
                    ASheet.ARect.Bottom := PageControl.Height - FTab.GetOuterMargin * 2;
                  end;
                end;
                ty := FTab.GetOuterMargin;
                First := true;
              end
              else
              begin
                First := false;
              end;
              ty := ty + SheetSize.cy + FTab.GetInnerMargin;
              mx := Max(mx, SheetSize.cx);
              if DoDraw then
                ASheet.ALine := CurRow;
            end;
        end;
        brfl := false;
      end;

      // increment counter
      inc(i);
      if i = PageControl.FPages.Count then
      begin
        i := 0;
        brfl := true;
      end;
    until false;

    if (PageControl.FPages.Count > 0) then
    begin
      case PageControl.TabPosition of
        etpTop,
        etpBottom:
          begin
            inc(ty, FTab.GetAscend + my);
            if DoDraw then
              for i := 0 to PageControl.FPages.Count - 1 do
              begin
                ASheet := PageControl.FPages[i];
                if ASheet.TabVisible then
                  ASheet.ARect.Bottom := my;
              end;
          end;
        etpLeft,
        etpRight:
          begin
            inc(tx, FTab.GetAscend + mx);
            if DoDraw then
              for i := 0 to PageControl.FPages.Count - 1 do
              begin
                ASheet := PageControl.FPages[i];
                if ASheet.TabVisible then
                  ASheet.ARect.Right := mx;
              end;
          end;
      end;
    end;

    // if we do not need painting, just return the size
    if not DoDraw then
    begin
      with PageControl do
        case TabPosition of
          etpTop,
          etpBottom:
            begin
              Size.cx := Width;
              Size.cy := ty;
            end;
          etpLeft,
          etpRight:
            begin
              Size.cy := Height;
              Size.cx := tx;
            end;
        end;
      exit;
    end;

    PageControl.ALines := CurRow + 1;

    if (PageControl.FPages.Count > 0) then
      if not PageControl.RaggedRight then
        ReorderPages(CurRow + 1);

    for i := 0 to PageControl.FPages.Count - 1 do
    begin
      if TElTabSheet(PageControl.FPages[i]).TabVisible then
      begin
        if PageControl.TabPosition in [etpLeft, etpRight] then
        begin
          TElTabSheet(PageControl.FPages[i]).ARect.Right := TElTabSheet(PageControl.FPages[i]).ARect.Left + mx;
        end
        else
        begin
          TElTabSheet(PageControl.FPages[i]).ARect.Bottom := TElTabSheet(PageControl.FPages[i]).ARect.Top + my;
        end;
      end;
    end;

    ASheet2:= nil;

    // define "reference points" that are used during painting
    with PageControl do
      case TabPosition of
        etpTop:
          begin
            sx := 0;
            sy := 0;
            if (PageControl.FPages.Count > 0) then
              inc(sy, FTab.GetAscend);
          end;
        etpBottom:
          begin
            sx := 0;
            sy := Height - my;
            if (PageControl.FPages.Count > 0) then
              dec(sy, FTab.GetAscend);
          end;
        etpLeft:
          begin
            sx := 0;
            sy := 0;
            //OLD sy := Height - 1;

            if (PageControl.FPages.Count > 0) then
              inc(sx, FTab.GetAscend);
          end;
        etpRight:
          begin
            sx := Width - mx;
            sy := 0;
            if (PageControl.FPages.Count > 0) then
              dec(sx, FTab.GetAscend);
          end;
      end;

    cx := sx;
    cy := sy;

    // perform drawing
    i := j;
    sf := true;
    CurRow := -1;
    if (PageControl.FPages.Count > 0) then
    repeat
      ASheet := PageControl.FPages[i];
      if (not sf) and (i = j) then break;
      sf := false;
      // only draw visible tabs
      if ASheet.TabVisible then
      begin
        // if new row is started,
        if ASheet.ALine <> CurRow then
        begin
          // set start coords for the new page
          // First := true;
          // ASheet2 := nil;
          if CurRow >= 0 then
            case PageControl.TabPosition of
              etpTop:
                begin
                  cx := sx;
                  cy := cy + my + FTab.GetRowMargin;
                end;
              etpBottom:
                begin
                  cx := sx;
                  cy := cy - my - FTab.GetRowMargin;
                end;
              etpLeft:
                begin
                  cy := sy;
                  cx := cx + mx + FTab.GetRowMargin;
                end;
              etpRight:
                begin
                  cy := sy;
                  cx := cx - mx - FTab.GetRowMargin;
                end;
            end;
          CurRow := ASheet.ALine;

          case PageControl.TabPosition of
            etpTop,
            etpBottom:
              inc(cx, FTab.GetOuterMargin);
            etpLeft:
              //OLD dec(cy, FTab.GetOuterMargin);
              inc(cy, FTab.GetOuterMargin);
            etpRight:
              inc(cy, FTab.GetOuterMargin);
          end
        end
        else
        begin
          case PageControl.TabPosition of
            etpTop,
            etpBottom:
              inc(cx, ASheet2.ARect.Right - ASheet2.ARect.Left);
            etpLeft:
              //OLD dec(cy, ASheet2.ARect.Bottom - ASheet2.ARect.Top);
              inc(cy, ASheet2.ARect.Bottom - ASheet2.ARect.Top);
            etpRight:
              inc(cy, ASheet2.ARect.Bottom - ASheet2.ARect.Top);
          end;
          FTab.DrawSpace(ACanvas, cx, cy, mx, my);
        end;

        //if FPageControl.TabPosition <> etpLeft then
        begin
          // define size for the tab
          TR.Left := cx;
          TR.Top := cy;
          TR.Right := cx + ASheet.ARect.Right;
          TR.Bottom := cy + ASheet.ARect.Bottom;
        {end
        else
        begin
          TR.Left  := cx;
          TR.Top   := cy - ASheet.ARect.Bottom - 1;
          TR.Right := cx + ASheet.ARect.Right;
          TR.Bottom:= cy;
        }
        end;

        ASheet.ARect := TR;
        FTab.AdjustDrawingSize(false, TR);
        ASheet.AComplete := true;

        if ((ASheet <> PageControl.ActivePage) or FTab.CanDrawTab(false)) then
          FTab.Draw(ACanvas, TR, ASheet);

        ASheet2 := ASheet;
        // First := false;
      end;

      // increment counter
      inc(i);
      if i = PageControl.FPages.Count then
        i := 0;
    until false;

    ASheet := PageControl.ActivePage;
    if (ASheet <> nil) and FTab.CanDrawTab(true) {and ASheet.FTabShowing} then
    begin
      TR := ASheet.ARect;
      FTab.AdjustDrawingSize(true, TR);
      ASheet.AComplete := true;
      FTab.Draw(ACanvas, TR, ASheet);
    end;

    FTab.DrawTabLine(ACanvas, Rect);
  end
  else
  begin
    mx := 0;
    my := 0;
    tx := 0;
    ty := 0;
    
    // measure tabs
    for i := 0 to PageControl.FPages.Count - 1 do
    begin
      ASheet := TElTabSheet(PageControl.FPages[i]);
      if (not ASheet.TabVisible) then
        Continue;
      SheetSize := MeasureSheet(ACanvas, ASheet);

      mx := Max(mx, SheetSize.cx);
      my := Max(my, SheetSize.cy);
      // calculate total height/width
      case PageControl.TabPosition of
        etpTop,
        etpBottom:
            tx := tx + SheetSize.cx + FTab.GetInnerMargin;
        etpLeft,
        etpRight:
            ty := ty + SheetSize.cy + FTab.GetInnerMargin;
      end;
      // possibly store the size of the sheet for further references
      if DoDraw then
        ASheet.ARect.BottomRight := TPoint(SheetSize);
    end;

    if (PageControl.FPages.Count > 0) then
    begin
      case PageControl.TabPosition of
        etpTop,
        etpBottom:
          begin
            if DoDraw then
              for i := 0 to PageControl.FPages.Count - 1 do
              begin
                ASheet := PageControl.FPages[i];
                if ASheet.TabVisible then
                  ASheet.ARect.Bottom := my;
              end;
          end;
        etpLeft,
        etpRight:
          begin
            if DoDraw then
              for i := 0 to PageControl.FPages.Count - 1 do
              begin
                ASheet := PageControl.FPages[i];
                if ASheet.TabVisible then
                  ASheet.ARect.Right := mx;
              end;
          end;
      end;
    end;

    // if drawing was not required, we just return the size of the tabs area
    if not DoDraw then
    begin
      with PageControl do
        case TabPosition of
          etpTop,
          etpBottom:
            begin
              Size.cx := Width;
              Size.cy := my;
              if ShowTabs and HasVisibleTabs then
                inc(Size.cy, FTab.GetAscend);
            end;
          etpLeft,
          etpRight:
            begin
              Size.cy := Height;
              Size.cx := mx;
              if ShowTabs and HasVisibleTabs then
                inc(Size.cx, FTab.GetAscend);
            end;
        end;
      exit;
    end;
    // add side margins
    if PageControl.TabPosition in [etpLeft, etpRight] then
      inc(ty, FTab.GetOuterMargin * 2)
    else
      inc(tx, FTab.GetOuterMargin * 2);
    
    // now we define max bounds for the tabs
    with PageControl do
      case TabPosition of
        etpTop,
        etpBottom:
            mx := Width;
        etpLeft,
        etpRight:
          my := Height;
      end;

    with PageControl do
      case TabPosition of
        etpTop:
          begin
            sx := 0;
            sy := 0;
            {
            if (PageControl.FPages.Count > 0) then
              inc(sy, FTab.GetAscend);
            }
          end;
        etpBottom:
          begin
            sx := 0;
            sy := Height - my;
            if (PageControl.FPages.Count > 0) then
              dec(sy, FTab.GetAscend);
          end;
        etpLeft:
          begin
            sx := 0;
            sy := 0;
            {
            if (PageControl.FPages.Count > 0) then
              inc(sx, FTab.GetAscend);
            }
          end;
        etpRight:
          begin
            sx := Width - mx;
            sy := 0;
            if (PageControl.FPages.Count > 0) then
              dec(sx, FTab.GetAscend);
          end;
      end;

    cx := sx;
    cy := sy;

    if (PageControl.FPages.Count > 0) then
    begin
      case PageControl.TabPosition of
        etpTop:
          inc(cy, FTab.GetAscend);
        etpLeft:
          inc(cx, FTab.GetAscend);
      end;
    end;

    ASheet2:= nil;
    // ASheet := nil;
    First  := true;
    dap    := false;

    // copy background to be drawn for arrows
    case PageControl.TabPosition of
      etpTop,
      etpBottom:
        begin
          PageControl.ScrollLeftRect :=
            Classes.Rect(Rect.Right - 2 * GetSystemMetrics(SM_CXHSCROLL), Rect.Top + Margin,
                         Rect.Right - GetSystemMetrics(SM_CXHSCROLL), Rect.Top + Margin + GetSystemMetrics(SM_CYHSCROLL));
          PageControl.ScrollRightRect :=
            Classes.Rect(Rect.Right - GetSystemMetrics(SM_CXHSCROLL), Rect.Top + Margin,
                         Rect.Right, Rect.Top + Margin + GetSystemMetrics(SM_CYHSCROLL));
          ScrollRect :=
            Classes.Rect(Rect.Right - 2 * GetSystemMetrics(SM_CXHSCROLL), Rect.Top + Margin,
                         Rect.Right, Rect.Top + Margin + GetSystemMetrics(SM_CYHSCROLL));
        end;
      etpLeft:
        begin
          FPageControl.ScrollRightRect :=
            Classes.Rect(Rect.Left + Margin, Rect.Bottom - 2 * GetSystemMetrics(SM_CYVSCROLL),
                         Rect.Left + Margin + GetSystemMetrics(SM_CXVSCROLL), Rect.Bottom - GetSystemMetrics(SM_CYVSCROLL));
          FPageControl.ScrollLeftRect :=
            Classes.Rect(Rect.Left + Margin, Rect.Bottom - GetSystemMetrics(SM_CYVSCROLL),
                         Rect.Left + Margin + GetSystemMetrics(SM_CXVSCROLL), Rect.Bottom);
          ScrollRect :=
            Classes.Rect(Rect.Left + Margin, Rect.Bottom - 2 * GetSystemMetrics(SM_CYVSCROLL),
                         Rect.Left + Margin + GetSystemMetrics(SM_CXVSCROLL), Rect.Bottom);
        end;
      etpRight:
        begin
          FPageControl.ScrollLeftRect :=
            Classes.Rect(Rect.Left + Margin, Rect.Bottom - 2 * GetSystemMetrics(SM_CYVSCROLL),
                         Rect.Left + Margin + GetSystemMetrics(SM_CXVSCROLL), Rect.Bottom - GetSystemMetrics(SM_CYVSCROLL));
          FPageControl.ScrollRightRect :=
            Classes.Rect(Rect.Left + Margin, Rect.Bottom - GetSystemMetrics(SM_CYVSCROLL),
                         Rect.Left + Margin + GetSystemMetrics(SM_CXVSCROLL), Rect.Bottom);
          ScrollRect :=
            Classes.Rect(Rect.Left + Margin, Rect.Bottom - 2 * GetSystemMetrics(SM_CYVSCROLL),
                         Rect.Left + Margin + GetSystemMetrics(SM_CXVSCROLL), Rect.Bottom);
        end;
    end;
    Bmp := TBitmap.Create;
    Bmp.Width := ScrollRect.Right - ScrollRect.Left;
    Bmp.Height := ScrollRect.Bottom - ScrollRect.Top;
    Bmp.Canvas.CopyRect(Classes.Rect(0, 0, Bmp.Width, Bmp.Height), ACanvas, ScrollRect);

    ASheet := nil;

    // draw tabs one by one
    for i := j to PageControl.FPages.Count - 1 do
    begin
      if (not TElTabSheet(PageControl.FPages[i]).TabVisible) then Continue;
      ASheet := TElTabSheet(PageControl.FPages[i]);

      (*
      sx := cx;
      sy := cy;
      *)
      case PageControl.TabPosition of
        etpTop,
        etpBottom:
          if cx > mx then break;
        etpLeft:
          //OLD if cy < 0 then break;
          if cy > my then break;
        etpRight:
          if cy > my then break;
      end;
      // for tabs make an offset for the first tab
      if First then
        case PageControl.TabPosition of
          etpTop,
          etpBottom:
            inc(cx, FTab.GetOuterMargin);
          etpLeft:
            //OLD dec(cy, FTab.GetOuterMargin);
            inc(cy, FTab.GetOuterMargin);
          etpRight:
            inc(cy, FTab.GetOuterMargin);
        end
      else
      begin
        case PageControl.TabPosition of
          etpTop,
          etpBottom:
            inc(cx, ASheet2.ARect.Right - ASheet2.ARect.Left);
          etpLeft:
            //OLD dec(cy, ASheet2.ARect.Bottom - ASheet2.ARect.Top);
            inc(cy, ASheet2.ARect.Bottom - ASheet2.ARect.Top);
          etpRight:
            inc(cy, ASheet2.ARect.Bottom - ASheet2.ARect.Top);
        end;
        FTab.DrawSpace(ACanvas, cx, cy, mx, my);
      end;

      // if FPageControl.TabPosition <> etpLeft then
      begin
        // define size for the tab
        TR.Left := cx;
        TR.Top := cy;
        TR.Right := cx + ASheet.ARect.Right;
        TR.Bottom := cy + ASheet.ARect.Bottom;
      end;

      ASheet.ARect := TR;
      FTab.AdjustDrawingSize(false, TR);
      case PageControl.TabPosition of
        etpTop,
        etpBottom:
          begin
            b := FPageControl.CanScrollLeft or (TR.Right > mx);
            if b then
            begin
              if (TR.Right <= ScrollRect.Left) then
                ASheet.AComplete := true;
            end
            else
            if (TR.Right <= mx) then
              ASheet.AComplete := true;
          end;
        etpLeft:
          begin
            b := FPageControl.CanScrollLeft or (TR.Bottom > my);
            if b then
            begin
              if (TR.Bottom <= ScrollRect.Bottom) then
                ASheet.AComplete := true;
            end
            else
            if (TR.Bottom <= my) then
              ASheet.AComplete := true;
          end;
        etpRight:
          begin
            b := FPageControl.CanScrollLeft or (TR.Bottom > my);
            if b then
             begin
              if (TR.Bottom <= ScrollRect.Bottom) then
                ASheet.AComplete := true;
            end
            else
            if (TR.Bottom <= my) then
              ASheet.AComplete := true;
          end;
      end;

      if (ASheet = PageControl.ActivePage) then dap := true;
      if ((ASheet <> PageControl.ActivePage) or FTab.CanDrawTab(false)) then
        FTab.Draw(ACanvas, TR, ASheet);
      ASheet2 := ASheet;
      First := false;
    end;

    if ASheet <> nil then
    begin
      with PageControl do
      case TabPosition of
        etpTop,
        etpBottom:
            if (tx > PageControl.Width) and
               (ASheet.Arect.Right > PageControl.Width - FTab.GetOuterMargin) then
              ASheet.AComplete := false;
        etpLeft,
        etpRight:
          if (ty > PageControl.Height)  and
             (ASheet.Arect.Bottom > PageControl.Height - FTab.GetOuterMargin) then
            ASheet.AComplete := false;
      end;
    end;

    // draw ActivePage if we are in Tabs style
    ASheet := PageControl.ActivePage;
    if (ASheet <> nil) and FTab.CanDrawTab(true) and ASheet.FTabShowing and dap then
    begin
      TR := ASheet.ARect;
      FTab.AdjustDrawingSize(true, TR);
      case PageControl.TabPosition of
        etpTop,
        etpBottom:
          begin
            b := FPageControl.CanScrollLeft or (TR.Right > mx);
            if b then
            begin
              if (TR.Right <= ScrollRect.Left) then
                ASheet.AComplete := true;
            end
            else
            if (TR.Right <= mx) then
              ASheet.AComplete := true;
          end;
        etpLeft:
          begin
            b := FPageControl.CanScrollLeft or (TR.Bottom > my);
            if b then
            begin
              if (TR.Bottom <= ScrollRect.Bottom) then
                ASheet.AComplete := true;
            end
            else
            if (TR.Bottom <= my) then
              ASheet.AComplete := true;
          end;
        etpRight:
          begin
            b := FPageControl.CanScrollLeft or (TR.Bottom > my);
            if b then
             begin
              if (TR.Bottom <= ScrollRect.Bottom) then
                ASheet.AComplete := true;
            end
            else
            if (TR.Bottom <= my) then
              ASheet.AComplete := true;
          end;
      end;

      FTab.Draw(ACanvas, TR, ASheet);
      ASheet.ARect := TR;
    end;

    FTab.DrawTabLine(ACanvas, Rect);

    csl := FPageControl.CanScrollLeft;
    csr := FPageControl.CanScrollRight;
    if csl or csr then
    begin
      ACanvas.CopyRect(ScrollRect, Bmp.Canvas, Classes.Rect(0, 0, Bmp.Width, Bmp.Height));
      FTab.DrawButtons(ACanvas, FPageControl.ScrollLeftRect, FPageControl.ScrollRightRect, csl, csr);
    end
    else
    begin
      SetRectEmpty(FPageControl.ScrollLeftRect);
      SetRectEmpty(FPageControl.ScrollRightRect);
    end;
    Bmp.Free;
  end;
end;

procedure TElTabs.ReorderPages(MaxRows : integer);
var
   TmpSheet,
   BreakSheet : TElTabSheet;

    function TabsOnRow(RowIndex : integer; var LastTab : TElTabSheet) : integer;
    var i : integer;
        ASheet : TElTabSheet;
    begin
      Result := 0;
      LastTab := nil;
      for i := 0 to FPageControl.FPages.Count - 1 do
      begin
        ASheet := TElTabSheet(FPageControl.FPages[i]);
        if ASheet.TabVisible then
        begin
          if ASheet.ALine = RowIndex then
          begin
            LastTab := ASheet;
            inc(Result);
          end;
        end;
      end;
    end;

    function DoReorder(BreakSheet: TElTabSheet) : integer;
    var CurRow : integer;
        i, j   : integer;
        mx, my,
        tx, ty : integer;
        ASheet : TElTabSheet;
        sf,
        First,
        brfl   : boolean;
        PageControl : TElPageControl;
        SheetSize   : TSize;
        CurMargin   : integer;
    begin
      mx := 0;
      my := 0;
      tx := 0;
      ty := 0;

      PageControl := FPageControl;
      CurRow := 0;

      with PageControl do
        if FirstTab <> nil then
          j := FPages.IndexOf(FirstTab)
        else
          j := 0;

      i := j;
      sf := true;
      First := true;
      brfl := false;
      repeat
        ASheet := PageControl.FPages[i];
        if (not sf) and (i = j) then break;
        sf := false;
        if ASheet.TabVisible then
        begin
          SheetSize := TSize(ASheet.ARect.BottomRight);

          case PageControl.TabPosition of
            etpTop,
            etpBottom:
              begin
                if not First then
                  CurMargin := FTab.GetInnerMargin
                else
                  CurMargin := 0;
                if (ASheet = BreakSheet) or brfl or (tx + SheetSize.cx + CurMargin > PageControl.Width) then
                begin
                  // go to new row
                  inc(ty, my + FTab.GetRowMargin);
                  mx := 0;
                  inc(CurRow);
                  if (tx + SheetSize.cx + CurMargin > PageControl.Width) then
                  begin
                    if First then
                    begin
                      SheetSize.cx := PageControl.Width - FTab.GetOuterMargin  * 2;
                      ASheet.ARect.Right := PageControl.Width - FTab.GetOuterMargin * 2;
                    end;
                  end;
                  tx := FTab.GetOuterMargin;
                  First := true;
                end
                else
                begin
                  First := false;
                end;
                tx := tx + SheetSize.cx + CurMargin;
                my := Max(my, SheetSize.cy);
                ASheet.ALine := CurRow;
              end;
            etpLeft,
            etpRight:
              begin
                if not First then
                  CurMargin := FTab.GetInnerMargin
                else
                  CurMargin := 0;
                if (ASheet = BreakSheet) or brfl or (ty + SheetSize.cy + CurMargin > PageControl.Height) then
                begin
                  // go to new row
                  inc(tx, mx + FTab.GetRowMargin);
                  my := 0;
                  inc(CurRow);
                  if (ty + SheetSize.cy + CurMargin > PageControl.Height) then
                  begin
                    if First then
                    begin
                      SheetSize.cy := PageControl.Height - FTab.GetOuterMargin * 2;
                      ASheet.ARect.Bottom := PageControl.Height - FTab.GetOuterMargin * 2;
                    end;
                  end;
                  ty := FTab.GetOuterMargin;
                  First := true;
                end
                else
                begin
                  First := false;
                end;
                ty := ty + SheetSize.cy + FTab.GetInnerMargin;
                mx := Max(mx, SheetSize.cx);
                ASheet.ALine := CurRow;
              end;
          end;
          brfl := false;
        end;

        // increment counter
        inc(i);
        if i = PageControl.FPages.Count then
        begin
          i := 0;
          brfl := true;
        end;
      until false;

     result := CurRow + 1;
   end;

   procedure AlignTabs(MaxRows : integer);
   var i, j : integer;
       L    : TElList;
       ASheet : TElTabSheet;
       tx,
       ty   : integer;
       acc,
       res,
       scale: double;

   begin
     L := TElList.Create;
     for i := 0 to MaxRows - 1 do
     begin
       // build a row
       L.Clear;
       for j := 0 to FPageControl.FPages.Count - 1 do
       begin
         ASheet := TElTabSheet(FPageControl.FPages[j]);
         if ASheet.TabVisible then
         begin
           if ASheet.ALine = i then
           begin
             L.Add(ASheet);
           end;
         end;
       end;
       tx := 0;
       ty := 0;
       acc := 0;
       for j := 0 to L.Count - 1 do
       begin
         case FPageControl.TabPosition of
           etpLeft,
           etpRight:
             begin
               inc(ty, TElTabSheet(L[j]).ARect.Bottom);
               if j > 0 then inc(ty, FTab.GetInnerMargin);
             end;
           etpTop,
           etpBottom:
             begin
               inc(tx, TElTabSheet(L[j]).ARect.Right);
               if j > 0 then inc(tx, FTab.GetInnerMargin);
             end;
         end;
       end;
       scale := 1;
       case FPageControl.TabPosition of
         etpLeft,
         etpRight:
           begin
             scale := (FPageControl.Height - FTab.GetOuterMargin * 2) / ty;
           end;
         etpTop,
         etpBottom:
           begin
             scale := (FPageControl.Width  - FTab.GetOuterMargin * 2) / tx;
           end;
       end;

       for j := 0 to L.Count - 1 do
       begin
         case FPageControl.TabPosition of
           etpLeft,
           etpRight:
             begin
               res := TElTabSheet(L[j]).ARect.Bottom * scale;
               acc := acc + frac(res);
               TElTabSheet(L[j]).ARect.Bottom := Trunc(res);
             end;
           etpTop,
           etpBottom:
             begin
               res := TElTabSheet(L[j]).ARect.Right * scale;
               acc := acc + frac(res);
               TElTabSheet(L[j]).ARect.Right := Trunc(res);
             end;
         end;
       end;
       if (L.Count > 0) and (acc > 0) then
       begin
         case FPageControl.TabPosition of
           etpLeft,
           etpRight:
             inc(TElTabSheet(L.Last).ARect.Bottom, trunc(acc));
           etpTop,
           etpBottom:
             inc(TElTabSheet(L.Last).ARect.Right, trunc(acc));
         end;
       end;
     end;
     L.Free;
   end;

begin
  if MaxRows > 1 then
  begin
    if TabsOnRow(MaxRows - 1, TmpSheet) < TabsOnRow(MaxRows - 2, BreakSheet) - 1 then
    begin
      if DoReorder(BreakSheet) > MaxRows then
        DoReorder(nil);
    end;
  end;
  AlignTabs(MaxRows);
end;


constructor TElTab.Create(Owner : TElTabs);
begin
  inherited Create;
  FOwner := Owner;
end;

function TElTab.GetOuterMargin: Integer;
begin
  Result := 0;
end;

function TElTab.GetInnerMargin: Integer;
begin
  Result := 0;
end;

procedure TElTab.DrawTabContents(Canvas : TCanvas; R : TRect; TabSheet :
    TElTabSheet);
var FPageControl : TElPageControl;
    {$ifndef MSWINDOWS}
    Flags        : integer;
    {$endif}
    AFont        : HFont;
    SR, R2       : TRect;
    Bmp          : TBitmap;
    i, x, y      : integer;
    Size         : TSize;
    ImIdx        : integer;
    aPosition    : TElTabPosition;
begin
  FPageControl := FOwner.FPageControl;
  SR := R;
  if (FPageControl.FActivePage = TabSheet) and (FPageControl.UseActiveTabFont) then
    Canvas.Font.Assign(FPageControl.ActiveTabFont)
  else
  if FPageControl.HotTrack and (FPageControl.FTrackTab = TabSheet) and (TabSheet.TabEnabled) then
    Canvas.Font.Assign(FPageControl.HotTrackFont)
  else
    Canvas.Font.Assign(FPageControl.Font);
  if (not TabSheet.TabEnabled) then
    Canvas.Font.Color := clBtnShadow; 
  {$ifdef MSWINDOWS}
  case FPageControl.TabPosition of
    etpLeft:
      AFont := FOwner.GetRotatedFont(Canvas, 900);
    etpRight:
      AFont := FOwner.GetRotatedFont(Canvas, -900);
    else
      AFont := FOwner.GetRotatedFont(Canvas, 0);
  end;
  Canvas.Font.Handle := AFont;
  {$endif}
  case FPageControl.TabPosition of
    etpTop,
    etpBottom:
      begin
        inc(R.Left, GetContentMargin);
        dec(R.Right, GetContentMargin);
      end;
    etpLeft,
    etpRight:
      begin
        if FPageControl.VerticalSideCaptions then
        begin
          dec(R.Bottom, GetContentMargin);
          inc(R.Top, GetContentMargin);
        end
        else
        begin
          inc(R.Left, GetContentMargin);
          dec(R.Right, GetContentMargin);
        end;
      end;
  end;

  if (not FPageControl.VerticalSideCaptions) then
    aPosition := etpTop
  else
    aPosition := FPageControl.FTabPosition;

  if (FPageControl.Images <> nil) and (FPageControl.ShowImages) then
  begin
    ImIdx := TabSheet.ImageIndex;
    FPageControl.TriggerGetImageEvent(TabSheet.PageIndex, ImIdx);

    if InRange(0, FPageControl.Images.Count - 1, ImIdx) then
    begin
      BMP := TBitmap.Create;
      BMP.Width := FPageControl.Images.Width;
      BMP.Height := FPageControl.Images.Height;

      case aPosition of
        etpTop,
        etpBottom:
          begin
            R2.Left := R.Left;
            R2.Top := R.Top + Max(0, (R.Bottom - R.Top - bmp.Height) div 2);
            R2.Right := Min(R.Right, R.Left + Bmp.Width);
            R2.Bottom := Min(R.Bottom, R2.Top + Bmp.Height);
          end;
        etpLeft:
          begin
            R2.Top := Max(R.Top, R.Bottom - Bmp.Height);
            R2.Left := R.Left + Max(0, (R.Right - R.Left - bmp.Width) div 2);
            R2.Bottom := R.Bottom;
            R2.Right := Min(R.Right, R.Left + Bmp.Width);
          end;
        etpRight:
          begin
            R2.Top := R.Top;
            R2.Left := R.Left + Max(0, (R.Right - R.Left - bmp.Width) div 2);
            R2.Bottom := Min(R.Bottom, R2.Top + Bmp.Height);
            R2.Right := Min(R.Right, R.Left + Bmp.Width);
          end;
      end;

      Bmp.Canvas.CopyRect(Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R2.Top), Canvas, R2);

      FPageControl.Images.Draw(BMP.Canvas, 0, 0, ImIdx);
      Canvas.CopyRect(R2, BMP.Canvas, Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R2.Top));

      BMP.Free;
    end;
    case aPosition of
      etpTop,
      etpBottom:
        inc(R.Left, FPageControl.Images.Width + Margin);
      etpLeft:
        dec(R.Bottom, FPageControl.Images.Height + Margin);
      etpRight:
        inc(R.Top, FPageControl.Images.Height + Margin);
    end;
  end;
  {$ifdef ELPACK_UNICODE}
  GetTextExtentPoint32W(Canvas.Handle, PWideChar(TabSheet.Caption), Length(TabSheet.Caption), Size);
  {$else}
  GetTextExtentPoint32(Canvas.Handle, PChar(TabSheet.Caption), Length(TabSheet.Caption), Size);
  {$endif}
  if (FPageControl.TabPosition in [etpLeft, etpRight]) and FPageControl.VerticalSideCaptions then
  begin
    i := Size.cx;
    Size.cx := Size.cy;
    Size.cy := i;
  end;
  if (FPageControl.TabPosition in [etpLeft, etpRight]) and FPageControl.VerticalSideCaptions then
  begin
    R.Left := Max(R.Left, R.Left + (R.Right - R.Left - Size.cx) div 2);
  end
  else
  begin
    R.Top  := Max(R.Top, R.Top + (R.Bottom - R.Top - Size.cy) div 2);
  end;
  x := 0;
  y := 0;
  case aPosition of
    etpTop,
    etpBottom:
      begin
        x := R.Left;
        y := R.Top;
      end;
    etpLeft:
      begin
        x := R.Left;
        y := R.Bottom;
      end;
    etpRight:
      begin
        x := R.Right;
        y := R.Top;
      end;
  end;

  Canvas.Brush.Style := bsClear;
  {$ifdef MSWINDOWS}
  {$ifdef ELPACK_UNICODE}
  if aPosition in [etpTop, etpBottom] then
    DrawTextW(Canvas.Handle, PWideChar(TabSheet.Caption), Length(TabSheet.Caption), R, DT_SINGLELINE)
  else
    ExtTextOutW(Canvas.Handle, x, y, ETO_CLIPPED, @R, PWideChar(TabSheet.Caption), Length(TabSheet.Caption), nil);
  {$else}
  if (aPosition in [etpTop, etpBottom]) then
    DrawText(Canvas.Handle, PChar(TabSheet.Caption), Length(TabSheet.Caption), R, DT_SINGLELINE)
  else
    ExtTextOut(Canvas.Handle, x, y, ETO_CLIPPED, @R, PChar(TabSheet.Caption), Length(TabSheet.Caption), nil);
  {$endif}
  {$else}
  Flags := Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter);
  QPainter_drawText(Canvas.Handle, @R, Flags, PWideString(@TabSheet.Caption), -1, nil, nil);
  {$endif}
  if FPageControl.DrawFocus and FPageControl.Focused and
     (TabSheet = FPageControl.ActivePage) then
  begin
    Canvas.Pen.Color := FPageControl.Font.Color;
    ElVCLUtils.DrawFocus(Canvas, SR);
  end;
end;

procedure TElTab.DrawSpace(Canvas : TCanvas; var cx, cy : integer; mx, my :
    integer);
begin
  // intentionally left blank
end;

function TElTab.CanDrawTab(ActiveDraw : boolean): Boolean;
begin
  result := not ActiveDraw;
end;

procedure TElTab.AdjustDrawingSize(Active : boolean; var R : TRect);
begin
  // intentionally left blank
end;

function TElTab.GetAscend: Integer;
begin
  Result := 0;
end;

procedure TElTab.Draw(ACanvas : TCanvas; R : TRect; TabSheet : TElTabSheet);
var DefDraw : boolean;
    PgCtl   : TElPageControl;
begin
  PgCtl := FOwner.FPageControl;
  PgCtl.TriggerDrawTabEvent(ACanvas, TabSheet, r, edsBackground, DefDraw);
  if DefDraw then
    FillTab(ACanvas, R, TabSheet);

  PgCtl.TriggerDrawTabEvent(ACanvas, TabSheet, r, edsEdges, DefDraw);
  if DefDraw then
    DrawTabEdges(ACanvas, R, TabSheet);
  InflateRect(R, -(Margin div 2), -(Margin div 2));
  PgCtl.TriggerDrawTabEvent(ACanvas, TabSheet, r, edsContents, DefDraw);
  if DefDraw then
    DrawTabContents(ACanvas, R, TabSheet);
  InflateRect(R, Margin div 2, Margin div 2);
  FixupTab(ACanvas, R, TabSheet);
  TabSheet.AShown := true;
end;

procedure TElTab.AdjustTabSize(var Size : TSize);
begin
  // intentionally left blank
end;

procedure TElTab.AdjustFillSize(After : boolean; var R : TRect; TabSheet : 
    TElTabSheet);
begin
  if After then
    InflateRect(R, 1, 1)
  else
    InflateRect(R, -1, -1);
end;

procedure TElTab.DrawTabLine(Canvas : TCanvas; R : TRect);
begin
  // intentionally left blank
end;

procedure TElTab.FillSpace(Canvas : TCanvas; Rect : TRect);
var R    : TRect;
    OldP : TPoint;
begin
  if IsThemeApplied then
  begin
    R := Rect;

    OldP := FOwner.FPageControl.BoundsRect.TopLeft;
    OffsetRect(R, OldP.x, OldP.y);

    SetMapMode(Canvas.Handle, MM_ANISOTROPIC);
    SetViewPortOrgEx(Canvas.Handle, -R.TopLeft.x, -R.TopLeft.y, @OldP);
    SendMessage(FOwner.FPageControl.Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
    SetViewPortOrgEx(Canvas.Handle, OldP.x, OldP.y, nil);
    SetMapMode(Canvas.Handle, MM_TEXT);
  end
  else
  begin
    Canvas.Brush.Color := FOwner.FPageControl.TabBkColor;
    Canvas.FillRect(Rect);
  end;
end;

procedure TElTab.FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : 
    TElTabSheet);
begin
  AdjustFillSize(false, Rect, TabSheet);
  if (FOwner.FPageControl.FImgForm = nil) or (csDesigning in FOwner.FPageControl.ComponentState) then
  begin
    Canvas.Brush.Style := bsSolid;
    if TabSheet.UseTabColor then
      Canvas.Brush.Color := TabSheet.TabColor
    else
    if TabSheet = FOwner.FPageControl.FActivePage then
      Canvas.Brush.Color := FOwner.FPageControl.FActiveTabColor
    else
      Canvas.Brush.Color := FOwner.FPageControl.FInactiveTabColor;
    Canvas.FillRect(Rect);
  end;
  AdjustFillSize(true, Rect, TabSheet);
end;

procedure TElTab.DrawButtons(Canvas : TCanvas; LeftRect, RightRect : TRect; CSL,
    CSR : boolean);
var
  iPartId: Integer;
  iStateId: Integer;
begin
  if IsThemeApplied then
  begin
    if FOwner.FPageControl.TabPosition in [etpLeft, etpRight] then
    begin
      if FOwner.FPageControl.TabPosition = etpLeft then
        iPartId := SPNP_DOWN
      else
        iPartId := SPNP_UP;
      if CSL then
      begin
        if FOwner.FPageControl.ScrollBtnState = pbsLeftBtnDown then
        begin
          iStateId := DNS_PRESSED;
        end
        else
        begin
          iStateId := DNS_NORMAL;
        end;
      end
      else
      begin
        iStateId := DNS_DISABLED;
      end;
    end
    else
    begin
      iPartId := SPNP_DOWNHORZ;
      if CSL then
      begin
        if FOwner.FPageControl.ScrollBtnState = pbsLeftBtnDown then
        begin
          iStateId := DNHZS_PRESSED;
        end
        else
        begin
          iStateId := DNHZS_NORMAL;
        end;
      end
      else
      begin
        iStateId := DNHZS_DISABLED;
      end;
    end;
    DrawThemeBackground(ScrollTheme, Canvas.Handle, iPartId, iStateId, LeftRect, nil);

    if FOwner.FPageControl.TabPosition in [etpLeft, etpRight] then
    begin
      if FOwner.FPageControl.TabPosition = etpLeft then
        iPartId := SPNP_UP
      else
        iPartId := SPNP_DOWN;
      if CSR then
      begin
        if FOwner.FPageControl.ScrollBtnState = pbsRightBtnDown then
        begin
          iStateId := UPS_PRESSED;
        end
        else
        begin
          iStateId := UPS_NORMAL;
        end;
      end
      else
      begin
        iStateId := UPS_DISABLED;
      end;
    end
    else
    begin
      iPartId := SPNP_UPHORZ;
      if CSR then
      begin
        if FOwner.FPageControl.ScrollBtnState = pbsRightBtnDown then
        begin
          iStateId := UPHZS_PRESSED;
        end
        else
        begin
          iStateId := UPHZS_NORMAL;
        end;
      end
      else
      begin
        iStateId := UPHZS_DISABLED;
      end;
    end;

    DrawThemeBackground(ScrollTheme, Canvas.Handle, iPartId, iStateId, RightRect, nil);
  end
  else
  begin
    DrawButtonFrameEx(Canvas.Handle, LeftRect, not FOwner.FPageControl.Flat,
                      FOwner.FPageControl.ScrollBtnState = pbsLeftBtnDown, FOwner.FPageControl.Color, false);
    DrawButtonFrameEx(Canvas.Handle, RightRect, not FOwner.FPageControl.Flat,
                      FOwner.FPageControl.ScrollBtnState = pbsRightBtnDown, FOwner.FPageControl.Color, false);
    case FOwner.FPageControl.TabPosition of
      etpTop,
      etpBottom:
        begin
          ElVCLUtils.DrawArrow(Canvas, eadLeft, LeftRect, clBtnText, csl);
          ElVCLUtils.DrawArrow(Canvas, eadRight, RightRect, clBtnText, csr);
        end;
      etpLeft:
        begin
          ElVCLUtils.DrawArrow(Canvas, eadDown, LeftRect, clBtnText, csl);
          ElVCLUtils.DrawArrow(Canvas, eadUp, RightRect, clBtnText, csr);
        end;
      etpRight:
        begin
          ElVCLUtils.DrawArrow(Canvas, eadUp, LeftRect, clBtnText, csl);
          ElVCLUtils.DrawArrow(Canvas, eadDown, RightRect, clBtnText, csr);
        end;
    end;
  end;
end;

function TElTab.GetRowMargin: Integer;
begin
  Result := 0;
end;

function TElStdTab.GetOuterMargin: Integer;
begin
  Result := 2;
end;

function TElStdTab.CanDrawTab(ActiveDraw : boolean): Boolean;
begin
  result := ActiveDraw;
end;

function TElStdTab.GetAscend: Integer;
begin
  Result := 2;
end;

procedure TElStdTab.AdjustDrawingSize(Active : boolean; var R : TRect);
var FPageControl : TElPageControl;
begin
  if Active then
  begin
    FPageControl := FOwner.FPageControl;
    case FPageControl.TabPosition of
      etpLeft:
        begin
          //inc(R.Right);
          dec(R.Left, GetAscend);
          inc(R.Bottom, GetOuterMargin);
          dec(R.Top, GetOuterMargin);
        end;
      etpTop:
        begin
          //inc(R.Top);
          dec(R.Top, GetAscend);
          dec(R.Left, GetOuterMargin);
          inc(R.Right, GetOuterMargin);
        end;
      etpRight:
        begin
          //dec(R.Left);
          inc(R.Right, GetAscend);
          inc(R.Bottom, GetOuterMargin);
          dec(R.Top, GetOuterMargin);
        end;
      etpBottom:
        begin
          dec(R.Top);
          inc(R.Bottom, GetAscend);
          dec(R.Left, GetOuterMargin);
          inc(R.Right, GetOuterMargin);
        end;
    end;
  end;
end;

procedure TElStdTab.DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : 
    TElTabSheet);
const ROUND_CORNER_SIZE = 2;
var
  iPartId: Integer;
  iStateId: Integer;
begin
  if IsThemeApplied and (TabPosition = etpTop) then
  begin
    if TabSheet = FOwner.FPageControl.FActivePage then
    begin
      iPartId := TABP_TOPTABITEM;
      iStateId := TIS_SELECTED;
    end
    else
    begin
      iPartId := TABP_TABITEM;
      if TabSheet = FOwner.FPageControl.FTrackTab then
      begin
        iStateId := TIS_HOT;
      end
      else
      begin
        if TabSheet = FOwner.FPageControl.FDownTab then
        begin
          iStateId := TIS_FOCUSED;
        end
        else
        begin
          iStateId := TIS_NORMAL;
        end;
      end;
    end;
    if not TabSheet.Enabled then
    begin
      iStateId := TIS_DISABLED;
    end;
    case FOwner.FPageControl.TabPosition of
      etpLeft:
        inc(R.Right, 1);
      etpRight:
        dec(R.Left, 1);
      etpTop:
       inc(R.Bottom, 1);
      etpBottom:
        dec(R.Bottom, 1);
    end;
    DrawThemeBackground(TabTheme, Canvas.Handle, iPartId, iStateId, R, nil);
    GetThemeBackgroundContentRect(TabTheme, Canvas.Handle, iPartId, iStateId, R, R);
    case FOwner.FPageControl.TabPosition of
      etpLeft:
        dec(R.Right, 1);
      etpRight:
        inc(R.Left, 1);
      etpTop:
       dec(R.Bottom, 1);
      etpBottom:
        inc(R.Bottom, 1);
    end;
  end
  else
  begin
    Canvas.Pen.Color := clBtnHighlight;
    case FOwner.FPageControl.TabPosition of
      etpLeft:
        with Canvas do
        begin
          MoveTo(r.right - 1, r.top);
          LineTo(r.left + ROUND_CORNER_SIZE, r.top);
          LineTo(r.left, r.top + ROUND_CORNER_SIZE);
          LineTo(r.left, r.bottom - ROUND_CORNER_SIZE);

          (* shadow *)
          if FOwner.FPageControl.Flat then
            Pen.Color := clBtnShadow
          else
            Pen.Color := cl3DDkShadow;
          MoveTo(r.left + 1, r.bottom - 2);
          LineTo(r.left + ROUND_CORNER_SIZE, r.bottom - 1);
          LineTo(r.right - 1, r.bottom - 1);

          (* shade *)
          if not FOwner.FPageControl.Flat then
          begin
            Pen.Color := clBtnShadow;
            MoveTo(r.left + ROUND_CORNER_SIZE, r.bottom - 2);
            LineTo(r.right - 1, r.bottom - 2);
          end;
          if TabSheet = FOwner.FPageControl.FActivePage then
            if FOwner.FPageControl.ShowBorder then
            begin
              if not FOwner.FPageControl.Flat then
              begin
                Pixels[R.Right - 1, R.Bottom - 2] := clBtnShadow;
                Pixels[R.Right - 1, R.Bottom - 1] := cl3DDkShadow;
              end
              else
              begin
                Pixels[R.Right - 1, R.Bottom - 1] := clBtnShadow;
              end;
            end;
        end;
      etpTop:
        with Canvas do
        begin
          (* highlight *)
          Pen.Color := clBtnHighlight;
          MoveTo (r.left, r.bottom - 1);
          LineTo (r.left, r.top + ROUND_CORNER_SIZE);
          LineTo (r.left + ROUND_CORNER_SIZE, r.top);
          LineTo (r.right - ROUND_CORNER_SIZE, r.top);

          (* shadow *)
          if FOwner.FPageControl.Flat then
            Pen.Color := clBtnShadow
          else
            Pen.Color := cl3DDkShadow;
          // SelectObject(dc, hbPen);
          MoveTo(R.right - 2, R.Top + 1);
          LineTo(R.right - 1, r.top + ROUND_CORNER_SIZE);
          LineTo(R.right - 1, r.bottom);

          (* shade *)
          if not FOwner.FPageControl.Flat then
          begin
            Pen.Color := clBtnShadow;
            MoveTo(r.right - 2, r.top + ROUND_CORNER_SIZE);
            LineTo(r.right - 2, r.bottom - 1);
          end;
        end;
      etpRight:
        with Canvas do
        begin
          MoveTo(r.left + 1, r.top);
          LineTo(r.right - ROUND_CORNER_SIZE - 1, r.top);
          LineTo(r.right - 1, r.top + ROUND_CORNER_SIZE);

          (* shadow *)
          if FOwner.FPageControl.Flat then
            Pen.Color := clBtnShadow
          else
            Pen.Color := cl3DDkShadow;
          LineTo(r.right - 1, r.bottom - ROUND_CORNER_SIZE - 1);
          LineTo(r.right - ROUND_CORNER_SIZE - 1, r.bottom - 1);
          LineTo(r.left, r.bottom - 1);

          (* shade *)
          if not FOwner.FPageControl.Flat then
          begin
            Pen.Color := clBtnShadow;
            MoveTo(r.right - 2, r.top + 2);
            LineTo(r.right - 2, r.bottom - 2);
            MoveTo(r.right - 3, r.bottom - 2);
            LineTo(r.left - 1,  r.bottom - 2);
          end;
        end;
      etpBottom:
        with Canvas do
        begin
          (* highlight *)

          MoveTo(r.left, r.top + 1);
          LineTo(r.left, r.bottom - ROUND_CORNER_SIZE - 1);
          LineTo(r.left + ROUND_CORNER_SIZE, r.bottom - 1);

          (* shadow *)
          if FOwner.FPageControl.Flat then
            Pen.Color := clBtnShadow
          else
            Pen.Color := cl3DDkShadow;
          LineTo(r.right - ROUND_CORNER_SIZE - 1, r.bottom - 1);
          LineTo(r.right, r.bottom - ROUND_CORNER_SIZE - 2);
          MoveTo(r.right - 1, r.bottom - ROUND_CORNER_SIZE - 1);
          LineTo(r.right - 1, r.top);
        
          (* shade *)
          if not FOwner.FPageControl.Flat then
          begin
            Pen.Color := clBtnShadow;
            MoveTo(r.left + ROUND_CORNER_SIZE, r.bottom - 2);
            LineTo(r.right - 2, r.bottom - 2);
            MoveTo(r.right - 2, r.bottom - ROUND_CORNER_SIZE - 1);
            LineTo(r.right - 2, r.top);
          end;
        end;
    end;
    InflateRect(r, -1, -1);
  end;
end;

procedure TElStdTab.AdjustTabSize(var Size : TSize);
var R,
    R1 : TRect;
    pid,
    sid: integer;
begin
  if IsThemeApplied then
  begin
    pid := TABP_TOPTABITEM;
    sid := TIS_SELECTED;
    R := Rect(0, 0, Size.cx, Size.cy);
    GetThemeBackgroundContentRect(TabTheme, FOwner.FPageControl.Canvas.Handle, pid, sid, R, R1);
    if FOwner.FPageControl.TabPosition in [etpLeft, etpRight] then
    begin
      inc(Size.cy, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cx, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end
    else
    begin
      inc(Size.cx, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cy, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end;
  end
  else
  begin
    inc(Size.cx, 2);
    inc(Size.cy, 2);
  end;
end;

procedure TElStdTab.DrawTabLine(Canvas : TCanvas; R : TRect);
var ASheet : TElTabSheet;
    R1 : TRect;
begin
  ASheet := FOwner.FPageControl.ActivePage;

  if IsThemeApplied then
  begin
    case FOwner.FPageControl.TabPosition of
      etpTop:
        begin
          R.Top := R.Bottom - 1;
          inc(R.Bottom, 2);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(ASheet.ARect.Left, R.Top, ASheet.ARect.Right, R.Bottom);
            InflateRect(R1, 1, 0);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpBottom:
        begin
          if FOwner.FPageControl.Flat then
            R.Bottom := R.Top + 1
          else
            R.Bottom := R.Top + 2;
          dec(R.Top);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(ASheet.ARect.Left, R.Top, ASheet.ARect.Right, R.Bottom);
            InflateRect(R1, 1, 0);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpLeft:
        begin
          R.Left := R.Right - 2;
          //inc(R.Right);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(R.Left, ASheet.ARect.Top, R.Right, ASheet.ARect.Bottom);
            InflateRect(R1, 0, 1);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpRight:
        begin
          if FOwner.FPageControl.Flat then
            R.Right := R.Left + 1
          else
            R.Right := R.Left + 2;
          dec(R.Left, 1);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(R.Left, ASheet.ARect.Top, R.Right, ASheet.ARect.Bottom);
            InflateRect(R1, 0, 1);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
    end;
    DrawThemeBackground(TabTheme, Canvas.Handle, TABP_PANE, 0, R, nil);
    exit;
  end;

  with Canvas do
  case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        Pen.Color := clBtnHighlight;
        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Left, R.Bottom - 1);
          LineTo(R.Right, R.Bottom - 1);
        end
        else
        begin
          if ASheet.ARect.Left > 0 then
          begin
            MoveTo(R.Left, R.Bottom - 1);
            LineTo(ASheet.ARect.Left - 1, R.Bottom - 1);
          end;
          if ASheet.ARect.Right + 2 < R.Right then
          begin
            MoveTo(ASheet.ARect.Right + 1, R.Bottom - 1);
            LineTo(R.Right, R.Bottom - 1);
          end;
        end;
      end;
    etpBottom:
      begin
        R.Bottom := R.Top + 2;
        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          if FOwner.FPageControl.Flat then
            DrawEdge(Handle, R, BDR_RAISEDINNER, BF_BOTTOM)
          else
            DrawEdge(Handle, R, BDR_RAISED, BF_BOTTOM);
        end
        else
        begin
          if ASheet.ARect.Left > 0 then
          begin
            R1 := Rect(R.Left, R.Top, ASheet.ARect.Left + 1, R.Bottom);
            if FOwner.FPageControl.Flat then
              DrawEdge(Handle, R1, BDR_RAISEDINNER, BF_BOTTOM)
            else
              DrawEdge(Handle, R1, BDR_RAISED, BF_BOTTOM);
          end;
          if ASheet.ARect.Right < R.Right then
          begin
            R1 := Rect(ASheet.ARect.Right - 1, R.Top, R.Right, R.Bottom);
            if FOwner.FPageControl.Flat then
              DrawEdge(Handle, R1, BDR_RAISEDINNER, BF_BOTTOM)
            else
              DrawEdge(Handle, R1, BDR_RAISED, BF_BOTTOM);
          end;
        end;

        if FOwner.FPageControl.ShowBorder then
        begin
          Pixels[R.Left, R.Bottom - 1] := clBtnHighlight;
          Pixels[R.Left, R.Bottom - 2] := clBtnHighlight;
          if FOwner.FPageControl.Flat then
            Pixels[R.Right -1, R.Bottom - 2] := clBtnShadow
          else
            Pixels[R.Right -1, R.Bottom - 2] := cl3DDkShadow;
        end;
      end;
    etpLeft:
      begin
        Pen.Color := clBtnHighlight;
        Dec(R.Bottom);

        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Right - 1, R.Top);
          LineTo(R.Right - 1, R.Bottom);
        end
        else
        begin
          if ASheet.ARect.Top > 0 then
          begin
            MoveTo(R.Right - 1, R.Top);
            LineTo(R.Right - 1, ASheet.ARect.Top);
          end;
          if ASheet.ARect.Bottom < R.Bottom then
          begin
            MoveTo(R.Right - 1, ASheet.ARect.Bottom);
            LineTo(R.Right - 1, R.Bottom);
          end;
        end;
        if FOwner.FPageControl.ShowBorder then
        begin
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 1, R.Bottom - 1] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);
      end;
    etpRight:
      begin
        R.Right := R.Left + 2;
        Dec(R.Bottom);

        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          if FOwner.FPageControl.Flat then
            DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RIGHT)
          else
            DrawEdge(Handle, R, BDR_RAISED, BF_RIGHT);
        end
        else
        begin
          if ASheet.ARect.Top > 0 then
          begin
            R1 := Rect(R.Left, R.Top, R.Right, ASheet.ARect.Top + 1);
            if FOwner.FPageControl.Flat then
              DrawEdge(Handle, R1, BDR_RAISEDINNER, BF_RIGHT)
            else
              DrawEdge(Handle, R1, BDR_RAISED, BF_RIGHT);
          end;
          if ASheet.ARect.Bottom < R.Bottom then
          begin
            R1 := Rect(R.Left, ASheet.ARect.Bottom - 1, R.Right, R.Bottom);
            if FOwner.FPageControl.Flat then
              DrawEdge(Handle, R1, BDR_RAISEDINNER, BF_RIGHT)
            else
              DrawEdge(Handle, R1, BDR_RAISED, BF_RIGHT);
          end;
        end;

        if FOwner.FPageControl.ShowBorder then
        begin
          Pixels[R.Right - 2, R.Top] := clBtnHighlight;
          Pixels[R.Right - 1, R.Top] := clBtnHighlight;
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 2, R.Bottom] := cl3DDkShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 2, R.Bottom] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);

      end;
  end;
end;

procedure TElStdTab.AdjustFillSize(After : boolean; var R : TRect; TabSheet : 
    TElTabSheet);
begin
  inherited;
  if TabSheet = FOwner.FPageControl.FActivePage then
    case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        if After then
        begin
          dec(R.Right);
          dec(R.Bottom);
        end
        else
        begin
          inc(R.Right);
          inc(R.Bottom);
        end;
      end;
    etpBottom:
      begin
        if After then
        begin
          inc(R.Top);
          //inc(R.Bottom);
          //dec(R.Right);
        end
        else
        begin
          dec(R.Top);
          //dec(R.Bottom);
          // inc(R.Right);
        end;
      end;
    etpLeft:
      begin
        if After then
        begin
          //dec(R.Left);
          dec(R.Right);
          //dec(R.Bottom);
        end
        else
        begin
          //inc(R.Left);
          inc(R.Right);
          //inc(R.Bottom);
        end;
      end;
    etpRight:
      begin
        if After then
        begin
          inc(R.Left, 2);
          //dec(R.Right);
          //dec(R.Bottom);
        end
        else
          dec(R.Left, 2);
          //inc(R.Right);
          //inc(R.Bottom);
      end;
  end
  else
  begin
    case FOwner.FPageControl.TabPosition of
      etpBottom:
        begin
        end;
      etpRight:
        begin
        end;
    end;
  end;
end;

procedure TElStdTab.FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : 
    TElTabSheet);
var ACtl   : TWinControl;
    BgRect : TRect;
begin
  AdjustFillSize(false, Rect, TabSheet);
  if (FOwner.FPageControl.FImgForm = nil) or (csDesigning in FOwner.FPageControl.ComponentState) then
  begin
    Canvas.Brush.Style := bsSolid;
    if TabSheet.UseTabColor then
      Canvas.Brush.Color := TabSheet.TabColor
    else
    if TabSheet = FOwner.FPageControl.FActivePage then
      Canvas.Brush.Color := FOwner.FPageControl.FActiveTabColor
    else
      Canvas.Brush.Color := FOwner.FPageControl.FInactiveTabColor;
    Canvas.FillRect(Rect);
  end
  else
  if TabSheet = FOwner.FPageControl.FActivePage then
  begin
    if (FOwner.FPageControl.FImgForm.Control <> FOwner.FPageControl) then
    begin
      ACtl := FOwner.FPageControl.FImgForm.GetRealControl;

      BgRect.Left := FOwner.FPageControl.Left;
      BgRect.Top :=  FOwner.FPageControl.Top;

      BgRect.TopLeft := FOwner.FPageControl.Parent.ClientToScreen(BgRect.TopLeft);
      BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);

      FOwner.FPageControl.FImgForm.PaintBkgnd(Canvas.Handle, Rect, BgRect.TopLeft, false);
    end;
  end;
  AdjustFillSize(true, Rect, TabSheet);
end;


function TElBtnTab.GetInnerMargin: Integer;
begin
  Result := Margin;
end;

procedure TElBtnTab.DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet :
    TElTabSheet);
var
  b : boolean;
  iPartId: Integer;
  iStateId: Integer;
begin
  if IsThemeApplied then
  begin
    iPartId := BP_PUSHBUTTON;
    if TabSheet.Enabled then
    begin
      if TabSheet = FOwner.FPageControl.FActivePage then
      begin
          iStateId := PBS_PRESSED;
      end
      else
      begin
        if TabSheet = FOwner.FPageControl.FTrackTab then
        begin
          iStateId := PBS_HOT;
        end
        else
        begin
          if TabSheet = FOwner.FPageControl.FDownTab then
          begin
            iStateId := PBS_PRESSED;
          end
          else
          begin
            iStateId := PBS_NORMAL;
          end;
        end;
      end;
    end
    else
    begin
      iStateId := PBS_DISABLED;
    end;
    DrawThemeBackground(BtnTheme, Canvas.Handle, iPartId, iStateId, R, nil);
    GetThemeBackgroundContentRect(BtnTheme, Canvas.Handle, iPartId, iStateId, R, R);
  end
  else
begin
  b := (TabSheet = FOwner.FPageControl.ActivePage) or
       ((TabSheet = FOwner.FPageControl.FDownTab) and
        (TabSheet = FOwner.FPageControl.FTrackTab));  
  DrawButtonFrameEx(Canvas.Handle, R, not FOwner.FPageControl.Flat, b, FOwner.FPageControl.ActiveTabColor, FOwner.FPageControl.Flat);
  InflateRect(R, -2, -2);
  end;
end;

procedure TElBtnTab.DrawSpace(Canvas : TCanvas; var cx, cy : integer; mx, my :
    integer);
begin
  case FOwner.FPageControl.TabPosition of
    etpTop,
    etpBottom:
      begin
        inc(cx, GetInnerMargin);
      end;
    etpLeft:
      begin
        dec(cy, GetInnerMargin);
      end;
    etpRight:
      begin
        inc(cy, GetInnerMargin);
      end;
  end;
end;

procedure TElBtnTab.AdjustTabSize(var Size : TSize);
var R,
    R1 : TRect;
    pid,
    sid: integer;
begin
  if IsThemeApplied then
  begin
    pid := BP_PUSHBUTTON;
    sid := PBS_HOT;
    R := Rect(0, 0, Size.cx, Size.cy);
    GetThemeBackgroundContentRect(BtnTheme, FOwner.FPageControl.Canvas.Handle, pid, sid, R, R1);
    if FOwner.FPageControl.TabPosition in [etpLeft, etpRight] then
    begin
      inc(Size.cy, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cx, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end
    else
    begin
      inc(Size.cx, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cy, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end;
  end
  else
  begin
    inc(Size.cx, 4);
    inc(Size.cy, 4);
  end;
end;

function TElBtnTab.GetRowMargin: Integer;
begin
  Result := 2;
end;

function TElFlatBtnTab.GetInnerMargin: Integer;
begin
  Result := Margin * 2 + 2;
end;

procedure TElFlatBtnTab.DrawSpace(Canvas : TCanvas; var cx, cy : integer; mx,
    my : integer);
begin
  if not IsThemeApplied then
  begin
    case FOwner.FPageControl.TabPosition of
      etpTop,
      etpBottom:
        begin
          inc(cx, (GetInnerMargin -2) div 2);
          Canvas.Pen.Color := clBtnShadow;
          Canvas.MoveTo(cx - 1, cy);
          Canvas.LineTo(cx - 1, cy + my);
          Canvas.Pen.Color := clBtnHighlight;
          Canvas.MoveTo(cx, cy);
          Canvas.LineTo(cx, cy + my);
          inc(cx, (GetInnerMargin -2) div 2);
        end;
      etpLeft:
        begin
          dec(cy, (GetInnerMargin -2) div 2);

          Canvas.Pen.Color := clBtnHighlight;
          Canvas.MoveTo(cx, cy + 1);
          Canvas.LineTo(cx + mx, cy + 1);
          Canvas.Pen.Color := clBtnShadow;
          Canvas.MoveTo(cx, cy);
          Canvas.LineTo(cx + mx, cy);
          dec(cy, (GetInnerMargin -2) div 2);
        end;
      etpRight:
        begin
          inc(cy, (GetInnerMargin -2) div 2);

          Canvas.Pen.Color := clBtnShadow;
          Canvas.MoveTo(cx, cy - 1);
          Canvas.LineTo(cx + mx, cy - 1);
          Canvas.Pen.Color := clBtnHighlight;
          Canvas.MoveTo(cx, cy);
          Canvas.LineTo(cx + mx, cy);

          inc(cy, (GetInnerMargin -2) div 2);
        end;
    end;
  end
  else
    Inherited;
end;

procedure TElFlatBtnTab.DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet
    : TElTabSheet);
var
  iPartId: Integer;
  iStateId: Integer;
begin
  if IsThemeApplied then
  begin
    iPartId := BP_PUSHBUTTON;
    if TabSheet.Enabled then
    begin
      if TabSheet = FOwner.FPageControl.FActivePage then
      begin
          iStateId := PBS_PRESSED;
      end
      else
      begin
        if TabSheet = FOwner.FPageControl.FTrackTab then
        begin
          iStateId := PBS_HOT;
        end
        else
        begin
          if TabSheet = FOwner.FPageControl.FDownTab then
          begin
            iStateId := PBS_PRESSED;
          end
          else
          begin
            iStateId := PBS_NORMAL;
          end;
        end;
      end;
    end
    else
    begin
      iStateId := PBS_DISABLED;
    end;
    if iStateId <> PBS_NORMAL then
    begin
      DrawThemeBackground(BtnTheme, Canvas.Handle, iPartId, iStateId, R, nil);
    end;
    GetThemeBackgroundContentRect(BtnTheme, Canvas.Handle, iPartId, iStateId, R, R);
  end
  else
  begin
    if TabSheet = FOwner.FPageControl.FActivePage then
    begin
      DrawButtonFrameEx(Canvas.Handle, R, false, true, FOwner.FPageControl.ActiveTabColor, FOwner.FPageControl.Flat);
    end
    else
    if (TabSheet = FOwner.FPageControl.FDownTab) and
       (TabSheet = FOwner.FPageControl.FTrackTab) then
    begin
      DrawButtonFrameEx(Canvas.Handle, R, false, true, FOwner.FPageControl.ActiveTabColor, FOwner.FPageControl.Flat);
    end
    else
    if TabSheet = FOwner.FPageControl.FTrackTab then
    begin
      DrawButtonFrameEx(Canvas.Handle, R, false, false, FOwner.FPageControl.InactiveTabColor, FOwner.FPageControl.Flat);
    end;
    InflateRect(R, -2, -2);
  end;
end;

procedure TElFlatBtnTab.AdjustTabSize(var Size : TSize);
var R,
    R1 : TRect;
    pid,
    sid: integer;
begin
  if IsThemeApplied then
  begin
    pid := BP_PUSHBUTTON;
    sid := PBS_HOT;
    R := Rect(0, 0, Size.cx, Size.cy);
    GetThemeBackgroundContentRect(BtnTheme, FOwner.FPageControl.Canvas.Handle, pid, sid, R, R1);
    if FOwner.FPageControl.TabPosition in [etpLeft, etpRight] then
    begin
      inc(Size.cy, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cx, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end
    else
    begin
      inc(Size.cx, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cy, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end;
  end
  else
  begin
    inc(Size.cx, 4);
    inc(Size.cy, 4);
  end;
end;

procedure TElFlatBtnTab.AdjustFillSize(After : boolean; var R : TRect; TabSheet 
    : TElTabSheet);
begin
  if (TabSheet = FOwner.FPageControl.FActivePage) or
     (TabSheet = FOwner.FPageControl.FDownTab) or
     (TabSheet = FOwner.FPageControl.FTrackTab) then
  begin
    if After then
      InflateRect(R, 1, 1)
    else
      InflateRect(R, -1, -1);
  end;
end;

function TElFlatBtnTab.GetRowMargin: Integer;
begin
  Result := 2;
end;

procedure TElNetTab.DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : 
    TElTabSheet);
var
  iPartId: Integer;
  iStateId: Integer;
begin
  if IsThemeApplied and (TabPosition = etpTop) then
  begin
    if TabSheet = FOwner.FPageControl.FActivePage then
    begin
      iPartId := TABP_TOPTABITEM;
        iStateId := TIS_SELECTED;
    end
    else
    begin
      iPartId := TABP_TABITEM;
      if TabSheet = FOwner.FPageControl.FTrackTab then
      begin
        iStateId := TIS_HOT;
      end
      else
      begin
        if TabSheet = FOwner.FPageControl.FDownTab then
        begin
          iStateId := TIS_FOCUSED;
        end
        else
        begin
          iStateId := TIS_NORMAL;
        end;
      end;
    end;
    if not TabSheet.Enabled then
    begin
      iStateId := TIS_DISABLED;
    end;
    if (iStateId <> TIS_NORMAL) or (iPartId = TABP_TOPTABITEM) then
    begin
      DrawThemeBackground(TabTheme, Canvas.Handle, iPartId, iStateId, R, nil);
      GetThemeBackgroundContentRect(TabTheme, Canvas.Handle, iPartId, iStateId, R, R);
    end
    else
    begin
      DrawThemeBackground(TabTheme, Canvas.Handle, TABP_BODY, 0, R, nil);
      GetThemeBackgroundContentRect(TabTheme, Canvas.Handle, TABP_BODY, 0, R, R);
    end;
  end
  else
  begin
    if TabSheet = FOwner.FPageControl.FActivePage then
    begin
      Canvas.Pen.Color := IncColor(clBtnHighlight, 20, 20, 20);

      case FOwner.FPageControl.TabPosition of
        etpLeft:
          with Canvas do
          begin
            MoveTo(r.right - 1, r.top);
            LineTo(r.left, r.top);
            LineTo(r.left, r.bottom + 1);

            (* shadow *)
            if FOwner.FPageControl.Flat then
              Pen.Color := clBtnShadow
            else
              Pen.Color := cl3DDkShadow;
            MoveTo(r.left + 1, r.bottom);
            LineTo(r.right, r.bottom);
          end;
        etpTop:
          with Canvas do
          begin
            (* highlight *)
            MoveTo (r.left, r.bottom - 1);
            LineTo (r.left, r.top);
            LineTo (r.right, r.top);

            (* shadow *)
            if FOwner.FPageControl.Flat then
              Pen.Color := clBtnShadow
            else
              Pen.Color := cl3DDkShadow;
            MoveTo(r.right -1, r.top + 1);
            LineTo(r.right -1, r.bottom - 1);
          end;
        etpRight:
          with Canvas do
          begin
            MoveTo(r.left, r.top);
            LineTo(r.right, r.top);

            (* shadow *)
            if FOwner.FPageControl.Flat then
              Pen.Color := clBtnShadow
            else
              Pen.Color := cl3DDkShadow;
            LineTo(r.right, r.bottom + 1);
            MoveTo(r.left, r.bottom);
            LineTo(r.right, r.bottom);
          end;
        etpBottom:
          with Canvas do
          begin
            (* highlight *)

            MoveTo(r.left, r.top);
            LineTo(r.left, r.bottom + 1);

            (* shadow *)
            if FOwner.FPageControl.Flat then
              Pen.Color := clBtnShadow
            else
              Pen.Color := cl3DDkShadow;

            MoveTo(r.right, r.top);
            LineTo(r.right, r.bottom);
            LineTo(r.left, r.bottom);
          end;
      end;
    end;
    InflateRect(r, -1, -1);
  end;
end;

procedure TElNetTab.AdjustTabSize(var Size : TSize);
var R,
    R1 : TRect;
    pid,
    sid: integer;
begin
  if IsThemeApplied then
  begin
    pid := TABP_TOPTABITEM;
    sid := TIS_SELECTED;
    R := Rect(0, 0, Size.cx, Size.cy);
    GetThemeBackgroundContentRect(TabTheme, FOwner.FPageControl.Canvas.Handle, pid, sid, R, R1);
    if FOwner.FPageControl.TabPosition in [etpLeft, etpRight] then
    begin
      inc(Size.cy, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cx, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end
    else
    begin
      inc(Size.cx, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cy, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end;
  end
  else
  begin
    inc(Size.cx, 4);
    inc(Size.cy, 4);
  end;
end;

function TElNetTab.GetInnerMargin: Integer;
begin
  Result := 3;
end;

procedure TElNetTab.DrawSpace(Canvas : TCanvas; var cx, cy : integer; mx, my : 
    integer);
begin
  if IsThemeApplied and (TabPosition = etpTop) then
  begin
    Inherited;
  end
  else
  begin
    case FOwner.FPageControl.TabPosition of
      etpTop:
        begin
          inc(cx, GetInnerMargin div 2);
          Canvas.Pen.Color := clBtnFace;
          Canvas.MoveTo(cx, cy);
          Canvas.LineTo(cx, cy + my - GetInnerMargin - 1);
          inc(cx, GetInnerMargin div 2);
        end;
      etpBottom:
        begin
          inc(cx, GetInnerMargin div 2);
          Canvas.Pen.Color := clBtnFace;
          Canvas.MoveTo(cx, cy + GetInnerMargin);
          Canvas.LineTo(cx, cy + my - 1);
          inc(cx, GetInnerMargin div 2);
        end;
      etpLeft:
        begin
          dec(cy, GetInnerMargin div 2);
          Canvas.Pen.Color := clBtnFace;
          Canvas.MoveTo(cx, cy);
          Canvas.LineTo(cx + mx - GetInnerMargin - 1, cy);
          dec(cy, GetInnerMargin div 2);
        end;
      etpRight:
        begin
          inc(cy, GetInnerMargin div 2);
          Canvas.Pen.Color := clBtnFace;
          Canvas.MoveTo(cx + GetInnerMargin, cy);
          Canvas.LineTo(cx + mx, cy);
          inc(cy, GetInnerMargin div 2);
        end;
    end;
  end;
end;

function TElNetTab.CanDrawTab(ActiveDraw : boolean): Boolean;
begin
  result := ActiveDraw;
end;

procedure TElNetTab.AdjustDrawingSize(Active : boolean; var R : TRect);
var FPageControl : TElPageControl;
begin
  if Active then
  begin
    FPageControl := FOwner.FPageControl;
    case FPageControl.TabPosition of
      etpLeft:
        begin
          inc(R.Bottom);
          dec(R.Top);
        end;
      etpTop:
        begin
          dec(R.Left);
          inc(R.Right);
        end;
      etpRight:
        begin
          inc(R.Bottom);
          dec(R.Top);
        end;
      etpBottom:
        begin
          dec(R.Left);
          inc(R.Right);
        end;
    end;
  end;
end;

function TElNetTab.GetOuterMargin: Integer;
begin
  Result := 4;
end;

procedure TElNetTab.DrawTabLine(Canvas : TCanvas; R : TRect);
var ASheet : TElTabSheet;
    R1     : TRect;
begin
  ASheet := FOwner.FPageControl.ActivePage;

  if IsThemeApplied then
  begin
    case FOwner.FPageControl.TabPosition of
      etpTop:
        begin
          R.Top := R.Bottom - 1;
          inc(R.Bottom, 2);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(ASheet.ARect.Left, R.Top, ASheet.ARect.Right, R.Bottom);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpBottom:
        begin
          R.Bottom := R.Top + 1;
          dec(R.Top);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(ASheet.ARect.Left, R.Top, ASheet.ARect.Right, R.Bottom);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpLeft:
        begin
          R.Left := R.Right - 2;
          //inc(R.Right);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(R.Left, ASheet.ARect.Top, R.Right, ASheet.ARect.Bottom);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpRight:
        begin
          R.Right := R.Left + 2;
          dec(R.Left, 1);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(R.Left, ASheet.ARect.Top, R.Right, ASheet.ARect.Bottom);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
    end;
    DrawThemeBackground(TabTheme, Canvas.Handle, TABP_PANE, 0, R, nil);
    exit;
  end;

  ASheet := FOwner.FPageControl.ActivePage;
  with Canvas do
  case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        Pen.Color := clBtnHighlight;
        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Left, R.Bottom - 1);
          LineTo(R.Right, R.Bottom - 1);
        end
        else
        begin
          if ASheet.ARect.Left > 0 then
          begin
            MoveTo(R.Left, R.Bottom - 1);
            LineTo(ASheet.ARect.Left - 1, R.Bottom - 1);
          end;
          if ASheet.ARect.Right < R.Right then
          begin
            MoveTo(ASheet.ARect.Right, R.Bottom - 1);
            LineTo(R.Right, R.Bottom - 1);
          end;
        end;
      end;
    etpBottom:
      begin
        if not FOwner.FPageControl.Flat then
          Pen.Color := cl3DDkShadow
        else
          Pen.Color := clBtnShadow;
        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Left, R.Top);
          LineTo(R.Right, R.Top);
        end
        else
        begin
          if ASheet.ARect.Left > 0 then
          begin
            MoveTo(R.Left, R.Top);
            LineTo(ASheet.ARect.Left, R.Top);
          end;
          if ASheet.ARect.Right < R.Right then
          begin
            MoveTo(ASheet.ARect.Right + 1, R.Top);
            LineTo(R.Right, R.Top);
          end;
        end;
      end;
    etpLeft:
      begin
        Pen.Color := clBtnHighlight;
        Dec(R.Bottom);

        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Right - 1, R.Top);
          LineTo(R.Right - 1, R.Bottom);
        end
        else
        begin
          if ASheet.ARect.Top > 0 then
          begin
            MoveTo(R.Right - 1, R.Top);
            LineTo(R.Right - 1, ASheet.ARect.Top);
          end;
          if ASheet.ARect.Bottom < R.Bottom then
          begin
            MoveTo(R.Right - 1, ASheet.ARect.Bottom);
            LineTo(R.Right - 1, R.Bottom);
          end;
        end;
        if FOwner.FPageControl.ShowBorder then
        begin
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 1, R.Bottom - 1] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);
      end;
    etpRight:
      begin
        if not FOwner.FPageControl.Flat then
          Pen.Color := cl3DDkShadow
        else
          Pen.Color := clBtnShadow;

        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Left, R.Top);
          LineTo(R.Left, R.Bottom);
        end
        else
        begin
          if ASheet.ARect.Top > 0 then
          begin
            MoveTo(R.Left, R.Top);
            LineTo(R.Left, ASheet.ARect.Top);
          end;
          if ASheet.ARect.Bottom < R.Bottom then
          begin
            MoveTo(R.Left, ASheet.ARect.Bottom + 1);
            LineTo(R.Left, R.Bottom);
          end;
        end;

        (*
        if FOwner.FPageControl.ShowBorder then
        begin
          Pixels[R.Right - 2, R.Top] := clBtnHighlight;
          Pixels[R.Right - 1, R.Top] := clBtnHighlight;
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 2, R.Bottom] := cl3DDkShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 2, R.Bottom] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);
        *)
      end;
  end;
end;

(*
begin
  with Canvas do
  case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        Pen.Color := clBtnHighlight;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
      end;
    etpBottom:
      begin
        if not FOwner.FPageControl.Flat then
          Pen.Color := cl3DDkShadow
        else
          Pen.Color := clBtnShadow;
        MoveTo(R.Left, R.Top);
        LineTo(R.Right, R.Top);
      end;
    etpLeft:
      begin
        Pen.Color := clBtnHighlight;
        Dec(R.Bottom);
        MoveTo(R.Right - 1, R.Top);
        LineTo(R.Right - 1, R.Bottom);
        if FOwner.FPageControl.ShowBorder then
        begin
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 1, R.Bottom - 1] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);
      end;
    etpRight:
      begin
        if not FOwner.FPageControl.Flat then
          Pen.Color := cl3DDkShadow
        else
          Pen.Color := clBtnShadow;
        MoveTo(R.Left, R.Top);
        LineTo(R.Left, R.Bottom);
      end;
  end;
end;
*)

function TElNetTab.GetAscend: Integer;
begin
  Result := 2;
end;

procedure TElNetTab.FillSpace(Canvas : TCanvas; Rect : TRect);
begin
  if IsThemeApplied and (TabPosition = etpTop) then
  begin
    Inherited;
  end
  else
  begin
    Canvas.Brush.Color := FOwner.FPageControl.TabBkColorNetStyle;
    Canvas.FillRect(Rect);
  end;
end;

procedure TElNetTab.AdjustFillSize(After : boolean; var R : TRect; TabSheet :
    TElTabSheet);
begin
  inherited;
  if TabSheet = FOwner.FPageControl.FActivePage then
  case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        if After then
        begin
          dec(R.Right);
          dec(R.Bottom);
        end
        else
        begin
          inc(R.Right);
          inc(R.Bottom);
        end;
      end;
    etpBottom:
      begin
        if After then
        begin
          inc(R.Top, 2);
          dec(R.Bottom);
          dec(R.Right);
        end
        else
        begin
          dec(R.Top, 2);
          inc(R.Bottom);
          inc(R.Right);
        end;
      end;
    etpLeft:
      begin
        if After then
        begin
          dec(R.Right, 2);
          dec(R.Bottom);
        end
        else
        begin
          inc(R.Right, 2);
          inc(R.Bottom);
        end;
      end;
    etpRight:
      begin
        if After then
        begin
          inc(R.Left, 2);
          dec(R.Right);
          dec(R.Bottom);
        end
        else
          dec(R.Left, 2);
          inc(R.Right);
          inc(R.Bottom);
      end;
  end;
end;

procedure TElNetTab.FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : 
    TElTabSheet);
begin
  if TabSheet = FOwner.FPageControl.FActivePage then inherited;
end;

procedure TElNetTab.DrawButtons(Canvas : TCanvas; LeftRect, RightRect : TRect; 
    CSL, CSR : boolean);
var PageControl : TElPageControl;
    ColorRight,
    ColorLeft   : TColor;
begin
  if IsThemeApplied then
  begin
    Inherited;
  end
  else
  begin
    PageControl := FOwner.FPageControl;

    if CSL and (PageControl.FScrollBtnState in [pbsLeftBtnOver, pbsLeftBtnDown]) then
      DrawButtonFrameEx(Canvas.Handle, LeftRect, false,
                        PageControl.FScrollBtnState = pbsLeftBtnDown, PageControl.Color, PageControl.Flat);
    if CSR and (PageControl.FScrollBtnState in [pbsRightBtnOver, pbsRightBtnDown]) then
      DrawButtonFrameEx(Canvas.Handle, RightRect, false,
                      PageControl.FScrollBtnState = pbsRightBtnDown, PageControl.Color, PageControl.Flat);

    if PageControl.FScrollBtnState = pbsLeftBtnDown then
      ColorLeft := clBtnHighlight
    else
      ColorLeft := cl3DDkShadow;
    if PageControl.FScrollBtnState = pbsRightBtnDown then
      ColorRight := clBtnHighlight
    else
      ColorRight := cl3DDkShadow;

    case PageControl.TabPosition of
      etpTop,
      etpBottom:
        begin
          ElVCLUtils.DrawArrow(Canvas, eadLeft, LeftRect, ColorLeft, csl);
          ElVCLUtils.DrawArrow(Canvas, eadRight, RightRect, ColorRight, csr);
        end;
      etpLeft:
        begin
          ElVCLUtils.DrawArrow(Canvas, eadDown, LeftRect, ColorLeft, csl);
          ElVCLUtils.DrawArrow(Canvas, eadUp, RightRect, ColorRight, csr);
        end;
      etpRight:
        begin
          ElVCLUtils.DrawArrow(Canvas, eadUp, LeftRect, ColorLeft, csl);
          ElVCLUtils.DrawArrow(Canvas, eadDown, RightRect, ColorRight, csr);
        end;
    end;
  end;
end;


function TElPageControl.GetThemedClassName: WideString;
begin
  Result := 'TAB';
end;

procedure TElPageControl.CreateThemeHandle;
begin
  inherited;
  if ThemesAvailable then
  begin
    FBtnTheme := OpenThemeData(Handle, PWideChar(WideString('BUTTON')));
    FScrollTheme := OpenThemeData(Handle, PWideChar(WideString('SPIN')));
  end
  else
  begin
    FBtnTheme := 0;
    FScrollTheme := 0;
  end;
end;

procedure TElPageControl.FreeThemeHandle;
begin
  inherited;
  if ThemesAvailable then
  begin
    CloseThemeData(FBtnTheme);
    CloseThemeData(FScrollTheme);
  end;
  FBtnTheme := 0;
  FScrollTheme := 0;
end;

procedure TElPageControl.WMNCCreate(var Message: TMessage);
begin
  inherited;
  if UseXPThemes then
    CreateThemeHandle;
end;

procedure TElPageControl.WMNCDestroy(var Message: TMessage);
begin
  if UseXPThemes then
    FreeThemeHandle;
  inherited;
end;

procedure TElPageControl.WMSetCursor(var Message: TWMSetCursor);
var P : TPoint;
begin
  if FDoStartDrag = 1 then
    Message.Result := 1
  else
  begin
    with Message do
      if CursorWnd = Handle then
        case Smallint(HitTest) of
          HTBORDER:
            begin
              GetCursorPos(P);
              p := parent.ScreenToClient(p);
              dec(p.x, left);
              dec(p.y, top);
              if TabFromPoint(p) <> nil then
              begin
                Windows.SetCursor(Screen.Cursors[TabCursor]);
                Message.Result := 1;
              end
              else
                inherited;
            end;
          else
            inherited;
        end;
  end;
end;

procedure TElPageControl.WMNCMouseMove(var Message: TMessage);
var
  p: TPoint;
begin
  if (FDraggablePages)and(FDoStartDrag > -1) then
  begin
    p.x := TWMNCMouseMove(Message).XCursor;
    p.y := TWMNCMouseMove(Message).YCursor;
    p := parent.ScreenToClient(p);
    dec(p.x, left);
    dec(p.y, top);
    if FDragTab = TabFromPoint(p) then
    begin
      if FDoStartDrag = 0 then
      begin
        FDoStartDrag := 1;
        SetCursor(Screen.Cursors[DragCursor]);
      end;
    end
    else
      if FDoStartDrag = 0 then FDoStartDrag := -1;
  end;

  inherited;
  if (FHintCoords.x <> TWMNCMouseMove(Message).XCursor) or
     (FHintCoords.y <> TWMNCMouseMove(Message).YCursor) then
  begin
    if FHintWnd <> nil then
      ShowWindow(FhintWnd.Handle, SW_HIDE);

    FHintTimer.Enabled := false;
    FHintCoords := Point(TWMNCMouseMove(Message).XCursor, TWMNCMouseMove(Message).YCursor);
    if ShowTabHints then
      FHintTimer.Enabled := true;
  end;
end;

procedure TElPageControl.OnHintTimer(Sender : TObject);
var ASheet : TElTabSheet;
    R      : TRect;
    P      : TPoint;
    mx, my : integer;
    S      : string;

    function GetCursorHeightMargin : Integer;
    var
      IconInfo : TIconInfo;
      BitmapInfoSize : DWORD;
      BitmapBitsSize : DWORD;
      Bitmap : PBitmapInfoHeader;
      Bits : Pointer;
      BytesPerScanline, ImageSize : DWORD;
  {$WARNINGS OFF}
      function FindScanline(Source : Pointer; MaxLen : Cardinal;
        Value : Cardinal) : Cardinal; assembler;
      asm
                PUSH    ECX
                MOV     ECX,EDX
                MOV     EDX,EDI
                MOV     EDI,EAX
                POP     EAX
                REPE    SCASB
                MOV     EAX,ECX
                MOV     EDI,EDX
      end;

    begin
      { Default value is entire icon height }
      Result := GetSystemMetrics(SM_CYCURSOR);
      if GetIconInfo(GetCursor, IconInfo) then
      try
        GetDIBSizes(IconInfo.hbmMask, BitmapInfoSize, BitmapBitsSize);
        Bitmap := AllocMem(BitmapInfoSize + BitmapBitsSize);
        try
          Bits := Pointer(Longint(Bitmap) + BitmapInfoSize);
          if GetDIB(IconInfo.hbmMask, 0, Bitmap^, Bits^) and
            (Bitmap^.biBitCount = 1) then
          begin
            { Point Bits to the end of this bottom-up bitmap }
            with Bitmap^ do
            begin
              BytesPerScanline := ((biWidth * biBitCount + 31) and not 31) div 8;
              ImageSize := biWidth * BytesPerScanline;
              Bits := Pointer(Integer(Bits) + BitmapBitsSize - ImageSize);
              { Use the width to determine the height since another mask bitmap
                may immediately follow }
              Result := FindScanline(Bits, ImageSize, $FF);
              { In case the and mask is blank, look for an empty scanline in the
                xor mask. }
              if (Result = 0) and (biHeight >= 2 * biWidth) then
                Result := FindScanline(Pointer(Integer(Bits) - ImageSize),
                  ImageSize, $00);
              Result := Result div BytesPerScanline;
            end;
            Dec(Result, IconInfo.yHotSpot);
          end;
        finally
          FreeMem(Bitmap, BitmapInfoSize + BitmapBitsSize);
        end;
      finally
        if IconInfo.hbmColor <> 0 then DeleteObject(IconInfo.hbmColor);
        if IconInfo.hbmMask <> 0 then DeleteObject(IconInfo.hbmMask);
      end;
    end;

    function GetHint(Control: TElTabSheet) : string;
    var HintInfo : THintInfo;
        CanShow  : Boolean;
    begin
      HintInfo.HintControl := Control;
      HintInfo.HintPos := P;
      Inc(HintInfo.HintPos.Y, GetCursorHeightMargin);
      HintInfo.HintMaxWidth := Screen.Width;
      HintInfo.HintColor := Application.HintColor;
      HintInfo.CursorRect := Control.BoundsRect;
      HintInfo.HintStr := Control.Hint;
      HintInfo.ReshowTimeout := 0;
      HintInfo.HideTimeout := Application.HintHidePause;
      HintInfo.HintWindowClass := HintWindowClass;
      HintInfo.HintData := nil;

      CanShow := Control.Perform(CM_HINTSHOW, 0, Longint(@HintInfo)) = 0;
      if CanShow and Assigned(Application.OnShowHint) then
        Application.OnShowHint(HintInfo.HintStr, CanShow, HintInfo);
      if CanShow and (HintInfo.HintStr <> '') then
      begin
        result := HintInfo.HintStr;
      end;
    end;

begin
  FHintTimer.Enabled := false;
  P := Parent.ClientToScreen(Point(Left, Top));
  P.x := FHintCoords.X - P.x;
  P.y := FHintCoords.Y - P.y;
  ASheet := TabFromPoint(P);
  P := FHintCoords;
  if ASheet = nil then exit;
  S := GetHint(ASheet);
  if StrLen(PChar(S)) = 0 then                     
    exit;
  if FHintWnd = nil then
    FHintWnd := HintWindowClass.Create(Self);

  FHintWnd.Canvas.Font.Charset := Font.Charset;
{$ifndef CLX_USED}
{$IFDEF VCL_4_USED}
  mx := Screen.DesktopWidth + Screen.DesktopLeft;
  my := Screen.DesktopHeight + Screen.DesktopTop;
{$ELSE}
  mx := GetSystemMetrics(SM_CXSCREEN);
  my := GetSystemMetrics(SM_CYSCREEN);
{$ENDIF}
{$else}
  mx := Screen.Width;
  my := Screen.Height;
{$endif}

  R := FHintWnd.CalcHintRect(Screen.Width, S, nil);
  OffsetRect(R, P.X, P.Y + GetCursorHeightMargin);
{$ifndef CLX_USED}
{$IFDEF VCL_4_USED}
  if R.Left < Screen.DesktopLeft then
    OffsetRect(R, Screen.DesktopLeft - R.Left, 0);
  if R.Top < Screen.DesktopTop then
    OffsetRect(R, 0, Screen.DesktopTop - R.Top);
{$ELSE}
  if R.Left < 0 then OffsetRect(R, -R.Left, 0);
  if R.Top < 0 then OffsetRect(R, 0, -R.Top);
{$ENDIF}
{$else}
  if R.Left < 0 then OffsetRect(R, -R.Left, 0);
  if R.Top < 0 then OffsetRect(R, 0, -R.Top);
{$endif}
  if R.Right > mx then OffsetRect(R, -(R.Right - mx), 0);
  if R.Bottom > my then OffsetRect(R, -(R.Bottom - my), 0);

  FHintWnd.Color := Application.HintColor;
  FHintWnd.ActivateHint(R, S);
end;

procedure TElPageControl.WMMouseMove(var Message: TMessage);
begin
  inherited;
  if FHintWnd <> nil then
    ShowWindow(FhintWnd.Handle, SW_HIDE);
  FHintTimer.Enabled := false;
end;

procedure TElPageControl.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    FreeThemeHandle;
    CreateThemeHandle;
    SetWindowPos(
      Handle,
      0,
      0, 0, 0, 0,
      SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER
      );
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end;
  Message.Result := 1;
end;

procedure TElPageControl.SetSavvyMode(Value: Boolean);
var i : integer;
begin
  if FSavvyMode <> Value then
  begin
    FSavvyMode := Value;
    for i := 0 to PageCount - 1 do
      if FSavvyMode then
      begin
        if (Pages[i] <> ActivePage) and (Pages[i].HandleAllocated) then
          Pages[i].DestroyHandle;
      end
      else
      begin
        if (Pages[i] <> ActivePage) and (not Pages[i].HandleAllocated) then
          Pages[i].UpdateControlState;
      end;
  end;
end;

procedure TElPageControl.SetFlatTabBorderColor(Value: TColor);
begin
  if FFlatTabBorderColor <> Value then
  begin
    FFlatTabBorderColor := Value;
    if Style = etsFlatTabs then
    begin
      if ShowTabs then
        UpdateTabs(false);
      if (ActivePage <> nil) and ActivePage.Visible then
        ActivePage.Invalidate;
    end;
  end;
end;

procedure TElPageControl.SetTabBkColorNetStyle(Value: TColor);
begin
  if FTabBkColorNetStyle <> Value then
  begin
    FTabBkColorNetStyle := Value;
    if (Style = etsNetTabs) and ShowTabs then
      UpdateTabs(false);
  end;
end;

procedure TElPageControl.SetVerticalSideCaptions(Value: Boolean);
begin
  if FVerticalSideCaptions <> Value then
  begin
    FVerticalSideCaptions := Value;
    RebuildTabs(true);
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElPageControl.CMHintShow(var Message: TMessage);
{$else}
function TElPageControl.HintShow(var HintInfo : THintInfo): Boolean;
{$endif}
var T: WideChar;
{$ifndef CLX_USED}
  HintInfo : PHintInfo;
{$endif}
  l : integer;
  S : String;
  WS: String;
begin
{$ifndef CLX_USED}
  inherited;
  HintInfo := PHintInfo(Message.lParam);
{$else}
  inherited HintShow(HintInfo);
  result := true;
{$endif}
  if Length(FHint) = 0 then
  begin
    HintInfo.hintStr := '';
    exit;
  end;

  S := GetShortHint(inherited Hint);
  if HintInfo.HintStr = S then
  begin
    WS := GetShortHintW(FHint);
  end
  else
  begin
    S := FHint;
    WS := FHint;
  end;

  l := Length(S) + 1 + Length(WS) * 2;
  SetLength(HintInfo.HintStr, l + 4);
  Move(PChar(S)^, HintInfo.HintStr[1], Length(S) + 1);

  Move(WS[1], HintInfo.HintStr[Length(S) + 2], Length(WS) * 2);
  T := #0;
  Move(T, HintInfo.HintStr[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, HintInfo.HintStr[l + 3], sizeof(T));
end;

procedure TElPageControl.SetHint(Value: WideString);
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

procedure TElPageControl.SetActiveTabFont(Value: TFont);
begin
  FActiveTabFont.Assign(Value);
end;

procedure TElPageControl.SetUseActiveTabFont(Value: Boolean);
begin
  if FUseActiveTabFont <> Value then
  begin
    FUseActiveTabFont := Value;
    if ShowTabs then
      UpdateTabs(true);
  end;
end;

procedure TElPageControl.ActiveTabFontChange(Sender : TObject);
begin
  if (FActivePage <> nil) and UseActiveTabFont and ShowTabs then
    UpdateTabs(false); 
end;

procedure TElPageControl.TriggerDrawTabEvent(Canvas : TCanvas; Page : TElTabSheet; Rect : TRect; 
    DrawStep : TElTabDrawState; var DefaultDrawing : boolean);
begin
  DefaultDrawing := true;
  if assigned(FOnDrawTab) then FOnDrawTab(Self, Canvas, Page, Rect, DrawStep, DefaultDrawing); 
end;

function TElTabSheet.IsThemeApplied: Boolean;
begin
  Result := Assigned(PageControl) and PageControl.IsThemeApplied();
end;

function TElTabSheet.GetBtnTheme: HTheme;
begin
  if Assigned(PageControl) then
    Result := PageControl.BtnTheme
  else
    Result := 0;      
end;

function TElTabSheet.GetScrollTheme: HTheme;
begin
  if Assigned(PageControl) then
    Result := PageControl.ScrollTheme
  else
    Result := 0;                     
end;

function TElTabSheet.GetTabTheme: HTheme;
begin
  if Assigned(PageControl) then
    Result := PageControl.Theme
  else
    Result := 0;
end;                                   

procedure TElTabSheet.WMNCCalcSize(var Message: TWMNCCalcSize);
var                          
  R, R1: TRect;
begin
  if IsThemeApplied then
  begin
    if PageControl.ShowBorder then
    begin
      R := Message.CalcSize_Params.rgrc[0];
      inherited;
      R := Message.CalcSize_Params.rgrc[0];
      if Succeeded(GetThemeBackgroundContentRect(TabTheme, Canvas.Handle, TABP_PANE, 0, R, R1)) then
      begin
        {
        // page control will draw our line
        if PageControl.ShowTabs and (PageControl.Style in [etsTabs, etsNetTabs]) then
        begin
          case PageControl.TabPosition of
            etpTop: R1.Top := R.Top;
            etpBottom: R1.Bottom := R.Bottom;
            etpRight: R1.Right := R.Right;
            etpLeft: R1.Left := R.Left;
          end;
        end;
        }
        Message.CalcSize_Params.rgrc[0] := R1;
      end;
    end;
  end
  else
    inherited;
end;

procedure TElTabSheet.WMNCPaint(var Message: TMessage);
var
  RC,
  R1,
  R2,
  RW : TRect;
  DC : HDC;
  s  : integer;
begin
  if not IsThemeApplied then
  begin
    inherited;
  end
  else
  begin
    DC := GetWindowDC(Handle);
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    IntMapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);
    R2 := RW;

    if PageControl.ShowBorder then
      s := 0
    else
      s := 1;

    GetThemeBackgroundContentRect(TabTheme, DC, TABP_PANE, 0, RW, R1);
    Inc(RW.Left, (RW.Left - R1.Left) * s);
    Inc(RW.Top, (RW.Top - R1.Top) * s);
    Inc(RW.Right, (RW.Right - R1.Right) * s);
    Inc(RW.Bottom, (RW.Bottom - R1.Bottom) * s);

    if PageControl.ShowTabs and (PageControl.Style in [etsTabs, etsNetTabs, etsFlatTabs, etsAngledTabs]) then
    begin
      case PageControl.TabPosition of
        etpTop: RW.Top := RW.Top + (R2.Top - R1.Top);
        etpBottom: RW.Bottom := RW.Bottom + (R2.Bottom - R1.Bottom);
        etpRight: RW.Right := RW.Right + (R2.Right - R1.Right);
        etpLeft: RW.Left := RW.Left + (R2.Left - R1.Left);
      end;
    end;

    DrawThemeBackground(TabTheme, DC, TABP_PANE, 0, RW, @R2);
    ReleaseDC(Handle, DC);
  end;
end;

function TElTabs.GetBtnTheme: HTheme;
begin
  if Assigned(FPageControl) then
  begin
    Result := FPageControl.BtnTheme;
  end
  else
  begin
    Result := 0;
  end;
end;

function TElTabs.GetScrollTheme: HTheme;
begin
  if Assigned(FPageControl) then
  begin
    Result := FPageControl.ScrollTheme;
  end
  else
  begin
    Result := 0;
  end;
end;

function TElTabs.GetTabTheme: HTheme;
begin
  if Assigned(FPageControl) then
  begin
    Result := FPageControl.Theme;
  end
  else
  begin
    Result := 0;
  end;
end;

function TElTabs.IsThemeApplied: Boolean;
begin
  Result := Assigned(FPageControl) and FPageControl.IsThemeApplied;
end;

function TElTab.GetBtnTheme: HTheme;
begin
  if Assigned(FOwner) then
  begin
    Result := FOwner.BtnTheme;
  end
  else
  begin
    Result := 0;
  end;
end;

function TElTab.GetScrollTheme: HTheme;
begin
  if Assigned(FOwner) then
  begin
    Result := FOwner.ScrollTheme;
  end
  else
  begin
    Result := 0;
  end;
end;

function TElTab.GetTabTheme: HTheme;
begin
  if Assigned(FOwner) then
  begin
    Result := FOwner.TabTheme;
  end
  else
  begin
    Result := 0;
  end;
end;

function TElTab.IsThemeApplied: Boolean;
begin
  Result := Assigned(FOwner) and FOwner.IsThemeApplied;
end;

function TElTab.GetTabPosition: TElTabPosition;
begin
  Result := etpTop;
  if Assigned(FOwner) and Assigned(FOwner.FPageControl) then
  begin
    Result := FOwner.FPageControl.TabPosition;
  end;
end;

function TElTab.GetContentMargin: Integer;
begin
  Result := Margin;
end;

procedure TElTab.FixupTab(Canvas : TCanvas; R : TRect; TabSheet : TElTabSheet);
begin
  // intentionally left blank
end;

procedure TElBtnTab.FillTab(Canvas: TCanvas; Rect: TRect;
  TabSheet: TElTabSheet);
begin
  if not IsThemeApplied then
  begin
    inherited;
  end;
end;

procedure TElFlatBtnTab.FillTab(Canvas: TCanvas; Rect: TRect;
  TabSheet: TElTabSheet);
begin
  if not IsThemeApplied then
  begin
    inherited;
  end;
end;

procedure TElTabSheet.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    SetWindowPos(
      Handle,
      0,
      0, 0, 0, 0,
      SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER
      );
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end;
  Message.Result := 1;
end;

function TElTabSheet.GetUseXPThemes: Boolean;
begin
  Result := Assigned(PageControl) and PageControl.UseXPThemes; 
end;

procedure TElTabSheet.CreateWnd;
begin
  inherited;
  if ThemesAvailable then
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_FRAMECHANGED);
end;

procedure TElTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TElPageControl then
    PageControl := TElPageControl(Reader.Parent);
end;

procedure TElTabSheet.CMDialogChar(var Message: TCMDialogChar);
begin
  if Enabled and TabVisible and IsAccel(Message.CharCode, Caption) then
  begin
    FPageControl.ActivePage := Self;
  end
  else
    inherited;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElTabSheet.CMHintShow(var Message: TMessage);
{$else}
function TElTabSheet.HintShow(var HintInfo : THintInfo): Boolean; 
{$endif}
var T: WideChar;
{$ifndef CLX_USED}
  HintInfo : PHintInfo;
{$endif}
  l : integer;
  S : String;
  WS: WideString;
begin
{$ifndef CLX_USED}
  inherited;
  HintInfo := PHintInfo(Message.lParam);
{$else}
  inherited HintShow(HintInfo);
  result := true;
{$endif}
  if Length(FHint) = 0 then
  begin
    HintInfo.hintStr := '';
    exit;
  end;

  S := GetShortHint(inherited Hint);
  if HintInfo.HintStr = S then
  begin
    WS := GetShortHintW(FHint);
  end
  else
  begin
    S := FHint;
    WS := FHint;
  end;

  l := Length(S) + 1 + Length(WS) * 2;
  SetLength(HintInfo.HintStr, l + 4);
  Move(PChar(S)^, HintInfo.HintStr[1], Length(S) + 1);

  Move(WS[1], HintInfo.HintStr[Length(S) + 2], Length(WS) * 2);
  T := #0;
  Move(T, HintInfo.HintStr[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, HintInfo.HintStr[l + 3], sizeof(T));
end;

procedure TElTabSheet.SetHint(Value: WideString);
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


procedure TElTabSheet.SetUseTabColor(Value: Boolean);
begin
  if FUseTabColor <> Value then
  begin
    FUseTabColor := Value;
    if Parent <> nil then
      TElPageControl(Parent).UpdateTab(Self);
  end;
end;

procedure TEl2DFlatTab.AdjustDrawingSize(Active : boolean; var R : TRect);
var FPageControl : TElPageControl;
begin
  if Active then
  begin
    FPageControl := FOwner.FPageControl;
    case FPageControl.TabPosition of
      etpLeft:
        begin
          inc(R.Bottom);
          dec(R.Top);
        end;
      etpTop:
        begin
          dec(R.Left);
          inc(R.Right);
        end;
      etpRight:
        begin
          inc(R.Bottom);
          dec(R.Top);
        end;
      etpBottom:
        begin
          dec(R.Left);
          inc(R.Right);
        end;
    end;
  end;
end;

procedure TEl2DFlatTab.AdjustFillSize(After : boolean; var R : TRect; TabSheet 
    : TElTabSheet);
begin
  inherited;
  if TabSheet = FOwner.FPageControl.FActivePage then
  case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        if After then
        begin
          dec(R.Right);
          dec(R.Bottom);
        end
        else
        begin
          inc(R.Right);
          inc(R.Bottom);
        end;
      end;
    etpBottom:
      begin
        if After then
        begin
          inc(R.Top);
          dec(R.Bottom);
          dec(R.Right);
        end
        else
        begin
          dec(R.Top);
          inc(R.Bottom);
          inc(R.Right);
        end;
      end;
    etpLeft:
      begin
        if After then
        begin
          dec(R.Right);
          dec(R.Bottom);
        end
        else
        begin
          inc(R.Right);
          inc(R.Bottom);
        end;
      end;
    etpRight:
      begin
        if After then
        begin
          inc(R.Left);
          dec(R.Right);
          dec(R.Bottom);
        end
        else
          dec(R.Left);
          inc(R.Right);
          inc(R.Bottom);
      end;
  end;
end;

procedure TEl2DFlatTab.AdjustTabSize(var Size : TSize);
var R,
    R1 : TRect;
    pid,
    sid: integer;
begin
  if IsThemeApplied then
  begin
    pid := TABP_TOPTABITEM;
    sid := TIS_SELECTED;
    R := Rect(0, 0, Size.cx, Size.cy);
    GetThemeBackgroundContentRect(TabTheme, FOwner.FPageControl.Canvas.Handle, pid, sid, R, R1);
    if FOwner.FPageControl.TabPosition in [etpLeft, etpRight] then
    begin
      inc(Size.cy, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cx, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end
    else
    begin
      inc(Size.cx, (R1.Left - R.Left) + (R.Right - R1.Right));
      inc(Size.cy, (R1.Top - R.Top) + (R.Bottom - R1.Bottom));
    end;
  end
  else
  begin
    inc(Size.cx, 2);
    inc(Size.cy, 2);
  end;
end;

function TEl2DFlatTab.CanDrawTab(ActiveDraw : boolean): Boolean;
begin
  result := ActiveDraw;
end;

procedure TEl2DFlatTab.DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet : 
    TElTabSheet);
var
  iPartId: Integer;
  iStateId: Integer;
begin
  if IsThemeApplied and (TabPosition = etpTop) then
  begin
    if TabSheet = FOwner.FPageControl.FActivePage then
    begin
      iPartId := TABP_TOPTABITEM;
        iStateId := TIS_SELECTED;
    end
    else
    begin
      iPartId := TABP_TABITEM;
      if TabSheet = FOwner.FPageControl.FTrackTab then
      begin
        iStateId := TIS_HOT;
      end
      else
      begin
        if TabSheet = FOwner.FPageControl.FDownTab then
        begin
          iStateId := TIS_FOCUSED;
        end
        else
        begin
          iStateId := TIS_NORMAL;
        end;
      end;
    end;
    if not TabSheet.Enabled then
    begin
      iStateId := TIS_DISABLED;
    end;
    if (iStateId <> TIS_NORMAL) or (iPartId = TABP_TOPTABITEM) then
    begin
      DrawThemeBackground(TabTheme, Canvas.Handle, iPartId, iStateId, R, nil);
      GetThemeBackgroundContentRect(TabTheme, Canvas.Handle, iPartId, iStateId, R, R);
    end
    else
    begin
      DrawThemeBackground(TabTheme, Canvas.Handle, TABP_BODY, 0, R, nil);
      GetThemeBackgroundContentRect(TabTheme, Canvas.Handle, TABP_BODY, 0, R, R);
    end;
  end
  else
  begin
    if TabSheet = FOwner.FPageControl.FActivePage then
    begin
      Canvas.Pen.Color := FOwner.FPageControl.FlatTabBorderColor;// IncColor(clBtnHighlight, 20, 20, 20);

      case FOwner.FPageControl.TabPosition of
        etpLeft:
          with Canvas do
          begin
            MoveTo(r.right - 1, r.top);
            LineTo(r.left, r.top);
            LineTo(r.left, r.bottom + 1);

            MoveTo(r.left + 1, r.bottom);
            LineTo(r.right, r.bottom);
          end;
        etpTop:
          with Canvas do
          begin
            (* highlight *)
            MoveTo (r.left, r.bottom - 1);
            LineTo (r.left, r.top);
            LineTo (r.right, r.top);

            MoveTo(r.right -1, r.top + 1);
            LineTo(r.right -1, r.bottom - 1);
          end;
        etpRight:
          with Canvas do
          begin
            MoveTo(r.left, r.top);
            LineTo(r.right, r.top);

            LineTo(r.right, r.bottom + 1);
            MoveTo(r.left, r.bottom);
            LineTo(r.right, r.bottom);
          end;
        etpBottom:
          with Canvas do
          begin
            (* highlight *)

            MoveTo(r.left, r.top);
            LineTo(r.left, r.bottom + 1);

            MoveTo(r.right, r.top);
            LineTo(r.right, r.bottom);
            LineTo(r.left, r.bottom);
          end;
      end;
    end;
    InflateRect(r, -1, -1);
  end;
end;

procedure TEl2DFlatTab.DrawTabLine(Canvas : TCanvas; R : TRect);
var ASheet : TElTabSheet;
    R1     : TRect;
begin
  ASheet := FOwner.FPageControl.ActivePage;

  if IsThemeApplied then
  begin
    case FOwner.FPageControl.TabPosition of
      etpTop:
        begin
          R.Top := R.Bottom - 1;
          inc(R.Bottom, 2);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(ASheet.ARect.Left, R.Top, ASheet.ARect.Right, R.Bottom);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpBottom:
        begin
          R.Bottom := R.Top + 1;
          dec(R.Top);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(ASheet.ARect.Left, R.Top, ASheet.ARect.Right, R.Bottom);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpLeft:
        begin
          R.Left := R.Right - 2;
          //inc(R.Right);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(R.Left, ASheet.ARect.Top, R.Right, ASheet.ARect.Bottom);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
      etpRight:
        begin
          R.Right := R.Left + 2;
          dec(R.Left, 1);
          if (ASheet <> nil) and ASheet.AShown then
          begin
            R1 := Rect(R.Left, ASheet.ARect.Top, R.Right, ASheet.ARect.Bottom);
            with R1 do
              ExcludeCliprect(Canvas.Handle, Left, Top, Right, Bottom);
          end;
        end;
    end;
    DrawThemeBackground(TabTheme, Canvas.Handle, TABP_PANE, 0, R, nil);
    exit;
  end;

  Canvas.Pen.Color := FOwner.FPageControl.FlatTabBorderColor;

  ASheet := FOwner.FPageControl.ActivePage;
  with Canvas do
  case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Left, R.Bottom - 1);
          LineTo(R.Right, R.Bottom - 1);
        end
        else
        begin
          if ASheet.ARect.Left > 0 then
          begin
            MoveTo(R.Left, R.Bottom - 1);
            LineTo(ASheet.ARect.Left - 1, R.Bottom - 1);
          end;
          if ASheet.ARect.Right < R.Right then
          begin
            MoveTo(ASheet.ARect.Right, R.Bottom - 1);
            LineTo(R.Right, R.Bottom - 1);
          end;
        end;
      end;
    etpBottom:
      begin
        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Left, R.Top);
          LineTo(R.Right, R.Top);
        end
        else
        begin
          if ASheet.ARect.Left > 0 then
          begin
            MoveTo(R.Left, R.Top);
            LineTo(ASheet.ARect.Left, R.Top);
          end;
          if ASheet.ARect.Right < R.Right then
          begin
            MoveTo(ASheet.ARect.Right + 1, R.Top);
            LineTo(R.Right, R.Top);
          end;
        end;
      end;
    etpLeft:
      begin
        Dec(R.Bottom);

        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Right - 1, R.Top);
          LineTo(R.Right - 1, R.Bottom);
        end
        else
        begin
          if ASheet.ARect.Top > 0 then
          begin
            MoveTo(R.Right - 1, R.Top);
            LineTo(R.Right - 1, ASheet.ARect.Top - 1);
          end;
          if ASheet.ARect.Bottom < R.Bottom then
          begin
            MoveTo(R.Right - 1, ASheet.ARect.Bottom + 1);
            LineTo(R.Right - 1, R.Bottom);
          end;
        end;
        if FOwner.FPageControl.ShowBorder then
        begin
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 1, R.Bottom - 1] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);
      end;
    etpRight:
      begin

        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Left, R.Top);
          LineTo(R.Left, R.Bottom);
        end
        else
        begin
          if ASheet.ARect.Top > 0 then
          begin
            MoveTo(R.Left, R.Top);
            LineTo(R.Left, ASheet.ARect.Top);
          end;
          if ASheet.ARect.Bottom < R.Bottom then
          begin
            MoveTo(R.Left, ASheet.ARect.Bottom + 1);
            LineTo(R.Left, R.Bottom);
          end;
        end;

        (*
        if FOwner.FPageControl.ShowBorder then
        begin
          Pixels[R.Right - 2, R.Top] := clBtnHighlight;
          Pixels[R.Right - 1, R.Top] := clBtnHighlight;
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 2, R.Bottom] := cl3DDkShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 2, R.Bottom] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);
        *)
      end;
  end;
end;

function TEl2DFlatTab.GetAscend: Integer;
begin
  Result := 2;
end;

function TEl2DFlatTab.GetInnerMargin: Integer;
begin
  Result := 2;
end;

function TEl2DFlatTab.GetOuterMargin: Integer;
begin
  Result := 4;
end;

procedure TEl2DFlatTab.FillTab(Canvas : TCanvas; Rect : TRect; TabSheet : 
    TElTabSheet);
var ACtl   : TWinControl;
    BgRect : TRect;
begin
  AdjustFillSize(false, Rect, TabSheet);
  if (FOwner.FPageControl.FImgForm = nil) or (csDesigning in FOwner.FPageControl.ComponentState) then
  begin
    Canvas.Brush.Style := bsSolid;
    if TabSheet.UseTabColor then
      Canvas.Brush.Color := TabSheet.TabColor
    else
    if TabSheet = FOwner.FPageControl.FActivePage then
      Canvas.Brush.Color := FOwner.FPageControl.FActiveTabColor
    else
      Canvas.Brush.Color := FOwner.FPageControl.FInactiveTabColor;
    Canvas.FillRect(Rect);
  end
  else
  if TabSheet = FOwner.FPageControl.FActivePage then
  begin
    if (FOwner.FPageControl.FImgForm.Control <> FOwner.FPageControl) then
    begin
      ACtl := FOwner.FPageControl.FImgForm.GetRealControl;

      BgRect.Left := FOwner.FPageControl.Left;
      BgRect.Top :=  FOwner.FPageControl.Top;

      BgRect.TopLeft := FOwner.FPageControl.Parent.ClientToScreen(BgRect.TopLeft);
      BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);

      FOwner.FPageControl.FImgForm.PaintBkgnd(Canvas.Handle, Rect, BgRect.TopLeft, false);
    end;
  end;
  AdjustFillSize(true, Rect, TabSheet);
end;

function TElAngledTab.CanDrawTab(ActiveDraw : boolean): Boolean;
begin
  result := true//not ActiveDraw;
end;

procedure TElAngledTab.DrawTabEdges(Canvas : TCanvas; var R : TRect; TabSheet :
    TElTabSheet);
begin
  begin
    // if TabSheet = FOwner.FPageControl.FActivePage then
    begin
      Canvas.Pen.Color := clBtnShadow;

      case FOwner.FPageControl.TabPosition of
        etpLeft:
          with Canvas do
          begin
            if FOwner.FPageControl.Flat then
              Pen.Color := clBtnShadow
            else
              Pen.Color := cl3DDkShadow;

            MoveTo(r.right, r.bottom);
            LineTo(r.left, r.bottom - AngledOffset);
            if not FOwner.FPageControl.Flat then
            begin
              Canvas.Pen.Color := clBtnShadow;
              MoveTo(r.right, r.bottom - 1);
              LineTo(r.left, r.bottom - AngledOffset - 1);
            end;

            Canvas.Pen.Color := clBtnHighlight;
            LineTo(r.left, r.top + AngledOffset);
            LineTo(R.right, r.top);
          end;
        etpTop:
          with Canvas do
          begin
            Canvas.Pen.Color := clBtnHighlight;
            MoveTo (r.left, r.bottom - 1);
            LineTo (r.left + AngledOffset, r.top);
            LineTo (r.right -AngledOffset, r.top);

            if FOwner.FPageControl.Flat then
              Pen.Color := clBtnShadow
            else
              Pen.Color := cl3DDkShadow;

            LineTo(r.right, r.bottom - 1);
            if not FOwner.FPageControl.Flat then
            begin
              Canvas.Pen.Color := clBtnShadow;
              MoveTo(r.right - 1, r.bottom - 1);
              LineTo(R.right - AngledOffset - 1, r.top);
            end;
          end;
        etpRight:
          with Canvas do
          begin
            Canvas.Pen.Color := clBtnHighlight;
            MoveTo(r.left, r.top);
            LineTo(r.right -1, r.top + AngledOffset);

            if FOwner.FPageControl.Flat then
              Pen.Color := clBtnShadow
            else
              Pen.Color := cl3DDkShadow;

            LineTo(r.right-1, r.bottom - AngledOffset);
            LineTo(r.left, r.bottom);

            if not FOwner.FPageControl.Flat then
            begin
              Canvas.Pen.Color := clBtnShadow;
              MoveTo(r.left, r.bottom - 1);
              LineTo(r.right - 1, r.bottom - AngledOffset - 1);
              MoveTo(r.right - 2, r.bottom - AngledOffset - 1);
              LineTo(r.right - 2, r.top + AngledOffset);
            end;
          end;
        etpBottom:
          with Canvas do
          begin
            (* highlight *)
            Canvas.Pen.Color := cl3DDkShadow;
            Canvas.Pen.Color := clBtnHighlight;

            MoveTo(r.left, r.top);
            LineTo(r.left + AngledOffset, r.bottom - 1);

            if FOwner.FPageControl.Flat then
              Pen.Color := clBtnShadow
            else
              Pen.Color := cl3DDkShadow;
            MoveTo(r.left + AngledOffset, r.bottom - 1);
            LineTo(r.right - AngledOffset, r.bottom -1);
            LineTo(r.right, r.top);

            if not FOwner.FPageControl.Flat then
            begin
              Canvas.Pen.Color := clBtnShadow;
              MoveTo(r.left + AngledOffset + 1, r.bottom - 2);
              LineTo(r.right - AngledOffset, r.bottom - 2);
              MoveTo(r.right - 1, r.top);
              LineTo(r.right - AngledOffset - 1, r.bottom - 1);
            end;
          end;
      end;
    end;
    InflateRect(r, -1, -1);
  end;
end;

procedure TElAngledTab.DrawTabLine(Canvas : TCanvas; R : TRect);
var ASheet : TElTabSheet;
    R1 : TRect;
begin
  ASheet := FOwner.FPageControl.ActivePage;

  with Canvas do
  case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        Pen.Color := clBtnHighlight;
        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Left, R.Bottom - 1);
          LineTo(R.Right, R.Bottom - 1);
        end
        else
        begin
          if ASheet.ARect.Left > 0 then
          begin
            MoveTo(R.Left, R.Bottom - 1);
            LineTo(ASheet.ARect.Left{ - 2}, R.Bottom - 1);
          end;
          if ASheet.ARect.Right - 1 < R.Right then
          begin
            MoveTo(ASheet.ARect.Right - 1, R.Bottom - 1);
            LineTo(R.Right, R.Bottom - 1);
          end;
        end;
      end;
    etpBottom:
      begin
        if FOwner.FPageControl.Flat then
          R.Bottom := R.Top + 1
        else
          R.Bottom := R.Top + 2;
        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          if FOwner.FPageControl.Flat then
            DrawEdge(Handle, R, BDR_RAISEDINNER, BF_BOTTOM)
          else
            DrawEdge(Handle, R, BDR_RAISED, BF_BOTTOM);
        end
        else
        begin
          if ASheet.ARect.Left > 0 then
          begin
            R1 := Rect(R.Left, R.Top, ASheet.ARect.Left + 1, R.Bottom);
            if FOwner.FPageControl.Flat then
              DrawEdge(Handle, R1, BDR_RAISEDINNER, BF_BOTTOM)
            else
              DrawEdge(Handle, R1, BDR_RAISED, BF_BOTTOM);
          end;
          if ASheet.ARect.Right < R.Right then
          begin
            R1 := Rect(ASheet.ARect.Right - 1, R.Top, R.Right, R.Bottom);
            if FOwner.FPageControl.Flat then
              DrawEdge(Handle, R1, BDR_RAISEDINNER, BF_BOTTOM)
            else
              DrawEdge(Handle, R1, BDR_RAISED, BF_BOTTOM);
          end;
        end;

        if FOwner.FPageControl.ShowBorder then
        begin
          Pixels[R.Left, R.Bottom - 1] := clBtnHighlight;
          Pixels[R.Left, R.Bottom - 2] := clBtnHighlight;
          if FOwner.FPageControl.Flat then
            Pixels[R.Right -1, R.Bottom - 2] := clBtnShadow
          else
            Pixels[R.Right -1, R.Bottom - 2] := cl3DDkShadow;
        end;
      end;
    etpLeft:
      begin
        Canvas.Pen.Color := clRed;
        Canvas.Pen.Color := clBtnHighlight;
        Dec(R.Bottom);

        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          MoveTo(R.Right - 1, R.Top);
          LineTo(R.Right - 1, R.Bottom);
        end
        else
        begin
          if ASheet.ARect.Top > 0 then
          begin
            MoveTo(R.Right - 1, R.Top);
            LineTo(R.Right - 1, ASheet.ARect.Top + 1);
          end;
          if ASheet.ARect.Bottom < R.Bottom then
          begin
            MoveTo(R.Right - 1, ASheet.ARect.Bottom + 1);
            LineTo(R.Right - 1, R.Bottom);
          end;
        end;
        if FOwner.FPageControl.ShowBorder then
        begin
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 1, R.Bottom - 1] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);
      end;
    etpRight:
      begin
        if FOwner.FPageControl.Flat then
          R.Right := R.Left + 1
        else
          R.Right := R.Left + 2;
        Dec(R.Bottom);

        if (ASheet = nil) or (not ASheet.AShown) then
        begin
          if FOwner.FPageControl.Flat then
            DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RIGHT)
          else
            DrawEdge(Handle, R, BDR_RAISED, BF_RIGHT);
        end
        else
        begin
          if ASheet.ARect.Top > 0 then
          begin
            R1 := Rect(R.Left, R.Top, R.Right, ASheet.ARect.Top + 1);
            if FOwner.FPageControl.Flat then
              DrawEdge(Handle, R1, BDR_RAISEDINNER, BF_RIGHT)
            else
              DrawEdge(Handle, R1, BDR_RAISED, BF_RIGHT);
          end;
          if ASheet.ARect.Bottom < R.Bottom then
          begin
            R1 := Rect(R.Left, ASheet.ARect.Bottom - 1, R.Right, R.Bottom);
            if FOwner.FPageControl.Flat then
              DrawEdge(Handle, R1, BDR_RAISEDINNER, BF_RIGHT)
            else
              DrawEdge(Handle, R1, BDR_RAISED, BF_RIGHT);
          end;
        end;

        if FOwner.FPageControl.ShowBorder then
        begin
          Pixels[R.Right - 2, R.Top] := clBtnHighlight;
          Pixels[R.Right - 1, R.Top] := clBtnHighlight;
          if not FOwner.FPageControl.Flat then
          begin
            Pixels[R.Right - 2, R.Bottom] := cl3DDkShadow;
            Pixels[R.Right - 1, R.Bottom] := cl3DDkShadow;
          end
          else
          begin
            Pixels[R.Right - 2, R.Bottom] := clBtnShadow;
            Pixels[R.Right - 1, R.Bottom] := clBtnShadow;
          end;
        end;
        inc(R.Bottom);

      end;
  end;
end;

procedure TElAngledTab.AdjustFillSize(After : boolean; var R : TRect; TabSheet 
    : TElTabSheet);
begin
  inherited;
  //if TabSheet = FOwner.FPageControl.FActivePage then
    case FOwner.FPageControl.TabPosition of
    etpTop:
      begin
        if After then
        begin
          dec(R.Right);
          dec(R.Bottom);
        end
        else
        begin
          inc(R.Right);
          inc(R.Bottom);
        end;
      end;
    etpBottom:
      begin
        if After then
        begin
          inc(R.Top);
          //inc(R.Bottom);
          //dec(R.Right);
        end
        else
        begin
          dec(R.Top);
          //dec(R.Bottom);
          // inc(R.Right);
        end;
      end;
    etpLeft:
      begin
        if After then
        begin
          //dec(R.Left);
          dec(R.Right);
          //dec(R.Bottom);
        end
        else
        begin
          //inc(R.Left);
          inc(R.Right);
          //inc(R.Bottom);
        end;
      end;
    etpRight:
      begin
        if After then
        begin
          inc(R.Left, 2);
          //dec(R.Right);
          //dec(R.Bottom);
        end
        else
          dec(R.Left, 2);
          //inc(R.Right);
          //inc(R.Bottom);
      end;
  (*
  end
  else
  begin
    case FOwner.FPageControl.TabPosition of
      etpBottom:
        begin
        end;
      etpRight:
        begin
        end;
    end;
  *)
  end;
end;

procedure TElAngledTab.AdjustDrawingSize(Active : boolean; var R : TRect);
var FPageControl : TElPageControl;
begin
  //if Active then
  begin
    FPageControl := FOwner.FPageControl;
    case FPageControl.TabPosition of
      etpLeft:
        begin
          //inc(R.Right);
          dec(R.Left, GetAscend);
          inc(R.Bottom, GetOuterMargin);
          dec(R.Top, GetOuterMargin);
        end;
      etpTop:
        begin
          //inc(R.Top);
          dec(R.Top, GetAscend);
          dec(R.Left, GetOuterMargin);
          inc(R.Right, GetOuterMargin);
        end;
      etpRight:
        begin
          //dec(R.Left);
          inc(R.Right, GetAscend);
          inc(R.Bottom, GetOuterMargin);
          dec(R.Top, GetOuterMargin);
        end;
      etpBottom:
        begin
          dec(R.Top);
          inc(R.Bottom, GetAscend);
          dec(R.Left, GetOuterMargin);
          inc(R.Right, GetOuterMargin);
        end;
    end;
  end;
end;

procedure TElAngledTab.AdjustTabSize(var Size : TSize);
begin
  if FOwner.FPageControl.TabPosition in [etpLeft, etpRight] then
  begin
    inc(Size.cy, 2 + AngledOffset);
    inc(Size.cx, 2);
  end
  else
  begin
    inc(Size.cx, 2 + AngledOffset);
    inc(Size.cy, 2);
  end;
end;

procedure TElAngledTab.FillTab(Canvas : TCanvas; Rect : TRect; TabSheet :
    TElTabSheet);
var ACtl   : TWinControl;
    BgRect : TRect;
    Points : array[0..4] of TPoint;
    r      : TRect;
    NRgn   : HRgn;
    pc     : integer;
begin
  R := Rect;
  AdjustFillSize(false, Rect, TabSheet);

  CreateTabPoints(R, @Points);
  pc := 4;
  if (TabSheet <> FOwner.FPageControl.FActivePage) and
     (TabSheet <> FOwner.FPageControl.FirstTab) then
  begin
    case FOwner.FPageControl.TabPosition of
      etpLeft:
        begin
          Points[3] := Point(R.Left + (R.Right - R.Left) div 2, R.Top + AngledOffset div 2);
          Points[4] := Point(R.Right, R.Top + AngledOffset - 1);
        end;
      etpTop:
        begin
          Points[0] := Point(R.Left + AngledOffset div 2, R.Top + (R.Bottom - R.Top) div 2);
          Points[4] := Point(R.Left + AngledOffset, R.Bottom);
        end;
      etpRight:
        begin
          Points[0] := Point(R.Left + (R.Right - R.Left - 1) div 2, R.Top + AngledOffset div 2);
          Points[4] := Point(R.Left, R.Top + AngledOffset);
        end;
      etpBottom:
        begin
          Points[0] := Point(R.Left + AngledOffset div 2, R.Top + (R.Bottom - R.Top) div 2);
          Points[4] := Point(R.Left + AngledOffset, R.Top);
        end;
    end;
    pc := 5;
  end;

  SaveDCState := SaveDC(Canvas.Handle);
  NRgn := CreatePolygonRgn(Points, pc, WINDING);
  SelectClipRgn(Canvas.Handle, NRgn);

  if (FOwner.FPageControl.FImgForm = nil) or (csDesigning in FOwner.FPageControl.ComponentState) then
  begin
    Canvas.Brush.Style := bsSolid;
    if TabSheet.UseTabColor then
      Canvas.Brush.Color := TabSheet.TabColor
    else
    if TabSheet = FOwner.FPageControl.FActivePage then
      Canvas.Brush.Color := FOwner.FPageControl.FActiveTabColor
    else
      Canvas.Brush.Color := FOwner.FPageControl.FInactiveTabColor;
    FillRgn(Canvas.Handle, NRgn, Canvas.Brush.Handle);
  end
  else
  if TabSheet = FOwner.FPageControl.FActivePage then
  begin
    if (FOwner.FPageControl.FImgForm.Control <> FOwner.FPageControl) then
    begin
      ACtl := FOwner.FPageControl.FImgForm.GetRealControl;

      BgRect.Left := FOwner.FPageControl.Left;
      BgRect.Top :=  FOwner.FPageControl.Top;

      BgRect.TopLeft := FOwner.FPageControl.Parent.ClientToScreen(BgRect.TopLeft);
      BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);

      FOwner.FPageControl.FImgForm.PaintBkgnd(Canvas.Handle, Rect, BgRect.TopLeft, false);
    end;
  end;
  DeleteObject(NRgn);
  AdjustFillSize(true, Rect, TabSheet);
end;

function TElAngledTab.GetAscend: Integer;
begin
  Result := 2;
end;

function TElAngledTab.GetInnerMargin: Integer;
begin
  Result := -5;
end;

function TElAngledTab.GetOuterMargin: Integer;
begin
  Result := 4;
end;

function TElAngledTab.GetContentMargin: Integer;
begin
  Result := AngledOffset;
end;

procedure TElAngledTab.CreateTabPoints(R : TRect; Points : PPoint);
begin
  case FOwner.FPageControl.TabPosition of
    etpLeft:
      begin
        Points^ := Point(r.right, r.bottom + 1);
        Inc(Points);
        Points^ := Point(r.left, r.bottom - AngledOffset + 1);
        Inc(Points);
        Points^ := Point(r.left, r.top + AngledOffset - 1);
        Inc(Points);
        Points^ := Point(r.right, r.top - 1);
      end;
    etpTop:
      begin
        Points^ := Point(r.left - 1, r.bottom);
        Inc(Points);
        Points^ := Point(r.left + AngledOffset - 1, r.top);
        Inc(Points);
        Points^ := Point(r.right -AngledOffset + 1, r.top);
        Inc(Points);
        Points^ := Point(r.right + 1, r.bottom);
      end;
    etpRight:
      begin
        Points^ := Point(r.left, r.top - 1);
        Inc(Points);
        Points^ := Point(r.right, r.top + AngledOffset - 1);
        Inc(Points);
        Points^ := Point(r.right, r.bottom - AngledOffset + 1);
        Inc(Points);
        Points^ := Point(r.left, r.bottom + 1);
      end;
    etpBottom:
      begin
        Points^ := Point(r.left - 1, r.top);
        Inc(Points);
        Points^ := Point(r.left + AngledOffset - 1, r.bottom);
        Inc(Points);
        Points^ := Point(r.right -AngledOffset + 1, r.bottom);
        Inc(Points);
        Points^ := Point(r.right + 1, r.top);
      end;
  end;
end;

procedure TElAngledTab.FixupTab(Canvas : TCanvas; R : TRect; TabSheet :
    TElTabSheet);
begin
  RestoreDC(Canvas.Handle, SaveDCState);
end;



end.
