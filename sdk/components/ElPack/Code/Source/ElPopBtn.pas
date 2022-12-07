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

07/03/2002

  Added Alignment property for the text. It is active for multiline captions only 

06/05/2002

  Added support for HTML-formatted captions

05/31/2002

  With older versions of VCL pressing F1 on buttons caused an AV if 
  there was no PullDownMenu assigned. Fixed.

04/28/2002

  Buttons didn't work correctly with ElPopupMenu as PulldownMenu if they didn't
  have an owner. Fixed. 

04/18/2002

  DrawFocusFrame property added

03/17/2002

  Fixed the AV that happened when ImageIsAlphaBlended was turned off for bitmaps

03/09/2002

  Fixed drawing of disabled images when manifest is included 
  Fixed popup menu handling for ElPopupMenu

03/06/2002

  Added unicode hint
  Fixed disabled image creation for images that use imagelist

02/23/2002

  Added support for 32-bit images with alpha blending (used in XP).
  Such images are supported on all OS, not just WinXP

02/17/2002

  Fixed alignment of the text with XP styles enabled

01/29/2002

  Improved positioning of text and glyphs in buttons

01/16/2002

  Now GraphicButton is drawn without accelerator

12/25/2001

  Fixed measurement of size in CLX version

12/21/2001

  DownBackground can now be used without Background

12/09/2001

  Fixed sizing of the buttons with MeasureButton with XP styles enabled

11/28/2001

  Added MoneyFlat and accompanying properties
  Fixed the problem with raised frame that stays after pull-down menu is hidden

11/24/2001

  Improved drawing of hot images when mouse enters and leaves the button

11/21/2001

  Removed flicker in TElPopupButton with XP styles enabled 

11/09/2001

  Transparency fixed
  
10/26/2001

  Fixed painting of background with XP styles enabled

09/24/2001

  Improved flat borders behaviour

09/17/2001

  Added Windows XP Themes Support 

09/13/2001

  Fixed position of menu that pulls up

07/26/2001

  Added Unicode support

07/25/2001

  Added AdjustSpaceForGlyph property to use new rectangle calculation 

07/22/2001

  Changed the drawing rectangle calculation to look the same as TSpeedButton and
  TBitBtn 

06/06/2001

  Small memory leak fixed in ElPopupButton and ElGraphicButton with UseArrow
  set to true
  
05/27/2001 (c) Akzhan Abdulin

    Fixed Color property design-time storing issue (clWindow not stored)

03/10/2001

  Fixed possible AVs that could happen when image list is removed.

  Minor optimizations and readness improvements.

1/7/2001

  NumGlyphs property is now set correctly

12/16/2000 

  Border colors calculation fixed for custom border.

============================== Version 2.76 ====================================

12/04/2000

  When the glyph of disabled button is drawn from the imagelist, specified by
  Images property, this glyph is grayed before drawing now.

  The glyph is not being offset when the image for Down state differs from
  the image for normal state

============================== Version 2.75 ====================================

10/31/2000

  When the color is changed, the button is repaintàed. 

============================== Version 2.74 ====================================

09/23/2000

  When ElPopupButton.ImageList is assigned, UseImageList wasn't taken into
  account. Fixed.
  Added DownImages property for ElPopupButton and ElGraphicButton

09/14/2000

  ShowBorder property added to ElPopupButton and ElGraphicButton

*)

unit ElPopBtn;

interface

{$R 'ElPopBtn.res'}

uses
  SysUtils,
  Classes,
  TypInfo,
  ElBtnCtl,
  ElTmSchema,
  ElUxTheme,

{$ifndef CLX_USED}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Buttons,
  CommCtrl,
  Menus,
  ExtCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
{$ifdef VCL_4_USED}
  ImgList,
{$endif}
{$IFDEF VCL_4_USED}
  ActnList,
{$ENDIF}
{$else}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  Types,
  QTypes,
  Qt,
  QGraphics,
  QControls,
  QForms,
  QButtons,
  QImgList,
  ElCLXUtils,
  QMenus,
  QExtCtrls,
  QActnList,
{$endif}
  ElVCLUtils,
{$IFDEF HAS_HTML_RENDER}
  HTMLRender,
{$ENDIF}
{$ifdef HAS_HTML_RENDER}
  ElHandPt,
{$endif}
  ElTools,
  ElList,
  ElSndMap,
  ElStrUtils,
  ElImgFrm;

type
  TPullDownEvent = procedure(Sender : TObject) of object;

  TElButtonState = (ebsUp, ebsDisabled, ebsDown, ebsExclusive, ebsArrDown);

  TElButtonGlyph = class;

  {$ifndef CLX_USED}
  TElSpeedButton = class(TGraphicControl)
  private
    FTransparentColor : TColor;
    FAutoSize : Boolean;
    FNormalImage : TBitmap;
    FDisabledImage : TBitmap;
    FMouseInImage : TBitmap;
    FPressedImage : TBitmap;
    FFlat : Boolean;
    FDrawEdge : Boolean;
    FPressed : boolean;
    FOver : boolean;
    FPullTimer : TTimer;
    FPullDownBtn : TMouseButton;
    FPullDownInterval : Integer;
    FPullDownEnabled : Boolean;
    FPullDownMenu : TPopupMenu;
    FTransparent : Boolean;
    FOnPullDown : TPullDownEvent;
    procedure SetPullDownMenu(newValue : TPopupMenu);
    procedure SetTransparent(newValue : Boolean);
    procedure SetDrawEdge(newValue : Boolean);

    procedure WMMouseMove(var Msg : TWMMouseMove); message WM_MOUSEMOVE;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;

    procedure SetFlat(newValue : Boolean);
    procedure SetNormalImage(newValue : TBitmap);
    procedure SetDisabledImage(newValue : TBitmap);
    procedure SetMouseInImage(newValue : TBitmap);
    procedure SetPressedImage(newValue : TBitmap);
    procedure SetTransparentColor(newValue : TColor);
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure SetAutoSize(newValue : boolean); {$ifdef VCL_6_USED} override; {$endif}
    procedure TriggerPullDownEvent; virtual;
    procedure Paint; override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure OnTimer(Sender : TObject);
    procedure StartTimer;
    procedure PullMenu;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function InCircle(X, Y : integer) : Boolean; virtual;
  published
    property PullDownBtn : TMouseButton read FPullDownBtn write FPullDownBtn; { Published }
    property PullDownInterval : Integer read FPullDownInterval write FPullDownInterval default 1000; { Published }
    property PullDownEnabled : Boolean read FPullDownEnabled write FPullDownEnabled default false; { Published }
    property PullDownMenu : TPopupMenu read FPullDownMenu write SetPullDownMenu; { Published }
    property Transparent : Boolean read FTransparent write SetTransparent default true; { Published }
    property OnPullDown : TPullDownEvent read FOnPullDown write FOnPullDown;
    property DrawEdge : Boolean read FDrawEdge write SetDrawEdge default False; { Published }
    property Flat : Boolean read FFlat write SetFlat; { Published }
    property NormalImage : TBitmap read FNormalImage write SetNormalImage; { Published }
    property DisabledImage : TBitmap read FDisabledImage write SetDisabledImage; { Published }
    property MouseInImage : TBitmap read FMouseInImage write SetMouseInImage; { Published }
    property PressedImage : TBitmap read FPressedImage write SetPressedImage; { Published }
    property AutoSize : Boolean read FAutoSize write SetAutoSize default true; { Published }
    property TransparentColor : TColor read FTransparentColor write SetTransparentColor; { Published }

    //VCL properties
    property Align;
    property Color nodefault;
    property DragCursor;
    property DragMode;
    property ParentColor;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnDragOver;
    property OnDragDrop;
{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    property DockOrientation;
    property Floating;
    property DragKind;
{$ENDIF}
  end; { TElSpeedButton }
  {$endif}

  TPopupPlace = (ppDown, ppRight{$ifndef CLX_USED}, ppTop{$endif});

{$WARNINGS OFF}
  TCustomElPopupButton = class(TElButtonControl)
  protected
    FNumGlyphs: Integer;
    {$IFDEF HAS_HTML_RENDER}
    FCursor: TCursor;
    {$endif}
    FShadowsUseCustom : boolean;
    FShadowBtnHighlight,
    FShadowBtnShadow,
    FShadowBtnDkShadow : TColor;
    FBackground : TBitmap;
    FShadowFollowsColor : boolean;
    FDownBackground : TBitmap;
    FBackgroundDrawBorder : boolean;
    FThinFrame : Boolean;
    FHotImages      : TImageList;
    FDisabledImages : TImageList;
    FDownImages     : TImageList;
    FImageList      : TImageList;
    FOldStyled : Boolean;
    FUseImageList : boolean;
    FUseIcon : Boolean;
{$IFDEF USE_SOUND_MAP}
    FSoundMap : TElSoundMap;
{$ENDIF}
    FDownSound : TElSoundName;
    FUpSound : TElSoundName;
    FClickSound : TElSoundName;
    FArrowClickSound : TElSoundName;
    FIsSwitch : Boolean;
    FShowGlyph : Boolean;
    FShowText : Boolean;
    FUseArrow : Boolean;
    FShowFocus : Boolean;
    FMultiLine : Boolean;                        
    FGroupIndex : Integer;
    FGlyph : TElButtonGlyph;
    FDown : Boolean;
    FArrDown : boolean;
    FInMenu,
    FIgnoreClick  : boolean;
    FDragging : Boolean;
    FAllowAllUp : Boolean;
    FLayout : TButtonLayout;
    FSpacing : Integer;
    FMargin : Integer;
    FFlat : Boolean;
    FMouseInArrow,
    FMouseInControl : Boolean;
    FDisableAp : boolean;
    FPopupPlace : TPopupPlace;
    FDefault : Boolean;
    FCancel : Boolean;
    FActive : Boolean;
    FModalResult : TModalResult;
    FClicksDisabled : Boolean;
    FChLink,
    FNChLink,
    FDChLink,
    FHChLink: TChangeLink;
    FPullDownMenu : TPopupMenu;
    FOnArrowClick : TNotifyEvent;
    {$ifndef CLX_USED}
    FImgForm : TElImageForm;
    FImgFormChLink  : TImgFormChangeLink;
    {$endif}
    FShowBorder   : boolean;
    FAdjustSpaceForGlyph: Boolean;
    FArrTheme: HTheme;
    {$ifdef HAS_HTML_RENDER}
    FIsHTML  : Boolean;
    FRender  : TElHTMLRender;
    {$endif}
    {$IFDEF HAS_HTML_RENDER}
    FOnImageNeeded: TElHTMLImageNeededEvent;
    FOnLinkClick: TElHTMLLinkClickEvent;
    {$endif}
    FOrigState,
    FState : TElButtonState;
    FDrawDefaultFrame: Boolean;
    FImageIsAlphaBlended: Boolean;
    FDrawFocusFrame: Boolean;
    {$IFDEF HAS_HTML_RENDER}
    FLinkColor: TColor;
    FLinkPopupMenu: TPopupMenu;
    FLinkStyle: TFontStyles;
    {$endif}
    FTextRect: TRect;
    FChangeDisabledText: Boolean;
    FAlignment: TAlignment;

    procedure SetAlignment(Value: TAlignment);
{$ifdef HAS_HTML_RENDER}
    procedure SetIsHTML(Value: Boolean);
{$endif}
    procedure SetShowBorder(newValue : boolean);
    {$ifndef CLX_USED}
    procedure ImageFormChange(Sender : TObject);
    procedure SetImageForm(newValue : TElImageForm);
    {$endif}
    procedure SetPullDownMenu(newValue : TPopupMenu);
    procedure SetDefault(Value : Boolean);
    procedure SetPopupPlace(Value : TPopupPlace);
    procedure SetDisableAp(Value : boolean);
    procedure GlyphChanged(Sender : TObject);
    procedure UpdateExclusive;
    function GetGlyph : TBitmap;
    procedure SetGlyph(Value : TBitmap);
    function GetNumGlyphs : TNumGlyphs;
    procedure SetNumGlyphs(Value : TNumGlyphs);
    procedure SetDown(Value : Boolean);
    procedure SetAllowAllUp(Value : Boolean);
    procedure SetGroupIndex(Value : Integer);
    procedure SetLayout(Value : TButtonLayout);
    procedure SetSpacing(Value : Integer);
    procedure SetMargin(Value : Integer);
    procedure UpdateTracking;
    procedure IntMouseEnter;
    procedure IntMouseLeave;
    procedure IntEnabledChanged;
    function IntKeyDown(var Key: Word; Shift: TShiftState) : boolean;
    procedure IntKeyUp(var Key: Word; Shift: TShiftState);
    procedure IntTextChanged;
    {$ifdef CLX_USED}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    function WidgetFlags: Integer; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure ButtonPressed(Sender: TCustomElPopupButton; GroupIndex: Integer); virtual;
    procedure TextChanged; override;
    procedure EnabledChanged; override;
    {$endif}
    {$ifndef CLX_USED}
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;

    procedure WMKeyDown(var Message : TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TWMKeyUp); message WM_KEYUP;
    procedure WMLButtonDblClk(var Message : TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message : TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message : TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogKey(var Message : TCMDialogKey); message CM_DIALOGKEY;
    procedure CMFontChanged(var Message : TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message : TMessage); message CM_TEXTCHANGED;
    procedure CMColorChanged(var Message : TMessage); message CM_COLORCHANGED;
    procedure CMSysColorChange(var Message : TMessage); message CM_SYSCOLORCHANGE;
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
    procedure CMFocusChanged(var Message : TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CNCommand(var Message : TWMCommand); message CN_COMMAND;
    {$endif}
    procedure SetShowFocus(newValue : Boolean);
    procedure SetShowGlyph(newValue : Boolean);
    procedure SetShowText(newValue : Boolean);
    function GetIcon : TIcon;
    procedure SetIcon(newValue : TIcon);
    procedure SetIsSwitch(newValue : Boolean);
{$IFDEF USE_SOUND_MAP}
    procedure SetSoundMap(newValue : TElSoundMap);
{$ENdIF}
    procedure SetImageIndex(newValue : Integer);
    function GetImageIndex : integer;
    procedure SetUseIcon(newValue : Boolean);
    procedure SetImageList(newValue : TImageList);
    procedure SetUseImageList(newValue : Boolean);
    function GetUseImageList : boolean;
    procedure SetOldStyled(newValue : Boolean);
    procedure SetHotImages(newValue : TImageList);
    procedure SetDownImages(newValue : TImageList);
    procedure SetDisabledImages(newValue : TImageList);
    procedure ImagesChanged(Sender : TObject);
    procedure SetThinFrame(newValue : Boolean);
    procedure SetBackground(newValue : TBitmap);
    procedure SetDownBackground(newValue : TBitmap);
    procedure SetBackgroundDrawBorder(Value : boolean);
    procedure SetShadowFollowsColor(Value : Boolean);
    procedure SetShadowsUseCustom(Value : Boolean);
    procedure SetShadowBtnHighlight(Value : TColor);
    procedure SetShadowBtnShadow(Value : TColor);
    procedure SetShadowBtnDkShadow(Value : TColor);
    procedure SetAdjustSpaceForGlyph(Value: Boolean);
    {$IFDEF HAS_HTML_RENDER}
    procedure SetLinkPopupMenu(newValue : TPopupMenu);
    procedure SetLinkColor(newValue : TColor); virtual;
    procedure SetLinkStyle(newValue : TFontStyles); virtual;
    procedure DoLinkPopup(MousePos : TPoint);
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image
        : TBitmap);
    procedure TriggerLinkClickEvent(HRef : TElFString); virtual;
    {$endif}
    procedure SetUseArrow(newValue : boolean); virtual;

    procedure CreateThemeHandle; override;
    procedure FreeThemeHandle; override;

    {$ifndef CLX_USED}
    function GetPalette : HPALETTE; override;
    {$endif}
    procedure Loaded; override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer); override;
    procedure Paint; override;
    {$ifndef CLX_USED}
    procedure CreateParams(var Params : TCreateParams); override;
    {$endif}
    {$ifndef CLX_USED}
    procedure CreateWnd; override;
    {$else}
    procedure CreateWidget; override;
    {$endif}
    {$ifndef CLX_USED}
    procedure SetButtonStyle(ADefault : Boolean); virtual;
    procedure WndProc(var Message : TMessage); override;
    {$endif}
    procedure SetFlat(Value : Boolean); virtual;
    function GetChecked : Boolean; override;
    procedure SetChecked(newValue : Boolean); override;
{$IFDEF VCL_4_USED}
    function GetActionLinkClass : TControlActionLinkClass; override;
    procedure ActionChange(Sender : TObject; CheckDefaults : Boolean); override;
{$ENDIF}

    {$ifndef CLX_USED}
    {$ifdef VCL_5_USED}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$endif}
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    {$else}
    procedure DoContextPopup(const MousePos: TPoint; var Handled: Boolean); override;
    {$endif}

    property ClicksDisabled : Boolean read FClicksDisabled write FClicksDisabled;
    procedure Notification(AComponent : TComponent; operation : TOperation); override;

    procedure SetDrawDefaultFrame(Value: Boolean);
    function GetArrowSize: Integer; virtual;
    function DoSaveShadows: Boolean;
    procedure SetImageIsAlphaBlended(Value: Boolean);
    procedure SetDrawFocusFrame(Value: Boolean);
    {$IFDEF HAS_HTML_RENDER}
    procedure SetCursor(Value: TCursor); virtual;
    {$endif}
    procedure SetChangeDisabledText(Value: Boolean);

    property PullDownMenu : TPopupMenu read FPullDownMenu write SetPullDownMenu; { Published }
    property PopupPlace : TPopupPlace read FPopupPlace write SetPopupPlace default ppDown;
    property DisableAutoPopup : boolean read FDisableAp write SetDisableAp default false;
    property Cancel : Boolean read FCancel write FCancel default False;
    property Default : Boolean read FDefault write SetDefault default False;
    property ModalResult : TModalResult read FModalResult write FModalResult default 0;
    property AllowAllUp : Boolean read FAllowAllUp write SetAllowAllUp default False;
    property GroupIndex : Integer read FGroupIndex write SetGroupIndex default 0;
    property Down : Boolean read FDown write SetDown default False;
    property Flat : Boolean read FFlat write SetFlat default False;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property Layout : TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin : Integer read FMargin write SetMargin default -1;
    property NumGlyphs : TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property Spacing : Integer read FSpacing write SetSpacing default 4;
    property ShowFocus : Boolean read FShowFocus write SetShowFocus default true;
    property UseArrow : Boolean read FUseArrow write SetUseArrow default False;
    property ShadowFollowsColor : boolean read FShadowFollowsColor write SetShadowFollowsColor default true;
    property ShowGlyph : Boolean read FShowGlyph write SetShowGlyph default true;
    property ShowText : Boolean read FShowText write SetShowText default true;
    property OnArrowClick : TNotifyEvent read FOnArrowClick write FOnArrowClick;
    property Icon : TIcon read GetIcon write SetIcon;
    property UseIcon : Boolean read FUseIcon write SetUseIcon default False;
    property IsSwitch : Boolean read FIsSwitch write SetIsSwitch default False;
    property DownSound : TElSoundName read FDownSound write FDownSound;
    property UpSound : TElSoundName read FUpSound write FUpSound;
    property ClickSound : TElSoundName read FClickSound write FClickSound; { Published }
    property ArrowClickSound : TElSoundName read FArrowClickSound write FArrowClickSound; { Published }
{$IFDEF USE_SOUND_MAP}
    property SoundMap : TElSoundMap read FSoundMap write SetSoundMap;
{$ENDIF}
    property ImageIndex : Integer read GetImageIndex write SetImageIndex default -1;
    property Images : TImageList read FImageList write SetImageList;
    property DownImages : TImageList read FDownImages write SetDownImages;
    property HotImages : TImageList read FHotImages write SetHotImages;
    property DisabledImages : TImageList read FDisabledImages write SetDisabledImages;
    {$ifndef CLX_USED}
    property ImageForm      : TElImageForm read FImgForm write SetImageForm;
    {$endif}
    property ShowBorder     : boolean read FShowBorder write SetShowBorder default True;

    property ShadowsUseCustom  : boolean read FShadowsUseCustom write SetShadowsUseCustom default False;
    property ShadowBtnHighlight: TColor read FShadowBtnHighlight write SetShadowBtnHighlight stored DoSaveShadows;
    property ShadowBtnShadow   : TColor read FShadowBtnShadow write   SetShadowBtnShadow stored DoSaveShadows;
    property ShadowBtnDkShadow : TColor read FShadowBtnDkShadow write SetShadowBtnDkShadow stored DoSaveShadows;

    property UseImageList : Boolean read GetUseImageList write SetUseImageList default False;
    property OldStyled : Boolean read FOldStyled write SetOldStyled default False;
    property ThinFrame : Boolean read FThinFrame write SetThinFrame default False;
    property Background : TBitmap read FBackground write SetBackground;
    property DownBackground : TBitmap read FDownBackground write SetDownBackground;  { Protected }
    property BackgroundDrawBorder : boolean read FBackgroundDrawBorder write SetBackgroundDrawBorder default False;
    property AdjustSpaceForGlyph: Boolean read FAdjustSpaceForGlyph write 
        SetAdjustSpaceForGlyph default true;
    property DrawDefaultFrame: Boolean read FDrawDefaultFrame write 
        SetDrawDefaultFrame;
    property DrawFocusFrame: Boolean read FDrawFocusFrame write SetDrawFocusFrame
        default false;
    property ImageIsAlphaBlended: Boolean read FImageIsAlphaBlended write
        SetImageIsAlphaBlended default false;
    property ChangeDisabledText: Boolean read FChangeDisabledText write 
        SetChangeDisabledText default true;
{$ifdef HAS_HTML_RENDER}
    property IsHTML : Boolean read FIsHTML write SetIsHTML default false;
    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        FOnImageNeeded;
    property OnLinkClick: TElHTMLLinkClickEvent read FOnLinkClick write
        FOnLinkClick;
    property LinkColor: TColor read FLinkColor write SetLinkColor default clBlue;
    property LinkPopupMenu: TPopupMenu read FLinkPopupMenu write SetLinkPopupMenu;
    property LinkStyle: TFontStyles read FLinkStyle write SetLinkStyle;
    property Cursor: TCursor read FCursor write SetCursor;
{$endif}
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AClick(Arrow : boolean); virtual;
    procedure Click; override;
    property MouseInControl : Boolean read FMouseInControl;
  end;
{$WARNINGS ON}

  TElPopupButton = class(TCustomElPopupButton)
  protected
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
  published
    property Alignment;
    property Background;
    property BackgroundDrawBorder;
    property DownBackground;
    property ImageIndex;
    property UseImageList;
    property ImageIsAlphaBlended;
    property Images;
    property HotImages;
    property DisabledImages;
    property DrawDefaultFrame;
    property DrawFocusFrame;
    property PullDownMenu;
    property PopupPlace;
    property DisableAutoPopup;
    property Cancel;
    property Default;
    property ModalResult;
    {$ifndef CLX_USED}
    property MoneyFlat;
    property MoneyFlatActiveColor;
    property MoneyFlatInactiveColor;
    property MoneyFlatDownColor;
    {$endif}
    property AdjustSpaceForGlyph;
    property AllowAllUp;
    property GroupIndex;
    property Down;
    property Flat;
    property Glyph;
    {$ifndef CLX_USED}
    property ImageForm;
    {$endif}
{$ifdef HAS_HTML_RENDER}
    property IsHTML;
{$endif}
    {$ifdef HAS_HTML_RENDER}
    property LinkColor;
    property LinkPopupMenu;
    property LinkStyle;
    {$endif}
    property Layout;
    property Margin;
    property NumGlyphs;
    property ShadowFollowsColor;
    property ShadowsUseCustom;
    property ShadowBtnHighlight;
    property ShadowBtnShadow;
    property ShadowBtnDkShadow;

    property ShowFocus;
    property ShowGlyph;
    property ShowText;
    property Spacing;
    property UseArrow;
    property IsSwitch;
    property OnArrowClick;
    property Icon;
    property UseIcon;
    property ThinFrame;
    property TextDrawType;
    property Transparent;
    property DownSound;
    property UpSound;
    property ClickSound;
    property ArrowClickSound;
{$IFDEF USE_SOUND_MAP}
    property SoundMap;
{$ENDIF}
    property DownImages;
    property ShowBorder;
    property OldStyled;
    property UseXPThemes;

    // VCL properties
    property Caption;
    property Cursor;
    property Enabled;
    property TabStop default True;
    property TabOrder;
    property PopupMenu;
    property Color;
    property ParentColor;
    property Align;
    property Font;
    property HelpContext;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    {$IFDEF HAS_HTML_RENDER}
    property OnImageNeeded;
    property OnLinkClick;
    {$endif}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;

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

    property OnStartDock;
    property OnEndDock;
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
    {$endif}
  end;

{$IFDEF VCL_4_USED}
  TElPopupButtonActionLink = class(TElButtonActionlink)
  protected
    procedure SetImageIndex(Value: Integer); override;
    procedure SetChecked(Value : boolean); override;
  end;
{$ENDIF}

  TElGlyphList = class(TImageList)
  private
    Used : TBits;
    FCount : Integer;
    function AllocateIndex : Integer;
  public
    constructor CreateSize(AWidth, AHeight : Integer);
    destructor Destroy; override;
    function AddMasked(Image : TBitmap; MaskColor : TColor) : Integer;
    procedure Delete(Index : Integer);
    property Count : Integer read FCount;
  end;

  TElGlyphCache = class
  private
    GlyphLists : TElList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight : Integer) : TElGlyphList;
    procedure ReturnList(List : TElGlyphList);
    function Empty : Boolean;
  end;

  TElButtonGlyph = class
  private
    FImageList : TImageList;
    FImageIndex : Integer;
    FUseImageList : Boolean;
    FIcon : TIcon;
    FUseIcon : Boolean;
    FOriginal : TBitmap;
    FGlyphList : TElGlyphList;
    FIndexs : array[TElButtonState] of Integer;
    FTransparentColor : TColor;
    FNumGlyphs : TNumGlyphs;
    FOnChange : TNotifyEvent;
    FStretched : boolean;
    FStrW,
      FStrH : integer;
    //procedure ImageListChanged(Sender : TObject);
    procedure SetImageList(NewValue  : TImageList);
    procedure SetImageIndex(NewValue : Integer);
    procedure GlyphChanged(Sender : TObject);
    procedure IconChanged(Sender : TObject);
    procedure SetGlyph(Value : TBitmap);
    procedure SetNumGlyphs(Value : TNumGlyphs);
    procedure Repaint;
    function CreateButtonGlyph(State : TElButtonState) : Integer;
    procedure DrawButtonGlyph(Canvas : TCanvas; const GlyphPos : TPoint; State :
        TElButtonState; Transparent : Boolean; Color : TColor; AlphaBlended : 
        boolean);
    procedure DrawButtonText(Canvas : TCanvas; const Caption : TElFString; 
        TextBounds : TRect; State : TElButtonState; Multiline : boolean; Alignment 
        : TAlignment; Active, Transparent : boolean; TextDrawType : TElTextDrawType;
        UseThemesForText : boolean; Theme : HTheme; ThemePart, ThemeState : integer;
        ShowAccelChar : boolean {$ifdef HAS_HTML_RENDER}; IsHTML : boolean; 
        HTMLRender : TElHTMLRender{$endif} ; ChangeDisabledText : boolean);
    procedure CalcButtonLayout(Canvas : TCanvas; const Client : TRect; const Offset
        : TPoint; const Caption : TElFString; Layout : TButtonLayout; Margin, 
        Spacing : Integer; var GlyphPos : TPoint; var TextBounds : TRect; ShowGlyph,
        ShowText, MultiLine : boolean; ArrowWidth : integer; UseThemesForText : 
        boolean; Theme : HTheme; ThemePart, ThemeState : integer
        {$ifdef HAS_HTML_RENDER}; IsHTML : boolean; HTMLRender : TElHTMLRender{$endif}
        );
    function GetGlyphSize : TRect;
    procedure SetUseIcon(NewValue : boolean);
  protected
    property ImageList : TImageList read FImageList write SetImageList;
    property ImageIndex : Integer read FImageIndex write SetImageIndex default -1;
    property UseImageList : Boolean read FUseImageList write FUseImageList;
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    procedure ResetNumGlyphs;

    function Draw(Canvas : TCanvas; const Client : TRect; const Offset : TPoint;
        const Caption : TElFString; Layout : TButtonLayout; Margin, Spacing :
        Integer; State, GlyphState : TElButtonState; Alignment : TAlignment;
        Transparent : Boolean; Multiline : boolean; Active, ShowGlyph,
        ShowText : boolean; ArrowWidth : integer; TextDrawType : TElTextDrawType;
        Color : TColor; UseThemesForText : boolean; Theme : HTheme;
        ThemePart, ThemeState : integer; ShowAccelChar : boolean;
        ImageIsAlphaBlended : boolean
        {$ifdef HAS_HTML_RENDER}; IsHTML : boolean; HTMLRender : TElHTMLRender{$endif}
        ; ChangeDisabledText : boolean
        ) : TRect;
    procedure GetPaintGlyphSize(R : TRect; var Size : TPoint);
    function CalcButtonWidth(Canvas : TCanvas; var MaxHeight : integer; const 
        Offset : TPoint; const Caption : TElFString; Layout : TButtonLayout; Margin,
        Spacing : Integer; ShowGlyph, ShowText, MultiLine : boolean; ArrowWidth : 
        integer; UseThemesForText : boolean; Theme : HTheme; ThemePart, ThemeState 
        : integer{$ifdef HAS_HTML_RENDER}; IsHTML : boolean; HTMLRender : TElHTMLRender{$endif}): Integer;
    property UseIcon : boolean read FUseIcon write SetUseIcon;
    property Icon : TIcon read FIcon;
    property Glyph : TBitmap read FOriginal write SetGlyph;
    property NumGlyphs : TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property GlyphSize : TRect read GetGlyphSize;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

{$WARNINGS OFF}
  TCustomElGraphicButton = class(TGraphicControl)
  protected
    FNumGlyphs: Integer;
    {$ifndef CLX_USED}
    FMenuWindowProc: TWndMethod;
    {$endif}
    {$IFDEF HAS_HTML_RENDER}
    FCursor: TCursor;
    {$endif}
    FShadowsUseCustom : boolean;
    FShadowBtnHighlight,
    FShadowBtnShadow,
    FShadowBtnDkShadow : TColor;
    FBackground : TBitmap;
    FShadowFollowsColor : boolean;
    FDownBackground : TBitmap;
    FBackgroundDrawBorder : boolean;
    FThinFrame : Boolean;
    FHotImages,
    FDownImages,
    FDisabledImages,
    FImageList : TImageList;
    FOldStyled : Boolean;
    FUseImageList : boolean;
    FUseIcon : Boolean;
{$IFDEF USE_SOUND_MAP}
    FSoundMap : TElSoundMap;
{$ENDIF}
    FDownSound : TElSoundName;
    FUpSound : TElSoundName;
    FClickSound : TElSoundName;
    FArrowClickSound : TElSoundName;
    FIsSwitch : Boolean;
    FShowGlyph : Boolean;
    FShowText : Boolean;
    FUseArrow : Boolean;
    FMultiLine : Boolean;
    FGroupIndex : Integer;
    FGlyph : TElButtonGlyph;
    FDown : Boolean;
    FArrDown : boolean;
    FInMenu,
    FIgnoreClick  : boolean;
    FDragging : Boolean;
    FAllowAllUp : Boolean;
    FLayout : TButtonLayout;
    FSpacing : Integer;
    FMargin : Integer;
    FFlat : Boolean;
    FMouseInArrow,
    FMouseInControl : Boolean;
    FDisableAp : boolean;
    FPopupPlace : TPopupPlace;
    FDefault : Boolean;
    FCancel : Boolean;
    FModalResult : TModalResult;
    FClicksDisabled : Boolean;
    FChLink,
    FNChLink,
    FDChLink,
    FHChLink: TChangeLink;
    FPullDownMenu : TPopupMenu;
    FOnArrowClick : TNotifyEvent;
    FTransparent  : boolean;
    FTextDrawType : TElTextDrawType;
    {$ifndef CLX_USED}
    FImgForm      : TElImageForm;
    FImgFormChLink  : TImgFormChangeLink;
    {$endif}
    FShowBorder   : boolean;
    FAdjustSpaceForGlyph: Boolean;
    {$IFDEF HAS_HTML_RENDER}
    FLinkColor: TColor;
    FLinkPopupMenu: TPopupMenu;
    FLinkStyle: TFontStyles;
    {$endif}
    {$ifdef HAS_HTML_RENDER}
    FIsHTML  : Boolean;
    FRender  : TElHTMLRender;
    {$endif}
    {$IFDEF HAS_HTML_RENDER}
    FOnImageNeeded: TElHTMLImageNeededEvent;
    FOnLinkClick: TElHTMLLinkClickEvent;
    {$endif}

    FUseXPThemes: Boolean;
    FTheme: HTheme;
    FArrTheme: HTheme;
    {$ifndef CLX_USED}
    FWnd: HWND;
    {$endif}
    FAlignment: TAlignment;

    procedure SetAlignment(Value: TAlignment);
{$ifdef HAS_HTML_RENDER}
    procedure SetIsHTML(Value: Boolean);
{$endif}
    function  IsColorStored : boolean;
    procedure SetShowBorder(newValue : boolean);
    {$ifndef CLX_USED}
    procedure ImageFormChange(Sender : TObject);
    procedure SetImageForm(newValue : TElImageForm);
    {$endif}
    procedure SetTransparent(newValue : Boolean); virtual;
    procedure SetTextDrawType(newValue : TElTextDrawType);
    procedure SetPullDownMenu(newValue : TPopupMenu);

    procedure SetPopupPlace(Value : TPopupPlace);
    procedure SetDisableAp(Value : boolean);
    procedure GlyphChanged(Sender : TObject);
    procedure UpdateExclusive;
    function GetGlyph : TBitmap;
    procedure SetGlyph(Value : TBitmap);
    function GetNumGlyphs : TNumGlyphs;
    procedure SetNumGlyphs(Value : TNumGlyphs);
    procedure SetDown(Value : Boolean);
    procedure SetAllowAllUp(Value : Boolean);
    procedure SetGroupIndex(Value : Integer);
    procedure SetLayout(Value : TButtonLayout); virtual;
    procedure SetSpacing(Value : Integer); virtual;
    procedure SetMargin(Value : Integer); virtual;
    procedure UpdateTracking;

    procedure IntMouseEnter;
    procedure IntMouseLeave;
    procedure IntEnabledChanged;
    procedure IntTextChanged;

    {$ifdef CLX_USED}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ButtonPressed(Sender: TCustomElGraphicButton; GroupIndex: Integer); virtual;
    procedure TextChanged; override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    {$endif}

    {$ifndef CLX_USED}
    procedure CMEnabledChanged(var Message : TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message : TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogKey(var Message : TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message : TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message : TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message : TMessage); message CM_TEXTCHANGED;
    procedure CMColorChanged(var Message : TMessage); message CM_COLORCHANGED;
    procedure CMSysColorChange(var Message : TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    {$endif}

    procedure SetShowGlyph(newValue : Boolean); virtual;
    procedure SetShowText(newValue : Boolean); virtual;
    function GetIcon : TIcon;
    procedure SetIcon(newValue : TIcon);
    procedure SetIsSwitch(newValue : Boolean);
{$IFDEF USE_SOUND_MAP}
    procedure SetSoundMap(newValue : TElSoundMap);
{$ENDIF}
    procedure SetImageIndex(newValue : Integer); virtual;
    function GetImageIndex : integer;
    procedure SetUseIcon(newValue : Boolean); virtual;
    procedure SetImageList(newValue : TImageList); virtual;
    procedure SetUseImageList(newValue : Boolean); virtual;
    function GetUseImageList : boolean;
    procedure SetOldStyled(newValue : Boolean);
    procedure SetDownImages(newValue : TImageList);
    procedure SetHotImages(newValue : TImageList);
    procedure SetDisabledImages(newValue : TImageList);
    procedure ImagesChanged(Sender : TObject); virtual;
    procedure SetThinFrame(newValue : Boolean);
    procedure SetBackground(newValue : TBitmap);
    procedure SetDownBackground(newValue : TBitmap);
    procedure SetBackgroundDrawBorder(Value : boolean);
    procedure SetShadowFollowsColor(Value : Boolean);
    procedure SetShadowsUseCustom(Value : Boolean);
    procedure SetShadowBtnHighlight(Value : TColor);
    procedure SetShadowBtnShadow(Value : TColor);
    procedure SetShadowBtnDkShadow(Value : TColor);
    procedure SetAdjustSpaceForGlyph(Value: Boolean);
    procedure SetUseXPThemes(const Value: Boolean);

    procedure CreateThemeHandle;
    procedure FreeThemeHandle;

  protected
    FOrigState,
    FState : TElButtonState;
    FCaption: TElFString;

    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    FMoneyFlat: Boolean;
    FMoneyFlatDownColor: TColor;
    FMoneyFlatActiveColor: TColor;
    FMoneyFlatInactiveColor: TColor;
    FShortcutsEnabled: Boolean;
    FImageIsAlphaBlended: Boolean;
    FTextRect: TRect;
    FChangeDisabledText: Boolean;

    {$IFDEF HAS_HTML_RENDER}
    procedure SetLinkPopupMenu(newValue : TPopupMenu);
    procedure SetLinkColor(newValue : TColor); virtual;
    procedure SetLinkStyle(newValue : TFontStyles); virtual;
    procedure DoLinkPopup(MousePos : TPoint);
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image
        : TBitmap);
    procedure TriggerLinkClickEvent(HRef : TElFString); virtual;
    {$endif}

    {$ifndef CLX_USED}
    {$ifdef VCL_5_USED}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$endif}
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    {$else}
    procedure DoContextPopup(const MousePos: TPoint; var Handled: Boolean); override;
    {$endif}

    procedure SetUseArrow(newValue : boolean); virtual;
    {$ifndef CLX_USED}
    function GetPalette : HPALETTE; override;
    {$endif}
    procedure Loaded; override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer); override;
    procedure Paint; override;
    {$ifndef CLX_USED}
    procedure WndProc(var Message : TMessage); override;
    {$endif}
    procedure SetFlat(Value : Boolean); virtual;
    function GetChecked : Boolean; virtual;
    procedure SetChecked(newValue : Boolean); virtual;
{$IFDEF VCL_4_USED}
    function GetActionLinkClass : TControlActionLinkClass; override;
    procedure ActionChange(Sender : TObject; CheckDefaults : Boolean); override;
{$ENDIF}
    function DoSaveShadows : boolean;

    {$ifdef ELPACK_UNICODE}
    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}

    procedure Notification(AComponent : TComponent; operation : TOperation); override;

    procedure SetCaption(Value: TElFString);
    {$ifndef CLX_USED}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$endif}
    function GetThemePartID: Integer; virtual;
    function GetThemeStateID: Integer; virtual;
    function GetThemedClassName: WideString; virtual;
    function GetArrowThemePartID: Integer; virtual;
    function GetArrowThemeStateID: Integer; virtual;
    function GetArrowThemedClassName: WideString; virtual;

    function GetArrowSize: Integer; virtual;
    {$ifdef MSWINDOWS}
    procedure DrawThemedBackground(Canvas : TCanvas); virtual;
    {$endif}
    {$ifndef CLX_USED}
    procedure WMWindowPosChanged(var Message: TMessage); message WM_WINDOWPOSCHANGED;
    {$endif}
    function MeasureButton(LockHeight : boolean): TPoint;

    procedure SetMoneyFlat(Value: Boolean);
    procedure SetMoneyFlatDownColor(Value: TColor);
    procedure SetMoneyFlatActiveColor(Value: TColor);
    procedure SetMoneyFlatInactiveColor(Value: TColor);
    function GetMoneyFlat: Boolean;

    {$ifndef CLX_USED}
    procedure IntWndProc(var Message : TMessage);
    {$endif}
    procedure DoPullMenu; virtual;
    procedure SetShortcutsEnabled(Value: Boolean);
    function Focused: Boolean; virtual;
    procedure SetImageIsAlphaBlended(Value: Boolean);
    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);
    {$endif}
    procedure SetCursor(Value: TCursor); virtual;
    procedure SetChangeDisabledText(Value: Boolean);

    property ClicksDisabled : Boolean read FClicksDisabled write FClicksDisabled;

    property PullDownMenu : TPopupMenu read FPullDownMenu write SetPullDownMenu; { Published }
    property PopupPlace : TPopupPlace read FPopupPlace write SetPopupPlace default ppDown;
    property DisableAutoPopup : boolean read FDisableAp write SetDisableAp default false;
    property Cancel : Boolean read FCancel write FCancel default False;
    property ModalResult : TModalResult read FModalResult write FModalResult default 0;
    property AllowAllUp : Boolean read FAllowAllUp write SetAllowAllUp default False;
    property GroupIndex : Integer read FGroupIndex write SetGroupIndex default 0;
    property Down : Boolean read FDown write SetDown default False;
    property Flat : Boolean read FFlat write SetFlat default false;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property Layout : TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin : Integer read FMargin write SetMargin default -1;
    property NumGlyphs : TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property Spacing : Integer read FSpacing write SetSpacing default 4;

    property UseArrow : Boolean read FUseArrow write SetUseArrow default False;
    property ShadowFollowsColor : boolean read FShadowFollowsColor write SetShadowFollowsColor default true;
    property ShowGlyph : Boolean read FShowGlyph write SetShowGlyph default true;
    property ShowText : Boolean read FShowText write SetShowText default true;
    property OnArrowClick : TNotifyEvent read FOnArrowClick write FOnArrowClick;
    property Icon : TIcon read GetIcon write SetIcon;
    property UseIcon : Boolean read FUseIcon write SetUseIcon default false; { Protected }
    property IsSwitch : Boolean read FIsSwitch write SetIsSwitch default false;
    property DownSound : TElSoundName read FDownSound write FDownSound; { Published }
    property UpSound : TElSoundName read FUpSound write FUpSound; { Published }
    property ClickSound : TElSoundName read FClickSound write FClickSound; { Published }
    property ArrowClickSound : TElSoundName read FArrowClickSound write FArrowClickSound; { Published }
{$IFDEF USE_SOUND_MAP}
    property SoundMap : TElSoundMap read FSoundMap write SetSoundMap;
{$ENDIF}
    property ImageIndex : Integer read GetImageIndex write SetImageIndex default -1;
    property Images : TImageList read FImageList write SetImageList;
    property HotImages : TImageList read FHotImages write SetHotImages;
    property DisabledImages : TImageList read FDisabledImages write SetDisabledImages;
    property DownImages     : TImageList read FDownImages write SetDownImages;
    {$ifndef CLX_USED}
    property ImageForm      : TElImageForm read FImgForm write SetImageForm;
    {$endif}
    property ShowBorder     : boolean read FShowBorder write SetShowBorder default true;

    property ShadowsUseCustom  : boolean read FShadowsUseCustom write SetShadowsUseCustom default false;
    property ShadowBtnHighlight: TColor read FShadowBtnHighlight write SetShadowBtnHighlight stored DoSaveShadows default $00F7F7F5;
    property ShadowBtnShadow   : TColor read FShadowBtnShadow write   SetShadowBtnShadow stored DoSaveShadows default $00767A60;
    property ShadowBtnDkShadow : TColor read FShadowBtnDkShadow write SetShadowBtnDkShadow stored DoSaveShadows default $00595C48;

    property UseImageList : Boolean read GetUseImageList write SetUseImageList default false; { Protected }
    property OldStyled : Boolean read FOldStyled write SetOldStyled default false; { Protected }
    property ThinFrame : Boolean read FThinFrame write SetThinFrame default false;  { Protected }
    property Background : TBitmap read FBackground write SetBackground;  { Protected }
    property DownBackground : TBitmap read FDownBackground write SetDownBackground;  { Protected }
    property BackgroundDrawBorder : boolean read FBackgroundDrawBorder write SetBackgroundDrawBorder default false;
    property Transparent : boolean read FTransparent write SetTransparent default false;
    property TextDrawType : TElTextDrawType read FTextDrawType write SetTextDrawType default tdtNormal; { Published }
    property Checked : Boolean read GetChecked write SetChecked default false; { Protected }
    property Color stored IsColorStored nodefault;
    property AdjustSpaceForGlyph: Boolean read FAdjustSpaceForGlyph write
        SetAdjustSpaceForGlyph default true;
    property Caption: TElFString read FCaption write SetCaption;

    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
        True;

    property MoneyFlat: Boolean read GetMoneyFlat write SetMoneyFlat default false;
    property MoneyFlatDownColor: TColor read FMoneyFlatDownColor write
        SetMoneyFlatDownColor stored GetMoneyFlat;
    property MoneyFlatActiveColor: TColor read FMoneyFlatActiveColor write
        SetMoneyFlatActiveColor  stored GetMoneyFlat;
    property MoneyFlatInactiveColor: TColor read FMoneyFlatInactiveColor write
        SetMoneyFlatInactiveColor  stored GetMoneyFlat;
    property ImageIsAlphaBlended: Boolean read FImageIsAlphaBlended write 
        SetImageIsAlphaBlended default false;
    property ChangeDisabledText: Boolean read FChangeDisabledText write
        SetChangeDisabledText default true;
    property Alignment: TAlignment read FAlignment write SetAlignment default
        taCenter;

{$ifdef HAS_HTML_RENDER}
    property IsHTML : Boolean read FIsHTML write SetIsHTML default false;

    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        FOnImageNeeded;
    property OnLinkClick: TElHTMLLinkClickEvent read FOnLinkClick write
        FOnLinkClick;
    property LinkColor: TColor read FLinkColor write SetLinkColor default clBlue;
    property LinkPopupMenu: TPopupMenu read FLinkPopupMenu write SetLinkPopupMenu;
    property LinkStyle: TFontStyles read FLinkStyle write SetLinkStyle;
    property Cursor: TCursor read FCursor write SetCursor;
    {$endif}
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AClick(Arrow : boolean); virtual;

    function IsThemeApplied: Boolean;
    procedure Click; override;

    property MouseInControl : Boolean read FMouseInControl;
    property Theme: HTheme read FTheme;
    {$ifndef CLX_USED}
    property MenuWindowProc: TWndMethod read FMenuWindowProc write FMenuWindowProc;
    {$endif}
    property ShortcutsEnabled: Boolean read FShortcutsEnabled write 
        SetShortcutsEnabled default false;
  published
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
  end;
{$WARNINGS ON}

  TCustomElGraphicButtonClass = class of TCustomElGraphicButton;

  TElGraphicButton = class(TCustomElGraphicButton)
  published
    property Alignment;
    property Background;
    property BackgroundDrawBorder;
    property DownBackground;
    property ImageIsAlphaBlended;
    property ImageIndex;
    property UseImageList;
    property Images;
    property HotImages;
    property DisabledImages;
    property PullDownMenu;
    property PopupPlace;
    property DisableAutoPopup;
    property Cancel;
    property ChangeDisabledText;

    property ModalResult;

    property MoneyFlat;
    property MoneyFlatInactiveColor;
    property MoneyFlatActiveColor;
    property MoneyFlatDownColor;

    property AdjustSpaceForGlyph;
    property AllowAllUp;
    property GroupIndex;
    property Down;
    property Flat;
    property Glyph;
    {$ifndef CLX_USED}
    property ImageForm;
    {$endif}
{$ifdef HAS_HTML_RENDER}
    property IsHTML;
{$endif}
    property Layout;

    property Margin;
    property NumGlyphs;
    property ShadowFollowsColor;
    property ShadowsUseCustom;
    property ShadowBtnHighlight;
    property ShadowBtnShadow;
    property ShadowBtnDkShadow;

    property ShowGlyph;
    property ShowText;
    property Spacing;
    property UseArrow;
    property IsSwitch;
    property OnArrowClick;
    property Icon;
    {$ifdef HAS_HTML_RENDER}
    property LinkColor;
    property LinkPopupMenu;
    property LinkStyle;
    {$endif}
    property UseIcon;
    property ThinFrame;
    property TextDrawType;
    property Transparent;
    property DownSound;
    property UpSound;
    property ClickSound;
    property ArrowClickSound;
{$IFDEF USE_SOUND_MAP}
    property SoundMap;
{$ENDIF}
    property DownImages;
    property ShowBorder;
    property ShortcutsEnabled;
    property OldStyled;
    property UseXPThemes;

    // VCL properties
    property Caption;
    property Cursor;
    property Enabled;
    property PopupMenu;
    property Color;
    property ParentColor;
    property Align;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    {$IFDEF HAS_HTML_RENDER}
    property OnImageNeeded;
    property OnLinkClick;
    {$endif}

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;

{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    {$ifndef CLX_USED}
    property DockOrientation;
    property Floating;
    property DragKind;

    property OnStartDock;
    property OnEndDock;
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
    {$endif}
  end;

{$IFDEF VCL_4_USED}
  TElGraphicButtonActionLink = class(TControlActionLink)
  protected
    FClient : TCustomElGraphicButton;
    procedure AssignClient(AClient : TObject); override;
    function IsCheckedLinked : Boolean; override;
    function IsImageIndexLinked : Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsCaptionLinked: Boolean; override;

    procedure SetImageIndex(Value: Integer); override;
    procedure SetChecked(Value : Boolean); override;
    {$ifndef CLX_USED}
    procedure SetCaption(const Value: string); override;
    procedure SetHint(const Value: string); override;
    {$else}
    procedure SetCaption(const Value: TCaption); override;
    procedure SetHint(const Value: WideString); override;
    {$endif}
  public
  end;
{$ENDIF}

var
  GlyphCache : TElGlyphCache = nil;
  Pattern : TBitmap = nil;
  ButtonCount : Integer = 0;

procedure CreateBrushPattern;

var MenuCancelMsg : integer;

{$ifdef CLX_USED}
type HMenu = QObjectH;
{$endif}

function GetMenuHandle(AMenu : TMenu) : HMENU;

implementation

{ TElGlyphList }

{$ifndef CLX_USED}
{$ifndef ELPACK_SINGLECOMP}
uses ElMenus;
{$endif}
{$endif}
var FArrow : TBitmap;

function GetMenuHandle(AMenu : TMenu) : HMENU;
begin
{$ifndef CLX_USED}
{$ifndef ELPACK_SINGLECOMP}
  if AMenu is TElPopupMenu then
    result := TElPopupMenu(AMenu).Handle
  else
  if AMenu is TElMainMenu then
    result := TElMainMenu(AMenu).Handle
  else
{$endif}
{$endif}
    result := AMenu.Handle;
end;


constructor TElGlyphList.CreateSize(AWidth, AHeight : Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

destructor TElGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TElGlyphList.AllocateIndex : Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

{$ifndef CLX_USED}
function TElGlyphList.AddMasked(Image : TBitmap; MaskColor : TColor) : Integer;
const
  Mask: array[Boolean] of Longint = (0, ILC_MASK);
var
  DC : HDC;
begin
  if not HandleAllocated and (not ThemesAvailable) then
  begin
    DC := GetDC(0);
    if (GetDeviceCaps(DC, BITSPIXEL) = 32) then
      Handle := ImageList_Create(Width, Height, ILC_COLOR32 or Mask[Masked], AllocBy, AllocBy);
    ReleaseDC(0, DC);
  end;
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;
{$else}
function TElGlyphList.AddMasked(Image : TBitmap; MaskColor : TColor) : Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;
{$endif}

procedure TElGlyphList.Delete(Index : Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ TElGlyphCache }

constructor TElGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TElList.Create;
end;

destructor TElGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

function TElGlyphCache.GetList(AWidth, AHeight : Integer) : TElGlyphList;
var
  I : Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TElGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure TElGlyphCache.ReturnList(List : TElGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

function TElGlyphCache.Empty : Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

procedure CreateBrushPattern;
var
  X, Y : Integer;
begin
  Pattern := TBitmap.Create;
  Pattern.Width := 8;
  Pattern.Height := 8;
  with Pattern.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, Pattern.Width, Pattern.Height));
    {$ifdef CLX_USED}
    Pen.Color := clBtnHighlight;
    {$endif}
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then { toggles between even/odd pixles }
{$ifndef CLX_USED}
          Pixels[X, Y] := clBtnHighlight; { on even/odd rows }
{$else}
          Pattern.Canvas.DrawPoint(X, Y);
{$endif}
  end;
end;

{ TElButtonGlyph }

constructor TElButtonGlyph.Create;
var
  I : TElButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TElGlyphCache.Create;
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FImageIndex := -1;
end;

destructor TElButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Repaint;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  FIcon.Free;
  inherited Destroy;
end;

procedure TElButtonGlyph.Repaint;
var
  I : TElButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure TElButtonGlyph.SetImageIndex(NewValue : Integer);
begin
  if FImageIndex <> NewValue then
  begin
    FImageIndex := NewValue;
    Repaint;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TElButtonGlyph.SetImageList(NewValue : TImageList);
begin
  if FImageList <> NewValue then
  begin
    FImageList := NewValue;
    Repaint;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TElButtonGlyph.IconChanged(Sender : TObject);
begin
  if FIcon.Empty then UseIcon := false;
  Repaint;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TElButtonGlyph.GlyphChanged(Sender : TObject);
begin
  if Sender = FOriginal then
  begin
    {$ifdef CLX_USED}
    if (FOriginal.Width <> 0) and (FOriginal.Height <> 0) then
    {$endif}
      FTransparentColor := FOriginal.TransparentColor;

    ResetNumGlyphs;
    Repaint;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TElButtonGlyph.ResetNumGlyphs;
var Value : TBitmap;
    Glyphs: Integer;

begin
  Value := FOriginal;
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
  if (Value = nil) or (Value.Empty) then SetNumGlyphs(1);
end;

procedure TElButtonGlyph.SetGlyph(Value : TBitmap);
begin
  Repaint;
  FOriginal.Assign(Value);
  {$ifndef CLX_USED}
  FTransparentColor := Value.TransparentColor;
  {$endif}
end;

procedure TElButtonGlyph.SetNumGlyphs(Value : TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Repaint;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
    FNumGlyphs := Value;
  end;
end;

function TElButtonGlyph.CreateButtonGlyph(State : TElButtonState) : Integer;
{$ifndef CLX_USED}
const
  ROP_DSPDxax = $00E20746;
{$endif}
var
  TmpImage{$ifndef CLX_USED}, DDB, MonoBmp{$endif} : TBitmap;
  IWidth, IHeight : Integer;
  IRect, ORect : TRect;
  I : TElButtonState;
  {$ifndef CLX_USED}
  DestDC : HDC;
  {$endif}
begin
  if (State = ebsDown) and (NumGlyphs < 3) then State := ebsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TElGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    {$ifndef CLX_USED}
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    {$endif}
    I := State;
    if Ord(I) >= NumGlyphs then I := ebsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      ebsUp, ebsDown,
        ebsArrDown,
        ebsExclusive :
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
{$IFDEF VER90}
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor);
{$ELSE}
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
{$ENDIF}
        end;
      ebsDisabled :
        begin
          {$ifndef CLX_USED}
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
{$IFNDEF VER90}
            DDB.HandleType := bmDDB;
{$ENDIF}
            if NumGlyphs > 1 then
              with TmpImage.Canvas do
              begin { Change white & gray to clBtnHighlight and clBtnShadow }
                CopyRect(IRect, DDB.Canvas, ORect);
                MonoBmp.Monochrome := True;
                MonoBmp.Width := IWidth;
                MonoBmp.Height := IHeight;

              { Convert white to clBtnHighlight }
                DDB.Canvas.Brush.Color := clWhite;
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnHighlight;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
                DDB.Canvas.Brush.Color := clGray;
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnShadow;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
                DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnFace;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
{$IFNDEF VER90}
                HandleType := bmDDB;
{$ENDIF}
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := cl3DDkShadow;//clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
          {$else}
          if NumGlyphs > 1 then
            TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect)
          else
            FGlyphList.Draw(TmpImage.Canvas, 0, 0, FIndexs[ebsUp], itImage, False);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
          {$endif}
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TElButtonGlyph.DrawButtonGlyph(Canvas : TCanvas; const GlyphPos : 
    TPoint; State : TElButtonState; Transparent : Boolean; Color : TColor; 
    AlphaBlended : boolean);
var
  Index   : Integer;
  TC, OTC : TColor;
  BMP     : TBitmap;
  FStyle  : TBrushStyle;
  {$ifndef CLX_USED}
  IcW,
    IcH   : Integer;
  {$endif}
  {$ifndef CLX_USED}
  IconInfo: TIconInfo;
  ABMP    : Windows.TBitmap;
  {$endif}

{$ifndef CLX_USED}
const
  ROP_DSPDxax = $00E20746;
{$endif}

  {$ifndef CLX_USED}
  procedure SetDisabled(FOriginal : TBitmap);
  var MonoBmp : TBitmap;
      SrcDC,
      DestDC  : HDC;

  begin
    MonoBmp := TBitmap.Create;
    with MonoBmp do
    begin
      Width := FOriginal.Width;
      Height := FOriginal.Height;
      Monochrome := True;      
    end;
    SrcDC := MonoBmp.Canvas.Handle;

    monoBmp.Canvas.CopyRect(Rect(0, 0, FOriginal.Width, FOriginal.Height), FOriginal.Canvas, Rect(0, 0, FOriginal.Width, FOriginal.Height));
    //BitBlt(SrcDC, 0, 0, FOriginal.Width, FOriginal.Height, FOriginal.Canvas.Handle, 0, 0, SRCCOPY);

    FOriginal.Canvas.Brush.Color := cl3DDkShadow;
    FOriginal.Canvas.FillRect(Rect(0, 0, FOriginal.Width, FOriginal.Height));

    { Convert Black to clBtnHighlight }
    FOriginal.Canvas.Brush.Color := clBtnHighlight;
    DestDC := FOriginal.Canvas.Handle;

    Windows.SetTextColor(DestDC, clWhite);
    Windows.SetBkColor(DestDC, clBlack);

    BitBlt(DestDC, 1, 1, FOriginal.Width - 1, FOriginal.Height - 1, SrcDC, 0, 0, ROP_DSPDxax);

    { Convert Black to clBtnShadow }

    FOriginal.Canvas.Brush.Color := clBtnShadow;
    DestDC := FOriginal.Canvas.Handle;

    Windows.SetTextColor(DestDC, clWhite);
    Windows.SetBkColor(DestDC, clBlack);

    BitBlt(DestDC, 0, 0, FOriginal.Width, FOriginal.Height, SrcDC, 0, 0, ROP_DSPDxax);

    MonoBmp.Free;
  end;
  {$endif}

begin
  if FUseImageList and (FImageList <> nil) then
  begin
    if ImageIndex <> -1 then
    begin
      if FStretched or (State = ebsDisabled) then
      begin
        BMP := TBitmap.Create;
        {$ifndef CLX_USED}
        Bmp.PixelFormat := pf24bit;
        {$endif}
        
        if FStretched then
        begin
          BMP.Width := FOriginal.Width div FNumGlyphs;
          BMP.Height := FOriginal.Height;
        end
        else
        begin
          BMP.Width := FImageList.Width;
          Bmp.Height := FImageList.Height;
        end;
        {$ifndef CLX_USED}
        with GlyphPos do
          if Transparent or (State = ebsExclusive) or (Color = clNone) then
            ImageList_DrawEx(FImageList.Handle, ImageIndex, BMP.Canvas.Handle, 0, 0, 0, 0,
              clNone, clNone, ILD_Transparent)
          else
            ImageList_DrawEx(FImageList.Handle, ImageIndex, BMP.Canvas.Handle, 0, 0, 0, 0,
              ColortoRGB(clBtnFace){CLR_DEFAULT}, clNone, ILD_Normal);
        {$else}
        with GlyphPos do
          if Transparent or (State = ebsExclusive) or (Color = clNone) then
          begin
            FImageList.Draw(Bmp.Canvas, 0, 0, 0, itImage, State <> ebsDisabled);
          end
          else
          begin
            FImageList.BkColor := Color;
            FImageList.Draw(Bmp.Canvas, 0, 0, 0, itImage, State <> ebsDisabled);
          end;
        {$endif}
        // Bmp.SaveToFile('e:\nomanifset.bmp');
        FStyle := Canvas.Brush.Style;
        {$ifndef CLX_USED}
        TC := BMP.Canvas.Pixels[0, BMP.Height - 1];
        {$else}
        TC := ElCLXUtils.GetPixel(BMP.Canvas, 0, BMP.Height - 1);
        {$endif}
        OTC := Canvas.Brush.Color;
        Canvas.Brush.Color := TC;

        if FStretched then
        begin
          Canvas.Brush.Style := bsClear;

          {$ifndef CLX_USED}
          if AlphaBlended then
            AlphaCopyRect(Canvas, Rect(GlyphPos.X, GlyphPos.Y, FStrW + GlyphPos.X, FStrH + GlyphPos.Y), BMP.Canvas, Rect(0, 0, BMP.Width, BMP.Height), 255, false)
          else
            Canvas.BrushCopy(Rect(GlyphPos.X, GlyphPos.Y, FStrW + GlyphPos.X, FStrH + GlyphPos.Y),
              BMP, Rect(0, 0, BMP.Width, BMP.Height), TC);
          {$else}
          Canvas.Start;
          DrawTransparentBitmapEx(Canvas.Handle, Bmp, GlyphPos.X, GlyphPos.Y, Rect(0, 0, BMP.Width, BMP.Height), TC);
          Canvas.Stop;
          {$endif}
        end
        else
        begin
          {$ifndef CLX_USED}

          SetDisabled(Bmp);

          TC := Bmp.Canvas.Pixels[0, BMP.Height - 1];
          Canvas.Brush.Color := TC;
          Canvas.Brush.Style := bsClear;

          DrawTransparentBitmapEx(Canvas.Handle, Bmp, GlyphPos.X, GlyphPos.Y, Rect(0, 0, BMP.Width, BMP.Height), TC);

            //Canvas.BrushCopy(Rect(GlyphPos.X, GlyphPos.Y, BMP.Width + GlyphPos.X, BMP.Height + GlyphPos.Y),
            //  BMP, Rect(0, 0, BMP.Width, BMP.Height), TC);

          {$else}
          Canvas.Start;
          DrawTransparentBitmapEx(Canvas.Handle, Bmp, GlyphPos.X, GlyphPos.Y, Rect(0, 0, BMP.Width, BMP.Height), TC);
          Canvas.Stop;
          {$endif}
        end;
        Canvas.Brush.Color := OTC;
        Canvas.Brush.Style := FStyle;
        Bmp.Free;
      end
      else
        {$ifndef CLX_USED}
        if AlphaBlended then
        begin
          BMP := TBitmap.Create;
          BMP.Width := FImageList.Width;
          Bmp.Height := FImageList.Height;
          ImageList_DrawEx(FImageList.Handle, ImageIndex, BMP.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Normal);
          AlphaCopyRect(Canvas, Rect(GlyphPos.X, GlyphPos.Y, BMP.Width + GlyphPos.X, BMP.Height + GlyphPos.Y), BMP.Canvas, Rect(0, 0, BMP.Width, BMP.Height), 255, false);
          Bmp.Free;
        end
        else
        with GlyphPos do
          if Transparent or (State = ebsExclusive) or (Color = clNone) then
            ImageList_DrawEx(FImageList.Handle, ImageIndex, Canvas.Handle, X, Y, 0, 0,
              clNone, clNone, ILD_Transparent)
          else
            ImageList_DrawEx(FImageList.Handle, ImageIndex, Canvas.Handle, X, Y, 0, 0,
              ColorToRGB(Color), clNone, ILD_Normal);
        {$else}
        with GlyphPos do
          if Transparent or (State = ebsExclusive) or (Color = clNone) then
          begin
            FImageList.Draw(Canvas, X, Y, ImageIndex, itImage, State <> ebsDisabled);
          end
          else
          begin
            FImageList.BkColor := Color;
            FImageList.Draw(Canvas, X, Y, ImageIndex, itImage, State <> ebsDisabled);
          end;
        {$endif}
    end;
    exit;
  end;
  if FUseIcon then
  begin
    {$ifndef CLX_USED}
    GetIconInfo(Icon.Handle, IconInfo);
    GetObject(IconInfo.hbmColor, sizeof(ABMP), @ABMP);
    IcH := ABMP.bmHeight;
    IcW := ABMP.bmWidth;
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
    if FStretched then
      DrawIconEx(Canvas.Handle, GlyphPos.X, GlyphPos.Y, FIcon.Handle, FStrW, FStrH, 0, 0, DI_NORMAL)
    else
      DrawIconEx(Canvas.Handle, GlyphPos.X, GlyphPos.Y, FIcon.Handle, IcW, IcH, 0, 0, DI_NORMAL);
    {$else}
    Canvas.Draw(GlyphPos.X, GlyphPos.Y, FIcon);
    {$endif}
    exit;
  end;
  if (FOriginal = nil) or (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
  if FStretched then
  begin
    BMP := TBitmap.Create;
    BMP.Width := FOriginal.Width div FNumGlyphs;
    BMP.Height := FOriginal.Height;
    {$ifndef CLX_USED}
    with GlyphPos do
      if Transparent or (State = ebsExclusive) then
        ImageList_DrawEx(FGlyphList.Handle, Index, BMP.Canvas.Handle, 0, 0, 0, 0,
          clNone, clNone, ILD_Transparent)
      else
        ImageList_DrawEx(FGlyphList.Handle, Index, BMP.Canvas.Handle, 0, 0, 0, 0,
          ColorToRGB(clBtnFace), clNone, ILD_Normal);
    {$else}
    with GlyphPos do
      if Transparent or (State = ebsExclusive) or (Color = clNone) then
      begin
        FGlyphList.Draw(Bmp.Canvas, 0, 0, Index, itImage, State <> ebsDisabled);
      end
      else
      begin
        FGlyphList.BkColor := clBtnFace;
        FGlyphList.Draw(Bmp.Canvas, 0, 0, Index, itImage, State <> ebsDisabled);
      end;
    {$endif}
    FStyle := Canvas.Brush.Style;
    {$ifndef CLX_USED}
    TC := BMP.Canvas.Pixels[0, BMP.Height - 1];
    {$else}
    TC := ElCLXUtils.GetPixel(BMP.Canvas, 0, BMP.Height - 1);
    {$endif}
    OTC := Canvas.Brush.Color;
    Canvas.Brush.Color := TC;
    Canvas.Brush.Style := bsClear;
    {$ifndef CLX_USED}
    if AlphaBlended then
      AlphaCopyRect(Canvas, Rect(GlyphPos.X, GlyphPos.Y, FStrW + GlyphPos.X, FStrH + GlyphPos.Y), BMP.Canvas, Rect(0, 0, BMP.Width, BMP.Height), 255, false)
    else
      Canvas.BrushCopy(Rect(GlyphPos.X, GlyphPos.Y, FStrW + GlyphPos.X, FStrH + GlyphPos.Y),
        BMP, Rect(0, 0, BMP.Width, BMP.Height), TC);
    {$else}
    Canvas.Start;
    DrawTransparentBitmapEx(Canvas.Handle, Bmp, GlyphPos.X, GlyphPos.Y, Rect(0, 0, BMP.Width, BMP.Height), TC);
    Canvas.Stop;
    {$endif}
    Canvas.Brush.Color := OTC;
    Canvas.Brush.Style := FStyle;
    Bmp.Free;
  end
  else
    {$ifndef CLX_USED}
    if AlphaBlended then
    begin
      BMP := TBitmap.Create;
      BMP.Width := FGlyphList.Width;
      Bmp.Height := FGlyphList.Height;
      ImageList_DrawEx(FGlyphList.Handle, Index, BMP.Canvas.Handle, 0, 0, 0, 0,
        clNone, clNone, ILD_Normal);

      AlphaCopyRect(Canvas, Rect(GlyphPos.X, GlyphPos.Y, BMP.Width + GlyphPos.X, BMP.Height + GlyphPos.Y), BMP.Canvas, Rect(0, 0, BMP.Width, BMP.Height), 255, false);
      Bmp.Free;
    end
    else
    with GlyphPos do
    begin
      if Transparent or (State = ebsExclusive) or (Color = clNone) then
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          clNone, clNone, ILD_Transparent)
      else
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          ColorToRGB(Color), clNone, ILD_Normal);
    end;
    {$else}
    with GlyphPos do
      if Transparent or (State = ebsExclusive) or (Color = clNone) then
      begin
        FGlyphList.Draw(Canvas, X, Y, Index, itImage, State <> ebsDisabled);
      end
      else
      begin
        FGlyphList.BkColor := clBtnFace;
        FGlyphList.Draw(Canvas, X, Y, Index, itImage, State <> ebsDisabled);
      end;
    {$endif}
end;

procedure TElButtonGlyph.DrawButtonText(Canvas : TCanvas; const Caption : 
    TElFString; TextBounds : TRect; State : TElButtonState; Multiline : boolean;
    Alignment : TAlignment; Active, Transparent : boolean; TextDrawType :
    TElTextDrawType; UseThemesForText : boolean; Theme : HTheme; ThemePart,
    ThemeState : integer; ShowAccelChar : boolean {$ifdef HAS_HTML_RENDER};
    IsHTML : boolean; HTMLRender : TElHTMLRender{$endif} ; ChangeDisabledText :
    boolean);
var
  Flags : integer;
begin
  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    {$ifdef CLX_USED}
    Canvas.Start;
    {$endif}
    HTMLRender.DrawText(Canvas, Point(0, 0), TextBounds, clNone);
    {$ifdef CLX_USED}
    Canvas.Stop;
    {$endif}
  end
  else
  {$endif}
  {$ifdef MSWINDOWS}
  if (Theme <> 0) and UseThemesForText then
  begin
    if not Multiline then
      Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE
    else
    begin
      Flags := 0;
      case Alignment of
        taLeftJustify: Flags := DT_LEFT;
        taRightJustify: Flags := DT_RIGHT;
        taCenter: Flags := DT_CENTER;
      end;
      Flags := Flags or DT_VCENTER;
    end;
    if not ShowAccelChar then
      Flags := Flags or DT_HIDEPREFIX;
    Canvas.Brush.Style := bsClear;
    {$ifndef CLX_USED}
    DrawThemeText(Theme, Canvas.Handle, ThemePart, ThemeState, PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, 0, TextBounds);
    {$else}
    Canvas.Start;
    DrawThemeText(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), ThemePart, ThemeState, PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, 0, TextBounds);
    Canvas.Stop;
    {$endif}
  end
  else
  {$endif}
  with Canvas do
  begin
    Brush.Style := bsClear;
    if (State = ebsDisabled) and ChangeDisabledText then
    begin
      OffsetRect(TextBounds, 1, 1);
      {$ifndef CLX_USED}
      if not Multiline then
        Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS
      else
        Flags := DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS;
      if not ShowAccelChar then
        Flags := Flags or DT_HIDEPREFIX;
      {$else}
      if not Multiline then
        Flags := Integer(AlignmentFlags_AlignVCenter) or
                 Integer(AlignmentFlags_AlignHCenter) or
                 Integer(AlignmentFlags_SingleLine)
      else
        Flags := Integer(AlignmentFlags_AlignVCenter) or
                 Integer(AlignmentFlags_AlignHCenter);
      {$endif}
      Font.Color := clBtnHighlight;
      if Transparent then Brush.Style := bsClear;
      {$ifndef CLX_USED}
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Handle, PWideChar(Caption), Length(Caption), TextBounds, Flags);
      {$else}
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      {$endif}
      {$else}
      Start;
      QPainter_drawText(Handle, @TextBounds, Flags, PWideString(@Caption), -1, nil, nil);
      Stop;
      {$endif}
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      {$ifndef CLX_USED}
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Handle, PWideChar(Caption), Length(Caption), TextBounds, Flags);
      {$else}
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      {$endif}
      {$else}
      Start;
      QPainter_drawText(Handle, @TextBounds, Flags, PWideString(@Caption), -1, nil, nil);
      Stop;
      {$endif}
    end
    else
    begin
      {$ifndef CLX_USED}
      if not Multiline then
        Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS
      else
        Flags := DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS;
      if not ShowAccelChar then
        Flags := Flags or DT_HIDEPREFIX;
      {$else}
      if not Multiline then
        Flags := Integer(AlignmentFlags_AlignVCenter) or
                 Integer(AlignmentFlags_AlignHCenter) or
                 Integer(AlignmentFlags_SingleLine)
      else
        Flags := Integer(AlignmentFlags_AlignVCenter) or
                 Integer(AlignmentFlags_AlignHCenter);
      {$endif}
      if Transparent then Brush.Style := bsClear;

      {$ifdef ELPACK_UNICODE}
      DrawTypedTextW(Canvas, TextBounds, Caption, Flags, TextDrawType);
      {$else}
      DrawTypedText(Canvas, TextBounds, Caption, Flags, TextDrawType);
      {$endif}

      Brush.Style := bsSolid;
      case TextDrawType of
        tdtNormal : InflateRect(TextBounds, 1, 1);
        tdtShadowed : InflateRect(TextBounds, 3, 3);
        tdtRaised : InflateRect(TextBounds, 2, 2);
      end;
      //if Active and (Length(Caption) > 0) then Windows.DrawFocusRect(Canvas.Handle, TextBounds);
    end;
  end;
end;
{$WARNINGS off}

procedure TElButtonGlyph.CalcButtonLayout(Canvas : TCanvas; const Client :
    TRect; const Offset : TPoint; const Caption : TElFString; Layout :
    TButtonLayout; Margin, Spacing : Integer; var GlyphPos : TPoint; var
    TextBounds : TRect; ShowGlyph, ShowText, MultiLine : boolean; ArrowWidth :
    integer; UseThemesForText : boolean; Theme : HTheme; ThemePart, ThemeState
    : integer{$ifdef HAS_HTML_RENDER};  IsHTML : boolean; HTMLRender : TElHTMLRender{$endif});
var
  TextPos : TPoint;
  ClientSize, GlyphSize, TextSize : TPoint;
  TotalSize : TPoint;
  GX, GY : integer;
  {$ifdef MSWINDOWS}
  Flags : integer;
  {$endif}
  AGraphic : TGraphic;
  IcW,
    IcH : Integer;
  {$ifndef CLX_USED}
  IconInfo : TIconInfo;
  ABMP : Windows.TBitmap;
  {$endif}
  ILBitmap : TBitmap;
begin
  ClientSize := Point(Client.Right - Client.Left - ArrowWidth, Client.Bottom - Client.Top);
  FStretched := false;

  if UseImageList and (FImageList <> nil) then
  begin
    ILBitmap := TBitmap.Create;
    if FImageIndex <> -1 then
    begin
      ILBitmap.Height := ImageList.Height;
      ILBitmap.Width := ImageList.Width;
    end;
    AGraphic := ILBitmap;
  end
  else
  if UseIcon then
  begin
    {$ifndef CLX_USED}
    GetIconInfo(Icon.Handle, IconInfo);
    GetObject(IconInfo.hbmColor, sizeof(ABMP), @ABMP);
    IcH := ABMP.bmHeight;
    IcW := ABMP.bmWidth;
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
    AGraphic := FIcon;
    {$else}
    IcH := Icon.Height;
    IcW := Icon.Width;
    {$endif}
  end
  else
    AGraphic := FOriginal;
  if (AGraphic <> nil) and (ShowGlyph) then
  begin
    if UseIcon then
    begin
      GX := IcW;
      GY := IcH;
    end else
    if AGraphic = ILBitmap then
    begin
      GX := AGraphic.Width;
      GY := AGraphic.Height;
    end else
    begin
      GX := AGraphic.Width div FNumGlyphs;
      GY := AGraphic.Height;
    end;
    if GY > ClientSize.y then
    begin
      GY := ClientSize.Y - 2;
      GX := MulDiv(GX, GY, ClientSize.Y);
      FStretched := true;
    end;
    if GX > ClientSize.X then
    begin
      GY := MulDiv(GY , ClientSize.x - 4, GX);
      GX := ClientSize.x - 2;
      FStretched := true;
    end;
    if FStretched then
    begin
      FStrW := GX;
      FStrH := GY;
    end;
    GlyphSize := Point(GX, GY);
  end
  else
    GlyphSize := Point(0, 0);

  if (Length(Caption) > 0) and (ShowText) then
  begin
    {$ifdef HAS_HTML_RENDER}
    if IsHTML then
    begin
      HTMLRender.PrepareText(Caption, 0, false);
      TextBounds := Rect(0, 0, HTMLRender.Data.TextSize.cx, HTMLRender.Data.TextSize.cy);
      with HTMLRender.Data.TextSize do
        TextSize := Point(cx, cy);
    end
    else
    {$endif}
    {$ifdef MSWINDOWS}
    if (Theme <> 0) and UseThemesForText then
    begin
      TextBounds := Rect(0, 0, ClientSize.X, 0);
      if MultiLine then
        Flags := DT_CALCRECT
      else
        Flags := DT_CALCRECT + DT_SINGLELINE;

      {$ifndef CLX_USED}
      GetThemeTextExtent(Theme, Canvas.Handle, ThemePart, ThemeState, PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, @Client, TextBounds);
      {$else}
      Canvas.Start;
      GetThemeTextExtent(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), ThemePart, ThemeState, PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, @Client, TextBounds);
      Canvas.Stop;
      {$endif}
      OffsetRect(TextBounds, -TextBounds.Left, -TextBounds.Top);
      TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
    end
    else
    {$endif}
    begin
      TextBounds := Rect(0, 0, ClientSize.X, 0);
      {$ifndef CLX_USED}
      if MultiLine then
        Flags := DT_CALCRECT
      else
        Flags := DT_CALCRECT + DT_SINGLELINE;
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Caption), Length(Caption), TextBounds, Flags);
      {$else}
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      {$endif}
      {$else}
      Canvas.TextExtent(Caption, TextBounds);
      {$endif}
      TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
        TextBounds.Top);
    end;
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y {+ 1}) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y {+ 1}) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X) div 2;
    TextPos.X := (ClientSize.X - TextSize.X) div 2;
  end;

  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft :
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight :
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop :
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom :
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  (*
  with TextPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  *)
  if Theme <> 0 then
    OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, Client.Top + TextPos.Y + Offset.Y)
  else
  begin
    OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
    // TextBounds.Right := TextBounds.Left + // Min(TextBounds.Right - TextBounds.Left, Client.Right - Client.Left - 1);
    // TextBounds.Bottom := TextBounds.Top + Min(TextBounds.Bottom - TextBounds.Top, Client.Bottom - Client.Top - 1);
  end;

  if UseImageList and (FImageList <> nil) then ILBitmap.Free;
end;

procedure TElButtonGlyph.GetPaintGlyphSize(R : TRect; var Size : TPoint);
var
  GX, GY : integer;
  ClientSize : TPoint;
  AGraphic : TGraphic;
  IcW,
    IcH : Integer;
  {$ifndef CLX_USED}
  IconInfo : TIconInfo;
  ABMP : Windows.TBitmap;
  {$endif}
  ILBitmap : TBitmap;
begin      
  AGraphic := nil;
  ClientSize.X := R.Right - R.Left;
  ClientSize.Y := R.Bottom - R.Top;
  if UseImageList and (FImageList <> nil) then
  begin
    ILBitmap := TBitmap.Create;
    if FImageIndex <> -1 then
    begin
      ILBitmap.Height := ImageList.Height;
      ILBitmap.Width := ImageList.Width;
      AGraphic := ILBitmap;
    end;
  end
  else
  if UseIcon then
  begin
    {$ifndef CLX_USED}
    GetIconInfo(Icon.Handle, IconInfo);
    GetObject(IconInfo.hbmColor, sizeof(ABMP), @ABMP);
    IcH := ABMP.bmHeight;
    IcW := ABMP.bmWidth;
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
    AGraphic := FIcon;
    {$else}
    IcH := Icon.Height;
    IcW := Icon.Width;
    {$endif}
  end
  else
    AGraphic := FOriginal;
  if (AGraphic <> nil) then
  begin
    if UseIcon then
    begin
      GX := IcW;
      GY := IcH;
    end
    else
    begin
      GX := AGraphic.Width div FNumGlyphs;
      GY := AGraphic.Height;
    end;
    if GY > ClientSize.Y then
    begin
      GY := ClientSize.Y - 2;
      GX := MulDiv(GX, GY, ClientSize.Y);
      GX := Trunc(GX * (GY / ClientSize.Y));
      FStretched := true;
    end;
    if GX > ClientSize.X then
    begin
      GY := MulDiv(GY, ClientSize.x - 4, GX);
      GX := ClientSize.X - 2;
      FStretched := true;
    end;
    if FStretched then
    begin
      FStrW := GX;
      FStrH := GY;
    end;
    Size := Point(GX, GY);
  end
  else
    Size := Point(0, 0);
  if UseImageList and (FImageList <> nil) then
    ILBitmap.Free;
end;

function TElButtonGlyph.Draw(Canvas : TCanvas; const Client : TRect; const 
    Offset : TPoint; const Caption : TElFString; Layout : TButtonLayout; Margin,
    Spacing : Integer; State, GlyphState : TElButtonState; Alignment : TAlignment; 
    Transparent : Boolean; Multiline : boolean; Active, ShowGlyph, ShowText : boolean;
    ArrowWidth : integer; TextDrawType : TElTextDrawType; Color : TColor; 
    UseThemesForText : boolean; Theme : HTheme; ThemePart, ThemeState : integer;
    ShowAccelChar : boolean; ImageIsAlphaBlended : boolean
    {$ifdef HAS_HTML_RENDER}; IsHTML : boolean; HTMLRender : TElHTMLRender{$endif}; ChangeDisabledText : boolean
    ): TRect;
var
  GlyphPos : TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, ShowGlyph, ShowText, Multiline, ArrowWidth, UseThemesForText,
    Theme, ThemePart, ThemeState{$ifdef HAS_HTML_RENDER}, IsHTML, HTMLRender{$endif});
  if ShowGlyph then
     DrawButtonGlyph(Canvas, GlyphPos, GlyphState, Transparent, Color, ImageIsAlphaBlended);
  if ShowText then
    DrawButtonText(Canvas, Caption, Result, State, MultiLine, Alignment, Active,
      Transparent, TextDrawType, UseThemesForText, Theme, ThemePart, ThemeState, ShowAccelChar{$ifdef HAS_HTML_RENDER}, IsHTML, HTMLRender{$endif}, ChangeDisabledText);
end;

procedure TElButtonGlyph.SetUseIcon(NewValue : boolean);
begin
  if newValue <> FUseIcon then
  begin
    FUseIcon := NewValue;
    Repaint;
  end;
end;

function TElButtonGlyph.GetGlyphSize : TRect;
{$ifndef CLX_USED}
var
  IconInfo : TIconInfo;
  ABMP : Windows.TBitmap;
{$endif}
begin
  if FUseImageList then
  begin
    result := Rect(0, 0, FImageList.Width, FImageList.Height);
  end
  else
  if UseIcon then
  begin
    {$ifndef CLX_USED}
    GetIconInfo(Icon.Handle, IconInfo);
    GetObject(IconInfo.hbmColor, sizeof(ABMP), @ABMP);
    result := Rect(0, 0, ABMP.bmWidth, ABMP.bmHeight);
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
    {$else}
    result := Rect(0, 0, Icon.Width, Icon.Height);
    {$endif}
  end
  else
    result := Rect(0, 0, FOriginal.Width div FNumGlyphs, FOriginal.Height);
end;

function TElButtonGlyph.CalcButtonWidth(Canvas : TCanvas; var MaxHeight : 
    integer; const Offset : TPoint; const Caption : TElFString; Layout : 
    TButtonLayout; Margin, Spacing : Integer; ShowGlyph, ShowText, MultiLine : 
    boolean; ArrowWidth : integer; UseThemesForText : boolean; Theme : HTheme; 
    ThemePart, ThemeState : integer
    {$ifdef HAS_HTML_RENDER}; IsHTML : boolean; HTMLRender : TElHTMLRender{$endif}): Integer;
var AGraphic : TGraphic;
    IcW,
      IcH : Integer;
    {$ifndef CLX_USED}
    IconInfo : TIconInfo;
    ABMP     : Windows.TBitmap;
    {$endif}
    ILBitmap : TBitmap;
    // Spacing  : integer;
    GX, GY   : integer;
    TextSize,
    GlyphSize: TPoint;
    {$ifdef MSWINDOWS}
    Client,
    {$endif}
    TextBounds: TRect;
    {$ifdef MSWINDOWS}
    Flags    : integer;
    {$endif}
begin
  // first get the graphic that will be used as a glyph
  if ShowGlyph then
  begin
    if UseImageList and (FImageList <> nil) then
    begin
      ILBitmap := TBitmap.Create;
      if FImageIndex <> -1 then
      begin
        ILBitmap.Height := ImageList.Height;
        ILBitmap.Width := ImageList.Width;
      end;
      AGraphic := ILBitmap;
    end
    else
    if UseIcon then
    begin
      {$ifndef CLX_USED}
      GetIconInfo(Icon.Handle, IconInfo);
      GetObject(IconInfo.hbmColor, sizeof(ABMP), @ABMP);
      IcH := ABMP.bmHeight;
      IcW := ABMP.bmWidth;
      DeleteObject(IconInfo.hbmColor);
      DeleteObject(IconInfo.hbmMask);
      AGraphic := FIcon;
      {$else}
      IcH := Icon.Height;
      IcW := Icon.Width;
      {$endif}
    end
    else
      AGraphic := FOriginal;
  end
  else
    AGraphic := nil;

  // determine actual graphic size
  if (AGraphic <> nil) then
  begin
    if UseIcon then
    begin
      GX := IcW;
      GY := IcH;
    end
    else
    if AGraphic = ILBitmap then
    begin
      GX := AGraphic.Width;
      GY := AGraphic.Height;
    end
    else
    begin
      GX := AGraphic.Width div FNumGlyphs;
      GY := AGraphic.Height;
    end;

    if MaxHeight <> 0 then
      if GY > MaxHeight then
      begin
        GY := MaxHeight - 2;
        GX := MulDiv(GX, GY, MaxHeight);
        FStretched := true;
      end;
    if FStretched then
    begin
      FStrW := GX;
      FStrH := GY;
    end;
    GlyphSize := Point(GX, GY);
  end
  else
    GlyphSize := Point(0, 0);

  // calculate text size
  if (Length(Caption) > 0) and (ShowText) then
  begin
    {$ifdef HAS_HTML_RENDER}
    if IsHTML then
    begin
      HTMLRender.PrepareText(Caption, 0, false);
      TextBounds := Rect(0, 0, HTMLRender.Data.TextSize.cx, HTMLRender.Data.TextSize.cy);
      with HTMLRender.Data.TextSize do
        TextSize := Point(cx, cy);
    end
    else
    {$endif}
    {$ifdef MSWINDOWS}
    if (Theme <> 0) and (UseThemesForText) then
    begin
      TextBounds := Rect(0, 0, 10000, 10000);
      if MultiLine then
        Flags := 0
      else
        Flags := DT_SINGLELINE;

      {$ifndef CLX_USED}
      GetThemeTextExtent(Theme, Canvas.Handle, ThemePart, ThemeState, PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, @Client, TextBounds);
      {$else}
      Canvas.Start;
      GetThemeTextExtent(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), ThemePart, ThemeState, PWideChar(WideString(Caption)), Length(WideString(Caption)), Flags, @Client, TextBounds);
      Canvas.Stop;
      {$endif}
      TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
    end
    else
    {$endif}
    begin
      TextBounds := Rect(0, 0, 10000, 10000);
      {$ifndef CLX_USED}
      if MultiLine then
        Flags := DT_CALCRECT
      else
        Flags := DT_CALCRECT + DT_SINGLELINE;
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Caption), Length(Caption), TextBounds, Flags);
      {$else}
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      {$endif}
      {$else}
      Canvas.TextExtent(Caption, TextBounds);
      {$endif}
      TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
        TextBounds.Top);
    end;
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0
  else
  if Spacing = -1 then
    Spacing := 2;

  if Margin = -1 then
    Margin := 2;

  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
     result := TextSize.X + GlyphSize.X + Margin * 2 + Spacing;
     if MaxHeight = 0 then
       MaxHeight := Max(TextSize.Y, GlyphSize.Y) + Margin * 2;
  end
  else
  begin
     result := Max(TextSize.X, GlyphSize.X) + Margin * 2;
     if MaxHeight = 0 then
       MaxHeight := TextSize.Y + GlyphSize.Y + Margin * 2 + Spacing;
  end;
  inc(Result, ArrowWidth);
  if ShowGLyph and UseImageList and (FImageList <> nil) then
    ILBitmap.Free;
end;

// =============================================================================

procedure TElPopupButton.MouseDown;
begin
  if CanFocus and TabStop then
     SetFocus;
  inherited;
end;

// =============================================================================

function TCustomElPopupButton.GetChecked : Boolean;
begin
  result := Down;
end;

procedure TCustomElPopupButton.SetChecked(newValue : Boolean);
begin
  Down := newValue;
end;

procedure TCustomElPopupButton.SetDefault(Value : Boolean);
{$ifndef CLX_USED}
var
{$IFNDEF VER90}
  Form : TCustomForm;
{$ELSE}
  Form : TForm;
{$ENDIF}
{$endif}
begin
  FDefault := Value;
  {$ifndef CLX_USED}
  if HandleAllocated then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.Perform(CM_FOCUSCHANGED, 0, Longint(Form.ActiveControl));
  end;
  {$endif}
  if DrawDefaultFrame then
    Invalidate;
end;

procedure TCustomElPopupButton.SetPopupPlace(Value : TPopupPlace);
begin
  FPopupPlace := Value;
end;

procedure TCustomElPopupButton.SetDisableAp(Value : boolean);
begin
  FDisableAp := Value;
end;

procedure TCustomElPopupButton.Paint;
var
  PaintRect, R,
  {$ifndef CLX_USED}
  R1, BgRect : TRect;
  {$endif}
  ArrRect    : TRect;
  Offset     : TPoint;
  dw : integer;
  aw : integer;
  {$ifndef CLX_USED}
  AColor : TColor;
  {$endif}
  {$ifndef CLX_USED}
  ACtl   : TWinControl;
  ax, ay : integer;
  OldP,
  P      : TPoint;
  {$endif}
  SaveIL : TImageList;
  GlyphState : TElButtonState;
  {$ifdef MSWINDOWS}
  sID, sID1  : integer;
  RClip   : TRect;
  {$endif}

  Canvas  : TCanvas;
  Bitmap  : TBitmap;

  {$ifdef CLX_USED}
  AQColor : QColorH;
  AQColorGroup : QColorGroupH;
  {$endif}

  procedure DrawButtonFrameEx(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean; ButtonColor : TColor; Thin : boolean);
  begin
    if ShadowsUseCustom then
       ElVCLUtils.DrawButtonFrameEx2(DC, rc, Focused, Pushed, ButtonColor, Thin, ColorToRGB(FShadowBtnHighlight), ColorToRGB(FShadowBtnDkShadow), ColorToRGB(Color), ColorToRGB(FShadowBtnShadow))
    else
       ElVCLUtils.DrawButtonFrameEx(DC, rc, Focused, Pushed, ButtonColor, Thin);
  end;

begin
  {$ifndef CLX_USED}
  sid := 0;
  {$endif}

  Bitmap := Tbitmap.Create;
  Bitmap.Width := ClientWidth;
  Bitmap.Height := ClientHeight;
  Canvas := Bitmap.Canvas;
  {$ifdef MSWINDOWS}
  if IsThemeApplied then
  begin
    {$ifndef CLX_USED}
    // DrawThemeParentBackground(Handle, Self.Canvas.Handle, ClientRect);
    P := Point(Left, Top);
    SetMapMode(Bitmap.Canvas.Handle, MM_ANISOTROPIC);
    SetViewPortOrgEx(Bitmap.Canvas.Handle, -P.x, -P.y, @OldP);
    SendMessage(Parent.Handle, WM_ERASEBKGND, Bitmap.Canvas.Handle, 0);
    SetViewPortOrgEx(Bitmap.Canvas.Handle, OldP.x, OldP.y, nil);
    SetMapMode(Bitmap.Canvas.Handle, MM_TEXT);
    {$else}
    Self.Canvas.Start;
    Canvas.Start;
    DrawThemeParentBackground(QWidget_winID(Handle), QPaintDevice_handle(QPainter_device(Self.Canvas.Handle)), ClientRect);
    Canvas.CopyRect(ClientRect, Self.Canvas, ClientRect );
    Self.Canvas.stop;
    Canvas.Stop;
    {$endif}
  end;
  {$endif}

  if (not Enabled) or (not Parent.Enabled) then
  begin
    FState := ebsDisabled;
    FDragging := False;
  end
  else
  if FState = ebsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := ebsExclusive
    else
      FState := ebsUp;
  Canvas.Font := Self.Font;
  PaintRect := ClientRect;

  {$ifndef CLX_USED}
  if FShadowFollowsColor then
    AColor := Color
  else
    AColor := clBtnFace;
  {$endif}
  
  {$ifdef MSWINDOWS}
  if not IsThemeApplied then
  {$endif}
  begin
    if not Transparent then
    begin
      {$ifndef CLX_USED}
      if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
      begin
        if (FImgForm.Control <> Self) then
        begin
          ACtl := FImgForm.GetRealControl;
          R1 := PaintRect;
          BgRect := R1;
          BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
          P := Parent.ClientToScreen(Point(Left, Top));
          ax := BgRect.Left - P.x;
          ay := BgRect.Top - P.y;

          BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
          BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
          BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);
          FImgForm.PaintBkgnd(Canvas.Handle, R1, Point(BgRect.Left - ax, BgRect.Top - ay), false);
        end
      end
      else
      {$endif}
      begin
        Canvas.Brush.Color := Color;
        if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) then
        begin
          if (not FDownBackground.Empty) then
            Canvas.CopyRect(ClientRect, DownBackground.Canvas, Rect(0, 0, DownBackground.Width - 1, DownBackground.Height - 1))
          else
            Canvas.FillRect(PaintRect);
        end
        else
        begin
          if (not Background.Empty) then
            Canvas.CopyRect(ClientRect, Background.Canvas, Rect(0, 0, Background.Width - 1, Background.Height - 1))
          else
            Canvas.FillRect(PaintRect);
        end;
      end;
    end
    {$ifndef CLX_USED}
    else
    begin
      GetClipBox(Self.Canvas.Handle, PaintRect);
      OffsetRect(PaintRect, Left, Top);
      RedrawWindow(Parent.Handle, @PaintRect, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN or RDW_UPDATENOW);
      OffsetRect(PaintRect, -Left, -Top);
      bitblt(Canvas.Handle, PaintRect.Left, PaintRect.Top, PaintRect.Right - PaintRect.Left, PaintRect.Bottom - PaintRect.Top, Self.Canvas.Handle, PaintRect.Left, PaintRect.Top, SRCCOPY);
    end
    {$endif}
    ;
    PaintRect := ClientRect;
  end
  ;

  dw := 0;
  aw := 0;

  if {$ifdef MSWINDOWS}(not IsThemeApplied) and {$endif}((Default and DrawDefaultFrame) or (Focused and DrawFocusFrame)) then
  begin
    {$ifndef CLX_USED}
    if MoneyFlat then
    begin
      if (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]) then
        Canvas.Brush.Color := MoneyFlatDownColor
      else
      if (FMouseInControl or FMouseInArrow) and (FState <> ebsDisabled) then
        Canvas.Brush.Color := MoneyFlatActiveColor
      else
        Canvas.Brush.Color := MoneyFlatInactiveColor;
      Canvas.FrameRect(PaintRect);
      InflateRect(PaintRect, -1, -1);
    end
    else
    {$endif}
    begin
      if not OldStyled then
        Canvas.Brush.Color := clBtnShadow
      else
        Canvas.Brush.Color := cl3DDkShadow;

      {$ifndef CLX_USED}
      Canvas.FrameRect(PaintRect);
      InflateRect(PaintRect, -1, -1);
      {$else}
      Canvas.Start;
      AQColor := QColor(Color);
      QStyle_drawRect(Application.Style.Handle,
                      Canvas.Handle,
                      PaintRect.Left,
                      PaintRect.Top,
                      PaintRect.Right - PaintRect.Left,
                      PaintRect.Bottom - PaintRect.Top,
                      AQColor, 1, nil);
      QColor_destroy(AQColor);
      QStyle_buttonRect(Application.Style.Handle, @PaintRect, PaintRect.Left,
                                                              PaintRect.Top,
                                                              PaintRect.Right - PaintRect.Left,
                                                              PaintRect.Bottom - PaintRect.Top);
      Canvas.Stop;
      {$endif}
    end;
  end;
  {$ifdef MSWINDOWS}
  if (not IsThemeApplied) or (FArrTheme = 0) then
  {$endif}
  begin
    if not FFlat then
    begin
      if FUseArrow then
      begin
        dw := GetArrowSize;
        aw := dw;
        Dec(PaintRect.Right, dw);
        {$ifdef CLX_USED}
        Dec(PaintRect.Right, 2);
        {$endif}
      end;
      if ((Background.Empty) or (BackgroundDrawBorder)) and ShowBorder then
      begin
        {$ifndef CLX_USED}
        if MoneyFlat then
        begin
          if (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]) then
            Canvas.Brush.Color := MoneyFlatDownColor
          else
          if (FMouseInControl or FMouseInArrow) and (FState <> ebsDisabled) then
            Canvas.Brush.Color := MoneyFlatActiveColor
          else
            Canvas.Brush.Color := MoneyFlatInactiveColor;
          Canvas.FrameRect(PaintRect);
        end
        else
        {$endif}
        begin
          {$ifndef CLX_USED}
          if OldStyled or ((FMouseInControl or Focused) and
             (not ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])))) then
             DrawButtonFrameEx(Canvas.Handle, PaintRect, (not ThinFrame),
                             (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]), AColor, ThinFrame)
          else
             DrawButtonFrameEx(Canvas.Handle, PaintRect, FOldStyled,
                             (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]), AColor, ThinFrame);
          {$else}
          Canvas.Start;
          AQColorGroup := Palette.ColorGroup(GetColorGroup(Self));
          with PaintRect do
            QStyle_drawButton(Application.Style.Handle,
                               Canvas.Handle,
                               Left,
                               Top,
                               Right - Left,
                               Bottom - Top,
                               AQColorGroup,
                               (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]),
                               Canvas.Brush.Handle);
          QStyle_buttonRect(Application.Style.Handle,
                            @PaintRect,
                            PaintRect.Left,
                            PaintRect.Top,
                            PaintRect.Right - PaintRect.Left,
                            PaintRect.Bottom - PaintRect.Top);
          Canvas.Stop;
          {$endif}
        end;
      end;
    end
    else
    begin
      if FUseArrow then
      begin
        dw := GetArrowSize;
        aw := dw;
        Dec(PaintRect.Right, dw);
        {$ifdef CLX_USED}
        Dec(PaintRect.Right, 2);
        {$endif}
      end;
      {$ifndef CLX_USED}
      if MoneyFlat then
      begin
        if (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]) then
          Canvas.Brush.Color := MoneyFlatDownColor
        else
        if (FMouseInControl or FMouseInArrow) and (FState <> ebsDisabled) then
          Canvas.Brush.Color := MoneyFlatActiveColor
        else
          Canvas.Brush.Color := MoneyFlatInactiveColor;
        Canvas.FrameRect(PaintRect);
      end
      else
      {$endif}
      begin
        if (FDown and FIsSwitch) or (FState in [ebsDown, {ebsArrDown, }ebsExclusive]) or
          ((FMouseInControl or FMouseInArrow) and (FState <> ebsDisabled)) or Focused or
          (csDesigning in ComponentState) then
        begin
        {$ifndef CLX_USED}
          if ((Background.Empty) or (BackgroundDrawBorder)) and ShowBorder then
             DrawButtonFrameEx(Canvas.Handle, PaintRect, false,
                             (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]), AColor, ThinFrame);
        {$else}
          if ((Background.Empty) or (BackgroundDrawBorder)) and ShowBorder then
          begin
            Canvas.Start;
            with PaintRect do
              QStyle_drawButton(Application.Style.Handle,
                                Canvas.Handle,
                                Left,
                                Top,
                                Right - Left,
                                Bottom - Top,
                                Palette.ColorGroup(GetColorGroup(Self)),
                                (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]),
                                Canvas.Brush.Handle);
            Canvas.Stop;
          end;
        {$endif}
        end;
      end;
    end;

    // draw the rectangle for the arrow
    if FUseArrow then
    begin
      if dw = 0 then
      begin
        dw := GetArrowSize;
        Dec(PaintRect.Right, dw);
        {$ifdef CLX_USED}
        Dec(PaintRect.Right, 2);
        {$endif}
      end;
      ArrRect := Rect(PaintRect.Right, 0, PaintRect.Right + dw, Height);
      {$ifdef CLX_USED}
      inc(ArrRect.Left, 2);
      inc(ArrRect.Right, 2);
      {$endif}
      if Default and DrawDefaultFrame then
        InflateRect(ArrRect, 0, -1);
      Canvas.Brush.Color := Color;
      if ((Background.Empty) or (BackgroundDrawBorder)) and FShowBorder then
      begin
        {$ifndef CLX_USED}
        if MoneyFlat then
        begin
          if (FState in [ebsDown, ebsArrDown]) then
            Canvas.Brush.Color := MoneyFlatDownColor
          else
          if (FMouseInControl or FMouseInArrow) and (FState <> ebsDisabled) then
            Canvas.Brush.Color := MoneyFlatActiveColor
          else
            Canvas.Brush.Color := MoneyFlatInactiveColor;
          Canvas.FrameRect(ArrRect);
        end
        else
        {$endif}
        begin
          if ((not FFlat) or ((FMouseInControl or FMouseInArrow) or
              (FState in [ebsArrDown, ebsExclusive])
               or (csDesigning in ComponentState))) then
            {$ifndef CLX_USED}
            DrawButtonFrameEx(Canvas.Handle, ArrRect, (FMouseInArrow or FOldStyled) and (not FThinFrame) and (not FDown), (FState in [ebsDown, ebsArrDown]), AColor, ThinFrame);
            {$else}
            begin
              Canvas.Start;
              with ArrRect do
              QStyle_drawButton(Application.Style.Handle,
                                Canvas.Handle,
                                Left,
                                Top,
                                Right - Left,
                                Bottom - Top,
                                Palette.ColorGroup(GetColorGroup(Self)),
                                (FState in [ebsDown, ebsArrDown]),
                                Canvas.Brush.Handle);
              Canvas.Stop;
            end;
            {$endif}
        end;
      end;
      {$ifndef CLX_USED}
      DrawArrow(Canvas, eadDown, ArrRect, Font.Color, FState <> ebsDisabled);
      {$else}
      Canvas.Start;
      QStyle_buttonRect(Application.Style.Handle,
                        @ArrRect,
                        ArrRect.Left,
                        ArrRect.Top,
                        ArrRect.Right - ArrRect.Left,
                        ArrRect.Bottom - ArrRect.Top);

      with ArrRect do
        QStyle_drawArrow(Application.Style.Handle,
                         Canvas.Handle,
                         ArrowType_DownArrow,
                         (FState in [ebsDown, ebsArrDown]),
                         Left,
                         Top,
                         Right - Left,
                         Bottom - Top,
                         Palette.ColorGroup(GetColorGroup(Self)),
                         true,
                         Canvas.Brush.Handle);
      Canvas.Stop;
      {$endif}
    end;
  end
  {$ifdef MSWINDOWS}
  else
  begin
    {$ifndef CLX_USED}
    DrawThemeBackground(Theme, Canvas.Handle, BP_PUSHBUTTON, 0, PaintRect, nil);
    {$else}
    Canvas.Start;
    DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_PUSHBUTTON, 0, PaintRect, nil);
    Canvas.Stop;
    {$endif}

    if not Enabled then
      sid := PBS_DISABLED
    else
    if (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]) then
      sid := PBS_PRESSED
    else
    if FMouseInControl or FMouseInArrow then
      sid := PBS_HOT
    else
    if (Default and DrawDefaultFrame) or Focused then
      sid := PBS_DEFAULTED
    else
      sid := PBS_NORMAL;
    if FUseArrow then
    begin
      dw := GetArrowSize;
      aw := dw;
      Dec(PaintRect.Right, dw);
    end;
    RClip := Canvas.ClipRect;
    {$ifndef CLX_USED}
    DrawThemeBackground(Theme, Canvas.Handle, BP_PUSHBUTTON, sID, PaintRect, @RClip);
    {$else}
    Canvas.Start;
    DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_PUSHBUTTON, sID, PaintRect, @RClip);
    Canvas.Stop;
    {$endif}

    if FUseArrow then
    begin
      if dw = 0 then
      begin
        dw := GetThemeSysSize(FArrTheme, SM_CXVSCROLL);
        Dec(PaintRect.Right, dw);
      end;
      ArrRect := Rect(PaintRect.Right, PaintRect.Top, PaintRect.Right + dw, PaintRect.Bottom);

      RClip := Canvas.ClipRect;
      if not Enabled then
        sid1 := CBXS_DISABLED
      else
      if FState in [ebsArrDown, ebsExclusive] then
        sid1 := CBXS_PRESSED
      else
      if FMouseInControl or FMouseInArrow then
        sid1 := CBXS_HOT
      else
        sid1 := CBXS_NORMAL;
      {$ifndef CLX_USED}
      DrawThemeBackground(FArrTheme, Canvas.Handle, CP_DROPDOWNBUTTON, sid1, ArrRect, @RClip);
      {$else}
      Canvas.Start;
      DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), CP_DROPDOWNBUTTON, sid1, ArrRect, @RClip);
      Canvas.Stop;
      {$endif}
    end;
  end
  {$endif}
  ;

  Offset.x := 0;
  Offset.y := 0;

  if (FState in [ebsDown, ebsExclusive]) or (FDown and FIsSwitch) then
  begin
    if (FState = ebsExclusive) or
       (UseImageList and ((DownImages = nil) or (DownImages = Images))) or
       (UseIcon) or
       ((not UseImageList) and (UseIcon) and (NumGlyphs < 2)) then
    begin
      Offset.X := 1;
      Offset.Y := 1;
    end;
  end;
  inc(PaintRect.Right, dw);

  if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) and (FDownImages <> nil) then
  begin
    SaveIL := FGlyph.FImageList;
    FGlyph.FImageList := FDownImages;
  end;

  if (FGlyph.FImageList = Self.FDisabledImages) and (FDisabledImages <> nil) and (FState = ebsDisabled) then
    GlyphState := ebsUp
  else
    GlyphState := FState;

  if AdjustSpaceForGlyph then
  begin
    if (not FThinFrame) then
      InflateRect(PaintRect, -2, -2)
    else
      InflateRect(PaintRect, -1, -1);
  end;
  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    FRender.Data.DefaultColor := Font.Color;
    FRender.Data.DefaultBgColor := clNone;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.LinkColor := LinkColor;
    FRender.Data.LinkStyle := LinkStyle;
    FRender.Data.Charset := Font.Charset;
  end;
  {$endif}

  FTextRect := FGlyph.Draw(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
    FSpacing, FState, GlyphState, Alignment, true, FMultiline, (Focused and FShowFocus), FShowGlyph,
    FShowText, Aw, TextDrawType, Color, {$ifdef MSWINDOWS}ParentFont and (Theme <> 0), Theme, BP_PUSHBUTTON, sid{$else}false,0,0,0{$endif}, true, ImageIsAlphaBlended{$ifdef HAS_HTML_RENDER}, IsHTML, FRender{$endif}, FChangeDisabledText);

  if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) and (FDownImages <> nil) then
  begin
    FGlyph.FImageList := SaveIL;
  end;
  if (Focused and FShowFocus) then
  begin
    R := PaintRect;
    dec(R.right, dw);
    {$ifdef MSWINDOWS}
    InflateRect(R, -2, -2);
    {$endif}
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Color := clBtnFace;
    {$ifndef CLX_USED}
    DrawFocusRect(Canvas.Handle, R);
    {$else}
    Canvas.DrawFocusRect(R);
    {$endif}
  end;

  {$ifndef CLX_USED}
  bitblt(Self.Canvas.Handle, 0, 0, ClientWidth, ClientHeight, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  {$else}
  Self.Canvas.CopyRect(ClientRect, Bitmap.Canvas, ClientRect);
  {$endif}
  Bitmap.Free;
end;

{$HINTS OFF}
procedure TCustomElPopupButton.AClick;
var
  q : TPoint;
  i : THandle;
var
{$IFNDEF VER90}
  Form : TCustomForm;
{$ELSE}
  Form : TForm;
{$ENDIF}

{$ifndef CLX_USED}
{$IFDEF VCL_4_USED}
const
  Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
{$ENDIF}
{$endif}

begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.ModalResult := ModalResult;
  if Arrow then
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(ArrowClickSound);
{$ENDIF}
    if Assigned(FOnArrowClick) then FOnArrowClick(Self);
  end
  else
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(ClickSound);
{$ENDIF}
    inherited Click;
    //FMouseInControl := false;
    Invalidate;
  end;
  if (PullDownMenu = nil) or (FDisableAp) or ((not Arrow) and FUseArrow) then exit;
  PullDownMenu.PopupComponent := Self;
  PullDownMenu.AutoPopup := True;
  q.X := 0;
  q.Y := 0;
  q := ClientToScreen(q);

  if FPopupPlace = ppRight then
  begin
    q.x := q.X + Width;
    q.y := q.Y - 1;
    //PullDownMenu.Popup(q.X + Width, q.Y - 1);
  end else
  if FPopupPlace = ppDown then
  begin
    q.x := q.X - 1;
    q.y := q.Y + height;
    //PullDownMenu.Popup(q.X - 1, q.Y + Height)
  {$ifndef CLX_USED}
  end
  else
  begin
    q.x := q.x - 1;
    q.y := q.y - GetSystemMetrics(SM_CYMENU) * PullDownMenu.Items.Count - GetSystemMetrics(SM_CXEDGE);
    //PullDownMenu.Popup(q.X - 1, q.Y - GetSystemMetrics(SM_CYMENU) * PullDownMenu.Items.Count);
  {$endif}
  end;

  PullDownMenu.PopupComponent := Self;
  {$ifndef CLX_USED}
  if not FUseArrow then
  begin
    FState := ebsDown;
    FOrigState := ebsDown;
    Invalidate;
  end;
  FInMenu := true;
  if Assigned(PullDownMenu.OnPopup) then PullDownMenu.OnPopup(PullDownMenu);
  TrackPopupMenu(GetMenuHandle(PullDownMenu), TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON, q.X, q.Y, 0, Handle, nil);
  FState := ebsUp;
  {$else}
  PullDownMenu.Popup(q.X, q.Y);
  {$endif}
end;

{$ifndef CLX_USED}
{$HINTS ON}
procedure TCustomElPopupButton.CreateParams(var Params : TCreateParams);
//const
//  ButtonStyles : array[Boolean] of DWORD = (BS_PUSHBUTTON, BS_DEFPUSHBUTTON);
begin
  inherited CreateParams(Params);
  //Params.Style := Params.Style or ButtonStyles[FDefault];
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElPopupButton.CreateWnd;
{$else}
procedure TCustomElPopupButton.CreateWidget;
{$endif}
begin
  inherited;
  FActive := FDefault;
end;

{$ifndef CLX_USED}
procedure TCustomElPopupButton.CNCommand(var Message : TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then
    Click;
end;

procedure TCustomElPopupButton.CMDialogKey(var Message : TCMDialogKey);
begin
  with Message do
    if (((CharCode = VK_RETURN) and FDefault) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TCustomElPopupButton.CMFocusChanged(var Message : TCMFocusChanged);
var
  b : boolean;
begin
  b := FActive;
  FActive := Message.Sender = Self;
  SetButtonStyle(FActive);
  if b <> FActive then
     Invalidate;
  inherited;
end;

{$endif}

procedure TCustomElPopupButton.UpdateExclusive;
{$ifndef CLX_USED}
var
  Msg : TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;
{$else}
var
  Msg: TCMButtonPressed;
  I: Integer;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.Control := Self;
    Msg.Index := FGroupIndex;
    Parent.Broadcast(Msg);
    for I := 0 to Parent.ControlCount-1 do
      if Parent.Controls[I] is TCustomElPopupButton then
        TCustomElPopupButton(Parent.Controls[I]).ButtonPressed(Self, FGroupIndex);
  end;
end;
{$endif}

procedure TCustomElPopupButton.GlyphChanged(Sender : TObject);
begin
  if (not (csLoading in ComponentState)) and
     (not UseIcon) and (not UseImageList) then
  Invalidate;
end;

{$ifndef CLX_USED}
function TCustomElPopupButton.GetPalette : HPALETTE;
begin
  Result := Glyph.Palette;
end;
{$endif}

function TCustomElPopupButton.GetGlyph : TBitmap;
begin
  Result := FGlyph.Glyph;
end;

procedure TCustomElPopupButton.SetGlyph(Value : TBitmap);
begin
  FGlyph.Glyph := Value;
  FGlyph.ResetNumGlyphs;
  Invalidate;
end;

function TCustomElPopupButton.GetNumGlyphs : TNumGlyphs;
begin
  Result := FGlyph.NumGlyphs;
end;

procedure TCustomElPopupButton.SetNumGlyphs(Value : TNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else if Value > 4 then
    Value := 4;
  // if Value <> FNumGlyphs then
  begin
    FNumGlyphs := Value;
    if (ComponentState * [csReading, csLoading] = []) then
    begin
      FGlyph.NumGlyphs := Value;
      Invalidate;
    end;
  end;
end;

procedure TCustomElPopupButton.SetDown(Value : Boolean);
begin
  if csLoading in ComponentState then
  begin
    FDown := value;
    exit;
  end;
  if (FGroupIndex = 0) and (not FIsSwitch) then Value := False;
  if FDown and (not FAllowAllUp) and (not FIsSwitch) then Exit;
  FDown := Value;
  if Value then
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
    if FIsSwitch then
      FState := ebsDown
    else
      FState := ebsExclusive;
    Invalidate;
  end
  else
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
    FState := ebsUp;
    Invalidate;
  end;
  if Value then UpdateExclusive;
end;

procedure TCustomElPopupButton.SetFlat(Value : Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    if Value then
    begin
      OldStyled := false;
    end;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetGroupIndex(Value : Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TCustomElPopupButton.SetLayout(Value : TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetMargin(Value : Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetSpacing(Value : Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetAllowAllUp(Value : Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElPopupButton.WMLButtonDblClk(var Message : TWMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;
{$endif}

procedure TCustomElPopupButton.UpdateTracking;
var
  P : TPoint;
begin
  if not Enabled then FMouseInControl := false;

  if FFlat and Enabled then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    {$ifndef CLX_USED}
    if Enabled and FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
    {$else}
    if Enabled and FMouseInControl then
      MouseLeave(Self)
    else
      MouseEnter(Self);
    {$endif}
  end;
end;

procedure TCustomElPopupButton.Loaded;
var
  State : TElButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := ebsUp
  else
    State := ebsDisabled;
  if FUseImageList then FGlyph.UseImageList := true;
  FGlyph.CreateButtonGlyph(State);
  FGlyph.NumGlyphs := FNumGlyphs;
  if FDown then
  begin
    FDown := false;
    Down := true;
  end;
  Invalidate;
end;

procedure TCustomElPopupButton.IntEnabledChanged;
const
  NewState : array[Boolean] of TElButtonState = (ebsDisabled, ebsUp);
begin
  if FGlyph <> nil then
  begin
    FGlyph.CreateButtonGlyph(NewState[Enabled]);
    if UseImageList then
    begin
      if (not Enabled) and (FDisabledImages <> nil) then
         FGlyph.ImageList := FDisabledImages
      else
         FGlyph.ImageList := FImageList;
    end;
  end;
  UpdateTracking;
  Invalidate;
end;

{$ifndef CLX_USED}
procedure TCustomElPopupButton.CMEnabledChanged(var Message : TMessage);
begin
  inherited;
  IntEnabledChanged;
end;
{$else}
procedure TCustomElPopupButton.EnabledChanged;
begin
  inherited;
  IntEnabledChanged;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElPopupButton.CMButtonPressed(var Message : TMessage);
var
  Sender : TCustomElPopupButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TCustomElPopupButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
        FState := ebsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TCustomElPopupButton.CMFontChanged(var Message : TMessage);
begin
  Invalidate;
end;

procedure TCustomElPopupButton.CMColorChanged(var Message : TMessage);
var ButtonColor : TColor;
    hls1, hls2, hls3, hls4 : integer;
    lum : integer;

begin
  inherited;
  if not ShadowsUseCustom then
  begin
    ButtonColor := ColorToRGB(Color);

    hls1 := RGBtoHLS(ButtonColor);
    hls2 := hls1;
    hls3 := hls1;
    hls4 := hls1;

    lum := Hi(LoWord(hls3));
    hls1 := (Min(239, (Hi(LoWord(hls3))  + lum div 3)) shl 8) or (hls1 and $FF00FF);
    hls2 := (Min(239, (Hi(LoWord(hls3))  - lum div 2)) shl 8) or (hls2 and $FF00FF);
    hls4 := (Min(239, (Hi(LoWord(hls3))  - lum div 3)) shl 8) or (hls4 and $FF00FF);

    FShadowBtnHighlight := HLStoRGB(hls1);
    FShadowBtnDkShadow  := HLStoRGB(hls2);
    FShadowBtnShadow    := HLStoRGB(hls4);
  end;
  Invalidate;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElPopupButton.CMTextChanged(var Message : TMessage);
begin
  inherited;
  IntTextChanged;
end;

procedure TCustomElPopupButton.CMSysColorChange(var Message : TMessage);
begin
  with TElButtonGlyph(FGlyph) do
  begin
    Invalidate;
    CreateButtonGlyph(FState);
  end;
end;

procedure TCustomElPopupButton.CMMouseEnter(var Message : TMessage);
begin
  inherited;
  IntMouseEnter;
end;

procedure TCustomElPopupButton.CMMouseLeave(var Message : TMessage);
begin
  inherited;
  IntMouseLeave;
end;
{$endif}

procedure TCustomElPopupButton.MouseDown(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
var
  l : integer;
{$IFDEF HAS_HTML_RENDER}
var
  P : TPoint;
  R : TRect;
  href : TElFString;
{$endif}
begin
  {$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin
    P.x := X;
    P.y := Y;
    R := FTextRect;
    if IsHTML and FRender.IsCursorOverLink(Point(P.X - R.Left, P.Y - R.Top), Point(0, 0), R, href) then
      exit;
  end;
  {$endif}
  FOrigState := ebsUp;
  if (csDesigning in ComponentState) or ((Parent <> nil) and (csDesigning in Parent.ComponentState)) then exit;
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    l := 11;
    if not FFlat then inc(l, 4);
    if (not FDown) and ((FUseArrow and not InRange(Width - l, Width, X)) or not FUseArrow) then
    begin
      FState := ebsDown;
{$IFDEF USE_SOUND_MAP}
      if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
      Invalidate;
    end
    else
    if (not FArrDown) and (FUseArrow and InRange(Width - l, Width, X)) then
    begin
{$IFDEF USE_SOUND_MAP}
      if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
      if FIgnoreClick then
        FIgnoreClick := false
      else
      begin
        FState := ebsArrDown;
        Invalidate;
        FOrigState := ebsArrDown;
        AClick(true);
        if not FInMenu then
          FDragging := true;
      end;
      exit;
    end;
    FDragging := True;
    FMouseInControl := true;
  end
  else
  if (Button <> mbLeft) then
    FIgnoreClick := false;
end;

procedure TCustomElPopupButton.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  NewState : TElButtonState;
var
  l   : integer;
  b        : boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if (not FDown) and (not FArrDown) then
      NewState := ebsUp
    else
      NewState := ebsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
    begin
      if FDown then
        NewState := ebsExclusive
      else
      begin
        l := GetArrowSize;
        // if not FFlat then inc(l, 4);
        if (FUseArrow and InRange(Width - l, Width, X)) then
          NewState := ebsArrDown
        else
          NewState := ebsDown;
      end;
    end
    else
    begin
      if UseArrow then
      begin
        FMouseInArrow := false;
        Invalidate;
      end;
    end;
    if NewState <> FState then
    begin
      FState := NewState;
      if FState = ebsDown then
      begin
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
      end
      else if FState = ebsUp then
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
      UpdateTracking;
      Invalidate;
    end;
  end
  else
  if UseArrow then
  begin
    //b := FMouseInArrow;
    FMouseInArrow := X > ClientWidth - GetArrowSize;

    if FMouseInArrow then
    begin
      b := FMouseInControl = false;
      FMouseInControl := false;
    end
    else
    begin
      b := FMouseInControl = false;
      FMouseInControl := true;
    end;
        
    if b <> FMouseInArrow then
      Invalidate;
  end;
end;

procedure TCustomElPopupButton.MouseUp(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
var
  DoClick : Boolean;
  ArrClick : Boolean;
  l : integer;
  {$IFDEF HAS_HTML_RENDER}
  P : TPoint;
  R : TRect;
  href : TElFString;
  {$endif}
begin
  inherited;
  {$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    R := FTextRect;

    if IsHTML and FRender.IsCursorOverLink(Point(P.X - R.Left, P.Y - R.Top), Point(0, 0), R, href) then
    begin
      TriggerLinkClickEvent(href);
      exit;
    end;
  end;
  {$endif}
  if FDragging and (Button = mbLeft) then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    l := 11;
    if not FFlat then inc(l, 4);
    ArrClick := (FUseArrow and InRange(Width - l, Width, X));
    if FGroupIndex = 0 then
    begin
      if FIsSwitch and (not ArrClick) then
      begin
        if FOrigState <> ebsArrDown then
        begin
          FDown := not FDown;
          if FDown then
          begin
            FState := ebsDown;
{$IFDEF USE_SOUND_MAP}
            if SoundMap <> nil then SoundMap.Play(DownSound);
{$endif}
          end
          else
          begin
            FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
            if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
          end;
        end else
        begin
          FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
          if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
        end;
      end
      else
      begin
        if ArrClick and FInMenu then DoClick := false else
        begin
          FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
          if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
        end;
      end;
      FMouseInControl := PtInRect(ClientRect, Point(X, Y));
      if not FMouseInControl then IntMouseLeave;
      if not (FState in [ebsExclusive, ebsArrDown, ebsDown]) then Invalidate;
    end
    else
    if DoClick then
    begin
      SetDown(not FDown);
      if FDown or FArrDown then Invalidate;
    end else
    begin
      if FDown then FState := ebsExclusive;
      Invalidate;
    end;
    if DoClick then AClick(ArrClick);
  end;
end;

{$ifndef CLX_USED}

{$ifdef VCL_5_USED}
procedure TCustomElPopupButton.WMContextMenu(var Message: TWMContextMenu);
var
  Pt, Temp: TPoint;
  Handled: Boolean;
  href : TElFString;
  R    : TRect;
begin
  if csDesigning in ComponentState then Exit;

  {$ifdef HAS_HTML_RENDER}
  Pt := ScreenToClient(SmallPointToPoint(Message.Pos));
  if IsHTML then
  begin
    R := FTextRect;
    if (LinkPopupMenu <> nil) and FRender.IsCursorOverLink(Point(Pt.X - R.Left, Pt.Y - R.Top), Point(0, 0), R, href) then
    begin
      Handled := False;
      Temp := ClientToScreen(Pt);
      DoContextPopup(Temp, Handled);
      Message.Result := Ord(Handled);
      if Handled then Exit;

      DoLinkPopup(ClientToScreen(Pt));
      Message.Result := 1;
    end
    else
      inherited
  end
  else
  {$endif}
    inherited;
end;
{$endif}

procedure TCustomElPopupButton.WMRButtonUp(var Message: TWMRButtonUp);
{$ifndef VCL_5_USED}
var P : TPoint;
    R : TRect;
    href : TElFString;
{$endif}
begin
  {$ifndef VCL_5_USED}
  if IsHTML then
  begin
    P := SmallPointToPoint(Message.Pos);
    R := FTextRect;
    if (LinkPopupMenu <> nil) and FRender.IsCursorOverLink(Point(P.X - R.Left, P.Y - R.Top), Point(0, 0), R, href) then
    begin
      if not (csNoStdEvents in ControlStyle) then
        with Message do
          MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos);
      if Message.Result = 0 then
      begin
        DoLinkPopup(P);
      end;
    end
    else
      inherited;
  end
  else
  {$endif}
    inherited;
end;

procedure TCustomElPopupButton.SetButtonStyle(ADefault : Boolean);
const
  BS_MASK = $000F;
var
  Style : Word;
begin
  if HandleAllocated then
  begin
    if ADefault then
      Style := BS_DEFPUSHBUTTON
    else
      Style := BS_PUSHBUTTON;
    if GetWindowLong(Handle, GWL_STYLE) and BS_MASK <> Style then
      SendMessage(Handle, BM_SETSTYLE, Style, 1);
  end;
end;

type
  TMenuItemEx = class(TMenuItem);

{$HINTS OFF}
procedure TCustomElPopupButton.WndProc(var Message : TMessage);
var P : TPoint;
    I : Integer;
    MenuItem: TMenuItem;
    FindKind: TFindItemKind;
    ContextID: Integer;
    DC: HDC;
    R : TRect;
    Item: Integer;

begin
  if Message.Msg = MenuCancelMsg then
  begin
    FInMenu := false;
    FDragging := false;
    FArrDown := false;
    FState := ebsUp;
    FMouseInControl := false;
    FMouseInArrow := false;
    Invalidate;
    Exit;
  end;

  //if FInMenu then
  try
    case Message.Msg of
{$IFNDEF VCL_5_USED}
      WM_COMMAND:
        if FPulldownMenu.DispatchCommand(Message.wParam) then Exit;
      WM_INITMENUPOPUP:
        with TWMInitMenuPopup(Message) do
          if FPulldownMenu.DispatchPopup(MenuPopup) then Exit;
      WM_MENUSELECT:
        with TWMMenuSelect(Message) do
        begin
          FindKind := fkCommand;
          if MenuFlag and MF_POPUP <> 0 then
            FindKind := fkHandle;
          MenuItem := FPulldownMenu.FindItem(IDItem, FindKind);
          if MenuItem <> nil then
          begin
            Application.Hint := MenuItem.Hint;
            Exit;
          end;
          Application.Hint := '';
        end;
      WM_HELP:
        with PHelpInfo(Message.LParam)^ do
        begin
            if (FPullDownMenu <> nil) and (FPulldownMenu.Handle = hItemHandle) then
            begin
              ContextID := FPulldownMenu.GetHelpContext(iCtrlID, True);
              if ContextID = 0 then
                ContextID := FPulldownMenu.GetHelpContext(hItemHandle, False);
              if Screen.ActiveForm = nil then Exit;
              if (biHelp in Screen.ActiveForm.BorderIcons) then
                Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID)
              else
                Application.HelpContext(ContextID);
              Exit;
            end;
        end;
{$ELSE}
      WM_COMMAND,
      WM_INITMENUPOPUP,
      WM_MENUSELECT,
      WM_DRAWITEM,
      WM_MEASUREITEM,
      WM_MENUCHAR,
      WM_HELP:
        begin
        {$ifdef ELPACK_COMPLETE}
        if (PulldownMenu is TElPopupMenu) then
          with Message do
            SendMessage(GetParentForm(Self).Handle, Msg, wParam, lParam)
        else
        {$endif}
          with Message do
            SendMessage(PopupList.Window, Msg, wParam, lParam);
        end;
{$ENDIF}
      WM_EXITMENULOOP:
        begin
          GetCursorPos(P);
          P := ScreenToClient(P);
          FDragging := true;
          PostMessage(Handle, MenuCancelMsg, 0, 0);
          if PtInRect(Rect(ClientWidth - GetArrowSize, 0, ClientWidth, ClientHeight), P) then
             FIgnoreClick := true;
          {$IFDEF VCL_5_USED}
          with Message do
               SendMessage(PopupList.Window, Msg, wParam, lParam);
          {$ENDIF}
        end;
      WM_QUERYENDSESSION:
        begin
          Message.result := 1;
          exit;
        end;
      end;
  except
    Application.HandleException(Self);
  end;
  if Message.Msg = CN_COMMAND then
    if FClicksDisabled then Exit;
  inherited WndProc(Message);
end;

{$HINTS ON}
procedure TCustomElPopupButton.WMKeyUp(var Msg: TWMKeyUp);
begin
  IntKeyUp(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
end;

procedure TCustomElPopupButton.WMGetDlgCode(var Msg : TMessage);
begin
  inherited;
  Msg.Result := DLGC_BUTTON or DLGC_WANTALLKEYS;
end;

procedure TCustomElPopupButton.WMKeyDown(var Message : TWMKeyDown); { private }
begin
  if not IntKeyDown(Message.CharCode, KeyDataToShiftState(Message.KeyData)) then
    inherited;
end; { WMKeyDown }
{$endif}


procedure TCustomElPopupButton.SetShowFocus(newValue : Boolean);
begin
  if (FShowFocus <> newValue) then
  begin
    FShowFocus := newValue;
    Invalidate;
  end; { if }
end; { SetShowFocus }

procedure TCustomElPopupButton.SetUseArrow(newValue : Boolean);
begin
  if (FUseArrow <> newValue) then
  begin
    FUseArrow := newValue;
    Invalidate;
  end; { if }
end; { SetUseArrow }

procedure TCustomElPopupButton.SetShowGlyph(newValue : Boolean);
begin
  if (FShowGlyph <> newValue) then
  begin
    FShowGlyph := newValue;
    Invalidate;
  end; { if }
end; { SetShowGlyph }

procedure TCustomElPopupButton.SetShowText(newValue : Boolean);
begin
  if (FShowText <> newValue) then
  begin
    FShowText := newValue;
    Invalidate;
  end; { if }
end; { SetShowText }

procedure TCustomElPopupButton.SetPullDownMenu(newValue : TPopupMenu);
begin
  if (FPullDownMenu <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FPullDownMenu <> nil then
      FPullDownMenu.RemoveFreeNotification(Self);
    {$endif}
    FPullDownMenu := newValue;
    if FPullDownMenu = nil then
      FDisableAp := true
    else
    begin
      FDisableAp := false;
      FPulldownMenu.FreeNotification(Self);
    end;
  end; { if }
end; { SetPullDownMenu }

function TCustomElPopupButton.GetIcon : TIcon;
begin
  result := FGlyph.Icon;
end;

procedure TCustomElPopupButton.SetIcon(newValue : TIcon);
begin
  FGlyph.Icon.Assign(newValue);
end;

procedure TCustomElPopupButton.SetIsSwitch(newValue : Boolean);
begin
  if (FIsSwitch <> newValue) then
    FIsSwitch := newValue;
end;

{$IFDEF USE_SOUND_MAP}
procedure TCustomElPopupButton.SetSoundMap(newValue : TElSoundMap);
begin
  if (FSoundMap <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FSoundMap <> nil then
      FSoundMap.RemoveFreeNotification(Self);
    {$endif}
    FSoundMap := newValue;
    {$ifdef VCL_5_USED}
    if FSoundMap <> nil then
      FSoundMap.FreeNotification(Self);
    {$endif}
  end;
end; { SetSoundMap }
{$ENDIF}

{$ifdef CLX_USED}
procedure TCustomElPopupButton.DoContextPopup(const MousePos: TPoint; var Handled: Boolean);
{$IFDEF HAS_HTML_RENDER}
var href : TElFString;
    Temp : TPoint;
    R    : TRect;
    {$ifdef CLX_USED}
    Grabber: QWidgetH;
    {$endif}
{$endif}
begin
  {$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin
    R := FTextRect;
    if (LinkPopupMenu <> nil) and FRender.IsCursorOverLink(Point(MousePos.X - R.Left, MousePos.Y - R.Top), Point(0, 0), R, href) then
    begin
      MouseUp(mbRight, [ssLeft], MousePos.X, MousePos.Y);
      Temp := ClientToScreen(MousePos);
      Grabber := QWidget_mouseGrabber;
      if Grabber <> nil then
        QWidget_releaseMouse(Grabber);
      LinkPopupMenu.PopupComponent := Self;
      LinkPopupMenu.Popup(Temp.X, Temp.Y);
      Handled := true;
    end
    else
      inherited
  end
  else
  {$endif}
    inherited DoContextPopup(MousePos, Handled);
end;
{$endif}

procedure TCustomElPopupButton.SetShowBorder;
begin
  if FShowBorder <> newValue then
  begin
    FShowBorder := newValue;
    Invalidate;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElPopupButton.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TCustomElPopupButton.SetImageForm(newValue : TElImageForm);
begin
  if FImgForm <> newValue then
  begin
    {$ifdef VCL_5_USED}
    if (FImgForm <> nil) and (not (csDestroying in FImgForm.ComponentState)) then
      FImgForm.RemoveFreeNotification(Self);
    {$endif}
    if FImgForm <> nil then
      FImgForm.UnRegisterChanges(FImgFormChLink);
    if newValue <> nil then
       newValue.FreeNotification(Self);
    FImgForm := newValue;
    if FImgForm <> nil then
      FImgForm.RegisterChanges(FImgFormChLink);
    if HandleAllocated then Invalidate;
  end;
end;
{$endif}

procedure TCustomElPopupButton.Notification(AComponent : TComponent; operation : TOperation);
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) then
  begin
{$IFDEF USE_SOUND_MAP}
    if (AComponent = FSoundMap) then SoundMap := nil;
{$ENDIF}
    if (AComponent = FImageList) then Images := nil;
    if (AComponent = FHotImages) then HotImages := nil;
    if (AComponent = FDownImages) then DownImages := nil;
    if (AComponent = FDisabledImages) then DisabledImages := nil;
    if AComponent = FPullDownMenu then PullDownMenu :=nil;
   {$ifdef HAS_HTML_RENDER}
   if AComponent = LinkPopupMenu then
     LinkPopupMenu := nil;
   {$endif}
    {$ifndef CLX_USED}
    if AComponent = FImgForm then ImageForm := nil;
    {$endif}
  end; { if }
end; { Notification }

function TCustomElPopupButton.GetImageIndex : integer;
begin
  result := FGlyph.FImageIndex;
end;

procedure TCustomElPopupButton.SetImageIndex(newValue : Integer);
begin
  if (FGlyph.FImageIndex <> newValue) then
  begin
    FGlyph.ImageIndex := newValue;
    if FGlyph.UseImageList then Invalidate;
  end; {if}
end;

procedure TCustomElPopupButton.SetImageList(newValue : TImageList);
var b : boolean;
begin
  if FImageList <> newValue then
  begin
    b := FImageList = nil;
    if not b then
    begin
        FImageList.UnregisterChanges(FChLink);
      {$ifdef VCL_5_USED}
      if (not (csDestroying in FImageList.ComponentState)) then
        FImageList.RemoveFreeNotification(Self);
      {$endif}
    end;
    FImageList := newValue;
    if FImageList <> nil then
    begin
      FImageList.RegisterChanges(FChLink);
      FImageList.FreeNotification(Self);
    end;
    FGlyph.ImageList := newValue;
    if FImageList = nil then
      UseImageList := false
    else
    if b and (not (csLoading in ComponentState)) then UseImageList := true;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetUseIcon(newValue : Boolean);
begin
  if (FUseIcon <> newValue) then
  begin
    FUseIcon := newValue;
    FGlyph.UseIcon := newValue;
    if not UseImageList then Invalidate;
  end; { if }
end; { SetUseIcon }

function TCustomElPopupButton.GetUseImageList : boolean;
begin
  result := FUseImageList;
end;

procedure TCustomElPopupButton.SetUseImageList(newValue : Boolean);
begin
  if (UseImageList <> newValue) then
  begin
    FUseImageList := newValue;
    if not (csLoading in ComponentState) then
    begin
      FGlyph.UseImageList := newValue;
      Invalidate;
    end;
  end; { if }
end; { SetUseImageList }

procedure TCustomElPopupButton.SetOldStyled(newValue : Boolean);
{ Sets data member FOldStyled to newValue. }
begin
  if (FOldStyled <> newValue) then
  begin
    if not (newValue and Flat) then
    begin
      FOldStyled := newValue;
      Repaint;
    end;
  end; { if }
end; { SetOldStyled }

procedure TCustomElPopupButton.ImagesChanged(Sender : TObject);
begin
  if FUseImageList then
  begin
    if (FMouseInControl and Enabled) and (Sender = Images) then Invalidate else
    if (not Enabled) and (Sender = DisabledImages) then Invalidate else
    if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) and (Sender = DownImages) then Invalidate else 
    if Sender = FImageList then Invalidate;
  end else
  begin
    if (Sender = FBackGround) or
         ((Sender = FDownBackground) and (FDown and FIsSwitch) or
         (FState in [ebsDown, ebsExclusive]))  then Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetDownImages(newValue : TImageList);
begin
  if (FDownImages <> newValue) then
  begin
    if FDownImages <> nil then
    begin
      {$ifdef VCL_5_USED}
      FDownImages.RemoveFreeNotification(Self);
      {$endif}
      FDownImages.UnregisterChanges(FNChLink);
    end;
    FDownImages := newValue;
    if FDownImages <> nil then FDownImages.RegisterChanges(FNChLink);
    if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) then
    begin
      if newValue = nil then FGlyph.ImageList := FImageList else FGlyph.ImageList := newValue;
      Invalidate;
    end;
    if FDownImages <> nil then
      FDownImages.FreeNotification(Self);
  end;  { if }
end;

procedure TCustomElPopupButton.SetHotImages(newValue : TImageList);
begin
  if (FHotImages <> newValue) then
  begin
    if FHotImages <> nil then
    begin
      {$ifdef VCL_5_USED}
      FHotImages.RemoveFreeNotification(Self);
      {$endif}
      FHotImages.UnregisterChanges(FHChLink);
    end;
    FHotImages := newValue;
    if FHotImages <> nil then
    begin
      FHotImages.RegisterChanges(FHChLink);
      FHotImages.FreeNotification(Self);
    end;
    if FMouseInControl then
    begin
      if newValue = nil then FGlyph.ImageList := FImageList else FGlyph.ImageList := newValue;
      Invalidate;
    end;
  end;  { if }
end;  { SetHotImages }

procedure TCustomElPopupButton.SetDisabledImages(newValue : TImageList);
begin
  if (FDisabledImages <> newValue) then
  begin
    if FDisabledImages <> nil then
    begin
      {$ifdef VCL_5_USED}
      FDisabledImages.RemoveFreeNotification(Self);
      {$endif}
      FDisabledImages.UnregisterChanges(FDChLink);
    end;
    FDisabledImages := newValue;
    if FDisabledImages <> nil then
    begin
      FDisabledImages.RegisterChanges(FDChLink);
      FDisabledImages.FreeNotification(Self);
    end;
    if not Enabled then
    begin
      if newValue <> nil then
        FGlyph.ImageList := newValue
      else
        FGlyph.ImageList := FImageList;
      Invalidate;
    end;
  end;  { if }
end;  { SetDisabledImages }

{$ifndef CLX_USED}
procedure TCustomElPopupButton.CMEnter(var Msg: TCMEnter);
begin
  inherited;
  Invalidate;
end;

procedure TCustomElPopupButton.CMExit(var Msg: TCMExit);
begin
  inherited;
  Invalidate;
end;
{$endif}

{$IFDEF VCL_4_USED}

function TCustomElPopupButton.GetActionLinkClass : TControlActionLinkClass;
begin
  result := TElPopupButtonActionLink;
end;

procedure TCustomElPopupButton.ActionChange(Sender : TObject; CheckDefaults : Boolean);
begin
  inherited;
  if Action is TAction then
  begin
    ImageIndex := TAction(Action).ImageIndex;
    Down := TAction(Action).Checked;
  end;
end;
{$ENDIF}

procedure TCustomElPopupButton.SetThinFrame(newValue : Boolean);
{ Sets data member FThinFrame to newValue. }
begin
  if (FThinFrame <> newValue) then
  begin
    FThinFrame := newValue;
    if newValue then FOldStyled := false;
    if OldStyled or ((FMouseInControl or Focused) and (not (FState in [ebsDown, ebsExclusive]))) then Invalidate;
  end;  { if }
end;  { SetThinFrame }

procedure TCustomElPopupButton.SetShadowsUseCustom(Value : Boolean);
begin
  if Value <> FShadowsUseCustom then
  begin
    FShadowsUseCustom := Value;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetShadowBtnHighlight(Value : TColor);
begin
  if Value <> FShadowBtnHighlight then
  begin
    FShadowBtnHighlight := Value;
    if FShadowsUseCustom then Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetShadowBtnShadow(Value : TColor);
begin
  if Value <> FShadowBtnShadow then
  begin
    FShadowBtnShadow := Value;
    if FShadowsUseCustom then Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetShadowBtnDkShadow(Value : TColor);
begin
  if Value <> FShadowBtnDkShadow then
  begin
    FShadowBtnDkShadow := Value;
    if FShadowsUseCustom then Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetShadowFollowsColor(Value : Boolean);
begin
  if Value <> FShadowFollowsColor then
  begin
    FShadowFollowsColor := Value;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetBackgroundDrawBorder(Value : boolean);
begin
  if Value <> FBackgroundDrawBorder then
  begin
    FBackgroundDrawBorder := Value;
    if not FBackground.Empty then Invalidate;      
  end;
end;

procedure TCustomElPopupButton.SetDownBackground(newValue : TBitmap);
{ Sets data member FDownBackground to newValue. }
begin
  FDownBackground.Assign(newValue);
end;  { SetDownBackground }

procedure TCustomElPopupButton.SetBackground(newValue : TBitmap);
{ Sets data member FBackground to newValue. }
begin
  FBackground.Assign(newValue);
end;  { SetBackground }

{$ifdef HAS_HTML_RENDER}
procedure TCustomElPopupButton.SetIsHTML(Value: Boolean);
begin
  if FIsHTML <> Value then
  begin
    FIsHTML := Value;
    if FIsHTML then
    begin
      FRender := TElHTMLRender.Create;
      FRender.OnImageNeeded := TriggerImageNeededEvent;
    end
    else
    begin
      FRender.Free;
      FRender := nil;
    end;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetLinkColor(newValue : TColor);
{ Sets data member FLinkColor to newValue. }
begin
  if (FLinkColor <> newValue) then
  begin
    FLinkColor := newValue;
    if IsHTML then
    begin
      FIsHTML := false;
      IsHTML := true;
    end;
  end;  { if }
end;  { SetLinkColor }

procedure TCustomElPopupButton.SetLinkPopupMenu(newValue : TPopupMenu);
begin
  if (FLinkPopupMenu <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FLinkPopupMenu <> nil then
      FLinkPopupMenu.RemoveFreeNotification(Self);
    {$endif}
    FLinkPopupMenu := newValue;
    if (newValue <> nil) then
       newValue.FreeNotification(Self);
  end;  { if }
end;  { SetLinkPopupMenu }

procedure TCustomElPopupButton.SetLinkStyle(newValue : TFontStyles);
{ Sets data member FLinkStyle to newValue. }
begin
  if (FLinkStyle <> newValue) then
  begin
    FLinkStyle := newValue;
    if IsHTML then
    begin
      FIsHTML := false;
      IsHTML := true;
    end;
  end;  { if }
end;  { SetLinkStyle }

procedure TCustomElPopupButton.DoLinkPopup(MousePos : TPoint);
begin
  if (FLinkPopupMenu <> nil) and FLinkPopupMenu.AutoPopup then
  begin
    {$ifndef CLX_USED}
    SendCancelMode(nil);
    {$endif}
    FLinkPopupMenu.PopupComponent := Self;
    if MousePos.X < 0 then
      MousePos := ClientToScreen(Point(0,0));
    FLinkPopupMenu.Popup(MousePos.X, MousePos.Y);
  end;
end;

procedure TCustomElPopupButton.TriggerImageNeededEvent(Sender : TObject; Src :
    TElFString; var Image : TBitmap);
begin
  Image := nil;
  if (assigned(FOnImageNeeded)) then
      FOnImageNeeded(Self, Src, Image);
end;  { TriggerImageNeededEvent }

procedure TCustomElPopupButton.TriggerLinkClickEvent(HRef : TElFString);
begin
  if (assigned(FOnLinkClick)) then
    FOnLinkClick(Self, HRef );
end;  { TriggerLinkClickEvent }

{$endif}

constructor TCustomElPopupButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
{$IFNDEF VER90}
  ControlStyle := [{$ifndef CLX_USED}csReflector, {$endif}csSetCaption, csCaptureMouse, csDoubleClicks, csOpaque];
{$ELSE}
  ControlStyle := [csSetCaption, csCaptureMouse, csDoubleClicks, csOpaque];
{$ENDIF}
  Width := 75;
  Height := 25;
  FAlignment := taCenter;
  TabStop := True;
  FMultiLine := False;
  FGlyph := TElButtonGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FFlat := false;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  Inc(ButtonCount);
  FUseArrow := False;
  FShowText := true;
  FShowGlyph := true;
  FGlyph.FImageIndex := -1;
  {$ifndef CLX_USED}
  Font.Color := clBtnText;
  ParentFont := True;
  {$else}
  ParentFont := False;
  Font.Color := clBlack;
  {$endif}
  FAdjustSpaceForGlyph := true;
  FChangeDisabledText := true;

  FChLink  := TChangeLink.Create; FChLink.OnChange := ImagesChanged;
  FDChLink := TChangeLink.Create; FDChLink.OnChange := ImagesChanged;
  FHChLink := TChangeLink.Create; FHChLink.OnChange := ImagesChanged;
  FNChLink := TChangeLink.Create; FNChLink.OnChange := ImagesChanged;
  FBackground := TBitmap.Create;
  FDownBackground := TBitmap.Create;
  FShadowFollowsColor := true;
  {$ifndef CLX_USED}
  FImgFormChLink  := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  FShowBorder := true;
  FShowFocus := true;
{$ifdef HAS_HTML_RENDER}
  FLinkStyle := [fsUnderline];
  FLinkColor := clBlue;
  {$endif}
  UseXPThemes := true;
end;

destructor TCustomElPopupButton.Destroy;
begin
  {$ifndef CLX_USED}
  ImageForm := nil;
  FImgFormChLink.Free;
  {$endif}
  UseArrow := false;
  FDownBackground.Free;
  FBackground.Free;
  Dec(ButtonCount);
  FChLink.Free;
  FDChLink.Free;
  FHChLink.Free;
  FNChLink.Free;
  if ButtonCount = 0 then
  begin
    Pattern.Free;
    Pattern := nil;
  end;
  FGlyph.Free;

  UseXPThemes := false;

  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
    FRender.Free;
  FRender := nil;
  {$endif}
  inherited Destroy;
end;

procedure TCustomElPopupButton.SetAdjustSpaceForGlyph(Value: Boolean);
begin
  if FAdjustSpaceForGlyph <> Value then
  begin
    FAdjustSpaceForGlyph := Value;
    Invalidate;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElPopupButton.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;
{$endif}

procedure TCustomElPopupButton.Click;
begin
  AClick(false);
end;

procedure TCustomElPopupButton.SetDrawDefaultFrame(Value: Boolean);
begin
  if FDrawDefaultFrame <> Value then
  begin
    FDrawDefaultFrame := Value;
    if Default then
      Invalidate;
  end;
end;

procedure TCustomElPopupButton.CreateThemeHandle;
begin
  inherited;
  if ThemesAvailable then
  begin
  {$ifndef CLX_USED}
    FArrTheme := OpenThemeData(Handle, 'COMBOBOX');
  {$else}
    {$ifdef MSWINDOWS}
    FArrTheme := OpenThemeData(QWidget_winID(Handle), 'COMBOBOX');
    {$endif}
  {$endif}
  end
  else
  begin
    FArrTheme := 0;
  end;
end;

procedure TCustomElPopupButton.FreeThemeHandle;
begin
  inherited;
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
  begin
    CloseThemeData(FArrTheme);
  end;
  {$endif}
  FArrTheme := 0;
end;

function TCustomElPopupButton.GetArrowSize: Integer;
var PS  : TSize;
begin
{$ifdef MSWINDOWS}
  if FArrTheme <> 0 then
  begin
    GetThemePartSizeTo('COMBOBOX', 0, CP_DROPDOWNBUTTON, CBXS_NORMAL, nil, TS_TRUE, PS);
    result := PS.cx;
  end
  else
{$endif}  
    result := 13;
end;

{$ifdef CLX_USED}
procedure TCustomElPopupButton.MouseEnter(AControl: TControl);
begin
  inherited;
  IntMouseEnter;
end;

procedure TCustomElPopupButton.MouseLeave(AControl: TControl);
begin
  inherited;
  IntMouseLeave;
end;

procedure TCustomElPopupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not IntKeyDown(Key, Shift) then
    inherited;
end;

procedure TCustomElPopupButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  IntKeyUp(Key, Shift);
end;

procedure TCustomElPopupButton.TextChanged;
begin
  inherited;
  IntTextChanged;
end;

procedure TCustomElPopupButton.ButtonPressed(Sender: TCustomElPopupButton; GroupIndex: Integer);
begin
  if GroupIndex = FGroupIndex then
  begin
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
        FState := ebsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TCustomElPopupButton.DoEnter;
begin
  {$ifndef CLX_USED}
  FActive := true;
  SetButtonStyle(FActive);
  {$endif}
  inherited;
  Invalidate;
end;

procedure TCustomElPopupButton.DoExit;
begin
  {$ifndef CLX_USED}
  FActive := false;
  SetButtonStyle(FActive);
  {$endif}
  inherited;
  Invalidate;
end;

function TCustomElPopupButton.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
var Button: QT.ButtonState;
    MouseButton: TMouseButton;
begin
  result := inherited EventFilter(Sender, Event);
  if QEvent_type(Event) = QEventType_MouseButtonDblClick then
    if FDown then DblClick;
end;

function TCustomElPopupButton.WidgetFlags: Integer;
begin
  result := Integer(WidgetFlags_WRepaintNoErase);
  //result := 0;
end;

{$endif}

function TCustomElPopupButton.IntKeyDown;
begin
  {$ifdef CLX_USED}
  if (((Key = KEY_RETURN) and FDefault) or
      ((Key = KEY_ESCAPE) and FCancel)) and
       (Shift = []) and CanFocus then
  begin
    Click;
    Result := true;
  end
  else
  {$endif}
  if {$ifndef CLX_USED}
     (Key = VK_SPACE)
    {$else}
     (Key = KEY_SPACE)
    {$endif}
     and Focused and
    (Shift = []) and CanFocus then
  begin
    if FState <> ebsDown then
    begin
      FState := ebsDown;
{$IFDEF USE_SOUND_MAP}
      if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
      Invalidate;
    end;
    Result := true;
  end
  else
  if (
    {$ifndef CLX_USED}
     (Key = VK_RETURN)
    {$else}
     (Key = KEY_RETURN)
    {$endif}
      and Focused) and
    (Shift = []) and CanFocus then
  begin
    if FIsSwitch then
    begin
      FDown := not FDown;
      if not FDown then
      begin
        FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
      end;
    end
    else
    begin
      FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
      if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
    end;
    Invalidate;
    Click;
    result := true;
  end
  else
  if
    {$ifndef CLX_USED}
     (Key = VK_ESCAPE)
    {$else}
     (Key = KEY_ESCAPE)
    {$endif}
     and (Shift = []) then
  begin
    result := true;
    {$ifndef CLX_USED}
    GetParentForm(Self).Perform(CM_DIALOGKEY, Key, ShiftStateToKeyData(Shift))
    {$endif}
  end
  else
  if
    {$ifndef CLX_USED}
    (Key = VK_DOWN)
    {$else}
    (Key = KEY_DOWN)
    {$endif}
    and (Shift = [ssAlt]) and
    Enabled and FUseArrow and Assigned(FPullDownMenu) and (FPullDownMenu.AutoPopup) then
  begin
    result := true;
    AClick(true)
  end
  else
    result := false;
end;

procedure TCustomElPopupButton.IntKeyUp;
begin
  if (
     {$ifndef CLX_USED}
     (Key = VK_SPACE)
     {$else}
     (Key = KEY_SPACE)
     {$endif}
     and Focused) and (Shift = []) and CanFocus then
  begin
    if FIsSwitch then
    begin
      FDown := not FDown;
      if not FDown then
      begin
        FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
      end;
    end
    else
    begin
      FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
      if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
    end;
    Invalidate;
    Click;
  end;
end;

procedure TCustomElPopupButton.IntMouseEnter;
var P     : TPoint;
    b     : boolean; 
begin
  b := false;
  if Enabled then
  begin
    if (not OldStyled) or IsThemeApplied then
      b := true;

    if UseArrow then
    begin
      GetCursorPos(P);
      P := ScreenToClient(P);
      FMouseInArrow := P.X > ClientWidth - GetArrowSize;
      if not FMouseInArrow then
        FMouseInControl := true;
    end
    else
      FMouseInControl := true;

    if UseImageList then
    begin
      if FHotImages <> nil then
      begin
        if FGlyph.ImageList <> FHotImages then
          b := true;
        FGlyph.ImageList := FHotImages;
      end
      else
      begin
        if FGlyph.ImageList <> FImageList then
          b := true;
        FGlyph.ImageList := FImageList;
      end;
    end;
    if b then
      Invalidate;
  end;
end;

procedure TCustomElPopupButton.IntTextChanged;
begin
  if (Pos(#13#10, Caption) > 0)
  {$ifdef HAS_HTML_RENDER}
  and (not IsHTML)
  {$endif}
  then
    FMultiline := true
  else
    FMultiline := false;
  Invalidate;
end;

procedure TCustomElPopupButton.IntMouseLeave;
var b : boolean;
begin
  b := false;
  if not OldStyled or IsThemeApplied then
    b := true;
  if Enabled and (not FDragging) then
  begin
    if UseImageList and (HotImages <> nil) then
    begin
      if FGlyph.ImageList <> FImageList then
        b := true;
      FGlyph.ImageList := FImageList;
    end;
  end;
  if FMouseInControl or FMouseInArrow then
    b := true;
  FMouseInControl := False;
  FMouseInArrow := false;
  if b then
    Invalidate;
end;

function TCustomElPopupButton.DoSaveShadows: Boolean;
begin
  result := ShadowsUseCustom and not (ShadowFollowsColor);
end;

procedure TCustomElPopupButton.SetImageIsAlphaBlended(Value: Boolean);
begin
  if FImageIsAlphaBlended <> Value then
  begin
    FImageIsAlphaBlended := Value;
    Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetDrawFocusFrame(Value: Boolean);
begin
  if FDrawFocusFrame <> Value then
  begin
    FDrawFocusFrame := Value;
    if Focused then
      Invalidate;
  end;
end;

{$ifdef HAS_HTML_RENDER}
procedure TCustomElPopupButton.SetCursor(Value: TCursor);
var P : TPoint;
    R : TRect;
    href : TElFString;
begin
  if (FCursor <> Value) then
  begin
    FCursor := Value;
    {$IFDEF HAS_HTML_RENDER}
    if IsHTML then
    begin
      GetCursorPos(P);
      P := ScreenToClient(P);
      R := FTextRect;
      if FRender.IsCursorOverLink(Point(P.X - R.Left, P.Y - R.Top), Point(0, 0), R, href) then
        inherited Cursor := crURLCursor
      else
        inherited Cursor := FCursor;
    end
    else
    {$endif}
      inherited Cursor := FCursor;
  end;  { if }
end;  { SetCursor }
{$endif}

procedure TCustomElPopupButton.SetChangeDisabledText(Value: Boolean);
begin
  if FChangeDisabledText <> Value then
  begin
    FChangeDisabledText := Value;
    if FState = ebsDisabled then
      Invalidate;
  end;
end;

procedure TCustomElPopupButton.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;


{$ifndef CLX_USED}
procedure TElSpeedButton.SetPullDownMenu(newValue : TPopupMenu);
begin
  if (FPullDownMenu <> newValue) then
  begin
    FPullDownMenu := newValue;
    if FPullDownMenu = nil then FPullDownEnabled := false;
  end; { if }
end; { SetPullDownMenu }

procedure TElSpeedButton.SetTransparent(newValue : Boolean);
begin
  if (FTransparent <> newValue) then
  begin
    FTransparent := newValue;
    Repaint;
  end; { if }
end; { SetTransparent }

procedure TElSpeedButton.Paint; { protected }
var
  xx, yy : integer;
  Bmp : TBitmap;
  IL : TImageList;

begin
  Bmp := nil;
  with Canvas do
  begin
    if not Transparent then
    begin
      pen.color := Color;
      Brush.Color := Color;
      Ellipse(1, 1, width - 2, height - 2); { fill inner button }
    end;
    if not Enabled then
      Bmp := FDisabledImage
    else if (not FPressed) and (not FOver) then
      Bmp := FNormalImage
    else if FPressed then
      Bmp := FPressedImage
    else if FOver then
      Bmp := FMouseInImage;
    if Bmp = nil then Bmp := FNormalImage;
    if Bmp <> nil then
    begin
      IL := TImageList.Create(self);
      xx := self.ClientWidth div 2 - Bmp.width div 2;
      if (bmp.width mod 2) <> 0 then dec(xx);
      yy := self.ClientHeight div 2 - Bmp.Height div 2;
      if (bmp.height mod 2) <> 0 then dec(yy);
      Brush.Color := Color;
      Il.Height := Bmp.Height;
      IL.Width := Bmp.Width;
      IL.AddMasked(Bmp, FTransparentColor);
      IL.DrawingStyle := dsNormal;
      IL.BkColor := clNone;
      IL.Draw(Canvas, XX, YY, 0);
      IL.Free;
    end;
    if (not FPressed) and ((FOver and FDrawEdge) or (not Flat)) or (csDesigning in ComponentState) then
    begin
      pen.color := clBtnHighlight; { highlighted edge }
      Arc(0, 0, width - 1, height - 1, width div 5 * 4, height div 5, width div 5, height div 5 * 4);
      pen.color := clBtnShadow; { shadowed edge }
      Arc(0, 0, width - 1, height - 1, width div 5, height div 5 * 4, width div 5 * 4, height div 5);
    end
    else if (FPressed and FDrawEdge) then
    begin
      pen.color := clBtnShadow; { shadowed edge }
      Arc(0, 0, width - 1, height - 1, width div 5 * 4, height div 5, width div 5, height div 5 * 4);
      pen.color := clBtnHighlight; { highlighted edge }
      Arc(0, 0, width - 1, height - 1, width div 5, height div 5 * 4, width div 5 * 4, height div 5);
    end;
  end;
end; { Paint }

procedure TElSpeedButton.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); { protected }
begin
  if InCircle(X, Y) then
  begin
    FPressed := true;
    if FPullDownEnabled and (FPullDownBtn = Button) then
      StartTimer
    else
    begin
      FPullTimer.Free;
      FPullTimer := nil;
    end;
    Repaint;
  end;
  inherited;
end; { MouseDown }

procedure TElSpeedButton.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); { protected }
begin
  if FPressed then
  begin
    FPressed := false;
    begin
      FPullTimer.Free;
      FPullTimer := nil;
    end;
    Repaint;
  end;
  inherited;
end; { MouseUp }

procedure TElSpeedButton.TriggerPullDownEvent;
begin
  if (assigned(FOnPullDown)) then
    FOnPullDown(Self);
end; { TriggerPullDownEvent }

procedure TElSpeedButton.SetDrawEdge(newValue : Boolean);
begin
  if (FDrawEdge <> newValue) then
  begin
    FDrawEdge := newValue;
    Repaint;
  end; { if }
end; { SetDrawEdge }

procedure TElSpeedButton.WMMouseMove(var Msg : TWMMouseMove); { private }
begin
  if (InCircle(Msg.XPos, Msg.YPos) and not FOver) then
  begin
    FOver := true;
    if ((MK_LBUTTON or MK_MBUTTON or MK_RBUTTON) and Msg.Keys) > 0 then FPressed := true;
    Repaint;
  end
  else if ((not InCircle(Msg.XPos, Msg.YPos)) and FOver) then
  begin
    FOver := false;
    FPressed := false;
    Repaint;
  end;
  inherited;
end; { WMMouseMove }

procedure TElSpeedButton.CMMouseLeave(var Msg : TMessage); { private }
var
  b : boolean;
begin
  inherited;
  b := false;
  if FPressed then
  begin
    FPressed := false;
    b := true;
  end;
  if FOver then
  begin
    FOver := false;
    b := true;
  end;
  if b then Repaint;
end; { CMMouseLeave }

function TElSpeedButton.InCircle(X, Y : integer) : Boolean; { public }
var
  xx, yy, ll : integer;
begin
  xx := abs(X - ClientWidth div 2);
  yy := abs(Y - ClientHeight div 2);
  ll := ClientWidth div 2;
  result := (sqr(xx) + sqr(yy)) <= sqr(ll);
end; { InCircle }

procedure TElSpeedButton.SetFlat(newValue : Boolean);
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    Repaint;
  end; { if }
end; { SetFlat }

procedure TElSpeedButton.SetNormalImage(newValue : TBitmap);
begin
  FNormalImage.Assign(newValue);
  if FAutoSize then
  begin
    ClientWidth := 3 + Trunc(sqrt(sqr(FNormalImage.Height) + sqr(FNormalImage.Width)));
    ClientHeight := ClientWidth;
  end;
  //if Enabled then Repaint;
end; { SetNormalImage }

procedure TElSpeedButton.SetDisabledImage(newValue : TBitmap);
begin
  FDisabledImage.Assign(newValue);
  if FAutoSize then
  begin
    ClientWidth := 3 + Trunc(sqrt(sqr(FDisabledImage.Height) + sqr(FDisabledImage.Width)));
    ClientHeight := ClientWidth;
  end;
  if not Enabled then Repaint;
end; { SetDisabledImage }

procedure TElSpeedButton.SetMouseInImage(newValue : TBitmap);
begin
  FMouseInImage.Assign(newValue);
  if FAutoSize then
  begin
    ClientWidth := 3 + Trunc(sqrt(sqr(FPressedImage.Height) + sqr(FPressedImage.Width)));
    ClientHeight := ClientWidth;
  end;
  if FOver then Repaint;
end; { SetMouseInImage }

procedure TElSpeedButton.SetPressedImage(newValue : TBitmap);
begin
  FPressedImage.Assign(newValue);
  if FAutoSize then
  begin
    ClientWidth := 3 + Trunc(sqrt(sqr(FPressedImage.Height) + sqr(FPressedImage.Width)));
    ClientHeight := ClientWidth;
  end;
  if FPressed then Repaint;
end; { SetPressedImage }

destructor TElSpeedButton.Destroy;
begin
  FNormalImage.Free;
  FDisabledImage.Free;
  FMouseInImage.Free;
  FPressedImage.Free;
  if FPullTimer <> nil then FPullTimer.Free;
  inherited Destroy;
end; { Destroy }

constructor TElSpeedButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FPullTimer := nil;
  FPullDownInterval := 1000;
  FPullDownEnabled := false;
  FTransparent := true;
  FDrawEdge := False;
  FNormalImage := TBitmap.Create;
  FDisabledImage := TBitmap.Create;
  FMouseInImage := TBitmap.Create;
  FPressedImage := TBitmap.Create;
  FAutoSize := true;
  Color := clBtnFace;
  Width := 20;
  Height := 20;
end; { Create }

procedure TElSpeedButton.SetAutoSize(newValue : boolean);
begin
  inherited;
  if FAutoSize <> newValue then
  begin
    FAutoSize := newValue;
    if FAutoSize then
    begin
      ClientWidth := 3 + Trunc(sqrt(sqr(FNormalImage.Height) + sqr(FNormalImage.Width)));
      ClientHeight := ClientWidth;
    end;
  end;
end;

procedure TElSpeedButton.SetTransparentColor(newValue : TColor);
begin
  if (FTransparentColor <> newValue) then
  begin
    FTransparentColor := newValue;
{$IFNDEF VER90}
    FNormalImage.TransparentColor := newValue;
    FDisabledImage.TransparentColor := newValue;
    FMouseInImage.TransparentColor := newValue;
    FPressedImage.TransparentColor := newValue;
{$ENDIF}
    Repaint;
  end; { if }
end; { SetTransparentColor }

procedure TElSpeedButton.WMEraseBkgnd(var Msg : TWMEraseBkgnd); { private }
begin
  Msg.Result := 1;
end; { WMEraseBkgnd }

procedure TElSpeedButton.StartTimer;
begin
  if FPullDownInterval > 0 then
  begin
    if FPullTimer = nil then FPullTimer := TTimer.Create(self);
    FPullTimer.Interval := FPullDownInterval;
    FPullTimer.OnTimer := OnTimer;
    FPullTimer.Enabled := true;
  end
  else
    PullMenu;
end;

procedure TElSpeedButton.OnTimer(Sender : TObject);
begin
  FPullTimer.Enabled := false;
  PullMenu;
end;

procedure TElSpeedButton.PullMenu;
var
  P : TPoint;
begin
  TriggerPullDownEvent;
  if FPullDownMenu <> nil then
  begin
    if FPullDownMenu.Alignment = paLeft then
    begin
      P.X := 0;
      P.Y := Height + 1;
    end
    else if FPullDownMenu.Alignment = paRight then
    begin
      P.X := Width;
      P.Y := Height + 1;
    end
    else
    begin
      P.X := Width div 2;
      P.Y := Height + 1;
    end;
    P := ClientToScreen(P);
    FPullDownMenu.Popup(P.X, P.Y);
  end;
end;

{$endif}

{$IFDEF VCL_4_USED}

procedure TElPopupButtonActionLink.SetChecked(Value : boolean);
begin
  if IsCheckedLinked then
  begin
    with TCustomElPopupButton(FClient) do
    begin
      ClicksDisabled := True;
      try
        Checked := Value;
      finally
        ClicksDisabled := False;
      end;
    end;
  end;
end;

procedure TElPopupButtonActionLink.SetImageIndex(Value: Integer);
begin
  if FClient is TCustomElPopupButton then
  begin
    TCustomElPopupButton(FClient).ImageIndex := value;
  end;
end;

{$ENDIF}


// =============================================================================

function TCustomElGraphicButton.GetChecked : Boolean;
begin
  result := Down;
end;

procedure TCustomElGraphicButton.SetChecked(newValue : Boolean);
begin
  Down := newValue;
end;

procedure TCustomElGraphicButton.SetPopupPlace(Value : TPopupPlace);
begin
  FPopupPlace := Value;
end;

procedure TCustomElGraphicButton.SetDisableAp(Value : boolean);
begin
  FDisableAp := Value;
end;

function TCustomElGraphicButton.IsColorStored : boolean;
begin
  result := not ParentColor;
end;

procedure TCustomElGraphicButton.SetShowBorder;
begin
  if FShowBorder <> newValue then
  begin
    FShowBorder := newValue;
    Invalidate;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElGraphicButton.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TCustomElGraphicButton.SetImageForm(newValue : TElImageForm);
begin
  if FImgForm <> newValue then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      if (FImgForm <> nil) and (not (csDestroying in FImgForm.ComponentState)) then
        FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnRegisterChanges(FImgFormChLink);
    end;
    if newValue <> nil then
       newValue.FreeNotification(Self);
    FImgForm := newValue;
    if FImgForm <> nil then FImgForm.RegisterChanges(FImgFormChLink);
    Invalidate;
  end;
end;
{$endif}

{$ifdef CLX_USED}
type THackWidgetControl = class(TWidgetControl) end;
{$endif}

procedure TCustomElGraphicButton.Paint;
var
  PaintRect,
  R1, BgRect : TRect;
  ACtl    : TWinControl;
  ArrRect : TRect;
  Offset : TPoint;
  dw : integer;
  aw : integer;
  AColor : TColor;
  ax, ay : integer;
  P      : TPoint;
  SaveIL : TImageList;
  GlyphState : TElButtonState;
  sID     : integer;
  RClip   : TRect;
  {$ifndef CLX_USED}
  Bitmap  : TBitmap;
  Canvas  : TCanvas;
  {$endif}
  {$ifdef CLX_USED}
  AQColor : QColorH;
  AQColorGroup : QColorGroupH;
  {$endif}

  procedure DrawButtonFrameEx(DC : HDC; rc : TRect; Focused : boolean; Pushed : boolean; ButtonColor : TColor; Thin : boolean);
  begin
    if ShadowsUseCustom then
       ElVCLUtils.DrawButtonFrameEx2(DC, rc, Focused, Pushed, ButtonColor, Thin, ColorToRGB(FShadowBtnHighlight), ColorToRGB(FShadowBtnDkShadow), ColorToRGB(Color), ColorToRGB(FShadowBtnShadow))
    else
       ElVCLUtils.DrawButtonFrameEx(DC, rc, Focused, Pushed, ButtonColor, Thin);
  end;

begin
  sid := 0;
  {$ifndef CLX_USED}
  Bitmap := Tbitmap.Create;
  Bitmap.Handle := CreateCompatibleBitmap(Self.Canvas.Handle, ClientWidth, ClientHeight);
  Canvas := Bitmap.Canvas;
  bitblt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Self.Canvas.Handle, 0, 0, SRCCOPY);
  {$endif}

  if (not Enabled) or (not Parent.Enabled) then
  begin
    FState := ebsDisabled;
    FDragging := False;
  end
  else
  if FState = ebsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := ebsExclusive
    else
      FState := ebsUp;
  Canvas.Font := Self.Font;
  PaintRect := ClientRect;

  if FShadowFollowsColor then
    AColor := Color
  else
    AColor := clBtnFace;

  {$ifdef MSWINDOWS}
  if not IsThemeApplied then
  {$endif}
  begin
    {$ifndef CLX_USED}
    if not Transparent then
    {$endif}
    begin
      {$ifndef CLX_USED}
      if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
      begin
        ACtl := FImgForm.GetRealControl;
        R1 := PaintRect;
        BgRect := R1;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        P := Parent.ClientToScreen(Point(Left, Top));
        ax := BgRect.Left - P.x;
        ay := BgRect.Top - P.y;

        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);
        FImgForm.PaintBkgnd(Canvas.Handle, R1, Point(BgRect.Left - ax, BgRect.Top - ay), false);
      end
      else
      {$endif}
      begin
        Canvas.Brush.Color := Color;
        if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) then
        begin
          if (not FDownBackground.Empty) then
            Canvas.CopyRect(ClientRect, DownBackground.Canvas, Rect(0, 0, DownBackground.Width - 1, DownBackground.Height - 1))
          else
            Canvas.FillRect(PaintRect);
        end
        else
        begin
          if (not Background.Empty) then
            Canvas.CopyRect(ClientRect, Background.Canvas, Rect(0, 0, Background.Width - 1, Background.Height - 1))
          else
            Canvas.FillRect(PaintRect);
        end;
      end;
    {$ifndef CLX_USED}
    end
    else
    begin
      bitblt(Bitmap.Canvas.Handle, 0, 0, ClientWidth, ClientHeight, Self.Canvas.Handle, 0, 0, SRCCOPY);
    {$endif}
    end;
    PaintRect := ClientRect;
  end;

  dw := 0;
  aw := 0;

  {$ifdef MSWINDOWS}
  if (not IsThemeApplied) or (FArrTheme = 0) then
  {$endif}
  begin
    if not FFlat then
    begin
      if FUseArrow then
      begin
        dw := GetArrowSize;
        aw := dw;
        Dec(PaintRect.Right, dw);
        {$ifdef CLX_USED}
        Dec(PaintRect.Right, 2);
        {$endif}
      end;
      if ((Background.Empty) or (BackgroundDrawBorder)) and ShowBorder then
      begin
        {$ifndef CLX_USED}
        if MoneyFlat then
        begin
          if (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]) then
            Canvas.Brush.Color := MoneyFlatDownColor
          else
          if (FMouseInControl or FMouseInArrow or Focused) and (FState <> ebsDisabled) then
            Canvas.Brush.Color := MoneyFlatActiveColor
          else
            Canvas.Brush.Color := MoneyFlatInactiveColor;
          Canvas.FrameRect(PaintRect);
          InflateRect(PaintRect, -1, -1);
        end
        else
        {$endif}
        begin
          {$ifndef CLX_USED}
          if OldStyled or ((FMouseInControl or Focused) and
             (not ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])))) then
             DrawButtonFrameEx(Canvas.Handle, PaintRect, (not ThinFrame),
                             (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]), AColor, ThinFrame)
          else
             DrawButtonFrameEx(Canvas.Handle, PaintRect, FOldStyled,
                             (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]), AColor, ThinFrame);
          {$else}
          Canvas.Start;
          AQColorGroup := THackWidgetControl(Parent).Palette.ColorGroup(GetColorGroup(Parent));
          with PaintRect do
             QStyle_drawButton(Application.Style.Handle,
                               Canvas.Handle,
                               Left,
                               Top,
                               Right - Left,
                               Bottom - Top,
                               AQColorGroup,
                               (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]),
                               Canvas.Brush.Handle);
          QStyle_buttonRect(Application.Style.Handle,
                            @PaintRect,
                            PaintRect.Left,
                            PaintRect.Top,
                            PaintRect.Right - PaintRect.Left,
                            PaintRect.Bottom - PaintRect.Top);
          Canvas.Stop;
          Inc(PaintRect.Right, 2);
          {$endif}
        end;
      end;
    end
    else
    begin
      if FUseArrow then
      begin
        dw := GetArrowSize;
        aw := dw;
        Dec(PaintRect.Right, dw);
        {$ifdef CLX_USED}
        Dec(PaintRect.Right, 2);
        {$endif}
      end;
      {$ifndef CLX_USED}
      if MoneyFlat then
      begin
        if (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]) then
          Canvas.Brush.Color := MoneyFlatDownColor
        else
        if (FMouseInControl or FMouseInArrow or Focused) and (FState <> ebsDisabled) then
          Canvas.Brush.Color := MoneyFlatActiveColor
        else
          Canvas.Brush.Color := MoneyFlatInactiveColor;
        Canvas.FrameRect(PaintRect);
        InflateRect(PaintRect, -1, -1);
      end
      else
      {$endif}
      begin
        if (FDown and FIsSwitch) or (FState in [ebsDown, {ebsArrDown, }ebsExclusive]) or
          ((FMouseInControl or FMouseInArrow or Focused) and (FState <> ebsDisabled)) or (csDesigning in ComponentState) then
        begin
          {$ifndef CLX_USED}
          if ((Background.Empty) or (BackgroundDrawBorder)) and ShowBorder then
             DrawButtonFrameEx(Canvas.Handle, PaintRect, false,
                             (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]), AColor, ThinFrame);
          {$else}
          Canvas.Start;
          if ((Background.Empty) or (BackgroundDrawBorder)) and ShowBorder then
          begin
            with PaintRect do
              QStyle_drawButton(Application.Style.Handle,
                                Canvas.Handle,
                                Left,
                                Top,
                                Right - Left,
                                Bottom - Top,
                                THackWidgetControl(Parent).Palette.ColorGroup(GetColorGroup(Parent)),
                                (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]),
                                Canvas.Brush.Handle);
          end;
          Canvas.Stop;
          Inc(PaintRect.Right, 2);
          {$endif}
        end;
      end;
    end;

    // draw the rectangle for the arrow
    if FUseArrow then
    begin
      if dw = 0 then
      begin
        dw := GetArrowSize;
        Dec(PaintRect.Right, dw);
        {$ifdef CLX_USED}
        //Dec(PaintRect.Right, 2);
        {$endif}
      end;
      ArrRect := Rect(PaintRect.Right, 0, PaintRect.Right + dw, Height);

      Canvas.Brush.Color := Color;

      if ((Background.Empty) or (BackgroundDrawBorder)) and FShowBorder then
      begin
        {$ifndef CLX_USED}
        if MoneyFlat then
        begin
          if (FState in [ebsArrDown, ebsDown]) then
            Canvas.Brush.Color := MoneyFlatDownColor
          else
          if (FMouseInControl or FMouseInArrow or Focused) and (FState <> ebsDisabled) then
            Canvas.Brush.Color := MoneyFlatActiveColor
          else
            Canvas.Brush.Color := MoneyFlatInactiveColor;
          Canvas.FrameRect(ArrRect);
        end
        else
        {$endif}
        begin
          if ((not FFlat) or ((FMouseInControl or FMouseInArrow or Focused) or
              (FState in [ebsArrDown, ebsExclusive])
               or (csDesigning in ComponentState))) then
          {$ifndef CLX_USED}
            DrawButtonFrameEx(Canvas.Handle, ArrRect, ({FMouseInArrow or }FOldStyled) and (not FThinFrame) and (not FDown), (FState in [ebsDown, ebsArrDown]), AColor, ThinFrame);
          {$else}
          begin
            Canvas.start;
            with ArrRect do
              QStyle_drawButton(Application.Style.Handle,
                                Canvas.Handle,
                                Left,
                                Top,
                                Right - Left,
                                Bottom - Top,
                                THackWidgetControl(Parent).Palette.ColorGroup(GetColorGroup(Parent)),
                                (FState in [ebsDown, ebsArrDown]),
                                Canvas.Brush.Handle);
            Canvas.Stop;
            //Inc(ArrRect.Right, 2);
          end;
          {$endif}
        end;
      end;
      {$ifndef CLX_USED}
      DrawArrow(Canvas, eadDown, ArrRect, Font.Color, FState <> ebsDisabled);
      {$else}
      Canvas.Start;
      QStyle_buttonRect(Application.Style.Handle,
                        @ArrRect,
                        ArrRect.Left,
                        ArrRect.Top,
                        ArrRect.Right - ArrRect.Left,
                        ArrRect.Bottom - ArrRect.Top);
      //Inc(ArrRect.Right, 2);
      with ArrRect do
        QStyle_drawArrow(Application.Style.Handle,
                         Canvas.Handle,
                         ArrowType_DownArrow,
                         (FState in [ebsDown, ebsArrDown]),
                         Left,
                         Top,
                         Right - Left,
                         Bottom - Top,
                         THackWidgetControl(Parent).Palette.ColorGroup(GetColorGroup(Parent)),
                         true,
                         Canvas.Brush.Handle);
        Canvas.Stop;
      {$endif}
    end;
  {$ifdef MSWINDOWS}
  end
  else
  begin
    //DrawThemedBackground(Canvas);
    sid := GetThemeStateID;
    if FUseArrow then
    begin
      dw := GetArrowSize;
      aw := dw;
      Dec(PaintRect.Right, dw);
    end;
    RClip := Canvas.ClipRect;

    {$ifndef CLX_USED}
    DrawThemeBackground(Theme, Canvas.Handle, GetThemePartID, sID, PaintRect, @RClip);
    {$else}
    Canvas.Start;
    OffsetRect(PaintRect, Left, Top);
    DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), GetThemePartID, sID, PaintRect, nil);
    OffsetRect(PaintRect, -Left, -Top);

    Canvas.Stop;
    {$endif}

    if FUseArrow then
    begin
      if dw = 0 then
      begin
        dw := GetArrowSize();
        Dec(PaintRect.Right, dw);
      end;
      ArrRect := Rect(PaintRect.Right, PaintRect.Top, PaintRect.Right + dw, PaintRect.Bottom);

      RClip := Canvas.ClipRect;
      {$ifndef CLX_USED}
      DrawThemeBackground(FArrTheme, Canvas.Handle, GetArrowThemePartID, GetArrowThemeStateID, ArrRect, @RClip);
      {$else}
      Canvas.Start;
      DrawThemeBackground(FArrTheme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), GetArrowThemePartID, GetArrowThemeStateID, ArrRect, @RClip);
      Canvas.Stop;
      {$endif}
    end;
  {$endif}
  end;

  Offset.x := 0;
  Offset.y := 0;

  if (FState in [ebsDown, ebsExclusive]) or (FDown and FIsSwitch) then
  begin
    if (FState = ebsExclusive) or
       (UseImageList and ((DownImages = nil) or (DownImages = Images))) or
       (UseIcon) or
       ((not UseImageList) and (UseIcon) and (NumGlyphs < 2)) then
    begin
      Offset.X := 1;
      Offset.Y := 1;
    end;
  end;
  inc(PaintRect.Right, dw);

  if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) and (FDownImages <> nil) then
  begin
    SaveIL := FGlyph.FImageList;
    FGlyph.FImageList := FDownImages;
  end;

  if (FGlyph.FImageList = Self.FDisabledImages) and (FDisabledImages <> nil) and (FState = ebsDisabled) then
    GlyphState := ebsUp
  else
    GlyphState := FState;

  if AdjustSpaceForGlyph then
  begin
    if (not FThinFrame) then
      InflateRect(PaintRect, -2, -2)
    else
      InflateRect(PaintRect, -1, -1);
  end;
  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    FRender.Data.DefaultColor := Font.Color;
    FRender.Data.DefaultBgColor := clNone;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.LinkColor := LinkColor;
    FRender.Data.LinkStyle := LinkStyle;
    FRender.Data.Charset := Font.Charset;
  end;
  {$endif}
  {$ifdef CLX_USED}
  {$IFDEF HAS_HTML_RENDER}
  if IsHTML then
    FRender.Data.TextOffset := Point(Left, Top);
  {$ENDIF}
  {$endif}
  FTextRect := FGlyph.Draw(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
    FSpacing, FState, GlyphState, Alignment, true, FMultiline, false, FShowGlyph,
    FShowText, Aw, TextDrawType, Color,
    {$ifdef MSWINDOWS}false{ParentFont and (Theme <> 0)}, FTheme, GetThemePartID, sid
    {$else}false, 0, 0, 0{$endif},
    ShortcutsEnabled, ImageIsAlphaBlended{$ifdef HAS_HTML_RENDER}, IsHTML, FRender{$endif}, FChangeDisabledText);
//  Canvas.Stop;

  {$ifdef CLX_USED}
  {$IFDEF HAS_HTML_RENDER}
  if IsHTML then
    FRender.Data.TextOffset := Point(0, 0);
  {$ENDIF}
  {$endif}

  if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) and (FDownImages <> nil) then
  begin
    FGlyph.FImageList := SaveIL;
  end;
  {$ifndef CLX_USED}
  bitblt(Self.Canvas.Handle, 0, 0, ClientWidth, ClientHeight, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  Bitmap.Free;
  {$endif}
end;

{$HINTS OFF}
procedure TCustomElGraphicButton.AClick;
var
{$IFNDEF VER90}
  Form : TCustomForm;
{$ELSE}
  Form : TForm;
{$ENDIF}

{$ifndef CLX_USED}
{$IFDEF VCL_4_USED}
const
   Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
{$ENDIF}
{$endif}

begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.ModalResult := ModalResult;
  if Arrow then
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(ArrowClickSound);
{$ENDIF}
    if Assigned(FOnArrowClick) then FOnArrowClick(Self);
  end
  else
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(ClickSound);
{$ENDIF}
    inherited Click;
    //FMouseInControl := false;
    Invalidate;
  end;
  if (PullDownMenu = nil) or (FDisableAp) or ((not Arrow) and FUseArrow) then exit;
  DoPullMenu;
end;

{$ifndef CLX_USED}
{$HINTS ON}
procedure TCustomElGraphicButton.CMDialogKey(var Message : TCMDialogKey);
begin
  with Message do
    if ShortcutsEnabled and ((((CharCode = VK_RETURN) and FDefault) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = [])) then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TCustomElGraphicButton.CMDialogChar(var Message : TCMDialogChar);
begin
  with Message do
    if ShortcutsEnabled and IsAccel(CharCode, Caption) then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;
{$endif}

procedure TCustomElGraphicButton.UpdateExclusive;
{$ifndef CLX_USED}
var
  Msg : TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;
{$else}
var
  Msg: TCMButtonPressed;
  I: Integer;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.Control := Self;
    Msg.Index := FGroupIndex;
    Parent.Broadcast(Msg);
    for I := 0 to Parent.ControlCount-1 do
      if Parent.Controls[I] is TCustomElGraphicButton then
        TCustomElGraphicButton(Parent.Controls[I]).ButtonPressed(Self, FGroupIndex);
  end;
end;
{$endif}

procedure TCustomElGraphicButton.GlyphChanged(Sender : TObject);
begin
  if (not (csLoading in ComponentState)) and
     (not UseIcon) and (not UseImageList) then
  Invalidate;
end;

{$ifndef CLX_USED}
function TCustomElGraphicButton.GetPalette : HPALETTE;
begin
  Result := Glyph.Palette;
end;
{$endif}

function TCustomElGraphicButton.GetGlyph : TBitmap;
begin
  Result := FGlyph.Glyph;
end;

procedure TCustomElGraphicButton.SetGlyph(Value : TBitmap);
begin
  FGlyph.Glyph := Value;
  FGlyph.ResetNumGlyphs;
  Invalidate;
end;

function TCustomElGraphicButton.GetNumGlyphs : TNumGlyphs;
begin
  Result := FGlyph.NumGlyphs;
end;

procedure TCustomElGraphicButton.SetNumGlyphs(Value : TNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else if Value > 4 then
    Value := 4;
  // if Value <> FNumGlyphs then
  begin
    FNumGlyphs := Value;
    if (ComponentState * [csReading, csLoading] = []) then
    begin
      FGlyph.NumGlyphs := Value;
      Invalidate;
    end;
  end;
end;

procedure TCustomElGraphicButton.SetDown(Value : Boolean);
begin
  if csLoading in ComponentState then
  begin
    FDown := value;
    exit;
  end;
  if (FGroupIndex = 0) and (not FIsSwitch) then Value := False;
  if FDown and (not FAllowAllUp) and (not FIsSwitch) then Exit;
  FDown := Value;
  if Value then
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
    if FIsSwitch then
      FState := ebsDown
    else
      FState := ebsExclusive;
    Invalidate;
  end
  else
  begin
{$IFDEF USE_SOUND_MAP}
    if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
    FState := ebsUp;
    Invalidate;
  end;
  if Value then UpdateExclusive;
end;

procedure TCustomElGraphicButton.SetFlat(Value : Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    if Value then
    begin
      OldStyled := false;
    end;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetGroupIndex(Value : Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TCustomElGraphicButton.SetLayout(Value : TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetMargin(Value : Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetSpacing(Value : Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetAllowAllUp(Value : Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TCustomElGraphicButton.UpdateTracking;
var
  P : TPoint;
begin
  if not Enabled then FMouseInControl := false;

  if FFlat and Enabled then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    {$ifndef CLX_USED}
    if Enabled and FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
    {$else}
    if Enabled and FMouseInControl then
      MouseLeave(Self)
    else
      MouseEnter(Self);
    {$endif}
  end;
end;

procedure TCustomElGraphicButton.Loaded;
var
  State : TElButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := ebsUp
  else
    State := ebsDisabled;
  if FUseImageList then
    FGlyph.UseImageList := true;
  FGlyph.CreateButtonGlyph(State);
  FGlyph.NumGlyphs := FNumGlyphs;
  if FDown then
  begin
    FDown := false;
    Down := true;
  end;
  Invalidate;
end;

{$ifndef CLX_USED}
procedure TCustomElGraphicButton.CMEnabledChanged(var Message : TMessage);
{$else}
procedure TCustomElGraphicButton.EnabledChanged;
{$endif}
begin
  inherited;
  IntEnabledChanged;
end;

{$ifndef CLX_USED}
procedure TCustomElGraphicButton.CMButtonPressed(var Message : TMessage);
var
  Sender : TCustomElGraphicButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TCustomElGraphicButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
        FState := ebsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElGraphicButton.CMFontChanged(var Message : TMessage);
{$else}
procedure TCustomElGraphicButton.FontChanged;
{$endif}
begin
  inherited;
  Invalidate;
end;

{$ifndef CLX_USED}
procedure TCustomElGraphicButton.CMColorChanged(var Message : TMessage);
var ButtonColor : TColor;
    hls1, hls2, hls3, hls4 : integer;
    lum : integer;

begin
  inherited;
  if not ShadowsUseCustom then
  begin
    ButtonColor := ColorToRGB(Color);

    hls1 := RGBtoHLS(ButtonColor);
    hls2 := hls1;
    hls3 := hls1;
    hls4 := hls1;

    lum := Hi(LoWord(hls3));
    hls1 := (Min(239, (Hi(LoWord(hls3))  + lum div 3)) shl 8) or (hls1 and $FF00FF);
    hls2 := (Min(239, (Hi(LoWord(hls3))  - lum div 2)) shl 8) or (hls2 and $FF00FF);
    hls4 := (Min(239, (Hi(LoWord(hls3))  - lum div 3)) shl 8) or (hls4 and $FF00FF);

    FShadowBtnHighlight := HLStoRGB(hls1);
    FShadowBtnDkShadow  := HLStoRGB(hls2);
    FShadowBtnShadow    := HLStoRGB(hls4);
  end;
  Invalidate;
end;

procedure TCustomElGraphicButton.CMTextChanged(var Message : TMessage);
begin
  IntTextChanged;
end;

procedure TCustomElGraphicButton.CMSysColorChange(var Message : TMessage);
begin
  with TElButtonGlyph(FGlyph) do
  begin
    Invalidate;
    CreateButtonGlyph(FState);
  end;
end;

procedure TCustomElGraphicButton.CMMouseEnter(var Message : TMessage);
begin
  inherited;
  IntMouseEnter;
  (*
  DoInv := false;
  if IsThemeApplied then
    DoInv := true;
  if (not FMouseInControl) and Enabled then
  begin
    if UseArrow then
    begin
      GetCursorPos(P);
      P := ScreenToClient(P);
      FMouseInArrow := P.X > ClientWidth - GetArrowSize;
      if not FMouseInArrow then
        FMouseInControl := true;
    end
    else
      FMouseInControl := true;
    DoInv := true;

    if UseImageList then
    begin
      if FHotImages <> nil then
        FGlyph.ImageList := FHotImages
      else
        FGlyph.ImageList := FImageList;
      DoInv := FHotImages <> FImageList;
    end;
  end;
  if DoInv then Invalidate;
  *)
end;

procedure TCustomElGraphicButton.CMMouseLeave(var Message : TMessage);
// var DoInv : boolean;
begin
  inherited;
  IntMouseLeave;
  (*
  DoInv := false;
  if IsThemeApplied then
    DoInv := true;
  if FMousEInArrow then
    DoInv := true;
  if FMouseInControl and Enabled and not FDragging then
  begin
    if UseImageList and (HotImages <> nil) then
    begin
      FGlyph.ImageList := FImageList;
      DoInv := FHotImages <> FImageList;
    end;
    if Flat or (not (OldStyled or ThinFrame)) then DoInv := true;
  end;
  FMouseInControl := False;
  FMouseInArrow := false;
  if DoInv then Invalidate;
  *)
end;
{$endif}

procedure TCustomElGraphicButton.MouseDown(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
var
  l : integer;
{$IFDEF HAS_HTML_RENDER}
var
  P : TPoint;
  R : TRect;
  href : TElFString;
{$endif}
begin
  {$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin
    P.x := X;
    P.y := Y;
    R := FTextRect;
    if IsHTML and FRender.IsCursorOverLink(Point(P.X - R.Left, P.Y - R.Top), Point(0, 0), R, href) then
      exit;
  end;
  {$endif}

  FOrigState := ebsUp;
  if (csDesigning in ComponentState) or ((Parent <> nil) and (csDesigning in Parent.ComponentState)) then exit;
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    l := 11;
    if not FFlat then inc(l, 4);
    if (not FDown) and ((FUseArrow and not InRange(Width - l, Width, X)) or not FUseArrow) then
    begin
      FState := ebsDown;
{$IFDEF USE_SOUND_MAP}
      if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
      Invalidate;
    end
    else
    if (not FArrDown) and (FUseArrow and InRange(Width - l, Width, X)) then
    begin
{$IFDEF USE_SOUND_MAP}
      if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
      if FIgnoreClick then FIgnoreClick := false else
      begin
        FState := ebsArrDown;
        Invalidate;
        FOrigState := ebsArrDown;
        AClick(true);
        if not FInMenu then
          FDragging := true;
      end;
      exit;
    end;
    FDragging := True;
    FMouseInControl := true;
  end else if (Button <> mbLeft) then FIgnoreClick := false;
end;

procedure TCustomElGraphicButton.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  NewState : TElButtonState;
var
  l : integer;
  b : boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if (not FDown) and (not FArrDown) then
      NewState := ebsUp
    else
      NewState := ebsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
    begin
      if FDown then
        NewState := ebsExclusive
      else
      begin
        l := GetArrowSize;
        // if not FFlat then inc(l, 4);
        if (FUseArrow and InRange(Width - l, Width, X)) then
          NewState := ebsArrDown
        else
          NewState := ebsDown;
      end;
    end
    else
    begin
      if UseArrow then
      begin
        FMouseInArrow := false;
        Invalidate;
      end;
    end;
    if NewState <> FState then
    begin
      FState := NewState;
      if FState = ebsDown then
      begin
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
      end
      else if FState = ebsUp then
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
      UpdateTracking;
      Invalidate;
    end;
  end
  else
  if UseArrow then
  begin
    //b := FMouseInArrow;
    FMouseInArrow := X > ClientWidth - GetArrowSize;

    if FMouseInArrow then
    begin
      b := FMouseInControl = false;
      FMouseInControl := false;
    end
    else
    begin
      b := FMouseInControl = false;
      FMouseInControl := true;
    end;
    if b <> FMouseInArrow then
      Invalidate;
  end;
end;

procedure TCustomElGraphicButton.MouseUp(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
var
  DoClick : Boolean;
  ArrClick : Boolean;
  l : integer;
  {$IFDEF HAS_HTML_RENDER}
  P : TPoint;
  R : TRect;
  href : TElFString;
  {$endif}
begin
  inherited;
  {$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    R := FTextRect;

    if IsHTML and FRender.IsCursorOverLink(Point(P.X - R.Left, P.Y - R.Top), Point(0, 0), R, href) then
    begin
      TriggerLinkClickEvent(href);
      exit;
    end;
  end;
  {$endif}
  if FDragging and (Button = mbLeft) then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    l := 11;
    if not FFlat then inc(l, 4);
    ArrClick := (FUseArrow and InRange(Width - l, Width, X));
    if FGroupIndex = 0 then
    begin
      if FIsSwitch and (not ArrClick) then
      begin
        if FOrigState <> ebsArrDown then
        begin
          FDown := not FDown;
          if FDown then
          begin
            FState := ebsDown;
{$IFDEF USE_SOUND_MAP}
            if SoundMap <> nil then SoundMap.Play(DownSound);
{$ENDIF}
          end
          else
          begin
            FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
            if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
          end;
        end else
        begin
          FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
          if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
        end;
      end
      else
      begin
        if ArrClick and FInMenu then
          DoClick := false
        else
        begin
          FState := ebsUp;
{$IFDEF USE_SOUND_MAP}
          if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
        end;
      end;
      FMouseInControl := PtInRect(ClientRect, Point(X, Y));
      if not FMouseInControl then
        IntMouseLeave;
      if not (FState in [ebsExclusive, ebsArrDown, ebsDown]) then
        Invalidate;
    end
    else
    if DoClick then
    begin
      SetDown(not FDown);
      if FDown or FArrDown then Invalidate;
    end
    else
    begin
      if FDown then
        FState := ebsExclusive;
      Invalidate;
    end;
    if DoClick then
    begin
      AClick(ArrClick);
    end;
  end;
end;

{$ifndef CLX_USED}
{$HINTS OFF}
procedure TCustomElGraphicButton.WndProc(var Message : TMessage);
var P : TPoint;
    I : Integer;
    MenuItem: TMenuItem;
    FindKind: TFindItemKind;
    ContextID: Integer;
    DC: HDC;
    R : TRect;
    Item: Integer;

begin
  if Message.Msg = MenuCancelMsg then
  begin
    FInMenu := false;
    FDragging := false;
    FArrDown := false;
    FState := ebsUp;
    FMouseInControl := false;
    FMouseInArrow := false;
    Invalidate;
    Exit;
  end;

  //if FInMenu then
  try
    case Message.Msg of
{$IFNDEF VCL_5_USED}
      WM_COMMAND:
        if FPulldownMenu.DispatchCommand(Message.wParam) then Exit;
      WM_INITMENUPOPUP:
        with TWMInitMenuPopup(Message) do
          if FPulldownMenu.DispatchPopup(MenuPopup) then Exit;
      WM_MENUSELECT:
        with TWMMenuSelect(Message) do
        begin
          FindKind := fkCommand;
          if MenuFlag and MF_POPUP <> 0 then FindKind := fkHandle;
          MenuItem := FPulldownMenu.FindItem(IDItem, FindKind);
          if MenuItem <> nil then
          begin
            Application.Hint := MenuItem.Hint;
            Exit;
          end;
          Application.Hint := '';
        end;
      WM_HELP:
        with PHelpInfo(Message.LParam)^ do
        begin
            if (FPullDownMenu <> nil) and (FPulldownMenu.Handle = hItemHandle) then
            begin
              ContextID := FPulldownMenu.GetHelpContext(iCtrlID, True);
              if ContextID = 0 then
                ContextID := FPulldownMenu.GetHelpContext(hItemHandle, False);
              if Screen.ActiveForm = nil then Exit;
              if (biHelp in Screen.ActiveForm.BorderIcons) then
                Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID)
              else
                Application.HelpContext(ContextID);
              Exit;
            end;
        end;
{$ELSE}
      WM_COMMAND,
      WM_INITMENUPOPUP,
      WM_MENUSELECT,
      WM_DRAWITEM,
      WM_MEASUREITEM,
      WM_MENUCHAR,
      WM_HELP:
        {$ifdef ELPACK_COMPLETE}
        if (PulldownMenu is TElPopupMenu) then
          with Message do
            SendMessage(GetParentForm(Self).Handle, Msg, wParam, lParam)
        else
        {$endif}
          with Message do
            SendMessage(PopupList.Window, Msg, wParam, lParam);
{$ENDIF}
      WM_EXITMENULOOP:
        begin
          GetCursorPos(P);
          P := ScreenToClient(P);
          FDragging := true;
          PostMessage(FWND, MenuCancelMsg, 0, 0);
          if PtInRect(Rect(ClientWidth - GetArrowSize, 0, ClientWidth, ClientHeight), P) then
             FIgnoreClick := true;
          {$IFDEF VCL_5_USED}
          with Message do
               SendMessage(PopupList.Window, Msg, wParam, lParam);
          {$ENDIF}
        end;
      WM_QUERYENDSESSION:
        begin
          Message.Result := 1;
          exit;
        end;
      end
  except
    Application.HandleException(Self);
  end;
  if Message.Msg = CN_COMMAND then
    if FClicksDisabled then Exit;
  inherited WndProc(Message);
end;
{$HINTS ON}
{$endif}
procedure TCustomElGraphicButton.SetUseArrow(newValue : Boolean);
begin
  if (FUseArrow <> newValue) then
  begin
    FUseArrow := newValue;
    Invalidate;
  end; { if }
end; { SetUseArrow }

procedure TCustomElGraphicButton.SetShowGlyph(newValue : Boolean);
begin
  if (FShowGlyph <> newValue) then
  begin
    FShowGlyph := newValue;
    Invalidate;
  end; { if }
end; { SetShowGlyph }

procedure TCustomElGraphicButton.SetShowText(newValue : Boolean);
begin
  if (FShowText <> newValue) then
  begin
    FShowText := newValue;
    Invalidate;
  end; { if }
end; { SetShowText }

procedure TCustomElGraphicButton.SetPullDownMenu(newValue : TPopupMenu);
begin
  if (FPullDownMenu <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FPullDownMenu <> nil then
      FPulldownMenu.RemoveFreeNotification(Self); 
    {$endif}
    FPullDownMenu := newValue;
    if FPullDownMenu = nil then
      FDisableAp := true
    else
    begin
      FDisableAp := false;
      FPulldownMenu.FreeNotification(Self);
    end;
  end; { if }
end; { SetPullDownMenu }

function TCustomElGraphicButton.GetIcon : TIcon;
begin
  result := FGlyph.Icon;
end;

procedure TCustomElGraphicButton.SetIcon(newValue : TIcon);
begin
  FGlyph.Icon.Assign(newValue);
end;

procedure TCustomElGraphicButton.SetIsSwitch(newValue : Boolean);
begin
  if (FIsSwitch <> newValue) then
    FIsSwitch := newValue;
end;

{$IFDEF USE_SOUND_MAP}
procedure TCustomElGraphicButton.SetSoundMap(newValue : TElSoundMap);
begin
  if (FSoundMap <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FSoundMap <> nil then
      FSoundMap.RemoveFreeNotification(Self);
    {$endif}
    FSoundMap := newValue;
    {$ifdef VCL_5_USED}
    if FSoundMap <> nil then
      FSoundMap.FreeNotification(Self);
    {$endif}
  end;
end; { SetSoundMap }
{$ENDIF}

procedure TCustomElGraphicButton.Notification(AComponent : TComponent; operation : TOperation);
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) then
  begin
{$IFDEF USE_SOUND_MAP}
    if (AComponent = FSoundMap) then SoundMap := nil;
{$ENDIF}
    if (AComponent = FImageList) then Images := nil;
    if (AComponent = FHotImages) then HotImages := nil;
    if (AComponent = FDownImages) then DownImages := nil;
    if (AComponent = FDisabledImages) then DisabledImages := nil;
    if AComponent = FPullDownMenu then PullDownMenu :=nil;
    {$ifdef HAS_HTML_RENDER}
    if AComponent = LinkPopupMenu then
      LinkPopupMenu := nil;
    {$endif}
    {$ifndef CLX_USED}
    if AComponent = FImgForm then ImageForm := nil;
    {$endif}
  end; { if }
end; { Notification }

function TCustomElGraphicButton.GetImageIndex : integer;
begin
  result := FGlyph.FImageIndex;
end;

procedure TCustomElGraphicButton.SetImageIndex(newValue : Integer);
begin
  if (FGlyph.FImageIndex <> newValue) then
  begin
    FGlyph.ImageIndex := newValue;
    if FGlyph.UseImageList then Invalidate;
  end; {if}
end;

procedure TCustomElGraphicButton.SetImageList(newValue : TImageList);
var b : boolean;
begin
  if FImageList <> newValue then
  begin
    b := FImageList = nil;
    if not b then
    begin
      {$ifdef VCL_5_USED}
      if (not (csDestroying in FImageList.ComponentState)) then
        FImageList.RemoveFreeNotification(Self);
      {$endif}
      FImageList.UnregisterChanges(FChLink);
    end;
    FImageList := newValue;
    if FImageList <> nil then
    begin
      FImageList.RegisterChanges(FChLink);
      FImageList.FreeNotification(Self);
    end;
    FGlyph.ImageList := newValue;
    if FImageList = nil then
      UseImageList := false
    else
    if b and (not (csLoading in ComponentState)) then
      UseImageList := true;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetUseIcon(newValue : Boolean);
begin
  if (FUseIcon <> newValue) then
  begin
    FUseIcon := newValue;
    FGlyph.UseIcon := newValue;
    if not UseImageList then Invalidate;
  end; { if }
end; { SetUseIcon }

function TCustomElGraphicButton.GetUseImageList : boolean;
begin
  result := FUseImageList;
end;

procedure TCustomElGraphicButton.SetUseImageList(newValue : Boolean);
begin
  if (UseImageList <> newValue) then
  begin
    FUseImageList := newValue;
    if not (csLoading in ComponentState) then
    begin
      FGlyph.UseImageList := newValue;
      Invalidate;
    end;
  end; { if }
end; { SetUseImageList }

procedure TCustomElGraphicButton.SetOldStyled(newValue : Boolean);
{ Sets data member FOldStyled to newValue. }
begin
  if (FOldStyled <> newValue) then
  begin
    if not (newValue and Flat) then
    begin
      FOldStyled := newValue;
      Repaint;
    end;
  end; { if }
end; { SetOldStyled }

procedure TCustomElGraphicButton.ImagesChanged(Sender : TObject);
begin
  if FUseImageList then
  begin
    if (FMouseInControl and Enabled) and (Sender = Images) then Invalidate else
    if (not Enabled) and (Sender = DisabledImages) then Invalidate else
    if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) and (Sender = DownImages) then Invalidate else
    if Sender = FImageList then Invalidate;
  end else
  begin
    if (Sender = FBackGround) or
         ((Sender = FDownBackground) and (FDown and FIsSwitch) or
         (FState in [ebsDown, ebsExclusive]))  then Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetDownImages(newValue : TImageList);
begin
  if (FDownImages <> newValue) then
  begin
    if FDownImages <> nil then
    begin
      {$ifdef VCL_5_USED}
      FDownImages.RemoveFreeNotification(Self);
      {$endif}
      FDownImages.UnregisterChanges(FNChLink);
    end;
    FDownImages := newValue;
    if FDownImages <> nil then FDownImages.RegisterChanges(FNChLink);
    if ((FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive])) then
    begin
      if newValue = nil then FGlyph.ImageList := FImageList else FGlyph.ImageList := newValue;
      Invalidate;
    end;
    if FDownImages <> nil then FDownImages.FreeNotification(Self);
  end;  { if }
end;

procedure TCustomElGraphicButton.SetHotImages(newValue : TImageList);
begin
  if (FHotImages <> newValue) then
  begin
    if FHotImages <> nil then
    begin
      {$ifdef VCL_5_USED}
      FHotImages.RemoveFreeNotification(Self);
      {$endif}
      FHotImages.UnregisterChanges(FHChLink);
    end;
    FHotImages := newValue;
    if FHotImages <> nil then
    begin
      FHotImages.RegisterChanges(FHChLink);
      FHotImages.FreeNotification(Self);
    end;
    if FMouseInControl then
    begin
      if newValue = nil then FGlyph.ImageList := FImageList else FGlyph.ImageList := newValue;
      Invalidate;
    end;
  end;  { if }
end;  { SetHotImages }

procedure TCustomElGraphicButton.SetDisabledImages(newValue : TImageList);
begin
  if (FDisabledImages <> newValue) then
  begin
    if FDisabledImages <> nil then
    begin
      {$ifdef VCL_5_USED}
      FDisabledImages.RemoveFreeNotification(Self);
      {$endif}
      FDisabledImages.UnregisterChanges(FDChLink);
    end;
    FDisabledImages := newValue;
    if FDisabledImages <> nil then
    begin
      FDisabledImages.RegisterChanges(FDChLink);
      FDisabledImages.FreeNotification(Self);
    end;
    if not Enabled then
    begin
      if newValue <> nil then
        FGlyph.ImageList := newValue
      else
        FGlyph.ImageList := FImageList;
      Invalidate;
    end;
  end;  { if }
end;  { SetDisabledImages }

{$IFDEF VCL_4_USED}

function TCustomElGraphicButton.GetActionLinkClass : TControlActionLinkClass;
begin
  result := TElGraphicButtonActionLink;
end;

procedure TCustomElGraphicButton.ActionChange(Sender : TObject; CheckDefaults : Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Checked = False) then
        Self.Checked := Checked;
    end;
    if Sender is TAction then
    begin
      Enabled := TAction(Sender).Enabled;
      Visible := TAction(Sender).Visible;
      Hint := TAction(Sender).Hint;
      Caption := TAction(Sender).Caption;
      ImageIndex := TAction(Sender).ImageIndex;
      Down := TAction(Sender).Checked;
    end;
  end;
end;
{$ENDIF}

procedure TCustomElGraphicButton.SetThinFrame(newValue : Boolean);
{ Sets data member FThinFrame to newValue. }
begin
  if (FThinFrame <> newValue) then
  begin
    FThinFrame := newValue;
    if newValue then FOldStyled := false;
    if OldStyled or (FMouseInControl and (not (FState in [ebsDown, ebsExclusive]))) then Invalidate;
  end;  { if }
end;  { SetThinFrame }

procedure TCustomElGraphicButton.SetShadowsUseCustom(Value : Boolean);
begin
  if Value <> FShadowsUseCustom then
  begin
    FShadowsUseCustom := Value;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetShadowBtnHighlight(Value : TColor);
begin
  if Value <> FShadowBtnHighlight then
  begin
    FShadowBtnHighlight := Value;
    if FShadowsUseCustom then Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetShadowBtnShadow(Value : TColor);
begin
  if Value <> FShadowBtnShadow then
  begin
    FShadowBtnShadow := Value;
    if FShadowsUseCustom then Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetShadowBtnDkShadow(Value : TColor);
begin
  if Value <> FShadowBtnDkShadow then
  begin
    FShadowBtnDkShadow := Value;
    if FShadowsUseCustom then Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetShadowFollowsColor(Value : Boolean);
begin
  if Value <> FShadowFollowsColor then
  begin
    FShadowFollowsColor := Value;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetBackgroundDrawBorder(Value : boolean);
begin
  if Value <> FBackgroundDrawBorder then
  begin
    FBackgroundDrawBorder := Value;
    if not FBackground.Empty then Invalidate;      
  end;
end;

procedure TCustomElGraphicButton.SetDownBackground(newValue : TBitmap);
{ Sets data member FDownBackground to newValue. }
begin
  FDownBackground.Assign(newValue);
end;  { SetDownBackground }

procedure TCustomElGraphicButton.SetBackground(newValue : TBitmap);
{ Sets data member FBackground to newValue. }
begin
  FBackground.Assign(newValue);
end;  { SetBackground }

procedure TCustomElGraphicButton.SetTextDrawType(newValue : TElTextDrawType);
begin
  if (FTextDrawType <> newValue) then
  begin
    FTextDrawType := newValue;
    Invalidate;
  end; { if }
end; { SetTextDrawType }

procedure TCustomElGraphicButton.SetTransparent(newValue : Boolean);
begin
  if (FTransparent <> newValue) then
  begin
    FTransparent := newValue;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];

    Repaint;
  end; { if }
end; { SetTransparent }

constructor TCustomElGraphicButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
{$IFNDEF VER90}
  ControlStyle := [{$ifndef CLX_USED}csReflector, {$endif}csSetCaption, csCaptureMouse, csDoubleClicks{$ifndef CLX_USED}, csOpaque{$endif}];
{$ELSE}
  ControlStyle := [csSetCaption, csCaptureMouse, csDoubleClicks, csOpaque];
{$ENDIF}
  Width := 75;
  Height := 25;

  FAlignment := taCenter;
  FChangeDisabledText := true;
  FMultiLine := False;
  FGlyph := TElButtonGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FFlat := false;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  Inc(ButtonCount);
  FUseArrow := False;
  FShowText := true;
  FShowGlyph := true;
  FGlyph.FImageIndex := -1;
  {$ifndef CLX_USED}
  PInteger(@Color)^ := clBtnFace;
  PInteger(@Font.Color)^ := clBtnText;
  {$endif}
  FChLink  := TChangeLink.Create; FChLink.OnChange := ImagesChanged;
  FDChLink := TChangeLink.Create; FDChLink.OnChange := ImagesChanged;
  FHChLink := TChangeLink.Create; FHChLink.OnChange := ImagesChanged;
  FNChLink := TChangeLink.Create; FNChLink.OnChange := ImagesChanged;
  FBackground := TBitmap.Create;
  FDownBackground := TBitmap.Create;
  FShadowFollowsColor := true;
  {$ifndef CLX_USED}
  MenuWindowProc := WndProc;

  FWnd := AllocateHWND(IntWndProc);
  FImgFormChLink  := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  FShowBorder := true;
  FAdjustSpaceForGlyph := true;
  {$ifdef MSWINDOWS}
  UseXPThemes := true;
  {$endif}
  {$ifdef HAS_HTML_RENDER}
  FLinkStyle := [fsUnderline];
  FLinkColor := clBlue;
  {$endif}  
end;

destructor TCustomElGraphicButton.Destroy;
begin
  UseArrow := false;
  {$ifndef CLX_USED}
  ImageForm := nil;
  FImgFormChLink.Free;
  DeallocateHWnd(FWnd);
  {$endif}
  FDownBackground.Free;
  FBackground.Free;
  Dec(ButtonCount);
  FChLink.Free;
  FDChLink.Free;
  FHChLink.Free;
  FNChLink.Free;
  
  if ButtonCount = 0 then
  begin
    Pattern.Free;
    Pattern := nil;
  end;
  FGlyph.Free;
  {$ifdef MSWINDOWS}
  UseXPThemes := false;
  {$endif}
  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
    FRender.Free;
  FRender := nil;
  {$endif}
  inherited Destroy;
end;

procedure TCustomElGraphicButton.SetAdjustSpaceForGlyph(Value: Boolean);
begin
  if FAdjustSpaceForGlyph <> Value then
  begin
    FAdjustSpaceForGlyph := Value;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetCaption(Value: TElFString);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    inherited Caption := Value;
    (*
    {$ifndef CLX_USED}
    Perform(CM_TEXTCHANGED, 0, 0);
    {$else}
    TextChanged;
    {$endif}
    *)
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElGraphicButton.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
{$endif}

procedure TCustomElGraphicButton.SetUseXPThemes(const Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    if ThemesAvailable then
    begin
      {$ifdef MSWINDOWS}
      if FUseXPThemes then
      begin
        CreateThemeHandle;
      end
      else
      begin
        FreeThemeHandle;
      end;
      {$endif}
      Invalidate;
    end;
  end;
end;

procedure TCustomElGraphicButton.CreateThemeHandle;
begin
  if ThemesAvailable then
  begin
    {$ifdef MSWINDOWS}
    {$ifndef CLX_USED}
    FTheme := OpenThemeData(0, PWideChar(GetThemedClassName()));
    FArrTheme := OpenThemeData(0, PWideChar(GetArrowThemedClassName()));
    {$else}
    FTheme := OpenThemeData(0, PWideChar(GetThemedClassName()));
    FArrTheme := OpenThemeData(0, PWideChar(GetArrowThemedClassName()));
    {$endif}
    {$endif}
  end
  else
  begin
    FTheme := 0;
    FArrTheme := 0;
  end;
end;

procedure TCustomElGraphicButton.FreeThemeHandle;
begin
  if ThemesAvailable then
  begin
    {$ifdef MSWINDOWS}
    CloseThemeData(FTheme);
    CloseThemeData(FArrTheme);
    {$endif}
  end;
  FTheme := 0;
  FArrTheme := 0;
end;

{$ifndef CLX_USED}
procedure TCustomElGraphicButton.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    FreeThemeHandle;
    CreateThemeHandle;
    Invalidate;
  end;
  Message.Result := 1;
end;
{$endif}

function TCustomElGraphicButton.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0) and (FArrTheme <> 0);
end;

function TCustomElGraphicButton.GetThemePartID: Integer;
begin
  Result := BP_PUSHBUTTON;
end;

function TCustomElGraphicButton.GetThemeStateID: Integer;
begin
  if not Enabled then
    result := PBS_DISABLED
  else
  if (FDown and FIsSwitch) or (FState in [ebsDown, ebsExclusive]) then
    result := PBS_PRESSED
  else
  if FMouseInControl or FMouseInArrow then
    result := PBS_HOT
  else
    result := PBS_NORMAL;
end;

function TCustomElGraphicButton.GetThemedClassName: WideString;
begin
  Result := 'BUTTON';
end;

function TCustomElGraphicButton.GetArrowThemePartID: Integer;
begin
  Result := CP_DROPDOWNBUTTON;
end;

function TCustomElGraphicButton.GetArrowThemeStateID: Integer;
begin
  if not Enabled then
    result := CBXS_DISABLED
  else
  if FState in [ebsArrDown, ebsExclusive] then
    result := CBXS_PRESSED
  else
  if FMouseInControl or FMouseInArrow or Focused then
    result := CBXS_HOT
  else
    result := CBXS_NORMAL;
end;

function TCustomElGraphicButton.GetArrowThemedClassName: WideString;
begin
  Result := 'COMBOBOX';
end;

function TCustomElGraphicButton.GetArrowSize: Integer;
var PS  : TSize;
begin
  {$ifdef MSWINDOWS}
  if FArrTheme <> 0 then
  begin
    {$ifndef CLX_USED}
    GetThemePartSizeTo(PWideChar(GetArrowThemedClassName), Canvas.Handle, GetArrowThemePartID, GetArrowThemeStateID, nil, TS_TRUE, PS);
    {$else}
    Canvas.Start;
    GetThemePartSizeTo(PWideChar(GetArrowThemedClassName), QPaintDevice_handle(QPainter_device(Canvas.Handle)), GetArrowThemePartID, GetArrowThemeStateID, nil, TS_TRUE, PS);
    Canvas.Stop;
    {$endif}
    result := PS.cx;
  end
  else
  {$endif}
    result := 13;
end;


procedure TCustomElGraphicButton.IntEnabledChanged;
const
  NewState : array[Boolean] of TElButtonState = (ebsDisabled, ebsUp);
begin
  FGlyph.CreateButtonGlyph(NewState[Enabled]);
  if UseImageList then
  begin
    if (not Enabled) and (FDisabledImages <> nil) then
       FGlyph.ImageList := FDisabledImages
    else
       FGlyph.ImageList := FImageList;
  end;
  UpdateTracking;
  Invalidate;
end;

procedure TCustomElGraphicButton.IntMouseEnter;
var P     : TPoint;
    b     : boolean; 
begin
  b := false;
  if Enabled then
  begin
    if (not OldStyled) or IsThemeApplied then
      b := true;

    if UseArrow then
    begin
      GetCursorPos(P);
      P := ScreenToClient(P);
      FMouseInArrow := P.X > ClientWidth - GetArrowSize;
      if not FMouseInArrow then
        FMouseInControl := true;
    end
    else
      FMouseInControl := true;

    if UseImageList then
    begin
      if FHotImages <> nil then
      begin
        if FGlyph.ImageList <> FHotImages then
          b := true;
        FGlyph.ImageList := FHotImages;
      end
      else
      begin
        if FGlyph.ImageList <> FImageList then
          b := true;
        FGlyph.ImageList := FImageList;
      end;
    end;
    if b then
      Invalidate;
  end;
end;

procedure TCustomElGraphicButton.IntTextChanged;
begin
  if (Pos(#13#10, Caption) > 0)
  {$ifdef HAS_HTML_RENDER}
  and (not IsHTML)
  {$endif}
   then
    FMultiline := true
  else
    FMultiline := false;
  Invalidate;
end;

procedure TCustomElGraphicButton.IntMouseLeave;
var b : boolean; 
begin
  b := false;
  if not OldStyled or IsThemeApplied then
    b := true;
  if Enabled and (not FDragging) then
  begin
    if UseImageList and (HotImages <> nil) then
    begin
      if FGlyph.ImageList <> FImageList then
        b := true;
      FGlyph.ImageList := FImageList;
    end;
  end;
  if FMouseInControl or FMouseInArrow then
    b := true;
  FMouseInControl := False;
  FMouseInArrow := false;
  if b then
    Invalidate;
end;

{$ifdef MSWINDOWS}
procedure TCustomElGraphicButton.DrawThemedBackground(Canvas : TCanvas);
begin
  if not Transparent then
  begin
    {$ifndef CLX_USED}
    DrawThemeBackground(Theme, Canvas.Handle, BP_PUSHBUTTON, 0, ClientRect, nil);
    {$else}
    Canvas.Start;
    DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_PUSHBUTTON, 0, ClientRect, nil);
    Canvas.Stop;
    {$endif}
  end;
end;
{$endif}

{$ifndef CLX_USED}

{$ifdef VCL_5_USED}
procedure TCustomElGraphicButton.WMContextMenu(var Message: TWMContextMenu);
var
  Pt, Temp: TPoint;
  Handled: Boolean;
  href : TElFString;
  R    : TRect;
begin
  if csDesigning in ComponentState then Exit;

  {$ifdef HAS_HTML_RENDER}
  Pt := ScreenToClient(SmallPointToPoint(Message.Pos));
  if IsHTML then
  begin
    R := FTextRect;
    if (LinkPopupMenu <> nil) and FRender.IsCursorOverLink(Point(Pt.X - R.Left, Pt.Y - R.Top), Point(0, 0), R, href) then
    begin
      Handled := False;
      Temp := ClientToScreen(Pt);
      DoContextPopup(Temp, Handled);
      Message.Result := Ord(Handled);
      if Handled then Exit;

      DoLinkPopup(ClientToScreen(Pt));
      Message.Result := 1;
    end
    else
      inherited
  end
  else
  {$endif}
    inherited;
end;
{$endif}

procedure TCustomElGraphicButton.WMRButtonUp(var Message: TWMRButtonUp);
{$ifndef VCL_5_USED}
var P : TPoint;
    R : TRect;
    href : TElFString;
{$endif}
begin
  {$ifndef VCL_5_USED}
  if IsHTML then
  begin
    P := SmallPointToPoint(Message.Pos);
    R := FTextRect;
    if (LinkPopupMenu <> nil) and FRender.IsCursorOverLink(Point(P.X - R.Left, P.Y - R.Top), Point(0, 0), R, href) then
    begin
      if not (csNoStdEvents in ControlStyle) then
        with Message do
          MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos);
      if Message.Result = 0 then
      begin
        DoLinkPopup(P);
      end;
    end
    else
      inherited;
  end
  else
  {$endif}
    inherited;
end;

procedure TCustomElGraphicButton.WMWindowPosChanged(var Message: TMessage);
begin
  inherited;
  (*if Parent <> nil then
  begin
    Windows.SetParent(FWnd, Parent.Handle);
    SetWindowPos(FWnd, HWND_BOTTOM, Left, Top, Width, Height, SWP_HIDEWINDOW);
  end;
  *)
end;
{$endif}

{$ifdef CLX_USED}

procedure TCustomElGraphicButton.MouseEnter(AControl: TControl);
begin
  inherited;
  IntMouseEnter;
end;

procedure TCustomElGraphicButton.MouseLeave(AControl: TControl);
begin
  inherited;
  IntMouseLeave;
end;

procedure TCustomElGraphicButton.TextChanged;
begin
  inherited;
  IntTextChanged;
end;

procedure TCustomElGraphicButton.ButtonPressed(Sender: TCustomElGraphicButton; GroupIndex: Integer);
begin
  if GroupIndex = FGroupIndex then
  begin
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
{$IFDEF USE_SOUND_MAP}
        if SoundMap <> nil then SoundMap.Play(UpSound);
{$ENDIF}
        FState := ebsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

{$endif}

function TCustomElGraphicButton.MeasureButton(LockHeight : boolean): TPoint;
var Aw : integer;
    inR,
    R  : TRect;
    AHeight : integer;
    
begin
  Canvas.Font.Assign(Font);
  if UseArrow then
  begin
    aw := GetArrowSize;
    {$ifdef MSWINDOWS}
    if IsThemeApplied then
    begin
      r := Rect(0,0,aw, aw);
      {$ifndef CLX_USED}
      GetThemeBackgroundExtent(Theme, Canvas.Handle, GetThemePartID, GetThemeStateID, @R, inR);
      {$else}
      Canvas.Start;
      GetThemeBackgroundExtent(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), GetThemePartID, GetThemeStateID, @R, inR);
      Canvas.Stop;
      {$endif}
      aw := inR.Right - inR.Left;
    end;
    {$endif}
  end
  else
    aw := 0;

  R := Rect(0, 0, Width, Height);
  {$ifdef MSWINDOWS}
  if IsThemeApplied then
  begin
    {$ifndef CLX_USED}
    GetThemeBackgroundContentRect(Theme, Canvas.Handle, GetThemePartID, GetThemeStateID, R, inR);
    {$else}
    Canvas.Start;
    GetThemeBackgroundContentRect(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), GetThemePartID, GetThemeStateID, R, inR);
    Canvas.Stop;
    {$endif}
    if EqualRect(R, inR) then
      InflateRect(inR, -2, -2); 
    inR := Rect(0,
                0,
                R.Right - R.Left + (R.Left - InR.Left + InR.Right - R.Right),
                R.Bottom - R.Top + (R.Top - InR.Top + InR.Bottom - R.Bottom));
  end
  else
  {$endif}
  begin
    {$ifndef CLX_USED}
    inR := R;
    dec(inR.Right, 4);
    dec(inR.Bottom, 4);
    {$else}
    QStyle_ButtonRect(Application.Style.Handle,
                            @inR,
                            R.Left,
                            R.Top,
                            R.Right - R.Left,
                            R.Bottom - R.Top);
    inR := Rect(0,
                0,
                R.Right - R.Left + (R.Left - InR.Left + InR.Right - R.Right),
                R.Bottom - R.Top + (R.Top - InR.Top + InR.Bottom - R.Bottom));
    {$endif}
  end;
  if LockHeight then
    AHeight := inR.Bottom
  else
    AHeight := 0;
  result := Point(
         FGlyph.CalcButtonWidth(Canvas, AHeight, Point(0, 0),
             Caption, Layout, Margin, Spacing,
             ShowGlyph, ShowText, FMultiline, aw,
             {$ifdef MSWINDOWS}false{ParentFont and (FTheme <> 0)},
             FTheme, GetThemePartID, GetThemeStateID{$else}false, 0, 0, 0{$endif}
             {$ifdef HAS_HTML_RENDER}, IsHTML, FRender{$endif}),
         0);
  result.y := AHeight + R.Bottom - inR.Bottom;

  inc(result.x, R.Right - inR.Right);
end; { MeasureButton }

function TCustomElGraphicButton.GetMoneyFlat: Boolean;
begin
  Result := FMoneyFlat;
end;

procedure TCustomElGraphicButton.SetMoneyFlat(Value: Boolean);
begin
  if FMoneyFlat <> Value then
  begin
    FMoneyFlat := Value;
    {$ifndef CLX_USED}
    Invalidate;
    {$endif}
  end;
end;

procedure TCustomElGraphicButton.SetMoneyFlatDownColor(Value: TColor);
begin
  if FMoneyFlatDownColor <> Value then
  begin
    FMoneyFlatDownColor := Value;
    {$ifndef CLX_USED}
    if MoneyFlat then Invalidate;
    {$endif}
  end;
end;

procedure TCustomElGraphicButton.SetMoneyFlatActiveColor(Value: TColor);
begin
  if FMoneyFlatActiveColor <> Value then
  begin
    FMoneyFlatActiveColor := Value;
    {$ifndef CLX_USED}
    if MoneyFlat then Invalidate;
    {$endif}
  end;
end;

procedure TCustomElGraphicButton.SetMoneyFlatInactiveColor(Value: TColor);
begin
  if FMoneyFlatInactiveColor <> Value then
  begin
    FMoneyFlatInactiveColor := Value;
    {$ifndef CLX_USED}
    if MoneyFlat then Invalidate;
    {$endif}
  end;
end;

function TCustomElGraphicButton.DoSaveShadows : boolean;
begin
  result := ShadowsUseCustom and not (ShadowFollowsColor); 
end;

procedure TCustomElGraphicButton.Click;
begin
  AClick(false);
end;

{$ifdef CLX_USED}
procedure TCustomElGraphicButton.DoContextPopup(const MousePos: TPoint; var Handled: Boolean);
{$IFDEF HAS_HTML_RENDER}
var href : TElFString;
    Temp : TPoint;
    R    : TRect;
    {$ifdef CLX_USED}
    Grabber: QWidgetH;
    {$endif}
{$endif}
begin
  {$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin
    R := FTextRect;
    if (LinkPopupMenu <> nil) and FRender.IsCursorOverLink(Point(MousePos.X - R.Left, MousePos.Y - R.Top), Point(0, 0), R, href) then
    begin
      MouseUp(mbRight, [ssLeft], MousePos.X, MousePos.Y);
      Temp := ClientToScreen(MousePos);
      Grabber := QWidget_mouseGrabber;
      if Grabber <> nil then
        QWidget_releaseMouse(Grabber);
      LinkPopupMenu.PopupComponent := Self;
      LinkPopupMenu.Popup(Temp.X, Temp.Y);
      Handled := true;
    end
    else
      inherited
  end
  else
  {$endif}
    inherited DoContextPopup(MousePos, Handled);
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElGraphicButton.IntWndProc(var Message : TMessage);
begin
  if @WindowProc <> nil then
    WindowProc(Message)
  else
  WndProc(Message);
end;
{$endif}
procedure TCustomElGraphicButton.DoPullMenu;
var
  q : TPoint;
begin
  PullDownMenu.PopupComponent := Self;
  PullDownMenu.AutoPopup := True;
  q.X := 0;
  q.Y := 0;
  q := ClientToScreen(q);

  if FPopupPlace = ppRight then
  begin
    q.x := q.X + Width;
    q.y := q.Y - 1;
  end else
  if FPopupPlace = ppDown then
  begin
    q.x := q.X - 1;
    q.y := q.Y + height;
  {$ifndef CLX_USED}
  end
  else
  begin
    q.x := q.x - 1;
    q.y := q.y - GetSystemMetrics(SM_CYMENU) * PullDownMenu.Items.Count;
    //PullDownMenu.Popup(q.X - 1, q.Y - GetSystemMetrics(SM_CYMENU) * PullDownMenu.Items.Count);
  {$endif}
  end;
  if q.x < 0 then q.x := 0;
  if q.y < 0 then q.y := 0;
  PullDownMenu.PopupComponent := Self;
  {$ifndef CLX_USED}
  if not FUseArrow then
  begin
    FState := ebsDown;
    FOrigState := ebsDown;
    Invalidate;
  end;
  FInMenu := true;
  if Assigned(PullDownMenu.OnPopup) then
    PullDownMenu.OnPopup(PullDownMenu);
  TrackPopupMenu(GetMenuHandle(PullDownMenu), TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON, q.X, q.Y, 0, FWND, nil);
  FState := ebsUp;
  {$else}
  PullDownMenu.Popup(Q.X, Q.Y);
  {$endif}
end;

procedure TCustomElGraphicButton.SetShortcutsEnabled(Value: Boolean);
begin
  if FShortcutsEnabled <> Value then
  begin
    FShortcutsEnabled := Value;
    Invalidate;
  end;
end;

function TCustomElGraphicButton.Focused: Boolean;
begin
  Result := false;
end;

{$ifdef HAS_HTML_RENDER}
procedure TCustomElGraphicButton.SetIsHTML(Value: Boolean);
begin
  if FIsHTML <> Value then
  begin
    FIsHTML := Value;
    if FIsHTML then
    begin
      FRender := TElHTMLRender.Create;
      FRender.OnImageNeeded := TriggerImageNeededEvent;
    end
    else
    begin
      FRender.Free;
      FRender := nil;
    end;
    Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetLinkColor(newValue : TColor);
{ Sets data member FLinkColor to newValue. }
begin
  if (FLinkColor <> newValue) then
  begin
    FLinkColor := newValue;
    if IsHTML then
    begin
      FIsHTML := false;
      IsHTML := true;
    end;
  end;  { if }
end;  { SetLinkColor }

procedure TCustomElGraphicButton.SetLinkPopupMenu(newValue : TPopupMenu);
begin
  if (FLinkPopupMenu <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FLinkPopupMenu <> nil then
      FLinkPopupMenu.RemoveFreeNotification(Self);
    {$endif}
    FLinkPopupMenu := newValue;
    if (newValue <> nil) then
       newValue.FreeNotification(Self);
  end;  { if }
end;  { SetLinkPopupMenu }

procedure TCustomElGraphicButton.SetLinkStyle(newValue : TFontStyles);
{ Sets data member FLinkStyle to newValue. }
begin
  if (FLinkStyle <> newValue) then
  begin
    FLinkStyle := newValue;
    if IsHTML then
    begin
      FIsHTML := false;
      IsHTML := true;
    end;
  end;  { if }
end;  { SetLinkStyle }

procedure TCustomElGraphicButton.DoLinkPopup(MousePos : TPoint);
begin
  if (FLinkPopupMenu <> nil) and FLinkPopupMenu.AutoPopup then
  begin
    {$ifndef CLX_USED}
    SendCancelMode(nil);
    {$endif}
    FLinkPopupMenu.PopupComponent := Self;
    if MousePos.X < 0 then
      MousePos := ClientToScreen(Point(0,0));
    FLinkPopupMenu.Popup(MousePos.X, MousePos.Y);
  end;
end;

procedure TCustomElGraphicButton.TriggerImageNeededEvent(Sender : TObject; Src :
    TElFString; var Image : TBitmap);
begin
  Image := nil;
  if (assigned(FOnImageNeeded)) then
      FOnImageNeeded(Self, Src, Image);
end;  { TriggerImageNeededEvent }

procedure TCustomElGraphicButton.TriggerLinkClickEvent(HRef : TElFString);
begin
  if (assigned(FOnLinkClick)) then
    FOnLinkClick(Self, HRef );
end;  { TriggerLinkClickEvent }
{$endif}

procedure TCustomElGraphicButton.SetImageIsAlphaBlended(Value: Boolean);
begin
  if FImageIsAlphaBlended <> Value then
  begin
    FImageIsAlphaBlended := Value;
    Invalidate;
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TCustomElGraphicButton.CMHintShow(var Message: TMessage);
{$else}
function TCustomElGraphicButton.HintShow(var HintInfo : THintInfo): Boolean;
{$endif}
var T: WideChar;
{$ifndef CLX_USED}
  HintInfo : PHintInfo;
{$endif}
  l : integer;
  S : String;
  WS : WideString;
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

procedure TCustomElGraphicButton.SetHint(Value: WideString);
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

{$ifdef HAS_HTML_RENDER}
procedure TCustomElGraphicButton.SetCursor(Value: TCursor);
var P : TPoint;
    R : TRect;
    href : TElFString;
begin
  if (FCursor <> Value) then
  begin
    FCursor := Value;
    {$IFDEF HAS_HTML_RENDER}
    if IsHTML then
    begin
      GetCursorPos(P);
      P := ScreenToClient(P);
      R := FTextRect;
      if FRender.IsCursorOverLink(Point(P.X - R.Left, P.Y - R.Top), Point(0, 0), R, href) then
        inherited Cursor := crURLCursor
      else
        inherited Cursor := FCursor;
    end
    else
    {$endif}
      inherited Cursor := FCursor;
  end;  { if }
end;  { SetCursor }
{$endif}

procedure TCustomElGraphicButton.SetChangeDisabledText(Value: Boolean);
begin
  if FChangeDisabledText <> Value then
  begin
    FChangeDisabledText := Value;
    if FState = ebsDisabled then
      Invalidate;
  end;
end;

procedure TCustomElGraphicButton.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

{$IFDEF VCL_4_USED}
procedure TElGraphicButtonActionLink.SetImageIndex(Value: Integer);
begin
  if FClient is TCustomElGraphicButton then
  begin
    TCustomElGraphicButton(FClient).ImageIndex := value;
  end;
end;

procedure TElGraphicButtonActionLink.AssignClient(AClient : TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TCustomElGraphicButton;
end;

function TElGraphicButtonActionLink.IsCheckedLinked : Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TElGraphicButtonActionLink.IsImageIndexLinked : Boolean;
begin
  result := true;
end;

procedure TElGraphicButtonActionLink.SetChecked(Value : Boolean);
begin
  if IsCheckedLinked then
  begin
    FClient.ClicksDisabled := True;
    try
      FClient.Checked := Value;
    finally
      FClient.ClicksDisabled := False;
    end;
  end;
end;

{$ifndef CLX_USED}
procedure TElGraphicButtonActionLink.SetCaption(const Value: string);
{$else}
procedure TElGraphicButtonActionLink.SetCaption(const Value: TCaption);
{$endif}
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
  inherited;
end;

procedure TElGraphicButtonActionLink.SetHint;
begin
  if IsHintLinked then
    FClient.Hint := Value;
  inherited;
end;

function TElGraphicButtonActionLink.IsCaptionLinked : Boolean;
begin
  result := false;
  if FClient is TCustomElGraphicButton then
    Result := TCustomElGraphicButton(FClient).Caption = (Action as TCustomAction).Caption;
end;

function TElGraphicButtonActionLink.IsHintLinked: Boolean;
begin
  result := false;
  if FClient is TCustomElGraphicButton then
    Result := TCustomElGraphicButton(FClient).Hint = StrPas(PChar((Action as TCustomAction).Hint));
end;

{$ENDIF}

initialization

{$ifndef CLX_USED}
  MenuCancelMsg := RegisterWindowMessage('El - Cancel pulldown menu');
{$endif}
  FArrow := TBitmap.Create;
  FArrow.LoadFromResourceName(HInstance, 'ELPOPUPBUTTONDOWNARROW');

finalization

  FArrow.Free;

end.
