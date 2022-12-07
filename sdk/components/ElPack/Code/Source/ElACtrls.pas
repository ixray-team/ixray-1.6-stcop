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

06/08/2002

  Fixed setting the width of DropDown list using DropDownWidth property

05/07/2002

  Fixed key handing in TElAdvancedComboBox (was spoiled 04/29)

04/29/2002

  Added HandleDialogKeys property to ElAdvancedComboBox

04/19/2002

  Line hint fixed in ElAdvancedListBox
  Implemented AutoCompletion property in ElAdvancedComboBox

04/17/2002

  Added DropDownWidth property to ElAdvancedComboBox

03/06/2002

  Added unicode hint

01/18/2002

  Fixed some problems with painting flat scrollbars

01/01/2002

  Fixed some problems with painting borders when focus is moved

12/28/2001

  Borders are redrawn correctly in ColorLineType mode when mouse cursor enters
  or leaves the control

12/21/2001

  Made ListWndProc and EidtWndProc methods of ComboBox virtual

11/28/2001

  LineBorderActiveColor and LineBorderInactiveColor properties added.

10/15/2001

  Improved Windows XP styles use 

10/12/2001

  Scrollbar is flat in dropdown listbox of ElAdvancedComboBox now.

10/09/2001

  Added XP support for all controls. XP support works always no matter if
  manifest file is present, so if you need to turn it off, you can do this only
  globally.

09/17/2001 (c) Akzhan Abdulin

  Added Windows XP Themes Support for ElAdvancedComboBox.
  Note that themed combobox will ignore some of its look'n'feel properties.

09/04/2001

  A nasty bug in ElAdvancedCombobox that prevented insertion of other controls
  into the combobox was fixed.

09/03/2001

  Horizontal scrollbar fixed to behave as it is supposed to

07/24/2001

  Added BtnFlat and BtnTransparent properties.

  In ElAdvancedComboBox component BorderStyle property is obsolete.

07/12/2001

  BorderSides property added.

11/24/2000

  Transparency handling improved

09/26/2000

  ElFlatMemo ignored WantReturns property value. Fixed.

09/05/2000

  ElFlatEdit and ElFlatMemo now don't grab dialog keys (Enter and Esc)

*)

unit ElACtrls;

interface

uses
  {$ifndef CLX_USED}
  Windows,
  StdCtrls,
  Messages,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  ElImgFrm,
{$ifdef VCL_6_USED}
Types,
{$endif}
  {$else}
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  Qt,
  QForms,
  Types,
  QGraphics,
  QControls,
  QStdCtrls,
  QExtCtrls,
  {$endif}
  SysUtils,
  Classes,
  ElTmSchema,
  ElUxTheme,
  ElTools,
{$ifdef ELPACK_NO_ADV_EDITS}
  ElEdits,
{$endif}
  ElVCLUtils;

type

{$ifdef ELPACK_NO_ADV_EDITS}
  TElAdvancedMemo = class(TElEdit)
  public
    constructor Create(AOwner : TComponent); override;

  end;
{$else}
  TElAdvancedMemo = class(TMemo)
  private
    FActiveBorderType: TElFlatBorderType;
    FBackground: TBitmap;
    FFlat: boolean;
    FFlatFocusedScrollBars: boolean;
    FInactiveBorderType: TElFlatBorderType;
    FMouseOver: boolean;
    FPainting: boolean;
    FPaintingTo: boolean;
    FTransparent: boolean;
    FUseBackground: boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    {$ifndef CLX_USED}
    FImgForm   : TElImageForm;
    FImgFormChLink : TImgFormChangeLink;
    {$endif}
    {$ifndef CLX_USED}
    FBorderSides: TElBorderSides;
    {$endif}
    FHandleDialogKeys: Boolean;
    FTheme: HTheme;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    {$ifndef CLX_USED}
    procedure ImageFormChange(Sender : TObject);
    procedure SetImageForm(newValue : TElImageForm);
    {$endif}
    procedure BackgroundChanged(Sender: TObject);
    {$ifndef CLX_USED}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNCtlColorEdit(var Msg: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Msg: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;

    procedure DrawBackground(DC: HDC; R: TRect);
    procedure DrawFlatBorder(DC: HDC);
    procedure DrawParentControl(DC: HDC);
    {$endif}
    procedure SetActiveBorderType(const Value: TElFlatBorderType);
    procedure SetBackground(const Value: TBitmap);
    procedure SetFlat(const Value: boolean);
    procedure SetFlatFocusedScrollBars(const Value: boolean);
    procedure SetInactiveBorderType(const Value: TElFlatBorderType);
    procedure SetTransparent(const Value: boolean);
    procedure SetUseBackground(const Value: boolean);
    {$ifndef CLX_USED}
    procedure SetBorderSides(Value: TElBorderSides);
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
        WM_WINDOWPOSCHANGED;
    {$endif}
  protected
    FUseXPThemes: Boolean;
    {$ifndef CLX_USED}
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    {$endif}
    procedure Change; override;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    {$ifndef CLX_USED}
    procedure DoPaint; dynamic;
    {$endif}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$ifndef CLX_USED}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    {$endif}
    procedure SetUseXPThemes(Value: Boolean);
    function IsThemeApplied: Boolean;

    procedure FreeThemeHandle; virtual;
    procedure CreateThemeHandle; virtual;
    {$ifndef CLX_USED}
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    {$else}
    procedure CreateWidget; override;
    procedure DestroyWidget; override;
    {$endif}
    function GetThemedClassName: WideString;
    {$ifndef CLX_USED}
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    {$endif}
    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);
    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Theme: HTheme read FTheme;
  published
    property ActiveBorderType: TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;
    property Align;
    property Background: TBitmap read FBackground write SetBackground;
    property Flat: boolean read FFlat write SetFlat default False;
    property FlatFocusedScrollBars: boolean read FFlatFocusedScrollBars write SetFlatFocusedScrollBars default False;
    property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;
    property Transparent: boolean read FTransparent write SetTransparent default False;
    property UseBackground: boolean read FUseBackground write SetUseBackground default False;
    {$ifndef CLX_USED}
    property ImageForm   : TElImageForm read FImgForm write SetImageForm;
    {$endif}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {$ifndef CLX_USED}
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    {$endif}
    property HandleDialogKeys: Boolean read FHandleDialogKeys write
        FHandleDialogKeys default false;
    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
        true;
    {$ifndef CLX_USED}
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
        SetLineBorderInactiveColor;
    {$endif}
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
  end;
{$endif}

  //TElFlatMemo = TElAdvancedMemo;

{$ifdef ELPACK_NO_ADV_EDITS}
  TCustomElAdvancedEdit = TCustomElEdit;
{$else}
  TCustomElAdvancedEdit = class(TCustomEdit)
  private
    FActiveBorderType: TElFlatBorderType;
    FAlignment: TAlignment;
    FBackground: TBitmap;
    FFlat: boolean;
    FInactiveBorderType: TElFlatBorderType;
    FMouseOver: boolean;
    FPainting: boolean;
    FPaintingTo: boolean;
    FReturnPressed: boolean;
    FTransparent: boolean;
    FUseBackground: boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    {$ifndef CLX_USED}
    FImgForm   : TElImageForm;
    FImgFormChLink : TImgFormChangeLink;
    {$endif}
    FHandleDialogKeys : boolean;
    {$ifndef CLX_USED}
    FBorderSides: TElBorderSides;
    {$endif}
    FTheme: HTheme;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    {$ifndef CLX_USED}
    procedure ImageFormChange(Sender : TObject);
    {$endif}
    procedure BackgroundChanged(Sender: TObject);
    {$ifndef CLX_USED}
    procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNCtlColorEdit(var Msg: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Msg: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure DrawBackground(DC: HDC; R: TRect);
    procedure DrawFlatBorder(DC: HDC);
    procedure DrawParentControl(DC: HDC);
    {$endif}
    procedure SetActiveBorderType(const Value: TElFlatBorderType);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBackground(const Value: TBitmap);
    procedure SetInactiveBorderType(const Value: TElFlatBorderType);
    procedure SetTransparent(const Value: boolean);
    procedure SetUseBackground(const Value: boolean);
    {$ifndef CLX_USED}
    procedure SetBorderSides(Value: TElBorderSides);
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
        WM_WINDOWPOSCHANGED;
    {$endif}
  protected
    FNoHandleEnter : boolean;
    FPasswordChar: Char;
    FUseXPThemes: Boolean;
    {$ifndef CLX_USED}
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    {$endif}

    {$ifndef MSWINDOWS}
    function NeedKey(Key: Integer; Shift: TShiftState; const KeyText: WideString): Boolean; override;
    {$endif}
    {$ifndef CLX_USED}
    procedure SetImageForm(newValue : TElImageForm); virtual;
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);
    {$endif}
    procedure SetFlat(const Value: boolean); virtual;

    {$ifndef CLX_USED}
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    {$endif}
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    {$ifndef CLX_USED}
    procedure DoPaint; dynamic;
    {$endif}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    property ActiveBorderType: TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Background: TBitmap read FBackground write SetBackground;
    property Flat: boolean read FFlat write SetFlat default False;
    property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;
    property Transparent: boolean read FTransparent write SetTransparent default False;
    property UseBackground: boolean read FUseBackground write SetUseBackground default False;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {$ifndef CLX_USED}
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
        SetLineBorderInactiveColor;
    {$endif}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$ifndef CLX_USED}
    procedure SetPasswordChar(Value: Char);
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    {$endif}
    procedure SetUseXPThemes(Value: Boolean); virtual;
    function IsThemeApplied: Boolean;

    {$ifndef CLX_USED}
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    {$else}
    procedure CreateWidget; override;
    procedure DestroyWidget; override;
    {$endif}
    procedure FreeThemeHandle; virtual;
    procedure CreateThemeHandle; virtual;
    function GetThemedClassName: WideString;
    {$ifndef CLX_USED}
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    {$endif}
    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MouseOver : boolean read FMouseOver;
    property Theme: HTheme read FTheme;
  published
    {$ifndef CLX_USED}
    property ImageForm   : TElImageForm read FImgForm write SetImageForm;
    {$endif}
    property HandleDialogKeys : boolean read FHandleDialogKeys write FHandleDialogKeys default false;
    {$ifndef CLX_USED}
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    {$endif}
    {$ifndef CLX_USED}
    property PasswordChar: Char read FPasswordChar write SetPasswordChar stored
        False default #0;
    {$endif}
    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
        true;
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
  end;
{$endif}
  
  TElAdvancedEdit = class(TCustomElAdvancedEdit)
  published
    property ActiveBorderType;
    property Align;
    property Alignment;
    {$IFDEF VCL_4_USED}
    property Anchors;
    {$ENDIF}
    property AutoSelect;
    property AutoSize;
    property Background;
    {$IFDEF VCL_4_USED}
    {$ifndef CLX_USED}
    property BiDiMode;
    {$endif}
    {$ENDIF}
    property BorderStyle;
    {$ifndef ELPACK_NO_ADV_EDITS}
    property CharCase;
    {$endif}
    property Color;
    {$IFDEF VCL_4_USED}
    property Constraints;
    {$ENDIF}
    property Cursor;
    {$ifndef CLX_USED}
    property DragCursor;
    {$endif}
    {$IFDEF VCL_4_USED}
    {$ifndef CLX_USED}
    property DragKind;
    {$endif}
    {$ENDIF}
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property HideSelection;
    {$ifndef CLX_USED}
    property ImeMode;
    property ImeName;
    {$endif}
    property InactiveBorderType;
    {$ifndef CLX_USED}
    property LineBorderActiveColor;
    property LineBorderInactiveColor;
    {$endif}

    property MaxLength;
    {$ifndef CLX_USED}
    {$ifndef ELPACK_NO_ADV_EDITS}
    property OEMConvert;
    {$endif}
    {$endif}
    {$IFDEF VCL_4_USED}
    {$ifndef CLX_USED}
    property ParentBiDiMode;
    {$endif}
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    {$ifndef CLX_USED}
    property PasswordChar;
    {$endif}
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property Text;
    property Transparent;
    property UseBackground;
    property Visible;
    property OnChange;
    property OnClick;
    {$IFDEF VCL_5_USED}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF VCL_4_USED}
    {$ifndef CLX_USED}
    property OnEndDock;
    {$endif}
    {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseLeave;
    property OnMouseUp;
    {$IFDEF VCL_4_USED}
    {$ifndef CLX_USED}
    property OnStartDock;
    {$endif}
    {$ENDIF}
    property OnStartDrag;
  end;

  TElAdvancedListBox = class(TListBox)
  private
    FActiveBorderType: TElFlatBorderType;
    FBackground: TBitmap;
    FFlat: boolean;
    FFlatFocusedScrollBars: boolean;
    FInactiveBorderType: TElFlatBorderType;
    FInvertSelection: boolean;
    FLastTopIndex: integer;
    FMouseOver: boolean;
    FSelectedColor: TColor;
    FSelectedFont: TFont;
    FTransparent: boolean;
    FUseBackground: boolean;
    {$ifndef CLX_USED}
    FImgForm   : TElImageForm;
    FImgFormChLink : TImgFormChangeLink;
    {$endif}
    FInVScroll,
    FInHScroll     : boolean;
    FTransparentSelection: Boolean;
    FBorderSides : TElBorderSides;
    FShowLineHint: Boolean;
    FCurHintItem : Integer;
    FStyle       : TListBoxStyle;
    FMaxWidth    : integer;
    FHorizontalScroll: Boolean;
    FHintTimer   : TTimer;
    FHintWnd     : THintWindow;
    FHintWndProc : TWndMethod;
    FTheme: HTheme;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    {$ifndef CLX_USED}
    procedure ImageFormChange(Sender : TObject);
    procedure SetImageForm(newValue : TElImageForm);
    {$endif}
    procedure BackgroundChanged(Sender: TObject);
    {$ifndef CLX_USED}
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Msg: TMessage); message CM_PARENTFONTCHANGED;
    procedure LBGetTopIndex(var Msg: TMessage); message LB_GETTOPINDEX;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Message: TMessage); message WM_HSCROLL;
    procedure WMNCMouseMove(var Message: TMessage); message WM_NCMOUSEMOVE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;

    procedure DrawBackground(DC: HDC; R: TRect);
    procedure DrawBackgroundEx(DC: HDC; R, SubR: TRect);
    procedure DrawFlatBorder(DC: HDC; HDragging, VDragging : boolean);
    procedure DrawParentControl(DC: HDC);
    procedure DrawParentControlEx(DC: HDC; R: TRect);
    {$endif}
    procedure IntMouseMove(XPos, YPos : SmallInt);
    procedure SelectedFontChanged(Sender: TObject);
    procedure SetActiveBorderType(const Value: TElFlatBorderType);
    procedure SetBackground(const Value: TBitmap);
    procedure SetFlat(const Value: boolean);
    procedure SetFlatFocusedScrollBars(const Value: boolean);
    procedure SetInactiveBorderType(const Value: TElFlatBorderType);
    procedure SetInvertSelection(const Value: boolean);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetTransparent(const Value: boolean);
    procedure SetUseBackground(const Value: boolean);
    procedure SetTransparentSelection(Value: Boolean);
    procedure SetBorderSides(Value: TElBorderSides);
    procedure SetStyle(Value: TListBoxStyle);
    procedure SetHorizontalScroll(Value: Boolean);
    {$ifndef CLX_USED}
    procedure ResetHorizontalExtent;
    procedure SetHorizontalExtent;
    {$endif}
    procedure CancelLineHint;
    procedure OnLineHintTimer(Sender : TObject);
    {$ifndef CLX_USED}
    procedure HintWndProc(var Message: TMessage);
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
        WM_WINDOWPOSCHANGED;
    {$endif}
  protected
    FUseXPThemes: Boolean;
    {$ifndef CLX_USED}
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    {$endif}
    function  GetItemWidth(Index: Integer): Integer; virtual;
    function GetParentCtlWidth: Integer; virtual;
    function GetParentCtlHeight: Integer; virtual;
    function  RealScreenToClient(APoint : TPoint) : TPoint; virtual;
    {$ifndef CLX_USED}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    {$else}
    procedure CreateWidget; override;
    procedure DestroyWidget; override;
    {$endif}
    {$ifndef CLX_USED}
    procedure DrawItem(Index: Integer; R: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;
    {$else}
    function DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState): Boolean; override;
    {$endif}
    function  GetBackground: TBitmap; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CreateHintWindow: THintWindow; virtual;
    {$ifndef CLX_USED}
    procedure WndProc(var Message: TMessage); override;
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    {$endif}
    procedure SetUseXPThemes(Value: Boolean);
    function IsThemeApplied: Boolean;

    procedure FreeThemeHandle; virtual;
    procedure CreateThemeHandle; virtual;
    function GetThemedClassName: WideString;
    {$ifndef CLX_USED}
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);
    {$endif}
    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);
    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Theme: HTheme read FTheme;
  published
    property ActiveBorderType: TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;
    property Background: TBitmap read GetBackground write SetBackground;
    property Flat: boolean read FFlat write SetFlat default False;
    property FlatFocusedScrollBars: boolean read FFlatFocusedScrollBars write SetFlatFocusedScrollBars default False;
    property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;
    property InvertSelection: boolean read FInvertSelection write SetInvertSelection default False;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clHighlight;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property Transparent: boolean read FTransparent write SetTransparent default False;
    property UseBackground: boolean read FUseBackground write SetUseBackground default False;
    {$ifndef CLX_USED}
    property ImageForm   : TElImageForm read FImgForm write SetImageForm;
    {$endif}
    property TransparentSelection: Boolean read FTransparentSelection write
        SetTransparentSelection default false;
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    property ShowLineHint: Boolean read FShowLineHint write FShowLineHint default
        false;
    property Style: TListBoxStyle read FStyle write SetStyle default lbStandard;
    property HorizontalScroll: Boolean read FHorizontalScroll write
        SetHorizontalScroll;
    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
        true;
    {$ifndef CLX_USED}
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
        SetLineBorderInactiveColor;
    {$endif}
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
  end;

  TElAdvancedComboBox = class(TComboBox)
  private
    FActiveBorderType : TElFlatBorderType;
    FInactiveBorderType : TElFlatBorderType;
    FFlat : boolean;
    BtnCanvas : TCanvas;
    FDropDownWidth: integer;
    {$ifndef CLX_USED}
    FImgForm   : TElImageForm;
    FImgFormChLink : TImgFormChangeLink;
    {$endif}
    FAutoCompletion: Boolean;
    FListInstance : pointer;
    FEditInstance : pointer;
    FSaveEditWndProc : Integer;
    FSaveListWndProc : Integer;
    FListWindowProc: TWndMethod;
    FEditWindowProc: TWndMethod;
    FHorizontalScroll: Boolean;
    //FFakeListBox     : TElAdvancedListBox;
    {$ifndef CLX_USED}
    FInHScroll,
    FInVScroll       : boolean;
    {$endif}
    FTheme: HTheme;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    {$ifndef CLX_USED}
    procedure ImageFormChange(Sender : TObject);
    {$endif}
    //procedure SetImageForm(newValue : TElImageForm);
    procedure SetFlat(newValue : Boolean);
    {$ifndef CLX_USED}
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure CMEnter(var Msg : TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg : TCMExit); message CM_EXIT;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    function IsFocused : boolean;
    {$endif}
    procedure SetActiveBorderType(newValue : TElFlatBorderType);
    procedure SetInactiveBorderType(newValue : TElFlatBorderType);
    procedure SetHorizontalScroll(Value: Boolean);
    {$ifndef CLX_USED}
    procedure SetHorizontalExtent;
    procedure ResetHorizontalExtent;
    {$endif}
    {$ifndef CLX_USED}
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
        WM_WINDOWPOSCHANGED;
    procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
    {$endif}
//    function GetDropDownWidth: integer;
  protected
    {$ifndef CLX_USED}
    FListHandle,
    FEditHandle : HWND;
    {$endif}
    FMouseOver : boolean;

    FHorzPos    : integer;
    FMaxWidth: integer;
    FBtnFlat: Boolean;
    FBtnTransparent: Boolean;
    FUseXPThemes: Boolean;
    FBtnThinFrame: Boolean;
    {$ifndef CLX_USED}
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    FHandleDialogKeys: Boolean;


    procedure DrawFlatBorder(DrawButton : boolean);
    procedure UpdateFrame;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message : TMessage); override;
    procedure ListWndProc(var Message : TMessage); virtual;
    procedure EditWndProc(var Message : TMessage); virtual;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    {$endif}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetBtnFlat(Value: Boolean);
    procedure SetBtnTransparent(Value: Boolean);
    function GetItemWidth(Index: Integer): Integer; virtual;
    {$ifndef CLX_USED}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$endif}
    procedure SetUseXPThemes(Value: Boolean);
    function IsThemeApplied: Boolean;

    {$ifndef CLX_USED}
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    {$else}
    procedure CreateWidget; override;
    procedure DestroyWidget; override;
    {$endif}
    procedure FreeThemeHandle; virtual;
    procedure CreateThemeHandle; virtual;
    function GetThemedClassName: WideString;

    procedure SetBtnThinFrame(Value: Boolean);
    {$ifndef CLX_USED}
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);
    {$endif}
    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoAutoComplete;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    {$ifndef CLX_USED}
    property ListHandle : HWND read FListHandle;
    property EditHandle : HWND read FEditHandle;
    property ListWindowProc: TWndMethod read FListWindowProc write FListWindowProc;
    property EditWindowProc: TWndMethod read FEditWindowProc write FEditWindowProc;
    {$endif}
    property Theme: HTheme read FTheme;
  published
    property Align;
    property Flat : Boolean read FFlat write SetFlat; { Published }

    property ActiveBorderType : TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;  { Published }
    property InactiveBorderType : TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;  { Published }
    property BtnFlat: Boolean read FBtnFlat write SetBtnFlat default false;
    property BtnTransparent: Boolean read FBtnTransparent write SetBtnTransparent
        default false;
    property HorizontalScroll: Boolean read FHorizontalScroll write SetHorizontalScroll;
    property ItemIndex;
    property ItemHeight;
    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
        true;
    property BtnThinFrame: Boolean read FBtnThinFrame write SetBtnThinFrame default
        true;
    {$ifndef CLX_USED}
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
        SetLineBorderInactiveColor;
    {$endif}
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth default 0;
    property AutoCompletion: Boolean read FAutoCompletion write FAutoCompletion;
    property HandleDialogKeys: Boolean read FHandleDialogKeys write 
        FHandleDialogKeys;
  end;

  TCustomElFlatEdit = TCustomElAdvancedEdit;
  TElFlatEdit = TElAdvancedEdit;
  TElFlatMemo = TElAdvancedMemo;
  TElFlatListBox = TElAdvancedListBox;
  TElFlatComboBox= TElAdvancedComboBox;

implementation

uses Clipbrd;

type
  THackWinControl = class(TWinControl);
  TListBoxHintWindow = class(THintWindow)
  protected
  {$ifdef CLX_USED}
    procedure VisibleChanged; override;
  {$endif}
  end;

{$ifdef CLX_USED}
procedure TListBoxHintWindow.VisibleChanged;
begin
  inherited;
  TElAdvancedListBox(Owner).CancelLinehint;
end;
{$endif}

{ TCustomElFlatEdit }

{$ifndef ELPACK_NO_ADV_EDITS}
procedure TCustomElAdvancedEdit.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
  {$ifndef CLX_USED}
  Perform(CM_COLORCHANGED, 0, 0);
  {$else}
  ColorChanged;
  {$endif} 
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.Change;
begin
  DoPaint;
  inherited;
end;

procedure TCustomElAdvancedEdit.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if (ThemesAvailable and IsThemeActive) or
     (Flat and (not Focused) and
      ((InactiveBorderType <> ActiveBorderType)) or ((InactiveBorderType = fbtColorLineBorder) and (LineBorderActiveColor <> LineBorderInactiveColor))) then DrawFlatBorder(0);
  DoMouseEnter;
end;

procedure TCustomElAdvancedEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if (ThemesAvailable and IsThemeActive) or
     (Flat and (not Focused) and
      ((InactiveBorderType <> ActiveBorderType)) or ((InactiveBorderType = fbtColorLineBorder) and (LineBorderActiveColor <> LineBorderInactiveColor))) then DrawFlatBorder(0);
  DoMouseLeave;
end;

procedure TCustomElAdvancedEdit.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty) or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
    Invalidate;
end;

procedure TCustomElAdvancedEdit.CNCtlColorEdit(var Msg: TWMCtlColorEdit);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty) or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
    SetBkMode(Msg.ChildDC, Windows.TRANSPARENT);
end;

procedure TCustomElAdvancedEdit.CNCtlColorStatic(var Msg: TWMCtlColorStatic);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty)  or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
    SetBkMode(Msg.ChildDC, Windows.TRANSPARENT);
end;
{$endif}

constructor TCustomElAdvancedEdit.Create(AOwner: TComponent);
begin
  inherited;
  FActiveBorderType := fbtSunken;
  FInactiveBorderType := fbtSunkenOuter;
  FAlignment := taLeftJustify;
  FHandleDialogKeys := false;
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChanged;
  FFlat := False;
  FMouseOver := False;
  FPainting := False;
  FPaintingTo := False;
  FTransparent := False;
  FUseBackground := False;
  FUseXPThemes := true;
  {$ifndef CLX_USED}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  {$ifndef CLX_USED}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}
  {$ifndef MSWINDOWS}
  InputKeys := [ikChars, ikNav];
  {$endif}
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.CreateParams(var Params: TCreateParams);
const
   Alignments: array [TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited;
  Params.Style := Params.Style or ES_MULTILINE or Alignments[FAlignment];
end;
{$endif}

destructor TCustomElAdvancedEdit.Destroy;
begin
  {$ifndef CLX_USED}
  ImageForm := nil;
  FImgFormChLink.Free;
  {$endif}
  FBackground.Free;
  inherited;
end;

procedure TCustomElAdvancedEdit.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomElAdvancedEdit.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.WMGetDlgCode(var Msg : TMessage);
begin
  Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.wParam, Msg.lParam);
  Msg.Result := (Msg.Result and (not DLGC_WANTALLKEYS)) or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if HandleDialogKeys then Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;
{$endif}

{$ifndef MSWINDOWS}
function TCustomElAdvancedEdit.NeedKey(Key: Integer; Shift: TShiftState; const
    KeyText: WideString): Boolea;
begin
  result := inherited NeedKey(Key, Shift, KeyText);
  if (Key = Key_Escape) or (Key = Key_Enter) or (Key = Key_Return) then
    result := HandleDialogKeys;
end;
{$endif}

procedure TCustomElAdvancedEdit.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$ifndef CLX_USED}
    if AComponent = FImgForm then
    begin
      FImgForm := nil;
      Invalidate;
    end;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.DoPaint;
const
  BorderOffsets: array [TBorderStyle] of integer = (1, -1);
var
  CtlDC, TempDC: HDC;
  OldBmp, TempBmp: HBITMAP;
begin
  FPainting := True;
  try
    if FTransparent or (FUseBackground and not FBackground.Empty) or
       ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
    begin
      HideCaret(Handle);
      CtlDC := GetDC(Handle);
      try
        TempDC := CreateCompatibleDC(CtlDC);
        try
          TempBmp := CreateCompatibleBitmap(CtlDC, ClientWidth +1, ClientHeight +1);
          try
            OldBmp := SelectObject(TempDC, TempBmp);
            FPaintingTo := True;
            try
              PaintTo(TempDC, 0, 0);
            finally
              FPainting := False;
            end;
            if FFlat and (not IsThemeApplied) then
               DrawFlatBorder(TempDC);
            BitBlt(CtlDC, BorderOffsets[BorderStyle], BorderOffsets[BorderStyle], ClientWidth, ClientHeight, TempDC, 1, 1, SRCCOPY);
            SelectObject(TempDC, OldBmp);
          finally
            DeleteObject(TempBmp);
          end;
        finally
          DeleteDC(TempDC);
        end;
      finally
        ReleaseDC(Handle, CtlDC);
      end;
      ShowCaret(Handle);
    end;
  finally
    FPainting := False;
  end;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.DrawBackground(DC: HDC; R: TRect);
var
  X, Y: integer;
begin
  if FUseBackground and not FBackground.Empty then
  begin
    X := R.Left; Y := R.Top;
    while Y < R.Bottom do
    begin
      while X < R.Right do
      begin
        BitBlt(DC, X, Y, R.Right - X, R.Bottom - Y,
          FBackground.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(X, FBackground.Width);
      end;
      X := R.Left;
      Inc(Y, FBackground.Height);
    end;
  end;
end;

procedure TCustomElAdvancedEdit.DrawFlatBorder(DC: HDC);
var
  BorderType: TElFlatBorderType;
  MustRelease: boolean;
  AColor     : TColor;
  R: TRect;
begin
  if (not FFlat) or (BorderStyle = bsNone) or (not HandleAllocated) then exit;
  if IsThemeApplied then
  begin
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_UPDATENOW);
    exit;
  end;
  MustRelease := (DC = 0);
  if MustRelease then DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);

    if Focused or FMouseOver then
    begin
      BorderType := FActiveBorderType;
      AColor := LineBorderActiveColor;
    end
    else
    begin
      BorderType := FInactiveBorderType;
      AColor := LineBorderInactiveColor;
    end;
    DrawFlatFrameEx2(DC, R, AColor, Color,  Focused or FMouseOver, Enabled, FBorderSides, BorderType);
  finally
    if MustRelease then ReleaseDC(Handle, DC);
  end;
end;

procedure TCustomElAdvancedEdit.DrawParentControl(DC: HDC);
var
  SavedDC: integer;
  P: TPoint;
begin
  if Assigned(Parent) then
  begin
    SavedDC := SaveDC(DC);
    try
      P := Parent.ScreenToClient(ClientOrigin);
      MoveWindowOrg(DC, -P.X, -P.Y);
      Parent.Perform(WM_ERASEBKGND, DC, 0);
      Parent.Perform(WM_PAINT, DC, 0);
      THackWinControl(Parent).PaintControls(DC, nil);
    finally
      RestoreDC(DC, SavedDC);
    end;
  end;
end;
{$endif}

procedure TCustomElAdvancedEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  {$ifndef CLX_USED}
  FReturnPressed := ((Key = VK_RETURN) and (Shift = [ssCtrl])) or (Key = VK_UP)
                    or (Key = VK_DOWN) or (Key = VK_PRIOR) or (Key = VK_NEXT);
  {$else}
  FReturnPressed := (((Key = KEY_RETURN) or (Key = KEY_RETURN)) and (Shift = [ssCtrl]))
                    or (Key = Key_UP) or (Key = Key_DOWN)
                    or (Key = Key_PRIOR) or (Key = Key_NEXT);

  {$endif}
end;

procedure TCustomElAdvancedEdit.KeyPress(var Key: Char);
begin
  inherited;
  {$ifndef CLX_USED}
  if ((Key = Char(VK_RETURN)) or FReturnPressed) and (not FNoHandleEnter) then
  {$else}
  if (((Key = Char(Key_Return)) or ((Key = Char(Key_Enter)))) or FReturnPressed) and (not FNoHandleEnter) then
  {$endif}
  begin
    Key := #0;
    {$ifndef CLX_USED}
    MessageBeep(0);
    {$endif}
  end;  
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TCustomElAdvancedEdit.SetImageForm(newValue : TElImageForm);
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
    if FImgForm <> nil then FImgForm.RegisterChanges(FImgFormChLink);
    if HandleAllocated then
    begin
      {$ifndef CLX_USED}
      RecreateWnd;
      Perform(CM_COLORCHANGED, 0, 0);
      {$else}
      RecreateWidget;
      colorChanged;
      {$endif}
    end;
  end;
end;
{$endif}

procedure TCustomElAdvancedEdit.SetActiveBorderType(const Value:
    TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    {$ifndef CLX_USED}
    if (Focused or FMouseOver) and (not IsThemeApplied)then DrawFlatBorder(0);
    {$endif}
  end;
end;

procedure TCustomElAdvancedEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end;  
end;

procedure TCustomElAdvancedEdit.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
end;

procedure TCustomElAdvancedEdit.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    {$ifdef MSWINDOWS}
    if Flat then
      Invalidate
    else
      RecreateWnd;
    {$endif}
  end;
end;

procedure TCustomElAdvancedEdit.SetInactiveBorderType(const Value: 
    TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    {$ifndef CLX_USED}
    if not Focused and not FMouseOver and (not IsThemeApplied) then DrawFlatBorder(0);
    {$endif}
  end;  
end;

procedure TCustomElAdvancedEdit.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;  
end;

procedure TCustomElAdvancedEdit.SetUseBackground(const Value: boolean);
begin
  if FUseBackground <> Value then
  begin
    FUseBackground := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
//  RW,
  R1,
  BgRect : TRect;
  ACtl   : TWinControl;
//  sid    : integer;
begin
  if (FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and
     (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
  begin
    if (FImgForm.Control <> Self) then
    begin
      ACtl := FImgForm.GetRealControl;
      R1 := ClientRect;
      BgRect := ClientRect;
      BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);

      BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
      BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
      BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);

      FImgForm.PaintBkgnd(Msg.DC, R1, Point(BgRect.Left, BgRect.Top), false);
    end;
  end else
  if FTransparent then
    DrawParentControl(Msg.DC)
  else
  if FUseBackground and not FBackground.Empty then
    DrawBackground(Msg.DC, ClientRect)
  else
  (*
  if IsThemeApplied then
  begin
    RW := BoundsRect;
    MapWindowPoints(Parent.Handle, Handle, RW, 2);
    if not Enabled then
      sid := ETS_DISABLED
    else
    if Focused then
      sid := ETS_FOCUSED
    else
    if ReadOnly then
      sid := ETS_READONLY
    else
      sid := ETS_NORMAL;
    DrawThemeBackground(Theme, Msg.DC, EP_EDITTEXT, sid, RW, nil);
  end
  else
  *)
    inherited;
end;

procedure TCustomElAdvancedEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  if not HandleAllocated then exit;
  if Flat and not FMouseOver and (not IsThemeApplied) then DrawFlatBorder(0);
end;

procedure TCustomElAdvancedEdit.WMMove(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomElAdvancedEdit.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty) or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate)))
  then
    if not FPainting and not FPaintingTo then DoPaint;
  if Flat and (not IsThemeApplied) then
    DrawFlatBorder(0);
end;

procedure TCustomElAdvancedEdit.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  if Flat and not FMouseOver and (not IsThemeApplied) then DrawFlatBorder(0);
end;

procedure TCustomElAdvancedEdit.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TCustomElAdvancedEdit.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) then
  begin
  if not (ebsLeft in BorderSides) then
    dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
  if not (ebsTop in BorderSides) then
    dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
  if not (ebsRight in BorderSides) then
    Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
  if not (ebsBottom in BorderSides) then
    Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  // Message.Result := WVR_REDRAW;
  end;
end;

procedure TCustomElAdvancedEdit.SetPasswordChar(Value: Char);
begin
  if FPasswordChar <> #0 then
  begin
    MessageBox(0, 'PasswordChar is not supported in ElAdvancedEdit', nil, 0);
  end;
end;

procedure TCustomElAdvancedEdit.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.WMWindowPosChanged(var Message:
    TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;
{$endif}
{
procedure TCustomElAdvancedEdit.WMNCPaint(var Msg : TMessage);
var DC : HDC;
    R,
    R1,
    R2 : TRect;
begin
  if ThemesAvailable and (BorderStyle = bsSingle) then
  begin
    R1 := BoundsRect;
    OffsetRect(R1, -R1.Left, - R1.Top);
    R.TopLeft := Parent.ClientToScreen(BoundsRect.TopLeft);
    R.BottomRight := Parent.ClientToScreen(BoundsRect.BottomRight);
    R2 := ClientRect;
    OffsetRect(R2, ClientOrigin.X - R.Left, ClientOrigin.Y - R.Top);

    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC <> 0 then
    begin
      with R2 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      DrawThemeBackgroundTo('EDIT', DC, 0, 0, R1, nil)
    end
    else
    begin
      DC := GetWindowDC(Handle);
      with R2 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      DrawThemeBackgroundTo('EDIT', DC, 0, 0, R1, nil)
    end;
    ReleaseDC(Handle, DC);
  end
  else
    inherited;
end;
}
{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.WMNCPaint(var Msg : TMessage);
var DC : HDC;
    RW,
    RC : TRect;
begin
  if IsThemeApplied and (BorderStyle = bsSingle) then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC = 0 then
    begin
      DC := GetWindowDC(Handle);
    end;

    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    DrawThemeBackground(FTheme, DC, 0, 0, RW, nil);
    ReleaseDC(Handle, DC);
  end
  else
  if not Flat and (BorderStyle = bsSingle) then
    inherited;
end;
{$endif}
procedure TCustomElAdvancedEdit.SetUseXPThemes(Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    {$ifdef MSWINDOWS}
    {$ifndef CLX_USED}
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$else}
    RedrawWindow(QWidget_winID(Handle), nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$endif}
    {$endif}
  end;
end;

function TCustomElAdvancedEdit.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TCustomElAdvancedEdit.CreateThemeHandle;
begin
  if (ThemesAvailable and IsThemeActive) then
    {$ifndef CLX_USED}
    FTheme := OpenThemeData(Handle, PWideChar(GetThemedClassName()))
    {$else}
    {$ifdef MSWINDOWS}
    FTheme := OpenThemeData(QWidget_winID(Handle), PWideChar(GetThemedClassName()))
    {$endif}
    {$endif}
  else
    FTheme := 0;
end;

procedure TCustomElAdvancedEdit.FreeThemeHandle;
begin
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
    CloseThemeData(FTheme);
  {$endif}
  FTheme := 0;
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.CreateWnd;
{$else}
procedure TCustomElAdvancedEdit.CreateWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and not IsThemeApplied then
  begin
    CreateThemeHandle;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.DestroyWnd;
{$else}
procedure TCustomElAdvancedEdit.DestroyWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and IsThemeApplied then
  begin
    FreeThemeHandle;
  end;
end;

function TCustomElAdvancedEdit.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    FreeThemeHandle;
    CreateThemeHandle;

    SetWindowPos( Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end;
  Message.Result := 1;
end;

procedure TCustomElAdvancedEdit.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then Invalidate;
  end;
end;

procedure TCustomElAdvancedEdit.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then Invalidate;
  end;
end;
{$endif}

procedure TCustomElAdvancedEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if IsThemeApplied then
    Invalidate;
end;


procedure TCustomElAdvancedEdit.WMPaste(var Message: TMessage);
var Clip  : TClipboard;
    Piece : String;
begin
  if ReadOnly then exit;
  Clip := Clipboard;
  Clip.Open;
  Piece := Clip.AsText;
  while (Pos(#10, Piece) > 0) do
    Delete(Piece, Pos(#10, Piece), 1);
  while (Pos(#13, Piece) > 0) do
    Delete(Piece, Pos(#13, Piece), 1);
  Clip.AsText := Piece;
  Clip.Close;
  inherited;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TCustomElAdvancedEdit.CMHintShow(var Message: TMessage);
{$else}
function TCustomElAdvancedEdit.HintShow(var HintInfo : THintInfo): Boolean;
{$endif}
var T: WideChar;
  l : integer;
  S : String;
  WS: WideString;
{$ifndef CLX_USED}
var
  HintInfo : PHintInfo;
{$endif}

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

procedure TCustomElAdvancedEdit.SetHint(Value: WideString);
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

{$endif ELPACK_NO_ADV_EDITS}

{ TElFlatListBox }
procedure TElAdvancedListBox.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  if Style = lbStandard then
  begin
    Canvas.Font.Assign(Font);
    Perform(LB_SETITEMHEIGHT, 0, Canvas.TextHeight('0'));
  end;
end;

procedure TElAdvancedListBox.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if (ThemesAvailable and IsThemeActive) or
    (Flat and (not Focused) and
    (not FlatFocusedScrollbars) and
    ((InactiveBorderType <> ActiveBorderType)) or ((InactiveBorderType = fbtColorLineBorder) and (LineBorderActiveColor <> LineBorderInactiveColor))) then DrawFlatBorder(0, false, false);
end;

procedure TElAdvancedListBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if (ThemesAvailable and IsThemeActive) or
   (Flat and (not Focused) and
    (not FlatFocusedScrollbars) and
    ((InactiveBorderType <> ActiveBorderType)) or ((InactiveBorderType = fbtColorLineBorder) and (LineBorderActiveColor <> LineBorderInactiveColor))) then
  DrawFlatBorder(0, false, false);
end;

procedure TElAdvancedListBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if FTransparent then Invalidate;
end;

procedure TElAdvancedListBox.CMParentFontChanged(var Msg: TMessage);
begin
  inherited;
  if ParentFont then
  begin
    FSelectedFont.OnChange := nil;
    try
      if Msg.WParam <> 0 then
        FSelectedFont.Assign(TFont(Msg.LParam))
      else
        FSelectedFont.Assign(Font);
      FSelectedFont.Color := clHighlightText;
    finally
      FSelectedFont.OnChange := SelectedFontChanged;
    end;
  end;
end;
{$endif}

constructor TElAdvancedListBox.Create(AOwner: TComponent);
begin
  inherited;
  {$ifndef CLX_USED}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}
  FActiveBorderType := fbtSunken;
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChanged;
  FFlat := False;
  FFlatFocusedScrollBars := False;
  FInactiveBorderType := fbtSunkenOuter;
  FInvertSelection := False;
  FLastTopIndex := 0;
  FSelectedColor := clHighlight;
  FSelectedFont := TFont.Create;
  FSelectedFont.Color := clHighlightText;
  FSelectedFont.OnChange := SelectedFontChanged;
  FTransparent := False;
  {$ifndef CLX_USED}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  FCurHintItem := -1;
  FUseXPThemes := true;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if not (csLoading in ComponentState) then
    if (Params.Style and LBS_OWNERDRAWFIXED = 0) and
      (Params.Style and LBS_OWNERDRAWVARIABLE = 0) then
      Params.Style := Params.Style or LBS_OWNERDRAWFIXED;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    Params.Style := Params.Style and (not WS_BORDER);
    Params.ExStyle := Params.ExStyle and (not WS_EX_CLIENTEDGE);
  end;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElAdvancedListBox.CreateWnd;
{$else}
procedure TElAdvancedListBox.CreateWidget;
{$endif}
begin
  inherited;
  {$ifndef CLX_USED}
  if Style = lbStandard then Perform(CM_FONTCHANGED, 0, 0);
  {$endif}
  if UseXPThemes and not IsThemeApplied then
  begin
    {$ifdef MSWINDOWS}
    CreateThemeHandle;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.DestroyWnd;
{$else}
procedure TElAdvancedListBox.DestroyWidget;
{$endif}
begin
  if UseXPThemes and IsThemeApplied then
  begin
    FreeThemeHandle;
  end;
  inherited;
end;

function TElAdvancedListBox.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

destructor TElAdvancedListBox.Destroy;
begin
{$ifndef CLX_USED}
  ImageForm := nil;
  FImgFormChLink.Free;
{$endif}
  FBackground.Free;
  FSelectedFont.Free;
  inherited;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.DrawBackground(DC: HDC; R: TRect);
var
  X, Y: integer;
begin
  if FUseBackground and not Background.Empty then
  begin
    X := R.Left; Y := R.Top;
    while Y < R.Bottom do
    begin
      while X < R.Right do
      begin
        BitBlt(DC, X, Y, R.Right - X, R.Bottom - Y,
          Background.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(X, Background.Width);
      end;
      X := R.Left;
      Inc(Y, Background.Height);
    end;
  end;
end;

procedure TElAdvancedListBox.DrawBackgroundEx(DC: HDC; R, SubR: TRect);
var
  Rgn: HRGN;
  SavedDC: integer;
begin
  SavedDC := SaveDC(DC);
  try
    Rgn := CreateRectRgnIndirect(SubR);
    try
      SelectClipRgn(DC, Rgn);
      DrawBackground(DC, R);
    finally
      DeleteObject(Rgn);
    end;
  finally
    RestoreDC(DC, SavedDC);
  end;
end;

procedure TElAdvancedListBox.DrawFlatBorder(DC: HDC; HDragging, VDragging : 
    boolean);
var
  R : TRect;
  BS: TElFlatBorderType;
  MustRelease: boolean;
  AColor : TColor;
  ARgn,
  Crgn   : HRgn;

const BordersFlat : array[boolean] of Integer = (0, WS_BORDER);
      Borders3D : array[boolean] of Integer = (0, WS_EX_CLIENTEDGE);

begin
  if not HandleAllocated then exit;
  if IsThemeApplied then
  begin
    if (BorderStyle = bsNone) then exit;
  
    ARgn := CreateRectRgnIndirect(R);
    R := ClientRect;
    CRgn := CreateRectRgnIndirect(R);
    CombineRgn(ARgn, ARgn, CRgn, RGN_DIFF);
    RedrawWindow(Handle, nil, ARgn, RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);
    DeleteObject(ARgn);
    DeleteObject(CRgn);
    exit;
  end;
  
  MustRelease := (DC = 0);
  if MustRelease then DC := GetWindowDC(Handle);
  R := Rect(0, 0, Width, Height);
  try
    if (BorderStyle = bsSingle) then
    begin
      if Focused or FMouseOver then
        BS := FActiveBorderType
      else
        BS := FInactiveBorderType;
      if Focused or FMouseOver then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;
      DrawFlatFrameEx2(DC, R, AColor, Color, Focused or FMouseOver, Enabled, FBorderSides, BS);
    end;
    if FFlatFocusedScrollBars or not (Focused or FMouseOver) then
      DrawFlatScrollbars(Handle, DC, R,
        (Focused or FMouseOver) and not FFlatFocusedScrollBars,
        ssBoth, HDragging, VDragging, False,
        GetWindowLong(Handle, GWL_STYLE) or BordersFlat[(not Ctl3D) and (BorderStyle = bsSingle)],
        GetWindowLong(Handle, GWL_EXSTYLE) or Borders3D[Ctl3D and (BorderStyle = bsSingle)]);
  finally
    if MustRelease then ReleaseDC(Handle, DC);
  end;
end;
{$endif}

function TElAdvancedListBox.GetParentCtlWidth: Integer;
var ACtl : TWinControl;
begin
{$ifndef CLX_USED}
  ACtl := FImgForm.GetRealControl;
{$else}
  ACtl := Self;
{$endif}
  result := ACtl.ClientWidth;
end;

function TElAdvancedListBox.GetParentCtlHeight: Integer;
var ACtl : TWinControl;
begin
{$ifndef CLX_USED}
  ACtl := FImgForm.GetRealControl;
{$else}
  ACtl := Self;
{$endif}
  result := ACtl.ClientHeight;
end;

function TElAdvancedListBox.RealScreenToClient(APoint : TPoint): TPoint;
var ACtl : TWinControl;
begin
{$ifndef CLX_USED}
  ACtl := FImgForm.GetRealControl;
{$else}
  ACtl := Self;
{$endif}
  result := ACtl.ScreenToClient(APoint);
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.DrawItem(Index: Integer; R: TRect; State:
    {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
{$else}
function TElAdvancedListBox.DrawItem(Index: Integer; R: TRect; State:
    TOwnerDrawState): Boolean;
{$endif}
var
  Flags, TextColor, BackColor: longint;
  TextRect: TRect;
  R1,
  BgRect  : TRect;
  P       : TPoint;
  ax, ay  : integer;
begin
  if (Style <> lbStandard) and Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, R, State {$ifdef CLX_USED}, Result{$endif})
  else
  begin
    {$ifndef CLX_USED}
    TextColor := GetTextColor(Canvas.Handle);
    BackColor := GetBkColor(Canvas.Handle);
    {$else}
    TextColor := Canvas.Font.Color;
    BackColor := Canvas.Brush.Color;
    {$endif}
    if (odSelected in State) and (not FInvertSelection) and FTransparentSelection then
    Canvas.Font.Color := Font.Color;

    if (odSelected in State) and (not FInvertSelection) and (not FTransparentSelection) then
    begin
      Canvas.Brush.Color := FSelectedColor;
      Canvas.FillRect(R);
    end
    else
    {$ifndef CLX_USED}
    if (FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate)) then
    begin
      if (FImgForm.Control <> Self) then
      begin
        R1 := R;
        BgRect := R;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        P := ClientToScreen(ClientRect.TopLeft);
        ax := BgRect.Left - P.x;
        ay := BgRect.Top - P.y;

        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);

        BgRect.TopLeft := RealScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := RealScreenToClient(BgRect.BottomRight);
        FImgForm.PaintBkgnd(Canvas.Handle, R1, Point(BgRect.Left - ax, BgRect.Top - ay), false);
      end;
      if (BgRect.Bottom > GetParentCtlHeight) or
         (BgRect.Right  > GetParentCtlWidth) or
         (BgRect.Left < 0) or (BgRect.Top < 0) then
        Canvas.FillRect(R);
    end else
    if FTransparent then
      DrawParentControlEx(Canvas.Handle, R)
    else
    if FUseBackground and not Background.Empty then
      DrawBackgroundEx(Canvas.Handle, ClientRect, R)
    else
    {$endif}
      if not FTransparentSelection then
        Canvas.FillRect(R);
    {$ifndef CLX_USED}
    if (odSelected in State) and FInvertSelection then
      InvertRect(Canvas.Handle, R);
    {$endif}
    if (odSelected in State) and (not FTransparentSelection)then
      Canvas.Font.Assign(FSelectedFont);
    if Index < Items.Count then
    begin
      TextRect := R;
      {$ifndef CLX_USED}
      Flags := DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX;
      {$else}
      Flags := Integer(AlignmentFlags_SingleLine) or
               Integer(AlignmentFlags_AlignVCenter) or
               Integer(AlignmentFlags_AlignLeft);
      {$endif}
      {$IFDEF VCL_4_USED}
      {$ifndef CLX_USED}
      Flags := DrawTextBiDiModeFlags(Flags);
      if not UseRightToLeftAlignment then
        Inc(TextRect.Left, 2)
      else
        Dec(TextRect.Right, 2);
      {$endif}
      {$ELSE}
      Inc(TextRect.Left, 2);
      {$ENDIF}
      {$ifndef CLX_USED}
      SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
      DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), TextRect, Flags);
      {$else}
      Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, Items[Index], Flags);
      {$endif}
    end;
    {$ifndef CLX_USED}
    SetTextColor(Canvas.Handle, TextColor);
    SetBkColor(Canvas.Handle, BackColor);
    {$else}
    Canvas.Pen.Color := TextColor;
    Canvas.Brush.Color := BackColor;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.DrawParentControl(DC: HDC);
var
  SavedDC: integer;
  P: TPoint;
begin
  if Assigned(Parent) then
  begin
    SavedDC := SaveDC(DC);
    try
      P := Parent.ScreenToClient(ClientOrigin);
      MoveWindowOrg(DC, -P.X, -P.Y);
      Parent.Perform(WM_ERASEBKGND, DC, 0);
      Parent.Perform(WM_PAINT, DC, 0);
      THackWinControl(Parent).PaintControls(DC, nil);
    finally
      RestoreDC(DC, SavedDC);
    end;
  end;
end;

procedure TElAdvancedListBox.DrawParentControlEx(DC: HDC; R: TRect);
var
  Rgn: HRGN;
  SavedDC: integer;
begin
  SavedDC := SaveDC(DC);
  try
    Rgn := CreateRectRgnIndirect(R);
    try
      SelectClipRgn(DC, Rgn);
      DrawParentControl(DC);
    finally
      DeleteObject(Rgn);
    end;
  finally
    RestoreDC(DC, SavedDC);
  end;
end;
{$endif}

function TElAdvancedListBox.GetBackground: TBitmap;
begin
  Result := FBackground;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.LBGetTopIndex(var Msg: TMessage);
var
  R: TRect;
begin
  inherited;
  if (FLastTopIndex <> Msg.Result) and (FUseBackground and not Background.Empty) then
  begin
    R := ClientRect;
    InvalidateRect(Handle, @R, False);
    FLastTopIndex := Msg.Result;
  end;
end;
{$endif}

procedure TElAdvancedListBox.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$ifndef CLX_USED}
    if AComponent = FImgForm then
    begin
      FImgForm := nil;
      Invalidate;
    end;
    {$endif}
  end;
end;

procedure TElAdvancedListBox.Loaded;
begin
  inherited;
  {$ifndef CLX_USED}
  RecreateWnd;
  {$endif}
end;

procedure TElAdvancedListBox.SelectedFontChanged(Sender: TObject);
begin
  ParentFont := False;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TElAdvancedListBox.SetImageForm(newValue : TElImageForm);
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
    if FImgForm <> nil then FImgForm.RegisterChanges(FImgFormChLink);
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;
{$endif}

procedure TElAdvancedListBox.SetActiveBorderType(const Value:
    TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    {$ifndef CLX_USED}
    if Focused or FMouseOver then DrawFlatBorder(0, false, false);
    {$endif}
  end;
end;

procedure TElAdvancedListBox.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
end;

procedure TElAdvancedListBox.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end;
end;

procedure TElAdvancedListBox.SetFlatFocusedScrollBars(const Value: boolean);
begin
  if FFlatFocusedScrollBars <> Value then
  begin
    FFlatFocusedScrollBars := Value;
    {$ifndef CLX_USED}
    if Focused then DrawFlatBorder(0, false, false);
    {$endif}
  end;
end;

procedure TElAdvancedListBox.SetInactiveBorderType(const Value: 
    TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    {$ifndef CLX_USED}
    if not Focused and not FMouseOver then DrawFlatBorder(0, false, false);
    {$endif}
  end;
end;

procedure TElAdvancedListBox.SetInvertSelection(const Value: boolean);
begin
  if FInvertSelection <> Value then
  begin
    FInvertSelection := Value;
    if (MultiSelect and (SelCount > 0)) or (ItemIndex > -1) then Invalidate;
  end;
end;

procedure TElAdvancedListBox.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    if (MultiSelect and (SelCount > 0)) or (ItemIndex > -1) then Invalidate;
  end;
end;

procedure TElAdvancedListBox.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
end;

procedure TElAdvancedListBox.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

procedure TElAdvancedListBox.SetUseBackground(const Value: boolean);
begin
  if FUseBackground <> Value then
  begin
    FUseBackground := Value;
    Invalidate;
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  R1, BgRect : TRect;
begin
  if (FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate)) then
  begin
    R1 := ClientRect;
    BgRect := ClientRect;
    BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);

    BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
    BgRect.TopLeft := RealScreenToClient(BgRect.TopLeft);
    BgRect.BottomRight := RealScreenToClient(BgRect.BottomRight);

    FImgForm.PaintBkgnd(Msg.DC, R1, Point(BgRect.Left{ - ax}, BgRect.Top{ - ay}), false);
  end else
  if FTransparent then
    DrawParentControl(Msg.DC)
  else
  if FUseBackground and not Background.Empty then
    DrawBackground(Msg.DC, ClientRect)
  else inherited;
end;

procedure TElAdvancedListBox.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  if not HandleAllocated then exit;
  if Flat and not FMouseOver then DrawFlatBorder(0, false, false);
end;

procedure TElAdvancedListBox.WMPaint(var Msg: TWMPaint);
var
  DC: HDC;
  R1, BgRect : TRect;
  P          : TPoint;
  ax, ay     : integer;
begin
  if FTransparent or (FUseBackground and not Background.Empty) or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
  begin
    Perform(LB_GETTOPINDEX, 0, 0);
    inherited;
    if Items.Count = 0 then
    begin
      DC := GetDC(Handle);
      try
        if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.Componentstate)) then
        begin
          if (FImgForm.Control <> Self) then
          begin
            R1 := ClientRect;
            BgRect := ClientRect;
            BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
            P := ClientToScreen(ClientRect.TopLeft);
            ax := BgRect.Left - P.x;
            ay := BgRect.Top - P.y;

            BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
            BgRect.TopLeft := RealScreenToClient(BgRect.TopLeft);
            BgRect.BottomRight := RealScreenToClient(BgRect.BottomRight);

            FImgForm.PaintBkgnd(DC, R1, Point(BgRect.Left - ax, BgRect.Top - ay), false);
          end
        end
        else
        if FTransparent then
          DrawParentControl(DC)
        else
          DrawBackground(DC, ClientRect);
      finally
        ReleaseDC(Handle, DC);
      end;
    end;  
  end
  else
    inherited;
  if FFlat then
  begin
    DC := GetWindowDC(Handle);
    try
      DrawFlatBorder(DC, FInHScroll, FInVScroll);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TElAdvancedListBox.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  if Flat and not FMouseOver then DrawFlatBorder(0, false, false);
end;

procedure TElAdvancedListBox.WMVScroll(var Message: TMessage);
begin
  inherited;
  if TWMScroll(Message).ScrollCode = SB_THUMBTRACK then
    FInVSCroll := true
  else
  if TWMScroll(Message).ScrollCode = SB_THUMBPOSITION then
    FInVSCroll := false;

  if (Flat and (((not FMouseOver) and (not Focused)) or (FlatFocusedScrollBars))) then
    DrawFlatBorder(0, FInHScroll, FInVScroll);
end;

procedure TElAdvancedListBox.WMHScroll(var Message: TMessage);
begin
  inherited;
  if TWMScroll(Message).ScrollCode = SB_THUMBTRACK then
    FInHSCroll := true
  else
  if TWMScroll(Message).ScrollCode = SB_THUMBPOSITION then
    FInHSCroll := false;

  if (Flat and (((not FMouseOver) and (not Focused)) or (FlatFocusedScrollBars))) then
    DrawFlatBorder(0, FInHScroll, FInVScroll);
end;

procedure TElAdvancedListBox.WMNCMouseMove(var Message: TMessage);
begin
  inherited;
  if FInVScroll or FInHScroll then
  begin
    if (Flat and
       (((not FMouseOver) and
         (not Focused)) or
       (FlatFocusedScrollBars)))
     then DrawFlatBorder(0, FInHScroll, FInVScroll);
  end;
end;

procedure TElAdvancedListBox.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  if FInVScroll or FInHScroll then
  begin
    if (Flat and
       (((not FMouseOver) and
         (not Focused)) or
       (FlatFocusedScrollBars))) then
      DrawFlatBorder(0, FInHScroll, FInVScroll);
  end;
  IntMouseMove(Message.XPos, Message.YPos);
end;
{$endif}

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElAdvancedListBox.CMHintShow(var Message: TMessage);
{$else}
function TElAdvancedListBox.HintShow(var HintInfo : THintInfo): Boolean;
{$endif}
{$ifndef CLX_USED}
var HintInfo : PHintInfo;
{$endif}
var T: WideChar;
    l : integer;
    S : String;
    WS: WideString;
begin
  {$ifndef CLX_USED}
  inherited;
  HintInfo := PHintInfo(Message.lParam);
  {$else}
  result := inherited HintShow(HintInfo);
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
{$endif}

{$ifndef CLX_USED}
procedure TElAdvancedListBox.HintWndProc(var Message: TMessage);
begin
  FHintWndProc(Message);
  if (Message.Msg = WM_SHOWWINDOW) and (Message.wParam = 0) then
  begin
    FHintWnd.WindowProc := FHintWndProc;
    CancelLineHint;
  end;
end;
{$endif}

procedure TElAdvancedListBox.OnLineHintTimer(Sender : TObject);
var i : integer;
    P : TPoint;
    R,
    R1: TRect;
begin
  FHintTimer.Enabled := false;
  if FHintWnd = nil then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    {$ifndef CLX_USED}
    i := ItemAtPos(P, true);//SendMessage(Handle, LB_ITEMFROMPOINT, 0, MakeLParam(P.X, P.Y));
    {$else}
    i := ItemAtPos(P, true);
    {$endif}
    if (i < 0) or (i >= Items.Count) then exit;
    
    if GetItemWidth(i) < ClientWidth then
      FCurHintItem := -1
    else
    if (i <> FCurHintItem) or (i < 0) or (i >= Items.Count) then
    begin
      if InRange(0, Items.Count - 1, i) then
      begin
        FCurHintItem := i;
        FHintTimer.Enabled := true;
      end
      else
        FCurHintItem := -1;
    end
    else
    begin
      FHintWnd := CreateHintWindow;
      R := FHintWnd.CalcHintRect(10000, Items[i], nil);
      {$ifndef CLX_USED}
      R1 := ItemRect(i);
      // SendMessage(Handle, LB_GETITEMRECT, i, Integer(@R1));
      {$else}
      R1 := ItemRect(i);
      {$endif}
      OffsetRect(R, R1.Left - 3, R1.Top - 3);
      R.TopLeft := ClientToScreen(R.TopLeft);
      R.BottomRight := ClientToScreen(R.BottomRight);
      FHintWnd.ActivateHint(R, Items[i]);
      {$ifndef CLX_USED}
      FHintWndProc := FHintWnd.WindowProc;
      FHintWnd.WindowProc := HintWndProc;
      {$endif}
      FHintTimer.Interval := Application.HintHidePause;
      FHintTimer.Enabled := true;
    end;
  end
  else
  begin
    FHintWnd.Free;
    FHintWnd := nil;
  end;
end;

procedure TElAdvancedListBox.CancelLineHint;
begin
  if FHintTimer <> nil then
    FHintTimer.Enabled := false;
  if FHintWnd <> nil then
  begin
    FHintWnd.Free;
    FHintWnd := nil;
  end;
end;

procedure TElAdvancedListBox.IntMouseMove(XPos, YPos : SmallInt);
var i : integer;
begin
  if (TopIndex <> -1) then
  begin
    i := TopIndex + (YPos div ItemHeight - 1);
    if YPos mod ItemHeight > 0 then
      inc(i);
    if i <> FCurHintItem then
    begin
      if FHintTimer <> nil then
        CancelLineHint;
      if ShowLineHint and (i < Items.Count) then
      begin
        FCurHintItem := i;
        if FHintTimer = nil then
          FHintTimer := TTimer.Create(nil);
        FHintTimer.Enabled := false;
        FHintTimer.OnTimer := OnLineHintTimer;
        FHintTimer.Interval:= Application.HintPause; 
        FHintTimer.Enabled := True;
      end
      else
        FCurHintItem := -1;
    end;
  end;
end;

procedure TElAdvancedListBox.SetTransparentSelection(Value: Boolean);
begin
  if FTransparentSelection <> Value then
  begin
    FTransparentSelection := Value;
    if ItemIndex > -1 then Invalidate;
  end;
end;

procedure TElAdvancedListBox.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    {$ifndef CLX_USED}
    if HandleAllocated then
      RecreateWnd;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    if (ebsLeft in BorderSides) then
      inc(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if (ebsTop in BorderSides) then
      inc(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if (ebsRight in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if (ebsBottom in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end
  else
  if (BorderStyle = bsSingle) then
  begin
    if not (ebsLeft in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if not (ebsTop in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if not (ebsRight in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if not (ebsBottom in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end;
  // Message.Result := WVR_REDRAW;
end;
{$endif}

procedure TElAdvancedListBox.SetStyle(Value: TListBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    FCurHintItem := -1;
    inherited Style := Value;
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.WndProc(var Message: TMessage);
begin
  if not HorizontalScroll then
    inherited WndProc(Message)
  else
  case Message.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        FMaxWidth := Max(FMaxWidth, GetItemWidth(Message.Result));
        SetHorizontalExtent;
      end;
    LB_DELETESTRING:
      begin
        if GetItemWidth(Message.wParam) >= FMaxWidth then
        begin
          Perform(WM_HSCROLL, SB_TOP, 0);
          inherited WndProc(Message);
          ResetHorizontalExtent;
        end
        else
          inherited WndProc(Message);
      end;
    LB_RESETCONTENT:
      begin
        FMaxWidth := 0;
        SetHorizontalExtent;
        Perform(WM_HSCROLL, SB_TOP, 0);
        inherited WndProc(Message);
      end;
    WM_SETFONT:
      begin
        inherited WndProc(Message);
        Canvas.Font.Assign(Self.Font);
        ResetHorizontalExtent;
        Exit;
      end;
    else
      inherited WndProc(Message);
  end;
end;
{$endif}

procedure TElAdvancedListBox.SetHorizontalScroll(Value: Boolean);
begin
  if FHorizontalScroll <> Value then
  begin
    FHorizontalScroll := Value;
    {$ifndef CLX_USED}
    if HandleAllocated then
      RecreateWnd;
    {$endif}
  end;
end;

function TElAdvancedListBox.GetItemWidth(Index: Integer): Integer;
var
  ATabWidth: Longint;
  S: string;
begin
  S := Items[Index] + 'W';
  {$ifndef CLX_USED}
  if TabWidth > 0 then
  begin
    ATabWidth := Round((TabWidth * Canvas.TextWidth('0')) * 0.25);
    Result := LoWord(GetTabbedTextExtent(Canvas.Handle, @S[1], Length(S),
      1, ATabWidth));
  end
  else
  {$endif}
    Result := Canvas.TextWidth(S);
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.ResetHorizontalExtent;
var
  I: Integer;
begin
  FMaxWidth := 0;
  for I := 0 to Items.Count - 1 do
    FMaxWidth := Max(FMaxWidth, GetItemWidth(I));
  SetHorizontalExtent;
end;

procedure TElAdvancedListBox.SetHorizontalExtent;
begin
  SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
end;
{$endif}

function TElAdvancedListBox.CreateHintWindow: THintWindow;
begin
  Result := TListBoxHintWindow.Create(nil);
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.IFMRepaintChildren(var Message: TMessage);
var i : integer;
begin
  inherited;
  Invalidate;
  for i := 0 to ControlCount -1 do
  begin
    if Controls[i] is TWinControl then
      PostMessage(TWinControl(Controls[i]).Handle, Message.Msg, Message.wParam, Message.lParam);
  end;
end;

procedure TElAdvancedListBox.WMWindowPosChanged(var Message:
    TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;
{$endif}
{
procedure TElAdvancedListBox.WMNCPaint(var Msg : TMessage);
var DC : HDC;
    R,
    R1,
    R2 : TRect;
begin
  inherited;
  if ThemesAvailable and (BorderStyle = bsSingle) then
  begin
    R1 := BoundsRect;
    OffsetRect(R1, -R1.Left, - R1.Top);
    R.TopLeft := Parent.ClientToScreen(BoundsRect.TopLeft);
    R.BottomRight := Parent.ClientToScreen(BoundsRect.BottomRight);
    R2 := ClientRect;
    OffsetRect(R2, ClientOrigin.X - R.Left, ClientOrigin.Y - R.Top);

    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC <> 0 then
    begin
      with R2 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      DrawThemeBackgroundTo('EDIT', DC, 0, 0, R1, nil)
    end
    else
    begin
      DC := GetWindowDC(Handle);
      with R2 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      DrawThemeBackgroundTo('EDIT', DC, 0, 0, R1, nil)
    end;
    ReleaseDC(Handle, DC);
  end
end;
}
{$ifndef CLX_USED}
procedure TElAdvancedListBox.WMNCPaint(var Msg : TMessage);
var DC : HDC;
    RW,
    RC : TRect;
begin
  
  inherited;
  if IsThemeApplied and (BorderStyle = bsSingle) then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC = 0 then
    begin
      DC := GetWindowDC(Handle);
    end;

    Windows.GetClientRect(Handle, RC);
    if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
      inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
    if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
      inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    DrawThemeBackground(FTheme, DC, 0, 0, RW, nil);
    ReleaseDC(Handle, DC);
  end;
end;
{$endif}

procedure TElAdvancedListBox.SetUseXPThemes(Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    {$ifdef MSWINDOWS}
    {$ifndef CLX_USED}
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$else}
    RedrawWindow(QWidget_winID(Handle), nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$endif}
    {$endif}
  end;
end;

function TElAdvancedListBox.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TElAdvancedListBox.CreateThemeHandle;
begin
  if (ThemesAvailable and IsThemeActive) then
    {$ifndef CLX_USED}
    FTheme := OpenThemeData(Handle, PWideChar(GetThemedClassName()))
    {$else}
    {$ifdef MSWINDOWS}
    FTheme := OpenThemeData(QWidget_winID(Handle), PWideChar(GetThemedClassName()))
    {$endif}
    {$endif}
  else
    FTheme := 0;
end;

procedure TElAdvancedListBox.FreeThemeHandle;
begin
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
    CloseThemeData(FTheme);
  {$endif}
  FTheme := 0;
end;

{$ifndef CLX_USED}
procedure TElAdvancedListBox.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    FreeThemeHandle;
    CreateThemeHandle;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end;
  Message.Result := 1;
end;

procedure TElAdvancedListBox.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then Invalidate;
  end;
end;

procedure TElAdvancedListBox.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then Invalidate;
  end;
end;
{$endif}

{$ifdef ELPACK_UNICODE}
procedure TElAdvancedListBox.SetHint(Value: WideString);
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

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElAdvancedMemo.CMHintShow(var Message: TMessage);
{$else}
function TElAdvancedMemo.HintShow(var HintInfo : THintInfo): Boolean;
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
{$endif}

{ TElFlatMemo }

{$ifndef ELPACK_NO_ADV_EDITS}
procedure TElAdvancedMemo.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
  {$ifndef CLX_USED}
  Perform(CM_COLORCHANGED, 0, 0);
  {$else}
  ColorChanged;
  {$endif}
end;

procedure TElAdvancedMemo.Change;
begin
{$ifndef CLX_USED}
  DoPaint;
{$endif}
  inherited;
end;

{$ifndef CLX_USED}
procedure TElAdvancedMemo.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if (ThemesAvailable and IsThemeActive) or
     (Flat and (not Focused) and
      ((ScrollBars <> ssNone) and (not FlatFocusedScrollbars)) and  
      ((InactiveBorderType <> ActiveBorderType)) or ((InactiveBorderType = fbtColorLineBorder) and (LineBorderActiveColor <> LineBorderInactiveColor))) then DrawFlatBorder(0);
  DoMouseEnter;
end;

procedure TElAdvancedMemo.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if (ThemesAvailable and IsThemeActive) or
     (Flat and (not Focused) and
      ((ScrollBars <> ssNone) and (not FlatFocusedScrollbars)) and  
      ((InactiveBorderType <> ActiveBorderType)) or ((InactiveBorderType = fbtColorLineBorder) and (LineBorderActiveColor <> LineBorderInactiveColor))) then DrawFlatBorder(0);
  DoMouseLeave;
end;

procedure TElAdvancedMemo.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty)  or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
    Invalidate;
end;

procedure TElAdvancedMemo.CNCtlColorEdit(var Msg: TWMCtlColorEdit);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty)   or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
    SetBkMode(Msg.ChildDC, Windows.TRANSPARENT);
end;

procedure TElAdvancedMemo.CNCtlColorStatic(var Msg: TWMCtlColorStatic);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty)   or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
  SetBkMode(Msg.ChildDC, Windows.TRANSPARENT);
end;
{$endif}

constructor TElAdvancedMemo.Create(AOwner: TComponent);
begin
  inherited;
  FActiveBorderType := fbtSunken;
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChanged;
  {$ifndef CLX_USED}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}
  FFlat := False;
  FFlatFocusedScrollBars := False;
  FInactiveBorderType := fbtSunkenOuter;
  FMouseOver := False;
  FPainting := False;
  FPaintingTo := False;
  FTransparent := False;
  FUseBackground := False;
  {$ifndef CLX_USED}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  FUseXPThemes := true;
end;

destructor TElAdvancedMemo.Destroy;
begin
  {$ifndef CLX_USED}
  ImageForm := nil;
  FImgFormChLink.Free;
  {$endif}
  FBackground.Free;
  inherited;
end;

procedure TElAdvancedMemo.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TElAdvancedMemo.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

{$ifndef CLX_USED}
procedure TElAdvancedMemo.DoPaint;
const
  BorderOffsets: array [TBorderStyle] of integer = (1, -1);
var
  CtlDC, TempDC: HDC;
  OldBmp, TempBmp: HBITMAP;
begin
  FPainting := True;
  if FTransparent or (FUseBackground and not FBackground.Empty)   or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
  begin
    HideCaret(Handle);
    CtlDC := GetDC(Handle);
    try
      TempDC := CreateCompatibleDC(CtlDC);
      try
        TempBmp := CreateCompatibleBitmap(CtlDC, ClientWidth + 1, ClientHeight + 1);
        try
          OldBmp := SelectObject(TempDC, TempBmp);
          FPaintingTo := True;
          try
            PaintTo(TempDC, 0, 0);
          finally
            FPaintingTo := False;
          end;
          if IsThemeApplied or FFlat then DrawFlatBorder(TempDC);
          BitBlt(CtlDC, BorderOffsets[BorderStyle], BorderOffsets[BorderStyle], ClientWidth, ClientHeight, TempDC, 1, 1, SRCCOPY);
          SelectObject(TempDC, OldBmp);
        finally
          DeleteObject(TempBmp);
        end;
      finally
        DeleteDC(TempDC);
      end;
    finally
      ReleaseDC(Handle, CtlDC);
    end;
    ShowCaret(Handle);
  end;
  FPainting := False;
end;

procedure TElAdvancedMemo.DrawBackground(DC: HDC; R: TRect);
var
  X, Y: integer;
begin
  if FUseBackground and not FBackground.Empty then
  begin
    X := R.Left; Y := R.Top;
    while Y < R.Bottom do
    begin
      while X < R.Right do
      begin
        BitBlt(DC, X, Y, R.Right - X, R.Bottom - Y,
          FBackground.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(X, FBackground.Width);
      end;
      X := R.Left;
      Inc(Y, FBackground.Height);
    end;
  end;
end;

procedure TElAdvancedMemo.DrawFlatBorder(DC: HDC);
var
  R : TRect;
  BS: TElFlatBorderType;
  MustRelease: boolean;
  AColor : TColor;
  ARgn,
  CRgn   : HRgn;
const BordersFlat : array[boolean] of Integer = (0, WS_BORDER);
      Borders3D : array[boolean] of Integer = (0, WS_EX_CLIENTEDGE);

begin
  //if (BorderStyle = bsNone) and (not IsThemeApplied) then exit;
  if not HandleAllocated then exit;
  if IsThemeApplied then
  begin
    if (BorderStyle = bsNone) then exit;
    ARgn := CreateRectRgnIndirect(R);
    R := ClientRect;
    CRgn := CreateRectRgnIndirect(R);
    CombineRgn(ARgn, ARgn, CRgn, RGN_DIFF);
    RedrawWindow(Handle, nil, ARgn, RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);
    DeleteObject(ARgn);
    DeleteObject(CRgn);
    exit;
  end;
  MustRelease := (DC = 0);
  if MustRelease then DC := GetWindowDC(Handle);
  R := Rect(0, 0, Width, Height);
  try
    if (BorderStyle = bsSingle) then
    begin
      if Focused or FMouseOver then
        BS := FActiveBorderType
      else
        BS := FInactiveBorderType;
      if Focused or FMouseOver then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;
      DrawFlatFrameEx2(DC, R, AColor, Color, Focused or FMouseOver, Enabled, FBorderSides, BS);
    end;
    if FFlatFocusedScrollBars or not (Focused or FMouseOver) then
      DrawFlatScrollbars(Handle, DC, R,
        (Focused or FMouseOver) and not FFlatFocusedScrollBars,
        ssBoth, False, False, False,
        GetWindowLong(Handle, GWL_STYLE) or BordersFlat[(not Ctl3D) and (BorderStyle = bsSingle)],
        GetWindowLong(Handle, GWL_EXSTYLE) or Borders3D[Ctl3D and (BorderStyle = bsSingle)]);
  finally
    if MustRelease then ReleaseDC(Handle, DC);
  end;
end;

procedure TElAdvancedMemo.DrawParentControl(DC: HDC);
var
  SavedDC: integer;
  P: TPoint;
begin
  if Assigned(Parent) then
  begin
    SavedDC := SaveDC(DC);
    try
      P := Parent.ScreenToClient(ClientOrigin);
      MoveWindowOrg(DC, -P.X, -P.Y);
      Parent.Perform(WM_ERASEBKGND, DC, 0);
      Parent.Perform(WM_PAINT, DC, 0);
      THackWinControl(Parent).PaintControls(DC, nil);
    finally
      RestoreDC(DC, SavedDC);
    end;
  end;
end;

procedure TElAdvancedMemo.WMGetDlgCode(var Msg : TMessage);
begin
  Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.wParam, Msg.lParam);
  Msg.Result := (Msg.Result and (not DLGC_WANTALLKEYS)) or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if WantReturns then Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
  if WantTabs then Msg.Result := Msg.Result or DLGC_WANTTAB;
  if HandleDialogKeys then
    Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;
{$endif}

procedure TElAdvancedMemo.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$ifndef CLX_USED}
    if AComponent = FImgForm then
    begin
      FImgForm := nil;
      Invalidate;
    end;
    {$endif}
  end;
end;

procedure TElAdvancedMemo.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    {$ifndef CLX_USED}
    if Focused or FMouseOver then DrawFlatBorder(0);
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedMemo.ImageFormChange(Sender : TObject);
begin
  if HandleAllocated then
    Invalidate;
end;

procedure TElAdvancedMemo.SetImageForm(newValue : TElImageForm);
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
    if FImgForm <> nil then FImgForm.RegisterChanges(FImgFormChLink);
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$else}
    ColorChanged;
    {$endif}
  end;
end;
{$endif}

procedure TElAdvancedMemo.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
end;

procedure TElAdvancedMemo.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end;
end;

procedure TElAdvancedMemo.SetFlatFocusedScrollBars(const Value: boolean);
begin
  if FFlatFocusedScrollBars <> Value then
  begin
    FFlatFocusedScrollBars := Value;
    {$ifndef CLX_USED}
    if Focused then DrawFlatBorder(0);
    {$endif}
  end;  
end;

procedure TElAdvancedMemo.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    {$ifndef CLX_USED}
    if not Focused and not FMouseOver then DrawFlatBorder(0);
    {$endif}
  end;
end;

procedure TElAdvancedMemo.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

procedure TElAdvancedMemo.SetUseBackground(const Value: boolean);
begin
  if FUseBackground <> Value then
  begin
    FUseBackground := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedMemo.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
//  RW,
  R1,
  BgRect : TRect;
  ACtl   : TWinControl;
//  sid    : integer;
begin
  if (FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in FImgForm.GetRealControl.Componentstate)) then
  begin
    if (FImgForm.Control <> Self) then
    begin
      ACtl := FImgForm.GetRealControl;
      R1 := ClientRect;
      BgRect := ClientRect;
      BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
      BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
      BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
      BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);

      FImgForm.PaintBkgnd(Msg.DC, R1, Point(BgRect.Left, BgRect.Top), false);
    end;
  end
  else
  if FTransparent then
    DrawParentControl(Msg.DC)
  else
  if FUseBackground and not FBackground.Empty then
    DrawBackground(Msg.DC, ClientRect)
  else
  (*
  if IsThemeApplied then
  begin
    RW := BoundsRect;
    MapWindowPoints(Parent.Handle, Handle, RW, 2);
    if not Enabled then
      sid := ETS_DISABLED
    else
    if Focused then
      sid := ETS_FOCUSED
    else
    if ReadOnly then
      sid := ETS_READONLY
    else
      sid := ETS_NORMAL;
    DrawThemeBackground(Theme, Msg.DC, EP_EDITTEXT, sid, RW, nil);
  end
  else
  *)
    inherited;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElAdvancedMemo.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  if not HandleAllocated then exit;
  if Flat and not FMouseOver then DrawFlatBorder(0);
end;

procedure TElAdvancedMemo.WMMove(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TElAdvancedMemo.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  if FTransparent or (FUseBackground and not FBackground.Empty) or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
    if not FPainting and not FPaintingTo then DoPaint;
  if FFlat and (not IsThemeApplied) then
    DrawFlatBorder(0);
end;

procedure TElAdvancedMemo.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  if Flat and (not FMouseOver) and (not IsThemeApplied) then DrawFlatBorder(0);
end;

procedure TElAdvancedMemo.WMKeyDown(var Message: TWMKey);
begin
  with Message do
    if (CharCode = VK_ESCAPE) and (KeyDataToShiftState(KeyData) = []) then
      GetParentForm(Self).Perform(CM_DIALOGKEY, CharCode, KeyData)
    else
      inherited;
end;

procedure TElAdvancedMemo.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TElAdvancedMemo.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    if (ebsLeft in BorderSides) then
      inc(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if (ebsTop in BorderSides) then
      inc(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if (ebsRight in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if (ebsBottom in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end
  else
  if (BorderStyle = bsSingle) then
  begin
    if not (ebsLeft in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if not (ebsTop in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if not (ebsRight in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if not (ebsBottom in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end;
  // Message.Result := WVR_REDRAW;
end;

procedure TElAdvancedMemo.IFMRepaintChildren(var Message: TMessage);
var i : integer;
begin
  inherited;
  Invalidate;
  for i := 0 to ControlCount -1 do
  begin
    if Controls[i] is TWinControl then
      PostMessage(TWinControl(Controls[i]).Handle, Message.Msg, Message.wParam, Message.lParam);
  end;
end;

procedure TElAdvancedMemo.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;

procedure TElAdvancedMemo.WMNCPaint(var Msg : TMessage);
var DC : HDC;
    RW,
    RC : TRect;
begin
  inherited;
  if IsThemeApplied and (BorderStyle = bsSingle) then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC = 0 then
    begin
      DC := GetWindowDC(Handle);
    end;

    Windows.GetClientRect(Handle, RC);
    if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
      inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
    if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
      inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    DrawThemeBackground(FTheme, DC, 0, 0, RW, nil);
    ReleaseDC(Handle, DC);
  end;
end;
{$endif}

procedure TElAdvancedMemo.SetUseXPThemes(Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    {$ifdef MSWINDOWS}
    {$ifndef CLX_USED}
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$else}
    RedrawWindow(QWidget_winID(Handle), nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$endif}
    {$endif}
  end;
end;

function TElAdvancedMemo.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TElAdvancedMemo.CreateThemeHandle;
begin
  if (ThemesAvailable and IsThemeActive) then
    {$ifndef CLX_USED}
    FTheme := OpenThemeData(Handle, PWideChar(GetThemedClassName()))
    {$else}
    {$ifdef MSWINDOWS}
    FTheme := OpenThemeData(QWidget_winID(Handle), PWideChar(GetThemedClassName()))
    {$endif}
    {$endif}
  else
    FTheme := 0;
end;

procedure TElAdvancedMemo.FreeThemeHandle;
begin
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
    CloseThemeData(FTheme);
  {$endif}
  FTheme := 0;
end;

{$ifndef CLX_USED}
procedure TElAdvancedMemo.CreateWnd;
{$else}
procedure TElAdvancedMemo.CreateWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and not IsThemeApplied then
  begin
    CreateThemeHandle;
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedMemo.DestroyWnd;
{$else}
procedure TElAdvancedMemo.DestroyWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and IsThemeApplied then
  begin
    FreeThemeHandle;
  end;
end;

function TElAdvancedMemo.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

{$ifndef CLX_USED}
procedure TElAdvancedMemo.WMThemeChanged(var Message: TMessage);
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

procedure TElAdvancedMemo.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then Invalidate;
  end;
end;

procedure TElAdvancedMemo.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then Invalidate;
  end;
end;
{$endif}

{$endif ELPACK_NO_ADV_EDITS}

{$ifndef CLX_USED}
procedure TElAdvancedComboBox.CMEnter(var Msg: TCMEnter);
begin
  inherited;
  Invalidate;
end;

procedure TElAdvancedComboBox.CMExit(var Msg: TCMExit);
begin
  inherited;
  Invalidate;
end;

procedure TElAdvancedComboBox.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := true;
  if (ThemesAvailable and IsThemeActive) or
    (Flat and (not IsFocused) and
    (not BtnThinFrame) and
    ((InactiveBorderType <> ActiveBorderType)) or ((InactiveBorderType = fbtColorLineBorder) and (LineBorderActiveColor <> LineBorderInactiveColor))) then UpdateFrame;
end;

procedure TElAdvancedComboBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := false;
  if (ThemesAvailable and IsThemeActive) or
    (Flat and (not IsFocused) and
    (not BtnThinFrame) and
    ((InactiveBorderType <> ActiveBorderType)) or ((InactiveBorderType = fbtColorLineBorder) and (LineBorderActiveColor <> LineBorderInactiveColor))) then UpdateFrame;
end;

procedure TElAdvancedComboBox.CNCommand(var Message: TWMCommand);
begin
  inherited;
  if Message.NotifyCode = CBN_DROPDOWN then
  begin
    if FDropDownWidth >= Width then
      Perform( CB_SETDROPPEDWIDTH, FDropDownWidth, 0);
  end;
  if Message.NotifyCode = CBN_CLOSEUP then Invalidate;
end;
{$endif}

constructor TElAdvancedComboBox.Create(AOwner: TComponent);
begin
  inherited;
  TControlCanvas(Canvas).Control := Self;
  BtnCanvas := TCanvas.Create;
  FActiveBorderType := fbtSunken;
  FInactiveBorderType := fbtSunkenOuter;

  {$ifndef CLX_USED}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  {$ifndef CLX_USED}
  FListInstance := MakeObjectInstance(ListWndProc);
  FEditInstance := MakeObjectInstance(EditWndProc);
  {$endif}
  FBtnThinFrame := true;
  FUseXPThemes := true;
  FDropDownWidth := 0;
end;

procedure TElAdvancedComboBox.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$ifndef CLX_USED}
    if AComponent = FImgForm then
    begin
      FImgForm := nil;
      Invalidate;
    end;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedComboBox.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElAdvancedComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and not WS_BORDER;
  Params.ExStyle := (Params.ExStyle and not WS_EX_CLIENTEDGE);
  if HorizontalScroll then
    Params.Style := Params.Style or WS_HSCROLL;
end;
{$endif}

destructor TElAdvancedComboBox.Destroy;
begin
  {$ifndef CLX_USED}
  FImgForm := nil;
  FImgFormChLink.Free;
  {$endif}
  BtnCanvas.Free;
  inherited;
  FreeObjectInstance(FListInstance);
  FreeObjectInstance(FEditInstance);
end;

{$ifndef CLX_USED}
procedure TElAdvancedComboBox.DrawFlatBorder(DrawButton: boolean);
var
  DC : HDC;
  R,
  rc : TRect;
  BS : TElFlatBorderType;
  AColor: TColor;
  iStateId: Integer;
begin
  R := Rect(0, 0, Width, Height);

  DC := GetWindowDC(Handle);
  try
{$IFDEF WIN32}
    if Enabled then
    begin
      if DroppedDown then
      begin
        iStateId := CBXS_PRESSED;
      end
      else
      begin
        if FMouseOver then
        begin
          iStateId := CBXS_HOT;
        end
        else
        begin
          iStateId := CBXS_NORMAL;
        end;
      end;
    end
    else
    begin
      iStateId := CBXS_DISABLED;
    end;
    if IsThemeApplied then
    begin
      rc := ClientRect;
      rc.Right := rc.right - GetSystemMetrics(SM_CXVSCROLL);
      InflateRect(rc, -2, -2);
      with RC do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
    end;

    rc := R;
    
    InflateRect(rc, -1, -1);
    rc.left := rc.right - GetSystemMetrics(SM_CXVSCROLL);
    if not (IsThemeApplied and
            SUCCEEDED(DrawThemeBackground(FTheme, DC, 0, 0, R, nil)) and
            SUCCEEDED(DrawThemeBackgroundTo('COMBOBOX', DC, CP_DROPDOWNBUTTON, iStateId, rc, nil))) then
{$ENDIF}
    begin
      if Flat then
      begin
        if (IsFocused or FMouseOver) then
           BS := FActiveBorderType
        else
           BS := FInactiveBorderType;
      end
        else BS := fbtSunken;

      BtnCanvas.Handle := DC;
      BtnCanvas.Brush.Color := Color;
      rc := ClientRect;
      if IsFocused or FMouseOver then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;
      DrawFlatFrameEx2(DC, R, AColor, Color, (IsFocused or FMouseOver), Enabled, AllBorderSides, BS);
      if Color = clWindow then
        AColor := clBtnFace
      else
        AColor := Color;

      rc := ClientRect;
      //with RC do
      //  ExcludeClipRect(DC, Left, Top, Right, Bottom);

      //if ThemesAvailable then
       //DrawThemeBackgroundTo('EDIT', DC, 0, 0, R, nil);

      rc := R;
      InflateRect(rc, -2, -2);
      rc.left := rc.right - GetSystemMetrics(SM_CXVSCROLL);
      if DrawButton then
      begin
        if BtnTransparent then
          BtnCanvas.Brush.Color := Color
        else
          BtnCanvas.Brush.Color := AColor;
        BtnCanvas.FillRect(rc);
        BtnCanvas.Brush.Color := AColor;
        if not BtnTransparent then
        begin
          if BtnFlat and not DroppedDown then
          begin
            BtnCanvas.FrameRect(rc);
          end  //DrawButtonFrameEx(DC, rc, (IsFocused and Flat), DroppedDown, AColor, true)
          else
          if not BtnTransparent then
          begin
            if Flat and (BS = fbtColorLineBorder) then
            begin
              if Focused or FMouseOver then
                AColor := LineBorderActiveColor
              else
                AColor := LineBorderInactiveColor;
              DrawFlatFrameEx2(DC, rc, AColor, BtnCanvas.Brush.Color, (not Flat) or (IsFocused or FMouseOver), Enabled, AllBorderSides, BS);
            end
            else
              DrawButtonFrameEx(DC, rc, (not Flat) or ((IsFocused or FMouseOver) and (not BtnThinFrame)), DroppedDown, AColor, BtnThinFrame);
          end;
        end;
        DrawArrow(BtnCanvas, eadDown, rc, Font.Color, Enabled);
      end;
      BtnCanvas.Handle := 0;
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

function TElAdvancedComboBox.IsFocused: Boolean;
var
  WND : HWND;
begin
  if ((GetWindowLong(Handle, GWL_STYLE) and 03) = CBS_DROPDOWN) then
  begin
    Wnd := GetTopWindow(Handle);
    if (Wnd <> 0) and (Wnd = GetFocus) then
    begin
      result := true;
      exit;
    end;
  end;
  result := GetFocus = Handle;//Focused;
end;
{$endif}

procedure TElAdvancedComboBox.SetActiveBorderType(newValue: TElFlatBorderType);
begin
  if (FActiveBorderType <> newValue) then
  begin
    FActiveBorderType := newValue;
    {$ifndef CLX_USED}
    if Flat and (Focused or FMouseOver) then UpdateFrame;
    {$endif}
  end;
end;

procedure TElAdvancedComboBox.SetFlat(newValue: Boolean);
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    {$ifndef CLX_USED}
    UpdateFrame;
    {$endif}
  end;
end;

procedure TElAdvancedComboBox.SetInactiveBorderType(newValue: 
    TElFlatBorderType);
begin
  if (FInactiveBorderType <> newValue) then
  begin
    FInactiveBorderType := newValue;
    {$ifndef CLX_USED}
    if Flat and not (Focused or FMouseOver) then UpdateFrame;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedComboBox.UpdateFrame;
var
  R : TRect;
begin
  R := Rect(0, 0, Width, Height);
  if HandleAllocated then
     RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame);
end;

procedure TElAdvancedComboBox.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TElAdvancedComboBox.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if not HandleAllocated then exit;
  if Flat then UpdateFrame;
end;

procedure TElAdvancedComboBox.WMNCPaint(var Msg: TMessage);
begin
  inherited;
  if Flat then
  begin
    DrawFlatBorder(false);
    Msg.Result := 0;
  end;
end;

procedure TElAdvancedComboBox.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  DrawFlatBorder(true);
end;

procedure TElAdvancedComboBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TElAdvancedComboBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  if Flat then UpdateFrame;
end;

procedure TElAdvancedComboBox.ListWndProc(var Message : TMessage);

    procedure DrawFlatBorder(Handle : HWND; DC: HDC; InHScroll, InVScroll : boolean);
    var
      R : TRect;
      MustRelease: boolean;
    begin
      if IsThemeApplied then exit;
      MustRelease := (DC = 0);
      if MustRelease then DC := GetWindowDC(Handle);
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      try
        if Flat then
          DrawFlatScrollbars(Handle, DC, R, false, ssBoth, InHScroll, InVScroll, False, GetWindowLong(Handle, GWL_STYLE), GetWindowLong(Handle, GWL_EXSTYLE));
      finally
        if MustRelease then ReleaseDC(Handle, DC);
      end;
    end;

begin
  if Message.Msg = WM_DESTROY then
  begin
    if FSaveListWndProc <> 0 then
    begin
      Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
      SetWindowLong(FListHandle, GWL_WNDPROC, FSaveListWndProc);
    end;
    if assigned(ListWindowProc) then
       ListWindowProc(Message);
    //FSaveListWndProc := 0;
    //FListHandle := 0;
    exit;
  end;
  if assigned(ListWindowProc) then
    ListWindowProc(Message);
  if Message.Msg = WM_NCPAINT then
  begin
    Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
    DrawFlatBorder(FListHandle, 0, false, false);
  end
  else
  if Message.Msg = WM_HSCROLL then
  begin
    Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
    if TWMScroll(Message).ScrollCode = SB_THUMBTRACK then
      FInHSCroll := true
    else
    if TWMScroll(Message).ScrollCode = SB_THUMBPOSITION then
      FInHSCroll := false;
    if Flat then
      DrawFlatBorder(FListHandle, 0, FInHScroll, FInVScroll);
  end
  else
  if Message.Msg = WM_VSCROLL then
  begin
    Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
    if TWMScroll(Message).ScrollCode = SB_THUMBTRACK then
      FInVSCroll := true
    else
    if TWMScroll(Message).ScrollCode = SB_THUMBPOSITION then
      FInVSCroll := false;

    if Flat then
      DrawFlatBorder(FListHandle, 0, FInHScroll, FInVScroll);
  end
  else
  if HorizontalScroll then
  begin
    case Message.Msg of
      LB_ADDSTRING, LB_INSERTSTRING:
        begin
          Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
          FMaxWidth := Max(FMaxWidth, GetItemWidth(Message.Result));
          SetHorizontalExtent;
        end;
      LB_DELETESTRING:
        begin
          if GetItemWidth(Message.wParam) >= FMaxWidth then
          begin
            Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
            ResetHorizontalExtent;
          end
          else
            Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
        end;
      LB_RESETCONTENT:
        begin
          FMaxWidth := 0;
          SetHorizontalExtent;
          Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
        end;
      WM_SETFONT:
        begin
          Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
          Canvas.Font.Assign(Self.Font);
          ResetHorizontalExtent;
        end;
      else
        Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
    end;
  end
  else
    Message.Result := CallWindowProc(Pointer(FSaveListWndProc), FListHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TElAdvancedComboBox.EditWndProc(var Message : TMessage);
begin
  if Message.Msg = WM_DESTROY then
  begin
    if FSaveEditWndProc <> 0 then
    begin
      SetWindowLong(FEditHandle, GWL_WNDPROC, FSaveEditWndProc);
      Message.Result := CallWindowProc(Pointer(FSaveEditWndProc), FEditHandle, Message.Msg, Message.wParam, Message.lParam);
    end;
    if assigned(EditWindowProc) then
      EditWindowProc(Message);
    FSaveEditWndProc := 0;
    FEditHandle := 0;
    exit;
  end
  else
  if Message.Msg = WM_CHAR then
  begin
    Message.Result := CallWindowProc(Pointer(FSaveEditWndProc), FEditHandle, Message.Msg, Message.wParam, Message.lParam);
    if AutoCompletion and (Style = csDropDown) and (TWMChar(Message).CharCode <> VK_BACK) then
     DoAutoComplete;
    exit;
  end
  else
  if HandleDialogKeys and (Message.Msg = WM_GETDLGCODE) then
  begin
    Message.Result := CallWindowProc(Pointer(FSaveEditWndProc), FEditHandle, Message.Msg, Message.wParam, Message.lParam);
    Message.Result := Message.Result or DLGC_WANTALLKEYS; 
  end
  else
  if assigned(EditWindowProc) then
    EditWindowProc(Message);
  Message.Result := CallWindowProc(Pointer(FSaveEditWndProc), FEditHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TElAdvancedComboBox.WndProc(var Message : TMessage);
var FChildHandle : HWND;
    buf : array[0..50] of char;
begin
  if (Message.Msg = WM_PARENTNOTIFY) then
  begin
    if LoWord(Message.wParam) = WM_DESTROY then
    begin
      if HWND(Message.lParam) = FEditHandle then
      begin
        if FSaveEditWndProc <> 0 then
          SetWindowLong(FEditHandle, GWL_WNDPROC, FSaveEditWndProc);
        FEditHandle := 0;
      end
      else
      if HWND(Message.lParam) = FListHandle then
      begin
        if FSaveListWndProc <> 0 then
          SetWindowLong(FListHandle, GWL_WNDPROC, FSaveListWndProc);
        FListHandle := 0;
      end;
    end;
  end;
  inherited;
  if (Message.Msg = WM_PARENTNOTIFY) then
  begin
    if LoWord(Message.wParam) = WM_CREATE then
    begin
      FChildHandle := Message.lParam;
      GetClassName(FChildHandle, @Buf, 50);
      if (StrPas(PAnsiChar(AnsiString(Buf))) = 'Edit') and
         ((GetWindowLong(Handle, GWL_STYLE) and CBS_DROPDOWN) = CBS_DROPDOWN) then
      begin
        FEditHandle := FChildHandle;
        FSaveEditWndProc := GetWindowLong(FEditHandle, GWL_WNDPROC);
        SetWindowLong(FEditHandle, GWL_WNDPROC, Integer(FEditInstance));
      end
      else
      if (StrPas(PAnsiChar(AnsiString(Buf))) = 'ComboLBox') then
      begin
        FListHandle := FChildHandle;
        if (FSaveListWndProc = 0) then
        begin
          if FSaveListWndProc <> GetWindowLong(FListHandle, GWL_WNDPROC) then
          begin
            FSaveListWndProc := GetWindowLong(FListHandle, GWL_WNDPROC);
            SetWindowLong(FListHandle, GWL_WNDPROC, Integer(FListInstance));
          end;
        end;
        if HorizontalScroll then
          SetWindowLong(FListHandle, GWL_STYLE, GetWindowLong(FListHandle, GWL_STYLE) or WS_HSCROLL);
      end;
    end;
  end;
end;

procedure TElAdvancedComboBox.WMThemeChanged(var Message: TMessage);
begin
  RecreateWnd;
  Message.Result := 1;
end;
{$endif}

procedure TElAdvancedComboBox.SetHorizontalScroll(Value: Boolean);
begin
  if FHorizontalScroll <> Value then
  begin
    FHorizontalScroll := Value;
    {$ifndef CLX_USED}
    if HandleAllocated then
      RecreateWnd;
    {$endif}
  end;
end;

function TElAdvancedComboBox.GetItemWidth(Index: Integer): Integer;
var
  S: string;
begin
  S := Items[Index] + 'W';
  Result := Canvas.TextWidth(S);
end;

{$ifndef CLX_USED}
procedure TElAdvancedComboBox.SetHorizontalExtent;
begin
  SendMessage(FListHandle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
  SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
end;

procedure TElAdvancedComboBox.ResetHorizontalExtent;
var
  I: Integer;
begin
  FMaxWidth := 0;
  for I := 0 to Items.Count - 1 do
    FMaxWidth := Max(FMaxWidth, GetItemWidth(I));
  SetHorizontalExtent;
end;

procedure TElAdvancedComboBox.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;

procedure TElAdvancedComboBox.WMWindowPosChanged(var Message:
    TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;
{$endif}

procedure TElAdvancedComboBox.SetUseXPThemes(Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    {$ifdef MSWINDOWS}
    {$ifndef CLX_USED}
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$else}
    RedrawWindow(QWidget_winID(Handle), nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$endif}
    {$endif}
  end;
end;

function TElAdvancedComboBox.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TElAdvancedComboBox.CreateThemeHandle;
begin
  if (ThemesAvailable and IsThemeActive) then
    {$ifndef CLX_USED}
    FTheme := OpenThemeData(Handle, PWideChar(GetThemedClassName()))
    {$else}
    {$ifdef MSWINDOWS}
    FTheme := OpenThemeData(QWidget_winID(Handle), PWideChar(GetThemedClassName()))
    {$endif}
    {$endif}
  else
    FTheme := 0;
end;

procedure TElAdvancedComboBox.FreeThemeHandle;
begin
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
    CloseThemeData(FTheme);
  {$endif}
  FTheme := 0;
end;

{$ifndef CLX_USED}
procedure TElAdvancedComboBox.CreateWnd;
{$else}
procedure TElAdvancedComboBox.CreateWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and not IsThemeApplied then
  begin
    CreateThemeHandle;
  end;
end;

{$ifndef CLX_USED}
procedure TElAdvancedComboBox.DestroyWnd;
{$else}
procedure TElAdvancedComboBox.DestroyWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and IsThemeApplied then
  begin
    FreeThemeHandle;
  end;
end;

function TElAdvancedComboBox.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

procedure TElAdvancedComboBox.SetBtnThinFrame(Value: Boolean);
begin
  if FBtnThinFrame <> Value then
  begin
    FBtnThinFrame := Value;
    Invalidate;
  end;
end;

procedure TElAdvancedComboBox.SetBtnFlat(Value: Boolean);
begin
  if FBtnFlat <> Value then
  begin
    FBtnFlat := Value;
    Invalidate;
  end;
end;

procedure TElAdvancedComboBox.SetBtnTransparent(Value: Boolean);
begin
  if FBtnTransparent <> Value then
  begin
    FBtnTransparent := Value;
    Invalidate;
  end;
end;

procedure TElAdvancedComboBox.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then Invalidate;
  end;
end;

procedure TElAdvancedComboBox.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then Invalidate;
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElAdvancedComboBox.CMHintShow(var Message: TMessage);
{$else}
function TElAdvancedComboBox.HintShow(var HintInfo : THintInfo): Boolean; 
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

procedure TElAdvancedComboBox.SetHint(Value: WideString);
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

procedure TElAdvancedComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TElAdvancedComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if AutoCompletion and not DroppedDown and (Shift = []) and (Style = csDropDown) then
  begin
    if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END] then
      DoAutoComplete;
  end;
  inherited;
end;

procedure TElAdvancedComboBox.DoAutoComplete;
var i : integer;
    S : String;
    ssl: Integer;
begin
  ssl := SelStart;
  S := Copy(Text, 1, ssl);
  if Length(S) > 0 then
  begin
    for i := 0 to Pred(Items.Count) do
    begin
      if Pos(S, Items[i]) = 1 then
      begin
        Text := Items[i];
        SendMessage(EditHandle, EM_SETSEL, Length(Text), ssl);
        Break;
      end;
    end;
  end;
end;

procedure TElAdvancedComboBox.WMGetDlgCode(var Msg : TMessage);
begin
  Msg.Result := DLGC_WANTCHARS or DLGC_WANTARROWS;
  if HandleDialogKeys then Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;


{$ifdef ELPACK_NO_ADV_EDITS}
constructor TElAdvancedMemo.Create(AOwner : TComponent);
begin
  inherited;
  Multiline := true;
  Width := 185;
  Height := 89;
  AutoSize := False;
  WordWrap := True;
end;

{$endif}

procedure TElAdvancedMemo.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    Params.Style := Params.Style and (not WS_BORDER);
    Params.ExStyle := Params.ExStyle and (not WS_EX_CLIENTEDGE);
  end;
end;

procedure TElAdvancedMemo.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if IsThemeApplied then
    Invalidate;
end;

{$ifdef ELPACK_UNICODE}
procedure TElAdvancedMemo.SetHint(Value: WideString);
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

end.


