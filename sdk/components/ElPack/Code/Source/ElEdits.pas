{$define DEBUG}
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Portions copyright (c) 2002, Alexander Hramov    }
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

  Tab fix. Some fixes for WMKeydown.    

07/22/2002

  Fixed vertical alignment of text (spoiled on 07/20/2002)
  Fixed typing of unicode text for special symbols 
  Fixed processing of Enter key in Unicode mode 

07/17/2002

  Fixed some problems with vertical alignment of single-line text

07/13/2001

  Added SetEditRect method and changed EditRect handling in descendant classes

06/25/2002

  Undo CRLF and Tab insert fixed. Selection fixed. Sets topline
  when scrollbars disabled. Reformat text when control resized.

06/12/2002

  MaxLength now works. Selection bug with Ctrl+Home fixed.
  Misc scrollbars fixed. Fixed bug when insert mutiline text
  with WordWrap is true. VK_DELETE event handler fixed
  (not worked when WordWrap is True)

06/10/2002

  Fixed default for Multiline. 
  Fixed height adjustment during form loading
  Fixed some memory leaks

06/08/2002

  Some internal fixes.

06/06/2002

  Add TElEditStrings.AddStrings, .Find and .Exchange, .Put,
  .GetObject (loopback), .PutObject (loopback) methods.
  Memory leaks closed.

05/25/2002

  Fixed setting position for different Aligment values.
  Add NotifyUserChangeOnly, add AdjustHeight method.

05/16/2002

  Fully rewritten code. Other principles of internal work.
  Work of editor now is faster, stronger. Multilevel undo.
  
04/19/2002

  Fixed assignment of multiline text via Assign.
  Added Scroll method

04/15/2002

  Setting SelText when SelLength = 0 causes insertion of the text.
  Pressing Backspace when SelLength > 0 causes deletion of selected text only now.

04/10/2002

  Alt- and Ctrl- modifiers are no more blocked, so it's possible to press
  Alt+<some key> to enter locale-specific characters and Euro symbol.

04/04/2002

  The text is centered now when Multiline is false and AlignBottom = true

03/26/2002

  Lowercase and Uppercase worked wrong (were mixed). Fixed.
  Clipboard Cut now shares Clipboard Copy technique.

  Clipboard Copy now correctly sets CF_TEXT data
  (there was PChar(SelText) instead of PChar(S)).

  Clipboard Copy now sets CF_TEXT data only under
  non-Windows NT/2000/XP platform because of
  Windows Clipboard autoconvertion feature.

  Clipboard Copy sets CF_UNICODETEXT data under both Unicode
  and non-Unicode modes because of localized text Clipboard
  Paste issues.

  Fixed Clipboard Paste under non-Unicode mode to use
  CF_UNICODETEXT-formatted data when possible
  because of localized text Clipboard Paste issues.

  Clipboard Paste now uses one Clipboard object
  and do not uses wrong Open/Close semantics.

  Clipboard Copy now uses one Clipboard object,
  uses exception-safe Open/Close semantics,
  and written in Paranoid style.

  TElCustomEdit now fills background with Color
  under XP Theming mode too.

03/22/2002

  In multiline mode, when BorderStyle = bsNone, junk was drawn around the control. Fixed. 

03/21/2002

  Fixed autosize issues when borders are turned off
  Fixed issues with entering numbers from numeric part of keyboard

03/20/2002

  Transparency issues fixed

03/18/2002

  Ctrl+BkSp and Ctrl+Delete delete words now 

03/15/2002

  Pressing Home when cursor is on position 0 removes selection 
  Now it's possible to set key in KeyPress to #0.

03/12/2002

  Autosize is now correctly set to true by default
  Combination of Ctrl and Alt does not prevent key from being processed.

03/06/2002

  Added unicode hint

02/26/2002

  Fixed scrollbars

02/23/2002

  Added CharCase property

02/18/2002

  Property UseCustomScrollBars added. Now you can use standard Windows or custom
  scrollbars.
  Support for mouse wheel added.

02/13/2002

  Fixed imageform handling

02/09/2002

  Fixed cursor size/position calculation for non-default fonts

02/01/2002

  Fixed some multiline problems

01/31/2002

  Fixed problems with Lines.Add, Lines.Insert, Lines.Clear
  Siginificantly improved speed of line addition.

  Now the control can handle even 20000 lines
  (although scrolling speed is terrible near the end of the text)

01/18/2002

  Fixed problems with scrollbar updating
  Fixed problems with IME Editors

01/12/2002

  Add AlignBottom public property. Used for vertical align text in single-line
  mode

01/06/2002

  Now the text is updated when Alignment, LeftMargin, RightMargin or TopMargin
  are changed 

01/02/2002

  Under Windows XP, now all input converted to Unicode

01/01/2002

  Fixed some problems with painting borders when focus is moved

11/28/2001

  LineBorderActiveColor and LineBorderInactiveColor properties added.
  Fixed the problem with mouse selection of the text

11/16/2001

  Fix small bug in StringToPassword

11/12/2001

  Add SelectAll method come fixes in WMSetFocusMethod. Added code for context
  menu support

11/08/2001
  
  Solved problem with sending wm_settext in design time

11/04/2001 
  
  Some fixes in PosFromChar, CharFromPos, WMCopy. Moved code which sets
  FLeftChar from MoveCaret to SetSelStart for normal horizontal scrolling

10/31/2001

  Select text with Shift + Left, Shift + Home and Ctrl + Shift + Left, 
  bug fixed, set ScrollBars position and range moved to SetScrollBarsInfo. 
  Emulate GetTextExtentExPointW for Win9x

10/30/2001

  PasswordChar assignment fixed

10/22/2001

  Some fix in unicode version

10/18/2001

  Add IME support and Unicode version of editor. Some optimization
  works.

10/15/2001

  Multiline editor completed.

10/09/2001

  Add ScrollBars, WordWrap properties to TElEdit class. Declare new class
  TElEditStrings for Multiline and WordWrap features.

*)

unit ElEdits; { TCustomElEdit component. }

{ EldoS Editor }

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  Clipbrd,
  ElTools,
  {$ifdef ELPACK_COMPLETE}
  ElImgFrm,
  {$endif}
  ElList,
  ElStack,
  ElTmSchema,
  ElUxTheme,
  ElVCLUtils,
  ElXPThemedControl,
  ExtCtrls,
  ElScrollBar,
  {$ifdef ELPACK_UNICODE}
  ElUnicodeStrings,
  Imm,
  {$endif}
  {$ifdef VCL_6_USED}
  Types,
  {$endif}
  ElStrUtils;

type
  EElEditorError = class(exception)
  end;

  TCustomElEdit = class;

{$ifdef MSWINDOWS}
{$ifdef ELPACK_UNICODE}
  TElFStrings = TElWideStrings;
  TElFStringList = TElWideStringList;
{$else}
  TElFStrings = TStrings;
  TElFStringList = TStringList;
{$endif}
{$else}
  TElFStrings = TStrings;
  TElFStringList = TStringList;
{$endif}

  TElActionType = (atInsert, atDelete, atLineBreak, atGroupBreak, atPaste,
                   atBackSpace, atDeleteSel, atInsertSel);

  TElAction = class(TPersistent)
  protected
    FAction: TElActionType;
    FStartPos, FEndPos: TPoint;
    FStr: TElFString;
  public
    procedure Assign(Source: TPersistent); override;
    property Action: TElActionType read FAction;
    property StartPos: TPoint read FStartPos;
    property EndPos: TPoint read FEndPos;
    property CString: TElFString read FStr;
  end;

  TElActionList = class(TPersistent)
  protected
    FAStack: TElStack;
    FMaxUndo: integer;
    FLockCount: integer;
    function GetCanUndo: boolean; virtual;
    procedure SetMaxUndo(const Value: integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock; 
    procedure AddAction(AAction: TElActionType; ASPos, AEPos: TPoint; AStr: TElFString);
    function PeekItem: TElAction;
    function PopItem: TElAction;
    procedure PushItem(Item: TElAction);
    property CanUndo: boolean read GetCanUndo;
    property MaxUndo: integer read FMaxUndo write SetMaxUndo default 10;
  end;

  {$ifdef ELPACK_UNICODE}
  TElParagraph = class(TElWideStringList)
  {$else}
  TElParagraph = class(TStringList)
  {$endif}
  protected
    FPCount: integer;
    {$ifdef ELPACK_UNICODE}
    procedure SetTextStr(const Value: WideString); override;
    function GetTextStr: WideString; override;
    function Get(Index: Integer): WideString; override;
    procedure Put(Index: Integer; const S: WideString); override;
    {$else}
    procedure SetTextStr(const Value: string); override;
    function GetTextStr: String; override;
    function Get(Index: Integer): String; override;
    procedure Put(Index: Integer; const S: String); override;
    {$endif}
  public
    {$ifdef ELPACK_UNICODE}
    property Text: WideString read GetTextStr write SetTextStr;
    {$else}
    property Text: String read GetTextStr write SetTextStr;
    {$endif}
  end;

{$warnings off}
  TElParagraphList = class(TElList)
  protected
    function Get(Index: Integer): TElParagraph;
    procedure Put(Index: Integer; const Value: TElParagraph);
    procedure Delete(Index: Integer);
  public
    procedure Clear; override;
    property Items[Index: Integer]: TElParagraph read Get write Put; default;
  end;

  {$ifdef ELPACK_UNICODE}
  TElEditStrings = class(TElWideStringList)
  {$else}
  TElEditStrings = class(TStringList)
  {$endif}
  private
    FElEdit : TCustomElEdit;
    RealStrings : TStringList;
    FParagraphs: TElParagraphList;
  protected
    FMaxLen : Integer;
    FIdxMaxLen : integer;
    FMaxStr : TElFString;
    FSaveStr : TElFString;
    FOnChange : TNotifyEvent;

    procedure Reformat;
    procedure Changed; override;

    {$ifdef ELPACK_UNICODE}
    function Get(Index: integer): WideString; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure SetTextStr(const Value: WideString); override;
    function GetTextStr: WideString; override;
    {$else}
    function Get(Index: integer): String; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure SetTextStr(const Value: String); override;
    function GetTextStr: String; override;
    {$endif}
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function GetObject(Index: Integer): TObject; override;
    function GetCount: integer; override;
    procedure ReformatParagraph(Index: integer);
    procedure ReCount(Index: integer);
    function GetParaString(Index: integer): TElFString;
    procedure SetParaString(Index: integer; const Value: TElFString);
    function GetParaCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    {$ifdef ELPACK_UNICODE}
    procedure AddStrings(Strings: TElWideStrings); override;
    function Add(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure InsertText(var ACaretX, ACaretY: integer; const S: WideString);
    function Find(const S: WideString; var Index: Integer): boolean; override;
    {$else}
    procedure AddStrings(Strings: TStrings); override;
    function Add(const S : String): Integer; override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure InsertText(var ACaretX, ACaretY: integer; const S: String);
    function Find(const S: String; var Index: Integer): boolean; override;
    {$endif}
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure IndexToParagraph(index: integer; var Paragraph, ParaIndex: integer);
    procedure CaretToParagraph(ACaretX, ACaretY: integer; var Paragraph, ParaOffs: integer);
    procedure CaretFromParagraph(Paragraph, ParaOffs: integer; var ACaretX, ACaretY: integer);
    function GetReText: TElFString;
    function CutString(var S: TElFString; Len: integer; var RealStr: boolean): TElFString;
    property ParagraphStrings[Index: integer]: TElFString read GetParaString write SetParaString;
    property ParagraphCount: integer read GetParaCount;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TElEditCharCase = (eecNormal, eecUpperCase, eecLowerCase);
  TElEditScrollDir = (esdLineUp, esdLineDown, esdPageUp, esdPageDown);

  TCustomElEdit = class(TElXPThemedControl)
  protected
    { Protected declarations }
    FModified : Boolean;
    FEditRect : TRect;
    FLeftMargin : Integer;
    FTopMargin : integer;
    FRightMargin : Integer;
    FMouseClick : boolean;
    FBorderStyle : TBorderStyle;
    FAutoSelect : Boolean;
    FHideSelection : Boolean;
    FAlignment : TAlignment;
    FReadOnly : Boolean;
    FWantTabs : Boolean;
    FPasswordChar : TElFChar;
    FMaxLength : Integer;
    FSelected: boolean;
    FSelLength : Integer;
    FSelStartX: integer;
    FSelStartY: integer;
    FSelFirstX: integer;
    FSelFirstY: integer;
    FSelLastX: integer;
    FSelLastY: integer;
    FMultiline: boolean;
    FTransparent : Boolean;
    FTabString : TElFString;
    FTabSpaces : integer;
    FHasCaret : boolean;
    FCaretX : integer;
    FCaretY : integer;
    { multi level undo & redo support }
    FUndo: TElActionList;
    FRedo: TElActionList;
    { end }
    FModifyCount : integer;
    FLineHeight : integer;
    FLeftChar : integer;
    FCharsInView : integer;
    FSelecting : boolean;
    FOnChange : TNotifyEvent;
    FOnSelectionChange : TNotifyEvent;
    FOnResize : TNotifyEvent;
    ForceLeftAlignment : boolean;
    FBackground: TBitmap;
    FUseBackground: Boolean;
    {$ifdef ELPACK_COMPLETE}
    FImgForm: TElImageForm;
    FImgFormChLink: TImgFormChangeLink;
    {$endif}
    FBorderSides: TElBorderSides;
    FActiveBorderType: TElFlatBorderType;
    FFlat: Boolean;
    FInactiveBorderType: TElFlatBorderType;
    FHandleDialogKeys: Boolean;
    FMouseOver: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;

    // Список строк для Multiline
    FElEditList: TElEditStrings;
    // Верхняя видимая строка
    FTopLine: integer;
    FWordWrap: boolean;
    FScrollBars: TScrollStyle;
    FLinesInRect: integer;
    FCharset: Integer;
    FKeybLayout: HKL;

    scbVert, scbHorz: TElScrollBar;
    FFlatFocusedScrollBars: Boolean;
    FUseCustomScrollBars : Boolean;
    FVertScrollBarStyles : TElScrollBarStyles;
    FHorzScrollBarStyles : TElScrollBarStyles;

    FAlienFocus : boolean;
    FAlignBottom : boolean;
    {$ifdef ELPACK_UNICODE}
    FKeys : WideString;
    FKeyDown : boolean;
    FHint: WideString;
    {$endif}
    FRTLContent: Boolean;
    FAutoSize: Boolean;
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    FNotifyUserChangeOnly: Boolean;
    FCharCase: TElEditCharCase;
    FChangeDisabledText: Boolean;
    FEnd: boolean;

    procedure CMCtl3DChanged(var Msg : TMessage); message CM_CTL3DCHANGED;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMKeyDown(var Msg : TWMKeyDown); message WM_KEYDOWN;
    procedure WMCut(var Msg : TMessage); message WM_CUT;
    procedure WMCopy(var Msg : TMessage); message WM_COPY;
    procedure WMPaste(var Msg : TMessage); message WM_PASTE;
    procedure WMClear(var Msg : TMessage); message WM_CLEAR;
    procedure WMSetText(var Msg : TMessage); message WM_SETTEXT;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMEnable(var Msg : TMessage); message WM_ENABLE;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMVScroll(var Msg : TWMScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg : TWMScroll); message WM_HSCROLL;
    procedure WMInputLangChange(var Msg: TMessage); message WM_INPUTLANGCHANGE;
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;

    {$ifdef ELPACK_UNICODE}
    procedure WMImeStartComposition(var Message : TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMImeComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    {$endif}

    procedure OnHScroll(Sender: TObject; ScrollCode: TElScrollCode;
                        var ScrollPos: Integer; var DoChange : boolean);
    procedure OnVScroll(Sender: TObject; ScrollCode: TElScrollCode;
                        var ScrollPos: Integer; var DoChange : boolean);
    procedure SBChanged(Sender: TObject);

    procedure SetReadOnly(newValue : Boolean);
    procedure SetAlignment(newValue : TAlignment);
    procedure SetLeftMargin(newValue : Integer);
    procedure SetRightMargin(newValue : Integer);
    procedure SetBorderStyle(newValue : TBorderStyle);
    procedure SetHideSelection(newValue : Boolean);
    function  GetPasswordChar : TElFString;
    procedure SetPasswordChar(newValue : TElFString);
    procedure SetTransparent(newValue : Boolean);
    procedure DoSetEditRect(newValue : TRect); 
    procedure SetTabSpaces(newValue : integer);
    procedure SetModified(newValue : Boolean);
    procedure SetText(newValue: TElFString);
    function GetSelectedText : TElFString;
    procedure SetBackground(const Value: TBitmap);
    procedure SetUseBackground(const Value: boolean);
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure SetBorderSides(Value: TElBorderSides);
    {$ifdef ELPACK_COMPLETE}
    procedure ImageFormChange(Sender : TObject);
    {$endif}
    procedure DrawBackground(DC: HDC; R: TRect);
    procedure DrawFlatBorder;
    procedure DrawParentControl(DC: HDC);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetTopLine(const Value: Integer);
    procedure UpdateHeight;
    {$ifndef CLX_USED}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ifdef ELPACK_COMPLETE}
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
        WM_WINDOWPOSCHANGED;
    {$endif}
    procedure BackgroundChanged(Sender: TObject);
    procedure SetFlatFocusedScrollBars(const Value: boolean);
    procedure AdjustHeight;
    {$endif}

    {$ifdef CLX_USED}
    procedure FontChanged; override;
    {$endif}
    procedure SetVertScrollBarStyles(newValue : TElScrollBarStyles);
    procedure SetHorzScrollBarStyles(newValue : TElScrollBarStyles);
    procedure SetUseCustomScrollBars(newValue : Boolean);

    procedure SetMaxLength(newValue : Integer); virtual;
    function  GetSelStart: integer; virtual;
    procedure SetSelStart(newValue : Integer); virtual;
    procedure SetSelLength(newValue : Integer); virtual;
    procedure SetSelText(const newValue : TElFString); virtual;
    function  StringToPassword(AString : TElFString) : TElFString;
    function  ExpandTabbedString(Text : TElFString) : TElFString;
    function  TextSize(ALine : TElFString) : TSize;
    procedure SetScrollBarsInfo;
    procedure MoveCaret(CharNum : integer);
    procedure MakeCaret;
    procedure RepaintText(Rect : TRect);
    procedure DrawTabbedText(DC : HDC; X, Y : integer; AText : TElFString; var Size : TSize);
    function  ConvertBreaksFormat(Text : TElFString) : TElFString;
    function  CharsFitRight(AWidth : integer; FText : TElFString;
                            StartPos : integer) : integer;
    function  CharsFitLeft(AWidth : integer; FText: TElFString; StartPos : integer) : integer;

    procedure Change; virtual;
    procedure TriggerSelectionChangeEvent; virtual;
    procedure TriggerResizeEvent; virtual;
    procedure KeyPress(var Key : Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    // procedure DrawFrame;
    procedure PaintText(Canvas : TCanvas);
    procedure CreateParams(var Params : TCreateParams); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    {$ifdef ELPACK_COMPLETE}
    procedure SetImageForm(newValue : TElImageForm); virtual;
    {$endif}
    procedure SetFlat(const Value: boolean); virtual;
    {$ifdef ELPACK_COMPLETE}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$endif}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetThemedClassName: WideString; override;
    function GetLinesCount: Integer;
    procedure SetWordWrap(Value: boolean);
    procedure SetLeftChar(Value: integer);
    procedure SetAutoSize(Value: Boolean);{$ifdef VCL_6_USED}override;{$endif}
    procedure CreateWnd; override;
    procedure WMGetText(var Message: TMessage); message WM_GETTEXT;
    procedure SetLines(Value: TElFStrings);
    function GetLines: TElFStrings;
    procedure SetTopMargin(Value: Integer);
    procedure SetAlignBottom(Value: boolean);
    // procedure UpdateFrame;
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure Loaded; override;
    procedure SetLineBorderActiveColor(Value: TColor); virtual;
    procedure SetLineBorderInactiveColor(Value: TColor); virtual;
    procedure EMSetRect(var Message: TMessage); message EM_SETRECT;
    procedure EMSetRectNP(var Message: TMessage); message EM_SETRECTNP;
    procedure SetActiveBorderType(const Value: TElFlatBorderType); virtual;
    procedure SetInactiveBorderType(const Value: TElFlatBorderType); virtual;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetUseXPThemes(const Value: Boolean); override;
    procedure SetCharCase(Value: TElEditCharCase);
    procedure SetSelection(SelX, SelY: integer);
    procedure UnSelect;
    procedure DeleteSelection;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;


    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
    procedure SetBottomAlign;
    function GetText: TElFString;
    function ConvertToCRLF(Str : TElFString): TElFString;
    procedure SetCaretX(const value: integer);
    procedure SetCaretY(const value: integer);
    function GetCaretXY: TPoint;
    procedure SetCaretXY(const value: TPoint);
    procedure CorrectLeftChar;
    procedure SetCaretPosition(const X, Y: integer);
    procedure SetMultiline(const Value: boolean);
    procedure SetMaxLevel(const Value: integer);
    function GetMaxLevel: integer;

    procedure EMGetSel(var Message: TMessage); message EM_GETSEL;
    procedure EMGetLine(var Message: TMessage); message EM_GETLINE;
    procedure EMGetLineCount(var Message: TMessage); message EM_GETLINECOUNT;
    procedure EMLineIndex(var Message: TMessage); message EM_LINEINDEX;
    procedure EMSetSel(var Message: TMessage); message EM_SETSEL;
    procedure EMReplaceSel(var Message: TMessage); message EM_REPLACESEL;
    procedure EMGetFirstVisibleLine(var Message: TMessage); message EM_GETFIRSTVISIBLELINE;
    procedure EMScroll(var Message: TMessage); message EM_SCROLL;
    procedure EMLineScroll(var Message: TMessage); message EM_LINESCROLL;
    procedure EMScrollCaret(var Message: TMessage); message EM_SCROLLCARET;
    procedure EMLineFromChar(var Message: TMessage); message EM_LINEFROMCHAR;
    procedure EMPosFromChar(var Message: TMessage); message EM_POSFROMCHAR;
    procedure EMCanUndo(var Message: TMessage); message EM_CANUNDO;
    procedure EMUndo(var Message: TMessage); message EM_UNDO;
    procedure SetChangeDisabledText(Value: Boolean);
    procedure SetEditRect(Value : TRect); virtual;

    property RTLContent: Boolean read FRTLContent write FRTLContent;
    property Background: TBitmap read FBackground write SetBackground;
    property UseBackground: Boolean read FUseBackground write SetUseBackground
        default False;
    property PasswordChar : TElFString read GetPasswordChar write SetPasswordChar; { Published }
    property MaxLength : Integer read FMaxLength write SetMaxLength default 0; { Published }
    property Transparent : Boolean read FTransparent write SetTransparent; { Published }

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange : TNotifyEvent read FOnSelectionChange write FOnSelectionChange;

    property ReadOnly : Boolean read FReadOnly write SetReadOnly default false; { Published }
    property WantTabs : Boolean read FWantTabs write FWantTabs default false; { Published }
    property Alignment : TAlignment read FAlignment write SetAlignment; { Published }
    property BorderStyle : TBorderStyle read FBorderStyle write SetBorderStyle; { Published }
    property AutoSelect : Boolean read FAutoSelect write FAutoSelect default false; { Published }
    property HideSelection : Boolean read FHideSelection write SetHideSelection default true; { Published }
    property OnResize : TNotifyEvent read FOnResize write FOnResize;
    property EditRect : TRect read FEditRect write DoSetEditRect; { Published }
    property TabSpaces : Integer read FTabSpaces write SetTabSpaces default 4;
    {$ifdef ELPACK_COMPLETE}
    property ImageForm: TElImageForm read FImgForm write SetImageForm;
    {$endif}
    property ActiveBorderType: TElFlatBorderType read FActiveBorderType write
        SetActiveBorderType default fbtSunken;
    property Flat: Boolean read FFlat write SetFlat default False;
    property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write
        SetInactiveBorderType default fbtSunkenOuter;


    property WordWrap: boolean read FWordWrap write SetWordWrap default false;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    property Lines: TElFStrings read GetLines write SetLines;
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
        SetLineBorderInactiveColor;
    property FlatFocusedScrollBars: Boolean read FFlatFocusedScrollBars write
        SetFlatFocusedScrollBars default False;
    property CharCase: TElEditCharCase read FCharCase write SetCharCase default
        eecNormal;
    property MaxUndoLevel : integer read GetMaxLevel write SetMaxLevel default 10;
    property Multiline : boolean read FMultiline write SetMultiline default false;
    property ChangeDisabledText: Boolean read FChangeDisabledText write 
        SetChangeDisabledText default false;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetNextWord(ACaret: TPoint) : TPoint;
    function GetPrevWord(ACaret: TPoint) : TPoint;

    function CaretFromChar(const CharNum: integer): TPoint;
    function CharFromCaret(const X, Y: integer): integer;
    function PosFromCaret(const X, Y: integer): TPoint; virtual;
    procedure CaretFromPos(APos: TPoint; var ACaretX, ACaretY: integer); virtual;

    procedure SelectAll;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure Scroll(ScrollDir : TElEditScrollDir);
    procedure ScrollCaret;
    function GetCanUndo: boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property SelStart : Integer read GetSelStart write SetSelStart; { Published }
    property SelLength : Integer read FSelLength write SetSelLength; { Published }
    property SelText : TElFString read GetSelectedText write SetSelText; { Published }
    property Modified : Boolean read FModified write SetModified default False; { Public }
    property SelectedText : TElFString read GetSelectedText;
    property MouseOver: Boolean read FMouseOver;
    property LinesCount: Integer read GetLinesCount;
    property TopLine: Integer read FTopLine write SetTopLine;
    property LeftChar: integer read FLeftChar write SetLeftChar;
    property LeftMargin : Integer read FLeftMargin write SetLeftMargin default 1; { Published }
    property RightMargin : Integer read FRightMargin write SetRightMargin default 2; { Published }

    property HandleDialogKeys: Boolean read FHandleDialogKeys write FHandleDialogKeys default false;
    property Text: TElFString read GetText write SetText;
    property TopMargin: Integer read FTopMargin write SetTopMargin default 1;
    property AlignBottom : boolean read FAlignBottom write SetAlignBottom default true;

    property CaretX: integer read FCaretX write SetCaretX;
    property CaretY: integer read FCaretY write SetCaretY;
    property CaretXY: TPoint read GetCaretXY write SetCaretXY;
    property CanUndo: boolean read GetCanUndo;
  published
    property VertScrollBarStyles : TElScrollBarStyles read FVertScrollBarStyles write SetVertScrollBarStyles;
    property HorzScrollBarStyles : TElScrollBarStyles read FHorzScrollBarStyles write SetHorzScrollBarStyles;
    property UseCustomScrollBars : Boolean read FUseCustomScrollBars write SetUseCustomScrollBars;  { Published }
    property NotifyUserChangeOnly: Boolean read FNotifyUserChangeOnly write FNotifyUserChangeOnly;

    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
    property TabStop default true;

    property UseXPThemes;
  end; { TCustomElEdit }

  TElEdit = class (TCustomElEdit)
  published
    property AutoSize;
    property Alignment;
    property AlignBottom;
    property Background;
    property BorderSides;
    property CharCase;
    property ChangeDisabledText;
    property UseBackground;
    property RTLContent;
    property PasswordChar;
    property MaxLength;
    property Transparent;
    property FlatFocusedScrollBars;
    property ReadOnly;
    property WantTabs;
    property LeftMargin;
    property RightMargin;
    property TopMargin; 
    property BorderStyle;
    property AutoSelect;
    property HandleDialogKeys;
    property HideSelection;
    property TabSpaces;
    property Lines stored false;

    property Text;

    {$ifdef ELPACK_COMPLETE}
    property ImageForm;
    {$endif}
    property ActiveBorderType;
    property Flat;
    property InactiveBorderType;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;
    property MaxUndoLevel;

    property WordWrap;
    property Multiline;
    property ScrollBars;

    property VertScrollBarStyles;
    property HorzScrollBarStyles;
    property UseCustomScrollBars;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnChange;
    property OnSelectionChange;

    property Align;

    {$IFDEF VCL_4_USED}
    property Anchors;
    {$ENDIF}
    property Color;
    {$IFDEF VCL_4_USED}
    property Constraints;
    {$ENDIF}
    property Ctl3D;
    property DragCursor;
    {$IFDEF VCL_4_USED}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

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

var
  RepaintAll : boolean = true;
  FlagEdit : boolean;

const
  ElFSpace = {$ifdef ELPACK_UNICODE}WideChar(#32){$else}#32{$endif};
  ElFTab = {$ifdef ELPACK_UNICODE}WideChar(#9){$else}#9{$endif};
  ElFCR = {$ifdef ELPACK_UNICODE}WideChar(#13){$else}#13{$endif};
  ElFLF = {$ifdef ELPACK_UNICODE}WideChar(#10){$else}#10{$endif};
  {$ifdef LINUX}
  ElFCRLF = {$ifdef ELPACK_UNICODE}WideChar(#13){$else}#13{$endif};
  {$else}
  ElFCRLF = {$ifdef ELPACK_UNICODE}WideString(#13#10){$else}#13#10{$endif};
  {$endif}

  ID_UNDO = 772;
  ID_CUT = 768;
  ID_COPY = 769;
  ID_PASTE = 770;
  ID_DELETE = 771;

procedure Register;

implementation

{$ifdef ELPACK_UNICODE}
type
  ToUnicodeProc = function(wVirtKey, wScanCode: UINT; const KeyState: TKeyboardState;
                            var pwszBuff; cchBuff: Integer; wFlags: UINT): Integer; stdcall;
var
  ToUnicode : ToUnicodeProc;
{$endif}

{ Вспомогательные функции для работы с RightToLeft языками }
function Revert(S: TElFString):TElFString;
var
  i: integer;
  {$ifdef ELPACK_UNICODE}
  S1 : WideString;
  {$endif}
begin
  i := LastPos(ElFCR, S);
  while i > 0 do
  begin
    Result := Result + Copy(S, i + 1, Length(S)) + ElFCR;
    {$ifdef ELPACK_UNICODE}
    S1 := S;
    WideDelete(S1, i, Length(S1));
    S := S1;
    {$else}
    Delete(S, i, Length(S));
    {$endif}
    i := LastPos(ElFCR, S);
  end;
  Result := Result + S;
end;

// move to ElVCLUtils.pas
function ElGetTextExtentExPoint(DC: HDC; lpszStr: PElFChar; cchString: integer;
                                 nMaxExtent: integer; var lpnFit: integer;
                                 alpDX: PInteger; var lpSize: TSize): BOOL;
{$ifdef ELPACK_UNICODE}
var
  i : integer;
{$endif}
begin
  Result := false;
  if (Length(lpszStr) = 0) then 
  begin
    lpnFit := 0;
    exit;
  end;
  {$ifdef ELPACK_UNICODE}
  if IsWinNTUp then
    {$ifndef VCL_4_USED}
    Result := GetTextExtentExPointW(DC, PWideChar(lpszStr), cchString, nMaxExtent, lpnFit, alpDX^, lpSize)
    {$else}
    Result := GetTextExtentExPointW(DC, PWideChar(lpszStr), cchString, nMaxExtent, @lpnFit, alpDX, lpSize)
    {$endif}
  else
  begin
    i := 0;
    GetTextExtentPoint32W(DC, PWideChar(lpszStr), i + 1, lpSize);
    while lpSize.cx <= nMaxExtent do
    begin
      if Assigned(alpDX) then
      begin
        Move(lpSize.cx, alpDX^, SizeOf(Integer));
        Inc(PChar(alpDX), SizeOf(Integer));
      end;
      inc(i);
      if i > Length(lpszStr) then
      begin
        lpnFit := Length(lpszStr);;
        exit;
      end;
      GetTextExtentPoint32W(DC, PWideChar(lpszStr), i + 1, lpSize);
    end;
    lpnFit := i;
  end;
  {$else}
    {$ifndef VCL_4_USED}
    Result := GetTextExtentExPointA(DC, PChar(lpszStr), cchString, nMaxExtent, lpnFit, alpDX^, lpSize);
    {$else}
    Result := GetTextExtentExPointA(DC, PChar(lpszStr), cchString, nMaxExtent, @lpnFit, alpDX, lpSize);
    {$endif}
  {$endif}
end;

function  TCustomElEdit.GetPasswordChar : TElFString;
begin
  if FPasswordChar = #0 then
    result := ''
  else
    result := FPasswordChar;
end;

procedure TCustomElEdit.SetPasswordChar(newValue : TElFString);
{ Sets data member FPasswordChar to newValue. }
begin
  if (PasswordChar <> newValue) then
  begin
    if Length(newValue) > 0 then
      FPasswordChar := newValue[1]
    else
      FPasswordChar := #0;
    Invalidate;
  end; { if }
end; { SetPasswordChar }

procedure TCustomElEdit.SetMaxLength(newValue : Integer);
{ Sets data member FMaxLength to newValue. }
begin
  if (FMaxLength <> newValue) then
  begin
    FMaxLength := newValue;
  end; { if }
end; { SetMaxLength }

function TCustomElEdit.GetSelStart: integer;
begin
  Result := CharFromCaret(CaretX, CaretY);
end;

procedure TCustomElEdit.SetSelStart(newValue : Integer);
{ Sets data member FSelStart to newValue. }
var
  FPos: TPoint;
begin
  FPos := CaretFromChar(newValue);
  UnSelect;
  CaretY := FPos.Y;
  CaretX := FPos.X;
  FSelStartX := CaretX;
  FSelStartY := CaretY;
end; { SetSelStart }

procedure TCustomElEdit.SetSelText(const newValue : TElFString);
var
  fx, fy: integer;
begin
  if FSelected then
    DeleteSelection;
  fx := CaretX;
  fy := CaretY;
  FElEditList.InsertText(fx, fy, newValue);
  SetSelection(fx, fy);
  CaretY := fy;
  CaretX := fx;
end;

procedure TCustomElEdit.SetSelLength(newValue : Integer);
{ Sets data member FSelLength to newValue. }
var
  FPos: TPoint;
begin
  FPos := CaretFromChar(SelStart + newValue);
  SetSelection(FPos.X, FPos.Y);
  CaretY := FPos.Y;
  CaretX := FPos.X;
end; { SetSelLength }

procedure TCustomElEdit.SetTransparent(newValue : Boolean);
{ Sets data member FTransparent to newValue. }
begin
  if (FTransparent <> newValue) then
  begin
    FTransparent := newValue;
    if HandleAllocated then RecreateWnd;
  end; { if }
end; { SetTransparent }

procedure TCustomElEdit.WMKeyDown(var Msg : TWMKeyDown); { private }
{$ifdef ELPACK_UNICODE}
var KeyState: TKeyboardState;
    s : WideString;
    Len : Integer;
    {.............................................................}
    function IsSpecialLanguage: Boolean;
    var Language: Integer;
    begin
      Language := (GetKeyboardLayout(0) and $FFFF);
      Result := (Language = $0439) {or  // Hindi
                (Language = $0404) or  // Chinese (Taiwan)
                (Language = $0804) or  //Chinese (PRC)
                (Language = $0c04) or  //Chinese (Hong Kong)
                (Language = $1004)};    //Chinese (Singapore)

    end;
    {.............................................................}
{$endif}
begin
  {$ifdef ELPACK_UNICODE}
  Len := 0;
  if IsWinNTUp {and IsSpecialLanguage } then
  begin
    if Msg.CharCode in [VK_NUMPAD0 .. VK_NUMPAD9] then
      Msg.CharCode := Msg.CharCode - VK_NUMPAD0 + ord('0');
    // Manual translation of key codes to Unicode
    // if Msg.CharCode in [ord('A')..ord('Z'),ord('0')..ord('9'),187..192,220] then
   if not (Msg.CharCode in [8, 9, 13]) then 
    begin
      SetLength(s, 5);
      if GetKeyboardState(KeyState) and ((KeyState[VK_CONTROL] and $80)=0) then
      begin
        Len := ToUnicode(Msg.CharCode, Msg.KeyData, KeyState, PWideChar(s)^, 5, 0);
        if Len > 0 then
        begin
          SetLength(s, Len);
          FKeys := s;
          FKeyDown := true;
        end;
      end;
    end;
  end;
  if Len<=0 then
    inherited
  else
  begin
    Msg.CharCode := 0;
    Msg.Result   := 0;
  end;
  {$else}
  inherited;
  {$endif}
end;

procedure TCustomElEdit.KeyDown(var Key: Word; Shift: TShiftState); { private }
const
  SpecKeys = [VK_DELETE, VK_BACK, VK_RETURN, VK_TAB];
  ROSpecKeys = [];
  SRArrowKeys = [VK_LEFT, VK_RIGHT, VK_HOME, VK_END];
  MRArrowKeys = [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT];
  ArrowKeys = SRArrowKeys + MRArrowKeys;
var
  {$ifdef ELPACK_UNICODE}
  S       : WideString;
  {$else}
  S       : String;
  {$endif}
  FX, FY: integer;
  P, PI: integer;
  FStr: TElFString;
  FPos: TPoint;
  FTStr: TElFString;

    procedure AcceptText;
    begin
      if (FMaxLength = 0) or (Length(Text) <= FMaxLength) then
      begin
        Modified := true;
        InvalidateRect(Handle, @FEditRect, false);
        Change;
        TriggerSelectionChangeEvent;
        SetScrollBarsInfo;
      end
      else
        InvalidateRect(Handle, @FEditRect, false);
    end;

begin
  inherited KeyDown(Key, Shift);
  if FSelecting then exit;
  if ((not FMultiline) and ((Key in MRArrowKeys) or (Key = VK_RETURN)))  then
  begin
    case Key of
      VK_DOWN : Key := VK_RIGHT;
      VK_UP : Key := VK_LEFT;
      VK_PRIOR : Key := VK_HOME;
      VK_NEXT : Key := VK_END;
      VK_RETURN : Key := 0;
    end;
  end;

  if RTLContent then
  begin
    case Key of
    VK_LEFT: Key := VK_RIGHT;
    VK_RIGHT: Key := VK_LEFT;
    VK_HOME: Key := VK_END;
    VK_END: Key := VK_HOME;
    end;
  end;
  if ((Shift = [ssCtrl]) or (Shift = [ssShift])) and (Key = VK_RETURN) then
    Shift := [];
  if Shift = [] then
  begin
    if FReadOnly and (not (Key in ArrowKeys)) then exit;
    if ((not FReadOnly) and (Key in SpecKeys)) or
      ((Key in ROSpecKeys)) then
    begin
      case Key of
        VK_BACK :
          begin
            if FSelected then
             DeleteSelection
            else
            begin
              FElEditList.CaretToParagraph(CaretX, CaretY, P, PI);
              FPos := CaretXY;
              if CaretX = 0 then
              begin
                if (PI = 0) and (P > 0) then
                begin
                  S := FElEditList.FParagraphs.Items[P].Text;
                  CaretY := CaretY - 1;
                  CaretX := Length(FElEditList.FParagraphs.Items[P - 1].Strings[FElEditList.FParagraphs.Items[P - 1].Count - 1]);
                  FUndo.AddAction(atBackSpace, FPos, CaretXY, ElFCRLF);
                  FElEditList.FParagraphs.Items[P - 1].Text := FElEditList.FParagraphs.Items[P - 1].Text + S;
                  FElEditList.FParagraphs.Delete(P);
                  FElEditList.ReformatParagraph(P - 1);
                  FElEditList.ReCount(P - 1);
                end
                else
                begin
                  CaretX := CaretX - 1;
                  {$ifdef ELPACK_UNICODE}
                  S := FElEditList.ParagraphStrings[CaretY];
                  FTStr := WideCopy(S, CaretX, 1);
                  WideDelete(S, CaretX, 1);
                  FElEditList.ParagraphStrings[CaretY] := S;
                  {$else}
                  S := FElEditList.ParagraphStrings[CaretY];
                  FTStr := Copy(S, CaretX, 1);
                  Delete(S, CaretX, 1);
                  FElEditList.ParagraphStrings[CaretY] := S;
                  {$endif}
                  if PI > 0 then
                  begin
                    FElEditList.CaretFromParagraph(P, PI - 1, FX, FY);
                    CaretY := FY;
                    CaretX := FX;
                    FSelStartX := CaretX;
                    FSelStartY := CaretY;
                  end
                  else
                  if (P = 0) and (Length(S) = 0) then
                    FElEditList.FParagraphs.Delete(0);
                  FUndo.AddAction(atBackSpace, FPos, CaretXY, FTStr);
                end;
              end
              else
              begin
                {$ifdef ELPACK_UNICODE}
                S := FElEditList.ParagraphStrings[CaretY];
                FTStr := WideCopy(S, CaretX, 1);
                WideDelete(S, CaretX, 1);
                FElEditList.ParagraphStrings[CaretY] := S;
                {$else}
                S := FElEditList.ParagraphStrings[CaretY];
                FTStr := Copy(S, CaretX, 1);
                Delete(S, CaretX, 1);
                FElEditList.ParagraphStrings[CaretY] := S;
                {$endif}
                FElEditList.CaretFromParagraph(P, PI - 1, FX, FY);
                CaretY := FY;
                CaretX := FX;
                FUndo.AddAction(atBackSpace, FPos, CaretXY, FTStr);
                if FElEditList.ParagraphCount <= FLinesInRect then
                  TopLine := 0;
              end;
            end;
          end;
        VK_DELETE :
          begin
            if FSelected then
             DeleteSelection
            else
            begin
              FElEditList.CaretToParagraph(CaretX, CaretY, P, PI);
              if PI < Length(FElEditList.{Paragraph}Strings[P]) then
              begin
                {$ifdef ELPACK_UNICODE}
                S := FElEditList.Strings[P];
                FUndo.AddAction(atDelete, CaretXY, CaretXY, WideCopy(S, CaretX + 1, 1));
                WideDelete(S, PI + 1, 1);
                {$else}
                S := FElEditList.Strings[P];
                FUndo.AddAction(atDelete, CaretXY, CaretXY, System.Copy(S, CaretX + 1, 1));
                Delete(S, P + 1, 1);
                FElEditList.Strings[P] := S;
                {$endif}
                FElEditList.Strings[P] := S;
                FElEditList.ReformatParagraph(P);
                FElEditList.ReCount(P);
              end
              else
              begin
                if (P + 1) < FElEditList.FParagraphs.Count then
                begin
                  FUndo.AddAction(atDelete, CaretXY, CaretXY, ElFCRLF);
                  S := FElEditList.FParagraphs.Items[P + 1].Text;
                  FElEditList.FParagraphs.Items[P].Text := FElEditList.FParagraphs.Items[P].Text + S;
                  FElEditList.FParagraphs.Delete(P + 1);
                  FElEditList.ReformatParagraph(P);
                  FElEditList.ReCount(P);
                end;
              end;
              FElEditList.CaretFromParagraph(P, PI, FX, FY);
              CaretY := FY;
              CaretX := FX;
              FSelStartX := CaretX;
              FSelStartY := CaretY;
            end;
            if FElEditList.ParagraphCount <= FLinesInRect then
              TopLine := 0;
          end
      else
        begin
          if (Key = VK_RETURN) and (((Length(Text) < (FMaxLength - 1)) and (FMaxLength > 0)) or (FMaxLength = 0)) then
          begin
            if FSelected then DeleteSelection;
            FElEditList.CaretToParagraph(CaretX, CaretY, P, PI);
            FX := CaretX;
            FY := CaretY;
            if (PI > 0) then
            begin
              FElEditList.FParagraphs.Insert(P + 1, TElParagraph.Create);

              FStr := FElEditList.FParagraphs.Items[P].Text;
              {$ifdef ELPACK_UNICODE}
              FStr := WideCopy(FStr, PI + 1, Length(FStr) - PI);
              {$else}
              FStr := Copy(FStr, PI + 1, Length(FStr) - PI);
              {$endif}

              S := FElEditList.FParagraphs.Items[P].Text;
              {$ifdef ELPACK_UNICODE}
              WideDelete(S, PI + 1, Length(S) - PI);
              {$else}
              Delete(S, PI + 1, Length(S) - PI);
              {$endif}
              FElEditList.FParagraphs.Items[P].Text := S;

              FElEditList.ReformatParagraph(P);
              if CaretX = 0 then
              begin
                FElEditList.FParagraphs.Items[P + 1].Add('');
                FElEditList.FParagraphs.Insert(P + 2, TElParagraph.Create);
                FElEditList.FParagraphs.Items[P + 2].Add(FStr);
                FElEditList.ReformatParagraph(P + 2);
                FUndo.AddAction(atLineBreak, Point((P + 1), 1), Point(0, 0), '');
              end
              else
              begin
                FElEditList.FParagraphs.Items[P + 1].Add(FStr);
                FElEditList.ReformatParagraph(P + 1);
              end;


              FElEditList.ReCount(P);
              CaretY := CaretY + 1;
              CaretX := 0;
              FSelStartX := CaretX;
              FSelStartY := CaretY;
            end;
            if PI = 0 then
            begin
              if FElEditList.FParagraphs.Count = 0 then
                FElEditList.FParagraphs.Items[0].Add('');
              FElEditList.FParagraphs.Insert(P, TElParagraph.Create);
              FElEditList.FParagraphs.Items[P].Add('');
              FElEditList.ReCount(P);
              CaretY := CaretY + 1;
              FSelStartX := CaretX;
              FSelStartY := CaretY;
            end;
            FUndo.AddAction(atInsert, Point(FX, FY), CaretXY, '');
          end
          else
          if (Key = VK_TAB) and (((Length(Text) < FMaxLength) and (FMaxLength > 0)) or  (FMaxLength = 0)) then
            begin
              if FSelected then DeleteSelection;
              FElEditList.CaretToParagraph(CaretX, CaretY, P, PI);
              S := FElEditList.ParagraphStrings[CaretY];
              {$ifdef ELPACK_UNICODE}
              WideInsert(ElFTab, S, CaretX + 1);
              {$else}
              Insert(ElFTab, S, CaretX + 1);
              {$endif}
              FElEditList.ParagraphStrings[CaretY] := S;
              FElEditList.CaretFromParagraph(P, PI + 1, FX, FY);
{              FElEditList.CaretFromParagraph(P, PI, FX, FY);

              FX := CaretX;
              FY := CaretY;
}
              CaretY := FY;
              CaretX := FX;
              FSelStartX := CaretX;
              FSelStartY := CaretY;

              FUndo.AddAction(atInsert, Point(FX, FY), CaretXY, FTabString);
            end;
        end;
      end;
      AcceptText;
    end
    else
    if Key in ArrowKeys then
    begin
      case Key of
        VK_LEFT :
          begin
            UnSelect;
            CaretX := CaretX - 1;
            FSelStartX := FCaretX;
            FSelStartY := FCaretY;
            TriggerSelectionChangeEvent;
          end;
        VK_RIGHT :
          begin
            UnSelect;
            CaretX := CaretX + 1;
            FSelStartX := FCaretX;
            FSelStartY := FCaretY;
            TriggerSelectionChangeEvent;
          end;
        VK_HOME :
          begin
            UnSelect;
            CaretX := 0;
            FSelStartX := FCaretX;
            FSelStartY := FCaretY;
            TriggerSelectionChangeEvent;
          end;
        VK_END :
          begin
            UnSelect;
            FEnd := true;
            CaretX := Length(FElEditList.ParagraphStrings[CaretY]);
            FEnd := false;
            FSelStartY := FCaretY;
            FSelStartX := FCaretX;
            TriggerSelectionChangeEvent;
          end;
        VK_UP :
          begin
            begin
              UnSelect;
              CaretY := CaretY - 1;
              FSelStartX := FCaretX;
              FSelStartY := FCaretY;
              TriggerSelectionChangeEvent;
            end;
          end;
        VK_DOWN :
          begin
            begin
              UnSelect;
              CaretY := CaretY + 1;
              FSelStartX := FCaretX;
              FSelStartY := FCaretY;
              TriggerSelectionChangeEvent;
            end;
          end;
        VK_NEXT :
          begin
            begin
              UnSelect;
              CaretY := CaretY + FLinesInRect;
              FSelStartX := FCaretX;
              FSelStartY := FCaretY;
              TriggerSelectionChangeEvent;
            end;
          end;
         VK_PRIOR :
           begin
            begin
              UnSelect;
              CaretY := CaretY - FLinesInRect;
              FSelStartX := FCaretX;
              FSelStartY := FCaretY;
              TriggerSelectionChangeEvent;
            end;
           end;
      end; // case
    end;
  end
  else
  if (Shift = [ssShift]) or (Shift = [ssCtrl, ssShift]) then
  begin
    if Shift = [ssShift] then
    begin
      if (Key = VK_DELETE) then
      begin
        SendMessage(Handle, WM_CUT, 0, 0);
        if NotifyUserChangeOnly then
          Change;
        Key := 0;
      end;
      if (Key = VK_INSERT) then
      begin
        SendMessage(Handle, WM_PASTE, 0, 0);
        if NotifyUserChangeOnly then
          Change;
        Key := 0;
      end;
    end;
    if Key in ArrowKeys then
    begin
      case Key of
        VK_LEFT :
          begin
            if ssCtrl in Shift then
              CaretXY := GetPrevWord(Point(CaretX - 1, CaretY))
            else
              CaretX := CaretX - 1;
            SetSelection(CaretX, CaretY);
            RepaintText(FEditRect);
          end; // VK_LEFT
        VK_RIGHT :
          begin
            if ssCtrl in Shift then
              CaretXY := GetNextWord(CaretXY)
            else
              CaretX := CaretX + 1;
            SetSelection(CaretX, CaretY);
            RepaintText(FEditRect);
          end;
        VK_HOME :
          begin
            if ssCtrl in Shift then
            begin
              CaretY := 0;
              CaretX := 0;
            end
            else
              CaretX := 0;
            SetSelection(CaretX, CaretY);
            RepaintText(FEditRect);
          end;
        VK_END :
          begin
            if ssCtrl in Shift then
            begin
              CaretY := FElEditList.ParagraphCount;
              CaretX := Length(FElEditList.ParagraphStrings[FElEditList.ParagraphCount - 1]);
            end
            else
            begin
              FEnd := true;
              CaretX := Length(FElEditList.ParagraphStrings[CaretY]);
              FEnd := false;
            end;
            SetSelection(CaretX, CaretY);
            RepaintText(FEditRect);
          end;
        VK_UP :
          begin
            CaretY := CaretY - 1;
            SetSelection(CaretX, CaretY);
            RepaintText(FEditRect);
          end; // VK_UP + Shift
        VK_DOWN :
          begin
            CaretY := CaretY + 1;
            SetSelection(CaretX, CaretY);
            RepaintText(FEditRect);
          end;
        VK_NEXT :
          begin
            CaretY := CaretY + FLinesInRect;
            SetSelection(CaretX, CaretY);
            RepaintText(FEditRect);
          end;
        VK_PRIOR :
          begin
            CaretY := CaretY - FLinesInRect;
            SetSelection(CaretX, CaretY);
            RepaintText(FEditRect);
          end; // VK_PRIOR + Shift
      end; // case
    end;
    inherited;
  end
  else
  if Shift = [ssCtrl] then
  begin
    if (Char(Key) = 'C') or (Key = VK_INSERT) then
    begin
      SendMessage(Handle, WM_COPY, 0, 0);
      Key := 0;
    end;
    if (Char(Key) = 'X') then
    begin
      SendMessage(Handle, WM_CUT, 0, 0);
      if NotifyUserChangeOnly then
        Change;
      Key := 0;
    end;
    if (Char(Key) = 'Z') then
    begin
      if not FReadOnly then
      begin
        Undo;
        if NotifyUserChangeOnly then
          Change;
      end;
      Key := 0;
    end;
    if (Char(Key) = 'V') then
    begin
      SendMessage(Handle, WM_PASTE, 0, 0);
      if NotifyUserChangeOnly then
        Change;
      Key := 0;
    end;
    if (Char(Key) = 'A') then
    begin
      SelectAll;
      Key := 0;
    end;
    if (Key = VK_BACK) or (Key = VK_DELETE) then
    begin
      if FSelected then
        DeleteSelection;

      if (Key = VK_BACK) then
      begin
        FPos := GetPrevWord(Point(CaretX - 1, CaretY));
        SetSelection(FPos.X, FPos.Y);
        DeleteSelection;
      end;

      if (Key = VK_DELETE) then
      begin
        FPos := GetNextWord(CaretXY);
        SetSelection(FPos.X, FPos.Y);
        DeleteSelection;
      end;
    end;
    if (Key in SRArrowKeys) then
    begin
      case Key of
        VK_LEFT :
          begin
            // ctrl+left
            FPos := GetPrevWord(Point(CaretX - 1, CaretY));
            if FSelected then
              UnSelect;
            CaretXY := FPos;
            FSelStartX := FCaretX;
            FSelStartY := FCaretY;
            TriggerSelectionChangeEvent;
            RepaintText(EditRect);
          end;
        VK_RIGHT :
          begin
            FPos := GetNextWord(CaretXY);
            if FSelected then
              UnSelect;
            CaretXY := FPos;
            FSelStartX := FCaretX;
            FSelStartY := FCaretY;
            TriggerSelectionChangeEvent;
            RepaintText(EditRect);
          end;
        VK_HOME :
          begin
            if FSelected then
              UnSelect;
            CaretX := 0;
            CaretY := 0;
            FSelStartX := FCaretX;
            FSelStartY := FCaretY;
          end;
        VK_END :
          begin
            if FSelected then
              UnSelect;
            FElEditList.CaretFromParagraph(FElEditList.FParagraphs.Count - 1,
                Length(FElEditList.FParagraphs.Items[FElEditList.FParagraphs.Count - 1].Text),
                FX, FY);
            CaretX := FX;
            CaretY := FY;
            FSelStartX := FCaretX;
            FSelStartY := FCaretY;
          end;
      end; // case
    end;
//   inherited;
  end
  else
    inherited;
end; { KeyDown }

procedure TCustomElEdit.WMCut(var Msg : TMessage); { private }
begin
  if FPasswordChar <> #0 then exit;
  WMCopy(Msg);
  if not ReadOnly then
  begin
    DeleteSelection;
    if not NotifyUserChangeOnly then
      Change;
    TriggerSelectionChangeEvent;
  end;
end; {WMCut}

procedure TCustomElEdit.WMCopy(var Msg : TMessage); { private }
var
  Clip : TClipboard;
  {$ifdef ELPACK_UNICODE}
  mem : Cardinal;
  ptr : Pointer;
  SelText : WideString;
  SelLen : integer;
  S : String;
  {$endif}
begin
  if FPasswordChar <> #0 then exit;
  if {FSelLength <> 0}FSelected then
  begin
    Clip := Clipboard;
    Clip.Open;
    {$ifdef ELPACK_UNICODE}
    SelText := GetSelectedText;
    SelLen := (Length(SelText) + 1) * 2;
    mem := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, SelLen);
    ptr := GlobalLock(mem);
    Move(PWideChar(SelText)^, ptr^, SelLen);
    GlobalUnlock(mem);
    Clip.SetAsHandle(CF_UNICODETEXT, mem);
    if Win32Platform <> VER_PLATFORM_WIN32_NT then
    begin
      S := SelText;
      SelLen := Length(S) + 1;
      mem := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SelLen);
      ptr := GlobalLock(mem);
      Move(PElFChar(SelText)^, ptr^, SelLen);
      GlobalUnlock(mem);
      Clip.SetAsHandle(CF_TEXT, mem);
    end;
    {$else}
    Clip.AsText := ConvertBreaksFormat(GetSelectedText);
    {$endif}
    Clip.Close;
  end;
end; {WMCopy}

procedure TCustomElEdit.WMPaste(var Msg : TMessage); { private }
var
  Clip : TClipboard;
  {$ifdef ELPACK_UNICODE}
  Piece    : WideString;
  {$else}
  Piece    : string;
  {$endif}
//  FSaveSel : TPoint;
  {$ifdef ELPACK_UNICODE}
  mem: Cardinal;
  ptr: Pointer;
  StrEnd : PWideChar;
  {$endif}
  fx, fy: integer;
  FPos: TPoint;
  GLen, PLen: integer;
begin
  if FReadOnly then exit;
  Clip := Clipboard;
  Clip.Open;
  {$ifdef ELPACK_UNICODE}
  if not Clipboard.HasFormat(CF_UNICODETEXT) then
  begin
    Piece := Clip.AsText;
  end
  else
  begin
    mem := Clipboard.GetAsHandle(CF_UNICODETEXT);
    SetLength(Piece, GlobalSize(mem));
    ptr := GlobalLock(mem);
    Move(ptr^, PWideChar(Piece)^, Length(Piece));
    StrEnd := WideStrScan(PWideChar(Piece), #0);
    if StrEnd <> nil then
      SetLength(Piece, StrEnd - PWideChar(Piece));
    GlobalUnlock(mem);
  end;
  {$else}
  Piece := Clip.AsText;
  {$endif}

  Clip.Close;
  PLen := Length(Piece);
  GLen := Length(Text);
  if PLen = 0 then exit;


  if ((PLen + GLen) > FMaxLength) and (FMaxLength > 0) then
  {$ifdef ELPACK_UNICODE}
    Piece := WideCopy(Piece, 1, (FMaxLength - GLen));
  {$else}
    Piece := Copy(Piece, 1, (FMaxLength - GLen));
  {$endif}

  if FSelected then
    DeleteSelection;

  FX := CaretX;
  FY := CaretY;
  FPos := CaretXY;
  FElEditList.InsertText(FX, FY, Piece);
  CaretY := FY;
  CaretX := FX;
  if not NotifyUserChangeOnly then
    Change;
  TriggerSelectionChangeEvent;
  RepaintText(FEditRect);
  FSelStartX := CaretX;
  FSelStartY := CaretY;
  FUndo.AddAction(atInsert, FPos, CaretXY, Piece);
end; {WMPaste}

procedure TCustomElEdit.WMClear(var Msg : TMessage); { private }
begin
  SelText := '';
end; { WMClear }

procedure TCustomElEdit.WMSetText(var Msg : TMessage); { private }
var
  {$ifdef ELPACK_UNICODE}
  ANewStr : WideString;
  Len : integer;
  Temp : string;
  {$ifdef VCL_6_USED}
  Dsgn : boolean;
  {$endif}
  {$else}
  ANewStr : String;
  {$endif}
begin
  {$ifdef ELPACK_UNICODE}
  if Msg.WParam = 1 then
    ANewStr := WideStrPas(PWideChar(Msg.lParam))
  else
  begin
    Temp := StrPas(PChar(Msg.LParam));
    {$ifdef VCL_6_USED}
    if (csDesigning in ComponentState) then
      Dsgn := (Pos(Name, Temp) <> 1)
    else
      Dsgn := false;

    if Dsgn then
      ANewStr := WideStrPas(PWideChar(Msg.lParam))
    else
    {$endif}
    begin
      Len := MultiByteToWideChar(GetACP(), MB_PRECOMPOSED or MB_USEGLYPHCHARS,
                                 PChar(Temp), Length(Temp), nil, 0);
      if Len > 0 then
      begin
        SetLength(ANewStr, Len);
        MultiByteToWideChar(GetACP(), MB_PRECOMPOSED or MB_USEGLYPHCHARS, PChar(Temp),
                            Length(Temp), PWideChar(ANewStr), Len)
      end
      else
        ANewStr := '';
    end;
  end;
  {$else}
  ANewStr := StrPas(PChar(Msg.lParam));
  {$endif}
  Msg.Result := 0;

  FElEditList.Text := ANewStr;

  if HandleAllocated then SetCaretPosition(CaretX, CaretY);
  Msg.Result := 1;
  if not NotifyUserChangeOnly then
    Change;
  TriggerSelectionChangeEvent;
  if HandleAllocated then RepaintText(EditRect);
end; { WMSetText }

procedure TCustomElEdit.WMNCPaint(var Msg : TMessage); { private }
var DC : HDC;
    RW,
    RC : TRect;
    b  : boolean;
begin
  b := false;
  if (BorderStyle = bsSingle) and (not Flat or not UseCustomScrollBars) then
  begin
    inherited;
    b := true;
  end;
  if IsThemeApplied and (BorderStyle = bsSingle) {and not Transparent} then
  begin
    //DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    //if DC = 0 then
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

    DrawThemeBackground(Theme, DC, 0, 0, RW, nil);
    ReleaseDC(Handle, DC);
  end
  else
  if (not b) and ((not Flat) or (not UseCustomScrollBars)) then 
    inherited
  else
  if (Flat or (FUseCustomScrollBars and Multiline) or IsThemeApplied) and (BorderStyle = bsSingle) then
  begin
    DrawFlatBorder;
    Msg.Result := 0;
  end;
end; { WMNCPaint }

procedure TCustomElEdit.WMEraseBkgnd(var Msg : TWMEraseBkgnd); { private }
var
  {$ifdef ELPACK_COMPLETE}
  ACtl   : TWinControl;
  R1,
  BgRect,
  {$endif}
  RW     : TRect;
  sid    : integer;
  //aBrush : HBrush;
begin
  {$ifdef ELPACK_COMPLETE}
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
  end
  else
  {$endif}
  if FTransparent and (not IsThemeApplied) then
    DrawParentControl(Msg.DC)
  else
  (*
  if FUseBackground and not FBackground.Empty then
    DrawBackground(Msg.DC, ClientRect)
  else
  if not IsThemeApplied then
  begin
    GetClipBox(Msg.DC, R1);
    IntersectRect(R, ClientRect, R1);
    ABrush := CreateSolidBrush(ColorToRGB(Color));
    FillRect(Msg.DC, R, ABrush);
    DeleteObject(ABrush);
  end
  else
  *)
  if IsThemeApplied then 
  begin
    RW := BoundsRect;
    if Parent <> nil then
      MapWindowPoints(Parent.Handle, Handle, RW, 2)
    else
    if ParentWindow <> 0 then
      MapWindowPoints(ParentWindow, Handle, RW, 2);

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
  end;
  Msg.Result := 1;
end;

procedure TCustomElEdit.Paint; { protected }
var
  BMP : TBitmap;
  Rect: TRect;
  R,
  R1   : TRect;
  aBrush : HBrush;
begin
  if not HandleAllocated then exit;
{$ifndef CLX_USED}
  if Flat and not IsThemeApplied then
    DrawFlatBorder;
{$endif}
  BMP := TBitmap.Create;
  BMP.Height := ClientHeight;
  BMP.Width := ClientWidth;
  if (FUseCustomScrollBars and Multiline) then
  begin
    if ScrollBars in [ssHorizontal, ssBoth] then
      BMP.Height := BMP.Height - FHorzScrollBarStyles.Width;
    if ScrollBars in [ssVertical, ssBoth] then
      BMP.Width := BMP.Width - FVertScrollBarStyles.Width;
  end;
  Rect := Canvas.ClipRect;
  if IsRectEmpty(Rect) then
    Rect := ClientRect;
  {$ifdef ELPACK_COMPLETE}
  if (FImgForm = nil) or (FImgForm.Backgroundtype = bgtColorFill) or
   (csDesigning in FImgForm.GetRealControl.ComponentState) then
  {$endif}
  begin
    if Transparent then
      bitblt(Bmp.Canvas.Handle, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, Canvas.Handle, Rect.Left, Rect.Top, SRCCOPY)
    else
    if FUseBackground and not FBackground.Empty then
      DrawBackground(BMP.Canvas.Handle, ClientRect)
    else
//    if not IsThemeApplied then
    begin
      GetClipBox(BMP.Canvas.Handle, R1);
      IntersectRect(R, ClientRect, R1);
      ABrush := CreateSolidBrush(ColorToRGB(Color));
      FillRect(BMP.Canvas.Handle, R, ABrush);
      DeleteObject(ABrush);
    end;
  {$ifdef ELPACK_COMPLETE}
  end
  else
  begin
    bitblt(Bmp.Canvas.Handle, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, Canvas.Handle, Rect.Left, Rect.Top, SRCCOPY);
  {$endif}
  end;
  BMP.Canvas.Brush.Color := Color;
  BMP.Canvas.Font.Assign(Font);
  PaintText(BMP.Canvas);
  Canvas.CopyRect(Rect, BMP.Canvas, Rect);
  if (ScrollBars in [ssBoth]) and (FUseCustomScrollBars and Multiline) then
  begin
    Rect := Classes.Rect(scbVert.Left, scbHorz.Top, scbVert.Left + scbVert.Width, scbHorz.Top + scbHorz.Height);
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := scbVert.Color;
    Canvas.FillRect(Rect);
  end;
  BMP.Free;
end;

(*
procedure TCustomElEdit.DrawFrame; { protected }
var
  Rgn : HRGN;
  ADC : HDC;
  LBR : TLogBrush;
  ABR,
    OBR : HBRUSH;
  R : TRect;
begin
  if HandleAllocated then
  begin
    if FHasCaret then HideCaret(Handle);
    SetRectEmpty(R);
    Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    ADC := GetWindowDC(Handle);
    GetWindowRgn(Handle, Rgn);
    Lbr.lbStyle := BS_SOLID;
    Lbr.lbColor := ColorToRgb(clBtnText);
    Lbr.lbHatch := 0;
    ABR := CreateBrushIndirect(Lbr);
    OBR := SelectObject(ADC, ABR);
    FrameRgn(ADC, Rgn, ABR, (Width - ClientWidth) div 2, (Height - ClientHeight) div 2);
    SelectObject(ADC, OBR);
    DeleteObject(ABR);

    DeleteObject(Rgn);
    ReleaseDC(Handle, ADC);
    if FHasCaret then
       ShowCaret(Handle);
  end;
end;
*)

procedure TCustomElEdit.DrawTabbedText(DC : HDC; X, Y : integer; AText : TElFString; var Size : TSize);
var
 DrawRect: TRect;
begin
  DrawRect.Left := EditRect.Left + FLeftMargin;
  DrawRect.Top  := EditRect.Top;
  DrawRect.Right := EditRect.Right - RightMargin;
  DrawRect.Bottom := EditRect.Bottom;
//  {$ifdef ELPACK_UNICODE}
//  S := ExpandTabbedString(AText);
//  {$else}
  AText := ExpandTabbedString(AText);
//  {$endif}
//  while {$ifdef ELPACK_UNICODE}WidePos(#13, S){$else}pos(#13, AText){$endif} > 0 do
//    {$ifdef ELPACK_UNICODE}
//    begin
//      WideDelete(S, WidePos(#13, S), 1);
//    end;
//    {$else}
//    System.Delete(AText, Pos(#13, AText), 1);
//    {$endif}
  {$ifdef ELPACK_UNICODE}
//  AText := S;
  ExtTextOutW(DC, X, Y, ETO_CLIPPED, @(DrawRect), PWideChar(AText), Length(AText),nil);
  GetTextExtentPoint32W(DC, PWideChar(AText), Length(AText), Size);
  {$else}
  ExtTextOut(DC, X, Y, ETO_CLIPPED, @(DrawRect), PChar(AText), Length(AText),nil);
  GetTextExtentPoint32(DC, PChar(AText), Length(AText), Size);
  {$endif}
end;

procedure TCustomElEdit.PaintText(Canvas : TCanvas); { protected }
var
  FText,
    FText1,
    FText2,
    FText3 : TElFString;
  sx, sy: integer;
  FSaveAlign : DWORD;
  FSaveColor,
    FSaveBkColor : COLORREF;
  TS : TSize;
  fl : integer;
  Delta: integer;
//  ls, le : integer;
  FSaveBrStyle : TBrushStyle;
  LinesToDraw : integer;

begin
  if not HandleAllocated then exit;
  if FHasCaret then HideCaret(Handle);
  if FChangeDisabledText and (not Enabled) then
    FSaveColor := SetTextColor(Canvas.Handle, ColorToRGB(clGrayText))
  else
    FSaveColor := SetTextColor(Canvas.Handle, ColorToRGB(Canvas.Font.Color));

  FSaveBrStyle := Canvas.Brush.Style;

  // Отрисовываем только видимые строки текста
  if FTopLine < 0 then
   TopLine := 0;
  LinesToDraw := (FTopLine + FLinesInRect) - 1;
  if (FTopLine + FLinesInRect) > FElEditList.ParagraphCount - 1 then
    LinesToDraw := FElEditList.ParagraphCount - 1;

  for fl := FTopLine to LinesToDraw do
  begin
    FText := ExpandTabbedString(StringToPassword(FElEditList.ParagraphStrings[fl]));
    FCharsInView := CharsFitRight((EditRect.Right - FRightMargin) - (EditRect.Left + FLeftMargin), FText, FLeftChar);
    {$ifdef ELPACK_UNICODE}
    FText := WideCopy(FText, FLeftChar + 1, FCharsInView);
    {$else}
    FText := Copy(FText, FLeftChar + 1, FCharsInView);
    {$endif}

    TS := TextSize(FText);
    sy := (FLineHeight * ((fl - FTopLine) + 1)) + FTopMargin;
    case Alignment of
      taRightJustify: sx := (EditRect.Right - FRightMargin) - TS.cx - 2;
      taCenter:       sx := ((EditRect.Right - FRightMargin) div 2) - (TS.cx div 2);
    else
      sx := 0;
    end;
    Inc(sx, FLeftMargin);
    FSaveAlign := SetTextAlign(Canvas.Handle, TA_BOTTOM or TA_NOUPDATECP or TA_LEFT);
    Canvas.Brush.Style := bsClear;
    if {(SelLength = 0) or }(HideSelection and not Focused) or
       ((FSelFirstX = FSelLastX) and (FSelFirstY = FSelLastY)) or
       ((fl < FSelFirstY) or (fl > FSelLastY)) then
      DrawTabbedText(Canvas.Handle, SX, SY, FText, TS)
    else
    begin
      if fl = FSelFirstY then
      begin
        Delta := FSelFirstX - FLeftChar;

        if Delta < 0 then
          FText1 := ''
        else
          FText1 := Copy(FText, 1, Delta);

        if FSelFirstY = FSelLastY then
        begin
          if Delta < 0 then
          begin
            {$ifdef ELPACK_UNICODE}
            FText2 := WideCopy(FText, 1, (FSelLastX - FLeftChar));
            FText3 := WideCopy(FText, (FSelLastX - FLeftChar) + 1, Length(FText));
            {$else}
            FText2 := Copy(FText, (FSelFirstX + 1), (FSelLastX - FSelFirstX));
            FText3 := Copy(FText, (FSelLastX + 1), Length(FText));
            {$endif}
          end
          else
          begin
            {$ifdef ELPACK_UNICODE}
            FText2 := WideCopy(FText, Delta + 1, (FSelLastX - FSelFirstX));
            FText3 := WideCopy(FText, Delta + (FSelLastX - FSelFirstX) + 1, Length(FText));
            {$else}
            FText2 := Copy(FText, (FSelFirstX + 1), (FSelLastX - FSelFirstX));
            FText3 := Copy(FText, (FSelLastX + 1), Length(FText));
            {$endif}
          end;
        end
        else
        begin
          {$ifdef ELPACK_UNICODE}
          FText2 := WideCopy(FText, Delta + 1{FSelFirstX + 1}, Length(FText));
          {$else}
          FText2 := Copy(FText, Delta + 1{FSelFirstX + 1}, Length(FText));
          {$endif}
          FText3 := '';
        end;
      end
      else
      begin
        Delta := FSelLastX - FLeftChar;
        if fl = FSelLastY then
        begin
          if Delta < 0 then
          begin
            FText1 := '';
            FText2 := '';
            FText3 := Copy(FText, 1, Length(FText));
          end
          else
          begin
            FText1 := '';
            FText2 := Copy(FText, 1, Delta);
            FText3 := Copy(FText, Delta + 1, Length(FText));
          end;
        end
        else
        begin
          FText1 := '';
          FText2 := FText;
          FText3 := '';
        end;
      end;
      SetTextAlign(Canvas.Handle, TA_BOTTOM or TA_NOUPDATECP or TA_LEFT);

      if Length(FText1) > 0 then
      begin
        Canvas.Brush.Style := bsClear;
        DrawTabbedText(Canvas.Handle, SX, SY, FText1, TS);
        inc(sx, TS.cx);
      end;
      if Length(FText2) > 0 then
      begin
        Canvas.Brush.Style := bsSolid;
        SetBkColor(Canvas.Handle, ColorToRGB(clHighlight));
        SetTextColor(Canvas.Handle, ColorToRGB(clHighlightText));
        DrawTabbedText(Canvas.Handle, SX, SY, FText2, TS);
        inc(sx, TS.cx);
        if FChangeDisabledText and (not Enabled) then
          SetTextColor(Canvas.Handle, ColorToRGB(clGrayText))
        else
          SetTextColor(Canvas.Handle, ColorToRGB(Canvas.Font.Color));
        SetBkColor(Canvas.Handle, ColorToRGB(Canvas.Brush.Color));
      end
      else
      begin
        Canvas.Brush.Color := FSaveBkColor;
        Canvas.Brush.Style := FSaveBrStyle;
      end;
      if Length(FText3) > 0 then
      begin
        SetBkMode(Canvas.Handle, Windows.Transparent);
        Canvas.Brush.Style := bsSolid;
        DrawTabbedText(Canvas.Handle, SX, SY, FText3, TS);
      end;
    end;
    SetTextAlign(Canvas.Handle, FSaveAlign);
    SetTextColor(Canvas.Handle, FSaveColor);
    SetBkColor(Canvas.Handle, FSaveBkColor);
    Canvas.Brush.Style := FSaveBrStyle;
  end;
  if FHasCaret then ShowCaret(Handle);
end;

procedure TCustomElEdit.Change;
{ Triggers the OnChange event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnChange)) then
    FOnChange(Self);
end; { TriggerChangeEvent }

procedure TCustomElEdit.TriggerSelectionChangeEvent;
{ Triggers the OnSelectionChange event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnSelectionChange)) then
    FOnSelectionChange(Self);
end; { TriggerSelectionChangeEvent }

procedure TCustomElEdit.SetReadOnly(newValue : Boolean);
{ Sets data member FReadOnly to newValue. }
begin
  if (FReadOnly <> newValue) then
  begin
    FReadOnly := newValue;
  end; { if }
end; { SetReadOnly }

procedure TCustomElEdit.SetAlignment(newValue : TAlignment);
{ Sets data member FAlignment to newValue. }
begin
  if (FAlignment <> newValue) then
  begin
    FAlignment := newValue;
    if not RTLContent then
      if FAlignment <> taLeftJustify then
        WordWrap := True;
    Invalidate;
    if FHasCaret then SetCaretPosition(CaretX, CaretY);
  end; { if }
end; { SetAlignment }

procedure TCustomElEdit.SetLeftMargin(newValue : Integer);
{ Sets data member FLeftMargin to newValue. }
begin
  if (FLeftMargin <> newValue) then
  begin
    FLeftMargin := newValue;
    Invalidate;
    if FHasCaret then SetCaretPosition(CaretX, CaretY);
  end; { if }
end; { SetLeftMargin }

procedure TCustomElEdit.SetRightMargin(newValue : Integer);
{ Sets data member FRightMargin to newValue. }
begin
  if (FRightMargin <> newValue) then
  begin
    FRightMargin := newValue;
    Invalidate;
    if FHasCaret then SetCaretPosition(CaretX, CaretY);
  end; { if }
end; { SetRightMargin }

procedure TCustomElEdit.SetBorderStyle(newValue : TBorderStyle);
{ Sets data member FBorderStyle to newValue. }
begin
  if (FBorderStyle <> newValue) then
  begin
    FBorderStyle := newValue;
    if HandleAllocated then RecreateWnd;
  end; { if }
end; { SetBorderStyle }

procedure TCustomElEdit.SetHideSelection(newValue : Boolean);
{ Sets data member FHideSelection to newValue. }
begin
  if (FHideSelection <> newValue) then
  begin
    FHideSelection := newValue;
    if not Focused then
       Invalidate;
  end; { if }
end; { SetHideSelection }

procedure TCustomElEdit.TriggerResizeEvent;
begin
  if (assigned(FOnResize)) then
    FOnResize(Self);
end; { TriggerResizeEvent }

function TCustomElEdit.CaretFromChar(const CharNum: integer): TPoint;
var
  i: integer;
  Len: integer;
begin
  Len := 0;
  Result.X := 0;
  Result.Y := 0;
  for i := 0 to FElEditList.Count - 1 do
  begin
    Len := Len + Length(FElEditList.Strings[i]) + 2;
    if Len >= CharNum then
    begin
      FElEditList.CaretFromParagraph(i, (CharNum - (Len - (Length(FElEditList.Strings[i]) + 2))), Result.X, Result.Y);
      break;
    end;
  end;
end;

function TCustomElEdit.CharFromCaret(const X, Y: integer): integer;
var
  i: integer;
  P, PO: integer;
begin
  Result := 0;
  FElEditList.CaretToParagraph(X, Y, P, PO);
  for i := 0 to P - 1 do
    inc(Result, Length(FElEditList.Strings[i]) + 2);
  inc(Result, PO);
end;

function TCustomElEdit.PosFromCaret(const X, Y: integer): TPoint;
var
  fy, fx: integer;
  FStr: TElFString;
  TS: TSize;
  FText1: TElFString;
begin
  if (FElEditList.ParagraphCount >= Y) then
  begin
    FText1 := FElEditList.ParagraphStrings[Y];
    {$ifdef ELPACK_UNICODE}
    FStr := WideCopy(FElEditList.ParagraphStrings[Y], FLeftChar + 1, (X - FLeftChar));
    GetTextExtentPoint32W(Canvas.Handle, PWideChar(FStr), Length(FStr), TS);
    {$else}
    FStr := Copy(FElEditList.ParagraphStrings[Y], FLeftChar + 1, (X - FLeftChar));
    GetTextExtentPoint32(Canvas.Handle, PChar(FStr), Length(FStr), TS);
    {$endif}
  end
  else
    EElEditorError.CreateFmt('Invalid Y position (%d)', [Y]);

  fy := FLineHeight * (Y - TopLine);

  Canvas.Font.Assign(Font);
  TS := TextSize(FStr);
  if X < FLeftChar then fx := -2
                   else fx := TS.cx;
  if Alignment = taCenter then
    fX := fX + abs(((EditRect.Right - FRightMargin) div 2) - (TextSize(FText1).cx div 2));
  if Alignment = taRightJustify then
    fX := fX + abs((EditRect.Right - FRightMargin) - TextSize(FText1).cx) - 2;

  fx := fx + FLeftMargin;
  fy := fy + FTopMargin;
  if fx > (FEditRect.Right - FRightMargin) then fx := -2;
  if Y >= (TopLine + FLinesInRect) then fy := fy + FEditRect.Bottom;

  Result := Point(fx, fy);
end;

function TCustomElEdit.CharsFitLeft;
var
  FText1, FText2 : TElFString;
  i, j, k : integer;
  TS : TSize;
begin
  FText1 := Copy(StringToPassword(FText), 1, StartPos);
  k := Length(FText1);
  SetLength(FText2, k);
  for i := 0 to k - 1 do
    FText2[i + 1] := FText1[k - i];
  FText1 := ExpandTabbedString(FText2);
  j := Length(FText1) - 1;
  if j = -1 then
  begin
    result := 0;
    exit;
  end;
  if HandleAllocated then
    {$ifdef ELPACK_UNICODE}
    GetTextExtentPoint32W(Canvas.Handle, PWideChar(FTabString), Length(FTabString), TS);
    {$else}
    GetTextExtentPoint32(Canvas.Handle, PChar(FTabString), Length(FTabString), TS);
    {$endif}

  j := AWidth;

  // here we can ignore margins
  if HandleAllocated then
    ElGetTextExtentExPoint(Canvas.Handle, PElFChar(FText1), Length(FText1), j, i, nil, TS);

  result := i;

  for i := 1 to Length(FText2) do
    if FText2[i] = ElFTab then dec(result, FTabSpaces - 1);

  exit;
end;

function TCustomElEdit.CharsFitRight(AWidth : integer; FText : TElFString; StartPos : integer) : integer;
var
  i, j   : integer;
  //tc, tv : integer;
  TS     : TSize;
  FText1 : TElFString;

begin
  // Оставляем в строке символы, начиная со стартовой позиции,
  // и преобразовываем их в символ пароля, если нужно
  {$ifdef ELPACK_UNICODE}
  FText := WideCopy(StringToPassword(FText), StartPos + 1, Length(FText));
  {$else}
  FText := Copy(StringToPassword(FText), StartPos + 1, Length(FText));
  {$endif}

  // Заменяем символ табуляции на заданную строку заполнения.
  FText1 := ExpandTabbedString(FText);
  j := Length(FText1) - 1; // В j длина строки со стартовой позиции
  if j = -1 then // Если длина строки = -1 то выходим
  begin
    result := 0;
    exit;
  end;
  //Вычисляем длину и высоту строки символов в пикселах
  if HandleAllocated then
    {$ifdef ELPACK_UNICODE}
    GetTextExtentPoint32W(Canvas.Handle, PWideChar(FTabString), Length(FTabString), TS);
    {$else}
    GetTextExtentPoint32(Canvas.Handle, PChar(FTabString), Length(FTabString), TS);
    {$endif}

  j := AWidth; // В j общая длина строки
  // here we can ignore margins
  if HandleAllocated then
    ElGetTextExtentExPoint(Canvas.Handle, PElFChar(FText), Length(FText), j, i, nil, TS);

  result := i;

  for i := 1 to Length(FText) do
    if FText[i] = ElFTab then dec(result, FTabSpaces - 1);

  exit;
end;

procedure TCustomElEdit.CaretFromPos(APos: TPoint; var ACaretX, ACaretY: integer);
var
  i, j,
    fl : integer;
  p,
    p1,
    sp : PInteger;
  TS : TSize;
  sx,
    cv : integer;
    // used in multi-row mode
  le: integer;
  // used in tab expansion
  tc,
    tv : integer;
  FText, ST: TElFString;
begin
  fl := APos.y div FLineHeight;
  inc(fl, FTopLine);
  if fl < 0 then fl := 0;
  if fl > FElEditList.ParagraphCount - 1 then
    fl := FElEditList.ParagraphCount - 1;

  ACaretY := fl;
  ST := FElEditList.ParagraphStrings[fl];

  le := length(ST);

  if RTLContent then
    // to do
  {$ifdef ELPACK_UNICODE}
    FText := StringToPassword(WideCopy(ST, FLeftChar, le - FLeftChar))
  {$else}
    FText := StringToPassword(Copy(ST, FLeftChar, le - FLeftChar))
  {$endif}
  else
  {$ifdef ELPACK_UNICODE}
    FText := StringToPassword(WideCopy(ST, FLeftChar, (le - FLeftChar) + 1));
  {$else}
    FText := StringToPassword(Copy(ST, FLeftChar, (le - FLeftChar) + 1));
  {$endif}

  if Length(FText) = 0 then
  begin
    ACaretX := FLeftChar;
    exit;
  end;

  {$ifdef ELPACK_UNICODE}
  GetTextExtentPoint32W(Canvas.Handle, PWideChar(FTabString), Length(FTabString), TS);
  {$else}
  GetTextExtentPoint32(Canvas.Handle, PChar(FTabString), Length(FTabString), TS);
  {$endif}
  tv := TS.cx;

  tc := 0;

  TS := TextSize(FText);
  case Alignment of
    taRightJustify: sx := (EditRect.Right - FRightMargin) - TS.cx;
    taCenter:       sx := (EditRect.Right - FRightMargin) div 2 - TS.cx div 2;
    else            sx := 0;
  end;
  Inc(sx, FLeftMargin);

  j := Length(FText) - 1;
  GetMem(P, (j + 1) * sizeof (Integer));
  sp := p;

  // in the next line we can ignore margins
  ElGetTextExtentExPoint(Canvas.Handle, PElFChar(FText), Length(FText), TextSize(FText).cx, i, p, TS);
  ACaretX := FLeftChar;

  if FText[1] = ElFTab then
  begin
    inc(tc, tv - p^);
    p^ := tv;
  end;

  cv := p^ div 2;
  if sx + cv >= APos.x then
  begin
    FreeMem(sp);
    exit;
  end;
  for i := 0 to j do
  begin
    if i > 0 then
    begin
      if FText[i+1] = ElFTab then
      begin
        p1 := p;
        dec(p1);
        tc := tc + tv - (p^ - p1^){width of TAB character};
        p^ := p^ + tv - (p^ - p1^){width of TAB character};
        cv := tv div 2;
      end else
      begin
        p^ := p^ + tc;
        cv := p^;
        dec(p);
        cv := (cv - p^) div 2;
        inc(p);
      end;
    end;
    if (sx + P^ - cv) > APos.x then
    begin
      ACaretX := (i + FLeftChar);
      FreeMem(sp);
      exit;
    end;
    inc(p);
  end;
  FreeMem(sp);
//  FText := FElEditList.GetCR(fl);
  ACaretX := Length(FText) + FLeftCHar;
end;

function TCustomElEdit.GetLinesCount: Integer;
begin
  result := FElEditList.ParagraphCount;
end;

procedure TCustomElEdit.SelectAll;
var
  xCaretX: integer;
  xCaretY: integer;
begin

  xCaretY := FElEditList.ParagraphCount - 1;
  xCaretX := Length(FElEditList.ParagraphStrings[FElEditList.ParagraphCount - 1]);

  if (xCaretY >= TopLine + FLinesInRect) then
  begin
    FCaretY := xCaretY;
    FCaretX := xCaretX;
    SetSelection(FCaretX, FCaretY);
    FSelStartX := 0;
    FSelStartY := 0;
    SetCaretPosition(-100, -100);
  end
  else
  begin
    CaretY := xCaretY;
    CaretX := xCaretX;
    FSelStartX := 0;
    FSelStartY := 0;
    SetSelection(CaretX, CaretY);
  end;
  TriggerSelectionChangeEvent;
  RepaintText(EditRect);
end;
{
procedure TCustomElEdit.SelectAll;
var S : TElFString;
    xCaretX, xCaretY : integer;
begin
  FSelStartX := 0;
  FSelStartY := 0;
  if Multiline then
  begin
    xCaretY := FElEditList.ParagraphCount - 1;
    S := FElEditList.ParagraphStrings[FElEditList.ParagraphCount - 1];
    if Length(S) = 0 then
    begin
      if FElEditList.ParagraphCount = 1 then
        xCaretX := 0
      else
      begin
        xCaretX := Length(FElEditList.ParagraphStrings[FElEditList.ParagraphCount - 2]);
        dec(xCaretY);
      end;
      CaretY := xCaretY;
      CaretX := xCaretX;
    end
    else
      CaretX := Length(S);
  end
  else
  begin
    CaretY := 0;
    CaretX := Length(FElEditList.ParagraphStrings[0]);
    FSelStartX := 0;
    FSelStartY := 0;
  end;
  SetSelection(CaretX, CaretY);
  TriggerSelectionChangeEvent;
  RepaintText(EditRect);
end;
}
procedure TCustomElEdit.SetTabSpaces(newValue : Integer);
begin
  if (FTabSpaces <> newValue) and (newValue > 0) then
  begin
    FTabSpaces := newValue;
    SetLength(FTabString, FTabSpaces);
    {$ifdef ELPACK_UNICODE}
    FillMemory(PWideChar(FTabString), FTabSpaces, 32);
    {$else}
    FillMemory(PChar(FTabString), FTabSpaces, 32);
    {$endif}
    SetCaretPosition(CaretX, CaretY);
    RepaintText(EditRect);
  end; {if}
end;

procedure TCustomElEdit.DoSetEditRect(newValue : TRect);
{ Sets data member FEditRect to newValue. }
begin
  FEditRect := newValue;
  FLinesInRect := (FEditRect.Bottom - FTopMargin) div FLineHeight;
  if (FAlignBottom) then
    SetBottomAlign;
  SetScrollBarsInfo;
end; { SetEditRect }

procedure TCustomElEdit.MakeCaret;
var
  TTM : TTextMetric;
begin
  if HandleAllocated then
  begin
    Canvas.Font.Assign(Font);
    GetTextMetrics(Canvas.Handle, TTM);
    if fsBold in Font.Style then
      FHasCaret := CreateCaret(Handle, 0, GetSystemMetrics(SM_CXBORDER) * 2, Abs(TTM.tmHeight))
    else
      FHasCaret := CreateCaret(Handle, 0, GetSystemMetrics(SM_CXBORDER), Abs(TTM.tmHeight));
  end;
end;

procedure TCustomElEdit.MoveCaret(CharNum : integer);
var
//  FNewPos  : TPoint;
  FCharPos : TPoint;
begin
  if HandleAllocated then
    FCharPos := Point(FCaretX, FCaretY)//PosFromChar(FText, CharNum)
  else
    FCharPos := Point(0, EditRect.Top);

//  FNewPos := FCharPos;
  if FHasCaret then
  begin
    HideCaret(Handle);
    SetCaretPos(FCharPos.X, FCharPos.Y - 1);
    if Transparent then
      RepaintText(EditRect);
    ShowCaret(Handle);
  end;
end;

procedure TCustomElEdit.SetCaretX(const value: integer);
var
  P1,
  P2,
  PI,
  Len,
  TC: integer;
begin
  if (Value <> FCaretX) or (Alignment <> taLeftJustify) then
  begin
    FCaretX := Value;
    Len := Length(FElEditList.ParagraphStrings[FCaretY]);
    if ((FCaretY < FElEditList.ParagraphCount - 1) and (FCaretX >= Len) and (not FEnd)) then
    begin
      FElEditList.IndexToParagraph(CaretY, P1, PI);
      TC := CaretY;
      CaretY := CaretY + 1;
      FElEditList.IndexToParagraph(CaretY, P2, PI);
      if (P1 = P2) then
      begin
        if (FCaretX > Len) then
          FCaretX := 1
        else
          FCaretX := 0;
      end
      else
      begin
        if (FCaretX > Len) then
          FCaretX := 0
        else
          FCaretY := TC;
      end;
    end;
    if ((FCaretY = FElEditList.ParagraphCount - 1) and (FCaretX > Len)) then
      FCaretX := Length(FElEditList.ParagraphStrings[FCaretY]);
    if (FCaretX < 0) and (FCaretY > 0) then
    begin
      FElEditList.IndexToParagraph(CaretY, P1, PI);
      CaretY := CaretY - 1;
      FElEditList.IndexToParagraph(CaretY, P2, PI);
      if FCaretY < FElEditList.ParagraphCount - 1 then
      begin
        if P1 = P2 then
          FCaretX := Length(FElEditList.ParagraphStrings[FCaretY]) - 1
        else
          FCaretX := Length(FElEditList.ParagraphStrings[FCaretY]);
      end
      else
        FCaretX := 0;
    end
    else
    if (FCaretY = 0) and (FCaretX < 0) then
      FCaretX := 0;
    CorrectLeftChar;
    SetCaretPosition(FCaretX, FCaretY);
  end;
end;

procedure TCustomElEdit.SetCaretY(const value: integer);
//var Len: integer;
begin
  if (FCaretY <> value) then
  begin
    FCaretY := Value;
    if FCaretY > FElEditList.ParagraphCount - 1 then
      FCaretY := FElEditList.ParagraphCount - 1;
    if FCaretY < 0 then FCaretY := 0;
    if FCaretY < TopLine then
    begin
      TopLine := FCaretY;
      InvalidateRect(Handle, @FEditRect, false);
    end;
    if FCaretY >= (TopLine + FLinesInRect) then
    begin
      TopLine := (FCaretY - FLInesInRect) + 1;
      InvalidateRect(Handle, @FEditRect, false);
    end;

    //Len := Length(FElEditList.ParagraphStrings[FCaretY]);

//    if FCaretX > Len then
//      CaretX := Len;

    CorrectLeftChar;
    SetCaretPosition(FCaretX, FCaretY);
  end;
end;

function TCustomElEdit.GetCaretXY: TPoint;
begin
  Result := Point(CaretX, CaretY);
end;

procedure TCustomElEdit.SetCaretXY(const Value: TPoint);
begin
  CaretY := Value.Y;
  CaretX := Value.X;
end;


procedure TCustomElEdit.CorrectLeftChar;
var
  FText1: TElFString;
  TS: TSize;
  i: integer;
begin
  if HandleAllocated then
  begin
    if FElEditList.ParagraphCount > 0 then
      FText1 := FElEditList.ParagraphStrings[CaretY]
    else
      FText1 := '';

    TS := TextSize(FText1);
    if TS.CX > (EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin) then
      ForceLeftAlignment := true;

    if TS.CX <= (EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin) then
      ForceLeftAlignment := false;
  end;

  if (Alignment = taCenter) and not (ForceLeftAlignment) then
  begin
    // if text width is less than edit width, we center-align the text
    if TS.cx < (EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin) then
    begin
      FLeftChar := 0;
      SetScrollBarsInfo;
    end;
  end;
  if (Alignment = taLeftJustify) or ForceLeftAlignment then
  begin
    i := CharsFitRight((EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin), FText1, FLeftChar);
    if (FCaretX > FLeftChar + i) then
    begin
      if HandleAllocated then
      begin
        {$ifdef ELPACK_UNICODE}
        TS := TextSize(WideCopy(StringToPassword(FText1), FLeftChar + 1, FCaretX - FLeftChar));
        {$else}
        TS := TextSize(Copy(StringToPassword(FText1), FLeftChar + 1, FCaretX - FLeftChar));
        {$endif}
        if TS.CX >= ((EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin)) then
        begin
          i := CharsFitLeft(((EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin)) div 4 * 3, FText1, FCaretX);
          if (not FWordWrap) then
          begin
            FLeftChar := FCaretX - i;
            SetScrollBarsInfo;
          end;
          if FLeftChar < 0 then
          begin
            FLeftChar := 0;
            SetScrollBarsInfo;
          end;
          RepaintText(EditRect);
        end;
      end;
    end
    else
    if (FCaretX <= FLeftChar) and (FLeftChar > 0) then
    begin
      i := CharsFitLeft(((EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin)) div 4, FText1, FCaretX);
      if (not FWordWrap) then
        FLeftChar := FCaretX - i;
      if FLeftChar < 0 then
        FLeftChar := 0;
      SetScrollBarsInfo;
      if HandleAllocated then
        RepaintText(EditRect);
    end
  end
  else
  if Alignment = taRightJustify then
  begin
    if HandleAllocated then
    begin
      if TS.CX < ((EditRect.Right - FRightMargin) - (EditRect.Left + FLeftMargin)) then
      begin
        if FLeftChar <> 0 then
        begin
          FLeftChar := 0;
          SetScrollBarsInfo;
          RepaintText(EditRect);
        end;
      end
      else
      begin
        i := CharsFitRight((EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin), FText1, FLeftChar);
        if (FCaretX >= FLeftChar + i) then
        begin
          if HandleAllocated then
          begin
            TS := TextSize(Copy(StringToPassword(FText1), FLeftChar + 1, FCaretX - FLeftChar));
            if TS.CX >= (EditRect.Right - RightMargin) - (EditRect.Left + LeftMargin) then
            begin
              if not FWordWrap then
              begin
                FLeftChar := FCaretX - i;
                SetScrollBarsInfo;
              end;
              if FLeftChar < 0 then
              begin
                FLeftChar := 0;
                SetScrollBarsInfo;
              end;
              RepaintText(EditRect);
            end;
          end;
        end
        else
        if ((FCaretX <= FLeftChar) and (FLeftChar > 0))  then
        begin
          FLeftChar := FCaretX;
          SetScrollBarsInfo;
          if FLeftChar < 0 then
          begin
            FLeftChar := 0;
            SetScrollBarsInfo;
          end;
          if HandleAllocated then RepaintText(EditRect);
        end;
      end;
    end;
  end;
end;

procedure TCustomElEdit.SetCaretPosition(const X, Y: integer);
var
  FPos: TPoint;
begin
  FPos := PosFromCaret(X, Y);
  if FHasCaret then
  begin
    HideCaret(Handle);
    SetCaretPos(FPos.X, FPos.Y);
    if Transparent then
      RepaintText(EditRect);
    ShowCaret(Handle);
  end;
end;

procedure TCustomElEdit.SetMultiline(const Value: boolean);
begin
  if FMultiline <> Value then
  begin
    FMultiline := Value;
    if Value then
      AlignBottom := false;
    if not FMultiline then
     ScrollBars := ssNone;
    if HandleAllocated and (ComponentState * [csLoading, csReading] = []) then
      RecreateWnd
    else
      SetScrollBarsInfo;
    FElEditList.Text := Text;
    if HandleAllocated then
      SetEditRect(ClientRect);
  end;
end;

procedure TCustomElEdit.EMGetSel(var Message: TMessage);
var ss, se : integer;
begin
  ss := SelStart;

  if SelLength >= 0 then
    se := ss + SelLength
  else
  begin
    ss := SelStart + SelLength;
    se := SelStart;
  end;
  Message.result := se shr 16 or ss;
  if Message.lParam <> 0 then
    PDWord(Message.lParam)^ := se;
  if Message.wParam <> 0 then
    PDWord(Message.wParam)^ := ss;
end;

procedure TCustomElEdit.EMGetLine(var Message: TMessage);
var S : TElFString;
    S1: string;
    i : integer;
begin
  if InRange(0, Lines.Count - 1, Message.wParam) then
    S := Lines.Strings[Message.wParam]
  else
    S := '';

  if Message.lParam = 0 then
    Message.Result := 0
  else
  begin
    i := PWord(Pointer(Message.lParam))^;
    S1 := S;
    i := Min(Length(S1), i);
    if i > 0 then
      Move(PChar(S1)^, PChar(Pointer(Message.lParam))^, i);
    Message.Result := i;
  end;
  if not InRange(0, Lines.Count - 1, Message.wParam) then
    Message.result := 0;
end;

procedure TCustomElEdit.EMGetLineCount(var Message: TMessage);
begin
  Message.Result := Lines.Count;
end;

procedure TCustomElEdit.EMLineIndex(var Message: TMessage);
var
  P, PI: integer;
  fx, fy: integer;
begin
  if (Message.wParam = -1) then
  begin
    FElEditList.IndexToParagraph(CaretY, P, PI);
    FElEditList.CaretFromParagraph(P, 0, fx, fy);
    Message.Result := CharFromCaret(0, fy);
  end;
  if Message.wParam > FElEditList.Count - 1 then
    Message.Result := -1
  else
  begin
    FElEditList.IndexToParagraph(Message.wParam, P, PI);
    FElEditList.CaretFromParagraph(P, 0, fx, fy);
    Message.Result := CharFromCaret(0, Message.wParam);
  end;
end;

procedure TCustomElEdit.EMSetSel(var Message: TMessage);
begin
  if Message.WParam = -1 then
  begin
    UnSelect;
  end
  else
  if (Message.WParam = 0) and (Message.lParam = -1) then
    SelectAll
  else
  begin
    if Message.WParam < Message.lParam then
    begin
      SelStart := Message.lParam;
      SelLength := -(Message.lParam - Message.WParam);
      //if FSelLength = 0 then
      //  RepaintText(EditRect);
    end
    else
    begin
      SelStart := Message.wParam;
      SelLength := (Message.lParam - Message.WParam);
      //if FSelLength = 0 then
      //  RepaintText(EditRect);
    end;
  end;
end;

procedure TCustomElEdit.EMReplaceSel(var Message: TMessage);
var S : string;
begin
  if Message.lParam = 0 then exit;
  if Message.wParam <> 0 then
    DeleteSelection;

  S := StrPas(PChar(Pointer(Message.lParam)));
  SetSelText(S);
end;

procedure TCustomElEdit.EMGetFirstVisibleLine(var Message: TMessage);
begin
  Message.Result := Self.TopLine
end;

procedure TCustomElEdit.EMScroll(var Message: TMessage);
begin
  Scroll(TElEditScrollDir(Message.wParam));
end;

procedure TCustomElEdit.EMLineScroll(var Message: TMessage);
begin
  TopLine := TopLine + Message.lParam;
end;


procedure TCustomElEdit.EMScrollCaret(var Message: TMessage);
begin
  ScrollCaret;
end;

procedure TCustomElEdit.EMLineFromChar(var Message: TMessage);
var
  P, PI : integer;
begin
  FElEditList.IndexToParagraph(CaretFromChar(Message.wParam).Y, P, PI);
  Message.Result := P;
end;

procedure TCustomElEdit.EMPosFromChar(var Message: TMessage);
var
  P : TPoint;
begin
  P := CaretFromChar(Message.wParam);
  P := PosFromCaret(P.X, P.Y);
  Message.Result := P.Y shl 16 or P.X;
end;

function TCustomElEdit.StringToPassword(AString : TElFString) : TElFString;
var
  j : integer;
begin
  if FPasswordChar = #0 then
  begin
    Result := AString;
    exit;
  end;
  j := Length(AString);
  SetLength(Result, j);
  while j > 0 do
  begin
    if AString[j] in [ElFCR, ElFLF] then
      Result[j] := AString[j]
    else
      Result[j] := FPasswordChar;
    dec(j);
  end;
end;

function TCustomElEdit.ExpandTabbedString(Text : TElFString) : TElFString;
var
  i : integer;
  {$ifdef ELPACK_UNICODE}
  S : WideString;
  {$endif}
begin
  while {$ifdef ELPACK_UNICODE}WidePos(ElFTab, Text){$else}Pos(#9, Text){$endif} > 0 do
  begin
    {$ifdef ELPACK_UNICODE}
    S := Text;
    i := WidePos(ElFTab, S);
    WideDelete(S, i, 1);
    WideInsert(FTabString, S, i);
    Text := S;
    {$else}
    i := Pos(#9, Text);
    Delete(Text, i, 1);
    Insert(FTabString, Text, i);
    {$endif}
  end;
  result := Text;
end;

function TCustomElEdit.TextSize(ALine : TElFString) : TSize;
begin
  Result := TSize(Point(0, 0));
  Canvas.Font.Assign(Font);
  ALine := ExpandTabbedString(StringToPassword(ALine));
  if HandleAllocated then
    {$ifdef ELPACK_UNICODE}
    GetTextExtentPoint32W(Canvas.Handle, PWideChar(ALine), Length(ALine), Result);
    {$else}
    GetTextExtentPoint32(Canvas.Handle, PChar(ALine), Length(ALine), Result);
    {$endif}
end;

procedure TCustomElEdit.RepaintText(Rect : TRect);
var R : TRect;
begin
  if HandleAllocated then
  begin
    if Transparent and (not IsThemeApplied)
    {$ifdef ELPACK_COMPLETE}
    and ((FImgForm = nil) or (csDesigning in ComponentState))
    {$endif}
    then
    begin
      R := EditRect;
      MapWindowPoints(Handle, Parent.Handle, R, 2);
      InvalidateRect(Parent.Handle, @R, true);
      Parent.Update;
    end;
    InvalidateRect(Handle, @Rect, true);
  end;
end;

procedure TCustomElEdit.CutToClipboard;
begin
  SendMessage(Handle, WM_CUT, 0, 0);
end; {CutToClipboard}

procedure TCustomElEdit.CopyToClipboard;
begin
  SendMessage(Handle, WM_COPY, 0, 0);
end; {CopyToClipboard}

procedure TCustomElEdit.PasteFromClipboard;
begin
  SendMessage(Handle, WM_PASTE, 0, 0);
end; {PasteFromClipboard}

procedure TCustomElEdit.Undo;
var
  FX, FY: integer;
  AFAction: TElAction;
begin
  if FUndo.CanUndo then
  begin
    AFAction := FUndo.PopItem;
    with AFAction do
      case FAction of
      atDeleteSel, atDelete, atBackSpace:
        begin
          FRedo.PushItem(AFAction);
          FX := FEndPos.X;
          FY := FEndPos.Y;
          FElEditList.InsertText(FX, FY, FStr);
          CaretXY := FStartPos;
          FSelStartX := CaretX;
          FSelStartY := CaretY;
          if FAction = atDeleteSel then
            if (FStartPos.Y > FY) or ((FStartPos.Y = FY) and (FStartPos.X >= FX)) then
              SetSelection(FEndPos.X, FEndPos.Y)
            else
              SetSelection(FX, FY)
        end;
      atInsert:
        begin
          FRedo.PushItem(AFAction);
          CaretXY := FStartPos;
          FSelStartX := FStartPos.X;
          FSelStartY := FStartPos.Y;
          SetSelection(FEndPos.X, FEndPos.Y);
          FUndo.Lock;
          DeleteSelection;
          FUndo.UnLock;
        end;
      atLineBreak:
        begin
          FRedo.PushItem(AFAction);

        end;
      end;
    RepaintText(FEditRect);
  end
  else
    Modified := false;
end; {Undo}

procedure TCustomElEdit.SetModified(newValue : Boolean);
{ Sets data member FModified to newValue. }
begin
  if newValue then
    inc(FModifyCount)
  else
    dec(FModifyCount);
end; { SetModified }

procedure TCustomElEdit.CreateParams(var Params : TCreateParams); { protected }
const
  BorderStyles : array[TBorderStyle] of Cardinal = (0, WS_BORDER);
  MultilineStyles: array[boolean] of Cardinal = (0, ES_MULTILINE);
  ScrollBar : array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_VSCROLL or WS_HSCROLL);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or MultilineStyles[FMultiline];
    if (not FUseCustomScrollBars) and Multiline then
      Style := Style or ScrollBar[FScrollBars];

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      if RTLContent then ExStyle := WS_EX_LEFTSCROLLBAR or WS_EX_RTLREADING or WS_EX_RIGHT;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
    if Transparent then
      ExStyle := ExStyle or WS_EX_TRANSPARENT;
  end;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    Params.Style := Params.Style and (not WS_BORDER);
    Params.ExStyle := Params.ExStyle and (not WS_EX_CLIENTEDGE);
  end;
end; { CreateParams }

procedure TCustomElEdit.WMSetFocus(var Msg : TWMSetFocus); { private }
begin
  inherited;
  if CanFocus then
  begin
    MakeCaret;
    if FHasCaret then
    begin
      SetCaretPosition(CaretX, CaretY);
      ShowCaret(Handle);
    end;
    if AutoSelect then
      SelectAll;
    if FHideSelection then
      RepaintText(EditRect);
  end;
  FAlienFocus := false;
  {$ifndef CLX_USED}
  if Flat and (not IsThemeApplied) then DrawFlatBorder;
  {$endif}
end; { WMSetFocus }

procedure TCustomElEdit.WMKillFocus(var Msg : TWMKillFocus); { private }
begin
  if FHasCaret then
  begin
    DestroyCaret;
    FHasCaret := false;
    //if Transparent then RepaintText(EditRect);
  end;
  if Msg.FocusedWnd = 0 then
    FAlienFocus := true;
  inherited;
  if not HandleAllocated then exit;
  if FHideSelection then RepaintText(EditRect);
  {$ifndef CLX_USED}
  if Flat and (not IsThemeApplied) then DrawFlatBorder;
  {$endif}
end; { WMKillFocus }

procedure TCustomElEdit.WMEnable(var Msg : TMessage); { private }
begin
  inherited;
  if Msg.WParam = 0 then
  begin
    if FHasCaret then
    begin
      DestroyCaret;
      FHasCaret := false;
    end
  end
  else
  begin
    if Focused then
    begin
      MakeCaret;
    end;
  end;
end; { WMEnable }

procedure TCustomElEdit.WMSize(var Msg : TWMSize); { private }
var
  ACaretX, ACaretY, P, PO: integer;
begin
  inherited;
  if HandleAllocated then
  begin
    FElEditList.CaretToParagraph(CaretX, CaretY, P, PO);
    FElEditList.Reformat;
    FElEditList.ReCount(0);
    FElEditList.CaretFromParagraph(P, PO, ACaretX, ACaretY);
    CaretY := ACaretY;
    CaretX := ACaretX;
    FSelStartX := CaretX;
    FSelStartY := CaretY;
    if ((not FMultiline) and AutoSize) and (ComponentState * [csLoading, csReading, csDestroying] = []) then
      AdjustHeight;
    SetEditRect(ClientRect);
  end;
end; { WMSize }


procedure TCustomElEdit.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); { protected }
var
  APnt : TPoint;
  fx, fy: integer;
  FBeg, FEnd: TPoint;
begin
  if (Enabled) and (CanFocus) and (not (csDesigning in ComponentState)) and (GetFocus <> Handle) then
  begin
    FMouseClick := true;
    CaretFromPos(Point(X, Y), FX, FY);
    CaretY := FY;
    CaretX := FX;
    FSelStartX := CaretX;
    FSelStartY := CaretY;
    SetFocus;
    FMouseClick := false;
  end;
  if ssDouble in Shift then
  begin
    CaretFromPos(Point(X, Y), FX, FY);
    CaretY := FY;
    CaretX := FX;
    FBeg := GetPrevWord(CaretXY);
    FEnd := GetNextWord(CaretXY);
    FSelStartX := FBeg.X;
    FSelStartY := FBeg.Y;
    SetSelection(FEnd.X, FEnd.Y);
    CaretXY := FEnd;
    RepaintText(FEditRect);
  end
  else
  if mbLeft = Button then
  begin
    CaretFromPos(Point(X, Y), FX, FY);
    CaretY := FY;
    CaretX := FX;
    if not (ssShift in Shift) then
    begin
      UnSelect;
      FSelStartX := FX;
      FSelStartY := FY;
    end
    else
      SetSelection(CaretX, CaretY);
    TriggerSelectionChangeEvent;
    RepaintText(EditRect);
    FSelecting := true;
  end;
  if mbRight = Button then
  begin
    APnt := Point(X, Y);
    APnt := ClientToScreen(APnt);
    if Assigned(PopupMenu) then
    begin
      PopupMenu.PopupComponent := Self;
      inherited;
      if PopupMenu.AutoPopup then
        PopupMenu.Popup(APnt.X, APnt.Y);
      exit;
    end;
  end;
  SetCapture(Handle);
  inherited;
end; { MouseDown }

procedure TCustomElEdit.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); { protected }
begin
  ReleaseCapture;
  if not FSelecting then
  begin
  end
  else
  begin
    FSelecting := false;
  end;
  inherited;
end; { MouseUp }

procedure TCustomElEdit.MouseMove(Shift : TShiftState; X, Y : Integer); { protected }
var
//  FNewChar : integer;
  FX, FY: integer;
begin
  if (FUseCustomScrollBars and Multiline) then
    Perform(WM_SETCURSOR, Handle, WM_MOUSEMOVE shl 16 or HTCLIENT);
  if {(FSelected) and }(ssLeft in Shift) then
  begin
    FSelected := true;
    CaretFromPos(Point(X, Y), FX, FY);
    SetSelection(FX, FY);
    CaretY := FY;
    CaretX := FX;
    TriggerSelectionChangeEvent;
    RepaintText(FEditRect);
  end
  else
  begin

  end;
  inherited;
end; {MouseMove}

procedure TCustomElEdit.KeyPress(var Key : Char); { protected }
var
  ANewStr: TElFString;
  SKey : Char;
  FCarY, FCarX, P, PO: integer;
  {$ifdef ELPACK_UNICODE}
  S1 : WideString;
  {$else}
  S1 : string;
  {$endif}
  FTStr: TElFString;
  FPos: TPoint;
begin
  SKey := Key;
  inherited KeyPress(SKey);

  if (Integer(Key) in [VK_BACK, VK_RETURN, VK_ESCAPE, VK_TAB, 127]) or
     (Key = #0) or
     (FSelecting) then
    exit;

  if FReadOnly or (SKey = #0) or (SKey < #32) or ((Length(Text) >= FMaxLength) and (FMaxLength > 0)) then
  begin
    {$ifdef ELPACK_UNICODE}
    FKeys := '';
    {$endif}
    exit;
  end;
  Key := SKey;

  if CharCase <> eecNormal then
  begin
    {$ifndef ELPACK_UNICODE}
    if CharCase = eecUppercase then
      Key := Uppercase(Key)[1]
    else
      Key := Lowercase(Key)[1];
    {$else}
    if CharCase = eecUppercase then
      Key := Char(WideUppercase(Key)[1])
    else
      Key := Char(WideLowercase(Key)[1]);
    {$endif}
  end;
  if FSelected then
    DeleteSelection;

  if RTLContent then
  begin
//    LineFromChar(FText, FSelStart, fl, fc);
//    S := FElEditList.ParagraphStrings[fl];
//    {$ifdef ELPACK_UNICODE}
//    S1 := S;
//    if (IsWinNTUp) and (FKeyDown) then
//    begin
//      WideInsert(FKeys, S1, fc + 1);
//      FTStr := FKeys;
//      FKeyDown := false;
//    end
//    else
//    begin
//      WideInsert(Key, S1, fc + 1);
//      FTStr := Key;
//    end;
//    S := S1;
//    {$else}
//    Insert(Key, S, fc + 1);
//    {$endif}
//    FElEditList.Put(fl, S);
  end
  else
  begin
    FElEditList.CaretToParagraph(CaretX, CaretY, P, PO);
    S1 := FElEditList.ParagraphStrings[CaretY];
    {$ifdef ELPACK_UNICODE}
    if (IsWinNTUp) and (FKeyDown) then
    begin
      WideInsert(FKeys, S1, CaretX + 1);
      FTStr := FKeys;
      FKeyDown := false;
    end
    else
    begin
      WideInsert(Key, S1, CaretX + 1);
      FTStr := Key;
    end;
    {$else}
    Insert(Key, S1, CaretX + 1);
    {$endif}
    FElEditList.ParagraphStrings[CaretY] := S1;
  end;

  if (FMaxLength = 0) or (Length(ANewStr) <= FMaxLength) then
  begin
    FPos := CaretXY;
    FElEditList.CaretFromParagraph(P, PO + 1, FCarX, FCarY);
    CaretY := FCarY;
    CaretX := FCarX;
    FSelStartY := CaretY;
    FSelStartX := CaretX;
    FUndo.AddAction(atInsert, FPos, CaretXY, FTStr);
    Modified := true;
    Change;
    TriggerSelectionChangeEvent;
    InvalidateRect(Handle, @FEditRect, false);
  end
end;

procedure TCustomElEdit.SetText(newValue: TElFString);
begin
  if (ComponentState * [csLoading, csReading, csDestroying] <> []) then
  begin
    FElEditList.Text := newValue;
    exit;
  end;
  if (FElEditList.Text <> newValue) then
  begin

    {$ifndef ELPACK_UNICODE}
    if FCharCase = eecUppercase then
      NewValue := Uppercase(NewValue)
    else
    if FCharCase = eecLowercase then
      NewValue := Lowercase(NewValue);
    {$else}
    if FCharCase = eecUppercase then
      NewValue := WideUppercase(NewValue)
    else
    if FCharCase = eecLowercase then
      NewValue := WideLowercase(NewValue);
    {$endif}

    if FElEditList.ParagraphCount > 0 then 
      FUndo.AddAction(atDelete, Point(0, 0), Point(FElEditList.ParagraphCount - 1,
                  Length(FElEditList.ParagraphStrings[FElEditList.ParagraphCount - 1])),
                  newValue);

    if HandleAllocated then
      SendMessage(Handle, WM_SETTEXT,
      {$ifdef ELPACK_UNICODE}
      1, Integer(PWideChar(newValue)))
      {$else}
      0, Integer(PChar(newValue)))
      {$endif}
    else
    begin
      if not NotifyUserChangeOnly then
        Change;
      FElEditList.Text := newValue;
      if not NotifyUserChangeOnly then Change;
    end;
    SetScrollBarsInfo;
  end; { if }
end; { SetText }

function TCustomElEdit.GetNextWord(ACaret: TPoint) : TPoint;
var
  P: {$ifdef ELPACK_UNICODE}PWideChar{$else}PChar{$endif};
  j : integer;
  CharNum: integer;
  wasbl : boolean;
  PA, PI: integer;
begin
  FElEditList.CaretToParagraph(ACaret.X, ACaret.Y, PA, PI);
  CharNum := PI;
  {$ifdef ELPACK_UNICODE}
  P := PWideChar(FElEditList.Strings[PA]);
  {$else}
  P := PChar(FElEditList.Strings[PA]);
  {$endif}
  inc(P, CharNum);
  wasbl := (p^ in [ElFTab, ElFSpace]);
  if p^ in [ElFCR, ElFLF] then
  begin
    Result := Point(CharNum + 1, PI);
    exit;
  end;
  j := 0;
  while (p^ <> #13) and (p^ <> #10) do
  begin
    if P^ = #0 then
    begin
      if ACaret.Y < FElEditList.ParagraphCount - 1 then
        Result := Point(0, ACaret.Y + 1)
      else
        Result := Point(CharNum, ACaret.Y);
      exit;
    end;
    if ((P + 1)^ = #0) then
    begin
      FElEditList.CaretFromParagraph(PA, Length(FElEditList.Strings[PA]), ACaret.X, ACaret.Y);
      Result := ACaret;
      exit;
    end;
    if wasbl xor (p^ in [ElFTab, ElFSpace]) then
    begin
      while (p^ in [ElFTab, ElFSpace]) do
      begin
        inc(j);
        inc(p);
      end;
      FElEditList.CaretFromParagraph(PA, CharNum + j, ACaret.X, ACaret.Y);
      Result := ACaret;
      exit;
    end;
    inc(j);
    inc(p);
  end;
  Result := Point(CharNum + j, ACaret.Y);
end;

function TCustomElEdit.GetPrevWord(ACaret: TPoint): TPoint;
var
  p : {$ifdef ELPACK_UNICODE}PWideChar{$else}PChar{$endif};
  i, j: integer;
  CharNum: integer;
  wasbl : boolean;
  PA, PI: integer;
begin
  FElEditList.CaretToParagraph(ACaret.X, ACaret.Y, PA, PI);
  CharNum := PI;
  {$ifdef ELPACK_UNICODE}
  P := PWideChar(FElEditList.Strings[PA]);
  {$else}
  P := PChar(FElEditList.Strings[PA]);
  {$endif}
  inc(P, CharNum);
  wasbl := (p^ in [ElFTab, ElFSpace]);
  if p^ in [ElFCR, ElFLF] then
  begin
    result := Point(CharNum, ACaret.Y);
    exit;
  end;
  i := CharNum;
  j := 0;
  while (p^ <> ElFCR) and (p^ <> ElFLF) do
  begin
    if i < 0 then
    begin
      if PA > 0 then
        FElEditList.CaretFromParagraph(PA - 1, Length(FElEditList.Strings[PA - 1]), ACaret.X, ACaret.Y)
      else
        ACaret := Point(0, 0);

      Result := ACaret;
      exit;
    end;
    if wasbl xor (p^ in [ElFTab, ElFSpace]) then
    begin
      while (not (p^ in [ElFTab, ElFSpace])) do
      begin
        dec(i);
        inc(j);
        dec(p);
        if ((i < 0) or (p^ = ElFCR) or (p^ = ElFLF)) then break;
      end;
      FElEditList.CaretFromParagraph(PA, CharNum - j + 1, ACaret.X, ACaret.Y);
      Result := ACaret;
      exit;
    end;
    dec(i);
    inc(j);
    dec(p);
    if i = 0 then
    begin
      FElEditList.CaretFromParagraph(PA, 0, ACaret.X, ACaret.Y);
      Result := ACaret;
      exit;
    end;
  end;
  Result := Point(CharNum - j + 1, ACaret.Y);
end;

function TCustomElEdit.ConvertBreaksFormat(Text : TElFString) : TElFString;
var
  i : integer;
  {$ifdef ELPACK_UNICODE}
  S : WideString;
  {$endif}
begin
  result := Text;
  i := 1;
  while i < Length(result) do
  begin
    if Result[i + 1] = #10 then
    begin
      {$ifdef ELPACK_UNICODE}
      S := Result;
      WideInsert(#13, S, i + 1);
      result := S;
      {$else}
      insert(#13, Result, i + 1);
      {$endif}
      inc(i, 2);
    end
    else if Result[i + 1] = #13 then
    begin
      {$ifdef ELPACK_UNICODE}
      S := Result;
      WideInsert(#10, S, i + 2);
      result := S;
      {$else}
      insert(#10, Result, i + 2);
      {$endif}
      inc(i, 2);
    end
    else
      inc(i);
  end;
end; {ConvertBreaksFormat}

procedure TCustomElEdit.WMGetDlgCode(var Msg : TWMGetDlgCode); { private }
begin
  Msg.Result := DefWindowProc(Handle, Msg.Msg, TMessage(Msg).wParam, TMessage(Msg).lParam);
  Msg.Result := (Msg.Result and (not DLGC_WANTALLKEYS)) or DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_HASSETSEL;
  if WantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
  if HandleDialogKeys {or Multiline} then
    Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end; { WMGetDlgCode }

{$ifdef ELPACK_UNICODE}
// Вспомогательные функции для преобразования символов
function KeyToUnicode(const Key: String): WideString;
var KeyboardCodePage: integer;
    Buf: String;
    LenBuf: Integer;
    LenRes: integer;
    Locale: LCID;
// const LOCALE_IDEFAULTANSICODEPAGE     = $00001004;
begin
  // Получаем правильную локаль
  Locale := GetKeyboardLayout(0) and $FFFF;
  LenBuf := GetLocaleInfo(Locale, LOCALE_IDEFAULTANSICODEPAGE, nil, 0);
  SetLength(Buf, LenBuf);
  GetLocaleInfo(Locale, LOCALE_IDEFAULTANSICODEPAGE, PChar(Buf), LenBuf);
  KeyboardCodePage := StrToIntDef(Buf, GetACP);

  // Получаем длину результирующей строки
  LenRes := MultiByteToWideChar(KeyboardCodePage, MB_PRECOMPOSED or MB_USEGLYPHCHARS, PChar(Key), Length(Key), nil, 0);
  SetLength(Result, LenRes);
  MultiByteToWideChar(KeyboardCodePage, MB_PRECOMPOSED or MB_USEGLYPHCHARS, PChar(Key), Length(Key), PWideChar(Result), LenRes);
end;

(*
function UniCodeToAnsi(Charset: integer; const S: WideString): AnsiString;
var
  DefChar : Char;
  Flags : Integer;
  Len : integer;
  Res : integer;
begin
  Flags := WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  DefChar := '?';
  Len := Length(s) div 2;
  Res := WideCharToMultiByte(Charset, Flags, PWideChar(s), Len, nil, 0, @DefChar, nil);
  if (Res = 0) and (Charset <> CP_ACP) then
  begin
    Charset := CP_ACP;
    Res := WideCharToMultiByte(Charset, Flags, PWideChar(s), Len, nil, 0, @DefChar, nil);
  end;
  if Res <> 0 then
  begin
    SetLength(Result, Res * 2);
    WideCharToMultiByte(Charset, Flags, PWideChar(s), Len, PAnsiChar(Result), Res * 2, @DefChar, nil);
  end
  else
  begin
    SetLength(Result, Len);
    FillChar(PChar(Result)^, Len, '?');
  end;
end;
*)

procedure TCustomElEdit.WMImeStartComposition(var Message: TMessage);
var
  IMC: HIMC;
  LogFont: TLogFont;
  CF: TCompositionForm;
begin
  IMC := ImmGetContext(Handle);
  if IMC <> 0 then
  begin
    // Заполняем структуру TLogFont по хэндлу шрифта холста
    GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
    // Устанавливаем шрифт для использования в IME
    ImmSetCompositionFont(IMC, @LogFont);
    // Устанаваливаем режим для задания размера - задание Rect
    CF.dwStyle := CFS_RECT;
    CF.rcArea  := EditRect;
    inc(CF.rcArea.Left, LeftMargin);
    dec(CF.rcArea.Right, RightMargin);
    // Текущую позицию
    CF.ptCurrentPos := PosFromCaret(CaretX, CaretY);

    ImmSetCompositionWindow(IMC, @CF);
    // Освобождаем контекст
    ImmReleaseContext(Handle, IMC);
  end;
  inherited;
end;

type
  TIMECompositionStringProc = function(hImc: HIMC; dWord1: DWORD; lpBuf: pointer; dwBufLen: DWORD): Longint; stdcall;

procedure TCustomElEdit.WMImeComposition(var Message: TMessage);
var
  IMC: HIMC;
  S: String;
  SNT: WideString;
  Size: Integer;
  StrEnd: PWideChar;
  ImmGetCompositionStringW: TIMECompositionStringProc;
  fx, fy: integer;
begin
  // Закончили ввод слова?
  if ((Message.LParam and GCS_RESULTSTR) <> 0) then
  begin
    IMC := ImmGetContext(Handle);
    if IMC<>0 then
    begin
      // Работаем в NT, 2000 или XP?
      if IsWinNTUp then
      begin
        try
          SNT := '';
          // Получаем размер строки для последующего использования
          if GetModuleHandle('IMM32') <> 0 then
          begin
            ImmGetCompositionStringW := GetProcAddress(GetModuleHandle('IMM32'), 'ImmGetCompositionStringW');
            if @ImmGetCompositionStringW <> nil then
            begin
              // Получаем размер строки для последующего использования
              Size := ImmGetCompositionStringW(IMC, GCS_RESULTSTR, nil, 0);
              SetLength(SNT, Size);
              FillWideChar(PWideChar(SNT)^, Size, #0);
              // Получаем результирующую строку.
              ImmGetCompositionStringW(IMC, GCS_RESULTSTR, PWideChar(SNT), Size);
            end;
          end;
        finally
          ImmReleaseContext(Handle, IMC);
        end;
        // Нулевые символы удаляем
        StrEnd := WideStrScan(PWideChar(SNT), WideChar(#0));
        if StrEnd <> nil then
          SetLength(SNT, StrEnd - PWideChar(SNT));
      end
      else
      begin
        // IME support for Win95-98
        // Unfortunately, should properly work not for all versions
        // (you'll get a line of '?')
        S := '';
        try
          Size := ImmGetCompositionStringA(IMC, GCS_RESULTSTR, nil, 0);
          SetLength(s, Size);
          ImmGetCompositionStringA(IMC, GCS_RESULTSTR, PChar(s), Size);
        finally
          ImmReleaseContext(Handle, IMC);
        end;
        SNT := KeyToUnicode(s);
      end;

      if FCharCase = eecUppercase then
        SNT := WideUppercase(SNT)
      else
      if FCharCase = eecLowercase then
        SNT := WideLowercase(SNT);

      if FSelected then
        DeleteSelection;

      fx := CaretX;
      fy := CaretY;

      FElEditList.InsertText(fx, fy, SNT);

      CaretY := fy;
      CaretX := fx;
      FSelStartX := fx;
      FSelStartY := fy;
      RepaintText(FEditRect);
      Message.Result := 0;
    end
  end
  else
    inherited;
end;
{$endif}

function TCustomElEdit.GetSelectedText : TElFString;
var
  bP, bPO: integer;
  eP, ePO: integer;
  i: integer;
  // fast;
  L, Size, Count: Integer;
  {$ifdef ELPACK_UNICODE}
  P: PWideChar;
  S: WideString;
  {$else}
  P: PChar;
  S: String;
  {$endif}
begin
   // {!}to do fast getseltext
   FElEditList.CaretToParagraph(FSelFirstX, FSelFirstY, bP, bPO);
   FElEditList.CaretToParagraph(FSelLastX, FSelLastY, eP, ePO);
   if eP = bP then
     {$ifdef ELPACK_UNICODE}
     Result := WideCopy(FElEditList.FParagraphs.Items[bP].Text, bPO + 1, (ePO - bPO))
     {$else}
     Result := Copy(FElEditList.FParagraphs.Items[bP].Text, bPO + 1, (ePO - bPO))
     {$endif}
   else
   begin
     {$ifdef ELPACK_UNICODE}
     Result := WideCopy(FElEditList.FParagraphs.Items[bP].Text, bPO + 1,
                        Length(FElEditList.FParagraphs.Items[bP].Text)) + ElFCRLF;
     {$else}
     Result := Copy(FElEditList.FParagraphs.Items[bP].Text, bPO + 1,
                    Length(FElEditList.FParagraphs.Items[bP].Text)) + ElFCRLF;
     {$endif}

     Size := Length(Result);
     Count := Size;
     inc(Size, ePO);
     for I := bP + 1 to eP - 1 do
       Inc(Size, Length(FElEditList.FParagraphs.Items[i].Text) + 2);
     SetLength(Result, Size);
     {$ifdef ELPACK_UNICODE}
     P := PWideChar(Result);
     {$else}
     P := PChar(Result);
     {$endif}
     inc(P, Count);
     for I := bP + 1 to eP - 1 do
     begin
       S := FElEditList.FParagraphs.Items[i].Text;
       L := Length(S);
       if L <> 0 then
       begin
         {$ifdef ELPACK_UNICODE}
         WideMove(S[1], P^, L);
         {$else}
         Move(S[1], P^, L);
         {$endif}
         Inc(P, L);
       end;
       P^ := #13;
       Inc(P);
       P^ := #10;
       Inc(P);
     end;
     {$ifdef ELPACK_UNICODE}
     S := WideCopy(FElEditList.FParagraphs.Items[eP].Text, 1, ePO);
     {$else}
     S := Copy(FElEditList.FParagraphs.Items[eP].Text, 1, ePO);
     {$endif}
     L := Length(S);
     if L > 0 then
     begin
       {$ifdef ELPACK_UNICODE}
       WideMove(S[1], P^, L);
       {$else}
       Move(S[1], P^, L);
       {$endif}
       // Inc(P, L);
     end;
   end;
end;

destructor TCustomElEdit.Destroy;
begin
  if FHasCaret then
    DestroyCaret();
  FHasCaret := false;
  {$ifdef ELPACK_COMPLETE}
  FImgFormChLink.Free;
  {$endif}
//  FElEditList.FParagraphs.Items[0].Free;
  FElEditList.Free;
  FBackground.Free;
  scbVert.Free;
  scbHorz.Free;
  FVertScrollBarStyles.Free;
  FHorzScrollBarStyles.Free;
  FUndo.Free;
  FRedo.Free;
  inherited;
end; { Destroy }

constructor TCustomElEdit.Create(AOwner : TComponent);
begin
  NotifyUserChangeOnly := true;
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csCaptureMouse, csOpaque] + [csFixedHeight];
  FMultiline := false;
  FlagEdit := false;
  FEnd := false;
  Ctl3D := true;
  SetLeftMargin(1);
  SetRightMargin(5);
  FAlignBottom := true;
  if not FAlignBottom then
    FTopMargin := 1;
  Width := 200;
  Height := 100;
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChanged;
  // Height := Abs(Font.Height) + GetSystemMetrics(SM_CYBORDER) * 2 + 4;
  FLineHeight := Abs(Font.Height) + 2;
  FEditRect.Right := 100 - GetSystemMetrics(SM_CXBORDER);
  FEditRect.Bottom := Height - GetSystemMetrics(SM_CYBORDER);
  FTabSpaces := 4;
  FTabString := '    ';
  FTopLine := 0;
  FElEditList := TElEditStrings.Create;
  FAutoSize := true;
  FAlienFocus := false;
  FElEditList.FElEdit := Self;

//  SetScrollBarsInfo;
  FBorderStyle := bsSingle;
  ParentColor := false;
  Color := clWindow;
  Cursor := crIBeam;
  MoveCaret(0);
  FActiveBorderType := fbtSunken;
  FInactiveBorderType := fbtSunkenOuter;
  FModified := False;
  TabStop := true;
  {$ifdef MSWINDOWS}
  FBorderSides := AllBorderSides;
  {$endif}
  {$ifdef ELPACK_COMPLETE}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  FHideSelection := true;
  FUseCustomScrollBars := true;
  scbVert := TElScrollBar.Create(nil);
  scbHorz := TElScrollBar.Create(nil);
  with scbVert do
  begin
    Parent := Self;
    Kind := sbVertical;
    OnScroll := OnVScroll;
    Ctl3D := false;
    TabStop := false;
    Width := GetSystemMetrics(SM_CXHTHUMB);
    FVertScrollBarStyles := TElScrollBarStyles.Create(scbVert, Self);
    FVertScrollBarStyles.ThumbMode := etmAuto;
    FVertScrollBarStyles.OnChange := SBChanged;
    Visible := (FUseCustomScrollBars and Multiline) and (ScrollBars in [ssVertical, ssBoth]);
  end;
  dec(FEditRect.Right, FVertScrollBarStyles.Width - 1);
  with scbHorz do
  begin
    Parent := Self;
    OnScroll := OnHScroll;
    Ctl3D := false;
    TabStop := false;
    Height := GetSystemMetrics(SM_CYVTHUMB);
    FHorzScrollBarStyles := TElScrollBarStyles.Create(scbHorz, Self);
    FVertScrollBarStyles.ThumbMode := etmAuto;
    FHorzScrollBarStyles.OnChange := SBChanged;
    Visible := (FUseCustomScrollBars and Multiline) and (ScrollBars in [ssHorizontal, ssBoth]);
  end;
  dec(FEditRect.Bottom, FHorzScrollBarStyles.Width - 1);
//  AdjustHeight;
  SetScrollBarsInfo;
  {$ifdef ELPACK_UNICODE}
  ToUnicode := GetProcAddress(GetModuleHandle('USER32'), 'ToUnicode');
  CaretX := 0;
  CaretY := 0;
  {$endif}
  FUndo := TElActionList.Create;
  FRedo := TElActionList.Create;
end; { Create }

procedure TCustomElEdit.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
end;

{$ifdef ELPACK_COMPLETE}
procedure TCustomElEdit.SetImageForm(newValue : TElImageForm);
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
      FImgForm.RegisterChanges(FImgFormChLink);
    if HandleAllocated then
    begin
      {$ifdef MSWINDOWS}
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

procedure TCustomElEdit.SetUseBackground(const Value: boolean);
begin
  if FUseBackground <> Value then
  begin
    FUseBackground := Value;
    {$ifdef MSWINDOWS}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

procedure TCustomElEdit.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    if not UseCustomScrollBars or not Multiline then
      inherited;

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
  begin
    if (not UseCustomScrollBars) or (not Multiline) then
    begin
      inherited;
      if not (ebsLeft in BorderSides) then
        dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CXEDGE));
      if not (ebsTop in BorderSides) then
        dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CYEDGE));
      if not (ebsRight in BorderSides) then
        Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CXEDGE));
      if not (ebsBottom in BorderSides) then
        Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CYEDGE));
    end
    else
    begin
      if BorderStyle = bsSingle then
      begin
        if (ebsLeft in BorderSides) then
          inc(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CXEDGE));
        if (ebsTop in BorderSides) then
          inc(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CYEDGE));
        if (ebsRight in BorderSides) then
          dec(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CXEDGE));
        if (ebsBottom in BorderSides) then
          dec(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CYEDGE));
      end;
    end;
  end;
  // Message.Result := WVR_REDRAW;
end;

procedure TCustomElEdit.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TCustomElEdit.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    {$ifdef MSWINDOWS}
    if Focused or FMouseOver then DrawFlatBorder;
    {$endif}
  end;
end;

procedure TCustomElEdit.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    {$ifdef MSWINDOWS}
    if HandleAllocated then
      if Flat then
        Invalidate
      else
        RecreateWnd;
    SetScrollBarsInfo;
    {$endif}
  end;
end;

procedure TCustomElEdit.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    {$ifdef MSWINDOWS}
    if not Focused and not FMouseOver then DrawFlatBorder;
    {$endif}
  end;  
end;

{$ifdef ELPACK_COMPLETE}
procedure TCustomElEdit.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;

procedure TCustomElEdit.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
  
end;

procedure TCustomElEdit.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;
{$endif}

procedure TCustomElEdit.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$ifdef ELPACK_COMPLETE}
    if AComponent = FImgForm then
    begin
      ImageForm := nil;
    end;
    {$endif}
  end;
end;

procedure TCustomElEdit.DrawBackground(DC: HDC; R: TRect);
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

procedure TCustomElEdit.DrawFlatBorder;
var
  DC : HDC;
  R  : TRect;
  b  : boolean;
  BS : TElFlatBorderType;
  AColor : TColor;
const BordersFlat : array[boolean] of Integer = (0, WS_BORDER);
      Borders3D : array[boolean] of Integer = (0, WS_EX_CLIENTEDGE);
begin
  if not HandleAllocated then exit;
  R := Rect(0, 0, Width, Height);
  DC := GetWindowDC(Handle);
  try
    if IsThemeApplied (*and (BorderStyle = bsSingle)*) then
    begin
      (*
      R1 := ClientRect;
      R1.TopLeft := Parent.ScreenToClient(ClientToScreen(R1.TopLeft));

      ax := Left - R1.Left;
      ay := Top  - R1.Top;

      R1 := ClientRect;
      OffsetRect(R1, -ax, -ay);

      with R1 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      DrawThemeBackground(Theme, DC, 0, 0, R, nil);
      *)
      RedrawWindow(Handle, @R, 0, RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);
      exit;
    end
    else
    begin
      if BorderStyle = bsSingle then
      begin
        b := Focused or FMouseOver;
        if b then
           BS := FActiveBorderType
        else
           BS := FInactiveBorderType;
        if Focused or FMouseOver then
          AColor := LineBorderActiveColor
        else
          AColor := LineBorderInactiveColor;
        if not Flat then
          BS := fbtSunken;
        DrawFlatFrameEx2(DC, R, AColor, Color, b, Enabled, FBorderSides, BS);
      end;
      if not IsThemeApplied then
      begin
        if FFlatFocusedScrollBars or not (Focused or FMouseOver) then
          DrawFlatScrollbars(Handle, DC, R,
            (Focused or FMouseOver) and not FFlatFocusedScrollBars,
            ScrollBars, False, False, False,
          GetWindowLong(Handle, GWL_STYLE) or BordersFlat[(not Ctl3D) and (BorderStyle = bsSingle)],
          GetWindowLong(Handle, GWL_EXSTYLE) or Borders3D[Ctl3D and (BorderStyle = bsSingle)]);
      end;
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

type
  THackWinControl = class(TWinControl);

procedure TCustomElEdit.DrawParentControl(DC: HDC);
var
  SavedDC: integer;
  P: TPoint;
  R: TRect;
begin
  if Assigned(Parent) then
  begin
    SavedDC := SaveDC(DC);
    try
      P := Parent.ScreenToClient(ClientOrigin);
      MoveWindowOrg(DC, -P.X, -P.Y);
      R := EditRect;
      with r do
        IntersectClipRect(DC, Left, Top, Right, Bottom);
      Parent.Perform(WM_ERASEBKGND, DC, 0);
      Parent.Perform(WM_PAINT, DC, 0);
      THackWinControl(Parent).PaintControls(DC, nil);
    finally
      RestoreDC(DC, SavedDC);
    end;
  end;
end;

function TCustomElEdit.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

procedure TCustomElEdit.SetWordWrap(Value: boolean);
begin
  if (FWordWrap <> Value) then
  begin
    FWordWrap := Value;
    Invalidate;
    SetScrollBarsInfo;
  end;
end;

procedure TCustomElEdit.SetScrollBars(const Value: TScrollStyle);
begin
    if (FScrollBars <> Value) {and (FMultiline){and ((Value <> ssNone) and (FMultiline)))} then
    begin
      if FMultiline then
        FScrollBars := Value
      else
        FScrollBars := ssNone;
      if HandleAllocated then
      begin
        if (ComponentState * [csLoading, csReading] = []) then
          RecreateWnd;
        SetScrollBarsInfo;
      end;
    end;
end;

procedure TCustomElEdit.WMVScroll(var Msg : TWMScroll);
var
  b : boolean;
  sc: TElScrollCode;
  sp: integer;
begin
 b := false;
 sc := escTrack;
 case Msg.ScrollCode of
   SB_LINEDOWN:
   begin
     sc := escLineDown;
     sp := FTopLine + 1;
   end;
   SB_LINEUP:
   begin
     sc := escLineUp;
     sp := FTopLine - 1;
   end;
   SB_PAGEUP:
   begin
     sc := escPageUp;
     sp := FTopLine - FLinesInRect;
   end;
   SB_PAGEDOWN:
   begin
     sc := escPageDown;
     sp := FTopLine + FLinesInRect;
   end;
   SB_THUMBTRACK:
   begin
     sc := escTrack;
     sp := Msg.Pos;
   end;
   SB_THUMBPOSITION:
   begin
     sc := escPosition;
     sp := Msg.Pos;
   end;
   SB_TOP:
   begin
     sc := escTop;
     sp := 0;
   end;
   SB_BOTTOM:
   begin
     sc := escBottom;
     sp := FElEditList.ParagraphCount - FLinesInRect;
   end;
   SB_ENDSCROLL:
   begin
     sc := escEndScroll;
     sp := FTopLine;
   end;
 end;
 if (sp >= 0) and (sp <= FElEditList.ParagraphCount - 1) then
   OnVScroll(Self, sc, sp, b);
end;

procedure TCustomElEdit.WMHScroll(var Msg : TWMScroll);
var
  b : boolean;
  sc: TElScrollCode;
  sp: integer;
begin
 b := false;
 sc := escTrack;
 case Msg.ScrollCode of
   SB_LINEDOWN:
   begin
     sc := escLineDown;
     sp := FLeftChar + 5;
   end;
   SB_LINEUP:
   begin
     sc := escLineUp;
     sp := FLeftChar - 5;
   end;
   SB_PAGEUP:
   begin
     sc := escPageUp;
     sp := FLeftChar - ClientWidth div 2;
   end;
   SB_PAGEDOWN:
   begin
     sc := escPageDown;
     sp := FLeftChar + ClientWidth div 2;
   end;
   SB_THUMBPOSITION:
   begin
     sc := escPosition;
     sp := Msg.Pos;
   end;
   SB_THUMBTRACK:
   begin
     sc := escTrack;
     sp := Msg.Pos;
   end;
   SB_LEFT:
   begin
     sc := escTop;
     sp := 0;
   end;
   SB_RIGHT:
   begin
     sc := escBottom;
     sp := FElEditList.FMaxLen - scbHorz.Page - 1;
   end;
   SB_ENDSCROLL:
   begin
     sc := escEndScroll;
     sp := FLeftChar;
   end;
 end;
 if (sp >= 0) and (sp <= FElEditList.FMaxLen) then
   OnHScroll(Self, sc, sp, b);
end;

procedure TCustomElEdit.WMInputLangChange(var Msg: TMessage);
begin
  FCharSet := Msg.WParam;
  FKeybLayout := HKL(Msg.LParam);
end;

procedure TCustomElEdit.WMCommand(var Msg: TWMCommand);
begin
  case Msg.ItemID of
  // Select All in Windows 9x/ME - 1025, in NT/2k/XP - 177
  177, 1025: SelectAll;
  ID_PASTE: PasteFromClipboard;
  ID_CUT: CutToClipboard;
  ID_COPY: CopyToClipboard;
  ID_DELETE:
    begin
      if FSelected then DeleteSelection
    end;
  ID_UNDO: Undo;
  end;
end;

procedure TCustomElEdit.WMMouseWheel(var Msg: TWMMouseWheel);
var
  Dy : integer;
  sl : integer;
begin
  if IsWinNT or IsWin98 then
     SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @sl, SPIF_SENDCHANGE)
  else
     sl := 3;
  if sl = 0 then sl := 1;
  Dy := Msg.WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  if Dy <> 0 then
  begin
    scbVert.Position := TopLine - Dy;
    TopLine := scbVert.Position;
    scbVert.Position := FTopLine;
    InvalidateRect(Handle, @FEditRect, false);
  end;
end;

procedure TCustomElEdit.WMCaptureChanged(var Msg: TMessage);
begin
  inherited;
end;

procedure TCustomElEdit.SetLeftChar(Value: integer);
begin
  if (FLeftChar <> Value) and (not FWordWrap) then
  begin
    FLeftChar := Value;
    if FLeftChar < 0 then FLeftChar := 0;
    if FLeftChar > FElEditList.FMaxLen then FLeftChar := FElEditList.FMaxLen;

    if FHasCaret then SetCaretPosition(CaretX, CaretY);
    Invalidate;
    SetScrollBarsInfo;
  end;
end;

procedure TCustomElEdit.SetTopLine(const Value: integer);
begin
  if FTopLine <> Value then
  begin
    if Value < 0 then
      if FTopLine = 0 then exit
                      else FTopLine := 0
    else
      if (Value + FLinesInRect) > FElEditList.ParagraphCount then
      begin
        if (FTopLine = FElEditList.ParagraphCount - FLinesInRect) and (FTopLine > 0) then exit;
        //if FTopLine > 0 then
        FTopLine := Min(Value, FElEditList.ParagraphCount - FLinesInRect);
        if FTopLine < 0 then
          FTopLine := 0;
        //else
        //  FTopLine := 0;
      end
      else
        FTopLine := Value;

    if FHasCaret then SetCaretPosition(CaretX, CaretY);
    Invalidate;
    SetScrollBarsInfo;
  end;
end;

procedure TCustomElEdit.SetScrollBarsInfo;
var
  FMin: integer;
  si  : TScrollInfo;
  FCharsMax: integer;
  FMaxStr: TElFString;
  VScrollRect: TRect;
  HScrollRect: TRect;
begin
  if not HandleAllocated then exit;
  // Set Scrollbars properties
  // VScroll
  if (FUseCustomScrollBars and Multiline) and (ScrollBars in [ssVertical, ssBoth]) then
  begin
    if ScrollBars in [ssHorizontal, ssBoth] then
      VScrollRect := Rect(ClientWidth - FVertScrollBarStyles.Width, 0, FVertScrollBarStyles.Width, ClientHeight - FHorzScrollBarStyles.Width)
    else
      VScrollRect := Rect(ClientWidth - FVertScrollBarStyles.Width, 0, FVertScrollBarStyles.Width, ClientHeight);
    with VScrollRect do
      scbVert.SetBounds(Left,Top, Right, Bottom);
  end;

  FMin := (FElEditList.ParagraphCount - FLinesInRect);
  if (FMin > 0) then
  begin
    if ScrollBars in [ssVertical, ssBoth] then
    begin
      si.cbSize := SizeOf(Si);
      si.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;
      si.nMin := 0;
      si.nMax := FElEditList.ParagraphCount - 1;
      si.nPage := FLinesInRect;
      si.nPos := TopLine;

      if FUseCustomScrollBars then
      begin
        EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH);
        scbVert.Visible := true;
        scbVert.Enabled := true;
      end
      else
      begin
        EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
        scbVert.Visible := false;
        SetScrollInfo(Handle, SB_VERT, si, true);
      end;
      scbVert.SetScrollInfo(si, true);
    end
    else
      if FUseCustomScrollBars then
        scbVert.Visible := false;
  end
  else
  begin
//    FTopLine := 0;
    if ScrollBars in [ssVertical, ssBoth] then
    begin
      if FUseCustomScrollBars then
      begin
        scbVert.Max := scbVert.Min;
        scbVert.Enabled := false;
        scbVert.Visible := true;
        Invalidate;
      end
      else
      begin
        scbVert.Visible := false;
        EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH);
        si.cbSize := sizeof(Si);
        si.fMask := SIF_DISABLENOSCROLL or SIF_RANGE;
        si.nMin := 0;
        si.nMax := 0;
        SetScrollInfo(Handle, SB_VERT, si, true);
        scbVert.SetScrollInfo(si, true);
      end;
    end
    else
      if FUseCustomScrollBars then
        scbVert.Visible := false;
  end;
  // HScroll
//  if FWordWrap then
//  begin
//    if ScrollBars in [ssHorizontal, ssBoth] then
//    begin
//      if FUseCustomScrollBars then
//      begin
//        if ScrollBars in [ssVertical, ssBoth] then
//          HScrollRect := Rect(0, ClientHeight - FHorzScrollBarStyles.Width, ClientWidth - FVertScrollBarStyles.Width, FHorzScrollBarStyles.Width)
//        else
//          HScrollRect := Rect(0, ClientHeight - FHorzScrollBarStyles.Width, ClientWidth, FHorzScrollBarStyles.Width);
//        with HScrollRect do
//          scbHorz.SetBounds(Left, Top, Right, Bottom);
//        scbHorz.Visible := true;
//        scbHorz.Enabled := false;
//      end
//      else
//        EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH);
//    end
//  end
//  else
  begin
    if ScrollBars in [ssHorizontal, ssBoth] then
    begin
      if FUseCustomScrollBars then
      begin
        if ScrollBars in [ssVertical, ssBoth] then
          HScrollRect := Rect(0, ClientHeight - FHorzScrollBarStyles.Width, ClientWidth - FVertScrollBarStyles.Width, FHorzScrollBarStyles.Width)
        else
          HScrollRect := Rect(0, ClientHeight - FHorzScrollBarStyles.Width, ClientWidth, FHorzScrollBarStyles.Width);
        with HScrollRect do
          scbHorz.SetBounds(Left, Top, Right, Bottom);
      end;

      if FElEditList.FParagraphs.Count > 0 then
        FMaxStr := FElEditList.FMaxStr;
      if {((FElEditList.FIdxMaxLen < FElEditList.ParagraphCount) and }(TextSize(FMaxStr).cx > ((EditRect.Right - FRightMargin) - (EditRect.Left + FLeftMargin)))
         {or (FLeftChar > 0)} then
//      if ((FElEditList.FIdxMaxLen < FElEditList.Count) and (TextSize(FMaxStr).cx > ((EditRect.Right - FRightMargin) - (EditRect.Left + FLeftMargin))))
//         {or (FLeftChar > 0)} then
      begin
        FCharsMax := CharsFitRight((EditRect.Right - FRightMargin) - (EditRect.Left + FLeftMargin), FMaxStr, 0);

        si.cbSize := SizeOf(Si);
        si.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;
        si.nMin := 0;
        si.nMax := FElEditList.FMaxLen;
        si.nPage := FCharsMax - 4;
        si.nPos := FLeftChar;

        if FUseCustomScrollBars then
        begin
          EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH);
          scbHorz.Visible := true;
          scbHorz.Enabled := true;
        end
        else
        begin
          scbHorz.Visible := false;
          EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);
          SetScrollInfo(Handle, SB_HORZ, si, true);
        end;
        scbHorz.SetScrollInfo(si, true);
      end
      else
      begin
        LeftChar := 0;
        if FUseCustomScrollBars then
        begin
          scbHorz.Max := scbHorz.Min;
          scbHorz.Enabled := false;
          scbHorz.Visible := true;
        end
        else
        begin
          scbHorz.Visible := false;
          EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH);
          si.cbSize := sizeof(Si);
          si.fMask := SIF_DISABLENOSCROLL or SIF_RANGE;
          si.nMin := 0;
          si.nMax := 0;
          SetScrollInfo(Handle, SB_HORZ, si, true);
          scbHorz.SetScrollInfo(si, true);
        end;
      end;
    end
    else
      if FUseCustomScrollBars then
        scbHorz.Visible := false;
  end;
  if (Flat and ((not (Focused or FMouseOver)) or (FlatFocusedScrollBars and (not FUseCustomScrollBars) or (not Multiline)))) or IsThemeApplied then
    DrawFlatBorder;
end;

{$ifndef CLX_USED}
procedure TCustomElEdit.CMFontChanged(var Message: TMessage);
{$else}
procedure TCustomElEdit.FontChanged;
{$endif}
begin
  inherited;
  FLineHeight := Abs(Font.Height) + 2;
  FLinesInRect := (FEditRect.Bottom - FTopMargin) div FLineHeight;

  FElEditList.Reformat;
  SetScrollBarsInfo;
  if FHasCaret then
  begin
    DestroyCaret;
    MakeCaret;
    SetCaretPosition(CaretX, CaretY);
    ShowCaret(Handle);
  end;
end;

procedure TCustomElEdit.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    UpdateHeight;
  end;
end;

procedure TCustomElEdit.UpdateHeight;
begin
  if FAutoSize and (BorderStyle = bsSingle) then
    ControlStyle := ControlStyle + [csFixedHeight]
  else
    ControlStyle := ControlStyle - [csFixedHeight];
end;

procedure TCustomElEdit.CreateWnd;
var R : TRect;
begin
  inherited;
  FCharsInView := 0;
  SetScrollBarsInfo;
  UpdateHeight;
  if (not (csLoading in ComponentState)) and (not Multiline) and AutoSize then
    AdjustHeight;
  R := ClientRect;
  SetEditRect(R);
  if Flat or IsThemeApplied then DrawFlatBorder;
end;

procedure TCustomElEdit.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
  {$ifndef CLX_USED}
  Perform(CM_COLORCHANGED, 0, 0);
  {$else}
  ColorChanged;
  {$endif} 
end;

procedure TCustomElEdit.WMGetText(var Message: TMessage);
var
  S: String;
begin
  if Message.wParam = 0 then
  begin
    Message.Result := 0;
  end
  else
  begin
    S := FElEditList.Text;
    Message.Result := Length(S);
    Move(PChar(S)^, PChar(Message.lParam)^, Message.Result + 1);
  end;
end;

procedure TCustomElEdit.SetLines(Value: TElFStrings);
begin
  FElEditList.Text := Value.Text;
end;

function TCustomElEdit.GetLines: TElFStrings;
begin
  Result := FElEditList
end;

procedure TCustomElEdit.SetAlignBottom(Value: boolean);
begin
  if (FAlignBottom <> Value) and (not FMultiLine) then
  begin
    FAlignBottom := Value;
    if FAlignBottom then
      FTopMargin := FEditRect.Bottom - FLineHeight - 2
    else
      FTopMargin := 1;
    if FHasCaret then SetCaretPosition(CaretX, CaretY);
    Invalidate;
  end
  else
  if Multiline then
  begin
    FAlignBottom := Value;
    FTopMargin := 1;
    if FHasCaret then SetCaretPosition(CaretX, CaretY);
    Invalidate;
  end;
end;

procedure TCustomElEdit.SetTopMargin(Value: Integer);
begin
  if (FTopMargin <> Value) and (not FAlignBottom) then
  begin
    FTopMargin := Value;
    Invalidate;
    if FHasCaret then SetCaretPosition(CaretX, CaretY);
  end;
end;

(*
procedure TCustomElEdit.UpdateFrame;
var R : TRect;
begin
  if not HandleAllocated then exit;
  R := Rect(0, 0, Width, Height);
  RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_Frame or rdw_UpdateNow or rdw_NoChildren);
end;
*)

{$ifndef CLX_USED}
procedure TCustomElEdit.CMMouseEnter(var Msg : TMessage);  { private }
{$else}
procedure TCustomElEdit.MouseEnter(AControl: TControl);
{$endif}
begin
  inherited;
  FMouseOver := true;
  DoMouseEnter;
  {$ifndef CLX_USED}
  if (Flat and not Focused) and (not IsThemeApplied) then DrawFlatBorder;
  {$endif}
end;  { CMMouseEnter }

{$ifndef CLX_USED}
procedure TCustomElEdit.CMMouseLeave(var Msg : TMessage);  { private }
{$else}
procedure TCustomElEdit.MouseLeave;
{$endif}
begin
  FMouseOver := false;
  DoMouseLeave;
  {$ifndef CLX_USED}
  if (Flat and not Focused) and (not IsThemeApplied) then DrawFlatBorder;
  {$endif}
  inherited;
end;  { CMMouseLeave }

procedure TCustomElEdit.DoMouseEnter;
begin
  if assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCustomElEdit.DoMouseLeave;
begin
  if assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TCustomElEdit.Loaded;
begin
  inherited;
  scbVert.Loaded;
  scbHorz.Loaded;
//  S := FText;
//  FText := '';
//  Text := S;
  SetScrollbarsInfo;
  if HandleAllocated then
  begin
    if (not Multiline) and AutoSize then
      AdjustHeight;
    SetEditRect(ClientRect);
  end;
end;

procedure TCustomElEdit.SetFlatFocusedScrollBars(const Value: boolean);
begin
  if FFlatFocusedScrollBars <> Value then
  begin
    FFlatFocusedScrollBars := Value;
    {$ifndef CLX_USED}
    if (Focused) and (not FUseCustomScrollBars) and Multiline then DrawFlatBorder;
    {$endif}
  end;
end;

procedure TCustomElEdit.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then Invalidate;
  end;
end;

procedure TCustomElEdit.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then Invalidate;
  end;
end;

procedure TCustomElEdit.EMSetRect(var Message: TMessage);
begin
  EditRect := (PRect(Message.lParam))^;
  Invalidate;
end;

procedure TCustomElEdit.EMSetRectNP(var Message: TMessage);
begin
  EditRect := (PRect(Message.lParam))^;
end;

procedure TCustomElEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if IsThemeApplied or FChangeDisabledText then
    Invalidate;
end;

procedure TCustomElEdit.SetUseXPThemes(const Value: Boolean);
begin
  inherited;
  RecreateWnd;
end;

procedure TCustomElEdit.OnHScroll(Sender: TObject; ScrollCode: TElScrollCode;
                                var ScrollPos: Integer; var DoChange : boolean);
begin
  if (FLeftChar <> ScrollPos) then
  begin
    if (scbHorz.Page + ScrollPos) > (FElEditList.FMaxLen) then
      LeftChar := FElEditList.FMaxLen - scbHorz.Page + 1
    else
      LeftChar := ScrollPos;

    DoChange := true;
    DrawFlatBorder;
  end;
end;

procedure TCustomElEdit.OnVScroll(Sender: TObject; ScrollCode: TElScrollCode;
                                var ScrollPos: Integer; var DoChange : boolean);
begin
  if (FTopLine <> ScrollPos) then
  begin
    TopLine := ScrollPos;
    DoChange := true;
    DrawFlatBorder;
  end;
end;

procedure TCustomElEdit.SetVertScrollBarStyles(newValue : TElScrollBarStyles);
begin
  FVertScrollBarStyles.Assign(newValue);
end;  { SetVertScrollBarStyles }

procedure TCustomElEdit.SetHorzScrollBarStyles(newValue : TElScrollBarStyles);
begin
  FHorzScrollBarStyles.Assign(newValue);
end;  { SetHorzScrollBarStyles }

procedure TCustomElEdit.SBChanged(Sender: TObject);
begin
  if FUseCustomScrollBars then
  begin
    FScrollBars := ssNone;
    if (scbVert.Visible) and (scbHorz.Visible) then
      FScrollBars := ssBoth
    else
      if scbVert.Visible then
        FScrollBars := ssVertical
      else
        if scbVert.Visible then
          FScrollBars := ssHorizontal;
  end;
  SetScrollBarsInfo;
  Invalidate;
end;

procedure TCustomElEdit.SetUseCustomScrollBars(newValue : Boolean);
{ Sets data member FUseCustomScrollBars to newValue. }
begin
  if (FUseCustomScrollBars <> newValue) then
  begin
    FUseCustomScrollBars := newValue;
    if Multiline then
    begin
      RecreateWnd;
      SetScrollBarsInfo;
    end;
  end;  { if }
end;  { SetUseCustomScrollBars }

procedure TCustomElEdit.DeleteSelection;
var
  bP, bPO: integer;
  eP, ePO: integer;
  FX, FY: integer;
  i: integer;
  {$ifdef ELPACK_UNICODE}
  S: WideString;
  {$else}
  S: String;
  {$endif}
  FTstr: TElFString;
begin
  if FSelected then
  begin
    FTstr := SelText;
    FElEditList.CaretToParagraph(FSelFirstX, FSelFirstY, bP, bPO);
    FElEditList.CaretToParagraph(FSelLastX, FSelLastY, eP, ePO);
    if eP = bP then
    begin
      S := FElEditList.FParagraphs.Items[bP].Text;
      {$ifdef ELPACK_UNICODE}
      WideDelete(S, bPO + 1, (ePO - bPO));
      {$else}
      Delete(S, bPO + 1, (ePO - bPO));
      {$endif}
      FElEditList.FParagraphs.Items[bP].Text := S;
    end
    else
    begin
      S := FElEditList.FParagraphs.Items[bP].Text;
      {$ifdef ELPACK_UNICODE}
      WideDelete(S, bPO + 1, Length(S));
      {$else}
      Delete(S, bPO + 1, Length(S));
      {$endif}
      FElEditList.FParagraphs.Items[bP].Text := S;
      for i := eP - 1 downto bP + 1 do
        FElEditList.FParagraphs.Delete(i);

      S := FElEditList.FParagraphs.Items[bP + 1].Text;
      {$ifdef ELPACK_UNICODE}
      WideDelete(S, 1, ePO);
      {$else}
      Delete(S, 1, ePO);
      {$endif}
      FElEditList.FParagraphs.Items[bP].Text := FElEditList.FParagraphs.Items[bP].Text + S;
      FElEditList.FParagraphs.Delete(bP + 1);
    end;
    FElEditList.ReformatParagraph(bP);
    FElEditList.ReCount(bP);
    FElEditList.CaretFromParagraph(bP, bPO, FX, FY);
    FUndo.AddAction(atDeleteSel, CaretXY, Point(FX, FY), FTStr);
    CaretY := FY;
    CaretX := FX;
    FSelStartY := CaretY;
    FSelStartX := CaretX;
    UnSelect;
  end;
end;

procedure TCustomElEdit.UnSelect;
begin
  if FSelected then
  begin
    FSelFirstX := FSelStartX;
    FSelFirstY := FSelStartY;
    FSelLastX := FSelStartX;
    FSelLastY := FSelStartY;
    FSelected := false;
    TriggerSelectionChangeEvent;
    RepaintText(EditRect);
  end;
end;

procedure TCustomElEdit.SetSelection(SelX, SelY: integer);
begin
//  if FSelected then
//  begin
    if SelY <= FSelStartY then
    begin
      FSelFirstY := SelY;
      FSelLastY := FSelStartY;
    end
    else
    begin
      FSelFirstY := FSelStartY;
      FSelLastY := SelY;
    end;
    if (SelY < FSelStartY) or ((SelX <= FSelStartX) and (SelY <= FSelStartY)) then
    begin
      FSelFirstX := SelX;
      FSelLastX := FSelStartX;
    end
    else
    begin
      FSelFirstX := FSelStartX;
      FSelLastX := SelX;
    end;
    FSelected := true;
{  end
  else
  begin
    FSelStartX := SelX;
    FSelStartY := SelY;
    FSelFirstX := SelX;
    FSelFirstY := SelY;
    FSelLastX := SelX;
    FSelLastY := SelY;
    FSelected := true;
  end;}
end;

procedure TCustomElEdit.SetCharCase(Value: TElEditCharCase);
begin
  if FCharCase <> Value then
  begin
    FCharCase := Value;
    if FCharCase <> eecNormal then
    begin
      {$ifndef ELPACK_UNICODE}
      if FCharCase = eecUppercase then
        Text := Uppercase(Text)
      else
        Text := Lowercase(Text);
      {$else}
      if FCharCase = eecUppercase then
        Text := WideUppercase(Text)
      else
        Text := WideLowercase(Text);
      {$endif}
    end;
  end;
end;

procedure TCustomElEdit.WMSetCursor(var Message: TWMSetCursor);
var Control : TControl;
    P: TPoint;
begin
  if (FUseCustomScrollBars and Multiline) then
  with Message do
    if CursorWnd = Handle then
      if Smallint(HitTest) = HTCLIENT then
      begin
        GetCursorPos(P);
        Control := ControlAtPos(ScreenToClient(P), true{$ifdef VCL_5_USED}, true{$endif});
        if Control is TElScrollBar then
        begin
          Windows.SetCursor(Screen.Cursors[Control.Cursor]);
          exit;
        end;
      end;
  inherited;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TCustomElEdit.CMHintShow(var Message: TMessage);
{$else}
function TCustomElEdit.HintShow(var HintInfo : THintInfo): Boolean; 
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

procedure TCustomElEdit.SetHint(Value: WideString);
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

procedure TCustomElEdit.SetBottomAlign;
begin
  FTopMargin := FEditRect.Top + (FEditRect.Bottom - FEditRect.Top - FLineHeight) div 2;
end;

procedure TCustomElEdit.Scroll(ScrollDir : TElEditScrollDir);
var Msg : TWMVScroll;
begin
  Msg.Msg := WM_VSCROLL;
  Msg.Pos := 0;
  Msg.ScrollBar := SB_VERT;
  Msg.Result := 0;
  case ScrollDir of
    esdLineUp:
      Msg.ScrollCode := SB_LINEUP;
    esdLineDown:
      Msg.ScrollCode := SB_LINEDOWN;
    esdPageUp:
      Msg.ScrollCode := SB_PAGEUP;
    esdPageDown:
      Msg.ScrollCode := SB_PAGEDOWN;
  end;
  WMVSCroll(Msg);
end;

procedure TCustomElEdit.SetMaxLevel(const Value: integer);
begin
  FUndo.MaxUndo := Value;
end;

function TCustomElEdit.GetMaxLevel: integer;
begin
  Result := FUndo.MaxUndo;
end;

function TCustomElEdit.GetCanUndo: boolean;
begin
  Result := FUndo.CanUndo;
end;

procedure TCustomElEdit.ScrollCaret;
begin
  (*
  if Multiline and ((TopLine > Self.FCaretPos.y) or (TopLine + FLinesInRect <= TopLine)) then
    TopLine := Max(0, FCaretPos.y - FLinesInRect shr 1);
  if Multiline then
    SetScrollBarsInfo;

  RepaintText;
  *)
//  SetSelStart(SelStart);
  SetCaretPosition(CaretX, CaretY);
end;

function TCustomElEdit.GetText: TElFString;
var
  {$ifdef ELPACK_UNICODE}
  S: WideString;
  {$else}
  S: String;
  {$endif}
begin
  S := FElEditList.Text;
  {$ifdef ELPACK_UNICODE}
  Result := WideCopy(S, 1, Length(S) - Length(ElFCRLF));
  {$else}
  Result := Copy(S, 1, Length(S) - Length(ElFCRLF));
  {$endif}
end;

function TCustomElEdit.ConvertToCRLF(Str : TElFString): TElFString;
var i, j : integer;
    ps, pd : PElFChar;
begin
  j := 0;
  for i := 1 to Length(Str) do
    if (Str[i] = #13) and (Str[i+1] <> #10) then inc(j);
  if (Length(Str) > 0) and (j > 0) then
  begin
    SetLength(Result, Length(Str) + j);
    ps := @Str[1];
    pd := @Result[1];
    while ps^ <> #0 do
    begin
      if ps^ = #13 then
      begin
        pd^ := ps^; inc(ps); inc(pd);
        if (ps^ <> #10) then
        begin
          pd^ := #10;
          inc(pd);
        end;
      end
      else
      begin
        pd^ := ps^; inc(ps); inc(pd);
      end;
    end;
  end
  else
    Result := Str;
end;

procedure TCustomElEdit.EMCanUndo(var Message: TMessage);
begin
  Message.Result := integer(CanUndo);
end;

procedure TCustomElEdit.EMUndo(var Message: TMessage);
begin
  Message.Result := Integer(CanUndo);
  Undo;
end;

procedure TCustomElEdit.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics,
  Metrics: TTextMetric;
begin
  if not FMultiline then
  begin 
    DC := GetDC(0);
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);

    if BorderStyle = bsSingle then
    begin
      if Ctl3D then
        I := GetSystemMetrics(SM_CYEDGE) * 4
      else
        I := GetSystemMetrics(SM_CYBORDER) * 6;
    end
    else
      I := 0;

    Height := Metrics.tmHeight + I;
    if (FAlignBottom) then
      SetBottomAlign;
  end;
end;

procedure TCustomElEdit.SetChangeDisabledText(Value: Boolean);
begin
  if FChangeDisabledText <> Value then
  begin
    FChangeDisabledText := Value;
    if not Enabled then
      RepaintText(EditRect);
  end;
end;

procedure TCustomElEdit.SetEditRect(Value : TRect);
begin
  if scbVert <> nil then
  begin
    if scbVert.Visible then
      Value.Right := Value.Right - scbVert.Width;
  end;
  if scbHorz <> nil then
  begin
    if scbHorz.Visible then
      Value.Bottom := Value.Bottom - scbHorz.Height;
  end;
  DoSetEditRect(Value);
end;

procedure TCustomElEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  //if not HandleAllocated then
  //  SetEditRect(Rect(0, 0, AWidth, AHeight));
end;

procedure TCustomElEdit.CMCtl3DChanged(var Msg : TMessage);
begin
  inherited;
  if HandleAllocated then
    SetEditRect(ClientRect);
end; { CMCtl3DChanged }

{ TElEditStrings }

{$ifdef ELPACK_UNICODE}
function TElEditStrings.Get(Index: integer): WideString;
{$else}
function TElEditStrings.Get(Index: integer): String;
{$endif}
begin
  Result := FParagraphs.Items[Index].Text;
end;

{$ifdef ELPACK_UNICODE}
procedure TElEditStrings.Put(Index: Integer; const S: WideString);
{$else}
procedure TElEditStrings.Put(Index: Integer; const S: String);
{$endif}
begin
  Changing;
  FParagraphs.Items[Index].Text := S;
  Changed;
end;

procedure TElEditStrings.PutObject(Index: Integer; AObject: TObject);
begin
  ;
end;

function TElEditStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TElEditStrings.GetCount: integer;
begin
  Result := FParagraphs.Count;
end;


function TElEditStrings.CutString(var S: TElFString;
                   Len: integer; var RealStr: boolean): TElFString;
var
  CRLFPos : integer;
begin
  if not FElEdit.RTLContent then
  begin
    {$ifdef ELPACK_UNICODE}
    CRLFPos := WidePos(ElFCR, S);
    {$else}
    CRLFPos := Pos(#13, S);
    {$endif}

    if Len = Length(S) then Dec(Len);

    if (((CRLFPos = 0) or (CRLFPos > Len)) and FElEdit.FWordWrap and (Length(S) > Len{ + 1})) then
    begin
      CRLFPos := Len;
      // Обрезаем строку по границе слова.
      while CRLFPos > 1 do
        if S[CRLFPos] = #32 then
          break
        else
          dec(CRLFPos);
      if CRLFPos = 1 then CRLFPos := Len;
      // to do
      (*      {$ifdef ELPACK_UNICODE}
      WideInsert(ElFCR, WideString(S), CRLFPos);
      {$else}
      Insert(ElFCR, S, CRLFPos);
      {$endif}*)
    end;

    {$ifdef ELPACK_UNICODE}
    Result := WideCopy(S, CRLFPos + 1, Length(S));
    S := WideCopy(S, 1, CRLFPos);
    {$else}
    Result := System.Copy(S, CRLFPos + 1, Length(S));
    S := System.Copy(S, 1, CRLFPos);
    {$endif}
  end
  else
  begin
    {$ifdef ELPACK_UNICODE}
    CRLFPos := WideLastPos(ElFCR, S);
    {$else}
    CRLFPos := LastPos(#13, S);
    {$endif}

    if (((CRLFPos = 0) or ((Length(S) - CRLFPos) - 1 > Len)) and FElEdit.FWordWrap) then
    begin
      RealStr := false;
      CRLFPos := Length(S) - Len;
      // Обрезаем строку по границе слова.
      while CRLFPos < Length(S) do
        if S[CRLFPos] = #32 then break
                            else inc(CRLFPos);
    end
    else
      RealStr := Boolean(RealStrings.IndexOf(IntToStr(Count - 1)) < 0);

    {$ifdef ELPACK_UNICODE}
    Result:=WideCopy(S, 1, CRLFPos - 1);
    S:=WideCopy(S, CRLFPos + 1, Length(S));
    {$else}
    Result:=System.Copy(S, CRLFPos - 1, Length(S));
    S:=System.Copy(S, 1, CRLFPos);
    {$endif}
  end;
end;

procedure TElEditStrings.IndexToParagraph(index: integer; var Paragraph, ParaIndex: integer);
var
  FBegin, FMiddle, FEnd: integer;
begin
  Paragraph := 0;
  ParaIndex := 0;
  FBegin := 0;
  FEnd := FParagraphs.Count - 1;
  while FBegin <= FEnd do
  begin
    FMiddle := (FBegin + FEnd) shr 1;
    if Index > FParagraphs.Items[FMiddle].FPCount + (FParagraphs.Items[FMiddle].Count - 1) then
      FBegin := FMiddle + 1
    else
    begin
      FEnd := FMiddle - 1;
      if (Index >= FParagraphs.Items[FMiddle].FPCount) and
         (Index <= (FParagraphs.Items[FMiddle].FPCount + FParagraphs.Items[FMiddle].Count - 1)) then
      begin
        Paragraph := FMiddle;
        ParaIndex := Index - FParagraphs.Items[FMiddle].FPCount;
        FBegin := FEnd + 1;
      end;
    end;
  end;
end;

procedure TElEditStrings.CaretToParagraph(ACaretX, ACaretY: integer; var Paragraph, ParaOffs: integer);
var
  i, PI: integer;
begin
  IndexToParagraph(ACaretY, Paragraph, PI);
  ParaOffs := ACaretX;
  for i := 0 to PI - 1 do
    ParaOffs := ParaOffs + Length(FParagraphs.Items[Paragraph].Strings[i]);
  if ParaOffs > Length(Strings[Paragraph]) then
    ParaOffs := Length(Strings[Paragraph]);
  if ParaOffs < 0 then
    ParaOffs := 0;
end;

procedure TElEditStrings.CaretFromParagraph(Paragraph, ParaOffs: integer; var ACaretX, ACaretY: integer);
var
  FLen, i: integer;
begin
  ACaretY := FParagraphs.Items[Paragraph].FPCount;
  FLen := Length(FParagraphs.Items[Paragraph].Strings[0]);
  i := 1;
  while ((ParaOffs > FLen) and (FParagraphs.Items[Paragraph].Count > i)) do
  begin
    FLen := FLen + Length(FParagraphs.Items[Paragraph].Strings[i]);
    inc(i);
  end;
  ACaretY := ACaretY + (i - 1);
  ACaretX := abs(FLen - Length(FParagraphs.Items[Paragraph].Strings[i - 1]) - ParaOffs);
  if ACaretX < 0 then ACaretX := -1;
end;

function TElEditStrings.GetParaCount: integer;
begin
  if FParagraphs.Count > 0 then
    Result := FParagraphs.Items[FParagraphs.Count - 1].FPCount + FParagraphs.Items[FParagraphs.Count - 1].Count
  else
    Result := 0;
end;

function TElEditStrings.GetParaString(Index: integer): TElFString;
var
  P, Pi: integer;
begin
  IndexToParagraph(Index, P, Pi);
  if FParagraphs.Count > 0 then
    Result := FParagraphs.Items[P].Strings[Pi]
  else
    Result := '';
end;

procedure TElEditStrings.SetParaString(Index: integer; const Value: TElFString);
var
  P, Pi: integer;
begin
  IndexToParagraph(Index, P, Pi);
  if FParagraphs.Count = 0 then
  begin
    if Index > 0 then
      EElEditorError.CreateFmt('List index outbound (%d)', [Index])
    else
      FParagraphs.Items[P].Strings[Pi] := Value;
  end
  else
  begin
    FParagraphs.Items[P].Strings[Pi] := Value;
    ReformatParagraph(P);
    ReCount(P);
  end;
end;

procedure TElEditStrings.ReCount(Index: integer);
var
  i: integer;
begin
  for i := Index to FParagraphs.Count - 1 do
  begin
    if i = 0 then
      FParagraphs.Items[i].FPCount := 0
    else
      FParagraphs.Items[i].FPCount := FParagraphs.Items[i - 1].FPCount + FParagraphs.Items[i - 1].Count;
  end;
  FElEdit.SetScrollBarsInfo;
end;


procedure TElEditStrings.ReformatParagraph(Index: integer);
var
  T: TElFString;
  S: TElFString;
  StartPos, i : integer;
  FPara: TElParagraph;
  RStr: boolean;
begin
//  S := Get(Index);
  FPara := FParagraphs.Items[Index];
  S := FPara.Text;
  FPara.Clear;
  FPara.Add('');
  if Index = FIdxMaxLen then
    FMaxLen := 0;
  if FElEdit.WordWrap then
  begin
    i := 0;
    while (i <= FPara.Count - 1) do
    begin
      if FElEdit.RTLContent then
        StartPos := {$ifdef ELPACK_UNICODE}WideLastPos(#13, S){$else}LastPos(#13, S){$endif}
      else
        StartPos := 0;

      if FElEdit.TextSize(S).cx >= (FElEdit.EditRect.Right - FElEdit.RightMargin) then
      begin
        T := CutString(S, FElEdit.CharsFitRight(FElEdit.FEditRect.Right - FElEdit.FRightMargin,
                       S, StartPos), RStr);
        FPara.Put(i, S);
        FPara.Insert(i + 1, T);
        if (FElEDit.RTLContent and (not RStr)) then
          RealStrings.Insert(i, IntToStr(i));
        S := T;
      end
      else
        if i = 0 then FPara.Put(i, S);
      Inc(i);
    end;
  end
  else
  begin
    i := Length(S);
    if FMaxLen < i then
    begin
      FMaxLen := i;
      FIdxMaxLen := Index;
      FMaxStr := S;
    end;
    FPara.Put(0, S);
  end;
end;

procedure TElEditStrings.Reformat;
var
  i: integer;

begin
  FMaxLen := 0;
  FIdxMaxLen := 0;
  FMaxStr := '';
  for i := 0 to FParagraphs.Count - 1 do
    ReformatParagraph(i);
  FElEdit.SetScrollBarsInfo;
end;

procedure TElEditStrings.Changed;
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
  if FElEdit.HandleAllocated then 
    InvalidateRect(FElEdit.Handle, @FElEdit.FEditRect, false);
end;

{$ifdef ELPACK_UNICODE}
function TElEditStrings.GetTextStr: WideString;
{$else}
function TElEditStrings.GetTextStr: String;
{$endif}
var
  I, L, Size, Count: Integer;
  {$ifdef ELPACK_UNICODE}
  P: PWideChar;
  S: WideString;
  {$else}
  P: PChar;
  S: String;
  {$endif}
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do
    Inc(Size, Length(FParagraphs.Items[i].Text) + 2);
  SetLength(Result, Size);
  {$ifdef ELPACK_UNICODE}
  P := PWideChar(Result);
  {$else}
  P := PChar(Result);
  {$endif}
  for I := 0 to Count - 1 do
  begin
    S := FParagraphs.Items[i].Text;
    L := Length(S);
    if L <> 0 then
    begin
      {$ifdef ELPACK_UNICODE}
      WideMove(S[1], P^, L);
      {$else}
      System.Move(S[1], P^, L);
      {$endif}
      Inc(P, L);
    end;
    P^ := #13;
    Inc(P);
    P^ := #10;
    Inc(P);
  end;
end;

{$ifdef ELPACK_UNICODE}
procedure TElEditStrings.SetTextStr(const Value: WideString);
var
  S: WideString;
  P, Start: PWideChar;
{$else}
procedure TElEditStrings.SetTextStr(const Value: String);
var
  S: String;
  P, Start: PChar;
{$endif}
  i: integer;
begin
  if Text <> Value then
  begin
    if FElEdit.Multiline then
    begin
      BeginUpdate;
      try
        Clear;
        FParagraphs.Clear;
        {$ifdef ELPACK_UNICODE}
        P := PWideChar(Value);
        {$else}
        P := PChar(Value);
        {$endif}
        i := 0;
        if P <> nil then
          while P^ <> #0 do
          begin
            Start := P;
            while not ((P^ = #0) or (P^ = #13) or (P^ = #10)) do
              Inc(P);
            {$ifdef ELPACK_UNICODE}
            SetWideString(S, Start, (P - Start));
            {$else}
            SetString(S, Start, (P - Start));
            {$endif}
            FParagraphs.Add(TElParagraph.Create);
            FParagraphs.Items[i].Text := S;
            if (P^ = #13) and ((P + 1)^ = #10) then
            begin
              inc(P, 2);
              if P^ = #0 then
                FParagraphs.Insert(i + 1, TElParagraph.Create);
            end
            else
              if (P^ <> #0) then inc(P, 1);
            inc(i);
          end;
      finally
        EndUpdate;
      end;
      Reformat;
      ReCount(0);
    end
    else
    begin
      BeginUpdate;
      try
        Clear;
        FParagraphs.Clear;
        FParagraphs.Items[0].Put(0, Value);
      finally
        EndUpdate;
      end;
    end;
  end;
  if FElEdit.HandleAllocated then
  begin
    FElEdit.CaretY := 0;
    FElEdit.CaretX := 0;
  end
  else
  begin
    FElEdit.FCaretY := 0;
    FElEdit.FCaretX := 0;
  end;
end;

function TElEditStrings.GetReText: TElFString;
var
  i: integer;
  ReStr: TElFString;
begin
  for i := Count - 1 downto 0 do
  begin
    ReStr := ReStr + ParagraphStrings[i];
    if ((RealStrings.IndexOf(IntToStr(i)) >= 0) and (i > 0)) then
      ReStr := ReStr + ElFCR;
  end;
  Result := ReStr;
end;

constructor TElEditStrings.Create;
begin
  inherited;
  // to do
  RealStrings := TStringList.Create;
  FParagraphs := TElParagraphList.Create;
end;

destructor TElEditStrings.Destroy;
begin
  RealStrings.Free;
  FParagraphs.Free;
  inherited;
end;

procedure TElEditStrings.Delete(Index: integer);
begin
  Changing;
  FParagraphs.Delete(Index);
  Changed;
end;

procedure TElEditStrings.Exchange(Index1, Index2: Integer);
begin
  Changing;
  FParagraphs.Exchange(Index1, Index2);
  Changed;
end;

procedure TElEditStrings.Clear;
begin
  Changing;
  inherited;
  FParagraphs.Clear;
  Changed;
end;

{$ifdef ELPACK_UNICODE}
procedure TElEditStrings.AddStrings(Strings: TElWideStrings);
{$else}
procedure TElEditStrings.AddStrings(Strings: TStrings);
{$endif}
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      Add(Strings[I]);
  finally
    EndUpdate;
  end;
end;

{$ifdef ELPACK_UNICODE}
function TElEditStrings.Add(const S: WideString): Integer;
{$else}
function TElEditStrings.Add(const S: String): Integer;
{$endif}
begin
  if FParagraphs.Count > 0 then
    Result := FParagraphs.Count
  else
    Result := 0;
  Insert(Result, S);
end;

{$ifdef ELPACK_UNICODE}
function TElEditStrings.Find(const S: WideString; var Index: Integer): boolean;
{$else}
function TElEditStrings.Find(const S: String; var Index: Integer): boolean;
{$endif}
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    {$ifdef ELPACK_UNICODE}
    C := WideCompareText(Get(i), S);
    {$else}
    C := AnsiCompareText(Get(i), S);
    {$endif}
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{$ifdef ELPACK_UNICODE}
procedure TElEditStrings.Insert(Index: Integer; const S : WideString);
{$else}
procedure TElEditStrings.Insert(Index: Integer; const S : String);
{$endif}
var
  {$ifdef ELPACK_UNICODE}
  P, Start: PWideChar;
  S1: WideString;
  {$else}
  P, Start: PChar;
  S1: String;
  {$endif}
  i: integer;
begin
  Changing;
  if FElEdit.FMultiline then
  begin
    {$ifdef ELPACK_UNICODE}
    P := PWideChar(S);
    {$else}
    P := PChar(S);
    {$endif}
    i := Index;
    while (P^ <> #0) do
    begin
      Start := P;
      while not ((P^ = #13) or (P^ = #10) or (P^ = #0)) do
        Inc(P);
      {$ifdef ELPACK_UNICODE}
      SetWideString(S1, Start, (P - Start));
      {$else}
      SetString(S1, Start, (P - Start));
      {$endif}
      FParagraphs.Insert(i, TElParagraph.Create);
      FParagraphs.Items[i].Text := S1;
      ReformatParagraph(i);
      if (P^ = #13) and ((P + 1)^ = #10) then
      begin
        inc(P, 2);
        if P^ = #0 then
          FParagraphs.Insert(i + 1, TElParagraph.Create);
      end
      else
        if (P^ <> #0) then inc(P, 1);
      inc(i);
    end;
    ReCount(Index);
  end;
  Changed;
end;

{$ifdef ELPACK_UNICODE}
procedure TElEditStrings.InsertText(var ACaretX, ACaretY: integer; const S: WideString);
{$else}
procedure TElEditStrings.InsertText(var ACaretX, ACaretY: integer; const S: String);
{$endif}
var
  {$ifdef ELPACK_UNICODE}
  P, Start: PWideChar;
  S1, D, S2: WideString;
  {$else}
  P, Start: PChar;
  S1, D, S2: String;
  {$endif}
  PA, PO: integer;
  CountBefore: integer;
  FFlag: boolean;
  S2Len: Integer;
begin
  Changing;
  S2Len := -1;
  FFlag := false;
  CountBefore := Count;
  CaretToParagraph(ACaretX, ACaretY, PA, PO);
  D := Get(PA);
  {$ifdef ELPACK_UNICODE}
  P := PWideChar(S);
  {$else}
  P := PChar(S);
  {$endif}
  Start := P;
  while not ((P^ = #13) or (P^ = #10) or (P^ = #0)) do
    Inc(P);
  {$ifdef ELPACK_UNICODE}
  SetWideString(S1, Start, (P - Start));
  {$else}
  SetString(S1, Start, (P - Start));
  {$endif}
  if (P^ = #13) and ((P + 1)^ = #10) then
  begin
    if (P + 2)^ <> #0 then
      inc(P, 2)
    else
      FFLag := true;
  end
  else
    if (P^ <> #0) then inc(P, 1);
  if (P^ <> #0) or (S = ElFCRLF) then
  begin
    {$ifdef ELPACK_UNICODE}
    S2 := WideCopy(D, PO + 1, Length(D));
    WideDelete(D, PO + 1, Length(D));
    {$else}
    S2 := System.Copy(D, PO + 1, Length(D));
    System.Delete(D, PO + 1, Length(D));
    {$endif}
    S2Len := Length(S2);
    if not FFlag then
      S2 := P + S2;
    Insert(PA + 1, S2);
  end;
  {$ifdef ELPACK_UNICODE}
  WideInsert(S1, D, PO + 1);
  {$else}
  System.Insert(S1, D, PO + 1);
  {$endif}
  FParagraphs.Items[PA].Text := D;
  ReformatParagraph(PA);
  ReCount(PA);
  if S2Len >= 0 then
  begin
    PA := PA + (Count - CountBefore);
    if CountBefore = 0 then
      Dec(PA);
    PO := Length(Strings[PA]) - S2Len;
  end
  else
    PO := PO + Length(S1);
  CaretFromParagraph(PA, PO, ACaretX, ACaretY);
  Changed;
end;


{ TElParagraph }

{$ifdef ELPACK_UNICODE}
procedure TElParagraph.SetTextStr(const Value: WideString);
{$else}
procedure TElParagraph.SetTextStr(const Value: String);
{$endif}
begin
  inherited;
end;

{$ifdef ELPACK_UNICODE}
function TElParagraph.Get(Index: Integer): WideString;
{$else}
function TElParagraph.Get(Index: Integer): String;
{$endif}
begin
  if ((Count = 0) and (Index = 0)) then
    Add('');
  Result := inherited Get(IndeX);
end;


{$ifdef ELPACK_UNICODE}
procedure TElParagraph.Put(Index: Integer; const S: WideString);
{$else}
procedure TElParagraph.Put(Index: Integer; const S: String);
{$endif}
begin
  if ((Count = 0) and (Index = 0)) then
    Add('');
  inherited Put(Index, S);
end;

{$ifdef ELPACK_UNICODE}
function TElParagraph.GetTextStr: WideString;
{$else}
function TElParagraph.GetTextStr: String;
{$endif}
var
  i: integer;
begin
//  Result := inherited GetTextStr;
  Result := '';
  for i := 0 to Count - 1 do
    Result := Result + Get(i);
end;

{ TElParagraphList }

function TElParagraphList.Get(Index: Integer): TElParagraph;
begin
  if ((Count = 0) and (Index = 0)) then
    Add(TElParagraph.Create);
  Result := TElParagraph(inherited Get(Index));
end;

procedure TElParagraphList.Put(Index: Integer; const Value: TElParagraph);
begin
  inherited Put(Index, Value);
end;

procedure TElParagraphList.Delete(Index: integer);
begin
  Get(Index).Free;
  Remove(Get(Index));
end;

procedure TElParagraphList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Get(i).Free;
  inherited Clear;
end;

{ TElAction }

procedure TElAction.Assign(Source: TPersistent);
begin
  if (Source is TElAction) then
  begin
    FAction := TElAction(Source).FAction;
    FStartPos := TElAction(Source).FStartPos;
    FEndPos := TElAction(Source).FEndPos;
    FStr := TElAction(Source).FStr;
  end
  else
    inherited Assign(Source);
end;

{ TElActionList }

constructor TElActionList.Create;
begin
  FAStack := TElStack.Create;
  FLockCount := 0;
end;

destructor TElActionList.Destroy;
begin
  while not FAStack.Empty do
    TElAction(FAStack.Pop).Free;
  FAStack.Free;
end;

procedure TElActionList.AddAction(AAction: TElActionType; ASPos,
  AEPos: TPoint; AStr: TElFString);
var
  NewAction: TElAction;
begin
  if FLockCount = 0 then
  begin
    NewAction := TElAction.Create;
    try
      with NewAction do
      begin
        FAction := AAction;
        FStartPos := ASPos;
        FEndPos := AEPos;
        FStr := AStr;
      end;
      PushItem(NewAction);
    except
      NewAction.Free;
    end;
  end;
end;

function TElActionList.GetCanUndo: boolean;
begin
  Result := not FAStack.Empty;
end;

function TElActionList.PeekItem: TElAction;
begin
  Result := TElAction(FAStack.Last);
end;

function TElActionList.PopItem: TElAction;
begin
  Result := TElAction(FAStack.Pop);
end;

procedure TElActionList.PushItem(Item: TElAction);
begin
  if (FAStack.Count = FMaxUndo) and (FMaxUndo > 0) then
  begin
    TElAction(FAStack.Items[0]).Free;
    FAStack.ShiftUp(1);
  end;
  FAStack.Push(Item);
end;

procedure TElActionList.SetMaxUndo(const Value: integer);
begin
  if FMaxUndo <> Value then
  begin
    if FAStack.Count > Value then 
      FAStack.ShiftUp(abs(FMaxUndo - Value));
    FMaxUndo := Value;
  end;
end;

procedure TElActionList.Lock;
begin
  inc(FLockCount);
end;

procedure TElActionList.UnLock;
begin
  if FLockCount > 0 then
    dec(FLockCount);
end;

procedure Register;
begin
  RegisterComponents('EldoS Edit', [TElEdit]);
end;

end.



