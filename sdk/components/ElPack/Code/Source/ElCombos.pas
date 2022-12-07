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

07/15/2002

  Corrected button frame drawing n XP with themes disabled

07/10/2002

  AutoSelect property made published

04/19/2002

  Added ShowLineHint property

04/15/2002

  Pressing Enter when the combo is dropped down and when it's on dialog box with default button
  caused this button to react instead of the combo box.

02/26/2002

  Color change is not working anymore when Enabled is changed.

02/11/2002

  Text specified in designtime was lost in runtime. Fixed.

01/29/2002

  Fixed width of the visible text

01/23/2002

  Unicode support added

01/15/2002

  Dropdown form now correctly positions itself on the top of the combo's parent form

01/11/2002

  Fixed vertical text alignment problems with BorderStyle = bsNone
  Added ButtonShortCut and AltButtonShortcut properties

01/01/2002

  Fixed some problems with painting borders when focus is moved

12/21/2001

  Now OnDropDown is fired before the list is dropped. This lets you fill the list
  in OnDropDown event handler.

12/17/2001

  Fixed ElCombo to NOT call OnChange event when the text is changed programmatically

11/25/2001

  AdjustDropDownPos property added. Now, if the list doesn't fit into parent form,
  it is dropped up, not down

Version History
09/17/2001 (c) Akzhan Abdulin

  Added Windows XP Themes Support for ElComboBox.
  Note that themed combobox will ignore some its look'n'feel properties.

  I recommend using of TElAdvancedComboBox instead of this component.

07/23/2001

  AltBtnShowFrame and BtnShowFrame properties added.

06/20/2001

  DropDownWidth property added for dropped down list width setting.

03/21/2001

  Fixed the problem with auto-size combo boxes, that spoiled form fonts.

03/09/2001

  Fixed drawing artefacts on buttons.

12/23/2000

  CanDrop property added. For read-only comboboxes it acts similar to Enabled,
  but the font color is not changed.

12/20/2000

  Small resource leak fixed in ElCombos.SetAutoSize (By the way, this leak
  exists in VCL).

09/28/2000

  OnChange was not fired after selecting an item from the drop-down list. Fixed

09/05/2000

  AutoSize property published

*)

unit ElCombos;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  Graphics,
  ElACtrls,
  Forms,
  ElImgFrm,
  Menus,
{$ifdef VCL_6_USED}
  Types,
{$endif}
  ElTools,
  ElVCLUtils,
  Buttons,
  ElPopBtn,
{$IFDEF WIN32}
  ElUxTheme,
{$ENDIF}
  stdctrls,
{$ifdef ELPACK_UNICODE}
  ElUnicodeStrings,
{$endif}
  ElEdits,
  ElListBox,
  ElStrUtils,
  ElFrmPers;

type
  TElComboBox = class;

  TElComboButton = class(TGraphicControl)
  private
    FFlat: boolean;
    FFocused: boolean;
    FMouseOver: boolean;
    FTransparent: boolean;
    FArrowColor : TColor;
    FGlyph : TElButtonGlyph;
    FDrawFrame: Boolean;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetFocused(const Value: boolean);
    procedure SetDown(const Value : boolean);
    procedure SetTransparent(const Value: boolean);
    procedure SetDrawFrame(Value: Boolean);
  protected
    ExtGlyph : boolean;
    FDown: boolean;
    KeepColor : boolean;
    procedure DrawArrow(R: TRect); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Paint; override;
    procedure GlyphChanged(Sender : TObject);
    function GetGlyph : TBitmap;
    procedure SetGlyph (Value : TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property Flat: boolean read FFlat write FFlat;
    property Down : boolean read FDown write SetDown;
    property Focused: boolean read FFocused write SetFocused;
    property Transparent: boolean read FTransparent write SetTransparent;
    property DrawFrame: Boolean read FDrawFrame write SetDrawFrame default true;
  end;

  TElComboListBox = class(TElListBox)
  private
    FCombo: TElComboBox;
    procedure WMLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseActivate(var Msg: TMessage); message WM_MOUSEACTIVATE;
    procedure WMMouseMove(var Msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMRButtonDown(var Msg: TMessage); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TMessage); message WM_RBUTTONUP;
    procedure CNCommand(var Msg : TMessage); message CN_COMMAND;
  protected
    procedure ResetContent; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function  GetBackground: TBitmap; override;
    procedure DrawItem(Index: Integer; R: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TElComboBox = class(TCustomElEdit)
  protected
    ChangeAllowed  : Boolean;
    FDropDownWidth : Integer;
    FAutoCompletion: Boolean;
    FAltButtonShortcut: TShortcut;
    FButtonShortcut: TShortcut;
    FAltBtnAlign: TLeftRight;
    FAltBtnWidth: integer;
    FBtnFlat : Boolean;
    FBtnTransparent: boolean;
    FAltButton,
    FButton: TElComboButton;
    FDropDownCount: integer;
    FForm: TForm;
    FItemIndex: Integer;
    FListBox: TElComboListBox;
    FOnAltBtnClick: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FSaveColor : TColor;
    FSaveFlat  : boolean;
    FForcedText: boolean;
    FIgnoreItemIdx: boolean;
    FCanDrop,
    FDroppedDown : boolean;
    FAdjustDropDownPos: Boolean;
//    FHorizontalScroll: Boolean;
    FStyle: TComboBoxStyle;
    FOnDrawItem: TDrawItemEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FEditCanvas: TCanvas;
    FCanvas: TCanvas;
    FDropDownAlignment: TAlignment;
    function GetItemHeight: Integer;
    procedure SetItemHeight(Value: Integer);
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
      ComboProc: Pointer); virtual;

    function GetListTransparentSelection: Boolean;
    procedure SetListTransparentSelection(Value: Boolean);
    procedure SetDropDownWidth(const Value: Integer);
    function GetBtnDrawFrame: Boolean;
    procedure SetBtnDrawFrame(Value: Boolean);
    function GetAltBtnDrawFrame: Boolean;
    procedure SetAltBtnDrawFrame(Value: Boolean);
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMSetCursor(var Msg: TMessage); message WM_SETCURSOR;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMContextMenu(var Message: TMessage); message WM_CONTEXTMENU;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure EMSetReadOnly(var Msg : TMessage); message EM_SETREADONLY;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure ButtonClick(Sender: TObject);
    procedure AltButtonClick(Sender: TObject);
    function GetBtnColor: TColor;
    procedure GetDropDownValue;
    function GetDroppedDown: boolean;
    function GetItems: TElFStrings;
    function GetListColor: TColor;
    function GetListInvertSelection: boolean;
    function GetSorted: boolean;
    function GetUseBackground: boolean;
    procedure ListBoxClick(Sender: TObject);
    procedure SetBtnColor(const Value: TColor);
    procedure SetBtnTransparent(const Value: boolean);
    procedure SetDropDownCount(const Value: integer);
    procedure SetDroppedDown(const Value: boolean);
    procedure SetCanDrop(const Value : boolean);
    procedure SetEditRect(Value : TRect); override;
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TElFStrings);
    procedure SetListColor(const Value: TColor);
    procedure SetListInvertSelection(const Value: boolean);
    procedure SetSorted(const Value: boolean);
    procedure SetUseBackground(const Value: boolean);
    function  GetDroppedIndex : integer;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
//    procedure SetHorizontalScroll(Value: boolean);
    procedure Paint; override;
  protected
    procedure MeasureItem(Index: Integer; var Height: Integer); virtual;
    procedure DrawItem(Index: Integer; R: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); virtual;

    function  GetListImageForm : TElImageForm;
    procedure SetListImageForm(newValue : TElImageForm);
    {$ifdef MSWINDOWS}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    {$endif}
    procedure DoDropDown; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure SetBtnFlat(newValue : Boolean); virtual;
    function GetBtnArrowColor : TColor; virtual;
    procedure SetBtnArrowColor(newValue : TColor); virtual;

    function GetListSelectedColor : TColor;
    procedure SetListSelectedColor(newValue : TColor);
    function GetAltBtnColor : TColor;
    procedure SetAltBtnColor(Value : TColor);
    function GetAltBtnTransparent : boolean;
    procedure SetAltBtnTransparent(Value : boolean);
    function GetAltBtnFlat : boolean;
    procedure SetAltBtnFlat(Value : boolean);
    function GetAltBtnGlyph : TBitmap;
    procedure SetAltBtnGlyph(Value : TBitmap);
    procedure SetAltBtnWidth(Value : integer);
    function GetAltBtnVisible : boolean;
    procedure SetAltBtnVisible(Value : boolean);

    procedure SetAltBtnAlign(Value : TLeftRight);
    procedure DoAutoComplete;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure SetReadOnly(Value : boolean);
    procedure SetLineBorderActiveColor(Value: TColor); override;
    procedure SetLineBorderInactiveColor(Value: TColor); override;
    procedure SetActiveBorderType(const Value: TElFlatBorderType); override;
    procedure SetInactiveBorderType(const Value: TElFlatBorderType); override;
    procedure SetFlat(const Value: boolean); override;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure DestroyWnd; override;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    function GetShowLineHint: Boolean;
    procedure SetShowLineHint(Value: Boolean);
    procedure SetStyle(Value: TComboBoxStyle); virtual;
    function GetEditHandle: HWND;
    procedure SetDropDownAlignment(Value: TAlignment);
    property EditHandle: HWND read GetEditHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure CloseUp(AcceptValue: boolean);
    procedure DropDown;
    property DroppedDown: boolean read GetDroppedDown write SetDroppedDown;
    property DroppedIndex : integer read GetDroppedIndex;
    property Ctl3D;
    property ParentCtl3D;
    property Canvas: TCanvas read FCanvas;
  published
    property ActiveBorderType;
    property Align;
    property Alignment;
    {$IFDEF VCL_4_USED}
    property Anchors;
    {$ENDIF}
    property Background;
    {$IFDEF VCL_4_USED}
    property BiDiMode;
    {$ENDIF}
    property BorderStyle;
    property BorderSides;
    property VertScrollBarStyles;
    property HorzScrollBarStyles;
    property UseCustomScrollBars;

    property BtnColor         : TColor read GetBtnColor write SetBtnColor default clBtnFace;
    property BtnTransparent   : boolean read FBtnTransparent write SetBtnTransparent default False;
    property BtnFlat          : Boolean read FBtnFlat write SetBtnFlat default false;
    property BtnArrowColor    : TColor read GetBtnArrowColor write SetBtnArrowColor;  { Published }

    property AltBtnColor      : TColor read GetAltBtnColor write SetAltBtnColor default clBtnFace;
    property AltBtnTransparent: boolean read GetAltBtnTransparent write SetAltBtnTransparent default false;
    property AltBtnFlat       : Boolean read GetAltBtnFlat write SetAltBtnFlat default false;
    property AltBtnGlyph      : TBitmap read GetAltBtnGlyph write SetAltBtnGlyph;
    property AltBtnVisible    : Boolean read GetAltBtnVisible write SetAltBtnVisible  default false;
    property AltBtnWidth      : Integer read FAltBtnWidth write SetAltBtnWidth;
    property AltBtnPosition   : TLeftRight read FAltBtnAlign write SetAltBtnAlign default taRightJustify;
    property OnAltButtonClick : TNotifyEvent read FOnAltBtnClick write FOnAltBtnClick;

    property CanDrop : boolean read FCanDrop write SetCanDrop default True;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 8;
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth default -1;
    property ListTransparentSelection: Boolean read GetListTransparentSelection write SetListTransparentSelection default false;
    property BtnDrawFrame: Boolean read GetBtnDrawFrame write SetBtnDrawFrame default true;
    property AutoCompletion: Boolean read FAutoCompletion write FAutoCompletion;
    property AltBtnDrawFrame: Boolean read GetAltBtnDrawFrame write SetAltBtnDrawFrame default true;
    property Items: TElFStrings read GetItems write SetItems;
    property ListColor: TColor read GetListColor write SetListColor default clWindow;
    property ListImageForm : TElImageForm read GetListImageForm write SetListImageForm;
    property ListInvertSelection: boolean read GetListInvertSelection write SetListInvertSelection default False;
    property ListSelectedColor : TColor read GetListSelectedColor write SetListSelectedColor;
    property Sorted: boolean read GetSorted write SetSorted default False;
    property UseBackground: boolean read GetUseBackground write SetUseBackground default False;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property AdjustDropDownPos: Boolean read FAdjustDropDownPos write
        FAdjustDropDownPos default true;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;

    property AutoSelect;
    property AutoSize;
    property CharCase;
    property TopMargin;
    property LeftMargin;
    property RightMargin;
    property RTLContent;
    property PasswordChar;
    property Multiline default false;
    property WantTabs;
    property HandleDialogKeys;
    property HideSelection;
    property TabSpaces;
    property Lines stored false;

    property Color;
    {$IFDEF VCL_4_USED}
    property Constraints;
    {$ENDIF}
    property DragCursor;
    {$IFDEF VCL_4_USED}
    property DragKind;
    {$ENDIF}
    property DragMode;

    property Enabled;
    property Flat;
    property Font;
    {$ifdef ELPACK_COMPLETE}
    property ImageForm;
    {$endif}
    property ImeMode;
    property ImeName;
    property InactiveBorderType;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;

    property MaxLength;
    {$IFDEF VCL_4_USED}
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly write SetReadOnly default false;
    property ShowHint;

    property TabOrder;
    property TabStop;
    property Text;
    property Transparent;
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
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseEnter;
    property OnMouseLeave;
    {$IFDEF VCL_4_USED}
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;
    property AltButtonShortcut: TShortcut read FAltButtonShortcut write
        FAltButtonShortcut;
    property ButtonShortcut: TShortcut read FButtonShortcut write FButtonShortcut;
    property ShowLineHint: Boolean read GetShowLineHint write SetShowLineHint
        default false;
//    property HorizontalScroll: Boolean read FHorizontalScroll write SetHorizontalScroll;
    property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property DropDownAlignment: TAlignment read FDropDownAlignment write SetDropDownAlignment default taRightJustify;
  end;

implementation

{$IFDEF WIN32}
uses
  ElTmSchema;
{$ENDIF}

{ TElComboButton }

procedure TElComboButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if not FFocused then Invalidate;
end;

procedure TElComboButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if not FFocused then Invalidate;
end;

destructor TElComboButton.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

constructor TElComboButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [{csCaptureMouse, }csOpaque, csDoubleClicks, csNoDesignVisible];
  FMouseOver := False;
  FGlyph := TElButtonGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  Cursor := crArrow;
  DrawFrame := true;
end;

procedure TElComboButton.DrawArrow(R: TRect);
var
  X, Y: integer;

  procedure DoDraw(Color: Integer);
  var
    Done: Integer;
    Line: TRect;
  begin
    Line := Rect(X, Y, X + 7, Y + 1);
    Done := 4;
    while Done <> 0 do
    begin
      FillRect(Canvas.Handle, Line, Color);
      InflateRect(Line, -1, 0);
      OffsetRect(Line, 0, 1);
      Dec(Done);
    end;
  end;

begin
  X := R.Left + (R.Right - R.Left - 7) div 2;
  Y := R.Top + (R.Bottom - R.Top - 4) div 2;
  if Enabled then
    DoDraw(COLOR_BTNTEXT + 1)
  else
  begin
    DoDraw(COLOR_BTNHILIGHT + 1);
    //DoDraw(COLOR_BTNFACE + 1);
    (*
    Inc(X); Inc(Y);
    DoDraw(COLOR_BTNHILIGHT + 1);
    Dec(X); Dec(Y);
    DoDraw(COLOR_BTNSHADOW + 1);
    *)
  end;
end;

function TElComboButton.GetGlyph : TBitmap;
begin
  Result := FGlyph.Glyph;
end;

procedure TElComboButton.SetGlyph (Value : TBitmap);
begin
  FGlyph.Glyph := Value;
  FGlyph.ResetNumGlyphs;
  Invalidate;
end;

procedure TElComboButton.GlyphChanged(Sender : TObject);
begin
  Invalidate;
end;

procedure TElComboButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    FDown := True;
    Invalidate;
  end;
end;

procedure TElComboButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    (*if FDown <> TElComboBox(Owner).DroppedDown then
    begin
      FDown := not FDown;
      Invalidate;
    end;*)
    Click;
  end;
end;

procedure TElComboButton.Paint;
const
  Borders: array [Boolean, Boolean] of DWORD =
    ((EDGE_RAISED, EDGE_SUNKEN), (BDR_RAISEDINNER, BDR_SUNKENOUTER));
  DownStates : array[boolean] of TElButtonState = (ebsUp, ebsDown);
var
  R: TRect;
  AColor,
  Color : TColor;
  iStateId: Integer;

begin
  R := ClientRect;
  if not FTransparent then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(R);
  end;
  if (not ThemesAvailable) or (GetWindowTheme(Parent.Handle) = 0) then
  begin
    if FDrawFrame then
    begin
      if TElComboBox(Owner).Flat and
      (((FMouseOver or FFocused) and (TElComboBox(Owner).ActiveBorderType = fbtColorLineBorder)) or
       ((not (FMouseOver or FFocused)) and (TElComboBox(Owner).InactiveBorderType = fbtColorLineBorder))) then
      begin
        if FMouseOver or FFocused then
          AColor := TElComboBox(Owner).LineBorderActiveColor
        else
          AColor := TElComboBox(Owner).LineBorderInactiveColor;
        DrawFlatFrameEx2(Canvas.Handle, R, AColor, Self.Color, not FFlat or FMouseOver or FFocused, Enabled, AllBorderSides, fbtColorLineBorder);
        if not Transparent then
        begin
          InflateRect(R, -1, -1);
          DrawFlatFrameEx2(Canvas.Handle, R, Self.Color, Self.Color, false, true, AllBorderSides, fbtColorLineBorder);
          InflateRect(R, 1, 1);
        end;
      end
      else
        DrawEdge(Canvas.Handle, R, Borders[FFlat and not (FMouseOver or FFocused), FDown], BF_RECT);
      InflateRect(R, -2, -2);
    end;
  end;
  if Enabled or KeepColor then
   Color := Self.FArrowColor
  else
   Color := clBtnShadow;
  if not ExtGlyph then
  begin
{$IFDEF WIN32}
    if Enabled then
    begin
      if Down then
      begin
        iStateId := Ord(CBXS_PRESSED);
      end
      else
      begin
        if FMouseOver then
        begin
          iStateId := Ord(CBXS_HOT);
        end
        else
        begin
          iStateId := Ord(CBXS_NORMAL);
        end;
      end;
    end
    else
    begin
      iStateId := Ord(CBXS_DISABLED);
    end;
    if not (ThemesAvailable and Succeeded(DrawThemeBackgroundTo('COMBOBOX', Canvas.Handle, Ord(CP_DROPDOWNBUTTON), iStateId, R, nil))) then
{$ENDIF}
    begin
      ElVCLUtils.DrawArrow(Canvas, eadDown, R, Color, true)
    end;
  end
  else
  begin
    FGlyph.Draw(Canvas, R, Point(0, 0), '', blGlyphTop, -1, -1, DownStates[FDown],
                DownStates[FDown], taCenter, true, false, FFlat and not (FMouseOver or FFocused),
                true, false, 0, tdtNormal, clNone, false, 0, 0, 0, false, false{$ifdef HAS_HTML_RENDER}, false, nil{$endif}, true);
  end;
end;

procedure TElComboButton.SetDown(const Value : boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
    Invalidate;
  end;
end;

procedure TElComboButton.SetFocused(const Value: boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
  end;
end;

procedure TElComboButton.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
  end;
end;

procedure TElComboButton.SetDrawFrame(Value: Boolean);
begin
  if FDrawFrame <> Value then
  begin
    FDrawFrame := Value;
    Invalidate;
  end;
end;

{ TElComboListBox }

constructor TElComboListBox.Create(AOwner: TComponent);
begin
  inherited;
  FCombo := TElComboBox(AOwner);
  BorderStyle := bsNone;
  TabStop := False;
end;

procedure TElComboListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

function TElComboListBox.GetBackground: TBitmap;
begin
  Result := FCombo.Background;
end;

procedure TElComboListBox.WMLButtonDown(var Msg: TMessage);
begin
  MouseCapture := True;
end;

procedure TElComboListBox.DrawItem(Index: Integer; R: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
begin
  if (FCombo.FStyle = csOwnerDrawFixed)or(FCombo.FStyle = csOwnerDrawVariable) then
  begin
    FCombo.FCanvas := Canvas;
    FCombo.DrawItem(Index, R, State);
  end
  else
    inherited DrawItem(Index, R, State);
end;

procedure TElComboListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if FCombo.FStyle = csOwnerDrawVariable then
    FCombo.MeasureItem(Index, Height)
  else
    inherited;
end;

procedure TElComboListBox.WndProc(var Message: TMessage);
begin
  if FCombo<>nil then
    FCombo.ComboWndProc(Message, 0, nil);
  inherited WndProc(Message);
end;

procedure TElComboListBox.WMLButtonUp(var Msg: TWMLButtonUp);
begin
  MouseCapture := False;
  Click;
  FCombo.CloseUp(PtInRect(ClientRect, Point(Msg.XPos, Msg.YPos)));
end;

procedure TElComboListBox.WMMouseActivate(var Msg: TMessage);
begin
  Msg.Result := MA_NOACTIVATE;
end;

procedure TElComboListBox.WMMouseMove(var Msg: TWMMouseMove);
var
  Index: integer;
  FOldEvent: TNotifyEvent;
begin
  with Msg do
  begin
    Index := ItemIndex;
    if (YPos >= ClientHeight) then
    begin
      if Index >= 0 then Inc(Index);
    end
    else
    if YPos >= 0 then
      Index := Perform(LB_ITEMFROMPOINT, 0, MakeLParam(XPos, YPos));
    if MouseCapture and
      ((XPos < 0) or (XPos > ClientWidth) or ((Index = 0) and (YPos < 0)) or
        ((Index = Items.Count) and (YPos > ClientHeight))) then
      Index := -1;
    if ((YPos < 0) and (Index > 0)) or (Index = Items.Count) then
      Dec(Index);
    FOldEvent := OnClick;
    ItemIndex := Index;
    OnClick := FOldEvent;
  end;
  inherited;
end;

procedure TElComboListBox.WMRButtonDown(var Msg: TMessage);
begin
end;

procedure TElComboListBox.WMRButtonUp(var Msg: TMessage);
begin
end;

procedure TElComboListBox.ResetContent;
begin
  inherited;
  if not FCombo.ReadOnly then
  begin
    Fcombo.FForcedText := true;
    FCombo.ItemIndex := -1;
    Fcombo.FForcedText := false;
  end
  else
    FCombo.ItemIndex := -1;
end;

procedure TElComboListBox.CNCommand(var Msg : TMessage);
begin
  inherited;
  if TWMCommand(Msg).NotifyCode = LBN_SELCHANGE then
  begin
    FCombo.ChangeAllowed := true;
    FCombo.ItemIndex := ItemIndex;
    FCombo.ChangeAllowed := false;
  end;
end;
                       
{ TElComboBox }

procedure TElComboBox.AltButtonClick(Sender: TObject);
begin
  FAltButton.Down := false;
  FAltButton.Invalidate;
  if Assigned(FOnAltBtnClick) then FOnAltBtnClick(Self);
end;

procedure TElComboBox.ButtonClick(Sender: TObject);
begin
  SetFocus;
  DropDown;
end;

procedure TElComboBox.Click;
begin
  inherited;
  if FForm.Visible then CloseUp(False);
end;

procedure TElComboBox.CloseUp(AcceptValue: boolean);
begin
  if FForm.Visible then
  begin
    SetWindowPos(FForm.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FForm.Visible := False;
    FDroppedDown := false;
  end;
  if AcceptValue then
  begin
    GetDropDownValue;
    Change;
  end;
  FButton.FDown := false;
  FButton.Repaint;
  DoDropDown;
end;

procedure TElComboBox.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FButton) and (Msg.Sender <> FListBox)
    and (Msg.Sender <> FForm) then
    CloseUp(False);
end;

procedure TElComboBox.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  FButton.KeepColor := not CanDrop;
  FButton.Enabled := Enabled;
  (*if Enabled then
     Color := FSaveColor
  else
  begin
    FSaveColor := Color;
    Color := clbtnFace//clBtnShadow;
  end;
  *)
end;

procedure TElComboBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  FListBox.Font.Assign(Font);
  FListBox.SelectedFont.Assign(Font);
  FListBox.SelectedFont.Color := clHighlightText;
  Invalidate;
end;

procedure TElComboBox.WMKeyDown(var Message: TWMKeyDown);
begin
  if (Message.CharCode = VK_RETURN) and FForm.Visible then
  begin
    CloseUp(True);
    Message.Result := 1;
  end;
  if ReadOnly then
  begin
    inherited;
    HideCaret(Handle);
    SendMessage(Handle, EM_SETSEL, -1, -1);
  end
  else
  begin
    inherited;
  end;
end;

procedure TElComboBox.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = EN_UPDATE) and ReadOnly then
  begin
    HideCaret(Handle);
  end
  else
    inherited;
end;

procedure TElComboBox.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if ReadOnly then
  begin
    with Message do
      MouseUp(mbLeft, KeysToShiftState(Keys), XPos, YPos);
    SendMessage(Handle, EM_SETSEL, -1, -1);
    HideCaret(Handle);
    exit;
  end;
  inherited;
end;

procedure TElComboBox.WMMButtonDblClk(var Message: TWMMButtonDblClk);
begin
  if ReadOnly then
  begin
    Message.Result := 0;
    with Message do
      MouseDown(mbMiddle, KeysToShiftState(Keys) + [ssDouble], XPos, YPos);
    SendMessage(Handle, EM_SETSEL, -1, -1);
    HideCaret(Handle);
  end else
    inherited;
end;

procedure TElComboBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if ReadOnly then
  begin
    SendCancelMode(Self);
    if csClickEvents in ControlStyle then ControlState := ControlState + [csClicked];
    if not (csNoStdEvents in ControlStyle) then
    with Message do
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
    DroppedDown := not DroppedDown;
    SendMessage(Handle, EM_SETSEL, -1, -1);
    HideCaret(Handle);
  end else inherited;
end;

procedure TElComboBox.EMSetReadOnly(var Msg : TMessage);
begin
  if Boolean(Msg.wParam) then
  begin
    if GetFocus = Handle then
       HideCaret(Handle);
    if Items.IndexOf(inherited Text) = -1 then
    begin
      FForcedText := true;
      Text := '';
      FForcedText := false;
    end;
  end;
end;

procedure TElComboBox.WMMouseWheel(var Msg: TWMMouseWheel);
var
  Dy : integer;
  sl : integer;
begin
  //if IsWinNT or IsWin98 then SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @sl, SPIF_SENDCHANGE) else sl := 3;
  //if sl = 0 then
  sl := 1;
  Dy := Msg.WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  if Dy <> 0 then
  begin
    if Self.DroppedDown then
    begin
      if InRange(0, Items.Count - 1, FListBox.ItemIndex - dy) then
         FListBox.ItemIndex := FListBox.ItemIndex - dy;
    end
    else
      if InRange(0, Items.Count - 1, ItemIndex - dy) then
         ItemIndex := ItemIndex - dy;
  end;
end; { WMMouseWheel }

procedure TElComboBox.WMContextMenu(var Message: TMessage);
var
  Pt, Temp: TPoint;
  {$IFDEF VCL_5_USED}
  Handled: Boolean;
  {$ENDIF}
  PopupMenu: TPopupMenu;
begin
  if ReadOnly then
  begin
    Pt.x := LOWORD(Message.lParam);
    Pt.y := HIWORD(Message.lParam);
    if Pt.X < 0 then
      Temp := Pt
    else
    begin
      Temp := ScreenToClient(Pt);
      if not PtInRect(ClientRect, Temp) then
      begin
        inherited;
        Exit;
      end;
    end;

    {$IFDEF VCL_5_USED}
    Handled := False;
    DoContextPopup(Temp, Handled);
    Message.Result := Ord(Handled);
    if Handled then Exit;
    {$ENDIF}

    PopupMenu := GetPopupMenu;
    if (PopupMenu <> nil) and PopupMenu.AutoPopup then
    begin
      SendCancelMode(nil);
      PopupMenu.PopupComponent := Self;
      if Pt.X < 0 then
        Pt := ClientToScreen(Point(0,0));
      PopupMenu.Popup(Pt.X, Pt.Y);
    end;
    Message.Result := 1;
  end else
    inherited;
end;

procedure TElComboBox.WMSetCursor(var Msg: TMessage);
begin
  if ReadOnly then
    Windows.SetCursor(Screen.Cursors[crArrow])
  else
    inherited;
end;

procedure TElComboBox.CMTextChanged(var Msg: TMessage);
begin
  if not FIgnoreItemIdx then
  begin
    if ReadOnly and not FForcedText then
      if (Items.IndexOf(inherited Text) = -1) and (ItemIndex <> -1) then inherited Text := Items[ItemIndex];
    FForcedText := true;
    ItemIndex := Items.IndexOf(inherited Text);
    FForcedText := false;
  end;
end;

type TTrickyForm = class(TForm)
     protected
       procedure CreateParams(var Params: TCreateParams); override;
     end;

procedure TTrickyForm.CreateParams(var Params: TCreateParams);  { protected }
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_POPUP or WS_BORDER;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    WndParent := GetDesktopWindow;
  end;
end;  { CreateParams }

constructor TElComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FEditCanvas := inherited Canvas;
  FCanvas := FEditCanvas;
  NotifyUserChangeOnly := true;
  Multiline := false;
  FCanDrop := true;
  FDropDownCount := 8;
  FDropDownWidth := -1;
  FItemIndex := -1;
  FAdjustDropDownPos := true;
  FButton := TElComboButton.Create(Self);
  FAltButton := TElComboButton.Create(Self);
  FAltButton.Visible := false;
  FAltBtnAlign := taRightJustify;
  ControlStyle := ControlStyle - [csCaptureMouse, csFixedHeight];
  with FButton do
  begin
    ParentColor := False;
    Color := clBtnFace;
    Parent := Self;
    Visible := True;
    Width := GetSystemMetrics(SM_CYVSCROLL);
    OnClick := ButtonClick;
  end;
  with FAltButton do
  begin
    ExtGlyph := true;
    ParentColor := False;
    Color  := clBtnFace;
    Parent := Self;
    Width  := GetSystemMetrics(SM_CYVSCROLL);
    OnClick := AltButtonClick;
  end;
  Self.FAltBtnWidth := FAltButton.Width;

  FForm := TTrickyForm.CreateNew(nil);
  FForm.Visible := False;
  FForm.BorderStyle := bsNone;

  with TElFormPersist.Create(FForm) do
    TopMost := true;

  FListBox := TElComboListBox.Create(Self);
  FListBox.Parent := FForm;
  FListBox.Align := alClient;
  FListBox.OnClick := ListBoxClick;
  AutoSelect := true;
  FDropDownAlignment := taRightJustify;
end;

procedure TElComboBox.CreateParams(var Params: TCreateParams);
const
  ComboBoxStyles: array[TComboBoxStyle] of DWORD = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWFIXED,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWVARIABLE);
begin
  inherited;
//  CreateSubClass(Params, 'COMBOBOX');
  Params.Style := Params.Style or WS_CLIPCHILDREN or ComboBoxStyles[FStyle];
end;
{
const
  ComboBoxStyles: array[TComboBoxStyle] of DWORD = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWFIXED,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWVARIABLE);
  CharCases: array[TEditCharCase] of DWORD = (0, CBS_UPPERCASE, CBS_LOWERCASE);
  Sorts: array[Boolean] of DWORD = (0, CBS_SORT);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'COMBOBOX');
  with Params do
  begin
    Style := Style or (WS_VSCROLL or CBS_HASSTRINGS or CBS_AUTOHSCROLL) or
      ComboBoxStyles[FStyle] or Sorts[FSorted] or CharCases[FCharCase];
  end;
end;
}

procedure TElComboBox.WMThemeChanged(var Message: TMessage);
begin
  RecreateWnd;
  Message.Result := 1;
end;
function TElComboBox.GetListImageForm : TElImageForm;
begin
  result := FListBox.ImageForm;
end;

procedure TElComboBox.SetListImageForm(newValue : TElImageForm);
begin
  FListBox.ImageForm := newValue;
end;

destructor TElComboBox.Destroy;
begin
  FAltButton.Free;
  FButton.Free;
  FListBox.Free;
  FForm.Free;
  inherited;
end;

procedure TElComboBox.DoDropDown;
begin
  if Assigned(FOnDropDown) then FOnDropDown(Self);
end;

procedure TElComboBox.DropDown;
var
  P, P1: TPoint;
  PF   : TCustomForm;
begin
  if FForm.Visible then
  begin
    CloseUp(False);
  end
  else
  if CanDrop then
  begin
    if not Focused then SetFocus;
    if ReadOnly then
    begin
      SendMessage(Handle, EM_SETSEL, -1, -1);
      HideCaret(Handle);
    end;
    FDroppedDown := true;
    DoDropDown;

    if FDropDownWidth > 0 then
      FForm.Width := FDropDownWidth
    else
      FForm.Width := Width;
    FIgnoreItemIdx := true;
    FForcedText := true;

    if FListBox.Items.Count = 0 then
      FForm.Height := FListBox.ItemHeight
    else
    if FListBox.Items.Count < FDropDownCount then
      FForm.Height := FListBox.ItemHeight * FListBox.Items.Count
    else
      FForm.Height := FListBox.ItemHeight * FDropDownCount;
    FForcedText := false;
    FIgnoreItemIdx := false;
    FForm.Height := FForm.Height + GetSystemMetrics(SM_CYBORDER) * 2;
    P := Parent.ClientToScreen(Point(Left, Top));
    Inc(P.X, Width);
    Inc(P.Y, Height);

    if FDropDownAlignment = taRightJustify then
      Dec(P.X, FForm.Width)
    else
      Dec(P.X, self.Width);

    if P.Y + FForm.Height > Screen.Height then
      P.Y := P.Y - FForm.Height - Height;
    if AdjustDropDownPos then
    begin
      PF := GetParentForm(Self);
      P1 := Point(0, PF.ClientHeight);
      P1 := PF.ClientToScreen(P1);
      if P.Y + FForm.Height > P1.y then
      begin
        P1 := Point(0, 0);
        P1 := PF.ClientToScreen(P1);
        if P.Y - Height - FForm.Height >= P1.y then
        P.Y := P.Y - Height - FForm.Height;
      end;
    end;
    FListBox.ItemIndex := ItemIndex;

    SetWindowPos(FForm.Handle, HWND_TOPMOST, P.X, P.Y, 0, 0,
      SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
    FButton.FDown := true;
    FButton.Invalidate;
    FForm.Visible := True;
    SetWindowPos(FForm.Handle, HWND_TOPMOST, P.X, P.Y, 0, 0,
      SWP_NOACTIVATE or SWP_NOSIZE);
  end
  else
    FButton.Down := false;
end;

function TElComboBox.GetBtnColor: TColor;
begin
  Result := FButton.Color;
end;

procedure TElComboBox.GetDropDownValue;
begin
  FItemIndex := FListBox.ItemIndex;
  if FItemIndex > -1 then
    Text := FListBox.Items[FItemIndex]
  else
    Text := '';
  if AutoSelect and (Text <> '') then
    SelectAll;
end;

function TElComboBox.GetDroppedDown: boolean;
begin
  Result := FDroppedDown;
end;

function TElComboBox.GetItems: TElFStrings;
begin
  Result := FListBox.Items;
end;

function TElComboBox.GetListColor: TColor;
begin
  Result := FListBox.Color;
end;

function TElComboBox.GetListInvertSelection: boolean;
begin
  Result := FListBox.InvertSelection;
end;

function TElComboBox.GetSorted: boolean;
begin
  Result := FListBox.Sorted;
end;

function TElComboBox.GetUseBackground: boolean;
begin
  Result := inherited UseBackground;
end;

procedure TElComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ShortCut(Key, Shift) = ButtonShortCut then
    DropDown
  else
  if ShortCut(Key, Shift) = AltButtonShortCut then
    AltButtonClick(FAltButton)
  else
  if AutoCompletion and not FForm.Visible and (Shift = []) and (not ReadOnly) then
  begin
    if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END] then
      DoAutoComplete;
  end;
  if (Key = VK_DOWN) or (Key = VK_UP) then
    if Shift = [ssAlt] then
      DropDown
    else begin
      FListBox.Perform(WM_KEYDOWN, Key, 0);
      Key := 0;
    end
  else
  if ((Key = VK_NEXT) or (Key = VK_PRIOR)) and FForm.Visible then
  begin
    FListBox.Perform(WM_KEYDOWN, Key, 0);
    Key := 0;
  end
  else
  if (Key = VK_ESCAPE) and FForm.Visible then
  begin
    CloseUp(False);
    Key := 0;
  end
  else
  if (Key = VK_RETURN) and FForm.Visible then
  begin
    CloseUp(true);
    Key := 0;
  end;
  inherited;
end;

procedure TElComboBox.ListBoxClick(Sender: TObject);
begin
  if (FListBox.ItemIndex = -1) and (FListBox.Items.Count > 0) then
    FListBox.ItemIndex := 0;
  FIgnoreItemIdx := true;
  ItemIndex := FListBox.ItemIndex;
  FIgnoreItemIdx := false;
end;

procedure TElComboBox.Loaded;
var i : integer;
begin
  inherited;
  FListBox.RecreateWnd;
  i := FItemIndex;
  FItemIndex := -2;
  ItemIndex := i;
end;

procedure TElComboBox.SetBtnColor(const Value: TColor);
begin
  FButton.Color := Value;
end;

procedure TElComboBox.SetBtnTransparent(const Value: boolean);
begin
  if FBtnTransparent <> Value then
  begin
    FBtnTransparent := Value;
    FButton.Transparent := Value;
    Invalidate;
  end;  
end;

procedure TElComboBox.SetDropDownCount(const Value: integer);
begin
  if (FDropDownCount <> Value) and (Value > 0) then
    FDropDownCount := Value;
end;

procedure TElComboBox.SetCanDrop(const Value : boolean);
begin
  if FCanDrop <> Value then
  begin
    FCanDrop := Value;
    FButton.KeepColor := not CanDrop;
    FButton.Enabled := Value;
    if (not FCanDrop) then
       SetDroppedDown(false);
  end;
end;

procedure TElComboBox.SetDroppedDown(const Value: boolean);
begin
  if GetDroppedDown <> Value then DropDown;
end;

procedure TElComboBox.SetEditRect(Value : TRect);
var
  R: TRect;
  xoffs : integer;
begin
  if not HandleAllocated then exit;
  FButton.BoundsRect := Rect(ClientWidth - FButton.Width, 0, ClientWidth, ClientHeight);
  R := Rect(0, 0, ClientWidth, ClientHeight);
  if FButton.Visible then
    Dec(R.Right, FButton.Width);
  if FAltButton.Visible then
  begin
    if FAltBtnAlign = taLeftJustify then
    begin
      xOffs := FAltButton.Width;
      FAltButton.BoundsRect := Rect(0, 0, FAltButton.Width, ClientHeight);
    end
    else
    begin
      xOffs := 0;
      FAltButton.BoundsRect := Rect(R.Right - FAltButton.Width, 0, R.Right, ClientHeight);
      Dec(R.Right, FAltButton.Width);
    end;
  end
  else
    xOffs := 0;
  // Dec(R.Right, 3);
  R.Left := xOffs{ + 3};
  (*
  if BorderStyle = bsNone then
  begin
    R.Top := Max((ClientHeight - Abs(Font.Height)) div 2 - 1, 0);
    InflateRect(R, 0, 2);
    Perform(EM_SETRECTNP, 0, LongInt(@R));
    InflateRect(R, 0, -2);
    //R.Top := Max((ClientHeight - Abs(Font.Height)) div 2 - 1, 0);
  end;
  *)
  inherited SetEditRect(R);
end;

procedure TElComboBox.SetItemIndex(const Value: Integer);
var ChAllowed : boolean;
begin
  if ComponentState * [csLoading, csReading] <> [] then
    FItemIndex := Value
  else
  begin
    ChAllowed := ChangeAllowed;
    FListBox.OnClick := nil;
    FListBox.ItemIndex := Value;
    FListBox.OnClick := ListBoxClick;
    //if not DroppedDown then
    begin
      FItemIndex := Value;
      if not FForcedText then
      begin
        if (Value = -1) then
        begin
          if ReadOnly then
            inherited Text := '';
        end
        else
          inherited Text := Items[Value];
        SelectAll;
      end;
    end;
    if ChAllowed then
      Change;
  end;
end;

procedure TElComboBox.SetItems(const Value: TElFStrings);
begin
  FListBox.Items.Assign(Value);
end;

procedure TElComboBox.SetListColor(const Value: TColor);
begin
  FListBox.Color := Value;
end;

procedure TElComboBox.SetListInvertSelection(const Value: boolean);
begin
  FListBox.InvertSelection := Value;
end;

procedure TElComboBox.SetSorted(const Value: boolean);
begin
  FListBox.Sorted := Value;
end;

procedure TElComboBox.SetStyle(Value: TComboBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    ReadOnly := FStyle <> csDropDown;
    if FStyle = csOwnerDrawVariable then
      FListBox.Style := lbOwnerDrawVariable
    else
      FListBox.Style := lbOwnerDrawFixed;
    RecreateWnd;
  end;
end;

procedure TElComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
//  
end;

function TElComboBox.GetItemHeight: Integer;
begin
  Result := FListBox.ItemHeight;
end;

procedure TElComboBox.SetItemHeight(Value: Integer);
begin
  FListBox.ItemHeight := Value;
end;

procedure TElComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then FOnMeasureItem(Self, Index, Height);
end;

procedure TElComboBox.DrawItem(Index: Integer; R: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
begin
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, Index, R, State);
end;

procedure TElComboBox.Paint;
var
  R: TRect;
  {$ifndef VCL_5_USED}
  i : Word;
  T : StdCtrls.TOwnerDrawState;
  {$endif}
begin
  if (FItemIndex > -1) and ((FStyle = csOwnerDrawFixed) or (FStyle = csOwnerDrawVariable)) then
  begin
    R.Left := 0;
    R.Top := 0;
    R.Right := Width;
    R.Bottom := Height;
    FCanvas := FEditCanvas;
    {$ifdef VCL_5_USED}
    DrawItem(FItemIndex, R, [odComboBoxEdit]);
    {$else}
    i := 4096;
    Move(i, T, sizeof(i));
    DrawItem(FItemIndex, R, T);
    {$endif}
  end
  else
    inherited Paint;
end;

function TElComboBox.GetEditHandle:HWND;
begin
  Result := Handle;
end;

procedure TElComboBox.SetDropDownAlignment(Value: TAlignment);
begin
  if Value <> taCenter then
    FDropDownAlignment := Value;
end;

procedure TElComboBox.SetUseBackground(const Value: boolean);
begin
  if GetUseBackground <> Value then
  begin
    inherited UseBackground := Value;
    FListBox.UseBackground := Value;
  end;
end;

function TElComboBox.GetDroppedIndex : integer;
begin
  result := FListBox.ItemIndex;
end;
                    
procedure TElComboBox.WMKillFocus(var Msg: TMessage);
begin
  if not HandleAllocated then exit;
  CloseUp(False);
  FButton.Focused := False;
  inherited;
end;

procedure TElComboBox.WMSetFocus(var Msg: TMessage);
begin
  FButton.Focused := True;
  inherited;           
  if ReadOnly then
     HideCaret(Handle);
end;

procedure TElComboBox.SetBtnFlat(newValue : Boolean);
{ Sets data member FBtnFlat to newValue. }
begin
  if (FButton.FFlat <> newValue) then
  begin
    FBtnFlat := newValue;
    FButton.FFlat := newValue;
    Invalidate;
  end;  { if }
end;  { SetBtnFlat }

function TElComboBox.GetBtnArrowColor : TColor;
{ Returns the value for the BtnArrowColor property. }
begin
  GetBtnArrowColor := FButton.FArrowColor;
end;  { GetBtnArrowColor }

procedure TElComboBox.SetBtnArrowColor(newValue : TColor);
{ Sets the value for the BtnArrowColor property. }
begin
  if FButton.FArrowColor <> newValue then
  begin
    FButton.FArrowColor := newValue;
    Invalidate;
  end;
end;  { SetBtnArrowColor }

function TElComboBox.GetListSelectedColor : TColor;
begin
  result := FListBox.SelectedColor;
end;

procedure TElComboBox.SetListSelectedColor(newValue : TColor);
begin
  FListBox.SelectedColor := newValue;
end;

function TElComboBox.GetAltBtnColor : TColor;
begin
  result := FAltButton.Color;
end;

procedure TElComboBox.SetAltBtnColor(Value : TColor);
begin
  FAltButton.Color := Value;
end;

function TElComboBox.GetAltBtnTransparent : boolean;
begin
  result := FAltButton.Transparent;
end;

procedure TElComboBox.SetAltBtnTransparent(Value : boolean);
begin
  FAltButton.Transparent := Value;
  Invalidate;
end;

function TElComboBox.GetAltBtnFlat : boolean;
begin
  result := FAltButton.Flat;
end;

procedure TElComboBox.SetAltBtnFlat(Value : boolean);
begin
  FAltButton.Flat := Value;
  Invalidate;
end;

function TElComboBox.GetAltBtnGlyph : TBitmap;
begin
  result := FAltButton.Glyph;
end;

procedure TElComboBox.SetAltBtnGlyph(Value : TBitmap);
begin
  FAltButton.Glyph := Value;
end;

procedure TElComboBox.SetAltBtnWidth(Value : integer);
begin
  FAltBtnWidth := Value;
  FAltButton.Width := Value;
  if HandleAllocated then
  begin
    SetEditRect(ClientRect);
    Invalidate;
  end;
end;

function TElComboBox.GetAltBtnVisible : boolean;
begin
  result := FAltButton.Visible;
end;

procedure TElComboBox.SetAltBtnVisible(Value : boolean);
begin
  FAltButton.Visible := Value;
  if HandleAllocated then
  begin
    SetEditRect(ClientRect);
    Invalidate;
  end;
end;

procedure TElComboBox.SetAltBtnAlign(Value : TLeftRight);
begin
  FAltBtnAlign := Value;
  if HandleAllocated then
  begin
    SetEditRect(ClientRect);
    Invalidate;
  end;
end;

function TElComboBox.GetListTransparentSelection: Boolean;
begin
  Result := FListBox.TransparentSelection;
end;

procedure TElComboBox.SetListTransparentSelection(Value: Boolean);
begin
  FListBox.TransparentSelection := Value;
end;

procedure TElComboBox.SetDropDownWidth(const Value: Integer);
begin
  if Value < 1 then FDropDownWidth := -1 else FDropDownWidth := Value;
end;

function TElComboBox.GetBtnDrawFrame: Boolean;
begin
  Result := FButton.FDrawFrame;
end;

procedure TElComboBox.SetBtnDrawFrame(Value: Boolean);
begin
  FButton.FDrawFrame := Value;
end;

procedure TElComboBox.DoAutoComplete;
var i : integer;
    S : TElFString;
    ssl: Integer;
begin
  ssl := SelStart;
  S := Copy(Text, 1, ssl);
  if Length(S) > 0 then
  begin
    for i := 0 to Pred(FListBox.Items.Count) do
    begin
      {$ifdef ELPACK_UNICODE}
      if WidePos(S, FListBox.Items[i]) = 1 then
      {$else}
      if Pos(S, FListBox.Items[i]) = 1 then
      {$endif}
      begin
        Text := FListBox.Items[i];
        SelStart := ssl;
        SelLength := Length(Text) - ssl;
        Break;
      end;
    end;
  end;
end;

procedure TElComboBox.WMChar(var Message: TWMChar);
begin
  inherited;
  if AutoCompletion and (not ReadOnly) and (Message.CharCode <> VK_BACK) then
    DoAutoComplete;
end;

function TElComboBox.GetAltBtnDrawFrame: Boolean;
begin
  Result := FAltButton.DrawFrame;
end;

procedure TElComboBox.SetAltBtnDrawFrame(Value: Boolean);
begin
  FAltButton.DrawFrame := Value;
end;

procedure TElComboBox.SetReadOnly(Value : boolean);
begin
  inherited Readonly := Value;
  if ReadOnly then
  begin
    ControlStyle := ControlStyle - [csDoubleClicks];
  end
  else
  begin
    ControlStyle := ControlStyle + [csDoubleClicks];
  end;
end;

procedure TElComboBox.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  inherited;
  if HandleAllocated then
    FButton.Invalidate;
end;

procedure TElComboBox.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  inherited;
  if HandleAllocated then
    FButton.Invalidate;
end;

procedure TElComboBox.SetLineBorderActiveColor(Value: TColor);
begin
  inherited;
  if HandleAllocated then
    FButton.Invalidate;
end;

procedure TElComboBox.SetLineBorderInactiveColor(Value: TColor);
begin
  inherited;
  if HandleAllocated then
    FButton.Invalidate;
end;

procedure TElComboBox.SetFlat(const Value: boolean);
begin
  inherited;
  if HandleAllocated then
    FButton.Invalidate;
end;

procedure TElComboBox.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Message.CharCode = VK_RETURN) and FForm.Visible then
  begin                 
    CloseUp(true);
    Message.result := 1;
  end
  else
  if (Message.CharCode = VK_ESCAPE) and FForm.Visible then
  begin
    CloseUp(false);
    Message.result := 1;
  end
  else
    inherited;
end;

procedure TElComboBox.DestroyWnd;
begin
  if DroppedDown then
    CloseUp(false);
  inherited;
end;

procedure TElComboBox.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  if DroppedDown then
    Message.result := Message.Result or DLGC_WANTALLKEYS; 
end;

function TElComboBox.GetShowLineHint: Boolean;
begin
  Result := FListBox.ShowLineHint;
end;

procedure TElComboBox.SetShowLineHint(Value: Boolean);
begin
  FListBox.ShowLineHint := Value;
end;

end.
