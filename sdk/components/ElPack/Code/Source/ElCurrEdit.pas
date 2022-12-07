{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Portions Copyright (c) 2001, Akzhan Abdulin      }
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

03/09/2002

  ButtonCaption and CurrencySymbol made Unicode

03/06/2002

  Added unicode hint

02/21/2002

  Improved positioning of the text
  Changed editor ancestor to TElEdit

02/05/2002 (c) Akzhan Abdulin

  Calculation of width of fractional part editor fixed

01/23/2002

  Fixed vertical alignment of the parts

01/01/2002

  Fixed some problems with painting borders when focus is moved

12/24/2001

  Fixed sizing of editor parts when font is changed

12/21/2001

  When font is changed, the position of editor parts is recalculated now

10/25/2001

  Fixed painting of the borders with XP styles enabled

10/24/2001

  Editors' size is now adjusted correctly when the control is resized

08/21/2001 (c) Akzhan Abdulin

  Minor update for inplace editing support and cosmetic changes
  ReadOnly property added

07/15/2001 (c) Akzhan Abdulin

  AutoSelect property added.

  Sign, EnableSign and AbsoluteValue properties added.
  TElCurrencyEdit now supports negative values.

  NegativeSignColor and NegativeValueTextColor properties added.

  UpdatePartsWidth, AdjustEditorPositions and Paint methods were
  rewritten to be more precise.

  System currency symbol leading/trailing blanks now retrieved too. 

07/12/2001

  BorderSides property added.

05/20/2001 (c) Akzhan Abdulin

  Optional right-aligned button and its properties/events added

  TElCurrPartEdit class moved to implementation part,
  that so IElCurrPartEdit interface expanded and THackEdit class added

  TElCurrPartEdit ancestor replaced with TCustomElFlatEdit

  Changing of CurrencySymbolPosition now set UseSystemCurrencySymbol to false

04/12/2001 (c) Akzhan Abdulin

  Fractional part editor behaviour changed to use DecimalDigits correctly

  UpdatePartsWidth method updated and fixed to use AdjustEditorPositions method

  OnChange event now called when Modified set to True

  CurrencySymbol, CurrencySymbolPosition and UseSystemCurrencySymbol
  properties added

  Windows locale changing is handled

04/05/2001 (c) Akzhan Abdulin

  Integer part editor behaviour changed to override zero value
  Interfaces used to made this behavior class independent (like IntEditClass)

  Currency Editor now excludes floating point types and functions
  (floating point calculations are unsafe by precision)

  DecimalPlaces and UseSystemDecimalPlaces properties added

*)

unit ElCurrEdit;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  Math,
{$ifdef VCL_6_USED}
Types,
{$endif}
  Menus,
{$ifdef VCL_4_USED}
  ImgList,
{$endif}
  ElSndMap,
  ElTools,
  ElACtrls,
  ElPopBtn,
  ElEdits,
  ElUxTheme,
  ElTmSchema,
  ElXPThemedControl,
  ElStrUtils,
  ElVCLUtils;

const
  CURRBTNCOUNT = 1;
  CURRBTNMIN = 0;
  CURRBTNMAX = CURRBTNCOUNT - 1;
type


 TElCurrPartEdit = class(TElEdit)
  private
    FIsIntegerPart : boolean;
    OnPoint : TNotifyEvent;
    OnLeftPoint: TNotifyEvent;
  protected
    procedure WMChar(var Message: TMessage); message WM_CHAR;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
  end;
  IntEditClass  = TElCurrPartEdit;

  TElCurrencySymbolPosition = (ecsPosLeft, ecsPosRight);

  TElCurrencyEdit = class (TElXPThemedControl)
  protected
    FAbsValue : Currency;
    FDecimalPlaces: Byte;
    FUseSystemDecimalPlaces: Boolean;
    FCurrencySymbol: TElFString;
    FCurrencySymbolPosition: TElCurrencySymbolPosition;
    FUseSystemCurrencySymbol: Boolean;
    FBorderStyle: TBorderStyle;
    FFlat : boolean;
    FActiveBorderType: TElFlatBorderType;
    FInactiveBorderType: TElFlatBorderType;
    FMouseOver : boolean;
    FModified  : boolean;
    FDWidth    : integer;
    FSWidth    : integer;
    FBWidth    : integer;
    FSignWidth : integer;
    FDSWidth   : integer;

    FOnChange: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FBorderSides: TElBorderSides;

    FPartEditors: array[1..2] of IntEditClass;
    FButtons: array[CURRBTNMIN..CURRBTNMAX] of TElGraphicButton;
    FButtonClicks: array[CURRBTNMIN..CURRBTNMAX] of TNotifyEvent;
    FEnableSign: Boolean;
    FSign: Boolean;
    FNegativeValueTextColor: TColor;
    FNegativeSignColor: TColor;
    FHandleDialogKeys: Boolean;
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}
    FChangeDisabledText: Boolean;

    function FracValue(AValue : Currency) : string;
    procedure SetDecimalPlaces(const Value: Byte);
    procedure SetUseSystemDecimalPlaces(const Value: Boolean);
    function FindPart(Editor : IntEditClass) : integer;
    procedure SetFlat(const Value: boolean);
    procedure SetActiveBorderType(const Value: TElFlatBorderType);
    procedure SetInactiveBorderType(const Value: TElFlatBorderType);
    procedure SetBorderStyle(Value : TBorderStyle);
    procedure UpdateFrame;
    procedure SetModified(Value : boolean);
    procedure UpdateParts;
    procedure UpdatePartsWidth;
    procedure SetCurrencySymbol(const Value: TElFString);
    procedure SetUseSystemCurrencySymbol(const Value: Boolean);
    procedure SetCurrencySymbolPosition(
      const Value: TElCurrencySymbolPosition);
    function GetButtonCaption(Index: Integer): TElFString;
    function GetButtonColor(Index: Integer): TColor;
    function GetButtonDown(Index: Integer): Boolean;
    function GetButtonEnabled(Index: Integer): Boolean;
    function GetButtonFlat(Index: Integer): Boolean;
    function GetButtonVisible(Index: Integer): Boolean;
    function GetButtonWidth(Index: Integer): Integer;
    function GetButtonUseIcon(Index: Integer): Boolean;
    function GetButtonGlyph(Index: Integer): TBitmap;
    function GetButtonHint(Index: Integer): string;
    function GetButtonIcon(Index: Integer): TIcon;
    function GetButtonNumGlyphs(Index: Integer): integer;
    function GetButtonPopupPlace(Index: Integer): TPopupPlace;
    function GetButtonPullDownMenu(Index: Integer): TPopupMenu;
    function GetButtonUseImageList(Index: Integer): Boolean;
    function GetOnButtonClick(Index: Integer): TNotifyEvent;
    procedure SetButtonCaption(Index: Integer; Value: TElFString);
    procedure SetButtonColor(const Index: Integer; const Value: TColor);
    procedure SetButtonDown(const Index: Integer; const Value: Boolean);
    procedure SetButtonEnabled(const Index: Integer; const Value: Boolean);
    procedure SetButtonFlat(const Index: Integer; const Value: Boolean);
    procedure SetButtonVisible(const Index: Integer; const Value: Boolean);
    procedure SetButtonWidth(const Index, Value: Integer);
    procedure SetButtonUseIcon(const Index: Integer; const Value: Boolean);
    procedure SetButtonGlyph(const Index: Integer; const Value: TBitmap);
    procedure SetButtonHint(const Index: Integer; const Value: string);
    procedure SetButtonIcon(const Index: Integer; const Value: TIcon);
    procedure SetButtonNumGlyphs(const Index, Value: integer);
    procedure SetButtonPopupPlace(const Index: Integer;
      const Value: TPopupPlace);
    procedure SetButtonPullDownMenu(const Index: Integer;
      const Value: TPopupMenu);
    procedure SetButtonUseImageList(const Index: Integer;
      const Value: Boolean);
    procedure SetOnButtonClick(const Index: Integer; const Value: TNotifyEvent); { TNotifyEvent }
    function GetButtonDisabledImages(Index: Integer): TImageList;
    function GetButtonDownImages(Index: Integer): TImageList;
    function GetButtonHotImages(Index: Integer): TImageList;
    function GetButtonImageList(Index: Integer): TImageList;
    function GetButtonImageIndex(Index: Integer): TImageIndex;
    procedure SetButtonDisabledImages(const Index: Integer;
      const Value: TImageList);
    procedure SetButtonDownImages(const Index: Integer;
      const Value: TImageList);
    procedure SetButtonHotImages(const Index: Integer;
      const Value: TImageList);
    procedure SetButtonImageList(const Index: Integer;
      const Value: TImageList);
    procedure SetButtonImageIndex(const Index: Integer;
      const Value: TImageIndex);
    procedure ButtonClickTransfer(Sender : TObject);
    procedure SetBorderSides(Value: TElBorderSides);
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure SetEnableSign(const Value: Boolean);
    function GetSignValue: Currency;
    procedure SetSign(const Value: Boolean);
    procedure SetSignValue(const Value: Currency);
    procedure SetAutoSelect(const Value: Boolean);
    function GetAutoSelect: Boolean;
    procedure SetNegativeSignColor(const Value: TColor);
    procedure SetNegativeValueTextColor(const Value: TColor);
    procedure SetupPartsFont;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure OnEditorClick(Sender: TObject);
    procedure OnEditorDblClick(Sender: TObject);
    procedure OnEditorEndDrag(Sender: TObject; Target: TObject; X: Integer; Y: 
        Integer);
    procedure OnEditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X: Integer; Y: Integer);
    procedure OnEditorMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y:
        Integer);
    procedure OnEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X: Integer; Y: Integer);
    procedure OnEditorStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure OnEditorKeyPress(Sender: TObject; var Key: Char);
    procedure OnEditorDragDrop(Sender: TObject; Source: TObject; X: Integer; Y:
        Integer);
    procedure OnEditorDragOver(Sender: TObject; Source: TObject; X: Integer; Y:
        Integer; State: TDragState; var Accept: Boolean);

    function StoreCurrencySymbol: Boolean;
    function StoreDecimalPlaces: Boolean;

    procedure SetAbsValue(const Value: Currency);
    procedure DrawFlatBorder(DC: HDC);

    procedure AdjustEditorPositions; virtual;

    procedure CMCtl3DChanged(var Msg : TMessage); message CM_CTL3DCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMWinIniChange(var Msg : TMessage); message CM_WININICHANGE;

    procedure DoOnChange; dynamic;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;

    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure OnEditorChange(Sender : TObject);
    procedure OnEditorEnter(Sender :TObject);
    procedure OnEditorExit(Sender :TObject);
    procedure OnEditorPoint(Sender : TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnEditorLeftPoint(Sender : TObject);
    procedure OnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetThemedClassName: WideString; override;
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
    procedure SetChangeDisabledText(Value: Boolean);

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Loaded; override;

    property MouseOver : boolean read FMouseOver;
    property Modified  : boolean read FModified write SetModified;
    property AbsoluteValue: Currency read FAbsValue write SetAbsValue;
  published
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property Value: Currency read GetSignValue write SetSignValue;
    property Sign: Boolean read FSign write SetSign stored false;
    property EnableSign: Boolean read FEnableSign write SetEnableSign default False;

    property NegativeSignColor: TColor read FNegativeSignColor write SetNegativeSignColor default clNone;
    property NegativeValueTextColor: TColor read FNegativeValueTextColor write SetNegativeValueTextColor default clNone;

    property DecimalPlaces: Byte read FDecimalPlaces write SetDecimalPlaces stored StoreDecimalPlaces;
    property UseSystemDecimalPlaces: Boolean read FUseSystemDecimalPlaces write SetUseSystemDecimalPlaces default True;

    property CurrencySymbol: TElFString read FCurrencySymbol write
        SetCurrencySymbol stored StoreCurrencySymbol;
    property CurrencySymbolPosition: TElCurrencySymbolPosition read FCurrencySymbolPosition write SetCurrencySymbolPosition stored StoreCurrencySymbol default ecsPosRight;
    property UseSystemCurrencySymbol: Boolean read FUseSystemCurrencySymbol write SetUseSystemCurrencySymbol default False;

    property ButtonCaption: TElFString index 0 read GetButtonCaption write
        SetButtonCaption;
    property ButtonColor: TColor index 0 read GetButtonColor write SetButtonColor default clBtnFace;
    property ButtonDown: Boolean index 0 read GetButtonDown write SetButtonDown default false;
    property ButtonEnabled: Boolean index 0 read GetButtonEnabled write SetButtonEnabled default true;
    property ButtonFlat: Boolean index 0 read GetButtonFlat write SetButtonFlat default false;
    property ButtonUseIcon: Boolean index 0 read GetButtonUseIcon write SetButtonUseIcon default false;
    property ButtonVisible: Boolean index 0 read GetButtonVisible write SetButtonVisible default false;
    property ButtonWidth: Integer index 0 read GetButtonWidth write SetButtonWidth default 15;
    property ButtonGlyph : TBitmap index 0 read GetButtonGlyph write SetButtonGlyph;
    property ButtonHint : string index 0 read GetButtonHint write SetButtonHint;
    property ButtonIcon : TIcon index 0 read GetButtonIcon write SetButtonIcon;
    property ButtonNumGlyphs : integer index 0 read GetButtonNumGlyphs write SetButtonNumGlyphs default 1;
    property ButtonPopupPlace : TPopupPlace index 0 read GetButtonPopupPlace write SetButtonPopupPlace;
    property ButtonPullDownMenu : TPopupMenu index 0 read GetButtonPullDownMenu write SetButtonPullDownMenu;
    property ButtonUseImageList : Boolean index 0 read GetButtonUseImageList write SetButtonUseImageList default false;
    property ButtonImages : TImageList index 0 read GetButtonImageList write SetButtonImageList;
    property ButtonDownImages : TImageList index 0 read GetButtonDownImages write SetButtonDownImages;
    property ButtonHotImages : TImageList index 0 read GetButtonHotImages write SetButtonHotImages;
    property ButtonDisabledImages : TImageList index 0 read GetButtonDisabledImages write SetButtonDisabledImages;
    property ButtonImageIndex: TImageIndex index 0 read GetButtonImageIndex write SetButtonImageIndex;
    property OnButtonClick : TNotifyEvent index 0 read GetOnButtonClick write SetOnButtonClick;
    property BorderStyle : TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Flat : boolean read FFlat write SetFlat default false;
    property ActiveBorderType: TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;
    property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    property HandleDialogKeys: Boolean read FHandleDialogKeys write FHandleDialogKeys;
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
        SetLineBorderInactiveColor;
    property ChangeDisabledText: Boolean read FChangeDisabledText write 
        SetChangeDisabledText default false;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

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
    property ParentColor default false;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default true;
    property Visible;
    property UseXPThemes;

    property OnClick;
    {$IFDEF VCL_5_USED}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF VCL_4_USED}
    property OnEndDock;
    property OnEndDrag;
    {$ENDIF}
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF VCL_4_USED}
    property OnStartDock;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

const
  SSignStr = String(' -');
type
  THackWinControl = class(TWinControl);
  THackEdit = class(TCustomElFlatEdit);

     TElCurrEditButton = class(TElGraphicButton)
     protected
       function GetThemedClassName: WideString; override;
       function GetThemePartID: Integer; override;
       procedure DrawThemedBackground(Canvas : TCanvas); override;
     end;

{
********************************** TElCurrPartEdit ***********************************
}

procedure TElCurrPartEdit.WMChar(var Message: TMessage);
begin
  if (TWMChar(Message).CharCode = Ord(FormatSettings.DecimalSeparator))
     or (TWMChar(Message).CharCode = Ord('.'))
     or (TWMChar(Message).CharCode = Ord(','))
  then
    if Assigned(OnPoint) then
    begin
      OnPoint(Self);
    end
    else
  else
  begin
    if FIsIntegerPart then
    begin
      if
        (TWMChar(Message).CharCode >= Ord('0'))
        and (TWMChar(Message).CharCode <= Ord('9'))
        and (Text = '0')
      then
      begin
        Text := '';
      end;
      if
        (TWMChar(Message).CharCode = Ord('-'))
        and Assigned(Parent) and (Parent is TElCurrencyEdit)
        and TElCurrencyEdit(Parent).EnableSign
      then
      begin
        TElCurrencyEdit(Parent).Sign := not TElCurrencyEdit(Parent).Sign;
        TElCurrencyEdit(Parent).SetModified(True);
        Message.Result := 0;
        Exit;
      end;
    end;
    inherited;
  end;
end;

procedure TElCurrPartEdit.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  if TElCurrencyEdit(Parent).HandleDialogKeys then
    Message.Result := Message.Result or DLGC_WANTALLKEYS; 
  //Message.Result := DLGC_WANTARROWS;
end;

procedure TElCurrPartEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  if (Message.CharCode = VK_LEFT) and (KeyDataToShiftState(Message.KeyData) = []) then
  begin
    if (SelStart = 0) then
    begin
      if Assigned(OnLeftPoint) then
        OnLeftPoint(Self);
    end
    else
      inherited;
  end
  else
  if (Message.CharCode = VK_RIGHT) and
     (KeyDataToShiftState(Message.KeyData) = []) then
  begin
    if (SelStart = Length(Text)) then
    begin
      if Assigned(OnPoint) then
        OnPoint(Self)
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TElCurrPartEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or ES_NUMBER);
end;

procedure TElCurrPartEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  with Parent as TElCurrencyEdit do
    if FFlat or IsThemeApplied then
      DrawFlatBorder(0);
end;

procedure TElCurrPartEdit.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  with Parent as TElCurrencyEdit do
    if FFlat or IsThemeApplied then
      DrawFlatBorder(0);
end;


{
********************************** TElCurrencyEdit ***********************************
}

procedure TElCurrencyEdit.SetAbsValue(const Value: Currency);
begin
  if Value >= 0.0 then
  begin
    if FAbsValue <> Value then
    begin
      FAbsValue := Value;
      UpdateParts;
    end;
  end;
end;

procedure TElCurrencyEdit.UpdateParts;
begin
  FPartEditors[1].Text := IntToStr(Trunc(FAbsValue));
  FPartEditors[2].Text := FracValue(FAbsValue);
  UpdatePartsWidth;
end;

constructor TElCurrencyEdit.Create(AOwner : TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  {$ifdef MSWINDOWS}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}

  FBorderStyle := bsSingle;
  FDecimalPlaces := FormatSettings.CurrencyDecimals;
  FUseSystemDecimalPlaces := True;
  FCurrencySymbol := '';
  FCurrencySymbolPosition := ecsPosRight;
  FUseSystemCurrencySymbol := False;
  FEnableSign := False;
  FSign := False;
  FNegativeValueTextColor := clNone;
  FNegativeSignColor := clNone;

  for i :=  1 to 2 do
  begin
    FPartEditors[i] := IntEditClass.Create(Self);
    with FPartEditors[i] do
    begin
      Parent := Self;
      BorderStyle := bsNone;
      OnChange := OnEditorChange;
      OnExit   := OnEditorExit;
      OnPoint  := OnEditorPoint;
      OnLeftPoint := OnEditorLeftPoint;
      OnKeyDown := OnEditorKeyDown;
      OnKeyPress := OnEditorKeyPress;
      OnKeyUp := OnEditorKeyUp;
      OnStartDrag := OnEditorStartDrag;
      OnEndDrag := OnEditorEndDrag;
      OnDragOver := OnEditorDragOver;
      OnDragDrop := OnEditorDragDrop;
      OnMouseDown := OnEditorMouseDown;
      OnMouseMove := OnEditorMouseMove;
      OnMouseUp := OnEditorMouseUp;
      OnClick := OnEditorClick;
      OnDblClick := OnEditorDblClick;

      AutoSelect := True;
      AutoSize := false;
      if i = 1 then
        Text := '0'
      else
        Text := '00';
      ParentColor := False;
      Color := Self.Color;
      ParentFont := False;
      Font := Self.Font;
      TabStop := false;
      LeftMargin := 0;
      RightMargin := 0;
    end;
  end;
  FPartEditors[1].Alignment := taRightJustify;
  FPartEditors[2].Alignment := taLeftJustify;
  FPartEditors[2].MaxLength := 0;
  FPartEditors[1].FIsIntegerPart := true;

  FInactiveBorderType := fbtSunkenOuter;
  FActiveBorderType := fbtSunken;

  TabStop := true;
  Width := 100;
  Height := 21;
  ParentColor := false;
  Color := clWindow;

  for i := CURRBTNMIN to CURRBTNMAX do
  begin
    FButtons[i] := TElCurrEditButton.Create(nil);
    with FButtons[i] do
    begin
      Color := clBtnFace;
      Parent := Self;
      OnClick := ButtonClickTransfer;
      Width := 15;
      Flat := false;
      Visible := false;
      TabStop := false;
      if csDesigning in ComponentState then
        Enabled := false;
    end;
  end;
end;

destructor TElCurrencyEdit.Destroy;
var
  I: Integer;
begin
  for I := CURRBTNMIN to CURRBTNMAX do
  begin
    FButtons[I].Free;
  end;
  inherited Destroy;
end;

procedure TElCurrencyEdit.DrawFlatBorder(DC: HDC);
var
  BorderType: TElFlatBorderType;
  MustRelease: boolean;
  R,
  R1: TRect;
  b : boolean;
  ax,
  ay: integer;
  iStateId: Integer;
  AColor : TColor;
begin
  if ((not FFlat) and (not IsThemeApplied)) or (BorderStyle = bsNone) then
    exit;

  MustRelease := (DC = 0);
  if MustRelease then DC := GetWindowDC(Handle);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);

  try
    if IsThemeApplied then
    begin
      iStateId := 0;
      if not Enabled then
      begin
        iStateId := ETS_DISABLED;
      end;
      R1 := ClientRect;
      R1.TopLeft := Parent.ScreenToClient(ClientToScreen(R1.TopLeft));

      ax := Left - R1.Left;
      ay := Top  - R1.Top;

      R1 := ClientRect;
      OffsetRect(R1, -ax, -ay);

      with R1 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      (*
      for i := 1 to 2 do
      begin
        R1 := FPartEditors[i].BoundsRect;
        OffsetRect(R1, -ax, -ay);
        with R1 do
          ExcludeClipRect(DC, Left, Top, Right, Bottom);
      end;
      for i := 0 to CURRBTNCOUNT - 1 do
      if FButtons[i].Visible then
      begin
        R1 := FButtons[i].BoundsRect;
        OffsetRect(R1, -ax, -ay);
        with R1 do
          ExcludeClipRect(DC, Left, Top, Right, Bottom);
      end;
      *)
      DrawThemeBackground(Theme, DC, 0, iStateId, R, nil);
    end
    else
    begin
      b := Focused or (GetParent(GetFocus) = Handle) or FMouseOver;
      if b then
        BorderType := FActiveBorderType
      else
        BorderType := FInactiveBorderType;
      if b then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;

      DrawFlatFrameEx2(DC, R, AColor, Color, b, Enabled, FBorderSides, BorderType);
    end;
    finally
      if MustRelease then
        ReleaseDC(Handle, DC);
  end;
end;

procedure TElCurrencyEdit.Paint;
var cx: integer;
    s : string;
    eh: integer;
    
    function GetEditorHeight : integer;
    var //I : integer;
        DC: HDC;
        SysMetrics,
        Metrics: TTextMetric;
        SaveFont : HFont;
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      //I := GetSystemMetrics(SM_CYEDGE) * 2;
      result := Metrics.tmHeight{ + I};
    end;

begin
  if FFlat and not IsThemeApplied then
    DrawFlatBorder(Canvas.Handle);

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);

  eh :=  Abs(Font.Height) + 2 + (ClientHeight - Abs(Font.Height)) div 2;
  SetTextAlign(Canvas.Handle, TA_BOTTOM);

  if DecimalPlaces > 0 then
  begin
    cx := ClientWidth;
    Dec(cx, FBWidth + FDWidth + FDSWidth);
    if CurrencySymbolPosition = ecsPosRight then
    begin
      Dec(cx, FSWidth);
    end;
    S := FormatSettings.DecimalSeparator;
    TextOut(Canvas.Handle, cx, eh, PChar(S), Length(S));
  end;

  if CurrencySymbol <> '' then
  begin
    if CurrencySymbolPosition = ecsPosLeft then
    begin
      cx := FSignWidth;
    end
    else
    begin
      cx := ClientWidth - FSWidth - FBWidth;
    end;
    S := CurrencySymbol;
    TextOut(Canvas.Handle, cx, eh, PChar(S), Length(S));
  end;

  if Sign then
  begin
    S := SSignStr;
    if NegativeSignColor <> clNone then Canvas.Font.Color := NegativeSignColor;
    TextOut(Canvas.Handle, 0, eh, PChar(s), Length(S));
  end;
  SetTextAlign(Canvas.Handle, TA_BOTTOM);
end;

procedure TElCurrencyEdit.AdjustEditorPositions;
var
  x : integer;
  h : integer;
  I: Integer;
  w: Integer;
  le: Integer;

begin
  x := ClientWidth;
  h := ClientHeight;

  FBWidth := 0;
  for I := CURRBTNMAX downto CURRBTNMIN do
  begin
    if FButtons[I].Visible then
    begin
      w := FButtons[I].Width;
      FButtons[I].BoundsRect := Rect(x - w, 0, x, h);
      Dec(x, w);
      Inc(FBWidth, w);
    end;
  end;
  le := FSignWidth;
  if CurrencySymbolPosition = ecsPosLeft then
  begin
    Inc(le, FSWidth);
  end
  else
  begin
    Dec(x, FSWidth);
  end;
  Dec(x, FDWidth);
  FPartEditors[1].BoundsRect := Rect(le, 0, x - FDSWidth, h);
  FPartEditors[2].BoundsRect := Rect(x, 0, x + FDWidth, h);
  for i := 1 to 2 do
  begin
    FPartEditors[i].AlignBottom := false;
    FPartEditors[i].TopMargin := (ClientHeight - Abs(Font.Height)) div 2;
  end;

  Invalidate;
end;

procedure TElCurrencyEdit.CMColorChanged(var Message: TMessage);
var i : integer;
begin
  inherited;
  for i := 1 to 2 do
    FPartEditors[i].Color := Color;
end;


procedure TElCurrencyEdit.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if FFlat and (not Focused) and (not IsThemeApplied) then
    DrawFlatBorder(0);
  DoMouseEnter;
end;

procedure TElCurrencyEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if FFlat and (not Focused) and (not IsThemeApplied) then
    DrawFlatBorder(0);
  DoMouseLeave;
end;

procedure TElCurrencyEdit.CMEnabledChanged(var Message: TMessage);
var i : integer;
begin
  inherited;
  for i := 1 to 2 do
    FPartEditors[i].Enabled := Enabled;
  UpdateFrame;
end;

procedure TElCurrencyEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetupPartsFont;
  UpdatePartsWidth;
  // AdjustEditorPositions;
  UpdateFrame;
end;

procedure TElCurrencyEdit.CMSysColorChange(var Message: TMessage);
var i : integer;
begin
  inherited;
  for i := 1 to 2 do
    FPartEditors[i].Color := Color;
  Invalidate;
  UpdateFrame;
end;

procedure TElCurrencyEdit.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.result := 1;
end;

procedure TElCurrencyEdit.WMNCPaint(var Msg : TMessage);
var
  DC : HDC;
begin
  if not Flat and (BorderStyle = bsSingle) then
    inherited;
  if Flat or IsThemeApplied then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC = 0 then
    begin
      DC := GetWindowDC(Handle);
    end;
      DrawFlatBorder(DC);
    ReleaseDC(Handle, DC);
    Msg.Result := 0;
  end;
     end;

procedure TElCurrencyEdit.WMSetFocus(var Msg: TMessage);
var R, C : TWinControl;
begin
  inherited;
  R := FindControl(Msg.WParam);
  if (R <> nil) and (FindPart(IntEditClass(R)) <> -1) then
  begin
   C := THackWinControl(Parent).FindNextControl(Self, false, true, true);
   if C <> nil then
     C.SetFocus;
  end
  else
    FPartEditors[1].SetFocus;
  if FFlat and not FMouseOver then
    DrawFlatBorder(0);
end;

procedure TElCurrencyEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  if not HandleAllocated then exit;
  if FFlat and not FMouseOver then
    DrawFlatBorder(0);
end;

procedure TElCurrencyEdit.WMSize(var Message: TMessage);
begin
  inherited;
  AdjustEditorPositions;
end;

procedure TElCurrencyEdit.CreateWindowHandle(const Params: TCreateParams);
var i : integer;
begin
  inherited;
  for i :=  1 to 2 do
    SetWindowLong(FPartEditors[i].Handle, GWL_STYLE, GetWindowLong(
      FPartEditors[i].Handle, GWL_STYLE) or ES_NUMBER);
  UpdatePartsWidth;
end;

function TElCurrencyEdit.FindPart(Editor : IntEditClass) : integer;
begin
  result := -1;
  if Editor = FPartEditors[1] then
    result := 1
  else
  if Editor = FPartEditors[2] then
    result := 2;
end;

procedure TElCurrencyEdit.OnEditorChange(Sender : TObject);
var i, j : integer;
    p    : integer;
    s    : string;
begin
  p := FindPart(IntEditClass(Sender));

  S := IntEditClass(Sender).Text;

  if p = 1 then
  begin
    val(S, i, j);
    if (j = 0) then
    begin
      FAbsValue := i + FAbsValue - Trunc(FAbsValue);
      SetModified(true);
      exit;
    end;
    if Length(s) > 0 then
    begin
      if IntEditClass(Sender).HandleAllocated then
        i := IntEditClass(Sender).SelStart
      else
        i := 0;
      FPartEditors[1].Text := IntToStr(Trunc(FAbsValue));
      if IntEditClass(Sender).HandleAllocated then
        IntEditClass(Sender).SelStart := Min(i, Length(IntEditClass(Sender).Text));
    end;
  end
  else
  begin
    S := '0' + FormatSettings.DecimalSeparator + Copy(S, 1, DecimalPlaces);
    FAbsValue := StrToCurr(S) + Trunc(FAbsValue);
    UpdatePartsWidth;
    SetModified(true);

    if Length(S) > 0 then
    begin
      if IntEditClass(Sender).HandleAllocated then
        i := IntEditClass(Sender).SelStart
      else
        i := 0;
      FPartEditors[2].Text := FracValue(FAbsValue);
      UpdatePartsWidth;
      if IntEditClass(Sender).HandleAllocated then
        IntEditClass(Sender).SelStart := Min(i, Length(IntEditClass(Sender).Text));
    end;
  end;
end;

procedure TElCurrencyEdit.OnEditorEnter(Sender :TObject);
begin
//  IntEditClass(Sender).SelStart := 0;
//  IntEditClass(Sender).SelLength := 0;
end;

procedure TElCurrencyEdit.OnEditorExit(Sender :TObject);
begin
  IntEditClass(Sender).SelLength := 0;
  if IntEditClass(Sender).Text = '' then
  begin
    if Sender = FPartEditors[1] then
      FPartEditors[1].Text := IntToStr(Trunc(FAbsValue))
    else
    if Sender = FPartEditors[2] then
      if FAbsValue - Trunc(FAbsValue) = 0 then
        FPartEditors[2].Text := '0'
      else
        FPartEditors[2].Text := FracValue(FAbsValue);
    OnEditorChange(Sender);
  end;
  DoExit;
end;

procedure TElCurrencyEdit.OnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TElCurrencyEdit.OnEditorPoint(Sender : TObject);
begin
  if Sender = FPartEditors[1] then
    FPartEditors[2].SetFocus;
end;

procedure TElCurrencyEdit.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TElCurrencyEdit.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    if (not Focused) and (not FMouseOver) then
      UpdateFrame;
  end;
end;

procedure TElCurrencyEdit.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    if (Focused or FMouseOver) then
      UpdateFrame;
  end;
end;

procedure TElCurrencyEdit.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if HandleAllocated then
      if Flat then
        Invalidate
      else
        RecreateWnd;
  end;
end;

procedure TElCurrencyEdit.SetBorderStyle(Value : TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TElCurrencyEdit.SetModified(Value : boolean);
begin
  FModified := Value;
  if FModified then
    DoOnChange;
end;

procedure TElCurrencyEdit.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TElCurrencyEdit.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TElCurrencyEdit.UpdateFrame;
var R : TRect;
begin
  if Flat and HandleAllocated then
  begin
    R := Rect(0, 0, Width, Height);
    if (BorderStyle = bsSingle) and (not (csDestroying in ComponentState)) and (HandleAllocated) then
       RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame );
  end;
end;

procedure TElCurrencyEdit.OnEditorLeftPoint(Sender : TObject);
begin
  if (Sender = FPartEditors[2]) then
    FPartEditors[1].SetFocus;
end;

procedure TElCurrencyEdit.UpdatePartsWidth;
var
  S: string;
  IC: Char;
  CW: Integer;

begin
  if HandleAllocated then
  begin
    Canvas.Font.Assign(Font);

    // calculate sign symbol width in pixels
    if EnableSign then
      FSignWidth := Canvas.TextWidth(SSignStr)
    else
      FSignWidth := 0;

    // calculate decimal separator width in pixels
    FDSWidth := Canvas.TextWidth(FormatSettings.DecimalSeparator);

    // calculate fractional part editor width in pixels
    FDWidth := 0;
    if DecimalPlaces > 0 then
    begin
      SetLength(S, DecimalPlaces);
      for IC := '0' to '9' do
      begin
        FillChar(S[1], DecimalPlaces, IC);
        CW := Canvas.TextWidth(S);
        if CW > FDWidth then
        begin
          FDWidth := CW;
        end;
      end;
      // caret space
      Inc(FDWidth, 2);
    end;

    // calculate currency symbol width in pixels
    if CurrencySymbol <> '' then
      FSWidth := Canvas.TextWidth(CurrencySymbol)
    else
      FSWidth := 0;
    // adjust editor parts placement
    AdjustEditorPositions;
  end;
end;

procedure TElCurrencyEdit.Loaded;
begin
  inherited;
  UpdatePartsWidth;
end;

function TElCurrencyEdit.FracValue(AValue: Currency): string;
var
  IPos: Integer;
begin
  Result := CurrToStrF(AValue, ffFixed, DecimalPlaces);
  IPos := Pos(FormatSettings.DecimalSeparator, Result);
  if IPos > 0 then
    Result := Copy(Result, Succ(IPos), Length(Result))
  else
    Result := '';
  while Length(Result) < DecimalPlaces do Result := Result + '0';
end;

procedure TElCurrencyEdit.SetDecimalPlaces(const Value: Byte);
begin
  if FDecimalPlaces <> Value then
  begin
    FDecimalPlaces := Value;
    FUseSystemDecimalPlaces := False;
    UpdateParts;
  end;
end;

procedure TElCurrencyEdit.SetUseSystemDecimalPlaces(const Value: Boolean);
begin
  if FUseSystemDecimalPlaces <> Value then
  begin
    if Value then
    begin
      DecimalPlaces := FormatSettings.CurrencyDecimals;
    end;
    FUseSystemDecimalPlaces := Value;
  end;
end;

function TElCurrencyEdit.StoreDecimalPlaces: Boolean;
begin
  Result := not FUseSystemDecimalPlaces;
end;

procedure TElCurrencyEdit.DoOnChange;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

procedure TElCurrencyEdit.SetCurrencySymbol(const Value: TElFString);
begin
  if FCurrencySymbol <> Value then
  begin
    FCurrencySymbol := Value;
    UpdatePartsWidth;
    FUseSystemCurrencySymbol := False;
    Invalidate;
  end;
end;

procedure TElCurrencyEdit.SetUseSystemCurrencySymbol(const Value: Boolean);
begin
  if FUseSystemCurrencySymbol <> Value then
  begin
    if Value then
    begin
      CurrencySymbol := FormatSettings.CurrencyString;
      case FormatSettings.CurrencyFormat of
        0: CurrencySymbolPosition := ecsPosLeft;
        1: CurrencySymbolPosition := ecsPosRight;
        2:
        begin
          CurrencySymbolPosition := ecsPosLeft;
          CurrencySymbol := CurrencySymbol + ' ';
        end;
        3:
        begin
          CurrencySymbolPosition := ecsPosRight;
          CurrencySymbol := ' ' + CurrencySymbol;
        end;
      end;
    end;
    FUseSystemCurrencySymbol := Value;
  end;
end;

function TElCurrencyEdit.StoreCurrencySymbol: Boolean;
begin
  Result := not FUseSystemCurrencySymbol;
end;

procedure TElCurrencyEdit.SetCurrencySymbolPosition(
  const Value: TElCurrencySymbolPosition);
begin
  if FCurrencySymbolPosition <> Value then
  begin
    FCurrencySymbolPosition := Value;
    FUseSystemCurrencySymbol := False;
    AdjustEditorPositions;
  end;
end;

procedure TElCurrencyEdit.CMWinIniChange(var Msg: TMessage);
begin
  Inherited;
  Msg.Result := 0;
  if Application.UpdateFormatSettings then
  begin
    if UseSystemDecimalPlaces then
    begin
      UseSystemDecimalPlaces := False;
      UseSystemDecimalPlaces := True;
    end;
    if UseSystemCurrencySymbol then
    begin
      UseSystemCurrencySymbol := False;
      UseSystemCurrencySymbol := True;
    end;
    Invalidate;
  end;
end;

function TElCurrencyEdit.GetButtonCaption(Index: Integer): TElFString;
begin
  Result := FButtons[Index].Caption;
end;

function TElCurrencyEdit.GetButtonColor(Index: Integer): TColor;
begin
  Result := FButtons[Index].Color;
end;

function TElCurrencyEdit.GetButtonDown(Index: Integer): Boolean;
begin
  Result := FButtons[Index].Down;
end;

function TElCurrencyEdit.GetButtonEnabled(Index: Integer): Boolean;
begin
  Result := FButtons[Index].Enabled;
end;

function TElCurrencyEdit.GetButtonFlat(Index: Integer): Boolean;
begin
  Result := FButtons[Index].Flat;
end;

function TElCurrencyEdit.GetButtonVisible(Index: Integer): Boolean;
begin
  Result := FButtons[Index].Visible;
end;

function TElCurrencyEdit.GetButtonWidth(Index: Integer): Integer;
begin
  Result := FButtons[Index].Width;
end;

function TElCurrencyEdit.GetButtonUseIcon(Index: Integer): Boolean;
begin
  Result := FButtons[Index].UseIcon;
end;

function TElCurrencyEdit.GetButtonGlyph(Index: Integer): TBitmap;
begin
  Result := FButtons[Index].Glyph;
end;

function TElCurrencyEdit.GetButtonHint(Index: Integer): string;
begin
  Result := FButtons[Index].Hint;

end;

function TElCurrencyEdit.GetButtonIcon(Index: Integer): TIcon;
begin
  Result := FButtons[Index].Icon;
end;

function TElCurrencyEdit.GetButtonNumGlyphs(Index: Integer): integer;
begin
  Result := FButtons[Index].NumGlyphs;
end;

function TElCurrencyEdit.GetButtonPopupPlace(Index: Integer): TPopupPlace;
begin
  Result := FButtons[Index].PopupPlace;
end;

function TElCurrencyEdit.GetButtonPullDownMenu(Index: Integer): TPopupMenu;
begin
  Result := FButtons[Index].PullDownMenu;
end;

function TElCurrencyEdit.GetButtonUseImageList(Index: Integer): Boolean;
begin
  Result := FButtons[Index].UseImageList;
end;

function TElCurrencyEdit.GetOnButtonClick(Index: Integer): TNotifyEvent;
begin
  Result := FButtonClicks[Index];
end;

procedure TElCurrencyEdit.SetButtonCaption(Index: Integer; Value: TElFString);
begin
  FButtons[Index].Caption := Value;
end;

procedure TElCurrencyEdit.SetButtonColor(const Index: Integer;
  const Value: TColor);
begin
  FButtons[Index].Color := Value;
end;

procedure TElCurrencyEdit.SetButtonDown(const Index: Integer;
  const Value: Boolean);
begin
  FButtons[Index].Down := Value;
end;

procedure TElCurrencyEdit.SetButtonEnabled(const Index: Integer;
  const Value: Boolean);
begin
  FButtons[Index].Enabled := Value;
end;

procedure TElCurrencyEdit.SetButtonFlat(const Index: Integer;
  const Value: Boolean);
begin
  FButtons[Index].Flat := Value;
end;

procedure TElCurrencyEdit.SetButtonVisible(const Index: Integer;
  const Value: Boolean);
begin
  if FButtons[Index].Visible <> Value then
  begin
    FButtons[Index].Visible := Value;
    AdjustEditorPositions;
  end;
end;

procedure TElCurrencyEdit.SetButtonWidth(const Index, Value: Integer);
begin
  if FButtons[Index].Width <> Value then
  begin
    FButtons[Index].Width := Value;
    if FButtons[Index].Visible then AdjustEditorPositions;
  end;
end;

procedure TElCurrencyEdit.SetButtonUseIcon(const Index: Integer;
  const Value: Boolean);
begin
  FButtons[Index].UseIcon := Value;
end;

procedure TElCurrencyEdit.SetButtonGlyph(const Index: Integer;
  const Value: TBitmap);
begin
  FButtons[Index].Glyph := Value;
end;

procedure TElCurrencyEdit.SetButtonHint(const Index: Integer;
  const Value: string);
begin
  FButtons[Index].Hint := Value;
end;

procedure TElCurrencyEdit.SetButtonIcon(const Index: Integer;
  const Value: TIcon);
begin
  FButtons[Index].Icon := Value;
end;

procedure TElCurrencyEdit.SetButtonNumGlyphs(const Index, Value: integer);
begin
  FButtons[Index].NumGlyphs := Value;
end;

procedure TElCurrencyEdit.SetButtonPopupPlace(const Index: Integer;
  const Value: TPopupPlace);
begin
  FButtons[Index].PopupPlace := Value;
end;

procedure TElCurrencyEdit.SetButtonPullDownMenu(const Index: Integer;
  const Value: TPopupMenu);
begin
  FButtons[Index].PullDownMenu := Value;
end;

procedure TElCurrencyEdit.SetButtonUseImageList(const Index: Integer;
  const Value: Boolean);
begin
  FButtons[Index].UseImageList := Value;
end;

procedure TElCurrencyEdit.SetOnButtonClick(const Index: Integer;
  const Value: TNotifyEvent);
begin
  FButtonClicks[Index] := Value;
end;

procedure TElCurrencyEdit.ButtonClickTransfer(Sender: TObject);
var
  I: Integer;
begin
  for I := CURRBTNMIN to CURRBTNMAX do
  begin
    if Sender = FButtons[I] then
    begin
      if Assigned(FButtonClicks[I]) then FButtonClicks[I](Self);
      Break;
    end;
  end;
end;

function TElCurrencyEdit.GetButtonDisabledImages(Index: Integer): TImageList;
begin
  Result := FButtons[Index].DisabledImages;
end;

function TElCurrencyEdit.GetButtonDownImages(Index: Integer): TImageList;
begin
  Result := FButtons[Index].DownImages;
end;

function TElCurrencyEdit.GetButtonHotImages(Index: Integer): TImageList;
begin
  Result := FButtons[Index].HotImages;
end;

function TElCurrencyEdit.GetButtonImageList(Index: Integer): TImageList;
begin
  Result := FButtons[Index].Images;
end;

function TElCurrencyEdit.GetButtonImageIndex(Index: Integer): TImageIndex;
begin
  Result := FButtons[Index].ImageIndex;
end;

procedure TElCurrencyEdit.SetButtonDisabledImages(const Index: Integer;
  const Value: TImageList);
begin
  FButtons[Index].DisabledImages := Value;
end;

procedure TElCurrencyEdit.SetButtonDownImages(const Index: Integer;
  const Value: TImageList);
begin
  FButtons[Index].DownImages := Value;
end;

procedure TElCurrencyEdit.SetButtonHotImages(const Index: Integer;
  const Value: TImageList);
begin
  FButtons[Index].HotImages := Value;
end;

procedure TElCurrencyEdit.SetButtonImageList(const Index: Integer;
  const Value: TImageList);
begin
  FButtons[Index].Images := Value;
end;

procedure TElCurrencyEdit.SetButtonImageIndex(const Index: Integer;
  const Value: TImageIndex);
begin
  FButtons[Index].ImageIndex := Value;
end;
procedure TElCurrencyEdit.SetEnableSign(const Value: Boolean);
begin
  if FEnableSign <> Value then
  begin
    FEnableSign := Value;
    if FSign and not EnableSign then
    begin
      Sign := False;
    end;
    UpdatePartsWidth;
  end;
end;

procedure TElCurrencyEdit.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TElCurrencyEdit.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) and (not (ThemesAvailable and UseXPThemes)) then
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




function TElCurrencyEdit.GetSignValue: Currency;
begin
  if Sign then
  begin
    Result := - FAbsValue;
  end
  else
  begin
    Result := FAbsValue;
  end;
end;

procedure TElCurrencyEdit.SetSign(const Value: Boolean);
begin
  if FSign <> Value then
  begin
    if Value and not EnableSign then Exit;
    FSign := Value;
    SetupPartsFont;
    Invalidate;
  end;
end;

procedure TElCurrencyEdit.SetSignValue(const Value: Currency);
begin
  if Value < 0.0 then
  begin
    SetSign(True);
    SetAbsValue(- Value);
  end
  else
  begin
    SetSign(False);
    SetAbsValue(Value);
  end;
end;

procedure TElCurrencyEdit.SetAutoSelect(const Value: Boolean);
begin
  if AutoSelect <> Value then
  begin
    FPartEditors[1].AutoSelect := Value;
    FPartEditors[2].AutoSelect := Value;
  end;
end;

function TElCurrencyEdit.GetAutoSelect: Boolean;
begin
  Result := FPartEditors[1].AutoSelect;
end;

procedure TElCurrencyEdit.SetNegativeSignColor(const Value: TColor);
begin
  if FNegativeSignColor <> Value then
  begin
    FNegativeSignColor := Value;
    if Sign then
    begin
      Invalidate;
    end;
  end;
end;

procedure TElCurrencyEdit.SetNegativeValueTextColor(const Value: TColor);
begin
  if FNegativeValueTextColor <> Value then
  begin
    FNegativeValueTextColor := Value;
    if Sign then
    begin
      SetupPartsFont;
    end;
  end;
end;

procedure TElCurrencyEdit.SetupPartsFont;
var
  F: TFont;
  I: Integer;
begin
  if Sign and (NegativeValueTextColor <> clNone) then
  begin
    F := TFont.Create;
    try
      F.Assign(Font);
      F.Color := NegativeValueTextColor;
      for I := 1 to 2 do
      begin
        FPartEditors[I].Font.Assign(F);
      end;
    finally
      F.Free;
    end;
  end
  else
  begin
    for I := 1 to 2 do
    begin
      FPartEditors[I].Font.Assign(Font);
    end;
  end;
end;

function TElCurrencyEdit.GetReadOnly: Boolean;
begin
  Result := FPartEditors[1].ReadOnly;
end;

procedure TElCurrencyEdit.SetReadOnly(const Value: Boolean);
var
  I: Integer;
begin
  for I := 1 to 2 do
  begin
    FPartEditors[I].ReadOnly := Value;
  end;
end;

procedure TElCurrencyEdit.OnEditorClick(Sender: TObject);
begin
  Click;
end;  { ClickHandler }

procedure TElCurrencyEdit.OnEditorDblClick(Sender: TObject);
begin
  DblClick;
end;  { DblClickHandler }

procedure TElCurrencyEdit.OnEditorEndDrag(Sender: TObject; Target: TObject; X: 
    Integer; Y: Integer);
begin
  DoEndDrag(Target, X, Y);
end;  { EndDragHandler }

procedure TElCurrencyEdit.OnEditorKeyUp(Sender: TObject; var Key: Word; Shift: 
    TShiftState);
begin
  KeyUp(Key, Shift);
end;  { KeyUpHandler }

procedure TElCurrencyEdit.OnEditorMouseDown(Sender: TObject; Button: 
    TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  MouseDown(Button, Shift, X, Y);
end;  { MouseDownHandler }

procedure TElCurrencyEdit.OnEditorMouseMove(Sender: TObject; Shift: TShiftState;
    X: Integer; Y: Integer);
begin
  MouseMove(Shift, X, Y);
end;  { MouseMoveHandler }

procedure TElCurrencyEdit.OnEditorMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X: Integer; Y: Integer);
begin
  MouseUp(Button, Shift, X, Y);
end;  { MouseUpHandler }

procedure TElCurrencyEdit.OnEditorStartDrag(Sender: TObject; var DragObject: 
    TDragObject);
begin
  DoStartDrag(DragObject);
end;  { StartDragHandler }

procedure TElCurrencyEdit.OnEditorKeyPress(Sender: TObject; var Key: Char);
begin
  KeyPress(Key);
end;  { KeyPressHandler }

procedure TElCurrencyEdit.OnEditorDragDrop(Sender: TObject; Source: TObject; X: 
    Integer; Y: Integer);
begin
  DragDrop(Source, X, Y);
end;  { DragDropHandler }

procedure TElCurrencyEdit.OnEditorDragOver(Sender: TObject; Source: TObject; X: 
    Integer; Y: Integer; State: TDragState; var Accept: Boolean);
begin
  DragOver(Source, X, Y, State, Accept);
end;  { DragOverHandler }

function TElCurrencyEdit.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

procedure TElCurrencyEdit.CMCtl3DChanged(var Msg : TMessage);
begin
  inherited;
  RecreateWnd;
  // SetEditRect;
end; { CMCtl3DChanged }

procedure TElCurrencyEdit.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TElCurrencyEdit.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then
    if HandleAllocated then
      Invalidate;
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElCurrencyEdit.CMHintShow(var Message: TMessage);
{$else}
function TElCurrencyEdit.HintShow(var HintInfo : THintInfo): Boolean; 
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

procedure TElCurrencyEdit.SetHint(Value: WideString);
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

procedure TElCurrencyEdit.SetChangeDisabledText(Value: Boolean);
var i : integer;
begin
  if FChangeDisabledText <> Value then
  begin
    FChangeDisabledText := Value;
    for I := 1 to 2 do
    begin
      FPartEditors[I].ChangeDisabledText := Value;
    end;
  end;
end;

function TElCurrEditButton.GetThemedClassName: WideString;
begin
  Result := 'BUTTON';
end;

function TElCurrEditButton.GetThemePartID: Integer;
begin
  result := BP_PUSHBUTTON;
  ShowGlyph := not IsThemeApplied;
end;

procedure TElCurrEditButton.DrawThemedBackground(Canvas : TCanvas);
begin
  inherited;
end;

end.


