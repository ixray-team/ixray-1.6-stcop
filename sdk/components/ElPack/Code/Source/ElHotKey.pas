{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
unit ElHotKey;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
{$ifdef VCL_6_USED}
  Types,
{$endif}
  ElXPThemedControl,
  ElEdits,
  ElStrUtils;

type
  TElHKInvalidKey = (hcShift, hcAlt, hcCtrl, hcAltShift, hcAltCtrl, hcCtrlShift,
    hcCtrlAlftShift);
  TElHKInvalidKeys = set of TElHKInvalidKey;
  TElHKModifier = (hkShift, hkAlt, hkCtrl);
  TElHKModifiers = set of TElHKModifier;

  TElHotKey = class(TCustomElEdit)
  private
    { Private declarations }
    FText: TElFString;
    FKeyPressed: boolean;
    FInvalidKeys: TElHKInvalidKeys;
    FModifiers: TElHKModifiers;
    FShiftState: TShiftState;
    procedure SetShortCut(newValue: TShortCut);
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSysKeyDown(var Message: TWMKeyDown); message WM_SYSKEYDOWN;
    procedure SetInvalidKeys(Value: TElHKInvalidKeys);
    procedure SetModifiers(Value: TElHKModifiers);
    function ShiftStateToText(state: TShiftState): TElFString;
    function GetShortCut: TShortCut;
    function GetShiftState(state: TShiftState): TShiftState;
  protected
    { Protected declarations }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

  published
    { Published declarations }
    property HotKey: TShortCut read GetShortCut write SetShortCut;
    property InvalidKeys: TElHKInvalidKeys read FInvalidKeys write
      SetInvalidKeys;
    property Modifiers: TElHKModifiers read FModifiers write SetModifiers;

    property AutoSize;
    property Alignment;
    property Background;
    property BorderSides;
    property UseBackground;
    property RTLContent;
    property Transparent;
    property ReadOnly;
    property LeftMargin;
    property RightMargin;
    property TopMargin;
    property BorderStyle;
    property HideSelection;
{$IFDEF ELPACK_COMPLETE}
    property ImageForm;
{$ENDIF}
    property ActiveBorderType;
    property Flat;
    property InactiveBorderType;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;
    property ChangeDisabledText;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnChange;

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
{$IFNDEF CLX_USED}
    property OnStartDock;
    property OnEndDock;
{$ENDIF}
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

resourcestring
  rsShiftP = 'Shift+';
  rsAltP = 'Alt+';
  rsCtrlP = 'Ctrl+';
  rsLeftP = 'Left+';
  rsRightP = 'Right+';
  rsMiddleP = 'Middle+';
  rsDoubleP = 'Double+';

const
  nshift: array[0..6] of string = (rsShiftP,
    rsAltP,
    rsCtrlP,
    rsLeftP,
    rsRightP,
    rsMiddleP,
    rsDoubleP);

implementation

constructor TElHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText := 'None';
  Text := 'None';
  FModifiers := [];
  FShiftState := [];
end;

procedure TElHotKey.KeyDown(var Key: Word; Shift: TShiftState);
var
  newText: TElFString;
begin
  inherited KeyDown(Key, Shift);
  if ReadOnly then exit;
  newText := '';
  fkeypressed := false;
  FShiftState := GetShiftState(shift);
  newText := ShiftStateToText(FShiftState);
  if (key < VK_SHIFT) or (key > VK_MENU) then
  begin
    case key of
//      VK_PAUSE:   newText := newText+'Pause';
//      VK_NUMLOCK: newText := newText+'NumLock';
      VK_BACK: newText := '';
    else
      newText := newText + ShortCutToText(ShortCut(key, []));
    end;
    fkeypressed := true;
  end;
  if newText = '' then
  begin
    text := 'None';
    newText := 'None';
  end
  else
    if newText <> Text then
      text := newText;
end;

procedure TElHotKey.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if ReadOnly then exit;
  if FKeyPressed then
    FText := Text;
//  OnChange(self);
end;

procedure TElHotKey.SetShortCut(newValue: TShortCut);
begin
  if not ReadOnly then
  begin
    FText := ShortCutToText(newValue);
    Text := FText;
  end;
end;

function TElHotKey.GetShortCut: TShortCut;
begin
  Result := TextToShortCut(FText);
end;

procedure TElHotKey.WMChar(var Message: TWMChar);
begin
end;

procedure TElHotKey.WMKeyUp(var Message: TWMKeyUp);
var
  ShiftState: TShiftState;
begin
  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    KeyUp(CharCode, ShiftState);
  end;
end;

procedure TElHotKey.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
begin
  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    KeyDown(CharCode, ShiftState);
  end;
end;

procedure TElHotKey.WMSysKeyDown(var Message: TWMKeyDown);
begin
  WMKeyDown(Message);
end;

procedure TElHotKey.SetInvalidKeys(Value: TElHKInvalidKeys);
var
  key: word;
begin
  if ReadOnly then exit;
  FInvalidKeys := Value;
  if (hcAlt in FInvalidKeys) and (hkAlt in FModifiers) then
    exclude(FModifiers, hkAlt);
  if (hcCtrl in FInvalidKeys) and (hkCtrl in FModifiers) then
    exclude(FModifiers, hkCtrl);
  if (hcShift in FInvalidKeys) and (hkShift in FModifiers) then
    exclude(FModifiers, hkShift);
  if (hcAltShift in FInvalidKeys) and (hkAlt in FModifiers) and (hkShift in
    FModifiers) then FModifiers := FModifiers - [hkAlt, hkShift];
  if (hcAltCtrl in FInvalidKeys) and (hkAlt in FModifiers) and (hkCtrl in
    FModifiers) then FModifiers := FModifiers - [hkAlt, hkCtrl];
  if (hcCtrlShift in FInvalidKeys) and (hkShift in FModifiers) and (hkCtrl in
    FModifiers) then FModifiers := FModifiers - [hkShift, hkCtrl];
  if (hcCtrlAlftShift in FInvalidKeys) and (FModiFiers = [hkCtrl, hkAlt,
    hkShift]) then FModifiers := [];
  key := VK_Shift;
  KeyDown(key, []);
end;

function TElHotKey.ShiftStateToText(state: TShiftState): TElFString;
var
  i: byte;
begin
  Result := '';
  if ssCtrl   in state then result:=result+'Ctrl+';
  if ssShift  in state then result:=result+'Shift+';
  if ssAlt    in state then result:=result+'Alt+';
  if ssLeft   in state then result:=result+'Left+';
  if ssRight  in state then result:=result+'Right+';
  if ssMiddle in state then result:=result+'Middle+';
  if ssDouble in state then result:=result+'Double+';

end;

procedure TElHotKey.SetModifiers(Value: TElHKModifiers);
var
  key: word;
begin
  if ReadOnly then exit;
  FModifiers := Value;
  if (hcAlt in FInvalidKeys) and (hkAlt in FModifiers) then
    exclude(FInvalidKeys, hcAlt);
  if (hcCtrl in FInvalidKeys) and (hkCtrl in FModifiers) then
    exclude(FInvalidKeys, hcCtrl);
  if (hcShift in FInvalidKeys) and (hkShift in FModifiers) then
    exclude(FInvalidKeys, hcShift);
  if (hcAltShift in FInvalidKeys) and (hkAlt in FModifiers) and (hkShift in
    FModifiers) then exclude(FInvalidKeys, hcAltShift);
  if (hcAltCtrl in FInvalidKeys) and (hkAlt in FModifiers) and (hkCtrl in
    FModifiers) then exclude(FInvalidKeys, hcAltCtrl);
  if (hcCtrlShift in FInvalidKeys) and (hkShift in FModifiers) and (hkCtrl in
    FModifiers) then exclude(FInvalidKeys, hcCtrlShift);
  if FModiFiers = [hkCtrl, hkAlt, hkShift] then
    exclude(FInvalidKeys, hcCtrlAlftShift);
  key := VK_Shift;
  KeyDown(key, []);
end;

function TElHotKey.GetShiftState(state: TShiftState): TShiftState;
var
  b: byte;
begin
  b := byte(word(state));
  b := b or byte(FModifiers);
  Result := TShiftState(word(b));
  if ([ssShift, ssAlt, ssCtrl] * Result) <> [] then
  begin
    if (hcCtrlAlftShift in FInvalidKeys) and
      (ssAlt in Result) and (ssCtrl in Result) and
      (ssShift in Result) then
      if (ssAlt in FShiftState) and (ssCtrl in FShiftState) then
        Exclude(Result, ssShift)
      else
        if (ssAlt in FShiftState) and (ssShift in FShiftState) then
        Exclude(Result, ssCtrl)
      else
        if (ssCtrl in FShiftState) and (ssShift in FShiftState) then
        Exclude(Result, ssAlt);

    if (hcAltShift in FInvalidKeys) and (ssAlt in Result) and (ssShift in Result)
      then
      if ssShift in FShiftState then
        Exclude(Result, ssAlt)
      else
        Exclude(Result, ssShift);

    if (hcAltCtrl in FInvalidKeys) and (ssAlt in Result) and (ssCtrl in Result)
      then
      if ssCtrl in FShiftState then
        Exclude(Result, ssAlt)
      else
        Exclude(Result, ssCtrl);

    if (hcCtrlShift in FInvalidKeys) and (ssCtrl in Result) and
      (ssShift in Result) then
      if ssCtrl in FShiftState then
        Exclude(Result, ssShift)
      else
        Exclude(Result, ssCtrl);

    if (hcAlt in FInvalidKeys) then Exclude(Result, ssAlt);
    if (hcCtrl in FInvalidKeys) then Exclude(Result, ssCtrl);
    if (hcShift in FInvalidKeys) then Exclude(Result, ssShift);
  end;
end;

end.

