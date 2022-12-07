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

07/15/2002

  Some button properties (MoneyFlat*, FlatAlways) were not set in run-time. Fixed. 

07/14/2002

  Fixed height setting of the items in group boxes (spoiled on 06/04)

07/04/2002

  Setting MoneyFlat didn't change the flag for items

06/24/2002

  OnClick is now fired only once when clicking the radio button in ElRadioGroup
  VertOffset property published and corrected.

06/04/2002

  ItemSpacing and ItemHeight properties added.
  HorzOffset property added

04/09/2002

  Clearing items caused error when ItemIndex <> -1 in ElRadioGroup. Fixed. 

03/10/2002

  Hints property is Unicode now
  Incorrect typecast for FItems in destructor fixed 

01/27/2002

  ItemIndex was not changed after keyboard operations

12/18/2001

  FlatAlways property made published
  Fixed reading of ElRadioGroup.ItemIndex property

10/13/2001

  UseXPThemes made published

*)

unit ElCheckItemGrp;

interface

uses
  SysUtils,
  Classes,
{$ifndef CLX_USED}
{$ifdef VCL_6_USED}
  Types,
{$endif}
  Stdctrls,
  Messages,
  Controls,
  Windows,
  Graphics,
  ElImgFrm,
  ExtCtrls,
  Forms,
  {$ifdef VCL_4_USED}
  ImgList,
  {$endif}
  {$else}
  {$ifdef MSWINDOWS}
  // Windows,
  {$endif}
  QForms,
  QStdCtrls,
  QControls,
  QGraphics,
  QTypes,
  Qt,
  Types,
  QImgList,
  ElCLXUtils,
  {$endif}
{$ifdef ELPACK_UNICODE}
  ElUnicodeStrings,
{$endif}
  ElStrUtils,
  ElList,
  HTMLRender,
  ElTools,
  ElTmSchema,
  ElUxTheme,
  ElCheckCtl,
  ElGroupBox
  {$ifdef USE_SOUND_MAP}
  , ElSndMap
  {$endif}
  ;

type

  TElCheckItemClass = class of TElCheckItem;

  TElCheckItemGroup = class(TCustomElGroupBox)
  protected
    FAlignment: TLeftRight;
    FButtons: TElList;
    FColumns: Integer;
    FHints: TElFStrings;
    FItems: TElFStrings;
    FReading: Boolean;
    FUpdating: boolean;
    FHorzOffset: Integer;
    FItemHeight: Integer;
    FItemSpacing: Integer;
    FVertOffset: Integer;
    procedure ArrangeButtons;
    {$ifndef CLX_USED}
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$else}
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    {$endif}
    procedure HintsChange(Sender: TObject);
    procedure ItemsChange(Sender: TObject); virtual;
    procedure SetAlignment(newValue: TLeftRight);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetFlat(newValue : boolean); override;
    procedure SetHints(Value: TElFStrings);
    procedure SetItems(Value: TElFStrings);
    procedure UpdateButtons; virtual;
    {$ifndef CLX_USED}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$else}
    procedure Resize; override;
    {$endif}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ReadState(Reader: TReader); override;
    {$ifndef CLX_USED}
    procedure SetImageForm(Value : TElImageForm); override;
    {$endif}
    {$ifdef HAS_HTML_RENDER}
    procedure SetIsHTML(Value: Boolean); override;
    {$endif}
    procedure SetTransparent(newValue : boolean); override;
    procedure SetUseXPThemes(const Value: Boolean); override;
    procedure IntCreateItem; virtual; abstract;
    procedure ButtonClick(Sender: TObject); virtual;
    {$ifdef USE_SOUND_MAP}
    procedure SetCheckSound(Value: TElSoundName); override;
    {$endif}
    procedure SetGlyph(Value: TBitmap); override;
    procedure SetImages(Value: TImageList); override;
    {$ifdef USE_SOUND_MAP}
    procedure SetSoundMap(Value: TElSoundMap); override;
    {$endif}
    procedure SetUseCustomGlyphs(Value: Boolean); override;
    procedure SetUseImageList(Value: Boolean); override;
    procedure SetCheckboxChecked(Value: Boolean); override;
    procedure SetFlatAlways(Value: Boolean);
    {$ifndef CLX_USED}
    procedure SetMoneyFlatInactiveColor(Value: TColor); override;
    procedure SetMoneyFlatActiveColor(Value: TColor); override;
    procedure SetMoneyFlatDownColor(Value: TColor); override;
    procedure SetMoneyFlat(Value: Boolean); override;
    {$endif}
    function GetItemEnabled(Index: Integer): Boolean;

    procedure SetItemEnabled(Index: Integer; Value: Boolean);
    procedure SetHorzOffset(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetItemSpacing(Value: Integer);
    procedure SetVertOffset(Value: Integer);
    procedure Loaded; override;

    property Alignment: TLeftRight read FAlignment write SetAlignment default
        taRightJustify;
    property Columns: Integer read FColumns write SetColumns default 1;
    property Hints: TElFStrings read FHints write SetHints;
    property Items: TElFStrings read FItems write SetItems;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write
        SetItemEnabled;
    property HorzOffset: Integer read FHorzOffset write SetHorzOffset default 0;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default -1;
    property ItemSpacing: Integer read FItemSpacing write SetItemSpacing default -1;
    property VertOffset: Integer read FVertOffset write SetVertOffset default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef VCL_4_USED}
    procedure FlipChildren(AllLevels: Boolean); override;
    {$endif}
  end;

type
  TCustomElRadioGroup = class (TElCheckItemGroup)
  private
    FItemIndex: Integer;

    procedure SetItemIndex(Value: Integer);
  protected
    procedure IntCreateItem; override;
    procedure UpdateButtons; override;
    procedure ButtonClick(Sender: TObject); override;
    procedure ItemsChange(Sender: TObject); override;

    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TElRadioGroup = class(TCustomElRadioGroup)
  public
    property ItemEnabled;
  published
    property Align;
    property Alignment;
{$IFDEF VCL_4_USED}
    property Anchors;
    {$ifndef CLX_USED}
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$endif}
{$ENDIF}
    property BorderSides;
    property Caption;
    property CaptionColor;
    property CheckBoxChecked;
    property Color;
    property Columns;
    {$ifndef CLX_USED}
    property Ctl3D;
    property DragCursor;
    {$endif}
    property DragMode;
    property Enabled;
    property Flat;
    property FlatAlways;
    property Font;
    property Hints;
    property HorzOffset;
    {$ifndef CLX_USED}
    property ImageForm;
    {$endif}
  {$ifdef HAS_HTML_RENDER}
    property IsHTML;
  {$endif}
    property ItemIndex;
    property Items;
    property ItemHeight;
    property ItemSpacing;
    {$ifndef CLX_USED}
    property MoneyFlat;
    property MoneyFlatInactiveColor;
    property MoneyFlatActiveColor;
    property MoneyFlatDownColor;
    {$endif}
    property ParentColor;
    {$ifndef CLX_USED}
    property ParentCtl3D;
    {$endif}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCheckBox;
    property ShowFocus;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property VertOffset;
    property UseXPThemes;

    {$ifdef USE_SOUND_MAP}
    property CheckSound;
    property SoundMap;
    {$endif}
    property Glyph;
    property Images;
    property UseCustomGlyphs;
    property UseImageList;

    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

type

  TCustomElCheckGroup = class(TElCheckItemGroup)
  private
    function GetChecked(Index: Integer): Boolean;
    procedure SetChecked(Index: Integer; Value: Boolean);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetState(Index: Integer; Value: TCheckBoxState);
  protected
    FAllowGrayed: Boolean;
    procedure SetAllowGrayed(Value: Boolean);
    procedure IntCreateItem; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
  published
    property AllowGrayed: Boolean read FAllowGrayed write SetAllowGrayed default true;
  end;

  TElCheckGroup = class(TCustomElCheckGroup)
  public
    property ItemEnabled; 
  published
    property Align;
    property Alignment;
{$IFDEF VCL_4_USED}
    property Anchors;
    {$ifndef CLX_USED}
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$endif}
{$ENDIF}
    property BorderSides;
    property Caption;
    property CaptionColor;
    property CheckBoxChecked; 
    property Color;
    property Columns;
    {$ifndef CLX_USED}
    property Ctl3D;
    property DragCursor;
    {$endif}
    property DragMode;
    property Enabled;
    property Flat;
    property FlatAlways;
    property Font;
    property Hints;
    property HorzOffset;
    {$ifndef CLX_USED}
    property ImageForm;
    {$endif}
    {$ifdef HAS_HTML_RENDER}
    property IsHTML;
    {$endif}
    property Items;
    property ItemHeight;
    property ItemSpacing;
    {$ifndef CLX_USED}
    property MoneyFlat;
    property MoneyFlatInactiveColor;
    property MoneyFlatActiveColor;
    property MoneyFlatDownColor;
    {$endif}
    property ParentColor;
    {$ifndef CLX_USED}
    property ParentCtl3D;
    {$endif}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCheckBox;
    property ShowFocus;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property UseXPThemes;
    property VertOffset;

    {$ifdef USE_SOUND_MAP}
    property CheckSound;
    property SoundMap;
    {$endif}
    property Glyph;
    property Images;
    property UseCustomGlyphs;
    property UseImageList;


    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

implementation

type TElGroupCheckItem = class(TElCheckItem);

constructor TElCheckItemGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks];
  FButtons := TElList.Create;
  FHints := TElFStringList.Create;
  TElFStringList(FHints).OnChange := HintsChange;
  FItems := TElFStringList.Create;
  TElFStringList(FItems).OnChange := ItemsChange;
  FColumns := 1;
  FAlignment := taRightJustify;
  FItemHeight := -1;
  FItemSpacing := -1;
end;

destructor TElCheckItemGroup.Destroy;
begin
  SetButtonCount(0);
  TElFStringList(FItems).OnChange := nil;
  FHints.Free;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TElCheckItemGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  {$ifndef CLX_USED}
  DeferHandle: THandle;
  {$endif}
  ALeft: Integer;
  TopOffset : integer;
  TopPos, AHeight : integer;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    TopOffset := GetTopOffset;
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns - HorzOffset;
    I := Self.Height - TopOffset - 5;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := GetTopOffset + 1;
    {$ifndef CLX_USED}
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    {$endif}
    try

      if (ItemSpacing = -1) or (ItemHeight = -1) then
      begin
        AHeight := ButtonHeight;
        inc(TopMargin, (I mod ButtonsPerCol) div 2);
      end
      else
      begin
        AHeight := ItemHeight;
        TopMargin := GetTopOffset + 1;
      end;

      for I := 0 to FButtons.Count - 1 do
      begin

        if (ItemSpacing = -1) or (ItemHeight = -1) then
        begin
          TopPos := AHeight * (I mod ButtonsPerCol);
        end
        else
        begin
          TopPos := (AHeight + ItemSpacing) * (I mod ButtonsPerCol);
        end;

        Inc(TopPos, TopMargin + VertOffset);

        with TElGroupCheckItem(FButtons[I]) do
        begin
          Visible := True;
          AutoSize := false;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + HorzOffset + 5;
          {$ifndef CLX_USED}
          {$IFDEF VCL_4_USED}
          BiDiMode := Self.BiDiMode;
          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth - 5;
          {$ENDIF}
          {$endif}
          {$ifndef CLX_USED}
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft, TopPos, ButtonWidth, AHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          {$else}
          SetBounds(ALeft, TopPos, ButtonWidth, AHeight);
          {$endif}
        end;
      end;
    finally
      {$ifndef CLX_USED}
      EndDeferWindowPos(DeferHandle);
      {$endif}
    end;
  end;
end;

{$ifndef CLX_USED}
procedure TElCheckItemGroup.CMEnabledChanged(var Message: TMessage);
{$else}
procedure TElCheckItemGroup.EnabledChanged;
{$endif}
var
 I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    TElGroupCheckItem(FButtons[I]).Enabled := Enabled;
end;

{$ifndef CLX_USED}
procedure TElCheckItemGroup.CMFontChanged(var Message: TMessage);
{$else}
procedure TElCheckItemGroup.FontChanged;
{$endif}
begin
  inherited;
  if ComponentState * [csLoading, csReading] = [] then
    ArrangeButtons;
end;

{$ifdef VCL_4_USED}
procedure TElCheckItemGroup.FlipChildren(AllLevels: Boolean);
begin
end;
{$endif}

procedure TElCheckItemGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TElCheckItemGroup.HintsChange(Sender: TObject);
begin
  if not FReading then
  begin
    UpdateButtons;
  end;
end;

procedure TElCheckItemGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    UpdateButtons;
    if ComponentState * [csLoading, csReading] = [] then
      ArrangeButtons;
  end;
end;

procedure TElCheckItemGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TElCheckItemGroup.SetAlignment(newValue: TLeftRight);
var
  i : integer;
begin
  if (newValue <> FAlignment) then
  begin
    FAlignment:= newValue;
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).Alignment := FAlignment;
  end;
end;

procedure TElCheckItemGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    IntCreateItem;

  while FButtons.Count > Value do
    TElGroupCheckItem(FButtons.Last).Free;
end;

procedure TElCheckItemGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TElCheckItemGroup.SetFlat(newValue : boolean);
var
  i : integer;
begin
  if (newValue <> FFlat) then
  begin
    FFlat := newValue;
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).Flat := FFlat;
  end;
end;

procedure TElCheckItemGroup.SetHints(Value: TElFStrings);
begin
  FHints.Assign(Value);
end;

{$ifndef CLX_USED}
procedure TElCheckItemGroup.SetImageForm(Value : TElImageForm);
var i : integer;
begin
  inherited;
  for i := 0 to FButtons.Count - 1 do
    TElGroupCheckItem(FButtons[i]).ImageForm := Value;
end;
{$endif}

{$ifdef HAS_HTML_RENDER}
procedure TElCheckItemGroup.SetIsHTML(Value: Boolean);
var i : integer;
begin
  if IsHTML <> Value then
  begin
    inherited;
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).IsHTML := IsHTML;
  end;
end;
{$endif}

procedure TElCheckItemGroup.SetItems(Value: TElFStrings);
begin
  FItems.Assign(Value);
end;

procedure TElCheckItemGroup.SetTransparent(newValue: Boolean);
var i : integer;
begin
  if (Transparent <> newValue) then
  begin
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).Transparent := newValue;
    inherited;
  end;
end;

procedure TElCheckItemGroup.SetUseXPThemes(const Value: Boolean);
var i : integer;
begin
  inherited;
  for i := 0 to FButtons.Count - 1 do
    TElGroupCheckItem(FButtons[i]).UseXPThemes := UseXPThemes;
end;

procedure TElCheckItemGroup.UpdateButtons;
var
  I: Integer;
  Btn : TElGroupCheckItem;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
  begin
    Btn := TElGroupCheckItem(FButtons[I]);
    Btn.Caption := FItems[I];
    Btn.Alignment := Alignment;
    Btn.Transparent := Transparent;
    Btn.Flat := Flat;
    if FHints.Count > I then
      Btn.Hint := FHints[I];
{$ifdef HAS_HTML_RENDER}
    Btn.IsHTML := IsHTML;
{$endif}
    {$ifdef USE_SOUND_MAP}
    Btn.SoundMap := SoundMap;
    Btn.Checksound := CheckSound;
    {$endif}
    Btn.Images := Images;
    Btn.UseCustomGlyphs := UseCustomGlyphs;
    Btn.UseImageList := UseImageList;
    Btn.Glyph := Glyph;
    Btn.FlatAlways := true;
    {$ifndef CLX_USED}
    Btn.MoneyFlat := MoneyFlat;
    Btn.MoneyFlatActiveColor := MoneyFlatActiveColor;
    Btn.MoneyFlatInactiveColor := MoneyFlatInactiveColor;
    Btn.MoneyFlatDownColor := MoneyFlatDownColor;
    {$endif}
    Btn.Enabled := Enabled and ((not ShowCheckBox) or CheckboxChecked);
  end;
end;

{$ifndef CLX_USED}
procedure TElCheckItemGroup.WMSize(var Message: TWMSize);
{$else}
procedure TElCheckItemGroup.Resize;
{$endif}
begin
  inherited;
  if ComponentState * [csLoading, csReading] = [] then
    ArrangeButtons;
end;

procedure TElCheckItemGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    // Changed;
    Click;
  end;
end;

{$ifdef USE_SOUND_MAP}
procedure TElCheckItemGroup.SetCheckSound(Value: TElSoundName);
var i : integer;
begin
  if FCheckSound <> Value then
  begin
    inherited;
    FCheckSound := Value;
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).CheckSound := Value;
  end;
end;
{$endif}

procedure TElCheckItemGroup.SetGlyph(Value: TBitmap);
var i : integer;
begin
  inherited;
  for i := 0 to FButtons.Count - 1 do
    TElGroupCheckItem(FButtons[i]).Glyph := Value;
end;

procedure TElCheckItemGroup.SetImages(Value: TImageList);
var i : integer;
begin
  if FImages <> Value then
  begin
    inherited;
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).Images := Value;
  end;
end;

{$ifdef USE_SOUND_MAP}
procedure TElCheckItemGroup.SetSoundMap(Value: TElSoundMap);
var i : integer;
begin
  if FSoundMap <> Value then
  begin
    inherited;
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).SoundMap := Value;
  end;
end;
{$endif}

procedure TElCheckItemGroup.SetUseCustomGlyphs(Value: Boolean);
var i : integer;
begin
  if FUseCustomGlyphs <> Value then
  begin
    inherited;
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).UseCustomGlyphs := Value;
  end;
end;

procedure TElCheckItemGroup.SetUseImageList(Value: Boolean);
var i : integer;
begin
  if FUseImageList <> Value then
  begin
    inherited;
    for i := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[i]).UseImageList := Value;
  end;
end;

procedure TElCheckItemGroup.SetCheckboxChecked(Value: Boolean);
var i : integer;
begin
  if FCheckboxChecked <> Value then
  begin
    inherited;
    for I := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[I]).Enabled := Enabled and ((not ShowCheckBox) or CheckboxChecked);
  end;
end;

procedure TElCheckItemGroup.SetFlatAlways(Value: Boolean);
var i : integer;
begin
  if FFlatAlways <> Value then
  begin
    inherited;
    for I := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[I]).FlatAlways := Value;
  end;
end;

{$ifndef CLX_USED}
procedure TElCheckItemGroup.SetMoneyFlatInactiveColor(Value: TColor);
var i : integer;
begin
  if FMoneyFlatInactiveColor <> Value then
  begin
    inherited;
    for I := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[I]).FMoneyFlatInactiveColor := Value;
  end;
end;

procedure TElCheckItemGroup.SetMoneyFlatActiveColor(Value: TColor);
var i : integer;
begin
  if MoneyFlatActiveColor <> Value then
  begin
    inherited;
    for I := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[I]).MoneyFlatActiveColor := Value;
  end;
end;

procedure TElCheckItemGroup.SetMoneyFlatDownColor(Value: TColor);
var i : integer;
begin
  if MoneyFlatDownColor <> Value then
  begin
    inherited;
    for I := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[I]).MoneyFlatDownColor := Value;
  end;
end;

procedure TElCheckItemGroup.SetMoneyFlat(Value: Boolean);
var i : integer;
begin
  if MoneyFlat <> Value then
  begin
    inherited;
    for I := 0 to FButtons.Count - 1 do
      TElGroupCheckItem(FButtons[I]).MoneyFlat := Value;
  end;
end;

{$endif}

function TElCheckItemGroup.GetItemEnabled(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < Items.Count) then
    result := TElCheckItem(FButtons[Index]).Enabled
  else
    result := false;
end;

procedure TElCheckItemGroup.SetItemEnabled(Index: Integer; Value: Boolean);
begin
  if (Index >= 0) and (Index < Items.Count) then
    TElCheckItem(FButtons[Index]).Enabled := Value;
end;

procedure TElCheckItemGroup.SetHorzOffset(Value: Integer);
begin
  if FHorzOffset <> Value then
  begin
    FHorzOffset := Value;
    if ComponentState * [csLoading, csReading] = [] then
      ArrangeButtons;
  end;
end;

procedure TElCheckItemGroup.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    if ComponentState * [csLoading, csReading] = [] then
      ArrangeButtons;
  end;
end;

procedure TElCheckItemGroup.SetItemSpacing(Value: Integer);
begin
  if FItemSpacing <> Value then
  begin
    FItemSpacing := Value;
    if ComponentState * [csLoading, csReading] = [] then
      ArrangeButtons;
  end;
end;

procedure TElCheckItemGroup.SetVertOffset(Value: Integer);
begin
  if FVertOffset <> Value then
  begin
    FVertOffset := Value;
    if ComponentState * [csLoading, csReading] = [] then
      ArrangeButtons;
  end;
end;

procedure TElCheckItemGroup.Loaded;
begin
  inherited;
  ArrangeButtons;
end;

type
  TElGroupButton = class(TElRadioButton)
  private
//    FInClick: Boolean;
    FOwner: TComponent;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetChecked(newValue : Boolean); override;
  public
    constructor InternalCreate(ElRadioGroup: TCustomElRadioGroup);
    destructor Destroy; override;
    procedure Click; override;
  end;


constructor TElGroupButton.InternalCreate(ElRadioGroup: TCustomElRadioGroup);
begin
  inherited Create(ElRadioGroup);
  ElRadioGroup.FButtons.Add(Self);
  FOwner:= ElRadioGroup;
  Visible := False;
  Enabled := ElRadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := ElRadioGroup.ButtonClick;
  Parent := ElRadioGroup;
end;

destructor TElGroupButton.Destroy;
begin
  TElRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TElGroupButton.SetChecked(newValue: Boolean);
begin
  inherited;
end;

procedure TElGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TElRadioGroup(Parent).KeyPress(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TElRadioGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TElGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TElRadioGroup(Parent).KeyDown(Key, Shift);
end;

procedure TElGroupButton.Click;
begin
  if TElRadioGroup(Parent).CanModify then
    inherited;
end;

constructor TCustomElRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemIndex := -1;
end;

procedure TCustomElRadioGroup.ButtonClick(Sender: TObject);
var i : integer;
begin
  i := FButtons.IndexOf(Sender);
  if not FUpdating then
  begin
    if FItemIndex <> i then
    begin
      FItemIndex := i;
      inherited;
    end;
  end
  else
    inherited;
end;

procedure TCustomElRadioGroup.SetItemIndex(Value: Integer);
begin
  if ComponentState * [csReading, csLoading, csDestroying] <> [] then
    FItemIndex := Value
  else
  begin
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        TElGroupButton(FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
      begin
        TElGroupButton(FButtons[FItemIndex]).Checked := True;
        if Focused and TElGroupButton(FButtons[FItemIndex]).CanFocus then
           TElGroupButton(FButtons[FItemIndex]).SetFocus;
      end;
    end;
  end;
end;

procedure TCustomElRadioGroup.UpdateButtons;
var
  Btn : TElGroupButton;
begin
  inherited;

  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    Btn := TElGroupButton(FButtons[FItemIndex]);
    Btn.Checked := True;
    FUpdating := False;
  end;
  if ComponentState * [csLoading, csReading] = [] then
    ArrangeButtons;
  if HandleAllocated then
    Invalidate;
end;

procedure TCustomElRadioGroup.IntCreateItem;
begin
  TElGroupButton.InternalCreate(Self);
end;

procedure TCustomElRadioGroup.ItemsChange(Sender: TObject);
begin
  if ItemIndex >= Items.Count then
    ItemIndex := -1;
  inherited;
end;

type
  TElCheckGroupBox = class(TElCheckBox)
  private
    FOwner: TComponent;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetChecked(newValue : Boolean); override;
  public
    constructor InternalCreate(ElCheckGroup: TCustomElCheckGroup);
    destructor Destroy; override;
  end;

constructor TElCheckGroupBox.InternalCreate(ElCheckGroup : TCustomElCheckGroup);
begin
  inherited Create(ElCheckGroup);
  ElCheckGroup.FButtons.Add(Self);
  FOwner:= ElCheckGroup;
  Visible := False;
  Enabled := ElCheckGroup.Enabled;
  ParentShowHint := False;
  OnClick := ElCheckGroup.ButtonClick;
  Parent := ElCheckGroup;
end;

destructor TElCheckGroupBox.Destroy;
begin
  TElCheckGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TElCheckGroupBox.SetChecked(newValue: Boolean);
begin
  if TElCheckGroup(Parent).CanModify then inherited;
end;

procedure TElCheckGroupBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TElCheckGroup(Parent).KeyPress(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TElCheckGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TElCheckGroupBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TElCheckGroup(Parent).KeyDown(Key, Shift);
end;


constructor TCustomElCheckGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowGrayed := true;
end;

function TCustomElCheckGroup.GetChecked(Index: Integer): Boolean;
begin
  Result := TElCheckGroupBox(FButtons[Index]).Checked;
end;

procedure TCustomElCheckGroup.SetChecked(Index: Integer; Value: Boolean);
begin
  TElCheckGroupBox(FButtons[Index]).Checked := Value;
end;

function TCustomElCheckGroup.GetState(Index: Integer): TCheckBoxState;
begin
  Result := TElCheckGroupBox(FButtons[Index]).State;
end;

procedure TCustomElCheckGroup.SetState(Index: Integer; Value: TCheckBoxState);
begin
  TElCheckGroupBox(FButtons[Index]).State := Value;
end;

procedure TCustomElCheckGroup.SetAllowGrayed(Value: Boolean);
var i : integer;
begin
  if FAllowGrayed <> Value then
  begin
    FAllowGrayed := Value;
    for i := 0 to FButtons.Count - 1 do
      TElCheckGroupBox(FButtons[i]).AllowGrayed := Value;
  end;
end;

procedure TCustomElCheckGroup.IntCreateItem;
begin
  TElCheckGroupBox.InternalCreate(Self);
end;

end.

