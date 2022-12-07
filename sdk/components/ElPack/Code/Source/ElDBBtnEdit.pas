
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

03/16/2002

  ElDBButtonEdit made unicode

*)


unit ElDBBtnEdit;

interface

uses
     DB,
     DBCtrls,
     
     ElBtnEdit,

     Forms,
     Windows,
     Controls,
     StdCtrls,
     Messages,
{$ifdef VCL_6_USED}
Types,
{$endif}

     Classes,
     SysUtils;

type

  TElDBButtonEdit = class(TCustomElButtonEdit)
  private
    FDataLink: TFieldDataLink;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure ResetMaxLength;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateData(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    {$ifdef VCL_4_USED}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    {$endif}
    property Field: TField read GetField;
    property Text;
    property Lines; 
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;


    property TopMargin;
    property LeftMargin;
    property RightMargin;
    property AutoSize;
    property RTLContent;
    property BorderSides;
    property PasswordChar;
    property MaxLength;
    property Transparent;
    property FlatFocusedScrollBars;
    property WantTabs;
    property HandleDialogKeys;
    property HideSelection;
    property TabSpaces;
    {$ifdef ELPACK_COMPLETE}
    property ImageForm;
    {$endif}
    property WordWrap;
    property ScrollBars;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnChange;
    property OnSelectionChange;

    property Multiline;

    // inherited
    property Flat;
    property ActiveBorderType;
    property InactiveBorderType;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;

    property UseBackground;
    property Alignment;
    property AutoSelect;

    property Background;
    property ButtonCaption;
    property ButtonClickSound;
    property ButtonDownSound;
    property ButtonUpSound;
{$IFDEF USE_SOUND_MAP}
    property ButtonSoundMap;
{$ENDIF}
    property ButtonColor;
    property ButtonDown;
    property ButtonEnabled;
    property ButtonFlat;
    property ButtonGlyph;
    property ButtonHint;
    property ButtonIcon;
    property ButtonNumGlyphs;
    property ButtonPopupPlace;
    property ButtonPullDownMenu;
    property ButtonShortcut;
    property ButtonUseIcon;
    property ButtonVisible;
    property ButtonWidth;
    property OnButtonClick;

    property AltButtonCaption;
    property AltButtonClickSound;
    property AltButtonDownSound;
    property AltButtonUpSound;
{$IFDEF USE_SOUND_MAP}
    property AltButtonSoundMap;
{$ENDIF}
    property AltButtonColor;
    property AltButtonDown;
    property AltButtonEnabled;
    property AltButtonFlat;
    property AltButtonGlyph;
    property AltButtonHint;
    property AltButtonIcon;
    property AltButtonNumGlyphs;
    property AltButtonPopupPlace;
    property AltButtonPosition;
    property AltButtonPullDownMenu;
    property AltButtonShortcut;
    property AltButtonUseIcon;
    property AltButtonVisible;
    property AltButtonWidth;
    property OnAltButtonClick;

    // VCL properties
    property BorderStyle;
    property Ctl3D;
    property ParentCtl3D; 
    property Enabled;
    property TabStop default True;
    property TabOrder;
    property PopupMenu;
    property Color;
    property ParentColor;
    property Align;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property ReadOnly;

    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF VCL_4_USED}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF VCL_4_USED}
    property OnStartDock;
{$ENDIF}

{$IFDEF VCL_4_USED}
    property Anchors;
    property Constraints;
    property DockOrientation;
    property Floating;
    property DoubleBuffered;
    property DragKind;
{$ENDIF}
  end;

implementation

constructor TElDBButtonEdit.Create(AOwner : TComponent);
begin
  inherited;
  inherited ReadOnly := true;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TElDBButtonEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TElDBButtonEdit.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TElDBButtonEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TElDBButtonEdit.DataChange(Sender: TObject);
{$ifdef ELPACK_UNICODE}
var W : WideString;
{$endif}
begin
  if FDataLink.Field <> nil then
  begin
    if Alignment <> FDataLink.Field.Alignment then
    begin
      Text := '';  {forces update}
      Alignment := FDataLink.Field.Alignment;
    end;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType in [ftString{$ifdef VCL_4_USED}, ftWideString{$endif}]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    {$ifdef ELPACK_UNICODE}
    if (FDataLink.Field.DataType = ftWideString) and (not FDataLink.Field.IsNull) then
    begin
      W := FDataLink.Field.Value;
      Text := W;
    end
    else
    {$endif}
      Text := FDataLink.Field.AsString;
  end
  else
  begin
    Alignment := taLeftJustify;
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end;
end;

procedure TElDBButtonEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

{$ifdef VCL_4_USED}
function TElDBButtonEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;
{$endif}

function TElDBButtonEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TElDBButtonEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TElDBButtonEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TElDBButtonEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) or (Key = VK_BACK) then
    FDataLink.Edit;
  inherited KeyDown(Key, Shift);
end;

procedure TElDBButtonEdit.KeyPress(var Key: Char);
begin
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
  inherited KeyPress(Key);
end;

procedure TElDBButtonEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TElDBButtonEdit.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TElDBButtonEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString{$ifdef VCL_4_USED}, ftWideString{$endif}]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

procedure TElDBButtonEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TElDBButtonEdit.SetDataSource(Value: TDataSource);
begin
  {$ifdef VCL_5_USED}
  if FDataLink.DataSource <> nil then
    FDataLink.DataSource.RemoveFreeNotification(Self);
  {$endif}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

{$ifdef VCL_4_USED}
function TElDBButtonEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$endif}

procedure TElDBButtonEdit.UpdateData(Sender: TObject);
{$ifdef ELPACK_UNICODE}
var W : WideString;
{$endif}
begin
  {$ifdef ELPACK_UNICODE}
  if (FDataLink.Field.DataType = ftWideString) and (not FDataLink.Field.IsNull) then
  begin
    W := Text;
    FDataLink.Field.Value := W;
  end
  else
  {$endif}
    FDataLink.Field.Text := Text;
end;

{$ifdef VCL_4_USED}
function TElDBButtonEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;
{$endif}

procedure TElDBButtonEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TElDBButtonEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;


end.
 
