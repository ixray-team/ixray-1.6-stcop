{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
unit ElDBLookupCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ElACtrls, db, dbctrls;

type
  TElDBLookupListControl = class;
  TElDBLookupListBox = class;
  TElDBLookupComboBox = class;

  TElDBLookupListControl = class(TDBLookupControl)
  private
    { Private declarations }
    FElDBLookupListBox: TElDBLookupListBox;
  protected
    { Protected declarations }
    procedure KeyValueChanged; override;
    procedure UpdateListFields; override;
    procedure SelectCurrent;
    procedure Select(Value: Integer);
  public
    { Public declarations }
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    { Published declarations }
  end;

  TElDBLookUpComboControl = class(TDBLookupControl)
  private
    { Private declarations }
    FElDBLookupComboBox: TElDBLookupComboBox;
  protected
    { Protected declarations }
    procedure KeyValueChanged; override;
    procedure UpdateListFields; override;
    procedure SelectCurrent;
    procedure Select(Value: Integer);
    
//    procedure ListLinkDataChanged;
  public
    { Public declarations }
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    { Published declarations }
  end;

  TElDBLookupListBox = class(TElAdvancedListBox)
  private
    { Private declarations }
    FElDBLookupControl: TElDBLookupListControl;
    FOnChange : TNotifyEvent;
    FReadOnly: Boolean;
    FFieldCount: integer;
    FFields: array of TStrings;
    FFieldWidth: array of integer;

    procedure SetDataSource(Value: TDataSource);
    procedure SetListSource(Value: TDataSource);
    procedure SetDataFieldName(const Value: string);
    procedure SetListFieldName(const Value: string);
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListFieldIndex(Value: integer);
    function GetField: TField;
    function GetListFieldIndex: integer;
    function GetDataSource: TDataSource;
    function GetListSource: TDataSource;
    function GetListFieldName: string;
    function GetDataFieldName: string;
    function GetKeyFieldName: string;
    function GetKeyValue: Variant;
    function GetSelectedString: string;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    function GetFields(Index: integer): TStrings;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure SetFieldCount(Value: integer);
    procedure ClearFields;
    procedure AddItem(const Value: string; Field: integer);
    property Fields[Index: Integer]: TStrings read GetFields{ write SetField};
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property SelectedItem: string read GetSelectedString;
    property ListFieldIndex: integer read GetListFieldIndex write SetListFieldIndex;
    property Field: TField read GetField;
  published
    { Published declarations }
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property DataField: string read GetDataFieldName write SetDataFieldName;
    property ListField: string read GetListFieldName write SetListFieldName;
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
  end;

  TElDBLookupComboBox = class(TElAdvancedComboBox)
  private
    { Private declarations }
    FElDBLookupControl: TElDBLookupComboControl;
    FSelected: array of Boolean;
    FMaxItems: integer;
    FFieldCount: integer;
    FFields: array of TStrings;
    FFieldWidth: array of integer;
    FReadOnly: boolean;
    procedure SetDataSource(Value: TDataSource);
    procedure SetListSource(Value: TDataSource);
    procedure SetDataFieldName(const Value: string);
    procedure SetListFieldName(const Value: string);
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListFieldIndex(Value: integer);
    procedure SetSelected(index: integer; Value: Boolean);
    function GetSelected(index: integer): boolean;
    function GetField: TField;
    function GetListFieldIndex: integer;
    function GetDataSource: TDataSource;
    function GetListSource: TDataSource;
    function GetListFieldName: string;
    function GetDataFieldName: string;
    function GetKeyFieldName: string;
    function GetKeyValue: Variant;
    function GetSelectedString: string;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    function GetFields(Index: integer): TStrings;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure SetHScrollBarWidth;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
  protected
    { Protected declarations }
    procedure EditWndProc(var Message : TMessage); override;
    procedure ListWndProc(var Message : TMessage); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;

    procedure SetFieldCount(Value: integer);
    procedure ClearFields;
    procedure AddItem(const Value: string; Field: integer);
    property Fields[Index: Integer]: TStrings read GetFields{ write SetField};
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Change; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property SelectedItem: string read GetSelectedString;
    property ListFieldIndex: integer read GetListFieldIndex write SetListFieldIndex;
    property Field: TField read GetField;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
  published
    { Published declarations }
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property DataField: string read GetDataFieldName write SetDataFieldName;
    property ListField: string read GetListFieldName write SetListFieldName;
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
  end;

implementation

{ TElDBLookupListControl }

procedure TElDBLookupListControl.KeyValueChanged;
begin
  inherited KeyValueChanged;

  if ListLink.Active then
  begin
    if Field = nil then
    begin
      ListLink.DataSet.Locate(KeyField, KeyValue, [loCaseInsensitive]);
      FElDBLookupListBox.ItemIndex := ListLink.ActiveRecord;
      if Assigned(FElDBLookupListBox.OnChange) then
        FElDBLookupListBox.OnChange(FElDBLookupListBox);
    end
    else
       SelectCurrent;
  end;
end;

procedure TElDBLookupListControl.UpdateListFields;
var
  i, j, RecCount, ActiveRec, FieldCount: integer;
begin
  inherited UpdateListFields;
  FElDBLookupListBox.ClearFields;
  if ListLink.Active and( (KeyField<>'') or
    ( (Field<>nil) and (Field.FieldKind=fkLookup) )) then
  begin
    ListLink.BufferCount := ListSource.DataSet.RecordCount;
    ListLink.DataSet.DisableControls;
    i:=0;
    ActiveRec := ListLink.ActiveRecord;
    RecCount := ListLink.RecordCount;
    FieldCount := ListFields.Count;
    FElDBLookupListBox.SetFieldCount(FieldCount);
    repeat
      ListLink.ActiveRecord := i;
      for j := 0 to FieldCount-1 do
        FElDBLookupListBox.AddItem(TField(ListFields.Items[j]).DisplayText, j);
      inc(i);
    until i>=RecCount;

    for j := 0 to FieldCount-1 do
      FElDBLookupListBox.FFieldWidth[j] := TField(ListFields.Items[j]).DisplayWidth;

    ListLink.ActiveRecord := ActiveRec;
    ListLink.DataSet.EnableControls;
{
    if FElDBLookupListBox.ItemIndex <> ActiveRec then
    begin
      FElDBLookupListBox.ItemIndex := ActiveRec;
      if Assigned(FElDBLookupListBox.OnChange) then
        FElDBLookupListBox.OnChange(FElDBLookupListBox);
    end;
}
  end;
end;

procedure TElDBLookupListControl.SelectCurrent;
var
  i, RecCount, CurRec: integer;
  FKeyField: TField;
  idx: integer;
begin
  if ListLink.Active and (Field<>nil) then
  begin
    i := 0;
    CurRec := ListLink.ActiveRecord;
    RecCount := ListLink.RecordCount;
    FKeyField := GetFieldProperty(ListLink.DataSet, Self, KeyField);
    idx := FElDBLookupListBox.ItemIndex;
    ListLink.DataSet.DisableControls;
    repeat
      ListLink.ActiveRecord := i;
      try
        FElDBLookupListBox.Selected[i] := FKeyField.Value=Field.Value;
      finally
        inc(i);
      end;
    until i>=RecCount;
    ListLink.ActiveRecord := CurRec;
    ListSource.DataSet.EnableControls;
    if idx <> CurRec then
    begin
      FElDBLookupListBox.ItemIndex := CurRec;
      if Assigned(FElDBLookupListBox.OnChange) then
        FElDBLookupListBox.OnChange(FElDBLookupListBox);
    end;
  end;
end;

procedure TElDBLookupListControl.Select(Value: Integer);
var
  DataSet: TDataSet;
begin
  if ListLink.Active then
  begin
    DataSet:=ListSource.DataSet;
    DataSet.MoveBy(Value-DataSet.RecNo+1);
    if (DataSource<>nil)and(DataSource.DataSet.Active) then
      SelectKeyValue(DataSet.FieldValues[DataField])
    else
      SelectKeyValue(DataSet.FieldValues[KeyField]);
  end;
end;

{
procedure TElDBLookupListControl.ListLinkDataChanged;
begin
end;
}

function TElDBLookupListControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataSource<>nil)
    and (DataSource.ExecuteAction(Action));
end;

function TElDBLookupListControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataSource<>nil)
    and (DataSource.UpdateAction(Action));
end;

{ TElDBLookupComboControl }

procedure TElDBLookupComboControl.KeyValueChanged;
begin
  inherited KeyValueChanged;

  if ListLink.Active then
  begin
    if Field = nil then
    begin
      ListLink.DataSet.Locate(KeyField, KeyValue, [loCaseInsensitive]);
      FElDBLookupComboBox.ItemIndex := ListLink.ActiveRecord;
      if Assigned(FElDBLookupComboBox.OnChange) then
        FElDBLookupComboBox.OnChange(FElDBLookupComboBox);
    end
    else
       SelectCurrent;
  end;
end;

procedure TElDBLookupComboControl.UpdateListFields;
var
  i, j, RecCount, ActiveRec, FieldCount: integer;
begin
  inherited UpdateListFields;
  FElDBLookupComboBox.ClearFields;
  if ListLink.Active and( (KeyField<>'') or
    ( (Field<>nil) and (Field.FieldKind=fkLookup) )) then
  begin
    ListLink.BufferCount := ListSource.DataSet.RecordCount;
    ListLink.DataSet.DisableControls;
    ActiveRec := ListLink.ActiveRecord;
    i:=0;
    RecCount := ListLink.RecordCount;
    FieldCount := ListFields.Count;
    FElDBLookupComboBox.SetFieldCount(FieldCount);
    repeat
      ListLink.ActiveRecord := i;
      for j := 0 to FieldCount-1 do
        FElDBLookupComboBox.AddItem(TField(ListFields.Items[j]).DisplayText, j);
      inc(i);
    until i>=RecCount;
    for j := 0 to FieldCount-1 do
      FElDBLookupComboBox.FFieldWidth[j] := TField(ListFields.Items[j]).DisplayWidth;
    ListLink.ActiveRecord := ActiveRec;
    ListLink.DataSet.EnableControls;
    if FElDBLookupComboBox.ItemIndex <> ActiveRec then
    begin
      FElDBLookupComboBox.ItemIndex := ActiveRec;
      if Assigned(FElDBLookupComboBox.OnChange) then
        FElDBLookupComboBox.OnChange(FElDBLookupComboBox);
    end;
  end;
end;

procedure TElDBLookupComboControl.SelectCurrent;
var
  i, RecCount, CurRec: integer;
  FKeyField: TField;
begin
  if ListLink.Active and (Field<>nil) then
  begin
    i := 0;
    CurRec := ListLink.ActiveRecord;
    RecCount := ListLink.RecordCount;
    FKeyField := GetFieldProperty(ListLink.DataSet, Self, KeyField);
    ListLink.DataSet.DisableControls;
    repeat
      ListLink.ActiveRecord := i;
      try
        FElDBLookupComboBox.Selected[i] := FKeyField.Value=Field.Value;
      finally
        inc(i);
      end;
    until i>=RecCount;
    ListLink.ActiveRecord := CurRec;
    ListSource.DataSet.EnableControls;
    if FElDBLookupComboBox.ItemIndex <> CurRec then
    begin
      FElDBLookupComboBox.ItemIndex := CurRec;
      if Assigned(FElDBLookupComboBox.OnChange) then
        FElDBLookupComboBox.OnChange(FElDBLookupComboBox);
    end;
  end;
end;

procedure TElDBLookupComboControl.Select(Value: Integer);
var
  DataSet: TDataSet;
begin
  if ListLink.Active then
  begin
    DataSet:=ListSource.DataSet;
    DataSet.MoveBy(Value-DataSet.RecNo+1);
    if (DataSource<>nil)and(DataSource.DataSet.Active) then
      SelectKeyValue(DataSet.FieldValues[DataField])
    else
      SelectKeyValue(DataSet.FieldValues[KeyField]);
  end;
end;

{
procedure TElDBLookupComboControl.ListLinkDataChanged;
begin
end;
}

function TElDBLookupComboControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataSource<>nil)
    and (DataSource.ExecuteAction(Action));
end;

function TElDBLookupComboControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataSource<>nil)
    and (DataSource.UpdateAction(Action));
end;

{ TElDBLookupListBox }

constructor TElDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MultiSelect := true;
  FElDBLookupControl := TElDBLookupListControl.Create(Self);
  FElDBLookupControl.FElDBLookupListBox := Self;
  FFieldCount := 0;
  FReadOnly := false;
end;

destructor TElDBLookupListBox.Destroy;
var
  i: integer;
begin
  FElDBLookupControl.FElDBLookupListBox := nil;
  FElDBLookupControl.free;
  FElDBLookupControl := nil;

  SetLength(FFieldWidth, 0);
  for i := 1 to FFieldCount do
    FFields[i-1].Free;
  SetLength(FFields, 0);

  inherited Destroy;
end;

procedure TElDBLookupListBox.SetDataSource(Value: TDataSource);
begin
  FElDBLookupControl.DataSource := Value;
end;

procedure TElDBLookupListBox.SetListSource(Value: TDataSource);
begin
  FElDBLookupControl.ListSource := Value;
end;

procedure TElDBLookupListBox.SetDataFieldName(const Value: string);
begin
  FElDBLookupControl.DataField := Value;
end;

procedure TElDBLookupListBox.SetListFieldName(const Value: string);
begin
  FElDBLookupControl.ListField := Value;
end;

procedure TElDBLookupListBox.SetKeyFieldName(const Value: string);
begin
  FElDBLookupControl.KeyField := Value;
end;

function TElDBLookupListBox.GetDataSource: TDataSource;
begin
  Result := FElDBLookupControl.DataSource;
end;

function TElDBLookupListBox.GetListSource: TDataSource;
begin
  Result := FElDBLookupControl.ListSource;
end;

function TElDBLookupListBox.GetListFieldName: string;
begin
  Result := FElDBLookupControl.ListField;
end;

function TElDBLookupListBox.GetDataFieldName: string;
begin
  Result := FElDBLookupControl.DataField;
end;

function TElDBLookupListBox.GetKeyFieldName: string;
begin
  Result := FElDBLookupControl.KeyField;
end;

procedure TElDBLookupListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  FElDBLookupControl.Select(ItemAtPos(Point(X, Y),true));
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TElDBLookupListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  idx: integer;
begin
  if FReadOnly then exit;
  idx := ItemIndex;
  inherited;
  if (idx<>ItemIndex) and Assigned(FOnChange) then
  begin
    Invalidate;
    FOnChange(Self);
  end;
  MouseDown(mbLeft, [], Message.XPos, Message.YPos);
  invalidate;
end;

procedure TElDBLookupListBox.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FReadOnly then inherited;
end;

procedure TElDBLookupListBox.SetKeyValue(const Value: Variant);
begin
  FElDBLookupControl.KeyValue := Value;
end;

function TElDBLookupListBox.GetKeyValue: Variant;
begin
  Result := FElDBLookupControl.KeyValue;
end;

function TElDBLookupListBox.GetSelectedString: string;
var
  i: integer;
begin
  Result := '';
  for i:=0 to Items.Count-1 do
    if Selected[i] then Result := Result+Items.Strings[i]+';';
  if Result <> '' then SetLength(Result, length(Result)-1);
end;

procedure TElDBLookupListBox.SetListFieldIndex(Value: integer);
begin
  FElDBLookupControl.ListFieldIndex := Value;
end;

function TElDBLookupListBox.GetListFieldIndex: integer;
begin
  Result := FElDBLookupControl.ListFieldIndex;
end;

function TElDBLookupListBox.GetField: TField;
begin
  Result := FElDBLookupControl.Field;
end;

function TElDBLookupListBox.ExecuteAction(Action: TBasicAction): boolean;
begin
  Result := inherited ExecuteAction(Action) and FElDBLookupControl.ExecuteAction(Action);
end;

function TElDBLookupListBox.UpdateAction(Action: TBasicAction): boolean;
begin
  Result := inherited UpdateAction(Action) and FElDBLookupControl.UpdateAction(Action);
end;

procedure TElDBLookupListBox.WMChar(var Message: TWMChar);
var
  index: integer;
begin
  index := ItemIndex;
  inherited;// WMChar(Message);
  if index<>ItemIndex then FElDBLookupControl.Select(ItemIndex);
end;

procedure TElDBLookupListBox.WMKeyDown(var Message: TWMKeyDown);
begin
//  exit;
  inherited;
  FElDBLookupControl.Select(ItemIndex);
  if Assigned(FOnChange) then FOnChange(self);
  invalidate;
end;

procedure TElDBLookupListBox.DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
var
  i, TextWidth, TextHeight: integer;
  R: TRect;
  S: String;
begin
  Canvas.Font := Font;

  if Selected[Index] or (odSelected in State) then
  begin
    Canvas.Font.Color  := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end;

  if FFieldCount = 1 then
    inherited;

  if  FReadOnly then
    Canvas.Font.Color := clGrayText;

  if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
    Canvas.Pen.Color := clBtnFace
  else
    Canvas.Pen.Color := clBtnShadow;

  TextWidth := Canvas.TextWidth('0');
  TextHeight := Canvas.TextHeight('0');
  R := Rect;
  for i := 1 to FFieldCount do
  begin
      S := FFields[i-1].Strings[index];
      Canvas.FillRect(R);
      Canvas.TextOut(R.Left + 4, R.Top, S);
      R.Left := R.Left + FFieldWidth[i-1]*TextWidth;
      if i<FFieldCount then
      begin
        Canvas.MoveTo(R.Left-1, R.Top);
        Canvas.LineTo(R.Left-1, R.Top + TextHeight+3);
      end;
  end;
end;

function TElDBLookupListBox.GetFields(Index: integer): TStrings;
begin
  Result := FFields[Index];
end;

procedure TElDBLookupListBox.SetFieldCount(Value: integer);
var
  i: integer;
begin
  if Value>FFieldCount then
  begin
    SetLength(FFields, Value);
    SetLength(FFieldWidth, Value);
    inc(FFieldCount);
    for i := FFieldCount to Value do
      FFields[i-1] := TStringList.Create;
    FFieldCount := Value;
  end
  else
    if Value<FFieldCount then
    begin
      for i := Value+1 to FFieldCount do
        FFields[i-1].Free;
      SetLength(FFields, Value);
      SetLength(FFieldWidth, Value);
      FFieldCount := Value;
    end;
end;

procedure TElDBLookupListBox.ClearFields;
var
  i: integer;
begin
  Items.Clear;
  for i:= 1 to FFieldCount do
    FFields[i-1].Clear;
end;

procedure TElDBLookupListBox.AddItem(const Value: string; Field: integer);
begin
  Fields[Field].Add(Value);
  if Field=0 then
    Items.Add(Value);
end;

procedure TElDBLookupListBox.WMSize(var Message: TWMSize);
var
  ts: string;
   j: integer;
begin
  ts := '';
  for j := 0 to FFieldCount-1 do
    ts := ts + StringOfChar('0', FFieldWidth[j]);
  Perform( LB_SETHORIZONTALEXTENT, Canvas.TextWidth(ts), 0 );
  inherited;
end;

{ TElDBLookupComboBox }

constructor TElDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Style := csOwnerDrawVariable;
  FElDBLookupControl := TElDBLookupComboControl.Create(Self);
  FElDBLookupControl.FElDBLookupComboBox := Self;
  FMaxItems := 0;
  FFieldCount := 0;
  FReadOnly := false;
end;

destructor TElDBLookupComboBox.Destroy;
var
  i: integer;
begin
  FElDBLookupControl.FElDBLookupComboBox := nil;
  FElDBLookupControl.free;
  FElDBLookupControl := nil;
  SetLength(FSelected, 0);
  SetLength(FFieldWidth, 0);
  for i := 1 to FFieldCount do
    FFields[i-1].Free;
  SetLength(FFields, 0);
  inherited Destroy;
end;

procedure TElDBLookupComboBox.Change;
begin
  FElDBLookupControl.Select(ItemIndex);
  if Assigned(OnChange) then
    OnChange(self);
end;

procedure TElDBLookupComboBox.SetDataSource(Value: TDataSource);
begin
  FElDBLookupControl.DataSource := Value;
end;

procedure TElDBLookupComboBox.SetListSource(Value: TDataSource);
begin
  FElDBLookupControl.ListSource := Value;
end;

procedure TElDBLookupComboBox.SetDataFieldName(const Value: string);
begin
  FElDBLookupControl.DataField := Value;
end;

procedure TElDBLookupComboBox.SetListFieldName(const Value: string);
begin
  FElDBLookupControl.ListField := Value;
end;

procedure TElDBLookupComboBox.SetKeyFieldName(const Value: string);
begin
  FElDBLookupControl.KeyField := Value;
end;

function TElDBLookupComboBox.GetDataSource: TDataSource;
begin
  Result := FElDBLookupControl.DataSource;
end;

function TElDBLookupComboBox.GetListSource: TDataSource;
begin
  Result := FElDBLookupControl.ListSource;
end;

function TElDBLookupComboBox.GetListFieldName: string;
begin
  Result := FElDBLookupControl.ListField;
end;

function TElDBLookupComboBox.GetDataFieldName: string;
begin
  Result := FElDBLookupControl.DataField;
end;

function TElDBLookupComboBox.GetKeyFieldName: string;
begin
  Result := FElDBLookupControl.KeyField;
end;

procedure TElDBLookupComboBox.SetKeyValue(const Value: Variant);
begin
  FElDBLookupControl.KeyValue := Value;
end;

function TElDBLookupComboBox.GetKeyValue: Variant;
begin
  Result := FElDBLookupControl.KeyValue;
end;

function TElDBLookupComboBox.GetSelectedString: string;
var
  i: integer;
begin
  Result := '';
  for i:=0 to Items.Count-1 do
    if FSelected[i] then Result := Result+Items.Strings[i]+';';
  if Result <> '' then SetLength(Result, length(Result)-1);
end;

procedure TElDBLookupComboBox.SetListFieldIndex(Value: integer);
begin
  FElDBLookupControl.ListFieldIndex := Value;
end;

function TElDBLookupComboBox.GetListFieldIndex: integer;
begin
  Result := FElDBLookupControl.ListFieldIndex;
end;

function TElDBLookupComboBox.GetField: TField;
begin
  Result := FElDBLookupControl.Field;
end;

function TElDBLookupComboBox.ExecuteAction(Action: TBasicAction): boolean;
begin
  Result := inherited ExecuteAction(Action) and FElDBLookupControl.ExecuteAction(Action);
end;

function TElDBLookupComboBox.UpdateAction(Action: TBasicAction): boolean;
begin
  Result := inherited UpdateAction(Action) and FElDBLookupControl.UpdateAction(Action);
end;

procedure TElDBLookupComboBox.WMChar(var Message: TWMChar);
var
  index: integer;
begin
  if FReadOnly then exit;
  index := ItemIndex;
  inherited;// WMChar(Message);
  if index<>ItemIndex then FElDBLookupControl.Select(ItemIndex);
end;

function TElDBLookupComboBox.GetSelected(index: integer): boolean;
begin
  if (index >= Items.Count) or (index >= FMaxItems) then
    raise ERangeError.CreateFmt('Index out of range!',[])
  else
    Result := FSelected[index];
end;

procedure TElDBLookupComboBox.SetSelected(index: integer; Value: Boolean);
begin
  if (index >= Items.Count) or (index >= FMaxItems) then
    raise ERangeError.CreateFmt('Index out of range!',[])
  else
    FSelected[index] := Value;
end;

procedure TElDBLookupComboBox.EditWndProc(var Message : TMessage);
begin
  case Message.Msg of
    WM_KEYDOWN:          {EditDoKeyDown(Message)};
    WM_KEYUP:            {WMKeyUp(TWMKeyUp(Message))};
    WM_CHAR:             WMChar(TWMChar(Message));
    WM_MOUSEMOVE:        {WMMouseMove(TWMMouseMove(Message))};
  else
    inherited EditWndProc(Message);
  end;
end;

procedure TElDBLookupComboBox.DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
var
  i: integer;
  R: TRect;
  S: String;
  TextWidth, TextHeight: integer;
begin
  Canvas.Font := Font;

  if (FSelected[Index]) or (odSelected in State) then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end;

  if FFieldCount = 1 then inherited;

  if FReadOnly then
    Canvas.Font.Color := clGrayText;

  if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
    Canvas.Pen.Color := clBtnFace
  else
    Canvas.Pen.Color := clBtnShadow;

  TextWidth := Canvas.TextWidth('0');
  TextHeight := Canvas.TextHeight('0');
  R := Rect;
  for i := 1 to FFieldCount do
  begin
      if FFields[i-1].Count>Index then
        S := FFields[i-1].Strings[index]
      else
        S := '';
      Canvas.FillRect(R);
      Canvas.TextOut(R.Left + 4, R.Top, S);
      R.Left := R.Left + FFieldWidth[i-1]*TextWidth;
      if i<FFieldCount then
      begin
        Canvas.MoveTo(R.Left-1, R.Top);
        Canvas.LineTo(R.Left-1, R.Top + TextHeight+3);
      end;
  end;
end;

procedure TElDBLookupComboBox.ListWndProc(var Message : TMessage);
var
  Index: integer;
begin
  if  (Message.Msg = WM_SIZE) then
    SetHScrollBarWidth;
  if (Message.Msg = WM_LBUTTONDOWN)and FReadOnly then
  begin
    Index := ItemIndex;
    inherited ListWndProc(Message);
    ItemIndex := Index
  end
  else
  begin
  {
    if  (Message.Msg = WM_KEYDOWN) then
    begin
//      ListDoKeyDown(Message);
      exit;
    end;
}
    if (Message.Msg = WM_KEYDOWN)and FReadOnly then exit;
    if Message.Msg <> WM_MOUSEMOVE then
      inherited ListWndProc(Message);
    if Message.Msg = WM_LBUTTONDOWN then
      FElDBLookupControl.Select(ItemIndex);
  end;

  if  (Message.Msg = WM_KEYDOWN) then
  begin
    
  end;

end;

procedure TElDBLookupComboBox.AddItem(const Value: string; Field: integer);
var
  i, count: integer;
begin
  Fields[Field].Add(Value);
  if Field=0 then
  begin
    Items.Add(Value);
    if FMaxItems <= Items.Count then
    begin
      count := FMaxItems;
      FMaxItems := Items.Count + 1;
      SetLength(FSelected, FMaxItems);
      for i := count to FMaxItems do
        FSelected[i-1] := false;
    end;
  end;
end;
{
procedure TElDBLookupComboBox.Temp(var Message: TMessage);
begin
  MessageBox(Application.Handle, 'test', 'temp', MB_OK);
end;
}

function TElDBLookupComboBox.GetFields(Index: integer): TStrings;
begin
  Result := FFields[Index];
end;

procedure TElDBLookupComboBox.SetFieldCount(Value: integer);
var
  i: integer;
begin
  if Value>FFieldCount then
  begin
    SetLength(FFields, Value);
    SetLength(FFieldWidth, Value);
    inc(FFieldCount);
    for i := FFieldCount to Value do
      FFields[i-1] := TStringList.Create;
    FFieldCount := Value;
  end
  else
    if Value<FFieldCount then
    begin
      for i := Value+1 to FFieldCount do
        FFields[i-1].Free;
      SetLength(FFields, Value);
      SetLength(FFieldWidth, Value);
      FFieldCount := Value;
    end;
end;

procedure TElDBLookupComboBox.ClearFields;
var
  i: integer;
begin
  Items.Clear;
  for i:= 1 to FFieldCount do
    FFields[i-1].Clear;
end;

procedure TElDBLookupComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
X, Y: Integer);
begin
  if not ReadOnly then
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TElDBLookupComboBox.WMMouseWheel( var Message: TWMMouseWheel );
var
  idx: integer;
begin
  idx := ItemIndex;
  inherited;
  if idx <> ItemIndex then
  begin
    if FReadOnly then
    begin
      ItemIndex := idx;
      exit;
    end;
    FElDBLookupControl.Select( ItemIndex );
    if Assigned( OnChange ) then OnChange( Self );
  end;
end;


procedure TElDBLookupComboBox.SetHScrollBarWidth;
var
  ts: string;
   j: integer;
begin
  ts := '';
  for j := 0 to FFieldCount-1 do
    ts := ts + StringOfChar('0', FFieldWidth[j]);
  Perform( CB_SETHORIZONTALEXTENT, Canvas.TextWidth(ts), 0 );
end;


procedure TElDBLookupComboBox.CMFontChanged(var Msg: TMessage);
var tm : TTextMetric;
begin
  inherited;
  exit;
  Canvas.Font.Assign(Font);
  GetTextMetrics(Canvas.Handle, tm);
  Perform(CB_SETITEMHEIGHT, 0, tm.tmHeight);
end;

end.

