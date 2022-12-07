{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
unit ElDBWideLookupControls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ElListBox, ElCombos, db, dbctrls, elstrutils;

type
  TElWideDBLookupListBox = class;
  TElWideDBLookupComboBox = class;

  { TElWideDBLookupListControl }
  TElWideDBLookupListControl = class(TDBLookupControl)
  private
    { Private declarations }
    FElDBWideLookupListBox: TElWideDBLookupListBox;
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

  { TElWideDBLookUpComboControl }
  TElWideDBLookUpComboControl = class(TDBLookupControl)
  private
    { Private declarations }
    FElDBWideLookupComboBox: TElWideDBLookupComboBox;
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

  { TElWideDBLookupListBox }
  TElWideDBLookupListBox = class(TElListBox)
  private
    { Private declarations }
    FOnChange : TNotifyEvent;
    FElDBWideLookupControl: TElWideDBLookupListControl;
    FReadOnly: Boolean;

    FFieldCount: integer;
    FFields: array of TStrings;
    FFieldWidth: array of integer;
    FIndex: integer;

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
    function GetSelectedString: WideString;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;    
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    function GetFields(Index: integer): TStrings;
  protected
    { Protected declarations }
    procedure SetHorizontalExtent; override;
    procedure DrawItem(Index: Integer; R: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;

    procedure DoDrawText(ACanvas: TCanvas; const ACaption: TElFString;{ index: integer;}
      var Rect: TRect; Flags: LongInt); override;

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
    property SelectedItem: WideString read GetSelectedString;
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

  { TElWideDBLookupComboBox }
  TElWideDBLookupComboBox = class(TElComboBox)
  private
    { Private declarations }
    FSaveListWindowProc: TWndMethod;
    FElDBWideLookupControl: TElWideDBLookUpComboControl;

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
    procedure SetSelected(index: integer; Value: Boolean);
    function GetSelected(index: integer): Boolean;
    function GetField: TField;
    function GetListFieldIndex: integer;
    function GetDataSource: TDataSource;
    function GetListSource: TDataSource;
    function GetListFieldName: string;
    function GetDataFieldName: string;
    function GetKeyFieldName: string;
    function GetKeyValue: Variant;
    function GetSelectedString: WideString;
    procedure ListWindowProc(var Message: TMessage);
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    function GetFields(Index: integer): TStrings;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    { Protected declarations }

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
    procedure DoDrawText(ACanvas: TCanvas; Index: integer; var Rect: TRect; Flags:
        LongInt);

    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property SelectedItem: WideString read GetSelectedString;
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
  end;

implementation

{ TElWideDBLookupListControl }

procedure TElWideDBLookupListControl.KeyValueChanged;
begin
  inherited KeyValueChanged;

  if ListLink.Active then
  begin
    if Field = nil then
    begin
      ListLink.DataSet.Locate(KeyField, KeyValue, [loCaseInsensitive]);
      FElDBWideLookupListBox.ItemIndex := ListLink.ActiveRecord;
      if Assigned(FElDBWideLookupListBox.OnChange) then
        FElDBWideLookupListBox.OnChange(FElDBWideLookupListBox);
    end
    else
       SelectCurrent;
  end;
end;

procedure TElWideDBLookupListControl.UpdateListFields;
var
  FListField: TField;
  i, j, RecCount, ActiveRec, FieldCount: integer;
  W : WideString;
begin
  inherited UpdateListFields;
  FElDBWideLookupListBox.ClearFields;
  if ListLink.Active and( (KeyField<>'') or
    ( (Field<>nil) and (Field.FieldKind=fkLookup) )) then
  begin
    ListLink.BufferCount := ListSource.DataSet.RecordCount;
//    FListField := ListFields.Items[ListFieldIndex];
    ListLink.DataSet.DisableControls;
    i:=0;
    ActiveRec := ListLink.ActiveRecord;
    RecCount := ListLink.RecordCount;
    FieldCount := ListFields.Count;
    FElDBWideLookupListBox.SetFieldCount(FieldCount);
    repeat
      ListLink.ActiveRecord := i;
      for j := 0 to FieldCount-1 do
      begin
        FListField := TField(ListFields.Items[j]);
        if (FListField.DataType = ftWideString) and (not FListField.IsNULL) then
        begin
          W := FListField.Value;
          FElDBWideLookupListBox.AddItem(W, j);
        end
        else
          FElDBWideLookupListBox.AddItem(FListField.DisplayText, j);
      end;
      inc(i);
    until i>=RecCount;

    for j := 0 to FieldCount-1 do
      FElDBWideLookupListBox.FFieldWidth[j] := TField(ListFields.Items[j]).DisplayWidth;
    FElDBWideLookupListBox.SetHorizontalExtent;  

    ListLink.ActiveRecord := ActiveRec;
    ListLink.DataSet.EnableControls;
{
    if idx <> ActiveRec then
    begin
      FElDBWideLookupListBox.ItemIndex := ActiveRec;
      if Assigned(FElDBWideLookupListBox.OnChange) then
        FElDBWideLookupListBox.OnChange(FElDBWideLookupListBox);
    end;
}
  end;
end;

procedure TElWideDBLookupListControl.SelectCurrent;
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
    idx := FElDBWideLookupListBox.ItemIndex;
    ListLink.DataSet.DisableControls;
    repeat
      ListLink.ActiveRecord := i;
      try
        FElDBWideLookupListBox.Selected[i] := FKeyField.Value=Field.Value;
      finally
        inc(i);
      end;
    until i>=RecCount;
    ListLink.ActiveRecord := CurRec;
    ListSource.DataSet.EnableControls;
    if idx <> CurRec then
    begin
      FElDBWideLookupListBox.ItemIndex := CurRec;
      if Assigned(FElDBWideLookupListBox.OnChange) then
        FElDBWideLookupListBox.OnChange(FElDBWideLookupListBox);
    end;
  end;
end;

procedure TElWideDBLookupListControl.Select(Value: Integer);
var
  DataSet: TDataSet;
begin
  if ListLink.Active then
  begin
    DataSet:=ListSource.DataSet;
    DataSet.MoveBy(Value-DataSet.RecNo+1);
    if (DataSource<>nil)and(DataSource.DataSet.Active) then
      SelectKeyValue(DataSet.FieldValues[DataField]);
  end;
end;

{
procedure TElWideDBLookupListControl.ListLinkDataChanged;
begin
end;
}

function TElWideDBLookupListControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataSource<>nil)
    and (DataSource.ExecuteAction(Action));
end;

function TElWideDBLookupListControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataSource<>nil)
    and (DataSource.UpdateAction(Action));
end;

{ TElWideDBLookupComboControl }

procedure TElWideDBLookupComboControl.KeyValueChanged;
begin
  inherited KeyValueChanged;

  if ListLink.Active then
  begin
    if Field = nil then
    begin
      ListLink.DataSet.Locate(KeyField, KeyValue, [loCaseInsensitive]);
      FElDBWideLookupComboBox.ItemIndex := ListLink.ActiveRecord;
      if Assigned(FElDBWideLookupComboBox.OnChange) then
        FElDBWideLookupComboBox.OnChange(FElDBWideLookupComboBox);
    end
    else
       SelectCurrent;
  end;
end;

procedure TElWideDBLookupComboControl.UpdateListFields;
var
  FListField: TField;
  i, j, RecCount, ActiveRec, FieldCount: integer;
  W : WideString;
begin
  inherited UpdateListFields;
  FElDBWideLookupComboBox.ClearFields;
  if ListLink.Active and( (KeyField<>'') or
    ( (Field<>nil) and (Field.FieldKind=fkLookup) )) then
  begin
    ListLink.BufferCount := ListSource.DataSet.RecordCount;
    ListLink.DataSet.DisableControls;
    ActiveRec := ListLink.ActiveRecord;
    i:=0;
    RecCount := ListLink.RecordCount;
    FieldCount := ListFields.Count;
    FElDBWideLookupComboBox.SetFieldCount( FieldCount );
    repeat
      ListLink.ActiveRecord := i;
      for j := 0 to FieldCount-1 do
      begin
        FListField := TField(ListFields.Items[j]);
        if (FListField.DataType = ftWideString) and (not FListField.IsNULL) then
        begin
          W := FListField.Value;
          FElDBWideLookupComboBox.AddItem(w, j);
        end
        else
          FElDBWideLookupComboBox.AddItem(FListField.DisplayText, j);
      end;
      inc(i);
    until i>=RecCount;
    for j := 0 to FieldCount-1 do
      FElDBWideLookupComboBox.FFieldWidth[j] := TField(ListFields.Items[j]).DisplayWidth;
      
    ListLink.ActiveRecord := ActiveRec;
    ListLink.DataSet.EnableControls;
    if FElDBWideLookupComboBox.ItemIndex <> ActiveRec then
    begin
      FElDBWideLookupComboBox.ItemIndex := ActiveRec;
      if Assigned(FElDBWideLookupComboBox.OnChange) then
        FElDBWideLookupComboBox.OnChange(FElDBWideLookupComboBox);
    end;
  end;
end;

procedure TElWideDBLookupComboControl.SelectCurrent;
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
        FElDBWideLookupComboBox.Selected[i] := FKeyField.Value=Field.Value;
      finally
        inc(i);
      end;
    until i>=RecCount;
    ListLink.ActiveRecord := CurRec;
    ListSource.DataSet.EnableControls;
    if FElDBWideLookupComboBox.ItemIndex <> CurRec then
    begin
      FElDBWideLookupComboBox.ItemIndex := CurRec;
      if Assigned(FElDBWideLookupComboBox.OnChange) then
        FElDBWideLookupComboBox.OnChange(FElDBWideLookupComboBox);
    end;
  end;
end;

procedure TElWideDBLookupComboControl.Select(Value: Integer);
var
  DataSet: TDataSet;
begin
  if ListLink.Active then
  begin
    DataSet := ListSource.DataSet;
    DataSet.MoveBy(Value-DataSet.RecNo+1);
    if (DataSource<>nil)and(DataSource.DataSet.Active) then
      SelectKeyValue(DataSet.FieldValues[DataField])
    else
      SelectKeyValue(DataSet.FieldValues[KeyField]);
  end;
end;

{
procedure TElWideDBLookupComboControl.ListLinkDataChanged;
begin
end;
}

function TElWideDBLookupComboControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataSource<>nil)
    and (DataSource.ExecuteAction(Action));
end;

function TElWideDBLookupComboControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataSource<>nil)
    and (DataSource.UpdateAction(Action));
end;

{ TElWideDBLookupListBox }

constructor TElWideDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MultiSelect := true;
  FElDBWideLookupControl := TElWideDBLookupListControl.Create(Self);
  FElDBWideLookupControl.FElDBWideLookupListBox := Self;
  FReadOnly := false;
  FFieldCount := 0;
end;

destructor TElWideDBLookupListBox.Destroy;
var
  i: integer;
begin
  FElDBWideLookupControl.FElDBWideLookupListBox := nil;
  FElDBWideLookupControl.free;
  FElDBWideLookupControl := nil;

  SetLength(FFieldWidth, 0);
  for i := 1 to FFieldCount do
    FFields[i-1].Free;
  SetLength(FFields, 0);

  inherited Destroy;
end;

procedure TElWideDBLookupListBox.SetDataSource(Value: TDataSource);
begin
  FElDBWideLookupControl.DataSource := Value;
end;

procedure TElWideDBLookupListBox.SetListSource(Value: TDataSource);
begin
  FElDBWideLookupControl.ListSource := Value;
end;

procedure TElWideDBLookupListBox.SetDataFieldName(const Value: string);
begin
  FElDBWideLookupControl.DataField := Value;
end;

procedure TElWideDBLookupListBox.SetListFieldName(const Value: string);
begin
  FElDBWideLookupControl.ListField := Value;
end;

procedure TElWideDBLookupListBox.SetKeyFieldName(const Value: string);
begin
  FElDBWideLookupControl.KeyField := Value;
end;

function TElWideDBLookupListBox.GetDataSource: TDataSource;
begin
  Result := FElDBWideLookupControl.DataSource;
end;

function TElWideDBLookupListBox.GetListSource: TDataSource;
begin
  Result := FElDBWideLookupControl.ListSource;
end;

function TElWideDBLookupListBox.GetListFieldName: string;
begin
  Result := FElDBWideLookupControl.ListField;
end;

function TElWideDBLookupListBox.GetDataFieldName: string;
begin
  Result := FElDBWideLookupControl.DataField;
end;

function TElWideDBLookupListBox.GetKeyFieldName: string;
begin
  Result := FElDBWideLookupControl.KeyField;
end;

procedure TElWideDBLookupListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  FElDBWideLookupControl.Select(ItemAtPos(Point(X, Y),true));
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TElWideDBLookupListBox.WMLButtonDown(var Message: TWMLButtonDown);
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
end;

procedure TElWideDBLookupListBox.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FReadOnly then inherited;
end;

procedure TElWideDBLookupListBox.SetKeyValue(const Value: Variant);
begin
  FElDBWideLookupControl.KeyValue := Value;
end;

function TElWideDBLookupListBox.GetKeyValue: Variant;
begin
  Result := FElDBWideLookupControl.KeyValue;
end;

function TElWideDBLookupListBox.GetSelectedString: WideString;
var
  i: integer;
begin
  Result := '';
  for i:=0 to Items.Count-1 do
    if Selected[i] then Result := Result+Items.Strings[i]+';';
  if Result <> '' then SetLength(Result, length(Result)-1);
end;

procedure TElWideDBLookupListBox.SetListFieldIndex(Value: integer);
begin
  FElDBWideLookupControl.ListFieldIndex := Value;
end;

function TElWideDBLookupListBox.GetListFieldIndex: integer;
begin
  Result := FElDBWideLookupControl.ListFieldIndex;
end;

function TElWideDBLookupListBox.GetField: TField;
begin
  Result := FElDBWideLookupControl.Field;
end;

function TElWideDBLookupListBox.ExecuteAction(Action: TBasicAction): boolean;
begin
  Result := inherited ExecuteAction(Action) and FElDBWideLookupControl.ExecuteAction(Action);
end;

function TElWideDBLookupListBox.UpdateAction(Action: TBasicAction): boolean;
begin
  Result := inherited UpdateAction(Action) and FElDBWideLookupControl.UpdateAction(Action);
end;

procedure TElWideDBLookupListBox.WMChar(var Message: TWMChar);
var
  index: integer;
begin
  index := ItemIndex;
  inherited;// WMChar(Message);
  if index<>ItemIndex then FElDBWideLookupControl.Select(ItemIndex);
end;

procedure TElWideDBLookupListBox.DoDrawText(ACanvas: TCanvas; const ACaption: TElFString;
  {index: integer;} var Rect: TRect; Flags: LongInt);
var
  i, TextWidth, TextHeight: integer;
  R: TRect;
  S: WideString;
begin
  if FFieldCount > 1 then
  begin
    TextWidth := ACanvas.TextWidth( '0' );
    TextHeight:= Canvas.TextHeight( '0' );
    R := Rect;
    for i := 1 to FFieldCount do
    begin
      S := FFields[i-1].Strings[findex];
      inherited DoDrawText( ACanvas, S, R, Flags );
      R.Left := R.Left + FFieldWidth[i-1]*TextWidth;

      if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
        ACanvas.Pen.Color := clBtnFace
      else
        ACanvas.Pen.Color := clBtnShadow;

      if i<FFieldCount then
      begin
        ACanvas.MoveTo(R.Left-3, R.Top);
        ACanvas.LineTo(R.Left-3, R.Top + TextHeight+3);
      end;
    end;
  end
  else
    inherited DoDrawText( ACanvas, ACaption, Rect, Flags );
end;

procedure TElWideDBLookupListBox.DrawItem(Index: Integer; R: TRect; State:
{$ifndef VCL_5_USED}
    StdCtrls.TOwnerDrawState);
{$else}
    Windows.TOwnerDrawState);
{$endif}
begin
  findex := index;
  inherited DrawItem(Index, R, State);
end;

procedure TElWideDBLookupListBox.SetHorizontalExtent;
var
  ts: string;
   j: integer;
begin
  ts := '';
  for j := 0 to FFieldCount-1 do
    ts := ts + StringOfChar('0', FFieldWidth[j]);
  FMaxWidth := Canvas.TextWidth(ts);
  inherited;
end;

function TElWideDBLookupListBox.GetFields(Index: integer): TStrings;
begin
  Result := FFields[Index];
end;

procedure TElWideDBLookupListBox.SetFieldCount(Value: integer);
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

procedure TElWideDBLookupListBox.ClearFields;
var
  i: integer;
begin
  Items.Clear;
  for i:= 1 to FFieldCount do
    FFields[i-1].Clear;
end;

procedure TElWideDBLookupListBox.AddItem(const Value: string; Field: integer);
begin
  Fields[Field].Add(Value);
  if Field=0 then
    Items.Add(Value);
end;

{ TElWideDBLookupComboBox }

constructor TElWideDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FElDBWideLookupControl := TElWideDBLookUpComboControl.Create(Self);
  FElDBWideLookupControl.FElDBWideLookupComboBox := Self;
  FListBox.MultiSelect:= True;
  FSaveListWindowProc := FListBox.WindowProc;
  FListBox.WindowProc := ListWindowProc;
  FListBox.OnDrawText := DoDrawText;;
  FFieldCount := 0;
end;

destructor TElWideDBLookupComboBox.Destroy;
var
  i: integer;
begin
  FElDBWideLookupControl.FElDBWideLookupComboBox := nil;
  FElDBWideLookupControl.free;
  FElDBWideLookupControl := nil;
  SetLength(FFieldWidth, 0);
  for i := 1 to FFieldCount do
    FFields[i-1].Free;
  SetLength(FFields, 0);
  inherited Destroy;
end;

procedure TElWideDBLookupComboBox.SetDataSource(Value: TDataSource);
begin
  FElDBWideLookupControl.DataSource := Value;
end;

procedure TElWideDBLookupComboBox.SetListSource(Value: TDataSource);
begin
  FElDBWideLookupControl.ListSource := Value;
end;

procedure TElWideDBLookupComboBox.SetDataFieldName(const Value: string);
begin
  FElDBWideLookupControl.DataField := Value;
end;

procedure TElWideDBLookupComboBox.SetListFieldName(const Value: string);
begin
  FElDBWideLookupControl.ListField := Value;
end;

procedure TElWideDBLookupComboBox.SetKeyFieldName(const Value: string);
begin
  FElDBWideLookupControl.KeyField := Value;
end;

function TElWideDBLookupComboBox.GetDataSource: TDataSource;
begin
  Result := FElDBWideLookupControl.DataSource;
end;

function TElWideDBLookupComboBox.GetListSource: TDataSource;
begin
  Result := FElDBWideLookupControl.ListSource;
end;

function TElWideDBLookupComboBox.GetListFieldName: string;
begin
  Result := FElDBWideLookupControl.ListField;
end;

function TElWideDBLookupComboBox.GetDataFieldName: string;
begin
  Result := FElDBWideLookupControl.DataField;
end;

function TElWideDBLookupComboBox.GetKeyFieldName: string;
begin
  Result := FElDBWideLookupControl.KeyField;
end;

procedure TElWideDBLookupComboBox.SetKeyValue(const Value: Variant);
begin
  FElDBWideLookupControl.KeyValue := Value;
end;

function TElWideDBLookupComboBox.GetKeyValue: Variant;
begin
  Result := FElDBWideLookupControl.KeyValue;
end;

function TElWideDBLookupComboBox.GetSelectedString: WideString;
var
  i: integer;
begin
  Result := '';
  for i:=0 to Items.Count-1 do
    if FListBox.Selected[i] then Result := Result+Items.Strings[i]+';';
  if Result <> '' then SetLength(Result, length(Result)-1);
end;

procedure TElWideDBLookupComboBox.SetListFieldIndex(Value: integer);
begin
  FElDBWideLookupControl.ListFieldIndex := Value;
end;

function TElWideDBLookupComboBox.GetListFieldIndex: integer;
begin
  Result := FElDBWideLookupControl.ListFieldIndex;
end;

function TElWideDBLookupComboBox.GetField: TField;
begin
  Result := FElDBWideLookupControl.Field;
end;

function TElWideDBLookupComboBox.ExecuteAction(Action: TBasicAction): boolean;
begin
  Result := inherited ExecuteAction(Action) and FElDBWideLookupControl.ExecuteAction(Action);
end;

function TElWideDBLookupComboBox.UpdateAction(Action: TBasicAction): boolean;
begin
  Result := inherited UpdateAction(Action) and FElDBWideLookupControl.UpdateAction(Action);
end;

procedure TElWideDBLookupComboBox.WMChar(var Message: TWMChar);
begin
end;

function TElWideDBLookupComboBox.GetSelected(index: integer): Boolean;
begin
  Result := FListBox.Selected[index];
end;

procedure TElWideDBLookupComboBox.SetSelected(index: integer; Value: Boolean);
begin
  FListBox.Selected[index] := Value;
end;

procedure TElWideDBLookupComboBox.ListWindowProc(var Message: TMessage);
var
  Index: integer;
begin
  if (Message.Msg = WM_KEYDOWN)and ReadOnly then exit;
  if (Message.Msg = WM_LBUTTONUP)and ReadOnly then
  begin
    Index := ItemIndex;
    FSaveListWindowProc(Message);
    ItemIndex := Index
  end else
  begin
    FSaveListWindowProc(Message);
    if Message.Msg = WM_LBUTTONUP{DOWN} then
    begin
      FElDBWideLookupControl.Select(ItemIndex);
      if Assigned(OnClick) then OnClick(Self);
    end;
  end;
end;

procedure TElWideDBLookupComboBox.WMKeyDown(var Message: TMessage);
var
  idx: integer;
begin
  idx := ItemIndex;
  inherited;
  if (idx <> ItemIndex) and Assigned(OnChange) then
  begin
    FElDBWideLookupControl.Select(ItemIndex);
    OnChange(self);
  end;
end;
{
procedure TElWideDBLookupComboBox.WMKeyDown(var Message: TMessage);
begin
  inherited;
end;
}
procedure TElWideDBLookupComboBox.WMMouseMove(var Message: TWMMouseMove);
begin
end;

procedure TElWideDBLookupComboBox.WMMouseWheel( var Message: TWMMouseWheel );
var
  idx: integer;
begin
  idx := ItemIndex;
  inherited;
  if idx <> ItemIndex then
  begin
    if ReadOnly then
    begin
      ItemIndex := idx;
      exit;
    end;
    FElDBWideLookupControl.Select( ItemIndex );
    if Assigned( OnChange ) then OnChange( Self );
  end;
end;

procedure TElWideDBLookupComboBox.DoDrawText(ACanvas: TCanvas; Index: integer;
    var Rect: TRect; Flags: LongInt);
var
  i, TextWidth, TextHeight: integer;
  S: WideString;
begin
  TextWidth := ACanvas.TextWidth( '0' );
  TextHeight:= Canvas.TextHeight( '0' );
  for i := 1 to FFieldCount do
  begin
    S := FFields[i-1].Strings[index];

    {$ifndef CLX_USED}
    SetBkMode(ACanvas.Handle, Windows.TRANSPARENT);
    DrawTextW(ACanvas.Handle, PWideChar(S), Length(S), Rect, Flags);
    {$else}
    ACanvas.TextRect(Rect, Rect.Left, Rect.Top, S, Flags);
    {$endif}

    Rect.Left := Rect.Left + FFieldWidth[i-1]*TextWidth;
       if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
      ACanvas.Pen.Color := clBtnFace
    else
      ACanvas.Pen.Color := clBtnShadow;

    if i<FFieldCount then
    begin
      ACanvas.MoveTo(Rect.Left-3, Rect.Top);
      ACanvas.LineTo(Rect.Left-3, Rect.Top + TextHeight+3);
    end;
  end;
end;

function TElWideDBLookupComboBox.GetFields(Index: integer): TStrings;
begin
  Result := FFields[Index];
end;


procedure TElWideDBLookupComboBox.SetFieldCount(Value: integer);
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


procedure TElWideDBLookupComboBox.ClearFields;
var
  i: integer;
begin
  Items.Clear;
  for i:= 1 to FFieldCount do
    FFields[i-1].Clear;
end;


procedure TElWideDBLookupComboBox.AddItem(const Value: string; Field: integer);
begin
  Fields[Field].Add(Value);
  if Field=0 then
    Items.Add(Value);
end;

end.
