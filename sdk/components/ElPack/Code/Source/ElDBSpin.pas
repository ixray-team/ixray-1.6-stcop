
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

unit ElDBSpin;

interface

uses
     DB,
     DBCtrls,

     ElSpin,

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

  TElDBSpinEdit = class(TElSpinEdit)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function  GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
  protected
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SpinDownClick(Sender : TObject; Increment : Double); override;
    procedure SpinDrag(Sender : TObject; NewValue : Double); override;
    procedure SpinUpClick(Sender : TObject; Increment : Double); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef VCL_4_USED}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    {$endif}
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  TElDBFloatSpinEdit = class(TElFloatSpinEdit)
  private
    FDataLink: TFieldDataLink;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
  protected
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SpinDownClick(Sender : TObject; Increment : Double); override;
    procedure SpinDrag(Sender : TObject; NewValue : Double); override;
    procedure SpinUpClick(Sender : TObject; Increment : Double); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef VCL_4_USED}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    {$endif}
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

function TElDBSpinEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TElDBSpinEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TElDBSpinEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TElDBSpinEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TElDBSpinEdit.SetDataSource(Value: TDataSource);
begin
  {$ifdef VCL_5_USED}
  if FDataLink.DataSource <> nil then
    FDataLink.DataSource.RemoveFreeNotification(Self);
  {$endif}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TElDBSpinEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    ValueUndefined := false;
    Value := FDataLink.Field.AsInteger;
  end
  else
    ValueUndefined := true;
end;

procedure TElDBSpinEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

constructor TElDBSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := true;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
end;

destructor TElDBSpinEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TElDBSpinEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TElDBSpinEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

{$ifdef VCL_4_USED}
function TElDBSpinEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;
{$endif}

function TElDBSpinEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TElDBSpinEdit.KeyPress(var Key: Char);
var aValue : integer;
begin
  aValue := Value;
  if Key = #27 then
    FDataLink.Reset
  else
    FDataLink.Edit;
  inherited KeyPress(Key);
  if AValue <> Value then
    FDataLink.Modified;
end;

procedure TElDBSpinEdit.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TElDBSpinEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{$ifdef VCL_4_USED}
function TElDBSpinEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$endif}

procedure TElDBSpinEdit.UpdateData(Sender: TObject);
begin
  if Self.ValueUndefined then
    FDataLink.Field.Clear
  else
    FDataLink.Field.AsInteger := Self.Value;
end;

{$ifdef VCL_4_USED}
function TElDBSpinEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;
{$endif}

procedure TElDBSpinEdit.SpinDownClick(Sender : TObject; Increment : Double);
var AValue : integer;
begin
  if not ReadOnly then
    FDataLink.Edit;
  AValue := Value;
  inherited;
  if AValue <> Value then
    FDataLink.Modified;
end;

procedure TElDBSpinEdit.SpinDrag(Sender : TObject; NewValue : Double);
var AValue : integer;
begin
  if not ReadOnly then
    FDataLink.Edit;
  AValue := Value;
  inherited;
  if AValue <> Value then
    FDataLink.Modified;
end;

procedure TElDBSpinEdit.SpinUpClick(Sender : TObject; Increment : Double);
var AValue : integer;
begin
  if not ReadOnly then
    FDataLink.Edit;
  AValue := Value;
  inherited;
  if AValue <> Value then
    FDataLink.Modified;
end;

procedure TElDBSpinEdit.KeyDown(var Key : Word; Shift : TShiftState);
var aValue : integer;
begin
  if not ReadOnly then
    FDataLink.Edit;
  aValue := Value;
  inherited;
  if AValue <> Value then
    FDataLink.Modified;
end;
  
constructor TElDBFloatSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := true;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
end;

destructor TElDBFloatSpinEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TElDBFloatSpinEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TElDBFloatSpinEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TElDBFloatSpinEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    ValueUndefined := false;
    Value := FDataLink.Field.AsFloat;
  end
  else
    ValueUndefined := true;
end;

{$ifdef VCL_4_USED}
function TElDBFloatSpinEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;
{$endif}

function TElDBFloatSpinEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TElDBFloatSpinEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TElDBFloatSpinEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TElDBFloatSpinEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TElDBFloatSpinEdit.KeyPress(var Key: Char);
var aValue : Double;
begin
  aValue := Value;
  if Key = #27 then
    FDataLink.Reset
  else
    FDataLink.Edit;
  inherited KeyPress(Key);
  if AValue <> Value then
    FDataLink.Modified;
end;

procedure TElDBFloatSpinEdit.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TElDBFloatSpinEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TElDBFloatSpinEdit.SetDataSource(Value: TDataSource);
begin
  {$ifdef VCL_5_USED}
  if FDataLink.DataSource <> nil then
    FDataLink.DataSource.RemoveFreeNotification(Self);
  {$endif}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TElDBFloatSpinEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{$ifdef VCL_4_USED}
function TElDBFloatSpinEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$endif}

procedure TElDBFloatSpinEdit.UpdateData(Sender: TObject);
begin
  if Self.ValueUndefined then
    FDataLink.Field.Clear
  else
    FDataLink.Field.AsFloat := Value;
end;

{$ifdef VCL_4_USED}
function TElDBFloatSpinEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;
{$endif}

procedure TElDBFloatSpinEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TElDBFloatSpinEdit.SpinDownClick(Sender : TObject; Increment : 
    Double);
var AValue : Double;
begin
  if not ReadOnly then
    FDataLink.Edit;
  AValue := Value;
  inherited;
  if AValue <> Value then
    FDataLink.Modified;
end;

procedure TElDBFloatSpinEdit.SpinDrag(Sender : TObject; NewValue : Double);
var AValue : Double;
begin
  if not ReadOnly then
    FDataLink.Edit;
  AValue := Value;
  inherited;
  if AValue <> Value then
    FDataLink.Modified;
end;

procedure TElDBFloatSpinEdit.SpinUpClick(Sender : TObject; Increment : Double);
var AValue : Double;
begin
  if not ReadOnly then
    FDataLink.Edit;
  AValue := Value;
  inherited;
  if AValue <> Value then
    FDataLink.Modified;
end;

procedure TElDBFloatSpinEdit.KeyDown(var Key : Word; Shift : TShiftState);
var aValue : Double;
begin
  if not ReadOnly then
    FDataLink.Edit;
  aValue := Value;
  inherited;
  if AValue <> Value then
    FDataLink.Modified;
end;

end.

