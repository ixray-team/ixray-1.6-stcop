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

01/10/2002

  Changed ReadOnly property to support Unassigned 

*)

unit ElDBDTPick;

interface

uses
     DB,
     DBCtrls,

     {$ifdef VCL_6_USED}
     Variants,
     {$endif}
     ElDTPick,

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

  TElDBDateTimePicker = class(TElDateTimePicker)
  private
    FDataLink: TFieldDataLink;
    FNowForNULLValues: Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateData(Sender: TObject);
  protected
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetModified(newValue : Boolean); override;
    procedure TriggerChangeEvent; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CloseUp(AcceptValue: boolean); override;
    procedure DropDown; override;
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
    // property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property NowForNULLValues: Boolean read FNowForNULLValues write 
        FNowForNULLValues;
  end;

implementation

constructor TElDBDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UnassignedAllowed := true;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TElDBDateTimePicker.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TElDBDateTimePicker.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TElDBDateTimePicker.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TElDBDateTimePicker.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FDataLink.Field.IsNull then
    begin
      if not NowForNullValues then
      begin
        DateTime := FDataLink.Field.AsDateTime;
        Unassigned := true;
      end
      else
      begin
        Unassigned := false;
        DateTime := Now;
      end;
    end
    else
    begin
      Unassigned := false;
      DateTime := FDataLink.Field.AsDateTime;
    end;
  end;
  Modified := false;
end;

{$ifdef VCL_4_USED}
function TElDBDateTimePicker.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;
{$endif}

function TElDBDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TElDBDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TElDBDateTimePicker.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TElDBDateTimePicker.GetReadOnly: Boolean;
begin
 if (not (csDesigning in ComponentState)) and FDataLink.Active then
  Result := (FReadOnly or FDataLink.ReadOnly) or (not FDataLink.CanModify)
 else
  Result := FReadOnly;
end;

procedure TElDBDateTimePicker.KeyPress(var Key: Char);
begin
  if not ReadOnly then
  begin
    case Key of
      #8, ' ':
        FDataLink.Edit;
      #27:
        FDataLink.Reset;
    end;
    inherited KeyPress(Key);
  end;
end;

procedure TElDBDateTimePicker.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TElDBDateTimePicker.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TElDBDateTimePicker.SetDataSource(Value: TDataSource);
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

procedure TElDBDateTimePicker.SetReadOnly(Value: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

{$ifdef VCL_4_USED}
function TElDBDateTimePicker.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$endif}

procedure TElDBDateTimePicker.UpdateData(Sender: TObject);
begin
  if Unassigned then
    FDataLink.Field.Clear
  else
    FDataLink.Field.AsDateTime := DateTime;
end;

{$ifdef VCL_4_USED}
function TElDBDateTimePicker.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{$endif}

procedure TElDBDateTimePicker.SetModified(newValue : Boolean);
begin
  if FModified <> newValue then
  begin
    inherited;
    FDataLink.Modified;
  end;
end;

procedure TElDBDateTimePicker.TriggerChangeEvent;
begin
  FDataLink.DataSource.Edit;
  inherited;
end;  { TriggerChangeEvent }

procedure TElDBDateTimePicker.KeyDown(var Key : Word; Shift : TShiftState);
begin
  if not ReadOnly then
    inherited;
end;

procedure TElDBDateTimePicker.CMEnter(var Message: TMessage);
begin
  inherited;
  FDataLink.Reset;
  Modified := false;
end;

procedure TElDBDateTimePicker.CloseUp(AcceptValue: boolean);
begin
  {$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
  if FForm.Visible then
  begin
    if AcceptValue then
      FDataLink.Edit;
    inherited;
  end;
{$endif}
end;

procedure TElDBDateTimePicker.DropDown;
begin
  {$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
  if DroppedDown then
    inherited
  else
  begin
    if not ReadOnly then
      inherited
    else
      FCalButton.Down := false;
  end;
  {$endif}
end;




end.
