
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

  ElDBLabel made unicode

*)

unit ElDBLbl;

interface

uses
     DB,
     DBCtrls,

     ElHTMLLbl,
     ElStrUtils,

     Forms,
     Windows,
     Controls,
     StdCtrls,
     Messages,

     Classes,
     SysUtils;

type

  TElDBLabel = class(TElHTMLLabel)
  private
    FDataLink: TFieldDataLink;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetFieldText: TElFString;
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
  end;

implementation

constructor TElDBLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TElDBLabel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TElDBLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TElDBLabel.DataChange(Sender: TObject);
begin
  Caption := GetFieldText;
end;

{$ifdef VCL_4_USED}
function TElDBLabel.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;
{$endif}

function TElDBLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TElDBLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TElDBLabel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TElDBLabel.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TElDBLabel.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TElDBLabel.SetDataSource(Value: TDataSource);
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
function TElDBLabel.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

function TElDBLabel.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{$endif}

function TElDBLabel.GetFieldText: TElFString;
begin
  if not FDataLink.Active then
  begin
    if csDesigning in ComponentState then
      Result := Name
    else
      Result := ''
  end
  else
  if FDataLink.Field <> nil then
  {$ifdef ELPACK_UNICODE}
    if (FDataLink.Field.DataType = ftWideString) and (not FDataLink.Field.IsNull) then
      Result := FDataLink.Field.Value
    else
  {$endif}
      Result := FDataLink.Field.DisplayText;
end;

end.
