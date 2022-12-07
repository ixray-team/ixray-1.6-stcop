
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

unit ElSndMap;

interface

uses
{$ifdef MSWINDOWS}
  Windows,
  Messages,
  ElRegUtils,
  mmSystem,
{$ifdef VCL_6_USED}
Types,
{$endif}
{$endif}
  SysUtils,
  Classes,
  ElIni,
  ElTools,
  TypInfo;

type

{$ifndef VCL_6_USED}
  TElSoundName = string[255];
{$else}
  TElSoundName = string;
{$endif}

  TElSoundMap = class(TComponent)
  private
    FSchemes : TStringList;
    FStorage : TElIniFile;
    FStoragePath : string;
    FScheme : string;
    FApplicationKey : string;
    FApplicationName : string;
    FRegIni,
      ARegIni : TElIniFile;
    FEventKeys : TStringList;
    FMute : Boolean;
    function GetEventLabels(EventKey : string) : string;
    procedure SetEventLabels(EventKey : string; newValue : string);

    function GetEnabled(EventKey : string) : boolean;
    procedure SetEnabled(EventKey : string; newValue : boolean);

    function GetSchemes : TStringList;
    function GetEventKeys : TStringList;
    function GetEventValues(EventKey : string) : string;
    procedure SetEventValues(EventKey : string; newValue : string);
    procedure SetApplicationName(newValue : string);
    procedure SetApplicationKey(newValue : string);
    procedure SetScheme(newValue : string);
    procedure SetStorage(newValue : TElIniFile);
    procedure SetStoragePath(newValue : string);
  protected
    { Protected declarations }
    procedure Notification(AComponent : TComponent; operation : TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Play(EventKey : string);
    procedure Add(EventKey, EventLabel, EventValue : string; Enabled : boolean);
    procedure Delete(EventKey : string);
    procedure Loaded; override;

    property EventKeys : TStringList read GetEventKeys; { Published }
    property EventLabel[EventKey : string] : string read GetEventLabels write SetEventLabels; { Published }
    property EventValue[EventKey : string] : string read GetEventValues write SetEventValues; { Published }
    property EventEnabled[EventKey : string] : boolean read GetEnabled write SetEnabled; { Published }
    property Schemes : TStringList read GetSchemes;
  published
    property Mute : Boolean read FMute write FMute; { Published }
    property ApplicationName : string read FApplicationName write SetApplicationName; { Published }
    property ApplicationKey : string read FApplicationKey write SetApplicationKey; { Published }
    property Scheme : string read FScheme write SetScheme;
    property StoragePath : string read FStoragePath write SetStoragePath; { Published }
    property Storage : TElIniFile read FStorage write SetStorage;
  end; { TElSoundMap }

implementation

procedure TElSoundMap.Loaded;
begin
  inherited;
  if FStorage = nil then
    FStoragePath := '';
end;

function TElSoundMap.GetSchemes : TStringList;
begin
  FSchemes.Clear;
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  FRegIni.EnumSubKeys(FStoragePath + FRegIni.Delimiter + 'Schemes\Schemes', FSchemes);
  result := FSchemes;
end;

function TElSoundMap.GetEventKeys;
begin
  FEventKeys.Clear;
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  FRegIni.EnumSubKeys(FStoragePath + FRegIni.Delimiter + 'Schemes\Apps\' + ApplicationKey, FEventKeys);
  result := FEventKeys;
end; { GetEventNames }

function TElSoundMap.GetEventLabels;
var
  SaveKey : string;
begin
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  SaveKey := FRegIni.CurrentKey;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + 'EventLabels\' + EventKey, false) then
  begin
    FRegIni.ReadString('', '', '', result);
    FRegIni.OpenKey(SaveKey, false);
  end
  else
    result := '';
end; { GetEventValues }

procedure TElSoundMap.SetEventLabels(EventKey : string; newValue : string);
var
  SaveKey : string;
begin
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  SaveKey := FRegIni.CurrentKey;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + '\EventLabels\' + EventKey, true) then
  begin
    FRegIni.WriteString('', '', newValue);
    FRegIni.OpenKey(SaveKey, false);
  end;
end; { SetEventValues }

function TElSoundMap.GetEventValues(EventKey : string) : string;
var
  SaveKey : string;
begin
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  SaveKey := FRegIni.CurrentKey;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + 'Schemes\Apps\' + ApplicationKey + '\' + EventKey + '\' + Scheme, false) then
  begin
    FRegIni.ReadString('', '', '', result);
    FRegIni.OpenKey(SaveKey, false);
  end
  else
    result := '';
end; { GetEventValues }

procedure TElSoundMap.SetEventValues(EventKey : string; newValue : string);
var
  SaveKey : string;
begin
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  SaveKey := FRegIni.CurrentKey;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + 'Schemes\Apps\' + ApplicationKey + '\' + EventKey + '\' + Scheme, true) then
  begin
    FRegIni.WriteString('', '', newValue);
    FRegIni.OpenKey(SaveKey, false);
  end;
end; { SetEventValues }

function TElSoundMap.GetEnabled(EventKey : string) : boolean;
var
  SaveKey : string;
begin
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  SaveKey := FRegIni.CurrentKey;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + 'Schemes\Apps\' + ApplicationKey + '\' + EventKey, false) then
  begin
    FRegIni.ReadBool('', 'Disabled', false, result);
    result := not result;
    FRegIni.OpenKey(SaveKey, false);
  end
  else
    result := true;
end;

procedure TElSoundMap.SetEnabled(EventKey : string; newValue : boolean);
var
  SaveKey : string;
begin
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  SaveKey := FRegIni.CurrentKey;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + 'Schemes\Apps\' + ApplicationKey + '\' + EventKey, true) then
  begin
    FRegIni.WriteBool('', 'Disabled', (not newValue));
    FRegIni.OpenKey(SaveKey, false);
  end;
end;

procedure TElSoundMap.Play(EventKey : string); { public }
var
  S : string;
begin
  if not Mute then
  begin
    S := Trim(EventValue[EventKey]);
    if (Length(S) > 0) and EventEnabled[EventKey] then
      ElTools.PlaySound(PChar(s), 0, {$ifdef MSWINDOWS}SND_ASYNC{$else}0{$endif});
  end;
end; { Play }

procedure TElSoundMap.Add(EventKey, EventLabel, EventValue : string; Enabled : boolean);
var
  SaveKey : string;
begin
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  SaveKey := FRegIni.CurrentKey;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + 'Schemes\Apps\' + ApplicationKey + '\' + EventKey, true) then
  begin
    Self.EventLabel[EventKey] := EventLabel;
    Self.EventValue[EventKey] := EventValue;
    EventEnabled[EventKey] := Enabled;
    FRegIni.OpenKey(SaveKey, false);
  end;
end;

procedure TElSoundMap.Delete(EventKey : string); { public }
var
  SaveKey : string;
begin
  if FStorage = nil then
    FRegIni := ARegIni
  else
    FRegIni := FStorage;
  SaveKey := FRegIni.CurrentKey;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + 'Schemes\Apps\' + ApplicationKey, true) then
  begin
    FRegIni.Delete(EventKey, '');
    FRegIni.OpenKey(SaveKey, false);
  end;
  if FRegIni.OpenKey(FStoragePath + FRegIni.Delimiter + 'EventLabels', false) then
  begin
    FRegIni.Delete(EventKey, '');
    FRegIni.OpenKey(SaveKey, false);
  end;
end; { SetEventValues }

procedure TElSoundMap.SetApplicationKey(newValue : string);
begin
  if (FApplicationKey <> newValue) then
  begin
    {$ifdef MSWINDOWS}
    if not IsValidKeyName(newValue) then
      raise Exception.Create('Key name contains invalid characters')
    else
    {$endif}
      FApplicationKey := newValue;
  end; { if }
end; { SetApplicationKey }

procedure TElSoundMap.SetScheme(newValue : string);
begin
  if (FScheme <> newValue) then
  begin
    {$ifdef MSWINDOWS}
    if not IsValidKeyName(newValue) then
      raise Exception.Create('Scheme name contains invalid characters')
    else
    {$endif}
      FScheme := newValue;
  end; { if }
end; { SetApplicationKey }

procedure TElSoundMap.SetApplicationName(newValue : string);
begin
  if (FApplicationName <> newValue) then
  begin
    FApplicationName := newValue;
    if FStorage = nil then
      FRegIni := ARegIni
    else
      FRegIni := FStorage;
    FRegIni.WriteString(FStoragePath + FRegIni.Delimiter + 'Schemes\Apps\' + ApplicationKey, '', newValue);
  end; { if }
end; { SetApplicationKey }

procedure TElSoundMap.SetStorage(newValue : TElIniFile);
begin
  if (FStorage <> newValue) then
  begin
    FStorage := newValue;
    if FStorage = nil then
      StoragePath := '';
  end; { if }
end; { SetStorage }

procedure TElSoundMap.SetStoragePath(newValue : string);
begin
  if FStoragePath <> newValue then
  begin
    FStoragePath := newValue;
    if (FStorage = nil) and (not (csLoading in ComponentState)) then FStoragePath := '';
  end;
end;

procedure TElSoundMap.Notification(AComponent : TComponent; operation : TOperation);
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) then
  begin
    if (AComponent = FStorage) then Storage := nil;
  end; { if }
end; { Notification }

destructor TElSoundMap.Destroy;
begin
  FSchemes.Free;
  FEventKeys.Free;
  ARegIni.Free;
  inherited Destroy;
end; { Destroy }

constructor TElSoundMap.Create(AOwner : TComponent);
{ Creates an object of type TElSoundMap, and initializes properties. }
begin
  inherited Create(AOwner);
  ARegIni := TElIniFile.Create(Self);
  FEventKeys := TStringList.Create;
  ARegIni.UseRegistry := true;
  ARegIni.Path := '\AppEvents';
  FScheme := '.current';
  FSchemes := TStringList.Create;
end; { Create }

end.
