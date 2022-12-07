
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

unit ElOpts;

{ Component template to store and load properties }

interface

uses
  //Windows,
  SysUtils,
  //Messages,
  Classes,
  {$ifdef MSWINDOWS}
  Registry,
  {$endif}
  IniFiles,
  ElIni,
  ElTools,
{$ifdef VCL_6_USED}
Types,
{$endif}
  TypInfo;

type

  TElStorageType = (eosRegistry, eosIni, eosElIni);

  TElOptions = class (TComponent)
  protected
    FAutoSave: Boolean;
    FIniName: string;
    FIniSection: string;
    FLoading: Boolean;
    FStorage: TElIniFile;
    FStorageType: TElStorageType;
    procedure SetAutoSave(Value: Boolean); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Load; virtual;
    procedure Save; virtual;
    property Loading: Boolean read FLoading;
  published
    property AutoSave: Boolean read FAutoSave write SetAutoSave;
    property IniName: string read FIniName write FIniName;
    property IniSection: string read FIniSection write FIniSection;
    property Storage: TElIniFile read FStorage write FStorage;
    property StorageType: TElStorageType read FStorageType write FStorageType;
  end;

implementation

{
*************************************** TElOptions ***************************************
}
constructor TElOptions.Create(AOwner : TComponent);
  
  { Creates an object of type TElOptions, and initializes properties. }
  
begin
  inherited Create(AOwner);
  FStorageType := eosRegistry;
  FIniSection := 'Options';
end;

destructor TElOptions.Destroy;
begin
  if FAutoSave then Save;
  inherited Destroy;
end;
{$warnings off}
procedure TElOptions.Load;
var
{$ifdef MSWINDOWS}
  Ini: TRegIniFile;
{$endif}
  Ini1: TIniFile;
  Ini2: TElIniFile;
  PropName: string;
  i: Integer;
  j: Integer;
  PropCount: Integer;
  PropList: PPropList;
  S: string;
  Section: string;
  Obj : TObject;
begin

  FLoading := true;
  Section := FIniSection;
  case FStorageType of
    {$ifdef MSWINDOWS}
    eosRegistry : Ini := TRegIniFile.Create(FIniName);
    {$endif}
    eosIni : Ini1 := TIniFile.Create(FIniName);
    eosElIni :
      begin
        if FStorage = nil then
        begin
          Ini2 := TElIniFile.Create(nil);
          Ini2.Path := FIniName;
        end
        else
          Ini2 := FStorage;
        Section := Ini2.Delimiter + Section;
      end;
  end;
  PropCount := GetPropList(ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkClass], nil);
  GetMem(PropList, PropCount * sizeof(pointer));
  GetPropList(ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkClass], PropList);
  for i := 0 to PropCount - 1 do
  begin
    PropName := PropList[i]^.Name;
    case PropList[i]^.PropType^.Kind of
      tkString, tkLString :
        begin
          case FStorageType of
            {$ifdef MSWINDOWS}
            eosRegistry : S := Ini.ReadString(Section, PropName, GetStrProp(self, PropList[i]));
            {$endif}
            eosIni : S := Ini1.ReadString(Section, PropName, GetStrProp(self, PropList[i]));
            eosElIni : Ini2.ReadString(Section, PropName, GetStrProp(self, PropList[i]), S);
          end;
          SetStrProp(Self, PropList[i], S);
        end;
      tkEnumeration,
        tkInteger :
        begin
          case FStorageType of
            {$ifdef MSWINDOWS}
            eosRegistry : J := Ini.ReadInteger(Section, PropName, GetOrdProp(self, PropList[i]));
            {$endif}
            eosIni : J := Ini1.ReadInteger(Section, PropName, GetOrdProp(self, PropList[i]));
            eosElIni : Ini2.ReadInteger(Section, PropName, GetOrdProp(self, PropList[i]), J);
          end;
          SetOrdProp(self, PropList[i], J);
        end;
      tkClass:
        begin
          Obj := TObject(Pointer(GetOrdProp(Self, PropList[i])));
          if (Obj is TStrings) and (FStorageType = eosElIni) then
            Ini2.ReadMultiString(Section, PropName, TStrings(Obj));
        end;
    end;
  end;
  FreeMem(PropList);
  case FStorageType of
    {$ifdef MSWINDOWS}
    eosRegistry : Ini.Free;
    {$endif}
    eosIni : Ini1.Free;
    eosElIni :
      if FStorage <> Ini2 then Ini2.Free;
  end;
  FLoading := false;
  Loaded;
end;

procedure TElOptions.Save;
var
  {$ifdef MSWINDOWS}
  Ini: TRegIniFile;
  {$endif}
  Ini1: TIniFile;
  Ini2: TElIniFile;
  PropName: string;
  i: Integer;
  PropCount: Integer;
  PropList: PPropList;
  Section: string;
  Obj : TObject;
begin
  Section := FIniSection;
  case FStorageType of
  {$ifdef MSWINDOWS}
    eosRegistry : Ini := TRegIniFile.Create(FIniName);
  {$endif}
    eosIni : Ini1 := TIniFile.Create(FIniName);
    eosElIni :
      begin
        if FStorage = nil then
        begin
          Ini2 := TElIniFile.Create(nil);
          Ini2.Path := FIniName;
        end
        else
          Ini2 := FStorage;
        Section := Ini2.Delimiter + Section;
        Ini2.OpenKey(Section, true);
      end;
  end;
  PropCount := GetPropList(ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkClass], nil);
  GetMem(PropList, PropCount * sizeof(pointer));
  GetPropList(ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkClass], PropList);
  for i := 0 to PropCount - 1 do
  begin
    PropName := PropList[i]^.Name;
    case PropList[i]^.PropType^.Kind of
      tkString,
        tkLString :
        case FStorageType of
          {$ifdef MSWINDOWS}
          eosRegistry : Ini.WriteString(Section, PropName, GetStrProp(self, PropList[i]));
          {$endif}
          eosIni : Ini1.WriteString(Section, PropName, GetStrProp(self, PropList[i]));
          eosElIni : Ini2.WriteString('', PropName, GetStrProp(self, PropList[i]));
        end;
      tkEnumeration,
        tkInteger :
        case FStorageType of
          {$ifdef MSWINDOWS}
          eosRegistry : Ini.WriteInteger(Section, PropName, GetOrdProp(self, PropList[i]));
          {$endif}
          eosIni : Ini1.WriteInteger(Section, PropName, GetOrdProp(self, PropList[i]));
          eosElIni : Ini2.WriteInteger('', PropName, GetOrdProp(self, PropList[i]));
        end;
      tkClass:
        begin
          Obj := TObject(Pointer(GetOrdProp(Self, PropList[i])));
          if (Obj is TStrings) and (FStorageType = eosElIni) then
            Ini2.WriteMultiString('', PropName, TStrings(Obj));
        end;
    end;
  end;
  FreeMem(PropList);
  case FStorageType of
    {$ifdef MSWINDOWS}
    eosRegistry : Ini.Free;
    {$endif}
    eosIni : Ini1.Free;
    eosElIni :
      begin
        //Ini2.Save;
        if FStorage <> Ini2 then Ini2.Free;
      end;
  end;
end;
{$warnings on}
procedure TElOptions.SetAutoSave(Value: Boolean);
begin
  FAutoSave := Value;
end;

{$WARNINGS OFF}

{$WARNINGS ON}

end.
