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

unit ElPropTools;

interface

uses Classes, TypInfo, ElIni, ElTools, SysUtils;

function HasProperty(Comp : TObject; PropertyName : string) : boolean;
function GetPropertyRecord(Comp : TObject; PropertyName : string) : PPropInfo;

procedure StoreSetProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure StoreFloatProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure StoreObject(Comp : TObject; Storage : TElIniFile);
procedure StoreCollection(Collection : TCollection; Name : string; Storage : TElIniFile);
procedure StoreIntegerProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure StoreEnumProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure StoreStringProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure StoreStringList(Strings : TStrings; Name : string; Storage : TElIniFile);

procedure LoadSetProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure LoadFloatProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure LoadObject(Comp : TObject; Storage : TElIniFile);
procedure LoadCollection(Collection : TCollection; Name : string; Storage : TElIniFile);
procedure LoadIntegerProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure LoadEnumProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure LoadStringProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
procedure LoadStringList(Strings : TStrings; Name : string; Storage : TElIniFile);

implementation

procedure LoadSetProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
begin
  LoadIntegerProperty(Comp, Prop, Storage);
end;

procedure LoadFloatProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
var
  X : Extended;
  Stream : TDirectMemoryStream;
var
  i : integer;
begin
  Stream := TDirectMemoryStream.Create;
  i := sizeof(X);
  Stream.SetSize(i);
  Storage.ReadBinary('', Prop.Name, pchar(Stream.Memory)^, i);
  Stream.ReadBuffer(X, i);
  Stream.Free;
  SetFloatProp(Comp, Prop, X);
end;

function GetPropertyRecord(Comp : TObject; PropertyName : string) : PPropInfo;
var
  i : integer;
  PropCount : Integer;
  PropList : PPropList;
  PPI : PPropInfo;
begin
  PropCount := GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], nil);
  GetMem(PropList, PropCount * sizeof(pointer));
  GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], PropList);
  for i := 0 to PropCount - 1 do // Iterate
  begin
    if Uppercase(PropList[i].Name) = Uppercase(PropertyName) then
    begin
      New(PPI);
      Move(PropList[i]^, PPI^, sizeof(PPI^));
      result := PPI;
      FreeMem(PropList);
      exit;
    end;
  end; // for
  FreeMem(PropList);
  result := nil;
end;

function HasProperty(Comp : TObject; PropertyName : string) : boolean;
var
  i : integer;
  PropCount : Integer;
  PropList : PPropList;
begin
  PropCount := GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], nil);
  GetMem(PropList, PropCount * sizeof(pointer));
  GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], PropList);
  for i := 0 to PropCount - 1 do // Iterate
  begin
    if Uppercase(PropList[i].Name) = Uppercase(PropertyName) then
    begin
      result := true;
      FreeMem(PropList);
      exit;
    end;
  end; // for
  FreeMem(PropList);
  result := false;
end;

procedure LoadObject(Comp : TObject; Storage : TElIniFile);
var
  i : integer;
  PropCount : Integer;
  PropList : PPropList;
  Obj : TObject;
  FSaveKey : string;
begin
  PropCount := GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], nil);
  GetMem(PropList, PropCount * sizeof(pointer));
  GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], PropList);
  for i := 0 to PropCount - 1 do // Iterate
  begin
    case PropList[i]^.PropType^.Kind of //
      tkString,
        tkLString : LoadStringProperty(Comp, PropList[i], Storage);
      tkEnumeration,
        tkInteger : LoadIntegerProperty(Comp, PropList[i], Storage);
      tkSet : LoadSetProperty(Comp, PropList[i], Storage);
      tkFloat : LoadFloatProperty(Comp, PropList[i], Storage);
      tkClass :
        begin
          Obj := TObject(GetOrdProp(Comp, PropList[i]));
          if (Obj <> nil) then
          begin
            if Obj is TStrings then
              LoadStringList(Obj as TStrings, PropList[i].Name, Storage)
            else if Obj is TCollection then
              LoadCollection(Obj as TCollection, PropList[i].Name, Storage)
            else
            begin
              FSaveKey := Storage.CurrentKey;
              if Storage.OpenKey(PropList[i].Name, false) then LoadObject(Obj as TObject, Storage);
              Storage.OpenKey(FSaveKey, false);
            end;
          end;
        end;
    end; // case
  end; // for
  FreeMem(PropList);
end;

procedure LoadCollection(Collection : TCollection; Name : string; Storage : TElIniFile);
var
  FSaveKey,
    FSaveKey1 : string;
  i : integer;
begin
  FSaveKey := Storage.CurrentKey;
  if Storage.OpenKey(Name, false) then
  begin
    for i := 0 to Collection.Count - 1 do // Iterate
    begin
      FSaveKey1 := Storage.CurrentKey;
      if Storage.OpenKey('Item' + IntToStr(i), false) then
        LoadObject(Collection.Items[i], Storage);
      Storage.OpenKey(FSaveKey1, false);
    end; // for
  end;
  Storage.OpenKey(FSaveKey, false);
end;

procedure LoadIntegerProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
var
  i : integer;
begin
  Storage.ReadInteger('', Prop.Name, GetOrdProp(Comp, Prop), i);
  SetOrdProp(Comp, Prop, i);
end;

procedure LoadEnumProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
begin
  LoadIntegerProperty(Comp, Prop, Storage);
end;

procedure LoadStringProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
var
  S : string;
begin
  Storage.ReadString('', Prop.Name, GetStrProp(Comp, Prop), S);
  SetStrProp(Comp, Prop, S);
end;

procedure LoadStringList(Strings : TStrings; Name : string; Storage : TElIniFile);
begin
  Storage.ReadMultiString('', Name, Strings);
end;

type
  TFakeSet = set of 0..31;

procedure StoreStringList(Strings : TStrings; Name : string; Storage : TElIniFile);
begin
  Storage.WriteMultiString('', Name, Strings);
end;

procedure StoreObject(Comp : TObject; Storage : TElIniFile);
var
  i : integer;
  PropCount : Integer;
  PropList : PPropList;
  Obj : TObject;
  FSaveKey : string;
begin
  PropCount := GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], nil);
  GetMem(PropList, PropCount * sizeof(pointer));
  GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], PropList);
  for i := 0 to PropCount - 1 do // Iterate
  begin
    case PropList[i]^.PropType^.Kind of //
      tkString,
        tkLString : StoreStringProperty(Comp, PropList[i], Storage);
      tkEnumeration,
        tkInteger : StoreIntegerProperty(Comp, PropList[i], Storage);
      tkSet : StoreSetProperty(Comp, PropList[i], Storage);
      tkFloat : StoreFloatProperty(Comp, PropList[i], Storage);
      tkClass :
        begin
          Obj := TObject(GetOrdProp(Comp, PropList[i]));
          if (Obj <> nil) then
          begin
            if Obj is TStrings then
              StoreStringList(Obj as TStrings, PropList[i].Name, Storage)
            else if Obj is TCollection then
              StoreCollection(Obj as TCollection, PropList[i].Name, Storage)
            else
            begin
              FSaveKey := Storage.CurrentKey;
              if Storage.OpenKey(PropList[i].Name, true) then StoreObject(Obj as TObject, Storage);
              Storage.OpenKey(FSaveKey, false);
            end;
          end;
        end;
    end; // case
  end; // for
  FreeMem(PropList);
end;

procedure StoreCollection(Collection : TCollection; Name : string; Storage : TElIniFile);
var
  FSaveKey,
    FSaveKey1 : string;
  i : integer;
begin
  FSaveKey := Storage.CurrentKey;
  if Storage.OpenKey(Name, true) then
  begin
    for i := 0 to Collection.Count - 1 do // Iterate
    begin
      FSaveKey1 := Storage.CurrentKey;
      if Storage.OpenKey('Item' + IntToStr(i), true) then
        StoreObject(Collection.Items[i], Storage);
      Storage.OpenKey(FSaveKey1, false);
    end; // for
  end;
  Storage.OpenKey(FSaveKey, false);
end;

procedure StoreIntegerProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
var
  i : integer;
begin
  i := GetOrdProp(Comp, Prop);
  Storage.WriteInteger('', Prop.Name, i);
end;

procedure StoreEnumProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
begin
  StoreIntegerProperty(Comp, Prop, Storage);
end;

procedure StoreStringProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
var
  S : string;
begin
  S := GetStrProp(Comp, Prop);
  Storage.WriteString('', Prop.Name, S);
end;

procedure StoreSetProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
begin
  StoreIntegerProperty(Comp, Prop, Storage);
end;

procedure StoreFloatProperty(Comp : TObject; Prop : PPropInfo; Storage : TElIniFile);
var
  X : Extended;
  Stream : TDirectMemoryStream;
begin
  X := GetFloatProp(Comp, Prop);
  Stream := TDirectMemoryStream.Create;
  Stream.WriteBuffer(X, SizeOf(X));
  Storage.WriteBinary('', Prop.Name, pchar(Stream.Memory)^, sizeof(X));
  Stream.Free;
end;

end.
