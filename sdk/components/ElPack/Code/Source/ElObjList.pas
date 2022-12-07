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

unit ElObjList;

interface

uses
  SysUtils, Classes, ElList, ElIni;

type
  TElObjectListItem = class;

  TElObjectListItemClass = class of TElObjectListItem;
  
  TElObjListSortCompare = function(Item1,
                                   Item2: TElObjectListItem;
                                   Cargo: Pointer): Integer;  

  {:
  }
  TElObjectList = class (TElList)
  private
    FLastID: Cardinal;
    FListItemClass: TElObjectListItemClass;
    FOwner: TPersistent;
    function GetItems(Index: Integer): TElObjectListItem;
    procedure SetItems(Index: Integer; Value: TElObjectListItem);
  protected
    {$ifndef D_2}
    function GetOwner: TPersistent; override;
    {$endif}
  public
    constructor Create(Owner : TPersistent; ListItemClass :  
        TElObjectListItemClass);
    function Add: TElObjectListItem;
    procedure AddItem(Item : TElObjectListItem);
    procedure AfterLoad(IniFile : TElIniFile; KeyName : string); virtual;
    procedure AfterSave(IniFile : TElIniFile; KeyName : string); virtual;
    procedure Assign(Source : TPersistent); override;
    procedure BeforeLoad(IniFile : TElIniFile; KeyName : string); virtual;
    procedure BeforeSave(IniFile : TElIniFile; KeyName : string); virtual;
    function First: TElObjectListItem;
    function IndexOf(Item: TElObjectListItem): Integer;
    function IndexOfBack(StartIndex: integer; Item: TElObjectListItem): Integer;
    function IndexOfFrom(StartIndex: integer; Item: TElObjectListItem): Integer;
    procedure Insert(Index: Integer; Item: TElObjectListItem);
    function Last: TElObjectListItem;
    procedure Sort(Compare: TElObjListSortCompare; Cargo: Pointer);
    property Items[Index: Integer]: TElObjectListItem read GetItems write 
        SetItems; default;
    property ListItemClass: TElObjectListItemClass read FListItemClass write 
        FListItemClass;
  published
    property LastID: Cardinal read FLastID;
  end;
  
  {:
  }
  TElObjectListItem = class (TPersistent)
  private
    FID: Cardinal;
    FList: TElObjectList;
    FLoading: Boolean;
  protected
    {$ifndef D_2}
    function GetOwner: TPersistent; override;
    {$endif}
  public
    constructor Create(List : TElObjectList); virtual;
    destructor Destroy; override;
    procedure AfterLoad(IniFile : TElIniFile; KeyName : string); virtual;
    procedure AfterSave(IniFile : TElIniFile; KeyName : string); virtual;
    procedure BeforeLoad(IniFile : TElIniFile; KeyName : string); virtual;
    procedure BeforeSave(IniFile : TElIniFile; KeyName : string); virtual;
    property ID: Cardinal read FID;
    property List: TElObjectList read FList;
    property Loading: Boolean read FLoading write FLoading;
  end;
  
  {:
  }
  TElHeteroObjectList = class (TElObjectList)
  public
    procedure BeforeLoad(IniFile : TElIniFile; KeyName : string); override;
    procedure BeforeSave(IniFile : TElIniFile; KeyName : string); override;
  end;


implementation

{:
}
{
****************************** TElObjectListItem *******************************
}
constructor TElObjectListItem.Create(List : TElObjectList);
begin
  inherited Create;
  FList := List;
end;

destructor TElObjectListItem.Destroy;
begin
  inherited Destroy;
end;

procedure TElObjectListItem.AfterLoad(IniFile : TElIniFile; KeyName : string);
begin
end;

procedure TElObjectListItem.AfterSave(IniFile : TElIniFile; KeyName : string);
begin
  IniFile.WriteInteger(KeyName, 'ID', FID);
end;

procedure TElObjectListItem.BeforeLoad(IniFile : TElIniFile; KeyName : string);
var
  I: Integer;
begin
  IniFile.ReadInteger(KeyName, 'ID', FID, I);
  FID := I;
end;

procedure TElObjectListItem.BeforeSave(IniFile : TElIniFile; KeyName : string);
begin
end;

{$ifndef D_2}
function TElObjectListItem.GetOwner: TPersistent;
begin
  result := FList;
end;
{$endif}

{:
}
{
***************************** TElHeteroObjectList ******************************
}
procedure TElHeteroObjectList.BeforeLoad(IniFile : TElIniFile; KeyName : 
    string);
var
  I: Integer;
  J: Integer;
  K: Integer;
  S: String;
  PC: TElObjectListItemClass;
  SL: TStringList;
begin
    Clear;
    IniFile.ReadInteger(KeyName, 'HetItemsCount', 0, i);
    if i > 0 then
    begin
      SL := TStringList.Create;
      try
        IniFile.EnumSubKeys(KeyName, SL);
        k := SL.Count - 1;
        for j := 0 to k do
        begin
          if IniFile.ReadString(KeyName + IniFile.Delimiter + 'Item{' + 
              IntToStr(
              j) + '}', 'ClsNm', '', S) then
          begin
            PC := TElObjectListItemClass(FindClass(S));
            if PC <> nil then
               TElList(Self).Add(PC.Create(Self));
            {else
               TElList(Self).Add(nil)};
          end;
        end;
      finally
        SL.Free;
      end;
    end;
end;

procedure TElHeteroObjectList.BeforeSave(IniFile : TElIniFile; KeyName : 
    string);
begin
  inherited BeforeSave(IniFile, KeyName);
  IniFile.WriteInteger(KeyName, 'HetItemsCount', Count);
end;

{:
}
{
******************************** TElObjectList *********************************
}
constructor TElObjectList.Create(Owner : TPersistent; ListItemClass :  
    TElObjectListItemClass);
begin
  inherited Create;
  FOwner := Owner;
  FLastID := 0;
  FListItemClass := ListItemClass;
end;

function TElObjectList.Add: TElObjectListItem;
begin
  result := FListItemClass.Create(Self);
  result.FID := FLastID;
  inc(FLastID);
  inherited Add(result);
end;

procedure TElObjectList.AddItem(Item : TElObjectListItem);
begin
  inherited Add(Item);
end;

procedure TElObjectList.AfterLoad(IniFile : TElIniFile; KeyName : string);
begin
end;

procedure TElObjectList.AfterSave(IniFile : TElIniFile; KeyName : string);
begin
end;

procedure TElObjectList.Assign(Source : TPersistent);
var
  I: Integer;
  Item: TElObjectListItem;
  ItemClass: TElObjectListItemClass;
begin
  if Source is TElObjectList then
  begin
    Clear;
    for i := 0 to TElObjectList(Source).Count -1 do
    begin
      ItemClass := TElObjectListItemClass(TElObjectList(
          Source).Items[i].ClassType);
      Item := ItemClass.Create(Self);
      Item.Assign(TElObjectList(Source).Items[i]);
      inherited Add(Item);
    end;
  end else inherited;
end;

procedure TElObjectList.BeforeLoad(IniFile : TElIniFile; KeyName : string);
var
  I: Integer;
begin
  inherited;
  Clear;
  IniFile.ReadInteger(KeyName, 'ItemsCount', 0, i);
  while i > 0 do
  begin
    dec(i);
    Add;
  end;
end;

procedure TElObjectList.BeforeSave(IniFile : TElIniFile; KeyName : string);
begin
  IniFile.WriteInteger(KeyName, 'ItemsCount', Count);
end;

function TElObjectList.First: TElObjectListItem;
begin
  Result := TElObjectListItem(inherited First);
end;

function TElObjectList.GetItems(Index: Integer): TElObjectListItem;
begin
  result := TElObjectListItem(inherited Items[index]);
end;

{$ifndef D_2}
function TElObjectList.GetOwner: TPersistent;
begin
  result := fOwner;
end;
{$endif}

function TElObjectList.IndexOf(Item: TElObjectListItem): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TElObjectList.IndexOfBack(StartIndex: integer; Item: 
    TElObjectListItem): Integer;
begin
  Result := inherited IndexOfBack(StartIndex, Item);
end;

function TElObjectList.IndexOfFrom(StartIndex: integer; Item: 
    TElObjectListItem): Integer;
begin
  Result := inherited IndexOfFrom(StartIndex, Item);
end;

procedure TElObjectList.Insert(Index: Integer; Item: TElObjectListItem);
begin
  inherited Insert(Index, Item);
end;

function TElObjectList.Last: TElObjectListItem;
begin
  Result := TElObjectListItem(inherited Last);
end;

procedure TElObjectList.SetItems(Index: Integer; Value: TElObjectListItem);
begin
  inherited Items[index] := Value;
end;

procedure TElObjectList.Sort(Compare: TElObjListSortCompare; Cargo: Pointer);
begin
  inherited Sort(TElListSortCompare(Compare), Cargo);
end;


initialization
end.
