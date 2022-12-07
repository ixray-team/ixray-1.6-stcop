
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

unit ElColorMap;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, StdCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ExtCtrls, ElTools, ElList, ElIni, ElCRC32;

type
  PColorEntry = ^TColorEntry;
  TColorEntry = record
    Id : integer;
    Name,
      Group : string;
    UseFg,
      UseBk : boolean;
    FgColor,
      BkColor : TColor;
  end;

  TMapChangeLink = class
  private
    FOnChange : TNotifyEvent;
  protected
    procedure TriggerChangeEvent; virtual;
  published
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomColArray = array[1..16] of integer;

  {Fake class! Do not remove!}
  TElColorEntries = class
  end;

  TElColorMap = class(TComponent)
  private
    FStorage : TElIniFile;
    FList : TElList;
    FLinkList : TElList;
    FOnChange : TNotifyEvent;
    {Fake field! Do not remove!}
    FEntries : TElColorEntries;
    FChanging : boolean;
    FUpdCount : integer;

    function GetItems(index : integer) : TColorEntry;
    procedure SetItems(index : integer; newValue : TColorEntry);
    procedure NotifyLinks;
    function GetCount : Integer;
    procedure SetStorage(newValue : TElIniFile);
  protected
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure TriggerChangeEvent; virtual;
    procedure ReadData(Stream : TStream); virtual;
    procedure WriteData(Stream : TStream); virtual;
    procedure DefineProperties(Filer : TFiler); override;
  public
    CustomCols : TStringList;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    function RegisterNotifyChange(Link : TMapChangeLink) : Boolean; virtual;
    function UnregisterNotifyChange(Link : TMapChangeLink) : Boolean; virtual;
    function Edit(ACaption : string) : Boolean; virtual;
    function AddItem(var Entry : TColorEntry) : Integer;
    function InsertItem(Index : integer; var Entry : TColorEntry) : Integer;
    procedure DeleteItem(index : integer);
    procedure ClearItems;
    function EntryByID(ID : integer) : integer;
    function MakeID(Entry : TColorEntry) : integer;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    property Items[index : integer] : TColorEntry read GetItems write SetItems; default; { Published }
    property Count : Integer read GetCount; { Public }
    procedure Restore;
    procedure Save;
  published
    {Fake property! Do not remove! -- fake property for registering a property editor}
    property ItemsList : TElColorEntries read FEntries;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property Storage : TElIniFile read FStorage write SetStorage;
  end; { TElColorMap }

implementation

uses frmColorMapItems;

function TElColorMap.GetItems(index : integer) : TColorEntry;
begin
  result := PColorEntry(FList[index])^;
end; { GetItems }

procedure TElColorMap.SetItems(index : integer; newValue : TColorEntry);
begin
  PColorEntry(FList[index])^ := newValue;
  NotifyLinks;
end; { SetItems }

function TElColorMap.RegisterNotifyChange(Link : TMapChangeLink) : Boolean; { public }
begin
  if FLinkList.IndexOf(Link) = -1 then FLinkList.Add(Link);
  result := true;
end; { RegisterNotifyChange }

function TElColorMap.UnregisterNotifyChange(Link : TMapChangeLink) : Boolean; { public }
var
  i : integer;
begin
  i := FLinkList.IndexOf(Link);
  result := i <> -1;
  if result then FLinkList.Delete(i);
end; { UnregisterNotifyChange }

function TElColorMap.Edit; { public }
var
  i : integer;
  E : TColorEntry;
begin
  with TColorMapItemsForm.Create(Application) do
  begin
    Runtime := true;
    Caption := ACaption;
    try
      for i := 0 to Count - 1 do
      begin
        E := Items[i];
        Map.AddItem(E);
      end;
      result := (ShowModal = mrOK);
      if result then
      begin
        ClearItems;
        BeginUpdate;
        for i := 0 to Map.Count - 1 do
        begin
          E := Map.Items[i];
          AddItem(E);
        end;
        EndUpdate;
        result := true;
      end; { if }
    finally
      Free; { Free dialog. }
    end; { try/finally }
  end; { with }
end; { Edit }

function TElColorMap.AddItem(var Entry : TColorEntry) : Integer; { public }
var
  P : PColorEntry;
begin
  New(P);
  Entry.ID := MakeID(Entry);
  P^ := Entry;
  result := FList.Add(P);
  NotifyLinks;
end; { AddItem }

function TElColorMap.InsertItem(Index : integer; var Entry : TColorEntry) : Integer; { public }
var
  P : PColorEntry;
begin
  New(P);
  Entry.ID := MakeID(Entry);
  P^ := Entry;
  FList.Insert(Index, P);
  result := index;
  NotifyLinks;
end; { InsertItem }

procedure TElColorMap.DeleteItem(index : integer); { public }
var
  P : PColorEntry;
begin
  P := PColorEntry(FList[index]);
  FList.Delete(index);
  Dispose(P);
  NotifyLinks;
end; { DeleteItem }

procedure TElColorMap.TriggerChangeEvent;
begin
  if (assigned(FOnChange)) then FOnChange(Self);
end; { TriggerChangeEvent }

procedure TElColorMap.NotifyLinks; { private }
var
  i : integer;
begin
  if (csLoading in ComponentState) or FChanging or (FUpdCount > 0) then exit;
  FChanging := true;
  TriggerChangeEvent;
  for i := 0 to FLinkList.Count - 1 do
    TMapChangeLink(FLinkList[i]).TriggerChangeEvent;
  FChanging := false;
end; { NotifyLinks }

function TElColorMap.GetCount : Integer;
begin
  result := FList.Count;
end; { GetCount }

procedure TElColorMap.ClearItems; { public }
begin
  while FList.Count > 0 do
  begin
    Dispose(PColorEntry(FList[0]));
    FList.delete(0);
  end;
end; { ClearItems }

destructor TElColorMap.Destroy;
begin
  CustomCols.Free;
  ClearItems;
  FList.Free;
  FLinkList.Free;
  inherited Destroy;
end; { Destroy }

function TElColorMap.EntryByID(ID : integer) : integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to FList.count - 1 do
  begin
    if PColorEntry(FList[i]).ID = ID then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TElColorMap.MakeID(Entry : TColorEntry) : integer;
begin
  result := CrcStr(Entry.Name + '@' + Entry.Group);
end;

procedure TElColorMap.Assign(Source : TPersistent);
var
  i : integer;
  E : TColorEntry;
begin
  if Source is TElColorMap then
    with Source as TElColorMap do
    begin
      ClearItems;
      for i := 0 to Count - 1 do
      begin
        E := Items[i];
        Self.AddItem(E);
      end;
      NotifyLinks;
    end
  else
    inherited;
end;

procedure TElColorMap.ReadData(Stream : TStream);
var
  i : integer;
  T : PColorEntry;
begin
  Stream.ReadBuffer(i, sizeof(integer));
  while i > 0 do
  begin
    New(T);
    Stream.ReadBuffer(T.ID, sizeof(T.ID));
    Stream.ReadBuffer(T.UseFg, sizeof(boolean));
    Stream.ReadBuffer(T.UseBk, sizeof(boolean));
    ReadStringFromStream(Stream, T.Group);
    ReadStringFromStream(Stream, T.Name);
    if T.UseFg then Stream.ReadBuffer(T.FgColor, sizeof(T.FgColor));
    if T.UseBk then Stream.ReadBuffer(T.BkColor, sizeof(T.BkColor));
    FList.Add(T);
    dec(i);
  end;
end;

procedure TElColorMap.WriteData(Stream : TStream);
var
  i : integer;
  T : PColorEntry;
begin
  i := FList.Count;
  Stream.WriteBuffer(i, sizeof(integer));
  for i := 0 to FList.Count - 1 do
  begin
    T := PColorEntry(FList[i]);
    Stream.WriteBuffer(T.ID, sizeof(T.ID));
    Stream.WriteBuffer(T.UseFg, sizeof(boolean));
    Stream.WriteBuffer(T.UseBk, sizeof(boolean));
    WriteStringToStream(Stream, T.Group);
    WriteStringToStream(Stream, T.Name);
    if T.UseFg then Stream.WriteBuffer(T.FgColor, sizeof(T.FgColor));
    if T.UseBk then Stream.WriteBuffer(T.BkColor, sizeof(T.BkColor));
  end;
end;

procedure TElColorMap.DefineProperties(Filer : TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Items', ReadData, WriteData, true);
end;

procedure TElColorMap.BeginUpdate; { public }
begin
  inc(FUpdCount);
end; { BeginUpdate }

procedure TElColorMap.EndUpdate; { public }
begin
  dec(FUpdCount);
  if FUpdCount < 0 then FUpdCount := 0;
  if FUpdCount = 0 then NotifyLinks;
end; { EndUpdate }

procedure TElColorMap.SetStorage(newValue : TElIniFile);
begin
  if (FStorage <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FStorage <> nil then
      FStorage.RemoveFreeNotification(Self);
    {$endif}
    FStorage := newValue;
    if FStorage <> nil then FStorage.FreeNotification(Self);
  end; {if}
end;

procedure TElColorMap.Restore;
var
  i, j : integer;
  SList, SList1 : TStringList;
  Entry : TColorEntry;
  SaveKey,
    S : string;
  S1 : string;
begin
  if not Assigned(FStorage) then exit;
  SList := nil;
  SList1 := nil;
  try
    BeginUpdate;
    SList1 := TStringList.Create;
    SList := TStringList.Create;
    if FStorage.OpenKey(FStorage.Delimiter + Name, false) then
    begin
      ClearItems;
      FStorage.EnumSubKeys('', SList1);
      for i := 0 to SList1.Count - 1 do
      begin
        SaveKey := FStorage.CurrentKey;
        if FStorage.OpenKey(SList1[i], false) then
        begin
          SList.Clear;
          if FStorage.EnumValues('', SList) then
          begin
            for j := 0 to SList.Count - 1 do
            begin
              Entry.Group := SList1[i];
              Entry.Name := SList[j];
              if FStorage.ReadString('', SList[j], '', S) then
              begin
                if Pos(',', S) > 0 then
                begin
                  s1 := Trim(Copy(S, 1, Pos(',', S) - 1));
                  if Length(s1) > 0 then
                  begin
                    if s1[1] = '#' then s1[1] := '$';
                    Entry.UseFg := true;
                    Entry.FgColor := StringToColor(s1);
                  end
                  else
                    Entry.UseFg := false;
                end
                else
                  Entry.UseFg := false;
                if Pos(',', S) > 0 then
                begin
                  s1 := Trim(Copy(S, Pos(',', S) + 1, Length(S)));
                  if Length(s1) > 0 then
                  begin
                    if s1[1] = '#' then s1[1] := '$';
                    Entry.UseBk := true;
                    Entry.BkColor := StringToColor(s1);
                  end
                  else
                    Entry.UseBk := false;
                end
                else
                  Entry.UseBk := false;
                AddItem(Entry);
              end;
            end;
          end
          else
            FStorage.Delete(SaveKey, SList1[i]);
        end
        else
          FStorage.Delete('', SList1[i]);
        FStorage.OpenKey(SaveKey, false);
      end;
    end;
  finally
    SList.Free;
    SList1.Free;
    EndUpdate;
  end;
end;

procedure TElColorMap.Save;
var
  i : integer;
  Entry : PColorEntry;
  S : string;
  S1, S2 : string;
  b : boolean;
begin
  if not Assigned(FStorage) then exit;
  b := FStorage.LazyWrite;
  FStorage.LazyWrite := true;
  if not FStorage.OpenKey(FStorage.Delimiter + Name, true) then
  begin
    FStorage.LazyWrite := b;
    exit;
  end;
  for i := 0 to Self.Count - 1 do
  begin
    Entry := PColorEntry(FList[i]);
    if Entry.UseFg then
      S1 := ColorToString(Entry.FgColor)
    else
      s1 := '';
    if (length(s1) > 0) and (s1[1] = '$') then s1[1] := '#';
    if Entry.UseBk then
      S2 := ColorToString(Entry.BkColor)
    else
      s2 := '';
    if (length(s2) > 0) and (s2[1] = '$') then s2[1] := '#';
    s := s1 + ',' + s2;
    FStorage.WriteString(Entry.Group, Entry.Name, s);
  end;
  if not b then FStorage.Save;
  FStorage.LazyWrite := b;
end;

procedure TElColorMap.Notification(AComponent : TComponent;
  Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FStorage then Storage := nil;
  end;
end;

constructor TElColorMap.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FLinkList := TElList.Create;
  FList := TElList.Create;
  CustomCols := TStringList.Create;
end; { Create }

procedure TMapChangeLink.TriggerChangeEvent;
begin
  if (assigned(FOnChange)) then FOnChange(Self);
end; { TriggerChangeEvent }

end.
