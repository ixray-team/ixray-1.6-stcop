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

10/06/2201

  Added MoveToIns method

*)

unit ElMTree;

interface

uses Classes, SysUtils, ElList;

type
  TElMTree = class;

  TElMTreeItem = ^RElMTreeItem;
  RElMTreeItem = record
    Parent : TElMTreeItem;
    Data : pointer;
    List : TElList;
  end;

  TIterProc = procedure(Item : TElMTreeItem; Index : integer; var ContinueIterate : boolean;
    IterateData : pointer);

  TItemSaveEvent = procedure(Sender : TObject; Item : TElMTreeItem; Stream : TStream) of object;

  TElMTreeItemDelete = procedure(Sender : TObject; Item : TElMTreeItem; Data : pointer) of object;

  TElMTree = class
  private
    FRoot : TElMTreeItem;
    FCount : integer;
    FOnItemSave : TItemSaveEvent;
    FOnItemLoad : TItemSaveEvent;
    FOnItemDelete : TElMTreeItemDelete;
    function GetItem(index : integer) : TElMTreeItem;
  protected
    procedure TriggerItemSaveEvent(Item : TElMTreeItem; Stream : TStream); virtual;
    procedure TriggerItemLoadEvent(Item : TElMTreeItem; Stream : TStream); virtual;
    procedure TriggerItemDeleteEvent(Item : TElMTreeItem; Data : pointer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem(Parent : TElMTreeItem; Value : pointer) : TElMTreeItem;
    function InsertItem(Parent : TElMTreeItem; Index : integer; Value : pointer) : TElMTreeItem;
    procedure DeleteItem(Item : TElMTreeItem);
    procedure MoveTo(Item, NewParent : TElMTreeItem);
    procedure Clear;
    function GetIndex(Item : TElMTreeItem) : Integer;
    function GetAbsIndex(Item : TElMTreeItem) : Integer;
    procedure Iterate(IterateProc : TIterProc; IterateData : pointer);
    procedure SaveToStream(Stream : TStream); virtual;
    procedure LoadFromStream(Stream : TStream); virtual;
    procedure SaveSubTreeToStream(Item : TElMTreeItem; Stream : TStream); virtual;
         // Item will be saved too
    procedure LoadSubTreeFromStream(Item : TElMTreeItem; Stream : TStream); virtual;
    procedure MoveToIns(Item, NewParent : TElMTreeItem; Index : integer);
         // Item here is the "parent" item for all items loaded

    property Count : Integer read FCount; { Public }
    property Item[index : integer] : TElMTreeItem read GetItem; { Public }
    property OnItemSave : TItemSaveEvent read FOnItemSave write FOnItemSave;
    property OnItemLoad : TItemSaveEvent read FOnItemLoad write FOnItemLoad;
    property OnItemDelete : TElMTreeItemDelete read FOnItemDelete write FOnItemDelete;
    property Root : TElMTreeItem read FRoot;
  end;

implementation

procedure TElMTree.Iterate(IterateProc : TIterProc; IterateData : pointer);
var
  j : integer;
  DoContinue : boolean;

  procedure IntIterate(Item : TElMTreeItem);
  var
    i : integer;
  begin                                
    inc(j);
    if j >= 0 then IterateProc(Item, j, DoContinue, IterateData);
    if not (DoContinue) then exit;
    for i := 0 to Item^.List.Count - 1 do
    begin
      IntIterate(TElMTreeItem(Item.List[i]));
      if not (DoContinue) then exit;
    end;
  end;

begin
  j := -2;
  DoContinue := true;
  IntIterate(FRoot);
end;

function TElMTree.GetItem(index : integer) : TElMTreeItem;
type
  PGIRec = ^TGIRec;
  TGIRec = record
    j : integer;
    TSI : TElMTreeItem;
  end;
var
  GIRec : TGIRec;

  procedure IntGetItem(Item : TElMTreeItem; Index : integer; var ContinueIterate : boolean;
    IterateData : pointer);
  begin
    if Index = PGIRec(IterateData)^.j then
    begin
      PGIRec(IterateData)^.TSI := Item;
      ContinueIterate := false;
    end;
  end;

begin
  if (index < 0) or (index >= FCount) then
  begin
    result := nil;
    exit;
  end;
  GIRec.TSI := nil;
  GIRec.j := index;
  Iterate(@IntGetItem, @GIRec);
  result := GIRec.TSI;
end;

function TElMTree.AddItem(Parent : TElMTreeItem; Value : pointer) : TElMTreeItem; { public }
var
  TI : TElMTreeItem;
begin
  New(TI);
  TI^.List := TElList.Create;
  TI^.Data := Value;
  if Parent = nil then
    FRoot^.List.Add(TI)
  else
    Parent^.List.Add(TI);
  if Parent = nil then
    TI^.Parent := FRoot
  else
    TI^.Parent := Parent;
  result := ti;
  inc(FCount);
end; { AddItem }

function TElMTree.InsertItem(Parent : TElMTreeItem; Index : integer; Value : pointer) : TElMTreeItem; { public }
var
  TI : TElMTreeItem;
begin
  New(TI);
  TI^.List := TElList.Create;
  TI^.Data := Value;
  if Parent = nil then
  begin
    FRoot^.List.Insert(index, TI);
    TI^.Parent := FRoot;
  end
  else
  begin
    Parent^.List.Insert(Index, TI);
    TI^.Parent := Parent;
  end;
  result := ti;
  inc(FCount);
end; { InsertItem }

procedure TElMTree.DeleteItem(Item : TElMTreeItem); { public }
begin
  if (Item = nil) then exit;
  while Item^.List.Count > 0 do
    DeleteItem(Item^.List[0]);
  Item^.List.Free;
  Item^.Parent^.List.Remove(Item);
  TriggerItemDeleteEvent(Item, Item^.Data);
  Dispose(Item);
  dec(FCount);
end; { DeleteItem }

procedure TElMTree.Clear; { public }
begin
  while FRoot^.List.Count > 0 do
    DeleteItem(FRoot^.List[0]);
end; { Clear }

procedure TElMTree.MoveTo(Item, NewParent : TElMTreeItem); { public }
begin
  if Item = nil then exit;
  Item^.Parent.List.Remove(Item);
  if NewParent = nil then NewParent := FRoot;
  NewParent^.List.Add(Item);
  Item^.Parent := NewParent;
end; { MoveTo }



function TElMTree.GetIndex(Item : TElMTreeItem) : Integer; { public }
begin
  result := Item^.Parent^.List.IndexOf(Item);
end; { GetIndex }

function TElMTree.GetAbsIndex(Item : TElMTreeItem) : Integer; { public }
type
  PGIRec = ^TGIRec;
  TGIRec = record
    j : integer;
    TSI : TElMTreeItem;
  end;
var
  GIRec : TGIRec;

  procedure IntGetIndex(Item : TElMTreeItem; Index : integer; var ContinueIterate : boolean;
    IterateData : pointer);
  begin
    if PGIRec(IterateData)^.TSI = Item then
    begin
      PGIRec(IterateData)^.j := index;
      ContinueIterate := false;
    end;
  end;

begin
  if Item = nil then
  begin result := -1;
    exit;
  end;
  GIRec.j := -1;
  GIRec.TSI := Item;
  Iterate(@IntGetIndex, @GIRec);
  result := GIRec.j;
end;

procedure TElMTree.SaveToStream(Stream : TStream); { public }
begin
  SaveSubTreeToStream(FRoot, Stream);
end; { SaveToStream }

procedure TElMTree.LoadFromStream(Stream : TStream); { public }
begin
  LoadSubTreeFromStream(FRoot, Stream);
end; { LoadFromStream }

procedure TElMTree.SaveSubTreeToStream(Item : TElMTreeItem; Stream : TStream);

  procedure IntSave(Item : TElMTreeItem; Stream : TStream; Tree : TElMTree);
  var
    i, j : integer;
  begin
    i := Item^.List.Count;
    Stream.WriteBuffer(i, sizeof(integer));
    if (Item <> Tree.FRoot) then
      Tree.TriggerItemSaveEvent(Item, Stream);
    for j := 0 to i - 1 do
      IntSave(TElMTreeItem(Item^.List[j]), Stream, Tree);
  end;

begin
  if Item = nil then Item := FRoot;
  IntSave(Item, Stream, self);
end;

procedure TElMTree.LoadSubTreeFromStream(Item : TElMTreeItem; Stream : TStream);

  procedure IntLoad(Item : TElMTreeItem; Stream : TStream; Tree : TElMTree);
  var
    i, j : integer;
    NewItem : TElMTreeItem;
  begin
    Stream.ReadBuffer(i, sizeof(integer));
    if Item <> Tree.FRoot then
      Tree.TriggerItemLoadEvent(Item, Stream);
    if i < 0 then
       raise EOutOfMemory.Create(''); 
    for j := 0 to i - 1 do
    begin
      NewItem := Tree.AddItem(Item, nil);
      IntLoad(NewItem, Stream, Tree);
    end;
  end;

begin
  if Item = nil then Item := FRoot;
  IntLoad(Item, Stream, self);
end;

procedure TElMTree.TriggerItemSaveEvent(Item : TElMTreeItem; Stream : TStream);
{ Triggers the OnItemSave event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnItemSave)) then
    FOnItemSave(Self, Item, Stream);
end; { TriggerItemSaveEvent }

procedure TElMTree.TriggerItemLoadEvent(Item : TElMTreeItem; Stream : TStream);
{ Triggers the OnItemLoad event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnItemLoad)) then
    FOnItemLoad(Self, Item, Stream);
end; { TriggerItemLoadEvent }

procedure TElMTree.TriggerItemDeleteEvent(Item : TElMTreeItem; Data : pointer);
{ Triggers the OnItemDelete event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnItemDelete)) then
    FOnItemDelete(Self, Item, Data);
end; { TriggerItemDeleteEvent }

constructor TElMTree.Create; { public }
begin
  inherited;
  New(FRoot);
  FRoot.Parent := nil;
  FRoot.Data := nil;
  FRoot^.List := TElList.Create;
end; { Create }

destructor TElMTree.Destroy; { public }
begin
  Clear;
  FRoot^.List.Free;
  Dispose(FRoot);
  inherited;
end; { Destroy }

procedure TElMTree.MoveToIns(Item, NewParent : TElMTreeItem; Index : integer);
begin
  if Item = nil then exit;
  Item^.Parent.List.Remove(Item);
  if NewParent = nil then
    NewParent := FRoot;
  NewParent^.List.Insert(Index, Item);
  Item^.Parent := NewParent;
end; { MoveTo }


end.
