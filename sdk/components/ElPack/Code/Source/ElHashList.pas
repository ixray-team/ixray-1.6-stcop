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

unit ElHashList; { TElHashList component. }

{$Q-}
{$RANGECHECKS OFF}
{$ALIGN ON}

interface

uses
  SysUtils, ElCRC32(*, ElMD5*);

type
  EElHashListError = class(Exception)
  end;

const
  MaxHashListSize = Maxint div 16;

type
  PHash = ^THash;
{$IFDEF Ver120}
  THash = array[1..4] of LongWord;
{$ELSE}
  THash = array[1..4] of integer;
{$ENDIF}

  PHashRecord = ^THashRecord;
  THashRecord = record
    Hash : PHash;
    ItemData : pointer;
  end;

  PHashList = ^THashList;
  THashList = array[0..MaxHashListSize - 1] of PHashRecord;

  TElHashList = class;

  TElHashType = (ehtMD5, ehtQuick, ehtCRC32);

  OnHashDeleteEvent = procedure(Sender : TElHashList; Data : pointer) of object;

  THashInsertDupesMode = (himInsert, himRaise, himReplace, himIgnore, himMove);

  TElHashList = class(TObject)
  private
    FAutoClearObjects : Boolean;
    FNoCase : boolean;
    FHashType : TElHashType;
    FList : PHashList;
    FCount : Integer;
    FCapacity : Integer;
    FOnDelete : OnHashDeleteEvent;
    FQuickHash : Boolean;
    FInsertDupesMode : THashInsertDupesMode;
    FRaiseOnAbsence : Boolean;

    procedure Grow;
    procedure SetCapacity(NewCapacity : Integer);
    function GetItem(Hash : string) : pointer;
    procedure SetQuickHash(newValue : Boolean);
    procedure SetHashType(newValue : TElHashType);
    procedure SetAutoClearObjects(newValue : Boolean);
  protected
    { Protected declarations }

    function CalcQuickHash(Hash : string) : Integer; virtual;
  public
    { Public declarations }
    destructor Destroy; override;
    procedure AddItem(Hash : string; Value : Pointer);
    procedure DeleteItem(Hash : string);
    procedure InsertItem(Index : integer; Hash : string; Value : pointer);
    function GetIndex(Hash : string) : integer;
    procedure Clear;

    property Count : Integer read FCount; { Public }
    property Capacity : Integer read FCapacity; { Public }
    function GetByIndex(Index : integer) : Pointer;
    constructor Create;

    property Item[Hash : string] : pointer read GetItem; { Public }
    property OnDelete : OnHashDeleteEvent read FOnDelete write FOnDelete;
    property QuickHash : Boolean read FQuickHash write SetQuickHash; { Public }
    property RaiseOnAbsence : Boolean read FRaiseOnAbsence write FRaiseOnAbsence default False; { Public }
    property InsertDupesMode : THashInsertDupesMode read FInsertDupesMode write FInsertDupesMode; { Public }
    property HashType : TElHashType read FHashType write SetHashType; { Public }
    property NoCase : boolean read FNoCase write FNoCase;
    property AutoClearObjects : Boolean read FAutoClearObjects write SetAutoClearObjects; { Published }
  end; { TElHashList }

implementation

procedure TElHashList.AddItem(Hash : string; Value : Pointer); { public }
begin
  InsertItem(FCount, Hash, Value);
end; { AddItem }

procedure TElHashList.DeleteItem(Hash : string); { public }
var
  index : integer;
begin
  Index := GetIndex(Hash);
  if (index < 0) or (index >= FCount) then raise EElHashListError.Create('Hash not found.');
  if assigned(FOnDelete) then FOnDelete(Self, FList[Index]^.ItemData);

  if AutoClearObjects and (FList[Index]^.ItemData <> nil) then TObject(FList[Index]^.ItemData).Free;

  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(Pointer));
  if FCount < (FCapacity div 2) then SetCapacity(FCapacity div 2);
end; { DeleteItem }

procedure TElHashList.InsertItem(Index : integer; Hash : string; Value : pointer); { public }
var
  P : PHashRecord;
  (*
  MD5 : TCrMD5;
  *)
  b : boolean;
  i : integer;
  h : integer;
begin
  if (Index < 0) or (index > FCount) then raise EElHashListError.Create('Invalid position for HashList.');
  New(P);
  if FNoCase then Hash := Uppercase(Hash);
  case FHashType of
    ehtQuick : P^.Hash := pointer(CalcQuickHash(Hash));
    ehtCRC32,
    ehtMD5 : P^.Hash := pointer(CrcStr(Hash));
    (*
    ehtMD5 :
      begin
        New(P^.Hash);
        MD5 := TCrMD5.Create;
        MD5.InputType := SourceString;
        MD5.InputString := Hash;
        MD5.pOutputArray := @hashDigest(P^.Hash^);
        MD5.MD5_Hash;
        MD5.Free;
      end;
    *)
  end;
  i := 0;
  if FQuickHash or (FHashType = ehtCrc32) then
  begin
    if FQuickHash then
      h := CalcQuickHash(Hash)
    else
      h := CrcStr(Hash);
    while (i < FCount) and (integer(FList[i]^.Hash) <> h) do
      inc(i);
  end
  else
  begin
    while (i < FCount) and
      (not CompareMem(P.Hash, FList[i]^.Hash, SizeOf(Integer) * 4)) do
      inc(i);
  end;
  b := i <> FCount;
  if b then
  begin
    case FInsertDupesMode of
      himIgnore : exit;
      himRaise : raise EElHashListError.Create('Duplicate entry.');
      himReplace :
        begin
          i := GetIndex(Hash);
          Dispose(P);
          P := FList[i];
          if assigned(FOnDelete) then FOnDelete(Self, P.ItemData);
          P.ItemData := Value;
          exit;
        end;
    end;
  end;
  P^.ItemData := Value;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList[Index] := P;
  Inc(FCount);
end; { InsertItem }

function TElHashList.GetItem(Hash : string) : pointer; { public }
var
  i : integer;
begin
  if FNoCase then Hash := Uppercase(Hash);
  i := GetIndex(Hash);
  if i = -1 then
  begin
    if RaiseOnAbsence then
      raise EElHashListError.Create('Hash is absent.')
    else
      result := nil;
  end
  else
    result := FList[i]^.ItemData;
end; { GetItem }
{$WARNINGS off}

function TElHashList.GetIndex(Hash : string) : integer; { public }
var
(*
  MD5 : TCrMD5;
*)
{$IFDEF Ver120}
  arr : array[1..4] of LongWord;
{$ELSE}
  arr : array[1..4] of integer;
{$ENDIF}
  i : integer;
  h : integer;
begin
  if NoCase then Hash := Uppercase(Hash);
  case FHashType of
    ehtQuick : h := CalcQuickHash(Hash);
    ehtCRC32,
    ehtMD5 : h := CrcStr(Hash);
    (*
    ehtMD5 :
      begin
        MD5 := TCrMD5.Create;
        MD5.InputType := SourceString;
        MD5.InputString := Hash;
        MD5.pOutputArray := @arr;
        MD5.MD5_Hash;
        MD5.Free;
      end;
    *)
  end;
  i := 0;
  if (FQuickHash) or (FHashType <> ehtMD5) then
    while (i < FCount) and (integer(FList[i]^.Hash) <> h) do
      inc(i)
  else
    while (i < FCount) and (not CompareMem(@Arr, FList[i]^.Hash, SizeOf(Integer) * 4)) do
      inc(i);
  if i = FCount then
  begin
    if RaiseOnAbsence then
      raise EElHashListError.Create('Hash is absent.')
    else
      result := -1;
  end
  else
    result := i;
end; { GetIndex }
{$WARNINGS on}

procedure TElHashList.SetCapacity(NewCapacity : Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxHashListSize) then
    raise EElHashListError.Create('Invalid ElHashList capacity.');
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TElHashList.Grow;
var
  Delta : Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TElHashList.Clear; { public }
var
  J : Integer;
begin
  for j := 0 to FCount - 1 do // Iterate
  begin
    if assigned(FOnDelete) then FOnDelete(Self, FList[j]^.ItemData);
    if AutoClearObjects and (FList[j]^.ItemData <> nil) then TObject(FList[j]^.ItemData).Free;
    (*
    if FHashType = ehtMD5 then Dispose(FList[j]^.Hash);
    *)
    Dispose(FList[j]);
  end;
  FCount := 0;
  SetCapacity(0);
end; { Clear }

function TElHashList.GetByIndex(Index : integer) : Pointer; { public }
begin
  if (Index < 0) or (Index >= FCount) then
    raise EElHashListError.Create('Invalid index.')
  else
    result := FList[Index]^.ItemData;
end; { GetByIndex }

procedure TElHashList.SetHashType(newValue : TElHashType);
begin
  if FHashType <> newValue then
  begin
    Clear;
    FHashType := newValue;
    FQuickHash := FHashType = ehtQuick;
  end;
end;

procedure TElHashList.SetQuickHash(newValue : Boolean);
begin
  if (FQuickHash <> newValue) then
  begin
    Clear;
    FQuickHash := newValue;
    FHashType := ehtQuick;
  end; { if }
end; { SetQuickHash }

function TElHashList.CalcQuickHash(Hash : string) : Integer; { protected }
var
  i : integer;
begin
  result := 0;
  for i := 1 to Length(Hash) do
    result := result * 5 + byte(Hash[i]);
end; { CalcQuickHash }

procedure TElHashList.SetAutoClearObjects(newValue : Boolean);
{ Sets data member FAutoClearObjects to newValue. }
begin
  if (FAutoClearObjects <> newValue) then
  begin
    FAutoClearObjects := newValue;
  end; { if }
end; { SetAutoClearObjects }

destructor TElHashList.Destroy;
begin
  Clear;
  FreeMem(FList, FCapacity);
  inherited Destroy;
end; { Destroy }

constructor TElHashList.Create;
begin
  inherited;
  FRaiseOnAbsence := False;
  FInsertDupesMode := himInsert;
end; { Create }

end.
