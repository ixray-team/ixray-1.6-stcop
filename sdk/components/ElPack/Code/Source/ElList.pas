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

unit ElList;

interface

uses ElTools, ElContBase, Classes;

const AlignMem    =   $500; // Align at 1280 items

type
  TElListSortCompare = function(Item1,
                                Item2: Pointer;
                                Cargo: Pointer): Integer;

  TElListSortCompareEx = function(Item1,
                                  Item2: Pointer;
                                  Cargo: Pointer): Integer of object;


  TElListDeleteEvent = procedure (Sender: TObject; Item: Pointer) of object;

  {:
  }
  TElList = class (TPersistent)
  protected
    FAutoClearObjects: Boolean;
    FCapacity: Integer;
    FCount: Integer;
    FList: PPointerList;
    FOnDelete: TElListDeleteEvent;
    class procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): Pointer; virtual;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure IntDelete(Index: Integer);
    procedure TriggerDeleteEvent(Item: Pointer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function FastGet(Index: Integer): Pointer;
    function Add(Item: Pointer): Integer;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure DeleteRange(StartIndex, EndIndex: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TElList;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    function IndexOfBack(StartIndex: integer; Item: Pointer): Integer;
    function IndexOfFrom(StartIndex: integer; Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure MoveRange(CurStart, CurEnd, NewStart: integer);
    procedure Pack;
    function Remove(Item: Pointer): Integer;
    procedure Sort(Compare: TElListSortCompare; Cargo: Pointer);
    procedure SortC(Compare: TElListSortCompareEx; Cargo: Pointer);
    property AutoClearObjects: Boolean read FAutoClearObjects write
        FAutoClearObjects;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
    property OnDelete: TElListDeleteEvent read FOnDelete write FOnDelete;
  end;

implementation

uses SysUtils;

type
  {:
  }
  EElListError = class (Exception)
  end;

//T & R
{$ifdef D_3_UP}
resourcestring
{$else}
const
{$endif}
  rs_ListIndexOutOfBounds = 'List index [%d] out of bounds...';

procedure RaiseOutOfBoundsError(Ind: integer);
begin
  raise EelListError.CreateFmt(rs_ListIndexOutOfBounds, [Ind]);
// raise EListError.Create('List index out of bounds.');
end;

procedure QuickSortC(SortList: PPointerList; L, R: Integer;
  SCompare: TelListSortCompareEx; Cargo: Pointer);
var
  I, J : Integer;
  P, T : Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P, Cargo) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P, Cargo) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := SortList^[I];
          SortList^[I] := SortList^[J];
          SortList^[J] := T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortC(SortList, L, J, SCompare, Cargo);
    L := I;
  until I >= R;
end;

procedure QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TelListSortCompare; Cargo: Pointer);
var
  I, J : Integer;
  P, T : Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];

    repeat

      // rI := SCompare(SortList^[I], P, Cargo);
      while SCompare(SortList^[I], P, Cargo) < 0 do
        Inc(I);

      // rJ := SCompare(SortList^[J], P, Cargo);
      while SCompare(SortList^[J], P, Cargo) > 0 do
        Dec(J);

      if I <= J then
      begin

        if I <> J then
        begin
          T := SortList^[I];
          SortList^[I] := SortList^[J];
          SortList^[J] := T;
        end;

        Inc(I);
        Dec(J);
      end;
    until I > J;

    if L < J then QuickSort(SortList, L, J, SCompare, Cargo);

    L := I;
  until I >= R;
end;

{:
}
{:
}
{
*********************************** TElList ************************************
}
constructor TElList.Create;
begin
  inherited;
  FList := nil;
  FCount := 0;
  FCapacity := 0;
  FAutoClearObjects := FALSE;
  FOnDelete := nil;
end;

destructor TElList.Destroy;
begin
  Clear;
  inherited;
end;

function TElList.Add(Item: Pointer): Integer;
begin
  If FCount = FCapacity Then
  Begin
    Inc(FCapacity, Min(Count * 2 + 1, AlignMem));
    ReAllocMem(FList, FCapacity * SizeOf(Pointer));
  End;
  FList^[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure TElList.Assign(Source : TPersistent);
begin
  if Source is TElList then
  begin
    Clear;
    SetCapacity(TElList(Source).Capacity);
    SetCount(TElList(Source).Count);
    if FCount > 0 then
       System.Move(TElList(Source).FList^[0], FList^[0], FCount * sizeof(pointer));
  end else inherited;
end;

procedure TElList.Clear;
var
  I: Integer;
  P: Pointer;
begin
  For I := 0 to Count - 1 do
     TriggerDeleteEvent(FList^[I]);
  If AutoClearObjects then
    For I := 0 to Count - 1 do
    begin
      p := Get(i);
      try
        if (P <> nil) and (TObject(P) is TObject)
            then TObject(P).Free;
      except
      end;
    end;
  // Don't call two routines for this. Just assign
  FCount := 0;
  FCapacity := 0;
  ReallocMem(FList, 0);
end;

procedure TElList.IntDelete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount)
    then RaiseOutOfBoundsError(Index);
  Dec(FCount);
  if FCount > Index then
    System.Move(FList^[Index + 1],
                FList^[Index],
                (FCount - Index) * SizeOf(Pointer));
end;

procedure TElList.DeleteRange(StartIndex, EndIndex: Integer); 
var i : integer;
begin
  if ((StartIndex < 0) or (StartIndex >= FCount)) then
     RaiseOutOfBoundsError(StartIndex);
  if ((EndIndex < 0) or (EndIndex >= FCount)) or
     (EndIndex < StartIndex) then
     RaiseOutOfBoundsError(EndIndex);
  for i := StartIndex to EndIndex do 
    TriggerDeleteEvent(FList^[I]);
  if (FCount > EndIndex + 1) then
  begin
    System.Move(FList^[EndIndex + 1],
                FList^[StartIndex],
                (FCount - (EndIndex - StartIndex + 1)) * SizeOf(Pointer));    
  end;
  Dec(FCount, EndIndex - StartIndex + 1);
  If FCount < FCapacity shr 1 Then
  Begin
    FCapacity := FCapacity shr 1;
    ReAllocMem(FList, FCapacity * SizeOf(Pointer));
  End;
end;

procedure TElList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount)
    then RaiseOutOfBoundsError(Index);
  TriggerDeleteEvent(FList^[Index]);
  if AutoClearObjects then TObject(FList^[Index]).Free;

  Dec(FCount);
  if FCount > Index then
    System.Move(FList^[Index + 1],
                FList^[Index],
                (FCount - Index) * SizeOf(Pointer));
  If FCount < FCapacity shr 1 Then
  Begin
    FCapacity := FCapacity shr 1;
    ReAllocMem(FList, FCapacity * SizeOf(Pointer));
  End;
end;

class procedure TElList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
    MOV     EAX,[EBP+4]
  end;
  
begin
  raise EElListError.CreateFmt(Msg, [Data])at ReturnAddr;
end;

procedure TElList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    RaiseOutOfBoundsError(Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    RaiseOutOfBoundsError(Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TElList.Expand: TElList;
begin
  if FCount = FCapacity then
  Begin
    Inc(FCapacity, Min(Count * 2 + 1, AlignMem));
    ReallocMem(FList, FCapacity * SizeOf(Pointer));
  End;
  Result := Self;
end;

function TElList.First: Pointer;
begin
  Result := FList^[0];
end;

function TElList.FastGet(Index: Integer): Pointer;
begin
  Result := FList^[Index];
end;

function TElList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then RaiseOutOfBoundsError(Index);
  Result := FList^[Index];
end;

procedure TElList.Grow;
begin
  Inc(FCapacity, Min(Count * 2 + 1, AlignMem));
  ReAllocMem(FList, FCapacity * SizeOf(Pointer));
end;

function TElList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

function TElList.IndexOfBack(StartIndex: integer; Item: Pointer): Integer;
begin
  if (StartIndex < 0) or (StartIndex >= FCount) then RaiseOutOfBoundsError(
      StartIndex);
  Result := StartIndex;
  while (Result >= 0) and (FList^[Result] <> Item) do
    dec(Result);
end;

function TElList.IndexOfFrom(StartIndex: integer; Item: Pointer): Integer;
begin
  if (StartIndex < 0) or (StartIndex >= FCount) then
    RaiseOutOfBoundsError(StartIndex);
  Result := StartIndex;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TElList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    RaiseOutOfBoundsError(Index);
  if FCount = FCapacity then
  Begin
    Inc(FCapacity, Min(Count * 2 + 1, AlignMem));
    ReAllocMem(FList, FCapacity * SizeOf(Pointer));
  End;
  // if Index < FCount then == Useless. See first line.
  System.Move(FList^[Index],
              FList^[Index + 1],
              (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TElList.Last: Pointer;
begin
  if FCount = 0 then
    result := nil
  else
    Result := FList^[Pred(FCount)];
end;

procedure TElList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      RaiseOutOfBoundsError(NewIndex);
    Item := FList^[CurIndex];
    IntDelete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

procedure TElList.MoveRange(CurStart, CurEnd, NewStart: integer);
var
  bs: Integer;
  P: PChar;
begin
  if CurStart <> NewStart then
  begin
    if (NewStart < 0) or (NewStart >= FCount) or
      ((NewStart >= CurStart) and (NewStart <= CurEnd)) then
      RaiseOutOfBoundsError(NewStart);
    if (CurStart < 0) or (CurStart >= FCount) then
      RaiseOutOfBoundsError(CurStart);
    if (CurEnd < 0) or (CurEnd >= FCount) then
      RaiseOutOfBoundsError(CurEnd);
    if CurStart > NewStart then
    begin
      bs := CurEnd - CurStart + 1;
      GetMem(P, bs * SizeOf(Pointer));
      System.Move(FList^[CurStart], P^, BS * SizeOf(Pointer));
      System.Move(FList^[NewStart], FList^[NewStart + BS], (CurStart - 
          NewStart) * SizeOf(Pointer));
      System.Move(P^, FList^[NewStart], BS * SizeOf(Pointer));
      FreeMem(P);
    end else
    begin
      bs := CurEnd - CurStart + 1;
      GetMem(P, BS * SizeOf(Pointer));
      System.Move(FList^[CurStart], P^, BS * SizeOf(Pointer));
      System.Move(FList^[CurEnd + 1], FList^[CurStart], (NewStart - CurEnd) * 
          SizeOf(Pointer));
      NewStart := CurStart - 1 + NewStart - CurEnd;
      System.Move(P^, FList^[NewStart], BS * SizeOf(Pointer));
      FreeMem(P);
    end;
  end;
end;

procedure TElList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then Delete(I);
end;

procedure TElList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseOutOfBoundsError(Index);
  if (FList[Index] <> nil) And Assigned(FOnDelete) then
   FOnDelete(Self, FList^[Index]);
  FList^[Index] := Item;
end;

function TElList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  // changed by chmv. if Result <> -1 then
  if Result >= 0 then
  Begin
    TriggerDeleteEvent(FList^[Result]);
    Dec(FCount);
    // if Index < FCount then == Useless. See above.
    System.Move(FList^[Result + 1],
                FList^[Result],
                (FCount - Result) * SizeOf(Pointer));
    If FCount < FCapacity shr 1 Then
    Begin
      FCapacity := FCapacity shr 1;
      ReAllocMem(FList, FCapacity * SizeOf(Pointer));
    End;
  End;
end;

procedure TElList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    RaiseOutOfBoundsError(NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TElList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    RaiseOutOfBoundsError(NewCount);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0);
  FCount := NewCount;
end;

procedure TElList.Sort(Compare: TElListSortCompare; Cargo: Pointer);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1, Compare, Cargo);
end;

procedure TElList.SortC(Compare: TElListSortCompareEx; Cargo: Pointer);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSortC(FList, 0, Count - 1, Compare, Cargo);
end;

procedure TElList.TriggerDeleteEvent(Item: Pointer);
  
  { Triggers the OnDelete event. This is a virtual method (descendants of this 
      component can override it). }
  
begin
  if (assigned(FOnDelete)) then
    FOnDelete(Self, Item);
end;


end.
