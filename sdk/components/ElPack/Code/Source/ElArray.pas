
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

03/10/2001

  Fixed possible memory leaks that could happen when item is deleted.

  Minor optimization on deletion.

*)

unit ElArray;

interface

uses ElContBase;

type
  TElArraySortCompare = function(Item1, Item2: Pointer; Cargo: Pointer): Integer;

  TElArrayDeleteEvent = procedure(Sender: TObject; Item: Pointer) of object;

  TElArray = class
  protected
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
    FAutoClearObjects: Boolean;
    FOnDelete: TElArrayDeleteEvent;
    function Get(Index: Integer): Pointer; virtual;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure TriggerDeleteEvent(Item: Pointer); virtual;
    class procedure Error(const Msg: string; Data: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear;
    procedure Assign(AList: TElArray);
    procedure Delete(Index: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TElArray;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    function IndexOfFrom(StartIndex: integer; Item: Pointer): Integer;
    function IndexOfBack(StartIndex: integer; Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure MoveRange(CurStart, CurEnd, NewStart: integer);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TElArraySortCompare; Cargo: Pointer);
    property Capacity: Integer read FCapacity write SetCapacity default 0;
    property Count: Integer read FCount write SetCount default 0;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
    property AutoClearObjects: Boolean read FAutoClearObjects write FAutoClearObjects default False; { Published }
    property OnDelete: TElArrayDeleteEvent read FOnDelete write FOnDelete;
  end;

implementation

uses SysUtils
     {$ifndef KYLIX_USED}
     , Windows
     {$else}
     , Libc
     {$endif}
     ;
type
  EElArrayError = class(Exception);
//T & R
resourcestring
  rs_ListIndexOutOfBounds = 'List index [%d] out of bounds...';

procedure RaiseOutOfBoundsError(Ind: integer);
begin
  raise EElArrayError.CreateFmt(rs_ListIndexOutOfBounds, [Ind]);
// raise EListError.Create('List index out of bounds.');
end;

class procedure TElArray.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
    MOV     EAX,[EBP+4]
  end;

begin
  raise EElArrayError.CreateFmt(Msg, [Data])at ReturnAddr;
end;

constructor TElArray.Create;
begin
  inherited;
  FList := nil;
  FCount := 0;
  FCapacity := 0;
  FAutoClearObjects := FALSE;
  FOnDelete := nil;
end;

destructor TElArray.Destroy;
begin
  Clear;
  inherited;
end;

function TElArray.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

procedure TElArray.Assign(AList: TElArray);
begin
  Clear;
  SetCapacity(AList.Capacity);
  SetCount(AList.Count);
  System.Move(AList.FList[0], FList[0], FCount);
end;

procedure TElArray.Clear;
var
  I: integer;
  p: pointer;
begin
  if Assigned(FOnDelete) then
    for i := 0 to Count - 1 do
      if FList[i] <> nil then TriggerDeleteEvent(FList[i]);
  if AutoClearObjects then
    for i := 0 to Count - 1 do
    begin
      p := Get(i);
      try
        if (P <> nil) and (TObject(P) is TObject) then TObject(P).Free;
      except
      end;
    end;
  SetCount(0);
  SetCapacity(0);
end;

procedure TElArray.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then RaiseOutOfBoundsError(Index);
  TriggerDeleteEvent(FList[Index]);
  if AutoClearObjects then TObject(FList[Index]).Free;
  Dec(FCount);
  if Index < FCount then System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(Pointer));
  if FCount < (FCapacity div 2) then SetCapacity(FCapacity div 2);
end;

procedure TElArray.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) then RaiseOutOfBoundsError(Index1);
  if (Index2 < 0) then RaiseOutOfBoundsError(Index2);
  if (Index1 >= FCount) then Count := Index1 + 1;
  if (Index2 >= FCount) then Count := Index2 + 1;
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

function TElArray.Expand: TElArray;
begin
  if FCount = FCapacity then Grow;
  Result := Self;
end;

function TElArray.First: Pointer;
begin
  Result := Get(0);
end;

function TElArray.Get(Index: Integer): Pointer;
begin
  if (Index < 0) then RaiseOutOfBoundsError(Index);
  if (Index >= FCount) then result := nil else Result := FList[Index];
end;

procedure TElArray.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TElArray.IndexOfFrom(StartIndex: integer; Item: Pointer): Integer;
begin
  if (StartIndex < 0) then RaiseOutOfBoundsError(StartIndex);
  if (StartIndex >= FCount) then result := -1 else
  begin
    Result := StartIndex;
    while (Result < FCount) and (FList[Result] <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end;
end;

function TElArray.IndexOfBack(StartIndex: integer; Item: Pointer): Integer;
begin
  if (StartIndex < 0) then RaiseOutOfBoundsError(StartIndex);
  if (StartIndex >= FCount) then result := FCount - 1 else Result := StartIndex;
  while (Result >= 0) and (FList[Result] <> Item) do dec(Result);
end;

function TElArray.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList[Result] <> Item) do Inc(Result);
  if Result = FCount then Result := -1;
end;

procedure TElArray.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then RaiseOutOfBoundsError(Index);
  if FCount = FCapacity then Grow;
  if Index < FCount then System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(Pointer));
  FList[Index] := Item;
  Inc(FCount);
end;

function TElArray.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TElArray.MoveRange(CurStart, CurEnd, NewStart: integer);
var
  bs: integer;
  P: PChar;
begin
  if CurStart <> NewStart then
  begin
    if (NewStart < 0) or (NewStart >= FCount) or
      ((NewStart >= CurStart) and (NewStart <= CurEnd)) then RaiseOutOfBoundsError(NewStart);
    if (CurStart < 0) or (CurStart >= FCount) then RaiseOutOfBoundsError(CurStart);
    if (CurEnd < 0) or (CurEnd >= FCount) then RaiseOutOfBoundsError(CurEnd);
    if CurStart > NewStart then
    begin
      bs := CurEnd - CurStart + 1;
      GetMem(P, bs * SizeOf(Pointer));
      System.Move(FList[CurStart], P^, BS * SizeOf(Pointer));
      System.Move(FList[NewStart], FList[NewStart + BS], (CurStart - NewStart) * SizeOf(Pointer));
      System.Move(P^, FList[NewStart], BS * SizeOf(Pointer));
      FreeMem(P);
    end else
    begin
      bs := CurEnd - CurStart + 1;
      GetMem(P, BS * SizeOf(Pointer));
      System.Move(FList[CurStart], P^, BS * SizeOf(Pointer));
      System.Move(FList[CurEnd + 1], FList[CurStart], (NewStart - CurEnd) * SizeOf(Pointer));
      NewStart := CurStart - 1 + NewStart - CurEnd;
      System.Move(P^, FList[NewStart], BS * SizeOf(Pointer));
      FreeMem(P);
    end;
  end;
end;


procedure TElArray.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) then RaiseOutOfBoundsError(NewIndex);
    if (NewIndex >= FCount) then Count := NewIndex + 1;
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

procedure TElArray.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) then RaiseOutOfBoundsError(Index);
  if (Index >= FCount) then Count := Index + 1;
  if FList[Index] <> Item then
  begin
    TriggerDeleteEvent(FList[Index]);
    if AutoClearObjects then 
      try 
        TObject(FList[Index]).Free; 
      except 
      end;
    FList[Index] := Item;
  end;
end;

function TElArray.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end;

procedure TElArray.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then Delete(I);
end;

procedure TElArray.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    RaiseOutOfBoundsError(NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
{$ifndef KYLIX_USED}       
    FillMemory(@FList[FCapacity], NewCapacity - FCapacity, 0);
{$else}
    memset(@FList[FCapacity], 0, NewCapacity - FCapacity);
{$endif}
    FCapacity := NewCapacity;
  end;
end;

procedure TElArray.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    RaiseOutOfBoundsError(NewCount);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0);
  FCount := NewCount;
end;

procedure QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TElArraySortCompare; Cargo: Pointer);
var
  I, J, rI, rJ: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];

    repeat
      rI := SCompare(SortList^[I], P, Cargo);
      rJ := SCompare(SortList^[J], P, Cargo);

      while rI < 0 do
      begin
        Inc(I);
        rI := SCompare(SortList^[I], P, Cargo);
      end;

      while rJ > 0 do
      begin
        Dec(J);
        rJ := SCompare(SortList^[J], P, Cargo);
      end;

      if I <= J then
      begin
        if (I <> J) and ((rI <> 0) or (rJ <> 0)) then
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

procedure TElArray.Sort(Compare: TElArraySortCompare; Cargo: Pointer);
begin
  if (FList <> nil) and (Count > 0) then QuickSort(FList, 0, Count - 1, Compare, Cargo);
end;

procedure TElArray.TriggerDeleteEvent(Item: Pointer);
begin
  if (assigned(FOnDelete)) then FOnDelete(Self, Item);
end; { TriggerDeleteEvent }

end.
