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

{$POINTERMATH ON}

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
    FList: TList;
    FOnDelete: TElListDeleteEvent;

    class procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): Pointer; virtual;
    procedure Put(Index: Integer; Item: Pointer); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function GetCapacity(): Integer;
    function GetCount(): Integer;
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
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: TList read FList;
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
  FList := TList.Create;
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
  Result := FList.Add(Item);
end;

procedure TElList.Assign(Source : TPersistent);
begin
  if Source is TElList then
    FList.Assign(TElList(Source).FList)
  else
    inherited;
end;

procedure TElList.Clear;
begin
    FList.Clear;
end;

procedure TElList.DeleteRange(StartIndex, EndIndex: Integer); 
var i : integer;
begin
  if ((StartIndex < 0) or (StartIndex >= FList.Count)) then
     RaiseOutOfBoundsError(StartIndex);
  if ((EndIndex < 0) or (EndIndex >= FList.Count)) or
     (EndIndex < StartIndex) then
     RaiseOutOfBoundsError(EndIndex);
  for i := StartIndex to EndIndex do
  begin
    TriggerDeleteEvent(FList[i]);
    FList.Delete(i);
  end;
end;

procedure TElList.Delete(Index: Integer);
begin
  FList.Delete(Index);
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
begin
  FList.Exchange(index1, Index2);
end;

function TElList.First: Pointer;
begin
  Result := FList.First;
end;

function TElList.FastGet(Index: Integer): Pointer;
begin
  Result := FList[Index];
end;

function TElList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FList.Count) then RaiseOutOfBoundsError(Index);
  Result := FList[Index];
end;

function TElList.IndexOf(Item: Pointer): Integer;
begin
  Result := FList.IndexOf(item);
end;

function TElList.IndexOfBack(StartIndex: integer; Item: Pointer): Integer;
begin
  if (StartIndex < 0) or (StartIndex >= FList.Count) then RaiseOutOfBoundsError(
      StartIndex);
  Result := StartIndex;
  while (Result >= 0) and (FList[Result] <> Item) do
    dec(Result);
end;

function TElList.IndexOfFrom(StartIndex: integer; Item: Pointer): Integer;
begin
  if (StartIndex < 0) or (StartIndex >= FList.Count) then
    RaiseOutOfBoundsError(StartIndex);
  Result := StartIndex;
  while (Result < FList.Count) and (FList[Result] <> Item) do
    Inc(Result);
  if Result = FList.Count then
    Result := -1;
end;

procedure TElList.Insert(Index: Integer; Item: Pointer);
begin
  FList.Insert(Index, Item);
end;

function TElList.Last: Pointer;
begin
  Result := FList.Last;
end;

procedure TElList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TElList.MoveRange(CurStart, CurEnd, NewStart: integer);
var
  bs: Integer;
  P: PChar;
begin
  if CurStart <> NewStart then
  begin
    if (NewStart < 0) or (NewStart >= FList.Count) or
      ((NewStart >= CurStart) and (NewStart <= CurEnd)) then
      RaiseOutOfBoundsError(NewStart);
    if (CurStart < 0) or (CurStart >= FList.Count) then
      RaiseOutOfBoundsError(CurStart);
    if (CurEnd < 0) or (CurEnd >= FList.Count) then
      RaiseOutOfBoundsError(CurEnd);
    if CurStart > NewStart then
    begin
      bs := CurEnd - CurStart + 1;
      GetMem(P, bs * SizeOf(Pointer));
      System.Move(FList[CurStart]^, P^, BS * SizeOf(Pointer));
      System.Move(FList[NewStart]^, FList[NewStart + BS]^, (CurStart -
          NewStart) * SizeOf(Pointer));
      System.Move(P^, FList[NewStart]^, BS * SizeOf(Pointer));
      FreeMem(P);
    end else
    begin
      bs := CurEnd - CurStart + 1;
      GetMem(P, BS * SizeOf(Pointer));
      System.Move(FList[CurStart]^, P^, BS * SizeOf(Pointer));
      System.Move(FList[CurEnd + 1]^, FList[CurStart]^, (NewStart - CurEnd) *
          SizeOf(Pointer));
      NewStart := CurStart - 1 + NewStart - CurEnd;
      System.Move(P^, FList[NewStart]^, BS * SizeOf(Pointer));
      FreeMem(P);
    end;
  end;
end;

procedure TElList.Pack;
begin
  FList.Pack;
end;

procedure TElList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FList.Count) then
    RaiseOutOfBoundsError(Index);
  if (FList[Index] <> nil) And Assigned(FOnDelete) then
   FOnDelete(Self, FList[Index]);
  FList[Index] := Item;
end;

function TElList.Remove(Item: Pointer): Integer;
begin
  Result := FList.Remove(Item);
end;

procedure TElList.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

procedure TElList.SetCount(NewCount: Integer);
begin
  FList.Capacity := NewCount;
end;

procedure TElList.Sort(Compare: TElListSortCompare; Cargo: Pointer);
begin
  if (FList <> nil) and (FList.Count > 0) then
    QuickSort(FList.Items[0], 0, FList.Count - 1, Compare, Cargo);
end;

procedure TElList.SortC(Compare: TElListSortCompareEx; Cargo: Pointer);
begin
  if (FList <> nil) and (FList.Count > 0) then
    QuickSortC(FList.Items[0], 0, FList.Count - 1, Compare, Cargo);
end;

procedure TElList.TriggerDeleteEvent(Item: Pointer);
  
  { Triggers the OnDelete event. This is a virtual method (descendants of this 
      component can override it). }
  
begin
  if (assigned(FOnDelete)) then
    FOnDelete(Self, Item);
end;

function TElList.GetCapacity(): Integer;
begin
  Result := FList.Capacity;
end;

function TElList.GetCount(): Integer;
begin
  Result := FList.Count;
end;

end.
