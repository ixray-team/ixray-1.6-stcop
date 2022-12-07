{$define DEBUG}
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2001, EldoS                   }
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

03/22/2002 (c) Alexander V. Hramov

  Add TElStack.ShiftUp method for shift content of stack up.

*)

unit ElStack;

interface

uses Classes, SysUtils;

type
  EElStackError = class(exception)
  end;

type
  TElStack = class(TObject)
  private
    FList : PPointerList;
    FCount : Integer;
    FCapacity : Integer;
    function Get(index : integer) : pointer;
    procedure Put(index : integer; value : pointer);
    procedure Grow;
    procedure SetCapacity(NewCapacity : Integer);
  protected
    function GetLast: Pointer;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure ShiftUp(ACount: integer);
    procedure Push(value : pointer);
    function Pop : pointer;
    function Empty : boolean;
    property Capacity : integer read FCapacity write SetCapacity;
    property Items[Index : Integer] : Pointer read Get write Put; default;
    property Count : integer read FCount;
    property Last: Pointer read GetLast;
  end;

implementation

function TElStack.Empty : boolean;
begin
  result := Count = 0;
end;

function TElStack.Get(index : integer) : pointer;
begin
  result := FList^[index];
end;

procedure TElStack.Put(index : integer; value : pointer);
begin
  FList^[index] := value;
end;

destructor TElStack.Destroy;
begin
  FreeMem(FList, FCapacity);
  inherited Destroy;
end;

procedure TElStack.Grow;
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

procedure TElStack.SetCapacity(NewCapacity : Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    raise EElStackError.Create('Invalid ElStack capacity.');
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TElStack.Push;
begin
  if FCount = FCapacity then Grow;
  FList^[FCount] := value;
  Inc(FCount);
end;

function TElStack.Pop;
begin
  if FCount = 0 then raise EElStackError.Create('ElStack is empty.');
  result := FList^[FCount - 1];
  dec(FCount);
  if FCount < (FCapacity div 2) then SetCapacity(FCapacity div 2);
end;

procedure TElStack.Clear;
begin
  FCount := 0;
  SetCapacity(0);
end;

function TElStack.GetLast: Pointer;
begin
  if FCount = 0 then raise EElStackError.Create('ElStack is empty.');
  Result := FList^[FCount - 1];
end;

procedure TElStack.ShiftUp(ACount: integer);
begin
  if ACount > FCount then raise EElStackError.Create('Cannot shift ElStack');
  if FCount = 0 then raise EElStackError.Create('ElStack is empty.');
  Move(FList^[ACount], FList^, (FCount - ACount) * SizeOf(Pointer));
  FCount := FCount - ACount;
  SetCapacity(FCount);
end;

end.
