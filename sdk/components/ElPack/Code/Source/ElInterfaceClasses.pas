{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{   ElInterfaceClasses unit:                         }
{   Copyright (c) 2001 Akzhan Abdulin                }
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

unit ElInterfaceClasses;

interface

uses
  Classes;

type
  { IElObjectIdentity enables identity tests }

  IElObjectIdentity = interface
  ['{0C5A6289-FFE6-4DE3-B9D2-4E0B22D1A9E4}']
    function SameIdentity(ARef: IUnknown): Boolean;
  end;

  { IElObjectIdentity2 internally serves to test identity and meets ITC, IPC }

  IElObjectIdentity2 = interface
  ['{1847D467-CF11-4370-B5FF-762951CA0FA5}']
    function GetIdentity: Pointer;
  end;

  TElCustomIdentifiedObject = class(TInterfacedObject, IElObjectIdentity, IElObjectIdentity2)
  private
    FIdentity: Pointer;
  protected
    { IElObjectIdentity }
    function SameIdentity(ARef: IUnknown): Boolean;
    { IElObjectIdentity2 }
    function GetIdentity: Pointer;
  public
    procedure AfterConstruction; override;
  end;

  IElComparableObject = interface(IElObjectIdentity)
  ['{9D9174AC-5243-433C-A54C-6349E5D5778F}']
    function Compare(ARef: IUnknown): Integer;
  end;
  { IElInterfaceList provides additional FastGet method  }

  IElInterfaceList = interface(IInterfaceList)
  ['{BB275413-8170-4DDF-8843-B0288F5DDD3F}']
    function FastGet(Index: Integer): IUnknown;
    function Get(Index: Integer): IUnknown;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure Put(Index: Integer; Item: IUnknown);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);

    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: IUnknown;
    function IndexOf(Item: IUnknown): Integer;
    function Add(Item: IUnknown): Integer;
    procedure Insert(Index: Integer; Item: IUnknown);
    function Last: IUnknown;
    function Remove(Item: IUnknown): Integer;
    procedure Lock;
    procedure Unlock;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: IUnknown read Get write Put; default;
  end;

  { TElInterfaceList accepts duplicates  }

  TElInterfaceList = class(TInterfacedObject, IElInterfaceList, IInterfaceList)
  private
    FList: TThreadList;
  protected
    { IElInterfaceList }
    function FastGet(Index: Integer): IUnknown;
    function Get(Index: Integer): IUnknown;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure Put(Index: Integer; Item: IUnknown);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TElInterfaceList;
    function First: IUnknown;
    function IndexOf(Item: IUnknown): Integer;
    function Add(Item: IUnknown): Integer;
    procedure Insert(Index: Integer; Item: IUnknown);
    function Last: IUnknown;
    function Remove(Item: IUnknown): Integer;
    procedure Lock;
    procedure Unlock;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: IUnknown read Get write Put; default;
  end;

  IElInterfaceStack = interface
  ['{26DF724D-0250-4B7F-9E26-13C664F20D20}']
    procedure Push(Value: IUnknown);
    function GetFront: IUnknown;
    function Pop: IUnknown;
    procedure Clear;
    function Empty: Boolean;
  end;

  TElInterfaceStack = class(TInterfacedObject, IElInterfaceStack)
  private
    FList: IInterfaceList;
  protected
    { IElInterfaceStack }
    procedure Push(Value: IUnknown);
    function GetFront: IUnknown;
    function Pop: IUnknown;
    procedure Clear;
    function Empty: Boolean;
  public
    procedure AfterConstruction; override;
  end;

  IElAttributeList = interface
  ['{47F41D19-EE3C-4E0A-9F06-3CEF29F64000}']
    function GetText: String;
    procedure SetText(const Text: String);
    function GetName(const Index: Integer): String;
    function GetValue(const Index: Integer): String;
    procedure SetValue(const Index: Integer; const AValue: String);
    function GetCount: Integer;
    function GetAttribute(const AName: String): String;
    procedure SetAttribute(const AName: String; const AValue: String; Additive: Boolean);
    procedure _SetAttribute(const AName: String; const AValue: String);
    procedure RemoveAttribute(const AName: String);
    procedure Clear;

    property Text: String read GetText write SetText;
    property Names[const Index: Integer]: String read GetName;
    property Values[const Index: Integer]: String read GetValue write SetValue;
    property Count: Integer read GetCount;
    property Attributes[const Name: String]: String read GetAttribute write _SetAttribute;
  end;

  TElAttributeList = class(TInterfacedObject, IElAttributeList)
  private
    FNames: TStrings;
    FValues: TStrings;
  protected
    { IElAttributeList }
    function GetText: String;
    procedure SetText(const Text: String);
    function GetName(const Index: Integer): String;
    function GetValue(const Index: Integer): String;
    procedure SetValue(const Index: Integer; const AValue: String);
    function GetCount: Integer;
    function GetAttribute(const AName: String): String;
    procedure SetAttribute(const AName: String; const AValue: String; Additive: Boolean);
    procedure _SetAttribute(const AName: String; const AValue: String);
    procedure RemoveAttribute(const AName: String);
    procedure Clear;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, Consts;
{ TElCustomIdentifiedObject }

procedure TElCustomIdentifiedObject.AfterConstruction;
begin
  inherited;
  FIdentity := Self;
end;

function TElCustomIdentifiedObject.GetIdentity: Pointer;
begin
  Result := FIdentity;
end;

function TElCustomIdentifiedObject.SameIdentity(
  ARef: IUnknown): Boolean;
var
  Id: IElObjectIdentity2;
begin
  Result := Supports(ARef, IElObjectIdentity2, Id) and (FIdentity = Id.GetIdentity);
end;

{ TElInterfaceList }

constructor TElInterfaceList.Create;
begin
  inherited Create;
  FList := TThreadList.Create;
  FList.Duplicates := dupAccept;
end;

destructor TElInterfaceList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TElInterfaceList.Clear;
var
  I: Integer;
begin
  if FList <> nil then
  begin
    with FList.LockList do
    try
      for I := 0 to Count - 1 do
        IUnknown(List[I]) := nil;
      Clear;
    finally
      Self.FList.UnlockList;
    end;
  end;
end;

procedure TElInterfaceList.Delete(Index: Integer);
begin
  with FList.LockList do
  try
    Self.Put(Index, nil);
    Delete(Index);
  finally
    Self.FList.UnlockList;
  end;
end;

function TElInterfaceList.Expand: TElInterfaceList;
begin
  with FList.LockList do
  try
    Expand;
    Result := Self;
  finally
    Self.FList.Unlocklist;
  end;
end;

function TElInterfaceList.First: IUnknown;
begin
  Result := Get(0);
end;

function TElInterfaceList.Get(Index: Integer): IUnknown;
begin
  with FList.LockList do
  try
    if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
    Result := IUnknown(List[Index]);
  finally
    Self.FList.UnlockList;
  end;
end;

function TElInterfaceList.GetCapacity: Integer;
begin
  with FList.LockList do
  try
    Result := Capacity;
  finally
    Self.FList.UnlockList;
  end;
end;

function TElInterfaceList.GetCount: Integer;
begin
  with FList.LockList do
  try
    Result := Count;
  finally
    Self.FList.UnlockList;
  end;
end;

function TElInterfaceList.IndexOf(Item: IUnknown): Integer;
begin
  with FList.LockList do
  try
    Result := IndexOf(Pointer(Item));
  finally
    Self.FList.UnlockList;
  end;
end;

function TElInterfaceList.Add(Item: IUnknown): Integer;
begin
  with FList.LockList do
  try
    Result := Add(nil);
    IUnknown(List[Result]) := Item;
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TElInterfaceList.Insert(Index: Integer; Item: IUnknown);
begin
  with FList.LockList do
  try
    Insert(Index, nil);
    IUnknown(List[Index]) := Item;
  finally
    Self.FList.UnlockList;
  end;
end;

function TElInterfaceList.Last: IUnknown;
begin
  with FList.LockList do
  try
    Result := Self.Get(Count - 1);
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TElInterfaceList.Put(Index: Integer; Item: IUnknown);
begin
  with FList.LockList do
  try
    if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
    IUnknown(List[Index]) := Item;
  finally
    Self.FList.UnlockList;
  end;
end;

function TElInterfaceList.Remove(Item: IUnknown): Integer;
begin
  with FList.LockList do
  try
    Result := IndexOf(Pointer(Item));
    if Result > -1 then
    begin
      IUnknown(List[Result]) := nil;
      Delete(Result);
    end;
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TElInterfaceList.SetCapacity(NewCapacity: Integer);
begin
  with FList.LockList do
  try
    Capacity := NewCapacity;
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TElInterfaceList.SetCount(NewCount: Integer);
begin
  with FList.LockList do
  try
    Count := NewCount;
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TElInterfaceList.Exchange(Index1, Index2: Integer);
begin
  with FList.LockList do
  try
    Exchange(Index1, Index2);
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TElInterfaceList.Lock;
begin
  FList.LockList;
end;

procedure TElInterfaceList.Unlock;
begin
  FList.UnlockList;
end;

function TElInterfaceList.FastGet(Index: Integer): IUnknown;
begin
  Result := IUnknown(FList.LockList.List[Index]);
  FList.UnlockList;
end;

{ TElInterfaceStack }

procedure TElInterfaceStack.AfterConstruction;
begin
  inherited;
  FList := TElInterfaceList.Create;
end;

procedure TElInterfaceStack.Clear;
begin
  FList.Clear;
end;

function TElInterfaceStack.Empty: Boolean;
begin
  Result := (FList.Count = 0);
end;

function TElInterfaceStack.GetFront: IUnknown;
begin
  Result := FList.Last;
end;

function TElInterfaceStack.Pop: IUnknown;
var
  Index: Integer;
begin
  with FList do
  begin
    Lock;
    try
      Index := Pred(Count);
      Result := Items[Index];
      Delete(Index);
    finally
      Unlock;
    end;
  end;
end;

procedure TElInterfaceStack.Push(Value: IUnknown);
begin
  FList.Add(Value);
end;

{ TElAttributeList }

procedure TElAttributeList._SetAttribute(const AName, AValue: String);
begin
  SetAttribute(AName, AValue, False);
end;

procedure TElAttributeList.AfterConstruction;
begin
  inherited;
  FNames := TStringList.Create;
  FValues := TStringList.Create;
end;

procedure TElAttributeList.Clear;
begin
  FNames.Clear;
  FValues.Clear;
end;

destructor TElAttributeList.Destroy;
begin
  FNames.Free;
  FValues.Free;
  inherited;
end;

function TElAttributeList.GetAttribute(const AName: String): String;
var
  Index: Integer;
begin
  Index := FNames.IndexOf(AName);
  if Index >= 0 then
  begin
    Result := FValues[Index];
  end
  else
  begin
    Result := '';
  end;
end;

function TElAttributeList.GetCount: Integer;
begin
  Result := FNames.Count;
end;

function TElAttributeList.GetName(const Index: Integer): String;
begin
  Result := FNames[Index];
end;

function TElAttributeList.GetText: String;
var
  I: Integer;
  Name: String;
  Value: String;
begin
  Result := '';
  for I := 0 to Pred(FNames.Count) do
  begin
    Name := FNames[I];
    Value := FValues[I];
    if (Pos(' ', Name) > 0) or (Pos(#9, Name) > 0) then
    begin
      Name := '"' + Name + '"';
    end;
    Result := Result + ' ' + Name + '="' + Value + '"';
  end;
  Result := Trim(Result);
end;

function TElAttributeList.GetValue(const Index: Integer): String;
begin
  Result := FValues[Index];
end;

procedure TElAttributeList.RemoveAttribute(const AName: String);
var
  Index: Integer;
begin
  Index := FNames.IndexOf(AName);
  if Index >= 0 then
  begin
    FNames.Delete(Index);
    FValues.Delete(Index);
  end;
end;

procedure TElAttributeList.SetAttribute(const AName, AValue: String; Additive: Boolean);
var
  Index: Integer;
  O: String;
  V: String;
begin
  Index := FNames.IndexOf(AName);
  if Index >= 0 then
  begin
    V := AValue;
    if Additive then
    begin
      if V = '' then Exit;
      O := FValues[Index];
      if O <> '' then
      begin
        V := AValue + '; ' + O;
      end;
    end;
    FValues[Index] := V;
  end
  else
  begin
    FNames.Add(AName);
    FValues.Add(AValue);
  end;
end;

procedure TElAttributeList.SetText(const Text: String);
const
  QuoteChars = ['''', '"'];
var
  I: Integer;
  C: Char;
  InQuote: Boolean;
  QuoteChar: Char;
  Phrase: String;
  Name: String;
  NameParsed: Boolean;
  ValueParsed: Boolean;
begin
  Clear;
  Phrase := '';
  NameParsed := False;
  ValueParsed := False;
  InQuote := False;
  QuoteChar := #0;
  for I := 1 to Length(Text) do
  begin
    C := Text[I];
    if InQuote then
    begin
      if C = QuoteChar then
      begin
        InQuote := False;
        Continue;
      end;
    end
    else
    begin
      if C in QuoteChars then
      begin
        QuoteChar := C;
        InQuote := True;
        Continue;
      end;
    end;
    if not InQuote then
    begin
      if (C = '=') and not NameParsed then
      begin
        Name := Trim(Phrase);
        Phrase := '';
        NameParsed := True;
        ValueParsed := False;
        Continue;
      end;
      if (C in [' ', #9]) and ValueParsed then
      begin
        if Name <> '' then
        begin
          SetAttribute(Name, Trim(Phrase), True);
        end;
        Phrase := '';
        NameParsed := False;
        ValueParsed := False;
        Continue;
      end;
      if NameParsed and not (C in [' ', #9]) then
      begin
        ValueParsed := True;
      end;
    end;
    Phrase := Phrase + C;
  end;
  if (not NameParsed) and (Phrase <> '') then
  begin
    Name := Trim(Phrase);
    NameParsed := True;
    Phrase := '';
  end;
  if NameParsed then
  begin
    if Name <> '' then
    begin
      SetAttribute(Name, Trim(Phrase), True);
    end;
  end;
end;

procedure TElAttributeList.SetValue(const Index: Integer;
  const AValue: String);
begin
  FValues[Index] := AValue;
end;

end.
