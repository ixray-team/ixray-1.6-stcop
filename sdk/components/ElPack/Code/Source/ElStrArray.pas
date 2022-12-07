
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

unit ElStrArray;

interface

uses Classes,
     ElArray;

type

  PElStringItem = ^TElStringItem;
  TElStringItem = record
    FString: PChar;
    FObject: TObject;
  end;

  TElStringArray = class(TStrings)
  private
    FStoreAssociatedData : Boolean;
    FDuplicates : TDuplicates;
    FSorted : Boolean;
    FOnChanging : TNotifyEvent;
    FOnChange : TNotifyEvent;
    FArray : TElArray;
    FUpdateCount : integer;
    procedure OnItemDelete(Sender : TObject; Item : Pointer);
    procedure SetSorted(newValue : Boolean);
    procedure ExchangeItems(Index1, Index2: Integer); 
  protected
    procedure QuickSort(L, R: Integer); virtual;
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure TriggerChangingEvent; virtual;
    procedure TriggerChangeEvent; virtual;
    procedure InsertItem(Index: Integer; const S: string); virtual;
    function  Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function  GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure PutStringEntry(Index: Integer; const S : String; AObject: TObject); virtual;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;

    function  Add(const S: string): integer; override;
    function  AddStringEntry(const S: string; AObject: TObject): integer; virtual;

    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    function  IndexOf(const S: string): Integer; override;
    function  IIndexOf(const S: string): Integer;
    function  Find(const S: string; var Index: Integer): Boolean; virtual;
    procedure Sort; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure SaveToBinaryStream(Stream: TStream); virtual;
    procedure LoadFromBinaryStream(Stream: TStream); virtual;
    //procedure LoadFromStream(Stream: TStream); override;
    //procedure SaveToStream(Stream: TStream); override;

    constructor Create;
    destructor Destroy; override;
    
    property Duplicates : TDuplicates read FDuplicates write FDuplicates;  { Public }
    property Sorted : Boolean read FSorted write SetSorted;  { Public }
  published
    property OnChanging : TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property StoreAssociatedData : Boolean read FStoreAssociatedData write FStoreAssociatedData;  { Published }
  end;

implementation

uses SysUtils
{$ifdef VCL_6_USED}
     , SysConst,
     RTLConsts
{$endif}
{$ifndef CLX_USED}
     , Consts
{$endif};

function WriteStringToStream(S : TStream; Str : string) : boolean;
var
  i : integer;
begin
  i := Length(Str);
  try
    S.WriteBuffer(i, sizeof(integer));
    if i > 0 then
      S.WriteBuffer(Str[1], i);
    result := true;
  except
    result := false;
  end;
end;

function ReadStringFromStream(S : TStream; var Str : string) : boolean;
var
  SS : string;
  i : integer;
begin
  i := Length(Str);
  try
    S.ReadBuffer(i, sizeof(integer));
    if i < 0 then raise Exception.Create('Invalid string header read from the stream');
    if i = 0 then
      Str := ''
    else
    begin
      SetLength(SS, i);
      S.ReadBuffer(SS[1], i);
      Str := SS;
    end;
    result := true;
  except
    result := false;
  end;
end;

procedure TElStringArray.Clear;
begin
  if FArray.Count <> 0 then
  begin
    Changing;
    FArray.Clear;
    Changed;
  end;
end;

procedure TElStringArray.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TElStringArray.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TElStringArray.OnItemDelete(Sender : TObject; Item : Pointer);
var AR : PElStringItem;
begin
  if Item <> nil then
  begin
    AR := PElStringItem(Item);
    if AR.FString <> nil then FreeMem(AR.FString);
    FreeMem(AR);
  end;
end;

procedure TElStringArray.Delete(Index: Integer);
begin
  Changing;
  FArray.Delete(Index);
  Changed;
end;

function TElStringArray.AddStringEntry(const S: string; AObject: TObject): integer;
var Item : PElStringItem;
begin
  Changing;
  New(Item);
  GetMem(Item.FString, Length(S) + 1);
  StrPCopy(Item.FString, S);
  Item.FObject := AObject;
  result := FArray.Count;
  FArray.Insert(result, Item);
  Changed;
end;


procedure TElStringArray.InsertItem(Index: Integer; const S: string);
var Item : PElStringItem;
begin
  Changing;
  New(Item);
  GetMem(Item.FString, Length(S) + 1);
  StrPCopy(Item.FString, S);
  Item.FObject := nil;
  FArray.Insert(Index, Item);
  Changed;
end;

function TElStringArray.Add(const S: string): Integer;
begin
  if not Sorted then Result := FArray.Count else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(SDuplicateString, 0);
      end;
  InsertItem(Result, S);
end;

procedure TElStringArray.Insert(Index: Integer; const S: string);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) then Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

function TElStringArray.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

function TElStringArray.IIndexOf(const S: string): Integer;
var I , J : integer;
    Item  : PElStringItem;
begin
  result := -1;
  j := Self.FArray.Count;
  for i := 0 to j - 1 do    // Iterate
  begin
    Item  := PElStringItem(FArray.Items[i]);
    if Item <> nil then
    begin
      if AnsiStrIComp(Item.FString, PChar(S)) = 0 then
      begin
        result := i;
        exit;
      end;
    end;
  end;    // for
end;

function TElStringArray.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FArray.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := AnsiStrComp(PElStringItem(FArray[I]).FString, PChar(S));
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TElStringArray.SetSorted(newValue : Boolean);
begin
  if (FSorted <> newValue) then
  begin
    if newValue then Sort;
    FSorted := newValue;
  end;  { if }
end;  { SetSorted }

procedure TElStringArray.TriggerChangingEvent;
begin
  if (assigned(FOnChanging)) then
    FOnChanging(Self);
end;  { TriggerChangingEvent }

procedure TElStringArray.TriggerChangeEvent;
begin
  if (assigned(FOnChange)) then
    FOnChange(Self);
end;  { TriggerChangeEvent }

procedure TElStringArray.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: PChar;
begin
  repeat
    I := L;
    J := R;
    P := PElStringItem(FArray[(L + R) shr 1]).FString;
    repeat
      while AnsiStrComp(PElStringItem(FArray[I]).FString, P) < 0 do Inc(I);
      while AnsiStrComp(PElStringItem(FArray[J]).FString, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TElStringArray.Put(Index: Integer; const S: string);
var OV   : PChar;
    Item : PElStringItem;
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) then Error(SListIndexError, Index);
  Changing;
  if FArray.Count <= Index then
  begin
    New(Item);
    GetMem(Item.FString, Length(S) + 1);
    StrPCopy(Item.FString, S);
    Item.FObject := nil;
    FArray[Index] := Item;
  end else
  begin
    OV := PElStringItem(FArray[Index]).FString;
    if OV <> nil then FreeMem(OV);
    if Length(S) = 0 then PElStringItem(FArray[Index]).FString := nil else
    begin
      GetMem(PElStringItem(FArray[Index]).FString, Length(S) + 1);
      StrPCopy(PElStringItem(FArray[Index]).FString, S);
    end;
  end;                    
  Changed;
end;

procedure TElStringArray.PutStringEntry(Index: Integer; const S : String; AObject: TObject);
var OV   : PChar;
    Item : PElStringItem;
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) then Error(SListIndexError, Index);
  Changing;
  if FArray.Count <= Index then
  begin
    New(Item);
    GetMem(Item.FString, Length(S) + 1);
    StrPCopy(Item.FString, S);
    Item.FObject := AObject;
    FArray[Index] := Item;
  end else
  begin
    OV := PElStringItem(FArray[Index]).FString;
    if OV <> nil then FreeMem(OV);
    if Length(S) = 0 then PElStringItem(FArray[Index]).FString := nil else
    begin
      GetMem(PElStringItem(FArray[Index]).FString, Length(S) + 1);
      StrPCopy(PElStringItem(FArray[Index]).FString, S);
    end;
    PElStringItem(FArray[Index]).FObject := AObject;
  end;
  Changed;
end;

procedure TElStringArray.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) then Error(SListIndexError, Index);
  Changing;
  PElStringItem(FArray[Index]).FObject := AObject;
  Changed;
end;

procedure TElStringArray.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function TElStringArray.GetCount: Integer;
begin
  result := FArray.Count;
end;

function TElStringArray.Get(Index: Integer): string;
var Item : PElStringItem;
begin
  if (Index < 0) then Error(SListIndexError, Index);
  Item := PElStringItem(FArray[Index]);
  if Item = nil then result := '' else Result := StrPas(Item.FString);
end;

function TElStringArray.GetObject(Index: Integer): TObject;
var Item : PElStringItem;
begin
  if (Index < 0) then Error(SListIndexError, Index);
  Item := PElStringItem(FArray[Index]);
  if Item = nil then result := nil else Result := Item.FObject;
end;

procedure TElStringArray.Sort;
begin
  if not Sorted and (FArray.Count > 1) then
  begin
    Changing;
    QuickSort(0, FArray.Count - 1);
    Changed;
  end;
end;

procedure TElStringArray.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) then Error(SListIndexError, Index1);
  if (Index2 < 0) then Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TElStringArray.ExchangeItems(Index1, Index2: Integer);
begin
  FArray.Exchange(Index1, Index2);
end;

procedure TElStringArray.SaveToBinaryStream(Stream: TStream);
var i, j : integer;
    S    : String;
    O    : TObject;

begin
  j := FArray.Count;
  Stream.WriteBuffer(FStoreAssociatedData, sizeof(FStoreAssociatedData));
  Stream.WriteBuffer(j, sizeof(j));
  for i := 0 to j - 1 do    // Iterate
  begin
    S := Get(i);
    WriteStringToStream(Stream, S);
    if FStoreAssociatedData then
    begin
      O := Self.GetObject(i);
      Stream.WriteBuffer(O, sizeof(O));
    end;
  end;    // for
end;

procedure TElStringArray.LoadFromBinaryStream(Stream: TStream);
var i , j : integer;
    S     : String;
    O     : TObject;
    fos,
    fss   : boolean;

begin
  Changing;
  Stream.ReadBuffer(j, sizeof(j));
  Stream.ReadBuffer(fss, sizeof(fss));
  fos := Sorted;
  Sorted := false;
  for i := 0 to j - 1 do    // Iterate
  begin
    ReadStringFromStream(Stream, S);
    if fss then
    Stream.ReadBuffer(O, sizeof(O));
    AddStringEntry(S, O);
  end;    // for
  FSorted := fos;
  Changed;
end;

constructor TElStringArray.Create;
begin
  inherited;
  FArray := TElArray.Create;
  FArray.OnDelete := OnItemDelete;
end;

destructor TElStringArray.Destroy;
begin
  FArray.Free;
  inherited;
end;


end.

