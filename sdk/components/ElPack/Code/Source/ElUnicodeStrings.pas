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

06/25/2002

  IndexOfName was broken (reported by Paul Kienitz). Fixed

06/06/2002

  Replaced several AnsiCompareText calls to WideCompareText

05/12/2002

  Fixed setting text via Text property -- it skipped empty lines. 

04/24/2002

  Fixed loading of non-unicode text to the stream

04/17/2002

  Added SaveUnicode property and byte-order handling

*)

unit ElUnicodeStrings;

interface

uses Classes,
     SysUtils,
     {$ifndef CLX_USED}
     Consts,
     {$endif}
     ElArray,
     ElTools,
     {$ifdef VCL_6_USED}
     RTLConsts,
     {$endif}
     ElStrUtils;

const
  U_LSB_FIRST = WideChar($FEFF);
  U_MSB_FIRST = WideChar($FFFE);
{$ifdef BROKEN_UNICODE}
implementation
{$endif}

{$ifndef BROKEN_UNICODE}

type

    TElWideStrings = class;
    TElWideStringList = class;

{$ifdef MSWINDOWS}
{$ifdef ELPACK_UNICODE}
  TElFStrings = TElWideStrings;
  TElFStringList = TElWideStringList;
{$else}
{$endif}
{$else}
{$endif}


{$warnings off}

    PWideStringItem = ^TWideStringItem;
    TWideStringItem = record
      FString: WideString;
      FObject: TObject;
    end;

    PWideStringItemList = ^TWideStringItemList;
    TWideStringItemList = array[0..MaxListSize] of TWideStringItem;

    TElWideStrings = class(TPersistent)
    private
      FUpdateCount: Integer;
      FSaveUnicode: Boolean;
      FSaved : boolean;

      function GetCommaText: WideString;
      function GetName(Index: Integer): WideString;
      function GetValue(const Name: WideString): WideString;
      procedure ReadData(Reader: TReader);
      procedure SetCommaText(Value: WideString);
      procedure SetValue(const Name, Value: WideString);
      procedure WriteData(Writer: TWriter);
      procedure StrSwapByteOrder(Str: PWideChar);
    protected
      procedure DefineProperties(Filer: TFiler); override;
      {$ifdef VCL_4_USED}
      procedure Error(const Msg: string; Data: Integer); overload;
      {$else}
      procedure Error(const Msg: string; Data: Integer);
      {$endif}
      function Get(Index: Integer): WideString; virtual; abstract;
      function GetCapacity: Integer; virtual;
      function GetCount: Integer; virtual; abstract;
      function GetObject(Index: Integer): TObject; virtual;
      function GetTextStr: WideString; virtual;
      procedure Put(Index: Integer; const S: WideString); virtual;
      procedure PutObject(Index: Integer; AObject: TObject); virtual;
      procedure SetCapacity(NewCapacity: Integer); virtual;
      procedure SetTextStr(const Value: WideString); virtual;
      procedure SetUpdateState(Updating: Boolean); virtual;
    public
      destructor Destroy; override;
      function Add(const S: WideString): Integer; virtual;
      function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
      procedure AddStrings(Strings: TElWideStrings); virtual;
      procedure Append(const S: WideString);
      procedure Assign(Source: TPersistent); override;
      procedure BeginUpdate;
      procedure Clear; virtual; abstract;
      procedure Delete(Index: Integer); virtual; abstract;
      procedure EndUpdate;
      function Equals(Strings: TElWideStrings): Boolean;
      procedure Exchange(Index1, Index2: Integer); virtual;
      function GetText: PWideChar; virtual;
      function IndexOf(const S: WideString): Integer; virtual;
      function IndexOfName(const Name: WideString): Integer;
      function IndexOfObject(AObject: TObject): Integer;
      procedure Insert(Index: Integer; const S: WideString); virtual; abstract;
      procedure InsertObject(Index: Integer; const S: WideString; AObject: TObject);
      procedure LoadFromFile(const FileName: string); virtual;
      procedure LoadFromStream(Stream: TStream); virtual;
      procedure Move(CurIndex, NewIndex: Integer); virtual;
      procedure SaveToFile(const FileName: string); virtual;
      procedure SaveToStream(Stream: TStream); virtual;
      procedure SetText(Text: PWideChar); virtual;
      procedure AssignTo(Dest: TPersistent); override;
      property Capacity: Integer read GetCapacity write SetCapacity;
      property CommaText: WideString read GetCommaText write SetCommaText;
      property Count: Integer read GetCount;
      property Names[Index: Integer]: WideString read GetName;
      property Objects[Index: Integer]: TObject read GetObject write PutObject;
      property SaveUnicode: Boolean read FSaveUnicode write FSaveUnicode;
      property Strings[Index: Integer]: WideString read Get write Put; default;
      property Text: WideString read GetTextStr write SetTextStr;
      property Values[const Name: WideString]: WideString read GetValue write
          SetValue;

    end;

    TElWideStringListSortCompare = function(List: TElWideStringList; Index1, Index2: Integer): Integer;

    TElWideStringList = class(TElWideStrings)
    private
      FDuplicates: TDuplicates;
      FOnChange: TNotifyEvent;
      FOnChanging: TNotifyEvent;
      FSorted: Boolean;
      FCapacity: Integer;
      FCount: Integer;
      FList: PWideStringItemList;
      procedure ExchangeItems(Index1, Index2: Integer);
      procedure Grow;
      procedure InsertItem(Index: Integer; const S: WideString);
      procedure QuickSort(L, R: Integer; SCompare: TElWideStringListSortCompare);
      procedure SetSorted(Value: Boolean);
    protected
      procedure Changed; virtual;
      procedure Changing; virtual;
      function Get(Index: Integer): WideString; override;
      function GetCapacity: Integer; override;
      function GetCount: Integer; override;
      function GetObject(Index: Integer): TObject; override;
      procedure Put(Index: Integer; const S: WideString); override;
      procedure PutObject(Index: Integer; AObject: TObject); override;
      procedure SetCapacity(NewCapacity: Integer); override;
      procedure SetUpdateState(Updating: Boolean); override;
    public
      destructor Destroy; override;
      function Add(const S: WideString): Integer; override;
      procedure Clear; override;
      procedure CustomSort(Compare: TElWideStringListSortCompare); virtual;
      procedure Delete(Index: Integer); override;
      procedure Exchange(Index1, Index2: Integer); override;
      function Find(const S: WideString; var Index: Integer): Boolean; virtual;
      function IndexOf(const S: WideString): Integer; override;
      procedure Insert(Index: Integer; const S: WideString); override;
      procedure Sort; virtual;
      property Duplicates: TDuplicates read FDuplicates write FDuplicates;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
      property Sorted: Boolean read FSorted write SetSorted;
    end;

  TElWideStringArray = class(TElWideStrings)
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
    procedure InsertItem(Index: Integer; const S: WideString); virtual;
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    function  GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure PutStringEntry(Index: Integer; const S : WideString; AObject:
        TObject); virtual;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;

    function Add(const S: WideString): Integer; override;
    function AddStringEntry(const S: WideString; AObject: TObject): Integer; 
        virtual;

    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: WideString); override;
    function IndexOf(const S: WideString): Integer; override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
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

{$warnings on}

implementation

type

  PElWideStringItem = ^TElWideStringItem;
  TElWideStringItem = record
    FString: PWideChar;
    FObject: TObject;
  end;

destructor TElWideStrings.Destroy;
begin
  inherited Destroy;
end;

function TElWideStrings.Add(const S: WideString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TElWideStrings.AddObject(const S: WideString; AObject: TObject): 
    Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TElWideStrings.AddStrings(Strings: TElWideStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TElWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TElWideStrings.Assign(Source: TPersistent);
var i : integer;
begin
  if Source is TElWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TElWideStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end
  else
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      Text := TStrings(Source).Text;
      for i := 0 to TStrings(Source).Count - 1 do
        Objects[i] := TStrings(Source).Objects[i];
      {for i := 0 to TStrings(Source).Count -1 do
        Add(TStrings(Source)[i]);
        }
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TElWideStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TElWideStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TStrings then
        Result := not Equals(TElWideStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;
 
begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TElWideStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TElWideStrings.Equals(Strings: TElWideStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then
      Exit;
  Result := True;
end;

procedure TElWideStrings.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TElWideStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TElWideStrings.GetCapacity: Integer;
begin  // descendants may optionally override/replace this default implementation
  Result := Count;
end;

function TElWideStrings.GetCommaText: WideString;
var
  S: WideString;
  P: PWideChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := '""'
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not (P^ in [WideChar(#0)..WideChar(' '),WideChar('"'),WideChar(',')]) do
        Inc(P);
      {$ifdef VCL_4_USED}
      if (P^ <> WideChar(#0)) then
        S := WideQuotedStr(S, '"');
      {$endif}
      Result := Result + S + ',';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TElWideStrings.GetName(Index: Integer): WideString;
var
  P: PWideChar;
begin
  Result := Get(Index);
  P := WideStrScan(PWideChar(Result), '=');
  if Assigned(P) then
    SetLength(Result, P - PWideChar(Result))
  else
    SetLength(Result, 0);
end;

function TElWideStrings.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  I: Integer;
  S: WideString;
begin
  for I := 0 to GetCount - 1 do
  begin
    S := Get(I);
    P := WidePos('=', S);
    if (P <> 0) and (CompareWideStr(WideCopy(S, 1, P - 1), Name) = 0) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TElWideStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TElWideStrings.GetText: PWideChar;
var S : WideString;
    l : integer;
begin
  S := GetTextStr;
  l := (Length(S) + 1) * sizeof(WideChar);
  GetMem(Result, l);
  System.Move(S[1], Result[1], l);
end;

function TElWideStrings.GetTextStr: WideString;
var
  I, L, Size, Count: Integer;
  P: PWideChar;
  S: WideString;
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do
    Inc(Size, Length(Get(I)) + 2);
  SetLength(Result, Size);
  P := PWideChar(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      WideMove(S[1], P^, L);
      Inc(P, L);
    end;
    P^ := #13;
    Inc(P);
    P^ := #10;
    Inc(P);
  end;
end;

function TElWideStrings.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := WideCopy(Get(I), Length(Name) + 2, MaxInt shr 1)
  else
    Result := '';
end;

function TElWideStrings.IndexOf(const S: WideString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareWideStr(Get(Result), S) = 0 then
      Exit;
  Result := -1;
end;

function TElWideStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TElWideStrings.InsertObject(Index: Integer; const S: WideString; 
    AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TElWideStrings.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TElWideStrings.StrSwapByteOrder(Str: PWideChar);

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EDX contains address of string

asm
         PUSHF
         PUSH ESI
         PUSH EDI
         MOV ESI, EDX
         MOV EDI, ESI
         XOR EAX, EAX  // clear high order byte to be able to use 32bit operand below
         CLD
@@1:     LODSW
         OR EAX, EAX
         JZ @@2
         XCHG AL, AH
         STOSW
         JMP @@1

@@2:     POP EDI
         POP ESI
         POPF
end;

procedure TElWideStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  BytesRead: Integer;
  Order: WideChar;
  WS: WideString;
  S : String;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    BytesRead := Stream.Read(Order, 2);

    if (Order = U_LSB_FIRST) or (Order = U_MSB_FIRST) then
    begin
      FSaveUnicode := True;
      SetLength(WS, (Size - 2) div 2);
      Stream.Read(PWideChar(WS)^, Size - 2);

      if Order = U_MSB_FIRST then
        StrSwapByteOrder(PWideChar(WS));

      SetTextStr(WS);
    end
    else
    begin
      FSaveUnicode := False;
      Stream.Seek(-BytesRead, soFromCurrent);
      SetString(S, nil, Size);
      Stream.Read(Pointer(S)^, Size);
      SetTextStr(S);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TElWideStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TElWideStrings.Put(Index: Integer; const S: WideString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TElWideStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TElWideStrings.ReadData(Reader: TReader);
var i : integer;
    P : PWideChar;
    C : PChar;
    rv: integer;
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
    begin
      rv := Integer(Reader.ReadValue);
      if (rv <> 18) and
         (rv <> 12) and
         (rv <> 6) then
        raise EReadError.Create('Error while reading wide strings list');
      if rv = 18 then
      begin
        Reader.Read(i, SizeOf(Integer));
        GetMem(P, (i + 1) * sizeof(WideChar));
        Reader.Read(P^, (i) * sizeof(WideChar));
        P[i] := #0;
        Add(WideStrPas(P));
        FreeMem(P);
      end
      else
      begin
        i := 0;
        if rv = 12 then
          Reader.Read(i, SizeOf(Integer))
        else
          Reader.Read(i, SizeOf(Byte));
          
        GetMem(C, i + 1);
        Reader.Read(C^, i);
        C[i] := #0;
        Add(StrPas(C));
        FreeMem(C);
      end;
    end;
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TElWideStrings.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TElWideStrings.SaveToStream(Stream: TStream);
var
  SW, BOM: WideString;
  SA: String;
//  Allowed: Boolean;
//  Run: PWideChar;

begin
  // The application can decide in which format to save the content.
  // If FSaveUnicode is False then all strings are saved in standard ANSI format
  // which is also loadable by TStrings but you should be aware that all Unicode
  // strings are then converted to ANSI based on the current system locale.
  // An extra event is supplied to ask the user about the potential loss of information
  // when converting Unicode to ANSI strings.
  SW := GetTextStr;
  FSaved := False; // be pessimistic

//  if Allowed then
  begin
    // only save if allowed
    if FSaveUnicode then
    begin
      BOM := U_LSB_FIRST;
      Stream.WriteBuffer(PWideChar(BOM)^, 2);
      // SW has already been filled
      Stream.WriteBuffer(PWideChar(SW)^, 2 * Length(SW));
    end
    else
    begin
      // implicit conversion to ANSI
      SA := SW;
      Stream.WriteBuffer(PChar(SA)^, Length(SA));
    end;
    FSaved := True;
  end;
{var
  S: WideString;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S) * sizeof(WideChar));}
end;

procedure TElWideStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TElWideStrings.SetCommaText(Value: WideString);
var
  P, P1: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while P^ in [Widechar(#1)..Widechar(' ')] do inc(P);
    while P^ <> #0 do
    begin
      {$ifdef VCL_4_USED}
      if P^ = '"' then
        S := WideExtractQuotedStr(P, '"')
      else
      {$endif}
      begin
        P1 := P;
        while (P^ > ' ') and (P^ <> ',') do Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      while P^ in [Widechar(#1)..WideChar(' ')] do Inc(P);
      if P^ = ',' then
        repeat
          Inc(P);
        until not (Char(P^) in [#1..' ']);
    end;
  finally
    EndUpdate;
  end;
end;


procedure TElWideStrings.SetText(Text: PWideChar);
begin
  SetTextStr(Text);
end;

procedure TElWideStrings.SetTextStr(const Value: WideString);
var
  P, Start: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not ((P^ = #0) or (P^ = #13) or (P^ = #10)) do Inc(P);
        SetWideString(S, Start, P - Start);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TElWideStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TElWideStrings.SetValue(const Name, Value: WideString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TElWideStrings.WriteData(Writer: TWriter);
var
  I,
  K : Integer;
  J : TValueType;
  W : WideString;
begin
  Writer.WriteListBegin;
  //K := Count;
  //Writer.Write(K, sizeof(K));
  for I := 0 to Count - 1 do
  begin
    j := TValueType(18);
    Writer.Write(j, sizeof(TValueType));
    W := Get(I);
    K := Length(W);
    Writer.Write(K, sizeof(K));
    Writer.Write(W[1], Length(W) * sizeof(WideChar));
  end;
  Writer.WriteListEnd;
end;

procedure TElWideStrings.AssignTo(Dest: TPersistent);
var i : integer;
begin
  if Dest is TElWideStrings then
  begin
    Dest.Assign(Self)
  end
  else
  if Dest is TStrings then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Text := Text;
      for i := 0 to Count - 1 do
        TStrings(Dest).Objects[i] := Objects[i];
    finally
      TStrings(Dest).EndUpdate;
    end;
    exit;
  end;
  inherited;
end;

destructor TElWideStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then
    Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TElWideStringList.Add(const S: WideString): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(SDuplicateString, 0);
      end;
  InsertItem(Result, S);
end;

procedure TElWideStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TElWideStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TElWideStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TElWideStringList.CustomSort(Compare: TElWideStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

procedure TElWideStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
  Changed;
end;

procedure TElWideStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TElWideStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PWideStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TElWideStringList.Find(const S: WideString; var Index: Integer): 
    Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := WideCompareText(FList^[I].FString, S);
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
                   
function TElWideStringList.Get(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TElWideStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TElWideStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TElWideStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TElWideStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TElWideStringList.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else
  if not Find(S, Result) then
    Result := -1;
end;

procedure TElWideStringList.Insert(Index: Integer; const S: WideString);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

procedure TElWideStringList.InsertItem(Index: Integer; const S: WideString);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TElWideStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TElWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TElWideStringList.QuickSort(L, R: Integer; SCompare: 
    TElWideStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TElWideStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

procedure TElWideStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TElWideStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListWideCompare(List: TElWideStringList; Index1, Index2: Integer): Integer;
begin
  Result := WideCompareText(List.FList^[Index1].FString,
                            List.FList^[Index2].FString);
end;

procedure TElWideStringList.Sort;
begin
  CustomSort(StringListWideCompare);
end;

constructor TElWideStringArray.Create;
begin
  inherited;
  FArray := TElArray.Create;
  FArray.OnDelete := OnItemDelete;
end;

destructor TElWideStringArray.Destroy;
begin
  FArray.Free;
  inherited;
end;

function TElWideStringArray.Add(const S: WideString): Integer;
begin
  if not Sorted then
    Result := FArray.Count
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(SDuplicateString, 0);
      end;
  InsertItem(Result, S);
end;

function TElWideStringArray.AddStringEntry(const S: WideString; AObject:
    TObject): Integer;
var Item : PElWideStringItem;
begin
  Changing;
  New(Item);
  GetMem(Item.FString, (Length(S) + 1) * sizeof(WideChar));
  WideStrPCopy(Item.FString, S);
  Item.FObject := AObject;
  result := FArray.Count;
  FArray.Insert(result, Item);
  Changed;
end;

procedure TElWideStringArray.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TElWideStringArray.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TElWideStringArray.Clear;
begin
  if FArray.Count <> 0 then
  begin
    Changing;
    FArray.Clear;
    Changed;
  end;
end;

procedure TElWideStringArray.Delete(Index: Integer);
begin
  Changing;
  FArray.Delete(Index);
  Changed;
end;

procedure TElWideStringArray.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) then
    Error(SListIndexError, Index1);
  if (Index2 < 0) then
    Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TElWideStringArray.ExchangeItems(Index1, Index2: Integer);
begin
  FArray.Exchange(Index1, Index2);
end;

function TElWideStringArray.Find(const S: WideString; var Index: Integer): 
    Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FArray.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := WideStrComp(PElWideStringItem(FArray[I]).FString, PWideChar(S));
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function TElWideStringArray.Get(Index: Integer): WideString;
var Item : PElWideStringItem;
begin
  if (Index < 0) then Error(SListIndexError, Index);
  Item := PElWideStringItem(FArray[Index]);
  if Item = nil then
    result := ''
  else
    Result := WideStrPas(Item.FString);
end;

function TElWideStringArray.GetCount: Integer;
begin
  result := FArray.Count;
end;

function TElWideStringArray.GetObject(Index: Integer): TObject;
var Item : PElWideStringItem;
begin
  if (Index < 0) then Error(SListIndexError, Index);
  Item := PElWideStringItem(FArray[Index]);
  if Item = nil then result := nil else Result := Item.FObject;
end;

function TElWideStringArray.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else
    if not Find(S, Result) then Result := -1;
end;

procedure TElWideStringArray.Insert(Index: Integer; const S: WideString);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) then Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

procedure TElWideStringArray.InsertItem(Index: Integer; const S: WideString);
var Item : PElWideStringItem;
begin
  Changing;
  New(Item);
  GetMem(Item.FString, (Length(S) + 1) * sizeof(WideChar));
  WideStrPCopy(Item.FString, S);
  Item.FObject := nil;
  FArray.Insert(Index, Item);
  Changed;
end;

procedure TElWideStringArray.LoadFromBinaryStream(Stream: TStream);
var i , j : integer;
    S     : WideString;
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
    ReadWideStringFromStream(Stream, S);
    if fss then
      Stream.ReadBuffer(O, sizeof(O));
    AddStringEntry(S, O);
  end;    // for
  FSorted := fos;
  Changed;
end;

procedure TElWideStringArray.OnItemDelete(Sender : TObject; Item : Pointer);
var AR : PElWideStringItem;
begin
  if Item <> nil then
  begin
    AR := PElWideStringItem(Item);
    if AR.FString <> nil then
      FreeMem(AR.FString);
    FreeMem(AR);
  end;
end;

procedure TElWideStringArray.Put(Index: Integer; const S: WideString);
var OV   : PWideChar;
    Item : PElWideStringItem;
begin
   if Sorted then Error(SSortedListError, 0);
  if (Index < 0) then Error(SListIndexError, Index);
  Changing;
  if FArray.Count <= Index then
  begin
    New(Item);
    GetMem(Item.FString, (Length(S) + 1) * sizeof(WideChar));
    WideStrPCopy(Item.FString, S);
    Item.FObject := nil;
    FArray[Index] := Item;
  end else
  begin
    OV := PElWideStringItem(FArray[Index]).FString;
    if OV <> nil then FreeMem(OV);
    if Length(S) = 0 then
      PElWideStringItem(FArray[Index]).FString := nil
    else
    begin
      GetMem(PElWideStringItem(FArray[Index]).FString, (Length(S) + 1)  * sizeof(WideChar));
      WideStrPCopy(PElWideStringItem(FArray[Index]).FString, S);
    end;
  end;                    
  Changed;
end;

procedure TElWideStringArray.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) then Error(SListIndexError, Index);
  Changing;
  PElWideStringItem(FArray[Index]).FObject := AObject;
  Changed;
end;

procedure TElWideStringArray.PutStringEntry(Index: Integer; const S :
    WideString; AObject: TObject);
var OV   : PWideChar;
    Item : PElWideStringItem;
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) then Error(SListIndexError, Index);
  Changing;
  if FArray.Count <= Index then
  begin
    New(Item);
    GetMem(Item.FString, (Length(S) + 1)  * sizeof(WideChar));
    WideStrPCopy(Item.FString, S);
    Item.FObject := AObject;
    FArray[Index] := Item;
  end
  else
  begin
    OV := PElWideStringItem(FArray[Index]).FString;
    if OV <> nil then
      FreeMem(OV);
    if Length(S) = 0 then
      PElWideStringItem(FArray[Index]).FString := nil
    else
    begin
      GetMem(PElWideStringItem(FArray[Index]).FString, (Length(S) + 1) * sizeof(WideChar));
      WideStrPCopy(PElWideStringItem(FArray[Index]).FString, S);
    end;
    PElWideStringItem(FArray[Index]).FObject := AObject;
  end;
  Changed;
end;

procedure TElWideStringArray.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: PWideChar;
begin
  repeat
    I := L;
    J := R;
    P := PElWideStringItem(FArray[(L + R) shr 1]).FString;
    repeat
      while WideStrComp(PElWideStringItem(FArray[I]).FString, P) < 0 do Inc(I);
      while WideStrComp(PElWideStringItem(FArray[J]).FString, P) > 0 do Dec(J);
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

procedure TElWideStringArray.SaveToBinaryStream(Stream: TStream);
var i, j : integer;
    S    : WideString;
    O    : TObject;

begin
  j := FArray.Count;
  Stream.WriteBuffer(FStoreAssociatedData, sizeof(FStoreAssociatedData));
  Stream.WriteBuffer(j, sizeof(j));
  for i := 0 to j - 1 do    // Iterate
  begin
    S := Get(i);
    WriteWideStringToStream(Stream, S);
    if FStoreAssociatedData then
    begin
      O := GetObject(i);
      Stream.WriteBuffer(O, sizeof(O));
    end;
  end;    // for
end;

procedure TElWideStringArray.SetSorted(newValue : Boolean);
begin
  if (FSorted <> newValue) then
  begin
    if newValue then
      Sort;
    FSorted := newValue;
  end;  { if }
end;  { SetSorted }

procedure TElWideStringArray.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

procedure TElWideStringArray.Sort;
begin
  if not Sorted and (FArray.Count > 1) then
  begin
    Changing;
    QuickSort(0, FArray.Count - 1);
    Changed;
  end;
end;

procedure TElWideStringArray.TriggerChangeEvent;
begin
  if (assigned(FOnChange)) then
    FOnChange(Self);
end;  { TriggerChangeEvent }

procedure TElWideStringArray.TriggerChangingEvent;
begin
  if (assigned(FOnChanging)) then
    FOnChanging(Self);
end;  { TriggerChangingEvent }
{$endif}

end.
