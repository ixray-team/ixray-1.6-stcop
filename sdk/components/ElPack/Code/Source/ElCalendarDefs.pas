
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

unit ElCalendarDefs;

interface

uses ElTools, Classes, SysUtils,
     {$ifndef CLX_USED}
{$ifdef VCL_6_USED}
Types,
{$endif}
     Controls
     {$else}
     Qt,
     QControls
     {$endif}
     ;

type

  TDayOfWeek = 0..6;

  TElWeekEndDay = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

  TElWeekEndDays = set of TElWeekEndDay;

  TElHoliday = class(TCollectionItem)
  private
    FDescription : string;
    FFixedDate : Boolean;
    FDay : Word;
    FDayOfWeek : Word;
    FMonth : Word;
    FIsRest : Boolean;
    procedure SetFixedDate(newValue : Boolean);
    procedure SetDay(newValue : Word);
    procedure SetDayOfWeek(newValue : Word);
    procedure SetMonth(newValue : Word);
    procedure SetIsRest(newValue : Boolean);
  public
    constructor Create(Collection : TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure SaveToStream(Stream : TStream);
    procedure LoadFromStream(Stream : TStream);
  published
    property FixedDate : Boolean read FFixedDate write SetFixedDate default True;
    property Day : Word read FDay write SetDay;
    property DayOfWeek : Word read FDayOfWeek write SetDayOfWeek;
    property Month : Word read FMonth write SetMonth;
    property IsRest : Boolean read FIsRest write SetIsRest;
    property Description : string read FDescription write FDescription;
  end;

  TElHolidays = class(TCollection)
  private
    FOwner : TPersistent;
    function GetItems(Index : integer) : TElHoliday;
    procedure SetItems(Index : integer; newValue : TElHoliday);
  protected
    function GetOwner : TPersistent; override;
    procedure Update(Item : TCollectionItem); override;
  public
    constructor Create(AOwner : TComponent);
    function Add : TElHoliday;
    procedure SaveToStream(Stream : TStream);
    procedure LoadFromStream(Stream : TStream);
    property Items[Index : integer] : TElHoliday read GetItems write SetItems; default;
  end;

implementation

procedure TElHoliday.Assign(Source : TPersistent);
begin
  if Source is TElHoliday then
  begin
    with Source as TElHoliday do
    begin
      Self.FFixedDate := FFixedDate;
      Self.FDay := FDay;
      Self.FDayOfWeek := FDayOfWeek;
      Self.FMonth := FMonth;
      Self.FIsRest := FIsRest;
      Self.FDescription := FDescription;
    end;
  end
  else
    inherited;
end;

constructor TElHoliday.Create;
begin
  inherited;
  FFixedDate := True;
  FDay := 1;
  FMonth := 1;
  Description := 'New Year Day';
  FIsRest := true;
end;

destructor TElHoliday.Destroy;
begin
  inherited;
end;

procedure TElHoliday.SetFixedDate(newValue : Boolean);
begin
  if (FFixedDate <> newValue) then
  begin
    FFixedDate := newValue;
    Self.Changed(False);
  end; {if}
end;

procedure TElHoliday.SetDay(newValue : Word);
begin
  if (FDay <> newValue) then
  begin
    if (newValue > 31) or (newValue < 1) then raise Exception.Create('Day should be between 1 and the number of days in the month');
    FDay := newValue;
    Changed(False);
  end; {if}
end;

procedure TElHoliday.SetDayOfWeek(newValue : Word);
begin
  if (FDayOfWeek <> newValue) then
  begin
    if (newValue > 6) then raise Exception.Create('Day of Week number should be between 0 and 6');
    FDayOfWeek := newValue;
    Changed(False);
  end; {if}
end;

procedure TElHoliday.SetMonth(newValue : Word);
begin
  if (FMonth <> newValue) then
  begin
    if (newValue > 12) or (newValue < 1) then raise Exception.Create('Month number should be between 1 and 12');
    FMonth := newValue;
    Changed(False);
  end; {if}
end;

procedure TElHoliday.SetIsRest(newValue : Boolean);
begin
  if (FIsRest <> newValue) then
  begin
    FIsRest := newValue;
    Changed(False);
  end; {if}
end;

procedure TElHoliday.SaveToStream(Stream : TStream);
begin
  Stream.WriteBuffer(FIsRest, SizeOf(FIsRest));
  Stream.WriteBuffer(FMonth, SizeOf(FMonth));
  Stream.WriteBuffer(FDayOfWeek, SizeOf(FDayOfWeek));
  Stream.WriteBuffer(FDay, SizeOf(FDay));
  Stream.WriteBuffer(FFixedDate, SizeOf(FFixedDate));
  WriteStringToStream(Stream, FDescription);
end;

procedure TElHoliday.LoadFromStream(Stream : TStream);
begin
  Stream.ReadBuffer(FIsRest, SizeOf(FIsRest));
  Stream.ReadBuffer(FMonth, SizeOf(FMonth));
  Stream.ReadBuffer(FDayOfWeek, SizeOf(FDayOfWeek));
  Stream.ReadBuffer(FDay, SizeOf(FDay));
  Stream.ReadBuffer(FFixedDate, SizeOf(FFixedDate));
  ReadStringFromStream(Stream, FDescription);
end;

function TElHolidays.GetItems(Index : integer) : TElHoliday;
begin
  Result := TElHoliday(inherited GetItem(Index));
end;

procedure TElHolidays.SetItems(Index : integer; newValue : TElHoliday);
begin
  inherited SetItem(Index, newValue);
end;

function TElHolidays.GetOwner : TPersistent;
begin
  Result := FOwner;
end;

procedure TElHolidays.Update(Item : TCollectionItem);
begin
  if Assigned(FOwner) and (FOwner is TControl) and (not (csDestroying in TControl(FOwner).ComponentState)) then
    TControl(FOwner).Invalidate;
end;

function TElHolidays.Add : TElHoliday;
begin
  Result := TElHoliday(inherited Add);
end;

procedure TElHolidays.SaveToStream(Stream : TStream);
var
  i : integer;
begin
  i := Count;
  Stream.WriteBuffer(i, SizeOf(integer));
  for i := 0 to Count - 1 do // Iterate
  begin
    Items[i].SaveToStream(Stream);
  end; // for
end;

procedure TElHolidays.LoadFromStream(Stream : TStream);
var
  i, j : integer;
  AHoliday : TElHoliday;
begin
  Clear;
  Stream.ReadBuffer(i, SizeOf(integer));
  for j := 0 to i - 1 do // Iterate
  begin
    AHoliday := Add;
    AHoliday.LoadFromStream(Stream);
  end; // for
end;

constructor TElHolidays.Create(AOwner : TComponent);
begin
  FOwner := AOwner;
  inherited Create(TElHoliday);
end;

end.
 
