
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

(*

Version History

06/15/2002

  Added support for power events -- before the change suspending the 
  system caused incorrect behaviour after operations were resumed

04/06/2002

  Setting Precise property to true could cause an AV on application exit

12/16/2000

  Precise property added to ElTimerPool. Now when Precise is false, regular 
  timer is used to track timer ticks. 

*)
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

unit ElTimers;

interface

uses
  Windows, Messages, Classes;

type
  {:
  }
  TCustomElTimer = class (TObject)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOneTime: Boolean;
    FOnTimer: TNotifyEvent;
    FTag: Integer;
    procedure SetInterval(const Value: Cardinal);
  protected
    procedure DoTick; virtual;
    procedure DoTimer; virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OneTime: Boolean read FOneTime write FOneTime;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property Tag: Integer read FTag write FTag;
  public
    constructor Create; virtual;
  end;

  {:
  }
  TElTimer = class (TCustomElTimer)
  private
    FTimerID: Integer;
    FWnd: HWND;
    procedure WndProc(var Msg: TMessage);
  protected
    procedure SetEnabled(const Value: boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Enabled;
    property Interval;
    property OneTime;
    property OnTimer;
    property Tag;
  end;

  TElTimerPool = class;
  TElTimerPoolItem = class;

  {:
  }
  TElPoolTimer = class (TCustomElTimer)
  private
    FElapsed: Cardinal;
    FOwner: TElTimerPoolItem;
  protected
    procedure SetEnabled(const Value: boolean); override;
  public
    procedure Tick(TickCount : integer);
    property Elapsed: Cardinal read FElapsed write FElapsed;
    property Enabled;
    property Interval;
    property OneTime;
    property OnTimer;
    property Owner: TElTimerPoolItem read FOwner;
    property Tag;
  end;

  {:
  }
  TElTimerPoolItem = class (TCollectionItem)
  private
    FTimer: TElPoolTimer;
    FOnTimer: TNotifyEvent;
    function GetEnabled: Boolean;
    function GetInterval: Cardinal;
    function GetOneTime: Boolean;
    function GetOnTimer: TNotifyEvent;
    function GetTag: Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetOneTime(const Value: Boolean);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetTag(const Value: Integer);

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Timer: TElPoolTimer read FTimer;
  published
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
    property OneTime: Boolean read GetOneTime write SetOneTime;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    property Tag: Integer read GetTag write SetTag;
  end;

  {:
  }
  TElTimerPoolItems = class (TCollection)
  private
    FOwner: TElTimerPool;
    function GetItem(Index: integer): TElTimerPoolItem;
    procedure SetItem(Index: integer; const Value: TElTimerPoolItem);
  protected
    {$ifndef D_2}
    function GetOwner: TPersistent; override;
    {$endif}
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TElTimerPool);
    function Add: TElTimerPoolItem;
    property Items[Index: integer]: TElTimerPoolItem read GetItem write SetItem;
        default;
  end;

  {:
  }
  TElTimerPool = class (TComponent)
  private
    FEnableCount : integer;
    FItems: TElTimerPoolItems;
    FTimerID: Integer;
    FLastTick: DWORD;
    FPrecise : boolean;
    FWnd: HWND;
    FBlockEvents : boolean;
    procedure SetItems(Value: TElTimerPoolItems);
    procedure WndProc(var Msg: TMessage);
  protected
    procedure SetPrecise(newValue : boolean); virtual;
    procedure EnableTimer(Enable: boolean); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Tick(TickCount : integer);
  published
    property Items   : TElTimerPoolItems read FItems write SetItems;
    property Precise : boolean read FPrecise write SetPrecise default false;
  end;

implementation

uses
  MMSystem, ElTools;

const
  PBT_APMQUERYSUSPEND             = 0000;
  PBT_APMQUERYSTANDBY             = 0001;
  PBT_APMQUERYSUSPENDFAILED       = 0002;
  PBT_APMQUERYSTANDBYFAILED       = 0003;

  PBT_APMSUSPEND                  = 0004;
  PBT_APMSTANDBY                  = 0005;
  PBT_APMRESUMECRITICAL           = 0006;
  PBT_APMRESUMESUSPEND            = 0007;
  PBT_APMRESUMESTANDBY            = 0008;
  PBTF_APMRESUMEFROMFAILURE       = 00000001;

  PBT_APMBATTERYLOW               = 0009;
  PBT_APMPOWERSTATUSCHANGE        = $000A;
  PBT_APMOEMEVENT                 = $000B;
  PBT_APMRESUMEAUTOMATIC          = $0012;

const
{$IFDEF VCL_4_USED}
  MMTimeMessageID : Cardinal = 0;
{$ELSE}
  MMTimeMessageID : integer = 0;
{$ENDIF}

{ TCustomElTimer }

{
******************************** TCustomElTimer ********************************
}
constructor TCustomElTimer.Create;
begin
  inherited;
  FInterval := 1000;
end;

procedure TCustomElTimer.DoTick;
begin
  DoTimer;
  if OneTime then
     Enabled := False;
end;

procedure TCustomElTimer.DoTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TCustomElTimer.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
     FEnabled := Value;
end;

procedure TCustomElTimer.SetInterval(const Value: Cardinal);
begin
  if (FInterval <> Value) and (Value > 0) then
  begin
    FInterval := Value;
    if Enabled then
    begin
      Enabled := False;
      Enabled := True;
    end;  
  end;
end;

{ TElTimer }

{:
}
{
*********************************** TElTimer ***********************************
}
constructor TElTimer.Create;
begin
  inherited;
end;

destructor TElTimer.Destroy;
begin
  Enabled := False;
  inherited;
end;

procedure TElTimer.SetEnabled(const Value: boolean);
begin
  if Enabled <> Value then
  begin
    if Value then
    begin
      if FWnd = 0 then 
        FWnd := XAllocateHWND(Self, WndProc);
      FTimerID := SetTimer(FWnd, Cardinal(Self), FInterval, nil)
    end
    else
    begin
      KillTimer(FWnd, FTimerID);
      if FWnd <> 0 then 
        XDeallocateHWND(FWnd);
      FWnd := 0;
     end;
    inherited;
  end;
end;

procedure TElTimer.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_TIMER then
    DoTick
  else
    Msg.Result := DefWindowProc(FWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

{ TElTimerPool }

procedure HiResTimerCallbackProc(uID : integer; uMsg : integer; dwUser, dw1,
    dw2 : DWORD); stdcall;
begin
  PostMessage(TElTimerPool(dwUser).FWnd, MMTimeMessageID, 0, 0);
end;

{:
}
{
********************************* TElTimerPool *********************************
}
constructor TElTimerPool.Create(AOwner: TComponent);
begin
  inherited;
  FWnd := XAllocateHWND(Self, WndProc);
  FItems := TElTimerPoolItems.Create(Self);
  FTimerID := -1;
  FPrecise := false;
end;

destructor TElTimerPool.Destroy;
begin
  {
  if (FTimerID <> -1) then
    timeKillEvent(FTimerID);
  FTimerID := -1;
  }
  FItems.Free;
  FEnableCount := 0;
  EnableTimer(false);
  XDeallocateHWND(FWnd);
  inherited;
end;

procedure TElTimerPool.SetPrecise(newValue : boolean);
begin
  if newValue = FPrecise then exit;
  if FTimerID = - 1 then
     FPrecise := newValue
  else
  begin
    if newValue then
    begin
      KillTimer(FWnd, 1234);
      FTimerID := timeSetEvent(1, 1, @HiResTimerCallbackProc, Cardinal(Self),
            TIME_PERIODIC);
    end
    else
    begin
      timeKillEvent(FTimerID);
      FTimerID := SetTimer(FWnd, 1234, 10, nil);
      FLastTick:= timeGetTime;
    end;
    FPrecise := newValue;
  end;
end;

procedure TElTimerPool.EnableTimer(Enable: boolean);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    exit;
  end;
  if Enable then
  begin
    if FPrecise then
    begin
      if FTimerID = -1 then
        FTimerID := timeSetEvent(1, 1, @HiResTimerCallbackProc, Cardinal(Self),
            TIME_PERIODIC);
    end
    else
    begin
      if FTimerID = -1 then
      begin
        FTimerID := SetTimer(FWnd, 1234, 10, nil);
        FLastTick:= timeGetTime;
      end;
    end;
    inc(FEnableCount);
  end
  else
  if (FTimerID <> -1) then
  begin
    if (FEnableCount <= 0) then
    begin
      if Precise then
      begin
        timeKillEvent(FTimerID);
        Sleep(0);
      end
      else
        KillTimer(FWnd, 1234);

      FTimerID := -1;
    end;
    dec(FEnableCount);
  end;
end;

procedure TElTimerPool.SetItems(Value: TElTimerPoolItems);
begin
  FItems.Assign(Value);
end;

procedure TElTimerPool.Tick(TickCount : integer);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems.Items[I].FTimer.Tick(TickCount);
end;

procedure TElTimerPool.WndProc(var Msg: TMessage);
var i, j : DWORD;
begin
  if Msg.Msg = MMTimeMessageID then
  begin
    if not FBlockEvents then
      Tick(1);
  end
  else
  if Msg.Msg = WM_POWERBROADCAST then
  begin
    if (Msg.wParam = PBT_APMSUSPEND) or
       (Msg.wParam = PBT_APMSTANDBY) then
    begin
      FBlockEvents := true;
    end;
    if (Msg.wParam = PBT_APMRESUMEAUTOMATIC) or
       (Msg.wParam = PBT_APMRESUMECRITICAL) or
       (Msg.wParam = PBT_APMRESUMESUSPEND) then
    begin
      FBlockEvents := false;
      FLastTick := timeGetTime;
    end;
  end
  else
  if Msg.Msg = WM_TIMER then
  begin
    i := timeGetTime;
    j := i;
    if i >= FLastTick then
       i := i - FLastTick
    else
    {$IFDEF VCL_4_USED}
       i := DWORD(Int64(i) - FLastTick);
    {$ELSE}
       i := $FFFFFFFF - FLastTick + i;
    {$ENDIF}
    FLastTick := j;
    if not FBlockEvents then
      Tick(i);
  end
  else
    Msg.Result := DefWindowProc(FWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TElTimerPool.Loaded;
var i : integer;
begin
  inherited;
  for i := 0 to FItems.Count - 1 do
    if Items[i].Enabled then
    begin
      EnableTimer(true);
      break;
    end;
end;

{
******************************* TElTimerPoolItem *******************************
}
constructor TElTimerPoolItem.Create(Collection: TCollection);
begin
  inherited;
  FTimer := TElPoolTimer.Create;
  FTimer.FOwner := Self;
  Interval := 1000;
end;

destructor TElTimerPoolItem.Destroy;
begin
  Enabled := False;
  FTimer.Free;
  inherited;
end;

procedure TElTimerPoolItem.Assign(Source: TPersistent);
begin
  if Source is TElTimerPoolItem then
  begin
    Tag := TElTimerPoolItem(Source).Tag;
    Interval := TElTimerPoolItem(Source).Interval;
    OneTime := TElTimerPoolItem(Source).OneTime;
    Enabled := TElTimerPoolItem(Source).Enabled;
  end
  else inherited;
end;

function TElTimerPoolItem.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TElTimerPoolItem.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

function TElTimerPoolItem.GetOneTime: Boolean;
begin
  Result := FTimer.OneTime;
end;

function TElTimerPoolItem.GetOnTimer: TNotifyEvent;
begin
  Result := FOnTimer;
end;

function TElTimerPoolItem.GetTag: Integer;
begin
  Result := FTimer.Tag;
end;

procedure TElTimerPoolItem.SetEnabled(const Value: Boolean);
begin
  FTimer.Enabled := Value;
end;

procedure TElTimerPoolItem.SetInterval(const Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TElTimerPoolItem.SetOneTime(const Value: Boolean);
begin
  FTimer.OneTime := Value;
end;

procedure TElTimerPoolItem.SetOnTimer(const Value: TNotifyEvent);
begin
  FTimer.OnTimer := Value;
  FOnTimer := Value;
end;

procedure TElTimerPoolItem.SetTag(const Value: Integer);
begin
  FTimer.Tag := Value;
end;

{ TElTimerPoolItems }

{:
}
{
****************************** TElTimerPoolItems *******************************
}
constructor TElTimerPoolItems.Create(AOwner: TElTimerPool);
begin
  inherited Create(TElTimerPoolItem);
  FOwner := AOwner;
end;

function TElTimerPoolItems.Add: TElTimerPoolItem;
begin
  Result := TElTimerPoolItem(inherited Add);
end;

function TElTimerPoolItems.GetItem(Index: integer): TElTimerPoolItem;
begin
  Result := TElTimerPoolItem(inherited GetItem(Index));
end;

{$ifndef D_2}
function TElTimerPoolItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;
{$endif}

procedure TElTimerPoolItems.SetItem(Index: integer; const Value: 
    TElTimerPoolItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TElTimerPoolItems.Update(Item: TCollectionItem);
begin
  FOwner.EnableTimer(Count > 0);
end;

{ TElPoolTimer }

{:
}
{
********************************* TElPoolTimer *********************************
}

procedure TElPoolTimer.SetEnabled(const Value: boolean);
begin
  if Value <> Enabled then
  begin
    inherited;
    if csDesigning in TElTimerPoolItems(FOwner.Collection).FOwner.ComponentState then
      exit;
    TElTimerPoolItems(FOwner.Collection).FOwner.EnableTimer(Value);
    if Value then
       FElapsed := 0;
  end;
end;

procedure TElPoolTimer.Tick(TickCount : integer);
var i : integer;
begin
  if Enabled then
  begin
    Inc(FElapsed, TickCount);
    if FElapsed >= Interval then
    begin
      for i := 1 to FElapsed div Interval do
      begin
        DoTick;
        if not Enabled then
           break;
      end;
      FElapsed := 0;
    end;
  end;
end;

{ TElTimerPoolItem }

initialization
  MMTimeMessageID := RegisterWindowMessage('ElPack Timer pool tick message');

end.
 
