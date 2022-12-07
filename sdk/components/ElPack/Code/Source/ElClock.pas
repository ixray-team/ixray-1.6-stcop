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

unit ElClock;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, ElTools,
  ElList, ElStrUtils, Graphics, ElPanel, 
{$ifdef VCL_6_USED}
Types,
{$endif}
Menus;

type
  TElClock = class(TElPanel)
  protected
    FTimerPaused : Boolean;
    FStartTime : TDateTime;
    FPauseTime : TDateTime;
    FIsTimer : Boolean;
    FTimerActive : Boolean;
    FShowDate : Boolean;
    FShowHint : boolean;
    FTimer : TTimer;
    FTZone : TTImeZoneInformation;
    FLocal : Boolean;
    FSeconds : boolean;
    FAMPM : boolean;
    FCaption : string;
    FUseBias : Boolean;
    FBias : Integer;
    FShowWeekDay : Boolean;
    FUseCustomFormat : Boolean;
    FCustomFormat : string;
    FShowDaysInTimer : Boolean;
    FCountdownActive : Boolean;
    FOnCountdownDone : TNotifyEvent;
    FOnCountdownTick : TNotifyEvent;
    FCountdownPaused : Boolean;
    FCountdownTime : Integer;
    FSaveCDTime : Integer;
    FIsCountdown : boolean;
    FDummyStr   : TElFString;
    procedure SetIsCountdown(newValue : boolean);
    procedure SetCountdownTime(newValue : Integer);
    procedure SetCountdownPaused(newValue : Boolean);
    procedure SetCountdownActive(newValue : Boolean);
    procedure SetShowDaysInTimer(newValue : Boolean);
    procedure SetUseCustomFormat(newValue : Boolean);
    procedure SetCustomFormat(newValue : string);
    procedure SetShowWeekDay(newValue : Boolean);
    procedure SetUseBias(newValue : Boolean);
    procedure SetBias(newValue : Integer);
    function GetTimer : boolean;
    procedure SetTimer(value : boolean);
    procedure OnTimer(Sender : TObject);
    procedure WMMouseMove(var Msg : TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Msg : TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg : TWMLButtonUp); message WM_LBUTTONUP;
    procedure SetShowDate(newValue : Boolean);
    procedure SetShowHint(newValue : Boolean);
    procedure SetIsTimer(newValue : Boolean);
    function GetTimeElapsed : TDateTime;
    procedure SetTimerActive(newValue : Boolean);
    procedure SetTimerPaused(newValue : Boolean);
    procedure CreateTimer;
    procedure PaintBorders(Canvas : TCanvas; var R : TRect);
    procedure Paint; override;
    procedure InheritedPaint;
    procedure TriggerCountdownDoneEvent; virtual;
    procedure TriggerCountdownTickEvent; virtual;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Kick; virtual;
    procedure GetTime(var Time : TSystemTime); virtual;
    procedure ResetTimer;

    property TimeElapsed : TDateTime read GetTimeElapsed;
    property TimeZone : TTimeZoneInformation read FTZone write FTZone;
  published
    property Caption: TElFString read FDummyStr;
    property LocalTime : boolean read FLocal write FLocal default true;
    property ShowWeekDay : Boolean read FShowWeekDay write SetShowWeekDay;
    property ShowSeconds : boolean read FSeconds write FSeconds;
    property ShowDate : Boolean read FShowDate write SetShowDate; { Published }
    property AM_PM : boolean read FAMPM write FAMPM;
    property Labels : boolean read FShowHint write SetShowHint;
    property UseBias : Boolean read FUseBias write SetUseBias;
    property Bias : Integer read FBias write SetBias;
    property UseCustomFormat : Boolean read FUseCustomFormat write SetUseCustomFormat;
    property CustomFormat : string read FCustomFormat write SetCustomFormat;

    property IsTimer : Boolean read FIsTimer write SetIsTimer;
    property TimerActive : Boolean read FTimerActive write SetTimerActive;
    property TimerPaused : Boolean read FTimerPaused write SetTimerPaused default False;
    property ShowDaysInTimer : Boolean read FShowDaysInTimer write SetShowDaysInTimer;
    property IsCountdown : boolean read FIsCountdown write SetIsCountdown;
    property CountdownTime : Integer read FCountdownTime write SetCountdownTime;
    property CountdownActive : Boolean read FCountdownActive write SetCountdownActive;
    property CountdownPaused : Boolean read FCountdownPaused write SetCountdownPaused;
    property UseTimer : Boolean read GetTimer write SetTimer default true;

    property OnCountdownDone : TNotifyEvent read FOnCountdownDone write FOnCountdownDone;
    property OnCountdownTick : TNotifyEvent read FOnCountdownTick write FOnCountdownTick;

    property Align;
    property Alignment;
    property BevelInner default bvLowered;
    property BevelOuter default bvRaised;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Cursor;
    property Font;
    property Hint;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnResize;
{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    property DockOrientation;
    property Floating;
    property BevelKind;
    property DoubleBuffered;
    property DragKind;
{$ENDIF}
  private
  end;

type
  PShortTZ = ^TShortTZ;
  TShortTZ = packed record
    Bias,
      StandardBias,
      DayLightBias : LongInt;
    wReserved1, // year field
      StdMonth,
      StdDayOfWeek,
      StdDay,
      StdHour,
      StdMinute,
      StdSecond,
      wReserved2, // msec field
      wReserved3, // year field
      DLMonth,
      DLDayOfWeek,
      DLDay,
      DLHour,
      DLMinute,
      DLSecond,
      wReserved4 // msec field
      : word;

  end;

  PTimeZoneInfo = ^TTimeZoneInfo;
  TTimeZoneInfo = record
    KeyName : string;
    DisplayName : string;
    DltName : string;
    StdName : string;
    MapID : string;
    TimeZone : TTimeZoneInformation;
    STimeZone : TShortTZ;
  end;

function RetrieveTimeZoneInfo(TimeZoneInfoList : TElList) : boolean;
procedure ShortTZToTimeZoneInfo(ShortTZ : TShortTZ; var TZInfo : TTimeZoneInfo);
function TranslateTZDate(ADate : TSystemTime) : string;

var
  FDL : boolean;
  SysTimeZones : TElList;

implementation

uses ElVCLUtils;

constructor TElClock.Create(AOwner : TComponent);
begin
  inherited;
  BevelInner := bvLowered;
  BevelOuter := bvRaised;
  FLocal := true;
  UseTimer := true;
  FCustomFormat := 'DD xx';
end;

destructor TElClock.Destroy;
begin
  if FTimer <> nil then
  begin
    FTimer.Free;
    FTimer := nil;
  end;
  inherited;
end;

procedure TElClock.GetTime(var Time : TSystemTime);
var
  LocalTime : TSystemTime;
begin
  if FLocal then
    GetLocalTime(Time)
  else
  begin
    GetSystemTime(LocalTime);
    Time := LocalTime;
    UTCToZoneLocal(@FTZone, LocalTime, Time);
    if UseBias then
      DateTimeToSystemTime(IncTime(SystemTimeToDateTime(Time), 0, FBias, 0, 0), Time);
  end;
end;

procedure TElClock.Kick;
var
  CurTime : TSystemTime;
  Buffer : pchar;
  s, s1, s2 : string;
  DT : TDateTime;
begin
  if IsTimer then
  begin
    if FTimerActive then
    begin
      if FTimerPaused then
        DT := FPauseTime - FStartTime
      else
        DT := Now - FStartTime;
    end
    else
    begin
      DT := 0;
    end;
    if ShowDaysInTimer then
    begin
      FCaption := IntToStr(Round(DT)) + '.';
    end
    else
    begin
      FCaption := '';
    end;
    FCaption := FCaption + TimeToStr(Frac(DT));
    Repaint;
    exit;
  end
  else if IsCountDown then
  begin
    if FCountdownActive then
    begin
      if FCountdownPaused then
        DT := FStartTime - FPauseTime
      else
      begin
        DT := FStartTime - Now;
      end;
      FCountDownTime := Trunc(DT * 86400);
      if not FCountDownPaused then TriggerCountdownTickEvent;
      if (DT = 0) or (Now > FStartTime) then
      begin
        TriggerCountdownDoneEvent;
        CountdownActive := false;
      end;
    end
    else
      DT := CountdownTime / 86400;
    FCaption := TimeToStr(Frac(DT));
    Repaint;
    exit;
  end;
  GetMem(Buffer, 100);
  GetTime(CurTime);
  if UseCustomFormat then
  begin
    DT := SystemTimeToDateTime(CurTime);
    FCaption := ElTools.GetFormattedTimeString(DT, CustomFormat);
  end
  else
  begin
    if not FAMPM then
    begin
      s := IntToStr(CurTime.wHour);
      if Length(s) = 1 then
        S2 := '0' + s
      else
        S2 := s;
      s := IntToStr(CurTime.wMinute);
      if Length(s) = 1 then
        S2 := S2 + ':0' + s
      else
        S2 := S2 + ':' + s;
      if ShowSeconds then
      begin
        s := IntToStr(CurTime.wSecond);
        if Length(s) = 1 then
          S2 := S2 + ':0' + s
        else
          S2 := S2 + ':' + s;
      end;
    end
    else
    begin
      s := IntToStr(CurTime.wHour mod 12);
      if CurTime.wHour = 0 then s := '12';
      S2 := s;
      s := IntToStr(CurTime.wMinute);
      if Length(s) = 1 then
        S2 := S2 + ':0' + s
      else
        S2 := S2 + ':' + s;
      if ShowSeconds then
      begin
        s := IntToStr(CurTime.wSecond);
        if Length(s) = 1 then
          S2 := S2 + ':0' + s
        else
          S2 := S2 + ':' + s;
      end;
      if (CurTime.wHour div 12 = 0) then
        S2 := S2 + ' AM'
      else
        S2 := S2 + ' PM';
    end;
    GetDateFormat(LOCALE_USER_DEFAULT, DATE_SHORTDATE, @CurTime, nil, Buffer, 100);
    S1 := StrPas(Buffer);
    if FPressed then FCaption := S1
    else
    begin
      if FShowWeekDay then CurTime.wDayOfWeek := SysUtils.DayOfWeek(SystemTimeToDateTime(CurTime)) - 1;
      if FShowWeekDay and FShowDate then S1 := FormatSettings.ShortDayNames[CurTime.wDayOfWeek + 1] + ', ' + S1;
      FCaption := S2;
      if FShowDate then
        FCaption := FCaption + #13#10 + S1;
      if FShowWeekDay and (not FShowDate) then FCaption := FCaption + #13#10 + FormatSettings.LongDayNames[CurTime.wDayOfWeek + 1];
    end;
  end;
  if Labels then FCaption := FCaption + #13#10 + Hint;
  FreeMem(Buffer, 100);
end;

procedure TElClock.OnTimer(Sender : TObject);
begin
  if FIsTimer then
  begin
    if (not FTimerPaused) and FTimerActive then Kick;
  end
  else
    Kick;
  Repaint;
end;

procedure TElClock.CreateTimer;
begin
  FTimer := TTimer.Create(self);
  FTimer.Interval := 1000;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := true;
end;

function TElClock.GetTimer : boolean;
begin
  result := Assigned(FTimer);
end;

procedure TElClock.SetTimer(value : boolean);
begin
  if value <> Assigned(FTimer) then
  begin
    if value then
      CreateTimer
    else
    begin
      FTimer.Free;
      FTimer := nil;       
    end;
  end;
end;

procedure TElClock.WMLButtonDown(var Msg : TMessage); { private }
begin
  inherited;
  if (not ShowDate) then
  begin
    Kick;
    Repaint;
  end;
end; { WMLButtonDown }

procedure TElClock.WMLButtonUp(var Msg : TWMLButtonUp); { private }
begin
  inherited;
  Kick;
  Repaint;
end; { WMLButtonUp }

procedure TElClock.SetShowHint(newValue : Boolean);
begin
  if (FShowHint <> newValue) then
  begin
    FShowHint := newValue;
    Kick;
    Repaint;
  end; { if }
end; { SetShowDate }

procedure TElClock.SetShowDate(newValue : Boolean);
begin
  if (FShowDate <> newValue) then
  begin
    FShowDate := newValue;
    Kick;
    Repaint;
  end; { if }
end; { SetShowDate }

procedure TElClock.PaintBorders(Canvas : TCanvas; var R : TRect);
var
  TopColor,
    BottomColor : TColor;

const
  Alignments : array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

  procedure AdjustColors(Bevel : TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, R, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, R, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, R, TopColor, BottomColor, BevelWidth);
  end;
end;

procedure TElClock.InheritedPaint;
begin
  inherited Paint;
end;

procedure TElClock.Paint; { public }
var
  R : TRect;
  AL : integer;
begin
  inherited;
  R := ClientRect;
  if BevelInner <> bvNone then InflateRect(R, -BevelWidth, -BevelWidth);
  if BevelOuter <> bvNone then InflateRect(R, -BevelWidth, -BevelWidth);

  Canvas.Brush.Style := bsClear;
  case Alignment of
    taCenter : AL := DT_CENTER;
    taRightJustify : AL := DT_RIGHT;
  else
    AL := DT_LEFT;
  end;
  if Pos(#13#10, FCaption) = 0 then AL := AL or DT_SINGLELINE;
  DrawText(Canvas.Handle, PChar(FCaption), -1, R, DT_VCENTER or DT_NOCLIP or DT_EXPANDTABS or DT_NOPREFIX or AL);
end; { Paint }

procedure TElClock.SetIsTimer(newValue : Boolean);
begin
  if (FIsTimer <> newValue) then
  begin
    FIsTimer := newValue;
    if FIsTimer then
    begin
      FStartTime := Now;
      isCountdown := false;
    end;
  end; {if}
  Kick;
  Repaint;
end;

function TElClock.GetTimeElapsed : TDateTime;
begin
  result := Now - FStartTime;
end;

procedure TElClock.SetTimerActive(newValue : Boolean);
begin
  if (FTimerActive <> newValue) then
  begin
    FTimerActive := newValue;
    ResetTimer;
  end; {if}
  if IsTimer then
  begin
    Kick;
    Repaint;
  end;
end;

procedure TElClock.ResetTimer;
begin
  FStartTime := Now;
  FPauseTime := FStartTime;
  Kick;
  Repaint;
end;

procedure TElClock.SetTimerPaused(newValue : Boolean);
begin
  if (FTimerPaused <> newValue) then
  begin
    FTimerPaused := newValue;
    if NewValue then
    begin
      FPauseTime := Now;
    end
    else
    begin
      FStartTime := FStartTime + (Now - FPauseTime);
      Kick;
      Repaint;
    end;
  end;
end; {SetTimerPaused}

procedure TElClock.SetUseBias(newValue : Boolean);
begin
  if (FUseBias <> newValue) then
  begin
    FUseBias := newValue;
    if not IsTimer then
    begin
      Kick;
      Repaint;
    end;
  end; {if}
end;

procedure TElClock.SetBias(newValue : Integer);
begin
  if (FBias <> newValue) then
  begin
    FBias := newValue;
    if not IsTimer then
    begin
      Kick;
      Repaint;
    end;
  end; {if}
end;

procedure TElClock.SetShowWeekDay(newValue : Boolean);
begin
  if (FShowWeekDay <> newValue) then
  begin
    FShowWeekDay := newValue;
    if FShowDate then
    begin
      Kick;
      Repaint;
    end;
  end; {if}
end; {SetShowWeekDay}

procedure TElClock.SetUseCustomFormat(newValue : Boolean);
begin
  if (FUseCustomFormat <> newValue) then
  begin
    FUseCustomFormat := newValue;
    Kick;
    Repaint;
  end; {if}
end; {SetUseCustomFormat}

procedure TElClock.SetCustomFormat(newValue : string);
begin
  if (FCustomFormat <> newValue) then
  begin
    FCustomFormat := newValue;
    Kick;
    Repaint;
  end; {if}
end; {SetCustomFormat}

procedure TElClock.SetShowDaysInTimer(newValue : Boolean);
begin
  if (FShowDaysInTimer <> newValue) then
  begin
    FShowDaysInTimer := newValue;
    if FIsTimer then
    begin
      Kick;
      Repaint;
    end;
  end; {if}
end;

procedure TElClock.SetIsCountdown(newValue : boolean);
begin
  if (FIsCountdown <> newValue) then
  begin
    FIsCountdown := newValue;
    if FIsCountdown then
    begin
      FStartTime := Now + CountdownTime / 86400;
      IsTimer := false;
    end;
    Kick;
    Repaint;
  end; {if}
end;

procedure TElClock.SetCountdownTime(newValue : Integer);
begin
  if (FCountdownTime <> newValue) then
  begin
    FCountdownTime := newValue;
    FSaveCDTime := FCountdownTime;
    FStartTime := Now + CountdownTime / 86400;
  end; {if}
end;

procedure TElClock.SetCountdownPaused(newValue : Boolean);
begin
  if (FCountdownPaused <> newValue) then
  begin
    FCountdownPaused := newValue;
    if NewValue then
    begin
      FPauseTime := Now;
    end
    else
    begin
      FStartTime := FStartTime + (Now - FPauseTime);
      Kick;
      Repaint;
    end;
  end; {if}
end;

procedure TElClock.TriggerCountdownDoneEvent;
begin
  if (assigned(FOnCountdownDone)) then FOnCountdownDone(Self);
  CountdownTime := FSaveCDTime;
end;

procedure TElClock.SetCountdownActive(newValue : Boolean);
begin
  if (FCountdownActive <> newValue) then
  begin
    if newValue then
    begin
      FStartTime := Now + CountdownTime / 86400;
      FSaveCDTime := CountdownTime;
    end
    else
      CountdownTime := FSaveCDTime;
    FCountdownActive := newValue;
    Kick;
    Repaint;
  end; {if}
end;

procedure TElClock.TriggerCountdownTickEvent;
{ Triggers the OnCountdownTick event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnCountdownTick)) then
    FOnCountdownTick(Self);
end; { TriggerCountdownTickEvent }

procedure TElClock.WMMouseMove(var Msg : TWMMouseMove); { private }
var
  b : boolean;
begin
  b := Movable and FPressed;
  inherited;
  if b then
  begin
    Kick;
    Repaint;
  end;
end; { WMMouseMove }

procedure ShortTZToTimeZoneInfo(ShortTZ : TShortTZ; var TZInfo : TTimeZoneInfo);
begin
  TZInfo.TimeZone.Bias := ShortTZ.Bias;
  TZInfo.TimeZone.StandardBias := ShortTZ.StandardBias;
  TZInfo.TimeZone.DaylightBias := ShortTZ.DaylightBias;
  MoveMemory(@TZInfo.TimeZone.StandardDate, @(ShortTZ.wReserved1), sizeof(TSystemTime));
  MoveMemory(@TZInfo.TimeZone.DaylightDate, @(ShortTZ.wReserved3), sizeof(TSystemTime));
end;

function TranslateTZDate(ADate : TSystemTime) : string;
const
  ACounts : array[1..5] of string = ('First', 'Second', 'Third', 'Fourth', 'Last');
  ADays : array[0..6] of string = ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
  AMonthes : array[1..12] of string = ('January', 'February', 'March',
    'April', 'May', 'June',
    'July', 'August', 'September',
    'October', 'November', 'December');
begin
  result := ACounts[ADate.wDay] + ' ' + ADays[ADate.wDayOfWeek] + ' of ' + AMonthes[ADate.wMonth] + ' at '
    + TimeToStr(EncodeTime(ADate.wHour, ADate.wMinute, ADate.wSecond, 0));
end;

function RetrieveTimeZoneInfo;
type
  PByte = ^byte;
var
  KeyStr,
    SubKeyStr : string;
  Key,
    SubKey : HKey;
  ptz : PTimeZoneInfo;
  res : DWORD;
  i : integer;
  b1 : PChar;
  b2 : PByte;
  bufsize : DWORD;
  atime : TFileTime;
begin
  {$IFDEF ZONE_DEBUG}
  MessageBox(0, PChar('Reading time zones'), nil, 0);
  {$ENDIF}
  result := false;
  try
    if IsWinNT then
      KeyStr := 'Software\Microsoft\Windows NT\CurrentVersion\Time Zones'
    else
      KeyStr := 'Software\Microsoft\Windows\CurrentVersion\Time Zones';
    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, pchar(KeyStr), 0, KEY_READ, Key) <> ERROR_SUCCESS then
    begin
      MessageBox(0, 'Failed to open TimeZone key', '', 0);
      exit;
    end;
    i := 0;
    repeat
      New(PTZ);
      GetMem(b1, 100);
      BufSize := 100;
      res := RegEnumKeyEx(Key, i, b1 {key name}, BufSize, nil, nil, nil, @atime);
      if res = ERROR_SUCCESS then
      begin
        PTZ.KeyName := StrPas(B1);
        SubKeyStr := PTZ.KeyName;
        if RegOpenKeyEx(Key, PChar(SubKeyStr), 0, KEY_READ, SubKey) = ERROR_SUCCESS then
        begin
          GetMem(b2, 100);
          BufSize := 100;
          if RegQueryValueEx(SubKey, 'MapID', nil, nil, @(byte(B2^)), @BufSize) = ERROR_SUCCESS then PTZ.MapID := StrPas(PChar(B2))
          else
          begin
  {$IFDEF ZONE_DEBUG}
            MessageBox(0, PChar('Failed to retrieve MapID of this zone: ' + SubKeyStr), nil, 0);
  {$ENDIF}
          end;

          BufSize := 100;
          if RegQueryValueEx(SubKey, 'Display', nil, nil, @(byte(B2^)), @BufSize) = ERROR_SUCCESS then PTZ.DisplayName := StrPas(PChar(B2))
          else
          begin
  {$IFDEF ZONE_DEBUG}
            MessageBox(0, PChar('Failed to retrieve Display of this zone: ' + SubKeyStr), nil, 0);
  {$ENDIF}
          end;

          BufSize := sizeof(PTZ.STimeZone);
          if RegQueryValueEx(SubKey, 'TZI', nil, nil, @PTZ.STimeZone, @BufSize) = ERROR_SUCCESS then
            ShortTZToTimeZoneInfo(PTZ.STimeZone, PTZ^)
          else
          begin
  {$IFDEF ZONE_DEBUG}
            MessageBox(0, PChar('Failed to retrieve TZI of this zone: ' + SubKeyStr), nil, 0);
  {$ENDIF}
          end;
          BufSize := 100;
          if RegQueryValueEx(SubKey, 'Dlt', nil, nil, @(byte(B2^)), @BufSize) = ERROR_SUCCESS then
            PTZ.DltName := StrPas(PChar(B2))
          else
          begin
  {$IFDEF ZONE_DEBUG}
            MessageBox(0, PChar('Failed to retrieve Dlt of this zone: ' + SubKeyStr), nil, 0);
  {$ENDIF}
          end;
          BufSize := 100;
          if RegQueryValueEx(SubKey, 'Std', nil, nil, @(byte(B2^)), @BufSize) = ERROR_SUCCESS then
            PTZ.StdName := StrPas(PChar(B2))
          else
          begin
  {$IFDEF ZONE_DEBUG}
            MessageBox(0, PChar('Failed to retrieve Std of this zone: ' + SubKeyStr), nil, 0);
  {$ENDIF}
          end;
          RegCloseKey(SubKey);
          FreeMem(b2);
        end
        else
        begin
{$IFDEF ZONE_DEBUG}
          MessageBox(0, PChar('Failed to retrieve any information at all about this zone: ' + SubKeyStr), nil, 0);
{$ENDIF}
        end;
        TimeZoneInfoList.Add(PTZ);
      end
      else
      begin
{$IFDEF ZONE_DEBUG}
        MessageBox(0, PChar('Failed to query time zone ' + IntToStr(i)), nil, 0);
{$ENDIF}
        Dispose(PTZ);
      end;
      inc(i);
      FreeMem(b1, 100);
    until RES = ERROR_NO_MORE_ITEMS;
    RegCloseKey(Key);
    //MessageBox(0, PChar('Total items: ' + IntToStr(TimeZoneInfoList.Count)), '', 0);
    result := true;
  except
    on E : EOutOfMemory do result := false;
  end;
end;
{$warnings off}
var i : integer;

initialization
  FDL := false;
  SysTimeZones := TElList.Create;
  RetrieveTimezoneInfo(SysTimeZones);

finalization
  for i := 0 to SysTimeZones.Count - 1 do
      Dispose(PTimeZoneInfo(SysTimezones[i]));
  SysTimeZones.Free;

end.
