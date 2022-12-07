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

unit ElCalendDlg;

{$R *.DFM}

interface

uses
  Windows, Dialogs, SysUtils, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElCalendarDefs,
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  ElCalendar,
  ElACtrls,
  ElSpin,
  ElCombos,
  ElBtnCtl,
{$else}
  ComCtrls,
  CommCtrl,
{$endif}
  ElPopBtn,
  ElPanel,
  ElList,
  ExtCtrls,
  ElVCLUtils, ElXPThemedControl;

type
  TElCalendarForm = class(TForm)
    Panel2 : TPanel;
    TodayBtn : TElPopupButton;
    OkBtn : TElPopupButton;
    CancelBtn : TElPopupButton;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure TodayBtnClick(Sender : TObject);
    procedure OkBtnClick(Sender : TObject);
    procedure FormClose(Sender : TObject; var Action : TCloseAction);
    procedure CancelBtnClick(Sender : TObject);
    procedure CalendarChange(Sender : TObject);
    procedure CalendarClick(Sender: TObject);
  private
    FOnDeactivate : TNotifyEvent;
    FOnChange : TNotifyEvent;

    procedure WMQueryEndSession(var Message : TMessage); message WM_QueryEndSession;
    procedure WMActivate(var Msg : TWMActivate); message WM_ACTIVATE;
{$ifdef CALENDAR_USE_WINDOWS_CALENDAR}
    procedure WMNotify(var Msg : TWMNotify); message WM_NOTIFY;
{$endif}
  protected
    procedure TriggerChangeEvent; virtual;
  public
    IsModal : boolean;
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
    Calendar: TElCalendar;
    Panel1: TElPanel;
    PrevMonBtn: TElPopupButton;
    PrevYearBtn: TElPopupButton;
    NextMonBtn: TElPopupButton;
    NextYearBtn: TElPopupButton;
    YearSpin: TElSpinEdit;
    MonthCombo: TElAdvancedComboBox;

    procedure PrevYearBtnClick(Sender : TObject);
    procedure PrevMonBtnClick(Sender : TObject);
    procedure MonthComboChange(Sender : TObject);
    procedure NextMonBtnClick(Sender : TObject);
    procedure NextYearBtnClick(Sender : TObject);
    procedure YearSpinChange(Sender : TObject);
{$else}
    Calendar : TMonthCalendar;
{$endif}
    procedure SetNames;
    procedure UpdateLabel;
    destructor Destroy; override;
  published
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnDeactivate : TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

type
  TElCalendarDialog = class(TComponent)
  private
    FDate : TDateTime;
    FShowHolidays : Boolean;
    FShowPeriods : Boolean;
    FStartOfWeek : TDayOfWeek;
    FUseCurrentDate : Boolean;
    FGridLineWidth : Integer;
    FShowWeekNum : Boolean;
    FWeekEndDays : TElWeekEndDays;
    FPeriodStart : TDateTime;
    FPeriodLength : Integer;
    FPeriodInterval : Integer;
    FPeriodColor : TColor;
    FHolidayColor : TColor;
    FWeekEndColor : TColor;
    FUseSystemStartOfWeek: Boolean;
    FHolidays : TElHolidays;

    FOnChange : TNotifyEvent;

    procedure PrepareDialog(FrmDialogComponent : TElCalendarForm);
  protected
    FSelectionBorder: TElFlatBorderType;
    FDayCellBorder: TElFlatBorderType;
    FCurrentDayBorder: TElFlatBorderType;
    FUseLineColors: Boolean;
    FLineColorDark: TColor;
    FLineColorLight: TColor;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute : boolean;
  published
    property Date : TDateTime read FDate write FDate;
    property ShowHolidays : Boolean read FShowHolidays write FShowHolidays default true;
    property ShowPeriods : Boolean read FShowPeriods write FShowPeriods;
    property StartOfWeek : TDayOfWeek read FStartOfWeek write FStartOfWeek;
    property UseCurrentDate : Boolean read FUseCurrentDate write FUseCurrentDate;
    property GridLineWidth : Integer read FGridLineWidth write FGridLineWidth default 1;
    property ShowWeekNum : Boolean read FShowWeekNum write FShowWeekNum default false;
    property WeekEndDays : TElWeekEndDays read FWeekEndDays write FWeekEndDays;
    property PeriodStart : TDateTime read FPeriodStart write FPeriodStart;
    property PeriodLength : Integer read FPeriodLength write FPeriodLength default 1;
    property PeriodInterval : Integer read FPeriodInterval write FPeriodInterval default 28;
    property PeriodColor : TColor read FPeriodColor write FPeriodColor default clAqua;
    property HolidayColor : TColor read FHolidayColor write FHolidayColor;
    property WeekEndColor : TColor read FWeekEndColor write FWeekEndColor default clBtnText;
    property Holidays : TElHolidays read FHolidays;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property UseSystemStartOfWeek: Boolean read FUseSystemStartOfWeek write
        FUseSystemStartOfWeek;
    property SelectionBorder: TElFlatBorderType read FSelectionBorder write
        FSelectionBorder;
    property DayCellBorder: TElFlatBorderType read FDayCellBorder write 
        FDayCellBorder;
    property CurrentDayBorder: TElFlatBorderType read FCurrentDayBorder write 
        FCurrentDayBorder;
    property UseLineColors: Boolean read FUseLineColors write FUseLineColors;
    property LineColorDark: TColor read FLineColorDark write FLineColorDark;
    property LineColorLight: TColor read FLineColorLight write FLineColorLight;
  end;

var
  FormList : TElList;

implementation

procedure TElCalendarDialog.PrepareDialog(FrmDialogComponent : TElCalendarForm);
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
var
  ADay, AMonth, AYear : word;
{$endif}
begin
  with FrmDialogComponent do
  begin
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
    DecodeDate(Date, AYear, AMonth, ADay);
    Calendar.Day := ADay;
    Calendar.Month := AMonth;
    Calendar.Year := AYear;
    Calendar.ShowHolidays := FShowHolidays;
    Calendar.ShowPeriods := FShowPeriods;
    Calendar.StartOfWeek := FStartOfWeek;
    Calendar.UseSystemStartOfWeek := FUseSystemStartOfWeek;
    Calendar.UseCurrentDate := FUseCurrentDate;
    Calendar.GridLineWidth := GridLineWidth;
    Calendar.ShowWeekNum := ShowWeekNum;
    Calendar.WeekEndDays := WeekEndDays;
    Calendar.PeriodStart := PeriodStart;
    Calendar.PeriodLength := PeriodLength;
    Calendar.PeriodInterval := PeriodInterval;
    Calendar.PeriodColor := PeriodColor;
    Calendar.HolidayColor := HolidayColor;
    Calendar.WeekEndColor := WeekEndColor;
    Calendar.Holidays.Assign(FHolidays);
    Calendar.SelectionBorder := SelectionBorder;
    Calendar.DayCellBorder := DayCellBorder;
    Calendar.CurrentDayBorder := CurrentDayBorder;
    Calendar.UseLineColors := UseLineColors;
    Calendar.LineColorDark := LineColorDark;
    Calendar.LineColorLight := LineColorLight;
{$else}
    Calendar.Date := Date;
{$endif}
  end;
end;

function TElCalendarDialog.Execute : boolean;
var
  FrmDialogComponent : TElCalendarForm;
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  ADay, AMonth, AYear : word;
{$endif}
begin
  result := false;
  FrmDialogComponent := TElCalendarForm.Create(self);
  try
    PrepareDialog(FrmDialogComponent);
    FrmDialogComponent.IsModal := true;
    FrmDialogComponent.OnChange := OnChange;
    if (FrmDialogComponent.ShowModal = mrOk) then
      result := true;

{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
    ADay := FrmDialogComponent.Calendar.Day;
    AMonth := FrmDialogComponent.Calendar.Month;
    AYear := FrmDialogComponent.Calendar.Year;
    FDate := EncodeDate(AYear, AMonth, ADay);
{$else}
    FDate := FrmDialogComponent.Calendar.Date;
{$endif}
  finally
    FrmDialogComponent.Free;
  end;
end;

destructor TElCalendarDialog.Destroy;
begin
  FHolidays.Free;
  inherited Destroy;
end;

constructor TElCalendarDialog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FHolidays := TElHolidays.Create(nil);
  FShowWeekNum := False;
  FWeekEndDays := [Sat, Sun];
  FWeekEndColor := clBtnText;
  FGridLineWidth := 1;
  FShowHolidays := True;
  FPeriodLength := 1;
  FPeriodInterval := 28;
  FPeriodColor := clAqua;
  FDate := Now;
end;

destructor TElCalendarForm.Destroy;
begin
  FormList.Remove(Self);
  inherited;
end;

{$ifdef CALENDAR_USE_WINDOWS_CALENDAR}
procedure TElCalendarForm.WMNotify(var Msg : TWMNotify);
begin
  if (Msg.NMHdr.code = MCN_SELECT) then
  begin
    TriggerChangeEvent;
  end;
  inherited;
end;
{$endif}

procedure TElCalendarForm.WMActivate(var Msg : TWMActivate);
begin
  inherited;
  if Msg.Active = WA_INACTIVE then
    if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

procedure TElCalendarForm.WMQueryEndSession(var Message : TMessage);
begin
  inherited;
  Message.Result := 1;
end;

procedure TElCalendarForm.TriggerChangeEvent;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TElCalendarForm.SetNames;
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
var
  i : integer;
{$endif} 
begin
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  for i := 1 to 12 do
    MonthCombo.Items.Add(FormatSettings.LongMonthNames[i]);
{$endif}
end;

procedure TElCalendarForm.UpdateLabel;
begin
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  MonthCombo.ItemIndex := Calendar.Month - 1;
  YearSpin.Value := Calendar.Year;
{$endif}
end;

{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
procedure TElCalendarForm.PrevYearBtnClick(Sender : TObject);
begin
  Calendar.Year := Calendar.Year - 1;
  UpdateLabel;
end;

procedure TElCalendarForm.PrevMonBtnClick(Sender : TObject);
begin
  if Calendar.Month = 1 then
  begin
    Calendar.Month := 12;
    Calendar.Year := Calendar.Year - 1;
  end
  else
    Calendar.Month := Calendar.Month - 1;
  UpdateLabel;
end;
{$ENDIF}

procedure TElCalendarForm.FormCreate(Sender : TObject);
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
var Bmp : TBitmap;
{$endif}
begin
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  Calendar := TElCalendar.Create(Self);
{$else}
  Calendar := TMonthCalendar.Create(Self);
{$endif}
  with Calendar do
  begin
    Name := 'Calendar';
    Parent := Self;
    Left := 0;
    Top := 22;
    Width := 194;
    Height := 100;

{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
    Align := alClient;
    Flat := True;
    GridLineWidth := 0;
    StartOfWeek := 0;
    WeekEndDays := [Sun, Sat];
    WeekEndColor := clRed;
    ShowPeriods := False;
    HolidayColor := clBlack;
    ActiveBorderType := fbtNone;
    InactiveBorderType := fbtNone;
    BorderStyle := bsNone;
    UserNavigation := False;
    LineColorDark := clWindow;
{$else}
    TodayBtn.Visible := false;
{$endif}
    TabOrder := 1;
    OnClick := CalendarClick;
    OnChange := CalendarChange;
{$IFDEF VCL_4_USED}
    DockOrientation := doNoOrient;
    DoubleBuffered := False;
{$ENDIF}
  end;

{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  Panel1 := TElPanel.Create(Self);
  PrevMonBtn := TElPopupButton.Create(Self);
  PrevYearBtn := TElPopupButton.Create(Self);
  NextMonBtn := TElPopupButton.Create(Self);
  NextYearBtn := TElPopupButton.Create(Self);
  YearSpin := TElSpinEdit.Create(Self);
  MonthCombo := TElAdvancedComboBox.Create(Self);
  with Panel1 do
  begin
    Name := 'Panel1';
    Parent := Self;
    Left := 0;
    Top := 0;
    Width := 194;
    Height := 21;
    Align := alTop;
    BevelOuter := bvNone;
    TabOrder := 0;
  end;
  Bmp := TBitmap.Create;
  Bmp.Width := 10;
  Bmp.Height := 8;
  Bmp.PixelFormat := pf1Bit;
  try
    with PrevMonBtn do
    begin
      Name := 'PrevMonBtn';
      Parent := Panel1;
      Left := 14;
      Top := 0;
      Width := 14;
      Height := 21;
      Hint := 'Previous month';
      ImageIndex := 0;
      Flat := true;
	  DrawArrow(Bmp.Canvas, eadLeft, Rect(0,0,10,8), clBtnText, true);
      Glyph.Assign(Bmp);
      ShadowBtnHighlight := 16250869;
      ShadowBtnShadow := 7764576;
      ShadowBtnDkShadow := 5856328;
      ShowText := false;
      ShowGlyph := true;
      ShowFocus := False;
      TextDrawType := tdtNormal;
      Transparent := False;
      TabStop := False;
      TabOrder := 0;
      Align := alLeft;
      OnClick := PrevMonBtnClick;
    end;
    with PrevYearBtn do
    begin
      Name := 'PrevYearBtn';
      Parent := Panel1;
      Left := 0;
      Top := 0;
      Width := 14;
      Height := 22;
      Hint := 'Previous year';
      ImageIndex := 0;
      Flat := true;
      Bmp.Assign(nil);
      Bmp.Width := 10;
      Bmp.Height := 8;
      DrawArrow(Bmp.Canvas, eadLeft, Rect(2,0,10,8), clBtnText, true);
      DrawArrow(Bmp.Canvas, eadLeft, Rect(-2,0,6,8), clBtnText, true);
      Glyph := Bmp;
      ShowText := false;
      ShowGlyph := true;
      ShadowBtnHighlight := 16250869;
      ShadowBtnShadow := 7764576;
      ShadowBtnDkShadow := 5856328;
      ShowFocus := False;
      TextDrawType := tdtNormal;
      Transparent := False;
      TabStop := False;
      TabOrder := 1;
      Align := alLeft;
      OnClick := PrevYearBtnClick;
    end;
    with NextMonBtn do
    begin
      Name := 'NextMonBtn';
      Parent := Panel1;
      Left := 166;
      Top := 0;
      Width := 14;
      Height := 21;
      Hint := 'Next month';
      ImageIndex := 0;
      Flat := true;
      Bmp.Assign(nil);
      Bmp.Width := 10;
      Bmp.Height := 8;
      DrawArrow(Bmp.Canvas, eadRight, Rect(0,0,10,8), clBtnText, true);
      Bmp.Transparent := true;
      Glyph := Bmp;
      ShowText := false;
      ShowGlyph := true;
      ShadowBtnHighlight := 16250869;
      ShadowBtnShadow := 7764576;
      ShadowBtnDkShadow := 5856328;
      ShowFocus := False;
      TextDrawType := tdtNormal;
      Transparent := False;
      TabStop := False;
      TabOrder := 2;
      Align := alRight;
      OnClick := NextMonBtnClick;
    end;
    with NextYearBtn do
    begin
      Name := 'NextYearBtn';
      Parent := Panel1;
      Left := 180;
      Top := 0;
      Width := 14;
      Height := 21;
      Hint := 'Next year';
      ImageIndex := 0;
      Flat := true;
      Bmp.Assign(nil);
      Bmp.Width := 10;
      Bmp.Height := 8;
      DrawArrow(Bmp.Canvas, eadRight, Rect(0,0,6,8), clBtnText, true);
      DrawArrow(Bmp.Canvas, eadRight, Rect(4,0,10,8), clBtnText, true);
      Glyph := Bmp;
      ShowText := false;
      ShowGlyph := true;
      ShadowBtnHighlight := 16250869;
      ShadowBtnShadow := 7764576;
      ShadowBtnDkShadow := 5856328;
      ShowFocus := False;
      TextDrawType := tdtNormal;
      Transparent := False;
      TabStop := False;
      TabOrder := 3;
      Align := alRight;
      OnClick := NextYearBtnClick;
    end;
  finally
    Bmp.Free;
  end;
  with YearSpin do
  begin
    Name := 'YearSpin';
    Parent := Panel1;
    Left := 109;
    Top := 0;
    Width := 57;
    Height := 21;
    Value := 1;
    MaxValue := 4636;
    MinValue := 1;
    Increment := 1;
    LargeIncrement := 10;
    Align := alRight;
    AutoSize := False;
    Ctl3D := True;
    Flat := True;
	Color := clBtnFace;
    //InactiveBorderType := fbtColorLineBorder;
    //ActiveBorderType := fbtColorLineBorder;
    LineBorderActiveColor := clBtnFace;
    LineBorderInactiveColor := clBtnFace;
    ButtonFlat := true;
	BorderStyle := bsNone;
    ParentCtl3D := False;
    TabOrder := 5;
    OnChange := YearSpinChange;
  end;
  with MonthCombo do
  begin
    Name := 'MonthCombo';
    Parent := Panel1;
    Left := 28;
    Top := 0;
    Width := 81;
    Height := 21;
    Style := csDropDownList;
    DropDownCount := 12;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -11;
    Font.Name := 'MS Sans Serif';
    Font.Style := [];
    ItemHeight := 13;
    ParentFont := False;
    TabOrder := 4;
    OnChange := MonthComboChange;
    Align := alClient;
    Flat := True;
    BtnFlat := true;
    Color := clBtnFace;
    ActiveBorderType := fbtNone;
    InactiveBorderType := fbtNone;
    //BorderStyle := bsSingle;
  end;
{$ENDIF}

  SetNames;
end;

{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
procedure TElCalendarForm.MonthComboChange(Sender : TObject);
begin
  if (MonthCombo.ItemIndex >= 0) and (MonthCombo.ItemIndex < 12) then
    Calendar.Month := MonthCombo.ItemIndex + 1;
end;

procedure TElCalendarForm.NextMonBtnClick(Sender : TObject);
begin
  if Calendar.Month = 12 then
  begin
    Calendar.Month := 1;
    Calendar.Year := Calendar.Year + 1;
  end
  else
    Calendar.Month := Calendar.Month + 1;
  UpdateLabel;
end;

procedure TElCalendarForm.NextYearBtnClick(Sender : TObject);
begin
  Calendar.Year := Calendar.Year + 1;
  UpdateLabel;
end;

procedure TElCalendarForm.YearSpinChange(Sender : TObject);
var
  FSaveYear : integer;
begin
  FSaveYear := Calendar.Year;
  try
    Calendar.Year := Trunc(YearSpin.Value);
  except
    MessageDlg('The entered year is invalid', mtError, [mbOk], 0);
    Calendar.Year := FSaveYear;
  end;
end;
{$ENDIF}

procedure TElCalendarForm.FormShow(Sender : TObject);
begin
{$ifdef CALENDAR_USE_WINDOWS_CALENDAR}
  Calendar.SetBounds(0, 0, 0, 0);
  Height := Calendar.Height + GetSystemMetrics(SM_CYEDGE) * 2;
  Width := Calendar.Width + GetSystemMetrics(SM_CXEDGE) * 2;
{$endif}
  UpdateLabel;
end;

procedure TElCalendarForm.TodayBtnClick(Sender : TObject);
var
  Time : TSystemTime;
begin
  DateTimeToSystemTime(Now, Time);
{$ifdef CALENDAR_USE_WINDOWS_CALENDAR}
  Calendar.Date := Now;
{$else}
  Calendar.Day := Time.wDay;
  Calendar.Month := Time.wMonth;
  Calendar.Year := Time.wYear;
{$endif}
  UpdateLabel;
end;

procedure TElCalendarForm.OkBtnClick(Sender : TObject);
begin
  if not IsModal then Close;
end;

procedure TElCalendarForm.FormClose(Sender : TObject;
  var Action : TCloseAction);
begin
  Action := caFree;
end;

procedure TElCalendarForm.CancelBtnClick(Sender : TObject);
begin
  if not IsModal then Close;
end;

procedure TElCalendarForm.CalendarChange(Sender : TObject);
begin
  UpdateLabel;
end;

procedure TElCalendarForm.CalendarClick(Sender: TObject);
begin
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  TriggerChangeEvent;
{$endif}
end;

initialization
  FormList := TElList.Create;
finalization
  FormList.Free;

end.

