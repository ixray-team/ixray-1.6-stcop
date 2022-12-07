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

04/10/2002

  Now the week number is shown not for the first day, but for the first monday in row

02/21/2002

  Added handling of Enter key

12/12/2001

  Fixed weekend cells coloring when ShowWeekNum is true 

07/18/2001

  SelectionBorder, DayCellBorder, CurrentDayBorder properties added

07/12/2001

  BorderSides property added.

06/07/2001

  LineColorLight and LineColorDark didn't work. Fixed.
  UseLineColors property added.

09/04/2000

  PeriodInterval = 0 now doesn't freeze the system

*)

unit ElCalendar;

interface

uses
  Classes,
{$ifndef CLX_USED}
  Windows,
  Messages,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  Buttons,
{$ifdef VCL_6_USED}
Types,
{$endif}
  Grids,
{$ifdef VCL_4_USED}
  ImgList,
{$endif}
{$else}
  Types,
  QTypes,
  Qt,
  QGraphics,
  QControls,
  QForms,
  QButtons,
  QImgList,
  QGrids,
  QStdCtrls,
  ElCLXUtils,
{$endif}
  SysUtils,
  ElTools,
  ElList,
  ElVCLUtils,
  ElUxTheme,
  ElTmSchema,
  ElCalendarDefs;

type

  TElCalendar = class;

  TElCalendar = class(TCustomGrid)
  private
    FHolidayColor : TColor;
    FShowPeriods : Boolean;
    FPeriodStart : TDateTime;
    FPeriodLength : Word;
    FPeriodInterval : Word;
    FPeriodColor : TColor;
    FShowHolidays : Boolean;
    FHolidays : TElHolidays;
    FWeekEndColor : TColor;
    FWeekEndDays : TElWeekEndDays;
    FShowWeekNum : Boolean;
    FDate : TDateTime;
    FMonthOffset : Integer;
    FOnChange : TNotifyEvent;
    FReadOnly : Boolean;
    FStartOfWeek : TDayOfWeek;
    FUpdating : Boolean;
    FUserNavigation : boolean;
    FUseCurrentDate : Boolean;
    FTranslateDays : boolean;
    {$ifndef CLX_USED}
    FMouseOver : boolean;
    {$endif}
    FFlat : boolean;
    FActiveBorderType : TElFlatBorderType;
    FInactiveBorderType : TElFlatBorderType;
    FUseLineColors: Boolean;
    FBorderSides: TElBorderSides;
    FSelectionBorder: TElFlatBorderType;
    FDayCellBorder: TElFlatBorderType;
    FCurrentDayBorder: TElFlatBorderType;

    procedure SetActiveBorderType(newValue : TElFlatBorderType);
    procedure SetInactiveBorderType(newValue : TElFlatBorderType);
    function GetCellText(ACol, ARow : Integer) : string;
    function GetDateElement(Index : Integer) : Integer;
    procedure SetCalendarDate(Value : TDateTime);
    procedure SetDateElement(Index : Integer; Value : Integer);
    procedure SetStartOfWeek(Value : TDayOfWeek);
    procedure SetUseCurrentDate(Value : Boolean);
    function StoreCalendarDate : Boolean;
    procedure SetShowWeekNum(newValue : Boolean);
    procedure SetWeekEndDays(newValue : TElWeekEndDays);
    procedure SetWeekEndColor(newValue : TColor);
    {$ifndef CLX_USED}
    procedure CMHintShow(var Msg : TMessage); message CM_HINTSHOW;
    {$endif}
    procedure SetHolidays(newValue : TElHolidays);
    procedure FixHolidayDate(AHoliday : TElHoliday; var Date : TDateTime);
    procedure SetShowHolidays(newValue : Boolean);
    procedure SetShowPeriods(newValue : Boolean);
    procedure SetPeriodStart(newValue : TDateTime);
    procedure SetPeriodLength(newValue : Word);
    procedure SetPeriodInterval(newValue : Word);
    procedure SetPeriodColor(newValue : TColor);
    procedure SetHolidayColor(newValue : TColor);
    procedure SetDate(newValue : TDateTime);
    procedure SetFlat(newValue : Boolean);
    procedure SetTranslateDays(value : boolean);

    {$ifndef CLX_USED}
    procedure DrawFlatBorder;
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure WMVScroll(var Msg : TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg : TWMHScroll); message WM_HSCROLL;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMSize(var Message : TWMSize); message WM_SIZE;
    {$endif}
    function StoreDate: boolean;
    procedure SetLineColorLight(Value: TColor);
    procedure SetLineColorDark(Value: TColor);
    procedure SetUseLineColors(Value: Boolean);
    procedure SetSelectionBorder(Value: TElFlatBorderType);
    procedure SetDayCellBorder(Value: TElFlatBorderType);
    procedure SetCurrentDayBorder(Value: TElFlatBorderType);

  protected
    FLineColorLight: TColor;
    FLineColorDark: TColor;
    FUseSystemStartOfWeek: Boolean;
    procedure SetBorderSides(Value: TElBorderSides);

    procedure Change; dynamic;
    procedure ChangeMonth(Delta : Integer);
    procedure Click; override;
    function DaysThisMonth : Integer; virtual;
    procedure DrawCell(ACol, ARow : Longint; ARect : TRect; AState : TGridDrawState); override;
    function IsLeapYear(AYear : Integer) : Boolean; virtual;
    function SelectCell(ACol, ARow : Longint) : Boolean; override;
    {$ifdef CLX_USED}
    procedure Resize; override;
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}

    procedure Loaded; override;
    {$ifndef CLX_USED}
    procedure UpdateFrame;
    {$endif}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetUseSystemStartOfWeek(Value: Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar; virtual;
    procedure MouseToCell(X, Y : Integer; var ACol, ARow : Longint);

    property CalendarDate : TDateTime read FDate write SetCalendarDate stored StoreCalendarDate;
    property CellText[ACol, ARow : Integer] : string read GetCellText;
    function IsHoliday(AYear, AMonth, ADay : integer) : Boolean;
    function IsInPeriod(AYear, AMonth, ADay : word) : Boolean;

    function IsRestHoliday(AYear, AMonth, ADay : word) : Boolean;
  published
    property Flat : Boolean read FFlat write SetFlat; { Published }

    property Date : TDateTime read FDate write SetDate stored StoreDate;
    property Day : Integer index 3 read GetDateElement write SetDateElement stored False;
    property Holidays : TElHolidays read FHolidays write SetHolidays;
    property Month : Integer index 2 read GetDateElement write SetDateElement stored False;
    property ReadOnly : Boolean read FReadOnly write FReadOnly default False;

    property StartOfWeek : TDayOfWeek read FStartOfWeek write SetStartOfWeek;
    property TranslateDays : boolean read FTranslateDays write SetTranslateDays default true;
    property UseCurrentDate : Boolean read FUseCurrentDate write SetUseCurrentDate default True;
    property Year : Integer index 1 read GetDateElement write SetDateElement stored False;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    property ShowWeekNum : Boolean read FShowWeekNum write SetShowWeekNum default False;
    property WeekEndDays : TElWeekEndDays read FWeekEndDays write SetWeekEndDays;
    property WeekEndColor : TColor read FWeekEndColor write SetWeekEndColor default clRed;
    property ShowHolidays : Boolean read FShowHolidays write SetShowHolidays default True;
    property ShowPeriods : Boolean read FShowPeriods write SetShowPeriods;
    property PeriodStart : TDateTime read FPeriodStart write SetPeriodStart;
    property PeriodLength : Word read FPeriodLength write SetPeriodLength default 1;
    property PeriodInterval : Word read FPeriodInterval write SetPeriodInterval default 28;
    property PeriodColor : TColor read FPeriodColor write SetPeriodColor default clAqua;
    property HolidayColor : TColor read FHolidayColor write SetHolidayColor;
    property ActiveBorderType : TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;  { Published }
    property InactiveBorderType : TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;  { Published }
    property UserNavigation : boolean read FUserNavigation write FUserNavigation;
    property LineColorLight: TColor read FLineColorLight write SetLineColorLight
        stored FUseLineColors default clWindow;
    property LineColorDark: TColor read FLineColorDark write SetLineColorDark
        stored FUseLineColors default clBtnFace;
    property UseSystemStartOfWeek: Boolean read FUseSystemStartOfWeek write
        SetUseSystemStartOfWeek default false;
    property UseLineColors: Boolean read FUseLineColors write SetUseLineColors
        default true;
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;

    property SelectionBorder: TElFlatBorderType read FSelectionBorder write
        SetSelectionBorder default fbtSunken;
    property DayCellBorder: TElFlatBorderType read FDayCellBorder write
        SetDayCellBorder default fbtLine;
    property CurrentDayBorder: TElFlatBorderType read FCurrentDayBorder write
        SetCurrentDayBorder default fbtSunken;

    property Align;
    property BorderStyle;
    property Color;
    {$ifndef CLX_USED}
    property Ctl3D;
    {$endif}
    property Enabled;
    property Font;
    property GridLineWidth;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;

{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    {$ifndef CLX_USED}
    property DockOrientation;
    property Floating;
    property BevelKind;
    property DoubleBuffered;
    property DragKind;
    {$endif}
{$ENDIF}
  end;

implementation

resourcestring

  sDayS  = 'Day ';
  sWeekS = 'Week ';

constructor TElCalendar.Create(AOwner : TComponent);
begin
  FHolidays := TElHolidays.Create(Self);
  inherited Create(AOwner);
  {$ifdef MSWINDOWS}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}

  FUseCurrentDate := True;
  DefaultDrawing := false;

  FixedCols := 0;
  FixedRows := 1;
  ColCount := 7;
  RowCount := 7;

  ScrollBars := ssNone;
  Options := Options - [goRangeSelect, goColSizing, goRowSizing] + [goDrawFocusSelected];
  FDate := SysUtils.Date;
  FShowWeekNum := False;
  FWeekEndDays := [Sat, Sun];
  FWeekEndColor := clRed;
  FShowHolidays := True;
  FPeriodLength := 1;
  FPeriodInterval := 28;
  FPeriodColor := clAqua;
  FActiveBorderType := fbtSunken;
  FInactiveBorderType := fbtSunkenOuter;
  FTranslateDays := true;
  FLineColorLight := clWindow;
  FLineColorDark := clBtnFace;
  FSelectionBorder := fbtSunken;
  FDayCellBorder   := fbtLine;
  FCurrentDayBorder:= fbtSunken;

  UpdateCalendar;
end;

destructor TElCalendar.Destroy;
begin
  Destroying;
  FHolidays.Free;
  inherited;
end;

procedure TElCalendar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TElCalendar.Click;
var
  TheCellText : string;
  ACol, ARow : integer;
  p : TPoint;
begin
  TheCellText := CellText[Col, Row];
  if (TheCellText <> '') then
  begin
    Day := StrToInt(TheCellText);
    Change;
    inherited Click;
  end;
  GetCursorPos(p);
  with ScreenToClient(p) do
    MouseToCell(x, y, ACol, ARow);
  TheCellText := CellText[ACol, ARow];
  if (TheCellText = '') and UserNavigation then
  begin
    if ARow = 1 then
    begin
      PrevMonth;
      Change;
    end
    else
    if ARow >= 4 then
    begin
      NextMonth;
      Change;
    end;
  end;
end;

function TElCalendar.IsLeapYear(AYear : Integer) : Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function TElCalendar.DaysThisMonth : Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;

procedure TElCalendar.DrawCell(ACol, ARow : Longint; ARect : TRect; AState : TGridDrawState);
var
  TheText : string;
  SaveColor,
    SaveColor1,
    SaveFontColor : TColor;
  FSaveBStyle : TBrushStyle;
  R : TRect;
  iDayNum : integer;
  ib      : boolean;
begin
  TheText := CellText[ACol, ARow];
  SaveColor := Canvas.Brush.Color;
  SaveFontColor := Canvas.Font.Color;
  Canvas.Brush.Color := Color;
  FSaveBStyle := Canvas.Brush.Style;
  {$ifdef CLX_USED}
  Canvas.Brush.Style := bsSolid;
  {$endif}
  ib := false;
  if (gdFixed in AState) then
  begin
    Canvas.FillRect(ARect);
    {$ifndef CLX_USED}
    DrawFlatFrameEx2(Canvas.Handle, ARect, Canvas.Brush.Color, Canvas.Brush.Color, false, true, AllBorderSides, DayCellBorder);
    {$endif}
    InflateRect(ARect, -2, -2);
  end
  else
  begin
    if (not FShowWeekNum) or (ACol > 0) then
    begin
      if FShowWeekNum then dec(ACol);
      iDayNum := FMonthOffset + ACol + (ARow - 1) * 7;

      if (iDayNum > 0) and (iDayNum <= DaysThisMonth) and
         (EncodeDate(Year, Month, iDayNum) = Trunc(Now)) then
      begin
        Canvas.FillRect(ARect);
        {$ifndef CLX_USED}
        DrawFlatFrameEx2(Canvas.Handle, ARect, Canvas.Brush.Color, Canvas.Brush.Color, false, true, AllBorderSides, CurrentDayBorder);
        {$endif}
        InflateRect(ARect, -2, -2);
        ib := true;
      end;
      if FShowWeekNum then inc(ACol);
    end;
    if (gdSelected in AState) and (not ib) then
    begin
      Canvas.FillRect(ARect);
      {$ifndef CLX_USED}
      DrawFlatFrameEx2(Canvas.Handle, ARect, Canvas.Brush.Color, Canvas.Brush.Color, false, true, AllBorderSides, SelectionBorder);
      {$endif}
      InflateRect(ARect, -2, -2);
    end;
  end;

  if (gdSelected in AState) then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color := clHighlightText;
  end
  else
  if UseLineColors then
  begin
    Canvas.Font.Color := Font.Color;
    if ((ARow and 1) = 0) then
      Canvas.Brush.Color := LineColorLight
    else
      Canvas.Brush.Color := LineColorDark;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font.Color := Font.Color;
  end;

  if (not FShowWeekNum) or (ACol > 0) then
  begin
    if FShowWeekNum then dec(ACol);
    iDayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    ACol := (StartOfWeek + ACol) mod 7;
    if ACol < 0 then Inc(ACol, 7);
    if (TElWeekEndDay(ACol) in FWeekEndDays) then
      Canvas.Font.Color := FWeekEndColor;
    Canvas.FillRect(ARect);
    if ShowHolidays and IsHoliday(Year, Month, iDayNum) then
    begin
      SaveColor1 := Canvas.Brush.Color;
      Canvas.Brush.Color := FHolidayColor;
      R := Rect(ARect.Left, ARect.Top, ARect.Left + ((ARect.Right - ARect.Left) div 4), ARect.Top + ((ARect.Bottom - ARect.Top) div 4));
      OffsetRect(R, 1, 1);
      Canvas.FillRect(R);
      Canvas.Brush.Color := SaveColor1;
      if IsRestHoliday(Year, Month, iDayNum) and
         (not ((Selection.Left = ACol) and (Selection.Top = ACol))) then
        Canvas.Font.Color := FHolidayColor;
    end;
    if (iDayNum > 0) and (iDayNum <= DaysThisMonth) then
    begin
      if ShowPeriods and IsInPeriod(Year, Month, iDayNum) then
      begin
        SaveColor1 := Canvas.Brush.Color;
        Canvas.Brush.Color := FPeriodColor;
        R := Rect(ARect.Left + 1, ARect.Bottom - ((ARect.Bottom - ARect.Top) div 4) - 1, ARect.Right - 1, ARect.Bottom - 1);
        Canvas.FillRect(R);
        Canvas.Brush.Color := SaveColor1;
      end;
    end;
  end
  else
    Canvas.FillRect(ARect);

//  FSaveBStyle := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  with ARect, Canvas do
    TextRect(ARect, Left + (Right - Left - TextWidth(TheText)) div 2,
      Top + (Bottom - Top - TextHeight(TheText)) div 2, TheText);
  Canvas.Brush.Style := FSaveBStyle;
  Canvas.Brush.Color := SaveColor;
  Canvas.Font.Color := SaveFontColor;
end;

function TElCalendar.GetCellText(ACol, ARow : Integer) : string;
var
  DayNum : Integer;
  AYear, AMonth, ADay : Word;
  iWeekNum, FDay : integer;

  const USShortDayNames : array[1..7] of string = ('Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa');

begin
  if ARow = 0 then { day names at tops of columns }
  begin
    if FShowWeekNum then
    begin
      if ACol = 0 then
      begin
        Result := '#';
        Exit;
      end
      else
        dec(ACol);
    end;
    if FTranslateDays then
       Result := ShortDayNames[(StartOfWeek + ACol) mod 7 + 1]
    else
       Result := USShortDayNames[(StartOfWeek + ACol) mod 7 + 1];
  end
  else
  begin
    if FShowWeekNum then
    begin
      if ACol = 0 then
      begin
        try
          DecodeDate(FDate, AYear, AMonth, ADay);
          FDay := FMonthOffset + (ARow - 1) * 7 + (7 - StartOfWeek + 1) mod 7;
          if FDay > DaysPerMonth(AYear, AMonth) then
          begin
            if AMonth = 12 then
            begin
              FDay := FDay - DaysPerMonth(AYear, AMonth);
              AMonth := 1;
              Inc(AYear);
            end
            else
            begin
              FDay := FDay - DaysPerMonth(AYear, AMonth);
              Inc(AMonth);
            end;
          end;
          iWeekNum := WeekNumber(AYear, AMonth, FDay);
          Result := IntToStr(iWeekNum);
        except
          Result := '';
        end;
        Exit;
      end
      else
        dec(ACol);
    end;
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > DaysThisMonth) then
      Result := ''
    else
      Result := IntToStr(DayNum);
  end;
end;

function TElCalendar.SelectCell(ACol, ARow : Longint) : Boolean;
begin
  if ((not FUpdating) and FReadOnly) then
    Result := False
  else
  begin
    if (CellText[ACol, ARow] = '') then
    begin
      {if ARow > 3 then
      begin
        Updating;
        FUpdating := true;
        NextMonth;
        Day := 1;
        result := false;
        Updated;
      end else
      if ARow < 3 then
      begin
        Updating;
        PrevMonth;
        Day := DaysPerMonth(Year, Month);
        result := false;
        Updated;
      end;} result := true;
    end else Result := inherited SelectCell(ACol, ARow);
  end;
end;

procedure TElCalendar.SetCalendarDate(Value : TDateTime);
begin
  FDate := Value;
  UpdateCalendar;
end;

function TElCalendar.StoreCalendarDate : Boolean;
begin
  Result := not FUseCurrentDate;
end;

function TElCalendar.GetDateElement(Index : Integer) : Integer;
var
  AYear, AMonth, ADay : Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  case Index of
    1 : Result := AYear;
    2 : Result := AMonth;
    3 : Result := ADay;
  else
    Result := -1;
  end;
end;

procedure TElCalendar.SetDate(newValue : TDateTime);
begin
  if Trunc(FDate) <> Trunc(newValue) then
  begin
    FDate := NewValue;
    FUseCurrentDate := False;
    UpdateCalendar;
  end;
end;

procedure TElCalendar.SetDateElement(Index : Integer; Value : Integer);
var
  AYear, AMonth, ADay : Word;
begin
  if Value > 0 then
  begin
    DecodeDate(FDate, AYear, AMonth, ADay);
    case Index of
      1 :
        if AYear <> Value then
          AYear := Value
        else
          Exit;
      2 :
        if (Value <= 12) and (Value <> AMonth) then
          AMonth := Value
        else
          Exit;
      3 :
        if (Value <= DaysThisMonth) and (Value <> ADay) then
          ADay := Value
        else
          Exit;
    else
      Exit;
    end;
    ADay := Min(ADay, DaysPerMonth(AYear, AMonth));
    FDate := EncodeDate(AYear, AMonth, ADay);
    FUseCurrentDate := False;
    UpdateCalendar;
  end;
end;

procedure TElCalendar.SetStartOfWeek(Value : TDayOfWeek);
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;
    FUseSystemStartOfWeek := False;
    UpdateCalendar;
  end;
end;

procedure TElCalendar.SetUseCurrentDate(Value : Boolean);
begin
  if Value <> FUseCurrentDate then
  begin
    FUseCurrentDate := Value;
    if Value and (not (csLoading in ComponentState)) then
    begin
      FDate := SysUtils.Date;
      UpdateCalendar;
    end;
  end;
end;

{ Given a value of 1 or -1, moves to Next or Prev month accordingly }

procedure TElCalendar.ChangeMonth(Delta : Integer);
var
  AYear, AMonth, ADay : Word;
  NewDate : TDateTime;
  CurDay : Integer;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  CurDay := ADay;
  if Delta > 0 then
    ADay := DaysPerMonth(AYear, AMonth)
  else
    ADay := 1;
  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + Delta;
  DecodeDate(NewDate, AYear, AMonth, ADay);
  if DaysPerMonth(AYear, AMonth) > CurDay then
    ADay := CurDay
  else
    ADay := DaysPerMonth(AYear, AMonth);
  CalendarDate := EncodeDate(AYear, AMonth, ADay);
end;

procedure TElCalendar.PrevMonth;
begin
  ChangeMonth(-1);
end;

procedure TElCalendar.NextMonth;
begin
  ChangeMonth(1);
end;

procedure TElCalendar.NextYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
  Year := Year + 1;
end;

procedure TElCalendar.PrevYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
  Year := Year - 1;
end;

procedure TElCalendar.UpdateCalendar;
var
  AYear, AMonth, ADay : Word;
  FirstDate : TDateTime;
  imod : integer;
begin
  FUpdating := True;
  try
    DecodeDate(FDate, AYear, AMonth, ADay);
    FirstDate := EncodeDate(AYear, AMonth, 1);
    FMonthOffset := 2 - ((SysUtils.DayOfWeek(FirstDate) - StartOfWeek + 7) mod 7); { day of week for 1st of month }
    if FMonthOffset = 2 then FMonthOffset := -5;
    imod := 0;
    if FShowWeekNum then Inc(imod);
    MoveColRow((ADay - FMonthOffset) mod 7 + imod, (ADay - FMonthOffset) div 7 + 1, False, False);
    Invalidate;
  finally
    FUpdating := False;
  end;
end;

{$ifdef CLX_USED}
procedure TElCalendar.Resize;
{$else}
procedure TElCalendar.WMSize(var Message : TWMSize);
{$endif}
var
  GridLines : Integer;
  icolnum : integer;
begin
  if FShowWeekNum then
  begin
    GridLines := 7 * GridLineWidth;
    icolnum := 8;
  end
  else
  begin
    GridLines := 6 * GridLineWidth;
    icolnum := 7;
  end;
  {$ifndef CLX_USED}
  DefaultColWidth := (Message.Width - GridLines) div icolnum;
  DefaultRowHeight := (Message.Height - GridLines) div 7;
  inherited;
  {$else}
  inherited;
  DefaultColWidth := (Width - GridLines) div icolnum - 1;
  DefaultRowHeight := (Height - GridLines) div 7 - 1;
  {$endif}
end;

procedure TElCalendar.SetShowWeekNum(newValue : Boolean);
begin
  if (FShowWeekNum <> newValue) then
  begin
    FShowWeekNum := newValue;
    if FShowWeekNum then
    begin
      FixedCols := 1;
      ColCount := 8;
    end
    else
    begin
      ColCount := 7;
      FixedCols := 0;
    end;
    Width := Width - 1;
    Width := Width + 1;
    UpdateCalendar;
  end; {if}
end;

procedure TElCalendar.Loaded;
begin
  inherited;
  if UseCurrentDate then
  begin
    FUseCurrentDate := False;
    UseCurrentDate := true;
  end;
  if UseSystemStartOfWeek then
  begin
    FUseSystemStartOfWeek := false;
    UseSystemStartOfWeek  := true;
  end;
end;

procedure TElCalendar.SetWeekEndDays(newValue : TElWeekEndDays);
begin
  if (FWeekEndDays <> newValue) then
  begin
    FWeekEndDays := newValue;
    Repaint;
  end; {if}
end;

procedure TElCalendar.SetWeekEndColor(newValue : TColor);
begin
  if (FWeekEndColor <> newValue) then
  begin
    FWeekEndColor := newValue;
    Repaint;
  end; {if}
end;

procedure TElCalendar.SetHolidays(newValue : TElHolidays);
begin
  FHolidays.Assign(newValue);
end;

procedure TElCalendar.FixHolidayDate(AHoliday : TElHoliday; var Date : TDateTime);
var
  T1 : TSystemTime;
  FirstDate : TDateTime;
  FMonthOffset,
    Dim, DayNum : integer;
begin
  T1.wDayOfWeek := AHoliday.DayOfWeek;
  T1.wMonth := AHoliday.Month;
  T1.wDay := AHoliday.Day;
  T1.wYear := Year;
  FirstDate := EncodeDate(T1.wYear, T1.wMonth, 1);
  FMonthOffset := 2 - ((DateTimeToTimeStamp(FirstDate).Date mod 7 + 8) mod 7); { day of week for 1st of month }
  if FMonthOffset = 2 then FMonthOffset := -5;
  DayNum := FMonthOffset + T1.wDayOfWeek + (T1.wDay) * 7;
  if (DayNum < 1) then inc(DayNum, 7);
  Dim := DaysPerMonth(T1.wYear, T1.wMonth);
  while (DayNum > Dim) do
    dec(DayNum, 7);
  T1.wDay := DayNum;
  T1.wHour := 0;
  T1.wMinute := 0;
  T1.wSecond := 0;
  T1.wMilliseconds := 0;
  Date := EncodeDate(T1.wYear, T1.wMonth, T1.wDay) + EncodeTime(T1.wHour, T1.wMinute, T1.wSecond, T1.wMilliseconds);
end;

procedure TElCalendar.MouseToCell(X, Y : Integer; var ACol, ARow : Longint);
var
  Coord : TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

{$ifdef CLX_USED}
function TElCalendar.HintShow(var HintInfo : THintInfo): Boolean;
{$else}
procedure TElCalendar.CMHintShow(var Msg : TMessage);
{$endif}
var
{$ifndef CLX_USED}
  HintInfo : PHintInfo;
{$endif}
  ARow, ACol : integer;
  i, DayNum : integer;
  AHoliday : TElHoliday;
  ADate : TDateTime;
  wAYear,
    wAMonth,
    wADay : Word;
  ths : string;
  pp : TPoint;
begin
{$ifndef CLX_USED}
  HintInfo := PHintInfo(Msg.lParam);
{$else}
  result := false;
{$endif}
  if HintInfo.HintControl <> Self then Exit;
  pp := HintInfo.HintPos;
  pp := ScreenToClient(pp);
  with pp do
    MouseToCell(X, Y, ACol, ARow);
  Dec(ARow);
  if ARow < 0 then exit;
  if (not FShowWeekNum) or (ACol > 0) then
  begin
    if FShowWeekNum then Dec(ACol);
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum >= 1) and (DayNum <= DaysThisMonth) then
    begin
      ths := sDayS + IntToStr(DayNumber(Year, Month, DayNum)) + #13#10 +
        sWeekS + IntToStr(WeekNumber(Year, Month, DayNum));
      for i := 0 to FHolidays.Count - 1 do // Iterate
      begin
        AHoliday := FHolidays[i];
        if AHoliday.Month = Month then
        begin
          if AHoliday.FixedDate then
            ADate := EncodeDate(Year, Month, AHoliday.Day)
          else
            FixHolidayDate(AHoliday, ADate);
          DecodeDate(ADate, wAYear, wAMonth, wADay);
          if wADay = DayNum then
            ths := ths + #13#10 + AHoliday.Description;
        end;
      end; // for
      if ths <> '' then HintInfo.HintStr := ths;
    end;
  end;
end;

function TElCalendar.IsHoliday(AYear, AMonth, ADay : integer) : Boolean;
var
  I : Integer;
  AHoliday : TElHoliday;
  ADate : TDateTime;
  wAYear,
    wAMonth,
    wADay : Word;

begin
  result := false;
  for i := 0 to FHolidays.Count - 1 do // Iterate
  begin
    AHoliday := FHolidays[i];
    if AHoliday.Month = AMonth then
    begin
      if AHoliday.FixedDate then
        ADate := EncodeDate(Year, Month, AHoliday.Day)
      else
        FixHolidayDate(AHoliday, ADate);
      DecodeDate(ADate, wAYear, wAMonth, wADay);
      result := wADay = ADay;
      if Result then break;
    end;
  end; // for
end; {IsHoliday}

procedure TElCalendar.SetShowHolidays(newValue : Boolean);
begin
  if (FShowHolidays <> newValue) then
  begin
    FShowHolidays := newValue;
    UpdateCalendar;
  end; {if}
end; {SetShowHolidays}

procedure TElCalendar.SetShowPeriods(newValue : Boolean);
begin
  if (FShowPeriods <> newValue) then
  begin
    FShowPeriods := newValue;
    UpdateCalendar;
  end; {if}
end; {SetShowPeriods}

procedure TElCalendar.SetPeriodStart(newValue : TDateTime);
begin
  if (FPeriodStart <> newValue) then
  begin
    FPeriodStart := newValue;
    if FShowPeriods then UpdateCalendar;
  end; {if}
end; {SetPeriodStart}

procedure TElCalendar.SetPeriodInterval(newValue : Word);
begin
  if (FPeriodInterval <> newValue) then
  begin
    FPeriodInterval := newValue;
    if FShowPeriods then UpdateCalendar;
  end; {if}
end;

procedure TElCalendar.SetPeriodLength(newValue : Word);
begin
  if (FPeriodLength <> newValue) then
  begin
    FPeriodLength := newValue;
    if FShowPeriods then UpdateCalendar;
  end; {if}
end; {SetPeriodLength}

procedure TElCalendar.SetPeriodColor(newValue : TColor);
begin
  if (FPeriodColor <> newValue) then
  begin
    FPeriodColor := newValue;
    if FShowPeriods then UpdateCalendar;
  end; {if}
end; {SetPeriodColor}

function TElCalendar.IsInPeriod(AYear, AMonth, ADay : word) : Boolean;
var
  FDate, FDate1 : TDateTime;
begin
  result := false;
  FDate1 := EncodeDate(AYear, AMonth, ADay);
  FPeriodStart := Trunc(FPeriodStart);
  if FDate1 < FPeriodStart then exit;
  FDate := FPeriodStart;
  if FPeriodInterval > 0 then
  begin
    while true do
    begin
      FDate := FDate + FPeriodInterval;
      if FDate > FDate1 then
      begin
        FDate := FDate - FPeriodInterval + FPeriodLength;
        if FDate > FDate1 then result := true;
        break;
      end;
    end; // while
  end else
    result := (FDate1 >= FPeriodStart) and (FDate1 < FPeriodStart + FPeriodLength);
end; {IsInPeriod}

function TElCalendar.IsRestHoliday(AYear, AMonth, ADay : word) : Boolean;
var
  I : Integer;
  AHoliday : TElHoliday;
  ADate : TDateTime;
  wAYear,
    wAMonth,
    wADay : Word;

begin
  result := false;
  for i := 0 to FHolidays.Count - 1 do // Iterate
  begin
    AHoliday := FHolidays[i];
    if AHoliday.Month = AMonth then
    begin
      if AHoliday.FixedDate then
        ADate := EncodeDate(Year, Month, AHoliday.Day)
      else
        FixHolidayDate(AHoliday, ADate);
      DecodeDate(ADate, wAYear, wAMonth, wADay);
      result := (wADay = ADay) and (AHoliday.IsRest);
      if Result then break;
    end;
  end; // for
end; {IsRestHoliday}

procedure TElCalendar.SetHolidayColor(newValue : TColor);
begin
  if (FHolidayColor <> newValue) then
  begin
    FHolidayColor := newValue;
    if FShowHolidays then UpdateCalendar;
  end; {if}
end; {SetHolidayColor}

procedure TElCalendar.SetTranslateDays(value : boolean);
begin
  if FTranslateDays <> value then
  begin
    FTranslateDays := value;
    Invalidate;
  end;
end;

procedure TElCalendar.SetFlat(newValue : Boolean);
{ Sets data member FFlat to newValue. }
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    {$ifndef CLX_USED}
    UpdateFrame;
    {$endif}
  end; { if }
end; { SetFlat }

{$ifndef CLX_USED}
procedure TElCalendar.CMMouseEnter(var Msg : TMessage); { private }
begin
  inherited;
  FMouseOver := true;
  if Flat and (not Focused) and (not ThemesAvailable) then UpdateFrame;
end; { CMMouseEnter }

procedure TElCalendar.CMMouseLeave(var Msg : TMessage); { private }
begin
  inherited;
  FMouseOver := false;
  if Flat and (not Focused) and (not ThemesAvailable) then UpdateFrame;
end; { CMMouseLeave }

procedure TElCalendar.UpdateFrame; { protected }
var
  R : TRect;
begin
  R := Rect(0, 0, Width, Height);
  RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame);
end; { UpdateFrame }

procedure TElCalendar.DrawFlatBorder;
var
  DC : HDC;
  R  : TRect;
  b  : boolean;
  BS : TElFlatBorderType;
  Theme : HTheme;
begin
  R := Rect(0, 0, Width, Height);
  DC := GetWindowDC(Handle);
  try
    if ThemesAvailable and (BorderStyle = bsSingle) then
    begin
      Theme := OpenThemeData(0, 'EDIT');
      if Theme <> 0 then
      begin
        // DrawThemeBackground(Theme, DC, 0, 0, R, nil);
        DrawThemeEdge(Theme, DC, 0, 0, R, EDGE_SUNKEN, BF_MONO or BF_FLAT or BF_RECT, nil);
        CloseThemeData(Theme);
        exit;
      end;
    end;
    if (BorderStyle = bsSingle) then
    begin
      b := Focused or FMouseOver;
      if b then BS := FActiveBorderType else BS := FInactiveBorderType;
      DrawFlatFrameEx2(DC, R, Color, Color, b, Enabled, FBorderSides, BS);
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TElCalendar.WMNCPaint(var Msg : TMessage); { private }
begin
  if (not Flat) or ThemesAvailable then
  begin
    inherited;
    exit;
  end;
  inherited;
  DrawFlatBorder;
  Msg.Result := 0;
end; { WMNCPaint }

procedure TElCalendar.WMKillFocus(var Msg : TWMKillFocus); { private }
begin
  inherited;
  if not HandleAllocated then exit;
  if Flat then UpdateFrame;
end; { WMKillFocus }

procedure TElCalendar.WMSetFocus(var Msg : TWMSetFocus); { private }
begin
  inherited;
  if Flat then UpdateFrame;
end; { WMSetFocus }

procedure TElCalendar.WMPaint(var Msg : TWMPaint); { private }
begin
  if Flat or ThemesAvailable then DrawFlatBorder;
  inherited;
end; { WMPaint }

procedure TElCalendar.WMVScroll(var Msg : TWMVScroll); { private }
begin
  inherited;
  if Flat then DrawFlatBorder;
end; { WMVScroll }

procedure TElCalendar.WMHScroll(var Msg : TWMHScroll); { private }
begin
  inherited;
  if Flat then DrawFlatBorder;
end; { WMHScroll }
{$endif}

procedure TElCalendar.SetActiveBorderType(newValue : TElFlatBorderType);
{ Sets data member FActiveBorderType to newValue. }
begin
  if (FActiveBorderType <> newValue) then
  begin
    FActiveBorderType := newValue;
    {$ifndef CLX_USED}
    if (Focused or FMouseOver) then UpdateFrame;
    {$endif}
  end;  { if }
end;  { SetActiveBorderType }

procedure TElCalendar.SetInactiveBorderType(newValue : TElFlatBorderType);
{ Sets data member FInactiveBorderType to newValue. }
begin
  if (FInactiveBorderType <> newValue) then
  begin
    FInactiveBorderType := newValue;
    {$ifndef CLX_USED}
    if not (Focused or FMouseOver) then UpdateFrame;
    {$endif}
  end;  { if }
end;  { SetInactiveBorderType }

procedure TElCalendar.KeyDown(var Key: Word; Shift: TShiftState);  { protected }
var b : boolean;
    TheCellText: string;
    ACol, ARow : Word;

  function SameMonth (Date1, Date2 : TDateTime) : boolean;
  var Day1, Day2,
      Month1, Month2,
      Year1, Year2  : word;
  begin
    DecodeDate(Date1, Year1, Month1, Day1);
    DecodeDate(Date2, Year2, Month2, Day2);
    result := Month1 = Month2;
  end;

begin
  {$ifndef CLX_USED}
  if (Shift = []) //and (Key in [VK_DOWN, VK_UP, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT]) then
  {$else}
  if (Shift = []) //and (Key in [KEY_DOWN, KEY_UP, KEY_LEFT, KEY_RIGHT, KEY_PRIOR, KEY_NEXT]) then
  {$endif}
  then
  begin
    ACol := Col;
    ARow := Row;
    if Self.ShowWeekNum then
      dec(ACol);

    case Key of
      VK_RETURN:
        begin
          try
            TheCellText := CellText[ACol, ARow];
          except
            TheCellText := '';
          end;
          if (TheCellText <> '') then
          begin
            if StrToIntDef(TheCellText, -1) <> -1 then
            begin
              Day := StrToInt(TheCellText);
              Change;
              Click;
              exit;
            end;
          end;
          b := false;
        end;

      VK_DOWN:
        begin
          b := true;
          if UserNavigation then
            Date := Date + 7
          else
          if SameMonth(Date, Date + 7) then
            Date := Date + 7
          else
            b := false;
        end;
      VK_UP:
        begin
          b := true;
          if UserNavigation then
             Date := Date - 7
          else
          if SameMonth(Date, Date - 7) then
            Date := Date - 7
          else
            b := false;
        end;
      VK_LEFT:
        begin
          b := true;
          if UserNavigation then
             Date := Date - 1
          else
          if SameMonth(Date, Date - 1) then
            Date := Date - 1
          else
            b := false;
        end;
      VK_RIGHT:
        begin
          b := true;
          if UserNavigation then
             Date := Date + 1
          else
          if SameMonth(Date, Date + 1) then
             Date := Date + 1
          else b := false;
        end;
      VK_PRIOR:
        begin
          b := true;
          if UserNavigation then
            Date := Date - DaysThisMonth
          else
            b := false;
        end;
      VK_NEXT:
        begin
          b := true;
          if UserNavigation then
             Date := Date + DaysThisMonth
          else
             b := false;
        end;
      else
        b := false;
    end;
    if b then
    begin
      Change;
      Key := 0;
    end
    else
      inherited;
  end
  else
    inherited;
end;  { KeyDown }

function TElCalendar.StoreDate: boolean;
begin
  Result := not FUseCurrentDate;
end;

procedure TElCalendar.SetUseSystemStartOfWeek(Value: Boolean);
begin
  if FUseSystemStartOfWeek <> Value then
  begin
    if (not (csLoading in ComponentState)) then
      if Value then
        StartOfWeek := TDayOfWeek(GetSysStartDayOfWeek)
      else
        UpdateCalendar;
    FUseSystemStartOfWeek := Value;
  end;
end;

procedure TElCalendar.SetLineColorLight(Value: TColor);
begin
  if FLineColorLight <> Value then
  begin
    FLineColorLight := Value;
    Invalidate;
  end;
end;

procedure TElCalendar.SetLineColorDark(Value: TColor);
begin
  if FLineColorDark <> Value then
  begin
    FLineColorDark := Value;
    Invalidate;
  end;
end;

procedure TElCalendar.SetUseLineColors(Value: Boolean);
begin
  if FUseLineColors <> Value then
  begin
    FUseLineColors := Value;
    Invalidate;
  end;
end;

procedure TElCalendar.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    {$ifndef CLX_USED}
    if HandleAllocated then
      RecreateWnd;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElCalendar.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) then
  begin
    if not (ebsLeft in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if not (ebsTop in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if not (ebsRight in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if not (ebsBottom in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end;
  // Message.Result := WVR_REDRAW;
end;
{$endif}

procedure TElCalendar.SetSelectionBorder(Value: TElFlatBorderType);
begin
  if FSelectionBorder <> Value then
  begin
    FSelectionBorder := Value;
    Invalidate;
  end;
end;

procedure TElCalendar.SetDayCellBorder(Value: TElFlatBorderType);
begin
  if FDayCellBorder <> Value then
  begin
    FDayCellBorder := Value;
    Invalidate;
  end;
end;

procedure TElCalendar.SetCurrentDayBorder(Value: TElFlatBorderType);
begin
  if FCurrentDayBorder <> Value then
  begin
    FCurrentDayBorder := Value;
    Invalidate;
  end;
end;





end.
