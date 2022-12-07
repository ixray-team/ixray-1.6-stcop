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

06/20/2002

  When ChangeDisabledText was set to true, changing Enabled property didn't invalidate the control

06/18/2002

  Added ChangeDisabledText property

06/09/2002

  Day name (ddd, dddd modifiers) was not shown and selected. Fixed. 

05/28/2002

  Changing UnassignedChar didn't update  

05/13/2002

  Added UnassignedChar property

03/18/2002

  Default for AutoSize is now correctly set to true

03/06/2002

  Added unicode hint

02/09/2002

  Fixed manual hour setup in 24-hour mode (spoiled 01/24)

01/24/2002

  Fixed pm reset in 12-hour mode when entering text manually

01/10/2002

  UnassignedAllowed, Unassigned, UnassignedColor and ReadOnly properties added 

01/01/2002

  Fixed some problems with painting borders when focus is moved

12/21/2001

  ButtonVisible property added

11/28/2001

  LineBorderActiveColor and LineBorderInactiveColor properties added.

10/25/2001

  Fixed painting of the borders with XP styles enabled
  
10/19/2001

  Added ButtonDir, ButtonType and ButtonWidth properties.

08/31/2001

  Added MaxDate and MinDate properties 

07/26/2001

  Added Unicode support

07/12/2001

  BorderSides property added.

06/05/2001

  Fixed the problems with window size and sleection that appeared when using
  CALENDAR_USE_WINDOWS_CALENDAR option for popup calendar.

  OnChange event was not fired when changes were made via popup calendar. Fixed.

05/01/2001

  fixed alignment of the text when checkbox is displayed

04/10/2001

  Fixed the problem with Picker loosing current selection when focus is moved out of control.

03/06/2001

  Fixed the problem with center- and right-aligned text when checkbox is visible

02/02/2001

  Fixed the problem with HH specifier (it worked as H).
  Fixed the problem with construct '*:h:*' (one 'h' is used).

  Modified property was not updated when the user changed the value with keyboard.
  Fixed.

12/14/2000

  Fixed the problem with ddddd/dddddd and t/tt formats.

*)

unit ElDTPick;  { TElDateTimePicker component. }

interface

uses
  SysUtils,
  Classes,

{$ifndef CLX_USED}
{$ifdef VCL_6_USED}
  Types,
{$endif}
  Windows,
  Messages,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  Buttons,
  Menus,
  ExtCtrls,
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
  QMenus,
  QExtCtrls,
  ElCLXUtils,
{$endif}
  {$ifndef CLX_USED}
  ElACtrls,
  ElImgFrm,
  {$endif}
  ElSpinBtn,
  ElTools,
  ElList,
  ElExtBkgnd,
  ElPopBtn,
  ElCalendarDefs,
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
  ElCalendDlg,
{$endif}
  ElVCLUtils,
  ElStrUtils,
  ElTmSchema,
  ElUxTheme,
  ElXPThemedControl;

type

  {
  Possible values for DPart :
  -1: plain text
  0 : year long (4 digits)
  1 : year short (2 digits)
  2 : month
  3 : day
  4 : hour long (24 hours)
  5 : hour short (12 hours)
  6 : minute
  7 : second
  8 : am/pm sign
  9 : separator
  10: day name
  }

  TDTFPart = record
    Text   : string;
    DPart  : integer;
    TSPos,
    TEPos,
    SPos,
    EPos   : integer;
  end;
  PDTFPart = ^TDTFPart;

  TElDatePickerFormat = (edfShortDateLongTime,
                         edfLongDate,
                         edfShortDate,
                         edfLongTime,
                         edfShortTime,
                         edfCustom);

  TElDateTimePicker = class(TElXPThemedControl)
  protected
    FUnassignedChar : TElFChar;
    FHandleDialogKeys: Boolean;
    FModified : Boolean;
    FShowCheckBox : Boolean;
    FChecked : Boolean;
    FAltChangeMethod : Boolean;
    FShowPopupCalendar : Boolean;
    FNavigationInPopup : boolean;
    FBorderStyle : TBorderStyle;
    FOnChange : TNotifyEvent;
    Use12Hours : boolean;
    FCalButton : TElGraphicButton;
    FButton    : TElSpinButton;
    FBtnWidth  : Integer;
    DTFParts   : TElList;
    FFormatStr : string;
    FFormat    : TElDatePickerFormat;
    FDate      : TDateTime;
    FFocused   : boolean;
    FCurPart   : integer;
    FAlignment : TAlignment;
    FMouseOver : boolean;
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
    FForm      : TElCalendarForm;
{$endif}
    TmpDC      : HDC;
    FOnDropDown: TNotifyEvent;
    FGradientSteps : Integer;
    FGradientStartColor : TColor;
    FGradientEndColor : TColor;
    FTmpBmp : TBitmap;
    FTransparent : Boolean;
    FBackground : TBitmap;
    FBackgroundType : TElBkGndType;
    {$ifndef CLX_USED}
    FImgFormChLink  : TImgFormChangeLink;
    FImgForm : TElImageForm;
    {$endif}
    FActiveBorderType : TElFlatBorderType;
    FInactiveBorderType : TElFlatBorderType;
    FFlat : boolean;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;

    FDI   : string;
    FText : TElFString;
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
    FDroppedDown : boolean;
{$endif}
    InDblClick   : boolean;
    FOnCheckBoxChange: TNotifyEvent;
    {$ifndef CLX_USED}
    FBorderSides: TElBorderSides;
    {$endif}
    //FButtonWidth: Integer;
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    FButtonVisible: Boolean;
    FUnassigned: Boolean;
    FUnassignedColor: TColor;
    FUnassignedAllowed: Boolean;
    FReadOnly: Boolean;
    FButtonShowOnFocus: Boolean;
    FUseCurrentDate: Boolean;
    FButtonThinFrame: Boolean;
    FAutoSize: Boolean;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}
    FChangeDisabledText: Boolean;

    {$ifndef CLX_USED}
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure WMSysKeyDown(var Msg: TWMKeyDown); message WM_SYSKEYDOWN;
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Msg : TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Msg : TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Msg : TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Msg : TMessage); message CM_ENABLEDCHANGED;
    procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
        WM_WINDOWPOSCHANGED;
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$else}
    procedure FontChanged; override;
    procedure Resize; override;
    procedure EnabledChanged; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure PaletteChanged(Sender: TObject); override;
    {$endif}
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;

    procedure SetBorderStyle(Value: TBorderStyle);
    {$ifndef CLX_USED}
    procedure SetImageForm(newValue : TElImageForm);
    {$endif}
    procedure SetTransparent(newValue : boolean);
    procedure SetBackground(newValue : TBitmap);
    procedure SetBackgroundType(newValue : TElBkGndType);
    procedure ImageChange(Sender : TObject);
    {$ifndef CLX_USED}
    procedure ImageFormChange(Sender : TObject);
    {$endif}
    procedure SetGradientStartColor(newValue : TColor);
    procedure SetGradientEndColor(newValue : TColor);
    procedure SetGradientSteps(newValue : Integer);
    procedure RedoTmpBmp;
    {$ifndef CLX_USED}
    procedure DrawFlatBorder;
    {$endif}
    procedure InvalidateEdit;
    procedure CloseUp(AcceptValue: boolean); virtual;
    {$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
    procedure CalendarDeactivate(Sender : TObject);
    {$endif}
    
    procedure SpinUpClick(Sender : TObject; Increment : Double);
    procedure SpinDownClick(Sender : TObject; Increment : Double);
    procedure CalBtnClick(Sender : TObject);

    procedure IncValue(Increment : integer);
    procedure ParseFormat(AFormat : string);
    procedure UpdatePart;
    {$ifndef CLX_USED}
    procedure UpdateFrame;
    {$endif}
    procedure UpdateText;
    procedure OnDTFPartDelete(Sender : TObject; Item : Pointer);

    function GetStdFormat(Fmt : TElDatePickerFormat) : string;
    procedure TuneupCalendarControls;

    function GetCalendarUseLineColors: Boolean;
    procedure SetCalendarUseLineColors(Value: Boolean);

    function StoreStartOfWeek: Boolean;
    function GetCalendarWeekendColor: TColor;
    procedure SetCalendarWeekendColor(Value: TColor);
    {$ifndef CLX_USED}
    procedure DrawFlatFrame(Canvas : TCanvas; R : TRect);
    procedure SetBorderSides(Value: TElBorderSides);
    {$endif}
    procedure SetFormat(newValue : TElDatePickerFormat); virtual;
    procedure SetFormatStr(newValue : String); virtual;
    procedure SetDateTime(newValue: TDateTime); virtual;
    procedure SetAlignment(Value: TAlignment); virtual;
    function  GetBtnWidth   : integer; virtual;
    function  GetCheckDims  : integer; virtual;
    function  GetCheckWidth : integer; virtual;
    procedure SetEditRect;
    procedure DropDown; virtual;
    procedure Paint; override;
    procedure DoDropDown; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure TriggerChangeEvent; virtual;
    procedure SetActiveBorderType(newValue : TElFlatBorderType); virtual;
    procedure SetInactiveBorderType(newValue : TElFlatBorderType); virtual;
    procedure SetFlat(newValue : Boolean); virtual;
    procedure SetNavigationInPopup(newValue : boolean); virtual;
    procedure SetDate(newValue : TDateTime); virtual;
    function GetDate : TDateTime; virtual;

    procedure SetTime(newValue : TDateTime); virtual;
    function GetTime : TDateTime; virtual;
    {$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
    function GetDroppedDown : boolean;
    procedure SetDroppedDown(newValue : boolean); virtual;
    {$endif}
    function GetStartOfWeek: TDayOfWeek;
    procedure SetStartOfWeek(Value: TDayOfWeek);
    function GetWeekEndDays: TElWeekEndDays;
    procedure SetWeekEndDays(Value: TElWeekEndDays);
    procedure SetCalendarLineColorLight(Value: TColor);
    function GetCalendarLineColorDark: TColor;
    procedure SetCalendarLineColorDark(Value: TColor);
    function GetCalendarLineColorLight: TColor;
    function GetCalendarBackColor: TColor;
    procedure SetCalendarBackColor(Value: TColor);
    function GetUseSystemStartOfWeek: Boolean;
    procedure SetUseSystemStartOfWeek(Value: Boolean);

    procedure Notification(AComponent : TComponent; operation : TOperation); override;
    {$ifndef CLX_USED}
    procedure CreateParams(var Params : TCreateParams); override;
    {$endif}
    procedure Loaded; override;
    {$ifndef CLX_USED}
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    {$else}
    procedure CreateWidget; override;
    {$endif}
    {$ifndef CLX_USED}
    procedure AdjustHeight;
    procedure UpdateHeight;
    procedure CreateWnd; override;
    {$endif}
    procedure SetShowPopupCalendar(newValue : Boolean); virtual;
    procedure CalendarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CalendarChange(Sender: TObject);
    procedure DblClick; override;

    property BtnWidth : integer read GetBtnWidth;
    procedure SetShowCheckBox(newValue : Boolean); virtual;
    procedure SetChecked(newValue : Boolean); virtual;
    procedure SetModified(newValue : Boolean); virtual;
    procedure TriggerCheckboxChangeEvent;
    function GetCalendarCurrentDayBorder: TElFlatBorderType;
    procedure SetCalendarCurrentDayBorder(Value: TElFlatBorderType);
    function GetCalendarDayCellBorder: TElFlatBorderType;
    procedure SetCalendarDayCellBorder(Value: TElFlatBorderType);
    function GetCalendarSelectionBorder: TElFlatBorderType;
    procedure SetCalendarSelectionBorder(Value: TElFlatBorderType);
    procedure SetMinDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDateTime);
    procedure DoSetDateTime(ADate : TDateTime);
    function GetThemedClassName: WideString; override;
    procedure SetUseXPThemes(const Value: Boolean); override;
    function GetButtonDir: TElSpinBtnDir;
    procedure SetButtonDir(Value: TElSpinBtnDir);
    function GetButtonType: TElSpinBtnType;
    procedure SetButtonType(Value: TElSpinBtnType);
    procedure SetButtonWidth(Value: Integer);
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);
    procedure UpdateButtonStyles;
    procedure SetButtonVisible(Value: Boolean);
    procedure SetUnassigned(Value: Boolean);
    procedure SetUnassignedColor(Value: TColor);
    procedure SetUnassignedAllowed(Value: Boolean);
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetButtonShowOnFocus(Value: Boolean);
    function IsDateTimeStored: Boolean;
    procedure SetButtonThinFrame(Value: Boolean);
    procedure SetAutoSize(Value: Boolean); {$ifdef VCL_6_USED}reintroduce;{$endif}

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}
    function GetUnassignedChar: TElFString;
    procedure SetUnassignedChar(Value: TElFString);
    function MakeUnassignedString(Length : integer): TElFString;
    procedure SetChangeDisabledText(Value: Boolean);

  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property MouseOver   : boolean read FMouseOver;

    {$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
    property DroppedDown : boolean read GetDroppedDown write SetDroppedDown;
    {$endif}

    property Date : TDateTime read GetDate write SetDate;
    property Time : TDateTime read GetTime write SetTime;
  published
    property Format : TElDatePickerFormat read FFormat write SetFormat default edfShortDateLongTime;
    property FormatString : String read FFormatStr write SetFormatStr;  { Published }
    property DateTime: TDateTime read FDate write SetDateTime stored IsDateTimeStored;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    {$ifndef CLX_USED}
    property ImageForm : TElImageForm read FImgForm write SetImageForm;
    {$endif}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;
    property GradientStartColor : TColor read FGradientStartColor write SetGradientStartColor default clBlack;  { Protected }
    property GradientEndColor : TColor read FGradientEndColor write SetGradientEndColor default clBlack;  { Protected }
    property GradientSteps : Integer read FGradientSteps write SetGradientSteps default 16;  { Protected }
    property Background : TBitmap read FBackground write SetBackground;
    property BackgroundType : TElBkGndType read FBackgroundType write SetBackgroundType default bgtColorFill;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Transparent : Boolean read FTransparent write SetTransparent default false; { Protected }
    property ActiveBorderType : TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;
    property InactiveBorderType : TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;
    property Flat : Boolean read FFlat write SetFlat default false;
    property ShowPopupCalendar : Boolean read FShowPopupCalendar write SetShowPopupCalendar default false;  { Published }
    property NavigationInPopup : boolean read FNavigationInPopup write SetNavigationInPopup default true;
    property AltChangeMethod : Boolean read FAltChangeMethod write FAltChangeMethod;  { Published }
    property ShowCheckBox : Boolean read FShowCheckBox write SetShowCheckBox;  { Published }
    property Checked  : Boolean read FChecked write SetChecked default True;  { Published }
    property Modified : Boolean read FModified write SetModified;  { Published }

    property CalendarLineColorDark: TColor read GetCalendarLineColorDark write
        SetCalendarLineColorDark default clBtnShadow;
    property CalendarLineColorLight: TColor read GetCalendarLineColorLight write
        SetCalendarLineColorLight default clWindow;
    property CalendarBackColor: TColor read GetCalendarBackColor write
        SetCalendarBackColor default clWindow;
    property StartOfWeek: TDayOfWeek read GetStartOfWeek write SetStartOfWeek
        stored StoreStartOfWeek;
    property UseSystemStartOfWeek: Boolean read GetUseSystemStartOfWeek write
        SetUseSystemStartOfWeek;
    property WeekEndDays: TElWeekEndDays read GetWeekEndDays write SetWeekEndDays;
    property CalendarUseLineColors: Boolean read GetCalendarUseLineColors write
        SetCalendarUseLineColors default true;
    property CalendarWeekendColor: TColor read GetCalendarWeekendColor write
        SetCalendarWeekendColor;
    {$ifndef CLX_USED}
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    {$endif}
    property CalendarCurrentDayBorder: TElFlatBorderType read
        GetCalendarCurrentDayBorder write SetCalendarCurrentDayBorder;
    property CalendarDayCellBorder: TElFlatBorderType read GetCalendarDayCellBorder
        write SetCalendarDayCellBorder;
    property CalendarSelectionBorder: TElFlatBorderType read
        GetCalendarSelectionBorder write SetCalendarSelectionBorder;
    property MinDate: TDateTime read FMinDate write SetMinDate;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate;
    property HandleDialogKeys: Boolean read FHandleDialogKeys write
        FHandleDialogKeys default false;
    property ButtonDir: TElSpinBtnDir read GetButtonDir write SetButtonDir;
    property ButtonType: TElSpinBtnType read GetButtonType write SetButtonType;
    property ButtonWidth: Integer read FBtnWidth write SetButtonWidth;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;

    property OnCheckBoxChange: TNotifyEvent read FOnCheckBoxChange write
        FOnCheckBoxChange;

    {$IFDEF VCL_4_USED}
    property Anchors;
    {$ENDIF}
    property Align;
    property Color;
    {$ifndef CLX_USED}
    property Ctl3D;
    property DragCursor;
    {$endif}
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    {$ifndef CLX_USED}
    property ParentCtl3D;
    {$endif}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    { Inherited events: }
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
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write 
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write 
        SetLineBorderInactiveColor;
    property ButtonVisible: Boolean read FButtonVisible write SetButtonVisible 
        default true;
    property Unassigned: Boolean read FUnassigned write SetUnassigned default false;
    property UnassignedColor: TColor read FUnassignedColor write SetUnassignedColor
        default clRed;
    property UnassignedAllowed: Boolean read FUnassignedAllowed write 
        SetUnassignedAllowed default false;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default false;
    property ButtonShowOnFocus: Boolean read FButtonShowOnFocus write 
        SetButtonShowOnFocus default false;
    property UseCurrentDate: Boolean read FUseCurrentDate write FUseCurrentDate 
        default true;
    property ButtonThinFrame: Boolean read FButtonThinFrame write 
        SetButtonThinFrame default true;
    property UnassignedChar: TElFString read GetUnassignedChar write 
        SetUnassignedChar;
    property ChangeDisabledText: Boolean read FChangeDisabledText write
        SetChangeDisabledText default false;
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
  end;  { TElDateTimePicker }

implementation

procedure TElDateTimePicker.IncValue(Increment : integer);
var Part : PDTFPart;
    ST   : TSystemTime;
    DT   : TDateTime;
    i, j : integer;
begin
  if FCurPart = -1 then exit;
  Part := PDTFPart(DTFParts[FCurPart]);
  DateTimeToSystemTime(FDate, ST);
  case Part.DPart of
    0:
       begin
         if FAltChangeMethod then
         begin
           if (Increment > 0) or (FDate >= 367) then
           begin
             if Increment < 0 then
             begin
               if IsLeapYear(ST.wYear - 1) then
                  DoSetDateTime(FDate - 366)
               else
                  DoSetDateTime(FDate - 365);
             end else
             begin
               if IsLeapYear(ST.wYear) then
                  DoSetDateTime(FDate + 366)
               else
                  DoSetDateTime(FDate + 365);
             end;
           end;
           DateTimeToSystemTime(FDate, ST);
         end else
         begin
           ST.wYear := ST.wYear + Increment;
           if ST.wYear < 1900 then
              ST.wYear := 1900;
           ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
         end;
       end;
    1: begin
         if FAltChangeMethod then
         begin
           if (Increment > 0) or (FDate >= 367) then
           begin
             if Increment < 0 then
             begin
               if IsLeapYear(ST.wYear - 1) then
                  DoSetDateTime(FDate - 366)
               else
                  DoSetDateTime(FDate - 365);
             end else
             begin
               if IsLeapYear(ST.wYear) then
                  DoSetDateTime(FDate + 366)
               else
                  DoSetDateTime(FDate + 365);
             end;
           end;
           DateTimeToSystemTime(FDate, ST);
         end else
         begin
           i := ST.wYear mod 100;
           i := i + Increment;
           if i > 100 then
              i := i mod 100
           else
           if i < 0 then
              i := 100 + (i mod 100);
           if i > 50 then
              ST.wYear := 1900 + i
           else
              ST.wYear := 2000 + i;
           ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
         end;
       end;
    2: begin
         if FAltChangeMethod then
         begin
           if Increment < 0 then
           begin
             if ST.wMonth = 1 then
                i := DaysPerMonth(ST.wYear - 1, 12)
             else
                i := DaysPerMonth(ST.wYear, ST.wMonth - 1);
             if st.wDay > i then
               DoSetDateTime(FDate - DaysPerMonth(ST.wYear, ST.wMonth))
             else
               DoSetDateTime(FDate - i);
           end else
           begin
             if ST.wMonth = 12 then
                i := DaysPerMonth(ST.wYear + 1, 1)
             else
                i := DaysPerMonth(ST.wYear, ST.wMonth + 1);
             if ST.wDay > i then
               DoSetDateTime(FDate + DaysPerMonth(ST.wYear, ST.wMonth) - st.wDay + i)
             else
               DoSetDateTime(FDate + DaysPerMonth(ST.wYear, ST.wMonth));
           end;
           DateTimeToSystemTime(FDate, ST);
         end else
         begin
           j := ST.wMonth;
           j := j + Increment;
           if j > 12 then
              j := j mod 12;
           if j < 1 then
              j := 12 + j mod 12;
           if j = 0 then j := 12;
           ST.wMonth := j;
           ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
         end;
       end;
    3,
    10:begin
         if FAltChangeMethod then
         begin
           DoSetDateTime(FDate + Increment);
           DateTimeToSystemTime(FDate, ST);
         end
         else
         begin
           i := DaysPerMonth(ST.wYear, ST.wMonth);
           ST.wDay := ST.wDay + Increment;
           if ST.wDay > i then
              ST.wDay := ST.wDay mod i;
           if ST.wDay < 1 then
              ST.wDay := i + ST.wDay mod i;
           if ST.wDay = 0 then ST.wDay := i;
         end;
       end;
    4,
    5: begin
         if FAltChangeMethod then
         begin
           DoSetDateTime(IncTime(FDate, Increment, 0, 0, 0));
           DateTimeToSystemTime(FDate, ST);
         end
         else
         begin
           i := 24;
           j := ST.wHour;
           j := j + Increment;
           if j >= i then
              j := j mod i;
           if j < 0 then
              j := i + j mod i;
           ST.wHour := j;
         end;
       end;
    6: begin
         if FAltChangeMethod then
         begin
           DoSetDateTime(IncTime(FDate, 0, Increment, 0, 0));
           DateTimeToSystemTime(FDate, ST);
         end
         else
         begin
           i := 59;
           j := ST.wMinute;
           j := j + Increment;
           if j > i then
              j := (j - 1) mod i;
           if j < 0 then
              j := i + (j + 1) mod i;
           if j < 0 then
              j := i;
           ST.wMinute := j;
         end;
       end;
    7: begin
         if FAltChangeMethod then
         begin
           DoSetDateTime(IncTime(FDate, 0, 0, Increment, 0));
           DateTimeToSystemTime(FDate, ST);
         end else
         begin
           i := 59;
           j := ST.wSecond;
           j := j + Increment;
           if j > i then
              j := (j - 1) mod i;
           if j < 0 then
              j := i + (j + 1) mod i;
           if j < 0 then
              j := i;
           ST.wSecond := j;
         end;
       end;
    8: begin
         if FAltChangeMethod then
         begin
           if Increment > 0 then
              DoSetDateTime(IncTime(FDate, 12, 0, 0, 0))
           else
              DoSetDateTime(IncTime(FDate, -12, 0, 0, 0));

           DateTimeToSystemTime(FDate, ST);
         end else
         begin
           if ST.wHour >= 12 then
              ST.wHour := ST.wHour - 12
           else
              ST.wHour := ST.wHour + 12;
         end;
       end;
  end;
  DT := SystemTimeToDateTime(ST);
  if DT <> FDate then
  begin
    DoSetDateTime(DT);
    UpdateText;
    TriggerChangeEvent;
    InvalidateEdit;
  end;
end;

procedure TElDateTimePicker.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end;
end;

procedure TElDateTimePicker.UpdatePart;
var i : integer;
    Part  : PDTFPart;
begin
  if (FCurPart <> -1) then
     Part := PDTFPart(DTFParts[FCurPart])
  else
     Part := nil;
  if (Part = nil) or (not (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8])) then
  begin
    for i := 0 to DTFParts.Count - 1 do
    begin
      Part := PDTFPart(DTFParts[i]);
      if (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8]) then
      begin
        FCurPart := i;
        FDI := '';
        exit;
      end;
    end;
    FCurPart := -1;
    FDI := '';
  end;
end;

procedure TElDateTimePicker.UpdateText;
var i  : integer;
    s,
    S1 : TElFString;
    Part : PDTFPart;
    ST : TSystemTime;
    R  : TRect;
    x  : integer;
begin
  S := '';
  x := 0;
  DateTimeToSystemTime(FDate, ST);
  ST.WDayOfWeek := DayOfWeek(FDate) - 1;

  Canvas.Font.Assign(Font);

  for i := 0 to DTFParts.Count - 1 do
  begin
    Part := PDTFPart(DTFParts[i]);
    case Part.DPart of
      -1: begin
            S1 := Part.Text;
            Part.SPos := Length(S) + 1;
            Part.EPos := Part.SPos + Length(S1);
          end;
       0: begin
            if Unassigned then
              S1 := MakeUnassignedString(4)
            else
              S1 := IntToStr(ST.wYear);
          end;
       1: begin
            if Unassigned then
              S1 := MakeUnassignedString(2)
            else
            begin
              S1 := IntToStr(ST.wYear mod 100);
              if Length(S1) = 1 then
                 S1 := '0' + S1;
            end;
          end;
       2: begin
            if Unassigned then
              S1 := MakeUnassignedString(2)
            else
            begin
              if Part.Text = 'M' then
                S1 := IntToStr(St.wMonth)
              else
              if Part.Text = 'MM' then
              begin
                if ST.wMonth < 10 then
                  S1 := '0' + IntToStr(St.wMonth)
                else
                  S1 := IntToStr(St.wMonth);
              end else
              if Part.Text = 'MMM' then
                S1 := ShortMonthNames[St.wMonth]
              else
              if Part.Text = 'MMMM' then
                S1 := LongMonthNames[St.wMonth];
            end;
          end;
       3: begin
            if Unassigned then
              S1 := MakeUnassignedString(2)
            else
            begin
              if Length(Part.Text) = 1 then
              begin
                S1 := IntToStr(St.wDay)
              end else
              begin
                if St.wDay < 10 then
                  S1 := '0' + IntToStr(St.wDay)
                else
                  S1 := IntToStr(St.wDay);
              end;
            end;
          end;
       4: begin
            if Unassigned then
              S1 := MakeUnassignedString(2)
            else
            begin
              if Use12Hours then
              begin
                if ST.wHour mod 12 = 0 then
                   S1 := '12'
                else
                   S1:= IntToStr(ST.wHour mod 12);
              end else
              begin
                S1:= IntToStr(ST.wHour);
                if (Length(Part.Text) = 2) and (Length(S1) = 1) then
                  Insert('0', S1, 1);
              end;
            end;
          end;
       6: begin
            if Unassigned then
              S1 := MakeUnassignedString(2)
            else
            begin
              if Length(Part.Text) = 1 then
              begin
                S1 := IntToStr(St.wMinute)
              end else
              begin
                if St.wMinute < 10 then
                  S1 := '0' + IntToStr(St.wMinute)
                else
                  S1 := IntToStr(St.wMinute);
              end;
            end;
          end;
       7: begin
            if Unassigned then
              S1 := MakeUnassignedString(2)
            else
            begin
              if Length(Part.Text) = 1 then
              begin
                S1 := IntToStr(St.wSecond)
              end else
              begin
                if St.wSecond < 10 then
                  S1 := '0' + IntToStr(St.wSecond)
                else
                  S1 := IntToStr(St.wSecond);
              end;
            end;
          end;
       8: begin
            if Unassigned then
              S1 := 'am'
            else
            begin
              S1 := lowercase(Part.Text);
              if s1 = 'ampm' then
              begin
                if ST.wHour < 12 then
                   S1 := TimeAMString
                else
                   S1 := TimePMString;
              end else
              if s1 = 'am/pm' then
              begin
                if ST.wHour < 12 then
                   S1 := Copy(Part.Text, 1, 2)
                else
                   S1 := Copy(Part.Text, 4, 2);
              end else
              if s1 = 'a/p' then
              begin
                if ST.wHour < 12 then
                   S1 := Part.Text[1]
                else
                   S1 := Part.Text[3];
              end;
            end;
          end;
       9: begin
            if Part.Text = '/' then
               S1 := DateSeparator
            else
            if Part.Text = ':' then
               S1 := TimeSeparator;
          end;
       10:begin
            if Length(Part.Text) = 3 then
               S1 := ShortDayNames[ST.wDayOfWeek + 1]
            else
               S1 := LongDayNames[ST.wDayOfWeek + 1];
          end;
    end;

    Part.TSPos := Length(S) + 1;
    Part.TEPos := Part.TSPos + Length(S1);

    Part.SPos := X;

    SetRectEmpty(R);

    {$ifndef CLX_USED}
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(S1), Length(S1), R, DT_LEFT or DT_SINGLELINE or DT_TOP or DT_CALCRECT);
    {$else}
    DrawText(Canvas.Handle, PChar(S1), Length(S1), R, DT_LEFT or DT_SINGLELINE or DT_TOP or DT_CALCRECT);
    {$endif}
    {$else}
    Canvas.TextExtent(S1, R, Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignTop) or Integer(AlignmentFlags_SingleLine));
    // Canvas.TextRect(R, R.Left, R.Top, Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignTop) or Integer(AlignmentFlags_SingleLine));
    {$endif}

    Part.EPos := Part.SPos + R.Right - R.Left + 1;

    Inc(X, R.Right - R.Left);

    S := S + S1;
  end;

  {$warnings off}
  if Unassigned and (not Focused) and (Length(S) > 0) then
    S := MakeUnassignedString(Length(S));
    
  SetRectEmpty(R);
  {$warnings on}
  {$ifndef CLX_USED}
  {$ifdef ELPACK_UNICODE}
  ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R, DT_LEFT or DT_SINGLELINE or DT_TOP or DT_CALCRECT);
  {$else}
  DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_LEFT or DT_SINGLELINE or DT_TOP or DT_CALCRECT);
  {$endif}
  {$else}
  Canvas.TextExtent(S, R, Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignTop) or Integer(AlignmentFlags_SingleLine));
  {$endif}

  x := R.Right - R.Left + 1;

  SetEditRect;

  case Alignment of
    taLeftJustify:   x := GetCheckWidth;
    taRightJustify:  x := (ClientWidth - BtnWidth - GetCheckWidth - x);
    taCenter: x := (ClientWidth - BtnWidth - GetCheckWidth - x) div 2;
  end;

  X := Max(x, GetCheckWidth + {$ifndef CLX_USED}GetSystemMetrics(SM_CYBORDER) * 2{$else}4{$endif});

  if x > 0 then
  begin
    for i := 0 to DTFParts.Count - 1 do
    begin
      Part := PDTFPart(DTFParts[i]);
      Inc(Part.SPos, x);
      Inc(Part.EPos, x);
    end;
  end;

  FText := S;
end;

procedure TElDateTimePicker.ParseFormat(AFormat : string);
var P : PChar;
    pt: char;
    CurText : string;
    dtp : PDTFPart;
    Starter : Char;
    Count : integer;
    LastToken: char;
    c : integer;
    b : boolean;
    Format : string;

    procedure GetCount;
    var St : char;
    begin
      Count := 0;
      ST := UpCase(Starter);
      while UpCase(P^) = St do
      begin
        Inc(P);
        Inc(Count);
      end;
      dec(P);
    end;

begin
  Format := StrPas(PChar(AFormat));
  pt := #0;
  CurText := '';
  Use12Hours := false;
  P := PChar(Format);
  LastToken := #0;
  while P^ <> #0 do
  begin

    if (UpCase(P^) = 'M') and (Upcase(LastToken) = 'H') then
       P^ := 'N';

    if ((P^ = '''') or (P^ = '"')) then
    begin
      if pt = #0 then
         pt := P^
      else
      if pt = P^ then
      begin
        New(dtp);
        dtp.Text := CurText;
        LastToken := #0;
        CurText  := '';
        dtp.DPart := -1;
        DTFParts.Add(dtp);
        pt := #0;
      end
      else
        CurText := CurText + P^;
    end else
    if pt <> #0 then
       CurText := CurText + P^
    else
    begin
      if CurText <> '' then
      begin
        New(dtp);
        dtp.Text := CurText;
        LastToken := #0;
        CurText  := '';
        dtp.DPart := -1;
        DTFParts.Add(dtp);
      end;
      if Upcase(P^) = 'Y' then
      begin
        LastToken := P^;
        Starter := P^;
        GetCount;
        New(dtp);
        DTFParts.Add(dtp);
        if Count <= 2 then
        begin
          dtp.DPart := 1;
          dtp.Text := 'YY';
        end else
        begin
          dtp.DPart := 0;
          dtp.Text := 'YYYY';
        end;
      end else
      if Upcase(P^) = 'M' then
      begin
        LastToken := P^;
        Starter := P^;
        GetCount;
        New(dtp);
        dtp.DPart := 2;
        DTFParts.Add(dtp);
        SetLength(dtp.Text, Count);

        FillChar(PChar(dtp.Text)^, Count, Ord('M'));
      end else
      if Upcase(P^) = 'D' then
      begin
        LastToken := P^;
        Starter := P^;
        GetCount;
        if Count < 3 then
        begin
          New(dtp);
          dtp.DPart := 3;
          DTFParts.Add(dtp);
          SetLength(dtp.Text, Count);
          FillChar(PChar(dtp.Text)^, Count, Ord('D'));
        end
        else
        if Count < 5 then
        begin
          New(dtp);
          dtp.DPart := 10;
          DTFParts.Add(dtp);
          SetLength(dtp.Text, Count);
          FillChar(PChar(dtp.Text)^, Count, Ord('D'));
        end
        else
        begin
          c := (P - PChar(Format)) - count + 2;
          if count = 6 then
          begin
            Delete(Format, C, 6);
            Insert(LongDateFormat, Format, C);
            P := PChar(Format) + C - 2;
          end
          else
          begin
            Delete(Format, C, 5);
            Insert(ShortDateFormat, Format, C);
            P := PChar(Format) + C - 2;
          end;
        end;
      end else
      if Upcase(P^) = 'H' then
      begin
        LastToken := P^;
        inc(p);
        if (P^ = 'H') or (p^ = 'h') then
           Count := 2
        else
        begin
          Count := 1;
          dec(P);
        end;
        New(dtp);
        dtp.DPart := 4;
        DTFParts.Add(dtp);
        SetLength(dtp.Text, Count);
        FillChar(PChar(dtp.Text)^, Count, Ord('H'));
      end else
      if Upcase(P^) = 'N' then
      begin
        LastToken := P^;
        inc(p);
        if (P^ = 'N') or (p^ = 'n') or (p^='M') or (p^='m') then
           Count := 2
        else
        begin
          Count := 1;
          dec(P);
        end;
        New(dtp);
        dtp.DPart := 6;
        DTFParts.Add(dtp);
        SetLength(dtp.Text, Count);
        FillChar(PChar(dtp.Text)^, Count, Ord('N'));
      end else
      if Upcase(P^) = 'S' then
      begin
        LastToken := P^;
        inc(p);
        if (P^ = 'S') or (p^ = 's') then
           Count := 2
        else
        begin
          Count := 1;
          dec(P^);
        end;
        New(dtp);
        dtp.DPart := 7;
        DTFParts.Add(dtp);
        SetLength(dtp.Text, Count);
        FillChar(PChar(dtp.Text)^, Count, Ord('S'));
      end
      else
      if Upcase(P^) = 'C' then
      begin
        LastToken := P^;
        c := P - PChar(Format);
        Delete(Format, C + 1, 1);
        Insert(ShortDateFormat + #32 + LongTimeFormat, Format, C);
        P := PChar(Format) + C - 1;
      end
      else
      if Upcase(P^) = 'T' then
      begin
        LastToken := P^;
        c := P - PChar(Format);
        inc(P);
        if (p^ = 't') or (p^ = 'T') then
        begin
          Delete(Format, C + 1, 2);
          Insert(LongTimeFormat, Format, C + 1);
          P := PChar(Format) + C - 1;
        end
        else
        begin
          dec(p);
          Delete(Format, C + 1, 1);
          Insert(ShortTimeFormat, Format, C + 1);
          P := PChar(Format) + C - 1;
        end;
      end else
      if Upcase(P^) = 'A' then
      begin
        LastToken := P^;
        if ( (StrLIComp(P, 'AM/PM', 5) = 0)
           or (StrLIComp(P, 'A/P',   3) = 0)
           or (StrLIComp(P, 'AMPM',  4) = 0) ) then
          Use12Hours := true;
        b := false;
        if StrLIComp(P, 'AM/PM', 5) = 0 then
        begin
          c := 5;
          b := true;
        end else
        if StrLIComp(P, 'A/P', 3) = 0 then
        begin
          c := 3;
          b := true;
        end else
        if StrLIComp(P, 'AMPM', 4) = 0 then
        begin
          c := 4;
          b := true;
        end else
        if StrLIComp(P, 'AAAA', 4) = 0 then
        begin
          c := 4;
          b := false;
        end else
        if StrLIComp(P, 'AAA', 3) = 0 then
        begin
          c := 3;
          b := false;
        end else c:= 0;
        if b then
        begin
          New(dtp);
          dtp.DPart := 8;
          DTFParts.Add(dtp);
          SetLength(dtp.Text, c);
          StrPLCopy(PChar(dtp.Text), P, c);
        end else
        if c <> 0 then
        begin
          New(dtp);
          dtp.DPart := 10;
          DTFParts.Add(dtp);
          SetLength(dtp.Text, c);
          StrPLCopy(PChar(dtp.Text), P, c);
        end;
        if C > 0 then
          inc(P, C - 1);
      end else
      if (P^ = '/') or (P^ = ':') then
      begin
        New(dtp);
        dtp.DPart := 9;
        DTFParts.Add(dtp);
        dtp.Text := P^;
      end else
      if (P^ in [' ', '.', ',', ';', '\', '-', '_', '@']) then
        CurText := CurText + P^;
    end;
    inc(P);
  end;
end;

function TElDateTimePicker.GetStdFormat(Fmt : TElDatePickerFormat) : string;
begin
  case Fmt of
    edfShortDateLongTime: result := 'c';
    edfLongDate         : result := LongDateFormat;
    edfShortDate        : result := ShortDateFormat;
    edfLongTime         : result := LongTimeFormat;
    edfShortTime        : result := ShortTimeFormat;
    edfCustom           : result := 'c';
  end;
end;

procedure TElDateTimePicker.SetFormat(newValue : TElDatePickerFormat);
var AF : string;
begin
  if FFormat <> newValue then
  begin
    FFormat := newValue;
    FCurPart := -1;
    FDI := '';
    DTFParts.Clear;
    if FFormat = edfCustom then
       AF := FFormatStr
    else
       AF := GetStdFormat(FFormat);
    ParseFormat(AF);
    UpdatePart;
    UpdateText;
    InvalidateEdit;
  end;
end;

procedure TElDateTimePicker.SetFormatStr(newValue : String);
var AF : string;
begin
  if (FFormatStr <> newValue) then
  begin
    if newValue = '' then
       newValue := 'c';
    FFormatStr := newValue;
    FCurPart := -1;
    FDI := '';                
    DTFParts.Clear;
    if FFormat = edfCustom then
       AF := FFormatStr
    else
       AF := GetStdFormat(FFormat);
    ParseFormat(AF);
    UpdatePart;
    UpdateText;
    InvalidateEdit;
  end;  { if }
end;  { SetFormat }

procedure TElDateTimePicker.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    UpdateText;
    InvalidateEdit;
  end;
end;

procedure TElDateTimePicker.SetDate(newValue : TDateTime);
begin
  SetDateTime(Frac(FDate) + Trunc(newValue));
end;

function TElDateTimePicker.GetDate : TDateTime;
begin
  result := Trunc(FDate);
end;

procedure TElDateTimePicker.SetTime(newValue : TDateTime);
begin
  SetDateTime(Trunc(FDate) + Frac(newValue));
end;

function TElDateTimePicker.GetTime : TDateTime;
begin
  result := Frac(FDate);
end;

procedure TElDateTimePicker.SetDateTime(newValue: TDateTime);
{ Sets data member FDate to newValue. }
begin
  if (FDate <> newValue) then
  begin
    if (NewValue <= MaxDate) and (NewValue >= MinDate) then
    begin
      if NewValue < 2 then
         NewValue := 2;
      FDate := newValue;
      UpdateText;
      InvalidateEdit;
    end;
  end;  { if }
end;  { SetDate }

function TElDateTimePicker.GetCheckDims  : integer;
begin
  result := ClientHeight +1 - {$ifndef CLX_USED}GetSystemMetrics(SM_CYBORDER) * 2{$else}4{$endif};
end;

function TElDateTimePicker.GetCheckWidth : integer;
begin
  if FShowCheckBox then
    result := ClientHeight
  else
    result := 0;
end;

function TElDateTimePicker.GetBtnWidth : integer;
begin
  if FShowPopupCalendar then
     result := FBtnWidth * 2
  else
     result := FBtnWidth;
  if not (ButtonVisible and ((not ButtonShowOnFocus) or Focused)) then
    dec(Result, FBtnWidth);
end;

procedure TElDateTimePicker.DoDropDown;
begin
  if Assigned(FOnDropDown) then FOnDropDown(Self);
end;

procedure TElDateTimePicker.CloseUp(AcceptValue: boolean);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
  if FForm.Visible then
  begin
    SetWindowPos(FForm.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FForm.Visible := False;
    FDroppedDown := false;
    if AcceptValue then
    begin
      Date := Trunc(FForm.Calendar.Date);
      Unassigned := false;
      Modified := true;
      TriggerChangeEvent;
    end;
    FCalButton.Down := false;
    DoDropDown;
  end;
{$endif}
end;

type
     TElDTPickButton = class(TElGraphicButton)
     protected
       function GetThemedClassName: WideString; override;
       function GetThemePartID: Integer; override;
       function GetThemeStateID: Integer; override;
     end;

{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
type
      THackCalendarForm = class(TElCalendarForm)
      private
        procedure CMDeactivate(var Msg : TMessage); message CM_DEACTIVATE;
      protected
        procedure CreateWindowHandle(const Params: TCreateParams); override;
      public
        procedure CreateParams(var Params : TCreateParams); override;
      end;

procedure THackCalendarForm.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure THackCalendarForm.CMDeactivate(var Msg : TMessage);
begin
  inherited;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure THackCalendarForm.CreateParams(var Params : TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_POPUP or WS_DLGFRAME;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  end;
end;
{$endif}

procedure TElDateTimePicker.DropDown;
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
var
  P: TPoint;
{$endif}
begin
  {$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
  if FForm.Visible then
  begin
    CloseUp(False);
  end
  else
  begin
    if not ReadOnly then
    begin
      if not FFocused then
         SetFocus;

      P := Parent.ClientToScreen(Point(Left, Top));
      Inc(P.Y, Height);
      if P.Y + FForm.Height > Screen.Height then
        P.Y := P.Y - FForm.Height - Height;

      SetWindowPos(FForm.Handle, HWND_TOPMOST, P.X, P.Y, 0, 0,
        SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE or SWP_NOZORDER);

      FCalButton.Down := true;
      FDroppedDown := true;
      DoDropDown;

      FForm.Calendar.Date := FDate;
      FForm.Visible := True;
      FForm.Calendar.SetFocus;
    end;
  end;
{$endif}
end;

procedure TElDateTimePicker.SetEditRect;  { protected }
var
  R : TRect;
begin
  R := Rect(ClientWidth - FBtnWidth + 1, 0, ClientWidth, ClientHeight);
  if FShowPopupCalendar then
  begin
    FCalButton.BoundsRect := R;
    FCalButton.Visible := true;
    if ButtonVisible and ((not ButtonShowOnFocus) or Focused) then
      R := Rect(ClientWidth - FBtnWidth * 2 + 2, 0, ClientWidth - FBtnWidth + 1, ClientHeight)
    else
      R := Rect(ClientWidth - FBtnWidth + 1, 0, ClientWidth - 1, ClientHeight)
  end
  else
  begin
    if csDesigning in ComponentState then
       FCalButton.BoundsRect := Rect(-1, -1, -1, -1)
    else
       FCalButton.Visible := false;
  end;

  FButton.BoundsRect := R;
  if ButtonVisible and ((not ButtonShowOnFocus) or Focused) then
    FButton.Visible := true
  else
    if csDesigning in ComponentState then
      FButton.BoundsRect := Rect(-1, -1, -1, -1)
    else
      FButton.Visible := false;
end; {SetEditRect}

{$ifndef CLX_USED}
procedure TElDateTimePicker.DrawFlatBorder;
var
  DC : HDC;
  R,
  R1 : TRect;
  ax,
  ay : integer;
  b  : boolean;
  BS : TElFlatBorderType;
  AColor : TColor;
  
begin
  R := Rect(0, 0, Width, Height);
  DC := GetWindowDC(Handle);
  try
    if IsThemeApplied and (BorderStyle = bsSingle) then
    begin
      R1 := ClientRect;
      R1.TopLeft := Parent.ScreenToClient(ClientToScreen(R1.TopLeft));

      ax := Left - R1.Left;
      ay := Top  - R1.Top;

      R1 := ClientRect;
      OffsetRect(R1, -ax, -ay);

      with R1 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      DrawThemeBackground(Theme, DC, 0, 0, R, nil);
    end
    else
    if BorderStyle = bsSingle then
    begin
      b := Focused or FMouseOver;
      if b then
         BS := FActiveBorderType
      else
         BS := FInactiveBorderType;
      if Focused or FMouseOver then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;

      DrawFlatFrameEx2(DC, R, AColor, Color, b, Enabled, FBorderSides, BS);
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;
{$endif}

procedure TElDateTimePicker.RedoTmpBmp;
var BgRect,
    BgRect2 : TRect;
begin
  if BackgroundType in [bgtHorzGradient, bgtVertGradient, bgtColorFill, bgtCenterBitmap] then
  begin
    FTmpBmp.FreeImage;
  end else
  begin
    if (ClientWidth <> 0) and (ClientHeight <> 0) then
    begin
      FTmpBmp.Height := ClientHeight - 1;
      FTmpBmp.Width := ClientWidth - 1;
      BgRect := ClientRect;
      BgRect2 := BgRect;
      OffsetRect(BgRect2, BgRect2.Left, BgRect2.Top);
      ExtDrawBkgnd(FTmpBmp.Canvas.Handle, Handle, BgRect, BgRect, BgRect, BgRect2, false, Color, Color, false, Background, BackgroundType);
    end;
  end;
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.CMMouseEnter(var Msg : TMessage);  { private }
{$else}
procedure TElDateTimePicker.MouseEnter(AControl: TControl);
{$endif}
begin
  inherited;
  FMouseOver := true;
  // FButton.Flat := ((not Checked) and ShowCheckBox and Flat);
  FCalButton.OldStyled := FButton.OldStyled;
  {$ifndef CLX_USED}
  if (Flat and (not Focused)) and (not IsThemeApplied) then DrawFlatBorder;
  if Flat and ShowCheckBox then Invalidate;
  {$endif}
end;  { CMMouseEnter }

procedure TElDateTimePicker.SetGradientStartColor(newValue : TColor);
{ Sets data member FGradientStartColor to newValue. }
begin
  if (FGradientStartColor <> newValue) then
  begin
    FGradientStartColor := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then InvalidateEdit;
  end;  { if }
end;  { SetGradientStartColor }

procedure TElDateTimePicker.SetGradientEndColor(newValue : TColor);
{ Sets data member FGradientEndColor to newValue. }
begin
  if (FGradientEndColor <> newValue) then
  begin
    FGradientEndColor := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then InvalidateEdit;
  end;  { if }
end;  { SetGradientEndColor }

procedure TElDateTimePicker.SetGradientSteps(newValue : Integer);
{ Sets data member FGradientSteps to newValue. }
begin
  if (FGradientSteps <> newValue) and (newValue > 0) then
  begin
    FGradientSteps := newValue;
    if (FBackgroundType = bgtHorzGradient) or (FBackgroundType = bgtVertGradient) then InvalidateEdit;
  end;  { if }
end;  { SetGradientSteps }

procedure TElDateTimePicker.SetBackground(newValue : TBitmap);
begin
  FBackground.Assign(newValue);
  if (FBackground.Empty) and (FBackGroundType in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap])
     then BackgroundType := bgtColorFill;
end; {SetBackground}

procedure TElDateTimePicker.SetBackgroundType(newValue : TElBkGndType);
begin
  if (FBackgroundType <> newValue) then
  begin
    FBackgroundType := newValue;
    if (FBackground.Empty) and (FBackGroundType in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap])
       then FBackgroundType := bgtColorFill;
    RedoTmpBmp;
    InvalidateEdit;
  end; {if}
end; {SetBackgroundType}

procedure TElDateTimePicker.ImageChange(Sender : TObject);
begin
  if ((FBackground.Height = 0) or (FBackground.Width = 0)) then
    BackgroundType := bgtColorFill
  else
  begin
    if not (BackgroundType in [bgtColorFill, bgtCenterBitmap]) then RedoTmpBmp;
    Invalidate;
  end;
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.ImageFormChange(Sender : TObject);
begin
  InvalidateEdit;
end;
{$endif}

procedure TElDateTimePicker.SetTransparent(newValue : Boolean);
{ Sets data member FTransparent to newValue. }
begin
  if (FTransparent <> newValue) then
  begin
    FTransparent := newValue;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end; { if }
end; { SetTransparent }

{$ifndef CLX_USED}
procedure TElDateTimePicker.SetImageForm(newValue : TElImageForm);
begin
  if (FImgForm <> newValue) then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnregisterChanges(FImgFormChLink);
    end;
    FImgForm := newValue;
    if (newValue <> nil) then
    begin
      newValue.FreeNotification(Self);
      FImgForm.RegisterChanges(FImgFormChLink);
    end;
    if not (csDesigning in ComponentState) then InvalidateEdit;
  end;  { if }
end;  { SetImageForm }
{$endif}

{$ifndef CLX_USED}
procedure TElDateTimePicker.CMMouseLeave(var Msg : TMessage);  { private }
{$else}
procedure TElDateTimePicker.MouseLeave;
{$endif}
begin
  FMouseOver := false;
  //FButton.Flat := Flat and ((not Focused) or ((not Checked) and ShowCheckBox and Flat));
  FCalButton.OldStyled := FButton.OldStyled;
  {$ifndef CLX_USED}
  if (Flat and (not Focused)) and (not IsThemeApplied) then DrawFlatBorder;
  if Flat and ShowCheckBox then Invalidate;
  {$endif}
  inherited;
end;  { CMMouseLeave }

{$ifndef CLX_USED}
procedure TElDateTimePicker.CMSysColorChange(var Msg : TMessage);  { private }
{$else}
procedure TElDateTimePicker.PaletteChanged(Sender: TObject);
{$endif}
begin
  inherited;
  InvalidateEdit;
end;  { CMSysColorChange }

{$ifndef CLX_USED}
procedure TElDateTimePicker.CMFontChanged(var Msg : TMessage);  { private }
{$else}
procedure TElDateTimePicker.FontChanged;
{$endif}
begin
  inherited;
  {$ifndef CLX_USED}
  if (csFixedHeight in ControlStyle) and
     not ((csDesigning in ComponentState) and
     (csLoading in ComponentState)) then
    AdjustHeight;
  {$endif}
  UpdateText;
  SetEditRect;
end;  { CMFontChanged }

{$ifndef CLX_USED}
procedure TElDateTimePicker.CMCtl3DChanged(var Msg : TMessage); { private }
begin
  inherited;
  RecreateWnd;
end; { CMCtl3DChanged }
{$endif}

procedure TElDateTimePicker.SetActiveBorderType(newValue : TElFlatBorderType);
begin
  if (FActiveBorderType <> newValue) then
  begin
    FActiveBorderType := newValue;

    UpdateButtonStyles;

    {$ifndef CLX_USED}
    if (Focused or FMouseOver) then UpdateFrame;
    {$endif}
  end;  { if }
end;  { SetActiveBorderType }

procedure TElDateTimePicker.SetInactiveBorderType(newValue : TElFlatBorderType);
{ Sets data member FInactiveBorderType to newValue. }
begin
  if (FInactiveBorderType <> newValue) then
  begin
    FInactiveBorderType := newValue;
    {$ifndef CLX_USED}
    if (not (Focused or FMouseOver)) then UpdateFrame;
    {$endif}

    UpdateButtonStyles;

  end;  { if }
end;  { SetInactiveBorderType }

procedure TElDateTimePicker.SetNavigationInPopup(newValue : boolean);
begin
  if FNavigationInPopup <> newValue then
  begin
    FNavigationInPopup := newValue;
    if newValue then
    begin

{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
      FForm.Panel1.Visible := true;
      FForm.Height := FForm.Height + FForm.Panel1.Height;
{$endif}
{$endif}
    end else
    begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
      FForm.Panel1.Visible := false;
      FForm.Height := FForm.Height - FForm.Panel1.Height;
{$endif}
{$endif}
    end;
  end;
end;

procedure TElDateTimePicker.SetFlat(newValue : Boolean);
{ Sets data member FFlat to newValue. }
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;

    FButton.OldStyled := not Flat;

    UpdateButtonStyles;

    FCalButton.OldStyled := FButton.OldStyled;
    {$ifndef CLX_USED}
    UpdateFrame;
    {$endif}
  end;  { if }
end;  { SetFlat }

{$ifndef CLX_USED}
procedure TElDateTimePicker.WMSize(var Msg : TWMSize);  { private }
{$else}
procedure TElDateTimePicker.Resize;
{$endif}
begin
  inherited;
  UpdateText;
  SetEditRect;
  InvalidateEdit;
end;  { WMSize }

procedure TElDateTimePicker.CalBtnClick(Sender : TObject);
begin
  DropDown;
end;

procedure TElDateTimePicker.SpinUpClick(Sender : TObject; Increment : Double);
begin
  if CanFocus
  {$ifndef CLX_USED}
  and (Windows.GetFocus <> Handle)
  {$endif}
  then SetFocus;
  if not ReadOnly then
  begin
    if Unassigned then
    begin
      Unassigned := false;
      TriggerChangeEvent;
    end
    else
      IncValue(1);
  end;
end;

procedure TElDateTimePicker.SpinDownClick(Sender : TObject; Increment : Double);
begin
  if CanFocus
  {$ifndef CLX_USED}
  and (Windows.GetFocus <> Handle)
  {$endif}
  then SetFocus;
  if not ReadOnly then
  begin
    if Unassigned then
    begin
      Unassigned := false;
      TriggerChangeEvent;
    end
    else
      IncValue(-1);
  end;
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.WMSysKeyDown(var Msg: TWMKeyDown);
begin
  if (KeyDataToShiftState(Msg.KeyData) = [ssAlt]) and
     ShowPopupCalendar and
     (Not ShowCheckBox or Checked) then
  begin
    if (Msg.CharCode = VK_DOWN) then
    begin
      Msg.CharCode := 0;
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
      DroppedDown := true
{$endif}
    end
    else
    if (Msg.CharCode = VK_UP) then
    begin
      Msg.CharCode := 0;
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
      DroppedDown := false;
{$endif}
    end;
  end;
  inherited;
end;
{$endif}

procedure TElDateTimePicker.KeyDown(var Key : Word; Shift : TShiftState);
var i, ov: integer;
    Part : PDTFPart;
    b    : boolean;
    ST   : TSystemTime;
    ch   : boolean;
    wasPM: boolean;
begin
  if (Shift = []) then
  begin
    if (Key = VK_SPACE) and (ShowCheckBox and (FCurPart = -1)) then
    begin
      if ShowCheckBox then
      begin
        if not ReadOnly then
        begin
          Checked := not Checked;
          Modified := true;
          TriggerCheckboxChangeEvent;
        end;
      end;
      Key := 0;
    end
    else
    if ((not ShowCheckBox) or Checked) then
    begin
      if (Key = VK_DELETE) and (UnassignedAllowed) then
      begin
        if not ReadOnly then
        begin
          Unassigned := true;
          Modified := true;
          TriggerChangeEvent;
          Key := 0;
        end;
      end
      else
      if Key = VK_UP then
      begin
        if not ReadOnly then
        begin
          if Unassigned then
            Unassigned := false
          else
            IncValue(1);
          Modified := true;
        end;
        Key := 0;
      end
      else
      if Key = VK_DOWN then
      begin
        if not ReadOnly then
        begin
          if Unassigned then
            Unassigned := false
          else
            IncValue(-1);
          Modified := true;
        end;
        Key := 0;
      end
      else
      if Key = VK_LEFT then
      begin
        b := false;
        if FCurPart > 0 then
        for i := FCurPart - 1 downto 0 do
        begin
          Part := PDTFPart(DTFParts[i]);
          if (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8]) then
          begin
            FCurPart := i;
            FDI := '';
            UpdateText;
            InvalidateEdit;
            b := true;
            break;
          end;
        end;
        if not b and (ShowCheckBox) then
        begin
          if FCurPart = -1 then
          begin
            for i := DTFParts.Count - 1 downto 0 do
            begin
              Part := PDTFPart(DTFParts[i]);
              if (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8]) then
              begin
                FCurPart := i;
                FDI := '';
                UpdateText;
                InvalidateEdit;
                break;
              end;
            end;
          end else
          begin
            FCurPart := -1;
            InvalidateEdit;
          end;
        end;
        Key := 0;
      end else
      if Key = VK_RIGHT then
      begin
        if FCurPart <= DTFParts.Count - 1 then
        begin
          b := false;
          for i := FCurPart + 1 to DTFParts.Count - 1 do
          begin
            Part := PDTFPart(DTFParts[i]);
            if (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8]) then
            begin
              FCurPart := i;
              FDI := '';
              UpdateText;
              InvalidateEdit;
              b := true;
              break;
            end;
          end;
          if not b and (ShowCheckBox) then
          begin
            FCurPart := -1;
            InvalidateEdit;
          end;
        end;
        Key := 0;
      end
      else
      begin
        if FCurPart = -1 then
        begin
          inherited;
          exit;
        end;
        Part := PDTFPart(DTFParts[FCurPart]);
        ch := false;
        if (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8]) and
           ({$ifndef CLX_USED}
            (Key in [VK_NUMPAD0..VK_NUMPAD9]) or
            {$endif}
           (Char(Key) in
             ['0'..'9', 'a', 'A', 'p', 'P',
              GetTimeAMChar,
              GetTimePMChar,
              Upcase(GetTimeAMChar),
              Upcase(GetTimePMChar),
              DateSeparator,
              TimeSeparator])) then
        begin
          {$ifndef CLX_USED}
          if Key in [VK_NUMPAD0..VK_NUMPAD9] then
            Key := Key - (VK_NUMPAD0 - ord('0'));
          {$endif}
          DateTimeToSystemTime(FDate, ST);
          if Char(Key) in ['0'..'9'] then
          begin
            FDI := FDI + Char(Key);

            i := StrToIntDef(FDI, 1);
            case Part.DPart of
              0: begin
                   if Length(FDI) >= 4 then
                   begin
                     if (i < 1900) or (i > 9999) then
                     begin
                       FDI := FDI[Length(FDI)];
                       i := Byte(Key - ord('0') + 1900);
                     end;
                     if Length(FDI) >= 4 then
                     begin
                       if ST.wYear <> i then
                       begin
                         ST.wYear := i;
                         ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
                         Modified := true;
                         ch := true;
                       end;
                     end;
                   end;
                 end;
              1: begin
                   if (i < 0) or (i > 99) then
                   begin
                     FDI := FDI[Length(FDI)];
                     i := Byte(Key - ord('0'));
                   end;
                   ov := ST.wYear;

                   if i > 50 then
                      ST.wYear := 1900 + i
                   else
                      ST.wYear := 2000 + i;
                   if ov <> st.wYear then
                   begin
                     ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
                     Modified := true;
                     ch := true;
                   end;
                 end;
              2: begin
                   if (i < 1) or (i > 12) then
                   begin
                     if Byte(Key - Ord('0')) = 0 then
                     begin
                       i := st.wMonth;
                       FDI := '';
                     end
                     else
                     begin
                       i := Byte(Key - ord('0'));
                       FDI := FDI[Length(FDI)];
                     end;
                   end;

                   if ST.wMonth <> i then
                   begin
                     ch := true;
                     ST.wMonth := i;
                     ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
                     Modified := true;
                   end;
                 end;
              3: begin
                   if (i < 1) or (i > DaysPerMonth(ST.wYear, ST.wMonth)) then
                   begin
                     if Byte(Key - Ord('0')) = 0 then
                     begin
                       i := st.wDay;
                       FDI := '';
                     end
                     else
                     begin
                       i := Byte(Key - ord('0'));
                       FDI := FDI[Length(FDI)];
                     end;
                   end;
                   if St.wDay <> i then
                   begin
                     ST.wDay := i;
                     Modified := true;
                     ch := true;
                   end;
                 end;
              4,
              5: begin
                   if Use12Hours then
                   begin
                     if (i < 1) or (i > 12) then
                     begin
                       FDI := FDI[Length(FDI)];
                       i := Byte(Key - ord('0'));
                     end;
                   end
                   else
                   begin
                     if (i < 0) or (i > 23) then
                     begin
                       FDI := FDI[Length(FDI)];
                       i := Byte(Key - ord('0'));
                     end;
                   end;

                   ov := ST.wHour;

                   if Use12Hours then
                   begin
                     wasPM := ST.wHour >= 12;
                     if wasPM and (i<12) then
                        ST.wHour := i + 12
                     else
                     if (not wasPM) and (i = 12) then// user wants 12 AM.
                     begin
                       ST.wHour := 0;
                     end
                     else
                       ST.wHour := i;
                   end
                   else
                   begin
                     ST.wHour := i;
                   end;

                   if ov <> ST.wHour then
                   begin
                     Modified := true;
                     ch := true;
                   end;
                 end;
              6: begin
                   if (i < 0) or (i > 59) then
                   begin
                     FDI := FDI[Length(FDI)];
                     i := Byte(Key - ord('0'));
                   end;
                   if i <> ST.wMinute then
                   begin
                     ST.wMinute := i;
                     Modified := true;
                     ch := true;
                   end;
                 end;
              7: begin
                   if (i < 0) or (i > 59) then
                   begin
                     FDI := FDI[Length(FDI)];
                     i := Byte(Key - ord('0'));
                   end;
                   if i <> ST.wSecond then
                   begin
                     ST.wSecond := i;
                     Modified := true;
                     ch := true;
                   end;
                 end;
            end;
          end else
          if (FCurPart < DTFParts.Count - 2) and
             ((PDTFPart(DTFParts[FCurPart + 1]).DPart = 9) or
              ((PDTFPart(DTFParts[FCurPart + 1]).DPart = -1) and
               ((Part.DPart in [0..3]) and
                (PDTFPart(DTFParts[FCurPart + 1]).Text = DateSeparator)) or
               ((Part.DPart in [4..7]) and
                (PDTFPart(DTFParts[FCurPart + 1]).Text = TimeSeparator)))) then
          begin
            if Char(Key) = PDTFPart(DTFParts[FCurPart + 1]).Text then
            begin
              b := false;
              for i := FCurPart + 1 to DTFParts.Count - 1 do
              begin
                Part := PDTFPart(DTFParts[i]);
                if (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8]) then
                begin
                  FCurPart := i;
                  FDI := '';
                  UpdateText;
                  InvalidateEdit;
                  b := true;
                  break;
                end;
              end;
              if not b and (ShowCheckBox) then
              begin
                FCurPart := -1;
                InvalidateEdit;
              end;
            end;
          end
          else
          if Part.DPart = 8 then
          begin
            if ((Length(TimeAMString) > 0) and
                 ((Char(Key) = UpCase(TimeAMString[1])) or (Char(Key - 32) = UpCase(TimeAMString[1])))) then
            begin
              if ST.wHour >= 12 then
              begin
                Dec(ST.wHour, 12);
                Modified := true;
                ch := true;
              end;
            end else
            if ((Length(TimePMString) > 0) and
                 ((Char(Key) = UpCase(TimePMString[1])) or (Char(Key - 32) = UpCase(TimePMString[1])))) then
            begin
              if ST.wHour < 12 then
              begin
                Inc(ST.wHour, 12);
                Modified := true;
                ch := true;
              end;
            end else
            if ((LowerCase(Part.Text) = 'am/pm') and
                 (Char(Key) in ['a', 'p', 'a', 'p'])) then
            begin
              if (UpCase(Char(Key)) = 'A') and (ST.wHour >= 12) then
              begin
                Dec(ST.wHour, 12);
                Modified := true;
                ch := true;
              end
              else
              if (UpCase(Char(Key)) = 'P') and (St.wHour < 12) then
              begin
                Inc(ST.wHour, 12);
                Modified := true;
                ch := true;
              end;
            end;
          end;
          if ch then
            if not ReadOnly then
            begin
              if Unassigned then
                Unassigned := false
              else
                DoSetDateTime(SystemTimeToDateTime(ST));
            end;
          UpdateText;
          if ch and not ReadOnly then
            TriggerChangeEvent;
          InvalidateEdit;
        end;
      end;
    end;
  end;
  inherited;
end;

procedure TElDateTimePicker.OnDTFPartDelete(Sender : TObject; Item : Pointer);
begin
  if Item <> nil then
  begin
    Dispose(PDTFPart(Item));
  end;
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.UpdateFrame;
var R : TRect;
begin
  if not HandleAllocated then
     exit;
  R := Rect(0, 0, Width, Height);
  RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame );
end;
{$endif}

procedure TElDateTimePicker.Loaded;
var Fmt : string;
begin
  inherited;
  FButton.Width := FBtnWidth;
  if MinDate > MaxDate then
    MinDate := MaxDate;
  if UseCurrentDate then
    Date := Now; 
  if Date < MinDate then
    Date := MinDate;
  if Date > MaxDate then
    Date := MaxDate;
  Fmt := FFormatStr;
  FFormatStr := '~~~';
  SetFormatStr(Fmt);
  if ButtonVisible then
  begin
    SetEditRect;
    Invalidate;
  end;
  UpdateText;
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.CreateParams;
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or WS_CLIPCHILDREN;
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    if Transparent then
       ExStyle := ExStyle or WS_EX_TRANSPARENT
    else
       ExStyle := ExStyle and not WS_EX_TRANSPARENT;
    with Params.WindowClass do
      style := style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;
{$endif}

procedure TElDateTimePicker.Paint;  { protected }
var R, Rect,
    R1     : TRect;
    ax, ay : integer;
    {$ifndef CLX_USED}
    ACtl   : TWinControl;
    P      : TPoint;
    BgRect : TRect;
    {$endif}
    APart  : PDTFPart;
    AText  : TElFString;

begin

{$ifndef CLX_USED}
  if Flat and (not IsThemeApplied) then
    DrawFlatBorder;
{$endif}

  R := ClientRect;
  if ButtonVisible then
    Dec(R.Right, BtnWidth - 2);

  {$ifndef CLX_USED}
  if not Transparent then
  {$endif}
  begin
    {$ifndef CLX_USED}
    if (FImgForm <> nil) and (not (csDesigning in FImgForm.ComponentState)) then
    begin
      if FImgForm.Control <> Self then
      begin
        ACtl := FImgForm.GetRealControl;
        BgRect := R;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        P := Parent.ClientToScreen(Point(Left, Top));

        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);
        FImgForm.PaintBkgnd(Canvas.Handle, R, BgRect.TopLeft, false);
      end;
    end
    else
    {$endif}
    begin
      with Canvas do
      case BackgroundType of //
        bgtColorFill :
          begin
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := Color;
            Canvas.FillRect(R);
          end;
        bgtHorzGradient,
        bgtVertGradient:
          GradientFill(Canvas.Handle, R, GradientStartColor, GradientEndColor, GradientSteps, BackgroundType = bgtVertGradient);
        bgtStretchBitmap,
        bgtTileBitmap:
          begin
            CopyRect(R, FTmpBmp.Canvas, Classes.Rect(0, 0, FTmpBmp.Width, FTmpBmp.Height));
          end;
        bgtCenterBitmap :
          begin
            Brush.Color := Color;
            Rect := R;
            FillRect(Rect);
            R := Classes.Rect(0, 0, FBackground.Width, FBackground.Height);
            CenterRects(FBackground.Width, Rect.Right - Rect.Left, FBackground.Height, Rect.Bottom - Rect.Top, R1);
            OffsetRect(R1, Rect.Left, Rect.Top);
            CopyRect(R1, FBackground.Canvas, Classes.Rect(0, 0, FBackground.Width, FBackground.Height));
          end;
      end; // case
    end;
  {$ifndef CLX_USED}
  end
  else
  begin
    GetClipBox(Canvas.Handle, R);
    R1 := R;
    P := Parent.ScreenToClient(ClientToScreen(Point(0, 0)));
    with P do
      OffsetRect(R1, X, Y);
    RedrawWindow(Parent.Handle, @R1, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN or RDW_UPDATENOW);

    with R do
        BitBlt(Canvas.Handle, Left, Top, Right, Bottom, TmpDC, R.Left, R.Top, SRCCOPY);
  {$endif}
  end;

  R := ClientRect;
  if ButtonVisible then
    Dec(R.Right, BtnWidth - 2);
  
  if ShowCheckBox then
  begin
    R1 := Classes.Rect(0, 0, ClientHeight + 1, 0);
    R1.Bottom := R1.Right;
    ax := GetCheckDims;
    CenterRects(ax, R1.Right, ax, R1.Bottom, Rect);
    {$ifndef CLX_USED}
    if Checked then
      ay := DFCS_CHECKED
    else
      ay := 0;
    DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONCHECK or ay);
    if Flat then
      DrawFlatFrame(Canvas, Rect);
    {$else}
    Canvas.Start;

    if checked then
      ay := integer(QButtonToggleState_On)
    else
      ay := integer(QButtonToggleState_Off);

    QStyle_DrawIndicator(Application.Style.Handle,
                         Canvas.Handle,
                         Rect.Left,
                         Rect.Top,
                         Rect.Right - Rect.Left,
                         Rect.Bottom - Rect.Top,
                         QWidget_colorGroup(Handle),
                         ay, false, Enabled);

    Canvas.Stop;
    {$endif}

    if FFocused and (FCurPart = -1) then
       Canvas.DrawFocusRect(Rect);

    Inc(R.Left, ClientHeight);
  end;

  if DTFParts.Count > 0 then
  begin
    R1 := R;

    APart  := PDTFPart(DTFParts[0]);
    R1.Left := APart.SPos;
    APart  := PDTFPart(DTFParts.Last);
    R1.Right := APart.EPos + 1;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Assign(Font);
    if (ShowCheckBox and (not Checked)) or (ChangeDisabledText and (not Enabled)) then
       Canvas.Font.Color := clGrayText;

    {$ifndef CLX_USED}
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(FText), Length(FText), R1, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
    {$else}
    DrawText(Canvas.Handle, PChar(FText), Length(FText), R1, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
    {$endif}
    {$else}
    Canvas.TextRect(R1, R1.Left, R1.Top, FText, Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter) or Integer(AlignmentFlags_SingleLine));
    {$endif}

    if Unassigned and Focused then
    begin
      Canvas.Pen.Color := UnassignedColor;
      Canvas.MoveTo(R1.Left, R1.Bottom - 1);
      Canvas.LineTo(R1.Right, R1.Bottom - 1);
    end;
      
    if ((FCurPart > - 1) and FFocused) and ((not ShowCheckBox) or Checked) then
    begin
      R1 := R;
      APart := PDTFPart(DTFParts[FCurPart]);
      R1.Left := APart.SPos;
      R1.Right := APart.EPos;

      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
      {$ifndef CLX_USED}
      {$ifdef ELPACK_UNICODE}
      AText := WideCopy(FText, APart.TSPos, APart.TEPos - APart.TSPos + 1);
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(AText), Length(AText), R1, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
      {$else}
      AText := Copy(FText, APart.TSPos, APart.TEPos - APart.TSPos + 1);
      DrawText(Canvas.Handle, PChar(AText), Length(AText), R1, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
      {$endif}
      {$else}
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(R1);
      AText := WideCopy(FText, APart.TSPos, APart.TEPos - APart.TSPos + 1);
      Canvas.TextRect(R1, R1.Left, R1.Top, AText, Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter) or Integer(AlignmentFlags_SingleLine));
      {$endif}
    end;
  end;
end;  { Paint }

{$ifndef CLX_USED}
procedure TElDateTimePicker.WMPaint(var Msg : TWMPaint);  { private }
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  R : TRect;
  ARgn : HRGN;

begin
  if (Msg.DC <> 0) then
    PaintHandler(Msg)
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      //if not IsThemeApplied then
      //begin
        GetClipBox(DC, R);
        if IsRectEmpty(R) then
           R := ClientRect
        else
           InflateRect(R, 1, 1);
        with R do
         ARgn := CreateRectRgn(Left, Top, right, Bottom);
        SelectClipRgn(MemDC, ARgn);
      //end
      //else
      //  R := ClientRect;

      TmpDC := DC;

      Msg.DC := MemDC;
      WMPaint(Msg);
      SelectClipRgn(MemDC, 0);
      DeleteObject(ARgn);
      Msg.DC := 0;
      with R do
        BitBlt(DC, Left, Top, Right, Bottom, MemDC, Left, Top, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;  { WMPaint }
{$endif}

procedure TElDateTimePicker.Notification(AComponent : TComponent; operation : TOperation);
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) then
  begin
    {$ifndef CLX_USED}
    if (AComponent = FImgForm) then
      ImageForm := nil;
    {$endif}
  end;  { if }
end;  { Notification }

procedure TElDateTimePicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  { protected }
var
    APart : PDTFPart;
    i : integer;
    b : boolean;
begin
  inherited;
  if InDblClick then
     InDblClick := false
  else
  begin
    if CanFocus and (not FFocused) then
       SetFocus;
    if ShowCheckBox and (X < GetCheckWidth) then
    begin
      FCurPart := -1;
      Checked := not Checked;
      Modified := true;
      TriggerCheckboxChangeEvent;
    end
    else
      if (not ShowCheckBox) or Checked then
      begin
        FCurPart := -1;
        FDI := '';
        b := false;
        APart := nil;
        for i := 0 to DTFParts.Count - 1 do
        begin
          APart := PDTFPart(DTFParts[i]);
          if (X >= APart.SPos) and (X < APart.EPos) then
          begin
            if (APart.DPart in [0, 1, 2, 3, 4, 6, 7, 8, 10]) then
                b := true;
            break;
          end;
        end;

        if b then
        begin
          FCurPart := DTFParts.IndexOf(APart);
          FDI := '';
          InvalidateEdit;
        end;
      end;
  end;
end;  { MouseDown }

{$ifndef CLX_USED}
procedure TElDateTimePicker.WMNCPaint(var Msg : TMessage);  { private }
var DC : HDC;
begin
  if (not Flat) and (BorderStyle = bsSingle) then
    inherited;
  // if Flat and (not IsThemeApplied) then
  if Flat or IsThemeApplied then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC <> 0 then
       DrawFlatBorder
    else
    begin
      DC := GetWindowDC(Handle);
      DrawFlatBorder;
    end;
    ReleaseDC(Handle, DC);
    Msg.Result := 0;
  end;
end;  { WMNCPaint }
{$endif}

{$ifndef CLX_USED}
procedure TElDateTimePicker.WMSetFocus(var Msg : TWMSetFocus);  { private }
{$else}
procedure TElDateTimePicker.DoEnter;
{$endif}
begin
  inherited;
  FFocused := true;
  {$ifndef CLX_USED}
  if Flat then UpdateFrame;
  {$endif}
  //FButton.Flat := ((not Checked) and ShowCheckBox and Flat);
  FCalButton.OldStyled := not FButton.Flat;
  if (FCurPart = -1) and (DTFParts.Count > 0) then
    FCurPart := 0;
  if ButtonVisible and ButtonShowOnFocus then
  begin
    SetEditRect;
    Invalidate;
  end;
  if Unassigned then
    UpdateText;
  InvalidateEdit;
end;  { WMSetFocus }

{$ifndef CLX_USED}
procedure TElDateTimePicker.WMKillFocus(var Msg : TWMKillFocus);  { private }
{$else}
procedure TElDateTimePicker.DoExit;
{$endif}
begin
  FFocused := false;
  inherited;
  {$ifndef CLX_USED}
  if Flat then
    if HandleAllocated then 
      UpdateFrame;
  {$endif}
  //FButton.Flat := Flat and ((not FMouseOver) or ((not Checked) and ShowCheckBox and Flat));
  FCalButton.OldStyled := FButton.OldStyled;
  //FCurPart := -1;
  if ButtonVisible and ButtonShowOnFocus then
  begin
    SetEditRect;
    Invalidate;
  end;
  if Unassigned then
    UpdateText;
  InvalidateEdit;
end;  { WMKillFocus }

procedure TElDateTimePicker.InvalidateEdit;
var R : TRect;
begin
  if (csDestroying in ComponentState) or (Parent = nil) then
     exit;
  R := ClientRect;
  if not Transparent then
     Dec(R.Right, BtnWidth - 1);
  {$ifndef CLX_USED}
  InvalidateRect(Handle, @R, true);
  {$else}
  Inc(R.Bottom); Inc(R.Right);
  QWidget_update(Handle, @R);
  Dec(R.Bottom); Dec(R.Right);
  {$endif}
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.WMEraseBkgnd(var Msg : TWMEraseBkgnd);  { private }
begin
  Msg.Result := 1;
end;  { WMEraseBkgnd }

procedure TElDateTimePicker.WMGetDlgCode(var Msg : TWMGetDlgCode);  { private }
begin
  with TMessage(Msg) do
  begin
    Result := DefWindowProc(Handle, Msg, wParam, lParam);
    Result := (Result and (not DLGC_WANTALLKEYS)) or DLGC_WANTARROWS or DLGC_WANTCHARS;
    if HandleDialogKeys then
      result := result or DLGC_WANTALLKEYS;
  end;
end;  { WMGetDlgCode }
{$endif}

procedure TElDateTimePicker.TriggerChangeEvent;
begin
  if (assigned(FOnChange)) then
    FOnChange(Self);
end;  { TriggerChangeEvent }

(*
procedure TElDateTimePicker.WMChar(var Msg : TWMChar);  { private }
var Part : PDTFPart;
    i    : integer;
    ST   : TSystemTime;
    b    : boolean;
    ch   : boolean;
begin
  inherited;
  if FCurPart = -1 then
     exit;
  Part := PDTFPart(DTFParts[FCurPart]);
  ch := false;
  if (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8]) then
  begin
    DateTimeToSystemTime(FDate, ST);
    if Char(Msg.CharCode) in ['0'..'9'] then
    begin
      FDI := FDI + Char(Msg.CharCode);

      i := StrToIntDef(FDI, 1);
      case Part.DPart of
        0: begin
             if Length(FDI) >= 4 then
             begin
               if (i < 1900) or (i > 9999) then
               begin
                 FDI := FDI[Length(FDI)];
                 i := Byte(Msg.CharCode - ord('0') + 1900);
               end;
               if Length(FDI) >= 4 then
               begin
                 ST.wYear := i;
                 ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
                 Modified := true;
                 ch := true;
               end;
             end;
           end;
        1: begin
             if (i < 0) or (i > 99) then
             begin
               FDI := FDI[Length(FDI)];
               i := Byte(Msg.CharCode - ord('0'));
             end;
             if i > 50 then
                ST.wYear := 1900 + i
             else
                ST.wYear := 2000 + i;
             ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
             Modified := true;
             ch := true;
           end;
        2: begin
             if (i < 1) or (i > 12) then
             begin
               if Byte(Msg.CharCode - Ord('0')) = 0 then
               begin
                 i := st.wMonth;
                 FDI := '';
               end
               else
               begin
                 i := Byte(Msg.CharCode - ord('0'));
                 FDI := FDI[Length(FDI)];
               end;
             end;
             ST.wMonth := i;
             ST.wDay := Min(DaysPerMonth(ST.wYear, ST.wMonth), ST.wDay);
             Modified := true;
             ch := true;
           end;
        3: begin
             if (i < 1) or (i > DaysPerMonth(ST.wYear, ST.wMonth)) then
             begin
               if Byte(Msg.CharCode - Ord('0')) = 0 then
               begin
                 i := st.wDay;
                 FDI := '';
               end
               else
               begin
                 i := Byte(Msg.CharCode - ord('0'));
                 FDI := FDI[Length(FDI)];
               end;
             end;
             ST.wDay := i;
             Modified := true;
             ch := true;
           end;
        4,
        5: begin
             if Use12Hours then
             begin
               if (i < 0) or (i > 11) then
               begin
                 FDI := FDI[Length(FDI)];
                 i := Byte(Msg.CharCode - ord('0'));
               end;
             end else
             begin
               if (i < 0) or (i > 23) then
               begin
                 FDI := FDI[Length(FDI)];
                 i := Byte(Msg.CharCode - ord('0'));
               end;
             end;
             ST.wHour := i;
             Modified := true;
             ch := true;
           end;
        6: begin
             if (i < 0) or (i > 59) then
             begin
               FDI := FDI[Length(FDI)];
               i := Byte(Msg.CharCode - ord('0'));
             end;
             ST.wMinute := i;
             Modified := true;
             ch := true;
           end;
        7: begin
             if (i < 0) or (i > 59) then
             begin
               FDI := FDI[Length(FDI)];
               i := Byte(Msg.CharCode - ord('0'));
             end;
             ST.wSecond := i;
             Modified := true;
             ch := true;
           end;
      end;
    end else
    if (FCurPart < DTFParts.Count - 2) and
       ((PDTFPart(DTFParts[FCurPart + 1]).DPart = 9) or
        ((PDTFPart(DTFParts[FCurPart + 1]).DPart = -1) and
         ((Part.DPart in [0..3]) and
          (PDTFPart(DTFParts[FCurPart + 1]).Text = DateSeparator)) or
         ((Part.DPart in [4..7]) and
          (PDTFPart(DTFParts[FCurPart + 1]).Text = TimeSeparator)))) then
    begin
      if Char(Msg.CharCode) = PDTFPart(DTFParts[FCurPart + 1]).Text then
      begin
        b := false;
        for i := FCurPart + 1 to DTFParts.Count - 1 do
        begin
          Part := PDTFPart(DTFParts[i]);
          if (Part.DPart in [0, 1, 2, 3, 4, 6, 7, 8]) then
          begin
            FCurPart := i;
            FDI := '';
            UpdateText;
            InvalidateEdit;
            b := true;
            break;
          end;
        end;
        if not b and (ShowCheckBox) then
        begin
          FCurPart := -1;
          InvalidateEdit;
        end;
      end;
    end
    else
    if Part.DPart = 8 then
    begin
      if ((Length(TimeAMString) > 0) and
           ((Char(Msg.CharCode) = UpCase(TimeAMString[1])) or (Char(Msg.CharCode - 32) = UpCase(TimeAMString[1])))) then
      begin
        if ST.wHour >= 12 then
        begin
          Dec(ST.wHour, 12);
          Modified := true;
          ch := true;
        end;
      end else
      if ((Length(TimePMString) > 0) and
           ((Char(Msg.CharCode) = UpCase(TimePMString[1])) or (Char(Msg.CharCode - 32) = UpCase(TimePMString[1])))) then
      begin
        if ST.wHour < 12 then
        begin
          Inc(ST.wHour, 12);
          Modified := true;
          ch := true;
        end;
      end else
      if ((LowerCase(Part.Text) = 'am/pm') and
           (Char(Msg.CharCode) in ['a', 'p', 'a', 'p'])) then
      begin
        if (UpCase(Char(Msg.CharCode)) = 'A') and (ST.wHour >= 12) then
        begin
          Dec(ST.wHour, 12);
          Modified := true;
          ch := true;
        end else
        if (UpCase(Char(Msg.CharCode)) = 'P') and (St.wHour < 12) then
        begin
          Inc(ST.wHour, 12);
          Modified := true;
          ch := true;
        end;
      end;
    end;
    DoSetDateTime(SystemTimeToDateTime(ST));
    UpdateText;
    if ch then
      TriggerChangeEvent;
    InvalidateEdit;
  end;
end;  { WMChar }
*)

{$ifndef CLX_USED}
procedure TElDateTimePicker.CreateWindowHandle(const Params: TCreateParams);  { protected }
{$else}
procedure TElDateTimePicker.CreateWidget;
{$endif}
var Fmt : string;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    Fmt := FFormatStr;
    FFormatStr := '~~~';
    SetFormatStr(Fmt);
  end;
  FButton.Width := FBtnWidth;
  SetEditRect;
end;  { CreateWindowHandle }

{$ifndef CLX_USED}
procedure TElDateTimePicker.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and
     (Msg.Sender <> FCalButton)
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
     and (Msg.Sender <> FForm)
{$endif}
     then
     CloseUp(False);
end;
{$endif}

procedure TElDateTimePicker.CalendarChange(Sender: TObject);
begin
  CloseUp(true);
end;

procedure TElDateTimePicker.CalendarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then
      CloseUp(false);
end;

procedure TElDateTimePicker.SetShowPopupCalendar(newValue : Boolean);
{ Sets data member FShowPopupCalendar to newValue. }
begin
  if (FShowPopupCalendar <> newValue) then
  begin
    FShowPopupCalendar := newValue;
    if HandleAllocated then
      UpdateText;
    InvalidateEdit;
  end;  { if }
end;  { SetShowPopupCalendar }

destructor TElDateTimePicker.Destroy;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
  FForm.Free;
  FForm := nil;
{$endif}
  DTFParts.Free;
  FButton.Free;
  FButton := nil;
  FCalButton.Free;
  FCalButton := nil;

  {$ifndef CLX_USED}
  ImageForm := nil;

  FImgFormChLink.Free;
  FImgFormChLink := nil;
  {$endif}
  FTmpBmp.Free;
  FBackground.Free;
  inherited Destroy;
end;  { Destroy }

{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
procedure TElDateTimePicker.CalendarDeactivate(Sender : TObject);
begin
  CloseUp(False);
end;
{$endif}

procedure TElDateTimePicker.TuneupCalendarControls;
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
var i : integer;
{$endif}
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    if FForm.Components[i] is TWinControl then
    begin
      TWinControl(FForm.Components[i]).TabStop := false;
    end;
  end;
{$endif}
end;

procedure TElDateTimePicker.DblClick;  { public }
{$ifndef VCL_4_USED}
var P : TPoint;
{$endif}
begin
  inherited;
  if ((not ShowCheckBox) or Checked) then
  begin
    {$ifndef VCL_4_USED}
    GetCursorPos(P);
    if ShowCheckBox and (ScreenToClient(P).X < GetCheckWidth) then
    {$else}
    if ShowCheckBox and (ScreenToClient(Mouse.CursorPos).X < GetCheckWidth) then
    {$endif}
    begin
      InDblClick := true;
      exit;
    end;

    InDblClick := true;
    if ShowPopupCalendar then DropDown;
  end;
end;  { DblClick }

{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
procedure TElDateTimePicker.SetDroppedDown(newValue : boolean);
begin
  if FForm.Visible <> newValue then
     DropDown;
end;

function TElDateTimePicker.GetDroppedDown;
begin
  result := FForm.Visible;
end;
{$endif}

procedure TElDateTimePicker.SetShowCheckBox(newValue : Boolean);
begin
  if (FShowCheckBox <> newValue) then
  begin
    FShowCheckBox := newValue;

    if FShowCheckBox then
    begin
      FButton.Enabled := FChecked;
      FCalButton.Enabled := FChecked;
    end else
    begin
      FButton.Enabled := true;
      FCalButton.Enabled := true;
    end;
    // FButton.Flat := Flat and ((not (Focused or FMouseOver))  or ((not Checked) and ShowCheckBox and Flat));
    FCalButton.OldStyled := FButton.OldStyled;
    
    UpdateText;
    Invalidate;
  end;  { if }
end;  { SetShowCheckBox }

procedure TElDateTimePicker.SetModified(newValue : Boolean);
begin
  if FModified <> newValue then
  begin
    FModified := newValue;
    if FModified then
      TriggerChangeEvent;
  end;
end;

procedure TElDateTimePicker.SetChecked(newValue : Boolean);
begin
  if (FChecked <> newValue) then
  begin
    FChecked := newValue;
    if ShowCheckBox then
    begin
      FButton.Enabled := FChecked;
      FCalButton.Enabled := FChecked;
      // FButton.Flat := Flat and ((not (Focused or FMouseOver))  or ((not Checked) and ShowCheckBox and Flat));
      FCalButton.OldStyled := FButton.OldStyled;
    end;
    Invalidate;
  end;  { if }
end;  { SetChecked }
{$ifndef CLX_USED}
procedure TElDateTimePicker.CMEnabledChanged(var Msg : TMessage);  { private }
{$else}
procedure TElDateTimePicker.EnabledChanged;
{$endif}
begin
  inherited;
  FButton.Enabled := not ((not Enabled) or (ShowCheckBox and (not Checked)));
  FCalButton.Enabled := FButton.Enabled;
  if ChangeDisabledText then
    Invalidate; 
end;  { CMEnabledChanged }

constructor TElDateTimePicker.Create(AOwner : TComponent);
var ABmp : TBitmap;
    {$ifdef CLX_USED}
    PX   : TSize; 
    {$endif}
begin
  inherited Create(AOwner);
  {$ifndef CLX_USED}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}
  {$ifdef CLX_USED}
  InputKeys := [ikNav, ikChars];
  {$endif}

  FUnassignedChar := '0';
  FButtonVisible := true;
  {$ifndef CLX_USED}
  FBtnWidth := GetSystemMetrics(SM_CXVSCROLL);
  {$else}
  QStyle_scrollBarExtent(Application.Style.Handle, @PX);
  FBtnWidth := PX.cx;
  {$endif}
  //FButtonWidth := FBtnWidth;

  {$ifndef CLX_USED}
  FImgFormChLink  := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  
  FButton := TElSpinButton.Create(Self);
  FButton.Increment := 1;
  FButton.OldStyled := true;
  FButton.OnUpClick := SpinUpClick;
  FButton.OnDownClick := SpinDownClick;
  FButton.UseDrag := false;

  FCalButton := TElDTPickButton.Create(Self);
  FCalButton.Visible := false;
  FCalButton.ParentColor := false;
  FCalButton.Color   := clBtnFace;
  FCalButton.OnClick := CalBtnClick;
  FCalButton.Spacing := 0;
  FCalButton.Layout  := blGlyphTop;
  FCalButton.OldStyled := true;
  FCalButton.IsSwitch := true;
  FCalButton.AdjustSpaceForGlyph := false;

  ABMP := TBitmap.Create;
  ABmp.Width := 14;
  Abmp.Height := 4;
  ABmp.Canvas.Brush.Color := clFuchsia;
  ABmp.Canvas.FillRect(Rect(0, 0, 14, 4));
  ElVCLUtils.DrawArrow(ABmp.Canvas, eadDown, Rect(0, 0, 6, 3), clBtnText, true);
  ElVCLUtils.DrawArrow(ABmp.Canvas, eadDown, Rect(7, 0, 13, 3), clBtnText, false);
  ABmp.Transparent := true;
  FCalButton.Glyph := ABmp;
  FCalButton.NumGlyphs := 2;
  ABmp.Free;

  DTFParts := TElList.Create;
  DTFParts.OnDelete := OnDTFPartDelete;
  FAutoSize := true;
  
  FBackground := TBitmap.Create;
  FBackground.OnChange := ImageChange;
  FBackgroundType := bgtColorFill;
  FGradientSteps := 16;
  FBackgroundType := bgtColorFill;
  FTmpBmp := TBitmap.Create;
  FDate := Now;
  Width := 120;
  Height := 21;
  ParentColor := false;
  Color := clWindow;

{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
  FForm := THackCalendarForm.Create(nil);
  FForm.Visible := false;

  {
  with TElFormPersist.Create(FForm) do
    TopMost := true;
  }
  FForm.FormStyle := fsStayOnTop;
  FForm.Position := poDesigned;
  FForm.BorderStyle := bsNone;
  FForm.Height := FForm.Height - FForm.Panel2.Height;
  FForm.Panel2.Visible := false;
  FForm.Calendar.TabStop := false;
  FForm.Calendar.ShowHint := false;
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.UserNavigation := false;
  FForm.YearSpin.MinValue := 1900;
  {$ifdef VCL_4_USED}
  FForm.AutoSize := true;
  {$endif}
{$endif}
  FForm.OnKeyDown := CalendarKeyDown;
  FForm.OnChange := CalendarChange;
  FForm.OnDeactivate := CalendarDeactivate;
{$endif}
  FBorderStyle := bsSingle;
  FNavigationInPopup := true;

  TuneupCalendarControls;

  FInactiveBorderType := fbtSunkenOuter;
  FActiveBorderType := fbtSunken;

  FMinDate := 2;
  FMaxDate := 999999;

  FChecked := True;

  TabStop := true;

  FUnassignedColor := clRed;

  // this must be the last
  FButton.Parent := Self;
  FCalButton.Parent  := Self;
end;  { Create }

procedure TElDateTimePicker.SetCalendarLineColorLight(Value: TColor);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.LineColorLight := Value;
{$endif}
{$endif}
end;

procedure TElDateTimePicker.SetCalendarLineColorDark(Value: TColor);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.LineColorDark := Value;
{$endif}
{$endif}
end;

function TElDateTimePicker.GetCalendarLineColorDark: TColor;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  result := FForm.Calendar.LineColorDark;
{$else}
  result := clNone;
{$endif}
{$else}
  result := clNone;
{$endif}
end;

function TElDateTimePicker.GetCalendarLineColorLight: TColor;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  result := FForm.Calendar.LineColorLight;
{$else}
  result := clNone;
{$endif}
{$else}
  result := clNone;
{$endif}
end;

procedure TElDateTimePicker.SetCalendarBackColor(Value: TColor);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.Color := Value;
{$endif}
{$endif}
end;

function TElDateTimePicker.GetCalendarBackColor: TColor;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  result := FForm.Calendar.Color;
{$else}
  result := clNone;
{$endif}
{$else}
  result := clNone;
{$endif}
end;

function TElDateTimePicker.GetStartOfWeek: TDayOfWeek;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  Result := FForm.Calendar.StartOfWeek;
{$else}
  Result := 0;
{$endif}
{$else}
 result := 0;
{$endif}
end;

procedure TElDateTimePicker.SetStartOfWeek(Value: TDayOfWeek);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.StartOfWeek := Value;
{$ENDIF}
{$endif}
end;

function TElDateTimePicker.GetWeekEndDays: TElWeekEndDays;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  Result := FForm.Calendar.WeekEndDays;
{$else}
  Result := [];
{$ENDIF}
{$else}
  Result := [];
{$ENDIF}
end;

procedure TElDateTimePicker.SetWeekEndDays(Value: TElWeekEndDays);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.WeekEndDays := Value;
{$ENDIF}
{$endif}
end;

function TElDateTimePicker.GetUseSystemStartOfWeek: Boolean;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  Result := FForm.Calendar.UseSystemStartOfWeek;
{$else}
  Result := false;
{$endif}
{$else}
  Result := false;
{$endif}
end;

procedure TElDateTimePicker.SetUseSystemStartOfWeek(Value: Boolean);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.UseSystemStartOfWeek := Value;
{$endif}
{$endif}
end;

procedure TElDateTimePicker.TriggerCheckboxChangeEvent;
begin
  if assigned(FOnCheckboxChange) then
    FOnCheckboxChange(Self);
end;

function TElDateTimePicker.GetCalendarUseLineColors: Boolean;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  Result := FForm.Calendar.UseLineColors;
{$else}
  Result := false;
{$endif}
{$else}
  Result := false;
{$endif}
end;

procedure TElDateTimePicker.SetCalendarUseLineColors(Value: Boolean);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.UseLineColors := Value;
{$endif}
{$endif}
end;

function TElDateTimePicker.GetCalendarWeekendColor: TColor;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  result := FForm.Calendar.WeekEndColor;
{$else}
  Result := clNone;
{$endif}
{$else}
  Result := clNone;
{$endif}
end;

procedure TElDateTimePicker.SetCalendarWeekendColor(Value: TColor);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.WeekEndColor := Value;
{$endif}
{$endif}
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;
{$endif}


{$ifndef CLX_USED}
procedure TElDateTimePicker.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) and (not (ThemesAvailable and UseXPThemes)) then
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

function TElDateTimePicker.StoreStartOfWeek: Boolean;
begin
  Result := not UseSystemStartOfWeek;
end;

function TElDateTimePicker.GetCalendarCurrentDayBorder: TElFlatBorderType;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  result := FForm.Calendar.CurrentDayBorder;
{$else}
  Result := fbtSunken;
{$endif}
{$else}
  Result := fbtSunken;
{$endif}
end;

procedure TElDateTimePicker.SetCalendarCurrentDayBorder(Value:
    TElFlatBorderType);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.CurrentDayBorder := Value;
{$endif}
{$endif}
end;

function TElDateTimePicker.GetCalendarDayCellBorder: TElFlatBorderType;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  result := FForm.Calendar.DayCellBorder;
{$else}
  Result := fbtSunken;
{$endif}
{$else}
  Result := fbtSunken;
{$endif}
end;

procedure TElDateTimePicker.SetCalendarDayCellBorder(Value: TElFlatBorderType);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.DayCellBorder := Value;
{$endif}
{$endif}
end;

function TElDateTimePicker.GetCalendarSelectionBorder: TElFlatBorderType;
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  result := FForm.Calendar.SelectionBorder;
{$else}
  Result := fbtSunken;
{$endif}
{$else}
  Result := fbtSunken;
{$endif}
end;

procedure TElDateTimePicker.SetCalendarSelectionBorder(Value:
    TElFlatBorderType);
begin
{$ifdef DATETIMEPICKER_SUPPORT_CALENDAR}
{$ifndef CALENDAR_USE_WINDOWS_CALENDAR}
  FForm.Calendar.SelectionBorder := Value;
{$endif}
{$endif}
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;

procedure TElDateTimePicker.WMWindowPosChanged(var Message:
    TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;

procedure TElDateTimePicker.DrawFlatFrame(Canvas : TCanvas; R : TRect);
var AColor, Color : TColor;
begin
  if ((Focused or FMouseOver) and (ActiveBorderType = fbtColorLineBorder)) or
     ((not (Focused or FMouseOver)) and (InactiveBorderType = fbtColorLineBorder)) then
  begin
    if Enabled then
      Color := clWindow
    else
      Color := clBtnFace;
    if Focused or FMouseOver then
      AColor := LineBorderActiveColor
    else
      AColor := LineBorderInactiveColor;
    ELVCLUtils.DrawFlatFrameEx2(Canvas.Handle, R, AColor, Color, false, true, AllBorderSides, fbtColorLineBorder);
  end
  else
    ElVCLUtils.DrawFlatFrame(Canvas.Handle, R, clWindow, false);
end;

{$endif}

procedure TElDateTimePicker.SetMinDate(Value: TDateTime);
begin
  if (FMinDate <> Value) then
  begin
    if Value < 2 then
      Value := 2;
    FMinDate := Value;
    if (not (csLoading in ComponentState)) and (FDate < FMinDate) then
      Date := FMinDate;
  end;
end;

procedure TElDateTimePicker.SetMaxDate(Value: TDateTime);
begin
  if FMaxDate <> Value then
  begin
    FMaxDate := Value;
    if (not (csLoading in ComponentState)) and (FDate > FMaxDate) then
      Date := FMaxDate;
  end;
end;

procedure TElDateTimePicker.DoSetDateTime(ADate : TDateTime);
begin
  if (FDate <> ADate) then
  begin
    if (ADate < MaxDate) and (ADate > MinDate) then
    begin
      if ADate < 2 then
         ADate := 2;
      FDate := ADate;
    end;
  end;
end;

function TElDateTimePicker.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

procedure TElDateTimePicker.SetUseXPThemes(const Value: Boolean);
begin
  inherited;
  FButton.UseXPThemes := UseXPThemes;
  FCalButton.UseXPThemes := UseXPThemes;
end;

function TElDateTimePicker.GetButtonDir: TElSpinBtnDir;
begin
  Result := FButton.ButtonDirection;
end;

procedure TElDateTimePicker.SetButtonDir(Value: TElSpinBtnDir);
begin
  FButton.ButtonDirection := Value;
end;

function TElDateTimePicker.GetButtonType: TElSpinBtnType;
begin
  Result := FButton.ButtonType;
end;

procedure TElDateTimePicker.SetButtonType(Value: TElSpinBtnType);
begin
  FButton.ButtonType := Value;
end;

procedure TElDateTimePicker.SetButtonWidth(Value: Integer);
begin
  if FBtnWidth <> Value then
  begin
    FBtnWidth := Value;
    if not (csLoading in ComponentState) then
    begin
      FButton.Width := Value;
      SetEditRect;
    end;
  end;
end;

procedure TElDateTimePicker.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;

    UpdateButtonStyles;

    if Flat and (Focused or FMouseOver) then
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TElDateTimePicker.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;

    UpdateButtonStyles;

    if Flat and not (Focused or FMouseOver) then
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TElDateTimePicker.UpdateButtonStyles;
begin
  FButton.MoneyFlat := Flat and (InactiveBorderType = fbtColorLineBorder) and (ActiveBorderType = fbtColorLineBorder);
  FButton.MoneyFlatActiveColor := LineBorderActiveColor;
  FButton.MoneyFlatInactiveColor := LineBorderInactiveColor;
  FButton.MoneyFlatDownColor := LineBorderActiveColor;

  FCalButton.MoneyFlat := FButton.MoneyFlat;
  FCalButton.MoneyFlatActiveColor := LineBorderActiveColor;
  FCalButton.MoneyFlatInactiveColor := LineBorderInactiveColor;
  FCalButton.MoneyFlatDownColor := LineBorderActiveColor;
end;

procedure TElDateTimePicker.SetButtonVisible(Value: Boolean);
begin
  if FButtonVisible <> Value then
  begin
    FButtonVisible := Value;
    SetEditRect;
    Invalidate;
  end;
end;

procedure TElDateTimePicker.SetUnassigned(Value: Boolean);
begin
  if FUnassigned <> Value then
  begin
    FUnassigned := Value;
    UpdateText;
    InvalidateEdit;
  end;
end;

procedure TElDateTimePicker.SetUnassignedColor(Value: TColor);
begin
  if FUnassignedColor <> Value then
  begin
    FUnassignedColor := Value;
    if Unassigned and Focused then
      InvalidateEdit; 
  end;
end;

procedure TElDateTimePicker.SetUnassignedAllowed(Value: Boolean);
begin
  if FUnassignedAllowed <> Value then
  begin
    FUnassignedAllowed := Value;
    if Unassigned and (not FUnassignedAllowed) then
      Unassigned := false;
  end;
end;

function TElDateTimePicker.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TElDateTimePicker.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
  end;
end;

procedure TElDateTimePicker.SetButtonShowOnFocus(Value: Boolean);
begin
  if FButtonShowOnFocus <> Value then
  begin
    FButtonShowOnFocus := Value;
    if ButtonVisible then
    begin
      if not Focused then
      begin
        SetEditRect;
        Invalidate;
      end;
    end;
  end;
end;

function TElDateTimePicker.IsDateTimeStored: Boolean;
begin
  Result := not UseCurrentDate;
end;

procedure TElDateTimePicker.SetButtonThinFrame(Value: Boolean);
begin
  if FButtonThinFrame <> Value then
  begin
    FButtonThinFrame := Value;
    FButton.OldStyled := not Value;
    // FButton.OldStyled := not (ButtonThinFrame or not (MouseOver or Focused));
  end;
end;

procedure TElDateTimePicker.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    {$ifndef CLX_USED}
    UpdateHeight;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElDateTimePicker.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);

  if Ctl3D then
    I := GetSystemMetrics(SM_CYEDGE) * 4
  else
    I := GetSystemMetrics(SM_CYBORDER) * 6;

  Height := Metrics.tmHeight + I;
end;

procedure TElDateTimePicker.UpdateHeight;
begin
  if FAutoSize and (BorderStyle = bsSingle) then
  begin
    ControlStyle := ControlStyle + [csFixedHeight];
    AdjustHeight;
  end
  else
    ControlStyle := ControlStyle - [csFixedHeight];
end;

procedure TElDateTimePicker.CreateWnd;
begin
  inherited;
  UpdateHeight;
end;
{$endif}

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElDateTimePicker.CMHintShow(var Message: TMessage);
{$else}
function TElDateTimePicker.HintShow(var HintInfo : THintInfo): Boolean; 
{$endif}
var T: WideChar;
{$ifndef CLX_USED}
  HintInfo : PHintInfo;
{$endif}
  l : integer;
  S : String;
  WS: WideString;
begin
{$ifndef CLX_USED}
  inherited;
  HintInfo := PHintInfo(Message.lParam);
{$else}
  inherited HintShow(HintInfo);
  result := true;
{$endif}
  if Length(FHint) = 0 then
  begin
    HintInfo.hintStr := '';
    exit;
  end;

  S := GetShortHint(inherited Hint);
  if HintInfo.HintStr = S then
  begin
    WS := GetShortHintW(FHint);
  end
  else
  begin
    S := FHint;
    WS := FHint;
  end;

  l := Length(S) + 1 + Length(WS) * 2;
  SetLength(HintInfo.HintStr, l + 4);
  Move(PChar(S)^, HintInfo.HintStr[1], Length(S) + 1);

  Move(WS[1], HintInfo.HintStr[Length(S) + 2], Length(WS) * 2);
  T := #0;
  Move(T, HintInfo.HintStr[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, HintInfo.HintStr[l + 3], sizeof(T));
end;

procedure TElDateTimePicker.SetHint(Value: WideString);
var S : String;
    i,
    l : integer;
    T : WideChar;
begin
  FHint := Value;

  S := FHint;
  i := Length(S);
  l := Length(S) + 1 + Length(FHint) * 2;
  SetLength(S, l + 4);

  Move(FHint[1], S[i + 2], Length(FHint) * 2);
  T := #0;
  Move(T, S[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, S[l + 3], sizeof(T));
  inherited Hint := S;
end;

{$endif}

function TElDateTimePicker.GetUnassignedChar: TElFString;
begin
  Result := FUnassignedChar;
end;

procedure TElDateTimePicker.SetUnassignedChar(Value: TElFString);
begin
  if Length(Value) = 0 then
    FUnassignedChar := '0'
  else
    FUnassignedChar := Value[1];
  if Unassigned then
  begin
    UpdateText;
    InvalidateEdit;
  end;
end;

function TElDateTimePicker.MakeUnassignedString(Length : integer): TElFString;
begin
  {$ifdef ELPACK_UNICODE}
  Result := WideMakeString(Length, FUnassignedChar);
  {$else}
  Result := MakeString(Length, FUnassignedChar);
  {$endif}
end;

procedure TElDateTimePicker.SetChangeDisabledText(Value: Boolean);
begin
  if FChangeDisabledText <> Value then
  begin
    FChangeDisabledText := Value;
    if not Enabled then
      Invalidate;
  end;
end;


function TElDTPickButton.GetThemedClassName: WideString;
begin
  Result := 'COMBOBOX';
end;

function TElDTPickButton.GetThemePartID: Integer;
begin
  result := CP_DROPDOWNBUTTON;
  ShowGlyph := not IsThemeApplied;
end;

function TElDTPickButton.GetThemeStateID: Integer;
begin
  if not Enabled then
    result := CBXS_DISABLED
  else
  if FState in [ebsDown, ebsExclusive] then
    result := CBXS_PRESSED
  else
  if MouseInControl then
    result := CBXS_HOT
  else
    result := CBXS_NORMAL;
end;

end.


