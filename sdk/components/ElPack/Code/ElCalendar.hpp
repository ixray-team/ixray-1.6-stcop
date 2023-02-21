// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCalendar.pas' rev: 35.00 (Windows)

#ifndef ElcalendarHPP
#define ElcalendarHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Grids.hpp>
#include <System.SysUtils.hpp>
#include <ElTools.hpp>
#include <ElList.hpp>
#include <ElVCLUtils.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElCalendarDefs.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcalendar
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElCalendar;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElCalendar : public Vcl::Grids::TCustomGrid
{
	typedef Vcl::Grids::TCustomGrid inherited;
	
private:
	System::Uitypes::TColor FHolidayColor;
	bool FShowPeriods;
	System::TDateTime FPeriodStart;
	System::Word FPeriodLength;
	System::Word FPeriodInterval;
	System::Uitypes::TColor FPeriodColor;
	bool FShowHolidays;
	Elcalendardefs::TElHolidays* FHolidays;
	System::Uitypes::TColor FWeekEndColor;
	Elcalendardefs::TElWeekEndDays FWeekEndDays;
	bool FShowWeekNum;
	System::TDateTime FDate;
	int FMonthOffset;
	System::Classes::TNotifyEvent FOnChange;
	bool FReadOnly;
	Elcalendardefs::TDayOfWeek FStartOfWeek;
	bool FUpdating;
	bool FUserNavigation;
	bool FUseCurrentDate;
	bool FTranslateDays;
	bool FMouseOver;
	bool FFlat;
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FUseLineColors;
	Elvclutils::TElBorderSides FBorderSides;
	Elvclutils::TElFlatBorderType FSelectionBorder;
	Elvclutils::TElFlatBorderType FDayCellBorder;
	Elvclutils::TElFlatBorderType FCurrentDayBorder;
	void __fastcall SetActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetInactiveBorderType(Elvclutils::TElFlatBorderType newValue);
	System::UnicodeString __fastcall GetCellText(int ACol, int ARow);
	int __fastcall GetDateElement(int Index);
	void __fastcall SetCalendarDate(System::TDateTime Value);
	void __fastcall SetDateElement(int Index, int Value);
	void __fastcall SetStartOfWeek(Elcalendardefs::TDayOfWeek Value);
	void __fastcall SetUseCurrentDate(bool Value);
	bool __fastcall StoreCalendarDate();
	void __fastcall SetShowWeekNum(bool newValue);
	void __fastcall SetWeekEndDays(Elcalendardefs::TElWeekEndDays newValue);
	void __fastcall SetWeekEndColor(System::Uitypes::TColor newValue);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Msg);
	void __fastcall SetHolidays(Elcalendardefs::TElHolidays* newValue);
	void __fastcall FixHolidayDate(Elcalendardefs::TElHoliday* AHoliday, System::TDateTime &Date);
	void __fastcall SetShowHolidays(bool newValue);
	void __fastcall SetShowPeriods(bool newValue);
	void __fastcall SetPeriodStart(System::TDateTime newValue);
	void __fastcall SetPeriodLength(System::Word newValue);
	void __fastcall SetPeriodInterval(System::Word newValue);
	void __fastcall SetPeriodColor(System::Uitypes::TColor newValue);
	void __fastcall SetHolidayColor(System::Uitypes::TColor newValue);
	void __fastcall SetDate(System::TDateTime newValue);
	void __fastcall SetFlat(bool newValue);
	void __fastcall SetTranslateDays(bool value);
	void __fastcall DrawFlatBorder();
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	bool __fastcall StoreDate();
	void __fastcall SetLineColorLight(System::Uitypes::TColor Value);
	void __fastcall SetLineColorDark(System::Uitypes::TColor Value);
	void __fastcall SetUseLineColors(bool Value);
	void __fastcall SetSelectionBorder(Elvclutils::TElFlatBorderType Value);
	void __fastcall SetDayCellBorder(Elvclutils::TElFlatBorderType Value);
	void __fastcall SetCurrentDayBorder(Elvclutils::TElFlatBorderType Value);
	
protected:
	System::Uitypes::TColor FLineColorLight;
	System::Uitypes::TColor FLineColorDark;
	bool FUseSystemStartOfWeek;
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	DYNAMIC void __fastcall Change();
	void __fastcall ChangeMonth(int Delta);
	DYNAMIC void __fastcall Click();
	virtual int __fastcall DaysThisMonth();
	virtual void __fastcall DrawCell(int ACol, int ARow, const System::Types::TRect &ARect, Vcl::Grids::TGridDrawState AState);
	virtual bool __fastcall IsLeapYear(int AYear);
	virtual bool __fastcall SelectCell(int ACol, int ARow);
	virtual void __fastcall Loaded();
	void __fastcall UpdateFrame();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall SetUseSystemStartOfWeek(bool Value);
	
public:
	__fastcall virtual TElCalendar(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElCalendar();
	void __fastcall NextMonth();
	void __fastcall NextYear();
	void __fastcall PrevMonth();
	void __fastcall PrevYear();
	virtual void __fastcall UpdateCalendar();
	void __fastcall MouseToCell(int X, int Y, int &ACol, int &ARow);
	__property System::TDateTime CalendarDate = {read=FDate, write=SetCalendarDate, stored=StoreCalendarDate};
	__property System::UnicodeString CellText[int ACol][int ARow] = {read=GetCellText};
	bool __fastcall IsHoliday(int AYear, int AMonth, int ADay);
	bool __fastcall IsInPeriod(System::Word AYear, System::Word AMonth, System::Word ADay);
	bool __fastcall IsRestHoliday(System::Word AYear, System::Word AMonth, System::Word ADay);
	
__published:
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property System::TDateTime Date = {read=FDate, write=SetDate, stored=StoreDate};
	__property int Day = {read=GetDateElement, write=SetDateElement, stored=false, index=3, nodefault};
	__property Elcalendardefs::TElHolidays* Holidays = {read=FHolidays, write=SetHolidays};
	__property int Month = {read=GetDateElement, write=SetDateElement, stored=false, index=2, nodefault};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, default=0};
	__property Elcalendardefs::TDayOfWeek StartOfWeek = {read=FStartOfWeek, write=SetStartOfWeek, nodefault};
	__property bool TranslateDays = {read=FTranslateDays, write=SetTranslateDays, default=1};
	__property bool UseCurrentDate = {read=FUseCurrentDate, write=SetUseCurrentDate, default=1};
	__property int Year = {read=GetDateElement, write=SetDateElement, stored=false, index=1, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property bool ShowWeekNum = {read=FShowWeekNum, write=SetShowWeekNum, default=0};
	__property Elcalendardefs::TElWeekEndDays WeekEndDays = {read=FWeekEndDays, write=SetWeekEndDays, nodefault};
	__property System::Uitypes::TColor WeekEndColor = {read=FWeekEndColor, write=SetWeekEndColor, default=255};
	__property bool ShowHolidays = {read=FShowHolidays, write=SetShowHolidays, default=1};
	__property bool ShowPeriods = {read=FShowPeriods, write=SetShowPeriods, nodefault};
	__property System::TDateTime PeriodStart = {read=FPeriodStart, write=SetPeriodStart};
	__property System::Word PeriodLength = {read=FPeriodLength, write=SetPeriodLength, default=1};
	__property System::Word PeriodInterval = {read=FPeriodInterval, write=SetPeriodInterval, default=28};
	__property System::Uitypes::TColor PeriodColor = {read=FPeriodColor, write=SetPeriodColor, default=16776960};
	__property System::Uitypes::TColor HolidayColor = {read=FHolidayColor, write=SetHolidayColor, nodefault};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool UserNavigation = {read=FUserNavigation, write=FUserNavigation, nodefault};
	__property System::Uitypes::TColor LineColorLight = {read=FLineColorLight, write=SetLineColorLight, stored=FUseLineColors, default=-16777211};
	__property System::Uitypes::TColor LineColorDark = {read=FLineColorDark, write=SetLineColorDark, stored=FUseLineColors, default=-16777201};
	__property bool UseSystemStartOfWeek = {read=FUseSystemStartOfWeek, write=SetUseSystemStartOfWeek, default=0};
	__property bool UseLineColors = {read=FUseLineColors, write=SetUseLineColors, default=1};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property Elvclutils::TElFlatBorderType SelectionBorder = {read=FSelectionBorder, write=SetSelectionBorder, default=1};
	__property Elvclutils::TElFlatBorderType DayCellBorder = {read=FDayCellBorder, write=SetDayCellBorder, default=11};
	__property Elvclutils::TElFlatBorderType CurrentDayBorder = {read=FCurrentDayBorder, write=SetCurrentDayBorder, default=1};
	__property Align = {default=0};
	__property BorderStyle = {default=1};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property Enabled = {default=1};
	__property Font;
	__property GridLineWidth = {default=1};
	__property ParentColor = {default=0};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnStartDrag;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElCalendar(HWND ParentWindow) : Vcl::Grids::TCustomGrid(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elcalendar */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCALENDAR)
using namespace Elcalendar;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcalendarHPP
