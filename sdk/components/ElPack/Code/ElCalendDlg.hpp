// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCalendDlg.pas' rev: 35.00 (Windows)

#ifndef ElcalenddlgHPP
#define ElcalenddlgHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Dialogs.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElCalendarDefs.hpp>
#include <ElCalendar.hpp>
#include <ElACtrls.hpp>
#include <ElSpin.hpp>
#include <ElCombos.hpp>
#include <ElBtnCtl.hpp>
#include <ElPopBtn.hpp>
#include <ElPanel.hpp>
#include <ElList.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElVCLUtils.hpp>
#include <ElXPThemedControl.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcalenddlg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElCalendarForm;
class DELPHICLASS TElCalendarDialog;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElCalendarForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* Panel2;
	Elpopbtn::TElPopupButton* TodayBtn;
	Elpopbtn::TElPopupButton* OkBtn;
	Elpopbtn::TElPopupButton* CancelBtn;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall TodayBtnClick(System::TObject* Sender);
	void __fastcall OkBtnClick(System::TObject* Sender);
	void __fastcall FormClose(System::TObject* Sender, System::Uitypes::TCloseAction &Action);
	void __fastcall CancelBtnClick(System::TObject* Sender);
	void __fastcall CalendarChange(System::TObject* Sender);
	void __fastcall CalendarClick(System::TObject* Sender);
	
private:
	System::Classes::TNotifyEvent FOnDeactivate;
	System::Classes::TNotifyEvent FOnChange;
	HIDESBASE MESSAGE void __fastcall WMQueryEndSession(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMActivate(Winapi::Messages::TWMActivate &Msg);
	
protected:
	virtual void __fastcall TriggerChangeEvent();
	
public:
	bool IsModal;
	Elcalendar::TElCalendar* Calendar;
	Elpanel::TElPanel* Panel1;
	Elpopbtn::TElPopupButton* PrevMonBtn;
	Elpopbtn::TElPopupButton* PrevYearBtn;
	Elpopbtn::TElPopupButton* NextMonBtn;
	Elpopbtn::TElPopupButton* NextYearBtn;
	Elspin::TElSpinEdit* YearSpin;
	Elactrls::TElAdvancedComboBox* MonthCombo;
	void __fastcall PrevYearBtnClick(System::TObject* Sender);
	void __fastcall PrevMonBtnClick(System::TObject* Sender);
	void __fastcall MonthComboChange(System::TObject* Sender);
	void __fastcall NextMonBtnClick(System::TObject* Sender);
	void __fastcall NextYearBtnClick(System::TObject* Sender);
	void __fastcall YearSpinChange(System::TObject* Sender);
	void __fastcall SetNames();
	void __fastcall UpdateLabel();
	__fastcall virtual ~TElCalendarForm();
	
__published:
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnDeactivate = {read=FOnDeactivate, write=FOnDeactivate};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TElCalendarForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TElCalendarForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElCalendarForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElCalendarDialog : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::TDateTime FDate;
	bool FShowHolidays;
	bool FShowPeriods;
	Elcalendardefs::TDayOfWeek FStartOfWeek;
	bool FUseCurrentDate;
	int FGridLineWidth;
	bool FShowWeekNum;
	Elcalendardefs::TElWeekEndDays FWeekEndDays;
	System::TDateTime FPeriodStart;
	int FPeriodLength;
	int FPeriodInterval;
	System::Uitypes::TColor FPeriodColor;
	System::Uitypes::TColor FHolidayColor;
	System::Uitypes::TColor FWeekEndColor;
	bool FUseSystemStartOfWeek;
	Elcalendardefs::TElHolidays* FHolidays;
	System::Classes::TNotifyEvent FOnChange;
	void __fastcall PrepareDialog(TElCalendarForm* FrmDialogComponent);
	
protected:
	Elvclutils::TElFlatBorderType FSelectionBorder;
	Elvclutils::TElFlatBorderType FDayCellBorder;
	Elvclutils::TElFlatBorderType FCurrentDayBorder;
	bool FUseLineColors;
	System::Uitypes::TColor FLineColorDark;
	System::Uitypes::TColor FLineColorLight;
	
public:
	__fastcall virtual TElCalendarDialog(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElCalendarDialog();
	bool __fastcall Execute();
	
__published:
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property bool ShowHolidays = {read=FShowHolidays, write=FShowHolidays, default=1};
	__property bool ShowPeriods = {read=FShowPeriods, write=FShowPeriods, nodefault};
	__property Elcalendardefs::TDayOfWeek StartOfWeek = {read=FStartOfWeek, write=FStartOfWeek, nodefault};
	__property bool UseCurrentDate = {read=FUseCurrentDate, write=FUseCurrentDate, nodefault};
	__property int GridLineWidth = {read=FGridLineWidth, write=FGridLineWidth, default=1};
	__property bool ShowWeekNum = {read=FShowWeekNum, write=FShowWeekNum, default=0};
	__property Elcalendardefs::TElWeekEndDays WeekEndDays = {read=FWeekEndDays, write=FWeekEndDays, nodefault};
	__property System::TDateTime PeriodStart = {read=FPeriodStart, write=FPeriodStart};
	__property int PeriodLength = {read=FPeriodLength, write=FPeriodLength, default=1};
	__property int PeriodInterval = {read=FPeriodInterval, write=FPeriodInterval, default=28};
	__property System::Uitypes::TColor PeriodColor = {read=FPeriodColor, write=FPeriodColor, default=16776960};
	__property System::Uitypes::TColor HolidayColor = {read=FHolidayColor, write=FHolidayColor, nodefault};
	__property System::Uitypes::TColor WeekEndColor = {read=FWeekEndColor, write=FWeekEndColor, default=-16777198};
	__property Elcalendardefs::TElHolidays* Holidays = {read=FHolidays};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property bool UseSystemStartOfWeek = {read=FUseSystemStartOfWeek, write=FUseSystemStartOfWeek, nodefault};
	__property Elvclutils::TElFlatBorderType SelectionBorder = {read=FSelectionBorder, write=FSelectionBorder, nodefault};
	__property Elvclutils::TElFlatBorderType DayCellBorder = {read=FDayCellBorder, write=FDayCellBorder, nodefault};
	__property Elvclutils::TElFlatBorderType CurrentDayBorder = {read=FCurrentDayBorder, write=FCurrentDayBorder, nodefault};
	__property bool UseLineColors = {read=FUseLineColors, write=FUseLineColors, nodefault};
	__property System::Uitypes::TColor LineColorDark = {read=FLineColorDark, write=FLineColorDark, nodefault};
	__property System::Uitypes::TColor LineColorLight = {read=FLineColorLight, write=FLineColorLight, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Ellist::TElList* FormList;
}	/* namespace Elcalenddlg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCALENDDLG)
using namespace Elcalenddlg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcalenddlgHPP
