// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxDateUtil.pas' rev: 34.00 (Windows)

#ifndef MxdateutilHPP
#define MxdateutilHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxdateutil
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDateOrder : unsigned char { doMDY, doDMY, doYMD };

enum DECLSPEC_DENUM TDayOfWeekName : unsigned char { Sun, Mon, Tue, Wed, Thu, Fri, Sat };

typedef System::Set<TDayOfWeekName, TDayOfWeekName::Sun, TDayOfWeekName::Sat> TDaysOfWeek;

//-- var, const, procedure ---------------------------------------------------
static const TDateOrder DefaultDateOrder = (TDateOrder)(1);
extern DELPHI_PACKAGE bool FourDigitYear;
extern DELPHI_PACKAGE System::Byte CenturyOffset;
extern DELPHI_PACKAGE System::TDateTime NullDate;
extern DELPHI_PACKAGE bool __fastcall IsLeapYear(int AYear);
extern DELPHI_PACKAGE int __fastcall DaysPerMonth(int AYear, int AMonth);
extern DELPHI_PACKAGE System::TDateTime __fastcall FirstDayOfNextMonth(void);
extern DELPHI_PACKAGE System::TDateTime __fastcall FirstDayOfPrevMonth(void);
extern DELPHI_PACKAGE System::TDateTime __fastcall LastDayOfPrevMonth(void);
extern DELPHI_PACKAGE System::Word __fastcall ExtractDay(System::TDateTime ADate);
extern DELPHI_PACKAGE System::Word __fastcall ExtractMonth(System::TDateTime ADate);
extern DELPHI_PACKAGE System::Word __fastcall ExtractYear(System::TDateTime ADate);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncDate(System::TDateTime ADate, int Days, int Months, int Years);
extern DELPHI_PACKAGE void __fastcall DateDiff(System::TDateTime Date1, System::TDateTime Date2, System::Word &Days, System::Word &Months, System::Word &Years);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncDay(System::TDateTime ADate, int Delta);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncMonth(System::TDateTime ADate, int Delta);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncYear(System::TDateTime ADate, int Delta);
extern DELPHI_PACKAGE double __fastcall MonthsBetween(System::TDateTime Date1, System::TDateTime Date2);
extern DELPHI_PACKAGE bool __fastcall ValidDate(System::TDateTime ADate);
extern DELPHI_PACKAGE int __fastcall DaysInPeriod(System::TDateTime Date1, System::TDateTime Date2);
extern DELPHI_PACKAGE int __fastcall DaysBetween(System::TDateTime Date1, System::TDateTime Date2);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncTime(System::TDateTime ATime, int Hours, int Minutes, int Seconds, int MSecs);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncHour(System::TDateTime ATime, int Delta);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncMinute(System::TDateTime ATime, int Delta);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncSecond(System::TDateTime ATime, int Delta);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncMSec(System::TDateTime ATime, int Delta);
extern DELPHI_PACKAGE System::TDateTime __fastcall CutTime(System::TDateTime ADate);
extern DELPHI_PACKAGE System::Word __fastcall CurrentYear(void);
extern DELPHI_PACKAGE TDateOrder __fastcall GetDateOrder(const System::UnicodeString DateFormat);
extern DELPHI_PACKAGE System::Byte __fastcall MonthFromName(const System::UnicodeString S, System::Byte MaxLen);
extern DELPHI_PACKAGE System::TDateTime __fastcall StrToDateFmt(const System::UnicodeString DateFormat, const System::UnicodeString S);
extern DELPHI_PACKAGE System::TDateTime __fastcall StrToDateDef(const System::UnicodeString S, System::TDateTime Default);
extern DELPHI_PACKAGE System::TDateTime __fastcall StrToDateFmtDef(const System::UnicodeString DateFormat, const System::UnicodeString S, System::TDateTime Default);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DefDateFormat(bool FourDigitYear);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DefDateMask(System::WideChar BlanksChar, bool FourDigitYear);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FormatLongDate(System::TDateTime Value);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FormatLongDateTime(System::TDateTime Value);
}	/* namespace Mxdateutil */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXDATEUTIL)
using namespace Mxdateutil;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxdateutilHPP
