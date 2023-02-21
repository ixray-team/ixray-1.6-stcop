// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTools.pas' rev: 35.00 (Windows)

#ifndef EltoolsHPP
#define EltoolsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ShellAPI.hpp>
#include <Winapi.ActiveX.hpp>
#include <Winapi.MMSystem.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <ElStrUtils.hpp>
#include <System.TimeSpan.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltools
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDirectMemoryStream;
class DELPHICLASS TNamedFileStream;
struct TReducedDateTime;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TDirectMemoryStream : public System::Classes::TMemoryStream
{
	typedef System::Classes::TMemoryStream inherited;
	
public:
	HIDESBASE void __fastcall SetPointer(void * Ptr, int Size);
public:
	/* TMemoryStream.Destroy */ inline __fastcall virtual ~TDirectMemoryStream() { }
	
public:
	/* TObject.Create */ inline __fastcall TDirectMemoryStream() : System::Classes::TMemoryStream() { }
	
};

#pragma pack(pop)

typedef TDirectMemoryStream TElMemoryStream;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNamedFileStream : public System::Classes::TFileStream
{
	typedef System::Classes::TFileStream inherited;
	
private:
	System::UnicodeString FFileName;
	
public:
	__fastcall TNamedFileStream(const System::UnicodeString FileName, System::Word Mode);
	__property System::UnicodeString FileName = {read=FFileName};
public:
	/* TFileStream.Destroy */ inline __fastcall virtual ~TNamedFileStream() { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TReducedDateTime
{
public:
	System::Word Year;
	System::Word Month;
	System::Word DOW;
	System::Word Day;
	System::Word Hour;
	System::Word Min;
};


typedef void __fastcall (__closure *TMsgPumpRoutineEvent)(void);

typedef void __fastcall (__closure *TElWndMethod)(Winapi::Messages::TMessage &Message);

typedef System::StaticArray<System::UnicodeString, 38> Eltools__3;

//-- var, const, procedure ---------------------------------------------------
static const System::Word MAXPATHLEN = System::Word(0x400);
extern DELPHI_PACKAGE TMsgPumpRoutineEvent OnMessagePump;
extern DELPHI_PACKAGE bool IsLinux;
extern DELPHI_PACKAGE bool IsWin95;
extern DELPHI_PACKAGE bool IsWinNT;
extern DELPHI_PACKAGE bool IsWin2000;
extern DELPHI_PACKAGE bool IsWinNTUp;
extern DELPHI_PACKAGE bool IsWin2000Up;
extern DELPHI_PACKAGE bool IsWinXP;
extern DELPHI_PACKAGE bool IsWinXPUp;
extern DELPHI_PACKAGE bool IsWin95OSR2;
extern DELPHI_PACKAGE bool IsWin98;
extern DELPHI_PACKAGE bool IsWinME;
extern DELPHI_PACKAGE bool IsWin98Up;
extern DELPHI_PACKAGE HWND LastWin;
extern DELPHI_PACKAGE unsigned LastProcessID;
extern DELPHI_PACKAGE Eltools__3 ElementFormatList;
extern DELPHI_PACKAGE void __fastcall PlaySound(System::WideChar * Name, unsigned Flags1, unsigned Flags2);
extern DELPHI_PACKAGE System::Word __fastcall swapInt16(System::Word w);
extern DELPHI_PACKAGE int __fastcall swapInt32(int i);
extern DELPHI_PACKAGE double __fastcall SwapDouble(double d);
extern DELPHI_PACKAGE int __fastcall GetSysStartDayOfWeek(void);
extern DELPHI_PACKAGE System::TDateTime __fastcall GetTime(System::TDateTime DateTime);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetCommonAppDataFolder(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetUserAppDataFolder(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetUserLocalAppDataFolder(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetSpecialFolder(const int CSIDL);
extern DELPHI_PACKAGE System::UnicodeString __fastcall IncludeTrailingBackslash2(const System::UnicodeString Path);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetTempFile(System::UnicodeString SDir);
extern DELPHI_PACKAGE bool __fastcall TimeInMask(System::UnicodeString CronMask, const TReducedDateTime &T);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetSystemDir(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetShortPath(System::UnicodeString Path);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetTempDir(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetWindowsDir(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetFormattedTimeString(System::TDateTime ADate, System::UnicodeString Format);
extern DELPHI_PACKAGE int __fastcall DayNumber(int AYear, int AMonth, int ADay);
extern DELPHI_PACKAGE int __fastcall WeekNumber(int AYear, int AMonth, int ADay);
extern DELPHI_PACKAGE System::TDateTime __fastcall ExtractTime(System::TDateTime ATime);
extern DELPHI_PACKAGE System::TDateTime __fastcall ExtractDate(System::TDateTime ATime);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncTime(System::TDateTime ATime, int Hours, int Minutes, int Seconds, int MSecs);
extern DELPHI_PACKAGE void __fastcall CenterRects(int WS, int WT, int HS, int HT, System::Types::TRect &R);
extern DELPHI_PACKAGE bool __fastcall ReadTextFromStream(System::Classes::TStream* S, System::UnicodeString &Data);
extern DELPHI_PACKAGE void __fastcall WriteTextToStream(System::Classes::TStream* S, System::UnicodeString Data);
extern DELPHI_PACKAGE System::UnicodeString __fastcall encode_line(const void *buf, int size);
extern DELPHI_PACKAGE bool __fastcall FileNameValid(System::UnicodeString FileName);
extern DELPHI_PACKAGE int __fastcall GetFileSize(const System::UnicodeString FileName);
extern DELPHI_PACKAGE System::TDateTime __fastcall FileDateTime(const System::UnicodeString FileName);
extern DELPHI_PACKAGE bool __fastcall CreateFile(System::UnicodeString FileName);
extern DELPHI_PACKAGE void __fastcall EnsureDirExists(System::UnicodeString RootName, System::UnicodeString DirName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall EnsureValidFileName(System::UnicodeString PathName, System::UnicodeString FileName);
extern DELPHI_PACKAGE bool __fastcall DirExists(System::UnicodeString DirName);
extern DELPHI_PACKAGE bool __fastcall PurgeDir(System::UnicodeString DirName);
extern DELPHI_PACKAGE NativeUInt __fastcall RunProgram(System::UnicodeString StartName, System::UnicodeString Params, System::UnicodeString StartDir);
extern DELPHI_PACKAGE TReducedDateTime __fastcall MakeReducedDT(System::Word Year, System::Word Month, System::Word Day, System::Word DOW, System::Word Hour, System::Word Min);
extern DELPHI_PACKAGE bool __fastcall CompareReducedDT(const TReducedDateTime &T1, const TReducedDateTime &T2);
extern DELPHI_PACKAGE TReducedDateTime __fastcall DateTimeToReduced(System::TDateTime T);
extern DELPHI_PACKAGE System::TDateTime __fastcall ReducedToDateTime(const TReducedDateTime &T);
extern DELPHI_PACKAGE bool __fastcall IsBIn(int index, System::Byte storage);
extern DELPHI_PACKAGE int __fastcall Sign(int a);
extern DELPHI_PACKAGE bool __fastcall InRangeF(double L, double R, double x);
extern DELPHI_PACKAGE bool __fastcall InRange(int L, int R, int x);
extern DELPHI_PACKAGE int __fastcall Max(int a, int b);
extern DELPHI_PACKAGE int __fastcall Min(int a, int b);
extern DELPHI_PACKAGE System::TDateTime __fastcall SubtractTimes(System::TDateTime Time1, System::TDateTime Time2);
extern DELPHI_PACKAGE bool __fastcall RangesIntersect(int L1, int R1, int L2, int R2);
extern DELPHI_PACKAGE bool __fastcall WriteStringToStream(System::Classes::TStream* S, System::UnicodeString Str);
extern DELPHI_PACKAGE bool __fastcall ReadStringFromStream(System::Classes::TStream* S, System::UnicodeString &Str);
extern DELPHI_PACKAGE bool __fastcall WriteStringToStreamA(System::Classes::TStream* S, System::AnsiString Str);
extern DELPHI_PACKAGE bool __fastcall ReadStringFromStreamA(System::Classes::TStream* S, System::AnsiString &Str);
extern DELPHI_PACKAGE bool __fastcall WriteWideStringToStream(System::Classes::TStream* S, System::WideString Str);
extern DELPHI_PACKAGE bool __fastcall ReadWideStringFromStream(System::Classes::TStream* S, System::WideString &Str);
extern DELPHI_PACKAGE bool __fastcall WriteFStringToStream(System::Classes::TStream* S, Elstrutils::TElFString Str);
extern DELPHI_PACKAGE bool __fastcall ReadFStringFromStream(System::Classes::TStream* S, Elstrutils::TElFString &Str);
extern DELPHI_PACKAGE unsigned __fastcall ElDateTimeToSeconds(int ADay, int AMonth, int AYear, int AHours, int AMinute, int ASecond);
extern DELPHI_PACKAGE void __fastcall ElSecondsToDateTime(unsigned Seconds, int &ADay, int &AMonth, int &AYear, int &AHours, int &AMinute, int &ASecond);
extern DELPHI_PACKAGE int __fastcall DateToJulianDays(int ADay, int AMonth, int AYear);
extern DELPHI_PACKAGE void __fastcall JulianDaysToDate(int &ADay, int &AMonth, int &AYear, int JulianDate);
extern DELPHI_PACKAGE int __fastcall ElDayOfWeek(int ADay, int AMonth, int AYear);
extern DELPHI_PACKAGE int __fastcall DaysPerMonth(int AYear, int AMonth);
extern DELPHI_PACKAGE System::TDateTime __fastcall IncDate(System::TDateTime ADate, int Days, int Months, int Years);
extern DELPHI_PACKAGE System::TDateTime __fastcall NowToUTC(void);
extern DELPHI_PACKAGE void __fastcall UTCToZoneLocal(Winapi::Windows::PTimeZoneInformation lpTimeZoneInformation, const _SYSTEMTIME &lpUniversalTime, _SYSTEMTIME &lpLocalTime);
extern DELPHI_PACKAGE void __fastcall ZoneLocalToUTC(Winapi::Windows::PTimeZoneInformation lpTimeZoneInformation, _SYSTEMTIME &lpUniversalTime, const _SYSTEMTIME &lpLocalTime);
extern DELPHI_PACKAGE void __fastcall ElSystemTimeToTzSpecificLocalTime(Winapi::Windows::PTimeZoneInformation lpTimeZoneInformation, _SYSTEMTIME &lpUniversalTime, _SYSTEMTIME &lpLocalTime);
extern DELPHI_PACKAGE int __fastcall ZoneIDtoBias(System::UnicodeString ZoneID);
extern DELPHI_PACKAGE bool __fastcall SetPrivilege(System::UnicodeString sPrivilegeName, bool bEnabled);
extern DELPHI_PACKAGE HWND __fastcall WindowExists(System::UnicodeString ClassName, System::UnicodeString Caption, bool ExactMatch);
extern DELPHI_PACKAGE HWND __fastcall TopWindowExists(System::UnicodeString ClassName, System::UnicodeString Caption, bool ExactMatch);
extern DELPHI_PACKAGE System::UnicodeString __fastcall AppendSlash(const System::UnicodeString PathName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetModulePath(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetComputerName(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RectToString(const System::Types::TRect &Rect);
extern DELPHI_PACKAGE System::Types::TRect __fastcall StringToRect(System::UnicodeString AString);
extern DELPHI_PACKAGE void __fastcall ValFloat(System::UnicodeString Value, double Result, int &Error);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrFloat(double Value);
extern DELPHI_PACKAGE HWND __fastcall XAllocateHWND(System::TObject* Obj, TElWndMethod WndMethod);
extern DELPHI_PACKAGE void __fastcall XDeallocateHWND(HWND Wnd);
extern DELPHI_PACKAGE int __fastcall GetKeysState(void);
extern DELPHI_PACKAGE System::Classes::TShiftState __fastcall GetShiftState(void);
}	/* namespace Eltools */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTOOLS)
using namespace Eltools;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltoolsHPP
