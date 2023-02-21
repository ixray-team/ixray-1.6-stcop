// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElClock.pas' rev: 35.00 (Windows)

#ifndef ElclockHPP
#define ElclockHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElTools.hpp>
#include <ElList.hpp>
#include <ElStrUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <ElPanel.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Forms.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elclock
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElClock;
struct TShortTZ;
struct TTimeZoneInfo;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElClock : public Elpanel::TElPanel
{
	typedef Elpanel::TElPanel inherited;
	
protected:
	bool FTimerPaused;
	System::TDateTime FStartTime;
	System::TDateTime FPauseTime;
	bool FIsTimer;
	bool FTimerActive;
	bool FShowDate;
	bool FShowHint;
	Vcl::Extctrls::TTimer* FTimer;
	_TIME_ZONE_INFORMATION FTZone;
	bool FLocal;
	bool FSeconds;
	bool FAMPM;
	System::UnicodeString FCaption;
	bool FUseBias;
	int FBias;
	bool FShowWeekDay;
	bool FUseCustomFormat;
	System::UnicodeString FCustomFormat;
	bool FShowDaysInTimer;
	bool FCountdownActive;
	System::Classes::TNotifyEvent FOnCountdownDone;
	System::Classes::TNotifyEvent FOnCountdownTick;
	bool FCountdownPaused;
	int FCountdownTime;
	int FSaveCDTime;
	bool FIsCountdown;
	Elstrutils::TElFString FDummyStr;
	void __fastcall SetIsCountdown(bool newValue);
	void __fastcall SetCountdownTime(int newValue);
	void __fastcall SetCountdownPaused(bool newValue);
	void __fastcall SetCountdownActive(bool newValue);
	void __fastcall SetShowDaysInTimer(bool newValue);
	void __fastcall SetUseCustomFormat(bool newValue);
	void __fastcall SetCustomFormat(System::UnicodeString newValue);
	void __fastcall SetShowWeekDay(bool newValue);
	void __fastcall SetUseBias(bool newValue);
	void __fastcall SetBias(int newValue);
	bool __fastcall GetTimer();
	void __fastcall SetTimer(bool value);
	void __fastcall OnTimer(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Msg);
	void __fastcall SetShowDate(bool newValue);
	HIDESBASE void __fastcall SetShowHint(bool newValue);
	void __fastcall SetIsTimer(bool newValue);
	System::TDateTime __fastcall GetTimeElapsed();
	void __fastcall SetTimerActive(bool newValue);
	void __fastcall SetTimerPaused(bool newValue);
	void __fastcall CreateTimer();
	void __fastcall PaintBorders(Vcl::Graphics::TCanvas* Canvas, System::Types::TRect &R);
	virtual void __fastcall Paint();
	void __fastcall InheritedPaint();
	virtual void __fastcall TriggerCountdownDoneEvent();
	virtual void __fastcall TriggerCountdownTickEvent();
	
public:
	__fastcall virtual TElClock(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElClock();
	virtual void __fastcall Kick();
	virtual void __fastcall GetTime(_SYSTEMTIME &Time);
	void __fastcall ResetTimer();
	__property System::TDateTime TimeElapsed = {read=GetTimeElapsed};
	__property _TIME_ZONE_INFORMATION TimeZone = {read=FTZone, write=FTZone};
	
__published:
	__property Elstrutils::TElFString Caption = {read=FDummyStr};
	__property bool LocalTime = {read=FLocal, write=FLocal, default=1};
	__property bool ShowWeekDay = {read=FShowWeekDay, write=SetShowWeekDay, nodefault};
	__property bool ShowSeconds = {read=FSeconds, write=FSeconds, nodefault};
	__property bool ShowDate = {read=FShowDate, write=SetShowDate, nodefault};
	__property bool AM_PM = {read=FAMPM, write=FAMPM, nodefault};
	__property bool Labels = {read=FShowHint, write=SetShowHint, nodefault};
	__property bool UseBias = {read=FUseBias, write=SetUseBias, nodefault};
	__property int Bias = {read=FBias, write=SetBias, nodefault};
	__property bool UseCustomFormat = {read=FUseCustomFormat, write=SetUseCustomFormat, nodefault};
	__property System::UnicodeString CustomFormat = {read=FCustomFormat, write=SetCustomFormat};
	__property bool IsTimer = {read=FIsTimer, write=SetIsTimer, nodefault};
	__property bool TimerActive = {read=FTimerActive, write=SetTimerActive, nodefault};
	__property bool TimerPaused = {read=FTimerPaused, write=SetTimerPaused, default=0};
	__property bool ShowDaysInTimer = {read=FShowDaysInTimer, write=SetShowDaysInTimer, nodefault};
	__property bool IsCountdown = {read=FIsCountdown, write=SetIsCountdown, nodefault};
	__property int CountdownTime = {read=FCountdownTime, write=SetCountdownTime, nodefault};
	__property bool CountdownActive = {read=FCountdownActive, write=SetCountdownActive, nodefault};
	__property bool CountdownPaused = {read=FCountdownPaused, write=SetCountdownPaused, nodefault};
	__property bool UseTimer = {read=GetTimer, write=SetTimer, default=1};
	__property System::Classes::TNotifyEvent OnCountdownDone = {read=FOnCountdownDone, write=FOnCountdownDone};
	__property System::Classes::TNotifyEvent OnCountdownTick = {read=FOnCountdownTick, write=FOnCountdownTick};
	__property Align;
	__property Alignment = {default=2};
	__property BevelInner = {default=1};
	__property BevelOuter = {default=2};
	__property BevelWidth = {default=1};
	__property BorderWidth = {default=0};
	__property BorderStyle = {default=0};
	__property Color = {default=-16777201};
	__property Ctl3D;
	__property Cursor = {default=0};
	__property Font;
	__property Hint;
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property OnClick;
	__property OnDblClick;
	__property OnMouseMove;
	__property OnMouseDown;
	__property OnMouseUp;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnEndDrag;
	__property OnStartDrag;
	__property OnResize;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElClock(HWND ParentWindow) : Elpanel::TElPanel(ParentWindow) { }
	
};


typedef TShortTZ *PShortTZ;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TShortTZ
{
public:
	int Bias;
	int StandardBias;
	int DayLightBias;
	System::Word wReserved1;
	System::Word StdMonth;
	System::Word StdDayOfWeek;
	System::Word StdDay;
	System::Word StdHour;
	System::Word StdMinute;
	System::Word StdSecond;
	System::Word wReserved2;
	System::Word wReserved3;
	System::Word DLMonth;
	System::Word DLDayOfWeek;
	System::Word DLDay;
	System::Word DLHour;
	System::Word DLMinute;
	System::Word DLSecond;
	System::Word wReserved4;
};
#pragma pack(pop)


typedef TTimeZoneInfo *PTimeZoneInfo;

struct DECLSPEC_DRECORD TTimeZoneInfo
{
public:
	System::UnicodeString KeyName;
	System::UnicodeString DisplayName;
	System::UnicodeString DltName;
	System::UnicodeString StdName;
	System::UnicodeString MapID;
	_TIME_ZONE_INFORMATION TimeZone;
	TShortTZ STimeZone;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool FDL;
extern DELPHI_PACKAGE Ellist::TElList* SysTimeZones;
extern DELPHI_PACKAGE void __fastcall ShortTZToTimeZoneInfo(const TShortTZ &ShortTZ, TTimeZoneInfo &TZInfo);
extern DELPHI_PACKAGE System::UnicodeString __fastcall TranslateTZDate(const _SYSTEMTIME &ADate);
extern DELPHI_PACKAGE bool __fastcall RetrieveTimeZoneInfo(Ellist::TElList* TimeZoneInfoList);
}	/* namespace Elclock */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCLOCK)
using namespace Elclock;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElclockHPP
