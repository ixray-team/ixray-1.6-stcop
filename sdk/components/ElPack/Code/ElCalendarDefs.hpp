// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCalendarDefs.pas' rev: 35.00 (Windows)

#ifndef ElcalendardefsHPP
#define ElcalendardefsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ElTools.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcalendardefs
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHoliday;
class DELPHICLASS TElHolidays;
//-- type declarations -------------------------------------------------------
typedef System::Int8 TDayOfWeek;

enum DECLSPEC_DENUM TElWeekEndDay : unsigned char { Sun, Mon, Tue, Wed, Thu, Fri, Sat };

typedef System::Set<TElWeekEndDay, TElWeekEndDay::Sun, TElWeekEndDay::Sat> TElWeekEndDays;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElHoliday : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FDescription;
	bool FFixedDate;
	System::Word FDay;
	System::Word FDayOfWeek;
	System::Word FMonth;
	bool FIsRest;
	void __fastcall SetFixedDate(bool newValue);
	void __fastcall SetDay(System::Word newValue);
	void __fastcall SetDayOfWeek(System::Word newValue);
	void __fastcall SetMonth(System::Word newValue);
	void __fastcall SetIsRest(bool newValue);
	
public:
	__fastcall virtual TElHoliday(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElHoliday();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	
__published:
	__property bool FixedDate = {read=FFixedDate, write=SetFixedDate, default=1};
	__property System::Word Day = {read=FDay, write=SetDay, nodefault};
	__property System::Word DayOfWeek = {read=FDayOfWeek, write=SetDayOfWeek, nodefault};
	__property System::Word Month = {read=FMonth, write=SetMonth, nodefault};
	__property bool IsRest = {read=FIsRest, write=SetIsRest, nodefault};
	__property System::UnicodeString Description = {read=FDescription, write=FDescription};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElHolidays : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElHoliday* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	TElHoliday* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TElHoliday* newValue);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TElHolidays(System::Classes::TComponent* AOwner);
	HIDESBASE TElHoliday* __fastcall Add();
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	__property TElHoliday* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TElHolidays() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elcalendardefs */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCALENDARDEFS)
using namespace Elcalendardefs;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcalendardefsHPP
