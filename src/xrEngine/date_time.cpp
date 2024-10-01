////////////////////////////////////////////////////////////////////////////
//	Module 		: date_time.h
//	Created 	: 08.05.2004
//  Modified 	: 08.05.2004
//	Author		: Dmitriy Iassenev
//	Description : Date and time routines
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "date_time.h"

static u64 generate_time_impl	(u32 years, u32 months, u32 days, u32 hours, u32 minutes, u32 seconds, u32 milliseconds)
{
	u64 const extra_day_count	= ((years % 400 == 0) || ((years % 4 == 0) && (years % 100 != 0))) ? 1 : 0;
	u64 const years_minus_1		= u64(years - 1);
	u64	result					= years_minus_1*365 + years_minus_1/4 - years_minus_1/100 + years_minus_1/400;
	if (months >  1) result		+= u64(31);
	if (months >  2) result		+= u64(28 + extra_day_count);
	if (months >  3) result		+= u64(31);
	if (months >  4) result		+= u64(30);
	if (months >  5) result		+= u64(31);
	if (months >  6) result		+= u64(30);
	if (months >  7) result		+= u64(31);
	if (months >  8) result		+= u64(31);
	if (months >  9) result		+= u64(30);
	if (months > 10) result		+= u64(31);
	if (months > 11) result		+= u64(30);
	result						+= u64(days - 1);
	result						= result*u64(24) + u64(hours);
	result						= result*u64(60) + u64(minutes);
	result						= result*u64(60) + u64(seconds);
	result						= result*u64(1000) + u64(milliseconds);
	return						(result);
}

static void split_time_impl		(u64 time, u32 &years, u32 &months, u32 &days, u32 &hours, u32 &minutes, u32 &seconds, u32 &milliseconds)
{
	milliseconds				= u32(time%1000);
	time						/= 1000;
	seconds						= u32(time%60);
	time						/= 60;
	minutes						= u32(time%60);
	time						/= 60;
	hours						= u32(time%24);
	time						/= 24;

	u64 const p0				= time/(400*365 + 100 - 4 + 1);
	time						-= p0*(400*365 + 100 - 4 + 1);
	u64 const p1				= time/(100*365 + 25 - 1);
	time						-= p1*(100*365 + 25 - 1);
	u64 const p2				= time/(4*365 + 1);
	time						-= p2*(4*365 + 1);
	u64 const p3				= _min(u32(time)/365, 3);
	time						-= p3*365;
	years						= u32(400*p0 + 100*p1 + 4*p2 + p3 + 1);
	++time;

	u64 const extra_day_count	= ((years % 400 == 0) || ((years % 4 == 0) && (years % 100 != 0))) ? 1 : 0;
	VERIFY						(time - extra_day_count < 366);

	months						= 1;
	if (time > 31) {
		++months;
		time					-= 31;
	if (time > 28 + extra_day_count) {
		++months;
		time					-= 28 + extra_day_count;
	if (time > 31) {
		++months;
		time					-= 31;
	if (time > 30) {
		++months;
		time					-= 30;
	if (time > 31) {
		++months;
		time					-= 31;
	if (time > 30) {
		++months;
		time					-= 30;
	if (time > 31) {
		++months;
		time					-= 31;
	if (time > 31) {
		++months;
		time					-= 31;
	if (time > 30) {
		++months;
		time					-= 30;
	if (time > 31) {
		++months;
		time					-= 31;
	if (time > 30) {
		++months;
		time					-= 30;
	}}}}}}}}}}}
	days						= u32(time);
}

ENGINE_API u64 generate_time(u32 years, u32 months, u32 days, u32 hours, u32 minutes, u32 seconds, u32 milliseconds)
{
	u64 const result			= generate_time_impl(years, months, days, hours, minutes, seconds, milliseconds);

#ifdef DEBUG
#	if 0
	{
		u64 const milliseconds_in_day	= 24*60*60*1000;
		u32						l_years, l_months, l_days, l_hours, l_minutes, l_seconds, l_milliseconds;
		if (years > 1 || months > 1 || days > 1)
			split_time_impl		(result - milliseconds_in_day, l_years, l_months, l_days, l_hours, l_minutes, l_seconds, l_milliseconds);
		split_time_impl			(result, l_years, l_months, l_days, l_hours, l_minutes, l_seconds, l_milliseconds);
		split_time_impl			(result + milliseconds_in_day, l_years, l_months, l_days, l_hours, l_minutes, l_seconds, l_milliseconds);
		split_time_impl			(result, l_years, l_months, l_days, l_hours, l_minutes, l_seconds, l_milliseconds);
	}
#	endif // #if 0
	u32							l_years, l_months, l_days, l_hours, l_minutes, l_seconds, l_milliseconds;
	split_time_impl				(result, l_years, l_months, l_days, l_hours, l_minutes, l_seconds, l_milliseconds);
	u64 const new_result		= generate_time_impl(l_years, l_months, l_days, l_hours, l_minutes, l_seconds, l_milliseconds);
	VERIFY						(result == new_result);
	split_time_impl				(result, years, months, days, hours, minutes, seconds, milliseconds);
	VERIFY						(years == l_years);
	VERIFY						(months == l_months);
	VERIFY						(days == l_days);
	VERIFY						(hours == l_hours);
	VERIFY						(minutes == l_minutes);
	VERIFY						(seconds == l_seconds);
	VERIFY						(milliseconds == l_milliseconds);
#endif // #ifdef DEBUG
	return						(result);
}

ENGINE_API void split_time(u64 time, u32& years, u32& months, u32& days, u32& hours, u32& minutes, u32& seconds, u32& milliseconds)
{
	split_time_impl				(time, years, months, days, hours, minutes, seconds, milliseconds);
#ifdef DEBUG
	u64 const test_time			= generate_time_impl(years, months, days, hours, minutes, seconds, milliseconds);
	VERIFY						(test_time == time);
#endif // #ifdef DEBUG
}