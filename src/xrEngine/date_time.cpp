#include "stdafx.h"
#include "date_time.h"

u32 extra_day_count(u32 years)
{
	return ((years % 400 == 0) || ((years % 4 == 0) && (years % 100 != 0))) ? 1 : 0;
}

static u32 days_in_month[12] = { 31u, 28u, 31u, 30u, 31u, 30u, 31u, 31u, 30u, 31u, 30u, 31u };

u64 generate_time(u32 years, u32 months, u32 days, u32 hours, u32 minutes, u32 seconds, u32 milliseconds)
{
	u64 const years_minus_1 = u64(years - 1);
	u64	result = years_minus_1 * 365 + years_minus_1 / 4 - years_minus_1 / 100 + years_minus_1 / 400;
	for (u32 it = 0; it < months; it++)
	{
		if (months > it + 1)
		{
			result += days_in_month[it];
			if (it == 1)
			{
				result += extra_day_count(years);
			}
		}
	}

	result += u64(days - 1);
	result *= 24ul;
	result += u64(hours);
	result *= 60ul;
	result += u64(minutes);
	result *= 60ul;
	result += u64(seconds);
	result *= 1000ul;
	result += u64(milliseconds);
	return result;
}

void split_time(u64 time, u32& years, u32& months, u32& days, u32& hours, u32& minutes, u32& seconds, u32& milliseconds)
{
	milliseconds = u32(time % 1000u);
	time /= 1000;
	seconds = u32(time % 60u);
	time /= 60;
	minutes = u32(time % 60u);
	time /= 60;
	hours = u32(time % 24u);
	time /= 24;

	u64 const p0 = time / (400 * 365 + 100 - 4 + 1);
	time -= p0 * (400 * 365 + 100 - 4 + 1);
	u64 const p1 = std::min(time / (100 * 365 + 25 - 1), 3ui64);
	time -= p1 * (100 * 365 + 25 - 1);
	u64 const p2 = time / (4 * 365 + 1);
	time -= p2 * (4 * 365 + 1);
	u64 const p3 = std::min(time / 365u, 3ui64);
	time -= p3 * 365;
	years = u32(400 * p0 + 100 * p1 + 4 * p2 + p3 + 1);
	++time;

	months = 1;
	for (u32 it = 0; it < months; it++)
	{
		const u32 MonthDays = days_in_month[it] + (it == 1 ? extra_day_count(years) : 0);
		if (MonthDays < time)
		{
			++months;
			time -= MonthDays;
		}
	}
	days = u32(time);
}