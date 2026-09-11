#include "stdafx.h"
#include "DateTime.hpp"

Time::Time()
{
	t = time(nullptr);
	aTm = *localtime(&t);
}

int Time::GetSeconds() const
{
	return aTm.tm_sec;
}

int Time::GetMinutes() const
{
	return aTm.tm_min;
}

int Time::GetHours() const
{
	return aTm.tm_hour;
}

int Time::GetDay() const
{
	return aTm.tm_mday;
}

int Time::GetMonth() const
{
	return aTm.tm_mon + 1;
}

int Time::GetYear() const
{
	return aTm.tm_year + 1900;
}

Time::string Time::GetSecondsString() const
{
	return (GetSeconds() < 10) ? "0" + xr_string::ToString(GetSeconds()) : xr_string::ToString(GetSeconds());
}

Time::string Time::GetMinutesString() const
{
	return (GetMinutes() < 10) ? "0" + xr_string::ToString(GetMinutes()) : xr_string::ToString(GetMinutes());
}

Time::string Time::GetHoursString() const
{
	return (GetHours() < 10) ? "0" + xr_string::ToString(GetHours()) : xr_string::ToString(GetHours());
}

Time::string Time::GetDayString() const
{
	return (GetDay() < 10) ? "0" + xr_string::ToString(GetDay()) : xr_string::ToString(GetDay());
}

Time::string Time::GetMonthString() const
{
	return (GetMonth() < 10) ? "0" + xr_string::ToString(GetMonth()) : xr_string::ToString(GetMonth());
}

Time::string Time::GetYearString() const
{
	return xr_string::ToString(GetYear());
}