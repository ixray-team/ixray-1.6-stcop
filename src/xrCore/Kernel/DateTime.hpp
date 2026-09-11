#pragma once
#include <ctime>
#include <string>

class XRCORE_API Time
{
private:
	time_t t;
	tm aTm;
	using string = xr_string;

public:
	Time();

	// Convert time to string
	string GetSecondsString() const;
	string GetMinutesString() const;
	string GetHoursString() const;

	// Convert date to string
	string GetDayString() const;
	string GetMonthString() const;
	string GetYearString() const;

	int GetSeconds() const;
	int GetMinutes() const;
	int GetHours() const;

	int GetDay() const;
	int GetMonth() const;
	int GetYear() const;
};