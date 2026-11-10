#include "stdafx.h"
#include "BuildId.h"

// computing build id
ENGINE_API const char* build_date;
ENGINE_API u32 build_id;

static const char* month_id[12] =
{
	"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
};

static int days_in_month[12] = 
{
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

static int start_day	= 31;	// 31
static int start_month	= 1;	// January
static int start_year	= 1999;	// 1999

void ComputeBuildID()
{
	PROF_EVENT("compute_build_id");
	build_date = __DATE__;

	int days;
	int months = 0;
	int years;

	string16 month;

	string256 buffer;
	xr_strcpy(buffer, __DATE__);

	sscanf(buffer, "%15s %d %d", month, &days, &years);

	for (int i = 0; i < 12; i++)
	{
		if (_stricmp(month_id[i], month))
		{
			continue;
		}

		months = i;
		break;
	}

	build_id = (years - start_year) * 365 + days - start_day;

	for (int i = 0; i < months; ++i)
	{
		build_id += days_in_month[i];
	}

	for (int i = 0; i < start_month - 1; ++i)
	{
		build_id -= days_in_month[i];
	}
}