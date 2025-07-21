#pragma once

#include "../../xrCore/xrCore.h"

xr_vector<shared_str> ParseGameItems(CInifile*);
xr_vector<shared_str> ParseGameQuests();

void RunSQLRequest();

struct SLoginInfo
{
	string32 Login;
	string32 Pass;
	string32 DataBase;
	string128 Host;
};

extern SLoginInfo GLoginInfo;