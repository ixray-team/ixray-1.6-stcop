#pragma once

#include "../../xrCore/xrCore.h"

xr_vector<shared_str> ParseGameItems(CInifile*);
xr_vector<shared_str> ParseGameQuests();

void RunSQLRequest();

struct SLoginInfo
{
	shared_str Login;
	shared_str Pass;
	shared_str DataBase;
	shared_str Host;
};

extern SLoginInfo GLoginInfo;