#pragma once

#include "../../xrCore/xrCore.h"
#include <mysql/jdbc.h>

xr_vector<shared_str> ParseGameItems(CInifile*);
xr_vector<shared_str> ParseGameQuests();

void RunSQLRequest(bool Quest, bool Items, const char* SubDB);

struct SLoginInfo
{
	string32 Login;
	string32 Pass;
	string32 DataBase;
	string128 Host;

	xr_vector<shared_str> SubDB;
	volatile float ProgressStatus = 0.f;
	volatile float SubProgressStatus = 0.f;
};

extern sql::Driver* GSQLDriver;
extern sql::Connection* GSQLConnector;

extern SLoginInfo GLoginInfo;