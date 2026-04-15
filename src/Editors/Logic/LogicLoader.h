#pragma once
#include <string>
#include <vector>
#include "LogicMetainfo.h"

class LogicLoader
{
public:
	[[deprecated]] static xr_vector<FState> LoadAsStates(const xr_string& folder);
	static xr_vector<FState> LoadFromFile(const xr_string& filename);
};
