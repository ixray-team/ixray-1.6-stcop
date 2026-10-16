#pragma once
#include "../xrEngine/IGame_Persistent.h"

namespace LevelStartInfo
{
	bool Find(const IGame_Persistent::params& Params, shared_str& LevelName);
}
