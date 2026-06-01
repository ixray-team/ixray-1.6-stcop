#pragma once

#include "WristwatchTypes.h"

class CWristwatchSurgeProvider
{
public:
	SWristwatchSurgeState QueryState() const;

private:
	bool TryCallLuaState(SWristwatchSurgeState& outState) const;
};
