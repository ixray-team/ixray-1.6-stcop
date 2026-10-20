#pragma once
#include "Debug.h"
class CELSharedDebugDrawWorld final
{
public:
	static void Draw(const xr_vector<SELDebugRecord>& Records, const xr_vector<SELDebugEvent>& Events, bool DrawGeometry, bool DrawText);
};
