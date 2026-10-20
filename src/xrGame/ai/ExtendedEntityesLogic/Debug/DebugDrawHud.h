#pragma once
#include "Debug.h"
class CELSharedDebugDrawHud final
{
public:
	static void Draw(const xr_vector<SELDebugRecord>& Records, const SELDebugFrame& Frame, const SELDebugPerformance& Performance);
};
