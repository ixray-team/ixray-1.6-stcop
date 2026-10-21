#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"

// Общая ограниченная память статических проходов; динамические объекты в неё не попадают.
class CELSharedFlyingSpaceCache final
{
public:
	static bool Find(const Fvector& Start, const Fvector& End, float Radius, float CenterHeight, EELGeometryResult& Result);
	static void Remember(const Fvector& Start, const Fvector& End, float Radius, float CenterHeight, EELGeometryResult Result);
	static void BeginFrame();
	static void Clear();
	static u32 Hits();
	static u32 Misses();
	static u32 TileCount();

private:
	struct SState;
	static SState& State();
	CELSharedFlyingSpaceCache() = delete;
};
