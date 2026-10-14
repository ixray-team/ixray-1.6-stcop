#pragma once

#include "../xrCore/Collision/ISpatial.h"

using qResultVec = xr_vector<ISpatialShared>;
using qResultIt = qResultVec::iterator;

struct SPHCollisionQuery
{
	ISpatialShared Owner;
	Fbox Bounds;
	qResultVec Spatial;
	u64 Revision = 0;
	bool IsComplete = false;
};

struct SPHCollideContext
{
	qResultVec Spatial;
	const SPHCollisionQuery* Query = nullptr;
};
