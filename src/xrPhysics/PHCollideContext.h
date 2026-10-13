#pragma once

#include "../xrCore/Collision/ISpatial.h"
#include "../3rd-party/ode/include/ode/common.h"

class CPHObject;

using qResultVec = xr_vector<ISpatialShared>;
using qResultIt = qResultVec::iterator;

struct SPHCollidePair
{
	CPHObject* Obj1 = nullptr;
	CPHObject* Obj2 = nullptr;
	dGeomID O1 = nullptr;
	dGeomID O2 = nullptr;
	bool Static = false;
};

struct SPHCollideContext
{
	qResultVec Spatial;
	xr_vector<SPHCollidePair> Pairs;
	xr_vector<CPHObject*> Objects;
	xr_vector<u32> PairBounds;
	xr_vector<CPHObject*> DirtyObjects;

	dGeomID MotionRay = nullptr;
	bool Collecting = false;

	SPHCollideContext();
	~SPHCollideContext();

	dGeomID GetMotionRay();
	void Reset();
};
