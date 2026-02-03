#pragma once
#include "xr_collide_form.h"

class ENGINE_API CCF_DynamicMesh :
	public CCF_Skeleton
{
	using inherited = CCF_Skeleton;

public:
	CCF_DynamicMesh(CObject* _owner) :CCF_Skeleton(_owner)
	{};

	virtual BOOL _RayQuery(const collide::ray_defs& Q, collide::rq_results& R);
};