#include "stdafx.h"
#include "cf_dynamic_mesh.h"
#include "xr_object.h"

#include "../Include/xrRender/RenderVisual.h"
#include "../Include/xrRender/Kinematics.h"

#ifdef DEBUG
#	include "IPHdebug.h"
#endif

bool CCF_DynamicMesh::_RayQuery(const collide::ray_defs& Q, collide::rq_results& R)
{
	const int StartCount = R.r_count();

	if (!inherited::_RayQuery(Q, R))
	{
		return false;
	}

	VERIFY(owner);
	VERIFY(owner->Visual());

	IKinematics* K = owner->Visual()->dcast_PKinematics();
	VERIFY(K);

	collide::rqVec& CollideResults = R.r_results();
	CollideResults.erase
	(
		std::remove_if
		(
			CollideResults.begin() + StartCount,
			CollideResults.end(),
			[&Q, &K, this](collide::rq_result& r)
			{
				VERIFY(r.IsStatic() || r.GetDynamic() == owner);

				IKinematics::pick_result br;
				if (!K->PickBone(
					owner->XFORM(),
					br,
					Q.range,
					Q.start,
					Q.dir,
					static_cast<u16>(r.element)))
				{
					return true;
				}

				r.range = br.dist;
				return false;
			}
		),
		CollideResults.end()
	);

	VERIFY(R.r_count() >= StartCount);
	return R.r_count() > StartCount;
}