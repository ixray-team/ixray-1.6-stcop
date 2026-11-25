////////////////////////////////////////////////////////////////////////////
//	Module 		: aimers_bone.cpp
//	Created 	: 04.04.2008
//  Modified 	: 08.04.2008
//	Author		: Dmitriy Iassenev
//	Description : bone aimer class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "aimers_bone.h"
#include "../Include/xrRender/Kinematics.h"

template <u32 bone_count>
aimers::bone<bone_count>::bone(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target, pose_cache& cache, const char* (&bones)[bone_count])
	: inherited(object, animation_id, animation_start, target, cache)
{
	static_assert(bone_count <= max_pose_bones, "Too many bones for the pose cache");

	for (u32 i = 0; i < bone_count; ++i)
	{
		m_bones_ids[i] = m_kinematics.LL_BoneID(bones[i]);
		VERIFY2(m_bones_ids[i] != BI_NONE, bones[i]);
	}

	Fmatrix local_bones[bone_count] = {};
	fill_bones(m_bones_ids, bone_count, local_bones);

	const Fmatrix inverted_start = Fmatrix(m_start_transform).invert();
	for (u32 i = 0; i + 1 < bone_count; ++i)
	{
		const Fmatrix bone_global = Fmatrix().mul_43(m_start_transform, local_bones[i]);
		const Fmatrix end_global = Fmatrix().mul_43(m_start_transform, local_bones[bone_count - 1]);

		Fmatrix& result = m_result[i];
		aim_at_position(bone_global.c, end_global.c, end_global.k, result);
		result = Fmatrix(inverted_start).mulB_43(result).mulB_43(m_start_transform);

		Fvector angles = zero_vel;
		result.getXYZ(angles);
		angles.mul(1.0f / float(bone_count - i - 1));
		result.setXYZ(angles);
		VERIFY(_valid(result));

		rotate_children(m_bones_ids[i], result, m_bones_ids, bone_count, local_bones);
	}

	m_result[bone_count - 1] = Fidentity;
}

template <u32 bone_count>
Fmatrix const& aimers::bone<bone_count>::get_bone(u32 const& bone_id) const
{
	VERIFY(bone_id < bone_count);
	return m_result[bone_id];
}

template class aimers::bone<3>;
