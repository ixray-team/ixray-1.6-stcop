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
#include "../xrEngine/bone.h"

template <u32 bone_count>
aimers::bone<bone_count>::bone(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target, const char* (&bones)[bone_count])
	: inherited(object, animation_id, animation_start, target)
{
	for (u32 i = 0; i < bone_count; ++i)
	{
		m_bones_ids[i] = m_kinematics.LL_BoneID(bones[i]);
		VERIFY2(m_bones_ids[i] != BI_NONE, bones[i]);
	}

	compute_bones(0);
}

template <u32 bone_count>
Fmatrix const& aimers::bone<bone_count>::get_bone(u32 const& bone_id) const
{
	VERIFY(bone_id < bone_count);
	return m_result[bone_id];
}

template <u32 bone_count>
void aimers::bone<bone_count>::compute_bones(u32 const bone_id)
{
	VERIFY(bone_id < bone_count);

	Fmatrix& bone_matrix = m_result[bone_id];
	if (bone_id + 1 == bone_count)
	{
		bone_matrix = Fidentity;
		return;
	}

	u32 bones_ids[] = { bone_id, bone_count - 1 };
	Fmatrix local_bones[bone_count] = {};
	fill_bones(bones_ids, m_bones_ids, local_bones, m_bones);

	aim_at_position(m_bones[bone_id].c, m_bones[bone_count - 1].c, m_bones[bone_count - 1].k, bone_matrix);
	bone_matrix = Fmatrix(m_start_transform).invert().mulB_43(bone_matrix).mulB_43(m_start_transform);

	Fvector angles = zero_vel;
	bone_matrix.getXYZ(angles);
	angles.mul(1.0f / float(bone_count - bone_id - 1));
	bone_matrix.setXYZ(angles);
	VERIFY(_valid(bone_matrix));

	CBoneInstance& instance = m_kinematics.LL_GetBoneInstance(m_bones_ids[bone_id]);
	const BoneCallback old_callback = instance.callback();
	void* old_callback_param = instance.callback_param();
	instance.set_callback(bctCustom, &inherited::callback, &bone_matrix);

	compute_bones(bone_id + 1);

	instance.set_callback(bctCustom, old_callback, old_callback_param);
}

template class aimers::bone<3>;
