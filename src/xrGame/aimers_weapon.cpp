////////////////////////////////////////////////////////////////////////////
//	Module 		: aimers_weapon.cpp
//	Created 	: 04.04.2008
//  Modified 	: 04.04.2008
//	Author		: Dmitriy Iassenev
//	Description : weapon aimer class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "aimers_weapon.h"
#include "../Include/xrRender/Kinematics.h"
#include "Weapon.h"

using aimers::weapon;

weapon::weapon(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target, pose_cache& cache, const char* bone0,
	const char* bone1, const char* weapon_bone0, const char* weapon_bone1, CWeapon const& weapon)
	: inherited(object, animation_id, animation_start, target, cache), m_weapon(weapon)
{
	const char* bones[bones_count] = {bone0, bone1, weapon_bone0, weapon_bone1};
	for (u32 i = 0; i < bones_count; ++i)
	{
		m_bones_ids[i] = m_kinematics.LL_BoneID(bones[i]);
		VERIFY2(m_bones_ids[i] != BI_NONE, bones[i]);
	}

	fill_bones(m_bones_ids, bones_count, m_local_bones);

	compute_bone(bone_id0);

	Fmatrix& bone_0 = m_result[bone_id0];
	VERIFY(_valid(bone_0));
	Fvector angles = zero_vel;
	bone_0.getXYZ(angles);
	VERIFY(_valid(angles));
	angles.mul(.5f);
	bone_0.setXYZ(angles);
	VERIFY(_valid(bone_0));

	rotate_children(m_bones_ids[bone_id0], bone_0, m_bones_ids, bones_count, m_local_bones);

	compute_bone(bone_id1);
}

void weapon::compute_bone(u32 const bone_id)
{
	VERIFY(bone_id < weapon_bone_id0);

	Fmatrix transform = Fidentity;
	CInventoryItem::GetHandsBasis(m_local_bones[weapon_bone_id1], m_local_bones[weapon_bone_id0], transform);
	transform.mulA_43(m_start_transform);
	transform.mulB_43(m_weapon.m_ActiveOffset.GetXFORM());

	Fvector position = zero_vel;
	m_weapon.FirePoint(transform, m_weapon.ItemDesc().FireBone, m_weapon.ItemDesc().FirePoint, position);
	const Fvector direction = transform.k;

	Fvector bone_position = zero_vel;
	m_start_transform.transform_tiny(bone_position, m_local_bones[bone_id].c);

	VERIFY(_valid(bone_position));
	VERIFY(_valid(position));
	VERIFY(_valid(direction));
	aim_at_position(bone_position, position, direction, m_result[bone_id]);
	VERIFY(_valid(m_result[bone_id]));
	VERIFY(_valid(m_start_transform));
	VERIFY(_valid(Fmatrix(m_start_transform).invert()));
	m_result[bone_id] = Fmatrix(m_start_transform).invert().mulB_43(m_result[bone_id]).mulB_43(m_start_transform);
}
