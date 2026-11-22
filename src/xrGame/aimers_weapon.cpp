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
#include "../xrEngine/bone.h"
#include "Weapon.h"

using aimers::weapon;

weapon::weapon(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target, const char* bone0,
	const char* bone1, const char* weapon_bone0, const char* weapon_bone1, CWeapon const& weapon)
	: inherited(object, animation_id, animation_start, target), m_weapon(weapon)
{
	const char* bones[4] = {bone0, bone1, weapon_bone0, weapon_bone1};
	for (u32 i = 0; i < 4; ++i)
	{
		m_bones_ids[i] = m_kinematics.LL_BoneID(bones[i]);
	}

	const CBoneData& bone_data = m_kinematics.LL_GetData(m_bones_ids[weapon_bone_id0]);
	m_bones_ids[parent_weapon_bone_id0] = bone_data.GetParentID();
	VERIFY(m_bones_ids[parent_weapon_bone_id0] != BI_NONE);

	compute_bone(bone_id0);

	Fmatrix& bone_0 = m_result[bone_id0];
	VERIFY(_valid(bone_0));
	Fvector angles = zero_vel;
	bone_0.getXYZ(angles);
	VERIFY(_valid(angles));
	angles.mul(.5f);
	bone_0.setXYZ(angles);
	VERIFY(_valid(bone_0));

	CBoneInstance& bone = m_kinematics.LL_GetBoneInstance(m_bones_ids[bone_id0]);
	const BoneCallback& old_callback = bone.callback();
	void* old_callback_param = bone.callback_param();
	bone.set_callback(bctCustom, &callback, &bone_0);

	compute_bone(bone_id1);

	bone.set_callback(bctCustom, old_callback, old_callback_param);
}

void weapon::compute_bone(u32 const bone_id)
{
	VERIFY(bone_id < weapon_bone_id0);

	u32 bones_ids[] = {bone_id, weapon_bone_id0, weapon_bone_id1, parent_weapon_bone_id0};
	fill_bones(bones_ids, m_bones_ids, m_local_bones, m_bones);

	Fmatrix transform = Fidentity;
	CInventoryItem::GetHandsBasis(m_local_bones[weapon_bone_id1], m_local_bones[weapon_bone_id0], transform);
	transform.mulA_43(m_start_transform);
	transform.mulB_43(m_weapon.m_ActiveOffset.GetXFORM());

	Fvector position = zero_vel;
	transform.transform_tiny(position, m_weapon.vLoadedFirePoint);
	const Fvector direction = transform.k;

	VERIFY(_valid(m_bones[bone_id]));
	VERIFY(_valid(position));
	VERIFY(_valid(direction));
	aim_at_position(m_bones[bone_id].c, position, direction, m_result[bone_id]);
	VERIFY(_valid(m_result[bone_id]));
	VERIFY(_valid(m_start_transform));
	VERIFY(_valid(Fmatrix(m_start_transform).invert()));
	m_result[bone_id] = Fmatrix(m_start_transform).invert().mulB_43(m_result[bone_id]).mulB_43(m_start_transform);
}
