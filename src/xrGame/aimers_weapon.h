////////////////////////////////////////////////////////////////////////////
//	Module 		: aimers_weapon.h
//	Created 	: 04.04.2008
//  Modified 	: 08.04.2008
//	Author		: Dmitriy Iassenev
//	Description : weapon aimer class
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "aimers_base.h"

class CWeapon;

namespace aimers
{

class weapon final : public base
{
	using inherited = base;

	enum
	{
		bone_id0 = u32(0),
		bone_id1,
		weapon_bone_id0,
		weapon_bone_id1,
		bones_count,
	};

	Fmatrix m_result[2] = {};
	Fmatrix m_local_bones[bones_count] = {};
	u16 m_bones_ids[bones_count] = {};
	const CWeapon& m_weapon;

	void compute_bone(u32 const bone_id);

public:
	weapon(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target, pose_cache& cache, const char* bone0,
	const char* bone1, const char* weapon_bone0, const char* weapon_bone1, CWeapon const& weapon);

	inline Fmatrix const& get_bone(const u32& bone_id) const
	{
		VERIFY(bone_id < 2);
		return (m_result[bone_id]);
	}
};

}
