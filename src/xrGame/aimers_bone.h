////////////////////////////////////////////////////////////////////////////
//	Module 		: aimers_bone.h
//	Created 	: 04.04.2008
//  Modified 	: 08.04.2008
//	Author		: Dmitriy Iassenev
//	Description : bone aimer class
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "aimers_base.h"

namespace aimers
{

template <u32 bone_count>
class bone final : public base
{
	using inherited = base;

public:
	bone(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target, const char* (&bones)[bone_count]);
	Fmatrix const& get_bone(u32 const& bone_id) const;

private:
	void compute_bones(u32 const bone_id);

	Fmatrix m_result[bone_count] = {};
	Fmatrix m_bones[bone_count] = {};
	u16 m_bones_ids[bone_count] = {};
};

}
