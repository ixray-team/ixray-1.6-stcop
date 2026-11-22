////////////////////////////////////////////////////////////////////////////
//	Module 		: aimers_base.h
//	Created 	: 04.04.2008
//  Modified 	: 08.04.2008
//	Author		: Dmitriy Iassenev
//	Description : aimers base class
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../Include/xrRender/animation_motion.h"

class CGameObject;
class CBoneInstance;
class IKinematics;
class IKinematicsAnimated;

namespace aimers
{

class base
{
public:
	base(const base& other) = delete;
	base& operator=(const base& other) = delete;
	base(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target);

protected:
	template <u32 bone_count0, u32 bone_count1>
	void fill_bones(u32 const (&bones)[bone_count0], u16 const (&bones_ids)[bone_count1],
		Fmatrix (&local_bones)[bone_count1], Fmatrix (&global_bones)[bone_count1])
	{
		static_assert(bone_count0 <= bone_count1, "Invalid arrays passed");
		fill_bones(bones, bone_count0, bones_ids, bone_count1, local_bones, global_bones);
	}

	void fill_bones(u32 const* bones, u32 bones_count, u16 const* bones_ids, u32 bones_ids_count, Fmatrix* local_bones, Fmatrix* global_bones);
	void aim_at_position(Fvector const& bone_position, Fvector const& object_position, Fvector object_direction, Fmatrix& result);
	static void callback(CBoneInstance* bone);

protected:
	Fmatrix m_start_transform = Fidentity;
	CGameObject& m_object;
	IKinematics& m_kinematics;
	IKinematicsAnimated& m_animated;
	Fvector const& m_target;
	MotionID m_animation_id = {};
	bool m_animation_start = false;
};

}