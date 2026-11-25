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

constexpr u32 max_pose_bones = 4;

struct pose_cache
{
	const IKinematics* kinematics = nullptr;
	MotionID animation;
	u16 bones_ids[max_pose_bones] = {};
	u32 count = 0;
	Fmatrix local_bones[max_pose_bones] = {};
};

class base
{
public:
	base(const base& other) = delete;
	base& operator=(const base& other) = delete;
	base(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target, pose_cache& cache);

protected:
	void fill_bones(u16 const* bones_ids, u32 count, Fmatrix* local_bones);
	void rotate_children(u16 root_id, Fmatrix const& rotation, u16 const* bones_ids, u32 count, Fmatrix* local_bones) const;
	void aim_at_position(Fvector const& bone_position, Fvector const& object_position, Fvector object_direction, Fmatrix& result);

protected:
	Fmatrix m_start_transform = Fidentity;
	CGameObject& m_object;
	IKinematics& m_kinematics;
	IKinematicsAnimated& m_animated;
	Fvector const& m_target;
	pose_cache& m_cache;
	MotionID m_animation_id;
	bool m_animation_start = false;
};

}
