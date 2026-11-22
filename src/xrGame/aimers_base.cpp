////////////////////////////////////////////////////////////////////////////
//	Module 		: aimers_base.cpp
//	Created 	: 04.04.2008
//  Modified 	: 08.04.2008
//	Author		: Dmitriy Iassenev
//	Description : aimers base class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "aimers_base.h"
#include "GameObject.h"
#include "../Include/xrRender/Kinematics.h"
#include "animation_movement_controller.h"
#include "../Include/xrRender/KinematicsAnimated.h"

using aimers::base;

namespace
{
	void SafeDirection(Fvector& direction)
	{
		if (direction.magnitude() < EPS_L)
		{
			direction.set(0.0f, 0.0f, EPS_L);
		}
	}

	Fmatrix RotationBetween(const Fvector& from, const Fvector& to, const Fvector& hint)
	{
		Fmatrix result = Fidentity;
		Fvector axis = Fvector().crossproduct(from, to);
		const float sin_alpha = clampr(axis.magnitude(), -1.0f, 1.0f);
		const float cos_alpha = clampr(from.dotproduct(to), -1.0f, 1.0f);

		if (!fis_zero(sin_alpha))
		{
			result.rotation(axis.div(sin_alpha), atan2f(sin_alpha, cos_alpha));
			return result;
		}

		if (cos_alpha > 0.0f)
		{
			result.identity();
			return result;
		}

		axis.crossproduct(from, hint);
		if (fis_zero(axis.magnitude()))
		{
			Fvector right = zero_vel;
			Fvector::generate_orthonormal_basis(from, axis, right);
		}
		else
		{
			axis.normalize();
		}

		result.rotation(axis, PI);
		return result;
	}
}

base::base(CGameObject* object, const char* animation_id, bool animation_start, Fvector const& target)
	: m_object(*object), m_kinematics(*PKinematics(object->Visual())), m_animated(*object->Visual()->dcast_PKinematicsAnimated()),
	m_target(target), m_animation_id(m_animated.LL_MotionID(animation_id)), m_animation_start(animation_start)
{
	const animation_movement_controller* controller = m_object.animation_movement();
	m_start_transform = controller ? controller->start_transform() : m_object.XFORM();
	VERIFY(_valid(m_start_transform));
}

void base::fill_bones(u32 const* bones, u32 bones_count, u16 const* bones_ids, u32 bones_ids_count, Fmatrix* local_bones, Fmatrix* global_bones)
{
	CBoneInstance& root_bone = m_kinematics.LL_GetBoneInstance(m_kinematics.LL_GetBoneRoot());
	const BoneCallback callback = root_bone.callback();
	void* callback_params = root_bone.callback_param();

	const animation_movement_controller* controller = m_object.animation_movement();
	if (!m_animation_start || !controller || !controller->IsBlending())
	{
		root_bone.set_callback(bctCustom, nullptr, nullptr);
	}

	const u8 channel_id = 1;
	const u8 channel_mask = 1 << channel_id;

	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		if (!m_animation_start)
		{
			if (CBlend* blend = m_animated.LL_PlayCycle(part, m_animation_id, false, nullptr, nullptr, channel_id))
			{
				blend->timeCurrent = blend->timeTotal - (SAMPLE_SPF + EPS);
			}
			continue;
		}

		for (u32 i = 0, count = m_animated.LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = m_animated.LL_PartBlend(part, i);
			CBlend* new_blend = m_animated.LL_PlayCycle(part, blend->motionID, true, nullptr, nullptr, channel_id);
			VERIFY(new_blend);
			*new_blend = *blend;
			new_blend->channel = channel_id;
		}
	}

	for (u32 n = 0; n < bones_count; ++n)
	{
		const u32 i = bones[n];
		VERIFY(i < bones_ids_count);
		m_kinematics.LL_GetBoneLocalTransform(bones_ids[i], local_bones[i]);
		VERIFY(_valid(local_bones[i]));
		global_bones[i].mul_43(m_start_transform, local_bones[i]);
		VERIFY(_valid(global_bones[i]));
	}

	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		m_animated.LL_CloseCycle(part, channel_mask);
	}

	root_bone.set_callback(bctCustom, callback, callback_params);
}

void base::callback(CBoneInstance* bone)
{
	VERIFY(bone);

	const Fmatrix* rotation = static_cast<Fmatrix*>(bone->callback_param());
	VERIFY(rotation);
	VERIFY2(_valid(*rotation), "base::callback[rotation] ");

	const Fvector position = bone->mTransform.c;
	bone->mTransform.mulA_43(*rotation);
	bone->mTransform.c = position;
	VERIFY2(_valid(bone->mTransform), "base::callback ");
}

void base::aim_at_position(Fvector const& bone_position, Fvector const& object_position, Fvector object_direction, Fmatrix& result)
{
	object_direction.normalize();

	const float offset = Fvector().sub(bone_position, object_position).dotproduct(object_direction);
	const Fvector current_point = Fvector().mad(object_position, object_direction, offset);
	VERIFY(_valid(current_point));

	Fvector bone2current = Fvector().sub(current_point, bone_position);
	SafeDirection(bone2current);
	const float sphere_radius_sqr = bone2current.square_magnitude();

	Fvector direction_target = Fvector().sub(m_target, bone_position);
	SafeDirection(direction_target);
	const float invert_magnitude = 1.0f / direction_target.magnitude();
	direction_target.mul(invert_magnitude);
	VERIFY2(fsimilar(direction_target.magnitude(), 1.0f), make_string<const char*>("[%f][%f] [%f][%f][%f] [%f][%f][%f]",
		direction_target.magnitude(), invert_magnitude, VPUSH(m_target), VPUSH(bone_position)));

	const float to_circle_center = sphere_radius_sqr * invert_magnitude;
	const Fvector circle_center = Fvector().mad(bone_position, direction_target, to_circle_center);
	VERIFY(_valid(circle_center));

	const Fplane plane = Fplane().build(circle_center, direction_target);
	VERIFY2(_valid(plane), make_string<const char*>("[%f][%f][%f] [%f][%f][%f] [%f][%f][%f] %f", VPUSH(circle_center), VPUSH(direction_target), VPUSH(plane.n), plane.d));

	Fvector projection = zero_vel;
	plane.project(projection, current_point);

	Fvector projection2circle_center = Fvector().sub(projection, circle_center);
	SafeDirection(projection2circle_center);
	const Fvector center2projection_direction = projection2circle_center.normalize();

	const float circle_radius = _sqrt(std::max(sphere_radius_sqr - _sqr(to_circle_center), 0.0f));
	const Fvector target_point = Fvector().mad(circle_center, center2projection_direction, circle_radius);
	VERIFY(_valid(target_point));

	const Fvector current_direction = Fvector(bone2current).normalize();

	Fvector target2bone = Fvector().sub(target_point, bone_position);
	SafeDirection(target2bone);
	const Fvector target_direction = target2bone.normalize();

	const Fmatrix transform0 = RotationBetween(current_direction, target_direction, direction_target);
	VERIFY(_valid(transform0));

	Fvector target2target_point = Fvector().sub(m_target, target_point);
	SafeDirection(target2target_point);
	const Fvector new_direction = target2target_point.normalize();

	Fvector old_direction = zero_vel;
	transform0.transform_dir(old_direction, object_direction);
	const Fmatrix transform1 = RotationBetween(old_direction, new_direction, target_direction);
	VERIFY(_valid(transform1));

	result.mul_43(transform1, transform0);
	VERIFY(_valid(result));
}
