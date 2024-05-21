#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SKELETON_H__
#define __XR_SKELETON_H__

#include "xr_vector2.h"
#include "xr_vector3.h"
#include "xr_obb.h"
#include "xr_sphere.h"
#include "xr_cylinder.h"

namespace xray_re {

enum {
	MAX_BONES	= 64,
};

enum s_bone_shape_type {
	ST_NONE		= 0,
	ST_BOX		= 1,
	ST_SPHERE	= 2,
	ST_CYLINDER	= 3,
};

enum s_bone_shape_flag {
	SF_NO_PICKABLE		= 0x1,
	SF_REMOVE_AFTER_BREAK	= 0x2,
	SF_NO_PHYSICS		= 0x4,
};

struct s_bone_shape {
			s_bone_shape();
	void		reset();
	uint16_t	type;
	uint16_t	flags;
	fobb		box;
	fsphere		sphere;
	fcylinder	cylinder;
};

enum s_joint_type {
	JT_RIGID	= 0,
	JT_CLOTH	= 1,
	JT_JOINT	= 2,
	JT_WHEEL	= 3,
	JT_NONE		= 4,
	JT_SLIDER	= 5,
};

enum s_joint_flag {
	JF_BREAKABLE	= 0x1,
};

// for x, y, z
struct s_joint_limit {
			s_joint_limit();
	void		reset();
	fvector2	limit;
	float		spring_factor;
	float		damping_factor;
};

struct s_joint_ik_data {
			s_joint_ik_data();
	void		reset();
	uint32_t	type;
	s_joint_limit	limits[3];
	float		spring_factor;
	float		damping_factor;
	uint32_t	ik_flags;
	float		break_force;
	float		break_torque;
	float		friction;
};

inline s_bone_shape::s_bone_shape() { reset(); }
inline s_joint_limit::s_joint_limit() { reset(); }
inline s_joint_ik_data::s_joint_ik_data() { reset(); }

} // end of namespace xray_re

#endif
