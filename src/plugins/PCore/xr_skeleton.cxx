#include "xr_skeleton.h"

using namespace xray_re;

void s_bone_shape::reset()
{
	type = ST_NONE;
	flags = 0;
	box.reset();
	sphere.reset();
	cylinder.reset();
}

void s_joint_limit::reset()
{
	limit.set();
	spring_factor = 1.f;
	damping_factor = 1.f;
}

void s_joint_ik_data::reset()
{
	type = JT_RIGID;
	for (uint_fast32_t i = 3; i != 0;)
		limits[--i].reset();
	spring_factor = 1.f;
	damping_factor = 1.f;
	ik_flags = 0;
	break_force = 0;
	break_torque = 0;
	friction = 1.f;
}
