#pragma once

float CalcMotionSpeed(const shared_str& anim_name);

namespace HudRotation
{
	inline Fquaternion FromEuler(const Fvector& degrees)
	{
		Fmatrix rotation;
		rotation.setHPB(deg2rad(degrees.x), deg2rad(degrees.y), deg2rad(degrees.z));
		return Fquaternion().set(rotation);
	}

	inline Fvector ToEuler(const Fquaternion& rotation)
	{
		Fvector hpb;
		Fmatrix().rotation(rotation).getHPB(hpb);
		return hpb.mul(180.f / PI);
	}

	inline Fvector Delta(const Fquaternion& from, const Fquaternion& to)
	{
		Fquaternion delta;
		delta.mul(to, Fquaternion().inverse(from));
		if (delta.w < 0.f)
		{
			delta.set(-delta.w, -delta.x, -delta.y, -delta.z);
		}

		Fvector axis;
		float angle;
		if (!delta.get_axis_angle(axis, angle))
		{
			return zero_vel;
		}

		return axis.mul(angle * 180.f / PI);
	}

	inline Fquaternion Apply(const Fvector& degrees, const Fquaternion& rotation)
	{
		const float angle = degrees.magnitude();
		if (angle <= EPS_S)
		{
			return rotation;
		}

		Fvector axis = Fvector().div(degrees, angle);
		Fquaternion step;
		step.rotation(axis, deg2rad(angle));
		return Fquaternion().mul(step, rotation);
	}
}
inline float hud_smooth(float t)
{
	return t * t * (3.f - 2.f * t);
}

inline bool hud_motion_listed(const xr_vector<shared_str>& list, const shared_str& motion)
{
	for (const shared_str& name : list)
	{
		const size_t length = name.size();
		if (length > 0 && name.c_str()[length - 1] == '*' ? strncmp(motion.c_str(), name.c_str(), length - 1) == 0 : motion == name)
		{
			return true;
		}
	}

	return false;
}

inline Fmatrix scale_layer_offset(const Fmatrix& offset, float factor)
{
	Fvector rotation;
	offset.getXYZ(rotation);
	rotation.mul(factor);

	Fmatrix result;
	result.setXYZ(rotation);
	result.translate_over(Fvector().mul(offset.c, factor));
	return result;
}
