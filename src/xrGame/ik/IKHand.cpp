#include "StdAfx.h"
#include "IKHand.h"

#include "../../xrEngine/bone.h"

ICF u16 hand_ik_find_bone(IKinematics* K, const char* name)
{
	if (!K || !name || !name[0])
	{
		return BI_NONE;
	}

	u16 id = K->LL_BoneID(name);
	if (id != BI_NONE)
	{
		return id;
	}

	string64 prefixed;
	xr_sprintf(prefixed, sizeof(prefixed), "bip01_%s", name);
	return K->LL_BoneID(prefixed);
}

ICF void hand_ik_rotation_between(Fmatrix& r, const Fvector& from, const Fvector& to, float weight)
{
	Fvector f = from;
	Fvector t = to;
	f.normalize_safe();
	t.normalize_safe();

	float dot = f.dotproduct(t);
	clamp(dot, -1.0f, 1.0f);

	if (dot > 1.0f - EPS || fis_zero(weight))
	{
		r.identity();
		return;
	}

	Fvector axis;
	float angle = 0.0f;

	if (dot < -1.0f + EPS)
	{
		Fvector up;
		up.set(0.0f, 1.0f, 0.0f);
		axis.crossproduct(f, up);
		if (axis.square_magnitude() < EPS)
		{
			up.set(0.0f, 0.0f, 1.0f);
			axis.crossproduct(f, up);
		}
		axis.normalize_safe();
		angle = PI;
	}
	else
	{
		axis.crossproduct(f, t);
		axis.normalize_safe();
		angle = acosf(dot);
	}

	r.rotation(axis, angle * weight);
}

struct SHandIKSavedCallback
{
	SHandIKSavedCallback(CBoneInstance& bi) :
		m_bi(bi),
		m_callback(bi.callback()),
		m_param(bi.callback_param()),
		m_overwrite(bi.callback_overwrite()),
		m_type(bi.callback_type())
	{
	}

	~SHandIKSavedCallback()
	{
		m_bi.set_callback(m_type, m_callback, m_param, m_overwrite);
	}

	SHandIKSavedCallback(const SHandIKSavedCallback&) = delete;
	SHandIKSavedCallback& operator=(const SHandIKSavedCallback&) = delete;

	CBoneInstance& m_bi;
	BoneCallback m_callback;
	void* m_param;
	bool m_overwrite;
	u32 m_type;
};

static void hand_ik_apply_callback(CBoneInstance* B)
{
	B->mTransform.set(*(const Fmatrix*)B->callback_param());
}

void CIKHand::Reset()
{
	for (u8 i = 0; i < k_bone_count; ++i)
	{
		m_bones[i] = BI_NONE;
		m_bone_names[i] = "";
	}

	m_target_bone_id = BI_NONE;
	m_captured_target.set(0.0f, 0.0f, 0.0f);
	m_target_manual.set(0.0f, 0.0f, 0.0f);
	m_hand_rot.set(0.0f, 0.0f, 0.0f);
	m_anim_filter = false;
	m_anim_name = "";
	m_anim_id.invalidate();
	m_anim_from = 0.0f;
	m_anim_to = 1.0f;
	m_animated_end.set(0.0f, 0.0f, 0.0f);
	m_smoothed_valid = false;
	m_animated_end_valid = false;
	m_target_initialized = false;
}

void CIKHand::ResolveBones(IKinematics* K, const char* upper, const char* fore, const char* hand)
{
	const char* names[k_bone_count] = { upper, fore, hand };

	for (u8 i = 0; i < k_bone_count; ++i)
	{
		m_bone_names[i] = names[i] ? names[i] : "";
		m_bones[i] = hand_ik_find_bone(K, names[i]);
	}
}

bool CIKHand::IsConfigured() const
{
	if (!m_enabled || m_weight <= 0.0f)
	{
		return false;
	}

	for (u8 i = 0; i < k_bone_count; ++i)
	{
		if (m_bones[i] == BI_NONE)
		{
			return false;
		}
	}

	return true;
}

void CIKHand::Solve(IKinematics* K, const Fvector& target_skel)
{
	if (!K || !IsConfigured())
	{
		m_smoothed_valid = false;
		return;
	}

	m_target.set(target_skel);

	const bool smooth_target = (m_target_source == EHandIKSource::eCaptured);

	if (smooth_target && m_smoothing > 0.0f && m_smoothed_valid)
	{
		float t = 1.0f - expf(-m_smoothing * Device.fTimeDelta);
		clamp(t, 0.0f, 1.0f);
		m_target_smoothed.lerp(m_target_smoothed, target_skel, t);
	}
	else
	{
		m_target_smoothed.set(target_skel);
		m_smoothed_valid = true;
	}

	Fmatrix orig[k_bone_count];
	Fvector origP[k_bone_count];
	for (u8 i = 0; i < k_bone_count; ++i)
	{
		orig[i].set(K->LL_GetTransform(m_bones[i]));
		origP[i].set(orig[i].c);
	}

	const float len_up = origP[0].distance_to(origP[1]);
	const float len_fore = origP[1].distance_to(origP[2]);
	if (len_up < EPS || len_fore < EPS)
	{
		return;
	}

	Fvector points[k_bone_count];
	for (u8 i = 0; i < k_bone_count; ++i)
	{
		points[i].set(origP[i]);
	}

	Fvector root = origP[0];
	const float max_reach = len_up + len_fore;

	if (m_shoulder_shift && root.distance_to(m_target_smoothed) > max_reach)
	{
		Fvector dir;
		dir.sub(m_target_smoothed, root);
		dir.normalize_safe();

		float shift = root.distance_to(m_target_smoothed) - max_reach;
		clamp(shift, 0.0f, m_shoulder_shift_max);
		shift *= m_weight;

		root.mad(dir, shift);
	}

	points[0].set(root);

	Fvector solve_target = m_target_smoothed;
	const float max_reach_safe = max_reach * 0.9995f;

	if (root.distance_to(solve_target) > max_reach)
	{
		Fvector dir;
		dir.sub(solve_target, root);
		dir.normalize_safe();
		solve_target.mad(root, dir, max_reach_safe);
	}

	{
		const s32 iterations = std::max(m_iterations, 1);
		for (s32 iter = 0; iter < iterations; ++iter)
		{
			points[2].set(solve_target);

			{
				Fvector dir;
				dir.sub(points[1], points[2]);
				dir.normalize_safe();
				points[1].mad(points[2], dir, len_fore);
			}

			{
				Fvector dir;
				dir.sub(points[0], points[1]);
				dir.normalize_safe();
				points[0].mad(points[1], dir, len_up);
			}

			points[0].set(root);

			{
				Fvector dir;
				dir.sub(points[1], points[0]);
				dir.normalize_safe();
				points[1].mad(points[0], dir, len_up);
			}

			{
				Fvector dir;
				dir.sub(points[2], points[1]);
				dir.normalize_safe();
				points[2].mad(points[1], dir, len_fore);
			}

			if (points[2].distance_to(solve_target) <= m_tolerance)
			{
				break;
			}
		}

		if (!fis_zero(m_pole_angle))
		{
			Fvector axis;
			axis.sub(points[2], points[0]);
			axis.normalize_safe();

			if (axis.square_magnitude() > EPS)
			{
				Fmatrix rot;
				rot.rotation(axis, m_pole_angle * PI / 180.0f);

				Fvector offset;
				offset.sub(points[1], points[0]);
				rot.transform_dir(offset);
				points[1].add(points[0], offset);
			}
		}
	}

	Fmatrix xforms[k_bone_count];
	Fmatrix racc;
	racc.identity();

	for (u8 i = 0; i < k_bone_count; ++i)
	{
		if (i + 1 < k_bone_count)
		{
			Fvector cur;
			cur.sub(origP[i + 1], origP[i]);
			cur.normalize_safe();

			Fvector cur_rot;
			racc.transform_dir(cur_rot, cur);

			Fvector desired;
			desired.sub(points[i + 1], points[i]);
			desired.normalize_safe();

			Fmatrix qi;
			hand_ik_rotation_between(qi, cur_rot, desired, m_weight);

			Fmatrix rn;
			rn.mul(qi, racc);
			racc = rn;
		}

		Fmatrix rotated;
		rotated.mul(racc, orig[i]);

		Fvector pos;
		if (i == 0)
		{
			pos.set(root);
		}
		else
		{
			Fmatrix inv_parent;
			inv_parent.invert(orig[i - 1]);

			Fmatrix local;
			local.mul(inv_parent, orig[i]);

			Fmatrix child;
			child.mul(xforms[i - 1], local);
			pos.set(child.c);
		}

		xforms[i].set(rotated);
		xforms[i].c.set(pos);

		if (i + 1 == k_bone_count && HasHandRotation())
		{
			Fmatrix hand_rot;
			hand_rot.setHPB(m_hand_rot.x * PI / 180.0f, m_hand_rot.y * PI / 180.0f, m_hand_rot.z * PI / 180.0f);
			xforms[i].mulB_43(hand_rot);
		}
	}

	SHandIKSavedCallback saved0(K->LL_GetBoneInstance(m_bones[0]));
	SHandIKSavedCallback saved1(K->LL_GetBoneInstance(m_bones[1]));
	SHandIKSavedCallback saved2(K->LL_GetBoneInstance(m_bones[2]));

	K->LL_GetBoneInstance(m_bones[0]).set_callback(bctCustom, hand_ik_apply_callback, &xforms[0], true);
	K->LL_GetBoneInstance(m_bones[1]).set_callback(bctCustom, hand_ik_apply_callback, &xforms[1], true);
	K->LL_GetBoneInstance(m_bones[2]).set_callback(bctCustom, hand_ik_apply_callback, &xforms[2], true);

	CBoneData& start_bone = K->LL_GetData(m_bones[0]);
	const u16 parent_id = start_bone.GetParentID();

	if (parent_id != BI_NONE)
	{
		K->Bone_Calculate(&start_bone, &K->LL_GetTransform(parent_id));
	}
}

void CIKHand::ApplyHandRotation(IKinematics* K)
{
	if (!K || m_bones[k_bone_count - 1] == BI_NONE || !HasHandRotation())
	{
		return;
	}

	Fmatrix hand_rot;
	hand_rot.setHPB(m_hand_rot.x * PI / 180.0f, m_hand_rot.y * PI / 180.0f, m_hand_rot.z * PI / 180.0f);

	Fmatrix xform;
	xform.set(K->LL_GetTransform(m_bones[k_bone_count - 1]));
	xform.mulB_43(hand_rot);

	SHandIKSavedCallback saved(K->LL_GetBoneInstance(m_bones[k_bone_count - 1]));
	K->LL_GetBoneInstance(m_bones[k_bone_count - 1]).set_callback(bctCustom, hand_ik_apply_callback, &xform, true);

	CBoneData& hand_bone = K->LL_GetData(m_bones[k_bone_count - 1]);
	const u16 parent_id = hand_bone.GetParentID();

	if (parent_id != BI_NONE)
	{
		K->Bone_Calculate(&hand_bone, &K->LL_GetTransform(parent_id));
	}
}
