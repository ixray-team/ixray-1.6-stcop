#pragma once

#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/animation_motion.h"

enum class EHandIKSource : u8
{
	eItem0 = 0,
	eItem1,
	eSelf,
	eCaptured,
	eCount
};

class CIKHand
{
public:
	static constexpr u8 k_bone_count = 3;

	shared_str m_bone_names[k_bone_count];
	u16 m_bones[k_bone_count] = { BI_NONE, BI_NONE, BI_NONE };

	bool m_enabled = false;
	bool m_debug_draw = false;
	EHandIKSource m_target_source = EHandIKSource::eItem0;
	shared_str m_target_bone_name;
	Fvector m_target_offset = { 0.0f, 0.0f, 0.0f };
	bool m_shoulder_shift = false;
	float m_shoulder_shift_max = 0.15f;
	float m_weight = 1.0f;
	float m_smoothing = 15.0f;
	float m_pole_angle = 0.0f;
	s32 m_iterations = 16;
	float m_tolerance = 0.001f;

	Fvector m_captured_target = { 0.0f, 0.0f, 0.0f };
	Fvector m_target_manual = { 0.0f, 0.0f, 0.0f };
	Fvector m_hand_rot = { 0.0f, 0.0f, 0.0f };

	bool m_anim_filter = false;
	shared_str m_anim_name;
	MotionID m_anim_id = {};
	float m_anim_from = 0.0f;
	float m_anim_to = 1.0f;

	// runtime
	u16 m_target_bone_id = BI_NONE;
	Fvector m_target = { 0.0f, 0.0f, 0.0f };
	Fvector m_target_smoothed = { 0.0f, 0.0f, 0.0f };
	Fvector m_animated_end = { 0.0f, 0.0f, 0.0f };
	bool m_smoothed_valid = false;
	bool m_animated_end_valid = false;
	bool m_target_initialized = false;

	void Reset();
	void ResolveBones(IKinematics* K, const char* upper, const char* fore, const char* hand);
	bool IsConfigured() const;
	bool HasHandRotation() const { return !fis_zero(m_hand_rot.x) || !fis_zero(m_hand_rot.y) || !fis_zero(m_hand_rot.z); }
	void Solve(IKinematics* K, const Fvector& target_skel);
	void ApplyHandRotation(IKinematics* K);
};
