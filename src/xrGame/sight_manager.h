////////////////////////////////////////////////////////////////////////////
//	Module 		: sight_manager.h
//	Created 	: 27.12.2003
//  Modified 	: 27.12.2003
//	Author		: Dmitriy Iassenev
//	Description : Sight manager
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "setup_manager.h"
#include "sight_control_action.h"
#include "aimers_base.h"

namespace LevelGraph
{
	class CVertex;
}

class CAI_Stalker;

class CSightManager final : public CSetupManager<CSightControlAction, CAI_Stalker, u32>
{
public:
	using inherited = CSetupManager<CSightControlAction, CAI_Stalker, u32>;

	enum aiming_type : u32
	{
		aiming_none = 0,
		aiming_weapon = 1,
		aiming_head = 2,
	};

	enum animation_frame_type : u32
	{
		animation_frame_none = 0,
		animation_frame_start = 1,
		animation_frame_end = 2,
	};

private:
	struct bone_parameters_base
	{
		Fmatrix m_rotation = Fidentity;
	};

	struct bone_parameters : bone_parameters_base
	{
		float m_factor = 0.0f;
	};

	struct parameters
	{
		bone_parameters m_spine;
		bone_parameters m_shoulder;
		bone_parameters m_head;
	};

	struct parameters_base
	{
		bone_parameters_base m_spine;
		bone_parameters_base m_shoulder;
		bone_parameters_base m_head;
	};

	parameters m_current;
	parameters_base m_target;
	shared_str m_animation_id;
	aiming_type m_aiming_type = aiming_none;
	animation_frame_type m_animation_frame = animation_frame_none;

	float m_max_left_angle = 0.0f;
	float m_max_right_angle = 0.0f;
	bool m_enabled = true;
	bool m_turning_in_place = false;

	aimers::pose_cache m_weapon_pose;
	aimers::pose_cache m_head_pose;

public:
	CSightManager(CAI_Stalker* object) : inherited(object) {}
	~CSightManager() override = default;

	void Load(const char* section);
	void reinit() override;
	void update() override;
	void remove_links(CObject* object);

	void Exec_Look(float dt);
	void SetPointLookAngles(const Fvector& tPosition, float& yaw, float& pitch, Fvector const& look_position, const CGameObject* object = nullptr);
	void SetFirePointLookAngles(const Fvector& tPosition, float& yaw, float& pitch, Fvector const& look_position, const CGameObject* object = nullptr);
	void SetDirectionLook();
	void SetLessCoverLook(const LevelGraph::CVertex* tpNode, bool bDifferenceLook);
	void SetLessCoverLook(const LevelGraph::CVertex* tpNode, float fMaxHeadTurnAngle, bool bDifferenceLook);
	void vfValidateAngleDependency(float x1, float& x2, float x3);
	bool GetDirectionAnglesByPrevPositions(float& yaw, float& pitch);
	bool GetDirectionAngles(float& yaw, float& pitch);

	template <typename... Args>
	void setup(Args... args) { setup(CSightAction(args...)); }
	void setup(const CSightAction& sight_action);

	bool use_torso_look() const { return m_actions.empty() || current_action().use_torso_look(); }
	bool turning_in_place() const { return m_turning_in_place; }
	bool enabled() const { return m_enabled; }
	void enable(bool value);

	Fvector aiming_position() const;
	Fvector object_position() const;

	Fmatrix const& current_spine_rotation() const { return m_current.m_spine.m_rotation; }
	Fmatrix const& current_shoulder_rotation() const { return m_current.m_shoulder.m_rotation; }
	Fmatrix const& current_head_rotation() const { return m_current.m_head.m_rotation; }

	void bone_aiming()
	{
		m_animation_id = nullptr;
		m_aiming_type = aiming_none;
	}

	void bone_aiming(shared_str const& animation_id, animation_frame_type const animation_frame, aiming_type const aiming_type)
	{
		VERIFY(aiming_none != aiming_type);

		m_animation_id = animation_id;
		m_animation_frame = animation_frame;
		m_aiming_type = aiming_type;
	}

private:
	bool aim_target(Fvector& my_position, Fvector& aim_target, const CGameObject* object) const;
	void process_action(float const time_delta);
	void adjust_orientation();
	void slerp_rotations(float time_delta, float angular_speed);
	void compute_aiming(float time_delta, float angular_speed);
};
