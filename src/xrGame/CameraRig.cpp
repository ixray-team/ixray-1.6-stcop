#include "StdAfx.h"
#include "CameraRig.h"
#include "Actor.h"
#include "Weapon.h"
#include "Inventory.h"
#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"
#include "Level.h"
#include "player_hud.h"
#include "../xrEngine/CameraBase.h"
#include "../xrEngine/ObjectAnimator.h"
#include "../Include/xrRender/Kinematics.h"
#include "PlayerHudUtils.h"
#include "HudItem.h"
#include "ActorEffector.h"
#include "InertionData.h"
#include "WeaponBinoculars.h"
#include "WeaponPistol.h"
#include "../xrEngine/motion.h"

static SCameraRigParams	g_rig;
static int				g_rig_enabled = -1;
int						g_camera_rig = -1;

static const SCameraRigParam g_rig_params[] =
{
	{ "Arms and IK", "hud_ik_share", nullptr, &g_rig.hud_ik_share, nullptr, 1, false, 0.01f },
	{ "Arms and IK", "left_hand_ik_follow", nullptr, &g_rig.left_hand_ik_follow, nullptr, 1, false, 0.005f },
	{ "Arms and IK", "left_hand_ik_blend", "left_hand_ik_blend", &g_rig.left_hand_ik_blend, nullptr, 1, false, 0.005f },
	{ "Arms and IK", "left_hand_finger_grip", "left_hand_finger_grip", &g_rig.left_hand_finger_grip, nullptr, 1, false, 0.01f },
	{ "Arms and IK", "left_hand_attach_blend", "left_hand_attach_blend", &g_rig.left_hand_attach_blend, nullptr, 1, false, 0.005f },
	{ "Arms and IK", "aim_left_elbow_offset", "aim_left_elbow_offset", &g_rig.aim_left_elbow_offset.x, nullptr, 3, false, 0.001f },

	{ "Inertia and sway", "turn_sway_smooth", nullptr, &g_rig.turn_sway_smooth, nullptr, 1, false, 0.002f },
	{ "Inertia and sway", "inertia_speed", nullptr, &g_rig.inertia_speed, nullptr, 1, false, 0.01f },
	{ "Inertia and sway", "inertia_damping", "inertia_damping", &g_rig.inertia_damping, nullptr, 1, false, 0.01f },
	{ "Inertia and sway", "look_sway_pos", "look_sway_pos", &g_rig.look_sway_pos.x, nullptr, 3, false, 0.0005f },
	{ "Inertia and sway", "aim_sway_scale", "aim_sway_scale", &g_rig.aim_sway_scale, nullptr, 1, false, 0.01f },
	{ "Inertia and sway", "look_sway_inertia", "look_sway_inertia", &g_rig.look_sway_inertia, nullptr, 1, false, 0.005f },
	{ "Inertia and sway", "look_sway_damping", "look_sway_damping", &g_rig.look_sway_damping, nullptr, 1, false, 0.01f },

	{ "Animation transitions", "hud_inertialization", nullptr, &g_rig.hud_inertialization, nullptr, 1, false, 0.005f },
	{ "Animation transitions", "idle_fade", "idle_fade", &g_rig.idle_fade, nullptr, 1, false, 0.005f },
	{ "Animation transitions", "additive_blend_in", "additive_blend_in", &g_rig.additive_blend_in, nullptr, 1, false, 0.005f },
	{ "Animation transitions", "additive_blend_out", "additive_blend_out", &g_rig.additive_blend_out, nullptr, 1, false, 0.005f },
	{ "Animation transitions", "aim_blend_inertia", "aim_blend_inertia", &g_rig.aim_blend_inertia, nullptr, 1, false, 0.005f },

	{ "Sprint and safemode", "sprint_pose_inertia", "sprint_pose_inertia", &g_rig.sprint_pose_inertia, nullptr, 1, false, 0.005f },
	{ "Sprint and safemode", "sprint_blend_in", "sprint_blend_in", &g_rig.sprint_blend_in, nullptr, 1, false, 0.005f },
	{ "Sprint and safemode", "sprint_left_grip_range", "sprint_left_grip_range", &g_rig.sprint_left_grip_range, nullptr, 1, false, 0.01f },
	{ "Sprint and safemode", "sprint_left_cancel_return", "sprint_left_cancel_return", &g_rig.sprint_left_cancel_return, nullptr, 1, false, 0.005f },
	{ "Sprint and safemode", "safemode_damping", "safemode_damping", &g_rig.safemode_damping, nullptr, 1, false, 0.01f },

	{ "Aim reload", "aim_reload_motion", "aim_reload_motion", &g_rig.aim_reload_motion, nullptr, 1, false, 0.01f },
	{ "Aim reload", "aim_reload_blend", nullptr, &g_rig.aim_reload_blend, nullptr, 1, false, 0.005f },
	{ "Aim reload", "aim_reload_tail", nullptr, &g_rig.aim_reload_tail, nullptr, 1, false, 0.005f },
	{ "Aim reload", "aim_reload_hud_offset_pos", "aim_reload_hud_offset_pos", &g_rig.aim_reload_hud_offset_pos.x, nullptr, 3, false, 0.0005f },
	{ "Aim reload", "aim_reload_hud_offset_rot", "aim_reload_hud_offset_rot", &g_rig.aim_reload_hud_offset_rot.x, nullptr, 3, false, 0.05f },
	{ "Aim reload", "aim_reload_zoom_out", "aim_reload_zoom_out", &g_rig.aim_reload_zoom_out, nullptr, 1, false, 0.01f },
	{ "Aim reload", "aim_reload_pose_inertia", "aim_reload_pose_inertia", &g_rig.aim_reload_pose_inertia, nullptr, 1, false, 0.005f },
	{ "Aim reload", "aim_reload_pose_inertia_out", "aim_reload_pose_inertia_out", &g_rig.aim_reload_pose_inertia_out, nullptr, 1, false, 0.005f },
	{ "Aim reload", "aim_reload_hud_push_pos", "aim_reload_hud_push_pos", &g_rig.aim_reload_hud_push_pos.x, nullptr, 3, false, 0.0005f },

	{ "Hand sway", "left_hand_sway_look", "left_hand_sway_look", &g_rig.left_hand_sway_look.x, nullptr, 3, false, 0.05f },
	{ "Hand sway", "left_hand_sway_bob", "left_hand_sway_bob", &g_rig.left_hand_sway_bob.x, nullptr, 3, false, 0.05f },
	{ "Hand sway", "left_hand_sway_jump", "left_hand_sway_jump", &g_rig.left_hand_sway_jump.x, nullptr, 3, false, 0.1f },
	{ "Hand sway", "right_hand_sway_look", "right_hand_sway_look", &g_rig.right_hand_sway_look.x, nullptr, 3, false, 0.05f },
	{ "Hand sway", "right_hand_sway_jump", "right_hand_sway_jump", &g_rig.right_hand_sway_jump.x, nullptr, 3, false, 0.1f },
	{ "Hand sway", "left_hand_follow_lag", "left_hand_follow_lag", &g_rig.left_hand_follow_lag, nullptr, 1, false, 0.005f },
	{ "Hand sway", "left_hand_follow_turn", "left_hand_follow_turn", &g_rig.left_hand_follow_turn, nullptr, 1, false, 0.01f },
	{ "Hand sway", "left_hand_follow_move", "left_hand_follow_move", &g_rig.left_hand_follow_move.x, nullptr, 3, false, 0.01f },
	{ "Hand sway", "left_hand_follow_move_rot", "left_hand_follow_move_rot", &g_rig.left_hand_follow_move_rot, nullptr, 1, false, 0.01f },
	{ "Hand sway", "left_hand_bob_pos", "left_hand_bob_pos", &g_rig.left_hand_bob_pos.x, nullptr, 3, false, 0.01f },
	{ "Hand sway", "left_hand_bob_rot", "left_hand_bob_rot", &g_rig.left_hand_bob_rot.x, nullptr, 3, false, 0.01f },
	{ "Hand sway", "hand_sway_inertia", "hand_sway_inertia", &g_rig.hand_sway_inertia, nullptr, 1, false, 0.005f },
	{ "Hand sway", "hand_sway_damping", "hand_sway_damping", &g_rig.hand_sway_damping, nullptr, 1, false, 0.01f },

	{ "Weapon turn", "turn_pivot_look", "turn_pivot_look", &g_rig.turn_pivot_look.x, nullptr, 3, false, 0.05f },
	{ "Weapon turn", "turn_pivot_max", "turn_pivot_max", &g_rig.turn_pivot_max, nullptr, 1, false, 0.05f },
	{ "Weapon turn", "turn_pivot_inertia", "turn_pivot_inertia", &g_rig.turn_pivot_inertia, nullptr, 1, false, 0.005f },
	{ "Weapon turn", "turn_pivot_damping", "turn_pivot_damping", &g_rig.turn_pivot_damping, nullptr, 1, false, 0.01f },
	{ "Weapon turn", "aim_turn_pivot_scale", "aim_turn_pivot_scale", &g_rig.aim_turn_pivot_scale, nullptr, 1, false, 0.01f },
	{ "Weapon turn", "turn_pivot_grip_lag", "turn_pivot_grip_lag", &g_rig.turn_pivot_grip_lag, nullptr, 1, false, 0.01f },
	{ "Weapon turn", "turn_pivot_lift", "turn_pivot_lift", &g_rig.turn_pivot_lift, nullptr, 1, false, 0.05f },
	{ "Weapon turn", "turn_pivot_lift_pos", "turn_pivot_lift_pos", &g_rig.turn_pivot_lift_pos, nullptr, 1, false, 0.0005f },
	{ "Weapon turn", "turn_pivot_lift_max", "turn_pivot_lift_max", &g_rig.turn_pivot_lift_max, nullptr, 1, false, 0.0005f },
	{ "Weapon turn", "turn_pivot_lift_inertia", "turn_pivot_lift_inertia", &g_rig.turn_pivot_lift_inertia, nullptr, 1, false, 0.005f },
	{ "Weapon turn", "turn_pivot_lift_fall", "turn_pivot_lift_fall", &g_rig.turn_pivot_lift_fall, nullptr, 1, false, 0.05f },
	{ "Weapon turn", "turn_tilt", "turn_tilt", &g_rig.turn_tilt, nullptr, 1, false, 0.05f },
	{ "Weapon turn", "turn_tilt_max", "turn_tilt_max", &g_rig.turn_tilt_max, nullptr, 1, false, 0.05f },

	{ "Shot fingers", "shot_fingers_chance", nullptr, &g_rig.shot_fingers_chance, nullptr, 1, false, 0.01f },
	{ "Shot fingers", "shot_fingers_delay", nullptr, &g_rig.shot_fingers_delay, nullptr, 1, false, 0.005f },
	{ "Shot fingers", "shot_fingers_blend", nullptr, &g_rig.shot_fingers_blend, nullptr, 1, false, 0.005f },
	{ "Shot fingers", "shot_fingers_blend_out", nullptr, &g_rig.shot_fingers_blend_out, nullptr, 1, false, 0.005f },
	{ "Shot fingers", "shot_fingers_burst_chance", nullptr, &g_rig.shot_fingers_burst_chance, nullptr, 1, false, 0.005f },
	{ "Shot fingers", "shot_fingers_max_chance", nullptr, &g_rig.shot_fingers_max_chance, nullptr, 1, false, 0.01f },
	{ "Shot fingers", "shot_fingers_full_chance", nullptr, &g_rig.shot_fingers_full_chance, nullptr, 1, false, 0.01f },

	{ "Carry pose", "crouch_hud_offset_pos", "crouch_hud_offset_pos", &g_rig.crouch_hud_offset_pos.x, nullptr, 3, false, 0.0005f },
	{ "Carry pose", "crouch_hud_offset_rot", "crouch_hud_offset_rot", &g_rig.crouch_hud_offset_rot.x, nullptr, 3, false, 0.05f },
	{ "Carry pose", "crouch_pose_inertia", "crouch_pose_inertia", &g_rig.crouch_pose_inertia, nullptr, 1, false, 0.005f },
	{ "Carry pose", "crouch_pose_inertia_out", "crouch_pose_inertia_out", &g_rig.crouch_pose_inertia_out, nullptr, 1, false, 0.005f },
	{ "Carry pose", "crouch_pose_delay", "crouch_pose_delay", &g_rig.crouch_pose_delay, nullptr, 1, false, 0.005f },
	{ "Carry pose", "walk_hud_offset_pos", "walk_hud_offset_pos", &g_rig.walk_hud_offset_pos.x, nullptr, 3, false, 0.0005f },
	{ "Carry pose", "walk_hud_offset_rot", "walk_hud_offset_rot", &g_rig.walk_hud_offset_rot.x, nullptr, 3, false, 0.05f },
	{ "Carry pose", "walk_pose_delay", "walk_pose_delay", &g_rig.walk_pose_delay, nullptr, 1, false, 0.05f },
	{ "Carry pose", "walk_pose_inertia", "walk_pose_inertia", &g_rig.walk_pose_inertia, nullptr, 1, false, 0.005f },
	{ "Carry pose", "walk_pose_inertia_out", "walk_pose_inertia_out", &g_rig.walk_pose_inertia_out, nullptr, 1, false, 0.005f },
	{ "Carry pose", "carry_pose_damping", "carry_pose_damping", &g_rig.carry_pose_damping, nullptr, 1, false, 0.01f },

	{ "Shot animation", "shot_anim_variation", "shot_anim_variation", &g_rig.shot_anim_variation, nullptr, 1, false, 0.01f },
	{ "Shot animation", "shot_anim_twist", "shot_anim_twist", &g_rig.shot_anim_twist, nullptr, 1, false, 0.01f },
	{ "Shot animation", "shot_anim_blend", nullptr, &g_rig.shot_anim_blend, nullptr, 1, false, 0.005f },
	{ "Shot animation", "shot_anim_tail", nullptr, &g_rig.shot_anim_tail, nullptr, 1, false, 0.005f },
	{ "Shot animation", "aim_shot_motion", "aim_shot_motion", &g_rig.aim_shot_motion, nullptr, 1, false, 0.01f },

	{ "Shot camera", "cam_shoot_variation", "cam_shoot_variation", &g_rig.cam_shoot_variation, nullptr, 1, false, 0.01f },
	{ "Shot camera", "cam_shoot_mirror", "cam_shoot_mirror", nullptr, &g_rig.cam_shoot_mirror, 0, false, 0.f },
	{ "Shot camera", "cam_shoot_speed_variation", "cam_shoot_speed_variation", &g_rig.cam_shoot_speed_variation, nullptr, 1, false, 0.005f },
	{ "Shot camera", "shot_fov_kick", "rig_fov_kick", &g_rig.shot_fov_kick, nullptr, 1, false, 0.05f },
	{ "Shot camera", "aim_fov_kick_scale", nullptr, &g_rig.aim_fov_kick_scale, nullptr, 1, false, 0.01f },
	{ "Shot camera", "fov_stiffness", nullptr, &g_rig.fov_stiffness, nullptr, 1, false, 1.f },
	{ "Shot camera", "fov_damping", nullptr, &g_rig.fov_damping, nullptr, 1, false, 0.1f },

	{ "Camera layers", "motion_blend_in", nullptr, &g_rig.blend_in[crcMotion], nullptr, 1, false, 0.005f },
	{ "Camera layers", "motion_blend_out", nullptr, &g_rig.blend_out[crcMotion], nullptr, 1, false, 0.005f },
	{ "Camera layers", "action_blend_in", nullptr, &g_rig.blend_in[crcAction], nullptr, 1, false, 0.005f },
	{ "Camera layers", "action_blend_out", nullptr, &g_rig.blend_out[crcAction], nullptr, 1, false, 0.005f },
	{ "Camera layers", "shot_blend_in", nullptr, &g_rig.blend_in[crcShot], nullptr, 1, false, 0.005f },
	{ "Camera layers", "shot_blend_out", nullptr, &g_rig.blend_out[crcShot], nullptr, 1, false, 0.005f },
	{ "Camera layers", "move_blend_in", nullptr, &g_rig.blend_in[crcMove], nullptr, 1, false, 0.005f },
	{ "Camera layers", "move_blend_out", nullptr, &g_rig.blend_out[crcMove], nullptr, 1, false, 0.005f },
	{ "Camera layers", "sprint_blend_in", nullptr, &g_rig.blend_in[crcSprint], nullptr, 1, false, 0.005f },
	{ "Camera layers", "sprint_blend_out", nullptr, &g_rig.blend_out[crcSprint], nullptr, 1, false, 0.005f },

	{ "Punch", "punch", "rig_punch", &g_rig.punch.x, nullptr, 3, true, 0.01f },
	{ "Punch", "punch_stiffness", nullptr, &g_rig.punch_stiffness, nullptr, 1, false, 1.f },
	{ "Punch", "punch_damping", nullptr, &g_rig.punch_damping, nullptr, 1, false, 0.1f },
	{ "Punch", "punch_hud_scale", nullptr, &g_rig.punch_hud_scale, nullptr, 1, false, 0.01f },
	{ "Punch", "aim_punch_scale", nullptr, &g_rig.aim_punch_scale, nullptr, 1, false, 0.01f },

	{ "Kick", "kick_pos", "rig_kick_pos", &g_rig.kick_pos.x, nullptr, 3, false, 0.0005f },
	{ "Kick", "kick_rot", "rig_kick_rot", &g_rig.kick_rot.x, nullptr, 3, true, 0.01f },
	{ "Kick", "kick_stiffness", nullptr, &g_rig.kick_stiffness, nullptr, 1, false, 1.f },
	{ "Kick", "kick_damping", nullptr, &g_rig.kick_damping, nullptr, 1, false, 0.1f },
	{ "Kick", "aim_kick_scale", nullptr, &g_rig.aim_kick_scale, nullptr, 1, false, 0.01f },

	{ "Shake", "shot_trauma", "rig_shot_trauma", &g_rig.shot_trauma, nullptr, 1, false, 0.005f },
	{ "Shake", "hit_trauma", nullptr, &g_rig.hit_trauma, nullptr, 1, false, 0.01f },
	{ "Shake", "explosion_trauma", nullptr, &g_rig.explosion_trauma, nullptr, 1, false, 0.01f },
	{ "Shake", "trauma_decay", nullptr, &g_rig.trauma_decay, nullptr, 1, false, 0.01f },
	{ "Shake", "shake_frequency", nullptr, &g_rig.shake_frequency, nullptr, 1, false, 0.1f },
	{ "Shake", "shake_hud_scale", nullptr, &g_rig.shake_hud_scale, nullptr, 1, false, 0.01f },
	{ "Shake", "shake_rot", nullptr, &g_rig.shake_rot.x, nullptr, 3, true, 0.01f },
	{ "Shake", "shake_pos", nullptr, &g_rig.shake_pos.x, nullptr, 3, false, 0.0005f },

	{ "Landing", "land_min_speed", nullptr, &g_rig.land_min_speed, nullptr, 1, false, 0.05f },
	{ "Landing", "land_max_speed", nullptr, &g_rig.land_max_speed, nullptr, 1, false, 0.05f },
	{ "Landing", "land_pitch", nullptr, &g_rig.land_pitch, nullptr, 1, true, 0.01f },
	{ "Landing", "land_roll", nullptr, &g_rig.land_roll, nullptr, 1, true, 0.01f },
	{ "Landing", "land_height", nullptr, &g_rig.land_height, nullptr, 1, false, 0.001f },
	{ "Landing", "land_trauma", nullptr, &g_rig.land_trauma, nullptr, 1, false, 0.01f },
	{ "Landing", "land_hud_scale", nullptr, &g_rig.land_hud_scale, nullptr, 1, false, 0.01f },
	{ "Landing", "land_stiffness", nullptr, &g_rig.land_stiffness, nullptr, 1, false, 1.f },
	{ "Landing", "land_damping", nullptr, &g_rig.land_damping, nullptr, 1, false, 0.1f },

	{ "Roll", "strafe_roll", nullptr, &g_rig.strafe_roll, nullptr, 1, true, 0.005f },
	{ "Roll", "look_roll", nullptr, &g_rig.look_roll, nullptr, 1, true, 0.005f },
	{ "Roll", "roll_max", nullptr, &g_rig.roll_max, nullptr, 1, true, 0.01f },
	{ "Roll", "aim_roll_scale", nullptr, &g_rig.aim_roll_scale, nullptr, 1, false, 0.01f },
	{ "Roll", "roll_stiffness", nullptr, &g_rig.roll_stiffness, nullptr, 1, false, 1.f },
	{ "Roll", "roll_damping", nullptr, &g_rig.roll_damping, nullptr, 1, false, 0.1f },
};

static void rig_load_param(const SCameraRigParam& it)
{
	static const SCameraRigParams defaults;
	const char* field = it.Flag != nullptr ? (const char*)it.Flag : (const char*)it.Value;
	const ptrdiff_t offset = field - (const char*)&g_rig;
	std::memcpy((char*)&g_rig + offset, (const char*)&defaults + offset, it.Flag != nullptr ? sizeof(bool) : sizeof(float) * it.Count);

	if (pSettings->line_exist("camera_rig", it.Key))
	{
		if (it.Flag != nullptr)
		{
			*it.Flag = pSettings->r_bool("camera_rig", it.Key);
		}
		else if (it.Count == 3)
		{
			*(Fvector*)it.Value = pSettings->r_fvector3("camera_rig", it.Key);
		}
		else
		{
			*it.Value = pSettings->r_float("camera_rig", it.Key);
		}
	}

	for (u32 i = 0; it.Degrees && i < it.Count; i++)
	{
		it.Value[i] = deg2rad(it.Value[i]);
	}
}

void CCameraRig::Reload()
{
	g_rig = {};
	g_rig_enabled = 0;

	if (!pSettings->section_exist("camera_rig"))
	{
		return;
	}

	for (const SCameraRigParam& it : g_rig_params)
	{
		rig_load_param(it);
	}

	g_rig_enabled = pSettings->read_if_exists<bool>("camera_rig", "enabled", true);
}

void CCameraRig::ReloadGroup(const char* group)
{
	if (!pSettings->section_exist("camera_rig"))
	{
		return;
	}

	for (const SCameraRigParam& it : g_rig_params)
	{
		if (xr_strcmp(it.Group, group) == 0)
		{
			rig_load_param(it);
		}
	}
}

bool CCameraRig::Enabled()
{
	if (g_rig_enabled < 0)
	{
		Reload();
	}

	return g_camera_rig < 0 ? !!g_rig_enabled : !!g_camera_rig;
}

const SCameraRigParams& CCameraRig::Params()
{
	Enabled();
	return g_rig;
}

const SCameraRigParam* CCameraRig::ParamList(u32& count)
{
	count = u32(std::size(g_rig_params));
	return g_rig_params;
}

const SCameraRigParam* CCameraRig::FindHudParam(const char* hud_key)
{
	for (const SCameraRigParam& it : g_rig_params)
	{
		if (it.HudKey != nullptr && xr_strcmp(it.HudKey, hud_key) == 0)
		{
			return &it;
		}
	}

	return nullptr;
}

void CCameraRig::ResetWeaponCache()
{
	WeaponSect = nullptr;
}

CCameraRig::~CCameraRig()
{
	for (SLayer& layer : Layers)
	{
		xr_delete(layer.anim);
	}
}

void CCameraRig::FadeChannel(ECameraRigChannel channel)
{
	for (SLayer& layer : Layers)
	{
		if (layer.active && layer.channel == channel)
		{
			layer.fade_out = true;
		}
	}
}

void CCameraRig::StopChannel(ECameraRigChannel channel)
{
	FadeChannel(channel);
}

void CCameraRig::SeekChannel(ECameraRigChannel channel, float progress)
{
	for (SLayer& layer : Layers)
	{
		if (layer.anim == nullptr || !layer.active || layer.fade_out || layer.channel != channel)
		{
			continue;
		}

		const SAnimParams& params = layer.anim->anim_param();
		layer.anim->SetTime(params.min_t + std::clamp(progress, 0.f, 1.f) * (params.max_t - params.min_t));
	}
}

void CCameraRig::PreviewAnim(const char* name, ECameraRigChannel channel, float time, float speed)
{
	const auto find = [&]() -> SLayer*
	{
		SLayer* found = nullptr;
		for (SLayer& it : Layers)
		{
			if (it.anim == nullptr || it.channel != channel || xr_strcmp(it.anim->Name(), name) != 0)
			{
				continue;
			}

			if (found == nullptr || it.active && (!found->active || found->fade_out))
			{
				found = &it;
			}
		}

		return found;
	};

	SLayer* layer = find();
	if (layer == nullptr)
	{
		if (!PlayAnim(name, channel, speed, false))
		{
			return;
		}

		layer = find();
		if (layer == nullptr)
		{
			return;
		}
	}

	const SAnimParams& params = layer->anim->anim_param();
	if (time >= params.max_t - EPS_L)
	{
		if (layer->active)
		{
			layer->fade_out = true;
		}

		return;
	}

	if (!layer->active)
	{
		layer->active = true;
		layer->hud_affect = false;
		layer->scale_hpb.set(1.f, 1.f, 1.f);
		layer->scale_pos.set(1.f, 1.f, 1.f);
		layer->weight = g_rig.blend_in[channel] > 0.f ? 0.f : 1.f;
	}

	layer->anim->Speed() = speed;
	layer->anim->SetTime(std::max(time, params.min_t));
	layer->fade_out = false;
}

bool CCameraRig::PlayAnim(const char* name, ECameraRigChannel channel, float speed, bool hud_affect, const Fvector& scale_hpb, const Fvector& scale_pos)
{
	string_path path;
	if (!Enabled() || name == nullptr || !FS.exist(path, "$game_anims$", name))
	{
		return false;
	}

	for (const SLayer& layer : Layers)
	{
		if (channel == crcMove && layer.active && !layer.fade_out && layer.channel == channel && xr_strcmp(layer.anim->Name(), name) == 0)
		{
			return true;
		}
	}

	if (channel != crcShot)
	{
		FadeChannel(channel);
	}

	SLayer* slot = &Layers[0];
	for (SLayer& layer : Layers)
	{
		if (!layer.active)
		{
			slot = &layer;
			break;
		}

		if (layer.weight < slot->weight)
		{
			slot = &layer;
		}
	}

	if (slot->anim == nullptr)
	{
		slot->anim = new CObjectAnimator();
	}

	slot->anim->Load(name);
	slot->anim->Play(false);
	slot->anim->Speed() = speed;
	slot->channel = channel;
	slot->hud_affect = hud_affect;
	slot->scale_hpb = scale_hpb;
	slot->scale_pos = scale_pos;
	slot->active = true;
	slot->fade_out = false;
	slot->weight = g_rig.blend_in[channel] > 0.f ? 0.f : 1.f;
	return true;
}

void CCameraRig::OnHit(float power)
{
	Trauma = std::clamp(Trauma + power * g_rig.hit_trauma, 0.f, 1.f);
}

void CCameraRig::OnExplosion(float power)
{
	Trauma = std::clamp(Trauma + power * g_rig.explosion_trauma, 0.f, 1.f);
}

void CCameraRig::OnShot(CWeapon* weapon)
{
	if (!Enabled() || weapon == nullptr)
	{
		return;
	}

	const shared_str& sect = weapon->HudSection();
	if (WeaponSect != sect)
	{
		WeaponSect = sect;
		WeaponPunch = pSettings->line_exist(sect, "rig_punch") ? Fvector(pSettings->r_fvector3(sect, "rig_punch")).mul(PI / 180.f) : g_rig.punch;
		WeaponKickRot = pSettings->line_exist(sect, "rig_kick_rot") ? Fvector(pSettings->r_fvector3(sect, "rig_kick_rot")).mul(PI / 180.f) : g_rig.kick_rot;
		WeaponKickPos = pSettings->read_if_exists<Fvector3>(sect, "rig_kick_pos", g_rig.kick_pos);
		WeaponTrauma = pSettings->read_if_exists<float>(sect, "rig_shot_trauma", g_rig.shot_trauma);
		WeaponFovKick = pSettings->read_if_exists<float>(sect, "rig_fov_kick", g_rig.shot_fov_kick);
	}

	const float aim = weapon->GetAimFactor();
	const attachable_hud_item* hud_item = weapon->HudItemData();
	const float mass = hud_item != nullptr ? hud_item->HudMass : 1.f;
	const float kick = _sqrt(g_rig.kick_stiffness) * lerp(1.f, g_rig.aim_kick_scale, aim) / _sqrt(mass);
	Punch.velocity.mad(Fvector().set(WeaponPunch.y * ::Random.randFs(1.f), WeaponPunch.x, WeaponPunch.z * ::Random.randFs(1.f)), _sqrt(g_rig.punch_stiffness) * lerp(1.f, g_rig.aim_punch_scale, aim));
	Kick.velocity.mad(Fvector().set(WeaponKickRot.y * ::Random.randFs(1.f), WeaponKickRot.x, WeaponKickRot.z * ::Random.randFs(1.f)), kick);
	KickPos.velocity.mad(WeaponKickPos, kick);
	Trauma = std::clamp(Trauma + WeaponTrauma * lerp(1.f, g_rig.aim_kick_scale, aim), 0.f, 1.f);

	if (FovValid && !fis_zero(WeaponFovKick))
	{
		const float zoom = std::min(Fov.value.x / std::max(g_fov, 1.f), 1.f);
		Fov.velocity.x += WeaponFovKick * lerp(1.f, g_rig.aim_fov_kick_scale, aim) * zoom * _sqrt(g_rig.fov_stiffness) * 2.71828f;
	}
}

void CCameraRig::OnLand(float speed)
{
	if (!Enabled())
	{
		return;
	}

	const float power = std::clamp((speed - g_rig.land_min_speed) / std::max(g_rig.land_max_speed - g_rig.land_min_speed, EPS_L), 0.f, 1.f);
	Land.velocity.mad(Fvector().set(-g_rig.land_pitch, -g_rig.land_height, g_rig.land_roll * ::Random.randFs(1.f)), _sqrt(g_rig.land_stiffness) * power);
	Trauma = std::clamp(Trauma + g_rig.land_trauma * power, 0.f, 1.f);
}

static float rig_noise(float time, float seed)
{
	const auto hash = [seed](float x)
	{
		const float v = std::sin(x * 127.1f + seed * 311.7f) * 43758.5453f;
		return (v - floorf(v)) * 2.f - 1.f;
	};

	const float base = floorf(time);
	const float frac = time - base;
	return lerp(hash(base), hash(base + 1.f), frac * frac * (3.f - 2.f * frac));
}

void CCameraRig::UpdateLayers(float dt, Fvector& pos, Fvector& hpb, Fvector& hud_pos, Fvector& hud_hpb)
{
	for (SLayer& layer : Layers)
	{
		if (!layer.active)
		{
			continue;
		}

		const SAnimParams& params = layer.anim->anim_param();
		layer.fade_out |= params.t_current >= params.max_t;

		const float blend = layer.fade_out ? g_rig.blend_out[layer.channel] : g_rig.blend_in[layer.channel];
		layer.weight = std::clamp(blend > 0.f ? layer.weight + (layer.fade_out ? -dt : dt) / blend : (layer.fade_out ? 0.f : 1.f), 0.f, 1.f);
		if (layer.fade_out && layer.weight <= 0.f)
		{
			layer.active = false;
			continue;
		}

		layer.anim->Update(dt);

		const float weight = layer.weight * layer.weight * (3.f - 2.f * layer.weight);
		Fvector anim_hpb;
		layer.anim->XFORM().getHPB(anim_hpb);
		anim_hpb.mul(layer.scale_hpb);
		const Fvector anim_pos = Fvector().mul(layer.anim->XFORM().c, layer.scale_pos);
		hpb.mad(anim_hpb, weight);
		pos.mad(anim_pos, weight);

		if (layer.hud_affect)
		{
			hud_hpb.mad(anim_hpb, weight);
			hud_pos.mad(anim_pos, weight);
		}
	}
}

void CCameraRig::Update(CActor* actor, float fov)
{
	if (!Enabled())
	{
		return;
	}

	const float dt = Device.fTimeDelta;
	if (!FovValid)
	{
		Fov.value.set(fov, 0.f, 0.f);
		FovValid = true;
	}

	PIItem active_item = actor->inventory().ActiveItem();
	CHudItem* item = active_item != nullptr ? active_item->cast_hud_item() : nullptr;
	const float aim = item != nullptr ? item->GetAimFactor() : 0.f;

	Fvector pos = {}, hpb = {}, hud_pos = {}, hud_hpb = {};
	UpdateLayers(dt, pos, hpb, hud_pos, hud_hpb);

	const Fvector zero = {};
	Fov.value.spring_inertion(Fvector().set(fov, 0.f, 0.f), Fov.velocity, dt, g_rig.fov_stiffness, g_rig.fov_damping);
	Punch.value.spring_inertion(zero, Punch.velocity, dt, g_rig.punch_stiffness, g_rig.punch_damping);
	Kick.value.spring_inertion(zero, Kick.velocity, dt, g_rig.kick_stiffness, g_rig.kick_damping);
	KickPos.value.spring_inertion(zero, KickPos.velocity, dt, g_rig.kick_stiffness, g_rig.kick_damping);
	Land.value.spring_inertion(zero, Land.velocity, dt, g_rig.land_stiffness, g_rig.land_damping);

	CCameraBase* cam = actor->cam_FirstEye();
	Fvector right;
	right.crossproduct(cam->vNormal, cam->vDirection);
	const float lateral = actor->character_physics_support()->movement()->GetVelocity().dotproduct(right);
	const float roll = std::clamp(lateral * g_rig.strafe_roll - actor->fFPCamYawMagnitude * g_rig.look_roll, -g_rig.roll_max, g_rig.roll_max) * lerp(1.f, g_rig.aim_roll_scale, aim);
	Roll.value.spring_inertion(Fvector().set(0.f, 0.f, roll), Roll.velocity, dt, g_rig.roll_stiffness, g_rig.roll_damping);

	LookRate.x = std::clamp(actor->fFPCamYawMagnitude, -PI_MUL_2, PI_MUL_2);
	LookRate.y = std::clamp(actor->fFPCamPitchMagnitude, -PI_MUL_2, PI_MUL_2);
	LookRate.z = _sqrt(LookRate.x * LookRate.x + LookRate.y * LookRate.y);
	LookAim = aim;

	Trauma = std::max(Trauma - g_rig.trauma_decay * dt, 0.f);
	const float shake = Trauma * Trauma * std::min(Fov.value.x / std::max(g_fov, 1.f), 1.f);
	const float time = Device.fTimeGlobal * g_rig.shake_frequency;
	const Fvector shake_hpb = { rig_noise(time, 1.f) * g_rig.shake_rot.y * shake, rig_noise(time, 2.f) * g_rig.shake_rot.x * shake, rig_noise(time, 3.f) * g_rig.shake_rot.z * shake };
	const Fvector shake_pos = { rig_noise(time, 4.f) * g_rig.shake_pos.x * shake, rig_noise(time, 5.f) * g_rig.shake_pos.y * shake, rig_noise(time, 6.f) * g_rig.shake_pos.z * shake };
	const Fvector land_hpb = { 0.f, Land.value.x, Land.value.z };
	const Fvector land_pos = { 0.f, Land.value.y, 0.f };

	WorldHpb.add(hpb, Punch.value).add(Roll.value).add(shake_hpb).add(land_hpb);
	WorldPos.add(pos, shake_pos).add(land_pos);
	HudCamHpb.mad(hud_hpb, Punch.value, g_rig.punch_hud_scale).add(Roll.value).mad(shake_hpb, g_rig.shake_hud_scale).add(land_hpb);
	HudCamPos.mad(hud_pos, shake_pos, g_rig.shake_hud_scale).add(land_pos);
	HudHpb.mad(Kick.value, land_hpb, g_rig.land_hud_scale - 1.f);
	HudPos.mad(KickPos.value, land_pos, g_rig.land_hud_scale - 1.f);
	UpdateFrame = Device.dwFrame;
}

static void rig_offset(SCamEffectorInfo& info, const Fvector& pos, const Fvector& hpb)
{
	Fmatrix base;
	base.identity();
	base.k = info.d;
	base.j = info.n;
	base.i.crossproduct(info.n, info.d);
	base.c = info.p;

	Fmatrix offset;
	offset.setHPB(hpb.x, hpb.y, hpb.z);
	offset.c = pos;

	Fmatrix result;
	result.mul_43(base, offset);
	info.d = result.k;
	info.n = result.j;
	info.r = result.i;
	info.p = result.c;
}

void CCameraRig::Apply(SCamEffectorInfo& world, SCamEffectorInfo& hud)
{
	if (UpdateFrame != Device.dwFrame)
	{
		return;
	}

	rig_offset(world, WorldPos, WorldHpb);
	rig_offset(hud, HudCamPos, HudCamHpb);
	world.fFov = Fov.value.x;
	hud.fFov = Fov.value.x;
}

static float rig_mass_factor(const attachable_hud_item* item)
{
	return powf(std::max(item != nullptr ? item->HudMass : 1.f, 0.1f), 0.25f);
}

void CCameraRig::UpdateLookSway(const attachable_hud_item* item)
{
	if (LookFrame == Device.dwFrame)
	{
		return;
	}

	LookFrame = Device.dwFrame;

	const Fvector& LookPos = item != nullptr ? item->LookSwayPos : g_rig.look_sway_pos;
	const float Scale = lerp(1.f, item != nullptr ? item->AimSwayScale : g_rig.aim_sway_scale, LookAim);
	const Fvector Target = Fvector().set(LookRate.x * LookPos.x, -LookRate.y * LookPos.y, -LookRate.z * LookPos.z).mul(Scale);
	SpringSway(LookSway.value, LookSway.velocity, Target, LookHalfLife(item), LookDamping(item));
}

float CCameraRig::LookHalfLife(const attachable_hud_item* item)
{
	return (item != nullptr ? item->LookSwayInertia : g_rig.look_sway_inertia) * rig_mass_factor(item);
}

float CCameraRig::LookDamping(const attachable_hud_item* item)
{
	return item != nullptr ? item->LookSwayDamping : g_rig.look_sway_damping;
}

void CCameraRig::ApplyHud(Fmatrix& trans, const attachable_hud_item* item)
{
	if (UpdateFrame != Device.dwFrame)
	{
		return;
	}

	UpdateLookSway(item);

	Fmatrix offset;
	offset.setHPB(HudHpb.x, HudHpb.y, HudHpb.z);
	offset.c.add(HudPos, LookSway.value);
	trans.mulB_43(offset);
}

float CCameraRig::HudIkShare()
{
	return Enabled() ? std::clamp(g_rig.hud_ik_share, 0.f, 1.f) : 0.f;
}

float CCameraRig::TurnSwaySmooth()
{
	return Enabled() ? g_rig.turn_sway_smooth : 0.f;
}

float CCameraRig::HudInertialization()
{
	return Enabled() ? g_rig.hud_inertialization : 0.f;
}

float CCameraRig::ShotAnimTail()
{
	return g_rig.shot_anim_tail;
}

float CCameraRig::AimReloadTail()
{
	return g_rig.aim_reload_tail;
}

float CCameraRig::MassSpeed(float mass)
{
	return g_rig.inertia_speed / powf(std::max(mass, 0.1f), 0.25f);
}

float CCameraRig::ShotAnimBlend()
{
	return g_rig.shot_anim_blend;
}

float CCameraRig::LeftHandIkFollow()
{
	return g_rig.left_hand_ik_follow;
}

float CCameraRig::AimReloadBlend()
{
	return g_rig.aim_reload_blend;
}

bool CCameraRig::SpringSway(Fvector& value, Fvector& velocity, const Fvector& target, float half_life, float damping)
{
	if (!Enabled())
	{
		return false;
	}

	HudSpring::Follow(value, velocity, target, half_life, damping, Device.fTimeDelta);
	return true;
}

void CCameraRig::TransformBone(IKinematics* model, u16 bone, const Fmatrix& delta)
{
	CBoneInstance& instance = model->LL_GetBoneInstance(bone);
	CBoneData& data = model->LL_GetData(bone);
	instance.mTransform.mulA_43(delta);
	instance.mRenderTransform.mul_43(instance.mTransform, data.m2b_transform);

	for (CBoneData* child : data.children)
	{
		TransformBone(model, child->GetSelfID(), delta);
	}
}

static void rig_rotate_bone(IKinematics* model, u16 bone, const Fvector& pivot, Fvector from, Fvector to)
{
	from.normalize_safe();
	to.normalize_safe();

	Fvector axis;
	axis.crossproduct(from, to);
	const float sin = axis.magnitude();
	if (sin <= EPS_S)
	{
		return;
	}

	Fmatrix delta;
	delta.rotation(axis.div(sin), atan2f(sin, from.dotproduct(to)));
	Fvector rotated;
	delta.transform_dir(rotated, pivot);
	delta.c.sub(pivot, rotated);
	CCameraRig::TransformBone(model, bone, delta);
}

void CCameraRig::SolveLimb(IKinematics* model, const u16* chain, const Fmatrix& target, float weight, const Fvector& PoleOffset)
{
	if (weight <= EPS || chain[0] == BI_NONE || chain[1] == BI_NONE || chain[2] == BI_NONE)
	{
		return;
	}

	const Fvector root = model->LL_GetTransform(chain[0]).c;
	const Fvector joint = model->LL_GetTransform(chain[1]).c;
	const Fvector end = model->LL_GetTransform(chain[2]).c;
	const float upper = root.distance_to(joint);
	const float lower = joint.distance_to(end);

	Fvector dir;
	dir.sub(Fvector().lerp(end, target.c, weight), root);
	const float length = dir.magnitude();
	if (upper <= EPS_L || lower <= EPS_L || length <= EPS_L)
	{
		return;
	}

	dir.div(length);
	const float reach = std::clamp(length, std::abs(upper - lower) + EPS_L, upper + lower - EPS_L);

	Fvector pole;
	pole.sub(joint, root).add(PoleOffset);
	pole.mad(dir, -pole.dotproduct(dir));
	if (pole.square_magnitude() <= EPS_S)
	{
		return;
	}

	pole.normalize();
	const float cos = std::clamp((upper * upper + reach * reach - lower * lower) / (2.f * upper * reach), -1.f, 1.f);

	Fvector new_joint;
	new_joint.mad(root, dir, cos * upper).mad(pole, _sqrt(1.f - cos * cos) * upper);
	rig_rotate_bone(model, chain[0], root, Fvector().sub(joint, root), Fvector().sub(new_joint, root));
	rig_rotate_bone(model, chain[1], new_joint, Fvector().sub(model->LL_GetTransform(chain[2]).c, new_joint), Fvector().mad(root, dir, reach).sub(new_joint));

	const Fmatrix& hand = model->LL_GetTransform(chain[2]);
	Fquaternion q_from, q_to, q_blend;
	q_from.set(hand);
	q_to.set(target);
	q_blend.slerp(q_from, q_to, weight);

	Fmatrix desired;
	desired.rotation(q_blend);
	desired.c = hand.c;

	Fmatrix delta;
	delta.mul_43(desired, Fmatrix().invert(hand));
	TransformBone(model, chain[2], delta);
}

static const shared_str& hud_retarget_source_idle(const shared_str& motion)
{
	static xr_hash_map<shared_str, shared_str> sources;
	static xr_hash_set<shared_str> idles;
	static bool built = false;
	if (!built)
	{
		built = true;
		string128 name;
		const auto read_list = [&name](CInifile::Sect& sect, const char* key, xr_vector<shared_str>& list)
		{
			const char* value = nullptr;
			if (sect.line_exist(key, &value) && value != nullptr)
			{
				for (int i = 0, count = _GetItemCount(value); i < count; ++i)
				{
					list.emplace_back(_GetItem(value, i, name));
				}
			}
		};

		for (int pass = 0; pass < 3; ++pass)
		{
			for (CInifile::Sect& sect : pSettings->sections())
			{
				const char* idle = nullptr;
				if (!sect.line_exist("anm_idle", &idle) || idle == nullptr || _GetItemCount(idle) == 0)
				{
					continue;
				}

				const bool motion_set = !sect.line_exist("item_visual") && !sect.line_exist("visual");
				if (motion_set != (pass == 0))
				{
					continue;
				}

				const shared_str idle_motion = _GetItem(idle, 0, name);
				idles.insert(idle_motion);
				xr_vector<shared_str> retarget;
				read_list(sect, "retarget_motions", retarget);

				for (const CInifile::Item& line : sect.Data)
				{
					const char* alias = line.first.c_str();
					if (alias == nullptr || strstr(alias, "anm_") != alias || strstr(alias, "anm_bp_") == alias || line.second.size() == 0
						|| (motion_set ? xr_strcmp(alias, "anm_idle") == 0 : strstr(alias, "anm_idle") == alias || hud_motion_listed(retarget, line.first) != (pass == 2)))
					{
						continue;
					}

					sources.try_emplace(shared_str(_GetItem(line.second.c_str(), 0, name)), idle_motion);
				}
			}
		}
	}

	static xr_hash_map<shared_str, shared_str> by_name;
	if (const auto named = by_name.find(motion); named != by_name.end())
	{
		return named->second;
	}

	shared_str found;
	xr_string prefix = motion.c_str() != nullptr ? motion.c_str() : "";
	for (size_t split = prefix.rfind('_'); split != xr_string::npos && split > 0; split = prefix.rfind('_'))
	{
		prefix.resize(split);
		const shared_str candidate = (prefix + "_idle").c_str();
		if (idles.contains(candidate))
		{
			found = candidate;
			break;
		}
	}

	if (found.size() == 0)
	{
		if (const auto it = sources.find(motion); it != sources.end())
		{
			found = it->second;
		}
	}

	return by_name.try_emplace(motion, found).first->second;
}

struct SHudRigField
{
	const char*						Key;
	float attachable_hud_item::*	Float;
	Fvector attachable_hud_item::*	Vector;
	float							Min = -flt_max;
	float							Max = flt_max;
};

static const SHudRigField g_hud_rig_fields[] =
{
	{ "aim_reload_motion", &attachable_hud_item::AimReloadMotion, nullptr },
	{ "aim_reload_hud_offset_pos", nullptr, &attachable_hud_item::AimReloadOffsetPos },
	{ "aim_reload_hud_offset_rot", nullptr, &attachable_hud_item::AimReloadOffsetRot },
	{ "aim_reload_zoom_out", &attachable_hud_item::AimReloadZoomOut, nullptr, 0.f, 1.f },
	{ "aim_reload_pose_inertia", &attachable_hud_item::AimReloadPoseInertia, nullptr, 0.f },
	{ "aim_reload_pose_inertia_out", &attachable_hud_item::AimReloadPoseInertiaOut, nullptr, 0.f },
	{ "aim_reload_hud_push_pos", nullptr, &attachable_hud_item::AimReloadPushPos },
	{ "crouch_hud_offset_pos", nullptr, &attachable_hud_item::CrouchOffsetPos },
	{ "crouch_hud_offset_rot", nullptr, &attachable_hud_item::CrouchOffsetRot },
	{ "crouch_pose_inertia", &attachable_hud_item::CrouchPoseInertia, nullptr, 0.f },
	{ "crouch_pose_inertia_out", &attachable_hud_item::CrouchPoseInertiaOut, nullptr },
	{ "crouch_pose_delay", &attachable_hud_item::CrouchPoseDelay, nullptr, 0.f },
	{ "walk_hud_offset_pos", nullptr, &attachable_hud_item::WalkOffsetPos },
	{ "walk_hud_offset_rot", nullptr, &attachable_hud_item::WalkOffsetRot },
	{ "walk_pose_delay", &attachable_hud_item::WalkPoseDelay, nullptr, 0.f },
	{ "walk_pose_inertia", &attachable_hud_item::WalkPoseInertia, nullptr, 0.f },
	{ "walk_pose_inertia_out", &attachable_hud_item::WalkPoseInertiaOut, nullptr, 0.f },
	{ "carry_pose_damping", &attachable_hud_item::CarryPoseDamping, nullptr, 0.f },
	{ "look_sway_pos", nullptr, &attachable_hud_item::LookSwayPos },
	{ "aim_sway_scale", &attachable_hud_item::AimSwayScale, nullptr, 0.f },
	{ "look_sway_inertia", &attachable_hud_item::LookSwayInertia, nullptr, 0.f },
	{ "look_sway_damping", &attachable_hud_item::LookSwayDamping, nullptr, 0.f },
	{ "left_hand_sway_look", nullptr, &attachable_hud_item::LeftSwayLook },
	{ "left_hand_sway_bob", nullptr, &attachable_hud_item::LeftSwayBob },
	{ "left_hand_sway_jump", nullptr, &attachable_hud_item::LeftSwayJump },
	{ "right_hand_sway_look", nullptr, &attachable_hud_item::RightSwayLook },
	{ "right_hand_sway_jump", nullptr, &attachable_hud_item::RightSwayJump },
	{ "left_hand_follow_lag", &attachable_hud_item::LeftFollowLag, nullptr, 0.f },
	{ "left_hand_follow_turn", &attachable_hud_item::LeftFollowTurn, nullptr, 0.f, 1.f },
	{ "left_hand_follow_move", nullptr, &attachable_hud_item::LeftFollowMove },
	{ "left_hand_follow_move_rot", &attachable_hud_item::LeftFollowMoveRot, nullptr, 0.f, 1.f },
	{ "left_hand_bob_pos", nullptr, &attachable_hud_item::LeftBobPos },
	{ "left_hand_bob_rot", nullptr, &attachable_hud_item::LeftBobRot },
	{ "aim_left_elbow_offset", nullptr, &attachable_hud_item::AimLeftElbowOffset },
	{ "hand_sway_inertia", &attachable_hud_item::HandSwayInertia, nullptr, 0.f },
	{ "hand_sway_damping", &attachable_hud_item::HandSwayDamping, nullptr, 0.f },
	{ "turn_pivot_look", nullptr, &attachable_hud_item::TurnPivotLook },
	{ "turn_pivot_max", &attachable_hud_item::TurnPivotMax, nullptr, 0.f },
	{ "turn_pivot_inertia", &attachable_hud_item::TurnPivotInertia, nullptr, 0.f },
	{ "turn_pivot_damping", &attachable_hud_item::TurnPivotDamping, nullptr, 0.f },
	{ "turn_pivot_lift_fall", &attachable_hud_item::TurnPivotLiftFall, nullptr, 1.f },
	{ "aim_turn_pivot_scale", &attachable_hud_item::AimTurnPivotScale, nullptr, 0.f },
	{ "turn_pivot_grip_lag", &attachable_hud_item::TurnPivotGripLag, nullptr, 0.f, 1.f },
	{ "turn_pivot_lift", &attachable_hud_item::TurnPivotLift, nullptr },
	{ "turn_pivot_lift_pos", &attachable_hud_item::TurnPivotLiftPos, nullptr },
	{ "turn_pivot_lift_max", &attachable_hud_item::TurnPivotLiftMax, nullptr, 0.f },
	{ "turn_pivot_lift_inertia", &attachable_hud_item::TurnPivotLiftInertia, nullptr, 0.f },
	{ "turn_tilt", &attachable_hud_item::TurnTilt, nullptr },
	{ "turn_tilt_max", &attachable_hud_item::TurnTiltMax, nullptr, 0.f },
	{ "aim_blend_inertia", &attachable_hud_item::AimBlendInertia, nullptr, 0.f },
	{ "sprint_pose_inertia", &attachable_hud_item::SprintPoseInertia, nullptr, 0.f },
	{ "sprint_blend_in", &attachable_hud_item::SprintBlendIn, nullptr, 0.f },
	{ "sprint_left_grip_range", &attachable_hud_item::SprintLeftGripRange, nullptr, 0.f },
	{ "sprint_left_cancel_return", &attachable_hud_item::SprintLeftCancelReturn, nullptr, 0.f },
	{ "shot_anim_variation", &attachable_hud_item::ShotVariation, nullptr },
	{ "shot_anim_twist", &attachable_hud_item::ShotTwist, nullptr },
	{ "aim_shot_motion", &attachable_hud_item::AimShotMotion, nullptr },
	{ "idle_fade", &attachable_hud_item::IdleFade, nullptr },
	{ "inertia_damping", &attachable_hud_item::InertiaDamping, nullptr },
	{ "additive_blend_in", &attachable_hud_item::AdditiveBlendIn, nullptr, 0.f },
	{ "additive_blend_out", &attachable_hud_item::AdditiveBlendOut, nullptr, 0.01f },
	{ "left_hand_attach_blend", &attachable_hud_item::LeftHandAttachBlend, nullptr, 0.f },
	{ "left_hand_ik_blend", &attachable_hud_item::LeftHandIkBlend, nullptr, 0.f },
	{ "left_hand_finger_grip", &attachable_hud_item::LeftHandFingerGrip, nullptr, 0.f, 1.f },
};

void attachable_hud_item::LoadRigParams()
{
	const shared_str& sect_name = m_sect_name;

	const auto load_bones = [&](const char* key, xr_vector<u16>& bones)
	{
		bones.clear();
		const char* list = pSettings->read_if_exists<str_c>(sect_name, key, nullptr);
		string128 bone_name;
		for (int i = 0, count = list != nullptr ? _GetItemCount(list) : 0; i < count; ++i)
		{
			const u16 bone = m_model->LL_BoneID(_GetItem(list, i, bone_name));
			if (bone != BI_NONE)
			{
				bones.push_back(bone);
			}
		}
	};

	load_bones("left_hand_item_bones", LeftHandItemBones);
	load_bones("right_hand_item_bones", RightHandItemBones);

	const char* pivot_bone = pSettings->read_if_exists<str_c>(sect_name, "turn_pivot_bone", pSettings->read_if_exists<str_c>("camera_rig", "turn_pivot_bone", "wpn_body"));
	TurnPivotBone = m_model_combined ? BI_NONE : m_model->LL_BoneID(pivot_bone);

	if (IKinematicsAnimated* animated = m_model->dcast_PKinematicsAnimated())
	{
		animated->LL_SetInertialization(CCameraRig::HudInertialization());
		animated->LL_SetSmoothBlend(CCameraRig::Enabled());

		const char* exclude = pSettings->read_if_exists<str_c>(sect_name, "inertialization_exclude_bones", pSettings->read_if_exists<str_c>("camera_rig", "hud_inertialization_exclude_bones", nullptr));
		if (exclude != nullptr)
		{
			string128 bone_name;
			for (int i = 0, count = _GetItemCount(exclude); i < count; ++i)
			{
				const u16 bone = m_model->LL_BoneID(_GetItem(exclude, i, bone_name));
				if (bone != BI_NONE)
				{
					animated->LL_SetInertializationBone(bone, false);
				}
			}
		}
	}

	for (const SHudRigField& it : g_hud_rig_fields)
	{
		const SCameraRigParam* param = CCameraRig::FindHudParam(it.Key);
		if (it.Vector != nullptr)
		{
			const Fvector def = param != nullptr ? Fvector().set(param->Value[0], param->Value[1], param->Value[2]) : Fvector().set(0.f, 0.f, 0.f);
			this->*it.Vector = pSettings->read_if_exists<Fvector3>(sect_name, it.Key, def);
		}
		else
		{
			const float def = param != nullptr ? param->Value[0] : 0.f;
			this->*it.Float = std::clamp(pSettings->read_if_exists<float>(sect_name, it.Key, def), it.Min, it.Max);
		}
	}
}

float* attachable_hud_item::RigField(const char* key, u32& count)
{
	for (const SHudRigField& it : g_hud_rig_fields)
	{
		if (xr_strcmp(it.Key, key) == 0)
		{
			count = it.Vector != nullptr ? 3 : 1;
			return it.Vector != nullptr ? &(this->*it.Vector).x : &(this->*it.Float);
		}
	}

	count = 0;
	return nullptr;
}

void attachable_hud_item::PlayRetargetBase(const shared_str& alias)
{
	player_hud_motion* base = m_hand_motions.find_motion(alias);
	if (base == nullptr || base->m_animations.empty() || g_player_hud->GetModel() == nullptr)
	{
		return;
	}

	const motion_descr& M = base->m_animations[0];
	g_player_hud->CycleFade = IdleFade;
	g_player_hud->CycleMode = {};
	g_player_hud->CycleMode.Type = EHudCycleMode::KeepBase;

	const CMotionDef* md = nullptr;
	g_player_hud->anim_play(m_attach_place_idx, M.mid, false, md, 1.f, nullptr);
	if (m_model->dcast_PKinematicsAnimated() != nullptr)
	{
		anim_play(base->m_base_name != base->m_additional_name ? base->m_additional_name : M.name, eNoMix, 1.f, base);
	}

	g_player_hud->CycleFade = 0.f;
	g_player_hud->CycleMode = {};
}

void attachable_hud_item::UpdateAttachPose()
{
	shared_str pose;
	IkMark = HudAttachMarks::Ik;
	FingersMark = HudAttachMarks::Fingers;
	if (m_parent_hud_item != nullptr)
	{
		m_parent_hud_item->item().FindAttachPose(pose, IkMark, FingersMark);
	}

	if (pose == AttachPose)
	{
		return;
	}

	AttachPose = pose;
	AttachPoseReference = nullptr;
	m_hand_motions.PoseOverrides.clear();
	if (pose.size() != 0 && m_hand_motions.find_motion(pose) != nullptr)
	{
		if (const player_hud_motion* base = m_hand_motions.find_motion("anm_idle"); base != nullptr && !base->m_animations.empty())
		{
			AttachPoseReference = base->m_animations[0].name;
		}

		for (const char* idle : { "anm_idle", "anm_idle_aim", "anm_idle_moving", "anm_idle_sprint" })
		{
			if (m_hand_motions.find_motion(idle) != nullptr)
			{
				m_hand_motions.PoseOverrides[idle] = pose;
			}
		}
	}

	LeftHandAttachRefTried = false;
	LeftHandAttachRefValid = false;
	FingerGripPoseMotion = nullptr;
}

xr_vector<u16> player_hud::left_arm_blend_bones() const
{
	xr_vector<u16> bones;
	if (m_model == nullptr || Ik.LeftIkChain[0] == BI_NONE || Ik.LeftIkChain[1] == BI_NONE)
	{
		return bones;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	const u16 clavicle = kinematics->LL_GetData(Ik.LeftIkChain[0]).GetParentID();
	if (clavicle != BI_NONE && clavicle != kinematics->LL_GetBoneRoot())
	{
		bones.push_back(clavicle);
	}

	bones.push_back(Ik.LeftIkChain[0]);
	bones.push_back(Ik.LeftIkChain[1]);
	return bones;
}

bool attachable_hud_item::RetargetMode(const shared_str& motion, shared_str& reference) const
{
	reference = nullptr;
	if (m_model_combined || !CCameraRig::Enabled())
	{
		return false;
	}

	const auto base_name = [this](const char* alias)
	{
		for (const player_hud_motion& it : m_hand_motions.m_anims)
		{
			if (it.m_alias_name == alias)
			{
				return it.m_base_name;
			}
		}

		return shared_str();
	};

	if (!hud_motion_listed(RetargetMotions, motion))
	{
		return false;
	}

	const shared_str& source = hud_retarget_source_idle(base_name(motion.c_str()));
	if (RetargetReference.size() != 0 && source == RetargetSource)
	{
		reference = RetargetReference;
		return true;
	}

	const player_hud_motion* own_idle = OwnIdle();
	const shared_str own = own_idle != nullptr ? own_idle->m_base_name : shared_str();
	if (source.size() != 0 ? source != own : RetargetReference.size() != 0)
	{
		reference = source.size() != 0 ? source : RetargetReference;
		return true;
	}

	reference = own.size() != 0 ? own : AttachPoseReference;
	return AttachPoseReference.size() != 0;
}

bool attachable_hud_item::RetargetLayer(const shared_str& alias, MotionID& motion, shared_str& reference) const
{
	const auto It = RetargetLayers.find(alias);
	if (It == RetargetLayers.end() || m_model_combined || !CCameraRig::Enabled() || g_player_hud->GetModel() == nullptr || hud_motion_listed(RetargetMotions, alias))
	{
		return false;
	}

	const MotionID Layer = g_player_hud->GetModel()->ID_Cycle_Safe(It->second.Motion);
	if (!Layer.valid())
	{
		return false;
	}

	motion = Layer;
	reference = It->second.Reference;
	return true;
}

void attachable_hud_item::LoadRetargetLayers()
{
	RetargetLayers.clear();
	for (auto It = RetargetMotions.begin(); It != RetargetMotions.end();)
	{
		if (!pSettings->section_exist(*It))
		{
			++It;
			continue;
		}

		const shared_str Reference = pSettings->read_if_exists<str_c>(*It, "retarget_reference", nullptr);
		for (const CInifile::Item& Line : pSettings->r_section(*It).Data)
		{
			if (strstr(Line.first.c_str(), "anm_") == Line.first.c_str() && Line.second.size() != 0)
			{
				string128 Motion;
				RetargetLayers[Line.first] = { _GetItem(Line.second.c_str(), 0, Motion), Reference };
			}
		}

		It = RetargetMotions.erase(It);
	}
}

const player_hud_motion* attachable_hud_item::OwnIdle() const
{
	const player_hud_motion* numbered = nullptr;
	for (const player_hud_motion& it : m_hand_motions.m_anims)
	{
		if (it.m_animations.empty())
		{
			continue;
		}

		if (it.m_alias_name == "anm_idle")
		{
			return &it;
		}

		const char* name = it.m_alias_name.c_str();
		if (numbered == nullptr && strstr(name, "anm_idle_") == name && isdigit(static_cast<unsigned char>(name[9])))
		{
			numbered = &it;
		}
	}

	return numbered;
}

MotionID attachable_hud_item::AliasMotion(const char* alias) const
{
	for (const player_hud_motion& it : m_hand_motions.m_anims)
	{
		if (it.m_alias_name == alias && !it.m_animations.empty())
		{
			return it.m_animations[0].mid;
		}
	}

	return MotionID();
}

bool attachable_hud_item::SprintLayered() const
{
	if (m_model_combined || !CCameraRig::Enabled() || !AliasMotion("anm_idle_sprint").valid())
	{
		return false;
	}

	const shared_str alias = "anm_idle_sprint";
	return hud_motion_listed(RetargetMotions, alias) || hud_motion_listed(AdditiveMotions, alias);
}

shared_str attachable_hud_item::SprintLayerReference() const
{
	shared_str reference;
	RetargetMode("anm_idle_sprint", reference);
	if (reference.size() != 0)
	{
		return reference;
	}

	if (AdditiveReference.size() != 0)
	{
		return AdditiveReference;
	}

	if (const player_hud_motion* idle = OwnIdle())
	{
		return idle->m_base_name;
	}

	return reference;
}

bool attachable_hud_item::AdditiveAlias(const shared_str& alias) const
{
	if (strstr(alias.c_str(), "anm_idle_sprint") == alias.c_str() && SprintLayered())
	{
		return true;
	}

	shared_str reference;
	return RetargetMode(alias, reference) || (CCameraRig::Enabled() && hud_motion_listed(AdditiveMotions, alias));
}

u8 attachable_hud_item::LeftHandAttachMotionMode(const shared_str& motion) const
{
	shared_str reference;
	if (RetargetMode(motion, reference))
	{
		return 0;
	}

	if (hud_motion_listed(LeftHandAttachAnimatedArmMotions, motion))
	{
		return 2;
	}

	return hud_motion_listed(LeftHandAttachMotions, motion) ? 1 : 0;
}

static float hud_motion_amount(IKinematicsAnimated* ka, MotionID motion)
{
	float amount = 0.f;
	float total = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == 0)
		{
			total += B->Weight();
			if (B->motionID == motion)
			{
				amount += B->Weight();
			}
		}
	}

	return total > EPS ? amount / total : 0.f;
}

static float hud_motion_time(IKinematicsAnimated* ka, MotionID motion, u8 channel = 0)
{
	float time = -1.f;
	float amount = -1.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == channel && B->motionID == motion && B->blendAmount > amount)
		{
			amount = B->blendAmount;
			time = B->timeCurrent;
		}
	}

	return time;
}

static float hud_mark_time(CMotionDef* def, float motion_time)
{
	const float speed = def->Speed();
	return speed <= EPS || fsimilar(speed, 1.f) ? motion_time : motion_time / speed;
}

static float hud_mark_ramp(float delta, float blend)
{
	return blend > EPS ? std::clamp(delta / blend, 0.f, 1.f) : (delta >= 0.f ? 1.f : 0.f);
}

struct SHudMarkSpan
{
	const motion_marks::interval*	First = nullptr;
	const motion_marks::interval*	Active = nullptr;
	int								ActiveIndex = -1;
};

static SHudMarkSpan hud_mark_span(const CMotionDef* def, const char* name, float time)
{
	SHudMarkSpan span;
	for (const motion_marks& marks : def->marks)
	{
		if (xr_strcmp(marks.name, name) != 0)
		{
			continue;
		}

		for (int index = 0; index < int(marks.intervals.size()); ++index)
		{
			const motion_marks::interval& interval = marks.intervals[index];
			if (span.First == nullptr || interval.first < span.First->first)
			{
				span.First = &interval;
			}

			if (interval.first <= time && (span.Active == nullptr || interval.first > span.Active->first))
			{
				span.Active = &interval;
				span.ActiveIndex = index;
			}
		}
	}

	return span;
}

static bool hud_motion_local_pose(IKinematicsAnimated* ka, MotionID motion, float time, const xr_vector<u16>& bones, xr_vector<Fquaternion>& pose, xr_vector<Fvector>* positions = nullptr)
{
	pose.clear();
	if (positions != nullptr)
	{
		positions->clear();
	}

	IKinematics* kinematics = ka->dcast_PKinematics();
	for (u16 bone : bones)
	{
		Fmatrix bone_ms, parent_ms;
		if (!ka->LL_GetMotionBoneTransform(motion, time, bone, bone_ms) || !ka->LL_GetMotionBoneTransform(motion, time, kinematics->LL_GetData(bone).GetParentID(), parent_ms))
		{
			pose.clear();
			if (positions != nullptr)
			{
				positions->clear();
			}

			return false;
		}

		const Fmatrix local = Fmatrix().mul_43(Fmatrix().invert(parent_ms), bone_ms);
		pose.push_back(Fquaternion().set(local));
		if (positions != nullptr)
		{
			positions->push_back(local.c);
		}
	}

	return !pose.empty();
}

static float hud_ancor_deviation(const Fmatrix& current, const Fmatrix& reference)
{
	Fquaternion q_current, q_reference;
	q_current.set(current);
	q_reference.set(reference);
	const float dot = std::min(std::abs(q_current.x * q_reference.x + q_current.y * q_reference.y + q_current.z * q_reference.z + q_current.w * q_reference.w), 1.f);
	return std::max(current.c.distance_to(reference.c) / 0.1f, 2.f * acosf(dot) / deg2rad(30.f));
}

static MotionID hud_dominant_motion(IKinematicsAnimated* ka, u8 channel = 0)
{
	MotionID motion;
	float amount = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == channel && B->blend_state() != CBlend::eFREE_SLOT && B->blendAmount > amount)
		{
			amount = B->blendAmount;
			motion = B->motionID;
		}
	}

	return motion;
}

static float hud_motion_settle(IKinematicsAnimated* ka, const player_hud_motion_container& motions, float settle_time, bool ahead)
{
	xr_vector<MotionID> idles;
	for (const player_hud_motion& motion : motions.m_anims)
	{
		if (strstr(motion.m_alias_name.c_str(), "anm_idle") == motion.m_alias_name.c_str())
		{
			for (const motion_descr& descr : motion.m_animations)
			{
				idles.push_back(descr.mid);
			}
		}
	}

	float settled = 0.f;
	float total = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B == nullptr || B->channel != 0 || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f || B->timeTotal <= EPS || std::find(idles.begin(), idles.end(), B->motionID) != idles.end())
		{
			continue;
		}

		if (B->blend_state() == CBlend::eFalloff)
		{
			settled += B->Weight();
		}
		else if (ahead)
		{
			const float speed = std::abs(B->speed) > EPS ? std::abs(B->speed) : 1.f;
			settled += B->Weight() * (1.f - hud_mark_ramp(std::max(B->timeTotal - B->timeCurrent, 0.f) / speed, settle_time));
		}

		total += B->Weight();
	}

	return total > 0.f ? std::clamp(settled / total, 0.f, 1.f) : 0.f;
}

struct SHudFingerHold
{
	MotionID	Motion;
	float		Hold = 0.f;
	float		StartHold = 0.f;
	float		StartTime = 0.f;
	float		Idle = 0.f;
};

static SHudFingerHold hud_finger_hold(IKinematicsAnimated* ka, const shared_str& mark, float blend)
{
	SHudFingerHold result;
	if (ka == nullptr)
	{
		return result;
	}

	float total = 0.f;
	float best = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B == nullptr || B->channel != 0 || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f)
		{
			continue;
		}

		total += B->Weight();

		CMotionDef* def = ka->LL_GetMotionDef(B->motionID);
		if (def == nullptr || def->marks.empty() || B->Weight() <= best)
		{
			continue;
		}

		const float time = hud_mark_time(def, B->timeCurrent);

		const SHudMarkSpan span = hud_mark_span(def, mark.c_str(), time);
		if (span.First == nullptr)
		{
			continue;
		}

		best = B->Weight();
		result.Motion = B->motionID;
		const float speed = def->Speed();
		const motion_marks::interval* interval = span.Active != nullptr ? span.Active : span.First;
		result.StartTime = speed <= EPS || fsimilar(speed, 1.f) ? interval->first : interval->first * speed;
		if (span.Active == nullptr)
		{
			result.StartHold = 1.f;
			result.Idle = 0.f;
		}
		else if (time <= interval->second)
		{
			result.StartHold = std::max(1.f - hud_mark_ramp(time - interval->first, blend), hud_mark_ramp(time - (interval->second - blend), blend));
			result.Idle = 0.f;
		}
		else
		{
			result.StartHold = 1.f;
			result.Idle = hud_mark_ramp(time - interval->second, blend);
		}
	}

	result.Hold = total > 0.f ? best / total : 0.f;
	return result;
}

static bool hud_attach_mark(IKinematicsAnimated* ka, const shared_str& mark, float blend, float& weight, MotionID& motion, bool& pre_start)
{
	if (ka == nullptr)
	{
		return false;
	}

	const CBlend* best = nullptr;
	CMotionDef* best_def = nullptr;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B == nullptr || B->channel != 0 || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f || (best != nullptr && B->blendAmount <= best->blendAmount))
		{
			continue;
		}

		CMotionDef* def = ka->LL_GetMotionDef(B->motionID);
		if (def == nullptr)
		{
			continue;
		}

		if (std::any_of(def->marks.begin(), def->marks.end(), [&mark](const motion_marks& marks) { return marks.name == mark && !marks.is_empty(); }))
		{
			best = B;
			best_def = def;
		}
	}

	if (best == nullptr)
	{
		return false;
	}

	motion = best->motionID;
	pre_start = false;
	const float time = hud_mark_time(best_def, best->timeCurrent);
	CMotion* root = ka->LL_GetRootMotion(best->motionID);
	const float length = root != nullptr ? hud_mark_time(best_def, root->GetLength()) : 0.f;

	const SHudMarkSpan span = hud_mark_span(best_def, mark.c_str(), time);
	if (span.Active == nullptr)
	{
		pre_start = true;
		weight = 1.f;
	}
	else if (span.Active->second >= length - EPS_L || time < span.Active->second)
	{
		pre_start = span.Active == span.First;
		weight = pre_start ? 1.f - hud_mark_ramp(time - span.Active->first, blend) : 0.f;
	}
	else
	{
		weight = hud_mark_ramp(time - span.Active->second, blend);
	}

	return true;
}

bool player_hud::HandAttachRelative(attachable_hud_item* item, MotionID hands_motion, float hands_time, MotionID item_motion, float item_time, Fmatrix& relative, Fmatrix* ancor_ms)
{
	IKinematicsAnimated* weapon = item->m_model->dcast_PKinematicsAnimated();
	if (m_model == nullptr || weapon == nullptr || Ik.LeftIkChain[2] == BI_NONE || item->LeftIkBone == BI_NONE)
	{
		return false;
	}

	Fmatrix ancor, wrist, attach_bone;
	if (!m_model->LL_GetMotionBoneTransform(hands_motion, hands_time, m_ancors[item->m_attach_place_idx], ancor)
		|| !m_model->LL_GetMotionBoneTransform(hands_motion, hands_time, Ik.LeftIkChain[2], wrist)
		|| !weapon->LL_GetMotionBoneTransform(item_motion, item_time, item->LeftIkBone, attach_bone))
	{
		return false;
	}

	Fmatrix attach;
	attach.mul_43(ancor, item->m_attach_offset).mulB_43(attach_bone);
	relative.mul_43(Fmatrix().invert(attach), wrist);
	if (ancor_ms != nullptr)
	{
		ancor_ms->set(ancor);
	}

	return true;
}

bool player_hud::LeftHandAttachReference(attachable_hud_item* item)
{
	IKinematicsAnimated* weapon = item->m_model->dcast_PKinematicsAnimated();
	player_hud_motion* anm = item->m_hand_motions.find_motion(item->LeftHandAttachTarget);
	if (m_model == nullptr || weapon == nullptr || anm == nullptr || anm->m_animations.empty() || Ik.LeftIkChain[2] == BI_NONE)
	{
		return false;
	}

	const motion_descr& M = anm->m_animations[0];
	const shared_str& item_anim = anm->m_base_name != anm->m_additional_name ? anm->m_additional_name : M.name;
	MotionID item_motion = weapon->ID_Cycle_Safe(item_anim);
	if (!item_motion.valid())
	{
		item_motion = weapon->ID_Cycle_Safe("idle");
	}

	const float hands_time = std::max(hud_motion_time(m_model, M.mid), 0.f);
	const float item_time = std::max(hud_motion_time(weapon, item_motion), 0.f);
	if (!HandAttachRelative(item, M.mid, hands_time, item_motion, item_time, item->LeftHandAttachRef, &item->LeftHandAttachAncorRef))
	{
		return false;
	}

	const xr_vector<u16> arm_bones = left_arm_subtree();
	hud_motion_local_pose(m_model, M.mid, hands_time, arm_bones, item->LeftHandAttachPoseRef, &item->LeftHandAttachPosRef);
	return !arm_bones.empty() && m_model->LL_GetMotionBoneTransform(M.mid, hands_time, arm_bones[0], item->LeftHandAttachArmRootRef);
}

bool player_hud::RetargetHandTarget(u16 ancor, u16 wrist, MotionID motion, u8 channel, MotionID reference, Fmatrix& result, float strength, float* deviation)
{
	if (m_model == nullptr || wrist == BI_NONE || !motion.valid() || !reference.valid())
	{
		return false;
	}

	const auto relative = [&](MotionID sample, float at, Fmatrix& out)
	{
		Fmatrix ancor_ms, wrist_ms;
		if (!m_model->LL_GetMotionBoneTransform(sample, at, ancor, ancor_ms) || !m_model->LL_GetMotionBoneTransform(sample, at, wrist, wrist_ms))
		{
			return false;
		}

		out.mul_43(Fmatrix().invert(ancor_ms), wrist_ms);
		return true;
	};

	Fquaternion rotation;
	Fvector position = { 0.f, 0.f, 0.f };
	float total = 0.f;
	for (u32 i = 0, count = m_model->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = m_model->LL_PartBlend(0, i);
		Fmatrix sample;
		if (B == nullptr || B->channel != channel || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f || !(B->additive_ref == reference) || !relative(B->motionID, B->timeCurrent, sample))
		{
			continue;
		}

		const Fquaternion q = Fquaternion().set(sample);
		total += B->Weight();
		const float share = B->Weight() / total;
		if (share >= 1.f)
		{
			rotation = q;
			position = sample.c;
		}
		else
		{
			rotation.slerp(Fquaternion(rotation), q, share);
			position.lerp(position, sample.c, share);
		}
	}

	Fmatrix source, reference_relative, target;
	if (total > 0.f)
	{
		source.rotation(rotation);
		source.c = position;
	}
	else
	{
		const float time = hud_motion_time(m_model, motion, channel);
		if (time < 0.f || !relative(motion, time, source))
		{
			return false;
		}
	}

	const u8 BaseMask = u8(~((1 << channel) | (1 << LAYER_ADDITIVE_CHANNEL)));
	Fmatrix AncorBase, WristBase;
	if (!m_model->LL_GetBoneTransformMasked(ancor, BaseMask, AncorBase) || !m_model->LL_GetBoneTransformMasked(wrist, BaseMask, WristBase) || !relative(reference, 0.f, reference_relative))
	{
		return false;
	}

	target.mul_43(Fmatrix().invert(AncorBase), WristBase);

	if (strength < 1.f)
	{
		Fquaternion blended;
		blended.slerp(Fquaternion().set(reference_relative), Fquaternion().set(source), std::max(strength, 0.f));
		Fvector blended_pos;
		blended_pos.lerp(reference_relative.c, source.c, std::max(strength, 0.f));
		source.rotation(blended);
		source.c = blended_pos;
	}

	if (deviation != nullptr)
	{
		*deviation = hud_ancor_deviation(source, reference_relative);
	}

	result.mul_43(source, Fmatrix().mul_43(Fmatrix().invert(reference_relative), target));
	result.c.add(source.c, Fvector().sub(target.c, reference_relative.c));
	return true;
}

static float hud_channel_weight(IKinematicsAnimated* ka, u8 channel)
{
	float weight = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == channel && B->blend_state() != CBlend::eFREE_SLOT)
		{
			weight += B->blendAmount;
		}
	}

	return std::clamp(weight, 0.f, 1.f);
}

static void HudOverlayHandRelative(IKinematicsAnimated* Model, u16 Ancor, u16 Wrist, const Fmatrix& Base, Fmatrix& Result)
{
	const auto Relative = [&](MotionID Motion, float Time, Fmatrix& Out)
	{
		Fmatrix AncorMs, WristMs;
		if (!Model->LL_GetMotionBoneTransform(Motion, Time, Ancor, AncorMs) || !Model->LL_GetMotionBoneTransform(Motion, Time, Wrist, WristMs))
		{
			return false;
		}

		Out.mul_43(Fmatrix().invert(AncorMs), WristMs);
		return true;
	};

	Fquaternion Rotation;
	Rotation.identity();
	Fvector Position = { 0.f, 0.f, 0.f };
	float Total = 0.f;
	for (u32 i = 0, Count = Model->LL_PartBlendsCount(0); i < Count; i++)
	{
		const CBlend* B = Model->LL_PartBlend(0, i);
		Fmatrix Now, Start;
		if (B == nullptr || B->channel != ADDITIVE_CHANNEL || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f || B->additive_ref.valid() || !Relative(B->motionID, B->timeCurrent, Now) || !Relative(B->motionID, 0.f, Start))
		{
			continue;
		}

		const Fquaternion Delta = Fquaternion().set(Fmatrix().mul_43(Now, Fmatrix().invert(Start)));
		const Fvector Shift = Fvector().sub(Now.c, Start.c);
		const float Weight = B->Weight();
		Total += Weight;
		const float Share = Total > EPS ? Weight / Total : 1.f;
		Rotation.slerp(Fquaternion(Rotation), Delta, Share);
		Position.lerp(Position, Shift, Share);
	}

	if (Total <= 0.f)
	{
		Result.set(Base);
		return;
	}

	const float Amount = std::min(Total, 1.f);
	Rotation.slerp(Fquaternion().identity(), Fquaternion(Rotation), Amount);
	Position.mul(Amount);

	Result.mul_43(Fmatrix().rotation(Rotation), Base);
	Result.c.add(Base.c, Position);
}

static float hud_cycle_progress(IKinematicsAnimated* ka)
{
	float progress = -1.f;
	float amount = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == 0 && B->stop_at_end && B->blend_state() != CBlend::eFalloff && B->timeTotal > EPS && B->blendAmount >= amount)
		{
			amount = B->blendAmount;
			progress = std::clamp(B->timeCurrent / B->timeTotal, 0.f, 1.f);
		}
	}

	return progress;
}

struct SHandsIkFrame
{
	attachable_hud_item*	Item = nullptr;
	IKinematics*			Model = nullptr;
	CWeapon*				Weapon = nullptr;
	u32						State = 0;
	bool					Active = false;
	bool					HasRight = false;
	bool					HasLeft = false;
	bool					HasAttach = false;
	u16						Ancor = BI_NONE;
	bool					AncorInRightHand = false;
	bool					RetargetRight = false;
	bool					RetargetLeft = false;
	float					SprintLayer = 0.f;
	Fmatrix					SprintAncorDelta = Fidentity;
	Fmatrix					SprintRightBase = Fidentity;
	Fmatrix					SprintLeftBase = Fidentity;
	Fmatrix					SprintRightLayer = Fidentity;
	Fmatrix					SprintLeftLayer = Fidentity;
	bool					SprintRight = false;
	bool					SprintLeft = false;
	bool					SprintLayerRight = false;
	bool					SprintLayerLeft = false;
	float					SprintLeftLayerWeight = 1.f;
	bool					SprintLeftPin = false;
	float					SprintLeftPinWeight = 0.f;
	Fmatrix					SprintLeftPinTarget = Fidentity;
	bool					Overlay = false;
	bool					AdditiveBase = false;
	Fmatrix					AdditiveAncorBase = Fidentity;
	Fmatrix					OverlayAncorDelta = Fidentity;
	Fmatrix					OverlayRightBase = Fidentity;
	Fmatrix					OverlayLeftBase = Fidentity;
	bool					OverlayRight = false;
	bool					OverlayLeft = false;
	bool					ReloadState = false;
	bool					Firing = false;
	bool					MoveWeapon = false;
	Fmatrix					SprintLayerAncor = Fidentity;
	SHudFingerHold			Finger;
	bool					FingerMark = false;
	bool					FingerHold = false;
	float					FingerStart = 0.f;
	float					FingerSettle = 0.f;
	Fmatrix					Delta = Fidentity;
	Fmatrix					RightTarget = Fidentity;
	Fmatrix					LeftTarget = Fidentity;
	Fmatrix					AttachTarget = Fidentity;
	float					AttachWeight = 0.f;
	float					AttachTargetWeight = -1.f;
	float					TargetWeight = 0.f;
	xr_vector<u16>			ArmBones;
	bool					AttachPose = false;
};

static Fmatrix hud_bone_local(IKinematics* model, u16 bone)
{
	return Fmatrix().mul_43(Fmatrix().invert(model->LL_GetTransform(model->LL_GetData(bone).GetParentID())), model->LL_GetTransform(bone));
}

static void hud_blend_local(IKinematics* model, u16 bone, const Fquaternion& target, float weight, const Fvector* position = nullptr)
{
	const Fmatrix parent = model->LL_GetTransform(model->LL_GetData(bone).GetParentID());
	const Fmatrix bone_ms = model->LL_GetTransform(bone);
	const Fmatrix local = Fmatrix().mul_43(Fmatrix().invert(parent), bone_ms);

	Fquaternion anim, result;
	anim.set(local);
	result.slerp(anim, target, weight);

	Fmatrix new_local;
	new_local.rotation(result);
	new_local.c = position != nullptr ? Fvector().lerp(local.c, *position, weight) : local.c;
	const Fmatrix new_ms = Fmatrix().mul_43(parent, new_local);
	CCameraRig::TransformBone(model, bone, Fmatrix().mul_43(new_ms, Fmatrix().invert(bone_ms)));
}

static void hud_blend_target(Fmatrix& target, const Fmatrix& held, float weight)
{
	Fquaternion q_anim, q_held, q_blend;
	q_anim.set(target);
	q_held.set(held);
	q_blend.slerp(q_anim, q_held, weight);

	Fvector position;
	position.lerp(target.c, held.c, weight);
	target.rotation(q_blend);
	target.c = position;
}

static Fmatrix hud_wrist_relative(IKinematics* Model, u16 Ancor, u16 Wrist)
{
	return Fmatrix().mul_43(Fmatrix().invert(Model->LL_GetTransform(Ancor)), Model->LL_GetTransform(Wrist));
}

void player_hud::update_hands_ik(const Fmatrix& cam_trans, CActor* actor)
{
	SHandsIkFrame F;
	if (!HandsIkBegin(F, actor))
	{
		return;
	}

	const Fmatrix AnimRelative = F.HasLeft ? hud_wrist_relative(F.Model, F.Ancor, Ik.LeftIkChain[2]) : Fidentity;

	HandsIkSprint(F);
	HandsIkWeaponPose(F, actor);
	HandsIkFingerState(F);

	if (!F.MoveWeapon && !F.RetargetRight && !F.RetargetLeft && !F.OverlayRight && !F.OverlayLeft && !F.SprintRight && !F.SprintLeft && !F.SprintLayerRight && !F.SprintLayerLeft && !(F.HasAttach && (Ik.LeftIkWeight > 0.f || F.Item->LeftHandAttachBlend > 0.f)) && !F.FingerHold && F.FingerSettle <= 0.f && F.FingerStart <= 0.f && !HandsIkSwayNeeded(F))
	{
		HandsIkFingerInertia(F);
		Sprint.LeftWristLast = AnimRelative;
		return;
	}

	HandsIkWeaponDelta(F);
	HandsIkTurnPivot(F, cam_trans, actor);
	HandsIkHandTargets(F);
	HandsIkAttachTarget(F);
	UpdateHandJump(actor, F.Item->HandSwayInertia);
	HandsIkLeftSway(F, cam_trans, actor);
	HandsIkRightSway(F, actor);
	HandsIkReach(F);
	const Fmatrix WristBefore[2] = { F.HasLeft ? F.Model->LL_GetTransform(Ik.LeftIkChain[2]) : Fidentity, F.HasRight ? F.Model->LL_GetTransform(Ik.RightIkChain[2]) : Fidentity };
	const Fmatrix AncorBefore = F.Model->LL_GetTransform(F.Ancor);
	HandsIkSolve(F);
	HandsIkFingers(F);
	HandsIkFingerInertia(F);
	F.Item->update(true);
	HandsIkItemBones(F, WristBefore, AncorBefore);
	if (F.HasLeft)
	{
		Sprint.LeftWristLast = hud_wrist_relative(F.Model, F.Ancor, Ik.LeftIkChain[2]);
	}
}

bool player_hud::HandsIkBegin(SHandsIkFrame& F, CActor* actor)
{
	if (Ik.LeftHandAttachSnapFrame != u32(-1) && Device.dwFrame > Ik.LeftHandAttachSnapFrame && m_model != nullptr)
	{
		for (u16 bone : left_arm_subtree())
		{
			m_model->LL_SetInertializationBone(bone, true);
		}

		Ik.LeftHandAttachSnapFrame = u32(-1);
	}

	attachable_hud_item* item = m_attached_items[0];
	IKinematics* model = m_model != nullptr ? m_model->dcast_PKinematics() : nullptr;
	if (item == nullptr || item->m_model_combined || model == nullptr || actor == nullptr || !CCameraRig::Enabled())
	{
		Ik.LeftIkWeight = 0.f;
		Ik.RetargetWeight = 0.f;
		AimReload.ResetPose();
		return false;
	}

	const u32 state = item->m_parent_hud_item->AnimState();
	bool active = m_attached_items[1] == nullptr && item->LeftIkBone != BI_NONE;
	if (active)
	{
		active = state == CHUDState::eIdle || item->m_parent_hud_item->WpnCanShoot() && (state == CWeapon::eFire || state == CWeapon::eFire2);
	}

	const char* SprintAlias = Sprint.Stage == EHudSprintStage::Start ? "anm_idle_sprint_start" : Sprint.Stage == EHudSprintStage::Loop ? "anm_idle_sprint" : Sprint.Stage == EHudSprintStage::End ? "anm_idle_sprint_end" : nullptr;
	Sprint.LeftFree = SprintAlias != nullptr ? hud_motion_listed(item->LeftHandFreeMotions, SprintAlias) : Sprint.LeftFree && hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL) > 0.f;
	active = active && !item->LeftHandFree && !Sprint.LeftFree && !PreviewGripBlocked;

	Ik.LeftIkWeight = HudBlend::Ramp(Ik.LeftIkWeight, active, item->LeftHandIkBlend, Device.fTimeDelta);


	Ik.LeftGripActive = active;

	const bool has_right = Ik.RightIkChain[0] != BI_NONE && Ik.RightIkChain[1] != BI_NONE && Ik.RightIkChain[2] != BI_NONE;
	const bool has_left = m_attached_items[1] == nullptr && Ik.LeftIkChain[0] != BI_NONE && Ik.LeftIkChain[1] != BI_NONE && Ik.LeftIkChain[2] != BI_NONE;
	const bool has_attach = has_left && item->LeftIkBone != BI_NONE;
	const u16 ancor = m_ancors[item->m_attach_place_idx];

	u16 ancor_parent = ancor;
	while (ancor_parent != BI_NONE && ancor_parent != Ik.RightIkChain[2])
	{
		ancor_parent = model->LL_GetData(ancor_parent).GetParentID();
	}

	const bool ancor_in_right_hand = ancor_parent != BI_NONE;

	const float RetargetLimit = item->RetargetLayerPlaying ? 1.f - hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL) : 1.f;
	const float RetargetFade = std::max(item->IdleFade, 0.1f);
	Ik.RetargetWeight = std::min(HudBlend::Ramp(Ik.RetargetWeight, item->RetargetPlaying, item->RetargetLayerPlaying ? RetargetFade : 0.f, RetargetFade, Device.fTimeDelta), RetargetLimit);
	const bool retarget = Ik.RetargetWeight > 0.f && !ancor_in_right_hand;
	if (retarget)
	{
		Ik.RetargetRightValid = has_right && RetargetHandTarget(ancor, Ik.RightIkChain[2], item->RetargetMotion, ADDITIVE_CHANNEL, item->RetargetReferenceMotion, Ik.RetargetRightTarget);
		Ik.RetargetLeftValid = has_left && RetargetHandTarget(ancor, Ik.LeftIkChain[2], item->RetargetMotion, ADDITIVE_CHANNEL, item->RetargetReferenceMotion, Ik.RetargetLeftTarget);
	}

	F.Item = item;
	F.Model = model;
	F.Weapon = item->m_parent_hud_item->cast_weapon();
	F.State = state;
	F.Active = active;
	F.HasRight = has_right;
	F.HasLeft = has_left;
	F.HasAttach = has_attach;
	F.Ancor = ancor;
	F.AncorInRightHand = ancor_in_right_hand;
	F.RetargetRight = retarget && has_right && Ik.RetargetRightValid;
	F.RetargetLeft = retarget && has_left && Ik.RetargetLeftValid;

	const u8 OverlayMask = u8(~(1 << ADDITIVE_CHANNEL));
	F.AdditiveBase = item->AdditivePlaying && hud_channel_weight(m_model, ADDITIVE_CHANNEL) > 0.f && m_model->LL_GetBoneTransformMasked(ancor, OverlayMask, F.AdditiveAncorBase);
	const u8 OverlayHandMask = u8(~((1 << ADDITIVE_CHANNEL) | (1 << LAYER_ADDITIVE_CHANNEL)));
	Fmatrix OverlayAncorBase;
	const bool Overlay = item->AdditiveOverlay && F.AdditiveBase && m_model->LL_GetBoneTransformMasked(ancor, OverlayHandMask, OverlayAncorBase);
	F.Overlay = Overlay;
	if (Overlay)
	{
		F.OverlayAncorDelta.mul_43(model->LL_GetTransform(ancor), Fmatrix().invert(OverlayAncorBase));
	}

	F.OverlayRight = Overlay && has_right && !ancor_in_right_hand && m_model->LL_GetBoneTransformMasked(Ik.RightIkChain[2], OverlayHandMask, F.OverlayRightBase);
	F.OverlayLeft = Overlay && has_left && !item->LeftHandFree && m_model->LL_GetBoneTransformMasked(Ik.LeftIkChain[2], OverlayHandMask, F.OverlayLeftBase);

	const auto OverlayHand = [&](u16 Wrist, Fmatrix& Base)
	{
		const Fmatrix Relative = Fmatrix().mul_43(Fmatrix().invert(OverlayAncorBase), Base);
		Fmatrix Moved;
		HudOverlayHandRelative(m_model, ancor, Wrist, Relative, Moved);
		Base.mul_43(OverlayAncorBase, Moved);
	};

	if (F.OverlayRight)
	{
		OverlayHand(Ik.RightIkChain[2], F.OverlayRightBase);
	}

	if (F.OverlayLeft)
	{
		OverlayHand(Ik.LeftIkChain[2], F.OverlayLeftBase);
	}

	return true;
}

void player_hud::HandsIkSprint(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	const u16 ancor = F.Ancor;
	const bool has_right = F.HasRight;
	const bool has_left = F.HasLeft;
	const bool ancor_in_right_hand = F.AncorInRightHand;

	const float sprint_layer = hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL);
	const bool sprint_action = sprint_layer > 0.f && F.State != CHUDState::eIdle && !item->RetargetPlaying && !item->AdditivePlaying;
	Sprint.HandWeight = HudBlend::Ramp(Sprint.HandWeight, sprint_action, item->SprintLayerBlend(), Device.fTimeDelta);

	const bool sprint_hands = Sprint.HandWeight > 0.f && sprint_layer > 0.f && SHudSprint::BoneDelta(m_model, ancor, F.SprintAncorDelta);
	F.SprintRight = sprint_hands && has_right && !ancor_in_right_hand && SHudSprint::BaseTransform(m_model, Ik.RightIkChain[2], F.SprintRightBase);
	F.SprintLeft = sprint_hands && has_left && !Sprint.LeftFree && SHudSprint::BaseTransform(m_model, Ik.LeftIkChain[2], F.SprintLeftBase);

	const MotionID sprint_motion = sprint_layer > 0.f && Sprint.Enabled(m_model, m_attached_items[0]) ? hud_dominant_motion(m_model, LAYER_ADDITIVE_CHANNEL) : MotionID();
	F.SprintLayerRight = sprint_motion.valid() && has_right && !ancor_in_right_hand && RetargetHandTarget(ancor, Ik.RightIkChain[2], sprint_motion, LAYER_ADDITIVE_CHANNEL, Sprint.ReferenceMotion, F.SprintRightLayer, hud_smooth(sprint_layer));
	HandsIkSprintLeft(F, sprint_motion, sprint_layer);
	F.SprintLayer = sprint_layer;
	Sprint.SmoothLayer(F);
}

void player_hud::HandsIkSprintLeft(SHandsIkFrame& F, MotionID SprintMotion, float SprintLayer)
{
	const attachable_hud_item* Item = F.Item;
	const bool Valid = SprintMotion.valid() && F.HasLeft;
	const bool Grip = Sprint.LeftFree && Item->SprintLeftGripRange > 0.f;

	float Deviation = 0.f;
	F.SprintLayerLeft = Valid && (!Sprint.LeftFree || Grip) && RetargetHandTarget(F.Ancor, Ik.LeftIkChain[2], SprintMotion, LAYER_ADDITIVE_CHANNEL, Sprint.ReferenceMotion, F.SprintLeftLayer, hud_smooth(SprintLayer), &Deviation);
	F.SprintLeftLayerWeight = Grip ? 1.f - hud_smooth(std::clamp(Deviation / Item->SprintLeftGripRange, 0.f, 1.f)) : 1.f;

	SHudSprintLeftCancel& Cancel = Sprint.LeftCancel;
	if (SprintLayer <= 0.f)
	{
		Cancel.Active = false;
	}

	if (!Cancel.Active)
	{
		return;
	}

	if (Sprint.Stage == EHudSprintStage::Start || Sprint.Stage == EHudSprintStage::Loop)
	{
		Cancel.Weight = HudBlend::Ramp(Cancel.Weight, false, Item->SprintLayerBlend(), Device.fTimeDelta);
		if (Cancel.Weight <= 0.f)
		{
			Cancel.Active = false;
			return;
		}
	}
	else
	{
		Cancel.Weight = 1.f;
		Cancel.Time += Device.fTimeDelta;
		Cancel.Hold = Item->SprintLeftCancelReturn > 0.f ? 1.f - hud_smooth(std::clamp(Cancel.Time / Item->SprintLeftCancelReturn, 0.f, 1.f)) : 0.f;
	}

	F.SprintLeftPinWeight = hud_smooth(Cancel.Weight);
	F.SprintLeftPin = Valid && RetargetHandTarget(F.Ancor, Ik.LeftIkChain[2], SprintMotion, LAYER_ADDITIVE_CHANNEL, Sprint.ReferenceMotion, F.SprintLeftPinTarget, 0.f);
	if (F.SprintLeftPin)
	{
		hud_blend_target(F.SprintLeftPinTarget, Cancel.Relative, Cancel.Hold);
	}
}

static void hud_decay_residual(Fmatrix& Residual, float Factor)
{
	Fquaternion Rotation;
	Rotation.slerp(Fquaternion().set(Residual), Fquaternion().identity(), Factor);
	const Fvector Position = Fvector().mul(Residual.c, 1.f - Factor);
	Residual.rotation(Rotation);
	Residual.c = Position;
}

void player_hud::HandsIkWeaponPose(SHandsIkFrame& F, CActor* actor)
{
	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	CWeapon* weapon = F.Weapon;
	const u32 state = F.State;
	const u16 ancor = F.Ancor;

	const bool reload_state = weapon != nullptr && (state == CWeapon::eReload || state == CWeapon::eMisfire || state == CWeapon::eLightMis || state == CWeapon::eMagCheck
		|| state == CWeapon::eLoadChamber || state == CWeapon::eUnloadChamber || state == CWeapon::eChamberCheck);
	Fmatrix sprint_layer_ancor = Fidentity;
	if (F.SprintLayer > 0.f)
	{
		Fmatrix full, base;
		if (m_model->LL_GetBoneTransformMasked(ancor, u8(-1), full) && m_model->LL_GetBoneTransformMasked(ancor, u8(~(1 << LAYER_ADDITIVE_CHANNEL)), base))
		{
			sprint_layer_ancor.mul_43(full, Fmatrix().invert(base));
		}
	}

	if (state == CHUDState::eIdle && AimReload.Weight <= 0.f && ShotWeight <= 0.f)
	{
		AimReload.Ref.mul_43(Fmatrix().invert(sprint_layer_ancor), model->LL_GetTransform(ancor));
	}

	const bool aim_reload = reload_state && weapon->IsZoomed() && SHudAimReload::Wanted(item);
	AimReload.Update(item, aim_reload);

	const bool firing = weapon != nullptr && (state == CWeapon::eFire || state == CWeapon::eFire2) && (item->ShotVariation > 0.f || item->ShotTwist > 0.f || item->AimShotMotion < 1.f);
	if (F.Overlay || (firing && F.AdditiveBase))
	{
		ShotRef.mul_43(Fmatrix().invert(sprint_layer_ancor), F.AdditiveAncorBase);
	}
	else if (!firing && ShotWeight <= 0.f)
	{
		ShotRef.mul_43(Fmatrix().invert(sprint_layer_ancor), model->LL_GetTransform(ancor));
	}

	const float shot_blend = CCameraRig::ShotAnimBlend();
	ShotWeight = HudBlend::Ramp(ShotWeight, firing, shot_blend, Device.fTimeDelta);

	F.ReloadState = reload_state;
	F.Firing = firing;
	F.SprintLayerAncor = sprint_layer_ancor;
	F.MoveWeapon = F.HasRight && (Ik.LayersIk || AimReload.Weight > 0.f || ShotWeight > 0.f);
}

void player_hud::HandsIkFingerState(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	const bool has_left = F.HasLeft;

	const SHudFingerHold finger = has_left ? hud_finger_hold(m_model, item->FingersMark, 0.3f) : SHudFingerHold();
	if (has_left && Ik.FingerGripBones.empty())
	{
		Ik.FingerGripBones = left_hand_fingers();
	}

	const bool finger_mark = finger.Motion.valid() && finger.Hold > 0.f;
	const bool finger_hold = finger_mark && finger.Idle > 0.f && !Ik.FingerGripBones.empty();
	float finger_settle = 0.f;
	if (F.HasAttach && F.Weapon != nullptr && !Ik.FingerGripBones.empty() && !item->LeftHandFree && (item->AttachPose.size() != 0 || item->LeftHandAttachBlend > 0.f))
	{
		const float settle_time = item->LeftHandAttachBlend > 0.f ? item->LeftHandAttachBlend : std::max(item->IdleFade, 0.1f);
		finger_settle = hud_motion_settle(m_model, item->m_hand_motions, settle_time, !finger_mark && Ik.LeftHandAttachWeight <= 0.f);
	}

	F.Finger = finger;
	F.FingerMark = finger_mark;
	F.FingerHold = finger_hold;
	float finger_start = 0.f;
	if (finger_mark && finger.StartHold > 0.f && !Ik.FingerGripBones.empty())
	{
		if (Ik.FingerStartMotion != finger.Motion || !fsimilar(Ik.FingerStartTime, finger.StartTime) || Ik.FingerStartPose.size() != Ik.FingerGripBones.size())
		{
			Ik.FingerStartMotion = finger.Motion;
			Ik.FingerStartTime = finger.StartTime;
			hud_motion_local_pose(m_model, finger.Motion, finger.StartTime, Ik.FingerGripBones, Ik.FingerStartPose, &Ik.FingerStartPos);
		}

		if (Ik.FingerStartPose.size() == Ik.FingerGripBones.size() && Ik.FingerStartPos.size() == Ik.FingerGripBones.size())
		{
			finger_start = hud_smooth(finger.StartHold) * finger.Hold;
		}
	}

	F.FingerSettle = finger_settle;
	F.FingerStart = finger_start;
}

void player_hud::HandsIkWeaponDelta(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	CWeapon* weapon = F.Weapon;
	const u16 ancor = F.Ancor;
	const bool move_weapon = F.MoveWeapon;
	const Fmatrix& sprint_layer_ancor = F.SprintLayerAncor;

	Fmatrix& delta = F.Delta;
	delta = move_weapon && Ik.LayersIk ? Ik.LayersIkOffset : Fidentity;
	if (move_weapon && AimReload.Weight > 0.f)
	{
		const Fmatrix anim = Fmatrix().mul_43(Fmatrix().invert(sprint_layer_ancor), model->LL_GetTransform(ancor));

		if (F.ReloadState)
		{
			AimReload.UpdateRelease(hud_cycle_progress(m_model));
		}

		const float release = AimReload.Release;
		const float blend = AimReload.Weight * hud_smooth(release);
		const float weight = hud_smooth(blend);
		const Fmatrix motion = Fmatrix().mul_43(Fmatrix().invert(AimReload.Ref), anim);
		const Fmatrix damped = Fmatrix().mul_43(AimReload.Ref, scale_layer_offset(motion, lerp(1.f, item->AimReloadMotion, weight)));
		const Fmatrix correction = Fmatrix().mul_43(damped, Fmatrix().invert(anim));
		delta.mulB_43(Fmatrix().mul_43(Fmatrix().mul_43(sprint_layer_ancor, correction), Fmatrix().invert(sprint_layer_ancor)));
	}

	if (move_weapon && ShotWeight > 0.f)
	{
		const Fmatrix anim = Fmatrix().mul_43(Fmatrix().invert(sprint_layer_ancor), model->LL_GetTransform(ancor));

		if (F.Firing)
		{
			const float progress = hud_cycle_progress(m_model);
			const float tail = CCameraRig::ShotAnimTail();
			ShotRelease = progress < 0.f || tail <= 0.f ? 1.f : std::clamp((1.f - progress) / tail, 0.f, 1.f);
		}

		const float blend = ShotWeight * hud_smooth(ShotRelease);
		const float weight = hud_smooth(blend);
		const Fmatrix motion = Fmatrix().mul_43(Fmatrix().invert(ShotRef), anim);

		const float aim = lerp(1.f, item->AimShotMotion, weapon->GetAimFactor());

		Fvector rot;
		motion.getXYZ(rot);
		const float climb = rot.x;
		rot.x *= lerp(1.f, ShotRotScale.x * aim, weight);
		rot.y = rot.y * lerp(1.f, ShotRotScale.y * aim, weight) + climb * ShotTwist.x * aim * weight;
		rot.z = rot.z * lerp(1.f, ShotRotScale.z * aim, weight) + climb * ShotTwist.y * aim * weight;

		Fvector pos = motion.c;
		pos.x *= lerp(1.f, ShotPosScale.x * aim, weight);
		pos.y *= lerp(1.f, ShotPosScale.y * aim, weight);
		pos.z *= lerp(1.f, ShotPosScale.z * aim, weight);

		Fmatrix varied;
		varied.setXYZ(rot);
		varied.translate_over(pos);
		const Fmatrix correction = Fmatrix().mul_43(Fmatrix().mul_43(ShotRef, varied), Fmatrix().invert(anim));
		delta.mulB_43(Fmatrix().mul_43(Fmatrix().mul_43(sprint_layer_ancor, correction), Fmatrix().invert(sprint_layer_ancor)));
	}
}

void player_hud::HandsIkHandTargets(SHandsIkFrame& F)
{
	IKinematics* model = F.Model;
	const u16 ancor = F.Ancor;
	const Fmatrix& delta = F.Delta;

	const float retarget_weight = hud_smooth(Ik.RetargetWeight);
	const auto retarget_blend = [&](Fmatrix& target, const Fmatrix& local)
	{
		hud_blend_target(target, Fmatrix().mul_43(delta, Fmatrix().mul_43(model->LL_GetTransform(ancor), local)), retarget_weight);
	};

	const float sprint_hand_weight = hud_smooth(Sprint.HandWeight);
	const auto sprint_blend = [&](Fmatrix& target, const Fmatrix& base)
	{
		hud_blend_target(target, Fmatrix().mul_43(delta, Fmatrix().mul_43(F.SprintAncorDelta, base)), sprint_hand_weight);
	};

	const float sprint_layer_weight = F.SprintLayer > 0.f ? 1.f : 0.f;
	const auto sprint_layer_blend = [&](Fmatrix& target, const Fmatrix& local, float weight)
	{
		hud_blend_target(target, Fmatrix().mul_43(delta, Fmatrix().mul_43(model->LL_GetTransform(ancor), local)), sprint_layer_weight * weight);
	};

	Fmatrix& right_target = F.RightTarget;
	right_target = F.HasRight ? Fmatrix().mul_43(delta, model->LL_GetTransform(Ik.RightIkChain[2])) : Fidentity;
	if (F.SprintLayerRight)
	{
		sprint_layer_blend(right_target, F.SprintRightLayer, 1.f);
	}

	if (F.SprintRight)
	{
		sprint_blend(right_target, F.SprintRightBase);
	}

	if (F.OverlayRight)
	{
		right_target.mul_43(delta, Fmatrix().mul_43(F.OverlayAncorDelta, F.OverlayRightBase));
	}

	if (F.RetargetRight)
	{
		retarget_blend(right_target, Ik.RetargetRightTarget);
	}

	Fmatrix& left_target = F.LeftTarget;
	left_target = F.HasLeft ? Fmatrix().mul_43(delta, model->LL_GetTransform(Ik.LeftIkChain[2])) : Fidentity;
	if (F.SprintLayerLeft)
	{
		sprint_layer_blend(left_target, F.SprintLeftLayer, F.SprintLeftLayerWeight);
	}

	if (F.SprintLeftPin)
	{
		sprint_layer_blend(left_target, F.SprintLeftPinTarget, F.SprintLeftPinWeight);
	}

	if (F.SprintLeft)
	{
		sprint_blend(left_target, F.SprintLeftBase);
	}

	if (F.OverlayLeft)
	{
		left_target.mul_43(delta, Fmatrix().mul_43(F.OverlayAncorDelta, F.OverlayLeftBase));
	}

	if (F.RetargetLeft)
	{
		retarget_blend(left_target, Ik.RetargetLeftTarget);
	}

	Sprint.SmoothBase(m_model, F);
}

void player_hud::HandsIkAttachTarget(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	const u16 ancor = F.Ancor;
	const bool active = F.Active;
	const Fmatrix& delta = F.Delta;
	const Fmatrix& left_target = F.LeftTarget;

	Fmatrix& attach_target = F.AttachTarget;
	float& attach_weight = F.AttachWeight;
	float& attach_target_weight = F.AttachTargetWeight;
	attach_target = left_target;
	attach_weight = hud_smooth(Ik.LeftIkWeight);
	attach_target_weight = -1.f;
	if (!F.HasAttach)
	{
		return;
	}

	Fmatrix attach;
	attach.mul_43(Fmatrix().invert(m_transform), Fmatrix().mul_43(item->m_item_transform, item->m_model->LL_GetTransform(item->LeftIkBone)));
	attach.mulA_43(delta);
	const Fmatrix animated = Fmatrix().mul_43(Fmatrix().invert(attach), left_target);

	const auto follow_attach = [&](bool snap)
	{
		const float follow = CCameraRig::LeftHandIkFollow();
		if (snap || follow <= 0.f)
		{
			item->LeftIkOffset.set(animated);
			return;
		}

		const float factor = HudBlend::Smooth(follow, Device.fTimeDelta);
		Fquaternion q_from, q_to, q_blend;
		q_from.set(item->LeftIkOffset);
		q_to.set(animated);
		q_blend.slerp(q_from, q_to, factor);

		Fvector position;
		position.lerp(item->LeftIkOffset.c, animated.c, factor);
		item->LeftIkOffset.rotation(q_blend);
		item->LeftIkOffset.c = position;
	};

	if (item->LeftHandAttachBlend <= 0.f)
	{
		if (active && !item->LeftIkFixed)
		{
			follow_attach(Ik.LeftIkWeight < 1.f);
		}

		attach_target.mul_43(attach, item->LeftIkOffset);
		return;
	}

	if (!item->LeftHandAttachRefTried)
	{
		item->LeftHandAttachRefTried = true;
		item->LeftHandAttachRefValid = LeftHandAttachReference(item);
		Ik.ArmRetargetMotion = MotionID();
	}
	else if (item->LeftHandAttachRefValid && (Ik.LeftHandAttachReaching || Ik.LeftHandAttachWeight > 0.f || Ik.LeftHandAttachHold > 0.f))
	{
		LeftHandAttachReference(item);
	}

	if (!item->LeftHandAttachRefValid)
	{
		Ik.LeftHandAttachReaching = false;
	}

	const bool hold = Ik.LeftHandAttachHold > 0.f && item->LeftHandAttachRefValid;
	Ik.LeftHandAttachHold = std::max(Ik.LeftHandAttachHold - Device.fTimeDelta, 0.f);

	const float release_time = std::max(CCameraRig::HudInertialization(), 0.2f);
	Ik.LeftHandAttachWeight = HudBlend::Ramp(Ik.LeftHandAttachWeight, Ik.LeftHandAttachReaching || hold, item->LeftHandAttachBlend, release_time, Device.fTimeDelta);

	float mark_weight = 0.f;
	MotionID mark_motion;
	bool mark_pre_start = false;
	const bool mark_attach = hud_attach_mark(m_model, item->IkMark, item->LeftHandAttachBlend, mark_weight, mark_motion, mark_pre_start);
	if (mark_attach)
	{
		Ik.LeftHandAttachWeight = mark_weight;
	}

	attach_weight = hud_smooth(Ik.LeftHandAttachWeight);

	float arm_target = 1.f;
	if (Ik.LeftHandAttachAnimatedArm)
	{
		const float deviation = hud_ancor_deviation(model->LL_GetTransform(ancor), item->LeftHandAttachAncorRef);
		arm_target = std::clamp((1.f - deviation) / 0.7f, 0.f, 1.f);
	}

	Ik.LeftHandAttachArmWeight = Ik.LeftHandAttachWeight <= 0.f || mark_attach ? arm_target
		: Ik.LeftHandAttachArmWeight + std::clamp(arm_target - Ik.LeftHandAttachArmWeight, -Device.fTimeDelta / item->LeftHandAttachBlend, Device.fTimeDelta / item->LeftHandAttachBlend);
	attach_target.mul_43(attach, item->LeftHandAttachRef);

	if (!mark_attach || !mark_pre_start)
	{
		Ik.AttachOffsetMotion = MotionID();
		return;
	}

	if (mark_weight <= 0.f)
	{
		return;
	}

	IKinematicsAnimated* weapon_model = item->m_model->dcast_PKinematicsAnimated();
	const MotionID weapon_motion = weapon_model != nullptr ? hud_dominant_motion(weapon_model) : MotionID();
	if (Ik.AttachOffsetMotion != mark_motion)
	{
		Ik.AttachOffsetMotion = mark_motion;
		Ik.AttachOffsetSampled = weapon_motion.valid() && HandAttachRelative(item, mark_motion, 0.f, weapon_motion, 0.f, Ik.AttachOffsetRef);
		if (!Ik.AttachOffsetSampled)
		{
			Ik.AttachOffsetRef.set(animated);
		}
	}

	Fmatrix source = animated;
	if (Ik.AttachOffsetSampled)
	{
		const float hands_time = std::max(hud_motion_time(m_model, mark_motion), 0.f);
		const float weapon_time = std::max(hud_motion_time(weapon_model, weapon_motion), 0.f);
		if (!HandAttachRelative(item, mark_motion, hands_time, weapon_motion, weapon_time, source))
		{
			source = animated;
		}
	}

	const float offset = hud_smooth(mark_weight);
	Fmatrix grip_rotation, ref_rotation, source_rotation;
	grip_rotation.rotation(Fquaternion().set(item->LeftHandAttachRef));
	ref_rotation.rotation(Fquaternion().set(Ik.AttachOffsetRef));
	source_rotation.rotation(Fquaternion().set(source));

	Fquaternion q_offset;
	q_offset.slerp(Fquaternion().identity(), Fquaternion().set(Fmatrix().mul_43(grip_rotation, Fmatrix().invert(ref_rotation))), offset);

	Fmatrix shifted;
	shifted.mul_43(Fmatrix().rotation(q_offset), source_rotation);
	shifted.c.mad(source.c, Fvector().sub(item->LeftHandAttachRef.c, Ik.AttachOffsetRef.c), offset);
	attach_target.mul_43(attach, shifted);
	attach_target_weight = 1.f;
}

static bool hud_is_unit(const Fvector& Value)
{
	return Value.similar(Fvector().set(1.f, 1.f, 1.f));
}

static Fmatrix hud_weighted_transform(const Fmatrix& Transform, float Weight)
{
	Fquaternion Rotation;
	Rotation.slerp(Fquaternion().identity(), Fquaternion().set(Transform), Weight);
	Fmatrix Result;
	Result.rotation(Rotation);
	Result.c.mul(Transform.c, Weight);
	return Result;
}

static bool hud_left_follow_custom(const attachable_hud_item* Item)
{
	return Item->LeftFollowLag > 0.f || Item->LeftFollowTurn < 1.f || !hud_is_unit(Item->LeftFollowMove) || Item->LeftFollowMoveRot < 1.f || !hud_is_unit(Item->LeftBobPos) || !hud_is_unit(Item->LeftBobRot);
}

static bool hud_turn_pivot_custom(const SHandsIkFrame& F)
{
	const attachable_hud_item* Item = F.Item;
	if (F.Weapon == nullptr || F.Weapon->cast_weapon_knife() != nullptr || Item->m_model_combined)
	{
		return false;
	}

	return !fis_zero(Item->TurnPivotLook.square_magnitude()) || Item->TurnPivotGripLag < 1.f || !fis_zero(Item->TurnPivotLift) || !fis_zero(Item->TurnPivotLiftPos) || !fis_zero(Item->TurnTilt);
}

static bool hud_hand_sway_custom(const attachable_hud_item* Item)
{
	return !fis_zero(Item->LeftSwayLook.square_magnitude()) || !fis_zero(Item->LeftSwayBob.square_magnitude()) || !fis_zero(Item->LeftSwayJump.square_magnitude()) || !fis_zero(Item->RightSwayLook.square_magnitude()) || !fis_zero(Item->RightSwayJump.square_magnitude());
}

bool player_hud::HandsIkSwayNeeded(const SHandsIkFrame& F) const
{
	if (!F.HasLeft && !F.HasRight)
	{
		return false;
	}

	const auto Moving = [](const SHudVectorSpring& Spring)
	{
		return Spring.Value.square_magnitude() > EPS_S || Spring.Velocity.square_magnitude() > EPS_S;
	};

	return hud_hand_sway_custom(F.Item) || hud_left_follow_custom(F.Item) || hud_turn_pivot_custom(F) || Moving(Ik.TurnPivot.Lag) || Moving(Ik.TurnPivot.Lift) || std::abs(Ik.TurnPivot.Tilt) > EPS_S || std::abs(Ik.TurnPivot.TiltVelocity) > EPS_S || Moving(Ik.LeftSway) || Moving(Ik.RightSway);
}

Fmatrix player_hud::LeftFollowTarget(const SHandsIkFrame& F, const Fmatrix& CamTrans, const Fmatrix& Seen, CActor* actor)
{
	attachable_hud_item* Item = F.Item;
	Fmatrix Target = Fmatrix().mul_43(Seen, Fmatrix().invert(MovementLayersOffset));

	if (!hud_is_unit(Item->LeftFollowMove) || Item->LeftFollowMoveRot < 1.f)
	{
		const auto& Tune = Item->m_measures.m_hands_positions.hands_offsets_tune;
		const Fquaternion Current = HudRotation::FromEuler(Item->hands_attach_rot());
		Fquaternion Wanted;
		Wanted.slerp(HudRotation::FromEuler(Tune[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault]), Current, Item->LeftFollowMoveRot);

		Fmatrix Attach;
		Attach.rotation(Current);
		Attach.c = Item->hands_attach_pos();
		Fmatrix Rest;
		Rest.rotation(Wanted);
		const Fvector& RestPos = Tune[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
		Rest.c.sub(Item->hands_attach_pos(), RestPos).mul(Item->LeftFollowMove).add(RestPos);
		Target.mulB_43(Fmatrix().mul_43(Fmatrix().invert(Attach), Rest));
	}

	if (Item->LeftFollowTurn < 1.f)
	{
		Fmatrix LookPre, Look;
		if (Item->m_parent_hud_item->GetHudLookInertia(LookPre, Look))
		{
			const Fmatrix Pre = Fmatrix().mul_43(Fmatrix().invert(CamTrans), LookPre);
			const Fmatrix Scaled = Fmatrix().mul_43(scale_layer_offset(Look, Item->LeftFollowTurn), Fmatrix().invert(Look));
			Target.mulA_43(Fmatrix().mul_43(Fmatrix().mul_43(Pre, Scaled), Fmatrix().invert(Pre)));
		}

		Target.c.mad(actor->Cameras().Rig().LookSwayOffset(), Item->LeftFollowTurn - 1.f);
	}

	return Target;
}

void player_hud::HandsIkLeftFollow(SHandsIkFrame& F, const Fmatrix& CamTrans, float Free, CActor* actor)
{
	const attachable_hud_item* Item = F.Item;
	const Fmatrix Hud = Fmatrix().mul_43(Fmatrix().invert(CamTrans), m_transform);
	const Fmatrix Seen = Fmatrix().mul_43(Hud, F.Delta);
	const bool Continuous = Ik.LeftFollow.Frame + 1 == Device.dwFrame;
	Ik.LeftFollow.Frame = Device.dwFrame;

	if (!hud_left_follow_custom(Item))
	{
		return;
	}

	const Fmatrix Target = LeftFollowTarget(F, CamTrans, Seen, actor);
	const Fquaternion TargetRotation = Fquaternion().set(Target);
	if (!Continuous || Item->LeftFollowLag <= 0.f)
	{
		Ik.LeftFollow = { TargetRotation, Target.c, {}, {}, Device.dwFrame };
	}
	else
	{
		Fvector Offset = HudRotation::Delta(TargetRotation, Ik.LeftFollow.Rotation);
		HudSpring::Follow(Offset, Ik.LeftFollow.RotationVelocity, zero_vel, Item->LeftFollowLag, Item->HandSwayDamping, Device.fTimeDelta);
		Ik.LeftFollow.Rotation = HudRotation::Apply(Offset, TargetRotation);
		HudSpring::Follow(Ik.LeftFollow.Position, Ik.LeftFollow.PositionVelocity, Target.c, Item->LeftFollowLag, Item->HandSwayDamping, Device.fTimeDelta);
	}

	if (Free <= 0.f)
	{
		return;
	}

	Fmatrix Desired;
	Desired.rotation(Ik.LeftFollow.Rotation);
	Desired.c = Ik.LeftFollow.Position;

	Fvector LayerRot;
	MovementLayersOffset.getXYZ(LayerRot);
	Fmatrix Bob;
	Bob.setXYZ(Fvector().mul(LayerRot, Item->LeftBobRot));
	Bob.translate_over(Fvector().mul(MovementLayersOffset.c, Item->LeftBobPos));
	Desired.mulB_43(Bob);

	const Fmatrix Correction = Fmatrix().mul_43(Fmatrix().mul_43(Fmatrix().invert(Hud), Desired), Fmatrix().mul_43(Fmatrix().invert(Seen), Hud));
	F.LeftTarget.mulA_43(hud_weighted_transform(Correction, Free));
}

void player_hud::UpdateHandJump(CActor* actor, float HalfLife)
{
	const u32 State = actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);
	const float VerticalSpeed = actor->character_physics_support()->movement()->GetVelocity().y;
	const bool Airborne = (State & (mcJump | mcFall)) != 0;

	if ((State & mcJump) != 0 && (Ik.HandJump.StatePrev & mcJump) == 0)
	{
		Ik.HandJump.Time = 0.f;
		Ik.HandJump.Power = 1.f;
	}

	constexpr u32 LandingFlags = mcLanding | mcLanding2;
	if ((State & LandingFlags) != 0 && (Ik.HandJump.StatePrev & LandingFlags) == 0)
	{
		Ik.HandJump.Time = 0.f;
		Ik.HandJump.Power = -std::clamp(-Ik.HandJump.FallSpeed / 6.f, 0.f, 1.f);
	}

	Ik.HandJump.StatePrev = State;
	Ik.HandJump.FallSpeed = Airborne ? std::min(VerticalSpeed, Ik.HandJump.FallSpeed) : 0.f;
	Ik.HandJump.Time += Device.fTimeDelta;
	if (!Airborne && Ik.HandJump.Time >= 0.15f)
	{
		Ik.HandJump.Power = 0.f;
	}

	const bool Rising = Airborne && VerticalSpeed > 0.f && Ik.HandJump.Power >= 1.f;
	const float Target = Rising || Ik.HandJump.Time < 0.15f ? Ik.HandJump.Power : 0.f;
	HudSpring::Step(Ik.HandJump.Weight, Ik.HandJump.Velocity, Target, std::abs(Target) > std::abs(Ik.HandJump.Weight) ? HalfLife * 0.5f : HalfLife, 1.f, Device.fTimeDelta);
}

static Fmatrix hud_pivot_rotation(const Fvector& Hpb, const Fvector& Pivot)
{
	Fmatrix Rotation;
	Rotation.setHPB(Hpb.x, Hpb.y, Hpb.z);
	Fvector Rotated;
	Rotation.transform_dir(Rotated, Pivot);
	Rotation.c.sub(Pivot, Rotated);
	return Rotation;
}

static Fvector hud_look_sway(CActor* actor, const Fvector& Look)
{
	const float Yaw = std::clamp(actor->fFPCamYawMagnitude, -PI_MUL_2, PI_MUL_2);
	const float Pitch = std::clamp(actor->fFPCamPitchMagnitude, -PI_MUL_2, PI_MUL_2);
	return Fvector().set(-Yaw * Look.y, -Pitch * Look.x, Yaw * Look.z);
}

static Fvector hud_jump_sway(const Fvector& Jump, float Weight)
{
	return Fvector().set(Jump.y, Jump.x, Jump.z).mul(Weight * PI / 180.f);
}

void player_hud::HandsIkLeftSway(SHandsIkFrame& F, const Fmatrix& CamTrans, CActor* actor)
{
	const attachable_hud_item* Item = F.Item;
	if (!F.HasLeft)
	{
		Ik.LeftSway = {};
		return;
	}

	const float AttachWeight = F.HasAttach ? std::clamp(F.AttachTargetWeight >= 0.f ? F.AttachTargetWeight : F.AttachWeight, 0.f, 1.f) : 0.f;
	const float Free = 1.f - hud_smooth(AttachWeight);

	HandsIkLeftFollow(F, CamTrans, Free, actor);

	const Fvector Bob = Fvector().mul(MovementLayersOffset.c, 100.f);
	const Fvector& BobScale = Item->LeftSwayBob;
	const Fvector Target = hud_look_sway(actor, Item->LeftSwayLook).add(Fvector().set(-Bob.x * BobScale.y, Bob.y * BobScale.x, Bob.x * BobScale.z)).mul(Free * PI / 180.f);
	HudSpring::Follow(Ik.LeftSway.Value, Ik.LeftSway.Velocity, Target, Item->HandSwayInertia, Item->HandSwayDamping, Device.fTimeDelta);

	const Fvector Hpb = hud_jump_sway(Item->LeftSwayJump, Ik.HandJump.Weight * Free).add(Ik.LeftSway.Value);
	if (Hpb.square_magnitude() > EPS_S)
	{
		F.LeftTarget.mulA_43(hud_pivot_rotation(Hpb, F.LeftTarget.c));
	}
}

void player_hud::HandsIkRightSway(SHandsIkFrame& F, CActor* actor)
{
	const attachable_hud_item* Item = F.Item;
	if (!F.HasRight)
	{
		Ik.RightSway = {};
		return;
	}

	const Fvector Target = hud_look_sway(actor, Item->RightSwayLook).mul(PI / 180.f);
	HudSpring::Follow(Ik.RightSway.Value, Ik.RightSway.Velocity, Target, Item->HandSwayInertia, Item->HandSwayDamping, Device.fTimeDelta);

	const Fvector Hpb = hud_jump_sway(Item->RightSwayJump, Ik.HandJump.Weight).add(Ik.RightSway.Value);
	if (Hpb.square_magnitude() <= EPS_S)
	{
		return;
	}

	const Fmatrix Rotation = hud_pivot_rotation(Hpb, F.RightTarget.c);
	F.RightTarget.mulA_43(Rotation);
	if (!F.AncorInRightHand)
	{
		F.Delta.mulA_43(Rotation);
	}
}

static float hud_soft_limit(float Value, float Max)
{
	return Max > 0.f ? Max * tanhf(Value / Max) : Value;
}

static Fmatrix hud_rotation_about(const Fmatrix& Rotation, const Fvector& Pivot)
{
	Fmatrix Result = Rotation;
	Fvector Rotated;
	Result.transform_dir(Rotated, Pivot);
	Result.c.sub(Pivot, Rotated);
	return Result;
}

void player_hud::UpdateTurnPivot(const attachable_hud_item* Item, CActor* actor)
{
	SHudTurnPivot& T = Ik.TurnPivot;
	const float Dt = Device.fTimeDelta;
	const float Aim = lerp(1.f, Item->AimTurnPivotScale, Item->m_parent_hud_item->GetAimFactor());
	const float RateSmooth = HudBlend::Smooth(Item->TurnPivotInertia * 0.5f, Dt);
	T.Rate.x += (std::clamp(actor->fFPCamYawMagnitude, -PI_MUL_2, PI_MUL_2) - T.Rate.x) * RateSmooth;
	T.Rate.y += (std::clamp(actor->fFPCamPitchMagnitude, -PI_MUL_2, PI_MUL_2) - T.Rate.y) * RateSmooth;
	const float Yaw = T.Rate.x * Aim;
	const float Pitch = T.Rate.y * Aim;

	const float Max = Item->TurnPivotMax;
	const Fvector& Look = Item->TurnPivotLook;
	const Fvector LagTarget = { deg2rad(hud_soft_limit(-Yaw * Look.y, Max)), deg2rad(hud_soft_limit(-Pitch * Look.x, Max)), deg2rad(hud_soft_limit(Yaw * Look.z, Max)) };
	HudSpring::Follow(T.Lag.Value, T.Lag.Velocity, LagTarget, Item->TurnPivotInertia, Item->TurnPivotDamping, Dt);
	HudSpring::Step(T.Tilt, T.TiltVelocity, deg2rad(hud_soft_limit(-Yaw * Item->TurnTilt, Item->TurnTiltMax)), Item->TurnPivotInertia, 1.f, Dt);

	const float Speed = std::abs(Yaw);
	const float Envelope = Speed > T.LiftSpeed ? Item->TurnPivotLiftInertia : Item->TurnPivotLiftInertia * std::max(Item->TurnPivotLiftFall, 1.f);
	T.LiftSpeed += (Speed - T.LiftSpeed) * HudBlend::Smooth(Envelope, Dt);
	const Fvector LiftTarget = { deg2rad(hud_soft_limit(T.LiftSpeed * Item->TurnPivotLift, Max)), hud_soft_limit(T.LiftSpeed * Item->TurnPivotLiftPos, Item->TurnPivotLiftMax), 0.f };
	HudSpring::Follow(T.Lift.Value, T.Lift.Velocity, LiftTarget, Item->TurnPivotLiftInertia, 1.f, Dt);
}

Fvector player_hud::TurnPivotPoint(const SHandsIkFrame& F) const
{
	const attachable_hud_item* Item = F.Item;
	Fvector Pivot;
	if (Item->TurnPivotBone != BI_NONE)
	{
		Pivot = Item->m_model->LL_GetTransform(Item->TurnPivotBone).c;
		Item->m_attach_offset.transform_tiny(Pivot);
		F.Model->LL_GetTransform(F.Ancor).transform_tiny(Pivot);
	}
	else
	{
		Pivot = F.Model->LL_GetTransform(F.HasRight ? Ik.RightIkChain[2] : F.Ancor).c;
	}

	F.Delta.transform_tiny(Pivot);
	return Pivot;
}

Fvector player_hud::TurnGripShift(const attachable_hud_item* Item, const Fvector& Pivot, const Fmatrix& CamTrans, CActor* actor) const
{
	Fvector World;
	m_transform.transform_tiny(World, Pivot);
	Fvector Settled = World;
	Fmatrix LookPre, Look;
	if (Item->m_parent_hud_item->GetHudLookInertia(LookPre, Look))
	{
		Fmatrix().mul_43(Fmatrix().mul_43(LookPre, Fmatrix().invert(Look)), Fmatrix().invert(LookPre)).transform_tiny(Settled);
	}

	Fvector Sway;
	CamTrans.transform_dir(Sway, actor->Cameras().Rig().LookSwayOffset());
	Settled.sub(Sway);

	Fvector Shift;
	Fmatrix().invert(m_transform).transform_dir(Shift, Fvector().sub(Settled, World));
	return Shift.mul(1.f - Item->TurnPivotGripLag);
}

void player_hud::HandsIkTurnPivot(SHandsIkFrame& F, const Fmatrix& CamTrans, CActor* actor)
{
	const attachable_hud_item* Item = F.Item;
	if (!hud_turn_pivot_custom(F))
	{
		Ik.TurnPivot = {};
		return;
	}

	UpdateTurnPivot(Item, actor);
	const SHudTurnPivot& T = Ik.TurnPivot;

	const Fmatrix ModelToCam = Fmatrix().mul_43(Fmatrix().invert(CamTrans), m_transform);
	const Fmatrix CamToModel = Fmatrix().invert(ModelToCam);
	Fvector Pivot = TurnPivotPoint(F);

	Fvector Shift = Item->TurnPivotGripLag < 1.f ? TurnGripShift(Item, Pivot, CamTrans, actor) : Fvector().set(0.f, 0.f, 0.f);
	Fvector Up;
	CamToModel.transform_dir(Up, Fvector().set(0.f, T.Lift.Value.y, 0.f));
	Shift.add(Up);
	F.Delta.mulA_43(Fmatrix().translate(Shift));
	Pivot.add(Shift);

	const Fmatrix CamRotation = Fmatrix().setHPB(T.Lag.Value.x, T.Lag.Value.y + T.Lift.Value.x, T.Lag.Value.z + T.Tilt);
	const Fmatrix Rotation = Fmatrix().mul_43(Fmatrix().mul_43(CamToModel, CamRotation), ModelToCam);
	F.Delta.mulA_43(hud_rotation_about(Rotation, Pivot));
}

void player_hud::HandsIkItemBones(SHandsIkFrame& F, const Fmatrix* WristBefore, const Fmatrix& AncorBefore)
{
	attachable_hud_item* Item = F.Item;
	IKinematics* ItemModel = Item->m_model;
	if (ItemModel == nullptr || Item->m_model_combined || Item->LeftHandItemBones.empty() && Item->RightHandItemBones.empty())
	{
		return;
	}

	const Fmatrix ItemBefore = Fmatrix().mul_43(AncorBefore, Item->m_attach_offset);
	const Fmatrix ItemAfterInverse = Fmatrix().invert(Fmatrix().mul_43(F.Model->LL_GetTransform(F.Ancor), Item->m_attach_offset));
	const u16 Root = ItemModel->LL_GetBoneRoot();
	const Fvector RootPos = ItemModel->LL_GetTransform(Root).c;
	const Fvector RootBind = Fmatrix().invert(ItemModel->LL_GetData(Root).m2b_transform).c;

	const auto Follow = [&](const xr_vector<u16>& Bones, u16 Wrist, const Fmatrix& Before)
	{
		if (Bones.empty() || Wrist == BI_NONE)
		{
			return;
		}

		const Fmatrix Hand = Fmatrix().mul_43(F.Model->LL_GetTransform(Wrist), Fmatrix().invert(Before));
		const Fmatrix Delta = Fmatrix().mul_43(Fmatrix().mul_43(ItemAfterInverse, Hand), ItemBefore);

		for (u16 Bone : Bones)
		{
			const float Rest = Fmatrix().invert(ItemModel->LL_GetData(Bone).m2b_transform).c.distance_to(RootBind);
			const float Separation = ItemModel->LL_GetTransform(Bone).c.distance_to(RootPos) - Rest;
			const float Weight = hud_smooth(std::clamp((Separation - 0.01f) / 0.03f, 0.f, 1.f));
			if (Weight <= 0.f)
			{
				continue;
			}

			CCameraRig::TransformBone(ItemModel, Bone, hud_weighted_transform(Delta, Weight));
		}
	};

	Follow(Item->LeftHandItemBones, F.HasLeft ? Ik.LeftIkChain[2] : BI_NONE, WristBefore[0]);
	Follow(Item->RightHandItemBones, F.HasRight ? Ik.RightIkChain[2] : BI_NONE, WristBefore[1]);
}

void player_hud::HandsIkReach(SHandsIkFrame& F)
{
	IKinematics* model = F.Model;
	const bool has_right = F.HasRight;
	const bool has_left = F.HasLeft;

	Fvector shift = zero_vel;
	const auto reach = [&](const u16* chain, const Fvector& target)
	{
		const Fvector root = model->LL_GetTransform(chain[0]).c;
		const float length = (root.distance_to(model->LL_GetTransform(chain[1]).c) + model->LL_GetTransform(chain[1]).c.distance_to(model->LL_GetTransform(chain[2]).c)) * 0.995f;
		const Fvector dir = Fvector().sub(target, root).sub(shift);
		const float distance = dir.magnitude();
		if (distance > length)
		{
			shift.mad(dir, 1.f - length / distance);
		}
	};

	F.TargetWeight = F.AttachTargetWeight >= 0.f ? F.AttachTargetWeight : F.AttachWeight;
	const Fvector left_goal = Fvector().lerp(F.LeftTarget.c, F.AttachTarget.c, F.HasAttach ? F.TargetWeight : 0.f);
	for (int i = 0; i < 4; i++)
	{
		if (has_right)
		{
			reach(Ik.RightIkChain, F.RightTarget.c);
		}

		if (has_left)
		{
			reach(Ik.LeftIkChain, left_goal);
		}
	}

	if (shift.square_magnitude() > EPS_S)
	{
		Fvector world_shift;
		m_transform.transform_dir(world_shift, shift);
		m_transform.c.add(world_shift);
		F.RightTarget.c.sub(shift);
		F.LeftTarget.c.sub(shift);
		F.AttachTarget.c.sub(shift);
		F.Delta.mulA_43(Fmatrix().translate(Fvector().invert(shift)));
	}
}

void player_hud::HandsIkSolve(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	const bool has_right = F.HasRight;
	const bool has_attach = F.HasAttach;
	const float attach_weight = F.AttachWeight;
	const float target_weight = F.TargetWeight;
	const Fmatrix& left_target = F.LeftTarget;
	const Fmatrix& attach_target = F.AttachTarget;

	if (!F.AncorInRightHand || !has_right)
	{
		CCameraRig::TransformBone(model, F.Ancor, F.Delta);
	}

	if (has_right)
	{
		CCameraRig::SolveLimb(model, Ik.RightIkChain, F.RightTarget, 1.f);
	}

	if (!F.HasLeft)
	{
		return;
	}

	const bool attach_pose = has_attach && item->LeftHandAttachBlend > 0.f && item->LeftHandAttachRefValid && attach_weight > 0.f;
	const Fvector ElbowOffset = Fvector().mul(item->AimLeftElbowOffset, item->m_parent_hud_item->GetAimFactor());

	F.ArmBones = left_arm_subtree();
	F.AttachPose = attach_pose;
	const xr_vector<u16>& arm_bones = F.ArmBones;
	if (attach_pose && !arm_bones.empty() && item->LeftHandAttachPoseRef.size() == arm_bones.size())
	{
		const xr_vector<u16> upper_bones = left_arm_blend_bones();
		const float arm = hud_smooth(Ik.LeftHandAttachArmWeight);
		const bool retarget_arm = Ik.LeftHandAttachAnimatedArm && ArmRetargetReference(item, arm_bones);
		{
			const Fmatrix current = model->LL_GetTransform(arm_bones[0]);
			Fmatrix attach_root = item->LeftHandAttachArmRootRef;
			Fmatrix sprint_root_delta;
			if (F.SprintLayer > 0.f && SHudSprint::BoneDelta(m_model, model->LL_GetData(arm_bones[0]).GetParentID(), sprint_root_delta))
			{
				attach_root.mulA_43(sprint_root_delta);
			}

			Fquaternion q_current, q_attach, q_arm, q_result;
			q_current.set(current);
			q_attach.set(attach_root);
			Fvector arm_pos = attach_root.c;
			float root_weight = attach_weight * arm;
			if (retarget_arm)
			{
				Fquaternion q_retarget;
				q_retarget.set(Fmatrix().mul_43(Fmatrix().mul_43(Fmatrix().rotation(q_current), Fmatrix().invert(Fmatrix().rotation(Fquaternion().set(Ik.ArmRetargetRootRef)))), Fmatrix().rotation(q_attach)));
				q_arm.slerp(q_retarget, q_attach, arm);
				arm_pos.lerp(Fvector().add(attach_root.c, Fvector().sub(current.c, Ik.ArmRetargetRootRef.c)), attach_root.c, arm);
				root_weight = attach_weight;
			}
			else
			{
				q_arm = q_attach;
			}

			q_result.slerp(q_current, q_arm, root_weight);

			Fmatrix result;
			result.rotation(q_result);
			result.c.lerp(current.c, arm_pos, root_weight);
			CCameraRig::TransformBone(model, arm_bones[0], Fmatrix().mul_43(result, Fmatrix().invert(current)));
		}

		for (size_t i = 1; i < arm_bones.size(); ++i)
		{
			const u16 bone = arm_bones[i];

			if (F.FingerMark && std::find(Ik.FingerGripBones.begin(), Ik.FingerGripBones.end(), bone) != Ik.FingerGripBones.end())
			{
				continue;
			}

			const Fquaternion& target_pose = item->LeftHandAttachPoseRef[i];
			if (std::find(upper_bones.begin(), upper_bones.end(), bone) == upper_bones.end())
			{
				hud_blend_local(model, bone, target_pose, attach_weight, item->LeftHandAttachPosRef.size() == arm_bones.size() ? &item->LeftHandAttachPosRef[i] : nullptr);
			}
			else if (!retarget_arm)
			{
				hud_blend_local(model, bone, target_pose, attach_weight * arm);
			}
			else
			{
				const Fmatrix local = hud_bone_local(model, bone);
				const Fmatrix offset = Fmatrix().mul_43(Fmatrix().rotation(target_pose), Fmatrix().invert(Fmatrix().rotation(Ik.ArmRetargetPose[i])));
				Fquaternion animated;
				animated.slerp(Fquaternion().set(Fmatrix().mul_43(offset, local)), target_pose, arm);
				hud_blend_local(model, bone, animated, attach_weight);
			}
		}

		Fquaternion q_anim, q_attach, q_blend;
		q_anim.set(left_target);
		q_attach.set(attach_target);
		q_blend.slerp(q_anim, q_attach, target_weight);

		Fmatrix target;
		target.rotation(q_blend);
		target.c.lerp(left_target.c, attach_target.c, target_weight);
		CCameraRig::SolveLimb(model, Ik.LeftIkChain, target, 1.f, ElbowOffset);
	}
	else
	{
		CCameraRig::SolveLimb(model, Ik.LeftIkChain, left_target, 1.f, ElbowOffset);
		if (has_attach && target_weight > 0.f)
		{
			CCameraRig::SolveLimb(model, Ik.LeftIkChain, attach_target, target_weight, ElbowOffset);
		}
	}
}

void player_hud::HandsIkFingerInertia(SHandsIkFrame& F)
{
	const size_t count = Ik.FingerGripBones.size();
	const float time = CCameraRig::HudInertialization();
	if (!F.HasLeft || count == 0 || time <= 0.f)
	{
		Ik.FingerPrevRot.clear();
		return;
	}

	const bool fresh = Ik.FingerPrevRot.size() != count || Device.dwFrame != Ik.FingerResidualFrame + 1;
	Ik.FingerResidualFrame = Device.dwFrame;
	if (fresh)
	{
		Ik.FingerPrevRot.assign(count, Fquaternion().identity());
		Ik.FingerPrevPos.assign(count, Fvector().set(0.f, 0.f, 0.f));
		Ik.FingerResidualRot.assign(count, Fquaternion().identity());
		Ik.FingerResidualPos.assign(count, Fvector().set(0.f, 0.f, 0.f));
	}

	const auto angle = [](const Fquaternion& q)
	{
		return 2.f * acosf(std::min(std::abs(q.w), 1.f));
	};

	const float jump_rot = std::max(deg2rad(8.f), deg2rad(400.f) * Device.fTimeDelta);
	const float jump_pos = std::max(0.002f, 0.1f * Device.fTimeDelta);
	const float factor = HudBlend::Smooth(time / 3.f, Device.fTimeDelta);
	for (size_t i = 0; i < count; ++i)
	{
		const u16 bone = Ik.FingerGripBones[i];
		const Fmatrix local = hud_bone_local(F.Model, bone);
		const Fquaternion current = Fquaternion().set(local);
		const Fmatrix current_rot = Fmatrix().rotation(current);

		if (!fresh)
		{
			const Fmatrix step = Fmatrix().mul_43(Fmatrix().rotation(Ik.FingerPrevRot[i]), Fmatrix().invert(current_rot));
			const float jump = angle(Fquaternion().set(step));
			const Fvector step_pos = Fvector().sub(Ik.FingerPrevPos[i], local.c);
			if (jump > jump_rot || step_pos.magnitude() > jump_pos)
			{
				Ik.FingerResidualRot[i].set(Fmatrix().mul_43(Fmatrix().rotation(Ik.FingerResidualRot[i]), step));
				Ik.FingerResidualPos[i].add(step_pos);
			}

			Ik.FingerResidualRot[i].slerp(Fquaternion(Ik.FingerResidualRot[i]), Fquaternion().identity(), factor);
			Ik.FingerResidualPos[i].mul(1.f - factor);
		}

		Ik.FingerPrevRot[i] = current;
		Ik.FingerPrevPos[i] = local.c;

		const Fquaternion applied = Fquaternion().set(Fmatrix().mul_43(Fmatrix().rotation(Ik.FingerResidualRot[i]), current_rot));
		const Fvector applied_pos = Fvector().add(local.c, Ik.FingerResidualPos[i]);
		hud_blend_local(F.Model, bone, applied, 1.f, &applied_pos);
	}

}

void player_hud::HandsIkFingers(SHandsIkFrame& F)
{
	if (!F.HasLeft)
	{
		return;
	}

	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	const xr_vector<u16>& arm_bones = F.ArmBones;
	const bool finger_hold = F.FingerHold;
	const float finger_settle = F.FingerSettle;

	if (F.FingerStart > 0.f)
	{
		for (size_t i = 0; i < Ik.FingerGripBones.size(); ++i)
		{
			hud_blend_local(model, Ik.FingerGripBones[i], Ik.FingerStartPose[i], F.FingerStart, &Ik.FingerStartPos[i]);
		}
	}

	const bool attach_ref = item->LeftHandAttachRefValid && item->LeftHandAttachPoseRef.size() == arm_bones.size();
	const bool idle_ref = (finger_hold || finger_settle > 0.f) && !attach_ref && FingerGripReference(item);
	const auto finger_target = [&](size_t i, Fquaternion& target, const Fvector*& position)
	{
		position = nullptr;
		if (attach_ref)
		{
			const auto it = std::find(arm_bones.begin(), arm_bones.end(), Ik.FingerGripBones[i]);
			if (it == arm_bones.end())
			{
				return false;
			}

			const size_t index = it - arm_bones.begin();
			target = item->LeftHandAttachPoseRef[index];
			position = item->LeftHandAttachPosRef.size() == arm_bones.size() ? &item->LeftHandAttachPosRef[index] : nullptr;
			return true;
		}

		if (idle_ref)
		{
			target = item->FingerGripPose[i];
			position = item->FingerGripPos.size() == Ik.FingerGripBones.size() ? &item->FingerGripPos[i] : nullptr;
			return true;
		}

		return false;
	};

	if (finger_hold)
	{
		const float weight = hud_smooth(F.Finger.Idle) * F.Finger.Hold * item->LeftHandFingerGrip;
		for (size_t i = 0; i < Ik.FingerGripBones.size() && weight > 0.f; ++i)
		{
			Fquaternion grip;
			const Fvector* grip_position = nullptr;
			if (!finger_target(i, grip, grip_position))
			{
				continue;
			}

			hud_blend_local(model, Ik.FingerGripBones[i], grip, weight, grip_position);
		}
	}

	if (finger_settle > 0.f)
	{
		const float weight = hud_smooth(finger_settle);
		for (size_t i = 0; i < Ik.FingerGripBones.size(); ++i)
		{
			Fquaternion target;
			const Fvector* position = nullptr;
			if (finger_target(i, target, position))
			{
				hud_blend_local(model, Ik.FingerGripBones[i], target, weight, position);
			}
		}
	}
}

void player_hud::OnHudMotionStarted(const shared_str& motion)
{
	if (Ik.LeftHandAttachWeight > 0.f && m_model != nullptr && strstr(motion.c_str(), "anm_idle") == motion.c_str())
	{
		for (u16 bone : left_arm_subtree())
		{
			m_model->LL_SetInertializationBone(bone, false);
		}

		Ik.LeftHandAttachSnapFrame = Device.dwFrame;

		const attachable_hud_item* item = m_attached_items[0];
		const float arm_return = item != nullptr ? (1.f - Ik.LeftHandAttachArmWeight) * item->LeftHandAttachBlend : 0.f;
		Ik.LeftHandAttachHold = CCameraRig::HudInertialization() > 0.f ? 0.f : std::max(std::max(item != nullptr ? item->IdleFade : 0.f, 0.3f), arm_return) + 0.05f;
	}
	else
	{
		Ik.LeftHandAttachHold = 0.f;
	}

	attachable_hud_item* item = m_attached_items[0];
	const u8 mode = item != nullptr && item->LeftHandAttachBlend > 0.f && CCameraRig::Enabled() ? item->LeftHandAttachMotionMode(motion) : 0;
	Ik.LeftHandAttachReaching = mode != 0;
	Ik.LeftHandAttachAnimatedArm = mode == 2 || (mode == 0 && Ik.LeftHandAttachAnimatedArm && Ik.LeftHandAttachWeight > 0.f);

	if (item == nullptr || item->LeftHandAttachBlend <= 0.f || !CCameraRig::Enabled() || m_model == nullptr)
	{
		return;
	}

	bool shown = Ik.LeftHandAttachWeight >= 1.f;
	if (!shown)
	{
		const player_hud_motion* anm = item->m_hand_motions.find_motion(item->LeftHandAttachTarget);
		shown = anm != nullptr && !anm->m_animations.empty() && hud_motion_amount(m_model, anm->m_animations[0].mid) >= 0.99f;
	}

	if (Ik.LeftHandAttachReaching && shown)
	{
		Ik.LeftHandAttachWeight = 1.f;
	}
}

bool player_hud::FingerGripReference(attachable_hud_item* item)
{
	shared_str idle = "anm_idle";
	if (CHudItem* hud_item = item->m_parent_hud_item)
	{
		const shared_str current = hud_item->SetCurrentStateAnimation(hud_item->SetCurrentIdleAnimation());
		if (item->m_hand_motions.find_motion(current) != nullptr)
		{
			idle = current;
		}
	}

	if (item->FingerGripPoseMotion != idle)
	{
		item->FingerGripPoseMotion = idle;
		item->FingerGripPose.clear();

		const player_hud_motion* anm = item->m_hand_motions.find_motion(idle);
		if (m_model != nullptr && anm != nullptr && !anm->m_animations.empty())
		{
			hud_motion_local_pose(m_model, anm->m_animations[0].mid, 0.f, Ik.FingerGripBones, item->FingerGripPose, &item->FingerGripPos);
		}
	}

	return !item->FingerGripPose.empty() && item->FingerGripPose.size() == Ik.FingerGripBones.size();
}

bool player_hud::ArmRetargetReference(attachable_hud_item* item, const xr_vector<u16>& bones)
{
	const MotionID motion = hud_dominant_motion(m_model);
	if (!motion.valid() || !item->LeftHandAttachRefValid)
	{
		return false;
	}

	if (Ik.ArmRetargetMotion == motion)
	{
		return Ik.ArmRetargetPose.size() == bones.size();
	}

	Ik.ArmRetargetMotion = motion;
	Ik.ArmRetargetPose.clear();

	CMotion* root = m_model->LL_GetRootMotion(motion);
	if (root == nullptr)
	{
		return false;
	}

	const u16 ancor = m_ancors[item->m_attach_place_idx];
	const float length = root->GetLength();
	float best_time = 0.f;
	float best = flt_max;
	for (float time = 0.f; time <= length; time += SAMPLE_SPF)
	{
		Fmatrix ancor_ms;
		if (!m_model->LL_GetMotionBoneTransform(motion, time, ancor, ancor_ms))
		{
			return false;
		}

		const float deviation = hud_ancor_deviation(ancor_ms, item->LeftHandAttachAncorRef);
		if (deviation < best)
		{
			best = deviation;
			best_time = time;
		}
	}

	if (bones.empty() || !m_model->LL_GetMotionBoneTransform(motion, best_time, bones[0], Ik.ArmRetargetRootRef))
	{
		return false;
	}

	return hud_motion_local_pose(m_model, motion, best_time, bones, Ik.ArmRetargetPose) && Ik.ArmRetargetPose.size() == bones.size();
}

xr_vector<u16> player_hud::left_hand_fingers() const
{
	xr_vector<u16> bones;
	if (m_model == nullptr || Ik.LeftIkChain[2] == BI_NONE)
	{
		return bones;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	bones.push_back(Ik.LeftIkChain[2]);
	for (size_t i = 0; i < bones.size(); ++i)
	{
		for (CBoneData* child : kinematics->LL_GetData(bones[i]).children)
		{
			bones.push_back(child->GetSelfID());
		}
	}

	bones.erase(bones.begin());
	return bones;
}

xr_vector<u16> player_hud::left_arm_subtree() const
{
	xr_vector<u16> bones = left_arm_blend_bones();
	if (bones.empty())
	{
		return bones;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	bones.resize(1);
	for (size_t i = 0; i < bones.size(); ++i)
	{
		for (CBoneData* child : kinematics->LL_GetData(bones[i]).children)
		{
			bones.push_back(child->GetSelfID());
		}
	}

	return bones;
}

void player_hud::OnShotAnimation(const attachable_hud_item* item)
{
	if (item == nullptr || !CCameraRig::Enabled())
	{
		return;
	}

	const float variation = item->ShotVariation;
	ShotRotScale.set(1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation));
	ShotPosScale.set(1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation));
	ShotTwist.set(::Random.randFs(item->ShotTwist), ::Random.randFs(item->ShotTwist));

	const SCameraRigParams& rig = CCameraRig::Params();
	++Ik.ShotFingers.BurstShots;
	const float base = rig.shot_fingers_chance;
	const float chance = base > 0.f ? std::min(base + rig.shot_fingers_burst_chance * float(Ik.ShotFingers.BurstShots - 1), std::max(rig.shot_fingers_max_chance, base)) : 0.f;
	Ik.ShotFingers.Pending = chance > 0.f && ::Random.randF() < chance;
	Ik.ShotFingers.SinceShot = 0.f;
	Ik.ShotFingers.Stopping = Ik.ShotFingers.Active;
}

static xr_vector<u16> hud_bone_descendants(IKinematics* kinematics, u16 bone, u16 exclude)
{
	xr_vector<u16> bones = { bone };
	for (size_t i = 0; i < bones.size(); ++i)
	{
		for (CBoneData* child : kinematics->LL_GetData(bones[i]).children)
		{
			if (child->GetSelfID() != exclude)
			{
				bones.push_back(child->GetSelfID());
			}
		}
	}

	bones.erase(bones.begin());
	return bones;
}

static bool hud_item_zoomed(const attachable_hud_item* Item)
{
	const CWeapon* Weapon = Item->m_parent_hud_item->cast_weapon();
	return Weapon != nullptr && Weapon->IsZoomed();
}

void player_hud::StartShotFingers(attachable_hud_item* item)
{
	SHudShotFingers& S = Ik.ShotFingers;
	const SHudRetargetLayer* layer = nullptr;
	for (const auto& [alias, it] : item->RetargetLayers)
	{
		if (alias == "anm_idle")
		{
			layer = &it;
			break;
		}

		if (layer == nullptr && strstr(alias.c_str(), "anm_idle") == alias.c_str())
		{
			layer = &it;
		}
	}

	const MotionID motion = layer != nullptr && layer->Motion.size() != 0 ? m_model->ID_Cycle_Safe(layer->Motion) : MotionID();
	CMotionDef* def = motion.valid() ? m_model->LL_GetMotionDef(motion) : nullptr;
	if (def == nullptr)
	{
		return;
	}

	const xr_string prefix = pSettings->read_if_exists<str_c>("camera_rig", "shot_fingers_mark", "mm_shoot");
	const float speed = def->Speed() <= EPS ? 1.f : def->Speed();
	CMotion* root = m_model->LL_GetRootMotion(motion);
	const float length = root != nullptr ? root->GetLength() / speed : flt_max;

	xr_vector<std::pair<float, float>> intervals;
	for (const motion_marks& marks : def->marks)
	{
		const xr_string_view mark_name = marks.name.c_str();
		if (!mark_name.starts_with(prefix) || mark_name.size() != prefix.size() && mark_name[prefix.size()] != '_')
		{
			continue;
		}

		for (const motion_marks::interval& interval : marks.intervals)
		{
			intervals.emplace_back(interval.first, interval.second);
		}
	}

	std::sort(intervals.begin(), intervals.end());
	for (size_t i = 0; i < intervals.size(); ++i)
	{
		const float next = i + 1 < intervals.size() ? intervals[i + 1].first : length;
		intervals[i].second = std::min({ intervals[i].second, length, next });
	}

	std::erase_if(intervals, [](const std::pair<float, float>& it) { return it.second <= it.first + EPS_L; });
	if (intervals.empty())
	{
		return;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	S.Full = !hud_item_zoomed(item) && ::Random.randF() < CCameraRig::Params().shot_fingers_full_chance;
	if (S.Full)
	{
		S.Bones = hud_bone_descendants(kinematics, kinematics->LL_GetBoneRoot(), BI_NONE);
	}
	else
	{
		S.Bones.clear();
		const u16 ancor = m_ancors[item->m_attach_place_idx];
		for (const u16 wrist : { Ik.RightIkChain[2], Ik.LeftIkChain[2] })
		{
			if (wrist != BI_NONE)
			{
				const xr_vector<u16> fingers = hud_bone_descendants(kinematics, wrist, ancor);
				S.Bones.insert(S.Bones.end(), fingers.begin(), fingers.end());
			}
		}
	}

	const std::pair<float, float>& interval = intervals[::Random.randI(intervals.size())];
	S.Speed = speed;
	S.First = interval.first;
	S.Second = interval.second;
	S.Motion = motion;
	if (S.Bones.empty() || !hud_motion_local_pose(m_model, motion, S.First * S.Speed, S.Bones, S.Reference, &S.ReferencePos))
	{
		return;
	}

	S.Time = 0.f;
	S.Fade = 1.f;
	S.Stopping = false;
	S.Active = true;
}

void player_hud::UpdateShotFingers(bool BeforeIk)
{
	SHudShotFingers& S = Ik.ShotFingers;
	attachable_hud_item* item = m_attached_items[0];
	if (item == nullptr || item->m_model_combined || m_model == nullptr || !CCameraRig::Enabled())
	{
		S = {};
		return;
	}

	if (!BeforeIk)
	{
		if (S.Active && !S.Full)
		{
			ApplyShotFingers();
		}

		return;
	}

	const float blend = CCameraRig::Params().shot_fingers_blend;
	const bool idle = item->m_parent_hud_item->GetState() == CHUDState::eIdle;
	if (idle)
	{
		S.BurstShots = 0;
	}

	if (S.Pending)
	{
		S.SinceShot += Device.fTimeDelta;
		if (!S.Active && idle && S.SinceShot >= CCameraRig::Params().shot_fingers_delay)
		{
			S.Pending = false;
			StartShotFingers(item);
		}
	}

	if (!S.Active)
	{
		return;
	}

	if (S.Full && hud_item_zoomed(item))
	{
		S.Stopping = true;
	}

	S.Time += Device.fTimeDelta;
	if (S.Stopping)
	{
		S.Fade = blend > EPS ? S.Fade - Device.fTimeDelta / blend : 0.f;
	}

	const float blend_out = CCameraRig::Params().shot_fingers_blend_out;
	const float time = S.First + S.Time;
	if (time >= S.Second + blend_out || S.Fade <= 0.f)
	{
		S.Active = false;
		return;
	}

	const float edge = std::min(blend, (S.Second - S.First) * 0.5f);
	S.Weight = hud_smooth(hud_mark_ramp(time - S.First, edge) * hud_mark_ramp(S.Second + blend_out - time, blend_out) * std::clamp(S.Fade, 0.f, 1.f));
	if (S.Full)
	{
		ApplyShotFingers();
	}
}

void player_hud::ApplyShotFingers()
{
	SHudShotFingers& S = Ik.ShotFingers;
	const float time = std::min(S.First + S.Time, S.Second);
	const float weight = S.Weight;
	xr_vector<Fquaternion> pose;
	xr_vector<Fvector> positions;
	if (weight <= 0.f || !hud_motion_local_pose(m_model, S.Motion, time * S.Speed, S.Bones, pose, &positions) || pose.size() != S.Reference.size() || positions.size() != S.ReferencePos.size())
	{
		return;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	for (size_t i = 0; i < S.Bones.size(); ++i)
	{
		const u16 bone = S.Bones[i];
		Fmatrix reference, sample;
		reference.rotation(S.Reference[i]);
		sample.rotation(pose[i]);
		const Fmatrix delta = Fmatrix().mul_43(Fmatrix().invert(reference), sample);

		const Fmatrix parent = kinematics->LL_GetTransform(kinematics->LL_GetData(bone).GetParentID());
		const Fmatrix bone_ms = kinematics->LL_GetTransform(bone);
		const Fmatrix local = Fmatrix().mul_43(Fmatrix().invert(parent), bone_ms);
		Fmatrix moved_local = Fmatrix().mul_43(local, hud_weighted_transform(delta, weight));
		moved_local.c.mad(local.c, Fvector().sub(positions[i], S.ReferencePos[i]), weight);
		const Fmatrix moved = Fmatrix().mul_43(parent, moved_local);
		CCameraRig::TransformBone(kinematics, bone, Fmatrix().mul_43(moved, Fmatrix().invert(bone_ms)));
	}
}

void player_hud::SyncSprintBlend()
{
	CObject* entity = Level().CurrentEntity();
	CActor* actor = entity != nullptr ? entity->cast_actor() : nullptr;
	CHudItem* item = m_attached_items[0] != nullptr ? m_attached_items[0]->m_parent_hud_item : nullptr;

	bool sprint = false;
	if (actor != nullptr && item != nullptr && CCameraRig::Enabled())
	{
		const u32 state = item->GetState();
		const bool idle = state == CHUDState::eIdle || state == CHUDState::eSprintStart || state == CHUDState::eSprintEnd;
		CWeapon* weapon = item->cast_weapon();
		const bool sprint_action = weapon != nullptr && smart_cast<CWeaponPistol*>(weapon) == nullptr && (state == CWeapon::eReload || state == CWeapon::eSwitchMode || state == CWeapon::eSafemodeSwitch);
		const bool unpended = !item->IsPending() && state != CHUDState::eHiding && state != CHUDState::eHidden;
		const u32 movement = actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);
		const bool airborne = (movement & (ACTOR_DEFS::EMoveCommand::mcJump | ACTOR_DEFS::EMoveCommand::mcFall)) != 0;
		sprint = (idle || sprint_action || unpended) && (movement & ACTOR_DEFS::EMoveCommand::mcSprint) != 0 && !airborne;
	}

	Sprint.Update(m_model, m_attached_items[0], sprint);
}

void player_hud::UpdateCarryPose(CActor* actor)
{
	attachable_hud_item* item = m_attached_items[0];
	CWeapon* weapon = item != nullptr && item->m_parent_hud_item != nullptr ? item->m_parent_hud_item->cast_weapon() : nullptr;
	if (!CCameraRig::Enabled() || actor == nullptr || weapon == nullptr)
	{
		CrouchPoseValue = 0.f;
		CrouchPoseVelocity = 0.f;
		WalkPoseValue = 0.f;
		WalkPoseVelocity = 0.f;
		WalkTime = 0.f;
		CrouchTime = 0.f;
		return;
	}

	const u32 state = actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);
	const bool zoom = weapon->IsZoomed() || weapon->m_bIsAimStarted;
	const bool sprint = (state & ACTOR_DEFS::EMoveCommand::mcSprint) != 0;
	const bool crouch = (state & ACTOR_DEFS::EMoveCommand::mcCrouch) != 0 && !zoom;
	const bool walking = (state & ACTOR_DEFS::EMoveCommand::mcAnyMove) != 0 && !sprint && !zoom && weapon->GetState() == CHUDState::eIdle;

	WalkTime = walking ? WalkTime + Device.fTimeDelta : 0.f;
	const float walk_target = walking && WalkTime >= item->WalkPoseDelay ? 1.f : 0.f;
	CrouchTime = crouch ? CrouchTime + Device.fTimeDelta : 0.f;
	const float crouch_target = crouch && CrouchTime >= item->CrouchPoseDelay ? 1.f : 0.f;

	const float crouch_half_life = crouch_target < CrouchPoseValue && item->CrouchPoseInertiaOut >= 0.f ? item->CrouchPoseInertiaOut : item->CrouchPoseInertia;
	HudSpring::Step(CrouchPoseValue, CrouchPoseVelocity, crouch_target, crouch_half_life, item->CarryPoseDamping, Device.fTimeDelta);
	HudSpring::Step(WalkPoseValue, WalkPoseVelocity, walk_target, walk_target < WalkPoseValue ? item->WalkPoseInertiaOut : item->WalkPoseInertia, item->CarryPoseDamping, Device.fTimeDelta);
}

void player_hud::TriggerScriptLayersInertia(float half_life)
{
	if (half_life <= 0.f)
	{
		return;
	}

	ScriptInertiaPending = true;
	ScriptInertiaHalfLife = half_life;
}

Fmatrix player_hud::CarryPoseOffset() const
{
	const attachable_hud_item* item = m_attached_items[0];
	if (item == nullptr || (fis_zero(CrouchPoseValue) && fis_zero(WalkPoseValue)))
	{
		return Fidentity;
	}

	Fvector rot = Fvector().set(0.f, 0.f, 0.f).mad(item->CrouchOffsetRot, CrouchPoseValue).mad(item->WalkOffsetRot, WalkPoseValue);
	rot.mul(PI / 180.f);
	Fmatrix offset;
	offset.setXYZ(rot);
	offset.translate_over(Fvector().set(0.f, 0.f, 0.f).mad(item->CrouchOffsetPos, CrouchPoseValue).mad(item->WalkOffsetPos, WalkPoseValue));
	return offset;
}

void player_hud::UpdateSprintPose()
{
	const attachable_hud_item* item = m_attached_items[0];
	const u16 ancor = item != nullptr && item->m_attach_place_idx < m_ancors.size() ? m_ancors[item->m_attach_place_idx] : BI_NONE;
	Sprint.UpdatePose(m_model, item, ancor);
}

void player_hud::CutSprintEnd()
{
	Sprint.CutEnd(m_model);
}

void player_hud::UpdateRetargetLayerWeight()
{
	const attachable_hud_item* item = m_attached_items[0];
	if (m_model == nullptr || item == nullptr || !item->RetargetLayerPlaying)
	{
		return;
	}

	const float Power = std::max(1.f - hud_smooth(hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL)), 0.001f);
	for (u16 Part = 0, Count = m_model->partitions().count(); Part < Count; ++Part)
	{
		for (u32 i = 0, Blends = m_model->LL_PartBlendsCount(Part); i < Blends; i++)
		{
			CBlend* B = m_model->LL_PartBlend(Part, i);
			if (B != nullptr && B->channel == ADDITIVE_CHANNEL && B->motionID == item->RetargetMotion && B->blend_state() != CBlend::eFalloff && B->blend_state() != CBlend::eFREE_SLOT)
			{
				const float Fraction = B->blendPower > EPS ? B->blendAmount / B->blendPower : 1.f;
				B->blendPower = Power;
				B->blendAmount = Fraction * Power;
			}
		}
	}
}

void player_hud::ReloadRigParams()
{
	if (m_model != nullptr)
	{
		m_model->LL_SetInertialization(CCameraRig::HudInertialization());
		m_model->LL_SetSmoothBlend(CCameraRig::Enabled());
	}

	for (attachable_hud_item* item : m_pool)
	{
		item->LoadRigParams();
	}
}

void player_hud::LoadHandsIkChains(const shared_str& Sect)
{
	IKinematics* Kinematics = m_model->dcast_PKinematics();
	const auto LoadChain = [&](const char* Key, u16* Chain)
	{
		string128 Bone;
		const char* Names = pSettings->read_if_exists<str_c>(Sect, Key, pSettings->read_if_exists<str_c>("camera_rig", Key, nullptr));
		const bool Valid = Names != nullptr && _GetItemCount(Names) == 3;
		for (int i = 0; i < 3; i++)
		{
			Chain[i] = Valid ? Kinematics->LL_BoneID(_GetItem(Names, i, Bone)) : BI_NONE;
		}
	};

	LoadChain("left_hand_ik_chain", Ik.LeftIkChain);
	LoadChain("right_hand_ik_chain", Ik.RightIkChain);
}

void player_hud::ApplyLayersIk(const Fmatrix& Trans, const Fmatrix& TransRig, const Fmatrix& TransTurn, const Fvector& TurnShift, const Fmatrix& ScriptLayers, const Fmatrix& MovementLayers, const Fmatrix& CarryPose, CActor* actor, float IkShare)
{
	attachable_hud_item* Item = m_attached_items[0];
	Ik.LayersIk = IkShare > 0.f && actor != nullptr && Item != nullptr && !Item->m_model_combined && m_attached_items[1] == nullptr && m_animator_item == nullptr && Ik.RightIkChain[2] != BI_NONE;
	if (!Ik.LayersIk)
	{
		return;
	}

	const hud_item_measures& Measures = Item->m_measures;
	const Fvector& BasePos = Measures.m_hands_positions.hands_offsets_tune[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
	const Fvector& BaseRot = Measures.m_hands_positions.hands_offsets_tune[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault];

	const float OffsetShare = Item->IkEnabled ? IkShare : 0.f;
	Fquaternion RigidRotation;
	RigidRotation.slerp(HudRotation::FromEuler(attach_rot()), HudRotation::FromEuler(BaseRot), OffsetShare);
	Fmatrix RigidAttach;
	RigidAttach.rotation(RigidRotation);
	RigidAttach.c.lerp(attach_pos(), BasePos, OffsetShare);

	const float RigidShare = 1.f - IkShare;
	Fmatrix Rigid = TransTurn;
	Rigid.c.mad(TurnShift, -IkShare);
	Rigid.mulB_43(scale_layer_offset(Fmatrix().mul_43(Fmatrix().invert(TransTurn), TransRig), RigidShare));

	const Fmatrix CollisionInv = Fmatrix().invert(Item->CollisionXform);
	Fmatrix LookPre, Look;
	if (Item->m_parent_hud_item->GetHudLookInertia(LookPre, Look))
	{
		Rigid.mulB_43(Fmatrix().mul_43(Fmatrix().invert(TransRig), LookPre));
		Rigid.mulB_43(scale_layer_offset(Look, RigidShare));
		Rigid.mulB_43(Fmatrix().mul_43(Fmatrix().invert(Fmatrix().mul_43(LookPre, Look)), Trans).mulB_43(CollisionInv));
	}
	else
	{
		Rigid.mulB_43(Fmatrix().mul_43(Fmatrix().invert(TransRig), Trans).mulB_43(CollisionInv));
	}

	Rigid.mulB_43(scale_layer_offset(Item->CollisionXform, RigidShare));
	Rigid.mulB_43(RigidAttach);
	Rigid.mulB_43(scale_layer_offset(ScriptLayers, 1.f - IkShare));
	Rigid.mulB_43(scale_layer_offset(MovementLayers, 1.f - IkShare));
	Rigid.mulB_43(scale_layer_offset(CarryPose, 1.f - IkShare));

	Ik.LayersIkOffset.invert(Rigid).mulB_43(m_transform);
	m_transform.set(Rigid);

	m_transformL.set(Rigid);
}

bool SHudSprint::BaseTransform(IKinematicsAnimated* Model, u16 Bone, Fmatrix& Result)
{
	return Bone != BI_NONE && Model->LL_GetBoneTransformMasked(Bone, u8(~(1 << LAYER_ADDITIVE_CHANNEL)), Result);
}

bool SHudSprint::BoneDelta(IKinematicsAnimated* Model, u16 Bone, Fmatrix& Result)
{
	Fmatrix Base;
	if (Bone == BI_NONE || !BaseTransform(Model, Bone, Base))
	{
		return false;
	}

	Result.mul_43(Model->dcast_PKinematics()->LL_GetTransform(Bone), Fmatrix().invert(Base));
	return true;
}

bool SHudSprint::Enabled(IKinematicsAnimated* Model, const attachable_hud_item* Current) const
{
	return Model != nullptr && Current != nullptr && Current == Item && LoopMotion.valid();
}

void SHudSprint::Attach(IKinematicsAnimated* Model, const attachable_hud_item* Current)
{
	Stop(Model, Current->SprintLayerBlend());
	Item = Current;
	Resolve(Model, Current);

	BaseMotion = MotionID();
	BaseResidual[0] = Fidentity;
	BaseResidual[1] = Fidentity;
}

void SHudSprint::Resolve(IKinematicsAnimated* Model, const attachable_hud_item* Current)
{
	StartMotion = MotionID();
	LoopMotion = MotionID();
	EndMotion = MotionID();
	ReferenceMotion = MotionID();
	if (Model == nullptr || Current == nullptr || !Current->SprintLayered())
	{
		return;
	}

	StartMotion = Current->AliasMotion("anm_idle_sprint_start");
	LoopMotion = Current->AliasMotion("anm_idle_sprint");
	EndMotion = Current->AliasMotion("anm_idle_sprint_end");

	const shared_str Reference = Current->SprintLayerReference();
	if (Reference.size() != 0)
	{
		ReferenceMotion = Model->ID_Cycle_Safe(Reference);
	}
}

bool SHudSprint::Play(IKinematicsAnimated* Model, const attachable_hud_item* Current, MotionID Motion, bool Loop, float Phase, float Power, bool Inertialize)
{
	if (Model == nullptr || !Motion.valid())
	{
		return false;
	}

	CMotionDef* Def = Model->LL_GetMotionDef(Motion);
	const float Rate = 1.f / Current->SprintLayerBlend();
	float Accrue = Rate;
	if (Motion == StartMotion)
	{
		float BlendIn = Current->SprintBlendIn > 0.f ? Current->SprintBlendIn : Current->SprintLayerBlend();
		if (CMotion* Root = Model->LL_GetRootMotion(Motion))
		{
			const float Speed = std::max(Def != nullptr ? Def->Speed() : 1.f, EPS);
			BlendIn = std::min(BlendIn, Root->GetLength() / Speed * (1.f - std::clamp(Phase, 0.f, 1.f)));
		}

		Accrue = 1.f / std::max(BlendIn, 0.01f);
	}

	const bool Inertial = Inertialize && CCameraRig::HudInertialization() > 0.f;
	for (u16 Part = 0, Count = Model->partitions().count(); Part < Count; ++Part)
	{
		if (Inertial)
		{
			LayerSeam = true;
			Model->LL_InertializePart(Part, Current->SprintLayerBlend(), LAYER_ADDITIVE_CHANNEL);
			Model->LL_CloseCycle(Part, 1 << LAYER_ADDITIVE_CHANNEL);
		}
		else
		{
			Model->LL_FadeCycle(Part, Rate, 1 << LAYER_ADDITIVE_CHANNEL);
		}

		CBlend* B = Model->LL_PlayCycle(Part, Motion, !Inertial, Accrue, Rate, Def != nullptr ? Def->Speed() : 1.f, !Loop, nullptr, nullptr, LAYER_ADDITIVE_CHANNEL);
		if (B != nullptr)
		{
			B->additive_ref = ReferenceMotion;
			B->fall_at_end = false;
			B->blendPower = std::clamp(Power, 0.05f, 1.f);
			B->timeCurrent = std::clamp(Phase, 0.f, 1.f) * std::max(B->timeTotal - END_EPS, 0.f);
		}
	}

	CObject* Entity = Level().CurrentControlEntity();
	CActor* Actor = Entity != nullptr ? Entity->cast_actor() : nullptr;
	const char* Alias = Motion == StartMotion ? "anm_idle_sprint_start" : Motion == EndMotion ? "anm_idle_sprint_end" : nullptr;
	if (Actor != nullptr && Alias != nullptr)
	{
		shared_str Name;
		float CameraSpeed = CalcMotionSpeed(Alias);
		for (const player_hud_motion& It : Current->m_hand_motions.m_anims)
		{
			if (It.m_alias_name == Alias)
			{
				Name = It.m_base_name;
				CameraSpeed *= It.m_anim_speed;
				break;
			}
		}

		string_path Camera;
		xr_strconcat(Camera, "camera_effects\\weapon\\", Name.c_str(), ".anm");
		CCameraRig& Rig = Actor->Cameras().Rig();
		if (Name.size() != 0 && Rig.PlayAnim(Camera, crcSprint, CameraSpeed, false))
		{
			Rig.SeekChannel(crcSprint, Phase);
		}
		else
		{
			Rig.StopChannel(crcSprint);
		}
	}

	return true;
}

void SHudSprint::Stop(IKinematicsAnimated* Model, float Blend)
{
	Stage = EHudSprintStage::None;
	CObject* Entity = Level().CurrentControlEntity();
	if (CActor* Actor = Entity != nullptr ? Entity->cast_actor() : nullptr)
	{
		Actor->Cameras().Rig().StopChannel(crcSprint);
	}

	if (Model == nullptr)
	{
		return;
	}

	for (u16 Part = 0, Count = Model->partitions().count(); Part < Count; ++Part)
	{
		Model->LL_FadeCycle(Part, 1.f / std::max(Blend, 0.01f), 1 << LAYER_ADDITIVE_CHANNEL);
	}
}

float SHudSprint::Progress(IKinematicsAnimated* Model, MotionID Motion) const
{
	if (Model == nullptr || !Motion.valid())
	{
		return -1.f;
	}

	for (u16 Part = 0, Count = Model->partitions().count(); Part < Count; ++Part)
	{
		for (u32 i = 0, Blends = Model->LL_PartBlendsCount(Part); i < Blends; i++)
		{
			const CBlend* B = Model->LL_PartBlend(Part, i);
			if (B != nullptr && B->channel == LAYER_ADDITIVE_CHANNEL && B->motionID == Motion && B->blend_state() != CBlend::eFREE_SLOT && B->blend_state() != CBlend::eFalloff)
			{
				const float Length = B->timeTotal - END_EPS;
				return Length > EPS ? std::clamp(B->timeCurrent / Length, 0.f, 1.f) : 1.f;
			}
		}
	}

	return -1.f;
}

void SHudSprint::Freeze(IKinematicsAnimated* Model, MotionID Motion)
{
	if (Model == nullptr || !Motion.valid())
	{
		return;
	}

	for (u16 Part = 0, Count = Model->partitions().count(); Part < Count; ++Part)
	{
		for (u32 i = 0, Blends = Model->LL_PartBlendsCount(Part); i < Blends; i++)
		{
			CBlend* B = Model->LL_PartBlend(Part, i);
			if (B != nullptr && B->channel == LAYER_ADDITIVE_CHANNEL && B->motionID == Motion && B->blend_state() != CBlend::eFREE_SLOT)
			{
				B->playing = false;
			}
		}
	}
}

void SHudSprint::BeginLeftCancel()
{
	LeftCancel.Active = true;
	LeftCancel.Weight = 1.f;
	LeftCancel.Hold = 1.f;
	LeftCancel.Time = 0.f;
	LeftCancel.Relative = LeftWristLast;
}

void SHudSprint::Update(IKinematicsAnimated* Model, const attachable_hud_item* Current, bool Sprinting)
{
	if (Current != Item)
	{
		if (Stage != EHudSprintStage::None)
		{
			Stop(Model, Item != nullptr ? Item->SprintLayerBlend() : 0.2f);
		}

		Item = Current;
	}

	Resolve(Model, Current);
	if (!Enabled(Model, Current))
	{
		if (Stage != EHudSprintStage::None)
		{
			Stop(Model, Current != nullptr ? Current->SprintLayerBlend() : 0.2f);
		}

		return;
	}

	if (Sprinting)
	{
		if (Stage == EHudSprintStage::Start || Stage == EHudSprintStage::Loop)
		{
			return;
		}

		float Phase = 0.f;
		if (Stage == EHudSprintStage::End)
		{
			const float End = Progress(Model, EndMotion);
			Phase = End >= 0.f ? (1.f - End) * EndPower : 0.f;
		}

		if (Play(Model, Current, StartMotion, false, Phase))
		{
			Stage = EHudSprintStage::Start;
		}
		else if (Play(Model, Current, LoopMotion, true, 0.f))
		{
			Stage = EHudSprintStage::Loop;
		}
		else
		{
			Stop(Model, Current->SprintLayerBlend());
		}

		return;
	}

	if (Stage == EHudSprintStage::None || Stage == EHudSprintStage::End)
	{
		return;
	}

	const u32 State = Current->m_parent_hud_item != nullptr ? Current->m_parent_hud_item->GetState() : CHUDState::eHidden;
	const bool Leaving = State == CHUDState::eHiding || State == CHUDState::eHidden;

	EndPower = 1.f;
	LeftCancel.Active = false;
	const bool CancelStart = Stage == EHudSprintStage::Start;
	if (CancelStart)
	{
		const float Start = Progress(Model, StartMotion);
		EndPower = Start >= 0.f ? Start : 1.f;
		Freeze(Model, StartMotion);
	}

	CWeapon* EndWeapon = Current->m_parent_hud_item != nullptr ? Current->m_parent_hud_item->cast_weapon() : nullptr;
	const bool ToSafemode = EndWeapon != nullptr && EndWeapon->SprintEndsInSafemode();
	if (!Leaving && ToSafemode)
	{
		EndWeapon->OnSprintToSafemode();
	}

	if (!Leaving && !ToSafemode && Play(Model, Current, EndMotion, false, 0.f, EndPower))
	{
		Stage = EHudSprintStage::End;
		if (CancelStart && LeftFree && Current->SprintLeftGripRange > 0.f)
		{
			BeginLeftCancel();
		}
	}
	else
	{
		Stop(Model, Current->SprintLayerBlend());
	}
}

void SHudSprint::UpdateChain(IKinematicsAnimated* Model, const attachable_hud_item* Current)
{
	if (Stage == EHudSprintStage::None)
	{
		return;
	}

	const attachable_hud_item* Layered = Item;
	if (Layered == nullptr || !Enabled(Model, Current))
	{
		Stop(Model, Layered != nullptr ? Layered->SprintLayerBlend() : 0.2f);
		return;
	}

	if (Stage == EHudSprintStage::Loop)
	{
		return;
	}

	const bool Starting = Stage == EHudSprintStage::Start;
	const float Phase = Progress(Model, Starting ? StartMotion : EndMotion);
	if (Phase >= 0.f && Phase < 1.f)
	{
		return;
	}

	if (Starting && Play(Model, Layered, LoopMotion, true, 0.f, 1.f, true))
	{
		Stage = EHudSprintStage::Loop;
		return;
	}

	Stop(Model, Layered->SprintLayerBlend());
}

float SHudSprint::LayerDeviation(IKinematicsAnimated* Model, u16 Ancor)
{
	Fmatrix Full, Base;
	if (Model == nullptr || Ancor == BI_NONE || !Model->LL_GetBoneTransformMasked(Ancor, u8(-1), Full) || !Model->LL_GetBoneTransformMasked(Ancor, u8(~(1 << LAYER_ADDITIVE_CHANNEL)), Base))
	{
		return 0.f;
	}

	const Fmatrix Delta = Fmatrix().mul_43(Full, Fmatrix().invert(Base));
	Fquaternion Rotation;
	Rotation.set(Delta);
	const float Angle = 2.f * acosf(std::min(std::abs(Rotation.w), 1.f)) / deg2rad(30.f);
	const float Distance = Delta.c.magnitude() / 0.1f;
	return _sqrt(Angle * Angle + Distance * Distance);
}

float SHudSprint::PoseTarget(IKinematicsAnimated* Model, u16 Ancor)
{
	if (Model == nullptr)
	{
		return 0.f;
	}

	const float Weight = hud_smooth(hud_channel_weight(Model, LAYER_ADDITIVE_CHANNEL));
	if (Stage == EHudSprintStage::Start || Stage == EHudSprintStage::Loop)
	{
		if (Stage == EHudSprintStage::Start)
		{
			const float Phase = Progress(Model, StartMotion);
			PoseShape = Phase >= 0.f ? hud_smooth(Phase) : PoseShape;
		}
		else
		{
			PoseShape = 1.f;
		}

		ExitDeviation = -1.f;
		return Weight * PoseShape;
	}

	if (Weight <= 0.f)
	{
		ExitDeviation = -1.f;
		return 0.f;
	}

	const float Deviation = LayerDeviation(Model, Ancor);
	if (ExitDeviation < 0.f)
	{
		ExitDeviation = Deviation;
		ExitPose = Target;
	}

	if (ExitDeviation > 1e-3f)
	{
		return ExitPose * std::clamp(Deviation / ExitDeviation, 0.f, 1.f);
	}

	if (Stage == EHudSprintStage::End)
	{
		const float Phase = Progress(Model, EndMotion);
		PoseShape = Phase >= 0.f ? 1.f - hud_smooth(Phase) : PoseShape;
	}

	return Weight * PoseShape;
}

void SHudSprint::UpdatePose(IKinematicsAnimated* Model, const attachable_hud_item* Current, u16 Ancor)
{
	Target = PoseTarget(Model, Ancor);

	const float HalfLife = Current != nullptr ? Current->SprintPoseInertia : 0.f;
	const CWeapon* Weapon = Current != nullptr && Current->m_parent_hud_item != nullptr ? Current->m_parent_hud_item->cast_weapon() : nullptr;
	const float Damping = Weapon != nullptr ? Weapon->Safemode.SpringDamping() : CCameraRig::Params().safemode_damping;
	HudSpring::Step(Pose, PoseVelocity, Target, HalfLife, Damping, Device.fTimeDelta);
	Pose = std::clamp(Pose, -0.25f, 1.25f);

	if (Target <= 0.f && std::abs(Pose) < 1e-3f && std::abs(PoseVelocity) < 1e-2f)
	{
		Pose = 0.f;
		PoseVelocity = 0.f;
	}
}

void SHudSprint::CutEnd(IKinematicsAnimated* Model)
{
	if (Stage == EHudSprintStage::End && Item != nullptr)
	{
		Stop(Model, Item->SprintLayerBlend());
	}
}

void SHudSprint::SmoothLayer(SHandsIkFrame& F)
{
	const float Time = CCameraRig::HudInertialization() > 0.f ? F.Item->SprintLayerBlend() : 0.f;
	const bool Valid[2] = { F.SprintLayerRight, F.SprintLayerLeft };
	Fmatrix* Targets[2] = { &F.SprintRightLayer, &F.SprintLeftLayer };
	const bool Continuous = LayerFrame + 1 == Device.dwFrame;
	LayerFrame = Device.dwFrame;

	const float Factor = HudBlend::Smooth(Time / 3.f, Device.fTimeDelta);
	for (u32 i = 0; i < 2; ++i)
	{
		if (!Valid[i] || Time <= 0.f)
		{
			LayerResidual[i] = Fidentity;
			continue;
		}

		Fmatrix& Layer = *Targets[i];
		if (LayerSeam && Continuous)
		{
			LayerResidual[i].mul_43(LayerPrev[i], Fmatrix().invert(Layer));
		}
		else
		{
			hud_decay_residual(LayerResidual[i], Factor);
		}

		Layer.mulA_43(LayerResidual[i]);
		LayerPrev[i] = Layer;
	}

	LayerSeam = false;
}

void SHudSprint::SmoothBase(IKinematicsAnimated* Model, SHandsIkFrame& F)
{
	const Fmatrix AncorFull = Fmatrix().mul_43(F.Delta, F.Model->LL_GetTransform(F.Ancor));
	const Fmatrix AncorInverse = Fmatrix().invert(AncorFull);
	const MotionID Dominant = F.SprintLayer > 0.f ? hud_dominant_motion(Model) : MotionID();
	const bool BaseChanged = Dominant.valid() && BaseMotion.valid() && (Dominant != BaseMotion || F.State != BaseState);
	BaseMotion = Dominant;
	BaseState = F.State;

	const float Factor = HudBlend::Smooth(F.Item->SprintLayerBlend() / 3.f, Device.fTimeDelta);
	const bool Hands[2] = { F.HasRight && !F.AncorInRightHand, F.HasLeft };
	Fmatrix* Targets[2] = { &F.RightTarget, &F.LeftTarget };
	for (u32 i = 0; i < 2; ++i)
	{
		if (!Hands[i])
		{
			BaseResidual[i] = Fidentity;
			continue;
		}

		const Fmatrix Relative = Fmatrix().mul_43(AncorInverse, *Targets[i]);
		if (BaseChanged)
		{
			BaseResidual[i].mul_43(BasePrev[i], Fmatrix().invert(Relative));
		}
		else
		{
			hud_decay_residual(BaseResidual[i], Factor);
		}

		BasePrev[i].mul_43(BaseResidual[i], Relative);
		Targets[i]->mul_43(AncorFull, BasePrev[i]);
	}
}

bool SHudAimReload::Wanted(const attachable_hud_item* Item)
{
	return Item->AimReloadMotion < 1.f || Item->AimReloadZoomOut > 0.f || !fis_zero(Item->AimReloadOffsetPos.square_magnitude()) || !fis_zero(Item->AimReloadOffsetRot.square_magnitude()) || !fis_zero(Item->AimReloadPushPos.square_magnitude());
}

void SHudAimReload::Update(const attachable_hud_item* Item, bool Active)
{
	if (Active && Weight <= 0.f)
	{
		Release = 1.f;
	}

	Weight = HudBlend::Ramp(Weight, Active, CCameraRig::AimReloadBlend(), Device.fTimeDelta);
	const float Target = Weight * hud_smooth(Release);
	const float HalfLife = Target < PoseValue ? Item->AimReloadPoseInertiaOut : Item->AimReloadPoseInertia;
	HudSpring::Step(PoseValue, PoseVelocity, Target, HalfLife, 1.f, Device.fTimeDelta);
	const float PushTarget = Active && Release >= 1.f ? 1.f : 0.f;
	HudSpring::Step(PushValue, PushVelocity, PushTarget, Item->AimReloadPoseInertiaOut, 1.f, Device.fTimeDelta);
	const float DropTarget = Active && Release >= 1.f ? Weight : 0.f;
	HudSpring::Step(DropValue, DropVelocity, DropTarget, DropTarget < DropValue ? Item->AimReloadPoseInertiaOut : Item->AimReloadPoseInertia, 1.f, Device.fTimeDelta);
}

void SHudAimReload::UpdateRelease(float Progress)
{
	const float Tail = CCameraRig::AimReloadTail();
	if (Tail <= 0.f)
	{
		Release = 1.f;
	}
	else if (Progress >= 0.f)
	{
		Release = std::clamp((1.f - Progress) / Tail, 0.f, 1.f);
	}
}

float SHudSafemode::SpringDamping() const
{
	return Damping >= 0.f ? Damping : CCameraRig::Params().safemode_damping;
}

void SHudSafemode::Spring(const Fvector& CurrentPos, Fvector& SavedPos, const Fvector& CurrentRot, Fvector& SavedRot, float HalfLife)
{
	const float Zeta = SpringDamping();
	const auto Settle = [&](float Error, float& Velocity)
	{
		HudSpring::Step(Error, Velocity, 0.f, HalfLife, Zeta, Device.fTimeDelta);
		return Error;
	};

	for (int i = 0; i < 3; ++i)
	{
		SavedPos[i] = CurrentPos[i] + Settle(SavedPos[i] - CurrentPos[i], VelocityPos[i]);
		SavedRot[i] = angle_normalize_signed(CurrentRot[i] + Settle(-angle_difference_signed(CurrentRot[i], SavedRot[i]), VelocityRot[i]));
	}
}
