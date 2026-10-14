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

static SCameraRigParams	g_rig;
static int				g_rig_enabled = -1;

static const SCameraRigParam g_rig_params[] =
{
	{ "Arms and IK", "hud_ik_share", nullptr, &g_rig.hud_ik_share, nullptr, 1, false, 0.01f },
	{ "Arms and IK", "lean_hud_counter", nullptr, &g_rig.lean_hud_counter, nullptr, 1, false, 0.01f },
	{ "Arms and IK", "left_hand_ik_follow", nullptr, &g_rig.left_hand_ik_follow, nullptr, 1, false, 0.005f },
	{ "Arms and IK", "left_hand_ik_blend", "left_hand_ik_blend", &g_rig.left_hand_ik_blend, nullptr, 1, false, 0.005f },
	{ "Arms and IK", "left_hand_finger_tail", "left_hand_finger_tail", &g_rig.left_hand_finger_tail, nullptr, 1, false, 0.01f },
	{ "Arms and IK", "left_hand_finger_grip", "left_hand_finger_grip", &g_rig.left_hand_finger_grip, nullptr, 1, false, 0.01f },
	{ "Arms and IK", "left_hand_attach_blend", "left_hand_attach_blend", &g_rig.left_hand_attach_blend, nullptr, 1, false, 0.005f },

	{ "Inertia and sway", "turn_sway_smooth", nullptr, &g_rig.turn_sway_smooth, nullptr, 1, false, 0.002f },
	{ "Inertia and sway", "inertia_speed", nullptr, &g_rig.inertia_speed, nullptr, 1, false, 0.01f },
	{ "Inertia and sway", "inertia_damping", "inertia_damping", &g_rig.inertia_damping, nullptr, 1, false, 0.01f },
	{ "Inertia and sway", "mass_speed_exponent", nullptr, &g_rig.mass_speed_exponent, nullptr, 1, false, 0.01f },
	{ "Inertia and sway", "sway_damping_ratio", nullptr, &g_rig.sway_damping_ratio, nullptr, 1, false, 0.01f },

	{ "Animation transitions", "hud_inertialization", nullptr, &g_rig.hud_inertialization, nullptr, 1, false, 0.005f },
	{ "Animation transitions", "idle_fade", "idle_fade", &g_rig.idle_fade, nullptr, 1, false, 0.005f },
	{ "Animation transitions", "additive_blend_in", "additive_blend_in", &g_rig.additive_blend_in, nullptr, 1, false, 0.005f },
	{ "Animation transitions", "additive_blend_out", "additive_blend_out", &g_rig.additive_blend_out, nullptr, 1, false, 0.005f },

	{ "Sprint and safemode", "sprint_pose_inertia", "sprint_pose_inertia", &g_rig.sprint_pose_inertia, nullptr, 1, false, 0.005f },
	{ "Sprint and safemode", "sprint_pose_damping", "sprint_pose_damping", &g_rig.sprint_pose_damping, nullptr, 1, false, 0.01f },
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
	{ "Aim reload", "aim_reload_push_inertia", "aim_reload_push_inertia", &g_rig.aim_reload_push_inertia, nullptr, 1, false, 0.005f },
	{ "Aim reload", "aim_reload_drop_inertia_out", "aim_reload_drop_inertia_out", &g_rig.aim_reload_drop_inertia_out, nullptr, 1, false, 0.005f },

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

	g_rig_enabled = READ_IF_EXISTS(pSettings, r_bool, "camera_rig", "enabled", true);
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

	return !!g_rig_enabled;
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

void CCameraRig::ResetWeaponCache()
{
	m_weapon_sect = nullptr;
}

CCameraRig::~CCameraRig()
{
	for (SLayer& layer : m_layers)
	{
		xr_delete(layer.anim);
	}
}

void CCameraRig::FadeChannel(ECameraRigChannel channel)
{
	for (SLayer& layer : m_layers)
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

void CCameraRig::PreviewAnim(const char* name, ECameraRigChannel channel, float time, float speed)
{
	const auto find = [&]() -> SLayer*
	{
		SLayer* found = nullptr;
		for (SLayer& it : m_layers)
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

	for (const SLayer& layer : m_layers)
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

	SLayer* slot = &m_layers[0];
	for (SLayer& layer : m_layers)
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
	m_trauma = std::clamp(m_trauma + power * g_rig.hit_trauma, 0.f, 1.f);
}

void CCameraRig::OnExplosion(float power)
{
	m_trauma = std::clamp(m_trauma + power * g_rig.explosion_trauma, 0.f, 1.f);
}

void CCameraRig::OnShot(CWeapon* weapon)
{
	if (!Enabled() || weapon == nullptr)
	{
		return;
	}

	const shared_str& sect = weapon->HudSection();
	if (m_weapon_sect != sect)
	{
		m_weapon_sect = sect;
		m_weapon_punch = pSettings->line_exist(sect, "rig_punch") ? Fvector(pSettings->r_fvector3(sect, "rig_punch")).mul(PI / 180.f) : g_rig.punch;
		m_weapon_kick_rot = pSettings->line_exist(sect, "rig_kick_rot") ? Fvector(pSettings->r_fvector3(sect, "rig_kick_rot")).mul(PI / 180.f) : g_rig.kick_rot;
		m_weapon_kick_pos = READ_IF_EXISTS(pSettings, r_fvector3, sect, "rig_kick_pos", g_rig.kick_pos);
		m_weapon_trauma = READ_IF_EXISTS(pSettings, r_float, sect, "rig_shot_trauma", g_rig.shot_trauma);
		m_weapon_fov_kick = READ_IF_EXISTS(pSettings, r_float, sect, "rig_fov_kick", g_rig.shot_fov_kick);
	}

	const float aim = weapon->GetAimFactor();
	const attachable_hud_item* hud_item = weapon->HudItemData();
	const float mass = hud_item != nullptr ? hud_item->m_hud_mass : 1.f;
	const float kick = _sqrt(g_rig.kick_stiffness) * lerp(1.f, g_rig.aim_kick_scale, aim) / _sqrt(mass);
	m_punch.velocity.mad(Fvector().set(m_weapon_punch.y * ::Random.randFs(1.f), m_weapon_punch.x, m_weapon_punch.z * ::Random.randFs(1.f)), _sqrt(g_rig.punch_stiffness) * lerp(1.f, g_rig.aim_punch_scale, aim));
	m_kick.velocity.mad(Fvector().set(m_weapon_kick_rot.y * ::Random.randFs(1.f), m_weapon_kick_rot.x, m_weapon_kick_rot.z * ::Random.randFs(1.f)), kick);
	m_kick_pos.velocity.mad(m_weapon_kick_pos, kick);
	m_trauma = std::clamp(m_trauma + m_weapon_trauma * lerp(1.f, g_rig.aim_kick_scale, aim), 0.f, 1.f);

	if (m_fov_valid && !fis_zero(m_weapon_fov_kick))
	{
		const float zoom = std::min(m_fov.value.x / std::max(g_fov, 1.f), 1.f);
		m_fov.velocity.x += m_weapon_fov_kick * lerp(1.f, g_rig.aim_fov_kick_scale, aim) * zoom * _sqrt(g_rig.fov_stiffness) * 2.71828f;
	}
}

void CCameraRig::OnLand(float speed)
{
	if (!Enabled())
	{
		return;
	}

	const float power = std::clamp((speed - g_rig.land_min_speed) / std::max(g_rig.land_max_speed - g_rig.land_min_speed, EPS_L), 0.f, 1.f);
	m_land.velocity.mad(Fvector().set(-g_rig.land_pitch, -g_rig.land_height, g_rig.land_roll * ::Random.randFs(1.f)), _sqrt(g_rig.land_stiffness) * power);
	m_trauma = std::clamp(m_trauma + g_rig.land_trauma * power, 0.f, 1.f);
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
	for (SLayer& layer : m_layers)
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

void CCameraRig::Update(CActor* actor, float fov, float lean)
{
	if (!Enabled())
	{
		return;
	}

	const float dt = Device.fTimeDelta;
	if (!m_fov_valid)
	{
		m_fov.value.set(fov, 0.f, 0.f);
		m_fov_valid = true;
	}

	PIItem active_item = actor->inventory().ActiveItem();
	CHudItem* item = active_item != nullptr ? active_item->cast_hud_item() : nullptr;
	const float aim = item != nullptr ? item->GetAimFactor() : 0.f;

	Fvector pos = {}, hpb = {}, hud_pos = {}, hud_hpb = {};
	UpdateLayers(dt, pos, hpb, hud_pos, hud_hpb);

	const Fvector zero = {};
	m_fov.value.spring_inertion(Fvector().set(fov, 0.f, 0.f), m_fov.velocity, dt, g_rig.fov_stiffness, g_rig.fov_damping);
	m_punch.value.spring_inertion(zero, m_punch.velocity, dt, g_rig.punch_stiffness, g_rig.punch_damping);
	m_kick.value.spring_inertion(zero, m_kick.velocity, dt, g_rig.kick_stiffness, g_rig.kick_damping);
	m_kick_pos.value.spring_inertion(zero, m_kick_pos.velocity, dt, g_rig.kick_stiffness, g_rig.kick_damping);
	m_land.value.spring_inertion(zero, m_land.velocity, dt, g_rig.land_stiffness, g_rig.land_damping);

	CCameraBase* cam = actor->cam_FirstEye();
	Fvector right;
	right.crossproduct(cam->vNormal, cam->vDirection);
	const float lateral = actor->character_physics_support()->movement()->GetVelocity().dotproduct(right);
	const float roll = std::clamp(lateral * g_rig.strafe_roll - actor->fFPCamYawMagnitude * g_rig.look_roll, -g_rig.roll_max, g_rig.roll_max) * lerp(1.f, g_rig.aim_roll_scale, aim);
	m_roll.value.spring_inertion(Fvector().set(0.f, 0.f, roll), m_roll.velocity, dt, g_rig.roll_stiffness, g_rig.roll_damping);

	m_trauma = std::max(m_trauma - g_rig.trauma_decay * dt, 0.f);
	const float shake = m_trauma * m_trauma * std::min(m_fov.value.x / std::max(g_fov, 1.f), 1.f);
	const float time = Device.fTimeGlobal * g_rig.shake_frequency;
	const Fvector shake_hpb = { rig_noise(time, 1.f) * g_rig.shake_rot.y * shake, rig_noise(time, 2.f) * g_rig.shake_rot.x * shake, rig_noise(time, 3.f) * g_rig.shake_rot.z * shake };
	const Fvector shake_pos = { rig_noise(time, 4.f) * g_rig.shake_pos.x * shake, rig_noise(time, 5.f) * g_rig.shake_pos.y * shake, rig_noise(time, 6.f) * g_rig.shake_pos.z * shake };
	const Fvector land_hpb = { 0.f, m_land.value.x, m_land.value.z };
	const Fvector land_pos = { 0.f, m_land.value.y, 0.f };

	m_world_hpb.add(hpb, m_punch.value).add(m_roll.value).add(shake_hpb).add(land_hpb);
	m_world_pos.add(pos, shake_pos).add(land_pos);
	m_hud_cam_hpb.mad(hud_hpb, m_punch.value, g_rig.punch_hud_scale).add(m_roll.value).mad(shake_hpb, g_rig.shake_hud_scale).add(land_hpb);
	m_hud_cam_pos.mad(hud_pos, shake_pos, g_rig.shake_hud_scale).add(land_pos);
	m_hud_hpb.mad(m_kick.value, land_hpb, g_rig.land_hud_scale - 1.f);
	m_hud_pos.mad(m_kick_pos.value, land_pos, g_rig.land_hud_scale - 1.f);
	m_lean = lean;
	m_update_frame = Device.dwFrame;
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
	if (m_update_frame != Device.dwFrame)
	{
		return;
	}

	rig_offset(world, m_world_pos, m_world_hpb);
	rig_offset(hud, m_hud_cam_pos, m_hud_cam_hpb);
	world.fFov = m_fov.value.x;
	hud.fFov = m_fov.value.x;
}

void CCameraRig::ApplyHud(Fmatrix& trans)
{
	if (m_update_frame != Device.dwFrame)
	{
		return;
	}

	Fmatrix offset;
	offset.setHPB(m_hud_hpb.x, m_hud_hpb.y, m_hud_hpb.z);
	offset.c = m_hud_pos;
	trans.mulB_43(offset);
}

float CCameraRig::LeanCounter() const
{
	return m_update_frame == Device.dwFrame ? -m_lean * g_rig.lean_hud_counter : 0.f;
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
	return g_rig.inertia_speed / powf(std::max(mass, 0.1f), g_rig.mass_speed_exponent);
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

bool CCameraRig::SpringSway(Fvector& value, Fvector& velocity, const Fvector& target, float speed)
{
	if (!Enabled())
	{
		return false;
	}

	value.spring_inertion(target, velocity, Device.fTimeDelta, speed * speed, 2.f * speed * g_rig.sway_damping_ratio);
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

void CCameraRig::SolveLimb(IKinematics* model, const u16* chain, const Fmatrix& target, float weight)
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
	pole.sub(joint, root);
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
