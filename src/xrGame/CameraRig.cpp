#include "StdAfx.h"
#include "CameraRig.h"
#include "Actor.h"
#include "Weapon.h"
#include "Inventory.h"
#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"
#include "Level.h"
#include "../xrEngine/CameraBase.h"
#include "../xrEngine/ObjectAnimator.h"
#include "../Include/xrRender/Kinematics.h"

struct SCameraRigParams
{
	float	blend_in[crcCount]	= { 0.12f, 0.08f, 0.f, 0.15f };
	float	blend_out[crcCount]	= { 0.25f, 0.15f, 0.08f, 0.3f };
	float	fov_stiffness		= 500.f, fov_damping = 45.f;
	Fvector	punch				= { 0.45f, 0.2f, 0.35f };
	float	punch_stiffness		= 220.f, punch_damping = 16.f, punch_hud_scale = 1.f, aim_punch_scale = 0.5f;
	Fvector	kick_pos			= { 0.f, 0.004f, -0.03f };
	Fvector	kick_rot			= { 1.6f, 0.4f, 0.8f };
	float	kick_stiffness		= 320.f, kick_damping = 22.f, aim_kick_scale = 0.3f;
	float	shot_trauma			= 0.04f, hit_trauma = 0.6f, explosion_trauma = 0.9f, trauma_decay = 1.4f;
	float	shake_frequency		= 17.f, shake_hud_scale = 0.35f;
	Fvector	shake_rot			= { 2.f, 2.f, 3.f };
	Fvector	shake_pos			= { 0.008f, 0.008f, 0.004f };
	float	land_min_speed		= 4.f, land_max_speed = 14.f, land_pitch = 2.5f, land_roll = 0.8f, land_height = 0.07f;
	float	land_trauma			= 0.3f, land_hud_scale = 1.6f, land_stiffness = 160.f, land_damping = 12.f;
	float	strafe_roll			= 0.4f, look_roll = 0.12f, roll_max = 3.f, aim_roll_scale = 0.3f, roll_stiffness = 90.f, roll_damping = 16.f;
	float	sway_damping_ratio	= 0.6f;
	float	lean_hud_counter	= 0.7f;
};

static SCameraRigParams	g_rig;
static int				g_rig_enabled = -1;

bool CCameraRig::Enabled()
{
	if (g_rig_enabled >= 0)
	{
		return !!g_rig_enabled;
	}

	g_rig_enabled = pSettings->section_exist("camera_rig") && READ_IF_EXISTS(pSettings, r_bool, "camera_rig", "enabled", true);
	if (!g_rig_enabled)
	{
		return false;
	}

	SCameraRigParams& p = g_rig;
	const struct { const char* key; float* value; u32 count; bool deg; } params[] =
	{
		{ "motion_blend_in", &p.blend_in[crcMotion], 1, false }, { "motion_blend_out", &p.blend_out[crcMotion], 1, false },
		{ "action_blend_in", &p.blend_in[crcAction], 1, false }, { "action_blend_out", &p.blend_out[crcAction], 1, false },
		{ "shot_blend_in", &p.blend_in[crcShot], 1, false }, { "shot_blend_out", &p.blend_out[crcShot], 1, false },
		{ "move_blend_in", &p.blend_in[crcMove], 1, false }, { "move_blend_out", &p.blend_out[crcMove], 1, false },
		{ "fov_stiffness", &p.fov_stiffness, 1, false }, { "fov_damping", &p.fov_damping, 1, false },
		{ "punch", &p.punch.x, 3, true }, { "punch_stiffness", &p.punch_stiffness, 1, false }, { "punch_damping", &p.punch_damping, 1, false },
		{ "punch_hud_scale", &p.punch_hud_scale, 1, false }, { "aim_punch_scale", &p.aim_punch_scale, 1, false },
		{ "kick_pos", &p.kick_pos.x, 3, false }, { "kick_rot", &p.kick_rot.x, 3, true },
		{ "kick_stiffness", &p.kick_stiffness, 1, false }, { "kick_damping", &p.kick_damping, 1, false }, { "aim_kick_scale", &p.aim_kick_scale, 1, false },
		{ "shot_trauma", &p.shot_trauma, 1, false }, { "hit_trauma", &p.hit_trauma, 1, false },
		{ "explosion_trauma", &p.explosion_trauma, 1, false }, { "trauma_decay", &p.trauma_decay, 1, false },
		{ "shake_frequency", &p.shake_frequency, 1, false }, { "shake_hud_scale", &p.shake_hud_scale, 1, false },
		{ "shake_rot", &p.shake_rot.x, 3, true }, { "shake_pos", &p.shake_pos.x, 3, false },
		{ "land_min_speed", &p.land_min_speed, 1, false }, { "land_max_speed", &p.land_max_speed, 1, false },
		{ "land_pitch", &p.land_pitch, 1, true }, { "land_roll", &p.land_roll, 1, true }, { "land_height", &p.land_height, 1, false },
		{ "land_trauma", &p.land_trauma, 1, false }, { "land_hud_scale", &p.land_hud_scale, 1, false },
		{ "land_stiffness", &p.land_stiffness, 1, false }, { "land_damping", &p.land_damping, 1, false },
		{ "strafe_roll", &p.strafe_roll, 1, true }, { "look_roll", &p.look_roll, 1, true }, { "roll_max", &p.roll_max, 1, true },
		{ "aim_roll_scale", &p.aim_roll_scale, 1, false }, { "roll_stiffness", &p.roll_stiffness, 1, false }, { "roll_damping", &p.roll_damping, 1, false },
		{ "sway_damping_ratio", &p.sway_damping_ratio, 1, false }, { "lean_hud_counter", &p.lean_hud_counter, 1, false },
	};

	for (const auto& it : params)
	{
		if (pSettings->line_exist("camera_rig", it.key))
		{
			if (it.count == 3)
			{
				*(Fvector*)it.value = pSettings->r_fvector3("camera_rig", it.key);
			}
			else
			{
				*it.value = pSettings->r_float("camera_rig", it.key);
			}
		}

		for (u32 i = 0; it.deg && i < it.count; i++)
		{
			it.value[i] = deg2rad(it.value[i]);
		}
	}

	return true;
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

bool CCameraRig::PlayAnim(const char* name, ECameraRigChannel channel, float speed, bool hud_affect)
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
	}

	const float aim = weapon->GetAimFactor();
	const float kick = _sqrt(g_rig.kick_stiffness) * lerp(1.f, g_rig.aim_kick_scale, aim);
	m_punch.velocity.mad(Fvector().set(m_weapon_punch.y * ::Random.randFs(1.f), m_weapon_punch.x, m_weapon_punch.z * ::Random.randFs(1.f)), _sqrt(g_rig.punch_stiffness) * lerp(1.f, g_rig.aim_punch_scale, aim));
	m_kick.velocity.mad(Fvector().set(m_weapon_kick_rot.y * ::Random.randFs(1.f), m_weapon_kick_rot.x, m_weapon_kick_rot.z * ::Random.randFs(1.f)), kick);
	m_kick_pos.velocity.mad(m_weapon_kick_pos, kick);
	m_trauma = std::clamp(m_trauma + m_weapon_trauma * lerp(1.f, g_rig.aim_kick_scale, aim), 0.f, 1.f);
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
		hpb.mad(anim_hpb, weight);
		pos.mad(layer.anim->XFORM().c, weight);

		if (layer.hud_affect)
		{
			hud_hpb.mad(anim_hpb, weight);
			hud_pos.mad(layer.anim->XFORM().c, weight);
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
