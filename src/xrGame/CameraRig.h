#pragma once
#include "../xrEngine/CameraDefs.h"

class CActor;
class CWeapon;
class CObjectAnimator;
class IKinematics;
struct attachable_hud_item;

enum ECameraRigChannel
{
	crcMotion,
	crcAction,
	crcShot,
	crcMove,
	crcSprint,
	crcCount
};

struct SCameraRigParams
{
	float	blend_in[crcCount]	= { 0.12f, 0.08f, 0.f, 0.15f, 0.1f };
	float	blend_out[crcCount]	= { 0.25f, 0.15f, 0.08f, 0.3f, 0.2f };
	float	fov_stiffness		= 500.f, fov_damping = 45.f;
	float	shot_fov_kick		= 0.f, aim_fov_kick_scale = 0.5f;
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
	float	hud_ik_share		= 0.8f, turn_sway_smooth = 0.05f, aim_reload_blend = 0.25f, left_hand_ik_follow = 0.3f, shot_anim_blend = 0.1f;
	float	inertia_speed		= 1.3f, aim_reload_tail = 0.2f, shot_anim_tail = 0.3f;
	float	hud_inertialization	= 0.f;
	float	aim_reload_motion	= 1.f, shot_anim_variation = 0.f, shot_anim_twist = 0.f, aim_shot_motion = 1.f, idle_fade = 0.f, inertia_damping = 0.55f;
	float	additive_blend_in	= 0.f, additive_blend_out = 0.1f, left_hand_attach_blend = 0.f, left_hand_ik_blend = 0.25f, left_hand_finger_grip = 1.f;
	float	cam_shoot_variation	= 0.f, cam_shoot_speed_variation = 0.f;
	bool	cam_shoot_mirror	= false;
	float	aim_blend_inertia	= 0.f;
	float	sprint_pose_inertia	= 0.08f, safemode_damping = 0.6f, sprint_blend_in = 0.f, sprint_left_grip_range = 0.6f, sprint_left_cancel_return = 0.35f;
	Fvector	aim_reload_hud_offset_pos = { 0.f, 0.f, 0.f };
	Fvector	aim_reload_hud_offset_rot = { 0.f, 0.f, 0.f };
	Fvector	aim_reload_hud_push_pos = { 0.f, 0.f, 0.f };
	float	aim_reload_zoom_out	= 0.f, aim_reload_pose_inertia = 0.15f, aim_reload_pose_inertia_out = 0.05f;
	Fvector	crouch_hud_offset_pos = { 0.f, 0.f, 0.f };
	Fvector	crouch_hud_offset_rot = { 0.f, 0.f, 0.f };
	Fvector	walk_hud_offset_pos = { 0.f, 0.f, 0.f };
	Fvector	walk_hud_offset_rot = { 0.f, 0.f, 0.f };
	Fvector	look_sway_pos		= { 0.004f, 0.003f, 0.002f };
	float	aim_sway_scale		= 0.2f, look_sway_inertia = 0.04f, look_sway_damping = 0.8f;
	Fvector	left_hand_sway_look	= { 0.f, 0.f, 0.f };
	Fvector	left_hand_sway_bob	= { 0.f, 0.f, 0.f };
	Fvector	left_hand_sway_jump	= { 0.f, 0.f, 0.f };
	Fvector	right_hand_sway_look = { 0.f, 0.f, 0.f };
	Fvector	right_hand_sway_jump = { 0.f, 0.f, 0.f };
	Fvector	left_hand_bob_pos	= { 1.f, 1.f, 1.f };
	Fvector	left_hand_bob_rot	= { 1.f, 1.f, 1.f };
	float	left_hand_follow_lag = 0.f, left_hand_follow_turn = 1.f, left_hand_follow_move_rot = 1.f;
	Fvector	left_hand_follow_move = { 1.f, 1.f, 1.f };
	float	hand_sway_inertia	= 0.08f, hand_sway_damping = 0.6f;
	Fvector	turn_pivot_look		= { 0.f, 0.f, 0.f };
	float	turn_pivot_max		= 6.f, turn_pivot_inertia = 0.07f, turn_pivot_damping = 0.5f, aim_turn_pivot_scale = 0.3f, turn_pivot_grip_lag = 1.f;
	float	turn_pivot_lift		= 0.f, turn_pivot_lift_pos = 0.f, turn_pivot_lift_max = 0.01f, turn_pivot_lift_inertia = 0.08f, turn_pivot_lift_fall = 1.f, turn_tilt = 0.f, turn_tilt_max = 8.f;
	float	crouch_pose_inertia	= 0.25f, crouch_pose_inertia_out = -1.f, crouch_pose_delay = 0.f, walk_pose_delay = 5.f, walk_pose_inertia = 0.6f, walk_pose_inertia_out = 0.12f, carry_pose_damping = 0.8f;
};

struct SCameraRigParam
{
	const char*	Group;
	const char*	Key;
	const char*	HudKey;
	float*		Value;
	bool*		Flag;
	u32			Count;
	bool		Degrees;
	float		Speed;
};

class CCameraRig
{
	struct SLayer
	{
		CObjectAnimator*	anim		= nullptr;
		float				weight		= 0.f;
		ECameraRigChannel	channel		= crcMotion;
		bool				active		= false;
		bool				fade_out	= false;
		bool				hud_affect	= false;
		Fvector				scale_hpb	= { 1.f, 1.f, 1.f };
		Fvector				scale_pos	= { 1.f, 1.f, 1.f };
	};

	struct SSpring
	{
		Fvector				value		= {};
		Fvector				velocity	= {};
	};

	SLayer					Layers[12];
	SSpring					Punch, Kick, KickPos, Land, Roll, Fov, LookSway;
	Fvector					WorldPos = {}, WorldHpb = {}, HudCamPos = {}, HudCamHpb = {}, HudPos = {}, HudHpb = {};
	shared_str				WeaponSect;
	Fvector					WeaponPunch = {}, WeaponKickPos = {}, WeaponKickRot = {};
	float					WeaponTrauma = 0.f;
	float					WeaponFovKick = 0.f;
	float					Trauma = 0.f;
	Fvector					LookRate = {};
	float					LookAim = 0.f;
	u32						LookFrame = u32(-1);
	u32						UpdateFrame = u32(-1);
	bool					FovValid = false;

	void					FadeChannel		(ECameraRigChannel channel);
	void					UpdateLayers	(float dt, Fvector& pos, Fvector& hpb, Fvector& hud_pos, Fvector& hud_hpb);
	void					UpdateLookSway	(const attachable_hud_item* item);

public:
							~CCameraRig		();

	static bool				Enabled			();
	static void				Reload			();
	static void				ReloadGroup		(const char* group);
	static const SCameraRigParams& Params	();
	static const SCameraRigParam* ParamList	(u32& count);
	static const SCameraRigParam* FindHudParam(const char* hud_key);
	static float			HudIkShare		();
	static float			TurnSwaySmooth	();
	static float			AimReloadBlend	();
	static float			LeftHandIkFollow();
	static float			ShotAnimBlend	();
	static float			MassSpeed		(float mass);
	static float			AimReloadTail	();
	static float			ShotAnimTail	();
	static float			HudInertialization();
	static bool				SpringSway		(Fvector& value, Fvector& velocity, const Fvector& target, float half_life, float damping);
	static float			LookHalfLife	(const attachable_hud_item* item);
	static float			LookDamping		(const attachable_hud_item* item);
	static void				TransformBone	(IKinematics* model, u16 bone, const Fmatrix& delta);
	static void				SolveLimb		(IKinematics* model, const u16* chain, const Fmatrix& target, float weight);

	bool					PlayAnim		(const char* name, ECameraRigChannel channel, float speed, bool hud_affect, const Fvector& scale_hpb = { 1.f, 1.f, 1.f }, const Fvector& scale_pos = { 1.f, 1.f, 1.f });
	void					StopChannel		(ECameraRigChannel channel);
	void					SeekChannel		(ECameraRigChannel channel, float progress);
	void					PreviewAnim		(const char* name, ECameraRigChannel channel, float time, float speed);
	void					ResetWeaponCache();
	void					OnShot			(CWeapon* weapon);
	void					OnHit			(float power);
	void					OnExplosion		(float power);
	void					OnLand			(float speed);
	void					Update			(CActor* actor, float fov);
	void					Apply			(SCamEffectorInfo& world, SCamEffectorInfo& hud);
	void					ApplyHud		(Fmatrix& trans, const attachable_hud_item* item);
	const Fvector&			LookSwayOffset	() const { return LookSway.value; }
};
