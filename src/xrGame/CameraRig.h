#pragma once
#include "../xrEngine/CameraDefs.h"
#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/KinematicsAnimated.h"

class CActor;
class CWeapon;
class CObjectAnimator;
class IKinematics;
struct attachable_hud_item;
struct SHandsIkFrame;

namespace HudAttachMarks
{
	constexpr const char* Ik = "mm_ik";
	constexpr const char* Fingers = "mm_ik_fingers";
}

namespace HudSpring
{
	inline void Solve(float& value, float& velocity, float goal, float omega, float damping, float dt)
	{
		if (omega <= 0.f || dt <= 0.f)
		{
			return;
		}

		const float offset = value - goal;
		const float speed = velocity;
		const float zeta = std::max(damping, 0.f);

		if (std::abs(zeta - 1.f) < 1e-3f)
		{
			const float decay = std::exp(-omega * dt);
			const float slope = speed + omega * offset;
			value = goal + (offset + slope * dt) * decay;
			velocity = (speed - omega * slope * dt) * decay;
		}
		else if (zeta < 1.f)
		{
			const float damped = omega * std::sqrt(1.f - zeta * zeta);
			const float decay = std::exp(-zeta * omega * dt);
			const float cosine = std::cos(damped * dt);
			const float sine = std::sin(damped * dt);
			const float b = (speed + zeta * omega * offset) / damped;
			value = goal + decay * (offset * cosine + b * sine);
			velocity = decay * (speed * cosine - (offset * damped + zeta * omega * b) * sine);
		}
		else
		{
			const float root = omega * std::sqrt(zeta * zeta - 1.f);
			const float r1 = -zeta * omega + root;
			const float r2 = -zeta * omega - root;
			const float c2 = (speed - r1 * offset) / (r2 - r1);
			const float c1 = offset - c2;
			const float e1 = std::exp(r1 * dt);
			const float e2 = std::exp(r2 * dt);
			value = goal + c1 * e1 + c2 * e2;
			velocity = c1 * r1 * e1 + c2 * r2 * e2;
		}
	}

	inline void Step(Fvector& value, Fvector& velocity, const Fvector& goal, float omega, float damping, float dt)
	{
		Solve(value.x, velocity.x, goal.x, omega, damping, dt);
		Solve(value.y, velocity.y, goal.y, omega, damping, dt);
		Solve(value.z, velocity.z, goal.z, omega, damping, dt);
	}

	inline void Follow(Fvector& value, Fvector& velocity, const Fvector& goal, float half_life, float damping, float dt)
	{
		if (half_life <= 0.f)
		{
			value = goal;
			velocity.set(0.f, 0.f, 0.f);
			return;
		}

		Step(value, velocity, goal, 2.f * 0.69314718f / half_life, damping, dt);
	}

	inline void Step(float& value, float& velocity, float goal, float half_life, float damping, float dt)
	{
		if (half_life <= 0.f)
		{
			value = goal;
			velocity = 0.f;
			return;
		}

		Solve(value, velocity, goal, 2.f * 0.69314718f / half_life, damping, dt);
	}
}

namespace HudBlend
{
	inline float Ramp(float weight, bool on, float time_in, float time_out, float dt)
	{
		const float time = on ? time_in : time_out;
		if (time <= 0.f)
		{
			return on ? 1.f : 0.f;
		}

		return std::clamp(weight + (on ? dt : -dt) / time, 0.f, 1.f);
	}

	inline float Ramp(float weight, bool on, float time, float dt)
	{
		return Ramp(weight, on, time, time, dt);
	}

	inline float Smooth(float time, float dt)
	{
		return time > 0.f ? 1.f - expf(-dt / time) : 1.f;
	}
}

struct SHudVectorSpring
{
	Fvector Value = {};
	Fvector Velocity = {};
};

struct SHudLeftFollow
{
	Fquaternion Rotation;
	Fvector Position = {};
	Fvector PositionVelocity = {};
	Fvector RotationVelocity = {};
	u32 Frame = u32(-1);
};

struct SHudTurnPivot
{
	Fvector2 Rate = {};
	float LiftSpeed = 0.f;
	SHudVectorSpring Lag;
	SHudVectorSpring Lift;
	float Tilt = 0.f;
	float TiltVelocity = 0.f;
};

struct SHudShotFingers
{
	bool Pending = false;
	bool Active = false;
	bool Stopping = false;
	bool Full = false;
	float Weight = 0.f;
	float SinceShot = 0.f;
	u32 BurstShots = 0;
	float Time = 0.f;
	float First = 0.f;
	float Second = 0.f;
	float Speed = 1.f;
	float Fade = 1.f;
	MotionID Motion;
	xr_vector<u16> Bones;
	xr_vector<Fquaternion> Reference;
	xr_vector<Fvector> ReferencePos;
};

struct SHudHandJump
{
	u32 StatePrev = 0;
	float FallSpeed = 0.f;
	float Time = 1000.f;
	float Power = 0.f;
	float Weight = 0.f;
	float Velocity = 0.f;
};

struct SHudHandsIk
{
	u16 LeftIkChain[3] = { BI_NONE, BI_NONE, BI_NONE };
	u16 RightIkChain[3] = { BI_NONE, BI_NONE, BI_NONE };
	float LeftIkWeight = 0.f;
	bool LayersIk = false;
	Fmatrix LayersIkOffset = Fidentity;
	SHudVectorSpring LeftSway;
	SHudVectorSpring RightSway;
	SHudTurnPivot TurnPivot;
	SHudShotFingers ShotFingers;
	SHudHandJump HandJump;
	SHudLeftFollow LeftFollow;

	float RetargetWeight = 0.f;
	Fmatrix RetargetLeftTarget = Fidentity;
	Fmatrix RetargetRightTarget = Fidentity;
	bool RetargetLeftValid = false;
	bool RetargetRightValid = false;

	float LeftHandAttachWeight = 0.f;
	bool LeftHandAttachReaching = false;
	u32 LeftHandAttachSnapFrame = u32(-1);
	float LeftHandAttachHold = 0.f;
	float LeftHandAttachArmWeight = 1.f;
	bool LeftHandAttachAnimatedArm = false;
	bool LeftGripActive = false;

	xr_vector<u16> FingerGripBones;
	xr_vector<Fquaternion> FingerStartPose;
	xr_vector<Fvector> FingerStartPos;
	MotionID FingerStartMotion;
	float FingerStartTime = 0.f;
	xr_vector<Fquaternion> FingerPrevRot;
	xr_vector<Fvector> FingerPrevPos;
	xr_vector<Fquaternion> FingerResidualRot;
	xr_vector<Fvector> FingerResidualPos;
	u32 FingerResidualFrame = u32(-1);

	MotionID ArmRetargetMotion;
	xr_vector<Fquaternion> ArmRetargetPose;
	Fmatrix ArmRetargetRootRef = Fidentity;

	MotionID AttachOffsetMotion;
	bool AttachOffsetSampled = false;
	Fmatrix AttachOffsetRef = Fidentity;

	void ResetModel()
	{
		FingerGripBones.clear();
		ArmRetargetMotion = MotionID();
	}

	void ResetItem()
	{
		RetargetWeight = 0.f;
		RetargetLeftValid = false;
		RetargetRightValid = false;
		ArmRetargetMotion = MotionID();
		ShotFingers = {};
	}
};

struct SHudAimReload
{
	Fmatrix Ref = Fidentity;
	float Weight = 0.f;
	float Release = 1.f;
	float PoseValue = 0.f;
	float PoseVelocity = 0.f;
	float PushValue = 0.f;
	float PushVelocity = 0.f;
	float DropValue = 0.f;
	float DropVelocity = 0.f;

	float Pose() const { return std::clamp(PoseValue, 0.f, 1.f); }
	float Push() const { return std::clamp(PushValue, 0.f, 1.f); }
	float Drop() const { return std::clamp(DropValue, 0.f, 1.f); }

	void ResetPose()
	{
		PoseValue = 0.f;
		PoseVelocity = 0.f;
		PushValue = 0.f;
		PushVelocity = 0.f;
		DropValue = 0.f;
		DropVelocity = 0.f;
	}

	void Reset()
	{
		Weight = 0.f;
		Release = 1.f;
		ResetPose();
	}

	static bool Wanted(const attachable_hud_item* Item);
	void Update(const attachable_hud_item* Item, bool Active);
	void UpdateRelease(float Progress);
};

struct SHudSafemode
{
	Fvector VelocityPos = {};
	Fvector VelocityRot = {};
	float Damping = -1.f;
	bool FeedbackPending = false;
	bool FeedbackFrom = false;
	bool TogglePending = false;

	void ResetVelocity()
	{
		VelocityPos.set(0.f, 0.f, 0.f);
		VelocityRot.set(0.f, 0.f, 0.f);
	}

	float SpringDamping() const;
	void Spring(const Fvector& CurrentPos, Fvector& SavedPos, const Fvector& CurrentRot, Fvector& SavedRot, float HalfLife);
};

enum class EHudSprintStage : u8
{
	None,
	Start,
	Loop,
	End
};

struct SHudSprintLeftCancel
{
	bool Active = false;
	float Weight = 0.f;
	float Hold = 1.f;
	float Time = 0.f;
	Fmatrix Relative = Fidentity;
};

struct SHudSprint
{
	EHudSprintStage				Stage = EHudSprintStage::None;
	const attachable_hud_item*	Item = nullptr;
	MotionID					StartMotion;
	MotionID					LoopMotion;
	MotionID					EndMotion;
	MotionID					ReferenceMotion;
	bool						LayerSeam = false;
	bool						LeftFree = false;
	SHudSprintLeftCancel		LeftCancel;
	Fmatrix						LeftWristLast = Fidentity;
	u32							LayerFrame = u32(-1);
	Fmatrix						LayerPrev[2] = { Fidentity, Fidentity };
	Fmatrix						LayerResidual[2] = { Fidentity, Fidentity };
	Fmatrix						BasePrev[2] = { Fidentity, Fidentity };
	Fmatrix						BaseResidual[2] = { Fidentity, Fidentity };
	MotionID					BaseMotion;
	u32							BaseState = 0;
	float						EndPower = 1.f;
	float						Pose = 0.f;
	float						PoseShape = 0.f;
	float						ExitDeviation = -1.f;
	float						ExitPose = 0.f;
	float						Target = 0.f;
	float						PoseVelocity = 0.f;
	float						Yield = 0.f;
	float						HandWeight = 0.f;

	static bool		BaseTransform	(IKinematicsAnimated* Model, u16 Bone, Fmatrix& Result);
	static bool		BoneDelta		(IKinematicsAnimated* Model, u16 Bone, Fmatrix& Result);

	bool			Enabled			(IKinematicsAnimated* Model, const attachable_hud_item* Current) const;
	void			Attach			(IKinematicsAnimated* Model, const attachable_hud_item* Current);
	void			Update			(IKinematicsAnimated* Model, const attachable_hud_item* Current, bool Sprinting);
	void			UpdateChain		(IKinematicsAnimated* Model, const attachable_hud_item* Current);
	void			UpdatePose		(IKinematicsAnimated* Model, const attachable_hud_item* Current, u16 Ancor);
	void			CutEnd			(IKinematicsAnimated* Model);
	void			SmoothLayer		(SHandsIkFrame& F);
	void			SmoothBase		(IKinematicsAnimated* Model, SHandsIkFrame& F);
	void			Resolve			(IKinematicsAnimated* Model, const attachable_hud_item* Current);
	bool			Play			(IKinematicsAnimated* Model, const attachable_hud_item* Current, MotionID Motion, bool Loop, float Phase, float Power = 1.f, bool Inertialize = false);
	void			Stop			(IKinematicsAnimated* Model, float Blend);
	void			StopInertial	(IKinematicsAnimated* Model, float Blend);
	float			Progress		(IKinematicsAnimated* Model, MotionID Motion) const;
	static void		Freeze			(IKinematicsAnimated* Model, MotionID Motion);
	float			PoseTarget		(IKinematicsAnimated* Model, u16 Ancor);
	static float	LayerDeviation	(IKinematicsAnimated* Model, u16 Ancor);
	void			BeginLeftCancel	();
};

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
	Fvector	aim_left_elbow_offset = { 0.f, 0.f, 0.f };
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
	float	shot_fingers_chance	= 0.f, shot_fingers_delay = 0.2f, shot_fingers_blend = 0.08f, shot_fingers_blend_out = 0.3f, shot_fingers_burst_chance = 0.f, shot_fingers_max_chance = 1.f, shot_fingers_full_chance = 0.f;
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
	static void				SolveLimb		(IKinematics* model, const u16* chain, const Fmatrix& target, float weight, const Fvector& PoleOffset = Fvector().set(0.f, 0.f, 0.f));

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
