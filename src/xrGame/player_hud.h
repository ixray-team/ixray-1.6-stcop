#pragma once
#include "firedeps.h"

#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "actor_defs.h"
#include "../xrEngine/ObjectAnimator.h"
#include "../../xrUI/ui_base.h"

class player_hud;
struct SHandsIkFrame;
class CWatchDevice;
class CHudItem;
class CMotionDef;
class CHudAnimatorBase;
class CObjectAnimator;

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

struct SHudHandJump
{
	u32 StatePrev = 0;
	float FallSpeed = 0.f;
	float Time = 1000.f;
	float Power = 0.f;
	float Weight = 0.f;
	float Velocity = 0.f;
};

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

enum EHudMixType : u8
{
	eNoMix = 0,
	eMixHands,
	eMixAll
};

enum class EHudCycleMode : u8
{
	Replace,
	KeepBase,
	Additive
};

struct SHudCycleMode
{
	EHudCycleMode	Type = EHudCycleMode::Replace;
	float			BlendIn = 0.f;
	float			BlendOut = 0.f;
	bool			Loop = false;
	bool			FadeAdditive = false;
	MotionID		Reference;
};

struct SHudRetargetLayer
{
	shared_str		Motion;
	shared_str		Reference;
};

enum EHudOffsetAxis : u8
{
	eAxisPos,
	eAxisRot,
	eAxisMax
};
enum EHudOffsetType : u8
{
	eDefault,
	eAim,
	eAimGL,
	eAimAlt,
	eSafemode,
	eCollision,
	eHudOffsetsMax
};

enum EMovementLayers : u8
{
	eWalk = 0,
	eWalkSlow,
	eCrouch,
	eCrouchSlow,
	eSprint,
	eIdle,
	eIdleAim,
	eAimWalk,
	eStrafeLeft,
	eStrafeRight,
	eLayersCount
};

struct motion_descr final
{
	MotionID		mid;
	shared_str		name;
};

struct player_hud_motion final
{
	shared_str				m_alias_name;
	shared_str				m_base_name;
	shared_str				m_additional_name;
	xr_vector<shared_str>	m_bone_parts;
	float					m_anim_speed;
	xr_vector<motion_descr>	m_animations;
};

struct attachable_hud_item_motion final
{
	shared_str				m_alias_name;
	shared_str				m_name;
	xr_vector<shared_str>	m_bone_parts;
	float					m_anim_speed;
	xr_vector<motion_descr>	m_animations;
};

struct player_hud_motion_container final
{
	xr_vector<player_hud_motion> m_anims;
	xr_vector<attachable_hud_item_motion> m_item_anims;
	xr_hash_map<shared_str, bool> m_names;
	xr_vector<shared_str> m_banned_bone_parts = {};
	xr_hash_map<shared_str, shared_str> PoseOverrides;
	player_hud_motion* find_motion(const shared_str& name);
	attachable_hud_item_motion* find_item_motion(const shared_str& name);
	bool has_motion(const shared_str& name);
	void load(IKinematicsAnimated* model, const shared_str& sect, IKinematicsAnimated* item_model = nullptr);
	void load_default_motions(IKinematicsAnimated* model, const CInifile::Item& data, const shared_str& sect);
	void load_bonepart_motions(IKinematicsAnimated* model, const CInifile::Item& data, const shared_str& sect);
};

struct weapon_inertion final
{
	struct base_params
	{
		Fvector position;
		Fvector rotation;
		void Load(const shared_str& section, const shared_str& str, bool is_16x9);
	};

	base_params move_suicide_offset;

	base_params move_to_crouch_offset;
	base_params move_from_crouch_offset;
	base_params move_to_slow_crouch_offset;
	base_params move_from_slow_crouch_offset;

	base_params move_to_rlookout_offset;
	base_params move_from_rlookout_offset;
	base_params move_to_llookout_offset;
	base_params move_from_llookout_offset;

	base_params aim_move_to_crouch_offset;
	base_params aim_move_from_crouch_offset;
	base_params aim_move_to_slow_crouch_offset;
	base_params aim_move_from_slow_crouch_offset;

	base_params aim_move_to_rlookout_offset;
	base_params aim_move_from_rlookout_offset;
	base_params aim_move_to_llookout_offset;
	base_params aim_move_from_llookout_offset;

	base_params aim_move_jump_offset;
	base_params aim_move_fall_offset;
	base_params aim_move_landing_offset;
	base_params aim_move_landing2_offset;

	base_params move_rlookout_offset;
	base_params move_llookout_offset;

	base_params aim_move_rlookout_offset;
	base_params aim_move_llookout_offset;

	base_params move_left_offset;
	base_params move_right_offset;
	base_params move_forward_offset;
	base_params move_back_offset;

	base_params move_crouch_offset;
	base_params move_slow_crouch_offset;

	base_params move_jump_offset;
	base_params move_fall_offset;
	base_params move_landing_offset;
	base_params move_landing2_offset;

	float move_rlookout_offset_speed_factor = 1.0f;
	float move_llookout_offset_speed_factor = 1.0f;

	float aim_move_slow_crouch_factor = 1.0f;
	float aim_move_crouch_factor = 1.0f;
	float aim_move_slow_factor = 1.0f;

	float move_slow_crouch_factor = 1.0f;
	float move_crouch_factor = 1.0f;
	float move_slow_factor = 1.0f;

	bool no_other_hud_moving_while_suicide = false;

	u32 to_crouch_time = 0;
	u32 from_crouch_time = 0;
	u32 to_slow_crouch_time = 0;
	u32 from_slow_crouch_time = 0;

	u32 to_rlookout_time = 0;
	u32 from_rlookout_time = 0;
	u32 to_llookout_time = 0;
	u32 from_llookout_time = 0;

	float move_weaponhide_factor = 1.0f;
	float move_unzoom_factor = 1.0f;

	float move_speed_pos = 0.1f;
	float move_speed_rot = 0.4f;

	float move_suicide_speed_pos = 0.2f;
	float move_suicide_speed_rot = 0.002f;

	float move_stabilize_factor = 2.0f;

	void Load(const shared_str& section, bool is_16x9);
};

struct hud_item_measures final
{
	enum{e_fire_point=(1<<0), e_fire_point2=(1<<1), e_shell_point=(1<<2), e_16x9_mode_now=(1<<3)};
	Flags8							m_prop_flags;

	Fvector m_item_attach[EHudOffsetAxis::eAxisMax]; // pos,rot

	struct hud_hands_positions
	{
		void Load(const shared_str& section, bool is_16x9);
		void LoadAimParams(const shared_str& section, bool is_16x9, bool default_is_self);
		Fvector hands_offsets[EHudOffsetAxis::eAxisMax][EHudOffsetType::eHudOffsetsMax]; // pos,rot //normal, aim, gl, alt-aim, safemode
		Fvector hands_offsets_saved[EHudOffsetAxis::eAxisMax];							 // pos,rot
		Fvector hands_offsets_tune[EHudOffsetAxis::eAxisMax][EHudOffsetType::eHudOffsetsMax];
		bool bIs16x9 = false;
		shared_str sSection;
	} m_hands_positions;

	struct inertion_params
	{
		float m_tendto_speed;
		float m_tendto_speed_aim;
		float m_tendto_ret_speed;
		float m_tendto_ret_speed_aim;
	};

	struct collision_params
	{
		float stifness = 250.0f;
		float damping = 250.0f;
		Fvector obb_scale = { 1.0f, 1.0f, 1.0f };
		Fvector obb_pos = zero_vel;
		float dist_min = 0.5f;
		float dist_max = 0.8f;
	} m_collision_params;

	inertion_params m_inertion_params; //--#SM+#--
	weapon_inertion m_weapon_inertion;

	u16								m_fire_bone = u16(-1);
	Fvector							m_fire_point_offset;
	u16								m_fire_bone2 = u16(-1);
	Fvector							m_fire_point2_offset;
	u16								m_shell_bone = u16(-1);
	Fvector							m_shell_point_offset;

	Fvector							m_hands_attach_real[2];//pos,rot

	void load						(const shared_str& sect_name, IKinematics* K, bool combined_model);
};

struct attachable_hud_item final
{
	Fobb							m_collision_box;
	Fvector							m_collision_inertia_pos = zero_vel;
	Fvector							m_collision_inertia_pos_vel = zero_vel;
	Fvector							m_collision_inertia_rot = zero_vel;
	Fvector							m_collision_inertia_rot_vel = zero_vel;
	player_hud*						m_parent;
	CHudItem*						m_parent_hud_item;
	shared_str						m_sect_name;
	IKinematics*					m_model;
	u16								m_attach_place_idx;
	int								m_aim_bone_id = BI_NONE;
	u16								LeftIkBone = BI_NONE;
	Fmatrix							LeftIkOffset = Fidentity;
	bool							LeftIkFixed = false;
	float							LeftHandAttachBlend = 0.f;
	float							LeftHandIkBlend = 0.25f;
	float							LeftHandFingerGrip = 1.f;
	shared_str						LeftHandAttachTarget;
	shared_str						AttachPose;
	shared_str						IkMark = HudAttachMarks::Ik;
	shared_str						FingersMark = HudAttachMarks::Fingers;
	xr_vector<shared_str>			LeftHandAttachMotions;
	xr_vector<shared_str>			LeftHandAttachAnimatedArmMotions;
	Fmatrix							LeftHandAttachRef = Fidentity;
	bool							LeftHandAttachRefValid = false;
	bool							LeftHandAttachRefTried = false;
	xr_vector<Fquaternion>			FingerGripPose;
	xr_vector<Fvector>				FingerGripPos;
	shared_str						FingerGripPoseMotion;
	Fmatrix							LeftHandAttachAncorRef = Fidentity;
	Fmatrix							LeftHandAttachArmRootRef = Fidentity;
	xr_vector<Fquaternion>			LeftHandAttachPoseRef;
	xr_vector<Fvector>				LeftHandAttachPosRef;
	float							AimReloadMotion = 1.f;
	Fvector							AimReloadOffsetPos = {};
	Fvector							AimReloadOffsetRot = {};
	float							AimReloadZoomOut = 0.f;
	float							AimReloadPoseInertia = 0.15f;
	float							AimReloadPoseInertiaOut = 0.05f;
	Fvector							AimReloadPushPos = {};
	Fvector							CrouchOffsetPos = {};
	Fvector							CrouchOffsetRot = {};
	Fvector							WalkOffsetPos = {};
	Fvector							WalkOffsetRot = {};
	float							CrouchPoseInertia = 0.25f;
	float							CrouchPoseInertiaOut = -1.f;
	float							CrouchPoseDelay = 0.f;
	float							WalkPoseDelay = 5.f;
	float							WalkPoseInertia = 0.6f;
	float							WalkPoseInertiaOut = 0.12f;
	float							CarryPoseDamping = 0.8f;
	Fvector							LookSwayPos = {};
	float							AimSwayScale = 0.2f;
	float							LookSwayInertia = 0.04f;
	float							LookSwayDamping = 0.8f;
	Fvector							LeftSwayLook = {};
	Fvector							LeftSwayBob = {};
	Fvector							LeftSwayJump = {};
	Fvector							RightSwayLook = {};
	Fvector							RightSwayJump = {};
	xr_vector<u16>					LeftHandItemBones;
	xr_vector<u16>					RightHandItemBones;
	Fvector							LeftBobPos = { 1.f, 1.f, 1.f };
	Fvector							LeftBobRot = { 1.f, 1.f, 1.f };
	float							LeftFollowLag = 0.f;
	float							LeftFollowTurn = 1.f;
	Fvector							LeftFollowMove = { 1.f, 1.f, 1.f };
	float							LeftFollowMoveRot = 1.f;
	float							HandSwayInertia = 0.08f;
	float							HandSwayDamping = 0.6f;
	float							AimBlendInertia = 0.f;
	float							HudMass = 1.f;
	bool							IkEnabled = true;
	float							ShotVariation = 0.f;
	float							ShotTwist = 0.f;
	float							AimShotMotion = 1.f;
	float							IdleFade = 0.f;
	float							SprintBlendIn = 0.f;
	float							SprintLeftGripRange = 0.6f;
	float							SprintLeftCancelReturn = 0.35f;
	xr_vector<shared_str>			AdditiveMotions;
	float							AdditiveBlendIn = 0.f;
	float							AdditiveBlendOut = 0.1f;
	bool							AdditivePlaying = false;
	shared_str						AdditiveReference;
	float							SprintPoseInertia = 0.08f;
	xr_vector<shared_str>			RetargetMotions;
	shared_str						RetargetReference;
	xr_hash_map<shared_str, SHudRetargetLayer>	RetargetLayers;
	shared_str						AttachPoseReference;
	bool							RetargetPlaying = false;
	bool							RetargetLayerPlaying = false;
	MotionID						RetargetMotion;
	MotionID						RetargetReferenceMotion;
	xr_vector<shared_str>			LeftHandFreeMotions;
	bool							LeftHandFree = false;
	float							InertiaDamping = 0.55f;
	Fvector							InertiaVelPos = zero_vel;
	Fvector							InertiaVelRot = zero_vel;
	float							LandingRecoverTime = 1000.f;
	float							AirTime = 0.f;
	Fmatrix							CollisionXform = Fidentity;
	hud_item_measures				m_measures;
	shared_str						m_visual_name;
	bool							m_model_combined = false;

	//runtime positioning
	Fmatrix							m_attach_offset;
	Fmatrix							m_item_transform;

	player_hud_motion_container		m_hand_motions;
			
	u32 time_accumulator = 0;

	u32 tocrouch_time_remains = 0;
	u32 fromcrouch_time_remains = 0;
	u32 toslowcrouch_time_remains = 0;
	u32 fromslowcrouch_time_remains = 0;

	u32 torlookout_time_remains = 0;
	u32 fromrlookout_time_remains = 0;
	u32 tollookout_time_remains = 0;
	u32 fromllookout_time_remains = 0;

	void GetCurrentTargetOffset_aim(weapon_inertion& inertion_params, Fvector& pos, Fvector& rot, float& factor, u32& real);
	void GetCurrentTargetOffset(weapon_inertion& inertion_params, Fvector& pos, Fvector& rot, float& factor, u32& real);
	void AddOffsets(weapon_inertion::base_params& base, Fvector& pos, Fvector& rot, float koef = 1.0f);
	void AddSuicideOffset(weapon_inertion& inertion_params, const shared_str& section, Fvector& pos, Fvector& rot);

			attachable_hud_item(player_hud* pparent)
		: m_parent(pparent), m_upd_firedeps_frame(u32(-1)), m_parent_hud_item(NULL) {}
			~attachable_hud_item	();
	void load						(const shared_str& sect_name);
	void LoadRigParams				();
	float* RigField					(const char* key, u32& count);
	void update						(bool bForce);
	void update_hud_additional		(Fmatrix& trans);
	void setup_firedeps				(firedeps& fd);
	void render						();	
	void render_item_ui				();
	bool render_item_ui_query		();
	bool need_renderable			();
	u8 LeftHandAttachMotionMode		(const shared_str& motion) const;
	bool RetargetMode				(const shared_str& motion, shared_str& reference) const;
	bool RetargetLayer				(const shared_str& alias, MotionID& motion, shared_str& reference) const;
	void LoadRetargetLayers			();
	const player_hud_motion* OwnIdle	() const;
	MotionID AliasMotion				(const char* alias) const;
	bool SprintLayered				() const;
	shared_str SprintLayerReference	() const;
	float SprintLayerBlend			() const { return std::max(IdleFade, 0.1f); }
	void set_bone_visible			(const shared_str& bone_name, bool bVisibility, bool bSilent=false);
	void debug_draw_firedeps		();
	void UpdateInertion				(u32 delta, CActor* actor);

	//hands bind position
	Fvector&						hands_attach_pos();
	Fvector&						hands_attach_rot();

	//hands runtime offset
	Fvector&						hands_offset_pos();
	Fvector&						hands_offset_rot();

//props
	u32								m_upd_firedeps_frame;
	void		tune				(Fvector values);
	void		anim_play			(const shared_str& item_anm_name, EHudMixType bMixIn, float speed, player_hud_motion* anm = nullptr);
	void		anim_play_bonepart	(const shared_str& anim, bool bMixIn);
	void		PlayRetargetBase	(const shared_str& alias);
	void		UpdateAttachPose	();
	u32			anim_play			(const shared_str& anim_name, EHudMixType bMixIn, const CMotionDef*& md, u8& rnd, bool disable_random);

};

struct animator_item
{
	player_hud* m_parent = nullptr;
	IKinematics* m_item = nullptr;
	CHudAnimatorBase* m_animator = nullptr;

	Fmatrix m_attach_offset;
	Fmatrix m_item_transform;
	Fvector m_item_attach[2];
	u32	m_upd_firedeps_frame = u32(-1);
	bool IsPlaying = false;
	hud_item_measures::hud_hands_positions m_hands_positions;
	player_hud_motion_container	m_hand_motions;
	bool m_has_fire_point = false;
	u16 m_fire_bone = u16(-1);
	Fvector m_fire_point_offset;

	shared_str m_section;

	animator_item(CHudAnimatorBase* animator, player_hud* pParent, const shared_str& section);
	~animator_item();
	void update(bool bForce);
	void render();

	Fvector& hands_attach_pos();
	Fvector& hands_attach_rot();
	
	bool NeedMovementBlend();

	void update_hud_additional(Fmatrix& trans);

	void anim_play(const shared_str& item_anm_name, bool bMixIn, float speed);
	u32 anim_play(const shared_str& anim_name, bool bMixIn, const CMotionDef*& md);
	void			setup_firedeps		(firedeps& fd);
};

struct script_layer
{
	enum EBlendLayers : u8
	{
		eNone = 0,
		eAimStart,
		eAimEnd,
		eSafemodeIn,
		eSafemodeOut,
		eWatchesIn,
		eWatchesOut,
	};

	shared_str name;
	CObjectAnimator* anim = nullptr;
	float blend_scale = 0.0f;
	float power = 1.0f;
	float speed = 1.0f;
	Fvector2 blend_factors = { 0.4f, 0.4f }; //x+, y-
	bool active = false;
	bool mix = false;
	Fmatrix m_XFORM;
	u8 part = 0;
	u8 item = 0;
	u8 state = EBlendLayers::eNone;

	script_layer()
	{
		m_XFORM.identity();
		anim = new CObjectAnimator();
	}

	script_layer(const shared_str& name, float speed, float power, Fvector2 blend_factors, bool looped, bool mix, u8 part, u8 item, u8 state)
	{
		m_XFORM.identity();
		anim = new CObjectAnimator();

		Play(name, speed, power, blend_factors, looped, mix, part, item, state);
	}

	void Play(const shared_str& name, float speed, float power, Fvector2 blend_factors, bool looped, bool mix, u8 part, u8 item, u8 state)
	{
		m_XFORM.identity();

		this->name = name;
		this->power = power;
		this->speed = speed;
		this->blend_factors = blend_factors;

		anim->Load(*name);

		this->part = part;
		this->mix = mix;
		this->state = state;
		this->item = item;

		blend_scale = 0.0f;
		active = true;

		if (state != EBlendLayers::eNone)
		{
			CallStartCallback();
		}

		m_XFORM.identity();
		anim->Play(looped);
		anim->Speed() = speed;
	}

	void CallStartCallback();
	void CallEndCallback();

	~script_layer()
	{
		m_XFORM.identity();
		xr_delete(anim);
	}

	bool IsPlaying()
	{
		return anim->IsPlaying();
	}

	void Stop(bool bForce)
	{
		if (bForce)
		{
			anim->Stop();
			blend_scale = 0.0f;
			m_XFORM.identity();
		}
		else
		{
			mix = true;
		}

		active = false;
		state = EBlendLayers::eNone;
	}

	void Loop()
	{
		anim->Stop();
		anim->Play(true);
	}

	const Fmatrix& XFORM()
	{
		m_XFORM.set(anim->XFORM());

		Fvector rotation = zero_vel, position = zero_vel;
		m_XFORM.getXYZ(rotation);
		position = m_XFORM.c;

		const float weight = power * blend_scale * blend_scale * (3.0f - 2.0f * blend_scale);
		position.mul(weight);
		rotation.mul(weight);

		m_XFORM.setXYZ(rotation);
		m_XFORM.translate_over(position);

		return m_XFORM;
	}
};

struct SBlendParams
{
	shared_str camera_name;
	Fvector2 speed_power = { 1.0f, 1.0f };
	Fvector2 blend_params = { 0.4f, 0.4f };

	bool has_motion = false;

	void Load(const shared_str& section, const shared_str& anim_name, bool add_folder_name = true)
	{
		if (pSettings->line_exist(section, anim_name) && pSettings->r_string(section, *anim_name))
		{
			shared_str line = pSettings->r_string(section, *anim_name);
			string512 tmp = {};
			u8 params_count = _GetItemCount(*line);

			//name
			_GetItem(*line, 0, tmp);
			if (add_folder_name)
			{
				camera_name.printf("blend\\%s.anm", tmp);
			}
			else
			{
				camera_name._set(tmp);
			}

			//speed
			if (params_count >= 2)
			{
				_GetItem(*line, 1, tmp);
				speed_power.x = atof(tmp) ? atof(tmp) : 1.0f;
			}

			//power
			if (params_count >= 3)
			{
				_GetItem(*line, 2, tmp);
				speed_power.y = atof(tmp) ? atof(tmp) : 1.0f;
			}

			//blend factor +
			if (params_count >= 4)
			{
				_GetItem(*line, 3, tmp);
				blend_params.x = atof(tmp) ? atof(tmp) : 0.4f;
			}

			///blend factor -
			if (params_count >= 5)
			{
				_GetItem(*line, 4, tmp);
				blend_params.y = atof(tmp) ? atof(tmp) : 0.4f;
			}

			has_motion = true;
		}
	}
};

struct movement_layer
{
	Fmatrix m_XFORM;

	shared_str name;

	float speed = 1.0f;
	float power = 1.0f;
	float blend_scale = 0.0f;
	Fvector2 blend_factors = { 0.4f, 0.4f }; //x+, y-

	bool playing = false;


	CObjectAnimator* anim = nullptr;

	movement_layer()
	{
		m_XFORM.identity();
		anim = new CObjectAnimator();
	}

	~movement_layer()
	{
		m_XFORM.identity();
		xr_delete(anim);
	}

	const Fmatrix& XFORM()
	{
		m_XFORM.set(anim->XFORM());

		Fvector rotation = zero_vel, position = zero_vel;
		m_XFORM.getXYZ(rotation);
		position = m_XFORM.c;

		const float weight = power * blend_scale * blend_scale * (3.0f - 2.0f * blend_scale);
		position.mul(weight);
		rotation.mul(weight);

		m_XFORM.setXYZ(rotation);
		m_XFORM.translate_over(position);

		return m_XFORM;
	}

	void Load(const shared_str& anim_name)
	{
		name = anim_name;
		anim->Load(anim_name.c_str());
	}

	float Phase()
	{
		const SAnimParams& params = anim->anim_param();
		return anim->IsPlaying() && params.max_t > params.min_t ? (params.t_current - params.min_t) / (params.max_t - params.min_t) : -1.0f;
	}

	void Play(bool loop = true, float phase = -1.0f)
	{
		if (!anim->Name())
		{
			return;
		}

		if (anim->IsPlaying())
		{
			playing = true;
			return;
		}

		playing = true;
		anim->Speed() = speed;
		anim->Play(loop);

		const SAnimParams& params = anim->anim_param();
		if (phase > 0.0f && speed > 0.0f)
		{
			anim->Update(phase * (params.max_t - params.min_t) / speed);
		}
	}

	void Stop(bool bForce = false)
	{
		if (bForce)
		{
			anim->Stop();
			blend_scale = 0.0f;
			m_XFORM.identity();
		}

		playing = false;
	}

	void Reload(const shared_str& anim_name, Fvector2 speed_power, Fvector2 blend_factors)
	{
		if (anim_name.size() == 0)
		{
			return;
		}

		if (name != anim_name)
		{
			Stop(true);
			name = anim_name;
			anim->Clear();
			anim->Load(anim_name.c_str());
		}

		power = speed_power.y;
		speed = speed_power.x;
		this->blend_factors = blend_factors;
	}
};

struct BoneCallbackParams
{
	Fvector m_current = zero_vel;
	Fvector m_target = zero_vel;
};

enum EBoneCallbackParam
{
	r_finger0 = 0,
	r_finger01,
	r_finger02,
};

class player_hud
{
public: 
					player_hud			(bool invert = false);
					~player_hud			();
	void			load				(const shared_str& model_name);
	void			load_default		();
	void			ReloadRigParams		();
	void			update				(const Fmatrix& trans);
	void			render_hud			();	
	void			render_item_ui		();
	bool			render_item_ui_query();

	u32				anim_play			(u16 part, const MotionID& M, bool bMixIn, const CMotionDef*& md, float speed, IKinematicsAnimated* model);
	bool			check_anim			(const shared_str& anim_name, u16 place_idx);

	bool			animator_play			(const shared_str& anim_name, u16 place_idx = u16(-1), u16 part_id = u16(-1), bool bMixIn = false, float speed = 1.0f, u8 anm_idx = u8(0), bool impact_on_item = false, bool similar_check = false, PlayCallback Callback = PlayCallback(0), LPVOID CallbackParam = nullptr, bool UpdateCallbackType = 0);

	// анимация-эффект которая не будет отключать обычные анимации но позволит создать подрагивания уклонения и прочие анимационные эффекты
	void			animator_fx_play		(const shared_str& anim_name, u16 place_idx = u16(-1), u16 part_id = u16(-1), u8 anm_idx = u8(0), float blendAccrue = 1.f, float blendFalloff = 1.f, float Speed = 1.f, float Power = 1.f);

	const shared_str& section_name		() const {return m_sect_name;}

	attachable_hud_item* create_hud_item(const shared_str& sect);
	void			RemoveHudItem		(const shared_str& sect);
	void			attach_item			(CHudItem* item);
	bool			allow_activation	(CHudItem* item);
	attachable_hud_item* attached_item	(u16 item_idx)	{return m_attached_items[item_idx];};
	void			detach_item_idx		(u16 idx);
	void			detach_item			(CHudItem* item);
	void			detach_all_items	(){m_attached_items[0]=NULL; m_attached_items[1]=NULL;};

	void			calc_transform		(u16 attach_slot_idx, const Fmatrix& offset, Fmatrix& result, bool forceLegacyTransform = false);
	void			tune				(Fvector values);
	u32				motion_length		(const MotionID& M, const CMotionDef*& md, float speed, IKinematicsAnimated* model);
	u32				motion_length		(const shared_str& anim_name, const shared_str& hud_name, const CMotionDef*& md);
	void			OnMovementChanged	(ACTOR_DEFS::EMoveCommand cmd)	;
	void			RestoreHandBlends(const char* ignored_part);

	void			ResetBlockedPartID(){m_blocked_part_idx=u16(-1); };
	void			SetBlockedPartID(u16 val){m_blocked_part_idx = val; }
	void			SetHandsVisible(bool val){m_bhands_visible=val;};
	bool			GetHandsVisible(){return m_bhands_visible;};

	Fmatrix&		GetTransform(){return m_transform;};

	void			UpdateWeaponOffset(u32 delta);
	void			OnShotAnimation		(const attachable_hud_item* item);
	void			OnHudMotionStarted	(const shared_str& motion);

	IKinematics*	m_legs_model;
	s32				m_show_legs = 1;
	bool			m_need_reload = true;
	float			CycleFade = 0.f;
	SHudCycleMode	CycleMode;
	bool			Sprinting = false;

	void			SyncSprintBlend		();

	enum class ESprintAdditiveStage : u8
	{
		None,
		Start,
		Loop,
		End
	};

	ESprintAdditiveStage	SprintAdditiveStage = ESprintAdditiveStage::None;
	attachable_hud_item*	SprintAdditiveItem = nullptr;
	MotionID				SprintStartMotion;
	MotionID				SprintLoopMotion;
	MotionID				SprintEndMotion;
	MotionID				SprintReferenceMotion;
	bool					SprintLayerSeam = false;
	bool					SprintLeftFree = false;

	struct SSprintLeftCancel
	{
		bool				Active = false;
		float				Weight = 0.f;
		float				Hold = 1.f;
		float				Time = 0.f;
		Fmatrix				Relative = Fidentity;
	};

	SSprintLeftCancel		SprintLeftCancel;
	Fmatrix					LeftWristLast = Fidentity;
	u32						SprintLayerFrame = u32(-1);
	Fmatrix					SprintLayerPrev[2] = { Fidentity, Fidentity };
	Fmatrix					SprintLayerResidual[2] = { Fidentity, Fidentity };
	float					SprintEndPower = 1.f;
	float					SprintPose = 0.f;
	float					SprintPoseShape = 0.f;
	float					SprintExitDeviation = -1.f;
	float					SprintExitPose = 0.f;
	float					SprintTarget = 0.f;
	float					SprintPoseVelocity = 0.f;

	bool			SprintAdditiveEnabled	() const;
	float			SprintPoseWeight		() const { return SprintPose; }
	float			AimReloadPose			() const;
	float			AimReloadPush			() const;
	float			AimReloadDrop			() const;
	void			UpdateCarryPose			(CActor* actor);
	Fmatrix			CarryPoseOffset			() const;
	void			TriggerScriptLayersInertia(float half_life);
	void			UpdateSprintPose		();
	float			SprintPoseTarget		();
	float			SprintLayerDeviation	() const;
	void			ResolveSprintAdditive	(attachable_hud_item* item);
	bool			PlaySprintAdditive		(attachable_hud_item* item, MotionID motion, bool loop, float progress, float power = 1.f, bool inertialize = false);
	void			StopSprintAdditive		(float blend);
	float			SprintAdditiveProgress	(MotionID motion) const;
	void			FreezeSprintAdditive	(MotionID motion);
	void			UpdateSprintAdditive	(bool sprint);
	void			UpdateSprintAdditiveChain();
	void			UpdateRetargetLayerWeight();
	void			CutSprintEnd			();

	float			SprintYield = 0.f;

	shared_str		NextHUDSect;
	SBlendParams m_default_movement_layers_params[EMovementLayers::eLayersCount] = {};
	movement_layer* m_movement_layers[EMovementLayers::eLayersCount] = {};
	xr_vector<script_layer*> m_script_layers = {};

	IKinematicsAnimated* GetModel() { return m_model; }
	bool			PreviewGripBlocked = false;
	animator_item* create_animator_item(CHudAnimatorBase* animator, const shared_str& section);
	void			delete_animator_item();
	animator_item* GetAnimator() { return m_animator_item; }
	void UpdateMovementLayers(bool reload_anims = false);

	script_layer* PlayBlendAnm(const shared_str& name, float speed, float power, Fvector2 blend_factors, bool looped, bool mix, bool restart, u8 part, u8 item, u8 state);
	void StopBlendAnm(const shared_str& name, bool bForce = false);
	void StopAllBlendAnms(bool bForce);
	bool IsBlendAnmActive(const shared_str& name);

private:
	bool			need_render_hands	();
	void			update_inertion		(Fmatrix& trans);
	void			update_hands_ik		(const Fmatrix& cam_trans, CActor* actor);
	bool			HandsIkBegin		(SHandsIkFrame& F, CActor* actor);
	void			HandsIkSprint		(SHandsIkFrame& F);
	void			HandsIkSprintLeft	(SHandsIkFrame& F, MotionID SprintMotion, float SprintLayer);
	void			BeginSprintLeftCancel();
	void			SprintLayerSmooth	(SHandsIkFrame& F);
	void			HandsIkWeaponPose	(SHandsIkFrame& F, CActor* actor);
	void			HandsIkFingerState	(SHandsIkFrame& F);
	void			HandsIkWeaponDelta	(SHandsIkFrame& F);
	void			HandsIkHandTargets	(SHandsIkFrame& F);
	void			HandsIkAttachTarget	(SHandsIkFrame& F);
	bool			HandsIkSwayNeeded	(const SHandsIkFrame& F) const;
	void			HandsIkLeftSway		(SHandsIkFrame& F, const Fmatrix& CamTrans, CActor* actor);
	void			HandsIkRightSway	(SHandsIkFrame& F, CActor* actor);
	void			HandsIkItemBones	(SHandsIkFrame& F, const Fmatrix* WristBefore, const Fmatrix& AncorBefore);
	void			HandsIkLeftFollow	(SHandsIkFrame& F, const Fmatrix& CamTrans, float Free, CActor* actor);
	Fmatrix			LeftFollowTarget	(const SHandsIkFrame& F, const Fmatrix& CamTrans, const Fmatrix& Seen, CActor* actor);
	void			UpdateHandJump		(CActor* actor, float HalfLife);
	void			HandsIkReach		(SHandsIkFrame& F);
	void			HandsIkSolve		(SHandsIkFrame& F);
	void			HandsIkFingers		(SHandsIkFrame& F);
	void			HandsIkFingerInertia	(SHandsIkFrame& F);
	bool			LeftHandAttachReference	(attachable_hud_item* item);
	bool			HandAttachRelative	(attachable_hud_item* item, MotionID hands_motion, float hands_time, MotionID item_motion, float item_time, Fmatrix& relative, Fmatrix* ancor_ms = nullptr);
	bool			RetargetHandTarget	(u16 ancor, u16 wrist, MotionID motion, u8 channel, MotionID reference, Fmatrix& result, float strength = 1.f, float* deviation = nullptr);
	xr_vector<u16>	left_arm_blend_bones() const;
	xr_vector<u16>	left_hand_fingers() const;
	bool			FingerGripReference	(attachable_hud_item* item);
	bool			ArmRetargetReference(attachable_hud_item* item, const xr_vector<u16>& bones);
	float								RetargetWeight = 0.f;
	Fmatrix								RetargetLeftTarget = Fidentity;
	Fmatrix								RetargetRightTarget = Fidentity;
	bool								RetargetLeftValid = false;
	bool								RetargetRightValid = false;
	float								SprintHandWeight = 0.f;
	bool			SprintBaseTransform	(u16 bone, Fmatrix& result) const;
	bool			SprintBoneDelta		(u16 bone, Fmatrix& result) const;
	float								LeftHandAttachWeight = 0.f;
	bool								LeftHandAttachReaching = false;
	u32									LeftHandAttachSnapFrame = u32(-1);
	bool								LeftGripActive = false;
	xr_vector<u16>						FingerGripBones;
	xr_vector<Fquaternion>				FingerStartPose;
	xr_vector<Fvector>					FingerStartPos;
	MotionID							FingerStartMotion;
	float								FingerStartTime = 0.f;
	xr_vector<Fquaternion>				FingerPrevRot;
	xr_vector<Fvector>					FingerPrevPos;
	xr_vector<Fquaternion>				FingerResidualRot;
	xr_vector<Fvector>					FingerResidualPos;
	u32									FingerResidualFrame = u32(-1);
	MotionID							ArmRetargetMotion;
	xr_vector<Fquaternion>				ArmRetargetPose;
	Fmatrix								ArmRetargetRootRef = Fidentity;
	MotionID							AttachOffsetMotion;
	bool								AttachOffsetSampled = false;
	Fmatrix								AttachOffsetRef = Fidentity;
	float								LeftHandAttachHold = 0.f;
	float								LeftHandAttachArmWeight = 1.f;
	bool								LeftHandAttachAnimatedArm = false;
	xr_vector<u16>	left_arm_subtree	() const;
	const Fvector&	attach_rot			() const;
	const Fvector&	attach_pos			() const;

	shared_str							m_sect_name;

	Fmatrix								m_attach_offsetr, m_attach_offsetl;

	Fmatrix								m_transform;
	Fmatrix								m_transformL;

	IKinematicsAnimated*				m_model = nullptr;
	xr_vector<u16>						m_ancors;
	u16									LeftIkChain[3] = { BI_NONE, BI_NONE, BI_NONE };
	u16									RightIkChain[3] = { BI_NONE, BI_NONE, BI_NONE };
	float								LeftIkWeight = 0.f;
	Fmatrix								LayersIkOffset = Fidentity;
	Fmatrix								MovementLayersOffset = Fidentity;
	SHudVectorSpring					LeftSway;
	SHudVectorSpring					RightSway;
	SHudHandJump						HandJump;
	SHudLeftFollow						LeftFollow;
	Fvector								TurnOffset = zero_vel;
	Fmatrix								AimReloadRef = Fidentity;
	float								AimReloadWeight = 0.f;
	float								AimReloadRelease = 1.f;
	float								AimReloadPoseValue = 0.f;
	float								AimReloadPoseVelocity = 0.f;
	float								AimReloadPushValue = 0.f;
	float								AimReloadPushVelocity = 0.f;
	float								AimReloadDropValue = 0.f;
	float								AimReloadDropVelocity = 0.f;
	float								CrouchPoseValue = 0.f;
	float								CrouchPoseVelocity = 0.f;
	float								WalkPoseValue = 0.f;
	float								WalkPoseVelocity = 0.f;
	float								WalkTime = 0.f;
	float								CrouchTime = 0.f;
	Fmatrix								ScriptLayersLast = Fidentity;
	bool								ScriptInertiaPending = false;
	float								ScriptInertiaHalfLife = 0.f;
	float								ScriptInertiaTime = 0.f;
	Fvector								ScriptInertiaPos = {};
	Fquaternion							ScriptInertiaRot;
	Fmatrix								ShotRef = Fidentity;
	float								ShotWeight = 0.f;
	float								ShotRelease = 1.f;
	Fvector								ShotRotScale = { 1.f, 1.f, 1.f };
	Fvector								ShotPosScale = { 1.f, 1.f, 1.f };
	Fvector2							ShotTwist = { 0.f, 0.f };
	bool								LayersIk = false;
	u32									MovementKey = 0;
	attachable_hud_item*				m_attached_items[2];
	animator_item*						m_animator_item = nullptr;
	xr_vector<attachable_hud_item*>		m_pool;

	u16									m_blocked_part_idx;
	bool								m_bhands_visible;
	bool								m_binverted;
	int									item_idx_priority;
	void  LeftArmCallback(CBoneInstance* B);
	static void FingerCallback(CBoneInstance* B);
public:
	xr_map<EBoneCallbackParam, BoneCallbackParams*> m_bone_callback_params; // bonename,params

	IKinematics* m_watches_model = nullptr;
	Fmatrix m_watches_transform = Fidentity;
	Fmatrix m_watches_attach_offset = Fidentity;
	Fvector m_watches_pos = zero_vel;
	Fvector m_watches_rot = zero_vel;
	float m_watches_scale = 1.0f;
	u16 m_watches_bone = BI_NONE;
	CWatchDevice* m_watch_device = nullptr;

	CWatchDevice& WatchDevice() { return *m_watch_device; }
	void destroy_watches();

	void reset_thumb(bool bForce)
	{
		if (bForce)
		{
			m_bone_callback_params[r_finger0]->m_current.set(0.f, 0.f, 0.f);
			m_bone_callback_params[r_finger01]->m_current.set(0.f, 0.f, 0.f);
			m_bone_callback_params[r_finger02]->m_current.set(0.f, 0.f, 0.f);
		}

		m_bone_callback_params[r_finger0]->m_target.set(0.f, 0.f, 0.f);
		m_bone_callback_params[r_finger01]->m_target.set(0.f, 0.f, 0.f);
		m_bone_callback_params[r_finger02]->m_target.set(0.f, 0.f, 0.f);
	}
};

extern player_hud* g_player_hud;