#pragma once
#include "../xrEngine/CameraDefs.h"

class CActor;
class CWeapon;
class CObjectAnimator;
class IKinematics;

enum ECameraRigChannel
{
	crcMotion,
	crcAction,
	crcShot,
	crcMove,
	crcCount
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
	};

	struct SSpring
	{
		Fvector				value		= {};
		Fvector				velocity	= {};
	};

	SLayer					m_layers[12];
	SSpring					m_punch, m_kick, m_kick_pos, m_land, m_roll, m_fov;
	Fvector					m_world_pos = {}, m_world_hpb = {}, m_hud_cam_pos = {}, m_hud_cam_hpb = {}, m_hud_pos = {}, m_hud_hpb = {};
	shared_str				m_weapon_sect;
	Fvector					m_weapon_punch = {}, m_weapon_kick_pos = {}, m_weapon_kick_rot = {};
	float					m_weapon_trauma = 0.f;
	float					m_trauma = 0.f;
	float					m_lean = 0.f;
	u32						m_update_frame = u32(-1);
	bool					m_fov_valid = false;

	void					FadeChannel		(ECameraRigChannel channel);
	void					UpdateLayers	(float dt, Fvector& pos, Fvector& hpb, Fvector& hud_pos, Fvector& hud_hpb);

public:
							~CCameraRig		();

	static bool				Enabled			();
	static bool				SpringSway		(Fvector& value, Fvector& velocity, const Fvector& target, float speed);
	static void				TransformBone	(IKinematics* model, u16 bone, const Fmatrix& delta);
	static void				SolveLimb		(IKinematics* model, const u16* chain, const Fmatrix& target, float weight);

	bool					PlayAnim		(const char* name, ECameraRigChannel channel, float speed, bool hud_affect);
	void					StopChannel		(ECameraRigChannel channel);
	void					OnShot			(CWeapon* weapon);
	void					OnHit			(float power);
	void					OnExplosion		(float power);
	void					OnLand			(float speed);
	void					Update			(CActor* actor, float fov, float lean);
	void					Apply			(SCamEffectorInfo& world, SCamEffectorInfo& hud);
	void					ApplyHud		(Fmatrix& trans);
	float					LeanCounter		() const;
};
