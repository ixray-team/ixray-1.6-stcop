#pragma once

class CSE_Abstract;
class CPhysicItem;
class NET_Packet;
class CInventoryItem;
class CMotionDef;
class CCustomDetector;
class CWeaponMagazined;
class CWeaponMagazinedWGrenade;
class CWeaponBinoculars;
class CWeaponKnife;
class CWeaponBM16;
class CWeaponRPG7;
class CWeaponRG6;
class CWeapon;
class CMissile;
class CBolt;
class CGrenade;
class CPhysicsShellHolder;

#include "actor_defs.h"
#include "inventory_space.h"
#include "HudSound.h"
#include "InertionData.h"
#include "../xrScripts/script_export_space.h"

#include "HudTorchLight.h"

struct attachable_hud_item;
class motion_marks;

//class HudLightTorch;

class CHUDState
{
public:
enum EHudStates
{
		eIdle = 0,
		eShowing,
		eHiding,
		eHidden,
		eBore,
		eSprintStart,
		eSprintEnd,
		eDeviceSwitch,
		ePrepareDetector,
		ePrepareDetectorEnd,
		eFinishDetector,
		eLastBaseState = eFinishDetector,
};

private:
	u32						m_hud_item_state;
	u32						m_nextState;
	u32						m_dw_curr_state_time;
protected:
	u32						m_dw_curr_substate_time;
public:
							CHUDState			()					{SetState(eHidden);}
	IC		u32				GetNextState		() const			{return		m_nextState;}
	IC		u32				GetState			() const			{return		m_hud_item_state;}

	IC		void			SetState			(u32 v)				{m_hud_item_state = v; m_dw_curr_state_time=Device.dwTimeGlobal;ResetSubStateTime();}
	IC		void			SetNextState		(u32 v)				{m_nextState = v;}
	IC		u32				CurrStateTime		() const			{return Device.dwTimeGlobal-m_dw_curr_state_time;}
	IC		void			ResetSubStateTime	()					{m_dw_curr_substate_time=Device.dwTimeGlobal;}
	virtual void			SwitchState			(u32 S)				= 0;
	virtual void			OnStateSwitch		(u32 S)				= 0;
};

class CHudItem :public CHUDState
{
public:
							CHudItem			();
	virtual					~CHudItem			();
	virtual DLL_Pure*		_construct			();
protected:
	
	Flags16					m_huditem_flags;
	enum{
		fl_pending			= (1<<0),
		fl_renderhud		= (1<<1),
	};

	struct{
		const CMotionDef*		m_current_motion_def;
		shared_str				m_current_motion;
		u32						m_dwMotionCurrTm;
		u32						m_dwMotionStartTm;
		u32						m_dwMotionEndTm;
		u32						m_startedMotionState;
		u8						m_started_rnd_anim_idx;
		bool					m_bStopAtEndAnimIsRunning;
	};
public:
	virtual void				Load				(LPCSTR section);
	virtual void				LoadSounds			(LPCSTR section);
	virtual	BOOL				net_Spawn			(CSE_Abstract* DC)				{return TRUE;};
	virtual void				net_Destroy			()								{};
	virtual void				OnEvent				(NET_Packet& P, u16 type);

	virtual void				OnH_A_Chield		();
	virtual void				OnH_B_Chield		();
	virtual void				OnH_B_Independent	(bool just_before_destroy);
	virtual void				OnH_A_Independent	();
	
	virtual void				PlaySound			(LPCSTR alias, const Fvector& position, bool allowOverlap = false);

	virtual bool				Action				(u16 cmd, u32 flags)			{return false;}
			void				OnMovementChanged	(ACTOR_DEFS::EMoveCommand cmd)	;
	
	virtual	u8					GetCurrentHudOffsetIdx ()							{return 0;}

	BOOL						GetHUDmode			();
	IC BOOL						IsPending			()		const					{ return !!m_huditem_flags.test(fl_pending);}

	virtual bool				ActivateItem		();
	virtual void				DeactivateItem		();
	virtual bool				SendDeactivateItem	();
	virtual void				OnActiveItem		()				{};
	virtual void				OnHiddenItem		()				{};
	virtual void				SendHiddenItem		();			//same as OnHiddenItem but for client... (sends message to a server)...
	virtual void				OnMoveToRuck		(const SInvItemPlace& prev);

	bool						IsHidden			()	const		{	return GetState() == eHidden;}						// Does weapon is in hidden state
	bool						IsHiding			()	const		{	return GetState() == eHiding;}
	bool						IsShowing			()	const		{	return GetState() == eShowing;}

	virtual void				SwitchState			(u32 S);
	virtual void				OnStateSwitch		(u32 S);

	virtual void				OnAnimationEnd		(u32 state);
	virtual void				OnMotionMark		(u32 state, const motion_marks&);

	virtual void				PlayAnimIdle		();
	virtual void				PlayAnimBore		();
	virtual void				PlayAnimDeviceSwitch();
	bool						TryPlayAnimIdle		();
	virtual bool				MovingAnimAllowedNow ()				{return true;}

	virtual void				PlayAnimIdleMoving();
	virtual void				PlayAnimIdleMovingSlow();
	virtual void				PlayAnimIdleMovingCrouch();
	virtual void				PlayAnimIdleMovingCrouchSlow();
	virtual void				PlayAnimIdleSprint();

	virtual void				UpdateCL			();
	virtual void				renderable_Render	();


	virtual void				UpdateHudAdditonal	(Fmatrix&);


	virtual	void				UpdateXForm			()						= 0;

	u32							PlayHUDMotion		(const shared_str& M, BOOL bMixIn, u32 state);
	u32							PlayHUDMotion_noCB	(const shared_str& M, BOOL bMixIn);
	void						StopCurrentAnimWithoutCallback();
	bool						AddSuffixName		(shared_str& anim, LPCSTR suffix, LPCSTR test_suffix = "");
	shared_str					SetCurrentIdleAnimation();
	virtual shared_str			SetCurrentStateAnimation(const shared_str& first_name) { return first_name; }

	IC void						RenderHud				(BOOL B)	{ m_huditem_flags.set(fl_renderhud, B);}
	IC BOOL						RenderHud				()			{ return m_huditem_flags.test(fl_renderhud);}
	attachable_hud_item*		HudItemData				();
	virtual void				on_a_hud_attach			();
	virtual bool				HudAnimationExist		(const shared_str& anim_name);
	virtual void				on_b_hud_detach			();
	virtual void				render_hud_mode			()					{};
	virtual bool				need_renderable			()					{return true;};
	virtual void				render_item_3d_ui		()					{}
	virtual bool				render_item_3d_ui_query	()					{return false;}

	virtual bool				CheckCompatibility		(CHudItem*)			{return true;}

	virtual float GetHudFov();
	virtual bool AllowBore() { return !m_bDisableBore && m_eAnimationsFlags.test(EAnimationsFlags::af_bore); }

	enum EDevicesFlags
	{
		df_torch = (1 << 0),
		df_nvg = (1 << 1),
		df_clear_mask = (1 << 2),
		df_tacticaltorch = (1 << 3),
		df_laser = (1 << 4),
	};

	enum EAnimationsFlags
	{
		af_torch = (1 << 0),
		af_nvg = (1 << 1),
		af_clear_mask = (1 << 2),
		af_prepare_detector = (1 << 3),
		af_prepare_detector_end = (1 << 4),
		af_finish_detector = (1 << 5),
		af_det_hand_draw = (1 << 6),
		af_det_hand_hide = (1 << 7),
		af_det_hand_throw_start = (1 << 8),
		af_det_hand_throw_idle = (1 << 9),
		af_det_hand_throw_end = (1 << 10),
		af_det_hand_kick = (1 << 11),
		af_det_hand_lam = (1 << 12),
		af_bore = (1 << 13),
		af_firemode = (1 << 14),
	};

	enum ESoundsFlags
	{
		sf_headlamp = (1 << 0),
		sf_nv = (1 << 1),
		sf_prepare_detector = (1 << 2),
		sf_finish_detector = (1 << 3),
		sf_changefiremode = (1 << 4),
		sf_aim_start = (1 << 5),
		sf_aim_end = (1 << 6),
		sf_reload_empty = (1 << 7),
		sf_reload_jam = (1 << 8),
		sf_reload_empty_det = (1 << 9),
		sf_reload_jam_det = (1 << 10),
		sf_reload_jam_last = (1 << 11),
		sf_reload_jam_last_det = (1 << 12),
		sf_shoot_actor = (1 << 13),
		sf_shoot_actor_last = (1 << 14),
		sf_shoot_last = (1 << 15),
		sf_shoot_actor_sil = (1 << 16),
		sf_shoot_last_sil = (1 << 17),
		sf_shoot_actor_last_sil = (1 << 18),
		sf_draw = (1 << 19),
		sf_holster = (1 << 20),
		sf_throw_begin = (1 << 21),
		sf_throw = (1 << 22),
		sf_kick = (1 << 23),
		sf_grenade_change = (1 << 24),
		sf_shoot_grenade_actor = (1 << 25),
		sf_switch_g = (1 << 26),
		sf_reload_change = (1 << 27),
		sf_tacticaltorch = (1 << 28),
		sf_laser = (1 << 29),
	};

	Flags32 m_eDevicesFlags;
	Flags32 m_eAnimationsFlags;
	Flags32 m_eSoundsFlags;

	bool bDisablePrepareAnimation = false;

	virtual bool WpnCanShoot() const { return false; }
	bool SoundExist(LPCSTR section, LPCSTR sound_name);

	struct jitter_params
	{
		float pos_amplitude = 0.0f;
		float rot_amplitude = 0.0f;
		float stop_time = 0.0f;
	} m_jitter_params;

	jitter_params& GetCurJitterParams() { return m_jitter_params; }

protected:

	IC		void				SetPending			(BOOL H)			{ m_huditem_flags.set(fl_pending, H);}
	shared_str					hud_sect;
	shared_str					hud_sect_cache;

	//кадры момента пересчета XFORM и FirePos
	u32							dwFP_Frame;
	u32							dwXF_Frame;

	u32							m_animation_slot;

	HUD_SOUND_COLLECTION		m_sounds;
	HUD_SOUND_COLLECTION_LAYERED m_layered_sounds;
	InertionData				m_current_inertion;
	HudLightTorch				m_HudLight;
	float						m_nearwall_dist_max;
	float						m_nearwall_dist_min;
	float						m_nearwall_last_hud_fov;
	float						m_nearwall_target_hud_fov;
	float						m_nearwall_speed_mod;
	float						m_fHudFov;

	bool						m_bDisableBore;
	bool						m_bSwitchSprint = false;

	virtual void				SetModelBoneStatus(const char* bone, BOOL show);
	virtual void				SetMultipleBonesStatus(const char* section, const char* line, BOOL show);

private:
	CPhysicItem					*m_object;
	CInventoryItem				*m_item;

public:
	const shared_str&			HudSection				() const		{ return hud_sect;}
	IC CPhysicItem&				object					() const		{ VERIFY(m_object); return(*m_object);}
	IC CInventoryItem&			item					() const		{ VERIFY(m_item); return(*m_item);}
	IC		u32					animation_slot			()				{ return m_animation_slot;}
	InertionData&				CurrentInertionData		()				{ return m_current_inertion;}

	virtual void				on_renderable_Render	() = 0;
	virtual void				debug_draw_firedeps		() {};

	virtual CHudItem*			cast_hud_item			()				{ return this; }
	virtual CCustomDetector*	cast_custom_detector	()				{ return nullptr; }
	virtual CWeaponBinoculars* cast_weapon_binoculars() { return nullptr; }
	virtual CWeaponKnife* cast_weapon_knife() { return nullptr; }
	virtual CWeaponMagazined* cast_weapon_magazined() { return nullptr; }
	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() { return nullptr; }
	virtual CWeaponBM16* cast_weapon_bm16() { return nullptr; }
	virtual CWeapon* cast_weapon() { return nullptr; }
	virtual CWeaponRPG7* cast_weapon_rpg7() { return nullptr; }
	virtual CWeaponRG6* cast_weapon_rg6() { return nullptr; }
	virtual CGrenade* cast_grenade() { return nullptr; }
	virtual CMissile* cast_missile() { return nullptr; }
	virtual CBolt* cast_bolt() { return nullptr; }
	virtual CInventoryItem* cast_inventory_item() { return nullptr; }
	virtual CPhysicsShellHolder* cast_physics_shell_holder() { return nullptr; }
	virtual CPhysicItem* cast_physics_item() { return nullptr; }

protected:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

