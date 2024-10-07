#pragma once

class CSE_Abstract;
class CPhysicItem;
class NET_Packet;
class CInventoryItem;
class CMotionDef;
class HudLightTorch;

#include "actor_defs.h"
#include "inventory_space.h"
#include "hudsound.h"
#include "InertionData.h"

#include "HudLightTorch.h"

struct attachable_hud_item;
class motion_marks;

class CHUDState
{
public:
enum EHudStates {
		eIdle		= 0,
		eShowing,
		eHiding,
		eHidden,
		eBore,
		eSprintStart,
		eSprintEnd,
		eSwitchDevice,
		eSuicide,
		eSuicideStop,
		eLastBaseState = eSuicideStop,
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
protected:
							CHudItem			();
	virtual					~CHudItem			();
	virtual DLL_Pure*		_construct			();
	
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
	HudLightTorch m_HudLight;
public:
	virtual void				Load				(LPCSTR section);
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
	
	virtual	u8					GetCurrentHudOffsetIdx () const {return 0;}

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
	virtual void				OnMotionMark		(u32 state, const motion_marks&){};

	virtual void				PlayAnimIdle		();
	virtual void				PlayAnimBore		();
	bool						TryPlayAnimIdle		();
	virtual bool				MovingAnimAllowedNow ()				{return true;}

	virtual void				PlayAnimIdleMoving	();
	virtual void				PlayAnimIdleMovingSlow();
	virtual void				PlayAnimIdleMovingCrouchSlow();
	virtual void				PlayAnimIdleMovingCrouch();
	virtual void				PlayAnimIdleSprint	();
	virtual void				PlayAnimDevice		();

	virtual void				UpdateCL			();
	virtual void				renderable_Render	();
	virtual void				SetModelBoneStatus(const char* bone, BOOL show) const;
	virtual void				SetMultipleBonesStatus(const char* section, const char* line, BOOL show) const;

	virtual void				UpdateHudAdditonal	(Fmatrix&);


	virtual	void				UpdateXForm			()						= 0;

	virtual u32					PlayHUDMotion		(xr_string M, BOOL bMixIn, CHudItem*  W, u32 state, bool need_suffix = true);
	u32							PlayHUDMotion_noCB	(const shared_str& M, BOOL bMixIn);
	void						StopCurrentAnimWithoutCallback();
	virtual xr_string			NeedAddSuffix(const xr_string& M) { return M; }

	IC void						RenderHud				(BOOL B)	{ m_huditem_flags.set(fl_renderhud, B);}
	IC BOOL						RenderHud				()			{ return m_huditem_flags.test(fl_renderhud);}
	attachable_hud_item*		HudItemData				() const;
	virtual void				on_a_hud_attach			();
			bool				HudAnimationExist		(LPCSTR anim_name);
	virtual void				on_b_hud_detach			();
	virtual void				render_hud_mode			()					{};
	virtual bool				need_renderable			()					{return true;};
	virtual void				render_item_3d_ui		()					{}
	virtual bool				render_item_3d_ui_query	()					{return false;}

	virtual bool				CheckCompatibility		(CHudItem*)			{return true;}

	struct jitter_params
	{
		float pos_amplitude = 0.0f;
		float rot_amplitude = 0.0f;
	};

	virtual float GetHudFov();
	virtual const bool AllowBore() const { return !m_bDisableBore; }
	virtual xr_string GetActualCurrentAnim() const;
	bool CanStartAction(bool allow_aim_state = false) const;
	bool Weapon_SetKeyRepeatFlagIfNeeded(u32 kfACTTYPE) const;
	bool IsSuicideAnimPlaying() const;
	bool WpnCanShoot() const;
	bool StartCompanionAnimIfNeeded(const xr_string anim_name);
	void AssignDetectorAnim(const xr_string anm_alias, bool bMixIn = true, bool use_companion_section = false);
	const jitter_params& GetCurJitterParams() const;
	HudLightTorch GetLight() { return m_HudLight; }
	using TAnimationEffector = fastdelegate::FastDelegate<void()>;

	enum EDeviceFlags
	{
		DF_HEADLAMP = (1 << 0),
		DF_NIGHTVISION = (1 << 1),
		DF_TACTICALTORCH = (1 << 2),
		DF_TACTICALLASER = (1 << 3)
	};

	Flags32 fDeviceFlags;

	float getLookOutSpeedKoef(void) const;
	float getLookOutAmplK(void) const;
	float getControllerTime(void) const;
	float getControllerShootExplMinDist(void) const;
	bool isSuicideByAnimation(void) const;
protected:

	IC		void				SetPending			(BOOL H)			{ m_huditem_flags.set(fl_pending, H);}
	shared_str					hud_sect;
	shared_str					hud_sect_cache;

	//кадры момента пересчета XFORM и FirePos
	u32							dwFP_Frame;
	u32							dwXF_Frame;

	u32							m_animation_slot;

	HUD_SOUND_COLLECTION		m_sounds;
	InertionData				m_current_inertion;
	float						m_nearwall_dist_max;
	float						m_nearwall_dist_min;
	float						m_nearwall_last_hud_fov;
	float						m_nearwall_target_hud_fov;
	float						m_nearwall_speed_mod;
	float						m_fHudFov;
	float						m_fLookOutSpeedKoef;
	float						m_fLookOutAmplK;
	float						m_fControllerTime;
	float						m_fControllerShootExplMinDist;
	float m_fLR_CameraFactor; // Фактор бокового наклона худа при ходьбе [-1; +1]
	float m_fLR_MovingFactor; // Фактор бокового наклона худа при движении камеры [-1; +1]
	float m_fLR_InertiaFactor; // Фактор горизонтальной инерции худа при движении камеры [-1; +1]
	float m_fUD_InertiaFactor; // Фактор вертикальной инерции худа при движении камеры [-1; +1]

	bool						m_bDisableBore;
	bool						SwitchSprint;
	bool						m_bSuicideByAnimation;
	TAnimationEffector			lock_time_callback;
	u32							mark;
	int							_action_ppe;
	jitter_params  m_jitter_params;
private:
	CPhysicItem					*m_object;
	CInventoryItem				*m_item;

public:
	const shared_str&			HudSection				() const		{ return hud_sect;}
	IC CPhysicItem&				object					() const		{ VERIFY(m_object); return(*m_object);}
	IC CInventoryItem&			item					() const		{ VERIFY(m_item); return(*m_item);}
	IC		u32					animation_slot			()				{ return m_animation_slot;}
	InertionData&				CurrentInertionData		()				{ return m_current_inertion;}
	void						SetAnimationCallback(TAnimationEffector callback) { lock_time_callback = callback; }

	virtual void				on_renderable_Render	() = 0;
	virtual void				debug_draw_firedeps		() {};

	virtual CHudItem*			cast_hud_item			()				{ return this; }
};

