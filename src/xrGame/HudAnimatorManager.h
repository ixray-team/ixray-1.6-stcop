#pragma once

#include "HudSound.h"
#include "Actor.h"

class HUD_SOUND_COLLECTION;
class CActor;
class CMotionDef;
class CHudPdaAnimator;
class CHudItemAnimator;
class CBackpackAnimator;
class CHudStateAnimator;
class CHudAnimatorManager;
class CBurnAnimator;

class CHudAnimatorBase
{
protected:
	u32	m_dwMotionCurrTm = 0;
	u32	m_dwMotionStartTm = 0;
	u32	m_dwMotionEndTm = 0;
	bool m_bStopAtEndAnimIsRunning = true;
	const CMotionDef* m_current_motion_def = nullptr;

	bool m_bCanSprint = false;
	bool m_bNeedActivated = false;
	bool m_bIsPlaying = false;
	bool m_bHideUI = true;

	float m_fHudFov = 0.0f;
	float m_fHudFovFactor = 1.0f;

	shared_str m_section;
	HUD_SOUND_COLLECTION m_sounds;
	CHudAnimatorManager* m_manager = nullptr;

	xr_delegate<void()> m_left_callback;
	xr_delegate<void()> m_left2_callback;
	xr_delegate<void()> m_right_callback;
	xr_delegate<void()> m_right2_callback;
	xr_delegate<void()> m_start_callback;
	xr_delegate<void()> m_end_callback;

	shared_str m_sLuaLeftCallback = "null";
	shared_str m_sLuaLeft2Callback = "null";
	shared_str m_sLuaRightCallback = "null";
	shared_str m_sLuaRight2Callback = "null";
	shared_str m_sLuaStartCallback = "null";
	shared_str m_sLuaEndCallback = "null";
	shared_str m_sLuaModifySect = "null";
	shared_str m_sLuaPrecondFunc = "null";

	bool HudAnimationExist(const shared_str& name);

public:

	CHudAnimatorBase(CHudAnimatorManager* manager) : m_manager(manager) {}
	virtual ~CHudAnimatorBase();

	virtual void Load();
	void SetSection(const shared_str& new_section) { m_section = new_section; }
	shared_str GetSection() const { return m_section; }
	bool IsActive() const { return m_bIsPlaying || m_bNeedActivated; }
	virtual void StopAnimator();
	virtual float GetHudFov() const;
	virtual bool CanSprint() const { return m_bCanSprint; }

	void SetLeftCallback(xr_delegate<void()> callback) { m_left_callback = callback; }
	void SetLeft2Callback(xr_delegate<void()> callback) { m_left2_callback = callback; }
	void SetRightCallback(xr_delegate<void()> callback) { m_right_callback = callback; }
	void SetRight2Callback(xr_delegate<void()> callback) { m_right2_callback = callback; }
	void SetStartCallback(xr_delegate<void()> callback) { m_start_callback = callback; }
	void SetEndCallback(xr_delegate<void()> callback) { m_end_callback = callback; }

	void CallLeftCallback();
	void CallLeft2Callback();
	void CallRightCallback();
	void CallRight2Callback();
	void CallStartCallback();
	void CallEndCallback();

	virtual bool InputKeyPress(int cmd) { return false; }
	virtual bool InputKeyRelease(int cmd) { return false; }
	virtual bool InputKeyHold(int cmd) { return false; }

	virtual void UpdateHudAdditonal(Fmatrix&) {};

	virtual CHudItemAnimator* cast_item_animator() { return nullptr; }
	virtual CHudStateAnimator* cast_hud_state_animator() { return nullptr; }
	virtual CBackpackAnimator* cast_backpack_animator() { return nullptr; }
	virtual CBurnAnimator* cast_burn_animator() { return nullptr; }
	virtual CHudPdaAnimator* cast_pda_animator() { return nullptr; }
};

class CHudStateAnimator : public CHudAnimatorBase
{
protected:
	u32 m_current_state = eHidden;
	u32 m_on_animation_end_state = eHidden;

	u32 m_dw_curr_state_time = 0;
	u32	m_dw_curr_substate_time = 0;

	bool m_bDisableBore = true;
	bool m_bSwitchSprint = false;

	enum ESoundsFlags : u64
	{
		sf_headlamp = (1 << 0),
		sf_nv = (1 << 1),
		sf_sprint_start = (1 << 2),
		sf_sprint_end = (1 << 3),
	};

	Flags64 m_eSoundsFlags;

protected:
	u32 CurrStateTime() const { return Device.dwTimeGlobal - m_dw_curr_state_time; }
	void ResetSubStateTime() { m_dw_curr_substate_time = Device.dwTimeGlobal; }
	virtual void OnAnimationEnd(u32 state);
	virtual void OnStateSwitch(u32 state);
	void UpdateAnimation();
	void PlayMotion(const shared_str& name, bool blend, u32 state);
	virtual void OnMotionMark(const motion_marks& mark, u32 state);
	virtual void PlayAnimIdle();
	virtual void PlayAnimIdleMoving();
	virtual void PlayAnimIdleMovingSlow();
	virtual void PlayAnimIdleMovingCrouch();
	virtual void PlayAnimIdleMovingCrouchSlow();
	virtual void PlayAnimIdleSprint();
	virtual void PlayAnimDeviceSwitch();
	virtual bool TryPlayAnimIdle();

	virtual shared_str SetCurrentStateAnimation(const shared_str& anim_name) { return anim_name; }

public:
	enum EAnimatorStates
	{
		eHidden = 0,
		eIdle,
		eBore,
		eDeviceSwitch,
		eSprintStart,
		eSprintEnd,
		eShowing,
		eHiding,
		eLastAnimatorState = eHiding
	};

	enum EAnimationsFlags
	{
		af_torch = (1 << 0),
		af_nvg = (1 << 1),
		af_clear_mask = (1 << 2)
	};

	Flags32 m_eAnimationsFlags;

	enum EDevicesFlags
	{
		df_torch = (1 << 0),
		df_nvg = (1 << 1),
		df_clear_mask = (1 << 2)
	};

	Flags32 m_eDevicesFlags;

	CHudStateAnimator(CHudAnimatorManager* manager);
	virtual ~CHudStateAnimator() = default;

	virtual void Load() override;
	void Update();
	void OnMovementChanged();
	virtual void SwitchAnimator();

	virtual void ShowStateAnimator(const shared_str& section);
	virtual void HideStateAnimator();

	void SetState(u32 state) { OnStateSwitch(state); }
	u32 GetState() const { return m_current_state; }

	virtual void StopAnimator() override;

	virtual CHudStateAnimator* cast_hud_state_animator() override { return this; }
};

#include "HudPdaAnimator.h"
#include "HudItemAnimator.h"

class CHudAnimatorManager
{
	CActor* m_actor = nullptr;

	CHudPdaAnimator* m_pda_animator = nullptr;
	CHudItemAnimator* m_item_animator = nullptr;
	CHudStateAnimator* m_hud_state_animator = nullptr;
	CBackpackAnimator* m_backpack_animator = nullptr;
	CBurnAnimator* m_burn_animator = nullptr;

	CHudAnimatorBase* m_current_animator = nullptr;
	CHudAnimatorBase* m_target_animator = nullptr;

	bool m_bRestoreDevice = false;
	u8 m_iRestoreSlot = 0;

public:
	CHudAnimatorManager(CActor* parent);
	~CHudAnimatorManager();

	void Update();
	void StopGetAnimator();
	void OnMovementChanged();
	bool IsAnyAnimatorActive();
	bool CanSprint();
	float GetHudFov();
	bool& ForceHideItems();
	bool& RestoreDevice() { return m_bRestoreDevice; }
	u8& SlotToRestore() { return m_iRestoreSlot; }

	CActor* Parent() { return m_actor; }

	void SetCurrentAnimator(CHudAnimatorBase* animator) { m_current_animator = animator; }
	void SetTargetAnimator(CHudAnimatorBase* animator) { m_target_animator = animator; }

	bool InputKeyPress(int cmd);
	bool InputKeyRelease(int cmd);
	bool InputKeyHold(int cmd);

	CHudPdaAnimator* PdaAnimator() { return m_pda_animator; }
	CHudItemAnimator* ItemAnimator() { return m_item_animator; }
	CHudStateAnimator* HudStateAnimator() { return m_hud_state_animator; }
	CHudAnimatorBase* CurrentAnimator() { return m_current_animator; }
	CHudAnimatorBase* TargetAnimator() { return m_target_animator; }
	CBackpackAnimator* BackpackAnimator() { return m_backpack_animator; }
	CBurnAnimator* BurnAnimator() { return m_burn_animator; }
};