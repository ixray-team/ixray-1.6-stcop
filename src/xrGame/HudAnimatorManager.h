#pragma once

#include "HudSound.h"
#include "Actor.h"

class HUD_SOUND_COLLECTION;
class CActor;
class CMotionDef;
//class CHudPdaAnimator;
class CHudItemAnimator;
class CBackpackAnimator;
class CHudStateAnimator;

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
	bool m_bRestoreDetector = false;

	float m_fHudFov = 0.0f;
	float m_fHudFovFactor = 1.0f;

	shared_str m_section;
	HUD_SOUND_COLLECTION m_sounds;
	CActor* m_actor = nullptr;
	u8 m_iRestoreSlot = 0;

	bool HudAnimationExist(const shared_str& name);

public:

	CHudAnimatorBase(CActor* parent) : m_actor(parent) {}
	virtual ~CHudAnimatorBase();

	virtual void Load();
	void SetSection(const shared_str& new_section) { m_section = new_section; }
	shared_str GetSection() const { return m_section; }
	bool IsActive() const { return m_bIsPlaying || m_bNeedActivated; }
	virtual void StopAnimator();
	u8 GetSlotToRestore() const { return m_iRestoreSlot; }
	float GetHudFov() const;
	bool CanSprint() const { return m_bCanSprint; }
	bool NeedRestoreDetector() const { return m_bRestoreDetector; }

	virtual CHudItemAnimator* cast_item_animator() { return nullptr; }
	virtual CHudStateAnimator* cast_hud_state_animator() { return nullptr; }
	virtual CBackpackAnimator* cast_backpack_animator() { return nullptr; }
	//virtual CHudPdaAnimator* cast_pda_animator() { return nullptr; }
};

class CHudStateAnimator : public CHudAnimatorBase
{
	u32 m_current_state = eHidden;
	u32 m_on_animation_end_state = eHidden;

	u32 m_dw_curr_state_time = 0;
	u32	m_dw_curr_substate_time = 0;

	bool m_bDisableBore = true;
	bool m_bSwitchSprint = false;

protected:
	u32 CurrStateTime() const { return Device.dwTimeGlobal - m_dw_curr_state_time; }
	void ResetSubStateTime() { m_dw_curr_substate_time = Device.dwTimeGlobal; }
	virtual void OnAnimationEnd(u32 state);
	void OnStateSwitch(u32 state);
	void UpdateAnimation();
	void PlayMotion(const shared_str& name, bool blend, u32 state);
	//void OnMotionMark(const motion_marks& mark);
	void PlayAnimIdle();
	void PlayAnimIdleMoving();
	void PlayAnimIdleMovingSlow();
	void PlayAnimIdleMovingCrouch();
	void PlayAnimIdleMovingCrouchSlow();
	void PlayAnimIdleSprint();
	bool TryPlayAnimIdle();

public:
	enum EAnimatorStates
	{
		eHidden = 0,
		eIdle,
		eBore,
		eSprintStart,
		eSprintEnd,
		eShowing,
		eHiding,
		eLastAnimatorState = eHiding
	};

	CHudStateAnimator(CActor* parent, const shared_str& section);
	virtual ~CHudStateAnimator() = default;

	virtual void Load() override;
	void Update();
	void OnMovementChanged();
	virtual void SwitchAnimator();

	void SetState(u32 state) { OnStateSwitch(state); }
	u32 GetState() const { return m_current_state; }

	virtual CHudStateAnimator* cast_hud_state_animator() override { return this; }
};

//#include "HudPdaAnimator.h"
#include "HudItemAnimator.h"

class CHudAnimatorManager
{
	CActor* m_actor = nullptr;
	//CHudPdaAnimator* m_pda_animator = nullptr;
	CHudItemAnimator* m_item_animator = nullptr;
	CBackpackAnimator* m_backpack_animator = nullptr;
public:
	CHudAnimatorManager(CActor* parent);
	~CHudAnimatorManager();

	void Update();
	void StopGetAnimator();
	void OnMovementChanged();
	bool IsAnyAnimatorActive();
	bool CanSprint();
	float GetHudFov();
	void SetForceHideItems(bool value);
	bool IsForceHideItems();

	//CHudPdaAnimator* PdaAnimator() { return m_pda_animator; }
	CHudItemAnimator* ItemAnimator() { return m_item_animator; }
	CBackpackAnimator* BackpackAnimator() { return m_backpack_animator; }
};