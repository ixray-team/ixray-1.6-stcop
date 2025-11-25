#pragma once

#include "HudSound.h"
#include "Actor.h"

class HUD_SOUND_COLLECTION;
class CActor;
class CMotionDef;
//class CHudPdaAnimator;
class CHudItemAnimator;

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

	virtual CHudItemAnimator* cast_item_animator() { return nullptr; }
	//virtual CHudPdaAnimator* cast_pda_animator() { return nullptr; }
};

//#include "HudPdaAnimator.h"
#include "HudItemAnimator.h"

class CHudAnimatorManager
{
public:
	CHudAnimatorManager(CActor* parent);
	~CHudAnimatorManager();

	void Update();
	void StopGetAnimator();
	bool IsAnyAnimatorActive();
	bool CanSprint();
	float GetHudFov();
	void SetForceHideItems(bool value);
	bool IsForceHideItems();

	//CHudPdaAnimator* PdaAnimator() { return m_pda_animator; }
	CHudItemAnimator* ItemAnimator() { return m_item_animator; }

private:
	CActor* m_actor = nullptr;
	//CHudPdaAnimator* m_pda_animator = nullptr;
	CHudItemAnimator* m_item_animator = nullptr;
};