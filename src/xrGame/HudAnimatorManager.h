#pragma once

#include "HudSound.h"
#include "Actor.h"

class HUD_SOUND_COLLECTION;
class CActor;
class CMotionDef;

class CHudAnimatorManager
{
public:
	CHudAnimatorManager(CActor* parent);
	~CHudAnimatorManager();

	void Load();
	void SetSection(const shared_str& new_section) { m_section = new_section; }
	shared_str GetSection() const { return m_section; }
	void Update();
	bool IsActive() const { return m_bIsPlaying || m_bNeedActivated; }
	void StartAnimator(const shared_str& section);
	void StopAnimator();
	u8 GetSlotToRestore() const { return m_iRestoreSlot; }
	void SetLeftCallback(xr_delegate<void()> callback) { m_left_callback = callback; }
	void SetLeft2Callback(xr_delegate<void()> callback) { m_left2_callback = callback; }
	void SetRightCallback(xr_delegate<void()> callback) { m_right_callback = callback; }
	void SetRight2Callback(xr_delegate<void()> callback) { m_right2_callback = callback; }
	void SetStartCallback(xr_delegate<void()> callback) { m_start_callback = callback; }
	void SetEndCallback(xr_delegate<void()> callback) { m_end_callback = callback; }
	float GetHudFov() const;
	bool CanSprint() const { return m_bCanSprint; }
	void SetForceHideItems(bool value) { m_bForceHideItems = value; }
	bool IsForceHideItems() const { return m_bForceHideItems; }

private:

	void OnMotionMark(const motion_marks& mark);
	void OnAnimationEnd();
	void UpdateAnimation();
	void PlayMotion();

	void CallLeftCallback();
	void CallLeft2Callback();
	void CallRightCallback();
	void CallRight2Callback();
	void CallStartCallback();
	void CallEndCallback();

	shared_str m_section;
	HUD_SOUND_COLLECTION m_sounds;
	CActor* m_actor = nullptr;
	bool m_bRestoreDetector = false;
	u8 m_iRestoreSlot = 0;
	bool m_bNeedActivated = false;
	bool m_bIsPlaying = false;
	float m_fHudFov = 0.0f;
	bool m_bBlend = false;
	bool m_bForceHideItems = false;
	bool m_bCanSprint = false;

	xr_delegate<void()> m_left_callback = nullptr;
	xr_delegate<void()> m_left2_callback = nullptr;
	xr_delegate<void()> m_right_callback = nullptr;
	xr_delegate<void()> m_right2_callback = nullptr;
	xr_delegate<void()> m_start_callback = nullptr;
	xr_delegate<void()> m_end_callback = nullptr;

	u32	m_dwMotionCurrTm = 0;
	u32	m_dwMotionStartTm = 0;
	u32	m_dwMotionEndTm = 0;
	bool m_bStopAtEndAnimIsRunning = true;
	const CMotionDef* m_current_motion_def = nullptr;

	shared_str m_sLuaLeftCallback = "null";
	shared_str m_sLuaLeft2Callback = "null";
	shared_str m_sLuaRightCallback = "null";
	shared_str m_sLuaRight2Callback = "null";
	shared_str m_sLuaStartCallback = "null";
	shared_str m_sLuaEndCallback = "null";
	shared_str m_sLuaModifySect = "null";
	shared_str m_sLuaPrecondFunc = "null";
};