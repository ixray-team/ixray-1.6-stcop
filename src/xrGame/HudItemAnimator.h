#pragma once

#include "HudAnimatorManager.h"

class CHudItemAnimator final : public CHudAnimatorBase
{
public:
	CHudItemAnimator(CActor* parent) : CHudAnimatorBase(parent) {}
	virtual ~CHudItemAnimator() = default;

	virtual void Load() override;
	void Update();
	void StartAnimator(const shared_str& section);
	virtual void StopAnimator() override;

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

	virtual CHudItemAnimator* cast_item_animator() override { return this; }

private:
	void OnMotionMark(const motion_marks& mark);
	void OnAnimationEnd();
	void UpdateAnimation();
	void PlayMotion();

	bool m_bBlend = false;

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
};