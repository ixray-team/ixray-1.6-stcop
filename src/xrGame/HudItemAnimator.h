#pragma once

#include "HudAnimatorManager.h"
class CParticlesObject;

class CHudItemAnimator final : public CHudAnimatorBase
{
public:
	CHudItemAnimator(CHudAnimatorManager* manager) : CHudAnimatorBase(manager) {}
	virtual ~CHudItemAnimator() = default;

	virtual void Load() override;
	void Update();
	void StartAnimator(const shared_str& section);
	virtual void StopAnimator() override;

	virtual CHudItemAnimator* cast_item_animator() override { return this; }

private:
	void OnMotionMark(const motion_marks& mark);
	void OnAnimationEnd();
	void UpdateAnimation();
	void PlayMotion();

	bool m_bBlend = false;
};

class CBackpackAnimator final : public CHudStateAnimator
{
public:
	CBackpackAnimator(CHudAnimatorManager* m_manager, const shared_str& section);
	virtual ~CBackpackAnimator() = default;

	virtual void SwitchAnimator() override;

	virtual void OnMotionMark(const motion_marks& mark, u32 state) override;

	virtual CBackpackAnimator* cast_backpack_animator() override { return this; }
	virtual CHudStateAnimator* cast_hud_state_animator() override { return this; }

protected:
	virtual void OnAnimationEnd(u32 state) override;
};

class CBurnAnimator final : public CHudAnimatorBase
{
public:
	float m_burn_restore = 0.0f;
	CBurnAnimator(CHudAnimatorManager* m_manager, const shared_str& section);
	virtual ~CBurnAnimator() = default;
	virtual void Load() override;
	void Update();

	void StartFlameParticle();
	void PlayAnimBurn();

	virtual CBurnAnimator* cast_burn_animator() override { return this; }
	void StartAnimator();

private:
	void OnAnimationEnd();
	void UpdateAnimation();
	xr_shared_ptr<CParticlesObject> m_pFlameParticles;
};