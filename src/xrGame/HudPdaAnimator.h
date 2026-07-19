#pragma once

#include "HudAnimatorManager.h"
#include "player_hud.h"
#include "InertionData.h"
#include "../xrUI/Widgets/UIMessages.h"

class CHudPdaAnimator final : public CHudStateAnimator
{
public:
	CHudPdaAnimator(CHudAnimatorManager* manager, const shared_str& pda_section);
	virtual ~CHudPdaAnimator() = default;

	enum EPdaStates : u8
	{
		eBlowout = eLastAnimatorState + 1,
		eAimStart,
		eAimEnd,
	};

	virtual void Load() override;
	virtual void Update();
	virtual void StopAnimator() override;
	virtual float GetHudFov() const override;
	virtual void SwitchAnimator() override;
	bool SwitchZoom();
	virtual bool CanSprint() const override { return m_bCanSprint && !m_bIsZoomed && (GetState() == eIdle || GetState() == eSprintStart); }
	bool IsZoomed() const { return m_bIsZoomed; }

	virtual bool InputKeyPress(int cmd) override;

	virtual EHudOffsetType GetCurrentHudOffsetIdx() const;
	virtual void UpdateHudAdditonal(Fmatrix&) override;

	virtual CHudPdaAnimator* cast_pda_animator() { return this; }
	static float GetPDAScreen_kx();

protected:
	//void OnMotionMark(const motion_marks& mark);
	virtual void OnAnimationEnd(u8 state) override;
	virtual void OnStateSwitch(u8 state) override;
	virtual void PlayAnimIdle() override;
	virtual bool TryPlayAnimIdle() override;

	virtual shared_str SetCurrentStateAnimation(const shared_str& anim_name) override;

	float m_fBlowoutLevel = 1000.0f;
	float m_fZoomRotateTime = 0.25f;
	float m_fZoomRotationFactor = 0.0f;
	float m_fHudFovZoomFactor = 1.0f;

	bool m_bNeedBlowoutAnim = false;
	bool m_bIsZoomed = false;
	bool m_bIsEnabled = false;

	InertionData m_base_inertion;
	InertionData m_zoom_inertion;

	float m_thumb_rot[2]{};
	shared_str m_joystick_bone;
	bool bButtonL = false;
	bool bButtonR = false;
	Fvector target_joystickrot, joystickrot;
	float target_buttonpress, buttonpress;
	u16 m_joystick;

	void ResetJoystick(bool bForce)
	{
		if (bForce)
		{
			joystickrot.set(0.f, 0.f, 0.f);
			buttonpress = 0.f;
		}

		target_joystickrot.set(0.f, 0.f, 0.f);
		target_buttonpress = 0.f;
	}
	static void JoystickCallback(CBoneInstance* B);

public:
	void MouseMovement(float x, float y);
	bool OnMouseAction(float x, float y, EUIMessages mouse_action);
};