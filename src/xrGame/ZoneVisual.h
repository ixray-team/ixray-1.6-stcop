#pragma once

class CVisualZone : public CAnomalyZone
{
	using inherited = CAnomalyZone;

	MotionID m_idle_animation;
	MotionID m_attack_animation;
	u32 m_dwAttackAnimaionStart;
	u32 m_dwAttackAnimaionEnd;

public:
	CVisualZone();
	virtual ~CVisualZone();
	virtual bool net_Spawn(CSE_Abstract* DC);
	virtual void SwitchZoneState(EZoneState new_state);
	virtual void Load(const char* section);
	virtual void UpdateBlowout();

	CVisualZone* cast_visual_zone() override { return this; }
};
