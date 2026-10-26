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
	virtual ~CVisualZone() override;
	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual void SwitchZoneState(EZoneState new_state) override;
	virtual void Load(const char* section) override;
	virtual void UpdateBlowout();

	virtual CVisualZone* cast_visual_zone() override { return this; }
};
