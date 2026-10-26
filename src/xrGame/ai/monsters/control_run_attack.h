#pragma once
#include "control_combase.h"

class CControlRunAttack : public CControl_ComCustom<> {
	float			m_min_dist;
	float			m_max_dist;

	u32				m_min_delay;
	u32				m_max_delay;

	u32				m_time_next_attack;

public:
	virtual void	load					(const char* section) override;
	virtual void	reinit					() override;

	virtual void	on_event				(ControlCom::EEventType, ControlCom::IEventData*) override;
	virtual void	activate				() override;
	virtual void	on_release				() override;
	virtual bool	check_start_conditions	() override;
};

