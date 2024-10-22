#pragma once

#include "../monster_state_manager.h"

class CStateControlCamp : public CState {
protected:
	using inherited = CState;

	float			m_angle_from;
	float			m_angle_to;
	
	float			m_target_angle;
	u32				m_time_next_updated;

	CControllerBase* pControllerBase;

public:

	CStateControlCamp(CBaseMonster* object);
	virtual			~CStateControlCamp() override;

	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual bool	check_completion		() override;
	virtual bool	check_start_conditions	() override;

	virtual void	remove_links(CObject* object) override { inherited::remove_links(object); }

private:

	 void	update_target_angle		();

};
