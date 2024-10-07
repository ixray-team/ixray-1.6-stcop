#pragma once

#include "../monster_state_manager.h"

class CStateControlCamp : public CState {
	typedef	CState		inherited;

	float			m_angle_from;
	float			m_angle_to;
	
	float			m_target_angle;
	u32				m_time_next_updated;

	CControllerBase* m_pController;

public:

	CStateControlCamp(CBaseMonster* obj);
	virtual			~CStateControlCamp	() {}

	virtual void	initialize				();
	virtual void	execute					();
	virtual bool	check_completion		();
	virtual bool	check_start_conditions	();

	virtual void	remove_links			(CObject* object) {}

private:

	virtual void	update_target_angle		();

};
