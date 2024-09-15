#pragma once

class CStateControlFire : public CState {
	typedef	CState		inherited;

	u32				m_time_started;
	u32				m_time_state_last_execute;

public:

					CStateControlFire	(CBaseMonster *obj) : inherited(obj) {}
	virtual			~CStateControlFire	() {}

	virtual void	reinit					();
	virtual void	initialize				();
	virtual void	execute					();
	virtual void	finalize				();
	virtual void	critical_finalize		();
	virtual bool	check_completion		();
	virtual bool	check_start_conditions	();

};

#include "controller_state_attack_fire_inline.h"
