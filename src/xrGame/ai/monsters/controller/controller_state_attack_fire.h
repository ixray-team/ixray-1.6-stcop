#pragma once

#include "../state.h"

class CStateControlFire : public CState {
protected:
	using	inherited = CState;

	u32				m_time_started;
	u32				m_time_state_last_execute;

	CControllerBase* pControllerBase;

public:
	CStateControlFire(CBaseMonster* object);
	virtual			~CStateControlFire() override;

	virtual void	reinit					() override;
	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual void	finalize				() override;
	virtual void	critical_finalize		() override;
	virtual bool	check_completion		() override;
	virtual bool	check_start_conditions	() override;

};
