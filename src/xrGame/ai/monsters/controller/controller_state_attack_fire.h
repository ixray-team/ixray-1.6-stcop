#pragma once

template<typename _Object>
class CStateControlFire : public CState<_Object> {
	typedef	CState<_Object>		inherited;

	u32				m_time_started;
	u32				m_time_state_last_execute;

public:

					CStateControlFire	(_Object *obj) : inherited(obj) {}
	virtual ~CStateControlFire	() override = default;

	virtual void	reinit					() override;
	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual void	finalize				() override;
	virtual void	critical_finalize		() override;
	virtual bool	check_completion		() override;
	virtual bool	check_start_conditions	() override;

};

#include "controller_state_attack_fire_inline.h"
