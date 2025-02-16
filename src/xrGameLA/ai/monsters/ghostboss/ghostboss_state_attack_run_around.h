#pragma once
#include "../state.h"

template<typename _Object>
class	CStateGhostBossAttackRunAround : public CState<_Object> {
	typedef CState<_Object>	inherited;

	Fvector				selected_point;
	u32					time_started;

	Fvector				dest_direction;

public:
						CStateGhostBossAttackRunAround	(_Object *obj);
	virtual void		initialize					();
	virtual void		execute						();

	virtual bool		check_start_conditions		();
	virtual bool		check_completion			();
};

#include "ghostboss_state_attack_run_around_inline.h"