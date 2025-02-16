#pragma once
#include "../state.h"

template<typename _Object>
class	CStateBloodsuckerVampire : public CState<_Object> {
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32					m_time_last_vampire;
    

public:
						CStateBloodsuckerVampire		(_Object *obj);

	virtual void		reinit							();
	
	virtual void		initialize						();
	virtual	void		reselect_state					();
	virtual	void		finalize						();
	virtual	void		critical_finalize				();
	virtual bool		check_start_conditions			();
	virtual bool		check_completion				();

	virtual void		setup_substates					();
	virtual void		check_force_state				();

	const CEntityAlive *m_enemy;
    const CAI_Bloodsucker *monster;
//	float b_max_reach_distance;
};


#include "bloodsucker_vampire_inline.h"
