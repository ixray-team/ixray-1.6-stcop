#pragma once
#include "../state.h"

template<typename _Object>
class	CStateZombieChoke : public CState<_Object> {
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32					m_time_last_choke;
    

public:
						CStateZombieChoke		(_Object *obj);

	virtual void		reinit							();
	
	virtual void		initialize						();
	virtual	void		reselect_state					();
	virtual	void		finalize						();
	virtual	void		critical_finalize				();
	virtual	void		check_force_state					();
	virtual bool		check_start_conditions			();
	virtual bool		check_completion				();

	const CEntityAlive *m_enemy;
};


#include "zombie_choke_inline.h"
