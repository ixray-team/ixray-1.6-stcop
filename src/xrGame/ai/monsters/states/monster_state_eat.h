#pragma once

#include "../state.h"

class	CStateMonsterEat : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

	const CEntityAlive			*corpse;

	u32							m_time_last_eat;

public:
						CStateMonsterEat		(CBaseMonster*obj);
	virtual				~CStateMonsterEat		();

	virtual	void		reinit					();
	virtual void		initialize				();
	virtual void		finalize				();
	virtual void		critical_finalize		();
	virtual void		remove_links			(CObject* object);

	virtual	void		reselect_state			();
	virtual	void		setup_substates			();
	virtual bool		check_completion		();
	virtual bool		check_start_conditions	();

private:
	
			bool		hungry					();

};
