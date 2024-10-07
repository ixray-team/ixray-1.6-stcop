#pragma once

#include "../state.h"

class	CStateGroupEat : public CState {
protected:
	using inherited = CState		;
	using state_ptr = CState*	;

	const CEntityAlive			*corpse;

	CDogBase* m_pDog;

	u32							m_time_last_eat;

public:
	CStateGroupEat		(CBaseMonster*object);
	virtual				~CStateGroupEat		();

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
