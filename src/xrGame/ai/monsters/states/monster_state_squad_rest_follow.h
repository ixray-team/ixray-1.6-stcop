#pragma once

#include "../state.h"

class	CStateMonsterSquadRestFollow : public CState {
protected:
	typedef CState		inherited;
	typedef CState *	state_ptr;

	Fvector		last_point;

public:
						CStateMonsterSquadRestFollow	(CBaseMonster*obj);
	virtual				~CStateMonsterSquadRestFollow	();

	virtual void		initialize						();
	virtual void		reselect_state					();
	virtual void		setup_substates					();
	virtual void		check_force_state				();
	virtual void		remove_links					(CObject* object_) { inherited::remove_links(object_);}
};
