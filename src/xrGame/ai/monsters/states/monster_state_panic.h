#pragma once

#include "../state.h"

class	CStateMonsterPanic : public CState {
	typedef CState		inherited;
	typedef CState*	state_ptr;
	
public:
						CStateMonsterPanic		(CBaseMonster *obj);
	virtual				~CStateMonsterPanic		();

	virtual void		initialize				();
	virtual	void		reselect_state			();
	virtual void		check_force_state		();
	virtual void		setup_substates			();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
