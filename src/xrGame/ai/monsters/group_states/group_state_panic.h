#pragma once

#include "../state.h"

class	CStateGroupPanic : public CState {
	typedef CState	inherited;
	typedef CState*	state_ptr;
	
public:
						CStateGroupPanic		(CBaseMonster*obj);
	virtual				~CStateGroupPanic		();

	virtual void		initialize				();
	virtual	void		reselect_state			();
	virtual void		check_force_state		();
	virtual void		setup_substates			();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};
