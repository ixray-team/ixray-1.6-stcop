#pragma once

#include "../state.h"

class CStateMonsterHittedHide : public CState {
	typedef	CState		inherited;
	typedef	CState*	state_ptr;

public:

					CStateMonsterHittedHide	(CBaseMonster*obj) : inherited(obj) {}
	virtual			~CStateMonsterHittedHide() {}

	virtual void	initialize				();
	virtual void	execute					();
	virtual void	remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual bool 	check_completion		();
	virtual bool 	check_start_conditions	();
};
