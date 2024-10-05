#pragma once

#include "../state.h"

class	CStateMonsterHitted : public CState{
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

public:
					CStateMonsterHitted		(CBaseMonster*obj);
	virtual			~CStateMonsterHitted	();

	virtual	void	reselect_state			();
	virtual void	remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
