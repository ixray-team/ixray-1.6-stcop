#pragma once

#include "../state.h"

class	CStateMonsterFindEnemy : public CState {
protected:
	typedef CState	inherited;
	typedef CState*	state_ptr;

public:
						CStateMonsterFindEnemy	(CBaseMonster *obj);
	virtual				~CStateMonsterFindEnemy	();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual	void		reselect_state			();
};
