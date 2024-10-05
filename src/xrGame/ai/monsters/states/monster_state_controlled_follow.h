#pragma once

#include "../state.h"

class	CStateMonsterControlledFollow : public CState {
	typedef CState	inherited;
	typedef CState*	state_ptr;

public:
						CStateMonsterControlledFollow	(CBaseMonster *obj);
	virtual void		reselect_state					();
	virtual void		setup_substates					();
	virtual void		remove_links					(CObject* object_) { inherited::remove_links(object_);}
};
