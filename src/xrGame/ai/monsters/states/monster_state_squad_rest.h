#pragma once

#include "../state.h"

class	CStateMonsterSquadRest : public CState{
protected:
	typedef CState	inherited;
	typedef CState *	state_ptr;

	u32		time_next_state_reselect;

public:
						CStateMonsterSquadRest		(CBaseMonster*obj);
	virtual				~CStateMonsterSquadRest		();

	virtual void		reselect_state				();
	virtual void		setup_substates				();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
