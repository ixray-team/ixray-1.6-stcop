#pragma once

#include "../state.h"

class	CStateMonsterHearDangerousSound : public CState {
protected:
	typedef CState	inherited;
	typedef CState*	state_ptr;

public:
					CStateMonsterHearDangerousSound		(CBaseMonster*obj);
	virtual			~CStateMonsterHearDangerousSound	() {}

	virtual void	reselect_state						();
	virtual void	setup_substates						();
	virtual void	remove_links						(CObject* object_) { inherited::remove_links(object_);}
};
