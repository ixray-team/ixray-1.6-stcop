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

#include "monster_state_hear_danger_sound_inline.h"