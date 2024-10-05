#pragma once

#include "../state.h"

class	CStateMonsterHearHelpSound : public CState {
protected:
	typedef CState	inherited;
	typedef CState*	state_ptr;

public:
					CStateMonsterHearHelpSound	(CBaseMonster*obj);
	virtual			~CStateMonsterHearHelpSound	(){}

	virtual void	reselect_state				();
	virtual void	setup_substates				();

	virtual bool	check_start_conditions		();
	virtual bool	check_completion			();

	virtual void	remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
