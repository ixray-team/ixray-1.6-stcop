#pragma once

#include "../state.h"

class	CStateMonsterHearInterestingSound : public CState {
protected:
	typedef CState	inherited;
	typedef CState*	state_ptr;

public:
					CStateMonsterHearInterestingSound	(CBaseMonster*obj);
	virtual			~CStateMonsterHearInterestingSound	(){}

	virtual void	reselect_state						();
	virtual void	setup_substates						();
	virtual void	remove_links						(CObject* object_) { inherited::remove_links(object_);}

private:
			Fvector	get_target_position					();

};
