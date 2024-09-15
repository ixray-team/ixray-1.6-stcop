#pragma once

#include "../state.h"

class	CStateControllerFastMove : public CState {
protected:
	typedef CState		inherited;
public:
						CStateControllerFastMove	(CBaseMonster *obj) : inherited(obj) {}
	virtual void		initialize					();	
	virtual void		finalize					();	
	virtual void		critical_finalize			();

	virtual void		execute						();
};

#include "controller_state_attack_fast_move_inline.h"
