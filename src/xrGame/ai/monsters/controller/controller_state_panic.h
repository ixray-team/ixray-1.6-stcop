#pragma once
#include "../state.h"

class	CStateControllerPanic : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

	enum {
		eStateRun			= u32(0),
		eStateSteal,
		eStateLookAround,
	};

public:
						CStateControllerPanic	(CBaseMonster *obj);
	virtual				~CStateControllerPanic	();

	virtual void		reselect_state			();
};

#include "controller_state_panic_inline.h"
