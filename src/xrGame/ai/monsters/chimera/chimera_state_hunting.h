#pragma once
#include "../state.h"

class	CStateChimeraHunting : public CState {
protected:
	typedef CState inherited;
	typedef CState* state_ptr;

	enum {
		eStateMoveToCover,
		eStateComeOut
	};

public:
						CStateChimeraHunting	(CBaseMonster *obj);

	virtual	void		reselect_state			();
	virtual bool 		check_start_conditions	();	
	virtual bool 		check_completion		();	

};

#include "chimera_state_hunting_inline.h"
