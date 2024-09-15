#pragma once
#include "../state.h"

class	CStateChimeraHuntingComeOut : public CState {
protected:
	typedef CState inherited;

public:
						CStateChimeraHuntingComeOut	(CBaseMonster *obj);

	virtual	void		reselect_state				();
	virtual bool 		check_start_conditions		();	
	virtual bool 		check_completion			();	

};

#include "chimera_state_hunting_come_out_inline.h"
