#pragma once
#include "../state.h"

class	CStateChimeraHuntingMoveToCover : public CState {
protected:
	typedef CState inherited;

public:
						CStateChimeraHuntingMoveToCover	(CBaseMonster *obj);

	virtual void		initialize						();
	virtual	void		execute							();
	virtual bool 		check_start_conditions			();
	virtual bool 		check_completion				();
};

#include "chimera_state_hunting_move_to_cover_inline.h"
