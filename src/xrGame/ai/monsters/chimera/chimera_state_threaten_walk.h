#pragma once
#include "../state.h"

class CStateChimeraThreatenWalk : public CStateMonsterMoveToPointEx {
	typedef CStateMonsterMoveToPointEx		inherited;

public:
	IC					CStateChimeraThreatenWalk	(CBaseMonster *obj) : inherited(obj){}
	virtual	void		initialize					();	
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual bool		check_start_conditions		();
	
};

#include "chimera_state_threaten_walk_inline.h"
