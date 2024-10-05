#pragma once
#include "../state.h"

class CStateChimeraThreatenSteal : public CStateMonsterMoveToPointEx {
	typedef CStateMonsterMoveToPointEx	inherited;

public:
	IC					CStateChimeraThreatenSteal	(CBaseMonster *obj) : inherited(obj){}
	virtual	void		initialize					();	
	virtual void		finalize					();
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual bool		check_start_conditions		();
};
