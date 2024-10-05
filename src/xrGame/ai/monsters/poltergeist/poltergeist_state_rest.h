#pragma once

#include "../states/monster_state_rest.h"

class	CPoltergeistStateRest : public CStateMonsterRest {
protected:
	typedef CStateMonsterRest		inherited;
public:
						CPoltergeistStateRest		(CBaseMonster *obj) : inherited(obj) {}
	virtual	void		execute					();
};

