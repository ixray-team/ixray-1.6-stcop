#pragma once

#include "../states/monster_state_rest.h"

class	CPoltergeistStateRest : public CStateMonsterRest {
protected:
	using inherited = CStateMonsterRest	;

public:
	CPoltergeistStateRest(CBaseMonster* object);
	virtual ~CPoltergeistStateRest() override;

	virtual	void		execute					() override;
};

