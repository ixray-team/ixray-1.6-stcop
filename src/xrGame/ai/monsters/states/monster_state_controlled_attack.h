#pragma once

#include "../state.h"
#include "monster_state_attack.h"

class	CStateMonsterControlledAttack : public CStateMonsterAttack {
	typedef CStateMonsterAttack	inherited;

public:
						CStateMonsterControlledAttack	(CBaseMonster *obj);
	virtual void		initialize						();
	virtual	void		execute							();
	virtual void		finalize						();
	virtual void		critical_finalize				();

private:
	const CEntityAlive	*get_enemy						();
};


