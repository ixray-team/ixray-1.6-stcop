#pragma once

#include "../state.h"
#include "../states/monster_state_attack.h"

class	CStateBurerAttackMelee : public CStateMonsterAttack {
	typedef CStateMonsterAttack	inherited;

public:
						CStateBurerAttackMelee			(CBaseMonster *obj);
	virtual	bool		check_start_conditions			();
	virtual	bool		check_completion				();
};

#include "burer_state_attack_melee_inline.h"
