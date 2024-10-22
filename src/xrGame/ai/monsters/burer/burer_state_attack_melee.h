#pragma once

#include "../state.h"
#include "../states/monster_state_attack.h"

class	CStateBurerAttackMelee : public CStateMonsterAttack {
protected:
	using inherited = CStateMonsterAttack;

public:
						CStateBurerAttackMelee			(CBaseMonster *object);
						virtual ~CStateBurerAttackMelee() override;

	virtual	bool		check_start_conditions			() override;
	virtual	bool		check_completion				() override;
};
