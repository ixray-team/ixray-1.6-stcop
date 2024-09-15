#pragma once

class CStateMonsterAttackCampStealOut : public CStateMove {
	typedef CStateMove inherited;

public:
						CStateMonsterAttackCampStealOut	(CBaseMonster*obj);

	virtual	void		execute					();
	virtual bool 		check_completion		();
	virtual bool 		check_start_conditions	();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};

#include "monster_state_attack_camp_stealout_inline.h"
