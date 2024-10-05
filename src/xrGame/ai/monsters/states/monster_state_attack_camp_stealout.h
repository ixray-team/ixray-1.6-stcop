#pragma once

#include "../state.h"

class CStateMonsterAttackCampStealOut : public CStateMove {
	typedef CStateMove inherited;

public:
						CStateMonsterAttackCampStealOut	(CBaseMonster*obj);

	virtual	void		execute					();
	virtual bool 		check_completion		();
	virtual bool 		check_start_conditions	();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
