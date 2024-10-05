#pragma once

#include "../state.h"

class CStateMonsterAttackMelee : public CState {
	typedef CState inherited;

public:
						CStateMonsterAttackMelee	(CBaseMonster *obj);
	virtual				~CStateMonsterAttackMelee	();

	virtual	void		execute						();

	virtual bool 		check_completion			();
	virtual bool 		check_start_conditions		();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
