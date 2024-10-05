#pragma once
#include "../state.h"

class CStateMonsterAttackRunAttack : public CState {
	typedef CState inherited;

public:
						CStateMonsterAttackRunAttack	(CBaseMonster *obj) : inherited(obj) {};

	virtual	void		initialize						();
	virtual	void		execute							();
	virtual	void		finalize						();
	virtual	void		critical_finalize				();
	virtual void		remove_links					(CObject* object_) { inherited::remove_links(object_);}

	virtual bool 		check_completion				();
	virtual bool 		check_start_conditions			();
};
