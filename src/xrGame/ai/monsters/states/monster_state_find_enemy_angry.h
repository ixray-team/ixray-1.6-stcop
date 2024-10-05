#pragma once

#include "../state.h"

class CStateMonsterFindEnemyAngry : public CState {
	typedef CState inherited;

public:
						CStateMonsterFindEnemyAngry	(CBaseMonster*obj);
	virtual				~CStateMonsterFindEnemyAngry();

	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
