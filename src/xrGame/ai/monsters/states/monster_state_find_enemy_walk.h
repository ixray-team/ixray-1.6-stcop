#pragma once

#include "../state.h"

class CStateMonsterFindEnemyWalkAround : public CState {
	typedef CState inherited;

public:
						CStateMonsterFindEnemyWalkAround	(CBaseMonster*obj) : inherited(obj) {}
	virtual	void		execute						();
	virtual bool		check_completion			() {return false;}
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
