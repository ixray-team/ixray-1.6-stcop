#pragma once
#include "../monster_state_manager.h"

class CZombieBase;

class CStateManagerZombie : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

	CZombieBase* m_pZombie;

public:
						CStateManagerZombie		(CZombieBase *obj);
	virtual				~CStateManagerZombie	();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
