#pragma once
#include "../monster_state_manager.h"

class CZombie;

class CStateManagerZombie : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

	CZombie* m_pZombie;

public:
						CStateManagerZombie		(CZombie *obj);
	virtual				~CStateManagerZombie	();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
