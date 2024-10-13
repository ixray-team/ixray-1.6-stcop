#pragma once
#include "../monster_state_manager.h"

class CZombieBase;

class CZombieBaseStateManager : public CMonsterStateManager 
{
protected:
	using inherited = CMonsterStateManager;

	CZombieBase* pZombieBase;

public:
	CZombieBaseStateManager(CBaseMonster* object);
	virtual				~CZombieBaseStateManager() override;

	virtual	void		execute					() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}
};
