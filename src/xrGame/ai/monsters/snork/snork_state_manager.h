#pragma once
#include "../monster_state_manager.h"

class CSnorkBase;

class CSnorkBaseStateManager : public CMonsterStateManager 
{
protected:
	using				inherited = CMonsterStateManager;

	CSnorkBase* pSnorkBase;

public:
	CSnorkBaseStateManager(CBaseMonster* object);
	virtual				~CSnorkBaseStateManager() override;

	virtual	void		execute					() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object); }
};
