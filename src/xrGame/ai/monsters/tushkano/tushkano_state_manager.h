#pragma once
#include "../monster_state_manager.h"

class CTushkanoBase;

class CTushkanoBaseStateManager : public CMonsterStateManager 
{
protected:
	using inherited = CMonsterStateManager;

	CTushkanoBase* pTushkanoBase;

public:
	CTushkanoBaseStateManager(CBaseMonster*object);
	virtual				~CTushkanoBaseStateManager();

	virtual	void		execute					() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}
};
