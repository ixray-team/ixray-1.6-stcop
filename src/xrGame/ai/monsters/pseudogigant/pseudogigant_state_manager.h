#pragma once
#include "../monster_state_manager.h"

class CPseudoGiantBase;

class CPseudoGiantBaseStateManager : public CMonsterStateManager 
{
protected:
	using inherited = CMonsterStateManager;

	CPseudoGiantBase* pPseudoGiantBase;

public:

	CPseudoGiantBaseStateManager(CBaseMonster* object);
	virtual ~CPseudoGiantBaseStateManager() override;

	virtual void	execute				() override;
	virtual void	remove_links		(CObject* object) override { inherited::remove_links(object); }
};
