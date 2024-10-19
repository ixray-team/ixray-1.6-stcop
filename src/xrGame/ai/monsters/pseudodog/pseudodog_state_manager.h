#pragma once
#include "../monster_state_manager.h"

class CPseudoDogBase;

class CStateManagerPseudodog : public CMonsterStateManager 
{
	using inherited = CMonsterStateManager;
	
public:

					CStateManagerPseudodog	(CBaseMonster* object);
					virtual ~CStateManagerPseudodog() override;

	virtual void	execute					() override;
	virtual void	remove_links			(CObject* object) override { inherited::remove_links(object);}
};
