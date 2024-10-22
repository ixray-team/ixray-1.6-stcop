#pragma once
#include "../monster_state_manager.h"

class CPseudoDogBase;

class CPseudoDogBaseStateManager : public CMonsterStateManager
{
protected:
	using inherited = CMonsterStateManager;
	
public:

	CPseudoDogBaseStateManager(CBaseMonster* object);
					virtual ~CPseudoDogBaseStateManager() override;

	virtual void	execute					() override;
	virtual void	remove_links			(CObject* object) override { inherited::remove_links(object);}
};
