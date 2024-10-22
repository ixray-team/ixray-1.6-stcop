#pragma once
#include "../monster_state_manager.h"

class CBoarBase;

class CBoarBaseStateManager : public CMonsterStateManager
{
protected:
	using inherited = CMonsterStateManager;

	CBoarBase* pBoarBase;

public:

	CBoarBaseStateManager(CBaseMonster* object);
					virtual ~CBoarBaseStateManager();

	virtual void	execute				();
	virtual void	remove_links		(CObject* object) { inherited::remove_links(object);}
};
