#pragma once
#include "../monster_state_manager.h"

class CBoarBase;

class CustomBoarStateManager : public CMonsterStateManager
{
	using inherited = CMonsterStateManager;

	CBoarBase* m_pBoar;

public:

	CustomBoarStateManager(CBoarBase*object);
					virtual ~CustomBoarStateManager();

	virtual void	execute				();
	virtual void	remove_links		(CObject* object) { inherited::remove_links(object);}
};
