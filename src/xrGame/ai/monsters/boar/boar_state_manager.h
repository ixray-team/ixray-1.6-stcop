#pragma once
#include "../monster_state_manager.h"

class CustomBoar;

class CustomBoarStateManager : public CMonsterStateManager
{
	using inherited = CMonsterStateManager;

	CustomBoar* m_pBoar;

public:

	CustomBoarStateManager(CustomBoar*object);
					virtual ~CustomBoarStateManager();

	virtual void	execute				();
	virtual void	remove_links		(CObject* object) { inherited::remove_links(object);}
};
