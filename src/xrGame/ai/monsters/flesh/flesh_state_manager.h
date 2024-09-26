#pragma once
#include "../monster_state_manager.h"

class CustomFlesh;

class CustomFleshStateManager : public CMonsterStateManager {
	using inherited = CMonsterStateManager;

	CustomFlesh*m_pFlesh;

public:

			CustomFleshStateManager(CustomFlesh* object);
			virtual ~CustomFleshStateManager();

	virtual void	execute				();
	virtual void	remove_links		(CObject* object) { inherited::remove_links(object);}
};
