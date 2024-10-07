#pragma once
#include "../monster_state_manager.h"

class CFleshBase;

class CustomFleshStateManager : public CMonsterStateManager {
	using inherited = CMonsterStateManager;

	CFleshBase*m_pFlesh;

public:

			CustomFleshStateManager(CFleshBase* object);
			virtual ~CustomFleshStateManager();

	virtual void	execute				();
	virtual void	remove_links		(CObject* object) { inherited::remove_links(object);}
};
