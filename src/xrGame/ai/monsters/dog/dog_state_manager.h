#pragma once
#include "../monster_state_manager.h"

class CustomDog;

class CustomDogStateManager : public CMonsterStateManager {
	using inherited = CMonsterStateManager;

	CustomDog* m_pDog;

public:
	CustomDogStateManager(CustomDog*object);
					virtual ~CustomDogStateManager();

	virtual void	execute				();
			bool	check_eat			();
	virtual void	remove_links		(CObject* object) { inherited::remove_links(object);}
};
