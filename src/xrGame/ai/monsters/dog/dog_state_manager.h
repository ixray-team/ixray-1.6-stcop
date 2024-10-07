#pragma once
#include "../monster_state_manager.h"

class CDogBase;

class CustomDogStateManager : public CMonsterStateManager {
	using inherited = CMonsterStateManager;

	CDogBase* m_pDog;

public:
	CustomDogStateManager(CDogBase*object);
					virtual ~CustomDogStateManager();

	virtual void	execute				();
			bool	check_eat			();
	virtual void	remove_links		(CObject* object) { inherited::remove_links(object);}
};
