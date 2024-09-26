#pragma once
#include "../monster_state_manager.h"

class CustomFracture;

class CustomFractureStateManager : public CMonsterStateManager {
	using inherited = CMonsterStateManager;

public:
	CustomFractureStateManager(CustomFracture*object);
	virtual				~CustomFractureStateManager();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};
