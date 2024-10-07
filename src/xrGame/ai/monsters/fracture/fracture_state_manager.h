#pragma once
#include "../monster_state_manager.h"

class CFractureBase;

class CustomFractureStateManager : public CMonsterStateManager {
	using inherited = CMonsterStateManager;

public:
	CustomFractureStateManager(CFractureBase*object);
	virtual				~CustomFractureStateManager();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};
