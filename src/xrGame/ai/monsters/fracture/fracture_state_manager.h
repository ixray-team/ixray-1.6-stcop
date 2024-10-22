#pragma once
#include "../monster_state_manager.h"

class CFractureBase;

class CFractureBaseStateManager : public CMonsterStateManager {
protected:
	using inherited = CMonsterStateManager;

public:
	CFractureBaseStateManager(CFractureBase*object);
	virtual				~CFractureBaseStateManager() override;

	virtual	void		execute					() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}
};
