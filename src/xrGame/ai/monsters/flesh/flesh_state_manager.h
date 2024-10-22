#pragma once
#include "../monster_state_manager.h"

class CFleshBase;

class CFleshBaseStateManager : public CMonsterStateManager {
protected:
	using inherited = CMonsterStateManager;

	CFleshBase* pFleshBase;

public:

	CFleshBaseStateManager(CFleshBase* object);
			virtual ~CFleshBaseStateManager() override;

	virtual void	execute				() override;
	virtual void	remove_links		(CObject* object) override { inherited::remove_links(object);}
};
