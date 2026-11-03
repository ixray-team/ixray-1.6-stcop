#pragma once
#include "../monster_state_manager.h"

class CZombie;

class CStateManagerZombie : public CMonsterStateManager<CZombie> {
	typedef CMonsterStateManager<CZombie> inherited;

public:
						CStateManagerZombie		(CZombie *obj);
	virtual ~CStateManagerZombie	() override;

	virtual void		execute					() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};
