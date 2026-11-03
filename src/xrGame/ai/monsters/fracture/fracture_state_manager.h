#pragma once
#include "../monster_state_manager.h"

class CFracture;

class CStateManagerFracture : public CMonsterStateManager<CFracture> {
	typedef CMonsterStateManager<CFracture> inherited;

public:
						CStateManagerFracture	(CFracture *obj);
	virtual ~CStateManagerFracture	() override;

	virtual void		execute					() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};
