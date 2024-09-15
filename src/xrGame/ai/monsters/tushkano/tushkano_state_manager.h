#pragma once
#include "../monster_state_manager.h"

class CTushkano;

class CStateManagerTushkano : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

public:
						CStateManagerTushkano	(CTushkano *obj);
	virtual				~CStateManagerTushkano	();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
