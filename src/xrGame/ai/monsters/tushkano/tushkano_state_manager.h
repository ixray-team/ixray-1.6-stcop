#pragma once
#include "../monster_state_manager.h"

class CTushkano;

class CStateManagerTushkano : 
	public CMonsterStateManager<CTushkano> 
{
	using inherited = CMonsterStateManager<CTushkano>;

public:
						CStateManagerTushkano	(CTushkano *obj);
	virtual				~CStateManagerTushkano	();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
