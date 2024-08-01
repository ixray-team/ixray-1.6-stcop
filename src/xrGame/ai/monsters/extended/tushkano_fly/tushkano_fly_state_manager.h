///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Летающий)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../monster_state_manager.h"

class CTushkanoFly;

class CStateManagerTushkanoFly final : 
	public CMonsterStateManager<CTushkanoFly> 
{
	using inherited = CMonsterStateManager<CTushkanoFly>;

public:
	CStateManagerTushkanoFly(CTushkanoFly* _object);
	virtual				~CStateManagerTushkanoFly();

	virtual	void		execute();
	virtual void		remove_links(CObject* _object) { inherited::remove_links(_object); }
};
