#pragma once
#include "../../tushkano/tushkano.h"
#include "../../monster_state_manager.h"

class CStateManagerFrog : 
	public CMonsterStateManager<CFrog>, 
	public CTushkano 
{
	using inherited = CMonsterStateManager<CFrog>;

public:
	CStateManagerFrog(CFrog* _object);
	virtual ~CStateManagerFrog();
};
