#pragma once
#include "../../../tushkano/tushkano.h"
#include "../../../monster_state_manager.h"

class CStateManagerBeetle : 
	public CMonsterStateManager<CBeetle>,
	public CTushkano 
{
	using inherited = CMonsterStateManager<CBeetle>;

public:
	CStateManagerBeetle(CBeetle* _object);
	virtual ~CStateManagerBeetle();
};
