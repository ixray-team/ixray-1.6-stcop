#pragma once
#include "../../tushkano/tushkano.h" // base class
#include "../../monster_state_manager.h"

class CStateManagerBeetle : public CMonsterStateManager<CBeetle>, public CTushkano {
	typedef CMonsterStateManager<CBeetle> inherited;

public:
	CStateManagerBeetle(CBeetle* obj);
	virtual				~CStateManagerBeetle();
};
