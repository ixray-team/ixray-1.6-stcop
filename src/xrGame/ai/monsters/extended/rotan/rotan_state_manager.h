#pragma once
#include "../../tushkano/tushkano.h"
#include "../../monster_state_manager.h"

class CStateManagerRotan final : 
	public CMonsterStateManager<CRotan>, 
	public CTushkano 
{
	using inherited = CMonsterStateManager<CRotan>;

public:
	CStateManagerRotan(CRotan* _object);
	~CStateManagerRotan();
};
