#pragma once
#include "../../../extended/beetle/beetle_normal/beetle_state_manager.h"
#include "../../../monster_state_manager.h"

class CStateManagerBeetleBlack final : 
	public CMonsterStateManager<CBeetleBlack>, 
	public CBeetle
{
	using inherited = CMonsterStateManager<CBeetleBlack>;

public:
	CStateManagerBeetleBlack(CBeetleBlack* _object);
	virtual ~CStateManagerBeetleBlack();
};
