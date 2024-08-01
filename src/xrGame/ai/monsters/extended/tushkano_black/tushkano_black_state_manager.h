///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Матёрый)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../tushkano/tushkano.h"
#include "../../monster_state_manager.h"

class CStateManagerTushkanoBlack final :
	public CMonsterStateManager<CTushkanoBlack>,
	public CTushkano
{
	using inherited = CMonsterStateManager<CTushkanoBlack>;

public:
	CStateManagerTushkanoBlack(CTushkanoBlack* _object);
	~CStateManagerTushkanoBlack();
};