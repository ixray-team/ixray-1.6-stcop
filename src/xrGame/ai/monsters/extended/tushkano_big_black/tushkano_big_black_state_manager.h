///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан (Большой)
//	Мутант: Тушкан (Матёрый большой)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../extended/tushkano_big/tushkano_big_state_manager.h"
#include "../../monster_state_manager.h"

class CStateManagerTushkanoBigBlack final :
	public CMonsterStateManager<CTushkanoBigBlack>,
	public CTushkanoBig
{
	using inherited = CMonsterStateManager<CTushkanoBigBlack>;

public:
	CStateManagerTushkanoBigBlack(CTushkanoBigBlack* _object);
	virtual ~CStateManagerTushkanoBigBlack();
};
