///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Большой)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../tushkano/tushkano.h"
#include "../../monster_state_manager.h"

class CStateManagerTushkanoBig final :
	public CMonsterStateManager<CTushkanoBig>,
	public CTushkano
{
	using inherited = CMonsterStateManager<CTushkanoBig>;

public:
	CStateManagerTushkanoBig(CTushkanoBig* _object);
	~CStateManagerTushkanoBig();
};