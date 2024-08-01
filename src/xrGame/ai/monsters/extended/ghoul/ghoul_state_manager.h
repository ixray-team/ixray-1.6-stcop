///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Упырь (Подземный)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../tushkano/tushkano.h"
#include "../../monster_state_manager.h"

class CStateManagerGhoul :
	public CMonsterStateManager<CGhoul>,
	public CTushkano
{
	using inherited = CMonsterStateManager<CGhoul>;

public:
	CStateManagerGhoul(CGhoul* _object);
	virtual ~CStateManagerGhoul();
};