///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Ротан (Болотный)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrScripts/script_export_space.h"

class CRotan final : 
	public CTushkano 
{
public:
	CRotan();
	virtual ~CRotan();

	virtual	char* get_monster_class_name() { return (char*)"frog"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};