///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Лягушка
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrScripts/script_export_space.h"

class CFrog final : 
	public CTushkano 
{
public:
	CFrog();
	virtual ~CFrog();

	virtual	char* get_monster_class_name() { return (char*)"frog"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};