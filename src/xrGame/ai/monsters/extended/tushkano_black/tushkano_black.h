///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Матёрый)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrScripts/script_export_space.h"

class CTushkanoBlack final :
	public CTushkano
{
public:
	CTushkanoBlack();
	virtual ~CTushkanoBlack();

	virtual	char* get_monster_class_name() { return (char*)"tushkano_black"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};