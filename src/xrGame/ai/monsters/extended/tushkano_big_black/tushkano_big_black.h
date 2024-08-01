///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан (Большой)
//	Мутант: Тушкан (Матёрый большой)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrScripts/script_export_space.h"

class CTushkanoBigBlack final :
	public CTushkanoBig
{
public:
	CTushkanoBigBlack();
	virtual ~CTushkanoBigBlack();

	virtual	char* get_monster_class_name() { return (char*)"tushkano_big_black"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};