///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Большой)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrScripts/script_export_space.h"

class CTushkanoBig :
	public CTushkano
{
public:
	CTushkanoBig();
	virtual ~CTushkanoBig();

	virtual	char* get_monster_class_name() { return (char*)"tushkano_big"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};