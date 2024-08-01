///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Упырь (Подземный)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrScripts/script_export_space.h"

class CGhoul final :
	public CTushkano
{
public:
	CGhoul();
	virtual ~CGhoul();

	virtual	char* get_monster_class_name() { return (char*)"ghoul"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};