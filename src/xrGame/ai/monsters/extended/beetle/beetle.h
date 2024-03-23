#pragma once

#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrServerEntities/script_export_space.h"

class CBeetle :	public CTushkano {
public:
					CBeetle();
	virtual			~CBeetle();

	virtual	char*	get_monster_class_name () { return (char*)"beetle"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

add_to_type_list(CBeetle)
#undef script_type_list
#define script_type_list save_type_list(CBeetle)
