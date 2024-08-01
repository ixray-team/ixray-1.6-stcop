#pragma once
#include "../../../BaseMonster/base_monster.h"
#include "../../../controlled_entity.h"
#include "../../../../../../xrScripts/script_export_space.h"

class CBeetle :	public CTushkano 
{
public:
	CBeetle();
	virtual ~CBeetle();

	virtual	char*	get_monster_class_name () { return (char*)"beetle"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};