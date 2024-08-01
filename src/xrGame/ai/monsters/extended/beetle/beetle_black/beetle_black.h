#pragma once
#include "../../../BaseMonster/base_monster.h"
#include "../../../controlled_entity.h"
#include "../../../../../../xrScripts/script_export_space.h"

class CBeetleBlack final : public CBeetle 
{
public:
	CBeetleBlack();
	virtual ~CBeetleBlack();

	virtual	char* get_monster_class_name () { return (char*)"beetle_black"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};