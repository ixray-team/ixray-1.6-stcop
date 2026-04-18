#pragma once
#include "../basemonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CStateManagerFracture;

class CFracture final : public CBaseMonster {
	typedef		CBaseMonster		inherited;
	
public:
					CFracture 			();
	virtual			~CFracture 			();	

	virtual void	Load				(const char* section);
	virtual void	CheckSpecParams		(u32 spec_params);

	virtual	char*	get_monster_class_name () { return (char*)"fracture"; }


	DECLARE_SCRIPT_REGISTER_FUNCTION
};