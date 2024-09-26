#pragma once
#include "../BaseMonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CStateManagerFracture;

class CustomFracture : public CBaseMonster {
	using	inherited =	CBaseMonster	;
	
public:
	CustomFracture();
	virtual			~CustomFracture();

	virtual void	Load				(LPCSTR section);
	virtual void	CheckSpecParams		(u32 spec_params);

	virtual	char*	get_monster_class_name () { return (char*)"fracture"; }


	DECLARE_SCRIPT_REGISTER_FUNCTION
};