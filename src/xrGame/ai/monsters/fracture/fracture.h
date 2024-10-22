#pragma once
#include "../BaseMonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CStateManagerFracture;

class CFractureBase : public CBaseMonster {
protected:
	using	inherited =	CBaseMonster	;
	
public:
	CFractureBase();
	virtual			~CFractureBase() override;

	virtual void	Load				(LPCSTR section) override;
	virtual void	CheckSpecParams		(u32 spec_params) override;

	virtual	char*	get_monster_class_name () override { return (char*)"fracture"; }


	DECLARE_SCRIPT_REGISTER_FUNCTION
};