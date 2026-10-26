#pragma once
#include "../basemonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CStateManagerFracture;

class CFracture final : public CBaseMonster {
	typedef		CBaseMonster		inherited;
	
public:
					CFracture 			();
	virtual ~CFracture 			() override;

	virtual void	Load				(const char* section) override;
	virtual void	CheckSpecParams		(u32 spec_params) override;

	virtual char*	get_monster_class_name () override { return (char*)"fracture"; }


	DECLARE_SCRIPT_REGISTER_FUNCTION
};