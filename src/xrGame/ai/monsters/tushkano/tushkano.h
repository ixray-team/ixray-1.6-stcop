#pragma once
#include "../basemonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

class CTushkano final :	public CBaseMonster,
					public CControlledEntity<CTushkano> {


	typedef		CBaseMonster					inherited;
	typedef		CControlledEntity<CTushkano>	CControlled;

public:
					CTushkano 			();
	virtual ~CTushkano 			() override;

	virtual void	Load				(const char* section) override;
	virtual void	CheckSpecParams		(u32 spec_params) override;
	virtual char*	get_monster_class_name () override { return (char*) "tushkano"; }


	DECLARE_SCRIPT_REGISTER_FUNCTION
};