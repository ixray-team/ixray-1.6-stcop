#pragma once
#include "../BaseMonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

class CTushkanoBase :	public CBaseMonster,
					public CControlledEntity 
{
protected:
	using		inherited = CBaseMonster;
	using		CControlled = CControlledEntity;

public:
					CTushkanoBase 			();
	virtual			~CTushkanoBase 			() override;

	virtual void	Load				(LPCSTR section) override;
	virtual	char*	get_monster_class_name () override { return (char*) "tushkano"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};