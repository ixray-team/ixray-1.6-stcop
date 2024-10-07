#pragma once
#include "../BaseMonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

class CTushkanoBase :	public CBaseMonster,
					public CControlledEntity {


	typedef		CBaseMonster					inherited;
	typedef		CControlledEntity	CControlled;

public:
					CTushkanoBase 			();
	virtual			~CTushkanoBase 			();	

	virtual void	Load				(LPCSTR section);
	virtual	char*	get_monster_class_name () { return (char*) "tushkano"; }


	DECLARE_SCRIPT_REGISTER_FUNCTION
};