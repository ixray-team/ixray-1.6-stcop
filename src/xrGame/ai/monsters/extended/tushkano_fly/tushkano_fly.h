///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Летающий)
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrScripts/script_export_space.h"

class CTushkanoFly final : 
	public CBaseMonster,
	public CControlledEntity<CTushkanoFly> 
{
	using inherited = CBaseMonster;
	using CControlled = CControlledEntity<CTushkanoFly>;

public:
	CTushkanoFly();
	virtual			~CTushkanoFly();

	virtual void	Load(LPCSTR section);
	virtual void	CheckSpecParams(u32 spec_params);

	virtual	char*	get_monster_class_name() { return (char*)"tushkano_fly"; }


	DECLARE_SCRIPT_REGISTER_FUNCTION
};