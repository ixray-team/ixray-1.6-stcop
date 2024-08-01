///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Псевдогигант
//	Мутант: Прыгающий псевдогигант
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../BaseMonster/base_monster.h"
#include "../../controlled_entity.h"
#include "../../../../../xrScripts/script_export_space.h"

class CPseudogigantJumper final : 
	public CPseudogigant 
{
	using inherited = CPseudogigant;

	float m_damage_after_jump;
	float m_damage_after_jump_impulse;

public:
	CPseudogigantJumper();
	virtual ~CPseudogigantJumper();

	virtual void reinit();
	virtual void Load(LPCSTR section);

	virtual void EndStateJump();
	virtual void HitEntityInJump(const CEntity *pEntity);

	virtual	char* get_monster_class_name() { return (char*)"pseudogigant_jumper"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
