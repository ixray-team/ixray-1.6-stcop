#include "StdAfx.h"

#include "../controlled_entity.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "monster_state_controlled_attack.h"

CStateMonsterControlledAttack::CStateMonsterControlledAttack(CBaseMonster* obj) : inherited(obj)
{
}

void CStateMonsterControlledAttack::initialize()
{
	inherited::initialize();
	object->EnemyMan.force_enemy(get_enemy());
}

void CStateMonsterControlledAttack::execute()
{
	object->EnemyMan.force_enemy(get_enemy());
	inherited::execute();
}

void CStateMonsterControlledAttack::finalize()
{
	inherited::finalize();
	object->EnemyMan.unforce_enemy();
}

void CStateMonsterControlledAttack::critical_finalize()
{
	inherited::critical_finalize();
	object->EnemyMan.unforce_enemy();
}

const CEntityAlive* CStateMonsterControlledAttack::get_enemy()
{
	CControlledEntityBase* entity = smart_cast<CControlledEntityBase*>(object);
	VERIFY(entity);
	return smart_cast<const CEntityAlive*>(entity->get_data().m_object);
}
