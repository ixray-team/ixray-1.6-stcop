#pragma once

CStateMonsterControlledAttack::CStateMonsterControlledAttack(CBaseMonster*obj) : inherited(obj)
{
}

void CStateMonsterControlledAttack::initialize()
{
	inherited::initialize();
	this->object->EnemyMan.force_enemy(get_enemy());
}

void CStateMonsterControlledAttack::execute()
{
	this->object->EnemyMan.force_enemy(get_enemy());
	inherited::execute();
}

void CStateMonsterControlledAttack::finalize()
{
	inherited::finalize();
	this->object->EnemyMan.unforce_enemy();
}

void CStateMonsterControlledAttack::critical_finalize()
{
	inherited::critical_finalize();
	this->object->EnemyMan.unforce_enemy();
}

const CEntityAlive * CStateMonsterControlledAttack::get_enemy()
{
	CControlledEntityBase *entity = smart_cast<CControlledEntityBase *>(this->object);
	VERIFY(entity);
	return smart_cast<const CEntityAlive*>(entity->get_data().m_object);
}
