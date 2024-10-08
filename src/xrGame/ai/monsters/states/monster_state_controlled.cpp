#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "../controlled_entity.h"

#include "monster_state_controlled.h"

#include "monster_state_controlled_attack.h"
#include "monster_state_controlled_follow.h"

CStateMonsterControlled::CStateMonsterControlled(CBaseMonster* obj) : inherited(obj)
{
    this->add_state(eStateControlled_Attack, new CStateMonsterControlledAttack(obj));
    this->add_state(eStateControlled_Follow, new CStateMonsterControlledFollow(obj));
}


void CStateMonsterControlled::execute()
{
	CControlledEntityBase* pEntityBase = smart_cast<CControlledEntityBase*>(this->object);

	switch (pEntityBase->get_data().m_task) {
	case eTaskFollow:	this->select_state(eStateControlled_Follow);	break;
	case eTaskAttack: {
		// проверить валидность данных атаки
		const CEntity* enemy = pEntityBase->get_data().m_object;
		if (!enemy || enemy->getDestroy() || !enemy->g_Alive()) {
			pEntityBase->get_data().m_object = smart_cast<CControllerBase*>(this->object);
			this->select_state(eStateControlled_Follow);
		}
		else
			this->select_state(eStateControlled_Attack);	break;
	}
	default:			NODEFAULT;
	}

	this->get_state_current()->execute();

	this->prev_substate = this->current_substate;

}
