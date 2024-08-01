///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Летающий)
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "tushkano_fly.h"
#include "tushkano_fly_state_manager.h"

#include "../../control_animation_base.h"
#include "../../control_direction_base.h"
#include "../../control_movement_base.h"
#include "../../control_path_builder_base.h"

#include "../../states/monster_state_rest.h"
#include "../../states/monster_state_eat.h"
#include "../../states/monster_state_attack.h"
#include "../../states/monster_state_panic.h"
#include "../../states/monster_state_hear_danger_sound.h"
#include "../../states/monster_state_hitted.h"
#include "../../states/monster_state_controlled.h"
#include "../../states/monster_state_help_sound.h"

#include "../../../../entitycondition.h"


CStateManagerTushkanoFly::CStateManagerTushkanoFly(CTushkanoFly* _object) : inherited(_object)
{
	add_state(eStateRest, new CStateMonsterRest<CTushkanoFly>(_object));
	add_state(eStateAttack, new CStateMonsterAttack<CTushkanoFly>(_object));
	add_state(eStateEat, new CStateMonsterEat<CTushkanoFly>(_object));
	add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound<CTushkanoFly>(_object));
	add_state(eStatePanic, new CStateMonsterPanic<CTushkanoFly>(_object));
	add_state(eStateHitted, new CStateMonsterHitted<CTushkanoFly>(_object));
	add_state(eStateControlled, new CStateMonsterControlled<CTushkanoFly>(_object));
	add_state(eStateHearHelpSound, new CStateMonsterHearHelpSound<CTushkanoFly>(_object));
}

CStateManagerTushkanoFly::~CStateManagerTushkanoFly()
{

}

void CStateManagerTushkanoFly::execute()
{
	u32 state_id = u32(-1);

	if (!object->is_under_control()) 
	{
		object->CorpseMan.get_corpse();

		if (object->EnemyMan.get_enemy()) 
		{
			switch (object->EnemyMan.get_danger_type()) 
			{
				case eStrong:	state_id = eStatePanic; break;
				case eWeak:		state_id = eStateAttack; break;
			}
		}
		else if (object->HitMemory.is_hit()) 
		{
			state_id = eStateHitted;
		}
		else if (check_state(eStateHearHelpSound))
		{
			state_id = eStateHearHelpSound;
		}
		else if (object->hear_interesting_sound || object->hear_dangerous_sound) 
		{
			state_id = eStateHearDangerousSound;
		}
		else 
		{
			if (can_eat())	state_id = eStateEat;
			else 			state_id = eStateRest;
		}
	}
	else state_id = eStateControlled;

	// установить текущее состояние
	select_state(state_id);

	// выполнить текущее состояние
	get_state_current()->execute();

	prev_substate = current_substate;
}

