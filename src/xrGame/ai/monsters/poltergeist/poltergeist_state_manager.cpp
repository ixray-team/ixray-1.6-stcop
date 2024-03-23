#include "stdafx.h"
#include "poltergeist.h"
#include "poltergeist_state_manager.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "poltergeist_state_rest.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_attack.h"
#include "../states/monster_state_panic.h"
#include "poltergeist_state_attack_hidden.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hear_danger_sound.h"
#include "../states/monster_state_hitted.h"
#include "../../../entitycondition.h"

CStateManagerPoltergeist::CStateManagerPoltergeist(CPoltergeist *obj) : inherited(obj)
{
    CStateMonsterAttackMoveToHomePoint<CPoltergeist>* PolterAttackHands =
                                            xr_new<CStateMonsterAttackMoveToHomePoint<CPoltergeist>>(obj);

	add_state(eStateRest,					xr_new<CPoltergeistStateRest<CPoltergeist> > (obj));
	add_state(eStateEat,					xr_new<CStateMonsterEat<CPoltergeist> >(obj));

    add_state(eStateAttack,                 xr_new<CStateMonsterAttack<CPoltergeist>>(obj, PolterAttackHands));
	add_state(eStateAttack_AttackHidden,	xr_new<CStatePoltergeistAttackHidden<CPoltergeist> > (obj));

	add_state(eStatePanic,					xr_new<CStateMonsterPanic<CPoltergeist> >(obj));
	add_state(eStateHitted,					xr_new<CStateMonsterHitted<CPoltergeist> >(obj));
	add_state(eStateHearInterestingSound,	xr_new<CStateMonsterHearInterestingSound<CPoltergeist> >(obj));
	add_state(eStateHearDangerousSound,		xr_new<CStateMonsterHearDangerousSound<CPoltergeist> >(obj));
}

CStateManagerPoltergeist::~CStateManagerPoltergeist()
{
}

void CStateManagerPoltergeist::reinit()
{
	inherited::reinit();
	
	time_next_flame_attack	= 0;
	time_next_tele_attack	= 0;
	time_next_scare_attack	= 0;

}

void CStateManagerPoltergeist::execute()
{
    u32 state_id = u32(-1);

    if (object->ability_tele() && object->get_value_old_logic())
    {
        if (object->EnemyMan.get_enemy())
        {
            if (object->EnemyMan.get_enemy() != Actor())
            {
                switch (object->EnemyMan.get_danger_type())
                {
                    case eWeak:
                    {
                        if (object->EnemyMan.get_enemy()->conditions().health() < 0.5f)
                        {
                            object->SetValueDisableHide(false);

                            object->on_deactivate();
                            state_id = eStateAttack;
                        }
                        else
                        {
                            object->SetValueDisableHide(true);

                            object->on_activate();
                            state_id = eStatePanic;
                        }
                    }
                    break;

                    case eStrong:
                    {
                        object->on_activate();
                        state_id = eStatePanic;
                    }
                    break;
                }
            }
            else
            {
                switch (object->EnemyMan.get_danger_type())
                {
                    case eWeak:
                    case eStrong:
                    default:
                    {
                        if (Actor()->GetfHealth() < 0.5f)
                        {
                            object->SetValueDisableHide(false);

                            object->on_deactivate();
                            state_id = eStateAttack;
                        }
                        else if (Actor()->GetfHealth() > 0.5f)
                        {
                            object->SetValueDisableHide(false);

                            object->on_activate();
                            state_id = eStateAttack_AttackHidden;
                        }
                        else
                        {
                            object->SetValueDisableHide(false);

                            object->on_activate();
                            state_id = eStatePanic;
                        }
                    }
                }
            }
        }
        else if (object->HitMemory.is_hit())
        {
            state_id = eStateHitted;
        }
        else if (object->hear_interesting_sound || object->hear_dangerous_sound)
        {
            state_id = eStateHearDangerousSound;
        }
        else
        {
            if (can_eat())
            {
                state_id = eStateEat;
            }
            else
            {
                object->SetValueDisableHide(false);

                object->on_deactivate();
                state_id = eStateRest;
            }
        }

        if (state_id == eStateEat)
        {
            if (object->CorpseMan.get_corpse()->Position().distance_to(object->Position()) < 10.f)
            {
                object->on_deactivate();
            }
        }
    }
    else
    {
        if (object->EnemyMan.get_enemy() && object->detected_enemy())
        {
            state_id = eStateAttack_AttackHidden;
        }
        else
        {
            state_id = eStateRest;
        }
    }

    select_state(state_id);
    get_state_current()->execute();
    prev_substate = current_substate;
}
