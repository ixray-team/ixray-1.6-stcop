#pragma once

#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "../states/monster_state_rest.h"
#include "../states/monster_state_attack.h"
#include "../states/monster_state_panic.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hear_danger_sound.h"
#include "../states/monster_state_hitted.h"
#include "../../../entitycondition.h"
#include "../states/monster_state_controlled.h"
#include "../states/monster_state_help_sound.h"

#include "../monster_state_manager.h"

namespace StateManagerPseudogigantBase
{
    template <typename Object>
    class CStateManagerPseudogigantBase : public CMonsterStateManager<Object>
    {
        using inherited = CMonsterStateManager<Object>;

    public:
        CStateManagerPseudogigantBase(Object* _object) : inherited(_object)
        {
            this->add_state(eStateRest, new CStateMonsterRest<Object>(_object));
            this->add_state(eStatePanic, new CStateMonsterPanic<Object>(_object));
            this->add_state(eStateAttack, new CStateMonsterAttack<Object>(_object));
            this->add_state(eStateEat, new CStateMonsterEat<Object>(_object));
            this->add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound<Object>(_object));
            this->add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound<Object>(_object));
            this->add_state(eStateHitted, new CStateMonsterHitted<Object>(_object));
            this->add_state(eStateControlled, new CStateMonsterControlled<Object>(_object));
            this->add_state(eStateHearHelpSound, new CStateMonsterHearHelpSound<Object>(_object));
        }

        virtual void execute() override
        {
            u32 state_id = u32(-1);

            if (!this->object->is_under_control())
            {
                const CEntityAlive* enemy = this->object->EnemyMan.get_enemy();

                if (enemy)
                {
                    switch (this->object->EnemyMan.get_danger_type())
                    {
                    case eStrong: state_id = eStatePanic; break;
                    case eWeak: state_id = eStateAttack; break;
                    }

                }
                else if (this->object->HitMemory.is_hit())
                {
                    state_id = eStateHitted;
                }
                else if (this->check_state(eStateHearHelpSound))
                {
                    state_id = eStateHearHelpSound;
                }
                else if (this->object->hear_interesting_sound)
                {
                    state_id = eStateHearInterestingSound;
                }
                else if (this->object->hear_dangerous_sound)
                {
                    state_id = eStateHearDangerousSound;
                }
                else
                {
                    if (this->can_eat())
                        state_id = eStateEat;
                    else
                        state_id = eStateRest;
                }
            }
            else
            {
                state_id = eStateControlled;
            }

            this->select_state(state_id);
            this->get_state_current()->execute();
            this->prev_substate = this->current_substate;
        }

        virtual void remove_links(CObject* _object) override
        {
            inherited::remove_links(_object);
        }
    };
}