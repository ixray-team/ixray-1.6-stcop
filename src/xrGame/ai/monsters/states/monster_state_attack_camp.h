#pragma once

#include "../state.h"

class CStateMonsterAttackCamp : public CState {
    typedef CState inherited;
    typedef CState* state_ptr;

    u32 m_target_node;
public:
    CStateMonsterAttackCamp(CBaseMonster* obj);

    virtual void initialize();
    virtual void finalize();
    virtual void critical_finalize();
    virtual void remove_links(CObject* object_) { inherited::remove_links(object_); }

    virtual bool check_completion();
    virtual bool check_start_conditions();

    virtual void check_force_state();
    virtual void reselect_state();
    virtual void setup_substates();
};
