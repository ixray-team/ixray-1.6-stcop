///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../state.h"

template <typename _Object>
class CStateBloodsuckerIXVampire : 
    public CState<_Object>
{
    using inherited = CState<_Object>;
    using state_ptr = CState<_Object>*;

    const CEntityAlive* enemy;

public:
    CStateBloodsuckerIXVampire(_Object* obj);

    virtual void reinit();

    virtual void initialize();
    virtual void reselect_state();
    virtual void finalize();
    virtual void critical_finalize();
    virtual bool check_start_conditions();
    virtual bool check_completion();
    virtual void remove_links(CObject* object);

    virtual void setup_substates();
    virtual void check_force_state();
};

#include "bloodsucker_ix_vampire_inline.h"
