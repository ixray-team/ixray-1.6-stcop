///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../state.h"

template <typename _Object>
class CStateBloodsuckerIXVampireHide : 
    public CState<_Object>
{
    using inherited = CState<_Object>;
    using state_ptr = CState<_Object>*;

public:
    CStateBloodsuckerIXVampireHide(_Object* obj);

    virtual void reselect_state();
    virtual void setup_substates();
    virtual bool check_completion();
    virtual void remove_links(CObject* object) { inherited::remove_links(object); }
};

#include "bloodsucker_ix_vampire_hide_inline.h"
