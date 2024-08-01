///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../state.h"

template <typename _Object>
class CStateBloodsuckerIXVampireApproach : 
    public CState<_Object>
{
    using inherited = CState<_Object>;

public:
    CStateBloodsuckerIXVampireApproach(_Object* obj);
    virtual ~CStateBloodsuckerIXVampireApproach();

    virtual void initialize();
    virtual void execute();
    virtual void remove_links(CObject* object) { inherited::remove_links(object); }
};

#include "bloodsucker_ix_vampire_approach_inline.h"
