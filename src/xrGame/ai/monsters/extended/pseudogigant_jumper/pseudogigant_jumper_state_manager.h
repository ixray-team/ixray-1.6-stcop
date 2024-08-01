///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Псевдогигант
//	Мутант: Прыгающий псевдогигант
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../pseudogigant/pseudogigant_state_manager.h"

class CStateManagerPseudogigantJumper final : 
    public CStateManagerPseudogigantBase<CPseudogigantJumper> 
{
    using inherited = CStateManagerPseudogigantBase<CPseudogigantJumper>;

public:
    CStateManagerPseudogigantJumper(CPseudogigantJumper* _object) : inherited(_object) {}
    virtual ~CStateManagerPseudogigantJumper() {}
};