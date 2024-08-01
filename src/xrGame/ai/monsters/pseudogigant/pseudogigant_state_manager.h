#pragma once
#include "pseudogigant_state_manager_template.h"

using namespace StateManagerPseudogigantBase;

class CStateManagerPseudogigant : public CStateManagerPseudogigantBase<CPseudogigant> 
{
    using inherited = CStateManagerPseudogigantBase<CPseudogigant>;

public:
    CStateManagerPseudogigant(CPseudogigant* monster) : inherited(monster) {}
    virtual ~CStateManagerPseudogigant() {}
};
