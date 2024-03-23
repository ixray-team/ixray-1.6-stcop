#pragma once
#include "../../../monsters/pseudogigant/pseudogigant.h"
#include "../../monster_state_manager.h"

class CPseudogigantJumper;

class CStateManagerPseudogigantJumper : public CMonsterStateManager<CPseudogigantJumper> {
	typedef CMonsterStateManager<CPseudogigantJumper> inherited;
public:

	CStateManagerPseudogigantJumper(CPseudogigantJumper* monster);
	virtual ~CStateManagerPseudogigantJumper();

	virtual void	execute();
	virtual void	remove_links(CObject* object_) { inherited::remove_links(object_); }
};
