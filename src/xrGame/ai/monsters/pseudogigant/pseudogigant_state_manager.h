#pragma once
#include "../monster_state_manager.h"

class CPseudogigant;

class CStateManagerPseudogigant : public CMonsterStateManager<CPseudogigant> {
	typedef CMonsterStateManager<CPseudogigant> inherited;
public:

					CStateManagerPseudogigant(CPseudogigant*monster);
					virtual ~CStateManagerPseudogigant();

	virtual void	execute				();
	virtual void	remove_links		(CObject* object_) { inherited::remove_links(object_);}
};
