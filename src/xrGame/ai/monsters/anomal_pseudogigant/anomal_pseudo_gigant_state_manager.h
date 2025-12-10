#pragma once
#include "../monster_state_manager.h"

class CAnomalPseudoGigant;

class CStateManagerAnomalPseudoGigant : public CMonsterStateManager<CAnomalPseudoGigant> {
	typedef CMonsterStateManager<CAnomalPseudoGigant> inherited;
public:
					CStateManagerAnomalPseudoGigant(CAnomalPseudoGigant* monster);
	void	execute					() override;
	void	setup_substates			() override;
	void	remove_links			(CObject* object) override { inherited::remove_links(object);}
};

