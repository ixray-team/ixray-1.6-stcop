#pragma once
#include "../monster_state_manager.h"

class CBurerBase;

class CBurerBaseBaseStateManager : public CMonsterStateManager {
protected:
	using inherited = CMonsterStateManager;

	CBurerBase* pBurerBase;

public:
	CBurerBaseBaseStateManager(CBaseMonster* object);
					virtual ~CBurerBaseBaseStateManager() override;

	virtual void	execute					() override;
	virtual void	setup_substates			() override;
	virtual void	remove_links			(CObject* object) override { inherited::remove_links(object);}
};

