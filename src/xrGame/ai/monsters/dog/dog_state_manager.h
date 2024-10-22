#pragma once
#include "../monster_state_manager.h"

class CDogBase;

class CDogBaseStateManager : public CMonsterStateManager {
protected:
	using inherited = CMonsterStateManager;

	CDogBase* pDogBase;

public:
	CDogBaseStateManager(CDogBase* object);
					virtual ~CDogBaseStateManager() override;

	virtual void	execute				() override;
			bool	check_eat			();
	virtual void	remove_links		(CObject* object) override { inherited::remove_links(object);}
};
