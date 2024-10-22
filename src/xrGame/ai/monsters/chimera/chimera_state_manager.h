#pragma once
#include "../monster_state_manager.h"

class CChimeraBase;

class CChimeraBaseStateManager : public CMonsterStateManager
{
protected:
	using	inherited =			CMonsterStateManager;

public:
	CChimeraBaseStateManager(CChimeraBase* object);
	virtual				~CChimeraBaseStateManager() override;

	virtual	void		execute					() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object); }
};
