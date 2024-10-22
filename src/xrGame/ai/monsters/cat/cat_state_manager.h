#pragma once
#include "../monster_state_manager.h"

class CCatBase;

class CCatBaseStateManager : public CMonsterStateManager {
protected:
	using inherited = CMonsterStateManager;

	u32					m_rot_jump_last_time;

public:
	CCatBaseStateManager(CCatBase* object);
	virtual				~CCatBaseStateManager() override;

	virtual	void		execute				() override;
	virtual void		remove_links		(CObject* object) override { inherited::remove_links(object);}
};
