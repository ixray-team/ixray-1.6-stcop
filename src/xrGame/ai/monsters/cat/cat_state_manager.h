#pragma once
#include "../monster_state_manager.h"

class CCatBase;

class CustomCatStateManager : public CMonsterStateManager {

	using inherited = CMonsterStateManager;

	u32					m_rot_jump_last_time;

public:
	CustomCatStateManager(CCatBase* object);
	virtual				~CustomCatStateManager();

	virtual	void		execute				();
	virtual void		remove_links		(CObject* object) { inherited::remove_links(object);}
};
