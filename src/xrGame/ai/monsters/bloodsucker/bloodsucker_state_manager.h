#pragma once
#include "../monster_state_manager.h"

class CustomBloodsuker;

class CustomBloodsukerStateManager : public CMonsterStateManager
{
	using inherited = CMonsterStateManager;

	CustomBloodsucker* m_pBloodsucker;

public:
	CustomBloodsukerStateManager(CustomBloodsucker*object);
	virtual ~CustomBloodsukerStateManager();

	virtual void	execute						();
	virtual void	update						();
			void	drag_object					();
	virtual void	remove_links				(CObject* object) { inherited::remove_links(object);}
			bool	check_vampire				();
};
