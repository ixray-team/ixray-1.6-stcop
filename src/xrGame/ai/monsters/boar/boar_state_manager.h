#pragma once
#include "../monster_state_manager.h"

class CAI_Boar;

class CStateManagerBoar : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

public:

					CStateManagerBoar	(CAI_Boar *monster); 

	virtual void	execute				();
	virtual void	remove_links		(CObject* object_) { inherited::remove_links(object_);}
};
