#pragma once
#include "../monster_state_manager.h"

class CAI_Flesh;

class CStateManagerFlesh : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

public:

					CStateManagerFlesh	(CAI_Flesh *monster); 
	virtual void	execute				();
	virtual void	remove_links		(CObject* object_) { inherited::remove_links(object_);}
};
