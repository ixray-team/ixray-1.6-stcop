#pragma once
#include "../monster_state_manager.h"

class CAI_Bloodsucker;

class CStateManagerBloodsucker : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

public:
					CStateManagerBloodsucker	(CAI_Bloodsucker *monster); 
	virtual void	execute						();
	virtual void	update						();
			void	drag_object					();
	virtual void	remove_links				(CObject* object_) { inherited::remove_links(object_);}
			bool	check_vampire				();
};
