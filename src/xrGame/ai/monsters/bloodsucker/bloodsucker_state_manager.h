#pragma once
#include "../monster_state_manager.h"

class CAI_Bloodsucker;

class CStateManagerBloodsucker : public CMonsterStateManager<CAI_Bloodsucker> {
	typedef CMonsterStateManager<CAI_Bloodsucker> inherited;

public:
					CStateManagerBloodsucker	(CAI_Bloodsucker *monster);
	virtual void	execute						() override;
	virtual void	update						() override;
			void	drag_object					();
	virtual void	remove_links				(CObject* object_) override { inherited::remove_links(object_);}
			bool	check_vampire				();
};
