#pragma once

#include "action_planner_action_script.h"

class CAI_Stalker;

class CStalkerDangerByBurningPlanner : public CActionPlannerActionScript<CAI_Stalker> {
private:
	using inherited = CActionPlannerActionScript<CAI_Stalker>;

protected:
			void		add_evaluators					();
			void		add_actions						();

public:
						CStalkerDangerByBurningPlanner	(CAI_Stalker *object = nullptr, const char* action_name = "");
	virtual void		setup							(CAI_Stalker *object, CPropertyStorage *storage) override;
	virtual void		initialize						() override;
	virtual void		update							() override;
	virtual void		finalize						() override;
};
