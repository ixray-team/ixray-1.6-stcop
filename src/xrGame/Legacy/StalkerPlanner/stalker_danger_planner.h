////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_danger_planner.h
//	Created 	: 11.02.2005
//  Modified 	: 11.02.2005
//	Author		: Dmitriy Iassenev
//	Description : Stalker danger planner
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "action_planner_action_script.h"

class CAI_Stalker;

class CStalkerDangerPlanner : public CActionPlannerActionScript<CAI_Stalker> {
private:
	typedef CActionPlannerActionScript<CAI_Stalker> inherited;

protected:
			void		add_evaluators			();
			void		add_actions				();

public:
						CStalkerDangerPlanner	(CAI_Stalker *object = 0, const char* action_name = "");
	virtual void		setup					(CAI_Stalker *object, CPropertyStorage *storage) override;
	virtual void		initialize				() override;
	virtual void		update					() override;
	virtual void		finalize				() override;
};

#include "stalker_danger_planner_inline.h"