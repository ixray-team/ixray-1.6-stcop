////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_search_planner.h
//	Created 	: 03.10.2007
//  Modified 	: 03.10.2007
//	Author		: Dmitriy Iassenev
//	Description : Stalker search planner
////////////////////////////////////////////////////////////////////////////

#ifndef STALKER_SEARCH_PLANNER_H_INCLUDED
#define STALKER_SEARCH_PLANNER_H_INCLUDED

#include "action_planner_action_script.h"

class CAI_Stalker;

class CStalkerSearchPlanner : public CActionPlannerActionScript<CAI_Stalker> {
private:
	typedef CActionPlannerActionScript<CAI_Stalker>	inherited;

private:
			void	add_evaluators			();
			void	add_actions				();

public:
					CStalkerSearchPlanner	(CAI_Stalker *object = 0, const char* action_name = "");
	virtual void	setup					(CAI_Stalker *object, CPropertyStorage *storage) override;
	virtual void	update					() override;
	virtual void	initialize				() override;
	virtual void	finalize				() override;
};

#endif // STALKER_SEARCH_PLANNER_H_INCLUDED