#include "stdafx.h"

#include "FRbmkAgentManagerPlanner.h"

#include "agent_corpse_manager.h"
#include "agent_enemy_manager.h"
#include "agent_explosive_manager.h"
#include "agent_location_manager.h"
#include "agent_manager.h"
#include "agent_member_manager.h"
#include "danger_manager.h"
#include "item_manager.h"
#include "member_order.h"
#include "memory_manager.h"
#include "ai/stalker/ai_stalker.h"

FRbmkAgentManagerPlanner::FRbmkAgentManagerPlanner(CAgentManager* InOwner):Owner(InOwner)
{
}

FRbmkAgentManagerPlanner::~FRbmkAgentManagerPlanner()
{
}

void FRbmkAgentManagerPlanner::Update()
{
	if(IsEnemySelected())
	{
		RefreshState(0);
	}
	else if(IsDangerSelected())
	{
		RefreshState(1);
	}
	else if(IsItemSelected())
	{
		RefreshState(2);
	}
	else
	{
		RefreshState(3);
	}
}

bool FRbmkAgentManagerPlanner::IsItemSelected() const
{
	for (CMemberOrder* Members :Owner->member().members())
	{
		if (Members->object().memory().item().selected())
		{
			return true;
		}
	}
	return false;
}

bool FRbmkAgentManagerPlanner::IsEnemySelected() const
{
	for (CMemberOrder* Members :Owner->member().combat_members())
	{
		if (Members->object().memory().enemy().selected())
		{
			return true;
		}
	}
	return false;
}

bool FRbmkAgentManagerPlanner::IsDangerSelected() const
{
	for (CMemberOrder* Members :Owner->member().members())
	{
		if (Members->object().memory().danger().selected())
		{
			return true;
		}
	}
	return false;
}

void FRbmkAgentManagerPlanner::RefreshState(u32 NewState)
{
	if(CurrentState == NewState)
	{
		if(CurrentState == 0)
		{
			Owner->enemy().distribute_enemies();
			Owner->explosive().react_on_explosives();
			Owner->corpse().react_on_member_death();
		}
		else if(CurrentState == 1)
		{
			Owner->explosive().react_on_explosives();
			Owner->corpse().react_on_member_death();
		}
		return;
	}
	
	if(CurrentState == 3)
	{
		Owner->corpse().clear();
	}
	
	if(NewState == 0 || NewState == 1)
	{
		Owner->location().clear();
	}
	
	CurrentState = NewState;
}
