////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_stalker_impl.h
//	Created 	: 25.02.2003
//  Modified 	: 25.02.2003
//	Author		: Dmitriy Iassenev
//	Description : AI Behaviour for monster "Stalker" (inline functions implementation)
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../level.h"
#include "../../seniority_hierarchy_holder.h"
#include "../../team_hierarchy_holder.h"
#include "../../squad_hierarchy_holder.h"
#include "../../group_hierarchy_holder.h"
#include "../../effectorshot.h"
 
IC	CAgentManager &CAI_Stalker::agent_manager	() const
{
//  gr1ph to all: this method is called on update, it d be better to set a callback in order to set the agent manager and access it directly
//	return			(Level().seniority_holder().team(g_Team()).squad(g_Squad()).group(g_Group()).agent_manager());
	VERIFY2(m_agent_manager, make_string<const char*>("no agent manager for %s -> %d|%d|%d", *cName(), g_Team(), g_Squad(), g_Group()));
	return *m_agent_manager;
}

IC	Fvector CAI_Stalker::weapon_shot_effector_direction	(const Fvector &current) const
{
	VERIFY			(weapon_shot_effector().IsActive());
	Fvector			result;
	weapon_shot_effector().GetDeltaAngle(result);

	float			y,p;
	current.getHP	(y,p);

	result.setHP	(-result.y + y, -result.x + p);

	return			(result);
}
