#include "StdAfx.h"
#include "stalker_danger_by_burning_planner.h"
#include "ai/stalker/ai_stalker.h"
#include "script_game_object.h"
#include "stalker_danger_by_burning_actions.h"
#include "stalker_danger_grenade_actions.h"
#include "stalker_decision_space.h"
#include "stalker_danger_property_evaluators.h"
#include "stalker_property_evaluators.h"

using namespace StalkerDecisionSpace;

CStalkerDangerByBurningPlanner::CStalkerDangerByBurningPlanner	(CAI_Stalker *object, const char* action_name) :
	inherited				(object,action_name)
{
}

void CStalkerDangerByBurningPlanner::setup					(CAI_Stalker *object, CPropertyStorage *storage)
{
	inherited::setup		(object,storage);
	clear					();
	add_evaluators			();
	add_actions				();
}

void CStalkerDangerByBurningPlanner::initialize				()
{
	inherited::initialize	();
	
	CScriptActionPlanner::m_storage.set_property(eWorldPropertyCoverReached,false);
	CScriptActionPlanner::m_storage.set_property(eWorldPropertyLookedAround,false);
}

void CStalkerDangerByBurningPlanner::update					()
{
	inherited::update		();
}

void CStalkerDangerByBurningPlanner::finalize					()
{
	inherited::finalize		();
}

void CStalkerDangerByBurningPlanner::add_evaluators			()
{
	add_evaluator(eWorldPropertyDanger ,new CStalkerPropertyEvaluatorDangers(m_object,"danger"));
}

void CStalkerDangerByBurningPlanner::add_actions				()
{
	CStalkerActionBase		*action;

	action = new CStalkerActionDangerByBurningRunAway(m_object,"run away");
	add_effect(action, eWorldPropertyDanger, false);
	add_operator(eWorldOperatorDangerBurningRunAway, action);
	/*action					= new CStalkerActionDangerGrenadeTakeCover(m_object,"take cover");
	add_effect				(action,eWorldPropertyCoverActual,		true);
	add_effect				(action,eWorldPropertyCoverReached,		true);
	add_operator			(eWorldOperatorDangerGrenadeTakeCover,	action);

	action					= new CStalkerActionDangerGrenadeWaitForExplosion(m_object,"wait for explosion");
	add_condition			(action,eWorldPropertyCoverActual,		true);
	add_condition			(action,eWorldPropertyCoverReached,		true);
	add_condition			(action,eWorldPropertyGrenadeExploded,	false);
	add_effect				(action,eWorldPropertyGrenadeExploded,	true);
	add_operator			(eWorldOperatorDangerGrenadeWaitForExplosion,	action);

	action					= new CStalkerActionDangerGrenadeTakeCoverAfterExplosion(m_object,"take cover after explosion");
	add_condition			(action,eWorldPropertyGrenadeExploded,	true);
	add_effect				(action,eWorldPropertyCoverActual,		true);
	add_effect				(action,eWorldPropertyCoverReached,		true);
	add_operator			(eWorldOperatorDangerGrenadeTakeCoverAfterExplosion,	action);

	action					= new CStalkerActionDangerGrenadeLookAround(m_object,"look around");
	add_condition			(action,eWorldPropertyGrenadeExploded,	true);
	add_condition			(action,eWorldPropertyCoverActual,		true);
	add_condition			(action,eWorldPropertyCoverReached,		true);
	add_condition			(action,eWorldPropertyLookedAround,		false);
	add_effect				(action,eWorldPropertyLookedAround,		true);
	add_operator			(eWorldOperatorDangerGrenadeLookAround,	action);

	action					= new CStalkerActionDangerGrenadeSearch(m_object,"search");
	add_condition			(action,eWorldPropertyGrenadeExploded,	true);
	add_condition			(action,eWorldPropertyCoverActual,		true);
	add_condition			(action,eWorldPropertyCoverReached,		true);
	add_condition			(action,eWorldPropertyLookedAround,		true);
	add_effect				(action,eWorldPropertyDanger,			false);
	add_operator			(eWorldOperatorDangerGrenadeSearch,		action);*/
}
