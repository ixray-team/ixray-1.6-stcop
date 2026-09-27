#include "StdAfx.h"
#include "stalker_danger_by_burning_actions.h"

#include "ai_object_location.h"
#include "ai/stalker/ai_stalker.h"
#include "script_game_object.h"
#include "stalker_movement_manager_smart_cover.h"
#include "sight_manager.h"
#include "movement_manager_space.h"
#include "detail_path_manager_space.h"
#include "memory_manager.h"
#include "stalker_decision_space.h"

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundListenTo
//////////////////////////////////////////////////////////////////////////

CStalkerActionDangerByBurningRunAway::CStalkerActionDangerByBurningRunAway(CAI_Stalker *object, const char* action_name) :
	inherited(object,action_name)
{
}

void CStalkerActionDangerByBurningRunAway::initialize						()
{
	inherited::initialize	();
	//object().movement().set_desired_direction	(0);
	object().movement().set_path_type			(MovementManager::ePathTypeLevelPath);
	object().movement().set_detail_path_type	(DetailPathManager::eDetailPathTypeSmooth);
	object().movement().set_nearest_accessible_position();
	object().movement().set_body_state			(eBodyStateStand);
	object().movement().set_movement_type		(eMovementTypeStand);
	object().movement().set_mental_state(eMentalStatePanic);
	//object().movement().set_movement_type		(eMovementTypeRun);
	//object().movement().set_mental_state		(eMentalStatePanic);
	//object().sight().setup						(SightManager::eSightTypeCurrentDirection);
	//object().CObjectHandler::set_goal			(eObjectActionIdle);
}

void CStalkerActionDangerByBurningRunAway::execute()
{
	inherited::execute();
	
	if (!object().memory().danger().selected())
	{
		return;
	}
	
	/*auto NPCPos = object().Position();
	auto RandomDev = Fvector(Random.randF(-5.0f, 5.0f), 0.0f, Random.randF(-5.0f, 5.0f));
	Fvector Result = NPCPos + RandomDev;
	//auto lv = object().movement().restrictions().accessible_nearest(NPCPos + RandomDev, Result);
	object().movement().set_level_dest_vertex(ai().level_graph().vertex_id(Result));
	object().movement().set_desired_position(&Result);
	//object().movement().set_nearest_accessible_position(object().Position() + Fvector(Random.randF(-5.0f, 5.0f), 0.0f, Random.randF(-5.0f, 5.0f)), object().ai_location().level_vertex_id());
	
	if (!object().movement().path_completed()) {
		//object().CObjectHandler::set_goal(eObjectActionIdle);
		object().movement().set_mental_state(eMentalStatePanic);
		object().sight().setup(CSightAction(SightManager::eSightTypePathDirection, true, true));
		return;
	}*/
	
	set_property(StalkerDecisionSpace::eWorldPropertyDanger,false);
}

void CStalkerActionDangerByBurningRunAway::finalize()
{
	inherited::finalize();
}

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundCheck
//////////////////////////////////////////////////////////////////////////

/*CStalkerActionDangerBySoundCheck::CStalkerActionDangerBySoundCheck	(CAI_Stalker *object, const char* action_name) :
	inherited				(object,action_name)
{
}

void CStalkerActionDangerBySoundCheck::initialize						()
{
	inherited::initialize	();
	object().movement().set_desired_direction	(0);
	object().movement().set_path_type			(MovementManager::ePathTypeLevelPath);
	object().movement().set_detail_path_type	(DetailPathManager::eDetailPathTypeSmooth);
	object().movement().set_nearest_accessible_position();
	object().movement().set_body_state			(eBodyStateStand);
	object().movement().set_movement_type		(eMovementTypeStand);
	object().movement().set_mental_state		(eMentalStateDanger);
	object().sight().setup						(SightManager::eSightTypeCurrentDirection);
	object().CObjectHandler::set_goal			(eObjectActionIdle);
}

void CStalkerActionDangerBySoundCheck::execute							()
{
	inherited::execute		();
}

void CStalkerActionDangerBySoundCheck::finalize							()
{
	inherited::finalize		();
}

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundTakeCover
//////////////////////////////////////////////////////////////////////////

CStalkerActionDangerBySoundTakeCover::CStalkerActionDangerBySoundTakeCover	(CAI_Stalker *object, const char* action_name) :
	inherited				(object,action_name)
{
}

void CStalkerActionDangerBySoundTakeCover::initialize						()
{
	inherited::initialize	();
	object().movement().set_desired_direction	(0);
	object().movement().set_path_type			(MovementManager::ePathTypeLevelPath);
	object().movement().set_detail_path_type	(DetailPathManager::eDetailPathTypeSmooth);
	object().movement().set_nearest_accessible_position();
	object().movement().set_body_state			(eBodyStateStand);
	object().movement().set_movement_type		(eMovementTypeStand);
	object().movement().set_mental_state		(eMentalStateDanger);
	object().sight().setup						(SightManager::eSightTypeCurrentDirection);
	object().CObjectHandler::set_goal			(eObjectActionIdle);
}

void CStalkerActionDangerBySoundTakeCover::execute							()
{
	inherited::execute		();
}

void CStalkerActionDangerBySoundTakeCover::finalize							()
{
	inherited::finalize		();
}

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundLookOut
//////////////////////////////////////////////////////////////////////////

CStalkerActionDangerBySoundLookOut::CStalkerActionDangerBySoundLookOut	(CAI_Stalker *object, const char* action_name) :
	inherited				(object,action_name)
{
}

void CStalkerActionDangerBySoundLookOut::initialize						()
{
	inherited::initialize	();
	object().movement().set_desired_direction	(0);
	object().movement().set_path_type			(MovementManager::ePathTypeLevelPath);
	object().movement().set_detail_path_type	(DetailPathManager::eDetailPathTypeSmooth);
	object().movement().set_nearest_accessible_position();
	object().movement().set_body_state			(eBodyStateStand);
	object().movement().set_movement_type		(eMovementTypeStand);
	object().movement().set_mental_state		(eMentalStateDanger);
	object().sight().setup						(SightManager::eSightTypeCurrentDirection);
	object().CObjectHandler::set_goal			(eObjectActionIdle);
}

void CStalkerActionDangerBySoundLookOut::execute							()
{
	inherited::execute		();
}

void CStalkerActionDangerBySoundLookOut::finalize							()
{
	inherited::finalize		();
}

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundLookAround
//////////////////////////////////////////////////////////////////////////

CStalkerActionDangerBySoundLookAround::CStalkerActionDangerBySoundLookAround	(CAI_Stalker *object, const char* action_name) :
	inherited				(object,action_name)
{
}

void CStalkerActionDangerBySoundLookAround::initialize						()
{
	inherited::initialize	();
	object().movement().set_desired_direction	(0);
	object().movement().set_path_type			(MovementManager::ePathTypeLevelPath);
	object().movement().set_detail_path_type	(DetailPathManager::eDetailPathTypeSmooth);
	object().movement().set_nearest_accessible_position();
	object().movement().set_body_state			(eBodyStateStand);
	object().movement().set_movement_type		(eMovementTypeStand);
	object().movement().set_mental_state		(eMentalStateDanger);
	object().sight().setup						(SightManager::eSightTypeCurrentDirection);
	object().CObjectHandler::set_goal			(eObjectActionIdle);
}

void CStalkerActionDangerBySoundLookAround::execute							()
{
	inherited::execute		();
}

void CStalkerActionDangerBySoundLookAround::finalize							()
{
	inherited::finalize		();
}*/
