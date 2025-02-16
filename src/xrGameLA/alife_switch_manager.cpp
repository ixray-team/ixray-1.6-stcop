////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_switch_manager.cpp
//	Created 	: 25.12.2002
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator switch manager
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "alife_switch_manager.h"
#include "xrServer_Objects_ALife.h"
#include "alife_graph_registry.h"
#include "alife_object_registry.h"
#include "alife_schedule_registry.h"
#include "game_level_cross_table.h"
#include "xrserver.h"
#include "ai_space.h"
#include "level_graph.h"

#ifdef DEBUG
#	include "level.h"
#endif // DEBUG

using namespace ALife;

struct remove_non_savable_predicate {
	xrServer			*m_server;

	IC		 remove_non_savable_predicate(xrServer *server)
	{
		VERIFY			(server);
		m_server		= server;
	}

	IC	bool operator()	(const ALife::_OBJECT_ID &id) const
	{
		CSE_Abstract	*object = m_server->game->get_entity_from_eid(id);
		VERIFY			(object);
		CSE_ALifeObject	*alife_object = smart_cast<CSE_ALifeObject*>(object);
		VERIFY			(alife_object);
		return			(!alife_object->can_save());
	}
};

CALifeSwitchManager::~CALifeSwitchManager	()
{
}

void CALifeSwitchManager::add_online(CSE_ALifeDynamicObject *object, bool update_registries)
{
	START_PROFILE("ALife/switch/add_online")
	VERIFY							((ai().game_graph().vertex(object->m_tGraphID)->level_id() == graph().level().level_id()));

	object->m_bOnline				= true;

	NET_Packet						tNetPacket;
	CSE_Abstract					*l_tpAbstract = smart_cast<CSE_Abstract*>(object);
	server().entity_Destroy			(l_tpAbstract);
	object->s_flags.bor				(M_SPAWN_UPDATE);
	ClientID						clientID;
	clientID.set					(server().GetServerClient() ? server().GetServerClient()->ID.value() : 0);
	server().Process_spawn			(tNetPacket,clientID,FALSE,l_tpAbstract);
	object->s_flags.band				(u16(-1) ^ M_SPAWN_UPDATE);
	R_ASSERT3						(!object->used_ai_locations() || ai().level_graph().valid_vertex_id(object->m_tNodeID),"Invalid vertex for object ",object->name_replace());

#ifdef DEBUG
	if (psAI_Flags.test(aiALife))
		Msg							("[LSS] Spawning object [%s][%s][%d]",object->name_replace(),*object->s_name,object->ID);
#endif

	object->add_online				(update_registries);
	STOP_PROFILE
}

void CALifeSwitchManager::remove_online(CSE_ALifeDynamicObject *object, bool update_registries)
{
	START_PROFILE("ALife/switch/remove_online")
	object->m_bOnline			= false;
	
	m_saved_chidren				= object->children;
	CSE_ALifeTraderAbstract		*inventory_owner = smart_cast<CSE_ALifeTraderAbstract*>(object);
	if (inventory_owner) {
		m_saved_chidren.erase	(
			std::remove_if(
				m_saved_chidren.begin(),
				m_saved_chidren.end(),
				remove_non_savable_predicate(&server())
			),
			m_saved_chidren.end()
		);
	}

	server().Perform_destroy	(object,net_flags(TRUE,TRUE));
	VERIFY						(object->children.empty());

	_OBJECT_ID					object_id = object->ID;
	object->ID					= server().PerformIDgen(object_id);

#ifdef DEBUG
	if (psAI_Flags.test(aiALife))
		Msg						("[LSS] Destroying object [%s][%s][%d]",object->name_replace(),*object->s_name,object->ID);
#endif

	object->add_offline			(m_saved_chidren,update_registries);
	STOP_PROFILE
}

void CALifeSwitchManager::switch_online(CSE_ALifeDynamicObject *object)
{
	START_PROFILE("ALife/switch/switch_online")
#ifdef DEBUG
	if (psAI_Flags.test(aiALife))
		Msg						("[LSS][%d] Going online [%d][%s][%d] ([%f][%f][%f] : [%f][%f][%f]), on '%s'",Device.dwFrame,Device.dwTimeGlobal,object->name_replace(), object->ID,VPUSH(graph().actor()->o_Position),VPUSH(object->o_Position), "*SERVER*");
#endif
	object->switch_online		();
	STOP_PROFILE
}

void CALifeSwitchManager::switch_offline(CSE_ALifeDynamicObject *object)
{
	START_PROFILE("ALife/switch/switch_offline")
#ifdef DEBUG
	if (psAI_Flags.test(aiALife))
		Msg							("[LSS][%d] Going offline [%d][%s][%d] ([%f][%f][%f] : [%f][%f][%f]), on '%s'",Device.dwFrame,Device.dwTimeGlobal,object->name_replace(), object->ID,VPUSH(graph().actor()->o_Position),VPUSH(object->o_Position), "*SERVER*");
#endif
	object->switch_offline			();
	STOP_PROFILE
}

bool CALifeSwitchManager::synchronize_location(CSE_ALifeDynamicObject *I)
{
	START_PROFILE("ALife/switch/synchronize_location")
#ifdef DEBUG
	VERIFY3					(ai().level_graph().level_id() == ai().game_graph().vertex(I->m_tGraphID)->level_id(),*I->s_name,I->name_replace());
	if (!I->children.empty()) {
		u32					size = I->children.size();
		ALife::_OBJECT_ID	*test = (ALife::_OBJECT_ID*)_alloca(size*sizeof(ALife::_OBJECT_ID));
		Memory.mem_copy		(test,&*I->children.begin(),size*sizeof(ALife::_OBJECT_ID));
		std::sort			(test,test + size);
		for (u32 i=1; i<size; ++i) {
			VERIFY3			(test[i - 1] != test[i],"Child is registered twice in the child list",(*I).name_replace());
		}
	}
#endif // DEBUG

	// check if we do not use ai locations
	if (!I->used_ai_locations())
		return				(true);

	// check if we are not attached
	if (0xffff != I->ID_Parent)
		return				(true);

	// check if we are not online and have an invalid level vertex id
	if	(!I->m_bOnline && !ai().level_graph().valid_vertex_id(I->m_tNodeID))
		return				(true);

	return					((*I).synchronize_location());
	STOP_PROFILE
}

void CALifeSwitchManager::try_switch_online	(CSE_ALifeDynamicObject	*I)
{
	START_PROFILE("ALife/switch/try_switch_online")
	// so, the object is offline
	// checking if the object is not attached
	if (0xffff != I->ID_Parent) {
		// so, object is attached

#ifdef DEBUG
		if (psAI_Flags.test(aiALife)) {
			CSE_ALifeCreatureAbstract	*l_tpALifeCreatureAbstract = smart_cast<CSE_ALifeCreatureAbstract*>(objects().object(I->ID_Parent));

			Fvector tempposition = { 0.f, 0.f, 0.f };
			if (l_tpALifeCreatureAbstract)
			{
				tempposition = l_tpALifeCreatureAbstract->o_Position;
			}

		VERIFY2(!l_tpALifeCreatureAbstract || (l_tpALifeCreatureAbstract->fHealth >= EPS_L), make_string<const char*>("Variable l_tpALifeCreatureAbstract is not null OR Health of alifecreture is less than %f. Health = %f, pos = [x %f, y %f, z %f]",
			EPS_L, l_tpALifeCreatureAbstract->fHealth, tempposition.x, tempposition.y, tempposition.z));
	}

		// checking if parent is offline too
		Fvector tempposition = { 0.f, 0.f, 0.f };
		Fvector tempposition2 = { 0.f, 0.f, 0.f };
		if (objects().object(I->ID_Parent))
		{
			tempposition = objects().object(I->ID_Parent)->o_Position;
		}

		if (objects().object(I->ID))
		{
			tempposition2 = objects().object(I->ID)->o_Position;
		}

		VERIFY2
		(
			!objects().object(I->ID_Parent)->m_bOnline, make_string<const char*>("Parent online, item offline... ID = [%d] ID_Parent = [%d] NameParent = [%s], Name = [%s], posParent = [x %f, y %f, z %f], pos = [x %f, y %f, z %f]",
			I->ID, I->ID_Parent, objects().object(I->ID_Parent)->name_replace(), objects().object(I->ID)->name_replace(), tempposition.x, tempposition.y, tempposition.z, tempposition2.x, tempposition2.y, tempposition2.z)
		);
#endif
		return;
	}

	VERIFY2
	(
		(ai().game_graph().vertex(I->m_tGraphID)->level_id()!= ai().level_graph().level_id()) 
		||
		!Level().Objects.net_Find(I->ID)
		||
		Level().Objects.dump_all_objects(),
		make_string<const char*>("frame [%d] time [%d] object [%s] with id [%d] is offline, but is on the level",Device.dwFrame,Device.dwTimeGlobal,I->name_replace(),I->ID)
	);

	I->try_switch_online		();

	if (!I->m_bOnline && !I->keep_saved_data_anyway())
		I->client_data.clear	();

	STOP_PROFILE
}

void CALifeSwitchManager::try_switch_offline(CSE_ALifeDynamicObject	*I)
{
	START_PROFILE("ALife/switch/try_switch_offline")
	// checking if the object is not attached
	if (0xffff != I->ID_Parent) {
#ifdef DEBUG

		CSE_ALifeCreatureAbstract	*l_tpALifeCreatureAbstract = smart_cast<CSE_ALifeCreatureAbstract*>(objects().object(I->ID_Parent));

		/*Fvector tempposition3 = { 0.f, 0.f, 0.f };
		if (l_tpALifeCreatureAbstract)
		{
			tempposition3 = l_tpALifeCreatureAbstract->o_Position;
		}

		VERIFY2(!l_tpALifeCreatureAbstract || (l_tpALifeCreatureAbstract->fHealth >= EPS_L), make_string("Variable l_tpALifeCreatureAbstract is not null _OR_ Health of alifecreture is less than %f. Health = %f, pos = [x %f, y %f, z %f]",
			EPS_L, l_tpALifeCreatureAbstract->fHealth, tempposition3.x, tempposition3.y, tempposition3.z));*/

		// checking if parent is online too
		Fvector tempposition = { 0.f, 0.f, 0.f };
		Fvector tempposition2 = { 0.f, 0.f, 0.f };
		if (objects().object(I->ID_Parent))
		{
			tempposition = objects().object(I->ID_Parent)->o_Position;
		}

		if (objects().object(I->ID))
		{
			tempposition2 = objects().object(I->ID)->o_Position;
		}

		VERIFY2
		(
			objects().object(I->ID_Parent)->m_bOnline, make_string<const char*>("Parent offline, item online... ID = [%d] ID_Parent = [%d] NameParent = [%s], Name = [%s], posParent = [x %f, y %f, z %f], pos = [x %f, y %f, z %f]",
			I->ID, I->ID_Parent, objects().object(I->ID_Parent)->name_replace(), objects().object(I->ID)->name_replace(), tempposition.x, tempposition.y, tempposition.z, tempposition2.x, tempposition2.y, tempposition2.z)
		);
#endif
		return;
	}

	I->try_switch_offline	();
	STOP_PROFILE
}

void CALifeSwitchManager::switch_object	(CSE_ALifeDynamicObject	*I)
{
	if (I->redundant()) {
		release				(I);
		return;
	}

	if (!synchronize_location(I))
		return;

	if (I->m_bOnline)
		try_switch_offline	(I);
	else
		try_switch_online	(I);

	//Msg("! Engine stacktrace:");
	//Debug.log_stack_trace();

	if (I->redundant())
		release				(I);
}
