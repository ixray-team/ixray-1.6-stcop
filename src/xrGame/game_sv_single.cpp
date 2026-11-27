#include "StdAfx.h"
#include "game_sv_single.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "xrServer_Objects_ALife_Items.h"
#include "alife_simulator.h"
#include "alife_object_registry.h"
#include "alife_graph_registry.h"
#include "alife_time_manager.h"
#include "object_broker.h"
#include "GamePersistent.h"
#include "xrServer.h"
#include "../xrEngine/x_ray.h"
#include "ui/UILoadingScreen.h"
#include "Level.h"

game_sv_Single::game_sv_Single			()
{
	m_type						= eGameIDSingle;
};

game_sv_Single::~game_sv_Single			()
{
}

void	game_sv_Single::Create			(shared_str& options)
{
	inherited::Create					(options);

#ifndef NO_SINGLE
	if (strstr(*options,"/alife"))
		m_alife_simulator				= new CALifeSimulator(&server(),&options);
#endif //#ifndef NO_SINGLE

	switch_Phase						(GAME_PHASE_INPROGRESS);
}

void game_sv_Single::OnPlayerConnect(ClientID id_who)
{
	inherited::OnPlayerConnect(id_who);
	if (server().GetOwnerLevel() == nullptr || !server().GetOwnerLevel()->GetLevelDesc().RemoteTransport)
		return;

	xrClientData* client = server().ID_to_client(id_who);
	if (client == nullptr || client->owner != nullptr)
		return;

	const bool localHost = client->flags.bLocal || (server().GetServerClient() != nullptr && id_who == server().GetServerClient()->ID);
	if (localHost)
	{
		CSE_ALifeCreatureActor* actor = ai().get_alife() != nullptr ? ai().alife().graph().actor() : nullptr;
		if (actor == nullptr)
		{
			if (PendingRemotePlayers.size() < 64 && std::ranges::find(PendingRemotePlayers, id_who) == PendingRemotePlayers.end())
				PendingRemotePlayers.push_back(id_who);
			return;
		}
		client->owner = actor;
		if (client->ps != nullptr)
			client->ps->SetGameID(actor->ID);
		return;
	}

	const CSE_ALifeCreatureActor* referenceActor = ai().get_alife() != nullptr ? ai().alife().graph().actor() : nullptr;
	const CObject* referenceObject = Level().CurrentEntity();
	if (referenceActor == nullptr && referenceObject == nullptr)
	{
		if (PendingRemotePlayers.size() < 64 && std::ranges::find(PendingRemotePlayers, id_who) == PendingRemotePlayers.end())
		{
			PendingRemotePlayers.push_back(id_who);
			Msg("* Remote single-player client %u is waiting for the server actor", id_who.value());
		}
		return;
	}

	Fvector position = referenceActor != nullptr ? referenceActor->o_Position : referenceObject->Position();
	u32 levelVertex = referenceActor != nullptr ? referenceActor->m_tNodeID : ai().level_graph().vertex_id(position);
	GameGraph::_GRAPH_ID gameVertex = referenceActor != nullptr ? referenceActor->m_tGraphID : ai().cross_table().vertex(levelVertex).game_vertex_id();
	position.x += 2.f;
	if (ai().level_graph().valid_vertex_position(position))
		levelVertex = ai().level_graph().vertex_id(position);
	if (ai().get_cross_table() != nullptr && ai().level_graph().valid_vertex_id(levelVertex))
		gameVertex = ai().cross_table().vertex(levelVertex).game_vertex_id();

	CALifeSimulatorBase::SSpawnIdentity remoteIdentity;
	string64 remoteScriptStoryId;
	xr_sprintf(remoteScriptStoryId, "actor_remote_%u", id_who.value());
	remoteIdentity.ScriptStoryId = remoteScriptStoryId;
	CSE_Abstract* entity = ai().get_alife() != nullptr
		? GetAlifeSimulator()->spawn_item_online("actor", position, levelVertex, gameVertex, id_who, &remoteIdentity)
		: spawn_begin("actor");
	if (entity == nullptr)
	{
		Msg("! Remote single-player connection: actor spawn rejected for client %u", id_who.value());
		return;
	}
	entity->o_Position = position;
	if (CSE_ALifeDynamicObject* dynamic = entity->cast_alife_dynamic_object())
	{
		dynamic->m_tNodeID = levelVertex;
		dynamic->m_tGraphID = gameVertex;
		dynamic->m_script_story_ID = remoteScriptStoryId;
	}
	entity->set_name_replace(get_name_id(id_who));
	if (ai().get_alife() == nullptr)
	{
		entity->s_flags.assign(M_SPAWN_OBJECT_LOCAL | M_SPAWN_OBJECT_ASPLAYER);
		entity = spawn_end(entity, id_who);
	}
	if (entity == nullptr)
	{
		Msg("! Remote single-player connection: server rejected actor for client %u", id_who.value());
		return;
	}
	client->net_PassUpdates = true;
	if (client->ps != nullptr)
		client->ps->SetGameID(entity->ID);
	Msg("* Remote single-player client %u received actor %u (client %u, owner %u)", id_who.value(), entity->ID, client->ID.value(), client->owner != nullptr ? client->owner->ID : ALife::INVALID_OBJECT_ID);
}

/**
CSE_Abstract*		game_sv_Single::get_entity_from_eid		(u16 id)
{
	if (!ai().get_alife())
		return			(inherited::get_entity_from_eid(id));

	CSE_Abstract		*object = ai().alife().objects().object(id,true);
	if (!object)
		return			(inherited::get_entity_from_eid(id));

	return				(object);
}
/**/

void	game_sv_Single::OnCreate		(ALife::_OBJECT_ID id_who)
{
	if (!ai().get_alife())
		return;

	CSE_Abstract			*e_who			= get_entity_from_eid(id_who);
	VERIFY					(e_who);
	if (!e_who->m_bALifeControl)
		return;

	CSE_ALifeObject			*alife_object	= smart_cast<CSE_ALifeObject*>(e_who);
	if (!alife_object)
		return;

	alife_object->m_bOnline	= true;

	if (alife_object->ID_Parent != ALife::INVALID_OBJECT_ID) {
		CSE_ALifeDynamicObject			*parent = ai().alife().objects().object(alife_object->ID_Parent,true);
		if (parent) {
			CSE_ALifeTraderAbstract		*trader = smart_cast<CSE_ALifeTraderAbstract*>(parent);
			if (trader)
				alife().create			(alife_object);
			else {
				CSE_ALifeInventoryBox* const	box = smart_cast<CSE_ALifeInventoryBox*>(parent);
				if (box || smart_cast<CSE_ALifeInventoryItem*>(parent))
					alife().create		(alife_object);
				else
					alife_object->m_bALifeControl	= false;
			}
		}
		else
			alife_object->m_bALifeControl		= false;
	}
	else
		alife().create					(alife_object);
}

bool	game_sv_Single::OnTouch			(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what, bool bForced)
{
	CSE_Abstract*		e_who	= get_entity_from_eid(eid_who);		VERIFY(e_who	);
	CSE_Abstract*		e_what	= get_entity_from_eid(eid_what);	VERIFY(e_what	);

	if (ai().get_alife() && ai().alife().graph().has_level()) {
		CSE_ALifeInventoryItem	*l_tpALifeInventoryItem	= smart_cast<CSE_ALifeInventoryItem*>	(e_what);
		CSE_ALifeDynamicObject	*l_tpDynamicObject		= smart_cast<CSE_ALifeDynamicObject*>	(e_who);
		
		if	(
				l_tpALifeInventoryItem && 
				l_tpDynamicObject && 
				ai().alife().graph().level().object(l_tpALifeInventoryItem->base()->ID,true) &&
				ai().alife().objects().object(e_who->ID,true) &&
				ai().alife().objects().object(e_what->ID,true)
			)
			alife().graph().attach	(*e_who,l_tpALifeInventoryItem,l_tpDynamicObject->m_tGraphID,false,false);
#ifdef DEBUG
		else
			if (psAI_Flags.test(aiALife)) {
				Msg				("Cannot attach object [%s][%s][%d] to object [%s][%s][%d]",e_what->name_replace(),*e_what->s_name,e_what->ID,e_who->name_replace(),*e_who->s_name,e_who->ID);
			}
#endif
	}
	return true;
}

void game_sv_Single::OnDetach(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what)
{
	if (ai().get_alife() && ai().alife().graph().has_level())
	{
		CSE_Abstract*		e_who	= get_entity_from_eid(eid_who);		VERIFY(e_who	);
		CSE_Abstract*		e_what	= get_entity_from_eid(eid_what);	VERIFY(e_what	);

		CSE_ALifeInventoryItem *l_tpALifeInventoryItem = smart_cast<CSE_ALifeInventoryItem*>(e_what);
		if (!l_tpALifeInventoryItem)
			return;

		CSE_ALifeDynamicObject *l_tpDynamicObject = smart_cast<CSE_ALifeDynamicObject*>(e_who);
		if (!l_tpDynamicObject)
			return;
		
		if	(
				ai().alife().objects().object(e_who->ID,true) && 
				!ai().alife().graph().level().object(l_tpALifeInventoryItem->base()->ID,true) && 
				ai().alife().objects().object(e_what->ID,true)
			)
			alife().graph().detach(*e_who,l_tpALifeInventoryItem,l_tpDynamicObject->m_tGraphID,false,false);
		else {
			if (!ai().alife().objects().object(e_what->ID,true)) {
				auto id = l_tpALifeInventoryItem->base()->ID_Parent;
				l_tpALifeInventoryItem->base()->ID_Parent	= ALife::INVALID_OBJECT_ID;
				
				CSE_ALifeDynamicObject *dynamic_object = smart_cast<CSE_ALifeDynamicObject*>(e_what);
				VERIFY			(dynamic_object);
				dynamic_object->m_tNodeID		= l_tpDynamicObject->m_tNodeID;
				dynamic_object->m_tGraphID		= l_tpDynamicObject->m_tGraphID;
				dynamic_object->m_bALifeControl	= true;
				dynamic_object->m_bOnline		= true;
				alife().create	(dynamic_object);
				l_tpALifeInventoryItem->base()->ID_Parent	= id;
			}
#ifdef DEBUG
			else
				if (psAI_Flags.test(aiALife)) {
					Msg			("Cannot detach object [%s][%s][%d] from object [%s][%s][%d]",l_tpALifeInventoryItem->base()->name_replace(),*l_tpALifeInventoryItem->base()->s_name,l_tpALifeInventoryItem->base()->ID,l_tpDynamicObject->base()->name_replace(),l_tpDynamicObject->base()->s_name.c_str(), l_tpDynamicObject->ID);
				}
#endif
		}
	}
}


void	game_sv_Single::Update			()
{
	if (!PendingRemotePlayers.empty())
	{
		xr_vector<ClientID> pending;
		pending.swap(PendingRemotePlayers);
		for (const ClientID id : pending)
			OnPlayerConnect(id);
	}
	inherited::Update	();
}

void game_sv_Single::SetGameTimeFactor		(const float fTimeFactor)
{
	if (ai().get_alife() && ai().alife().initialized())
		return(alife().time_manager().set_time_factor(fTimeFactor));
	else
		return(inherited::SetGameTimeFactor(fTimeFactor));
}

ALife::_TIME_ID game_sv_Single::GetEnvironmentGameTime		()
{
	if (ai().get_alife() && ai().alife().initialized())
		return(alife().time_manager().game_time());
	else
		return(inherited::GetGameTime());
}

float game_sv_Single::GetEnvironmentGameTimeFactor		()
{
	return(inherited::GetGameTimeFactor());
}

void game_sv_Single::SetEnvironmentGameTimeFactor		(const float fTimeFactor)
{
	return(inherited::SetGameTimeFactor(fTimeFactor));
}

bool game_sv_Single::change_level					(NET_Packet &net_packet, ClientID sender)
{
	if (ai().get_alife())
		return					(alife().change_level(net_packet));
	else
		return					(true);
}

void game_sv_Single::save_game						(NET_Packet &net_packet, ClientID sender)
{
	if (!ai().get_alife())
		return;
	
	alife().save				(net_packet);
}

bool game_sv_Single::load_game					(NET_Packet &net_packet, ClientID sender)
{
	if (!ai().get_alife())
		return					(inherited::load_game(net_packet,sender));
	shared_str						game_name;
	net_packet.r_stringZ		(game_name);
	return						(alife().load_game(*game_name,true));
}

void game_sv_Single::reload_game				(NET_Packet &net_packet, ClientID sender)
{
}

void game_sv_Single::switch_distance			(NET_Packet &net_packet, ClientID sender)
{
	if (!ai().get_alife())
		return;

	alife().set_switch_distance	(net_packet.r_float());
}

void game_sv_Single::teleport_object			(NET_Packet &net_packet, ALife::_OBJECT_ID id)
{
	if (!ai().get_alife())
		return;
	
	GameGraph::_GRAPH_ID		game_vertex_id;
	u32						level_vertex_id;
	Fvector					position;

	net_packet.r			(&game_vertex_id,sizeof(game_vertex_id));
	net_packet.r			(&level_vertex_id,sizeof(level_vertex_id));
	net_packet.r_vec3		(position);

	alife().teleport_object (id,game_vertex_id,level_vertex_id,position);
}

void game_sv_Single::add_restriction			(NET_Packet &packet, ALife::_OBJECT_ID id)
{
	if (!ai().get_alife())
		return;

	ALife::_OBJECT_ID		restriction_id;
	packet >> restriction_id;

	RestrictionSpace::ERestrictorTypes	restriction_type;
	packet.r				(&restriction_type,sizeof(restriction_type));
	
	alife().add_restriction (id,restriction_id,restriction_type);
}

void game_sv_Single::remove_restriction			(NET_Packet &packet, ALife::_OBJECT_ID id)
{
	if (!ai().get_alife())
		return;

	ALife::_OBJECT_ID		restriction_id;
	packet >> restriction_id;

	RestrictionSpace::ERestrictorTypes	restriction_type;
	packet.r				(&restriction_type,sizeof(restriction_type));
	
	alife().remove_restriction (id,restriction_id,restriction_type);
}

void game_sv_Single::remove_all_restrictions	(NET_Packet &packet, ALife::_OBJECT_ID id)
{
	if (!ai().get_alife())
		return;

	RestrictionSpace::ERestrictorTypes	restriction_type;
	packet.r				(&restriction_type,sizeof(restriction_type));

	alife().remove_all_restrictions (id,restriction_type);
}

void game_sv_Single::sls_default				()
{
	if (SpawnResourceEnabled)
		alife().update_switch	();
}

shared_str game_sv_Single::level_name			(const shared_str &server_options) const
{
	if (!ai().get_alife())
		return				(inherited::level_name(server_options));
	return					(alife().level_name());
}

void game_sv_Single::on_death					(CSE_Abstract *e_dest, CSE_Abstract *e_src)
{
	inherited::on_death		(e_dest,e_src);

	if (!ai().get_alife())
		return;

	alife().on_death		(e_dest,e_src);
}

void game_sv_Single::restart_simulator			(const char* saved_game_name)
{
	PROF_EVENT("restart_simulator");
	shared_str				&options = *alife().server_command_line();

	m_alife_simulator->destroy();
	xr_delete				(m_alife_simulator);
	server().clear_ids		();

	xr_strcpy					(g_pGamePersistent->m_game_params.m_game_or_spawn,saved_game_name);
	xr_strcpy					(g_pGamePersistent->m_game_params.m_new_or_load,"load");

	pApp->SetLoadingScreen(new UILoadingScreen());
	pApp->LoadBegin			();
	m_alife_simulator		= new CALifeSimulator(&server(),&options);
	g_pGamePersistent->SetLoadStageTitle("st_client_synchronising");
	pApp->LoadForceFinish();
	g_pGamePersistent->LoadTitle		();
	Device.PreCache			(60, true, true);
	pApp->LoadEnd			();
}
