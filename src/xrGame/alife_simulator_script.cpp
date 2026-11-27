////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_simulator_script.cpp
//	Created 	: 25.12.2002
//  Modified 	: 13.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "alife_simulator.h"
#include "ai_space.h"
#include "alife_object_registry.h"
#include "alife_story_registry.h"
#include "../xrScripts/script_engine.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "restriction_space.h"
#include "alife_graph_registry.h"
#include "alife_spawn_registry.h"
#include "alife_registry_container.h"
#include "xrServer.h"
#include "Level.h"
#include "game_sv_single.h"

#include <luabind/iterator_policy.hpp>
#include <luabind/iterator_pair_policy.hpp>
#include <Actor.h>

#include "SaveObjectHelpers.h"

using namespace luabind;

typedef xr_vector<std::pair<shared_str,int> >	STORY_PAIRS;
typedef STORY_PAIRS								SPAWN_STORY_PAIRS;
const char*											_INVALID_STORY_ID		= "INVALID_STORY_ID";
const char*											_INVALID_SPAWN_STORY_ID	= "INVALID_SPAWN_STORY_ID";
STORY_PAIRS										story_ids;
SPAWN_STORY_PAIRS								spawn_story_ids;

namespace ScriptALifeDetail
{
	template <typename T>
	struct SContextCall
	{
		std::function<T()> Work;
		T Result{};
		void Invoke() { Result = Work(); }
	};

	template <typename T>
	T RunInServerContext(CALifeSimulator* self, std::function<T()> work)
	{
		if (self == nullptr || !Level().GetLevelDesc().RemoteTransport)
			return work();
		CLevel* owner = self->server().GetOwnerLevel();
		if (owner == nullptr || owner == g_pGameLevel)
			return work();
		SContextCall<T> call{ std::move(work) };
		owner->RunInContext(xr_make_delegate(&call, &SContextCall<T>::Invoke));
		return call.Result;
	}

	bool ResolveServerLocation(const Fvector& position, u32& levelVertex, GameGraph::_GRAPH_ID& gameVertex)
	{
		IGameGraph& graph = ai().game_graph();
		if (graph.valid_vertex_id(gameVertex))
			return true;

		const IGameLevelCrossTable* crossTable = ai().get_cross_table();
		if (crossTable != nullptr && !ai().level_graph().valid_vertex_id(levelVertex) && ai().level_graph().valid_vertex_position(position))
			levelVertex = ai().level_graph().vertex_id(position);
		if (crossTable != nullptr && ai().level_graph().valid_vertex_id(levelVertex))
		{
			const GameGraph::_GRAPH_ID resolved = crossTable->vertex(levelVertex).game_vertex_id();
			if (graph.valid_vertex_id(resolved))
			{
				gameVertex = resolved;
				return true;
			}
		}

		Msg("! Remote ALife spawn rejected: no server graph mapping at [%.3f %.3f %.3f], level vertex=%u, game vertex=%u",
			VPUSH(position), levelVertex, gameVertex);
		return false;
	}

	luabind::object NilForScript(lua_State* state)
	{
		lua_pushnil(state);
		luabind::object nil(state);
		nil.set();
		return nil;
	}

	luabind::object SmartForScript(lua_State* state, CLevel* caller, CSE_ALifeSmartZone* smart)
	{
		if (smart == nullptr)
			return NilForScript(state);
		CLevel* server = smart->m_alife_simulator != nullptr ? smart->m_alife_simulator->server().GetOwnerLevel() : nullptr;
		if (caller != nullptr && server != nullptr && caller != server && caller->GetLevelDesc().RemoteTransport && caller->Services.ScriptEngine != server->Services.ScriptEngine)
		{
			if (CSE_ALifeSmartZone* proxy = caller->RemoteSmartZoneProxy(smart, server); proxy != nullptr)
				return { state, proxy };
			return NilForScript(state);
		}
		return { state, smart };
	}

	luabind::object ObjectForScript(lua_State* state, CLevel* caller, CSE_ALifeDynamicObject* object)
	{
		if (object == nullptr)
			return NilForScript(state);
		if (CSE_ALifeSmartZone* smart = object->cast_smart_zone(); smart != nullptr)
			return SmartForScript(state, caller, smart);
		return { state, object };
	}

	luabind::object ObjectForScript(lua_State* state, CLevel* caller, CSE_Abstract* entity)
	{
		if (entity == nullptr)
			return NilForScript(state);
		CSE_ALifeDynamicObject* dynamic = entity->cast_alife_dynamic_object();
		CSE_ALifeSmartZone* smart = entity->cast_smart_zone();
		CSE_ALifeObject* base = entity->cast_alife_object();
		if (dynamic != nullptr)
			return ObjectForScript(state, caller, dynamic);
		if (smart != nullptr)
			return SmartForScript(state, caller, smart);
		if (base != nullptr)
			return { state, base };
		return NilForScript(state);
	}
}

CALifeSimulator *alife				()
{
	if (g_pGameLevel != nullptr && Level().GetLevelDesc().RemoteTransport)
	{
		xrServer* server = Level().RemoteServerEndpoint();
		game_sv_Single* single = server != nullptr && server->game != nullptr ? server->game->cast_game_sv_single() : nullptr;
		if (single != nullptr && single->GetAlifeSimulator() != nullptr)
			return single->GetAlifeSimulator();
	}
	return			(const_cast<CALifeSimulator*>(ai().get_alife()));
}

luabind::object alife_object		(const CALifeSimulator *self_, ALife::_OBJECT_ID object_id)
{
	if (self_ == nullptr)
		return ScriptALifeDetail::NilForScript(ai().script_engine().lua());
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(const_cast<CALifeSimulator*>(self_), [&]() -> luabind::object
	{
		if (CSE_ALifeDynamicObject* object = self_->objects().object(object_id, true))
			return ScriptALifeDetail::ObjectForScript(state, caller, object);
		CSE_Abstract* entity = self_->server().ID_to_entity(object_id);
		if (entity == nullptr && self_->MissingObjectDiagnosticCount++ < 64)
			Msg("* ALife object lookup: id=%u, entity=%p, registry=%zu, online=%u", object_id, entity, self_->objects().objects().size(), self_->server().GetEntitiesNum());
		return ScriptALifeDetail::ObjectForScript(state, caller, entity);
	});
}

const CALifeObjectRegistry::OBJECT_REGISTRY& alife_objects(const CALifeSimulator* self)
{
	VERIFY(self);
	return self->objects().objects();
}

bool valid_object_id						(const CALifeSimulator *self_, ALife::_OBJECT_ID object_id)
{
	VERIFY			(self_);
	return			(object_id != ALife::INVALID_OBJECT_ID);
}

ALife::_OBJECT_ID last_valid_id(const CALifeSimulator *self_)
{
	VERIFY(self_);
	return self_->server().TopValidID();
}

luabind::object story_object_id(const char* storyId)
{
	lua_State* state = ai().script_engine().lua();
	if (storyId == nullptr || *storyId == 0)
		return ScriptALifeDetail::NilForScript(state);
	CALifeSimulator* simulator = alife();
	if (simulator == nullptr)
		return ScriptALifeDetail::NilForScript(state);

	CLevel* owner = simulator->server().GetOwnerLevel();
	CScriptEngine* engine = owner != nullptr && owner->Services.ScriptEngine != nullptr ? (CScriptEngine*)owner->Services.ScriptEngine : &ai().script_engine();
	const ALife::_OBJECT_ID result = ScriptALifeDetail::RunInServerContext<ALife::_OBJECT_ID>(simulator, [&]
	{
		luabind::functor<luabind::object> getRegistry;
		if (!engine->functor("story_objects.get_story_objects_registry", getRegistry))
			return ALife::INVALID_OBJECT_ID;
		luabind::object registry = getRegistry();
		luabind::object ids = registry["id_by_story_id"];
		if (ids.type() != LUA_TTABLE)
			return ALife::INVALID_OBJECT_ID;
		luabind::object value = ids[storyId];
		if (value.type() != LUA_TNUMBER)
			return ALife::INVALID_OBJECT_ID;
		return luabind::object_cast<ALife::_OBJECT_ID>(value);
	});
	if (result == ALife::INVALID_OBJECT_ID)
		return ScriptALifeDetail::NilForScript(state);
	return { state, result };
}

luabind::object alife_object		(const CALifeSimulator *self_, const char* name)
{
	VERIFY			(self_);
	if (!name || !name[0])
		return ScriptALifeDetail::NilForScript(ai().script_engine().lua());
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	for (CALifeObjectRegistry::OBJECT_REGISTRY::const_iterator it = self_->objects().objects().begin(); it != self_->objects().objects().end(); ++it)
	{
		CSE_ALifeDynamicObject* object = it->second;
		if (xr_strcmp(object->name_replace(), name) == 0)
			return ScriptALifeDetail::ObjectForScript(state, caller, object);
	}
	return ScriptALifeDetail::NilForScript(state);
}

luabind::object alife_object		(const CALifeSimulator *self_, ALife::_OBJECT_ID id, bool no_assert)
{
	if (self_ == nullptr)
		return ScriptALifeDetail::NilForScript(ai().script_engine().lua());
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(const_cast<CALifeSimulator*>(self_), [&]() -> luabind::object
	{
		if (CSE_ALifeDynamicObject* object = self_->objects().object(id, no_assert))
			return ScriptALifeDetail::ObjectForScript(state, caller, object);
		CSE_Abstract* entity = self_->server().ID_to_entity(id);
		return ScriptALifeDetail::ObjectForScript(state, caller, entity);
	});
}

luabind::object alife_story_object	(const CALifeSimulator *self_, ALife::_STORY_ID id)
{
	if (self_ == nullptr)
		return ScriptALifeDetail::NilForScript(ai().script_engine().lua());
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(const_cast<CALifeSimulator*>(self_), [&]
	{
		return ScriptALifeDetail::ObjectForScript(state, caller, self_->story_objects().object(id, true));
	});
}

template <typename _id_type>
void generate_story_ids
(
	STORY_PAIRS& result,
	_id_type	INVALID_ID,
	const char* section_name,
	const char* INVALID_ID_STRING,
	const char* invalid_id_description,
	const char* invalid_id_redefinition,
	const char* duplicated_id_description
)
{
	result.clear();

	CInifile* Ini = pGameIni;

	const char* N, * V;
	u32 k;
	shared_str temp;
	const char* section = section_name;
	R_ASSERT(Ini->section_exist(section));

	for (k = 0; Ini->r_line(section, k, &N, &V); ++k)
	{
		temp = Ini->r_string_wb(section, N);

		R_ASSERT3(!strchr(*temp, ' '), invalid_id_description, *temp);
		R_ASSERT2(xr_strcmp(*temp, INVALID_ID_STRING), invalid_id_redefinition);

		STORY_PAIRS::const_iterator	I = result.begin();
		STORY_PAIRS::const_iterator	E = result.end();
		for (; I != E; ++I)
			R_ASSERT3((*I).first != temp, duplicated_id_description, *temp);

		result.push_back(std::make_pair(*temp, atoi(N)));
	}

	result.push_back(std::make_pair(INVALID_ID_STRING, INVALID_ID));
}

void kill_entity0			(CALifeSimulator *alife, CSE_ALifeMonsterAbstract *monster, const GameGraph::_GRAPH_ID &game_vertex_id)
{
	alife->kill_entity		(monster,game_vertex_id,0);
}

void kill_entity1			(CALifeSimulator *alife, CSE_ALifeMonsterAbstract *monster)
{
	alife->kill_entity		(monster,monster->m_tGraphID,0);
}

void add_in_restriction		(CALifeSimulator *alife, CSE_ALifeMonsterAbstract *monster, ALife::_OBJECT_ID id)
{
	alife->add_restriction	(monster->ID,id,RestrictionSpace::eRestrictorTypeIn);
}

void add_out_restriction	(CALifeSimulator *alife, CSE_ALifeMonsterAbstract *monster, ALife::_OBJECT_ID id)
{
	alife->add_restriction	(monster->ID,id,RestrictionSpace::eRestrictorTypeOut);
}

void remove_in_restriction	(CALifeSimulator *alife, CSE_ALifeMonsterAbstract *monster, ALife::_OBJECT_ID id)
{
	alife->remove_restriction	(monster->ID,id,RestrictionSpace::eRestrictorTypeIn);
}

void remove_out_restriction	(CALifeSimulator *alife, CSE_ALifeMonsterAbstract *monster, ALife::_OBJECT_ID id)
{
	alife->remove_restriction	(monster->ID,id,RestrictionSpace::eRestrictorTypeOut);
}

u32 get_level_id(CALifeSimulator *self_)
{
	return self_->graph().has_level() ? self_->graph().level().level_id() : u32(GameGraph::_LEVEL_ID(-1));
}

luabind::object CALifeSimulator__create	(CALifeSimulator *self_, ALife::_SPAWN_ID spawn_id)
{
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(self_, [&]() -> luabind::object
	{
		if (!Level().HasLevelResource(ELevelResource::Spawn))
		{
			Msg("! ALife spawn id[%i] rejected: the spawn resource is disabled", spawn_id);
			return ScriptALifeDetail::NilForScript(state);
		}
		const CALifeSpawnRegistry::SPAWN_GRAPH::CVertex* vertex = ai().alife().spawns().spawns().vertex(spawn_id);
		if (!vertex)
		{
			Msg("! Spawn id[%i] not found in spawns list!", spawn_id);
			g_pScriptEngine->print_stack();
			return ScriptALifeDetail::NilForScript(state);
		}

		CSE_ALifeDynamicObject* spawn = vertex->data()->object().cast_alife_dynamic_object();
		THROW								(spawn);

		CSE_ALifeDynamicObject				*object;
		self_->create						(object,spawn,spawn_id);

		return ScriptALifeDetail::ObjectForScript(state, caller, object);
	});
}

luabind::object CALifeSimulator__spawn_item		(CALifeSimulator *self_, const char* section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id)
{
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(self_, [&]() -> luabind::object
	{
		THROW								(self_);
		if (Level().GetLevelDesc().RemoteTransport && !ScriptALifeDetail::ResolveServerLocation(position, level_vertex_id, game_vertex_id))
			return ScriptALifeDetail::NilForScript(state);
		if (!pSettings->section_exist(section))
		{
			Msg("! Section [%s] not found in game configs!", section);
			g_pScriptEngine->print_stack();
		}

		CSE_Abstract* result = self_->spawn_item(section, position, level_vertex_id, game_vertex_id, ALife::INVALID_OBJECT_ID);
		if (result == nullptr)
			Msg("! Script ALife spawn returned null for '%s', node=%u, graph=%u", section, level_vertex_id, game_vertex_id);
		return ScriptALifeDetail::ObjectForScript(state, caller, result);
	});
}

CSE_Abstract *CALifeSimulator__spawn_item_build(CALifeSimulator *self_, const char* section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent, const char* build)
{
	return ScriptALifeDetail::RunInServerContext<CSE_Abstract*>(self_, [&]() -> CSE_Abstract*
	{
		if (Level().GetLevelDesc().RemoteTransport && !ScriptALifeDetail::ResolveServerLocation(position, level_vertex_id, game_vertex_id))
			return nullptr;
		if (!pSettings->section_exist(section))
		{
			Msg("! Section [%s] not found in game configs!", section);
			g_pScriptEngine->print_stack();
		}

		if (id_parent == ALife::INVALID_OBJECT_ID)
			return self_->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent, true, build);

		CSE_ALifeDynamicObject* object = ai().alife().objects().object(id_parent, true);
		if (!object)
		{
			Msg("! invalid parent id [%d] specified", id_parent);
			return nullptr;
		}
		if (!object->m_bOnline)
			return self_->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent, true, build);

		CSE_Abstract* item = self_->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent, false);
		if (item == nullptr)
			return nullptr;
		NET_Packet packet;
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
			SaveObjectNetPacketHelper::PrepareLocalSpawnPacket(packet, *item);
		else
		{
			packet.w_begin(M_SPAWN);
			packet.w_stringZ(section);
			item->Spawn_Write(packet, false);
		}
		self_->server().FreeID(item->ID, 0);
		F_entity_Destroy(item);

		ClientID clientID;
		clientID.set(ALife::INVALID_OBJECT_ID);
		u16 dummy;
		packet.r_begin(dummy);
		VERIFY(dummy == M_SPAWN || dummy == M_SPAWN_LOCAL);
		CSE_Abstract* result = self_->server().Process_spawn(packet, clientID);
		if (CSE_ALifeObject* alifeObject = smart_cast<CSE_ALifeObject*>(result))
			alifeObject->spawn_build(build);
		return result;
	});
}

CSE_Abstract *CALifeSimulator__spawn_item2(CALifeSimulator *self_, const char* section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent)
{
	return CALifeSimulator__spawn_item_build(self_, section, position, level_vertex_id, game_vertex_id, id_parent, nullptr);
}

luabind::object CALifeSimulator__spawn_item2_script(CALifeSimulator* self_, const char* section, const Fvector& position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent)
{
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	CSE_Abstract* result = CALifeSimulator__spawn_item2(self_, section, position, level_vertex_id, game_vertex_id, id_parent);
	luabind::object bound = ScriptALifeDetail::ObjectForScript(state, caller, result);
	if (result == nullptr || bound.type() == LUA_TNIL)
		Msg("! Script ALife parented spawn binding: section=%s, parent=%u, native=%p, lua-type=%d", section, id_parent, result, bound.type());
	return bound;
}

// Allows to call alife():register(se_obj) manually afterward so that packet editing can be done safely when spawning object with a parent
luabind::object CALifeSimulator__spawn_item3(CALifeSimulator *self, const char* section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent, bool reg = true)
{
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(self, [&]() -> luabind::object
	{
		if (reg == true)
			return CALifeSimulator__spawn_item2_script(self, section, position, level_vertex_id, game_vertex_id, id_parent);
		if (Level().GetLevelDesc().RemoteTransport && !ScriptALifeDetail::ResolveServerLocation(position, level_vertex_id, game_vertex_id))
			return ScriptALifeDetail::NilForScript(state);

		if (!pSettings->section_exist(section))
		{
			Msg("! Section [%s] not found in game configs!", section);
			g_pScriptEngine->print_stack();
		}

		if (id_parent == ALife::INVALID_OBJECT_ID)
			return ScriptALifeDetail::ObjectForScript(state, caller, self->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent));

		CSE_ALifeDynamicObject				*object = ai().alife().objects().object(id_parent, true);
		if (!object) {
			Msg("! invalid parent id [%d] specified", id_parent);
			return ScriptALifeDetail::NilForScript(state);
		}

		if (!object->m_bOnline)
			return ScriptALifeDetail::ObjectForScript(state, caller, self->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent));

		CSE_Abstract						*item = self->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent, false);

		return ScriptALifeDetail::ObjectForScript(state, caller, item);
	});
}

luabind::object CALifeSimulator__spawn_ammo		(CALifeSimulator *self_, const char* section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent, int ammo_to_spawn)
{
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(self_, [&]() -> luabind::object
	{
		if (Level().GetLevelDesc().RemoteTransport && !ScriptALifeDetail::ResolveServerLocation(position, level_vertex_id, game_vertex_id))
			return ScriptALifeDetail::NilForScript(state);

		CSE_ALifeDynamicObject* object = nullptr;
		if (id_parent != ALife::INVALID_OBJECT_ID)
		{
			object = ai().alife().objects().object(id_parent, true);
			if (!object)
			{
				Msg("! invalid parent id [%d] specified", id_parent);
				return ScriptALifeDetail::NilForScript(state);
			}
		}

		if (!object || !object->m_bOnline)
		{
			CSE_Abstract* item = self_->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent);
			CSE_ALifeItemAmmo* ammo = item->cast_item_ammo();
			THROW(ammo);
			THROW(ammo->m_boxSize >= ammo_to_spawn);
			ammo->a_elapsed = (u16)ammo_to_spawn;
			return ScriptALifeDetail::ObjectForScript(state, caller, item);
		}

		CSE_Abstract* item = self_->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent, false);
		CSE_ALifeItemAmmo* ammo = item->cast_item_ammo();
		THROW(ammo);
		THROW(ammo->m_boxSize >= ammo_to_spawn);
		ammo->a_elapsed = (u16)ammo_to_spawn;
		NET_Packet packet;
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
		{
			SaveObjectNetPacketHelper::PrepareLocalSpawnPacket(packet, *item);
		}
		else
		{
			packet.w_begin(M_SPAWN);
			packet.w_stringZ(section);
			item->Spawn_Write(packet, false);
		}
		self_->server().FreeID(item->ID, 0);
		F_entity_Destroy(item);

		ClientID clientID;
		clientID.set(ALife::INVALID_OBJECT_ID);
		u16 dummy;
		packet.r_begin(dummy);
		VERIFY(dummy == M_SPAWN || dummy == M_SPAWN_LOCAL);
		return ScriptALifeDetail::ObjectForScript(state, caller, self_->server().Process_spawn(packet, clientID));
	});
}

ALife::_SPAWN_ID CALifeSimulator__spawn_id		(CALifeSimulator *self_, ALife::_SPAWN_STORY_ID spawn_story_id)
{
	return								(((const CALifeSimulator *)self_)->spawns().spawn_id(spawn_story_id));
}

void CALifeSimulator__release(CALifeSimulator* self_, CSE_Abstract* object, bool)
{
	VERIFY(self_);

	if (object == nullptr)
	{
		return;
	}

	CSE_ALifeObject* alife_object = object->cast_alife_object();

	if (alife_object != nullptr && !alife_object->m_bOnline)
	{
		self_->release(object, true);
		return;
	}

	// awful hack, for stohe only
	NET_Packet							packet;
	packet.w_begin(M_EVENT);
	packet.w_u32(Level().timeServer());
	packet.w_u16(GE_DESTROY);
	packet << object->ID;
	Level().Send(packet, net_flags(true, true));
}

void CALifeSimulator__release2(CALifeSimulator *self, CSE_Abstract *object)
{
	CALifeSimulator__release(self, object, false);
						 
}

const char* get_level_name							(const CALifeSimulator *self_, int level_id)
{
	if (level_id < 0 || !ai().game_graph().header().level_exist((GameGraph::_LEVEL_ID)level_id))
		return "unknown";
	return *ai().game_graph().header().level((GameGraph::_LEVEL_ID)level_id).name();
}

CSE_ALifeCreatureActor *get_actor				(const CALifeSimulator *self_)
{
	if (self_ == nullptr)
		return nullptr;
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<CSE_ALifeCreatureActor*>(const_cast<CALifeSimulator*>(self_), [&]() -> CSE_ALifeCreatureActor*
	{
		CLevel* owner = self_->server().GetOwnerLevel();
		if (caller != nullptr && owner != nullptr && caller != owner && caller->GetLevelDesc().RemoteTransport)
		{
			const xrClientData* remoteClient = self_->server().ID_to_client(caller->GetClientID());
			if (remoteClient != nullptr && remoteClient->owner != nullptr)
				return smart_cast<CSE_ALifeCreatureActor*>(remoteClient->owner);
		}
		if (CSE_ALifeCreatureActor* actor = self_->graph().actor())
			return actor;
		const IClient* serverClient = self_->server().GetServerClient();
		const xrClientData* client = serverClient != nullptr ? self_->server().ID_to_client(serverClient->ID) : nullptr;
		return client != nullptr && client->owner != nullptr ? smart_cast<CSE_ALifeCreatureActor*>(client->owner) : nullptr;
	});
}

KNOWN_INFO_CONTAINER* registry(const CALifeSimulator* self_, const ALife::_OBJECT_ID& id)
{
	THROW(self_);
	return (self_->registry().get<CInfoPortionRegistry>().object(id, true));
}

bool has_info									(const CALifeSimulator *self_, const ALife::_OBJECT_ID &id, const char* info_id)
{
	auto known_info = registry(self_,id);
	if (!known_info)
	{
		return false;
	}
	return known_info->HasInfo(info_id);
}

bool dont_has_info								(const CALifeSimulator *self_, const ALife::_OBJECT_ID &id, const char* info_id)
{
	THROW(self_);
	// absurdly, but only because of scriptwriters needs
	return !has_info(self_,id,info_id);
}

void set_objects_per_update(CALifeSimulator* self, u32 count)
{
	self->objects_per_update(count);
}

void AlifeGiveInfo(const CALifeSimulator *alife, const ALife::_OBJECT_ID &id, const char* info_id)
{
	auto known_info = alife->registry().get<CInfoPortionRegistry>().object(id, true);
	if (!known_info)
	{
		return;
	}
	known_info->AddInfo(info_id, Level().GetGameTime());
}

void AlifeRemoveInfo(const CALifeSimulator *alife, const ALife::_OBJECT_ID &id, const char* info_id)
{
	auto known_info = alife->registry().get<CInfoPortionRegistry>().object(id, true);
	if (!known_info)
	{
		return;
	}
	known_info->RemoveInfo(info_id);
}

void teleport_object(CALifeSimulator* alife, ALife::_OBJECT_ID id, GameGraph::_GRAPH_ID game_vertex_id, u32 level_vertex_id, const Fvector& position)
{
	alife->teleport_object(id, game_vertex_id, level_vertex_id, position);
}

void IterateInfo(const CALifeSimulator* alife, const ALife::_OBJECT_ID& id, const luabind::functor<bool>& functor)
{
	const auto known_info = registry(alife, id);
	if (!known_info)
	{
		return;
	}

	for (const auto& it : known_info->Data)
	{
		if (functor(id, (const char*)(it).info_id.c_str()))
		{
			return;
		}
	}
}

luabind::object reprocess_spawn(CALifeSimulator* self, CSE_Abstract* object)
{
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(self, [&]() -> luabind::object
	{
		NET_Packet	packet;
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
		{
			SaveObjectNetPacketHelper::PrepareLocalSpawnPacket(packet, *object);
		} else
		{
			packet.w_begin(M_SPAWN);
			packet.w_stringZ(object->s_name);
			object->Spawn_Write(packet, false);
		}
		self->server().FreeID(object->ID, 0);
		F_entity_Destroy(object);

		ClientID							clientID;
		clientID.set(ALife::INVALID_OBJECT_ID);

		u16									dummy;
		packet.r_begin(dummy);

		return ScriptALifeDetail::ObjectForScript(state, caller, self->server().Process_spawn(packet, clientID));
	});
}

luabind::object try_to_clone_object(CALifeSimulator* self, CSE_Abstract* object, const char* section, const Fvector& position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent, bool bRegister = true)
{
	lua_State* state = ai().script_engine().lua();
	CLevel* caller = (CLevel*)g_pGameLevel;
	return ScriptALifeDetail::RunInServerContext<luabind::object>(self, [&]() -> luabind::object
	{
		CSE_ALifeItemWeaponMagazined* wpnmag = smart_cast<CSE_ALifeItemWeaponMagazined*>(object);
		if (wpnmag)
		{
			CSE_Abstract* absClone = self->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent, false);
			if (!absClone)
				return ScriptALifeDetail::NilForScript(state);

			CSE_ALifeItemWeaponMagazined* clone = smart_cast<CSE_ALifeItemWeaponMagazined*>(absClone);
			if (!clone)
				return ScriptALifeDetail::NilForScript(state);

			clone->wpn_flags = wpnmag->wpn_flags;
			clone->m_addon_flags = wpnmag->m_addon_flags;
			clone->m_fCondition = wpnmag->m_fCondition;
			clone->a_ammo_type = wpnmag->a_ammo_type;
			clone->m_upgrades = wpnmag->m_upgrades;
			clone->a_elapsed = wpnmag->a_elapsed;

			return bRegister ? reprocess_spawn(self, absClone) : ScriptALifeDetail::ObjectForScript(state, caller, absClone);
		}

		return ScriptALifeDetail::NilForScript(state);
	});
}

xr_vector<ALife::_OBJECT_ID>& get_children(const CALifeSimulator* self, CSE_Abstract* object)
{
	VERIFY(self);
	return object->children;
}

void set_start_position_from_smart(const char* smartName)
{
    string64 tmp;
    xr_sprintf(tmp, "%s", smartName);
    g_start_position_smart = tmp;
}
void set_start_position(Fvector& pos)
{
    g_start_position = pos;
}
void set_start_game_vertex_id(int id)
{
    g_start_game_vertex_id = id;
}

// Degraded sessions (sandbox/viewer) have no alife simulator; scripts still
// marshal their storage through these hooks, so keep a process-local fallback
// buffer to let the script-side save/load round-trip correctly.
static xr_vector<char> g_marshal_fallback_data;

int get_marshalized_data(lua_State* L)
{
	if (ai().get_alife() == nullptr)
	{
		lua_pushlstring(L, (const char*)g_marshal_fallback_data.data(), g_marshal_fallback_data.size());
		return 1;
	}
	auto& buf = ai().alife().marshal_save_data;
    lua_pushlstring(L, (const char*)buf.data(), buf.size());
	return 1;
}

void set_marshalized_data(lua_State* L)
{
	size_t len;
	const char *data = lua_tolstring(L, 1, &len);
	if (!data)
	{
		return;
	}
	if (ai().get_alife() != nullptr)
	{
		auto& buf = const_cast<CALifeSimulator&>(ai().alife()).marshal_save_data;
		buf.resize(len);
		std::memcpy(buf.data(), data, len);
	}
	else
	{
		g_marshal_fallback_data.assign(data, data + len);
	}
}

#pragma optimize("s",on)
void CALifeSimulator::script_register			(lua_State *L)
{
	module(L)
	[
		class_<CALifeSimulator>("alife_simulator")
			.def("last_valid_id",	&last_valid_id)
			.def("valid_object_id",			&valid_object_id)
			.def("level_id",				&get_level_id)
			.def("level_name",				&get_level_name)
			.def("object", (luabind::object (*) (const CALifeSimulator*, ALife::_OBJECT_ID))(alife_object))
			.def("object", (luabind::object (*) (const CALifeSimulator*, const char*))(alife_object))
			.def("object", (luabind::object (*) (const CALifeSimulator*, ALife::_OBJECT_ID, bool))(alife_object))
			.def("story_object",			(luabind::object (*) (const CALifeSimulator *,ALife::_STORY_ID))(alife_story_object))
			.def("set_switch_online",		(void (CALifeSimulator::*) (ALife::_OBJECT_ID,bool))(&CALifeSimulator::set_switch_online))
			.def("set_switch_offline",		(void (CALifeSimulator::*) (ALife::_OBJECT_ID,bool))(&CALifeSimulator::set_switch_offline))
			.def("set_interactive",			(void (CALifeSimulator::*) (ALife::_OBJECT_ID,bool))(&CALifeSimulator::set_interactive))
			.def("kill_entity",				&CALifeSimulator::kill_entity)
			.def("kill_entity",				&kill_entity0)
			.def("kill_entity",				&kill_entity1)
			.def("add_in_restriction",		&add_in_restriction)
			.def("add_out_restriction",		&add_out_restriction)
			.def("remove_in_restriction",	&remove_in_restriction)
			.def("remove_out_restriction",	&remove_out_restriction)
			.def("remove_all_restrictions",	&CALifeSimulator::remove_all_restrictions)
			.def("create",					&CALifeSimulator__create)
			.def("create",					&CALifeSimulator__spawn_item2_script)
			.def("create",					&CALifeSimulator__spawn_item)
			.def("create",					&CALifeSimulator__spawn_item3)
			.def("create_ammo",				&CALifeSimulator__spawn_ammo)
			.def("release",					&CALifeSimulator__release)
			.def("release",					&CALifeSimulator__release2)												  
			.def("spawn_id",				&CALifeSimulator__spawn_id)
			.def("actor",					&get_actor)
			.def("has_info",				&has_info)
			.def("dont_has_info",			&dont_has_info)
			.def("give_info",				&AlifeGiveInfo)
			.def("disable_info",			&AlifeRemoveInfo)										   
			.def("switch_distance",			&CALifeSimulator::switch_distance)
			.def("set_switch_distance",		&CALifeSimulator::set_switch_distance)
			.def("objects",					&alife_objects, return_stl_pair_iterator)
			.def("jump_to_level",			(void (CALifeSimulator::*) (const char*))(&CALifeSimulator::jump_to_level))

			.def("teleport_object", &::teleport_object)
			.def("iterate_info", &IterateInfo)
			.def("clone_weapon", &try_to_clone_object)
			.def("register", &reprocess_spawn)
			.def("set_objects_per_update", &set_objects_per_update)
			.def("set_process_time", &CALifeSimulator::set_process_time)
			.def("get_children", &get_children, return_stl_iterator),

        def("alife", &alife),
        def("get_story_object_id", &story_object_id),
        def("story_object_id", &story_object_id),
        def("set_start_position_from_smart", &set_start_position_from_smart),
        def("set_start_position", &set_start_position),
        def("set_start_game_vertex_id", &set_start_game_vertex_id),
        def("get_marshalized_data", &get_marshalized_data, raw<1>()),
        def("set_marshalized_data", &set_marshalized_data, raw<1>())
	];
	
	module(L, "alife_const")[
		def("dummy_alife", &alife)
	];
	object alife_consts = get_globals(L)["alife_const"];
	alife_consts["invalid_object_id"] = ALife::INVALID_OBJECT_ID;

	{
		if (story_ids.empty())
			generate_story_ids	(
				story_ids,
				INVALID_STORY_ID,
				"story_ids",
				"INVALID_STORY_ID",
				"Invalid story id description (contains spaces)!",
				"INVALID_STORY_ID redifinition!",
				"Duplicated story id description!"
			);

		luabind::class_<class_exporter<CALifeSimulator> >	instance("story_ids");

		for (const auto& pair : story_ids)
			instance = std::move(instance).enum_("_story_ids")[luabind::value(*pair.first, pair.second)];

		luabind::module(L)[std::move(instance)];
	}

	{
		if (spawn_story_ids.empty())
			generate_story_ids	(
				spawn_story_ids,
				INVALID_SPAWN_STORY_ID,
				"spawn_story_ids",
				"INVALID_SPAWN_STORY_ID",
				"Invalid spawn story id description (contains spaces)!",
				"INVALID_SPAWN_STORY_ID redifinition!",
				"Duplicated spawn story id description!"
			);

		luabind::class_<class_exporter<class_exporter<CALifeSimulator> > >	instance("spawn_story_ids");

		for (const auto& pair : spawn_story_ids)
			instance = std::move(instance).enum_("_spawn_story_ids")[luabind::value(*pair.first, pair.second)];

		luabind::module(L)[std::move(instance)];
	}
}

#if 0//def DEBUG
struct dummy {
    int count;
    lua_State* state;
    int ref;
};

void CALifeSimulator::validate			()
{
	typedef CALifeSpawnRegistry::SPAWN_GRAPH::const_vertex_iterator	const_vertex_iterator;
	const_vertex_iterator		I = spawns().spawns().vertices().begin();
	const_vertex_iterator		E = spawns().spawns().vertices().end();
	for ( ; I != E; ++I) {
		luabind::wrap_base		*base = smart_cast<luabind::wrap_base*>(&(*I).second->data()->object());
		if (!base)
			continue;

		if (!base->m_self.m_impl)
			continue;

		dummy					*_dummy = (dummy*)((void*)base->m_self.m_impl);
		lua_State				**_state = &_dummy->state;
		VERIFY2					(
			base->m_self.state(),
			make_string(
				"0x%08x name[%s] name_replace[%s]",
				*(int*)&_state,
				(*I).second->data()->object().name(),
				(*I).second->data()->object().name_replace()
			)
		);
	}
}
#endif //DEBUG
