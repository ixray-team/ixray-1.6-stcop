////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_simulator_script.cpp
//	Created 	: 25.12.2002
//  Modified 	: 13.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
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

#include <luabind/iterator_policy.hpp>
#include <luabind/iterator_pair_policy.hpp>

using namespace luabind;

typedef xr_vector<std::pair<shared_str,int> >	STORY_PAIRS;
typedef STORY_PAIRS								SPAWN_STORY_PAIRS;
LPCSTR											_INVALID_STORY_ID		= "INVALID_STORY_ID";
LPCSTR											_INVALID_SPAWN_STORY_ID	= "INVALID_SPAWN_STORY_ID";
STORY_PAIRS										story_ids;
SPAWN_STORY_PAIRS								spawn_story_ids;

CALifeSimulator *alife				()
{
	return			(const_cast<CALifeSimulator*>(ai().get_alife()));
}

CSE_ALifeDynamicObject *alife_object		(const CALifeSimulator *self_, ALife::_OBJECT_ID object_id)
{
	VERIFY			(self_);
	return			(self_->objects().object(object_id,true));
}

const CALifeObjectRegistry::OBJECT_REGISTRY& alife_objects(const CALifeSimulator* self)
{
	VERIFY(self);
	return self->objects().objects();
}

bool valid_object_id						(const CALifeSimulator *self_, ALife::_OBJECT_ID object_id)
{
	VERIFY			(self_);
	return			(object_id != 0xffff);
}

#ifdef DEBUG
CSE_ALifeDynamicObject *alife_object		(const CALifeSimulator *self_, LPCSTR name)
{
	VERIFY			(self_);
	
	for (CALifeObjectRegistry::OBJECT_REGISTRY::const_iterator it = self_->objects().objects().begin(); it != self_->objects().objects().end(); it++) {
		CSE_ALifeDynamicObject	*obj = it->second;
		if (xr_strcmp(obj->name_replace(),name) == 0)
			return	(it->second);
	}
	
	return			(0);
}
#endif // #ifdef DEBUG

CSE_ALifeDynamicObject *alife_object		(const CALifeSimulator *self_, ALife::_OBJECT_ID id, bool no_assert)
{
	VERIFY			(self_);
	return			(self_->objects().object(id,no_assert));
}

CSE_ALifeDynamicObject *alife_story_object	(const CALifeSimulator *self_, ALife::_STORY_ID id)
{
	return			(self_->story_objects().object(id,true));
}

template <typename _id_type>
void generate_story_ids		(
	STORY_PAIRS &result,
	_id_type	INVALID_ID,
	LPCSTR		section_name,
	LPCSTR		INVALID_ID_STRING,
	LPCSTR		invalid_id_description,
	LPCSTR		invalid_id_redefinition,
	LPCSTR		duplicated_id_description
)
{
	result.clear			();

    CInifile				*Ini = pGameIni;
    
    LPCSTR					N,V;
	u32 					k;
	shared_str				temp;
    LPCSTR					section = section_name;
    R_ASSERT				(Ini->section_exist(section));

	for (k = 0; Ini->r_line(section,k,&N,&V); ++k) {
		temp				= Ini->r_string_wb(section,N);
		
		R_ASSERT3			(!strchr(*temp,' '),invalid_id_description,*temp);
		R_ASSERT2			(xr_strcmp(*temp,INVALID_ID_STRING),invalid_id_redefinition);
		
		STORY_PAIRS::const_iterator	I = result.begin();
		STORY_PAIRS::const_iterator	E = result.end();
		for ( ; I != E; ++I)
			R_ASSERT3		((*I).first != temp,duplicated_id_description,*temp);
		
		result.push_back	(std::make_pair(*temp,atoi(N)));
	}

	result.push_back		(std::make_pair(INVALID_ID_STRING,INVALID_ID));
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
	return						(self_->graph().level().level_id());
}

CSE_ALifeDynamicObject *CALifeSimulator__create	(CALifeSimulator *self_, ALife::_SPAWN_ID spawn_id)
{
	const CALifeSpawnRegistry::SPAWN_GRAPH::CVertex	*vertex = ai().alife().spawns().spawns().vertex(spawn_id);
	THROW2								(vertex,"Invalid spawn id!");

	CSE_ALifeDynamicObject				*spawn = smart_cast<CSE_ALifeDynamicObject*>(&vertex->data()->object());
	THROW								(spawn);

	CSE_ALifeDynamicObject				*object;
	self_->create						(object,spawn,spawn_id);

	return								(object);
}

CSE_Abstract *CALifeSimulator__spawn_item		(CALifeSimulator *self_, LPCSTR section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id)
{
	THROW								(self_);
	return								(self_->spawn_item(section,position,level_vertex_id,game_vertex_id,ALife::_OBJECT_ID(-1)));
}

CSE_Abstract *CALifeSimulator__spawn_item2		(CALifeSimulator *self_, LPCSTR section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent)
{
	if (id_parent == ALife::_OBJECT_ID(-1))
		return							(self_->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent));

	CSE_ALifeDynamicObject				*object = ai().alife().objects().object(id_parent,true);
	if (!object) {
		Msg								("! invalid parent id [%d] specified",id_parent);
		return							(0);
	}

	if (!object->m_bOnline)
		return							(self_->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent));

	NET_Packet							packet;
	packet.w_begin						(M_SPAWN);
	packet.w_stringZ					(section);
	
	CSE_Abstract						*item = self_->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent,false);
	item->Spawn_Write					(packet,FALSE);
	self_->server().FreeID				(item->ID,0);
	F_entity_Destroy					(item);

	ClientID							clientID;
	clientID.set						(0xffff);

	u16									dummy;
	packet.r_begin						(dummy);
	VERIFY								(dummy == M_SPAWN);
	return								(self_->server().Process_spawn(packet,clientID));
}

CSE_Abstract *CALifeSimulator__spawn_ammo		(CALifeSimulator *self_, LPCSTR section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent, int ammo_to_spawn)
{
//	if (id_parent == ALife::_OBJECT_ID(-1))
//		return							(self->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent));
	CSE_ALifeDynamicObject				*object = 0;
	if (id_parent != ALife::_OBJECT_ID(-1)) {
		object							= ai().alife().objects().object(id_parent,true);
		if (!object) {
			Msg							("! invalid parent id [%d] specified",id_parent);
			return						(0);
		}
	}

	if (!object || !object->m_bOnline) {
		CSE_Abstract					*item = self_->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent);

		CSE_ALifeItemAmmo				*ammo = smart_cast<CSE_ALifeItemAmmo*>(item);
		THROW							(ammo);
		THROW							(ammo->m_boxSize >= ammo_to_spawn);
		ammo->a_elapsed					= (u16)ammo_to_spawn;

		return							(item);
	}

	NET_Packet							packet;
	packet.w_begin						(M_SPAWN);
	packet.w_stringZ					(section);
	
	CSE_Abstract						*item = self_->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent,false);

	CSE_ALifeItemAmmo					*ammo = smart_cast<CSE_ALifeItemAmmo*>(item);
	THROW								(ammo);
	THROW								(ammo->m_boxSize >= ammo_to_spawn);
	ammo->a_elapsed						= (u16)ammo_to_spawn;

	item->Spawn_Write					(packet,FALSE);
	self_->server().FreeID				(item->ID,0);
	F_entity_Destroy					(item);

	ClientID							clientID;
	clientID.set						(0xffff);

	u16									dummy;
	packet.r_begin						(dummy);
	VERIFY								(dummy == M_SPAWN);
	return								(self_->server().Process_spawn(packet,clientID));
}

ALife::_SPAWN_ID CALifeSimulator__spawn_id		(CALifeSimulator *self_, ALife::_SPAWN_STORY_ID spawn_story_id)
{
	return								(((const CALifeSimulator *)self_)->spawns().spawn_id(spawn_story_id));
}

void CALifeSimulator__release					(CALifeSimulator *self_, CSE_Abstract *object, bool)
{
	VERIFY								(self_);
//	self->release						(object,true);

	THROW								(object);
	CSE_ALifeObject						*alife_object = smart_cast<CSE_ALifeObject*>(object);
	THROW								(alife_object);
	if (!alife_object->m_bOnline) {
		self_->release					(object,true);
		return;
	}

	// awful hack, for stohe only
	NET_Packet							packet;
	packet.w_begin						(M_EVENT);
	packet.w_u32						(Level().timeServer());
	packet.w_u16						(GE_DESTROY);
	packet.w_u16						(object->ID);
	Level().Send						(packet,net_flags(TRUE,TRUE));
}

LPCSTR get_level_name							(const CALifeSimulator *self_, int level_id)
{
	LPCSTR								result_ = *ai().game_graph().header().level((GameGraph::_LEVEL_ID)level_id).name();
	return								(result_);
}

CSE_ALifeCreatureActor *get_actor				(const CALifeSimulator *self_)
{
	THROW								(self_);
	return								(self_->graph().actor());
}

KNOWN_INFO_VECTOR *registry						(const CALifeSimulator *self_, const ALife::_OBJECT_ID &id)
{
	THROW								(self_);
	return								(self_->registry(info_portions).object(id, true));
}

bool has_info									(const CALifeSimulator *self_, const ALife::_OBJECT_ID &id, LPCSTR info_id)
{
	const KNOWN_INFO_VECTOR				*known_info = registry(self_,id);
	if (!known_info)
		return							(false);

	if (std::find_if(known_info->begin(), known_info->end(), CFindByIDPred(info_id)) == known_info->end())
		return							(false);

	return								(true);
}

bool dont_has_info								(const CALifeSimulator *self_, const ALife::_OBJECT_ID &id, LPCSTR info_id)
{
	THROW								(self_);
	// absurdly, but only because of scriptwriters needs
	return								(!has_info(self_,id,info_id));
}

void set_objects_per_update(CALifeSimulator* self, u32 count)
{
	self->objects_per_update(count);
}

void teleport_object(CALifeSimulator* alife, ALife::_OBJECT_ID id, GameGraph::_GRAPH_ID game_vertex_id, u32 level_vertex_id, const Fvector& position)
{
	alife->teleport_object(id, game_vertex_id, level_vertex_id, position);
}

void IterateInfo(const CALifeSimulator* alife, const ALife::_OBJECT_ID& id, const luabind::functor<bool>& functor)
{
	const KNOWN_INFO_VECTOR* known_info = registry(alife, id);
	if (!known_info)
		return;

	xr_vector<shared_str>::const_iterator	I = known_info->begin();
	xr_vector<shared_str>::const_iterator	E = known_info->end();
	for (; I != E; ++I)
		if (functor(id, (LPCSTR)(*I).c_str()) == true)
			return;
}

CSE_Abstract* reprocess_spawn(CALifeSimulator* self, CSE_Abstract* object)
{
	NET_Packet	packet;
	packet.w_begin(M_SPAWN);
	packet.w_stringZ(object->s_name);

	object->Spawn_Write(packet, FALSE);
	self->server().FreeID(object->ID, 0);
	F_entity_Destroy(object);

	ClientID							clientID;
	clientID.set(0xffff);

	u16									dummy;
	packet.r_begin(dummy);

	return	(self->server().Process_spawn(packet, clientID));
}

CSE_Abstract* try_to_clone_object(CALifeSimulator* self, CSE_Abstract* object, LPCSTR section, const Fvector& position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent, bool bRegister = true)
{
	CSE_ALifeItemWeaponMagazined* wpnmag = smart_cast<CSE_ALifeItemWeaponMagazined*>(object);
	if (wpnmag)
	{
		CSE_Abstract* absClone = self->spawn_item(section, position, level_vertex_id, game_vertex_id, id_parent, false);
		if (!absClone)
			return (0);

		CSE_ALifeItemWeaponMagazined* clone = smart_cast<CSE_ALifeItemWeaponMagazined*>(absClone);
		if (!clone)
			return (0);

		clone->wpn_flags = wpnmag->wpn_flags;
		clone->m_addon_flags = wpnmag->m_addon_flags;
		clone->m_fCondition = wpnmag->m_fCondition;
		clone->ammo_type = wpnmag->ammo_type;
		clone->m_upgrades = wpnmag->m_upgrades;
		clone->a_elapsed = wpnmag->a_elapsed;

		return	bRegister ? reprocess_spawn(self, absClone) : absClone;
	}

	return (0);
}

xr_vector<u16>& get_children(const CALifeSimulator* self, CSE_Abstract* object)
{
	VERIFY(self);
	return object->children;
}

#pragma optimize("s",on)
void CALifeSimulator::script_register			(lua_State *L)
{
	module(L)
	[
		class_<CALifeSimulator>("alife_simulator")
			.def("valid_object_id",			&valid_object_id)
			.def("level_id",				&get_level_id)
			.def("level_name",				&get_level_name)
			.def("object",					(CSE_ALifeDynamicObject *(*) (const CALifeSimulator *,ALife::_OBJECT_ID))(alife_object))
			.def("object",					(CSE_ALifeDynamicObject *(*) (const CALifeSimulator *,ALife::_OBJECT_ID, bool))(alife_object))
			.def("story_object",			(CSE_ALifeDynamicObject *(*) (const CALifeSimulator *,ALife::_STORY_ID))(alife_story_object))
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
			.def("create",					&CALifeSimulator__spawn_item2)
			.def("create",					&CALifeSimulator__spawn_item)
			.def("create_ammo",				&CALifeSimulator__spawn_ammo)
			.def("release",					&CALifeSimulator__release)
			.def("spawn_id",				&CALifeSimulator__spawn_id)
			.def("actor",					&get_actor)
			.def("has_info",				&has_info)
			.def("dont_has_info",			&dont_has_info)
			.def("switch_distance",			&CALifeSimulator::switch_distance)
			.def("switch_distance",			&CALifeSimulator::set_switch_distance)
			.def("objects",					&alife_objects, return_stl_pair_iterator)
			.def("jump_to_level",			(void (CALifeSimulator::*) (LPCSTR))(&CALifeSimulator::jump_to_level))

			.def("teleport_object", &teleport_object)
			.def("iterate_info", &IterateInfo)
			.def("clone_weapon", &try_to_clone_object)
			.def("register", &reprocess_spawn)
			.def("set_objects_per_update", &set_objects_per_update)
			.def("set_process_time", &set_process_time)
			.def("get_children", &get_children, return_stl_iterator)

		,def("alife",						&alife)
	];

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