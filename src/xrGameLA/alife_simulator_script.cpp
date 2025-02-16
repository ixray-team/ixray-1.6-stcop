////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_simulator_script.cpp
//	Created 	: 25.12.2002
//  Modified 	: 13.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator script export
////////////////////////////////////////////////////////////////////////////

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
#include "CustomTimersManager.h"
#include "level.h"
#include "restriction_space.h"
#include "Store.h"

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

CSE_ALifeDynamicObject *alife_object		(const CALifeSimulator *self, ALife::_OBJECT_ID object_id)
{
	VERIFY			(self);
	return			(self->objects().object(object_id,true));
}

bool valid_object_id						(const CALifeSimulator *self, ALife::_OBJECT_ID object_id)
{
	VERIFY			(self);
	return			(object_id != 0xffff);
}

CSE_ALifeDynamicObject *alife_object		(const CALifeSimulator *self, LPCSTR name)
{
	VERIFY			(self);
	
	for (CALifeObjectRegistry::OBJECT_REGISTRY::const_iterator it = self->objects().objects().begin(); it != self->objects().objects().end(); it++) {
		CSE_ALifeDynamicObject	*obj = it->second;
		if (xr_strcmp(obj->name_replace(),name) == 0)
			return	(it->second);
	}
	
	return			(0);
}


CSE_ALifeDynamicObject *alife_object		(const CALifeSimulator *self, ALife::_OBJECT_ID id, bool no_assert)
{
	VERIFY			(self);
	return			(self->objects().object(id,no_assert));
}

const CALifeObjectRegistry::OBJECT_REGISTRY& alife_objects		(const CALifeSimulator *self)
{
	VERIFY			(self);
	return self->objects().objects();
}

CSE_ALifeDynamicObject *alife_story_object	(const CALifeSimulator *self, ALife::_STORY_ID id)
{
	return			(self->story_objects().object(id,true));
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

u32 get_level_id(CALifeSimulator *self)
{
	return						(self->graph().level().level_id());
}

CSE_ALifeDynamicObject *CALifeSimulator__create	(CALifeSimulator *self, ALife::_SPAWN_ID spawn_id)
{
	const CALifeSpawnRegistry::SPAWN_GRAPH::CVertex	*vertex = ai().alife().spawns().spawns().vertex(spawn_id);
	THROW2								(vertex,"Invalid spawn id!");

	CSE_ALifeDynamicObject				*spawn = smart_cast<CSE_ALifeDynamicObject*>(&vertex->data()->object());
	THROW								(spawn);

	CSE_ALifeDynamicObject				*object;
	self->create						(object,spawn,spawn_id);

	return								(object);
}

CSE_Abstract *CALifeSimulator__spawn_item		(CALifeSimulator *self, LPCSTR section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id)
{
	THROW								(self);
	return								(self->spawn_item(section,position,level_vertex_id,game_vertex_id,ALife::_OBJECT_ID(-1)));
}

CSE_Abstract *CALifeSimulator__spawn_item2		(CALifeSimulator *self, LPCSTR section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent)
{
	if (id_parent == ALife::_OBJECT_ID(-1))
		return							(self->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent));

	CSE_ALifeDynamicObject				*object = ai().alife().objects().object(id_parent,true);
	if (!object) {
		Msg								("! invalid parent id [%d] specified",id_parent);
		return							(0);
	}

	if (!object->m_bOnline)
		return							(self->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent));

	NET_Packet							packet;
	packet.w_begin						(M_SPAWN);
	packet.w_stringZ					(section);
	
	CSE_Abstract						*item = self->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent,false);
	item->Spawn_Write					(packet,FALSE);
	self->server().FreeID				(item->ID,0);
	F_entity_Destroy					(item);

	ClientID							clientID;
	clientID.set						(0xffff);

	u16									dummy;
	packet.r_begin						(dummy);
	VERIFY								(dummy == M_SPAWN);
	return								(self->server().Process_spawn(packet,clientID));
}

CStoreHouse *get_store(CALifeSimulator *self)
{
	return &(self->store_house());
	//return NULL;
}
#ifdef USE_TIMERS_MANAGER
CTimersManager *get_timers_manager(CALifeSimulator *self)
{
	return &(self->timers());
}
#endif
CSE_Abstract *CALifeSimulator__spawn_item3		(CALifeSimulator *self, LPCSTR section, ALife::_STORY_ID sid, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id)
{
	CSE_Abstract			*entity		= NULL;
	CSE_ALifeDynamicObject	*object		= NULL;
	THROW																		(self);
	//R_ASSERT																	(!self->story_objects().object(sid, false));
	entity								= self->spawn_item						(section, position, level_vertex_id, game_vertex_id, ALife::_OBJECT_ID(-1), false);
	object								= smart_cast<CSE_ALifeDynamicObject*>	(entity);
	R_ASSERT																	(object);
	object->m_story_id					= sid;
	self->register_object														(object, true);
	return entity;
}

CSE_Abstract *CALifeSimulator__spawn_item4		(CALifeSimulator *self, LPCSTR section, const Fvector &position, const Fvector &direction,
													 u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_STORY_ID sid = INVALID_STORY_ID)
{
	CSE_Abstract			*entity		= NULL;
	CSE_ALifeDynamicObject	*object		= NULL;
	THROW																		(self);
	//R_ASSERT																	(!self->story_objects().object(sid, false));
	entity								= self->spawn_item						(section, position, level_vertex_id, game_vertex_id, ALife::_OBJECT_ID(-1), false);
	object								= smart_cast<CSE_ALifeDynamicObject*>	(entity);
	R_ASSERT																	(object);
	object->m_story_id					= sid;
	object->o_Angle						= direction;
	self->register_object														(object, true);
	return entity;
}

CSE_Abstract *CALifeSimulator__spawn_anom		(CALifeSimulator *self, LPCSTR section, const Fvector &position,
													 u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, float zone_radius, u8 restr_type)
{
	THROW								 (self);
	CSE_Abstract			*entity		=(self->spawn_item(section,position,level_vertex_id,game_vertex_id,ALife::_OBJECT_ID(-1)));

	CSE_ALifeAnomalousZone		*anomaly = smart_cast<CSE_ALifeAnomalousZone*>(entity);
	THROW							  										  (anomaly);

	CShapeData::shape_def		_shape;
	_shape.data.sphere.P.set	(0.0f,0.0f,0.0f);
	_shape.data.sphere.R		= zone_radius;
	_shape.type					= CShapeData::cfSphere;
	anomaly->assign_shapes	(&_shape,1);
	anomaly->m_artefact_spawn_count = 0;//true
	anomaly->m_space_restrictor_type	= RestrictionSpace::ERestrictorTypes(restr_type);

	return entity;
}

CSE_Abstract *CALifeSimulator__spawn_anom_sid		(CALifeSimulator *self, LPCSTR section, const Fvector &position,
													 u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, float zone_radius, u8 restr_type, ALife::_STORY_ID sid = INVALID_STORY_ID)
{
	THROW								 (self);
	R_ASSERT							(!self->story_objects().object(sid, false));
	CSE_Abstract			*entity		=(self->spawn_item(section,position,level_vertex_id,game_vertex_id,ALife::_OBJECT_ID(-1)));

	CSE_ALifeAnomalousZone		*anomaly = smart_cast<CSE_ALifeAnomalousZone*>(entity);
	THROW							  										  (anomaly);

	CShapeData::shape_def		_shape;
	_shape.data.sphere.P.set	(0.0f,0.0f,0.0f);
	_shape.data.sphere.R		= zone_radius;
	_shape.type					= CShapeData::cfSphere;
	anomaly->assign_shapes	(&_shape,1);
	anomaly->m_artefact_spawn_count = 0;//true
	anomaly->m_space_restrictor_type	= RestrictionSpace::ERestrictorTypes(restr_type);

	anomaly->m_story_id					= sid;

	return entity;
}

CSE_Abstract *CALifeSimulator__spawn_ammo		(CALifeSimulator *self, LPCSTR section, const Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent, int ammo_to_spawn)
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
		CSE_Abstract					*item = self->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent);

		CSE_ALifeItemAmmo				*ammo = smart_cast<CSE_ALifeItemAmmo*>(item);
		THROW							(ammo);
		THROW							(ammo->m_boxSize >= ammo_to_spawn);
		ammo->a_elapsed					= (u16)ammo_to_spawn;

		return							(item);
	}

	NET_Packet							packet;
	packet.w_begin						(M_SPAWN);
	packet.w_stringZ					(section);
	
	CSE_Abstract						*item = self->spawn_item(section,position,level_vertex_id,game_vertex_id,id_parent,false);

	CSE_ALifeItemAmmo					*ammo = smart_cast<CSE_ALifeItemAmmo*>(item);
	THROW								(ammo);
	THROW								(ammo->m_boxSize >= ammo_to_spawn);
	ammo->a_elapsed						= (u16)ammo_to_spawn;

	item->Spawn_Write					(packet,FALSE);
	self->server().FreeID				(item->ID,0);
	F_entity_Destroy					(item);

	ClientID							clientID;
	clientID.set						(0xffff);

	u16									dummy;
	packet.r_begin						(dummy);
	VERIFY								(dummy == M_SPAWN);
	return								(self->server().Process_spawn(packet,clientID));
}

ALife::_SPAWN_ID CALifeSimulator__spawn_id		(CALifeSimulator *self, ALife::_SPAWN_STORY_ID spawn_story_id)
{
	return								(((const CALifeSimulator *)self)->spawns().spawn_id(spawn_story_id));
}

void CALifeSimulator__release					(CALifeSimulator *self, CSE_Abstract *object, bool value)
{
	VERIFY								(self);

	R_ASSERT2(object, "CALifeSimulator__release: Object is not exist.");

	CSE_ALifeObject						*alife_object = smart_cast<CSE_ALifeObject*>(object);
	THROW								(alife_object);
	if (!alife_object->m_bOnline) {
		self->release					(object,value);
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

LPCSTR get_level_name							(const CALifeSimulator *self, int level_id)
{
	LPCSTR								result = *ai().game_graph().header().level((GameGraph::_LEVEL_ID)level_id).name();
	return								(result);
}

CSE_ALifeCreatureActor *get_actor				(const CALifeSimulator *self)
{
	THROW								(self);
	return								(self->graph().actor());
}

KNOWN_INFO_VECTOR *registry						(const CALifeSimulator *self, const ALife::_OBJECT_ID &id)
{
	THROW								(self);
	return								(self->registry(info_portions).object(id, true));
}

class CFindByIDPred
{
public:
	CFindByIDPred(shared_str element_to_find) {element = element_to_find;}
	bool operator () (const INFO_DATA& data) const {return data.info_id == element;}
private:
	shared_str element;
};

bool has_info									(const CALifeSimulator *self, const ALife::_OBJECT_ID &id, LPCSTR info_id)
{
	const KNOWN_INFO_VECTOR				*known_info = registry(self,id);
	if (!known_info)
		return							(false);

	if (std::find_if(known_info->begin(), known_info->end(), CFindByIDPred(info_id)) == known_info->end())
		return							(false);

	return								(true);
}

bool dont_has_info								(const CALifeSimulator *self, const ALife::_OBJECT_ID &id, LPCSTR info_id)
{
	THROW								(self);
	// absurdly, but only because of scriptwriters needs
	return								(!has_info(self,id,info_id));
}
// lost alpha start
void teleport_entity(CALifeSimulator *self, CSE_Abstract *object, Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id)
{
	VERIFY(self);
	THROW(object);
	self->teleport_object(object->ID, game_vertex_id, level_vertex_id, position);
}

void teleport_entity(CALifeSimulator *self, ALife::_OBJECT_ID object_id, Fvector &position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id)
{
	VERIFY(self);
	self->teleport_object(object_id, game_vertex_id, level_vertex_id, position);
}



CSE_Abstract *CALifeSimulator__spawn_restrictor		(CALifeSimulator *self, LPCSTR section, const Fvector &position,
													 u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, float radius, u8 restr_type)
{
	THROW				(self);
	CSE_Abstract *entity		= (self->spawn_item(section,position,level_vertex_id,game_vertex_id,ALife::_OBJECT_ID(-1)));

	CSE_ALifeSpaceRestrictor *restr	= smart_cast<CSE_ALifeSpaceRestrictor*>(entity);
	THROW				(restr);

	CShapeData::shape_def		restr_shape;
	restr_shape.data.sphere.P.set		(0.0f,0.0f,0.0f);
	restr_shape.data.sphere.R		= radius;
	restr_shape.type			= CShapeData::cfSphere;

	restr->assign_shapes		(&restr_shape,1);
	restr->m_space_restrictor_type	= RestrictionSpace::ERestrictorTypes(restr_type);

	return entity;
}

void script_switch_to_offline(CALifeSimulator *self, u16 id)
{
	VERIFY(self);
	self->switch_to_offline(id);
}

void script_switch_to_online(CALifeSimulator *self, u16 id)
{
	VERIFY(self);
	self->switch_to_online(id);
}

void force_entity_update(const CALifeSimulator *self, u16 id)
{
	NET_Packet p;
	CSE_Abstract *sobj = 0;
	CSE_ALifeDynamicObject *dsobj = 0;
	VERIFY(self);
	dsobj = self->objects().object(id, true);
	R_ASSERT2(dsobj, make_string<const char*>("can't find entity [%d]", id));
	sobj = smart_cast<CSE_Abstract*>(dsobj);
	p.w_begin(M_UPDATE);
	sobj->UPDATE_Write(p);

}

void change_actor_level(CALifeSimulator *self, Fvector3 position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_graph_id, Fvector3 dir)
{
	NET_Packet P;
	VERIFY																(self);
	P.w_begin															(M_CHANGE_LEVEL);
	P.w_u16																(game_graph_id);
	P.w_u32																(level_vertex_id);
	P.w_vec3															(position);
	P.w_vec3															(dir);
	Level().Send														(P, net_flags(TRUE));
}

//void disable_info_portion						(const CALifeSimulator *self, const ALife::_OBJECT_ID &id)
//{
//	THROW								(self);
//}

//void give_info_portion							(const CALifeSimulator *self, const ALife::_OBJECT_ID &id)
//{
//	THROW								(self);
//}

CSE_ALifeItemWeapon* cast_to_weapon(CALifeSimulator *self, CSE_ALifeDynamicObject* obj)
{
	return smart_cast<CSE_ALifeItemWeapon*>(obj);
}

CSE_ALifeItemWeaponMagazinedWGL* cast_to_weapongl(CALifeSimulator *self, CSE_ALifeDynamicObject* obj)
{
	return smart_cast<CSE_ALifeItemWeaponMagazinedWGL*>(obj);
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
			.def("objects",					&alife_objects, return_stl_pair_iterator)
			.def("object",					(CSE_ALifeDynamicObject *(*) (const CALifeSimulator *,ALife::_OBJECT_ID))(alife_object))
			.def("object",					(CSE_ALifeDynamicObject *(*) (const CALifeSimulator *,LPCSTR))(alife_object))
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
			// lost alpha start
			.def("cast_to_weapongl", 		&cast_to_weapongl)
			.def("cast_to_weapon",			&cast_to_weapon)
			.def("teleport_entity",			(void(*) (CALifeSimulator*, ALife::_OBJECT_ID, Fvector&, u32, GameGraph::_GRAPH_ID))(teleport_entity))
			.def("teleport_entity",			(void(*) (CALifeSimulator*, CSE_Abstract*, Fvector&, u32, GameGraph::_GRAPH_ID))(teleport_entity))
			.def("change_actor_level",		&change_actor_level)
			.def("create_anomaly",			&CALifeSimulator__spawn_anom)
			.def("create_anomaly",			&CALifeSimulator__spawn_anom_sid)
			.def("switch_offline",			&script_switch_to_offline)
			.def("switch_online",			&script_switch_to_online)
			.def("force_entity_update",		&force_entity_update)
			.def("create",					(CSE_Abstract *(*) (CALifeSimulator *, LPCSTR, ALife::_STORY_ID, const Fvector &, u32, GameGraph::_GRAPH_ID))(CALifeSimulator__spawn_item3))
			.def("create",					(CSE_Abstract *(*) (CALifeSimulator *, LPCSTR, const Fvector &, const Fvector &, u32, GameGraph::_GRAPH_ID, ALife::_STORY_ID))(CALifeSimulator__spawn_item4))
			.def("create_restrictor",		&CALifeSimulator__spawn_restrictor)
			.def("store",					&get_store)
#ifdef USE_TIMERS_MANAGER
			.def("timer_manager",			&get_timers_manager)
#endif
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
			make_string<const char*>(
				"0x%08x name[%s] name_replace[%s]",
				*(int*)&_state,
				(*I).second->data()->object().name(),
				(*I).second->data()->object().name_replace()
			)
		);
	}
}
#endif //DEBUG