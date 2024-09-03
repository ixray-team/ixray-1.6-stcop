#include "stdafx.h"
#include "pch_script.h"
#include "map_manager.h"
#include "alife_registry_wrappers.h"
#include "inventoryowner.h"
#include "Level.h"
#include "Actor.h"
#include "relation_registry.h"
#include "GameObject.h"
#include "map_location.h"
#include "GameTaskManager.h"
#include "xrServer.h"
#include "game_object_space.h"

struct FindLocationBySpotID{
	shared_str	spot_id;
	u16			object_id;
	FindLocationBySpotID(const shared_str& s, u16 id):spot_id(s),object_id(id){}
	bool operator () (const SLocationKey& key){
		return (spot_id==key.spot_type)&&(object_id==key.object_id);
	}
};
struct FindLocationByID{
	u16			object_id;
	FindLocationByID(u16 id):object_id(id){}
	bool operator () (const SLocationKey& key){
		return (object_id==key.object_id);
	}
};

struct FindLocation{
	CMapLocation*			ml;
	FindLocation(CMapLocation* m):ml(m){}
	bool operator () (const SLocationKey& key){
		return (ml==key.location);
	}
};

void SLocationKey::save(IWriter &stream)
{
	stream.w		(&object_id,sizeof(object_id));

	stream.w_stringZ(spot_type);
	stream.w_u8(location->IsUserDefined() ? 1 : 0);
	stream.w_u8		(0);
	location->save	(stream);
}
	
void SLocationKey::load(IReader &stream)
{
	stream.r		(&object_id,sizeof(object_id));

	stream.r_stringZ(spot_type);
	u8	bUserDefined = stream.r_u8();
	stream.r_u8		();

	if (bUserDefined)
	{
		Level().Server->PerformIDgen(object_id);
		location = xr_new<CMapLocation>(*spot_type, object_id, true);
	}
	else
	{
		location = xr_new<CMapLocation>(*spot_type, object_id);
	}

	location  = new CMapLocation(*spot_type, object_id);

	location->load	(stream);
}

void SLocationKey::destroy()
{
	if (location && location->IsUserDefined())
		Level().Server->FreeID(object_id, 0);

	delete_data(location);
}

void CMapLocationRegistry::save(IWriter &stream)
{
	stream.w_u32			((u32)objects().size());
	iterator				I = m_objects.begin();
	iterator				E = m_objects.end();
	for ( ; I != E; ++I) {
		u32					size = 0;
		Locations::iterator	i = (*I).second.begin();
		Locations::iterator	e = (*I).second.end();
		for ( ; i != e; ++i) {
			VERIFY			((*i).location);
			if ((*i).location->Serializable())
				++size;
		}
		stream.w			(&(*I).first,sizeof((*I).first));
		stream.w_u32		(size);
		i					= (*I).second.begin();
		for ( ; i != e; ++i)
			if ((*i).location->Serializable())
				(*i).save	(stream);
	}
}


CMapManager::CMapManager()
{
	m_locations_wrapper = new CMapLocationWrapper();
	m_locations_wrapper->registry().init(1);
	m_locations = nullptr;
}

CMapManager::~CMapManager()
{
	delete_data		(m_deffered_destroy_queue); //from prev frame
	delete_data		(m_locations_wrapper);
}

CMapLocation* CMapManager::AddMapLocation(const shared_str& spot_type, u16 id)
{
	CMapLocation* l = new CMapLocation(spot_type.c_str(), id);
	Locations().push_back( SLocationKey(spot_type, id) );
	Locations().back().location = l;
	if (g_actor)
		Actor()->callback(GameObject::eMapLocationAdded)(spot_type.c_str(), id);

	return l;
}

CMapLocation* CMapManager::AddRelationLocation(CInventoryOwner* pInvOwner)
{
	if(!Level().CurrentViewEntity())return nullptr;

	ALife::ERelationType relation = ALife::eRelationTypeFriend;
	CInventoryOwner* pActor = smart_cast<CInventoryOwner*>(Level().CurrentViewEntity());
	relation =  RELATION_REGISTRY().GetRelationType(pInvOwner, pActor);
	shared_str sname = RELATION_REGISTRY().GetSpotName(relation);

	CEntityAlive* pEntAlive = smart_cast<CEntityAlive*>(pInvOwner);
	if( !pEntAlive->g_Alive() ) sname = "deadbody_location";


	R_ASSERT(!HasMapLocation(sname, pInvOwner->object_id()));
	CMapLocation* l = new CRelationMapLocation(sname, pInvOwner->object_id(), pActor->object_id());
	Locations().push_back( SLocationKey(sname, pInvOwner->object_id()) );
	Locations().back().location = l;
	return l;
}

CMapLocation* CMapManager::AddUserLocation(const shared_str& spot_type, const shared_str& level_name, Fvector position)
{
	u16 _id = Level().Server->PerformIDgen(0xffff);

	CMapLocation* l = xr_new<CMapLocation>(spot_type.c_str(), _id, true);
	l->InitUserSpot(level_name, position);

	Locations().push_back(SLocationKey(spot_type, _id));
	Locations().back().location = l;

	return l;
}

void CMapManager::Destroy(CMapLocation* ml)
{
	m_deffered_destroy_queue.push_back(ml);
}

void CMapManager::RemoveMapLocation(const shared_str& spot_type, u16 id)
{
	FindLocationBySpotID key(spot_type, id);
	Locations_it it = std::find_if(Locations().begin(),Locations().end(),key);
	if( it!=Locations().end() )
	{
		Level().GameTaskManager().MapLocationRelcase((*it).location);

		Destroy					((*it).location);
		Locations().erase		(it);
	}
}

void CMapManager::RemoveMapLocationByObjectID(u16 id) //call on destroy object
{
	FindLocationByID key(id);
	Locations_it it = std::find_if(Locations().begin(), Locations().end(), key);
	while( it!= Locations().end() )
	{
		Level().GameTaskManager().MapLocationRelcase((*it).location);

		Destroy					((*it).location);
		Locations().erase		(it);

		it = std::find_if(Locations().begin(), Locations().end(), key);
	}
}

void CMapManager::RemoveMapLocation(CMapLocation* ml)
{
	FindLocation key(ml);

	Locations_it it = std::find_if(Locations().begin(), Locations().end(), key);
	if( it!=Locations().end() )
	{
		Level().GameTaskManager().MapLocationRelcase((*it).location);

		Destroy					((*it).location);
		Locations().erase		(it);
	}

}

bool CMapManager::GetMapLocationsForObject(u16 id, xr_vector<CMapLocation*>& res)
{
	res.resize(0);
	Locations_it it			= Locations().begin();
	Locations_it it_e		= Locations().end();
	for(; it!=it_e;++it)
	{
		if((*it).actual && (*it).object_id==id)
			res.push_back((*it).location);
	}
	return (res.size()!=0);
}

bool CMapManager::HasMapLocation(const shared_str& spot_type, u16 id)
{
	CMapLocation* l = GetMapLocation(spot_type, id);
	
	return (l!=nullptr);
}

CMapLocation* CMapManager::GetMapLocation(const shared_str& spot_type, u16 id)
{
	FindLocationBySpotID key(spot_type, id);
	Locations_it it = std::find_if(Locations().begin(), Locations().end(), key);
	if( it!=Locations().end() )
		return (*it).location;
	
	return 0;
}

void CMapManager::GetMapLocations(const shared_str& spot_type, u16 id, xr_vector<CMapLocation*>& res)
{
	FindLocationBySpotID key(spot_type, id);
	Locations_it it = std::find_if(Locations().begin(), Locations().end(), key);
	
	while( it!=Locations().end() )
	{
		res.push_back((*it).location);
		it = std::find_if(++it, Locations().end(), key);
	}
}

void CMapManager::Update()
{
	PROF_EVENT("Map: Update");
	delete_data(m_deffered_destroy_queue); //from prev frame

	Locations_it it			= Locations().begin();
	Locations_it it_e		= Locations().end();

	for(u32 idx=0; it!=it_e;++it,++idx)
	{
		bool bForce		= Device.dwFrame%3 == idx%3;
		(*it).actual	= (*it).location->Update();

		if((*it).actual && bForce)
			(*it).location->CalcPosition();
	}
	std::sort( Locations().begin(),Locations().end() );

	while( (!Locations().empty())&&(!Locations().back().actual) )
	{
		Level().GameTaskManager().MapLocationRelcase(Locations().back().location);

		Destroy(Locations().back().location);
		Locations().pop_back();
	}
}

void CMapManager::DisableAllPointers()
{
	Locations_it it = Locations().begin();
	Locations_it it_e = Locations().end();

	for(; it!=it_e;++it)
		(*it).location->DisablePointer	();
}


Locations&	CMapManager::Locations	() 
{
	if(!m_locations)
	{
		m_locations = &m_locations_wrapper->registry().objects();
#ifdef DEBUG
		Msg("m_locations size=%d",m_locations->size());
#endif // #ifdef DEBUG
	}
	return *m_locations;
}

void CMapManager::OnObjectDestroyNotify(u16 id)
{
	RemoveMapLocationByObjectID(id);
}

#ifdef DEBUG
void CMapManager::Dump						()
{
	Msg("begin of map_locations dump");
	Locations_it it = Locations().begin();
	Locations_it it_e = Locations().end();
	for(; it!=it_e;++it)
	{
		Msg("spot_type=[%s] object_id=[%d]",*((*it).spot_type), (*it).object_id);
		(*it).location->Dump();
	}

	Msg("end of map_locations dump");
}
#endif

using namespace luabind;
void CMapManager::MapLocationsForEach(LPCSTR spot_type, u16 id, const luabind::functor<bool>& functor)
{
	xr_vector<CMapLocation*> res;
	Level().MapManager().GetMapLocations(spot_type, id, res);
	xr_vector<CMapLocation*>::iterator it = res.begin();
	xr_vector<CMapLocation*>::iterator it_e = res.end();
	for (; it != it_e; ++it)
	{
		CMapLocation* ml = *it;
		if (functor(ml) == true)
			return;
	}
}

void CMapManager::AllLocationsForEach(const luabind::functor<bool>& functor)
{
	Locations_it it = Locations().begin();
	Locations_it it_e = Locations().end();

	for (; it != it_e; ++it)
	{
		if (functor((*it).location) == true)
			return;
	}
}

#include "map_location.h"

#pragma optimize("s",on)
void CMapManager::script_register(lua_State* L)
{
	module(L)
	[
		class_<CMapManager>("CMapManager")
			.def(constructor<>())
			//.def("AddMapLocation", &CMapManager::AddMapLocation_script)
			//.def("AddUserLocation", &CMapManager::AddUserLocation_script)
			//.def("RemoveMapLocation", &CMapManager::RemoveMapLocation_script)
			//.def("HasMapLocation", &CMapManager::HasMapLocation_script)
			//.def("GetMapLocation", &CMapManager::GetMapLocation_script)
				.def("RemoveMapLocationByObjectID", &CMapManager::RemoveMapLocationByObjectID)
				.def("RemoveMapLocation", (void (CMapManager::*)(CMapLocation*)) & CMapManager::RemoveMapLocation)
			//.def("RemoveMapLocation", &CMapManager::RemoveMapLocation_script)
				.def("DisableAllPointers", &CMapManager::DisableAllPointers)
				.def("MapLocationsForEach", &CMapManager::MapLocationsForEach)
				.def("AllLocationsForEach", &CMapManager::AllLocationsForEach),

			class_<CMapLocation>("CMapLocation")
			//.def(constructor<>())
				.def("HintEnabled", &CMapLocation::HintEnabled)
				.def("GetHint", &CMapLocation::GetHint)
				.def("SetHint", &CMapLocation::SetHint)
				.def("PointerEnabled", &CMapLocation::PointerEnabled)
				.def("EnablePointer", &CMapLocation::EnablePointer)
				.def("DisablePointer", &CMapLocation::DisablePointer)
				.def("GetType", &CMapLocation::GetType)
				.def("SpotSize", &CMapLocation::SpotSize)
				.def("IsUserDefined", &CMapLocation::IsUserDefined)
				.def("SetUserDefinedFlag", &CMapLocation::SetUserDefinedFlag)
				.def("HighlightSpot", &CMapLocation::HighlightSpot)
				.def("Collidable", &CMapLocation::Collidable)
				.def("SpotEnabled", &CMapLocation::SpotEnabled)
				.def("EnableSpot", &CMapLocation::EnableSpot)
				.def("DisableSpot", &CMapLocation::DisableSpot)
				.def("GetLevelName", &CMapLocation::GetLevelName)
				.def("GetPosition", &CMapLocation::GetPosition)
				.def("ObjectID", &CMapLocation::ObjectID)
				.def("GetLastPosition", &CMapLocation::GetLastPosition)
			//.def("GetOwnerTaskID", &CMapLocation::GetOwnerTaskID)
	];
}