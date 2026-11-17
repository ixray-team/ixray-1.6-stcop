#include "StdAfx.h"
#include "pch_script.h"
#include "map_manager.h"
#include "alife_registry_wrappers.h"
#include "InventoryOwner.h"
#include "Level.h"
#include "Actor.h"
#include "relation_registry.h"
#include "GameObject.h"
#include "map_location.h"
#include "GameTask.h"
#include "GameTaskDefs.h"
#include "../xrEngine/string_table.h"
#include "GametaskManager.h"
#include "xrServer.h"
#include "game_object_space.h"
#include "../xrScripts/script_callback_ex.h"

struct FindLocationBySpotID
{
	shared_str	spot_id;
	ALife::_OBJECT_ID			object_id;
	FindLocationBySpotID(const shared_str& s, ALife::_OBJECT_ID id):spot_id(s),object_id(id){}
	bool operator () (const SLocationKey& key){
		return (spot_id==key.spot_type)&&(object_id==key.object_id);
	}
};

struct FindLocationByID
{
	ALife::_OBJECT_ID			object_id;
	FindLocationByID(ALife::_OBJECT_ID id):object_id(id){}
	bool operator () (const SLocationKey& key){
		return (object_id==key.object_id);
	}
};

struct FindLocation
{
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
		location = new CMapLocation(*spot_type, object_id, true);
	}
	else
	{
		location = new CMapLocation(*spot_type, object_id);
	}

	location->load	(stream);
}

void SLocationKey::destroy()
{
	if (location && location->IsUserDefined())
		Level().Server->FreeID(object_id, 0);

	delete_data(location);
}

ISaveObject& operator<<(ISaveObject& Object, SLocationKey& Data)
{
	BEGIN_CHUNK(Object, "SLocationKey")
	{
		Object << Data.object_id << Data.spot_type;
		if (Object.IsSave())
		{
			bool Value = Data.location->IsUserDefined();
			Object << Value;
		}
		else
		{
			bool Value;
			Object << Value;
			if (Value)
			{
				Level().Server->PerformIDgen(Data.object_id);
				Data.location = new CMapLocation(*Data.spot_type, Data.object_id, true);
			} else
			{
				Data.location = new CMapLocation(*Data.spot_type, Data.object_id);
			}
		}
		Data.location->serialize(Object);
	}
	return Object;
}

void CMapLocationRegistry::save(IWriter &stream)
{
	stream.w_u32((u32)objects().size());
	for (auto& vec_elems : m_objects)
	{
		u32 size = 0;
		for (auto& elem : vec_elems.second)
		{
			VERIFY(elem.location);
			if (elem.location->Serializable())
			{
				++size;
			}
		}
		stream.w(&vec_elems.first,sizeof(vec_elems.first));
		stream.w_u32(size);
		for (auto& elem : vec_elems.second)
		{
			if (elem.location->Serializable())
			{
				elem.save(stream);
			}
		}
	}
}


CMapManager::CMapManager()
{
	m_locations_wrapper = new CMapLocationWrapper();
	m_locations_wrapper->registry().init(1);
	m_locations = nullptr;
	m_activeUserNavigationLocationId = ALife::INVALID_OBJECT_ID;
	SpotFilterService.LoadCategories();
}

CMapManager::~CMapManager()
{
	delete_data		(m_deffered_destroy_queue); //from prev frame
	delete_data		(m_locations_wrapper);
}

CMapLocation* CMapManager::AddMapLocation(const shared_str& spot_type, ALife::_OBJECT_ID id)
{
	if (HasMapLocation(spot_type, id))
		return GetMapLocation(spot_type, id);

	CMapLocation* l = new CMapLocation(spot_type.c_str(), id);
	Locations().emplace_back(spot_type, id);
	Locations().back().location = l;
	ApplySpotFilterOnAdd(*l);
	if (g_actor)
		Actor()->callback(GameObject::eMapLocationAdded)(spot_type.c_str(), id);

	return l;
}

CMapLocation* CMapManager::AddRelationLocation(CInventoryOwner* pInvOwner)
{
	if(!Level().CurrentViewEntity())return nullptr;

	ALife::ERelationType relation = ALife::eRelationTypeFriend;
	CInventoryOwner* pActor = Level().CurrentViewEntity()->cast_inventory_owner();
	relation =  RELATION_REGISTRY().GetRelationType(pInvOwner, pActor);
	shared_str sname = RELATION_REGISTRY().GetSpotName(relation);

	CEntityAlive* pEntAlive = pInvOwner != nullptr ? pInvOwner->cast_entity_alive() : nullptr;
	if( !pEntAlive->g_Alive() ) sname = "deadbody_location";

	CMapLocation* l;
	if (!HasMapLocation(sname, pInvOwner->object_id()))
	{
		l = new CRelationMapLocation(sname, pInvOwner->object_id(), pActor->object_id());
		Locations().emplace_back(sname, pInvOwner->object_id());
		Locations().back().location = l;
	}
	else
	{
		l = GetMapLocation(sname, pInvOwner->object_id());
	}
	return l;
}

void CMapManager::RemoveRelationLocation(CInventoryOwner* pInvOwner)
{
	for (int t = ALife::eRelationTypeFriend; t < ALife::eRelationTypeLast; ++t)
	{
		ALife::ERelationType tt = (ALife::ERelationType)t;
		Level().MapManager().RemoveMapLocation(RELATION_REGISTRY().GetSpotName(tt), pInvOwner->object_id());
	}
	Level().MapManager().RemoveMapLocation("deadbody_location", pInvOwner->object_id());
}

CMapLocation* CMapManager::AddUserLocation(const shared_str& spot_type, const shared_str& level_name, Fvector position)
{
	ALife::_OBJECT_ID _id = Level().Server->PerformIDgen(ALife::INVALID_OBJECT_ID);

	CMapLocation* l = new CMapLocation(spot_type.c_str(), _id, true);
	l->InitUserSpot(level_name, position);

	Locations().emplace_back(spot_type, _id);
	Locations().back().location = l;
	ApplySpotFilterOnAdd(*l);

	return l;
}

shared_str CMapManager::UserSpotType()
{
	shared_str SpotType = pSettings->read_if_exists<str_c>("user_spots", "spot_type", "treasure");
	if (pGameGlobals->section_exist("pda_map") && pGameGlobals->line_exist("pda_map", "hand_spot_icon_xml_section"))
	{
		const shared_str OverrideSpotName = pGameGlobals->r_string("pda_map", "hand_spot_icon_xml_section");
		if (OverrideSpotName.c_str() != nullptr)
		{
			SpotType = OverrideSpotName;
		}
	}
	return SpotType;
}

void CMapManager::GetUserLocations(xr_vector<CMapLocation*>& Result)
{
	Result.clear();
	for (const SLocationKey& Key : Locations())
	{
		if (Key.location && Key.location->IsUserDefined())
		{
			Result.push_back(Key.location);
		}
	}
	std::sort(Result.begin(), Result.end(), [](const CMapLocation* Left, const CMapLocation* Right)
	{
		return Left->ObjectID() < Right->ObjectID();
	});
}

u32 CMapManager::GetPersonalSpotLimit() const
{
	if (!pGameGlobals || !pGameGlobals->line_exist("pda_map", "personal_spot_limit"))
	{
		return 0;
	}
	const int Limit = pGameGlobals->r_s32("pda_map", "personal_spot_limit");
	return Limit > 0 ? static_cast<u32>(Limit) : 0;
}

u32 CMapManager::GetPersonalSpotCount()
{
	xr_vector<CMapLocation*> UserLocations;
	GetUserLocations(UserLocations);
	return static_cast<u32>(UserLocations.size());
}

bool CMapManager::CanAddPersonalSpot()
{
	const u32 Limit = GetPersonalSpotLimit();
	return Limit == 0 || GetPersonalSpotCount() < Limit;
}

xr_string CMapManager::MakeDefaultPersonalSpotName(const shared_str& LevelName)
{
	string256 Name;
	xr_sprintf(Name, "%s %u", g_pStringTable->translate("st_personal_spot_quick_hint").c_str(), GetPersonalSpotCount() + 1);

	xr_string Result = Name;
	if (LevelName.size())
	{
		Result += " \xB7 ";
		Result += g_pStringTable->translate(LevelName).c_str();
	}
	return Result;
}

CMapLocation* CMapManager::FindPersonalSpot(ALife::_OBJECT_ID Id)
{
	xr_vector<CMapLocation*> UserLocations;
	GetUserLocations(UserLocations);
	for (CMapLocation* Location : UserLocations)
	{
		if (Location->ObjectID() == Id)
		{
			return Location;
		}
	}
	return nullptr;
}

CMapLocation* CMapManager::AddPersonalSpot(const shared_str& LevelName, const Fvector& Position, const char* Hint)
{
	if (!CanAddPersonalSpot())
	{
		return nullptr;
	}

	CMapLocation* Location = AddUserLocation(UserSpotType(), LevelName, Position);
	if (!Location)
	{
		return nullptr;
	}

	Location->SetHint(Hint ? Hint : "");
	Location->SetSerializable(true);
	return Location;
}

bool CMapManager::RemovePersonalSpot(CMapLocation* Location)
{
	if (!Location || !Location->IsUserDefined())
	{
		return false;
	}

	RemoveMapLocation(Location);
	return true;
}

bool CMapManager::TogglePersonalSpotNavigation(CMapLocation* Location)
{
	if (!Location || IsUserNavigationLocation(Location))
	{
		if (!HasActiveUserNavigationLocation())
		{
			return false;
		}
		ClearActiveUserNavigationLocation();
		return true;
	}

	if (!Location->IsUserDefined())
	{
		return false;
	}

	SetActiveUserNavigationLocation(Location);
	return true;
}

CMapLocation* CMapManager::GetAdjacentPersonalSpot(ALife::_OBJECT_ID CurrentId, bool Next)
{
	xr_vector<CMapLocation*> UserLocations;
	GetUserLocations(UserLocations);
	if (UserLocations.empty())
	{
		return nullptr;
	}

	const auto Found = std::find_if(UserLocations.begin(), UserLocations.end(), [CurrentId](const CMapLocation* Location)
	{
		return Location->ObjectID() == CurrentId;
	});

	const size_t Count = UserLocations.size();
	if (Found == UserLocations.end())
	{
		return UserLocations[Next ? 0 : Count - 1];
	}

	const size_t Current = static_cast<size_t>(std::distance(UserLocations.begin(), Found));
	return UserLocations[Next ? (Current + 1) % Count : (Current + Count - 1) % Count];
}

void CMapManager::Destroy(CMapLocation* ml)
{
	m_deffered_destroy_queue.push_back(ml);
}

void CMapManager::RemoveMapLocation(const shared_str& spot_type, ALife::_OBJECT_ID id)
{
	FindLocationBySpotID key(spot_type, id);
	Locations_it it = std::find_if(Locations().begin(),Locations().end(),key);
	if( it!=Locations().end() )
	{
		if (it->location && IsUserNavigationLocation(it->location))
		{
			ClearActiveUserNavigationLocation();
		}

		Level().GameTaskManager()->MapLocationRelcase(it->location);

		Destroy(it->location);
		Locations().erase(it);
	}
}

void CMapManager::RemoveMapLocationByObjectID(ALife::_OBJECT_ID id) //call on destroy object
{
	if (m_activeUserNavigationLocationId == id)
	{
		ClearActiveUserNavigationLocation();
	}

	FindLocationByID key(id);
	Locations_it it = std::ranges::find_if(Locations(), key);
	while( it!= Locations().end() )
	{
		Level().GameTaskManager()->MapLocationRelcase(it->location);

		Destroy(it->location);
		Locations().erase(it);

		it = std::ranges::find_if(Locations(), key);
	}
}

void CMapManager::RemoveMapLocation(CMapLocation* ml)
{
	if (ml && IsUserNavigationLocation(ml))
		ClearActiveUserNavigationLocation();

	FindLocation key(ml);

	Locations_it it = std::ranges::find_if(Locations(), key);
	if(it!=Locations().end())
	{
		Level().GameTaskManager()->MapLocationRelcase(it->location);

		Destroy(it->location);
		Locations().erase(it);
	}

}

bool CMapManager::GetMapLocationsForObject(ALife::_OBJECT_ID id, xr_vector<CMapLocation*>& res)
{
	res.resize(0);
	for (auto& elem : Locations())
	{
		if (elem.actual && elem.object_id == id)
		{
			res.push_back(elem.location);
		}
	}
	return !res.empty();
}

bool CMapManager::HasMapLocation(const shared_str& spot_type, ALife::_OBJECT_ID id)
{
	CMapLocation* l = GetMapLocation(spot_type, id);
	
	return (l!=nullptr);
}

CMapLocation* CMapManager::GetMapLocation(const shared_str& spot_type, ALife::_OBJECT_ID id)
{
	FindLocationBySpotID key(spot_type, id);
	Locations_it it = std::ranges::find_if(Locations(), key);
	if(it!=Locations().end())
	{
		return it->location;
	}
	
	return nullptr;
}

SQuestCompassTarget CMapManager::GetActiveQuestCompassTarget()
{
	SQuestCompassTarget Result;
	if (!Level().GameTaskManager())
		return Result;

	CGameTask* storyTask = Level().GameTaskManager()->ActiveTask(eTaskTypeStoryline);
	CGameTask* additionalTask = Level().GameTaskManager()->ActiveTask(eTaskTypeAdditional);
	if (storyTask && storyTask->HasActiveMapTarget())
	{
		const SGameTaskObjective& objective = storyTask->Objective(storyTask->ActiveObjectiveIdx());
		if (CMapLocation* activeLoc = GetMapLocation(objective.m_map_location, objective.m_map_object_id))
		{
			Result.Location = activeLoc;
			Result.Type = eTaskTypeStoryline;
			return Result;
		}
	}
	if (additionalTask && additionalTask->HasActiveMapTarget())
	{
		const SGameTaskObjective& objective = additionalTask->Objective(additionalTask->ActiveObjectiveIdx());
		if (CMapLocation* activeLoc = GetMapLocation(objective.m_map_location, objective.m_map_object_id))
		{
			Result.Location = activeLoc;
			Result.Type = eTaskTypeAdditional;
			return Result;
		}
	}
	return Result;
}

CMapLocation* CMapManager::GetActiveTaskCompassLocation()
{
	CMapLocation* userNavLocation = GetActiveUserNavigationLocation();
	if (userNavLocation)
		return userNavLocation;

	return GetActiveQuestCompassTarget().Location;
}

CMapLocation* CMapManager::GetActiveUserNavigationLocation()
{
	if (m_activeUserNavigationLocationId == ALife::INVALID_OBJECT_ID)
	{
		return nullptr;
	}

	FindLocationByID key(m_activeUserNavigationLocationId);
	Locations_it it = std::ranges::find_if(Locations(), key);
	if (it != Locations().end() && it->location && it->location->IsUserDefined())
	{
		return it->location;
	}

	return nullptr;
}

void CMapManager::SetActiveUserNavigationLocation(CMapLocation* ml)
{
	ClearActiveUserNavigationLocation();
	if (!ml || !ml->IsUserDefined())
	{
		return;
	}

	DisableAllPointers();
	m_activeUserNavigationLocationId = ml->ObjectID();
	ml->EnablePointer();
}

void CMapManager::ClearActiveUserNavigationLocation()
{
	if (m_activeUserNavigationLocationId != ALife::INVALID_OBJECT_ID)
	{
		FindLocationByID key(m_activeUserNavigationLocationId);
		Locations_it it = std::ranges::find_if(Locations(), key);
		if (it != Locations().end() && it->location && it->location->IsUserDefined())
		{
			it->location->DisablePointer();
		}
	}

	m_activeUserNavigationLocationId = ALife::INVALID_OBJECT_ID;
}

bool CMapManager::HasActiveUserNavigationLocation()
{
	return GetActiveUserNavigationLocation() != nullptr;
}

bool CMapManager::IsUserNavigationLocation(const CMapLocation* ml)
{
	return ml && ml->IsUserDefined() && m_activeUserNavigationLocationId == ml->ObjectID();
}

void CMapManager::GetMapLocations(const shared_str& spot_type, ALife::_OBJECT_ID id, xr_vector<CMapLocation*>& res)
{
	FindLocationBySpotID key(spot_type, id);
	Locations_it it = std::ranges::find_if(Locations(), key);
	
	while( it!=Locations().end() )
	{
		res.push_back(it->location);
		it = std::find_if(++it, Locations().end(), key);
	}
}

void CMapManager::Update()
{
	PROF_EVENT("Map: Update");
	if (Device.IsEditorMode())
	{
		return;
	}

	xrCriticalSectionGuard guard(UpdateCS);
	delete_data(m_deffered_destroy_queue); //from prev frame

	if (m_activeUserNavigationLocationId != ALife::INVALID_OBJECT_ID)
	{
		FindLocationByID key(m_activeUserNavigationLocationId);
		Locations_it activeIt = std::ranges::find_if(Locations(), key);
		if (activeIt == Locations().end() || !activeIt->location || !activeIt->location->IsUserDefined())
		{
			m_activeUserNavigationLocationId = ALife::INVALID_OBJECT_ID;
		}
	}

	Locations_it it			= Locations().begin();
	Locations_it it_e		= Locations().end();

	for(u32 idx=0; it!=it_e;++it,++idx)
	{
		bool bForce		= Device.dwFrame%3 == idx%3;
		it->actual	= it->location->Update();

		if(it->actual && bForce)
		{
			it->location->CalcPosition();
		}
	}

	std::sort(Locations().begin(), Locations().end());

	while ((!Locations().empty()) && (!Locations().back().actual))
	{
		Level().GameTaskManager()->MapLocationRelcase(Locations().back().location);

		Destroy(Locations().back().location);
		Locations().pop_back();
	}
}

void CMapManager::DisableAllPointers()
{
	for (auto& elem : Locations())
	{
		elem.location->DisablePointer();
	}
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

void CMapManager::RegisterSpotCategory(const shared_str& Id, const char* Patterns)
{
	xrCriticalSectionGuard Guard(UpdateCS);
	if (SpotFilterService.RegisterCategory(Id, Patterns))
	{
		ApplySpotFilter();
	}
}

void CMapManager::SetSpotCategoryHidden(const shared_str& Id, bool Hidden)
{
	xrCriticalSectionGuard Guard(UpdateCS);
	if (SpotFilterService.SetCategoryHidden(Id, Hidden))
	{
		ApplySpotFilter();
	}
}

void CMapManager::ApplySpotFilter()
{
	xrCriticalSectionGuard Guard(UpdateCS);
	for (const SLocationKey& Key : Locations())
	{
		CMapLocation* Location = Key.location;
		if (!Location)
		{
			continue;
		}

		switch (SpotFilterService.Evaluate(*Location))
		{
		case EMapSpotFilter::Visible:
			Location->EnableSpot();
			break;
		case EMapSpotFilter::Hidden:
			Location->DisableSpot();
			break;
		default:
			break;
		}
	}
}

void CMapManager::SaveSpotFilter(NET_Packet& Packet) const
{
	SpotFilterService.Save(Packet);
}

void CMapManager::LoadSpotFilter(IReader& Reader)
{
	xrCriticalSectionGuard Guard(UpdateCS);
	if (SpotFilterService.Load(Reader))
	{
		ApplySpotFilter();
	}
}

void CMapManager::ApplySpotFilterOnAdd(CMapLocation& Location) const
{
	if (IsSpotHidden(Location))
	{
		Location.DisableSpot();
	}
}

void CMapManager::OnObjectDestroyNotify(ALife::_OBJECT_ID id)
{
	RemoveMapLocationByObjectID(id);
}

#ifdef DEBUG
void CMapManager::Dump						()
{
	Msg("begin of map_locations dump");
	for (auto& elem : Locations())
	{
		Msg("spot_type=[%s] object_id=[%d]",*elem.spot_type, elem.object_id);
		elem.location->Dump();
	}
	Msg("end of map_locations dump");
}
#endif

using namespace luabind;
void CMapManager::MapLocationsForEach(const char* spot_type, ALife::_OBJECT_ID id, const luabind::functor<bool>& functor)
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
	for (auto& elem : Locations())
	{
		if (functor(elem.location, elem.spot_type.c_str()) == true)
		{
			return;
		}
	}
}

#include "map_location.h"

#pragma optimize("s",on)
namespace MapManagerScript
{
CMapLocation* AddPersonalSpot(CMapManager* MapManager, const char* LevelName, Fvector Position, const char* Hint)
{
	return MapManager->AddPersonalSpot(LevelName, Position, Hint);
}

CMapLocation* GetAdjacentPersonalSpot(CMapManager* MapManager, ALife::_OBJECT_ID CurrentId, bool Next)
{
	return MapManager->GetAdjacentPersonalSpot(CurrentId, Next);
}

void SetSpotCategoryHidden(CMapManager* MapManager, const char* Id, bool Hidden)
{
	MapManager->SetSpotCategoryHidden(Id, Hidden);
}

bool IsSpotCategoryHidden(CMapManager* MapManager, const char* Id)
{
	return MapManager->IsSpotCategoryHidden(Id);
}

void RegisterSpotCategory(CMapManager* MapManager, const char* Id, const char* Patterns)
{
	MapManager->RegisterSpotCategory(Id, Patterns);
}
} // namespace MapManagerScript

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
				.def("AllLocationsForEach", &CMapManager::AllLocationsForEach)
				.def("AddPersonalSpot", &MapManagerScript::AddPersonalSpot)
				.def("RemovePersonalSpot", &CMapManager::RemovePersonalSpot)
				.def("TogglePersonalSpotNavigation", &CMapManager::TogglePersonalSpotNavigation)
				.def("SetActiveUserNavigationLocation", &CMapManager::SetActiveUserNavigationLocation)
				.def("ClearActiveUserNavigationLocation", &CMapManager::ClearActiveUserNavigationLocation)
				.def("GetActiveUserNavigationLocation", &CMapManager::GetActiveUserNavigationLocation)
				.def("GetAdjacentPersonalSpot", &MapManagerScript::GetAdjacentPersonalSpot)
				.def("GetPersonalSpotLimit", &CMapManager::GetPersonalSpotLimit)
				.def("GetPersonalSpotCount", &CMapManager::GetPersonalSpotCount)
				.def("CanAddPersonalSpot", &CMapManager::CanAddPersonalSpot)
				.def("RegisterSpotCategory", &MapManagerScript::RegisterSpotCategory)
				.def("SetSpotCategoryHidden", &MapManagerScript::SetSpotCategoryHidden)
				.def("IsSpotCategoryHidden", &MapManagerScript::IsSpotCategoryHidden),

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
				.def("GetLevelName", &CMapLocation::GetLPLevelName)
				.def("GetPosition", &CMapLocation::GetPosition)
				.def("ObjectID", &CMapLocation::ObjectID)
				.def("GetLastPosition", &CMapLocation::GetLastPosition)
			//.def("GetOwnerTaskID", &CMapLocation::GetOwnerTaskID)
	];
}