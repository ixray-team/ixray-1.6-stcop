#pragma once
#include "object_interfaces.h"
#include "map_location_defs.h"
#include "GameTaskDefs.h"
#include "MapSpotFilter.h"

class CMapLocationWrapper;
class CInventoryOwner;
class CMapLocation;
class NET_Packet;
class IReader;

struct SQuestCompassTarget
{
	CMapLocation* Location = nullptr;
	ETaskType Type = eTaskTypeDummy;
};

class CMapManager
{
	CMapLocationWrapper*	m_locations_wrapper;
	Locations*				m_locations;
	xr_vector<CMapLocation*> m_deffered_destroy_queue;
	ALife::_OBJECT_ID m_activeUserNavigationLocationId;
public:

							CMapManager					();
							~CMapManager				();
	void			Update						();
	/*ICF */Locations&		Locations					();//{return *m_locations;}
	CMapLocation*			AddMapLocation				(const shared_str& spot_type, ALife::_OBJECT_ID id);
	CMapLocation*			AddRelationLocation			(CInventoryOwner* pInvOwner);
	void					RemoveRelationLocation		(CInventoryOwner* pInvOwner);	
	CMapLocation*			AddUserLocation				(const shared_str& spot_type, const shared_str& level_name, Fvector position);
	static shared_str		UserSpotType				();
	void					GetUserLocations			(xr_vector<CMapLocation*>& Result);
	CMapLocation*			AddPersonalSpot				(const shared_str& LevelName, const Fvector& Position, const char* Hint);
	u32						GetPersonalSpotLimit		() const;
	u32						GetPersonalSpotCount		();
	bool					CanAddPersonalSpot			();
	xr_string				MakeDefaultPersonalSpotName	(const shared_str& LevelName);
	CMapLocation*			FindPersonalSpot			(ALife::_OBJECT_ID Id);
	bool					RemovePersonalSpot			(CMapLocation* Location);
	bool					TogglePersonalSpotNavigation(CMapLocation* Location);
	CMapLocation*			GetAdjacentPersonalSpot		(ALife::_OBJECT_ID CurrentId, bool Next);
	void					RemoveMapLocation			(const shared_str& spot_type, ALife::_OBJECT_ID id);
	bool					HasMapLocation				(const shared_str& spot_type, ALife::_OBJECT_ID id);
	void					RemoveMapLocationByObjectID (ALife::_OBJECT_ID id); //call on destroy object
	void					RemoveMapLocation			(CMapLocation* ml);
	CMapLocation*			GetMapLocation				(const shared_str& spot_type, ALife::_OBJECT_ID id);
	CMapLocation*			GetActiveTaskCompassLocation	();
	SQuestCompassTarget		GetActiveQuestCompassTarget	();
	CMapLocation*			GetActiveUserNavigationLocation();
	void					SetActiveUserNavigationLocation(CMapLocation* ml);
	void					ClearActiveUserNavigationLocation();
	bool					HasActiveUserNavigationLocation();
	bool					IsUserNavigationLocation	(const CMapLocation* ml);
	void					GetMapLocations				(const shared_str& spot_type, ALife::_OBJECT_ID id, xr_vector<CMapLocation*>& res);
	void					DisableAllPointers			();
	bool					GetMapLocationsForObject	(ALife::_OBJECT_ID id, xr_vector<CMapLocation*>& res);
	void					OnObjectDestroyNotify		(ALife::_OBJECT_ID id);
	void					ResetStorage				() {m_locations = NULL;};
#ifdef DEBUG
	void					Dump						();
#endif
	void					Destroy						(CMapLocation*);

	const CMapSpotFilter&	SpotFilter					() const { return SpotFilterService; }
	void					RegisterSpotCategory		(const shared_str& Id, const char* Patterns);
	void					SetSpotCategoryHidden		(const shared_str& Id, bool Hidden);
	bool					IsSpotCategoryHidden		(const shared_str& Id) const { return SpotFilterService.IsCategoryHidden(Id); }
	EMapSpotFilter			GetSpotFilter				(const CMapLocation& Location) const { return SpotFilterService.Evaluate(Location); }
	bool					IsSpotHidden				(const CMapLocation& Location) const { return GetSpotFilter(Location) == EMapSpotFilter::Hidden; }
	void					ApplySpotFilter				();
	void					SaveSpotFilter				(NET_Packet& Packet) const;
	void					LoadSpotFilter				(IReader& Reader);

	xrCriticalSection		UpdateCS;

private:
	void					ApplySpotFilterOnAdd		(CMapLocation& Location) const;

	CMapSpotFilter			SpotFilterService;

public:

	void MapLocationsForEach(const char* spot_type, ALife::_OBJECT_ID id, const luabind::functor<bool>& functor);
	void AllLocationsForEach(const luabind::functor<bool>& functor);
	static void script_register(lua_State* L);
};
