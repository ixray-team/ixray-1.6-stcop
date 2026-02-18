#pragma once
#include "object_interfaces.h"
#include "map_location_defs.h"

class CMapLocationWrapper;
class CInventoryOwner;
class CMapLocation;

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
	void					RemoveMapLocation			(const shared_str& spot_type, ALife::_OBJECT_ID id);
	bool					HasMapLocation				(const shared_str& spot_type, ALife::_OBJECT_ID id);
	void					RemoveMapLocationByObjectID (ALife::_OBJECT_ID id); //call on destroy object
	void					RemoveMapLocation			(CMapLocation* ml);
	CMapLocation*			GetMapLocation				(const shared_str& spot_type, ALife::_OBJECT_ID id);
	CMapLocation*			GetActiveTaskCompassLocation	();
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

	xrCriticalSection		UpdateCS;

	void MapLocationsForEach(const char* spot_type, u16 id, const luabind::functor<bool>& functor);
	void AllLocationsForEach(const luabind::functor<bool>& functor);
	static void script_register(lua_State* L);
};
