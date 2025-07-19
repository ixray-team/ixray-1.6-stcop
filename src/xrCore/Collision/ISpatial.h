#ifndef XRENGINE_ISPATIAL_H_INCLUDED
#define XRENGINE_ISPATIAL_H_INCLUDED

//#pragma once
#include "../xrPool.h"

#include "xr_collide_defs.h"

#pragma pack(push,4)

/*
Requirements:
0. Generic
	* O(1) insertion
		- radius completely determines	"level"
		- position completely detemines "node"
	* O(1) removal
	* 
1. Rendering
	* Should live inside spatial DB
	* Should have at least "bounding-sphere" or "bounding-box"
	* Should have pointer to "sector" it lives in
	* Approximate traversal order relative to point ("camera")
2. Spatial queries
	* Should live inside spatial DB
	* Should have at least "bounding-sphere" or "bounding-box"
*/

const float						c_spatial_min	= 8.f;
//////////////////////////////////////////////////////////////////////////
enum
{
	STYPE_RENDERABLE			= (1<<0),
	STYPE_LIGHTSOURCE			= (1<<1),
	STYPE_COLLIDEABLE			= (1<<2),
	STYPE_VISIBLEFORAI			= (1<<3),
	STYPE_REACTTOSOUND			= (1<<4),
	STYPE_PHYSIC				= (1<<5),
	STYPE_OBSTACLE				= (1<<6),
	STYPE_SHAPE					= (1<<7),
	STYPE_LIGHTSOURCEHEMI		= (1<<8),
	STYPE_RENDERABLESHADOW		= (1<<9),
	STYPE_PARTICLE				= (1<<10),
	STYPE_GLOW					= (1<<11),
};
//////////////////////////////////////////////////////////////////////////
// Comment: 
//		ordinal objects			- renderable?, collideable?, visibleforAI?
//		physical-decorations	- renderable, collideable
//		lights					- lightsource
//		particles(temp-objects)	- renderable
//		glow					- renderable
//		sound					- ???
//////////////////////////////////////////////////////////////////////////
//class 				IRender_Sector;
//class 				ISpatial;
//class 				ISpatial_NODE;
//class 				ISpatial_DB;

//////////////////////////////////////////////////////////////////////////
// Fast type conversion
//class 			CObject;
//class 			IRenderable;
//class 			IRender_Light;
//
//namespace Feel { class Sound; }

//////////////////////////////////////////////////////////////////////////
class 				ISpatial_NODE;
class 				IRender_Sector;
class 				ISpatial_DB;
namespace Feel { class Sound; }
class 				IRenderable;
class 				IRender_Light;
class 				CPHObject;
class 				CGlow;

#include <variant>
class ISpatialOwner;

class XRCORE_API ISpatial:
	public std::enable_shared_from_this<ISpatial>
{
	friend class ISpatialOwner;
public:
	struct SpatialData
	{
		u32 type = 0;
		Fsphere sphere = {};

		// Cached node center for TBV optimization
		Fvector node_center = {};
		Fvector last_sector_point = {0.f,0.f,0.f};
		// Cached node bounds for TBV optimization
		float node_radius=EPS;

		// Cached parent node for "empty-members" optimization
		ISpatial_NODE* node_ptr = nullptr;		
		IRender_Sector* sector = nullptr;

		// allow different spaces
		ISpatial_DB* space = nullptr;
	};

	SpatialData spatial;

private:
	ISpatialOwner* RawOwner = nullptr;

public:
	BOOL spatial_inside		()			;

private:
	void	Register();
	void	Unregister();

	void	Move();
	Fvector SectorPoint();

public:
	Fvector OwnerSectorPoint();
	void OwnerReset(ISpatialOwner* ptr) { RawOwner = ptr; };

	void spatial_updatesector();

	CObject*		dcast_CObject		();
	Feel::Sound*	dcast_FeelSound		();
	IRenderable*	dcast_Renderable	();
	IRender_Light*	dcast_Light			();
	CPHObject*		dcast_CPHObject		();
	CGlow*			dcast_CGlow			();

				ISpatial		(ISpatial_DB* space, ISpatialOwner* TypeObject);
	virtual		~ISpatial		();
};

using ISpatialShared = xr_shared_ptr<ISpatial>;

class ISpatialOwner
{
public:
	ISpatialShared SpatialComponent;

public:
	virtual void spatial_create(ISpatial_DB* db, ISpatialOwner* owner, u32 type) { SpatialComponent = xr_make_shared<ISpatial>(db, owner); SpatialComponent->spatial.type = type; }
	virtual void spatial_register() { SpatialComponent->Register(); };
	virtual void spatial_unregister() { SpatialComponent->Unregister(); };

	virtual void	spatial_move() { SpatialComponent->Move(); };
	virtual Fvector	spatial_sector_point() { return SpatialComponent->SectorPoint(); }

	
	virtual CObject*		dcast_CObject		() { return nullptr; };
	virtual Feel::Sound*	dcast_FeelSound		() { return nullptr; };
	virtual IRenderable*	dcast_Renderable	() { return nullptr; };
	virtual IRender_Light*	dcast_Light			() { return nullptr; };
	virtual CPHObject*		dcast_CPHObject		() { return nullptr; };
	virtual CGlow*			dcast_CGlow			() { return nullptr; };

	virtual ~ISpatialOwner() { SpatialComponent->OwnerReset(nullptr); }
};


//////////////////////////////////////////////////////////////////////////
//class ISpatial_NODE;
class 	ISpatial_NODE
{
public:
	using ptrt = ptrdiff_t;
public:
	// parent node for "empty-members" optimization
	ISpatial_NODE* parent;
	// children nodes
	ISpatial_NODE* children[8];
	// own items
	xr_vector<ISpatialShared> items;
public:
	void						_init			(ISpatial_NODE* _parent);
	void						_remove			(ISpatialShared _S);
	void						_insert			(ISpatialShared _S);
	BOOL						_empty			()						
	{
		return items.empty() && (
			0==(
				ptrt(children[0])|ptrt(children[1])|
				ptrt(children[2])|ptrt(children[3])|
				ptrt(children[4])|ptrt(children[5])|
				ptrt(children[6])|ptrt(children[7])
				)
			);	
	}
};
////////////

//template <class T, int granularity>
//class	poolSS;
#ifndef	DLL_API
#	define DLL_API					__declspec(dllimport)
#endif // #ifndef	DLL_API

//////////////////////////////////////////////////////////////////////////
class XRCORE_API	ISpatial_DB
{
private:
	xr_vector<ISpatial_NODE*>		nodes;
	ISpatialShared					rt_insert_object;
public:
	xrSRWLock						db_lock;
	ISpatial_NODE*					m_root;
	Fvector							m_center;
	float							m_bounds;
	u32								stat_nodes;
	u32								stat_objects;
	CStatTimer						stat_insert;
	CStatTimer						stat_remove;
private:
	IC u32							_octant			(u32 x, u32 y, u32 z)			{	return z*4 + y*2 + x;	}
	IC u32							_octant			(Fvector& base, Fvector& rel)
	{
		u32 o	= 0;
		if (rel.x > base.x) o+=1;
		if (rel.y > base.y) o+=2;
		if (rel.z > base.z) o+=4;
		return	o;
	}

	ISpatial_NODE*					_node_create	();
	void 							_node_destroy	(ISpatial_NODE* &P);

	void							_insert			(ISpatial_NODE* N, Fvector& n_center, float n_radius);
	void							_remove			(ISpatial_NODE* N, ISpatial_NODE* N_sub);
public:
	ISpatial_DB();
	~ISpatial_DB();

	// managing
	void							initialize		(Fbox& BB);
	//void							destroy			();
	void							insert			(ISpatialShared S);
	void							remove			(ISpatialShared S);
	void							update			(u32 nodes=8);
	BOOL							verify			();

public:
	enum
	{
		O_ONLYFIRST		= (1<<0),
		O_ONLYNEAREST	= (1<<1),
		O_ORDERED		= (1<<2),
		O_force_u32		= u32(-1)
	};

	// query
	void							q_ray			(xr_vector<ISpatialShared>& R, u32 _o, u32 _mask_and, const Fvector& _start, const Fvector& _dir, float _range);
	void							q_box			(xr_vector<ISpatialShared>& R, u32 _o, u32 _mask_or, const Fvector& _center, const Fvector& _size);
	void							q_sphere		(xr_vector<ISpatialShared>& R, u32 _o, u32 _mask_or, const Fvector& _center, const float _radius);
	void							q_frustum		(xr_vector<ISpatialShared>& R, u32 _o, u32 _mask_or, const CFrustum& _frustum);
};

XRCORE_API extern ISpatial_DB*		g_SpatialSpace;
XRCORE_API extern ISpatial_DB*		g_SpatialSpacePhysic;
XRCORE_API extern ISpatial_DB*		g_SpatialSpaceLights;
#pragma pack(pop)

#endif // #ifndef XRENGINE_ISPATIAL_H_INCLUDED