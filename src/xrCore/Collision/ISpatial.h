#ifndef XRENGINE_ISPATIAL_H_INCLUDED
#define XRENGINE_ISPATIAL_H_INCLUDED

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

//////////////////////////////////////////////////////////////////////////
enum class ESPATIAL_TYPE : u64
{
	NONE				= 0ULL,
	INVALIDSECTOR		= bit_lshift(0ULL),
	RENDERABLE			= bit_lshift(1ULL),
	LIGHTSOURCE			= bit_lshift(2ULL),
	LIGHTSOURCEHEMI		= bit_lshift(3ULL),
	PHYSIC				= bit_lshift(4ULL),
	SHAPE				= bit_lshift(5ULL),
	PARTICLE			= bit_lshift(6ULL),

	COLLIDEABLE			= bit_lshift(7ULL),
	VISIBLEFORAI		= bit_lshift(8ULL),
	REACTTOSOUND		= bit_lshift(9ULL),
	OBSTACLE			= bit_lshift(10ULL),
	RENDERABLESHADOW	= bit_lshift(11ULL),

	LADDER				= bit_lshift(12ULL),

	ACTOR				= bit_lshift(13ULL),
	ACTOR_DEAD			= bit_lshift(14ULL),
	ACTOR_ALIVE			= bit_lshift(15ULL),

	AI					= bit_lshift(16ULL),
	AI_DEAD				= bit_lshift(17ULL),
	AI_ALIVE			= bit_lshift(18ULL),

	STALKER				= bit_lshift(19ULL),
	STALKER_WOUNDED		= bit_lshift(20ULL),
	STALKER_DEAD		= bit_lshift(21ULL),
	STALKER_ALIVE		= bit_lshift(22ULL),

	MONSTER				= bit_lshift(23ULL),
	MONSTER_DEAD		= bit_lshift(24ULL),
	MONSTER_ALIVE		= bit_lshift(25ULL),

	CROW				= bit_lshift(26ULL),
	CROW_DEAD			= bit_lshift(27ULL),
	CROW_ALIVE			= bit_lshift(28ULL),

	ITEM				= bit_lshift(29ULL),
	WEAPON				= bit_lshift(30ULL),
	MISSILE				= bit_lshift(31ULL),
	ROCKET				= bit_lshift(32ULL),
	ARTEFACT			= bit_lshift(33ULL),
	ANOMALY_DETECTOR	= bit_lshift(34ULL),

	CAR					= bit_lshift(35ULL),
	HELI				= bit_lshift(36ULL),

	PHYSIC_OBJECT		= bit_lshift(37ULL),
	PHYSIC_SHELL_HOLDER = bit_lshift(38ULL),
	PHYSIC_OBJECT_DESTR = bit_lshift(39ULL),
	PHYSIC_OBJECT_BRKBL = bit_lshift(40ULL),
	PHYSIC_MOVEMENT		= bit_lshift(41ULL),

	INV_BOX				= bit_lshift(42ULL),

	AI_DOOR				= bit_lshift(43ULL),

	LIGHT_LAMP			= bit_lshift(44ULL),

	LEVEL_CHANGER		= bit_lshift(45ULL),
	SPACE_RESTRICTOR	= bit_lshift(46ULL),
	ANOMALY_ZONE		= bit_lshift(47ULL),
	SIM_FACTION			= bit_lshift(48ULL),
	SMART_TERRAIN		= bit_lshift(49ULL),
	CAMP_ZONE			= bit_lshift(50ULL),
	SMART_COVER			= bit_lshift(51ULL),
	ANOMAL_ZONE_LOGIC	= bit_lshift(52ULL),

	//MAX_FLAG			= bit_lshift(63ULL),
};
ENUM_CLASS_FLAGS(ESPATIAL_TYPE);
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
struct ISpatial_NODE;
class IRender_Sector;
class ISpatial_DB;
class IRenderable;
class IRender_Light;
class CPHObject;
class CGlow;

namespace Feel { class Sound; }

class ISpatialOwner;

///////////////////////////////////ISpatial///////////////////////////////////////
class XRCORE_API ISpatial:
	public std::enable_shared_from_this<ISpatial>
{
	friend class ISpatialOwner;
public:
	struct SpatialData
	{
		ESPATIAL_TYPE type = ESPATIAL_TYPE::NONE;
		Fsphere sphere = {zero_vel, 0.f};

		// Cached node center for TBV optimization
		Fvector node_center = zero_vel;
		Fvector last_sector_point = zero_vel;
		// Cached node bounds for TBV optimization
		float node_radius = 0.f;
		float ssa_dyn_factor = 0.002f;
		float ssa_d_cam = 220.f;	

		// Cached parent node for "empty-members" optimization
		ISpatial_NODE* node_ptr = nullptr;		
		IRender_Sector* sector = nullptr;

		// allow different spaces
		ISpatial_DB* space = nullptr;

		size_t items_idx = 0;
	} spatial;

private:
	ISpatialOwner* RawOwner = nullptr;

public:
	ICF bool spatial_inside()
	{
		float dr = -(-spatial.node_radius + spatial.sphere.R);
		if (spatial.sphere.P.x < spatial.node_center.x - dr) return false;
		if (spatial.sphere.P.x > spatial.node_center.x + dr) return false;
		if (spatial.sphere.P.y < spatial.node_center.y - dr) return false;
		if (spatial.sphere.P.y > spatial.node_center.y + dr) return false;
		if (spatial.sphere.P.z < spatial.node_center.z - dr) return false;
		if (spatial.sphere.P.z > spatial.node_center.z + dr) return false;
		return true;
	}
	ICF bool verify_sp(Fvector& node_center, float node_radius)
	{
		float dr = -(-node_radius + spatial.sphere.R);
		if (spatial.sphere.P.x < node_center.x - dr) return false;
		if (spatial.sphere.P.x > node_center.x + dr) return false;
		if (spatial.sphere.P.y < node_center.y - dr) return false;
		if (spatial.sphere.P.y > node_center.y + dr) return false;
		if (spatial.sphere.P.z < node_center.z - dr) return false;
		if (spatial.sphere.P.z > node_center.z + dr) return false;
		return true;
	}

	void spatial_updatesector_internal();

private:
	void	Register();
	void	Unregister();

	void	Move();

public:
	Fvector OwnerSectorPoint();
	void OwnerReset(ISpatialOwner* ptr);

	ICF void spatial_updatesector()	
	{
		if (ESPATIAL_TYPE::NONE == (spatial.type& ESPATIAL_TYPE::INVALIDSECTOR))
			return;

		spatial_updatesector_internal();
	};

	CObject*		dcast_CObject		();
	Feel::Sound*	dcast_FeelSound		();
	IRenderable*	dcast_Renderable	();
	IRender_Light*	dcast_Light			();
	CPHObject*		dcast_CPHObject		();
	CGlow*			dcast_CGlow			();

	constexpr ISpatial(ISpatial_DB* space, ISpatialOwner* Owner) : RawOwner(Owner)
	{
		spatial.space = space;
	}
	virtual ~ISpatial(void) { Unregister(); }
};

using ISpatialShared = xr_shared_ptr<ISpatial>;

class ISpatialOwner
{
public:
	ISpatialShared SpatialComponent;

	virtual void spatial_create(ISpatial_DB* db, ISpatialOwner* owner, ESPATIAL_TYPE type) { SpatialComponent = xr_make_shared<ISpatial>(db, owner); SpatialComponent->spatial.type = type; }
	virtual void spatial_register() { SpatialComponent->Register(); };
	virtual void spatial_unregister() { SpatialComponent->Unregister(); };

	virtual void	spatial_move() { SpatialComponent->Move(); };
	virtual Fvector	spatial_sector_point() { return SpatialComponent->spatial.sphere.P; }

	
	virtual CObject*		dcast_CObject		() { return nullptr; };
	virtual Feel::Sound*	dcast_FeelSound		() { return nullptr; };
	virtual IRenderable*	dcast_Renderable	() { return nullptr; };
	virtual IRender_Light*	dcast_Light			() { return nullptr; };
	virtual CPHObject*		dcast_CPHObject		() { return nullptr; };
	virtual CGlow*			dcast_CGlow			() { return nullptr; };

	virtual ~ISpatialOwner() { SpatialComponent->OwnerReset(nullptr); }
};


///////////////////////////////////ISpatial_NODE///////////////////////////////////////
struct ISpatial_NODE
{
	// children nodes
	ISpatial_NODE* children[8]
	{
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
	};
	// own items
	xr_vector<ISpatialShared> items;
	// parent node for "empty-members" optimization
	ISpatial_NODE* parent = nullptr;

	u32 octant_in_parent = u32(-1);
	u32 childs_size = u32(0);

	constexpr ISpatial_NODE(ISpatial_NODE* _parent, u32 octant) :
		parent(_parent), octant_in_parent(octant)
	{
		if (_parent)
			_parent->childs_size++;

		items.reserve(64);
	}
	constexpr ~ISpatial_NODE()
	{
		if (parent)
			parent->childs_size--;
	}

	ICF void _insert(ISpatialShared S)
	{
		S->spatial.node_ptr = this;
		S->spatial.items_idx = items.size();
		items.push_back(S);
	}

	ICF void _remove(ISpatialShared S)
	{
		S->spatial.node_ptr = nullptr;

		size_t idx = S->spatial.items_idx;
		ISpatialShared& moved = fast_erase(items, idx);
		moved->spatial.items_idx = idx;
	}

	ICF bool _empty() const
	{
		return !!(items.empty() && childs_size == 0U);
	}
};

///////////////////////////////////ISpatial_DB///////////////////////////////////////
class XRCORE_API ISpatial_DB
{
private:
	xr_vector<ISpatial_NODE*>		nodes;
public:
	xrSRWLock						db_lock;
	ISpatial_NODE*					m_root = nullptr;
	Fvector							m_center = zero_vel;
	float							m_bounds = 0.f;
private:
	ICF u32							_octant			(u32 x, u32 y, u32 z)			{	return z*4 + y*2 + x;	}
	ICF u32							_octant			(Fvector& base, Fvector& rel)
	{
		u32 o	= 0;
		if (rel.x > base.x) o+=1;
		if (rel.y > base.y) o+=2;
		if (rel.z > base.z) o+=4;
		return	o;
	}

	ICF ISpatial_NODE* _node_create(ISpatial_NODE* parent = nullptr, u32 octant = u32(-1))
	{
		return nodes.emplace_back(new ISpatial_NODE(parent, octant));
	}

	ICF void _node_destroy(ISpatial_NODE*& P)
	{
		if(P)
		{
			for (size_t i = 0; i < nodes.size(); ++i)
			{
				ISpatial_NODE*& node = nodes[i];
				if (node == P)
				{
					if (i != nodes.size() - 1)
						node = nodes.back();
					nodes.pop_back();
					break;
				}
			}

			xr_delete(P);
		}
	}

	void							db_insert			(ISpatialShared S, ISpatial_NODE* N, Fvector& n_center, float n_radius);
	void							db_remove			(ISpatial_NODE* N, ISpatial_NODE* N_sub);
public:

	~ISpatial_DB()
	{
		_node_destroy(m_root);
	}

	// managing
	void							initialize		(Fbox& BB);
	void							insert			(ISpatialShared S);
	void							remove			(ISpatialShared S);

public:
	enum
	{
		O_ONLYFIRST		= (1<<0),
		O_ONLYNEAREST	= (1<<1),
		O_ORDERED		= (1<<2),
		O_force_u32		= u32(-1)
	};

	// query
	void q_ray(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask_and, const Fvector& _start, const Fvector& _dir, float _range);

	void q_box(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask_or, const Fbox& box);
	ICF void q_box(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask_or, const Fvector& _center, const Fvector& _dim)
	{ q_box(R, _o, _mask_or, Fbox().setb(_center, _dim)); };

	void q_obb(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask_or, const Fobb& obb);

	void q_sphere(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask_or, const Fsphere& sphere);
	ICF void q_sphere(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask_or, const Fvector& _center, const float _radius)
	{ q_sphere(R, _o, _mask_or, Fsphere{ _center, _radius }); };

	void q_frustum(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask_or, const CFrustum& _frustum);
};

XRCORE_API extern ISpatial_DB* g_SpatialSpace;
XRCORE_API extern ISpatial_DB* g_SpatialSpacePhysic;

#pragma pack(pop)

#endif // #ifndef XRENGINE_ISPATIAL_H_INCLUDED