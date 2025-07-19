#include "stdafx.h"
#include "ISpatial.h"

#define ENGINE_API
#include "../xrEngine/Render.h"

#ifdef DEBUG
#include "../xrEngine/xr_object.h"
#include "../xrEngine/PS_instance.h"
#endif


ISpatial_DB*		g_SpatialSpace			= nullptr;
ISpatial_DB*		g_SpatialSpacePhysic	= nullptr;
ISpatial_DB*		g_SpatialSpaceLights	= nullptr;

Fvector	c_spatial_offset	[8]	= 
{
	{ -1, -1, -1	},
	{  1, -1, -1	},
	{ -1,  1, -1	},
	{  1,  1, -1	},
	{ -1, -1,  1	},
	{  1, -1,  1	},
	{ -1,  1,  1	},
	{  1,  1,  1	}
};

CObject* ISpatial::dcast_CObject()
{
	return RawOwner ? RawOwner->dcast_CObject() : nullptr;
}

IRender_Light* ISpatial::dcast_Light()
{
	return RawOwner ? RawOwner->dcast_Light() : nullptr;
}

Feel::Sound* ISpatial::dcast_FeelSound()
{
	return RawOwner ? RawOwner->dcast_FeelSound() : nullptr;
}

IRenderable* ISpatial::dcast_Renderable() 
{
	return RawOwner ? RawOwner->dcast_Renderable() : nullptr;
}

CPHObject* ISpatial::dcast_CPHObject()
{
	return RawOwner ? RawOwner->dcast_CPHObject() : nullptr;
}

CGlow* ISpatial::dcast_CGlow()
{
	return RawOwner ? RawOwner->dcast_CGlow() : nullptr;
}

//////////////////////////////////////////////////////////////////////////
ISpatial::ISpatial(ISpatial_DB* space, ISpatialOwner* Owner)
{
	RawOwner = Owner;

	spatial.sphere.P.set	(0,0,0);
	spatial.sphere.R		= 0;
	spatial.node_center.set	(0,0,0);
	spatial.node_radius		= 0;
	spatial.node_ptr		= nullptr;
	spatial.sector			= nullptr;
	spatial.space			= space;
}

ISpatial::~ISpatial(void)
{
	Unregister();
}

BOOL ISpatial::spatial_inside()
{
	float	dr	= -(- spatial.node_radius + spatial.sphere.R);
	if (spatial.sphere.P.x < spatial.node_center.x - dr)	return FALSE;
	if (spatial.sphere.P.x > spatial.node_center.x + dr)	return FALSE;
	if (spatial.sphere.P.y < spatial.node_center.y - dr)	return FALSE;
	if (spatial.sphere.P.y > spatial.node_center.y + dr)	return FALSE;
	if (spatial.sphere.P.z < spatial.node_center.z - dr)	return FALSE;
	if (spatial.sphere.P.z > spatial.node_center.z + dr)	return FALSE;
	return TRUE;
}

BOOL	verify_sp	(ISpatialShared sp, Fvector& node_center, float node_radius)
{
	float	dr	= -(- node_radius + sp->spatial.sphere.R);
	if (sp->spatial.sphere.P.x < node_center.x - dr)	return FALSE;
	if (sp->spatial.sphere.P.x > node_center.x + dr)	return FALSE;
	if (sp->spatial.sphere.P.y < node_center.y - dr)	return FALSE;
	if (sp->spatial.sphere.P.y > node_center.y + dr)	return FALSE;
	if (sp->spatial.sphere.P.z < node_center.z - dr)	return FALSE;
	if (sp->spatial.sphere.P.z > node_center.z + dr)	return FALSE;
	return TRUE;
}

void ISpatial::Register()
{
	if (spatial.node_ptr)
	{
		// already registered - nothing to do
	}
	else 
	{
		// register
		R_ASSERT(spatial.space);
		xrSRWLockGuard guard(&spatial.space->db_lock, false);
		spatial.space->insert(shared_from_this());
		spatial.sector = 0;
	}
}

void ISpatial::Unregister()
{
	if (spatial.node_ptr)
	{
		// remove
		xrSRWLockGuard guard(&spatial.space->db_lock, false);
		spatial.space->remove(shared_from_this());
		spatial.node_ptr = nullptr;
		spatial.sector = nullptr;
	}
	else {
		// already unregistered
	}
}

void ISpatial::Move()
{
	if (spatial.node_ptr)
	{
		//*** check if we are supposed to correct it's spatial location
		if (spatial_inside())	
			return;		// ???

		xrSRWLockGuard guard(&spatial.space->db_lock, false);
		spatial.space->remove(shared_from_this());
		spatial.space->insert(shared_from_this());
	}
	else {
		//*** we are not registered yet, or already unregistered
		//*** ignore request
	}
}

Fvector ISpatial::SectorPoint()
{
	return spatial.sphere.P;
}

Fvector ISpatial::OwnerSectorPoint()
{
	return RawOwner ? RawOwner->spatial_sector_point() : spatial.sphere.P;
}

void ISpatial::spatial_updatesector()
{
	Fvector curr_sector_point = OwnerSectorPoint();

	if ((FALSE == curr_sector_point.similar(spatial.last_sector_point) || spatial.sector == nullptr))
	{
		PROF_EVENT("spatial_updatesector");
		if (IRender_Sector* S = ::Render->detectSector(OwnerSectorPoint()))
			spatial.sector = S;
	}

	spatial.last_sector_point = curr_sector_point;
};

//////////////////////////////////////////////////////////////////////////
void ISpatial_NODE::_init(ISpatial_NODE* _parent)
{
	parent		=	_parent;
	children[0]	=	children[1]	=	children[2]	=	children[3]	=
	children[4]	=	children[5]	=	children[6]	=	children[7]	=	nullptr;
	items.clear();
}

void ISpatial_NODE::_insert(ISpatialShared S)
{	
	S->spatial.node_ptr			=	this;
	items.push_back					(S);
	S->spatial.space->stat_objects	++;
}

void ISpatial_NODE::_remove(ISpatialShared S)
{	
	S->spatial.node_ptr = nullptr;
	auto it = std::find(items.begin(),items.end(),S);
	VERIFY(it!=items.end());
	items.erase(it);
	S->spatial.space->stat_objects	--;
}

//////////////////////////////////////////////////////////////////////////

ISpatial_DB::ISpatial_DB()
{
	m_root					= nullptr;
	stat_nodes				= 0;
	stat_objects			= 0;
}

ISpatial_DB::~ISpatial_DB()
{
	if ( m_root )
	{
		_node_destroy(m_root);
	}

	while (!nodes.empty())
	{
		nodes.pop_back();
	}
}

void ISpatial_DB::initialize(Fbox& BB)
{
	Fvector bbc, bbd;
	BB.get_CD(bbc, bbd);

	m_center.set(bbc);
	m_bounds = _max(_max(bbd.x, bbd.y), bbd.z);

	if (m_root == nullptr)
	{
		// initialize
		nodes.reserve(512);
		rt_insert_object = nullptr;
		m_root = _node_create();
		m_root->_init(nullptr);
	}
}

ISpatial_NODE* ISpatial_DB::_node_create()
{
	stat_nodes++;
	
	return nodes.emplace_back(new ISpatial_NODE);
}

void ISpatial_DB::_node_destroy(ISpatial_NODE*& P)
{
	//VERIFY(P->_empty());
	stat_nodes--;

	auto it = std::find(nodes.begin(), nodes.end(), P);

	if (it != nodes.end())
	{
		nodes.erase(it);
	}

	xr_delete(P);
}

void ISpatial_DB::_insert(ISpatial_NODE* N, Fvector& n_C, float n_R)
{
	//*** we are assured that object lives inside our node
	float	n_vR = 2 * n_R;
	VERIFY(N);
	VERIFY(verify_sp(rt_insert_object, n_C, n_vR));

	// we have to make sure we aren't the leaf node
	if (n_R <= c_spatial_min)
	{
		// this is leaf node
		N->_insert(rt_insert_object);
		rt_insert_object->spatial.node_center.set(n_C);
		rt_insert_object->spatial.node_radius = n_vR;		// vR
		return;
	}

	// we have to check if it can be putted further down
	float s_R = rt_insert_object->spatial.sphere.R;	// spatial bounds
	float c_R = n_R / 2;								// children bounds
	if (s_R < c_R)
	{
		// object can be pushed further down - select "octant", calc node position
		Fvector& s_C = rt_insert_object->spatial.sphere.P;
		u32			octant = _octant(n_C, s_C);
		Fvector		c_C;				c_C.mad(n_C, c_spatial_offset[octant], c_R);
		VERIFY(octant == _octant(n_C, c_C));				// check table assosiations
		ISpatial_NODE*& chield = N->children[octant];

		if (0 == chield)
		{
			chield = _node_create();
			VERIFY(chield);
			chield->_init(N);
			VERIFY(chield);
		}

		VERIFY(chield);
		_insert(chield, c_C, c_R);
		VERIFY(chield);
	}
	else
	{
		// we have to "own" this object (potentially it can be putted down sometimes...)
		N->_insert(rt_insert_object);
		rt_insert_object->spatial.node_center.set(n_C);
		rt_insert_object->spatial.node_radius = n_vR;
	}
}

void ISpatial_DB::insert(ISpatialShared S)
{
	if (m_root != nullptr && verify_sp(S, m_center, m_bounds))
	{
		// Object inside our DB
		rt_insert_object = S;
		_insert(m_root, m_center, m_bounds);
		VERIFY(S->spatial_inside());
	}
	else 
	{
		// Object outside our DB, put it into root node and hack bounds
		// Object will reinsert itself until fits into "real", "controlled" space

		if (m_root != nullptr)
		{
			m_root->_insert(S);
			S->spatial.node_center.set(m_center);
			S->spatial.node_radius = m_bounds;
		}
	}
#ifdef DEBUG
	stat_insert.End();
#endif
}

void ISpatial_DB::_remove(ISpatial_NODE* N, ISpatial_NODE* N_sub)
{
	if (0 == N)
		return;

	//*** we are assured that node contains N_sub and this subnode is empty
	u32 octant = u32(-1);
	if (N_sub == N->children[0])			octant = 0;
	else if (N_sub == N->children[1])		octant = 1;
	else if (N_sub == N->children[2])		octant = 2;
	else if (N_sub == N->children[3])		octant = 3;
	else if (N_sub == N->children[4])		octant = 4;
	else if (N_sub == N->children[5])		octant = 5;
	else if (N_sub == N->children[6])		octant = 6;
	else if (N_sub == N->children[7])		octant = 7;

	VERIFY(octant < 8);
	VERIFY(N_sub->_empty());
	_node_destroy(N->children[octant]);

	// Recurse
	if (N->_empty())
		_remove(N->parent, N);
}

void ISpatial_DB::remove(ISpatialShared S)
{
#ifdef DEBUG
	stat_remove.Begin();
#endif
	ISpatial_NODE* N = S->spatial.node_ptr;
	N->_remove(S);

	// Recurse
	if (N->_empty())
		_remove(N->parent, N);

#ifdef DEBUG
	stat_remove.End();
#endif
}

void ISpatial_DB::update(u32)
{
#ifdef DEBUG
	if (0 == m_root)
		return;

	//VERIFY(verify());
#endif
}
