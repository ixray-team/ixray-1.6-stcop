#include "stdafx.h"
#include "ISpatial.h"

#define ENGINE_API
#include "../xrEngine/Render.h"

#ifdef DEBUG
#include "../xrEngine/xr_object.h"
#endif

ISpatial_DB* g_SpatialSpace = nullptr;
ISpatial_DB* g_SpatialSpacePhysic = nullptr;

Fvector	c_spatial_offset[8]
{
	{-1.f, -1.f, -1.f},
	{ 1.f, -1.f, -1.f},
	{-1.f,  1.f, -1.f},
	{ 1.f,  1.f, -1.f},
	{-1.f, -1.f,  1.f},
	{ 1.f, -1.f,  1.f},
	{-1.f,  1.f,  1.f},
	{ 1.f,  1.f,  1.f}
};

///////////////////////////////////ISpatial///////////////////////////////////////
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
void ISpatial::Register()
{
	spatial.type |= ESPATIAL_TYPE::INVALIDSECTOR;
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
		spatial.sector = nullptr;
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
		//*** somehow it was determined that object has been moved
		spatial.type |= ESPATIAL_TYPE::INVALIDSECTOR;

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
void ISpatial::OwnerReset(ISpatialOwner* ptr)
{
	RawOwner = ptr;
}

Fvector ISpatial::OwnerSectorPoint()
{
	return RawOwner ? RawOwner->spatial_sector_point() : spatial.sphere.P;
}

void ISpatial::spatial_updatesector_internal()
{
	Fvector curr_sector_point = OwnerSectorPoint();

	if ((FALSE == curr_sector_point.similar(spatial.last_sector_point) || spatial.sector == nullptr))
	{
		PROF_EVENT(__FUNCTION__);
		spatial.type &= ~ESPATIAL_TYPE::INVALIDSECTOR;
		if (IRender_Sector* S = ::Render->detectSector(curr_sector_point))
			spatial.sector = S;
	}

	spatial.last_sector_point = curr_sector_point;
}

///////////////////////////////////ISpatial_DB///////////////////////////////////////
void ISpatial_DB::initialize(Fbox& BB)
{
	Fvector bbc, bbd;
	BB.get_CD(bbc, bbd);

	m_center.set(bbc);
	m_bounds = std::max(std::max(bbd.x, bbd.y), bbd.z);

	if (m_root == nullptr)
	{
		// initialize
		nodes.reserve(512);
		m_root = _node_create();
	}
}

void ISpatial_DB::db_insert(ISpatialShared S, ISpatial_NODE* N, Fvector& n_C, float n_R)
{
	//*** we are assured that object lives inside our node
	float n_vR = n_R * 2.f;
	VERIFY(N);
	VERIFY(S->verify_sp(n_C, n_vR));

	// we have to make sure we aren't the leaf node
	constexpr float c_spatial_min = 8.f;
	if (n_R <= c_spatial_min)
	{
		// this is leaf node
		N->_insert(S);
		S->spatial.node_center.set(n_C);
		S->spatial.node_radius = n_vR;		// vR
		return;
	}

	// we have to check if it can be putted further down
	float s_R = S->spatial.sphere.R;	// spatial bounds
	float c_R = n_R * 0.5f;				// children bounds
	if (s_R < c_R)
	{
		// object can be pushed further down - select "octant", calc node position
		Fvector& s_C = S->spatial.sphere.P;
		u32 octant = _octant(n_C, s_C);
		Fvector c_C{ n_C + c_spatial_offset[octant] * c_R };
		VERIFY(octant == _octant(n_C, c_C));				// check table assosiations
		ISpatial_NODE*& chield = N->children[octant];

		if (nullptr == chield)
		{
			chield = _node_create(N, octant);
			VERIFY(chield);
		}

		VERIFY(chield);
		db_insert(S, chield, c_C, c_R);
		VERIFY(chield);
	}
	else
	{
		// we have to "own" this object (potentially it can be putted down sometimes...)
		N->_insert(S);
		S->spatial.node_center.set(n_C);
		S->spatial.node_radius = n_vR;
	}
}

void ISpatial_DB::insert(ISpatialShared S)
{
	if (m_root != nullptr && S->verify_sp(m_center, m_bounds))
	{
		// Object inside our DB
		db_insert(S, m_root, m_center, m_bounds);
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
}

void ISpatial_DB::db_remove(ISpatial_NODE* N, ISpatial_NODE* N_sub)
{
	if (nullptr == N)
		return;

	//*** we are assured that node contains N_sub and this subnode is empty
	VERIFY(N_sub->_empty());
	_node_destroy(N->children[N_sub->octant_in_parent]);

	// Recurse
	if (N->_empty())
		db_remove(N->parent, N);
}

void ISpatial_DB::remove(ISpatialShared S)
{
	ISpatial_NODE* N = S->spatial.node_ptr;
	VERIFY(N);
	N->_remove(S);

	// Recurse
	if (N->_empty())
		db_remove(N->parent, N);
}
