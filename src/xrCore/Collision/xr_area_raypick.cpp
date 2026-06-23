#include "stdafx.h"
#include "xr_area.h"
#include "ISpatial.h"
#include "cl_intersect.h"

#define ENGINE_API
#include "../../xrEngine/xr_collide_form.h"
#include "../../xrEngine/xr_object.h"

#ifdef	DEBUG
static bool _cdb_bDebug = false;
XRCORE_API bool *cdb_bDebug = &_cdb_bDebug;
bool bDebug()
{
	return !!(*cdb_bDebug);
}
#endif
using namespace	collide;

namespace CObjectSpaceThreadData
{
	thread_local CDB::COLLIDER xrc;
	thread_local collide::rq_results r_temp;
	thread_local xr_vector<ISpatialShared> r_spatial;
}

//--------------------------------------------------------------------------------
// RayTest - Occluded/No
//--------------------------------------------------------------------------------
bool CObjectSpace::RayTest(const Fvector& start, const Fvector& dir, float range, collide::rq_target tgt, collide::ray_cache* cache, CObject* ignore_object)
{
	VERIFY(std::abs(dir.magnitude() - 1) < EPS);

	CObjectSpaceThreadData::r_temp.r_clear();
	CObjectSpaceThreadData::xrc.ray_options(CDB::OPT_ONLYFIRST);

	collide::ray_defs	Q(start, dir, range, CDB::OPT_ONLYFIRST, tgt);

	// dynamic test
	if (tgt & rqtDyn)
	{
		ESPATIAL_TYPE d_flags = ESPATIAL_TYPE::COLLIDEABLE | ((tgt & rqtObstacle) ? ESPATIAL_TYPE::OBSTACLE : ESPATIAL_TYPE::NONE) | ((tgt & rqtShape) ? ESPATIAL_TYPE::SHAPE : ESPATIAL_TYPE::NONE);

		// traverse object database
		g_SpatialSpace->q_ray(CObjectSpaceThreadData::r_spatial, 0, d_flags, start, dir, range);

		// Determine visibility for dynamic part of scene
		for (u32 o_it = 0; o_it < CObjectSpaceThreadData::r_spatial.size(); o_it++)
		{
			ISpatial* spatial = CObjectSpaceThreadData::r_spatial[o_it].get();
			CObject* collidable = spatial->dcast_CObject();
			if (collidable && (collidable != ignore_object) && collidable->collidable.model)
			{
				ECollisionFormType tp = collidable->collidable.model->Type();
				if ((tgt & (rqtObject | rqtObstacle)) && (tp == cftObject) && collidable->collidable.model->_RayQuery(Q, CObjectSpaceThreadData::r_temp))
					return true;

				if ((tgt & rqtShape) && (tp == cftShape) && collidable->collidable.model->_RayQuery(Q, CObjectSpaceThreadData::r_temp))
					return true;
			}
		}
	}
	// static test
	if (tgt & rqtStatic) {
		// If we get here - test static model
		if (cache)
		{
			// 0. similar query???
			if (cache->similar(start, dir, range)) {
				return cache->result;
			}

			// 1. Check cached polygon
			float _u, _v, _range;
			if (CDB::TestRayTri(start, dir, cache->verts, _u, _v, _range, false))
			{
				if (_range > 0 && _range < range) return true;
			}

			// 2. Polygon doesn't pick - real database query
			CObjectSpaceThreadData::xrc.ray_query(&Static, start, dir, range);
			auto Num = CObjectSpaceThreadData::xrc.r_count();
			if (!Num) {
				cache->set(start, dir, range, false);
				return false;
			}
			else {
				VERIFY(Num);
				// cache polygon
				cache->set(start, dir, range, true);
				auto& R = CObjectSpaceThreadData::xrc.r_any();
				auto& T = R.model->get_tris()[R.tris_id];
				auto& V = R.model->get_verts();
				cache->verts[0].set(V[T.verts[0]]);
				cache->verts[1].set(V[T.verts[1]]);
				cache->verts[2].set(V[T.verts[2]]);
				R.ModelWorldTransform.transform_tiny(cache->verts[0]);
				R.ModelWorldTransform.transform_tiny(cache->verts[1]);
				R.ModelWorldTransform.transform_tiny(cache->verts[2]);
				return true;
			}
		}
		else {
			CObjectSpaceThreadData::xrc.ray_query(&Static, start, dir, range);
			return CObjectSpaceThreadData::xrc.r_count();
		}
	}
	return false;
	CObjectSpaceThreadData::r_spatial.clear();
}
//--------------------------------------------------------------------------------
// RayPick
//--------------------------------------------------------------------------------
bool CObjectSpace::RayPick(const Fvector& start, const Fvector& dir, float range, rq_target tgt, rq_result& R, CObject* ignore_object)
{
	PROF_EVENT("CObjectSpace::RayPick");
	CObjectSpaceThreadData::r_temp.r_clear();

	R.reset();
	R.range = range; 
	R.element = -1;
	
	// static test
	if (tgt & rqtStatic)
	{
		CObjectSpaceThreadData::xrc.ray_options(CDB::OPT_ONLYNEAREST | CDB::OPT_CULL);
		CObjectSpaceThreadData::xrc.ray_query(&Static, start, dir, range);

		if (CObjectSpaceThreadData::xrc.r_count())
		{
			R.set_if_less(CObjectSpaceThreadData::xrc.r_any());
		}
	}

	// dynamic test
	if (tgt & rqtDyn)
	{
		collide::ray_defs Q(start, dir, R.range, CDB::OPT_ONLYNEAREST | CDB::OPT_CULL, tgt);
		// traverse object database
		ESPATIAL_TYPE d_flags = ESPATIAL_TYPE::COLLIDEABLE | ((tgt & rqtObstacle) ? ESPATIAL_TYPE::OBSTACLE : ESPATIAL_TYPE::NONE) | ((tgt & rqtShape) ? ESPATIAL_TYPE::SHAPE : ESPATIAL_TYPE::NONE);
		g_SpatialSpace->q_ray(CObjectSpaceThreadData::r_spatial, 0, d_flags, start, dir, range);
		// Determine visibility for dynamic part of scene

		for (u32 o_it = 0; o_it < CObjectSpaceThreadData::r_spatial.size(); o_it++)
		{
			PROF_EVENT("CObjectSpace::RayPick::for_loop");
			ISpatial* Spatial = CObjectSpaceThreadData::r_spatial[o_it].get();
			CObject* Collidable = Spatial->dcast_CObject();
			if (nullptr == Collidable)
			{
				continue;
			}
			if (Collidable == ignore_object)
			{
				continue;
			}

			ECollisionFormType tp = Collidable->collidable.model->Type();
			if (((tgt & (rqtObject | rqtObstacle)) && (tp == cftObject)) || ((tgt & rqtShape) && (tp == cftShape)))
			{
				u32 C = color_xrgb(64, 64, 64);
				Q.range = R.range;

				if (Collidable->collidable.model->_RayQuery(Q, CObjectSpaceThreadData::r_temp))
				{
					C = color_xrgb(128, 128, 196);
					R.set_if_less(CObjectSpaceThreadData::r_temp.r_any());
				}
#ifdef DEBUG
				if (bDebug())
				{
					Fsphere S;
					S.P = Spatial->sphere.P;
					S.R = Spatial->sphere.R;
					(*m_pRender)->dbgAddSphere(S, C);
				}
#endif
			}
		}
	}

	CObjectSpaceThreadData::r_spatial.clear();

	return (R.element >= 0);
}

//--------------------------------------------------------------------------------
// RayQuery
//--------------------------------------------------------------------------------
bool CObjectSpace::RayQuery(collide::rq_results& dest, const collide::ray_defs& R, collide::rq_callback* CB, LPVOID user_data, collide::test_callback* tb, CObject* ignore_object)
{
	// initialize query
	dest.r_clear();
	CObjectSpaceThreadData::r_temp.r_clear();

	rq_target	s_mask = rqtStatic;
	rq_target	d_mask = rq_target(((R.tgt & rqtObject) ? rqtObject : rqtNone) |
		((R.tgt & rqtObstacle) ? rqtObstacle : rqtNone) |
		((R.tgt & rqtShape) ? rqtShape : rqtNone));
	ESPATIAL_TYPE d_flags = ESPATIAL_TYPE::COLLIDEABLE | ((R.tgt & rqtObstacle) ? ESPATIAL_TYPE::OBSTACLE : ESPATIAL_TYPE::NONE) | ((R.tgt & rqtShape) ? ESPATIAL_TYPE::SHAPE : ESPATIAL_TYPE::NONE);

	// Test static
	if (R.tgt & s_mask)
	{
		CObjectSpaceThreadData::xrc.ray_options(R.flags);
		CObjectSpaceThreadData::xrc.ray_query(&Static, R.start, R.dir, R.range);
		for (auto& elem : CObjectSpaceThreadData::xrc.r_vec())
		{
			CObjectSpaceThreadData::r_temp.append_result(rq_result().set(elem.ModelWorldTransform, *elem.model, elem.range, elem.tris_id));
		}
	}
	// Test dynamic
	if (R.tgt & d_mask)
	{
		// Traverse object database
		g_SpatialSpace->q_ray(CObjectSpaceThreadData::r_spatial, 0, d_flags, R.start, R.dir, R.range);

		for (u32 o_it = 0; o_it < CObjectSpaceThreadData::r_spatial.size(); o_it++)
		{
			CObject* collidable = CObjectSpaceThreadData::r_spatial[o_it]->dcast_CObject();
			if (nullptr == collidable)
				continue;

			if (collidable == ignore_object)
				continue;

			if (ICollisionForm* cform = collidable->collidable.model)
			{
				ECollisionFormType tp = cform->Type();
				if (((R.tgt & (rqtObject | rqtObstacle)) && (tp == cftObject)) || ((R.tgt & rqtShape) && (tp == cftShape))) {
					if (tb && !tb(R, collidable, user_data))continue;
					cform->_RayQuery(R, CObjectSpaceThreadData::r_temp);
				}
			}
		}
	}

	if (CObjectSpaceThreadData::r_temp.r_count())
	{
		CObjectSpaceThreadData::r_temp.r_sort();
		for (auto& elem : CObjectSpaceThreadData::r_temp.r_results())
		{
			dest.append_result(elem);
			if (!(CB ? CB(elem, user_data) : true))
			{
				return dest.r_count();
			}
			if (R.flags & (CDB::OPT_ONLYNEAREST | CDB::OPT_ONLYFIRST))
			{
				return dest.r_count();
			}
		}
	}
	CObjectSpaceThreadData::r_spatial.clear();
	return dest.r_count();
}

bool CObjectSpace::RayQuery(collide::rq_results& r_dest, ICollisionForm* target, const collide::ray_defs& R)
{
	VERIFY(target);
	r_dest.r_clear();
	return target->_RayQuery(R, r_dest);
}