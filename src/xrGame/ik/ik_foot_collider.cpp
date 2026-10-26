#include "StdAfx.h"
#include "ik_foot_collider.h"

#include "../xrEngine/GameMtlLib.h"
#include "../xrCore/Collision/cl_intersect.h"
#include "../Include/xrRender/Kinematics.h"

#include "Level.h"
#include "GameObject.h"
#include "entity_alive.h"

//#include "ode_include.h"
#include "../xrPhysics/MathUtils.h"

#include "ik_collide_data.h"


#ifdef DEBUG
#include "PHDebug.h"
#endif

ik_foot_collider::ik_foot_collider(): previous_plane_valid( false ), previous_plane_time( 0 ) {}
static const Fplane invalide_plane = { -FLT_MAX, -FLT_MAX, -FLT_MAX, -FLT_MAX };

static const u32 ik_support_hold_ms = 120;
static const float ik_support_max_cos = 0.5f;

ICF void stabilize_support_plane( Fplane &p, Fplane &prev, bool prev_valid )
{
	if( !prev_valid )
		return;

	if( prev.n.dotproduct( p.n ) < 0.f )
	{
		prev.n.invert();
		prev.d = -prev.d;
	}

	if( prev.n.dotproduct( p.n ) < ik_support_max_cos )
	{
		const Fvector pt = Fvector().mul( p.n, -p.d );
		p.n.set( prev.n );
		p.d = -p.n.dotproduct( pt );
	}
}


struct ik_pick_result
{
	ik_pick_result(ik_foot_geom::e_collide_point _point): 
		p(invalide_plane),
		point( _point ),
		position( Fvector().set( -FLT_MAX, -FLT_MAX, -FLT_MAX ) ),
		range( FLT_MAX ),
		object( nullptr ),
		bone( u16(-1) )
	{
		triangle[0] = Fvector().set( -FLT_MAX, -FLT_MAX, -FLT_MAX );
		triangle[1] = Fvector().set( -FLT_MAX, -FLT_MAX, -FLT_MAX );
		triangle[2] = Fvector().set( -FLT_MAX, -FLT_MAX, -FLT_MAX );
	}
	Fplane	p;
	Fvector triangle[3];
	Fvector	position;
	ik_foot_geom::e_collide_point point;
	float	range;
	CObject	*object;
	u16		bone;
};

bool ignore_tri(const CDB::TRI& tri )
{
	SGameMtl* material=GMLib.GetMaterialByIdx( tri.material );

	return	(material->Flags.test( SGameMtl::flPassable )&& 
			!material->Flags.test( SGameMtl::flActorObstacle )
			) ||
			material->Flags.test( SGameMtl::flClimable );// ||
			//material->Flags.test( SGameMtl::flActorObstacle );
}

bool ignore_static_tri(const collide::rq_result& tri )
{
	VERIFY(tri.GetStatic()->tris.size() > tri.element );
	auto& triangle	= tri.GetStatic()->tris[tri.element];
	return ignore_tri( triangle );
}

IC bool ignore_object( CObject	*O )
{
	VERIFY( O );
	if( static_cast<CGameObject*>( O )->cast_entity_alive() /*&& static_cast<CGameObject*>( O )->cast_entity_alive()->g_Alive() */)
		return true;
	return false;
}

IC bool ignore_result(const collide::rq_result& R)
{
	if(!R.IsStatic())
	{
		return ignore_object( const_cast<CObject*>(R.GetDynamic()));
	}
	return ignore_static_tri( R );
}

IC void tri_plane(const Fvector &v0, const Fvector &v1, const Fvector &v2, Fplane &p )
{
	p.n.mknormal	( v0, v1, v2 );
	VERIFY( !fis_zero( p.n.magnitude() ) );
	p.n.invert();
	p.d = -p.n.dotproduct( v0 );
}

IC bool	get_plane_static(  ik_pick_result &r, Fvector &next_pos, float &next_range, const collide::rq_result	&R, float pick_dist, const Fvector &pos, const Fvector &pick_v )
{
	auto Obj = R.GetStatic();
	VERIFY( Obj->tris.size() > R.element );
	auto& tri = Obj->tris[R.element];
	
	R.xform.transform_tiny(r.triangle[0], Obj->verts[tri.verts[0]]);
	R.xform.transform_tiny(r.triangle[1], Obj->verts[tri.verts[1]]);
	R.xform.transform_tiny(r.triangle[2], Obj->verts[tri.verts[2]]);

	tri_plane( r.triangle[0], r.triangle[1], r.triangle[2], r.p );

	r.position.add(pos, Fvector().mul( pick_v, R.range  ) );
	next_pos.set( r.position );
	next_range = pick_dist - R.range;
	if( ignore_tri( tri ) )
	{
		next_pos.add( Fvector().mul( pick_v, EPS_L  ) );
		float dot = pick_v.dotproduct( r.p.n );
		if( 0.f < dot )
		{
			next_pos.add( Fvector().mul( r.p.n, EPS_L  ) );
		}
		//next_pos.add( Fvector().mul( r.p.n, EPS_L  ) );
		next_range -= EPS_L;
#ifdef DEBUG
		float u, v, d;
		VERIFY( !( CDB::TestRayTri( next_pos, pick_v, r.triangle, u, v, d , true ) && d > 0.f ) );
#endif // DEBUG
		return false;
	}
	r.range = R.range;
	return true;
}

IC bool	get_plane_dynamic(  ik_pick_result &r, Fvector &next_pos, float &next_range, const collide::rq_result	R, float pick_dist,const Fvector &pos, const Fvector &pick_v )
{

	next_pos.add( pos, Fvector( ).mul( pick_v, R.range + EPS_L ) );
	next_range = pick_dist - R.range - EPS_L;

	auto Obj = const_cast<CObject*>(R.GetDynamic());
	if(ignore_object(Obj))
	{
		return false;
	}

	IRenderVisual* V = Obj->Visual();
	if( V )
	{
		IKinematics *K = V->dcast_PKinematics();
		if( K )
		{
			float dist = pick_dist;
			IKinematics::pick_result res;
		
			if( K->PickBone(Obj->XFORM(), res , dist,  pos, pick_v, (u16) R.element ) )
			{
				//cld.collided = true;
				r.position.add( pos, Fvector( ).mul( pick_v, res.dist ) );
				r.p.n.invert(res.normal);
				r.p.d = -r.p.n.dotproduct( r.position );
				r.triangle[0] = res.tri[0];
				r.triangle[1] = res.tri[1];
				r.triangle[2] = res.tri[2];
				next_pos.set ( r.position );
				next_range	  = pick_dist - res.dist;
				r.range		  = res.dist; 
				r.object	  = Obj;
				r.bone		  = (u16)R.element;
				return true;
			}
		}
	}
	return false;
}

static const float		reach_dist		=1.5f	;
IC bool get_plane( ik_pick_result &r,  Fvector &next_pos, float &next_range, const collide::rq_result	R, float pick_dist,const Fvector &pos, const Fvector &pick_v )
{
	if(R.IsStatic())
	{
		return get_plane_static(  r, next_pos, next_range, R, pick_dist, pos,  pick_v  );
	}
	return get_plane_dynamic( r, next_pos, next_range, R, pick_dist, pos, pick_v );
}

bool Pick( ik_pick_result &r, const ik_pick_query &q , CObject* ignore_object)
{
	VERIFY( q.is_valid() );

	static thread_local collide::rq_results	results;
	collide::ray_defs	ray( q.pos(), q.dir(), q.range(), CDB::OPT_CULL, collide::rqtBoth );

	g_pGameLevel->ObjectSpace.RayQuery( results, ray, nullptr, nullptr, nullptr, ignore_object );

	bool collided = false;
#ifdef DEBUG
	collide::rq_result*	accepted = nullptr;
#endif
	Fvector next_pos	= q.pos();
	float	next_range	= q.range();
	
	for (auto& elem : results.results)
	{
		collided = get_plane( r, next_pos, next_range,elem, q.range(), q.pos(), q.dir() );
		if( collided )
		{
#ifdef DEBUG
			accepted = &elem;
#endif
			break;
		}
	}

#ifdef DEBUG
	if( ph_dbg_draw_mask1.test( phDbgDrawIKCollision ) && collided && accepted != nullptr && accepted->IsStatic() )
	{
		auto& tri = accepted->GetStatic()->get_tris()[accepted->element];
		DBG_DrawLine(q.pos(), r.position, color_xrgb(255, 0, 0));
		DBG_DrawTri(TriabgleCDBData{accepted->xform, accepted->GetStatic(), (size_t)(accepted->element)}, color_xrgb(255, 0, 0));
	}
#endif
	return collided;
}

//void DBG_DrawTri( const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 ac, bool solid );

void chose_best_plane( Fplane &p, const Fvector &v, Fplane &p0, Fplane &p1, Fplane &p2   )
{
	MAX_OF( p0.n.dotproduct(v), p = p0, p1.n.dotproduct(v), p = p1, p2.n.dotproduct(v), p = p2 );
}

#ifdef	DEBUG
void DBG_DrawTri( const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 ac, bool solid );
#endif

void ik_foot_collider::collide( SIKCollideData &cld, const ik_foot_geom &foot_geom, CGameObject *O, bool foot_step )
{
	VERIFY( foot_geom.is_valid() ); 
	cld.collided = false;
	cld.m_support_object = nullptr;
	cld.m_support_bone = u16(-1);
	
	
	float pick_dist = collide_dist;
	//if( foot_step )
	pick_dist += reach_dist ;

//////////////////////////////////////////////////////////////////////////////////////
	const Fvector& toe_pick_v	= cld.m_pick_dir; 
	const Fvector pos_toe		= Fvector().sub( foot_geom.pos_toe(), Fvector( ).mul( toe_pick_v, collide_dist ) );
	ik_pick_query  q_toe( ik_foot_geom::toe, pos_toe, toe_pick_v, pick_dist );

////////////////////////////////////////////////////////////////////////////////////////
	const	Fvector hill_pick_v = cld.m_pick_dir;
	const	Fvector pos_heel	= Fvector().sub( foot_geom.pos_heel(), Fvector( ).mul( hill_pick_v, collide_dist ) );
	ik_pick_query  q_heel( ik_foot_geom::heel, pos_heel, hill_pick_v, pick_dist );

//////////////////////////////////////////////////////////////////////////////////////////
	const	Fvector side_pick_v = cld.m_pick_dir;
	const	Fvector pos_side	= Fvector().sub( foot_geom.pos_side(), Fvector( ).mul( side_pick_v, collide_dist ) );
	ik_pick_query  q_side( ik_foot_geom::side, pos_side, side_pick_v, pick_dist );
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	if( previous_toe_query.is_equal		( q_toe  )	&& 
		previous_heel_query.is_equal( q_heel )	&& 
		previous_side_query.is_equal( q_side ) 
	){
		cld = previous_data;
		return;
	}

	const float foot_length = Fvector().sub( pos_toe, pos_heel ).magnitude() * 1.5f;

	previous_heel_query = q_heel;
	previous_side_query = q_side;
	previous_toe_query	= q_toe;

	ik_pick_result r_toe(ik_foot_geom::toe);
	cld.collided = Pick( r_toe, q_toe, O );
	cld.m_plane = r_toe.p;
	cld.m_collide_point = ik_foot_geom::toe;
	cld.m_support_object = r_toe.object;
	cld.m_support_bone = r_toe.bone;
//////////////////////////////////////////////////////////////////////////////////////
	

#ifdef DEBUG
	if( ph_dbg_draw_mask1.test( phDbgDrawIKCollision ) )
	{
		DBG_DrawPoint(pos_toe, 0.01f, color_xrgb(255, 0, 0));
		if (cld.collided)
			DBG_DrawPoint(r_toe.position, 0.01f, color_xrgb(0, 0, 255));
	}
#endif

	ik_pick_result r_heel(ik_foot_geom::heel); 
	bool heel_collided = Pick( r_heel, q_heel, O ) ;

	ik_pick_result r_side(ik_foot_geom::side); 
	bool side_collided = Pick( r_side, q_side, O ) ;

	bool toe_heel_compatible = cld.collided && heel_collided && Fvector().sub( r_heel.position, r_toe.position ).magnitude() < foot_length;
	bool toe_side_compatible = cld.collided && side_collided && Fvector().sub( r_side.position, r_toe.position ).magnitude() < foot_length;


/*
	if( hill_collided )
	{
		if( !cld.collided || (r_hill.position.y - r_toe.position.y) > EPS )
		{
			cld.m_plane = r_heel.p;
			cld.m_collide_point = ik_foot_geom::heel;
			cld.collided = true;
			cld.m_pick_dir = heel_pick_v;
			
		} 
	}
*/


	if( toe_heel_compatible && toe_side_compatible )
	{
		
			Fplane plane;
			tri_plane( r_toe.position, r_heel.position , r_side.position, plane );
			if( plane.n.dotproduct( r_toe.p.n ) < 0.f )
			{
							
				plane.n.invert();
				plane.d = -plane.d;
			}

			 cld.m_plane  = plane;
			cld.m_support_object = r_toe.object != nullptr ? r_toe.object : ( r_heel.object != nullptr ? r_heel.object : r_side.object );
			cld.m_support_bone = r_toe.bone != u16(-1) ? r_toe.bone : ( r_heel.bone != u16(-1) ? r_heel.bone : r_side.bone );
			stabilize_support_plane( cld.m_plane, previous_plane, previous_plane_valid );
			previous_plane = cld.m_plane;
			previous_plane_valid = true;
			previous_plane_time = Device.dwTimeGlobal;
#ifdef DEBUG
			if( ph_dbg_draw_mask1.test( phDbgDrawIKCollision ) )
			{
				DBG_DrawPoint(pos_toe, 0.01f, color_xrgb(255, 0, 0));
				if(cld.collided)
				{
					DBG_DrawTri(r_toe.position, r_heel.position, r_side.position, color_xrgb(0, 0, 255), false);
				}
			}
#endif
			previous_data = cld;
			return;
	}

	ik_pick_result r = r_toe;
	float range_best = r_toe.range;

	if( !cld.collided )
		range_best = FLT_MAX;

	if( heel_collided && ( !cld.collided || r_heel.range < range_best ) )
	{
		r = r_heel;
		range_best = r_heel.range;
		cld.collided = true;
	}

	if( side_collided && ( !cld.collided || r_side.range < range_best ) )
	{
		r = r_side;
		range_best = r_side.range;
		cld.collided = true;
	}

	if( cld.collided )
	{
		cld.m_plane = r.p;
		cld.m_collide_point =r.point;
		cld.m_support_object = r.object;
		cld.m_support_bone = r.bone;
		stabilize_support_plane( cld.m_plane, previous_plane, previous_plane_valid );
		previous_plane = cld.m_plane;
		previous_plane_valid = true;
		previous_plane_time = Device.dwTimeGlobal;
		previous_data = cld;
		return;
	}

	if( previous_plane_valid && Device.dwTimeGlobal - previous_plane_time <= ik_support_hold_ms )
	{
		cld.m_plane = previous_plane;
		cld.m_collide_point = ik_foot_geom::toe;
		cld.collided = true;
	}

	//chose_best_plane( cld.m_plane, pick_v, plane, r_hill.p, r_toe.p   );


	previous_data = cld;

/*
	float u,v,d;
	if( ( !cld.collided ||
		  !( CDB::TestRayTri(pos_hill, pick_v, r_toe.triangle, u, v, d, true ) && d > 0.f ) ) &&
		    Pick( r_hill, pos_hill, pick_v, pick_dist, O ) )
	{

		if( !cld.collided || r_hill.position.y > r_toe.position.y )
		{
			cld.m_plane = r_hill.p;
			cld.m_collide_point = SIKCollideData::heel;
		} 
		
		//else
		//{
		//		ik_pick_result r_foot;
		//		Fvector pos_foot = Fvector().sub( foot.c, Fvector( ).mul( pick_v, collide_dist ) );
		//		if( cld.collided && Pick( r_foot, pos_foot, pick_v, pick_dist, O ) )
		//		{
		//			Fplane plane;
		//			tri_plane( r_toe.position, r_hill.position , r_foot.position, plane );

		//			DBG_DrawTri(r_toe.position, r_hill.position, r_foot.position , color_xrgb( 255, 255, 255 ), false  );
		//		
		//			if( plane.n.dotproduct( r_hill.p.n ) < 0.f )
		//			{
		//				plane.n.invert();
		//				plane.d = -cld.m_plane.d;
		//			
		//			}
		//			chose_best_plane( cld.m_plane, pick_v, plane, r_hill.p, r_toe.p   );
		//		}
		//}
		
		cld.collided = true;
	}
*/
}

