#include "StdAfx.h"
#include "compiler.h"
#include "../../xrCore/Collision/cl_intersect.h"
#include "../xrForms/xrThread.h"
//#include <mmsystem.h>

const int	LIGHT_Count			=2;
const int	LIGHT_Total			=(2*LIGHT_Count+1)*(2*LIGHT_Count+1);

typedef	svector<R_Light_Fast*,1024>	LSelection;

IC bool RayPick(CDB::COLLIDER& DB, Fvector& P, Fvector& D, float r, R_Light_Fast& L)
{
	// 1. Check cached polygon
	float _u,_v,range;
	bool res = CDB::TestRayTri(P,D,L.tri,_u,_v,range,true);
	if (res) {
		if (range>0 && range<r) return true;
	}

	// 2. Polygon doesn't pick - real database query
	try { DB.ray_query(LevelPtr.get(), P, D, r); } catch (...) { Msg("* ERROR: Failed to trace ray"); }
	if (0==DB.r_count()) {
		return false;
	} else {
		// cache polygon
		CDB::RESULT&	rpinf	= *DB.r_begin();

		L.tri[0].set	(rpinf.verts[0]);
		L.tri[1].set	(rpinf.verts[1]);
		L.tri[2].set	(rpinf.verts[2]);
		return true;
	}
}

float LightPoint(CDB::COLLIDER& DB, Fvector &P, Fvector &N, LSelection& SEL)
{
	Fvector		Ldir,Pnew;
	Pnew.mad	(P,N,0.05f);

	R_Light_Fast**IT = SEL.begin(), **E = SEL.end();

	float	amount = 0;
	for (; IT!=E; IT++)
	{
		R_Light_Fast* L = *IT;
		if (L->type==LT_DIRECT) 
		{
			// Cos
			Ldir.invert	(L->direction);
			float D		= Ldir.dotproduct( N );
			if( D <=0 ) continue;

			// Raypick
			if (!RayPick(DB,Pnew,Ldir,1000.f,*L))	amount+=D*L->amount;
		} else {
			// Distance
			float sqD	= P.distance_to_sqr(L->position);
			if (sqD > L->range2) continue;
			
			// Dir
			Ldir.sub	(L->position,P);
			Ldir.normalize_safe();
			float D		= Ldir.dotproduct( N );
			if( D <=0 ) continue;
			
			// Raypick
			float R		= _sqrt(sqD);
			if (!RayPick(DB,Pnew,Ldir,R,*L))
				amount += (D*L->amount)/(L->attenuation0 + L->attenuation1*R + L->attenuation2*sqD);
		}
	}
	return amount;
}

class	LightThread : public CThread
{
	u32	Nstart, Nend;
public:
	LightThread			(u32 ID, u32 _start, u32 _end) : CThread(ID)
	{
		Nstart	= _start;
		Nend	= _end;
	}
	virtual void		Execute()
	{
		CDB::COLLIDER DB;
		DB.ray_options	(CDB::OPT_ONLYFIRST);

		xr_vector<R_Light_Fast>	Lights = g_lights;

		Fvector			P,D,PLP;
		D.set			(0,1,0);
		float coeff		= 0.5f*g_params.fPatchSize/float(LIGHT_Count);
		
		LSelection		Selected;
		float			LperN	= float(g_lights.size());
		for (u32 i=Nstart; i<Nend; i++)
		{
			vertex& N = g_nodes[i];
			
			// select lights
			Selected.clear();
			for (u32 L=0; L<Lights.size(); L++)
			{
				R_Light_Fast&	R = g_lights[L];
				if (R.type==LT_DIRECT)	Selected.push_back(&R);
				else {
					float dist = N.Pos.distance_to(R.position);
					if (dist-g_params.fPatchSize < R.range)
						Selected.push_back(&R);
				}
			}
			LperN = 0.9f*LperN + 0.1f*float(Selected.size());
			
			// lighting itself
			float amount=0;
			for (int x=-LIGHT_Count; x<=LIGHT_Count; x++) 
			{
				P.x = N.Pos.x + coeff*float(x);
				for (int z=-LIGHT_Count; z<=LIGHT_Count; z++) 
				{
					// compute position
					P.z = N.Pos.z + coeff*float(z);
					P.y = N.Pos.y;
					N.Plane.intersectRayPoint(P,D,PLP);	// "project" position
					P.y = PLP.y;
					
					// light point
					amount += LightPoint(DB,P,N.Plane.n,Selected);
				}
			}
			
			// calculation of luminocity
			N.LightLevel	= amount/float(LIGHT_Total);
			
			thProgress		= float(i-Nstart)/float(Nend-Nstart);
		}
	}
};