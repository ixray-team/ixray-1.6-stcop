#include "StdAfx.h"
#include "compiler.h"

#include "../../xrCore/Collision/cl_intersect.h"
#include "../../xrGame/quadtree.h"
#include "compiler_cover_helper.h"
#include "../xrForms/CompilersUI.h"

#include "../xrLC_Light/embree_raytracing/EmbreeGeometryBuilder.h"

Shader_xrLC_LIB*				g_shaders_xrlc	;
xr_vector<b_material>			g_materials		;
xr_vector<b_shader>				g_shader_render	;
xr_vector<b_shader>				g_shader_compile;
xr_vector<b_BuildTexture>		g_textures		;
xr_vector<b_rc_face>			g_rc_faces		;

extern xr_vector<bool>			g_cover_nodes;

// -------------------------------- Ray pick
IC float getLastRP_Scale(CDB::COLLIDER* DB)
{
	u32	tris_count = DB->r_count();
	float	scale = 1.f;
	Fvector B;

	for (u32 I = 0; I < tris_count; I++)
	{
		CDB::RESULT& rpinf = DB->r_begin()[I];
		b_rc_face& F = g_rc_faces[rpinf.id];

		if (F.dwMaterial >= g_materials.size())
			Msg("[%d] -> [%d]", F.dwMaterial, g_materials.size());

		b_material& M = g_materials[F.dwMaterial];
		b_texture& T = g_textures[M.surfidx];
		Shader_xrLCVec& LIB = g_shaders_xrlc->Library();

		if (M.shader_xrlc >= LIB.size())
			return 0;		//. hack

		Shader_xrLC& SH = LIB[M.shader_xrlc];

		if (!SH.flags.bLIGHT_CastShadow)
			continue;

		if (T.pSurface.Empty())
			T.bHasAlpha = false;

		// barycentric coords
		// note: W,U,V order
		B.set(1.0f - rpinf.u - rpinf.v, rpinf.u, rpinf.v);

		// calc UV
		Fvector2* cuv = F.t;
		Fvector2	uv;
		uv.x = cuv[0].x * B.x + cuv[1].x * B.y + cuv[2].x * B.z;
		uv.y = cuv[0].y * B.x + cuv[1].y * B.y + cuv[2].y * B.z;

		int U = iFloor(uv.x * float(T.dwWidth) + .5f);
		int V = iFloor(uv.y * float(T.dwHeight) + .5f);
		U %= T.dwWidth;		if (U < 0) U += T.dwWidth;
		V %= T.dwHeight;	if (V < 0) V += T.dwHeight;

		u32 pixel = ((u32*)*T.pSurface)[V * T.dwWidth + U];
		u32 pixel_a = color_get_A(pixel);
		float opac = 1.f - float(pixel_a) / 255.f;
		scale *= opac;
	}

	return scale;
}

IC float rayTrace	(CDB::COLLIDER* DB, Fvector& P, Fvector& D, float R)
{
	R_ASSERT	(DB);

	// 1. Polygon doesn't pick - real database query
	DB->ray_query	(LevelPtr.get(),P,D,R);

	// 2. Analyze polygons 
	if (0==DB->r_count()) {
		return 1;
	} else {
		return getLastRP_Scale(DB);
	}
}

// volumetric query
xr_atomic_u32 tAtomicIndex = 0;
  
extern void compute_cover_nodes();
extern void compute_non_covers();

static void compute_cover_value(CDB::COLLIDER& DB, CoverBuilder::Query& Q, u32 const& N, vertex& BaseNode, float const& cover_height, Cover& cover)
{
	Fvector& BasePos = BaseNode.Pos;
	Fvector		TestPos = BasePos; TestPos.y += cover_height;

	float	c_total[8] = { 0,0,0,0,0,0,0,0 };
	float	c_passed[8] = { 0,0,0,0,0,0,0,0 };

	// perform volumetric query
	Q.Init(BasePos);
	Q.Perform(N);

	// main cycle: trace rays and compute counts
	for (auto& ID : Q.q_List)
	{
		// calc dir & range
		R_ASSERT(ID < g_nodes.size());
		if (N == ID)		continue;

		vertex& N_ = g_nodes[ID];
		Fvector& Pos = N_.Pos;
		Fvector		Dir;
		Dir.sub(Pos, BasePos);
		float		range = Dir.magnitude();
		Dir.div(range);

		// raytrace
		int			sector = CoverBuilder::calcSphereSector(Dir);
		c_total[sector] += 1.f;
		c_passed[sector] += rayTrace(&DB, TestPos, Dir, range); //
	}
	Q.Clear();

	// analyze probabilities
	float	value[8];
	for (int dirs = 0; dirs < 8; dirs++) {
		R_ASSERT(c_passed[dirs] <= c_total[dirs]);
		if (c_total[dirs] == 0)	value[dirs] = 0;
		else					value[dirs] = float(c_passed[dirs]) / float(c_total[dirs]);
		clamp(value[dirs], 0.f, 1.f);
	}

	if (value[0] < .999f) {
		value[0] = value[0];
	}

	cover[0] = (value[2] + value[3] + value[4] + value[5]) / 4.f; clamp(cover[0], 0.f, 1.f);	// left
	cover[1] = (value[0] + value[1] + value[2] + value[3]) / 4.f; clamp(cover[1], 0.f, 1.f);	// forward
	cover[2] = (value[6] + value[7] + value[0] + value[1]) / 4.f; clamp(cover[2], 0.f, 1.f);	// right
	cover[3] = (value[4] + value[5] + value[6] + value[7]) / 4.f; clamp(cover[3], 0.f, 1.f);	// back
}


void	xrCover	(bool pure_covers)
{
	Status("Calculating...");

	if (!pure_covers)
		compute_cover_nodes	();
	else
		g_cover_nodes.assign(g_nodes.size(),true);

	// Start threads, wait, continue --- perform all the work 
	// se7kills : Переработал 
	tAtomicIndex = 0;
	thread_local CDB::COLLIDER		DB;
	thread_local CoverBuilder::Query				Q;

 	xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [](size_t threadID) 
	{
		DB.ray_options(CDB::OPT_CULL);
		Q.Begin(g_nodes.size());

		while (true)
		{
			u32 NodeID = tAtomicIndex.fetch_add(1);
			if (g_nodes.size() >= NodeID) break;

 			// initialize process
			vertex& BaseNode = g_nodes[NodeID];

			if (!g_cover_nodes[NodeID])
			{
				BaseNode.high_cover[0] = flt_max;
				BaseNode.high_cover[1] = flt_max;
				BaseNode.high_cover[2] = flt_max;
				BaseNode.high_cover[3] = flt_max;
				BaseNode.low_cover[0] = flt_max;
				BaseNode.low_cover[1] = flt_max;
				BaseNode.low_cover[2] = flt_max;
				BaseNode.low_cover[3] = flt_max;
				continue;
			}

			compute_cover_value(DB, Q, NodeID, BaseNode, high_cover_height, BaseNode.high_cover);
			compute_cover_value(DB, Q, NodeID, BaseNode, low_cover_height, BaseNode.low_cover);
		}
	});

	if (!pure_covers) {
		compute_non_covers	();
		return;
	}

	// Smooth
	Status			("Smoothing coverage mask...");
 	Nodes	Old		= g_nodes;
	for (u32 N=0; N<g_nodes.size(); N++)
	{
		vertex&	Base		= Old[N];
		vertex&	Dest		= g_nodes[N];
		
		for (int dir=0; dir<4; dir++)
		{
			float val		= 2*Base.high_cover[dir];
			float val2		= 2*Base.low_cover[dir];
			float cnt		= 2;
			
			for (int nid=0; nid<4; nid++) {
				if (Base.n[nid]!=InvalidNode) {
					val		+=  Old[Base.n[nid]].high_cover[dir];
					val2	+=  Old[Base.n[nid]].low_cover[dir];
					cnt		+=	1.f;
				}
			}
			Dest.high_cover[dir]	=  val/cnt;
			Dest.low_cover[dir]		=  val2/cnt;
		}
	}
}
