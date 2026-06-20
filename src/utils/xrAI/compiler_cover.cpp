#include "StdAfx.h"
#include "compiler.h"
#include "../xrForms/CompilersUI.h"
#include "../../xrCore/Collision/cl_intersect.h"
#include "../../xrGame/quadtree.h"
 
#include "compiler_cover_helper.h"
#include "compiler_data.h"
#include "../xrLC_Light/embree_raytracing/EmbreeGeometryBuilder.h"
#include "compiler_embree.h"

extern xr_vector<bool>			g_cover_nodes;
 
// --- ENERGY --- 
inline float getEnergy(const b_texture& T, Fvector2* TC, float hit_u, float hit_v)
{
	float Barry0 = 1.0f - hit_u - hit_v;

	// calc UV
	float u = TC[0].x * Barry0 + TC[1].x * hit_u + TC[2].x * hit_v;
	float v = TC[0].y * Barry0 + TC[1].y * hit_u + TC[2].y * hit_v;

	int U = (int)floor(u * T.dwWidth + 0.5f);
	int V = (int)floor(v * T.dwHeight + 0.5f);

	U = (U % T.dwWidth + T.dwWidth) % T.dwWidth;
	V = (V % T.dwHeight + T.dwHeight) % T.dwHeight;

 	u32 pixel_a = color_get_A( ((u32*)*T.pSurface)[V * T.dwWidth + U] );
	return 1.f - (float(pixel_a) / 255.f);
}
 

// --- OPCODE ---s
IC float rayTrace(CDB::COLLIDER* DB, Fvector& P, Fvector& D, float R)
{
	auto Energy = [DB]() -> float
	{
		float scale = 1.f;
		for (u32 I = 0; I < DB->r_count(); I++)
		{
			CDB::RESULT& RP = DB->r_begin()[I];
			FaceDataEmbree& F = (*(FaceDataEmbree*)(CAIRayTrace.static_geom.dummy[RP.id]));

			b_material& M = comp_data.g_materials[F.dwMaterial];
			Shader_xrLCVec& LIB = comp_data.g_shaders_xrlc->Library();
			if (M.shader_xrlc >= LIB.size())
			{
				return 0;
			}

			b_texture& T = comp_data.g_textures[M.surfidx];
			if (T.pSurface.Empty())
			{
				T.bHasAlpha = false;
				return 0;
			}
			scale *= getEnergy(T, F.getTC0(), RP.u, RP.v);
		}

		return scale;
	};

	// 1. Polygon doesn't pick - real database query
	DB->ray_query(comp_data.LevelPtr.get(), P, D, R);

	// 2. Analyze polygons
	return DB->r_count() == 0 ? 1 : Energy();
}

// --- EMBREE ---
void FilterRayTraceAI(const struct RTCFilterFunctionNArguments* args)
{
	if (!args->valid[0]) return;
	args->valid[0] = 0;

 	u32& primID = RTCHitN_primID(args->hit, args->N, 0);
	FaceDataEmbree* F = (FaceDataEmbree*)CAIRayTrace.static_geom.dummy[primID];
 	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	if (F == ctxt->skip) return;
 
	float& hit_u = RTCHitN_u(args->hit, args->N, 0);
	float& hit_v = RTCHitN_v(args->hit, args->N, 0);

	const b_material& M = comp_data.g_materials[F->dwMaterial];
	const b_texture& T = comp_data.g_textures[M.surfidx];

	// fetch pixel
	if (T.pSurface.Empty() || F->bOpaque)
	{
		ctxt->energy = 0;
		args->valid[0] = -1;
		return;
	}
 
	ctxt->energy *= getEnergy(T, F->getTC0(), hit_u, hit_v);
}

// volumetric query
  
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

		if (gCompilerMode.Embree)
		{
			c_passed[sector] += CAIRayTrace.Raytrace(TestPos, Dir, range, FilterRayTraceAI); //
		}
		else
		{
			c_passed[sector] += rayTrace(&DB, TestPos, Dir, range);							 //
		} 
 	}
	Q.Clear();

	// analyze probabilities
	float	value[8];
	for (int dirs = 0; dirs < 8; dirs++) 
	{
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
	static xr_atomic_u32 tAtomicIndex = 0;
	tAtomicIndex = 0;
	thread_local CDB::COLLIDER		DB;
	thread_local CoverBuilder::Query				Q;

	static xr_atomic_u32 tAtomicProcessed = 0;
	tAtomicProcessed = 0;

 	xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [](size_t threadID) 
	{
		DB.ray_options(CDB::OPT_CULL);
		Q.Begin(g_nodes.size());

		while (true)
		{
			u32 NodeID = tAtomicIndex.fetch_add(1);
			if (NodeID >= g_nodes.size()) break;

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

			AditionalData("Processing Node: %u/%u | CoverID: %u", NodeID, g_nodes.size(), tAtomicProcessed.fetch_add(1));

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
