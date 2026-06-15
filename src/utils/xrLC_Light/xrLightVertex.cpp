#include "stdafx.h"
#include "xrLightVertex.h"
#include "xrFace.h"
#include "xrLC_GlobalData.h"
#include "light_point.h"

#include "../../xrCore/Collision/xrCDB.h"
#include "CUDA/xrCuda_PackedLights.h"
#include "xrDeflector.h"

//-----------------------------------------------------------------------
typedef	xr_multimap<float,vecVertex>	mapVert;
typedef	mapVert::iterator				mapVertIt;
mapVert* g_trans;
xrCriticalSection g_trans_CS;

extern XRLC_LIGHT_API void		LightPoint		(CDB::COLLIDER* DB, CDB::MODEL* MDL, base_color_c &C, Fvector &P, Fvector &N, base_lighting& lights, u32 flags, Face* skip);
 
void	g_trans_register_internal		(Vertex* V)
{
	R_ASSERT	(V);

	const float eps		= EPS_L;
	const float eps2	= 2.f*eps;
	
	// Search
	const float key		= V->P.x;
	mapVertIt	it		= g_trans->lower_bound	(key);
	mapVertIt	it2		= it;

	// Decrement to the start and inc to end
	while (it!=g_trans->begin() && ((it->first+eps2)>key)) it--;
	while (it2!=g_trans->end() && ((it2->first-eps2)<key)) it2++;
	if (it2!=g_trans->end())	it2++;
	
	// Search
	for (; it!=it2; it++)
	{
		vecVertex&	VL		= it->second;
		Vertex* Front		= VL.front();
		R_ASSERT			(Front);
		if (Front->P.similar(V->P,eps))
		{
			VL.push_back		(V);
			return;
		}
	}

	// Register
	mapVertIt	ins			= g_trans->insert(std::make_pair(key,vecVertex()));
	ins->second.reserve		(32);
	ins->second.push_back	(V);
}

void	g_trans_register	(Vertex* V)
{
	g_trans_CS.Enter			();
	g_trans_register_internal	(V);
	g_trans_CS.Leave			();
}

//////////////////////////////////////////////////////////////////////////
bool GetTranslucency(const Vertex* V,float &v_trans )
{
	// Get transluency factor
			
	bool		bVertexLight= false;
	u32 		L_flags		= 0;
	for (u32 f=0; f<V->m_adjacents.size(); ++f)
	{
		Face*	F								=	V->m_adjacents[f];
		v_trans									+=	F->Shader().vert_translucency;
		if	(F->Shader().flags.bLIGHT_Vertex)	
			bVertexLight		= true;
	}
	v_trans				/=	float(V->m_adjacents.size());
	return bVertexLight;
}

xr_atomic_u32 TasksIds = 0;
void LightVertex()
{
	g_trans = new mapVert();

	// Start threads, wait, continue --- perform all the work
	UpdateCurrentPhase("Vertex");

	Status("Calculating...");

	u32 flags = LGetCurrentFlags() | LP_dont_hemi;
 	if (!gCompilerMode.CUDA)
	{
 		TasksIds = 0;
		xr_std_parallel_for([flags]()
		{
			while (true)
			{
				u32 tID = TasksIds.fetch_add(1);
				if (tID >= lc_global_data()->g_vertices().size()) break;

				Vertex* V = lc_global_data()->g_vertices()[tID];
				float		v_trans = 0.f;
				if (GetTranslucency(V, v_trans))
				{
					base_color_c		vC, old;
					V->C._get(old);

					LightPoint(EmbreeMain, vC, V->P, V->N, lc_global_data()->L_static(), flags, 0);

					vC._tmp_ = v_trans;
					vC.mul(.5f);
					vC.hemi = old.hemi;			// preserve pre-calculated hemisphere
					V->C._set(vC);

					g_trans_register(V);
				}
			}
		}, gCompilerMode.ThreadsPerWork);
	}
	else
 	{
#ifdef LCCUDA_BUILD
		int INDEX = 0;
		GPUTaskinSystem.RestartALL();
		GPUTaskinSystem.ColorsMapType = eCommon;
  		GPUTaskinSystem.current_flags = flags;

		xr_vector<float> v_transparency;
		v_transparency.resize(lc_global_data()->g_vertices().size());
		for (auto V : lc_global_data()->g_vertices())
		{
			float		v_trans = 0.f;
 			if (GetTranslucency(V, v_trans))
			{
				GPUTaskinSystem.LightPointPacked_add_task(GPUTaskinSystem.MakeKey(INDEX, 0), nullptr, V->P, V->N, 0);
			}
 			v_transparency[INDEX] = v_trans;
			INDEX++;
		}

		GPUTaskinSystem.LightPointPacked_run_tasks();
		for (auto& C : GPUTaskinSystem.task_colors)
		{
			int INDEX = GPUTaskinSystem.GetU(C.first);
			auto& V = lc_global_data()->g_vertices()[INDEX];
			auto& vC = C.second;
			float Transparency = v_transparency[INDEX];

			base_color_c old;
			V->C._get(old);

			vC._tmp_ = Transparency;
			vC.mul(.5f);
			vC.hemi = old.hemi;
			V->C._set(vC);

			g_trans_register(V);
		}

		GPUTaskinSystem.RestartALL();
#endif
	}
 
	// Process all groups
	Status("Transluenting...");
	for (mapVertIt it = g_trans->begin(); it != g_trans->end(); it++)
	{
		// Unique
		vecVertex& VL = it->second;
		std::sort(VL.begin(), VL.end());
		VL.erase(std::unique(VL.begin(), VL.end()), VL.end());

		// Calc summary color
		base_color_c C;
		for (u32 v = 0; v < VL.size(); v++)
		{
			base_color_c cc;
			VL[v]->C._get(cc);
			C.max(cc);
		}

		// Calculate final vertex color
		for (u32 v = 0; v < VL.size(); v++)
		{
			base_color_c vC;
			VL[v]->C._get(vC);

			base_color_c R;
			R.lerp(vC, C, vC._tmp_); // trans-level
			R.max(vC);
			VL[v]->C._set(R);
		}
	}

	xr_delete(g_trans);
	Status("Wating...");
}