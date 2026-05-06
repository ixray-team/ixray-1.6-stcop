#include "StdAfx.h"
#include "Build.h"

#include "../xrForms/CompilersUI.h"
#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrLightVertex.h"
#include "../xrLC_Light/xrFace.h"

#include "../../xrCore/xrSyncronize.h"
#include "../xrLC_Light/mu_model_light.h"
#include "../xrLC_Light/light_point.h"

#ifdef LCCUDA_BUILD
#include "../xrLC_Light/CUDA/CUDARayCast.h"
#endif 

extern XRCORE_API bool			g_bEnableStatGather;

void	CBuild::LMaps					()
{
	g_bEnableStatGather = true;

	mem_Compact();
	UpdateCurrentPhase("LMaps");

#ifdef LCCUDA_BUILD
	if (gCompilerMode.CUDA)
	{
		// Se7kills 
 		CTimer start_time; start_time.Start();

		GPUTaskinSystem.RestartALL();
		GPUTaskinSystem.ColorsMapType = eDeflectors;
		GPUTaskinSystem.current_flags = LGetCurrentFlags();

		auto& deflectors = lc_global_data()->g_deflectors();

		xr_atomic_u32 IndexTaskID = 0, IndexTaskApply = 0;
		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TID)
			{
				while (true)
				{
					u32 Index = IndexTaskID.fetch_add(1);
					if (Index >= deflectors.size()) break;
					CDeflector* D = deflectors[Index];
					D->LightGPU();
					AditionalData("*** [LMAPS] ID [%u/%u]", Index, deflectors.size());
				}
				GPUTaskinSystem.LightPointPacked_run_tasks(); // Завершаем задачи !
			});
		GPUTaskinSystem.RestartALL();

		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TID)
			{
				while (true)
				{
					u32 Index = IndexTaskApply.fetch_add(1);
					if (Index >= deflectors.size()) break;
					CDeflector* D = deflectors[Index];

					D->ApplyColors();
					D->ApplyExpandBordersGPU();
					AditionalData("*** [LMAPS] ApplyID [%u/%u]", Index, deflectors.size());
				}
			});

		clMsg("%f seconds", start_time.GetElapsed_sec());
   	}
	else
#endif
	{
		// Main process (4 threads)
		Status("Lighting...");

		CTimer start_time; 
		start_time.Start();
		
		xr_atomic_u32 LmapsTaskID = 0;
		xr_parallel_for(0, gCompilerMode.ThreadsPerWork, [&](int THREAD)
		{
			CDB::COLLIDER	DB;
			base_lighting	LightsSelected;
			while (true)
			{
				// Get task
				u32 IndexTask = LmapsTaskID.fetch_add(1);
				if (IndexTask >= lc_global_data()->g_deflectors().size()) break;
 				CDeflector* D = lc_global_data()->g_deflectors()[IndexTask];
				D->Light(&DB, &LightsSelected);
				AditionalData("Deflectors: %u / %u", IndexTask, lc_global_data()->g_deflectors().size());
			}
		}
		);
		clMsg("%f seconds", start_time.GetElapsed_sec());
	}
}
 
void CBuild::Light()
{
	auto InitModel = [this]()
	{
#ifdef LCCUDA_BUILD
		if (gCompilerMode.CUDA)
			GPUTaskinSystem.InitializeGPU_Model();				// Memory Usage 150-300MB !
		else
#endif
		if (gCompilerMode.Embree)
 			EmbreeMain.InitializeGeometry();
 		else
			BuildRapid(false);
	};

	auto UnloadModel = [this]()
	{
 		// Unloading Ray Casting Models !
		lc_global_data()->destroy_rcmodel();

		if (gCompilerMode.Embree)
			EmbreeMain.IntelEmbereUnloadAll();
#ifdef LCCUDA_BUILD
		if (gCompilerMode.CUDA)
			GPUTaskinSystem.DestroyGPU_Model();
#endif
	};

	auto BuildingUV = [this]()
	{
		Phase("Building Normals...");
		CalcNormals();
 

		Phase("Building tangent-basis ...");
 		xrPhase_TangentBasis();

		Phase("Building UV...");
		//****************************************** Resolve materials
		xrPhase_ResolveMaterials();
		IsolateVertices(true);

		//****************************************** UV mapping
		xrPhase_UVmap();
		IsolateVertices(true);

		//****************************************** Subdivide geometry
		xrPhase_Subdivide();
		IsolateVertices(true);
	};

	// Hemi MT - Calculate
	Light_prepare();

	// ***************************************** Computing UV
	BuildingUV();
	InitModel();
  
	//****************************************** AdaptiveHT расщет
	xrPhase_AdaptiveHT_calculate();		
	
	//****************************************** Implicit
	ImplicitLighting();
	  
	//****************************************** LMAPS
	LMaps();

	//****************************************** Starting MU
	run_mu_light();
 
	//****************************************** Vertex
 	LightVertex();
 
 	//****************************************** Merge LMAPS
	xrPhase_MergeLM();

	//****************************************** Merge geometry
	Phase("Merging geometry...");
 	xrPhase_MergeGeometry();
	UnloadModel();
}

void CBuild::LightVertex	()
{
	::LightVertex();
}