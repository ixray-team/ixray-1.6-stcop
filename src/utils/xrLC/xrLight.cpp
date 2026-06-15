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
  		xr_std_parallel_for([&]()
			{
				while (true)
				{
					u32 Index = IndexTaskID.fetch_add(1);
					if (Index >= deflectors.size())
					{
						break;
					}
					CDeflector* D = deflectors[Index];
					D->LightGPU();
					AditionalData("*** [LMAPS] ID [%u/%u]", Index, deflectors.size());
				}
				GPUTaskinSystem.LightPointPacked_run_tasks(); // Завершаем задачи !
			},
			gCompilerMode.ThreadsPerWork );
		GPUTaskinSystem.RestartALL();

		xr_std_parallel_for([&]()
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
			},
			gCompilerMode.ThreadsPerWork );

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
		xr_std_parallel_for([&]()
		{
 			base_lighting	LightsSelected;
			while (true)
			{
				// Get task
				u32 IndexTask = LmapsTaskID.fetch_add(1);
				if (IndexTask >= lc_global_data()->g_deflectors().size()) break;
 				CDeflector* D = lc_global_data()->g_deflectors()[IndexTask];
				D->Light(&LightsSelected);
				AditionalData("Deflectors: %u / %u", IndexTask, lc_global_data()->g_deflectors().size());
			}
		}, gCompilerMode.ThreadsPerWork );
		clMsg("%f seconds", start_time.GetElapsed_sec());
	}


	xrPhase_MergeLM();
}
 
void CBuild::Light()
{
	auto InitModel = [this]()
	{
		InitializeEmbreeDevice();

#ifdef LCCUDA_BUILD
		if (gCompilerMode.CUDA)
		{
			GPUTaskinSystem.InitializeGPU();
			GPUTaskinSystem.InitializeGPU_Model();				// Memory Usage 150-300MB !
		}
		else
#endif
 			EmbreeMain.InitializeGeometry();
	};

	auto BuildingUV = [this]()
	{
		CalcNormals();
 
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

	//****************************************** Vertex
	LightVertex();

	//****************************************** Starting MU
	run_mu_light();

	//****************************************** Merge geometry
	Phase("Merging geometry...");
 	xrPhase_MergeGeometry();

	// Unloading Ray Casting Models !
	EmbreeMain.IntelEmbereUnloadAll();
#ifdef LCCUDA_BUILD
	GPUTaskinSystem.DestroyGPU_Model();
#endif
}

void CBuild::LightVertex	()
{
	::LightVertex();
}