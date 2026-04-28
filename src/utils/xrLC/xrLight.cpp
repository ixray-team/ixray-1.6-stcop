#include "StdAfx.h"
#include "Build.h"

#include "../xrForms/CompilersUI.h"
#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrLightVertex.h"
#include "../xrLC_Light/xrFace.h"

#include "../../xrCore/xrSyncronize.h"

#include "../xrLC_Light/mu_model_light.h"
 
void CBuild::ProcessLMAPS_CPU()
{
	thread_local CDB::COLLIDER	DB;
	thread_local base_lighting	LightsSelected;
 
	u32 CurrentIndex = 0;
	static std::mutex task_CS;

	xr_parallel_for(0, gCompilerMode.ThreadsPerWork, [&](int THREAD)
		{
			while (true)
			{
				// Get task
				u32 IndexTask = 0;
				
				{
					std::lock_guard lock(task_CS);
 					IndexTask = CurrentIndex;
					CurrentIndex++;
 					Progress(float(CurrentIndex) / float(lc_global_data()->g_deflectors().size()));
 				}

				if (IndexTask >= lc_global_data()->g_deflectors().size()) break;

 				CDeflector* D = lc_global_data()->g_deflectors()[IndexTask];
 				if (IndexTask % 512 == 0)
					AditionalData("Deflectors: %u / %u", IndexTask, lc_global_data()->g_deflectors().size());


				// Perform operation
				try
				{
					D->Light(&DB, &LightsSelected);
				}
				catch (...)
				{
					clMsg("* ERROR: CLMThread::Execute - light");
				}
			}			
		}
	);
};


#ifdef LCCUDA_BUILD
#include "../xrLC_Light/xrDeflectorLight_Packed.h"
#include "../xrLC_Light/light_point.h"
#endif

extern XRCORE_API bool			g_bEnableStatGather;

void	CBuild::LMaps					()
{
	g_bEnableStatGather = true;

	mem_Compact();
	const bool Cuda   = gCompilerMode.CUDA;
	const bool Embree = gCompilerMode.Embree;

	string128 tmp_phase;
	sprintf(tmp_phase, "LIGHT: LMaps (*%s*)", Cuda ? "CUDA" : Embree ? "Embree" : "Opcode");
	Phase(tmp_phase);

#ifdef LCCUDA_BUILD
	if (gCompilerMode.CUDA)
	{
		// Se7kills 
 		CTimer start_time; start_time.Start();
	 
		GPUTaskinSystem.RestartALL();
		GPUTaskinSystem.ColorsMapType = eDeflectors;
		GPUTaskinSystem.current_flags = (gCompilerMode.LC_NoSun ? LP_dont_sun : 0) | LP_UseFaceDisable;
 
		CTimer tStats; tStats.Start();
 		auto ProcessDeflectors = [](xr_vector<CDeflector*>& deflectors)
		{
 			xr_atomic_u32 IndexTaskID = 0, IndexTaskApply = 0, IndexTaskExpand = 0;
			xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TID)
			{
				while (true)
				{
					u32 Index = IndexTaskID.fetch_add(1);
					if (Index >= deflectors.size()) break;
					CDeflector* D = deflectors[Index];

					D->LightGPU();
 
					AditionalData("*** [LMAPS] ID [%u/%u] W: %u | H: %u",
						Index, deflectors.size(), D->layer.width, D->layer.height);
				}

				// Система тасков щас иная
				GPUTaskinSystem.LightPointPacked_run_tasks();
			});

			xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TID)
			{
 				while (true)
				{
					u32 Index = IndexTaskApply.fetch_add(1);
					if (Index >= deflectors.size()) break;
					CDeflector* D = deflectors[Index];

					D->ApplyColors();
					D->ApplyExpandBordersGPU();

					AditionalData("*** [LMAPS] ApplyID [%u/%u] W: %u | H: %u",
						Index, deflectors.size(), D->layer.width, D->layer.height);
				}
			});


 		};

		u32 AreaCollected = 0; u32 IndexD = 0;
		xr_vector<CDeflector*> deflectors_map;
 		for (auto& D : lc_global_data()->g_deflectors())
		{
			// deflectors.
			if (AreaCollected > 8192 * 8192 * 20 || IndexD == lc_global_data()->g_deflectors().size() )
			{
				// Lmaps Process
				ProcessDeflectors(deflectors_map);
				// Merge LMAPS
				xrPhase_MergeLM(deflectors_map);

 				deflectors_map.clear();
				AreaCollected = 0;
 			}
			
			IndexD++;
			AreaCollected += D->layer.Area();
			deflectors_map.push_back(D);
		}

		if (deflectors_map.size())
		{
			// Lmaps Process
			ProcessDeflectors(deflectors_map);
			// Merge LMAPS
			xrPhase_MergeLM(deflectors_map);

			deflectors_map.clear();
			AreaCollected = 0;
		}
  
		clMsg("%d lightmaps builded", lc_global_data()->lightmaps().size());
   	}
	else
#endif
	{
		// Main process (4 threads)
		Status("Lighting...");

		CTimer start_time; start_time.Start();
		ProcessLMAPS_CPU();
		clMsg("%f seconds", start_time.GetElapsed_sec());
		 
		//****************************************** Merge LMAPS
		xrPhase_MergeLM( lc_global_data()->g_deflectors() );
	}



	clMsg("Start Destroy Deflectors: Memory: %llu mb used", u32(GetHeapMemory() / 1024 / 1024));
	for (u32 it = 0; it < lc_global_data()->g_deflectors().size(); it++)
		xr_delete(lc_global_data()->g_deflectors()[it]);
	lc_global_data()->g_deflectors().clear();
	clMsg("End Destroy Deflectors: Memory: %llu mb used", u32(GetHeapMemory() / 1024 / 1024));
}
 
void CBuild::Light()
{
 	//****************************************** Resolve materials
 	Phase("Resolving materials...");
 	xrPhase_ResolveMaterials();
	IsolateVertices(true);

	//****************************************** UV mapping
 	Phase("Build UV mapping...");
 	xrPhase_UVmap();
	IsolateVertices(true);
	 
	//****************************************** Subdivide geometry
	Phase("Subdividing geometry...");
	xrPhase_Subdivide();
	lc_global_data()->vertices_isolate_and_pool_reload();
	IsolateVertices(true);

	//****************************************** LMAPS
	LMaps();

	//****************************************** Starting MU
	run_mu_light();
 
	//****************************************** Implicit
  	ImplicitLighting();
	  
	//****************************************** Vertex
 	LightVertex();
 
	//****************************************** Merge geometry
	Phase("Merging geometry...");
 	xrPhase_MergeGeometry();
  
	//****************************************** Destroy RCast-model
 	Phase("Destroying ray-trace model...");
 	lc_global_data()->destroy_rcmodel();
	if (gCompilerMode.Embree)
		EmbreeMain.IntelEmbereUnloadAll();	
}

void CBuild::LightVertex	()
{
	::LightVertex();
}