#include "StdAfx.h"
#include "Build.h"

#include "../xrForms/CompilersUI.h"
#include "../xrForms/xrThread.h"

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
 				if (IndexTask % 8 == 0)
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

 
void	CBuild::LMaps					()
{
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
 		Status("Lighting Precalculate for GPU...");
		
		CTimer start_time; start_time.Start();
 		auto RunCollect = [&](xr_vector<CDeflector*>& deflectors, bool isFirst)
		{
			GPUTaskinSystem.RestartALL();

			GPUTaskinSystem.ColorsMapType = eDeflectors;
			GPUTaskinSystem.current_flags = (gCompilerMode.LC_NoSun ? LP_dont_sun : 0) | LP_UseFaceDisable;

			xr_atomic_u32 IndexTaskID = 0;
  			xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TID)
			{
				while (true)
				{
					u32 Index = IndexTaskID.fetch_add(1);
					if (Index >= deflectors.size()) break;

					CDeflector* D = deflectors[Index];
					if (D == nullptr) continue;

					isFirst ? D->LightGPU() : D->LowerResolutionGPU();
					
					AditionalData("*** [LMAPS] Lmap [%u/%u] W: %u | H: %u", Index, deflectors.size(), D->layer.width, D->layer.height);
				}

				// Система тасков щас иная
				GPUTaskinSystem.LightPointPacked_run_tasks();
			});
			 

			int IndexApply = 0;
			for (auto& D : deflectors)
			{
				if (D->ApplyColors()) 
					IndexApply++;

				AditionalData("*** [LMAPS] Apply Lmaps [%u/%u]", IndexApply, deflectors.size());
			}

			Msg("*** [LMAPS] Apply Lmaps [%u/%u]", IndexApply, deflectors.size());
		};

		auto& DEFLS = lc_global_data()->g_deflectors();
 		RunCollect(DEFLS, true);  // Обычный расщет
		RunCollect(DEFLS, false); // Проверка на размер (Если нужно перерасчитываем)

		Msg("*** [LMAPS] Apply Borders Started [%u]", DEFLS.size());

		xr_atomic_u32 IndexTaskID = 0;
		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TID)
		{
			while (true)
			{
				u32 Index = IndexTaskID.fetch_add(1);
				if (Index >= DEFLS.size()) { break; }
				CDeflector* D = lc_global_data()->g_deflectors()[Index];
				if (D != nullptr) D->ApplyExpandBordersGPU();
			}
		});
		 
		Msg("%f seconds", start_time.GetElapsed_sec());
  	}
	else
#endif
	{
		// Main process (4 threads)
		Status("Lighting...");

		CTimer start_time; start_time.Start();
		ProcessLMAPS_CPU();
		clMsg("%f seconds", start_time.GetElapsed_sec());
	}
}
 
void CBuild::Light()
{
 	//****************************************** Resolve materials
 	Phase("Resolving materials...");
 	xrPhase_ResolveMaterials();
	IsolateVertices(TRUE);

	//****************************************** UV mapping
 	Phase("Build UV mapping...");
 	xrPhase_UVmap();
	IsolateVertices(TRUE);
	 
	//****************************************** Subdivide geometry
	Phase("Subdividing geometry...");
	xrPhase_Subdivide();
	lc_global_data()->vertices_isolate_and_pool_reload();
	IsolateVertices(TRUE);

	//****************************************** Starting MU
	run_mu_light();
 
	//****************************************** Implicit
  	ImplicitLighting();

	//****************************************** LMAPS
  	LMaps();

	//****************************************** Vertex
 	LightVertex();
 
	//****************************************** Merge LMAPS
	Phase("LIGHT: Merging lightmaps...");
	xrPhase_MergeLM(0, lc_global_data()->g_deflectors().size());

	// Save Lmaps
	Phase("LIGHT: Save lightmaps...");
	xrPhase_SaveLmaps();
 
	//****************************************** Merge geometry
	Phase("Merging geometry...");
 	xrPhase_MergeGeometry();
  
	//****************************************** Destroy RCast-model
 	Phase("Destroying ray-trace model...");
 	lc_global_data()->destroy_rcmodel();
	if (gCompilerMode.Embree || gCompilerMode.CUDA)
		EmbreeMain.IntelEmbereUnloadAll();
}

void CBuild::LightVertex	()
{
	::LightVertex();
}