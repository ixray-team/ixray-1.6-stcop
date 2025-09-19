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
	thread_local HASH			H;
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
					D->Light(&DB, &LightsSelected, H);
				}
				catch (...)
				{
					clMsg("* ERROR: CLMThread::Execute - light");
				}
			}			
		}
	);

	xrPhase_MergeLM(0, lc_global_data()->g_deflectors().size());
};


#ifdef LCCUDA_BUILD
#include "../xrLC_Light/xrDeflectorLight_Packed.h"

u32 TotalGPU, TotalCPU, TotalCPUApply;
u32 TotalGarbage, TotalApply, TotalStage2;

void CBuild::LmapsStageGPU(int Stage, bool isFirst, size_t Begin, size_t End)
{
	thread_local HASH			H;
	GPUTaskinSystem.RestartALL();

	CTimer tStats; tStats.Start();
	static xrCriticalSection LockGuard;

 	{
		u32 IndexTaskID = Begin;
		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TID)
		{
			while (true)
			{
				LockGuard.Enter();
				if (IndexTaskID >= End) { LockGuard.Leave(); break; }
 				CDeflector* D = lc_global_data()->g_deflectors()[IndexTaskID];
				IndexTaskID += 1;
				LockGuard.Leave();

				if (Stage == 1)
					isFirst ? D->LightGPU(H) : D->LowerResolutionGPU(H);
				if (Stage == 2)
					D->EdgesLighting(H);

				if (Stage == 1)
					AditionalData("*** [LMAPS] Processing Lmaps [%u/%u]", IndexTaskID, lc_global_data()->g_deflectors().size());
				else if(Stage == 2)
					AditionalData("*** [LMAPS] Processing Edges [%u/%u]", IndexTaskID, lc_global_data()->g_deflectors().size());
			}				
		});
		GPUTaskinSystem.LightPointPackedDeflectorsRun();
	}

	u32 PGarbage = tStats.GetElapsed_ms(); tStats.Start();

	{
		std::atomic<u32> index_task;
 		auto task = [&](size_t ITask)
		{
			u32 Index = index_task.fetch_add(1, std::memory_order_relaxed);
			// Берём пачку задач
 			AditionalData("*** [LMAPS] Apply Colors [%u] total [%u]", Index, lc_global_data()->g_deflectors().size());
			CDeflector* D = lc_global_data()->g_deflectors()[ITask];
			D->ApplyColors();
		};
 		// xr_parallel_for(size_t(0), size_t(lc_global_data()->g_deflectors().size()), task);
		for (auto dID = 0; dID< lc_global_data()->g_deflectors().size(); dID++)
			task(dID);

		CTimer t; t.Start();
		index_task.store(0);
 		auto task_clear = [&](size_t IndexTask)
		{			
			u32 Index = index_task.fetch_add(1, std::memory_order_relaxed);
			AditionalData("*** [LMAPS] Clear Colors: %u/%u", Index, lc_global_data()->g_deflectors().size());

			auto D = lc_global_data()->g_deflectors()[IndexTask];
			D->ClearResults();
 		};
		xr_parallel_for(size_t(0), size_t(lc_global_data()->g_deflectors().size()), task_clear);

		clMsg("$ Elapsed Clearing: %u ms", t.GetElapsed_ms());
	}
	
	u32 PApply = tStats.GetElapsed_ms(); tStats.Start();
	
	if (Stage == 2 && !isFirst)
	{
		u32 IndexTaskID = Begin;
		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TID)
		{
			while (true)
			{
 				LockGuard.Enter();
				if (IndexTaskID >= End) { LockGuard.Leave(); break; }
				CDeflector* D = lc_global_data()->g_deflectors()[IndexTaskID];
				IndexTaskID += 1;
				LockGuard.Leave();

				if (D != nullptr)
					D->ApplyExpadBordersGPU();
				AditionalData("*** [LMAPS] Apply Borders [%u] total [%u]", IndexTaskID, lc_global_data()->g_deflectors().size());
			}
		});
	}

	u32 PExpand = tStats.GetElapsed_ms();  

	// clMsg("$ [LMAPS] Ready [%u/%u] total [%u] | Garbage: %u ms, Apply: %u ms, PExpand: %u ms", 
	// Begin, End, lc_global_data()->g_deflectors().size(),
	// PGarbage, PApply, PExpand);

	//clMsg("$ [LMAPS] GPU: %u ms | CPU Copy: %u ms | CPU[MT] Apply: %u ms",
	//	GPUTaskinSystem.ProcessingGPU, GPUTaskinSystem.ProcessingCPU_copy, GPUTaskinSystem.ProcessingCPU_result);

	TotalGPU += GPUTaskinSystem.ProcessingGPU;
	TotalCPU += GPUTaskinSystem.ProcessingCPU_copy;
	TotalCPUApply += GPUTaskinSystem.ProcessingCPU_result;

	TotalGarbage += PGarbage;
	TotalApply += PApply;
	TotalStage2 += PExpand;

	if (Stage == 2 && !isFirst)
	{
		clMsg("Deflectors: Merging lightmaps...");
		xrPhase_MergeLM(Begin, End);
	}
}
#endif



void	CBuild::LMaps					()
{
	mem_Compact();

#ifdef LCCUDA_BUILD
	if (gCompilerMode.CUDA)
	{
		Status("Lighting Precalculate for GPU...");

		CTimer start_time; start_time.Start();

		size_t SPLIT = 1024 * 256;
		for (size_t INDEX = 0; INDEX < lc_global_data()->g_deflectors().size();)
		{
			size_t end = std::min(INDEX + SPLIT, lc_global_data()->g_deflectors().size());
			clMsg("Start Working: %u to %u", INDEX, end);

 			LmapsStageGPU(1, true, INDEX, end);
			LmapsStageGPU(2, true, INDEX, end);

			LmapsStageGPU(1, false, INDEX, end);
			LmapsStageGPU(2, false, INDEX, end);
			INDEX += SPLIT;
		}
		clMsg("%f seconds", start_time.GetElapsed_sec());

 		clMsg("$ Garbage: %u ms, Apply: %u ms, PExpand: %u ms", TotalGarbage, TotalApply, TotalStage2);
 		clMsg("$ [LMAPS] GPU: %u ms | CPU Copy: %u ms | CPU[MT] Apply: %u ms",	TotalGPU, TotalCPU, TotalCPUApply);
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
 
	//****************************************** Implicit
	Phase("LIGHT: Implicit...");
 	ImplicitLighting();

	//****************************************** LMAPS
	Phase("LIGHT: LMaps...");
 	LMaps();

	//****************************************** Vertex
	Phase("LIGHT: Vertex...");
	LightVertex();
 

	//****************************************** Merge LMAPS
	// Phase("LIGHT: Merging lightmaps...");
	// xrPhase_MergeLM(0, lc_global_data()->g_deflectors().size());

	// Save Lmaps
	Phase("LIGHT: Save lightmaps...");
	xrPhase_SaveLmaps();
 
	//****************************************** Merge geometry
	Phase("Merging geometry...");
 	xrPhase_MergeGeometry();


	//****************************************** Starting MU
	Phase("LIGHT: Starting MU...");
	Light_prepare();
 	StartMu();
	
	//****************************************** Destroy RCast-model
 	Phase("Destroying ray-trace model...");
 	lc_global_data()->destroy_rcmodel();
	if (gCompilerMode.Embree)
		EmbreeMain.IntelEmbereUNLOAD();
}

void CBuild::LightVertex	()
{
	::LightVertex();
}