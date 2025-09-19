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

void CBuild::LmapsStageGPU(int Stage, bool isFirst, size_t Begin, size_t End)
{
	thread_local HASH			H;
	GPUTaskinSystem.RestartALL();
 
	{
		xr_atomic_u32 IndexTaskID = 0;
		xr_parallel_for(size_t(Begin), size_t(End), [&](size_t taskID)
			{
				CDeflector* D = lc_global_data()->g_deflectors()[taskID];
				if (Stage == 1)
					isFirst ? D->LightGPU(H) : D->LowerResolutionGPU(H);
				if (Stage == 2)
					D->EdgesLighting(H);

				AditionalData("*** [LMAPS] Ready [%u] total [%u]", IndexTaskID.load(), lc_global_data()->g_deflectors().size());
				IndexTaskID.fetch_add(1);
			});
		GPUTaskinSystem.LightPointPackedDeflectorsRun();
	}

	{
		xr_atomic_u32 IndexTaskID = 0;
		xr_parallel_for(size_t(Begin), size_t(End), [&](size_t taskID)
			{
				auto& D = lc_global_data()->g_deflectors()[taskID];
				if (D != nullptr)
					D->ApplyColors();

				AditionalData("*** [LMAPS] Apply Colors [%u] total [%u]", IndexTaskID.load(), lc_global_data()->g_deflectors().size());
				IndexTaskID.fetch_add(1);

			});
	}
	 
	if (Stage == 2 && !isFirst)
	{
		xr_atomic_u32 IndexTaskID = 0;
		xr_parallel_for(size_t(Begin), size_t(End), [&](size_t taskID)
		{
			auto D = lc_global_data()->g_deflectors()[taskID];
			if (D != nullptr)
				D->ApplyExpadBordersGPU();

			AditionalData("*** [LMAPS] Apply Borders [%u] total [%u]", IndexTaskID.load(), lc_global_data()->g_deflectors().size());
			IndexTaskID.fetch_add(1);
		});
  		clMsg("Deflectors: Merging lightmaps...");
		xrPhase_MergeLM(Begin, End);
	}

	clMsg("$ [LMAPS] Ready [%u/%u] total [%u]", Begin, End, lc_global_data()->g_deflectors().size());
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

		size_t SPLIT = 1024 * 1024;
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
  
void CBuild::BuildAdaptiveHT()
{
	if (!gCompilerMode.LC_BackingDisabled)
	{
		//****************************************** HEMI-Tesselate
 		Phase("Adaptive HT...");
		xrPhase_AdaptiveHT();
	}

	// Building normals
	Phase("Building normals...");
	mem_Compact();
	CalcNormals();

 	// Phase("Building collision database (CFORM)...");
	mem_Compact();
	BuildCForm();
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

 
	// se7kills fixed All stage then Disable
	if (!gCompilerMode.CUDA)
	{
		//****************************************** GLOBAL-RayCast model
		Phase("Building rcast-CFORM model...");
		Light_prepare();
		BuildRapid(TRUE);
	}
 
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
	if (lc_global_data()->GetIsIntelUse())
		EmbreeMain.IntelEmbereUNLOAD();
}

void CBuild::LightVertex	()
{
	::LightVertex();
}