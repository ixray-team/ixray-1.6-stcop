#include "StdAfx.h"
#include "Build.h"

#include "../xrForms/CompilersUI.h"
#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrLightVertex.h"
#include "../xrLC_Light/xrFace.h"

#include "../../xrCore/xrSyncronize.h"
#include "../xrLC_Light/mu_model_light.h"

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
		extern void RunCompileDeflectorsGPU (CBuild * build);
		RunCompileDeflectorsGPU(this);
		clMsg("%d lightmaps builded", lc_global_data()->lightmaps().size());
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
		Phase("Building UV...");
		//****************************************** Building normals
		CalcNormals();

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
	InitModel();

	// ***************************************** Computing UV
	BuildingUV();
 
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
 
	//****************************************** Merge geometry
	Phase("Merging geometry...");
 	xrPhase_MergeGeometry();
	UnloadModel();
}

void CBuild::LightVertex	()
{
	::LightVertex();
}