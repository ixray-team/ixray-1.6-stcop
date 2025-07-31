#include "StdAfx.h"
#include "Build.h"

#include "../xrLC_Light/xrDeflector.h"
#include "../xrForms/xrThread.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrLightVertex.h"

#include "../../xrCore/xrSyncronize.h"

#include "../xrLC_Light/mu_model_light.h"
xrCriticalSection task_CS;

#include <random>

static thread_local std::mt19937 rng = std::mt19937(std::random_device()());
xr_vector<int>		task_pool;
xr_atomic_u32		ProgressData;
class CLMThread		: public CThread
{
private:
	HASH			H;
	CDB::COLLIDER	DB;
	base_lighting	LightsSelected;
public:
	CLMThread	(u32 ID) : CThread(ID)
	{
		// thMonitor= TRUE;
		thMessages	= FALSE;
	}

	virtual void	Execute()
	{
		CDeflector* D	= 0;

		for (;;) 
		{
			// Get task
			task_CS.Enter		();
			Progress(float(ProgressData.load()) / float (lc_global_data()->g_deflectors().size()) );

			if (ProgressData.load() % 8 == 0)
				AditionalData("Deflectors: %u / %u", ProgressData.load(), lc_global_data()->g_deflectors().size() );

 			if (task_pool.empty())	
			{
				task_CS.Leave		();
				return;
			}

			u32 ID = task_pool.back();
			D					= lc_global_data()->g_deflectors()[ID];
			task_pool.pop_back	();
			task_CS.Leave		();

			ProgressData.fetch_add(1);

			

			// Perform operation
			try {
				D->Light	(&DB,&LightsSelected,H);
			} catch (...)
			{
				clMsg("* ERROR: CLMThread::Execute - light");
			}
		}
	}
};


#include "../xrForms/CompilersUI.h"
extern CompilersMode gCompilerMode;

void	CBuild::LMapsLocal				()
{
		FPU::m64r		();
		
		mem_Compact		();

		// Randomize deflectors
		std::shuffle(lc_global_data()->g_deflectors().begin(), lc_global_data()->g_deflectors().end(), rng);

		for(u32 dit = 0; dit<lc_global_data()->g_deflectors().size(); dit++)	
			task_pool.push_back(dit);
	

		// Main process (4 threads)
		Status			("Lighting...");
		CThreadManager	threads;
 		
		CTimer	start_time;	
		start_time.Start();				
		for				(int L=0; L< gCompilerMode.ThreadsPerWork; L++)	threads.start(new CLMThread (L));
		threads.wait	(500);
		clMsg			("%f seconds",start_time.GetElapsed_sec());
}

void	CBuild::LMaps					()
{
	LMapsLocal();
}
  
void CBuild::BuildAdaptiveHT()
{
	if (!gCompilerMode.LC_BackingDisabled)
	{
		//****************************************** HEMI-Tesselate
		FPU::m64r();
		Phase("Adaptive HT...");
		xrPhase_AdaptiveHT();
	}
}

#include "../xrLC_Light/xrFaceDefs.h"
#include "../xrLC_Light/xrFace.h"

#include "../xrForms/CompilersUI.h"
extern CompilersMode gCompilerMode;

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

	if (!gCompilerMode.LC_BackingDisabled)
	{
		//****************************************** GLOBAL-RayCast model
		Phase("Building rcast-CFORM model...");
		Light_prepare();
		BuildRapid(TRUE);

		//****************************************** Implicit
		Phase("LIGHT: Implicit...");
		if (gCompilerMode.Embree_SplitBVH)
			EmbreeMain.AttachGeometrys(true);
		ImplicitLighting();

		//****************************************** LMAPS
		Phase("LIGHT: LMaps...");
		if (gCompilerMode.Embree_SplitBVH)
			EmbreeMain.AttachGeometrys(false);
		LMaps();

		//****************************************** Vertex
		Phase("LIGHT: Vertex...");
		LightVertex();
 
		//****************************************** Merge LMAPS
		Phase("LIGHT: Merging lightmaps...");
		xrPhase_MergeLM();

		// Save Lmaps
		Phase("LIGHT: Save lightmaps...");
		xrPhase_SaveLmaps();
	}

	//****************************************** Merge geometry
	Phase("Merging geometry...");
 	xrPhase_MergeGeometry();

	if (!gCompilerMode.LC_BackingDisabled)
	{
		//****************************************** Starting MU
		Phase("LIGHT: Starting MU...");
		Light_prepare();
		if (gCompilerMode.Embree_SplitBVH)
			EmbreeMain.AttachGeometrys(true);
		StartMu();
	}
	
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