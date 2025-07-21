#include "StdAfx.h"
#include "Build.h"

#include "../xrLC_Light/xrDeflector.h"
#include "../xrForms/xrThread.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrLightVertex.h"

#include "../../xrCore/xrSyncronize.h"

#include "../xrLC_Light/mu_model_light.h"
xrCriticalSection	task_CS
#ifdef PROFILE_CRITICAL_SECTIONS
	(MUTEX_PROFILE_ID(task_C_S))
#endif // PROFILE_CRITICAL_SECTIONS
;

#include <random>

static thread_local std::mt19937 rng = std::mt19937(std::random_device()());
xr_vector<int>		task_pool;

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
			thProgress			= 1.f - float(task_pool.size()) / float(lc_global_data()->g_deflectors().size());
			if (task_pool.empty())	
			{
				task_CS.Leave		();
				return;
			}
			int ID = task_pool.back();

			StatusNoMsg("Deflectors %u|%u", ID, lc_global_data()->g_deflectors().size());

			D					= lc_global_data()->g_deflectors()[ID];
			task_pool.pop_back	();
			task_CS.Leave		();

			// Perform operation
			try 
			{
				D->Light	(&DB,&LightsSelected,H);
			} 
			catch (...)
			{
				clMsg("* ERROR: CLMThread::Execute - light");
			}
		}
	}
};

void	CBuild::LMapsLocal				()
{
		FPU::m64r		();
		
		mem_Compact		();

		// Randomize deflectors
#ifndef NET_CMP
		std::shuffle(lc_global_data()->g_deflectors().begin(), lc_global_data()->g_deflectors().end(), rng);
#endif

#ifndef NET_CMP	
for(u32 dit = 0; dit<lc_global_data()->g_deflectors().size(); dit++)	
		task_pool.push_back(dit);
#else
		task_pool.push_back(14);
		task_pool.push_back(16);
#endif
		

		// Main process (4 threads)
		Status			("Lighting...");
		CThreadManager	threads;
		const	u32	thNUM	= CPU::ID.n_threads;

		CTimer	start_time;	start_time.Start();				
		for				(int L=0; L<thNUM; L++)	
			threads.start(new CLMThread (L));
		threads.wait	(500);
		clMsg			("%f seconds",start_time.GetElapsed_sec());
}

void	CBuild::LMaps					()
{
		//****************************************** Lmaps
	Phase			("LIGHT: LMaps...");
	LMapsLocal();
}
 
void CBuild::Light()
{
//	g_params().m_lm_jitter_samples = 1;
 
//	g_params().m_lm_pixels_per_meter = 10;
//	g_params().m_lm_rms = 4;
//	g_params().m_lm_rms_zero = 4; 

	//****************************************** Wait for MU
	FPU::m64r();
	/// Phase("LIGHT: Waiting for MU-thread...");
	//mem_Compact();
	//wait_mu_base();


	//****************************************** Implicit
	{
		FPU::m64r		();
		Phase			("LIGHT: Implicit...");
		mem_Compact		();
		ImplicitLighting();
	}
	
	LMaps		();


	//****************************************** Vertex
	FPU::m64r		();
	Phase			("LIGHT: Vertex...");
	mem_Compact		();

	LightVertex		();


	//****************************************** Merge LMAPS
	{
		FPU::m64r		();
		Phase			("LIGHT: Merging lightmaps...");
		mem_Compact		();

		xrPhase_MergeLM	();
	}
}

void CBuild::LightVertex	()
{
	::LightVertex();
}