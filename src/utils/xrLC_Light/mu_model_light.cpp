#include "stdafx.h"
#include "xrFace.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"
#include "xrLC_GlobalData.h"
#include "mu_model_light.h"

#include "../xrForms/xrThread.h"
#include "../xrForms/CompilersUI.h"

#include "../../xrCore/xrSyncronize.h"

CThreadManager mu_materials;
CThreadManager mu_secondary;
 
xrCriticalSection csMUMAPS_LOCKS;

static int ThreadTaskID = 0;

// mu-light
 
class CMULight : public CThread
{
public:
	CMULight(u32 ID) : CThread(ID)
	{
		thMessages = FALSE;
	}

	virtual void Execute()
	{
		// Priority
		SetThreadPriority(Platform::GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
		Sleep(0);
		while (true)
		{
  			csMUMAPS_LOCKS.Enter();
			int ID = ThreadTaskID;

			if (ThreadTaskID >= inlc_global_data()->mu_refs().size())
			{
				csMUMAPS_LOCKS.Leave();
				break;
			}

			ThreadTaskID++;

			if (ID % 512 == 0)
				Status("Models %d/%d", ID, inlc_global_data()->mu_refs().size());
			thProgress = (float(ID) / float(inlc_global_data()->mu_refs().size()));

			csMUMAPS_LOCKS.Leave();

			// Light references
			inlc_global_data()->mu_refs()[ID]->calc_lighting	();
		}
	}
};

class CMULightCalculation : public CThread
{
public:
	CMULightCalculation(u32 ID) : CThread(ID)
	{
		thMessages = FALSE;
	}

	virtual void	Execute()
	{
 		// Priority
		SetThreadPriority(Platform::GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
		Sleep(0);

		while (true)
		{
			csMUMAPS_LOCKS.Enter();

			int ID = ThreadTaskID;
 			if (ThreadTaskID >= inlc_global_data()->mu_models().size())
			{
				csMUMAPS_LOCKS.Leave();
				break;
			}
 			ThreadTaskID++;
			// Light references
			inlc_global_data()->mu_models()[ID]->calc_materials();
			thProgress = (float(ID) / float(inlc_global_data()->mu_models().size()));
			if (ID%512 == 0)
				Status("Models %d/%d", ID, inlc_global_data()->mu_models().size());
			csMUMAPS_LOCKS.Leave();
 
			
			inlc_global_data()->mu_models()[ID]->calc_lighting();
		}
	}
};


#include "xrDeflectorLight_Packed.h"
#include <light_point.h>

void run_mu_light()
{
	// Priority
	SetThreadPriority(Platform::GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
	Sleep(0);

 	const bool Cuda = gCompilerMode.CUDA;
	const bool Embree = gCompilerMode.Embree;

	string128 tmp_phase;
	sprintf(tmp_phase, "LIGHT: Mu-Base (*%s*)", Embree || Cuda ? "Embree" : "Opcode");
	Phase(tmp_phase);

 	ThreadTaskID = 0;
	for (u32 thID = 0; thID < gCompilerMode.ThreadsPerWork; thID++)
		mu_materials.start(new CMULightCalculation(thID));
 	mu_materials.wait(100); 

 	sprintf(tmp_phase, "LIGHT: Mu-Refs (*%s*)", Cuda ? "CUDA" : Embree ? "Embree" : "Opcode");
	Phase(tmp_phase);
 
	// Light references
#ifdef LCCUDA_BUILD
	if (gCompilerMode.CUDA)
	{
 		GPUTaskinSystem.RestartALL();
 		GPUTaskinSystem.ColorsMapType = eMumodel;
		GPUTaskinSystem.current_flags = (gCompilerMode.LC_NoSun ? LP_dont_sun : 0) | LP_DEFAULT;
	
		// Gathering
		CTimer tStats; tStats.Start();

		xr_atomic_u32 REF_INDEX = 0;
 		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t ThreadID)
		{
			while (true)
			{
				u32 IndexTask = REF_INDEX.fetch_add(1);
				if (IndexTask >= inlc_global_data()->mu_refs().size()) break;

 				AditionalData("REF LIGHT: %u/%u", IndexTask, inlc_global_data()->mu_refs().size());
				auto MRef = inlc_global_data()->mu_refs()[IndexTask];
				MRef->calc_lighting_cuda_1();
			};

			// Завершаем накопленые данные
 			GPUTaskinSystem.LightPointPacked_run_tasks();
 		});
 		Msg("[MURefs] Elapsed For Compute: %u ms", tStats.GetElapsed_ms());
		
		// APPLY

		tStats.Start();
		u32 _REF_INDEX = 0;
		for (auto& REF : inlc_global_data()->mu_refs())
		{
			AditionalData("REF LIGHT APPLY: %u/%u", _REF_INDEX, inlc_global_data()->mu_refs().size());

			REF->calc_lighting_cuda_2();
			REF->calc_lighting_cuda_3();
			_REF_INDEX++;
		}
		Msg("[MURefs] Elapsed For Apply Colors: %u ms", tStats.GetElapsed_ms());

		GPUTaskinSystem.RestartALL(); // Выгружаем все Это последнее освещение 
	}
	else
#endif
	{
		ThreadTaskID = 0;
		for (u32 thID = 0; thID < gCompilerMode.ThreadsPerWork; thID++)
			mu_secondary.start(new CMULight(thID));
		mu_secondary.wait(100);
	}

}