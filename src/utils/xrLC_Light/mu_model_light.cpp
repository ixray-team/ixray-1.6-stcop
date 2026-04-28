#include "stdafx.h"
#include "xrFace.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"
#include "xrLC_GlobalData.h"
#include "mu_model_light.h"
#include "../xrForms/CompilersUI.h"
 
// mu-light
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

	static xr_atomic_u32 ThreadTaskID = 0;
 	xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [](size_t threadID) {
		while (true)
		{
			u32 tID = ThreadTaskID.fetch_add(1);
			if (tID >= inlc_global_data()->mu_models().size())  break;

			// Light references
			inlc_global_data()->mu_models()[tID]->calc_materials();
			inlc_global_data()->mu_models()[tID]->calc_lighting();
			AditionalData("MuModels %d/%d", tID, inlc_global_data()->mu_models().size());
		}
	});


 	sprintf(tmp_phase, "LIGHT: Mu-Refs (*%s*)", Cuda ? "CUDA" : Embree ? "Embree" : "Opcode");
	Phase(tmp_phase);
 	if (!gCompilerMode.CUDA)
	{
 		ThreadTaskID = 0;
		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [](size_t threadID) {

			while (true)
			{
				int tID = ThreadTaskID.fetch_add(1);
				if (ThreadTaskID >= inlc_global_data()->mu_refs().size()) break;
				// Light references
				inlc_global_data()->mu_refs()[tID]->calc_lighting();
				AditionalData("MuRefs %d/%d", tID, inlc_global_data()->mu_refs().size());
			}
		});
 	}
	else 
 	{
#ifdef LCCUDA_BUILD
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
		REF_INDEX = 0;
		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t ThreadID)
		{
			while (true)
			{
				u32 Index = REF_INDEX.fetch_add(1);
				if (Index >= inlc_global_data()->mu_refs().size()) break;
					
				auto REF = inlc_global_data()->mu_refs()[Index];
					
				REF->calc_lighting_cuda_2();
				REF->calc_lighting_cuda_3();

				AditionalData("REF LIGHT APPLY: %u/%u", Index, inlc_global_data()->mu_refs().size());
			}
 		} );

		Msg("[MURefs] Elapsed For Apply Colors: %u ms", tStats.GetElapsed_ms());

		GPUTaskinSystem.RestartALL(); // Выгружаем все Это последнее освещение 
#endif
	}
}