#include "stdafx.h"
#include "xrFace.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"
#include "xrLC_GlobalData.h"
#include "mu_model_light.h"
#include "../xrForms/CompilersUI.h"
 
// mu-light
#include "cuda/xrCuda_PackedLights.h"
#include <light_point.h>
#include "xrDeflector.h"
void run_mu_light()
{
	UpdateCurrentPhase("MU-Models");

	xr_atomic_u32 ThreadTaskID = 0;
	xr_std_parallel_for([&ThreadTaskID]()
	{
		while (true)
		{
			u32 tID = ThreadTaskID.fetch_add(1);
			if (tID >= inlc_global_data()->mu_models().size())
			{
				break;
			}

			// Light references
			inlc_global_data()->mu_models()[tID]->calc_materials();
			inlc_global_data()->mu_models()[tID]->calc_lighting();
			AditionalData("MuModels %d/%d", tID, inlc_global_data()->mu_models().size());
		} 
	}, gCompilerMode.ThreadsPerWork);

 	if (!gCompilerMode.CUDA)
	{
 		ThreadTaskID = 0;
		xr_std_parallel_for([&ThreadTaskID]()
		{
			while (true)
			{
				int tID = ThreadTaskID.fetch_add(1);
				if (tID >= inlc_global_data()->mu_refs().size()) break;
				
				// Light references
				inlc_global_data()->mu_refs()[tID]->calc_lighting();
				AditionalData("MuRefs %d/%d", tID, inlc_global_data()->mu_refs().size());
			}
		}, gCompilerMode.ThreadsPerWork);
 	}
	else 
 	{
#ifdef LCCUDA_BUILD
		extern void RunMURefsGPU();
		RunMURefsGPU();
#endif
	}
}