#include "stdafx.h"
#include "../Mesh/xrFace.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"
#include "../Build/xrLC_GlobalData.h"
#include "mu_model_light.h"
#include "../../UI/CompilersUI.h"
 
// mu-light
#include "../Light/LightingBackend.h"
#include "../Light/light_point.h"
#include "../Light/xrDeflector.h"
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

	GetLightingBackend().LightMURefs();
}
