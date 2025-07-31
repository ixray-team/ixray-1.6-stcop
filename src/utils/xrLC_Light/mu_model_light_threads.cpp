#include "stdafx.h"
#include "mu_model_light_threads.h"
#include "xrFace.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"
#include "xrLC_GlobalData.h"
#include "mu_model_light.h"

#include "../xrForms/xrThread.h"
#include "../../xrCore/xrSyncronize.h"

CThreadManager			mu_materials;
CThreadManager			mu_secondary;
 
xrCriticalSection		csMUMAPS_LOCKS;

int ThreadTaskID = 0;

// mu-light
 
class CMULight	: public CThread
{
public:
	CMULight	( u32 ID ) : CThread(ID)	{	thMessages	= FALSE; }

	virtual void	Execute	()
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
	CMULightCalculation(u32 ID) : CThread(ID) { thMessages = FALSE; }

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
 
#include "../xrForms/CompilersUI.h"
extern CompilersMode gCompilerMode;
 
void	run_mu_base( )
{
	// Priority
	SetThreadPriority(Platform::GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
	Sleep(0);

 
	ThreadTaskID = 0;
	for (u32 thID = 0; thID < gCompilerMode.ThreadsPerWork; thID++)
		mu_materials.start(new CMULightCalculation(thID));
 	mu_materials.wait(100); 

	// Light references
	ThreadTaskID = 0;
	for (u32 thID = 0; thID < gCompilerMode.ThreadsPerWork; thID++)
		mu_secondary.start(new CMULight(thID));
 	mu_secondary.wait(100);
}
 