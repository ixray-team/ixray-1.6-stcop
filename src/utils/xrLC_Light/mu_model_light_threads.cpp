#include "stdafx.h"
#include "mu_model_light_threads.h"
#include "xrface.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"
#include "xrlc_globaldata.h"
#include "mu_model_light.h"

#include "../xrForms/xrThread.h"
#include "../../xrcore/xrSyncronize.h"



CThreadManager			mu_base;

CThreadManager			mu_materials;
CThreadManager			mu_secondary;
#define		MU_THREADS	16

xrCriticalSection csMUMAPS_LOCKS;

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
			csMUMAPS_LOCKS.Leave();


			// Light references
			inlc_global_data()->mu_refs()[ID]->calc_lighting	();
			thProgress							= (float(ID)/float(inlc_global_data()->mu_refs().size()));
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

			csMUMAPS_LOCKS.Leave();
 
			
			inlc_global_data()->mu_models()[ID]->calc_lighting();
			thProgress = (float(ID) / float(inlc_global_data()->mu_models().size()));
		}
	}
};


	//void LC_WaitRefModelsNet();
class CMUThread : public CThread
{
public:
	CMUThread	(u32 ID) : CThread(ID)
	{
		thMessages	= FALSE;
	}
	virtual void	Execute()
	{
		// Priority
		SetThreadPriority	(Platform::GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
		Sleep				(0);
 
		/*
		for (u32 m=0; m<inlc_global_data()->mu_models().size(); m++)
		{
			inlc_global_data()->mu_models()[m]->calc_materials();
			inlc_global_data()->mu_models()[m]->calc_lighting	();
		}
		*/

		ThreadTaskID = 0;
		for (u32 thID = 0; thID < MU_THREADS; thID++)
			mu_materials.start(xr_new<CMULightCalculation>(thID));

		mu_materials.wait(100);
 
		// Light references
		ThreadTaskID = 0;
		for (u32 thID=0; thID < MU_THREADS; thID++)
			mu_secondary.start	( xr_new<CMULight> (thID) );
	
		mu_secondary.wait(100);
	}
};


void	run_mu_base( )
{
 	mu_base.start				(xr_new<CMUThread> (0));
}

void	wait_mu_base_thread		()
{
	mu_base.wait				(500);
} 