#include "stdafx.h"
#include "../../xrEngine/xrLevel.h"

#include "../xrForms/xrThread.h"

#include "global_calculation_data.h"
#include "LightThread.h"

#include "ppl.h"

#define NUM_THREADS		CPU::ID.n_threads

std::atomic<int> IDX;

 
void xrLight_Details()
{
	CTimer start_time;

 	CThreadManager Threads;
	 
//  3:31 min 3:29
/*	for (u32 thID = 0; thID < NUM_THREADS; thID++)
	{
		CThread* T = new LightThread(thID);
		T->thMessages = FALSE;
		T->thMonitor = FALSE;
		Threads.start(T);
	}
	
	Threads.wait();
*/

 // 3:15min, 4:03 ppl
	u32 MAX_Z = gl_data.slots_data.size_z();
	u32 MAX_X = gl_data.slots_data.size_x();
	thread_local CDB::COLLIDER		DB;
	DB.ray_options(CDB::OPT_CULL);
	DB.box_options(CDB::OPT_FULL_TEST);
	thread_local base_lighting		Selected;

	thread_local DWORDVec box_result;
 
	xr_parallel_for
	(
		size_t(0), size_t(MAX_Z),
		[&](size_t Z)
		{
			Status("Z: %u/%u, processed: %u", Z, gl_data.slots_data.size_z(), IDX.load());

			for (u32 X = 0; X < gl_data.slots_data.size_x(); X++)
			{

				DetailSlot& DS = gl_data.slots_data.get_slot(X, Z);
				if (!detail_slot_process(X, Z, DS))
					continue;
				if (!detail_slot_calculate(X, Z, DS, box_result, DB, Selected))
					continue;
				gl_data.slots_data.set_slot_calculated(X, Z);

			}

			IDX.fetch_add(1);
		}
	);
 

	Msg("%d seconds elapsed.", (start_time.GetElapsed_ms()) / 1000);
}
