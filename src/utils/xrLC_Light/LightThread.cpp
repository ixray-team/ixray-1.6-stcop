#include "stdafx.h"

#include "LightThread.h"

#include "global_calculation_data.h"
#include "mutex"

std::atomic<int> atomic = 0;
u32 MAX_SIZE = 0;
// u32 MIN_SIZE = 0;

#include "ppl.h"
 


void LightThread::Execute()
{
	CDB::COLLIDER		DB;
	DB.ray_options(CDB::OPT_CULL);
	DB.box_options(CDB::OPT_FULL_TEST);
	base_lighting		Selected;

	u32 MAX_Z = gl_data.slots_data.size_z();
	u32 MAX_X = gl_data.slots_data.size_x();

	Msg("Details : MAX_X: %u, MAX_Z: %u", MAX_X, MAX_Z);

 	for (;;)
	{
		u32 Z = atomic.load();
		atomic.fetch_add(1);
 		if (Z < MAX_X)
		{
 			for (u32 X = 0; X < gl_data.slots_data.size_x(); X++)
			{
				DetailSlot& DS = gl_data.slots_data.get_slot(X, Z);
				if (!detail_slot_process(X, Z, DS))
					continue;
				if (!detail_slot_calculate(X, Z, DS, box_result, DB, Selected))
					continue;
				gl_data.slots_data.set_slot_calculated(X, Z);
			
			}
			Status("Z: %u/%u", Z, gl_data.slots_data.size_z());
		}
		else
			break;
 
	}	
}