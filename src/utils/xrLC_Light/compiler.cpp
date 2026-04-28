#include "stdafx.h"
#include "../../xrEngine/xrLevel.h"
#include "detail_slot_calculate.h"
#include "global_calculation_data.h"
#include "embree_raytracing/EmbreeRayTrace.h"

void xrLight_Details()
{
	CTimer start_time;
   
  	thread_local CDB::COLLIDER		DB;
  
	xr_atomic_u32 IndexTask = 0;
	xr_parallel_for ( size_t(0), size_t( gl_data.slots_data.size_z() ),
		[&](size_t Z)
		{
			DB.ray_options(CDB::OPT_CULL);
			DB.box_options(CDB::OPT_FULL_TEST);

			base_lighting		Selected;
			xr_vector<u32>		box_result;
 			for (u32 X = 0; X < gl_data.slots_data.size_x(); X++)
			{
 				DetailSlot& DS = gl_data.slots_data.get_slot(X, Z);
				if (!detail_slot_process(X, Z, DS))												continue;
				if (!detail_slot_calculate(X, Z, DS, box_result, DB, Selected))					continue;
				gl_data.slots_data.set_slot_calculated(X, Z);

			}
 
			AditionalData("Processing TaskID[%u/%u]", IndexTask.fetch_add(1), gl_data.slots_data.size_z() );
		}
	);
 

	Msg("%d seconds elapsed.", (start_time.GetElapsed_ms()) / 1000);
}
