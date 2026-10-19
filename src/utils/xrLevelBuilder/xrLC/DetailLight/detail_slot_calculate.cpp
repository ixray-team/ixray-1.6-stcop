////////////////////////////////////////////////////////////////////////////
//	Created		: 27.03.2009
//	Author		: Konstantin Slipchenko
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "detail_slot_calculate.h"
#include "../../UI/CompilersUI.h"

#include "../../../../xrCore/Collision/cl_intersect.h"
#include "global_calculation_data.h"
#include "../Light/xrDeflector.h"
#include "../Light/LightingBackend.h"

//-----------------------------------------------------------------------------------------------------------------
const int	LIGHT_Count				=	7;
thread_local xr_vector<DetailsTask> rayTasks;

bool detail_slot_calculate(u32 _x, u32 _z)
{
	// Getter - Detail Slot
	auto& DS = gl_data.slots_data.get_slot(_x, _z);

	// Pre Calculation
	process_pallete(DS);
	if (gl_data.slots_data.skip_slot(_x, _z))		
		return false;

	// Slot Calc
	thread_local xr_vector<u32> box_result;
 	thread_local base_lighting Selected;
	thread_local CDB::COLLIDER DB;
	
	DB.ray_options(CDB::OPT_CULL);
	DB.box_options(CDB::OPT_FULL_TEST);
	
	///////////////////////////////////////////////////////////
	// Build slot BB & sphere
	Fbox	BB;
	gl_data.slots_data.get_slot_box( BB, _x, _z );

	Fsphere		S;
	BB.getsphere( S.P, S.R );


	CDB::TRI*	tris	= gl_data.RCAST_Model->get_tris().data();
	Fvector*	verts	= gl_data.RCAST_Model->get_verts().data();

	// select lights
	Selected.select		( gl_data.g_lights, S.P, S.R );
 
	// lighting itself
	u32				count	= 0;
	float coeff		= DETAIL_SLOT_SIZE_2/float(LIGHT_Count);


	// Select polygons
	Fvector				bbC, bbD;
	BB.get_CD(bbC, bbD);	
	bbD.add(0.01f);

	DB.box_query(gl_data.RCAST_Model, bbC, bbD);

	box_result.clear();
	for (auto& R : DB.r_vec())
		box_result.push_back(R.id);
	
	if (box_result.empty()) return false;

	for (int x=-LIGHT_Count; x<=LIGHT_Count; x++) 
	{
 		Fvector		P;
		P.x			= bbC.x + coeff*float(x);

		for (int z=-LIGHT_Count; z<=LIGHT_Count; z++) 
		{
			// compute position
			Fvector t_n;	t_n.set(0,1,0);
			P.z				= bbC.z + coeff*float(z);
			P.y				= BB.min.y-5;
			Fvector	dir;	dir.set		(0,-1,0);
			Fvector start;	start.set	(P.x, BB.max.y+EPS, P.z);
			
			float		r_u,r_v,r_range;
			for (xr_vector<u32>::iterator tit = box_result.begin(); tit != box_result.end(); tit++)
			{
				CDB::TRI&	T		= tris	[*tit];
				Fvector		V[3]	= { verts[T.verts[0]], verts[T.verts[1]], verts[T.verts[2]] };
				if (CDB::TestRayTri(start,dir,V,r_u,r_v,r_range,true))
				{
					if (r_range>=0.f)	
					{
						float y_test = start.y - r_range;
						if (y_test>P.y)	{
							P.y			= y_test+EPS;
							t_n.mknormal(V[0],V[1],V[2]);
						}
					}
				}
			}
			if (P.y<BB.min.y) continue;
			
			// light point
			DetailsTask data;
			data.SetDataRays(_x, _z, P, t_n, S.R, S.P);
			rayTasks.push_back(data);

			count			+= 1;
		}
	}


	GetLightingBackend().LightDetailSlot(DS, rayTasks, Selected, count);
	rayTasks.clear();

 	
	////////////////////////////////////////////////////////////
	return true;
}

void xrLight_Details()
{
	CTimer start_time;
	start_time.Start();

	xr_atomic_u32 IndexTask = 0;
	IndexTask = 0;
	 
	ILightingBackend& Backend = GetLightingBackend();
	Backend.BeginDetails();

	xr_std_parallel_for([&IndexTask, &Backend]()
		{
			while (true)
			{
				u32 Z = IndexTask.fetch_add(1);
				if (Z >= gl_data.slots_data.size_z()) break;

				for (u32 X = 0; X < gl_data.slots_data.size_x(); X++)
				{
					detail_slot_calculate(X, Z);
				}

				AditionalData("Processing TaskID[%u/%u]", Z, gl_data.slots_data.size_z());
			}

			Backend.FinishDetailsThread();

			rayTasks.clear();
			rayTasks.shrink_to_fit();
		}, gCompilerMode.ThreadsPerWork
	);

	Backend.EndDetails();

	Phase("Unloading data buffers...");
	gl_data.xrUnload();
 
	EmbreeMain.IntelEmbereUnloadAll();

	Msg("Total processing: %u ms.", start_time.GetElapsed_ms());
}
