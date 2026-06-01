////////////////////////////////////////////////////////////////////////////
//	Created		: 27.03.2009
//	Author		: Konstantin Slipchenko
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "detail_slot_calculate.h"
#include "../xrForms/CompilersUI.h"

#include "../../xrCore/Collision/cl_intersect.h"
#include "global_calculation_data.h"
#include "xrDeflector.h"

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
	base_color_c	amount;
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
			
 			if (gCompilerMode.Embree)
			{
				// light point
				DetailsTask data;
				data.SetDataRays(_x, _z, P, t_n, S.R, S.P);
				rayTasks.push_back(data);

			}
#ifdef LCCUDA_BUILD
			else if (gCompilerMode.CUDA)
			{
				size_t idx= GPUTaskinSystem.MakeKey(_x, _z);
 				GPUTaskinSystem.LightPointPacked_add_task(idx, nullptr, P, t_n, nullptr);
			}
#endif

			count			+= 1;
		}
	}


	if (gCompilerMode.Embree)
	{
		LightPoint_Details(rayTasks, Selected, 0);			// Идеально пакетные пашут ибо 225 из одной позиции запросов !
 		for (auto& task : rayTasks)
 			amount.add( task.C );
		rayTasks.clear();

		// calculation of luminocity (225 samples на 1 травинку) может и много :)
		amount.scale(count);
		amount.mul(.5f);

		// Пишется результат в (level.details) !
		DS.c_dir = DS.w_qclr(amount.sun, 15);
		DS.c_hemi = DS.w_qclr(amount.hemi, 15);
		DS.c_r = DS.w_qclr(amount.rgb.x, 15);
		DS.c_g = DS.w_qclr(amount.rgb.y, 15);
		DS.c_b = DS.w_qclr(amount.rgb.z, 15);
	}

 	
	////////////////////////////////////////////////////////////
	return true;
}

#ifdef LCCUDA_BUILD
#include "CUDA/xrCuda_PackedLights.h"
xr_vector<u32>			 samples;
xr_vector<base_color_c>  detail_colors;
u32 size_x;
u32 size_z;

void ApplyColorDetailGPU(size_t IndexTask, base_color_c& C) 
{
	u32 x = GPUTaskinSystem.GetU(IndexTask);
	u32 z = GPUTaskinSystem.GetV(IndexTask);	
	
	u32 idx = z * size_x + x;
	samples[idx]++;
	detail_colors[idx].add(C);
}

void ApplyColorsGPU()
{
	for (auto x = 0; x < gl_data.slots_data.size_x(); x++)
	for (auto z = 0; z < gl_data.slots_data.size_z(); z++)
	{
		// Getter - Detail Slot
		auto& DS = gl_data.slots_data.get_slot(x, z);

		u32 idx			= z * size_x + x;
		auto& count		= samples[idx];
		if (count > 0)
		{
			auto& color = detail_colors[idx];
			color.scale(count);
			color.mul(.5f);

			// Пишется результат в (level.details) !
			DS.c_dir	= DS.w_qclr(color.sun, 15);
			DS.c_hemi	= DS.w_qclr(color.hemi, 15);
			DS.c_r		= DS.w_qclr(color.rgb.x, 15);
			DS.c_g		= DS.w_qclr(color.rgb.y, 15);
			DS.c_b		= DS.w_qclr(color.rgb.z, 15);

			// if (color.hemi > 0.001)
			// 	Msg("Colors x[%u] z[%u] Hemi: %.3f Sampl: %u", x,z, color.hemi, count);
		}
 	}

	samples.clear();
	samples.shrink_to_fit();

	detail_colors.clear();
	detail_colors.shrink_to_fit();
}
#endif

void xrLight_Details()
{
	CTimer start_time;
	start_time.Start();

	static xr_atomic_u32 IndexTask = 0;
	IndexTask = 0;
	 
	if (gCompilerMode.Embree)
	{
		Status("Embree Initialize Models ...");
		EmbreeMain.InitializeDetails(gl_data.building_embree_faces);

		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [](size_t threadID)
			{
				while (true)
				{
					u32 Z = IndexTask.fetch_add(1);
					if (Z >= gl_data.slots_data.size_z()) break;

					for (u32 X = 0; X < gl_data.slots_data.size_x(); X++)
					{
						detail_slot_calculate(X, Z);
					}

					clMsg("Processing TaskID[%u/%u]", Z, gl_data.slots_data.size_z());
				}

				rayTasks.clear();
				rayTasks.shrink_to_fit();
			}
		);
	}
#ifdef LCCUDA_BUILD
	else
	{
 		size_x = gl_data.slots_data.size_x();
		size_z = gl_data.slots_data.size_z();

		samples.resize(size_x * size_z);
		detail_colors.resize(size_x * size_z);

		GPUTaskinSystem.InitializeGPU();
		GPUTaskinSystem.InitializeGPU_Model();

		GPUTaskinSystem.ColorsMapType = eDetails;

		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [](size_t threadID)
			{
				while (true)
				{
					u32 Z = IndexTask.fetch_add(1);
					AditionalData("Processing TaskID[%u/%u]", Z, gl_data.slots_data.size_z());

					if (Z >= gl_data.slots_data.size_z()) break;

					for (u32 X = 0; X < gl_data.slots_data.size_x(); X++)
						detail_slot_calculate(X, Z);
				}
				
				GPUTaskinSystem.LightPointPacked_run_tasks();
			}
		);

		ApplyColorsGPU();
	}
#endif

	Phase("Unloading data buffers...");
	gl_data.xrUnload();
 
	EmbreeMain.IntelEmbereUnloadAll();
#ifdef LCCUDA_BUILD
	GPUTaskinSystem.DestroyGPU_Model();
#endif

	Msg("Total processing: %u ms.", start_time.GetElapsed_ms());
}

