#include "stdafx.h"
#include "EmbreeLightingBackend.h"

#include "../../Build/Build.h"
#include "../../Build/xrLC_GlobalData.h"
#include "../../Mesh/xrFace.h"
#include "../../MUModels/xrMU_Model_Reference.h"
#include "../../DetailLight/global_calculation_data.h"
#include "../../../UI/CompilersUI.h"
#include "../xrDeflector.h"
#include "../xrLightVertex.h"
#include "../light_point.h"

extern void RunImplicitMultithread(ImplicitDeflector& defl);

void EmbreeLightingBackend::InitializeLevel()
{
	EmbreeMain.InitializeGeometry();
}

void EmbreeLightingBackend::LightAdaptiveHT(CBuild& Build)
{
	// Prepare
	Status("AdaptiveHT : base hemisphere ...");

	static xr_atomic_u32 ThreadWorkID_Adaptive = 0;

	ThreadWorkID_Adaptive = 0;
	xr_std_parallel_for([]()
	{
		while (true)
		{
			u32 taskID = ThreadWorkID_Adaptive.fetch_add(1);
			if (taskID >= lc_global_data()->g_vertices().size())	break;

			base_color_c		vC;
			vecVertex& verts = lc_global_data()->g_vertices();
			Vertex* V = verts[taskID];

			V->normalFromAdj();
			LightPoint(EmbreeMain, vC, V->P, V->N, pBuild->L_static(), LP_dont_rgb + LP_dont_sun, 0);
			vC.mul(0.5f);
			V->C._set(vC);
		}
	}, gCompilerMode.ThreadsPerWork);

	//////////////////////////////////////////////////////////////////////////
	Status("AdaptiveHT : Gathering lighting information...");
	Build.u_SmoothVertColors(5);
}

void EmbreeLightingBackend::LightImplicit(ImplicitDeflector& Deflector)
{
	RunImplicitMultithread(Deflector);
}

void EmbreeLightingBackend::LightDeflectors()
{
	// Main process (4 threads)
	Status("Lighting...");

	CTimer start_time;
	start_time.Start();

	xr_atomic_u32 LmapsTaskID = 0;
	xr_std_parallel_for([&LmapsTaskID]()
	{
		base_lighting	LightsSelected;
		while (true)
		{
			// Get task
			u32 IndexTask = LmapsTaskID.fetch_add(1);
			if (IndexTask >= lc_global_data()->g_deflectors().size()) break;
			CDeflector* D = lc_global_data()->g_deflectors()[IndexTask];
			D->Light(&LightsSelected);
			AditionalData("Deflectors: %u / %u", IndexTask, lc_global_data()->g_deflectors().size());
		}
	}, gCompilerMode.ThreadsPerWork );
	clMsg("%f seconds", start_time.GetElapsed_sec());
}

void EmbreeLightingBackend::LightVertices(u32 Flags)
{
	xr_atomic_u32 TasksIds = 0;
	xr_std_parallel_for([&TasksIds, Flags]()
	{
		while (true)
		{
			u32 tID = TasksIds.fetch_add(1);
			if (tID >= lc_global_data()->g_vertices().size()) break;

			Vertex* V = lc_global_data()->g_vertices()[tID];
			float		v_trans = 0.f;
			if (GetTranslucency(V, v_trans))
			{
				base_color_c		vC, old;
				V->C._get(old);

				LightPoint(EmbreeMain, vC, V->P, V->N, lc_global_data()->L_static(), Flags, 0);

				vC._tmp_ = v_trans;
				vC.mul(.5f);
				vC.hemi = old.hemi;			// preserve pre-calculated hemisphere
				V->C._set(vC);

				g_trans_register(V);
			}
		}
	}, gCompilerMode.ThreadsPerWork);
}

void EmbreeLightingBackend::LightMURefs()
{
	xr_atomic_u32 ThreadTaskID = 0;
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

void EmbreeLightingBackend::BeginDetails()
{
	Status("Embree Initialize Models ...");
	EmbreeMain.InitializeDetails(gl_data.building_embree_faces);
}

void EmbreeLightingBackend::LightDetailSlot(DetailSlot& Slot, xr_vector<DetailsTask>& Samples, base_lighting& Lights, u32 Count)
{
	base_color_c amount;

	LightPoint_Details(Samples, Lights, 0);			// Идеально пакетные пашут ибо 225 из одной позиции запросов !
	for (auto& task : Samples)
		amount.add( task.C );

	// calculation of luminocity (225 samples на 1 травинку) может и много :)
	amount.scale(Count);
	amount.mul(.5f);

	// Пишется результат в (level.details) !
	Slot.c_dir = Slot.w_qclr(amount.sun, 15);
	Slot.c_hemi = Slot.w_qclr(amount.hemi, 15);
	Slot.c_r = Slot.w_qclr(amount.rgb.x, 15);
	Slot.c_g = Slot.w_qclr(amount.rgb.y, 15);
	Slot.c_b = Slot.w_qclr(amount.rgb.z, 15);
}
