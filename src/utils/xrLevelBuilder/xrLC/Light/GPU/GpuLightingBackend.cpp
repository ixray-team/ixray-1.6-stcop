#include "stdafx.h"
#include "GpuLightingBackend.h"

#include "../../Build/Build.h"
#include "../../Build/xrLC_GlobalData.h"
#include "../../Mesh/xrFace.h"
#include "../../DetailLight/global_calculation_data.h"
#include "../../../UI/CompilersUI.h"
#include "../xrDeflector.h"
#include "../xrLightVertex.h"
#include "../light_point.h"
#include "GpuRayTracer.h"
#include "GpuPackedLighting.h"

extern void RunImplicitGPU(ImplicitDeflector& defl);
extern void RunMURefsGPU();

const char* GpuLightingBackend::GetName() const
{
	return Tracer.GetName();
}

void GpuLightingBackend::InitializeLevel()
{
	GPUTaskinSystem.Tracer = &Tracer;
	GPUTaskinSystem.InitializeGPU();
	GPUTaskinSystem.InitializeGPU_Model();				// Memory Usage 150-300MB !
}

void GpuLightingBackend::ReleaseLevel()
{
	GPUTaskinSystem.DestroyGPU_Model();
}

void GpuLightingBackend::LightAdaptiveHT(CBuild& Build)
{
	clMsg("Start Processing AdaptiveHT : MEMORY: %u mb", GetHeapMemory() / 1024 / 1024);

	GPUTaskinSystem.RestartALL();
	GPUTaskinSystem.ColorsMapType = eCommon;
	GPUTaskinSystem.current_flags = LP_dont_rgb + LP_dont_sun;

	for (size_t VertexID = 0; VertexID < lc_global_data()->g_vertices().size(); VertexID++)
	{
		// 1: VertexID, 2: SampleID
		auto& V = lc_global_data()->g_vertices()[VertexID];
		V->normalFromAdj();
		GPUTaskinSystem.LightPointPacked_add_task(GPUTaskinSystem.MakeKey(VertexID, 0), nullptr, V->P, V->N, 0);

		AditionalData("Vertex : %u / %u", VertexID, lc_global_data()->g_vertices().size());
	}

	GPUTaskinSystem.LightPointPacked_run_tasks();

	for (auto& TASK : GPUTaskinSystem.task_colors)
	{
		u32 U   = GPUTaskinSystem.GetU(TASK.first);
		auto& C = TASK.second;
		C.mul(0.5f);
		lc_global_data()->g_vertices()[U]->C._set(C);
	}

	GPUTaskinSystem.RestartALL();
}

void GpuLightingBackend::LightImplicit(ImplicitDeflector& Deflector)
{
	RunImplicitGPU(Deflector);
}

void GpuLightingBackend::LightDeflectors()
{
	// Se7kills
	CTimer start_time; start_time.Start();

	GPUTaskinSystem.RestartALL();
	GPUTaskinSystem.ColorsMapType = eDeflectors;
	GPUTaskinSystem.current_flags = LGetCurrentFlags();

	auto& deflectors = lc_global_data()->g_deflectors();

	xr_atomic_u32 IndexTaskID = 0, IndexTaskApply = 0;
	xr_std_parallel_for([&IndexTaskID, &deflectors]()
		{
			while (true)
			{
				u32 Index = IndexTaskID.fetch_add(1);
				if (Index >= deflectors.size())
				{
					break;
				}
				CDeflector* D = deflectors[Index];
				D->LightGPU();
				AditionalData("*** [LMAPS] ID [%u/%u]", Index, deflectors.size());
			}
			GPUTaskinSystem.LightPointPacked_run_tasks();
		},
		gCompilerMode.ThreadsPerWork );
	GPUTaskinSystem.RestartALL();

	xr_std_parallel_for([&IndexTaskApply, &deflectors]()
		{
			while (true)
			{
				u32 Index = IndexTaskApply.fetch_add(1);
				if (Index >= deflectors.size()) break;
				CDeflector* D = deflectors[Index];

				D->ApplyColors();
				D->ApplyExpandBordersGPU();
				AditionalData("*** [LMAPS] ApplyID [%u/%u]", Index, deflectors.size());
			}
		},
		gCompilerMode.ThreadsPerWork );

	clMsg("%f seconds", start_time.GetElapsed_sec());
}

void GpuLightingBackend::LightVertices(u32 Flags)
{
	int INDEX = 0;
	GPUTaskinSystem.RestartALL();
	GPUTaskinSystem.ColorsMapType = eCommon;
	GPUTaskinSystem.current_flags = Flags;

	xr_vector<float> v_transparency;
	v_transparency.resize(lc_global_data()->g_vertices().size());
	for (auto V : lc_global_data()->g_vertices())
	{
		float		v_trans = 0.f;
		if (GetTranslucency(V, v_trans))
		{
			GPUTaskinSystem.LightPointPacked_add_task(GPUTaskinSystem.MakeKey(INDEX, 0), nullptr, V->P, V->N, 0);
		}
		v_transparency[INDEX] = v_trans;
		INDEX++;
	}

	GPUTaskinSystem.LightPointPacked_run_tasks();
	for (auto& C : GPUTaskinSystem.task_colors)
	{
		int INDEX = GPUTaskinSystem.GetU(C.first);
		auto& V = lc_global_data()->g_vertices()[INDEX];
		auto& vC = C.second;
		float Transparency = v_transparency[INDEX];

		base_color_c old;
		V->C._get(old);

		vC._tmp_ = Transparency;
		vC.mul(.5f);
		vC.hemi = old.hemi;
		V->C._set(vC);

		g_trans_register(V);
	}

	GPUTaskinSystem.RestartALL();
}

void GpuLightingBackend::LightMURefs()
{
	RunMURefsGPU();
}

void GpuLightingBackend::OnStageFinished(ELightingStage Stage)
{
	switch (Stage)
	{
	case ELightingStage::Deflectors:
		XRay::RayTrace::GPU::CapturePreviewBakedColors(false);
		break;
	case ELightingStage::Vertices:
		XRay::RayTrace::GPU::CapturePreviewBakedColors(true);
		break;
	case ELightingStage::MUModels:
		XRay::RayTrace::GPU::CapturePreviewMU();
		break;
	}
}

void GpuLightingBackend::OnLevelFinished()
{
	XRay::RayTrace::GPU::ForgetPreviewFaces();
}

//////////////////////////////////////////////////////////////////////////
// xrDO
xr_vector<u32>			 samples;
xr_vector<base_color_c>  detail_colors;
u32 size_x;
u32 size_z;

// Called by GPUTaskinSystem for eDetails results
void ApplyColorDetailGPU(size_t IndexTask, base_color_c& C)
{
	u32 x = GPUTaskinSystem.GetU(IndexTask);
	u32 z = GPUTaskinSystem.GetV(IndexTask);

	u32 idx = z * size_x + x;
	samples[idx]++;
	detail_colors[idx].add(C);
}

static void ApplyColorsGPU()
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
		}
	}

	samples.clear();
	samples.shrink_to_fit();

	detail_colors.clear();
	detail_colors.shrink_to_fit();
}

void GpuLightingBackend::BeginDetails()
{
	size_x = gl_data.slots_data.size_x();
	size_z = gl_data.slots_data.size_z();

	samples.resize(size_x * size_z);
	detail_colors.resize(size_x * size_z);

	GPUTaskinSystem.Tracer = &Tracer;
	GPUTaskinSystem.InitializeGPU();
	GPUTaskinSystem.InitializeGPU_Model();

	GPUTaskinSystem.ColorsMapType = eDetails;
}

void GpuLightingBackend::LightDetailSlot(DetailSlot& Slot, xr_vector<DetailsTask>& Samples, base_lighting& Lights, u32 Count)
{
	for (DetailsTask& Task : Samples)
	{
		size_t idx = GPUTaskinSystem.MakeKey(Task.X, Task.Z);
		GPUTaskinSystem.LightPointPacked_add_task(idx, nullptr, Task.wP, Task.wN, nullptr);
	}
}

void GpuLightingBackend::FinishDetailsThread()
{
	GPUTaskinSystem.LightPointPacked_run_tasks();
}

void GpuLightingBackend::EndDetails()
{
	ApplyColorsGPU();
	GPUTaskinSystem.DestroyGPU_Model();
}
