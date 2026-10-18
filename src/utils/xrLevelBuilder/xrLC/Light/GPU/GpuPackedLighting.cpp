#include "stdafx.h"
#include "GpuPackedLighting.h"
#include "GpuRayTracer.h"

#include "../../../UI/CompilersUI.h"
#include "../light_point.h"
#include "../../Build/xrLC_GlobalData.h"
#include "../../Mesh/xrFace.h"
#include "../xrDeflector.h"
#include "../../MUModels/xrMU_Model_Reference.h"

CUDA_PackedLighting GPUTaskinSystem;
thread_local xr_vector<RayRecvestIndex>	recvest_array;
thread_local xr_vector<base_color_c>		recvest_colors;
extern void ApplyColorGPU(size_t IndexTask, base_color_c& C);
extern void ApplyColorDetailGPU(size_t IndexTask, base_color_c& C);
  
#define MAX_RAYS_PER_TASK   IGpuRayTracer::MaxRaysPerBatch		// Общее кол-во Задач (на запуск GPU)

// Initializes
void CUDA_PackedLighting::InitializeGPU()
{
	R_ASSERT(Tracer);
	Tracer->Initialize();
}

void CUDA_PackedLighting::InitializeGPU_Model()
{
	GpuScene Scene;
	XRay::RayTrace::GPU::BuildScene(Scene);
	Tracer->LoadScene(Scene);
}

void CUDA_PackedLighting::DestroyGPU_Model()
{
	Tracer->UnloadScene();
	RestartALL();
}

void CUDA_PackedLighting::RestartALL()
{
	// start
 	current_flags = 0;

	// clearing pool
	task_colors.clear();
}

// Deflectors
void CUDA_PackedLighting::LightPointPacked_add_task(size_t IndexTask, void* Owner, Fvector& P, Fvector& N, Face* skip)
{
	// MT SAFE
	if (recvest_array.size() >= MAX_RAYS_PER_TASK - 16)
		LightPointPacked_run_tasks(false);
 
 	RayRecvestIndex task_data;
 	task_data.INDEX_TASK	= IndexTask;
	task_data.P				= P;
	task_data.N				= N;
	task_data.Owner			= Owner;
	recvest_array.emplace_back( task_data );
}
 
void CUDA_PackedLighting::LightPointPacked_run_tasks(bool unload)
{
	if (recvest_array.size() > 0)
	{
		// Запускаем трейсинг
		Tracer->Trace(current_flags, recvest_array, recvest_colors);
		auto& colors = recvest_colors;

		for (size_t RecvestID = 0; RecvestID < recvest_array.size(); RecvestID++)
		{
			auto& RAY_INFO = recvest_array[RecvestID];

			switch (ColorsMapType)
			{
				case eImplicit:
				{
					ApplyColorGPU(RAY_INFO.INDEX_TASK, colors[RecvestID]);
				}break;

				case eDetails:
				{
					ApplyColorDetailGPU(RAY_INFO.INDEX_TASK, colors[RecvestID]);
				}break;

				case eDeflectors:
				{
					((CDeflector*)RAY_INFO.Owner)->ApplyColor(RAY_INFO.INDEX_TASK, colors[RecvestID]);
				}break;

				case eMumodel:
				{
					((xrMU_Reference*)RAY_INFO.Owner)->colors_cuda[RAY_INFO.INDEX_TASK].add(colors[RecvestID]);;
				}break;

				case eCommon:
				{
					task_colors[RAY_INFO.INDEX_TASK].add(colors[RecvestID]);
				}break;
 			}
		}

		recvest_array.clear();
	}
 	
 	if (unload)
	{
		clMsg("* [GPU] Unloading Rays");
		recvest_array.shrink_to_fit();
		recvest_colors.clear();
		recvest_colors.shrink_to_fit();
		Tracer->ReleaseThreadData();
	}
}
 