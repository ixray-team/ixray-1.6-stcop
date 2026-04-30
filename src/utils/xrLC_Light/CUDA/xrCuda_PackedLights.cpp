#include "stdafx.h"
#include "xrCuda_PackedLights.h"

#include <../xrForms/CompilersUI.h>
#include "CUDA/CUDARayCast.h"
#include "light_point.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"
#include "xrDeflector.h"
#include "xrMU_Model_Reference.h"

CUDA_PackedLighting GPUTaskinSystem;
thread_local xr_vector<RayRecvestIndex>	recvest_array;
extern void ApplyColorGPU(size_t IndexTask, base_color_c& C);
 

// Initializes

void CUDA_PackedLighting::InitializeGPU()
{
 	XRay::RayTrace::CUDA::InitializeGPU();
}

void CUDA_PackedLighting::InitializeGPU_Model()
{
 	XRay::RayTrace::CUDA::InitializeModel();
}

void CUDA_PackedLighting::DestroyGPU_Model()
{
 	XRay::RayTrace::CUDA::UnloadingModel();
	RestartALL();
}

void CUDA_PackedLighting::RestartALL()
{
	// start
	Recalculated = 0;
	current_flags = 0;

	// clearing pool
	xr_concurrent_unordered_map <size_t, base_color_c>  new_colors;
	task_colors.swap(new_colors);
}

// Deflectors
void CUDA_PackedLighting::LightPointPacked_add_task(size_t IndexTask, void* Owner, Fvector& P, Fvector& N, Face* skip)
{
	// MT SAFE
	if (recvest_array.size() >= MAX_RAYS_PER_TASK - 16)
		LightPointPacked_run_tasks();
 
 	RayRecvestIndex task_data;
 	task_data.INDEX_TASK	= IndexTask;
	task_data.P				= P;
	task_data.N				= N;
	task_data.Owner			= Owner;
	recvest_array.emplace_back( task_data );
}
 
void CUDA_PackedLighting::LightPointPacked_run_tasks()
{
 	if (recvest_array.size() <= 0) return;

 	// Initialize
	XRay::RayTrace::CUDA::RayTraceInitialize( current_flags );

	// Tasks
 	for (size_t RayIndex = 0; RayIndex < recvest_array.size(); RayIndex++)
		XRay::RayTrace::CUDA::RayTraceAddRay(recvest_array[RayIndex], RayIndex);

	// Запускаем трейсинг
	XRay::RayTrace::CUDA::RayTraceRun();

	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
	for (auto RecvestID = 0; RecvestID < recvest_array.size(); RecvestID++)
	{
		auto& RAY_INFO = recvest_array[RecvestID];
 		 
		switch (ColorsMapType)
		{
			case eImplicit:
			{
				ApplyColorGPU(RAY_INFO.INDEX_TASK, colors[RecvestID]);
			}break;
		
			case eDeflectors:
			{
				( (CDeflector*) RAY_INFO.Owner)->ApplyColor(RAY_INFO.INDEX_TASK, colors[RecvestID]);
			}break;
		
			case eMumodel:
			{
				( (xrMU_Reference*) RAY_INFO.Owner )->colors_cuda[RAY_INFO.INDEX_TASK].add(colors[RecvestID]);;
			}break;
		
			case eCommon:
			{
				task_colors[RAY_INFO.INDEX_TASK].add(colors[RecvestID]);
			}break;
		
		}		
 	}

	recvest_array.clear();
}
 