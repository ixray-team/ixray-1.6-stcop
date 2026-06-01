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
extern void ApplyColorDetailGPU(size_t IndexTask, base_color_c& C);
  
// Initialize TASKS
#define MAX_RAYS_PER_TASK   64*1024				// Общее кол-во Задач (на запуск GPU)
#define MAX_RAYS_PER_GPU	64*1024				// Кол-во задач которое может обработать GPU за 1 заход Слишком большое кол-во вызывает недогруз ГПУ
 
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
		// Initialize
		XRay::RayTrace::CUDA::RayTraceInitialize(current_flags, MAX_RAYS_PER_TASK);

		// Tasks
		for (size_t RecvestID = 0; RecvestID < recvest_array.size(); RecvestID++)
			XRay::RayTrace::CUDA::RayTraceAddRay(recvest_array[RecvestID], RecvestID);

		// Запускаем трейсинг
		XRay::RayTrace::CUDA::RayTraceRun();

		// Получаем результаты
		auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
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
		clMsg("* [CUDA] Unloading Rays");
		recvest_array.shrink_to_fit();
		XRay::RayTrace::CUDA::RayTraceUnload();
	}
}
 