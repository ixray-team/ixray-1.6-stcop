#include "stdafx.h"
#include "xrDeflectorLight_Packed.h"

#include <../xrForms/CompilersUI.h>
#include "CUDA/CUDARayCast.h"
#include "light_point.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"
#include "xrDeflector.h"
#include "xrMU_Model_Reference.h"

PackedLighting GPUTaskinSystem;
thread_local xr_vector<RayRecvestIndex>	task_pools_deflectors;
extern void ApplyColorGPU(size_t IndexTask, base_color_c& C);
 

// Initializes
void PackedLighting::InitializeGPU()
{
	clMsg("$ InitializeGPU RayTracing");
	XRay::RayTrace::CUDA::InitializeRayTracing();
}

// Deflectors
void PackedLighting::LightPointPacked_add_task(size_t IndexTask, void* Owner, Fvector& P, Fvector& N, Face* skip)
{
	// MT SAFE
	if (task_pools_deflectors.size() >= MAX_RAYS_PER_TASK - 1024)
		LightPointPacked_run_tasks();
 
	// RayRecvestIndex task_data;
	RayRecvestIndex task_data;
 	task_data.INDEX_TASK	= IndexTask;
	task_data.P				= P;
	task_data.N				= N;
	task_data.Owner			= Owner;
  	task_pools_deflectors.emplace_back( task_data );
}
 
void PackedLighting::LightPointPacked_run_tasks()
{
	auto& recvests = task_pools_deflectors;
	if (recvests.size() <= 0) return;

 	// Initialize
	XRay::RayTrace::CUDA::RayTraceInitialize( current_flags );

	// Tasks
 	for (size_t RayIndex = 0; RayIndex < recvests.size(); RayIndex++)
		XRay::RayTrace::CUDA::RayTraceAddRay(recvests[RayIndex], RayIndex);

	// Запускаем трейсинг
	XRay::RayTrace::CUDA::RayTraceRun();

	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
	for (auto RecvestID = 0; RecvestID < recvests.size(); RecvestID++)
	{
		auto& RAY_INFO = recvests[RecvestID];
 		 
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

	recvests.clear();
}
 
// Enumerate Faces
xr_hash_map<Face*, u32>   facesIndexes;
u32 GetFaceIndex(Face* F)
{
	return facesIndexes[F];
}

void SetFaceIndex(Face* F, u32 Index)
{
	facesIndexes[F] = Index;
}

