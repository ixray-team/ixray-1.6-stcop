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
 
void PackedLighting::InitializeGPU()
{
	clMsg("$ InitializeGPU RayTracing");
	XRay::RayTrace::CUDA::InitializeRayTracing();
}

//  
void PackedLighting::LightPointPacked(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
	{
		csEnter.Enter();
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
			LightPointPackedRun();
		csEnter.Leave();
	}

	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK = MakeKey(U, V);
	task_data.P = P;
	task_data.N = N;
	task_data.FaseSkip = GetFaceIndex(skip);
 
	task_pools.push_back(std::move(task_data));
}

void PackedLighting::LightPointPackedRun()
{
	// Инициализируем
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);

	// Устанавливаем параметры 
	for (size_t RayIndex = 0; RayIndex < task_pools.size(); RayIndex++)
		XRay::RayTrace::CUDA::RayTraceAddRay(task_pools[RayIndex], RayIndex);

	// Запускаем трейсинг
	XRay::RayTrace::CUDA::RayTraceRun(task_pools.size());

	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
	for (size_t TaskID = 0; TaskID < task_pools.size(); TaskID++)
	{
		auto& RAY_INFO = task_pools[TaskID];
		task_colors[RAY_INFO.INDEX_TASK].add(colors[TaskID]);
	}
	colors.clear();

	// Очистка
	task_pools.clear();
}


///
thread_local xr_vector<RayRecvestIndex> task_pools_implicit;
void PackedLighting::LightPointPacked_Implicit(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools_implicit.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
		LightPointPacked_ImplicitRun();
 
	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK = MakeKey(U, V);
 	task_data.P = P;
	task_data.N = N;
 	task_data.FaseSkip = GetFaceIndex(skip);

	task_pools_implicit.push_back(std::move(task_data));
}

extern void ApplyColorGPU(size_t IndexTask, base_color_c& C);
void PackedLighting::LightPointPacked_ImplicitRun()
{
 	// Инициализируем
 	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
  	
	// Устанавливаем параметры 
	auto& recvests = task_pools_implicit;
 	for (size_t RayIndex = 0; RayIndex < recvests.size(); RayIndex++)
		XRay::RayTrace::CUDA::RayTraceAddRay(recvests[RayIndex], RayIndex);

	// Запускаем трейсинг
	XRay::RayTrace::CUDA::RayTraceRun(recvests.size());

	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
  	for (size_t TaskID = 0; TaskID < recvests.size(); TaskID++)
	{
		auto& RAY_INFO = recvests[TaskID];
 		ApplyColorGPU(RAY_INFO.INDEX_TASK, colors[TaskID]);
  	}
 
 	// Очистка
	recvests.clear();
}

// Deflectors
 
// todo: Сделать для каждого потока очередь 
thread_local xr_vector<RayRecvestIndex>	task_pools_deflectors;

void PackedLighting::LightPointPackedDeflector(size_t IndexTask, CDeflector* D, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	// MT SAFE
	if (task_pools_deflectors.size() >= MAX_RAYS_PER_TASK - 1024)
       	LightPointPackedDeflectorsRun();
 
	// RayRecvestIndex task_data;
	RayRecvestIndex task_data;
 	task_data.INDEX_TASK = IndexTask;
	task_data.P = P;
	task_data.N = N;
	task_data.Owner = D; 
	task_data.FaseSkip = GetFaceIndex(skip);

	task_pools_deflectors.emplace_back( task_data );
}

void PackedLighting::LightPointPackedDeflectorsRun()
{ 
 	// Initialize
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), GPUTaskinSystem.current_flags);

	// Tasks
	auto& recvests = task_pools_deflectors;
	for (size_t RayIndex = 0; RayIndex < recvests.size(); RayIndex++)
		XRay::RayTrace::CUDA::RayTraceAddRay(recvests[RayIndex], RayIndex);

	// Запускаем трейсинг
	XRay::RayTrace::CUDA::RayTraceRun(recvests.size());

	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
	for (auto it = 0; it < recvests.size(); it++)
	{
		auto& RAY_INFO = recvests[it];
		auto D = (CDeflector*)RAY_INFO.Owner;
		D->ApplyColor(RAY_INFO.INDEX_TASK, colors[it]);
	}
	recvests.clear();
}

// MU-MODELS
thread_local xr_vector<RayRecvestIndex>	task_pools_mu;

void PackedLighting::LightPointPacked_MODEL(xrMU_Reference* MU, u32 I, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools_mu.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
  		LightPointPacked_MODELRun();
 
	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK			= I; 
	task_data.P = P;
	task_data.N = N;
	task_data.Owner = MU;
	task_data.FaseSkip = GetFaceIndex(skip);

 	task_pools_mu.push_back(std::move(task_data));

	// todo Add skiping faces
 }

void PackedLighting::LightPointPacked_MODELRun() 
{
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
	 
	// Устанавливаем параметры 
 	for (size_t it = 0; it < task_pools_mu.size(); it++)
  		XRay::RayTrace::CUDA::RayTraceAddRay(task_pools_mu[it], it);
 
	// Запускаем трейсинг
	XRay::RayTrace::CUDA::RayTraceRun(task_pools_mu.size());

	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
	for (int it = 0; it < task_pools_mu.size(); it++)
	{
		auto& RAY_INFO = task_pools_mu[it];
		auto MU = (xrMU_Reference*)RAY_INFO.Owner;
		if (MU != nullptr)
			MU->colors_cuda[RAY_INFO.INDEX_TASK].add(colors[it]);
	}
	task_pools_mu.clear();
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

