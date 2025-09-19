#include "stdafx.h"
#include "xrDeflectorLight_Packed.h"

#include <../xrForms/CompilersUI.h>
#include "../xrLC_Light/CUDA/CUDARayCast.h"
#include "light_point.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"
#include "xrDeflector.h"

PackedLighting GPUTaskinSystem;

void PackedLighting::InitializeGPU()
{
	XRay::RayTrace::CUDA::InitializeRayTracing();
}

void PackedLighting::LightPointPacked(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
	{
		csAdd.Enter();
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
			LightPointPackedRun();
		csAdd.Leave();
	}

	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK = MakeKey(U, V);
 	task_data.P = P;
	task_data.N = N;

	task_pools.push_back( std::move(task_data) );
}

void PackedLighting::LightPointPackedRun()
{
 	// Инициализируем
	if (!isInitializedGPU)
	{
		InitializeGPU();
 		isInitializedGPU = true;
	}
	
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);

	// Устанавливаем параметры 
 	for (auto& task : task_pools)
 		XRay::RayTrace::CUDA::RayTraceAddRay(task);

	// Запускаем трейсинг
 	XRay::RayTrace::CUDA::RayTraceRun();
	
	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
   	for (auto it = 0; it < task_pools.size(); it++) // Последний таск ID (Тоесть size)
	{
		auto& INFO = task_pools[it];
		Colors[INFO.INDEX_TASK].add(colors[it]);
	}
 
	// Очистка
   	task_pools.clear();
	colors.clear();
}

// Deflectors

void PackedLighting::LightPointPackedDeflector(CDeflector* D, u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
	{
		csAdd.Enter();
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)	// 2я проверка изза поточности
			LightPointPackedDeflectorsRun();
		csAdd.Leave();
	}

	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK = MakeKey(U, V); //= { U, V };
	task_data.P = P;
	task_data.N = N;
	task_data.Owner = D;
	task_data.skip = skip;
 	task_pools.push_back( std::move(task_data) );
}

void PackedLighting::LightPointPackedDeflectorsRun()
{		
	// Initialize
	if (!isInitializedGPU)
	{
		InitializeGPU();
		isInitializedGPU = true;
	}
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
	  
	// Устанавливаем параметры 
	for (auto& task : task_pools)
 		XRay::RayTrace::CUDA::RayTraceAddRay(task);

 	// Запускаем трейсинг
 	XRay::RayTrace::CUDA::RayTraceRun();

	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
 	for (int it = 0; it < task_pools.size();it++)
	{
		auto& RAY_INFO = task_pools[it];
		auto D = RAY_INFO.Owner;
 		if (D != nullptr)
 			D->color_map[RAY_INFO.INDEX_TASK].add(colors[it]);
	}

	// Очистка
	task_pools.clear();
	colors.clear();
}

