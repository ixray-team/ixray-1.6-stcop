#include "stdafx.h"
#include "xrDeflectorLight_Packed.h"

#include <../xrForms/CompilersUI.h>
#include "../xrLC_Light/CUDA/CUDARayCast.h"
#include "light_point.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"

PackedLighting GPUTaskinSystem;

void PackedLighting::InitializeGPU()
{
	XRay::RayTrace::CUDA::InitializeRayTracing();
}

void PackedLighting::LightPointPacked(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	tStats.Start();

	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
	{
		csAdd.Enter();
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
			LightPointPackedRun();
		csAdd.Leave();
	}


	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK = MakeKey(U, V);//{ U, V };
 	task_data.P = P;
	task_data.N = N;

	task_pools.push_back( std::move(task_data) );
 	StatsRaysAdd += tStats.GetElapsed_mcs();
}

void PackedLighting::LightPointPackedRun()
{
	//clMsg("$ Waiting Task: %u ms", tStats2.GetElapsed_ms());

 	// Инициализируем
	if (!isInitializedGPU)
	{
		InitializeGPU();
 		isInitializedGPU = true;
	}
	
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);

 	tStats.Start();
  	// Устанавливаем параметры 
 	for (auto& task : task_pools)
 		XRay::RayTrace::CUDA::RayTraceAddRay(task);
	StatsCopyRaysGPU += tStats.GetElapsed_mcs(); 


	// Запускаем трейсинг
	tStats.Start();
 	XRay::RayTrace::CUDA::RayTraceRun();
	StatsTraverseGPU += tStats.GetElapsed_mcs();
	
	
	// Получаем результаты
	tStats.Start();
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
	
	// Копируем то что получили
   	for (auto it = 0; it < task_pools.size(); it++) // Последний таск ID (Тоесть size)
	{
		auto& INFO = task_pools[it];
		Colors[INFO.INDEX_TASK].add(colors[it]);
	}
	StatsCopyResultGPU += tStats.GetElapsed_mcs();
	//clMsg("$ CopyResults: %u ms", tStats.GetElapsed_ms());
 
	tStats.Start();
	// Очистка
   	task_pools.clear();
	colors.clear();
	StatsClearingListGPU += tStats.GetElapsed_mcs();

	tStats2.Start();
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

	tStats.Start(); 
	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK = MakeKey(U, V); //= { U, V };
	task_data.P = P;
	task_data.N = N;
	task_data.Owner = D;
	task_data.skip = skip;
 	task_pools.push_back( std::move(task_data) );

	StatsRaysAdd += tStats.GetElapsed_mcs();
}

void PackedLighting::LightPointPackedDeflectorsRun()
{	
	// clMsg("$ Waiting Task: %u ms", tStats2.GetElapsed_ms());
	 	
	// Initialize
	if (!isInitializedGPU)
	{
		InitializeGPU();
		isInitializedGPU = true;
	}
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
	  
	tStats.Start();
	// Устанавливаем параметры 
	for (auto& task : task_pools)
 		XRay::RayTrace::CUDA::RayTraceAddRay(task);
	StatsCopyRaysGPU += tStats.GetElapsed_mcs();


	tStats.Start();
 	// Запускаем трейсинг
 	XRay::RayTrace::CUDA::RayTraceRun();
	StatsTraverseGPU += tStats.GetElapsed_mcs();
	
	
	tStats.Start();
	 
	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
	
	// Заполняем в дефолекторы
	int it = 0;
	for (auto RAY_INFO : task_pools)
	{
		DEF_Colors[RAY_INFO.Owner][RAY_INFO.INDEX_TASK].add(colors[it]);
		it++;
	}
	StatsCopyResultGPU += tStats.GetElapsed_mcs();
	// clMsg("$ CopyResults: %u ms", tStats.GetElapsed_ms());

	tStats.Start();
	// Очистка
	task_pools.clear();
	colors.clear();
	StatsClearingListGPU += tStats.GetElapsed_mcs();

	tStats2.Start();
}

