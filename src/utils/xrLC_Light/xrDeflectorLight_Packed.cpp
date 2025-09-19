#include "stdafx.h"
#include "xrDeflectorLight_Packed.h"

#include <../xrForms/CompilersUI.h>
#include "../xrLC_Light/CUDA/CUDARayCast.h"
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
	task_pools.push_back(std::move(task_data));
}

void PackedLighting::LightPointPackedRun()
{
	clMsg("$ Run GPU TASK");

 	// Инициализируем
	if (!isInitializedGPU)
	{
		InitializeGPU();
 		isInitializedGPU = true;
	}
	
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);

	CTimer t;
	u64 CopyRays = 0;
	u64 GPU = 0;
	u64 CopyColors = 0;
	// Устанавливаем параметры 
 	auto process = [&](size_t begin, size_t end)
	{
		if (begin >= task_pools.size())
			return;
		end = std::min(end, task_pools.size());

		t.Start();
		size_t RayIndex = 0;
		for (size_t it = begin; it < end; it++)
		{
			XRay::RayTrace::CUDA::RayTraceAddRay(task_pools[it], RayIndex);
			RayIndex += 1;
		}
		CopyRays += t.GetElapsed_mcs();

		t.Start();
		// Запускаем трейсинг
		XRay::RayTrace::CUDA::RayTraceRun(RayIndex);
		GPU += t.GetElapsed_mcs();

		// Получаем результаты
		t.Start();

		auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
		RayIndex = 0;
		for (int it = begin; it < end; it++, RayIndex++)
		{
			auto& RAY_INFO = task_pools[it];
			Colors[RAY_INFO.INDEX_TASK].add(colors[RayIndex]);
 		}
		colors.clear();

		CopyColors += t.GetElapsed_mcs();
	};


	size_t Splice = MAX_RAYS_PER_GPU;
	for (size_t it = 0; it < task_pools.size(); it += Splice)
	{
		process(it, it + Splice);
	}
 
	clMsg("# Copy Rays : %u | GPU : %u | Copy: Result: %u ms", CopyRays / 1000, GPU / 1000, CopyColors / 1000);
 
	//for (auto it = 0; it < task_pools.size(); it++)
	//	XRay::RayTrace::CUDA::RayTraceAddRay(task_pools[it], it);
	// Запускаем трейсинг
 	// XRay::RayTrace::CUDA::RayTraceRun(task_pools.size());
	// Получаем результаты
	// auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
   	// for (auto it = 0; it < task_pools.size(); it++) // Последний таск ID (Тоесть size)
	// {
	// 	auto& INFO = task_pools[it];
	// 	Colors[INFO.INDEX_TASK].add(colors[it]);
	// }
 
	// Очистка
   	task_pools.clear();
}

// Deflectors

void PackedLighting::LightPointPackedDeflector(CDeflector* D, u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
	{
		csAdd.Enter();
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
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
	clMsg("$ Run GPU TASK DEFLECTORS");

	// Initialize
	if (!isInitializedGPU)
	{
		InitializeGPU();
		isInitializedGPU = true;
	}
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
	  


	CTimer t;
	u64 CopyRays = 0;
	u64 GPU = 0;
	u64 CopyColors = 0;
	// Устанавливаем параметры 
	auto process = [&](size_t begin, size_t end)
	{
		if (begin >= task_pools.size())
			return;
		end = std::min(end, task_pools.size());

		t.Start();
		size_t RayIndex = 0;
		for (size_t it = begin; it < end; it++)
		{
			XRay::RayTrace::CUDA::RayTraceAddRay(task_pools[it], RayIndex);
			RayIndex += 1;
		}
		CopyRays += t.GetElapsed_mcs();

		t.Start();
		// Запускаем трейсинг
		XRay::RayTrace::CUDA::RayTraceRun(RayIndex);
		GPU += t.GetElapsed_mcs();

		// Получаем результаты
		t.Start();

		auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
		RayIndex = 0;
		for (int it = begin; it < end; it++, RayIndex++)
		{
			auto& RAY_INFO = task_pools[it];
			auto D = RAY_INFO.Owner;
			if (D != nullptr)
				D->color_map[RAY_INFO.INDEX_TASK].add(colors[RayIndex]);
		}
		colors.clear();

		CopyColors += t.GetElapsed_mcs();
	};

	size_t Splice = MAX_RAYS_PER_GPU;
	for (size_t it = 0; it < task_pools.size(); it += Splice)
	{
		process(it, it + Splice);
	}
  
	clMsg("# Copy Rays : %u | GPU : %u | Copy: Result: %u ms", CopyRays / 1000, GPU / 1000, CopyColors / 1000);


	// Очистка
	task_pools.clear();
	
}
 

// MU-MODELS

void PackedLighting::LightPointPacked_MODEL(xrMU_Reference* MU, u32 I, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
	{
		csAdd.Enter();
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
			LightPointPackedDeflectorsRun();
		csAdd.Leave();
	}

	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK			= I; 
	task_data.P = P;
	task_data.N = N;
	task_data.xrMODEL = MU;
	task_data.skip = skip;
	task_pools.push_back(std::move(task_data));
}

void PackedLighting::LightPointPacked_MODELRun() 
{
	// Initialize
	if (!isInitializedGPU)
	{
		InitializeGPU();
		isInitializedGPU = true;
	}
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
	 
	// Устанавливаем параметры 
	auto process = [&](size_t begin, size_t end)
	{
		if (begin >= task_pools.size())
			return;
		end = std::min(end, task_pools.size());
		size_t RayIndex = 0;
		for (size_t it = begin; it < end; it++)
		{
			XRay::RayTrace::CUDA::RayTraceAddRay(task_pools[it], RayIndex);
			RayIndex += 1;
		}
		// Запускаем трейсинг
		XRay::RayTrace::CUDA::RayTraceRun(RayIndex);

		// Получаем результаты
		auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
		RayIndex = 0;
		for (int it = begin; it < end; it++, RayIndex++)
		{
			auto& RAY_INFO = task_pools[it];
			auto MU = RAY_INFO.xrMODEL;
			if (MU != nullptr)
				MU->colors_cuda[RAY_INFO.INDEX_TASK].add(colors[RayIndex]);
		}
		colors.clear();
	};

	size_t Splice = MAX_RAYS_PER_GPU;
	for (size_t it = 0; it < task_pools.size(); it += Splice)
	{
		process(it, it + Splice);
	}
}
