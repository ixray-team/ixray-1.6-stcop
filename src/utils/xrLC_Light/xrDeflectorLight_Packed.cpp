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
	// task_data.skip = skip;
	task_pools.push_back(std::move(task_data));
}

void PackedLighting::LightPointPackedRun()
{
 	// Инициализируем
 	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
	xr_vector<base_color_c> colors_result; 
	colors_result.reserve(task_pools.size());
	
	// Устанавливаем параметры 
 	auto process = [&](size_t begin, size_t end)
	{
		if (begin >= task_pools.size())		return;
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
		colors_result.insert(colors_result.end(), colors.begin(), colors.end());
 		colors.clear();
	};

	size_t Splice = MAX_RAYS_PER_GPU;
	for (size_t it = 0; it < task_pools.size(); it += Splice)
 		process(it, it + Splice);
 
	for (size_t it = 0; it < task_pools.size(); it ++)
	{
		auto& RAY_INFO = task_pools[it];
		Colors[RAY_INFO.INDEX_TASK].add(colors_result[it]);
	}
 
	// Очистка
   	task_pools.clear();
}

// Deflectors

void PackedLighting::LightPointPackedDeflector(CDeflector* D,  u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
	{
		csAdd.Enter();
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
			LightPointPackedDeflectorsRun();
		csAdd.Leave();
	}

	RayRecvestIndex task_data;				// MT SAFE
	task_data.INDEX_TASK = MakeKey(U, V);   
	task_data.P = P;
	task_data.N = N;
	task_data.Owner = D;
  	task_pools.push_back(std::move(task_data));
}

void PackedLighting::LightPointPackedDeflectorsRun()
{
	// Устанавливаем параметры 
 	auto process = [](auto& recvests, auto& colors_result, size_t begin, size_t end)
	{
		if (begin >= recvests.size())				return;
		end = std::min(end, recvests.size());

		size_t RayIndex = 0;
		for (size_t it = begin; it < end; it++)
		{
			XRay::RayTrace::CUDA::RayTraceAddRay(recvests[it], RayIndex);
			RayIndex += 1;
		}

		// Запускаем трейсинг
		XRay::RayTrace::CUDA::RayTraceRun(RayIndex);

		// Получаем результаты
		auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
		colors_result.insert(colors_result.end(), colors.begin(), colors.end());
		colors.clear();
	};

	auto& rays = task_pools;
	// Initialize
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);

	// Result Alloca 
	xr_vector<base_color_c> colors_result;
	colors_result.reserve(rays.size());

	// Processing
	size_t Splice = MAX_RAYS_PER_GPU;
	for (size_t it = 0; it < rays.size(); it += Splice)
	{
		process(rays, colors_result, it, it + Splice);
	}

	for (int it = 0; it < rays.size(); it++)
	{
		auto& RAY_INFO = rays[it];
		auto D = (CDeflector*)RAY_INFO.Owner;
		if (D != nullptr)
			D->ApplyColor(RAY_INFO.INDEX_TASK, colors_result[it]);
		else
			clMsg("Deflector [%p] is nullptr", RAY_INFO.Owner);
	}

	colors_result.clear();
	rays.clear();
}

// MU-MODELS

void PackedLighting::LightPointPacked_MODEL(xrMU_Reference* MU, u32 I, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
	{
		csAdd.Enter();
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
			LightPointPacked_MODELRun();
		csAdd.Leave();
	}

	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK			= I; 
	task_data.P = P;
	task_data.N = N;
	task_data.Owner = MU;
//	task_data.skip = skip;

 	task_pools.push_back(std::move(task_data));
 }

void PackedLighting::LightPointPacked_MODELRun() 
{
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
	 
	// Устанавливаем параметры 
	auto process = [&](size_t begin, size_t end)
	{
	//	clMsg("Start Processing Rays: %u to %u", begin, end);

		if (begin >= task_pools.size())		return;
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
			auto MU = (xrMU_Reference*) RAY_INFO.Owner;
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

	task_pools.clear();
}
