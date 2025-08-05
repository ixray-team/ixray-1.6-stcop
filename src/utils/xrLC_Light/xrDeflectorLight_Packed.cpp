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
	
	static CTimer TaskT;

	// Устанавливаем параметры 
 	auto process = [&](size_t begin, size_t end, size_t& GPUms, size_t& CPUms)
	{
		if (begin >= task_pools.size())		return;
		end = std::min(end, task_pools.size());
 
		TaskT.Start();
		size_t RayIndex = 0;
		for (size_t it = begin; it < end; it++)
		{
			XRay::RayTrace::CUDA::RayTraceAddRay(task_pools[it], RayIndex);
			RayIndex += 1;
		}
		CPUms += TaskT.GetElapsed_ms(); TaskT.Start();

		// Запускаем трейсинг
		XRay::RayTrace::CUDA::RayTraceRun(RayIndex);
		GPUms += TaskT.GetElapsed_ms(); TaskT.Start();

		// Получаем результаты
		auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
		colors_result.insert(colors_result.end(), colors.begin(), colors.end());
 		colors.clear();

		CPUms += TaskT.GetElapsed_ms(); 
	};

	size_t Splice = MAX_RAYS_PER_GPU;
	size_t GPUms = 0, CPUms = 0;
	
	int Splices = task_pools.size() / Splice;
	int IndexSplit = 0;
	for (size_t it = 0; it < task_pools.size(); it += Splice, IndexSplit++)
	{
		process(it, it + Splice, GPUms, CPUms); 
		AditionalData("GPU Process %f", float(IndexSplit/Splices) * 100);
	}

	TaskT.Start();
	for (size_t it = 0; it < task_pools.size(); it ++)
	{
		auto& RAY_INFO = task_pools[it];
		Colors[RAY_INFO.INDEX_TASK].add(colors_result[it]);
	}
	size_t ApplyColorsMs = TaskT.GetElapsed_ms();
	// Очистка
   	task_pools.clear();


	clMsg("$ Elapsed GPU: %u ms | CPU Copy: %u ms | CPU Apply: %u ms", GPUms, CPUms, ApplyColorsMs);
}

// Deflectors

void PackedLighting::LightPointPackedDeflector(CDeflector* D,  u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{

	RayRecvestIndex task_data;				// MT SAFE

	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)
	{
		csEnter.Enter();
		
		if (task_pools.size() >= MAX_RAYS_PER_TASK - 1024)			// Хитрость чтобы часто не вызывать блокировку
			LightPointPackedDeflectorsRun();
		csEnter.Leave();
	}

	task_data.INDEX_TASK = MakeKey(U, V);   
	task_data.P = P;
	task_data.N = N;
	task_data.Owner = D;
  	task_pools.push_back(std::move(task_data));	
}

void PackedLighting::LightPointPackedDeflectorsRun()
{	
	// Initialize
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
 
	static CTimer TaskT;
	// Устанавливаем параметры 
 	auto process = [](auto& recvests, auto& colors_result, size_t begin, size_t end, size_t& ElapsedGPU, size_t& ElapsedCPU)
	{
		if (begin >= recvests.size())				return;
		end = std::min(end, recvests.size());
	
		TaskT.Start();
		size_t RayIndex = 0;
		for (size_t it = begin; it < end; it++)
		{
			XRay::RayTrace::CUDA::RayTraceAddRay(recvests[it], RayIndex);
			RayIndex += 1;
		}
		ElapsedCPU += TaskT.GetElapsed_ms(); TaskT.Start();
	
		// Запускаем трейсинг
 		XRay::RayTrace::CUDA::RayTraceRun(RayIndex);
		ElapsedGPU += TaskT.GetElapsed_ms(); TaskT.Start();
		// Получаем результаты
		
		auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
		colors_result.insert(colors_result.end(), colors.begin(), colors.end());
		colors.clear();

		ElapsedCPU += TaskT.GetElapsed_ms();
	};

	 auto& rays = task_pools;
 
 	 xr_vector<base_color_c> colors_result;
	 colors_result.reserve(rays.size());
 
	 size_t Splice = MAX_RAYS_PER_GPU;
	 size_t GPUElapsed = 0;
	 size_t CPUElapsed = 0;

	 int Splices = rays.size() / Splice;
	 int IndexSplit = 0;
	 for (size_t it = 0; it < rays.size(); it += Splice, IndexSplit++)
	 {
		 process(rays, colors_result, it, it + Splice, GPUElapsed, CPUElapsed);
		 
		 AditionalData("GPU Process %f", float(IndexSplit / Splices) * 100);
	 }
 
	 TaskT.Start();
 	 xr_parallel_for(size_t(0), rays.size(), [&](size_t it)
	 {
	 	auto& RAY_INFO = rays[it];
	 	auto D = (CDeflector*)RAY_INFO.Owner;
	 	if (D != nullptr)
			D->ApplyColor(RAY_INFO.INDEX_TASK, colors_result[it]);
	 });
	 size_t ProcessApplyColors = TaskT.GetElapsed_ms();
  
	colors_result.clear();
	rays.clear();

	clMsg("$ Elapsed GPU: %u ms | CPU Copy: %u ms | CPU[MT] Apply: %u ms", GPUElapsed, CPUElapsed, ProcessApplyColors);
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
