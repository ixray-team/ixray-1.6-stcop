#include "stdafx.h"
#include "xrDeflectorLight_Packed.h"

#include <../xrForms/CompilersUI.h>
#include "../xrLC_Light/CUDA/CUDARayCast.h"
#include "light_point.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"


#include "CUDA/Vector3HW.h"
#include "embree_raytracing/EmbreeRayTrace.h"

void copy_color(hardware_color& Chw, base_color_c& C)
{
	C.hemi = Chw.hemi;
	C.sun = Chw.sun;
	C.rgb = { Chw.rgb.x, Chw.rgb.y, Chw.rgb.z };
};

auto LightHW = [&](hardware_lighting& L)
{
	R_Light cuL;
	cuL.type = L.type;
	cuL.diffuse = { L.diffuse.x, L.diffuse.y, L.diffuse.z };
	cuL.position = { L.position.x, L.position.y, L.position.z };
	cuL.direction = { L.direction.x, L.direction.y, L.direction.z };
	cuL.range = L.range;
	cuL.range2 = L.range2;
	cuL.falloff = L.falloff;
	cuL.attenuation0 = L.attenuation0;
	cuL.attenuation1 = L.attenuation1;
	cuL.attenuation2 = L.attenuation2;
	cuL.energy = L.energy;
	return cuL;
};

auto Light = [&](R_Light& L, int type)
{
	hardware_lighting cuL;
	cuL.type = L.type;
	cuL.light_type = type;
	cuL.diffuse = { L.diffuse.x, L.diffuse.y, L.diffuse.z };
	cuL.position = { L.position.x, L.position.y, L.position.z };
	cuL.direction = { L.direction.x, L.direction.y, L.direction.z };
	cuL.range = L.range;
	cuL.range2 = L.range2;
	cuL.falloff = L.falloff;
	cuL.attenuation0 = L.attenuation0;
	cuL.attenuation1 = L.attenuation1;
	cuL.attenuation2 = L.attenuation2;
	cuL.energy = L.energy;
	return cuL;
};

// Embree

float RaytraceEmbreeNew(hardware_lighting& Lnew, HardwareVector& Pnew, HardwareVector& Dnew, float R)
{
	auto V = LightHW(Lnew);
	auto P = Fvector().set(Pnew.x, Pnew.y, Pnew.z);
	auto D = Fvector().set(Dnew.x, Dnew.y, Dnew.z);
 	return EmbreeMain.RaytraceEmbreeProcess(V, P, D, R, 0);

}

void CalculatePoint(hardware_lighting& L, HardwareVector& P, HardwareVector& N, hardware_color& C, int& RealProcessed)
{
	HardwareVector Ldir;
	HardwareVector Pnew = P;
	Pnew.Mad_Self(N, 0.01f);

	HardwareVector LightPosition(L.position);
	HardwareVector LightDirection(L.direction);
	HardwareVector LightDiffuse(L.diffuse);

	bool isSunOrHemi = L.light_type != LGroup::eRGB;
	float att = 0;
	switch (L.type)
	{
		case LT_DIRECT:
		{
			Ldir.Inverted(LightDirection);
			float D = Ldir.DotProduct(N);
			if (D <= 0)
				return;

			float trace = RaytraceEmbreeNew(L, Pnew, Ldir, 1000.f);
 			att = isSunOrHemi ? L.energy * trace : D * L.energy * trace;
		}
		break;

		case LT_POINT:
		{
			float sqD = P.DistanceSquared(LightPosition);
			if (sqD > L.range2)
				return;

			Ldir.Subtract(LightPosition, P).Normalize_Safe();
			float D = Ldir.DotProduct(N);
			if (D <= 0)
				return;

			float R = sqrt(sqD); // from api
			float trace = RaytraceEmbreeNew(L, Pnew, Ldir, R);
 			float scale = D * L.energy * trace;
 
			if (isSunOrHemi)
			{
				att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
			}
			else
			{
				att = scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
			}

		}break;

		case LT_SECONDARY:
		{
			float sqD = P.DistanceSquared(LightPosition);
			if (sqD > L.range2)
				return;

			Ldir.Subtract(LightPosition, P).Normalize_Safe();
			float D = Ldir.DotProduct(N);
			if (D <= 0)
				return;

			D *= -Ldir.DotProduct(LightDirection);
			if (D <= 0)
				return;

			float R = sqrt(sqD);
			float trace = RaytraceEmbreeNew(L, Pnew, Ldir, R);
 			att = powf(D, 0.125f) * L.energy * trace * (1 - R / L.range);

		}break;
	}

	switch (L.light_type)
	{
	case eSun:
		C.sun += att;
		break;
	case eHemi:
		C.hemi += att;
		break;
	case eRGB:
		C.rgb.x += att * L.diffuse.x;
		C.rgb.y += att * L.diffuse.y;
		C.rgb.z += att * L.diffuse.z;
		break;
	}
};

void ProcessRays(Fvector& P, Fvector& D, base_lighting& LS, hardware_color& Cnew)
{
	int LightsProcessed = 0;
	 
	HardwareVector Pos(P.x, P.y, P.z);
	HardwareVector Dir(D.x, D.y, D.z);
	for (auto& L : LS.sun)
	{
		hardware_lighting Lnew = Light(L, LGroup::eSun);
		CalculatePoint(Lnew, Pos, Dir, Cnew, LightsProcessed);
	}

	for (auto& L : LS.hemi)
	{
		hardware_lighting Lnew = Light(L, LGroup::eHemi);
		CalculatePoint(Lnew, Pos, Dir, Cnew, LightsProcessed);
	}

	for (auto& L : LS.rgb)
	{
		hardware_lighting Lnew = Light(L, LGroup::eRGB);
		CalculatePoint(Lnew, Pos, Dir, Cnew, LightsProcessed);
	}

//	Msg("Lights Processed: %u", LightsProcessed);

}

// Cannot Now use in MT
PackedLighting GPUTaskinSystem;

void PackedLighting::InitializeGPU()
{
	XRay::RayTrace::CUDA::InitializeRayTracing();
}

void PackedLighting::LightPointPacked(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
	tStats.Start();
	if (task_pools.size() >= MAX_RAYS_PER_TASK - 1)
 		LightPointPackedRun();
 
	RayRecvestIndex task_data;		// MT SAFE
 	task_data.INDEX_TASK = { U, V };
 	task_data.P = P;
	task_data.N = N;

	task_pools.push_back( std::move(task_data) );
 	StatsRaysAdd += tStats.GetElapsed_mcs();
}

void PackedLighting::LightPointPackedRun()
{
 	// Инициализируем
	if (!isInitializedGPU)
	{
		InitializeGPU();
 		isInitializedGPU = true;
	}
	
	tStats.Start();

	clMsg("*** Start Tracing Rays: %u", task_pools.size());
	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);

 	// Устанавливаем параметры 
 	for (auto& task : task_pools)
 		XRay::RayTrace::CUDA::RayTraceAddRay(task);
	
	// Запускаем трейсинг
	CTimer t; t.Start();
	XRay::RayTrace::CUDA::RayTraceRun();
	StatsTraverseGPU += t.GetElapsed_mcs();
	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();
	
	// Копируем то что получили
   	for (auto it = 0; it < task_pools.size(); it++) // Последний таск ID (Тоесть size)
	{
		auto& INFO = task_pools[it];
		Colors[INFO.INDEX_TASK].add(colors[it]);
	}
 
	// Очистка
   	task_pools.clear();
	colors.clear();

	StatsTotalGPU += tStats.GetElapsed_mcs();
}

// Deflectors

void PackedLighting::LightPointPackedDeflector(u32 U, u32 V, CDeflector* D, Fvector& P, Fvector& N, u32 flags, Face* skip)
{
   	// if (task_pools.size() >= MAX_RAYS_PER_TASK - 1)
	// 	LightPointPackedRun();

	tStats.Start();
 	 
	RayRecvestIndex task_data;		// MT SAFE
	task_data.INDEX_TASK = { U, V };
	task_data.P = P;
	task_data.N = N;
	task_data.Owner = D;
 	task_pools.push_back( std::move(task_data) );

	StatsRaysAdd += tStats.GetElapsed_mcs();
}

void PackedLighting::LightPointPackedDeflectorsRun()
{	
	csRayLaunched.Enter();

	tStats.Start();
 	// Initialize
	if (!isInitializedGPU)
	{
		InitializeGPU();
		isInitializedGPU = true;
	}

	clMsg("*** Start Tracing Rays: %u", task_pools.size());
 	XRay::RayTrace::CUDA::RayTraceInitialize(lc_global_data()->L_static(), current_flags);
 	 
	// Устанавливаем параметры 
	int CurrentRays = 0;
	for (auto& task : task_pools)
	{
		if (CurrentRays >= MAX_RAYS_PER_TASK - 1)
		{
			XRay::RayTrace::CUDA::RayTraceRun();
			CurrentRays = 0;
		}
		XRay::RayTrace::CUDA::RayTraceAddRay(task);
		CurrentRays++;
	}

	// Запускаем трейсинг
	 
	XRay::RayTrace::CUDA::RayTraceRun();
	StatsTraverseGPU += tStats.GetElapsed_mcs();
	 
	// Получаем результаты
	auto& colors = XRay::RayTrace::CUDA::RayTraceResult();

	// Заполняем в дефолекторы
	int it = 0;
	for (auto RAY_INFO : task_pools)
	{
		DEF_Colors[RAY_INFO.Owner][RAY_INFO.INDEX_TASK].add(colors[it]);
		it++;
	}
 
	// Очистка
	task_pools.clear();
	colors.clear();

	StatsTotalGPU += tStats.GetElapsed_mcs();

	csRayLaunched.Leave();
}

