

#include <cuda_runtime.h>
  
#include <optix.h>
#include <optix_device.h>
#include "Vector3HW.h"

#ifdef __INTELLISENSE__
#define __device__
#define __global__
#define __constant__
#define __shared__
#define __host__
#endif

// Jitter Select
 
extern "C"
{
	__constant__ OPTICK_Params g_params;
}

enum Flags
{
	LP_Default = 0,
	LP_UseFaceDisable	= (1 << 0),
	LP_dont_rgb			= (1 << 1),
	LP_dont_hemi		= (1 << 2),
	LP_dont_sun			= (1 << 3),
};
 
#define LT_DIRECT		0
#define LT_POINT		1
#define LT_SECONDARY	2
 
#define ENERGY_MIN 0.01f

#include "optix_types.h"

// Entry points

extern "C" __global__ void __miss__ms()
{
	// если ничего не встретили → свет проходит
}

extern "C" __global__ void __closesthit__ch()
{
	optixSetPayload_1(0);
}

__device__ void calculate_energy(Hardware_FaceData& F, Hardware_TextureData& T, int primID, float& energy)
{
 	// barycentrics
	const float2 bc		= optixGetTriangleBarycentrics();
	float hitU = bc.x;
	float hitV = bc.y;
 	float b0			= 1.0f - hitU - hitV;
 	 
	// interpolate UV
	float u = F.TC0[0].x * b0 + F.TC0[1].x * hitU + F.TC0[2].x * hitV;
	float v = F.TC0[0].y * b0 + F.TC0[1].y * hitU + F.TC0[2].y * hitV;

	int U = (int) floor (u * float(T.width) + .5f);
	int V = (int) floor (v * float(T.height) + .5f);
	U %= T.width;		if (U < 0) U += T.width;
	V %= T.height;		if (V < 0) V += T.height;

	float a = ( T.pSurface[V * T.width + U] / 255.0f );
	float Transparency = (1.f - a * a);
	energy *= Transparency;
}
 
// Косяк возможно в порядке пересечения в Embree идет все по hit_t < prev_t а Optix в разнобой

// Хотя наврятле там только *= opac; поидее порядок не сильно важен
extern "C" __global__ void __anyhit__ah()
{
	// Not used
	const int primID				= optixGetPrimitiveIndex();
	unsigned int task_id			= optixGetPayload_0();
 	unsigned int energy_int			= optixGetPayload_1();

 	float energy					= float(energy_int) / 100.0f;
 
	Hardware_FaceData&    F			= g_params.faces[primID];
	Hardware_TextureData& T			= g_params.textures[F.surfidx];

	if (F.bOpacue || T.pSurface == nullptr)
	{
		// Не имеюь прозрачности  → останавливаемся
 		optixSetPayload_1(0);
		optixTerminateRay();
		return;
	}
	  
	// energy attenuation (LUT)
	calculate_energy(F, T, primID, energy);				// Проверка тут на воду не понятно почему делает ее темной

	// opaque → остановить
 	if (energy < ENERGY_MIN)
	{
	 	optixSetPayload_1(0);
		optixTerminateRay();
		return; // closesthit будет вызван
	}
		
	// transparent → пропускаем и летим дальше
 	// Тут тоже сделал явное преобразование
	unsigned int EnergyReturn = float(energy * 100.0f);
	optixSetPayload_1(EnergyReturn);
	optixIgnoreIntersection();
}

__device__ float RunOptickTask(unsigned int TaskID, Hardware_Vector& P, Hardware_Vector& N, float Range)
{
	const float3 origin = P.getVector3();
	const float3 dir	= N.getVector3();

	const float minT = 1e-2f;
	const float maxT = Range;
	const float RayTime = 0;

	unsigned int Energy = 80;
 	
	// hits
	unsigned int CountHits = 0;
	
	Hit hits[64];
	g_params.rays[TaskID].Hits = hits;

	//// Обновить размер в CUDAContext В pipelineCompileOptions.numPayloadValues (Если менять кол-во Payloads)
	optixTrace(
		g_params.handle,
		origin,
		dir,
		minT, maxT, RayTime,
		OptixVisibilityMask(255),
		OPTIX_RAY_FLAG_ENFORCE_ANYHIT,
		0, 1, 0,
		TaskID, Energy, CountHits
	);

	// Баг был тут поченил

	float EnergyN = float(Energy) / 100.0f;
	
	return EnergyN < 1.0f ? EnergyN : 1.0f;
}

__device__ void CalculatePoint(Hardware_Raytask& Task, Hardware_Lighting& L, unsigned int TaskID)
{
	Hardware_Color& ColorUV = g_params.colors[TaskID];

	Hardware_Vector P(Task.Position);
	Hardware_Vector N(Task.Direction);

	Hardware_Vector Ldir;
	Hardware_Vector Pnew = P;
	Pnew.Mad_Self(N, 0.001f);

	Hardware_Vector LightPosition = { L.position.x, L.position.y, L.position.z };
	Hardware_Vector LightDirection = { L.direction.x, L.direction.y, L.direction.z };

	bool isSunOrHemi = L.light_type != eRGB;
	float att = 0;

	switch (L.type)
	{
	case LT_DIRECT:
	{
		Ldir.Inverted(LightDirection);
		float D = Ldir.DotProduct(N);
		if (D <= 0)
			return;

		float trace = RunOptickTask(TaskID, Pnew, Ldir, 1000.f);
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

		float R = sqrtf(sqD);
		float trace = RunOptickTask(TaskID, Pnew, Ldir, R);
		float scale = D * L.energy * trace;

		if (isSunOrHemi)
		{
			att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
		}
		else
		{
			att = scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
		}
		break;
	}

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

		float R = sqrtf(sqD);
		float trace = RunOptickTask(TaskID, Pnew, Ldir, R);
		att = powf(D, 0.125f) * L.energy * trace * (1.0f - R / L.range);
	}
	break;
	}

	switch (L.light_type)
	{
	case eSun:
	{
		ColorUV.sun += att;
	} break;

	case eHemi:
	{
		ColorUV.hemi += att;
	} break;

	case eRGB:
	{
		Hardware_Vector& rgb = ColorUV.rgb;
		rgb.x += att * L.diffuse.x;
		rgb.y += att * L.diffuse.y;
		rgb.z += att * L.diffuse.z;
	} break;

	}
}

__device__ void LightPoint(int index)
{
	int RayIndex = 0;
	int flags = g_params.flags;
	for (int i = 0; i < g_params.counts_lights; i++)
	{
		Hardware_Lighting& L = g_params.lights[i];

		if ((LP_dont_hemi & flags) && L.light_type == eHemi ||
			(LP_dont_rgb & flags) && L.light_type == eRGB ||
			(LP_dont_sun & flags) && L.light_type == eSun)			continue;

		RayIndex++;
		CalculatePoint(g_params.rays[index], L, index);
	}
}
 
extern "C" __global__ void __raygen__rg()
{
	const uint3 launch_idx = optixGetLaunchIndex();
	LightPoint(launch_idx.x);
}
