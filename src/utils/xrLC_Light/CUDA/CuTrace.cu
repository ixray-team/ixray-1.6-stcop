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

#define LT_DIRECT		0
#define LT_POINT		1
#define LT_SECONDARY	2

enum eType
{
	eSun = 0,
	eHemi = 1,
	eRGB = 2
};

__device__ float RunOptickTask(Hardware_Vector& P, Hardware_Vector& N, float Range)
{
	const float3 origin = P.getVector3();
	const float3 dir = N.getVector3();
	const float maxT = Range;

	unsigned int hit = 0;
	unsigned int Energy = 100;
	unsigned int HitsCollected = 0;

	//// Обновить размер в CUDAContext В pipelineCompileOptions.numPayloadValues (Если менять кол-во Payloads)
 	optixTrace(
		g_params.handle,
		origin,
		dir,
		0.0f, maxT, 0.0f,
		OptixVisibilityMask(255),
		OPTIX_RAY_FLAG_NONE,
		//OPTIX_RAY_FLAG_DISABLE_ANYHIT,
		0, 1, 0,
		hit, Energy, HitsCollected
	);

	return float (Energy / 100) ; // (hit == 0) ? 1.0f : 0.0f;
}

__device__ void CalculatePoint(Hardware_Lighting& L, Hardware_Vector& P, Hardware_Vector& N, Hardware_Color& C)
{
	Hardware_Vector Ldir;
	Hardware_Vector Pnew = P;
	Pnew.Mad_Self(N, 0.01f);

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

		float trace = RunOptickTask(Pnew, Ldir, 1000.f);
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
		float trace = RunOptickTask(Pnew, Ldir, R);
		float scale = D * L.energy * trace;

		if (isSunOrHemi)
		{
			att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
		}
		else
		{
			att = scale * (1.0f / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
		}
	}
	break;

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
		float trace = RunOptickTask(Pnew, Ldir, R);
		att = powf(D, 0.125f) * L.energy * trace * (1.0f - R / L.range);
	}
	break;
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
		{
			Hardware_Vector& rgb = C.rgb;
			rgb.x += att * L.diffuse.x;
			rgb.y += att * L.diffuse.y;
			rgb.z += att * L.diffuse.z;
  		} break;

	}
}

enum Flags
{
	LP_Default   =  0,
	LP_dont_rgb  = (1 << 1),
	LP_dont_hemi = (1 << 2),
	LP_dont_sun  = (1 << 3),
};

__device__ void LightPoint(Hardware_Vector& P, Hardware_Vector& N, Hardware_Color& ColorUV, unsigned char flags)
{
 	for (int i = 0; i < g_params.counts_lights; i++)
	{
		Hardware_Lighting& L = g_params.lights[i];

		if (!(LP_dont_hemi & flags) && L.type == eHemi)
			CalculatePoint(L, P, N, ColorUV);

		if (!(LP_dont_rgb & flags) && L.type == eRGB)
			CalculatePoint(L, P, N, ColorUV);

		if (!(LP_dont_sun & flags) && L.type == eSun)
			CalculatePoint(L, P, N, ColorUV);
	}
}

__device__ void run_tracing_new(int index)
{
	unsigned char flags = g_params.flags;

	Hardware_Raytask& Rays = g_params.rays[index];
	Hardware_Vector P(Rays.Position);
	Hardware_Vector N(Rays.Direction);

	Hardware_Color& ColorUV = g_params.colors[index];
	 
	LightPoint(P, N, ColorUV, flags);
}
 
#include "optix_types.h"

// Entry points
#define ENERGY_MIN 0.01f

extern "C" __global__ void __raygen__rg()
{
	const uint3 launch_idx = optixGetLaunchIndex();
	run_tracing_new(launch_idx.x);
}

extern "C" __global__ void __miss__ms()
{
	optixSetPayload_0(0);
}

extern "C" __global__ void __closesthit__ch()
{
	optixSetPayload_0(1);
}

extern "C" __global__ void __anyhit__ah()
{
	// Not used
	unsigned int hit				= optixGetPayload_0();
	unsigned int energy_int			= optixGetPayload_1();
	unsigned int hits_collected		= optixGetPayload_2();

	float energy   = energy_int / 100;

	// energy attenuation (LUT)
 	energy			*= 0.8f;
	hits_collected  += 1;

	// if (hits_collected > 2)
	// {
	// 	optixSetPayload_1(int(energy * 100));
	// 	return;
	// }
	
	// opaque → остановить
	if (energy < ENERGY_MIN)
	{
	 	optixSetPayload_1(0);
		return; // closesthit будет вызван
	}
	 
	// // transparent → пропускаем и летим дальше
	optixSetPayload_1( int(energy * 100) );
	optixSetPayload_2(hits_collected);

	optixIgnoreIntersection();
}
 
// se7kills TODO:  CDeflectorGPU Релизацию сделать ! (Полная копия до 2048 за раз)
// (На гпу расщитываем все даже for ( auto K : UVTri ) { if ( K.isInsize() ) { light_point() } } )

// Добавил VertexGPU, UVTriGPU, _TCF_GPU Для копирования в GPU
