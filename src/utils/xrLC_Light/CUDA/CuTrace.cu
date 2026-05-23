#include <cuda_runtime.h>
#include <optix.h>
#include <optix_device.h>

#include "IntelliSense.cuh"
#include "Vector3HW.cuh"

extern "C"
{
	__constant__ OPTICK_Params g_params;
}

enum Flags
{
	LP_Default = 0,
 	LP_dont_rgb			= (1 << 1),
	LP_dont_hemi		= (1 << 2),
	LP_dont_sun			= (1 << 3),
};
 
#define LT_DIRECT		0
#define LT_POINT		1
#define LT_SECONDARY	2
#define ENERGY_MIN		0.01f

#include "optix_types.h"

// Entry points
__device__ float calculate_energy(Hardware_FaceData& F)
{
	Hardware_TextureData& T = g_params.textures[F.surfidx];
	if (!T.pSurface)
		return 1.0f;

	// barycentrics
	const float2 bc = optixGetTriangleBarycentrics();
	float hitU = bc.x;
	float hitV = bc.y;
	float b0 = 1.0f - hitU - hitV;

	float2 TC0 = __half22float2(F.TC0[0]);
	float2 TC1 = __half22float2(F.TC0[1]);
	float2 TC2 = __half22float2(F.TC0[2]);

	float u = TC0.x * b0 + TC1.x * hitU + TC2.x * hitV;
	float v = TC0.y * b0 + TC1.y * hitU + TC2.y * hitV;

	int U = (int)floor(u * T.width + 0.5f);
	int V = (int)floor(v * T.height + 0.5f);

	U = (U % T.width + T.width) % T.width;
	V = (V % T.height + T.height) % T.height;
 
	float a = T.pSurface[V * T.width + U] / 255.0f;
  	return (a * a);
}




extern "C" __global__ void __anyhit__ah()
{
	const int primID	  = optixGetPrimitiveIndex();
 	Hardware_FaceData& F = g_params.faces[primID];
	if (F.bOpacue)
	{
		optixSetPayload_0(0); // visibility = 0
		optixTerminateRay();
		return;
	}

	// ❗ Embree-equivalent rule: binary cut
	if (calculate_energy(F) < 0.5f)
	{
		optixIgnoreIntersection(); // transparent → continue
		return;
	}
 
	// ❗ hit blocks ray
	optixSetPayload_0(0);
 	optixTerminateRay();

	/*
	
	const int primID = optixGetPrimitiveIndex();

	Hardware_FaceData& F = g_params.faces[primID];
	Hardware_TextureData& T = g_params.textures[F.surfidx];

	if (F.bOpacue || T.pSurface == nullptr)
	{
		// Не имеюь прозрачности  → останавливаемся
		optixSetPayload_1(0);
		optixTerminateRay();
		return;
	}

	unsigned int energy_int = optixGetPayload_1();
 	float energy = float(energy_int) / 10000.0f;
	// energy attenuation (LUT)
	energy *= ( 1.0f - calculate_energy(F) );				// Проверка тут на воду не понятно почему делает ее темной

	// opaque → остановить
	if (energy < ENERGY_MIN)
	{
		optixSetPayload_1(0);
		optixTerminateRay();
		return; // closesthit будет вызван
	}

	// transparent → пропускаем и летим дальше
	// Тут тоже сделал явное преобразование
	unsigned int EnergyReturn = float(energy * 10000.0f);
	optixSetPayload_1(EnergyReturn);
	optixIgnoreIntersection();
	*/
}


extern "C" __global__ void __miss__ms()
{
	// если ничего не встретили -> свет проходит
	optixSetPayload_0(1);
}

extern "C" __global__ void __closesthit__ch()
{
	optixSetPayload_0(0);
}

__device__ float RunOptickTask(Hardware_Vector& P, Hardware_Vector& N, float maxT)
{
	unsigned int visibility = 1;
	unsigned int Energy = 10000;
	const float3 origin = P.getVector3();
	const float3 dir = N.getVector3();

	optixTrace(
		g_params.handle,
		origin, dir,
		0.001f,
		maxT,
		0.0f,
		OptixVisibilityMask(255),
		OPTIX_RAY_FLAG_DISABLE_CLOSESTHIT |
		OPTIX_RAY_FLAG_ENFORCE_ANYHIT,
		0, 1, 0,
		visibility, Energy
	);

	return visibility ? 1.0f : 0.0f;
	// visibility ? 1.0f : 0.0f;
	
	// Баг был тут поченил
	// float EnergyN = float(Energy) / 10000.0f;
	// return EnergyN < 1.0f ? EnergyN : 1.0f;
}

__device__ void CalculatePoint(Hardware_Raytask& Task, Hardware_Lighting& L, unsigned int TaskID)
{
	Hardware_Color& ColorUV = g_params.colors[TaskID];

	Hardware_Vector P(Task.Position);
	Hardware_Vector N(Task.Direction);

	Hardware_Vector Ldir;
	Hardware_Vector Pnew = P;
	Pnew.Mad_Self(N, 0.01f);

	Hardware_Vector LightPosition  = { L.position.x, L.position.y, L.position.z };
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
		} break;

		case LT_POINT:
		{
			float sqD = P.DistanceSquared(LightPosition);
			if (sqD > L.range2) return;

			Ldir.Subtract(LightPosition, P).Normalize_Safe();
			float D = Ldir.DotProduct(N);
			if (D <= 0)			return;

			float R		= sqrtf(sqD);
			float trace = RunOptickTask(Pnew, Ldir, R);
			float scale = D * L.energy * trace;

			if (isSunOrHemi)
			{
				att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
			}
			else
			{
				att = scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
			}
 		} break;

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
		} break;

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
	unsigned char flags = g_params.flags;
	for (int i = 0; i < g_params.counts_lights; i++)
	{
		Hardware_Lighting& L = g_params.lights[i];

		if (((LP_dont_hemi & flags) && L.light_type == eHemi) ||
			((LP_dont_rgb & flags) && L.light_type == eRGB) ||
			((LP_dont_sun & flags) && L.light_type == eSun))
		{
			continue;
		}

 		CalculatePoint(g_params.rays[index], L, index);
	}
}
 
extern "C" __global__ void __raygen__rg()
{
	const uint3 launch_idx = optixGetLaunchIndex();
	LightPoint(launch_idx.x);
}