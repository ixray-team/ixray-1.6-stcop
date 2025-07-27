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

struct OPTICK_Params
{
	OptixTraversableHandle handle;

	unsigned char	 flags;
	hardware_raytask* rays;
	hardware_color* colors;
	hardware_lighting* lights;
	int counts_lights;
};

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

__device__ float RunOptickTask(HardwareVector& P, HardwareVector& N, float Range)
{
	const float3 origin = P.getVector3();
	const float3 dir = N.getVector3();
	const float maxT = Range;

	unsigned int hit = 0;

	optixTrace(
		g_params.handle,
		origin,
		dir,
		0.0f, maxT, 0.0f,
		OptixVisibilityMask(255),
		OPTIX_RAY_FLAG_DISABLE_ANYHIT,
		0, 1, 0,
		hit
	);

	return (hit == 0) ? 1.0f : 0.0f;
}

__device__ void CalculatePoint(hardware_lighting& L, HardwareVector& P, HardwareVector& N, hardware_color& C)
{
	HardwareVector Ldir;
	HardwareVector Pnew = P;
	Pnew.Mad_Self(N, 0.01f);

	HardwareVector LightPosition = { L.position.x, L.position.y, L.position.z };
	HardwareVector LightDirection = { L.direction.x, L.direction.y, L.direction.z };

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
		C.rgb.x += att * L.diffuse.x;
		C.rgb.y += att * L.diffuse.y;
		C.rgb.z += att * L.diffuse.z;
		break;
	}
}

enum Flags
{
	LP_Default   =  0,
	LP_dont_rgb  = (1 << 1),
	LP_dont_hemi = (1 << 2),
	LP_dont_sun  = (1 << 3),
};

__device__ void run_tracing_new(int index)
{
	unsigned char flags = g_params.flags;

	hardware_raytask& Rays = g_params.rays[index];
	HardwareVector P(Rays.Position);
	HardwareVector N(Rays.Direction);

	hardware_color& ColorUV = g_params.colors[index];

 
	for (int i = 0; i < g_params.counts_lights; i++)
	{
		hardware_lighting& L = g_params.lights[i];

		if (!(LP_dont_hemi & flags) && L.type == eHemi)
 			CalculatePoint(L, P, N, ColorUV);
 
		if (!(LP_dont_rgb & flags) && L.type == eRGB)
			CalculatePoint(L, P, N, ColorUV);

		if (!(LP_dont_sun & flags) && L.type == eSun)
			CalculatePoint(L, P, N, ColorUV);
	}
 
}

// Entry points

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
}
