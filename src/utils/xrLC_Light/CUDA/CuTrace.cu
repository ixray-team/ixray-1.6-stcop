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
 
struct ColorsRessult
{
	hardware_color Color;
	int		       Configured;
	int			   RealProcessed;
};

struct OPTICK_Params
{
	OptixTraversableHandle handle;

	ColorsRessult*		colors;
	hardware_lighting*  lights;
	int					counts_lights;

	float3 rayOrigin;
	float3 rayDir;
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
	eSun  = 0,
	eHemi = 1,
	eRGB  = 2
};

__device__ void RunOptickTask(HardwareVector& P, HardwareVector& N, float Range, float& ret)
{
	const float3 origin = P.getVector3();
	const float3 dir    = N.getVector3();
	const float maxT    = Range;

	unsigned int distance, faceID;
	optixTrace(
		g_params.handle,
		origin,
		dir,
		0.0f, maxT, 0.0f,           // minT, maxT, time
		OptixVisibilityMask(255),
		OPTIX_RAY_FLAG_NONE,
		0, 1, 0,
		distance, faceID
	);

	if (distance > 0)
		ret = 1;
	else
		ret = 0;
}
 

__device__ void CalculatePoint(hardware_lighting& L, HardwareVector& P, HardwareVector& N, hardware_color& C, int& RealProcessed)
{
	HardwareVector Ldir;
	HardwareVector Pnew = P;
	Pnew.Mad_Self(N, 0.01f);

	HardwareVector LightPosition(L.position);
	HardwareVector LightDirection(L.direction);
	HardwareVector LightDiffuse(L.diffuse);

	bool isSunOrHemi = L.light_type != eType::eRGB;
	float att = 0;
	switch (L.type)
	{
		case LT_DIRECT:
		{
			Ldir.Inverted(LightDirection);
			float D = Ldir.DotProduct(N);
			if (D <= 0)
				return;

			float trace = 1;
			RunOptickTask(Pnew, Ldir, 1000.f, trace); // todo skip
			att = isSunOrHemi ? L.energy * trace : D * L.energy * trace;
			RealProcessed++;
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
			float trace = 1;
			RunOptickTask(Pnew, Ldir, R, trace); // todo skip
			float scale = D * L.energy * trace;
			RealProcessed++;

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
			float trace = 1;
			RunOptickTask(Pnew, Ldir, R, trace); // todo skip
			RealProcessed++;

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
 
__device__ void run_tracing_new(int index)
{
	//ColorsRessult& Result = g_params.colors[index];
	HardwareVector P(g_params.rayOrigin);
	HardwareVector N(g_params.rayDir);

	ColorsRessult& Result = *g_params.colors;
	// RGB Lights 
	Result.RealProcessed = 0;

	for (auto i = 0; i < g_params.counts_lights; i++)
	{
		hardware_lighting& Light = g_params.lights[i];
		CalculatePoint(Light, P, N, Result.Color, Result.RealProcessed);
	}

	Result.Configured = 123;
}

// Callers
extern "C" __global__ void __raygen__rg()
{
 	// const float3 origin = g_params.rayOrigin;
	// const float3 dir    = g_params.rayDir;
 	// 
	// float maxT = 1000.0f;
	// unsigned int distance, faceID;
	// optixTrace(
	// 	g_params.handle,
	// 	origin,
	// 	dir,
	// 	0.0f, maxT, 0.0f,           // minT, maxT, time
	// 	OptixVisibilityMask(255),
	// 	OPTIX_RAY_FLAG_NONE,
	// 	0, 1, 0,
	// 	distance, faceID
	// );
	
	const uint3 launch_idx = optixGetLaunchIndex();
 	run_tracing_new (launch_idx.x);
}

extern "C" __global__ void __miss__ms()
{
	// Ничего не делаем
}

extern "C" __global__ void  __closesthit__ch()
{
}

extern "C" __global__ void  __anyhit__ah()
{
}


/*
	const float3 origin = make_float3(g_params.rayOrigin.x, g_params.rayOrigin.y, g_params.rayOrigin.z);
	const float3 dir = make_float3(g_params.rayDir.x, g_params.rayDir.y, g_params.rayDir.z);
	const float maxT = 1000;

	unsigned int p0, p1;
	optixTrace(
		g_params.handle,
		origin,
		dir,
		0.0f, maxT, 0.0f,           // minT, maxT, time
		OptixVisibilityMask(255),
		OPTIX_RAY_FLAG_NONE,
		0, 1, 0,
		p0, p1
	);
*/