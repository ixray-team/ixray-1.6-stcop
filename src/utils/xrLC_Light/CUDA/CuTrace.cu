#include <cuda_runtime.h>
#include <optix.h>
#include <optix_device.h>

struct RayHitResult
{
	float t;
	int faceId;
};

struct Params
{
	OptixTraversableHandle handle;
	float3 rayOrigin;
	float3 rayDir;
	float rayMaxT;
	RayHitResult* result;
};

extern "C"
{
	__constant__ Params g_params;
}

extern "C" __global__ void __raygen__rg()
{
	const float3 origin = g_params.rayOrigin;
	const float3 dir = g_params.rayDir;
	const float maxT = g_params.rayMaxT;

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

	RayHitResult& out = *g_params.result;
	out.t = __int_as_float(p0);
	out.faceId = p1;
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