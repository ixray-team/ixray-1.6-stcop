#include "StdAfx.h"
#include "compiler_embree.h"

CEmbreeRayTracer CAIRayTrace;



float CEmbreeRayTracer::Raytrace(Fvector& P, Fvector& N, float R, RTCFilterFunctionN funct)
{
	auto SetRay1 = [](RTCRay& ray, Fvector& P, Fvector& N, float R)
	{
		ray.org_x = P.x;
		ray.org_y = P.y;
		ray.org_z = P.z;
		ray.dir_x = N.x;
		ray.dir_y = N.y;
		ray.dir_z = N.z;
		ray.tfar = R;
		ray.tnear = 0.01f;
		ray.mask = (uint32_t)(-1);
		ray.flags = 0;
		ray.time = 0;
		ray.id = 0;
	};

	thread_local RayQueryContext ctxt;
	thread_local RTCOccludedArguments args;
	thread_local bool bInitialized = false;
	thread_local RTCRay ray;

	if (!bInitialized)
	{
		rtcInitRayQueryContext(&ctxt);
		args.context = &ctxt;
 		rtcInitOccludedArguments(&args);
		bInitialized = true;
 	}

	args.filter = funct;
 	args.flags = RTC_RAY_QUERY_FLAG_INVOKE_ARGUMENT_FILTER;
	ctxt.energy = 1;
	
	SetRay1(ray, P, N, R);
 	rtcOccluded1(Scene, &ray, &args);

	return ctxt.energy;
}

const char* GetDeviceConfig();

void CEmbreeRayTracer::LoadGeomBuffer()
{
	Scene = rtcNewScene(EmbreeDevice);
	rtcSetSceneFlags(Scene, scene_flags);
	rtcSetSceneBuildQuality(Scene, scene_quality);

 	Geometry = rtcNewGeometry(EmbreeDevice, RTC_GEOMETRY_TYPE_TRIANGLE);

	rtcSetSharedGeometryBuffer(Geometry, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, static_geom.vertex().data(), 0, sizeof(Fvector), static_geom.vertex().size());
	rtcSetSharedGeometryBuffer(Geometry, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, static_geom.faces().data(), 0, sizeof(Triangle), static_geom.faces().size());
  
 	rtcCommitGeometry(Geometry);
	rtcAttachGeometry(Scene, Geometry);
	rtcReleaseGeometry(Geometry);		// Релизим геометрию зачем нам она ? 

	// Завершаем построение 
	rtcCommitScene(Scene);
}

// Без инстансов и разной геометрии !

void CEmbreeRayTracer::Initialize( )
{
	auto fError = [](void* userPtr, enum RTCError code, const char* str)
	{
		R_ASSERT2(false, str);
	};
	// Initialize Scene
	EmbreeDevice = rtcNewDevice(GetDeviceConfig());
	rtcSetDeviceErrorFunction(EmbreeDevice, fError, nullptr);

	// Setup Container
	R_ASSERT(static_geom.faces().size() > 2);

	// RTC Scene 
  	LoadGeomBuffer();
}

void CEmbreeRayTracer::Deinitialize()
{
	if (Scene != nullptr)
	{
		rtcReleaseScene(Scene);
		Scene = nullptr;
	}

	rtcReleaseDevice(EmbreeDevice);
	EmbreeDevice = nullptr;

	static_geom.ClearAll();
}

