#pragma once
#include "../xrLC_Light/embree_raytracing/EmbreeGeometryBuilder.h"

#include <embree4/rtcore.h>

struct RayQueryContext : RTCRayQueryContext
{
	void* skip   = nullptr;
	float energy = 1.0f;
};

class CEmbreeRayTracer
{
private:
	RTCDevice EmbreeDevice = nullptr;
	RTCScene    Scene				= nullptr;		// Main Scene Raytrace !
	RTCGeometry Geometry			= nullptr;		// Main Scene Raytrace !

	RTCSceneFlags scene_flags		= RTC_SCENE_FLAG_NONE;
	RTCBuildQuality scene_quality	= RTC_BUILD_QUALITY_HIGH;
 
	/** NORMAL GEOM **/
 	void LoadGeomBuffer();

public:
	TriangleContainer static_geom;

	void Initialize();
	void Deinitialize();

	float Raytrace(Fvector& P, Fvector& N, float R, RTCFilterFunctionN filter);
};

extern CEmbreeRayTracer CAIRayTrace;