#pragma once

#include "../R_light.h"
#include "../base_lighting.h"
#include "../xrFace.h"

#include "Vector3HW.h"          // se7kills: HW Data Structure
#include <optix.h>
#include <optix_stubs.h>
#include <cuda_runtime.h>

struct RayRecvestIndex;

namespace XRay::RayTrace::CUDA
{
    // Отдельный инициализвтор
    void InitializeLights();

    // Загрузить Faces
    void InitializeFaces(xr_vector<Face*> Faces);

    // Загрузить Альфу Текстур
    void InitializeTexturesAlpha();

    // Builder Scene
	bool BuildSceneFromLCGlobalData(OptixDeviceContext context, CUstream stream, OptixMeshBuffers& outScene);

    // RayTracing
    void InitializeRayTracing();
 
    // Ray Trace Call
    void RayTraceInitialize(base_lighting& L, u8 CurrentFlags);

    void RayTraceAddRay(RayRecvestIndex& ray, size_t index);
    void RayTraceRun(size_t max_rays);

    xr_vector<base_color_c>& RayTraceResult();
}
