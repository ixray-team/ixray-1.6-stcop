#pragma once

#include "../R_light.h"
#include "../base_lighting.h"
#include "../xrFace.h"

//#include "Vector3HW.h"          // se7kills: HW Data Structure
#include <optix.h>
#include <optix_stubs.h>
#include <cuda_runtime.h>

struct RayRecvestIndex;

struct OptixMeshBuffers
{
    // Blas Model
    CUdeviceptr blasBuffer = 0;
    OptixTraversableHandle blasHandle = 0;

    // Tlas Model
    CUdeviceptr tlasBuffer = 0;
    OptixTraversableHandle tlasHandle = 0;
};

namespace XRay::RayTrace::CUDA
{
    // GPU
    void InitializeGPU();

    // Отдельный инициализвтор
    void InitializeLights();

    // Загрузить Faces
    void InitializeFaces(xr_vector<void*>& Faces);

    // Загрузить Альфу Текстур
    void InitializeTexturesAlpha();

    // Builder Scene
	bool BuildSceneFromLCGlobalData(OptixDeviceContext context, OptixMeshBuffers& outScene);

    // RayTracing
    void InitializeModel();
    void UnloadingModel();
 
    // Ray Trace Call
    void RayTraceInitialize(u8 CurrentFlags, size_t MaxRays);

    void RayTraceAddRay(RayRecvestIndex& ray, size_t index);
    void RayTraceRun();
    void RayTraceUnload();

    xr_vector<base_color_c>& RayTraceResult();
}
