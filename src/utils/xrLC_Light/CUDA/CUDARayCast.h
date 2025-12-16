#pragma once

#include "../R_light.h"
#include "../base_lighting.h"
#include "../xrFace.h"

#include "Vector3HW.h"          // se7kills: HW Data Structure
#include <optix.h>
#include <optix_stubs.h>
#include <cuda_runtime.h>

class PackedLighting;
  
struct RayRecvestIndex;
struct TextureData;
 

namespace XRay::RayTrace::CUDA
{
    // Builder Scene
	bool BuildSceneFromLCGlobalData(OptixDeviceContext context, CUstream stream, OptixMeshBuffers& outScene);

    // Textures (not used now)
    void InitializeTextures(xr_vector<TextureData>& gpuTextures, cudaTextureObject_t*& d_texObjects);

    // RayTracing
    void InitializeRayTracing();
 
    // Ray Trace Call
    void RayTraceInitialize(base_lighting& L, u8 CurrentFlags);

    void RayTraceAddRay(RayRecvestIndex& ray, size_t index);
    void RayTraceRun(size_t max_rays);

    xr_vector<base_color_c>& RayTraceResult();
}
 