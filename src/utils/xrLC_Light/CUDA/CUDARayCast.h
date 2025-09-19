#pragma once

#include "../R_Light.h"
#include "../base_lighting.h"
#include "../xrFace.h"

#include <optix.h>
#include <optix_stubs.h>
#include <cuda_runtime.h>

class PackedLighting;
  
struct RayRecvestIndex;

namespace XRay::RayTrace::CUDA
{
    struct OptixMeshBuffers
    {
        CUdeviceptr vertexBuffer = 0;
        CUdeviceptr indexBuffer = 0;
        CUdeviceptr blasBuffer = 0;
        OptixTraversableHandle blasHandle = 0;
        CUdeviceptr tlasBuffer = 0;
        OptixTraversableHandle tlasHandle = 0;
    };


    struct TextureData
    {
        u32 width;
        u32 height;
        u32* pSurface; // Указатель на GPU память
        bool hasAlpha;

        // Для CUDA texture objects
        cudaTextureObject_t texObj;
    };

    struct FaceData
    {
        int dwMaterial;
        u32 flags;
        Fvector2 tc0[3]; // UV координаты
    };

    struct MaterialData
    {
        int surfidx; // Индекс текстуры
    };
     
    // Builder Scene
	bool BuildSceneFromLCGlobalData(OptixDeviceContext context, CUstream stream, OptixMeshBuffers& outScene);

    // Textures (not used now)
    void InitializeTextures(xr_vector<TextureData>& gpuTextures, cudaTextureObject_t*& d_texObjects);

    // RayTracing
    void InitializeRayTracing();
 
    // Ray Trace Call
    // void RayTracePackNew(RayRecvestIndex* tasks, base_color_c* colors, u32 TaskPoolSize, u8 current_flags);

    void RayTraceInitialize(base_lighting& L, u8 CurrentFlags);

    void RayTraceAddRay(RayRecvestIndex& ray);
    void RayTraceRun();

    xr_vector<base_color_c>& RayTraceResult();
}
 