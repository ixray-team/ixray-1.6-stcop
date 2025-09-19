#pragma once

#include "../R_Light.h"
#include "../base_lighting.h"
#include "../xrFace.h"

#include <optix.h>
#include <optix_stubs.h>
#include <cuda_runtime.h>

struct RayRequest;
class PackedLighting;
 

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



	bool BuildSceneFromLCGlobalData(OptixDeviceContext context, CUstream stream, OptixMeshBuffers& outScene);
    void InitializeRayTracing();
    float RayTraceWrapper
    (
        OptixTraversableHandle handle,
        R_Light& L, Fvector& P, Fvector& D,
        float R, Face* skip, const FaceData* d_faces, const MaterialData* d_materials, const TextureData* d_textures
    );
    void InitializeTextures(xr_vector<TextureData>& gpuTextures, cudaTextureObject_t*& d_texObjects);
 
    void RayTracePackNew(PackedLighting& data_gpu, base_lighting& L);
}