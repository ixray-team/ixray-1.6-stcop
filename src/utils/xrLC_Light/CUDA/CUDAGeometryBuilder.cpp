#include "stdafx.h"
#include "CUDAGeometryBuilder.h"
#include "../../xrLC/Build.h"

#include "Vector3HW.h"
 
bool OptixGeometryBuilder::BuildBLAS(OptixDeviceContext context, OptixMeshBuffers& outBuffers, CUstream stream)
{
    if (vertices.empty() || triangles.empty()) return false;
 
    // 0. Временные буферы для построения
    CUdeviceptr  d_tempBuffer;
    CUdeviceptr  d_tmp_vertexBuffer;
    CUdeviceptr  d_tmp_indexBuffer;

    // 1. Загружаем вершины на GPU
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_tmp_vertexBuffer),
        sizeof(Fvector) * vertices.size()));
    CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(d_tmp_vertexBuffer),
        vertices.data(),
        sizeof(Fvector) * vertices.size(),
        cudaMemcpyHostToDevice));
  
    // 2. Загружаем индексы на GPU
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_tmp_indexBuffer),
        sizeof(CDB::TRI) * triangles.size()));
    CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(d_tmp_indexBuffer),
        triangles.data(),
        sizeof(CDB::TRI) * triangles.size(),
        cudaMemcpyHostToDevice));

    // 3. Настройка входных данных для BLAS
    OptixBuildInput buildInput = {};
    buildInput.type                                 = OPTIX_BUILD_INPUT_TYPE_TRIANGLES;

    buildInput.triangleArray.vertexFormat           = OPTIX_VERTEX_FORMAT_FLOAT3;
    buildInput.triangleArray.vertexStrideInBytes    = sizeof(Fvector);
    buildInput.triangleArray.numVertices            = static_cast<uint32_t>(vertices.size());
    buildInput.triangleArray.vertexBuffers          = &d_tmp_vertexBuffer;

    buildInput.triangleArray.indexFormat            = OPTIX_INDICES_FORMAT_UNSIGNED_INT3;
    buildInput.triangleArray.indexStrideInBytes     = sizeof(CDB::TRI);
    buildInput.triangleArray.numIndexTriplets       = static_cast<uint32_t>(triangles.size());
    buildInput.triangleArray.indexBuffer            = d_tmp_indexBuffer;

    static uint32_t flags                           = OPTIX_GEOMETRY_FLAG_NONE; 
    buildInput.triangleArray.flags = &flags;
    buildInput.triangleArray.numSbtRecords = 1;

    // 4. Настройка параметров сборки
    OptixAccelBuildOptions accelOptions = {};
    accelOptions.operation           = OPTIX_BUILD_OPERATION_BUILD;
    accelOptions.buildFlags          = OPTIX_BUILD_FLAG_ALLOW_COMPACTION | OPTIX_BUILD_FLAG_PREFER_FAST_TRACE;

    // 5. Вычисление требуемой памяти
    OptixAccelBufferSizes bufferSizes;
    OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &accelOptions, &buildInput, 1, &bufferSizes));
     
    clMsg("BLAS : ( Temp : %u mb | update size: %u mb | output: %u mb ) ", 
        bufferSizes.tempSizeInBytes / 1024 / 1024, 
        bufferSizes.tempUpdateSizeInBytes / 1024 / 1024,
        bufferSizes.outputSizeInBytes / 1024 / 1024 
     );


    // 6. Выделение памяти
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_tempBuffer), bufferSizes.tempSizeInBytes));
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outBuffers.blasBuffer), bufferSizes.outputSizeInBytes));


    // 7. Готовим дескриптор для запроса размера компактации
    OptixAccelEmitDesc emitDesc = {};
    CUdeviceptr d_compactedSize;
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_compactedSize), sizeof(uint64_t)));
    emitDesc.type = OPTIX_PROPERTY_TYPE_COMPACTED_SIZE;
    emitDesc.result = d_compactedSize;

    // 8. Сборка BLAS
    OPTIX_CHECK(  optixAccelBuild
    (
        context,
        stream, // CUDA stream
        &accelOptions,
        &buildInput,
        1,
        d_tempBuffer,
        bufferSizes.tempSizeInBytes,
        outBuffers.blasBuffer,
        bufferSizes.outputSizeInBytes,
        &outBuffers.blasHandle,
        &emitDesc, 1
    ));

    CUDA_CHECK(cudaStreamSynchronize(stream));

    // 9. Узнаём размер скомпактированной структуры
    uint64_t compactedSize = 0;
    CUDA_CHECK(cudaMemcpy(&compactedSize, reinterpret_cast<void*>(d_compactedSize), sizeof(uint64_t), cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaFree(reinterpret_cast<void*>(d_compactedSize)));
    
    // 10. Компактация, если это выгодно
    size_t size_precompact = bufferSizes.outputSizeInBytes;
    if (compactedSize != 0 && compactedSize < bufferSizes.outputSizeInBytes)
    {
        CUdeviceptr d_compactedBuffer;
        CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_compactedBuffer), compactedSize));
    
        OptixTraversableHandle compactedHandle;
        OPTIX_CHECK(optixAccelCompact(
            context,
            stream, // stream
            outBuffers.blasHandle,
            d_compactedBuffer,
            compactedSize,
            &compactedHandle
        ));

        CUDA_CHECK(cudaStreamSynchronize(stream));

        // Освобождаем старый буфер
        CUDA_CHECK(cudaFree(reinterpret_cast<void*>(outBuffers.blasBuffer)));
    
        // Сохраняем компактный
        outBuffers.blasBuffer = d_compactedBuffer;
        outBuffers.blasHandle = compactedHandle;
    }
 
    // 11. Освобождаем временный буфер
    CUDA_CHECK( cudaFree(reinterpret_cast<void*>(d_tempBuffer)));
    CUDA_CHECK( cudaFree(reinterpret_cast<void*>(d_tmp_vertexBuffer) ));
    CUDA_CHECK( cudaFree(reinterpret_cast<void*>(d_tmp_indexBuffer) ));
     
    return true;
}

bool OptixGeometryBuilder::BuildTLAS(OptixDeviceContext context, OptixMeshBuffers& outScene, CUstream stream)
{
    if (outScene.blasHandle == 0) {
        Msg("! ERROR: Invalid BLAS handle");
        return false;
    }

    // 1. Строим TLAS (один экземпляр BLAS)
    OptixInstance instance = {};
    float transform[12] = {
        1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f
    };

    memcpy(instance.transform, transform, sizeof(transform));
    instance.instanceId = 0;
    instance.sbtOffset = 0;
    instance.visibilityMask = 255;
    instance.flags = OPTIX_INSTANCE_FLAG_NONE;
    instance.traversableHandle = outScene.blasHandle;

    // 2. Алокация под GPU
    CUdeviceptr d_instances;
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_instances), sizeof(OptixInstance)));
    CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(d_instances), &instance, sizeof(OptixInstance), cudaMemcpyHostToDevice));
 
    // 3. Входные данные для структуры 
    OptixBuildInput buildInput = {};
    buildInput.type = OPTIX_BUILD_INPUT_TYPE_INSTANCES;
    buildInput.instanceArray.instances = d_instances;
    buildInput.instanceArray.numInstances = 1;


    // 4. Настройка параметров сборки
    OptixAccelBuildOptions buildOptions = {};
    buildOptions.buildFlags = OPTIX_BUILD_FLAG_PREFER_FAST_TRACE; // OPTIX_BUILD_FLAG_PREFER_FAST_TRACE
    buildOptions.operation  = OPTIX_BUILD_OPERATION_BUILD;

    // 5. Вычисление требуемой памяти
    OptixAccelBufferSizes bufferSizes;
    OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &buildOptions, &buildInput, 1, &bufferSizes));

    // 6. Выделение памяти
    CUdeviceptr d_tempBuffer;
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_tempBuffer), bufferSizes.tempSizeInBytes));
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outScene.tlasBuffer), bufferSizes.outputSizeInBytes));
 
    OPTIX_CHECK(optixAccelBuild(
        context,
        stream,
        &buildOptions,
        &buildInput,
        1,
        d_tempBuffer,
        bufferSizes.tempSizeInBytes,
        outScene.tlasBuffer,
        bufferSizes.outputSizeInBytes,
        &outScene.tlasHandle,
        nullptr, 0
     ));

    CUDA_CHECK(cudaStreamSynchronize(stream));

    CUDA_CHECK(cudaFree(reinterpret_cast<void*>(d_tempBuffer)));
    CUDA_CHECK(cudaFree(reinterpret_cast<void*>(d_instances)));

    return true;
}
 
// Scene Global Data
#include "../xrLC_GlobalData.h"
#include "../xrMU_Model_Reference.h"
#include <embree_raytracing/EmbreeRayTrace.h>
#include "xrDeflectorLight_Packed.h"

struct FaceDataEmbree;

size_t GetMemory();
 
bool XRay::RayTrace::CUDA::BuildSceneFromLCGlobalData(OptixDeviceContext context, CUstream stream, OptixMeshBuffers& outScene)
{
    xrLC_GlobalData* globalData = lc_global_data();
    if (!globalData)        return false;


    OptixGeometryBuilder geometryBuilder;
    
    size_t StartMemory = GetMemory();
    // 1. Обрабатываем статическую геометрию
    for (Face* F : globalData->g_faces())
    {
        const Shader_xrLC& SH = F->Shader();
        if (!SH.flags.bLIGHT_CastShadow) { continue; }

        u16 surfaceID = globalData->materials()[F->dwMaterial].surfidx;
        b_texture& T = globalData->textures()[surfaceID];

        bool isTransparent  = (!T.pSurface.Empty() && T.bHasAlpha);
        F->flags.bOpaque    = !isTransparent;
        geometryBuilder.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
    }

    // 2. Обрабатываем MU-референсы
    for (auto ref : globalData->mu_refs())
    {
        xr_vector<FaceDataEmbree> tempBuffer;
        ref->export_cform_rcast_new(tempBuffer);

        for (auto& pF : tempBuffer)
        {
            Face* F = (Face*)pF.ptr;
            b_material& M = globalData->materials()[F->dwMaterial];
            b_texture& T = globalData->textures()[M.surfidx];
 
            bool isTransparent  = (!T.pSurface.Empty() && T.bHasAlpha);
            F->flags.bOpaque    = isTransparent;
            geometryBuilder.AddFace(F, pF.v1, pF.v2, pF.v3);
        }
    }
  
    size_t pVertex = geometryBuilder.RawFacesSize() * 3;
    size_t pFaces  = geometryBuilder.RawFacesSize();
    geometryBuilder.RemoveDublicates_Batched();  
    geometryBuilder.RemoveDublicateFaces();

    Msg("$[GPU Accel Structure] Remove Dublicate Vert : %llu to %llu", pVertex, geometryBuilder.vertices.size());
    Msg("$[GPU Accel Structure] Remove Dublicate Face : %llu to %llu", pFaces, geometryBuilder.triangles.size());

    Msg("*[GPU Accel Structure] MU-Faces Memory: %u mb", u32( (GetMemory() - StartMemory) / 1024 / 1024));
  
    StartMemory = GetMemory();

    // 3. Строим BLAS
    if (!geometryBuilder.BuildBLAS(context, outScene, stream))          return false;
  
     // 4. Строим TLAS
    if (!geometryBuilder.BuildTLAS(context, outScene, stream))          return false;
    Msg("*[GPU Accel Structure] Cpu (GPU Used) Memory: %u mb", u32( (GetMemory() - StartMemory) / 1024 / 1024));

    StartMemory = GetMemory();
    // 5: Face Pointers Loading to GPU
    XRay::RayTrace::CUDA::InitializeFaces(geometryBuilder.facePointers);
    Msg("*[GPU Accel Structure] GPU FACES COPY Memory: %u mb", u32( (GetMemory() - StartMemory) / 1024 / 1024));
 

    geometryBuilder.Clear();
    geometryBuilder.MemoryDealoc();

    return true;
}

