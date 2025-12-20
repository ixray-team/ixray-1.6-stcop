#include "stdafx.h"
#include "CUDAGeometryBuilder.h"
#include "../../xrLC/Build.h"

#include "Vector3HW.h"


bool OptixGeometryBuilder::BuildBLAS(OptixDeviceContext context, OptixMeshBuffers& outBuffers)
{
    if (vertices.empty() || triangles.empty()) return false;
    Msg( "0. Start BLAS | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);

    // 1. Загружаем вершины на GPU
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outBuffers.vertexBuffer),
        sizeof(Fvector) * vertices.size()));
    CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(outBuffers.vertexBuffer),
        vertices.data(),
        sizeof(Fvector) * vertices.size(),
        cudaMemcpyHostToDevice));
    Msg("1. Loading *Vertex | CPUHeap: %u mb", GetHeapMemory() / 1024/ 1024);

    // 2. Загружаем индексы на GPU
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outBuffers.indexBuffer),
        sizeof(CDB::TRI) * triangles.size()));
    CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(outBuffers.indexBuffer),
        triangles.data(),
        sizeof(CDB::TRI) * triangles.size(),
        cudaMemcpyHostToDevice));

    Msg("2. Loading *Index | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);

    // 3. Настройка входных данных для BLAS
    OptixBuildInput buildInput = {};
    buildInput.type = OPTIX_BUILD_INPUT_TYPE_TRIANGLES;

    buildInput.triangleArray.vertexFormat           = OPTIX_VERTEX_FORMAT_FLOAT3;
    buildInput.triangleArray.vertexStrideInBytes    = sizeof(Fvector);
    buildInput.triangleArray.numVertices            = static_cast<uint32_t>(vertices.size());
    buildInput.triangleArray.vertexBuffers          = &outBuffers.vertexBuffer;

    buildInput.triangleArray.indexFormat            = OPTIX_INDICES_FORMAT_UNSIGNED_INT3;
    buildInput.triangleArray.indexStrideInBytes     = sizeof(CDB::TRI);
    buildInput.triangleArray.numIndexTriplets       = static_cast<uint32_t>(triangles.size());
    buildInput.triangleArray.indexBuffer            = outBuffers.indexBuffer;

    static uint32_t flags = OPTIX_GEOMETRY_FLAG_NONE;
    buildInput.triangleArray.flags = &flags;
    buildInput.triangleArray.numSbtRecords = 1;

    Msg("3. Starting Parrams BLAS | CPUHeap: %u mb | used: %u mb", GetHeapMemory() / 1024 / 1024);
 
    // 4. Настройка параметров сборки
    OptixAccelBuildOptions accelOptions = {};
    accelOptions.buildFlags = OPTIX_BUILD_FLAG_ALLOW_COMPACTION | OPTIX_BUILD_FLAG_PREFER_FAST_TRACE;
    accelOptions.operation  = OPTIX_BUILD_OPERATION_BUILD;

    // 5. Вычисление требуемой памяти
    OptixAccelBufferSizes bufferSizes;
    OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &accelOptions, &buildInput, 1, &bufferSizes));
    Msg("5. Calculate need memory BLAS | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);
     
    // 6. Выделение памяти
    CUdeviceptr tempBuffer;
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&tempBuffer), bufferSizes.tempSizeInBytes));
    Msg("6.1 Memory allocation BLAS  | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);

    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outBuffers.blasBuffer), bufferSizes.outputSizeInBytes));
    Msg("6.2 Memory allocation BLAS  | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);


    // 7. Готовим дескриптор для запроса размера компактации
    OptixAccelEmitDesc emitDesc = {};
    CUdeviceptr d_compactedSize;
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_compactedSize), sizeof(uint64_t)));
    emitDesc.type = OPTIX_PROPERTY_TYPE_COMPACTED_SIZE;
    emitDesc.result = d_compactedSize;
    Msg("7. Memory allocation d_compactedSize | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);

 
    // 8. Сборка BLAS
    OPTIX_CHECK(
    optixAccelBuild
    (
        context,
        0, // CUDA stream
        &accelOptions,
        &buildInput,
        1,
        tempBuffer,
        bufferSizes.tempSizeInBytes,
        outBuffers.blasBuffer,
        bufferSizes.outputSizeInBytes,
        &outBuffers.blasHandle,
        &emitDesc,
        1
    )
    );

    Msg("8. Building BLAS | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);


    // 9. Узнаём размер скомпактированной структуры
    uint64_t compactedSize = 0;
    CUDA_CHECK(cudaMemcpy(&compactedSize, reinterpret_cast<void*>(d_compactedSize), sizeof(uint64_t), cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaFree(reinterpret_cast<void*>(d_compactedSize)));

    Msg("9. Getting Compacted size memory | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);
 
    // 10. Компактация, если это выгодно
    size_t size_precompact = bufferSizes.outputSizeInBytes;
    if (compactedSize != 0 && compactedSize < bufferSizes.outputSizeInBytes)
    {
        CUdeviceptr d_compactedBuffer;
        CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_compactedBuffer), compactedSize));

        OptixTraversableHandle compactedHandle;
        OPTIX_CHECK(optixAccelCompact(
            context,
            0, // stream
            outBuffers.blasHandle,
            d_compactedBuffer,
            compactedSize,
            &compactedHandle
        ));

        // Освобождаем старый буфер
        CUDA_CHECK(cudaFree(reinterpret_cast<void*>(outBuffers.blasBuffer)));

        // Сохраняем компактный
        outBuffers.blasBuffer = d_compactedBuffer;
        outBuffers.blasHandle = compactedHandle;
    }

    Msg("10. Compacting size < orig | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);
 
    // clMsg("* [GPU DEVICE MEMORY] Build BLAS: FULL MEMORY(%u mb) / COMPACTED TO(%u mb)",
    //     size_precompact / 1024 / 1024 ,
    //     compactedSize / 1024 / 1024
    // );
     
    // 11. Освобождаем временный буфер
    CUDA_CHECK(cudaFree(reinterpret_cast<void*>(tempBuffer)));

    Msg("11. Clearing temp buffer | CPUHeap: %u mb", GetHeapMemory() / 1024 / 1024);


    return true;
}

bool OptixGeometryBuilder::BuildTLAS(OptixDeviceContext context, OptixMeshBuffers& outScene, CUstream stream)
{
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
    buildOptions.buildFlags = OPTIX_BUILD_FLAG_ALLOW_COMPACTION | OPTIX_BUILD_FLAG_PREFER_FAST_TRACE;
    buildOptions.operation = OPTIX_BUILD_OPERATION_BUILD;

    // 5. Вычисление требуемой памяти
    OptixAccelBufferSizes bufferSizes;
    OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &buildOptions, &buildInput, 1, &bufferSizes));

    // 6. Выделение памяти
    CUdeviceptr d_tempBuffer;
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_tempBuffer), bufferSizes.tempSizeInBytes));
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outScene.tlasBuffer), bufferSizes.outputSizeInBytes));

    // 7. Дескриптор компактации
    OptixAccelEmitDesc emitDesc = {};
    CUdeviceptr d_compactedSize;
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_compactedSize), sizeof(uint64_t)));
    emitDesc.type = OPTIX_PROPERTY_TYPE_COMPACTED_SIZE;
    emitDesc.result = d_compactedSize;
 
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
        &emitDesc,
        1
    ));

    CUDA_CHECK(cudaFree(reinterpret_cast<void*>(d_tempBuffer)));
    CUDA_CHECK(cudaFree(reinterpret_cast<void*>(d_instances)));

    return true;
}



// Scene Global Data
#include "../xrLC_GlobalData.h"
#include "../xrMU_Model_Reference.h"
#include <embree_raytracing/EmbreeRayTrace.h>
#include "xrDeflectorLight_Packed.h"

extern size_t GetHeapMemory();
struct FaceDataIntel;
 
bool XRay::RayTrace::CUDA::BuildSceneFromLCGlobalData(OptixDeviceContext context, CUstream stream, OptixMeshBuffers& outScene)
{
    xrLC_GlobalData* globalData = lc_global_data();
    if (!globalData)        return false;

    OptixGeometryBuilder geometryBuilder;
    // 1. Обрабатываем статическую геометрию    
    size_t Start                                                = GetHeapMemory();

    xr_vector<Face*>			adjacent_vec(6 * 2 * 3);

    for (Face* F : globalData->g_faces())
    {
        const Shader_xrLC& SH = F->Shader();
        if (!SH.flags.bLIGHT_CastShadow) continue;

        b_material& M = globalData->materials()[F->dwMaterial];
        b_texture& T = globalData->textures()[M.surfidx];

        bool isTransparent = !F->flags.bOpaque && T.pSurface && T.bHasAlpha;
        F->flags.bOpaque = !isTransparent;

        if (!isTransparent)
            geometryBuilder.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
        else
            geometryBuilder.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
    }

    // 2. Обрабатываем MU-референсы
    for (auto ref : globalData->mu_refs())
    {
        xr_vector<FaceDataIntel> tempBuffer;
        ref->export_cform_rcast_new(tempBuffer);

        for (auto& pF : tempBuffer)
        {
            Face* F = (Face*)pF.ptr;
            b_material& M = globalData->materials()[F->dwMaterial];
            b_texture& T = globalData->textures()[M.surfidx];

            bool isTransparent = !F->flags.bOpaque && T.pSurface && T.bHasAlpha;

            if (!isTransparent)
                  geometryBuilder.AddFace(F, pF.v1, pF.v2, pF.v3);
            else
                geometryBuilder.AddFace(F, pF.v1, pF.v2, pF.v3);
        }
    }
   
    size_t pVertex = geometryBuilder.RawFacesSize() * 3;
    size_t pFaces  = geometryBuilder.RawFacesSize();

    geometryBuilder.RemoveDublicates();
    geometryBuilder.RemoveDublicateFaces();

    clMsg("*[GPU Accel Structure] Remove Dublicate Vert : %llu to %llu", pVertex, geometryBuilder.vertices.size());
    clMsg("*[GPU Accel Structure] Remove Dublicate Face : %llu to %llu", pFaces, geometryBuilder.triangles.size());
    clMsg("![GPU Accel Structure] Collected faces: Memory: %u mb", (GetHeapMemory() - Start) / 1024 / 1024);

    // 3. Строим BLAS
    if (!geometryBuilder.BuildBLAS(context, outScene))          return false;

    clMsg("![GPU Accel Structure] Processing BLAS: Memory: %u mb", (GetHeapMemory() - Start) / 1024 / 1024);
  
    // 4. Строим TLAS
    if (!geometryBuilder.BuildTLAS(context, outScene, stream))  return false;

    clMsg("![GPU Accel Structure] Processing TLAS: Memory: %u mb", (GetHeapMemory() - Start) / 1024 / 1024);


    // 5: Face Pointers Loading to GPU
    XRay::RayTrace::CUDA::InitializeFaces(geometryBuilder.facePointers);

    clMsg("![GPU Accel Structure] Processing FACES COPY: Memory: %u mb", (GetHeapMemory() - Start) / 1024 / 1024);


    // 6. Индексируем фаейсы из созданой геометрии
    int Index = 0;
    for (auto& F : geometryBuilder.facePointers)
    {
        SetFaceIndex( F, Index );
        Index++;
    }
 
    geometryBuilder.Clear();
    geometryBuilder.MemoryDealoc();

    clMsg("![GPU Accel Structure] Memory in CPU (no cleared): % u mb", (GetHeapMemory() - Start) / 1024 / 1024);

    return true;
}
