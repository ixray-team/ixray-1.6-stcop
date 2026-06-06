#include "stdafx.h"
#include "CUDAGeometryBuilder.h"
#include "../../xrLC/Build.h"
// Scene Global Data
#include "../xrLC_GlobalData.h"
#include "../xrMU_Model_Reference.h"
#include <embree_raytracing/EmbreeRayTrace.h>
#include "global_calculation_data.h"

bool OptixGeometryBuilder::BuildBLAS(OptixDeviceContext context, OptixMeshBuffers& outBuffers)
{
    if (vertices.empty() || triangles.empty()) return false;
  
    // 0. Временные буферы для построения
    CUdeviceptr  d_tempBuffer;
    CUdeviceptr  d_tmp_vertexBuffer;
    CUdeviceptr  d_tmp_indexBuffer;
   
    // 1. Загружаем вершины на GPU
    CUDA_CHECK_2(cuMemAlloc(&d_tmp_vertexBuffer, sizeof(Fvector) * vertices.size()));
    CUDA_CHECK_2(cuMemcpyHtoD(d_tmp_vertexBuffer, vertices.data(), sizeof(Fvector) * vertices.size()));


    // 2. Загружаем индексы на GPU
	CUDA_CHECK_2(cuMemAlloc(&d_tmp_indexBuffer, sizeof(CDB::TRI) * triangles.size()));
	CUDA_CHECK_2(cuMemcpyHtoD(d_tmp_indexBuffer, triangles.data(), sizeof(CDB::TRI) * triangles.size()));


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
    accelOptions.buildFlags          = OPTIX_BUILD_FLAG_PREFER_FAST_TRACE;
    accelOptions.buildFlags         |= OPTIX_BUILD_FLAG_ALLOW_COMPACTION;

    // 5. Вычисление требуемой памяти
    OptixAccelBufferSizes bufferSizes;
    OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &accelOptions, &buildInput, 1, &bufferSizes));
     
    // 6. Выделение памяти
	CUDA_CHECK_2(cuMemAlloc(&d_tempBuffer, bufferSizes.tempSizeInBytes));
	CUDA_CHECK_2(cuMemAlloc(&outBuffers.blasBuffer, bufferSizes.outputSizeInBytes));
     
    // 7. Готовим дескриптор для запроса размера компактации
    OptixAccelEmitDesc emitDesc = {};
    CUdeviceptr d_compactedSize;
	CUDA_CHECK_2(cuMemAlloc(&d_compactedSize, sizeof(uint64_t)));
    emitDesc.type = OPTIX_PROPERTY_TYPE_COMPACTED_SIZE;
    emitDesc.result = d_compactedSize;

    CUstream stream;
	CUDA_CHECK_2(cuStreamCreate(&stream, CU_STREAM_DEFAULT));
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

    CUDA_CHECK_2(cuStreamSynchronize(stream));

    // 9. Узнаём размер скомпактированной структуры
    uint64_t compactedSize = 0;
    CUDA_CHECK_2(cuMemcpyDtoH(&compactedSize, d_compactedSize, sizeof(uint64_t)));
    CUDA_CHECK_2(cuMemFree(d_compactedSize));
    
    // 10. Компактация, если это выгодно
    size_t size_precompact = bufferSizes.outputSizeInBytes;
    if (compactedSize != 0 && compactedSize < bufferSizes.outputSizeInBytes)
    {
        CUdeviceptr d_compactedBuffer;
		CUDA_CHECK_2(cuMemAlloc(&d_compactedBuffer, compactedSize));
        OptixTraversableHandle compactedHandle;
        OPTIX_CHECK(optixAccelCompact(
            context,
            stream, // stream
            outBuffers.blasHandle,
            d_compactedBuffer,
            compactedSize,
            &compactedHandle
        ));

        CUDA_CHECK_2(cuStreamSynchronize(stream));

		// Освобождаем старый буфер
        CUDA_CHECK_2(cuMemFree(outBuffers.blasBuffer));

        // Сохраняем компактный
        outBuffers.blasBuffer = d_compactedBuffer;
        outBuffers.blasHandle = compactedHandle;
    }
 
    // 11. Освобождаем временный буфер
    CUDA_CHECK_2(cuStreamDestroy(stream));
    
    CUDA_CHECK_2(cuMemFree(d_tempBuffer));
	CUDA_CHECK_2(cuMemFree(d_tmp_vertexBuffer));
	CUDA_CHECK_2(cuMemFree(d_tmp_indexBuffer));
      
    return true;
}

bool OptixGeometryBuilder::BuildTLAS(OptixDeviceContext context, OptixMeshBuffers& outScene)
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
	CUDA_CHECK_2(cuMemAlloc(&d_instances, sizeof(OptixInstance)));
	CUDA_CHECK_2(cuMemcpyHtoD(d_instances, &instance, sizeof(OptixInstance)));

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
	CUDA_CHECK_2(cuMemAlloc(&d_tempBuffer, bufferSizes.tempSizeInBytes));
	CUDA_CHECK_2(cuMemAlloc(&outScene.tlasBuffer, bufferSizes.outputSizeInBytes));
 
    CUstream stream;
	CUDA_CHECK_2(cuStreamCreate(&stream, CU_STREAM_DEFAULT));

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

    CUDA_CHECK_2(cuStreamSynchronize(stream));
	CUDA_CHECK_2(cuStreamDestroy(stream));

    CUDA_CHECK_2(cuMemFree(d_tempBuffer));
	CUDA_CHECK_2(cuMemFree(d_instances));

    return true;
}
  
struct FaceDataEmbree;
bool XRay::RayTrace::CUDA::BuildSceneFromLCGlobalData(OptixDeviceContext context, OptixMeshBuffers& outScene)
{
    OptixGeometryBuilder geometryBuilder;
    size_t StartMemory = GetHeapMemory();

    if (gCompilerMode.builder_type == LCBuildingType::eLC)
    {
        xrLC_GlobalData* globalData = lc_global_data();
        if (!globalData)        return false;


        // 1. Обрабатываем статическую геометрию
        for (Face* F : globalData->g_faces())
        {
            const Shader_xrLC& SH = F->Shader();
            if (!SH.flags.bLIGHT_CastShadow) { continue; }

            u16 surfaceID = globalData->materials()[F->dwMaterial].surfidx;
            b_texture& T = globalData->textures()[surfaceID];

            bool isTransparent = (!T.pSurface.Empty() && T.bHasAlpha);
            F->flags.bOpaque = !isTransparent;
            geometryBuilder.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
        }

        // 2. Обрабатываем MU-референсы
        xr_vector<FaceDataEmbree> tempBuffer;
        for (auto ref : globalData->mu_refs())
        {
            tempBuffer.clear();
            ref->export_cform_rcast_new(tempBuffer);

            for (auto& pF : tempBuffer)
            {
                Face* F = (Face*)pF.ptr;
                b_material& M = globalData->materials()[F->dwMaterial];
                b_texture& T = globalData->textures()[M.surfidx];

                bool isTransparent = (!T.pSurface.Empty() && T.bHasAlpha);
                F->flags.bOpaque = isTransparent;
                geometryBuilder.AddFace(F, pF.v1, pF.v2, pF.v3);
            }
        }
        tempBuffer.clear();
        tempBuffer.shrink_to_fit();
    }
    else if (gCompilerMode.builder_type == LCBuildingType::eDO)
    {
       auto globalData         = &gl_data;
       if (!globalData)        return false;
       
        // 1. Обрабатываем статическую геометрию
       for (auto& F : globalData->building_embree_faces)
       {
           u16 surfaceID = globalData->g_materials[F.dwMaterial].surfidx;
           b_texture& T  = globalData->g_textures[surfaceID];
       
           bool isTransparent = (!T.pSurface.Empty() && T.bHasAlpha);
           F.bOpaque = !isTransparent;
           geometryBuilder.AddFace(&F, F.v1, F.v2, F.v3);
       }
    }
 
    size_t pVertex = geometryBuilder.RawFacesSize() * 3;
    size_t pFaces  = geometryBuilder.RawFacesSize();
    geometryBuilder.RemoveDublicates();  
    geometryBuilder.RemoveDublicateFaces();
    Msg("*[GPU Accel Structure] Collected Structure Faces Memory: %u mb", u32( (GetHeapMemory() - StartMemory) / 1024 / 1024));
  
    StartMemory = GetHeapMemory();

    // 3. Строим BLAS
    if (!geometryBuilder.BuildBLAS(context, outScene))          return false;
  
     // 4. Строим TLAS
    if (!geometryBuilder.BuildTLAS(context, outScene))          return false;
    Msg("*[GPU Accel Structure] Cpu (GPU Used) Memory: %u mb", u32( (GetHeapMemory() - StartMemory) / 1024 / 1024));

    // 5: Face Pointers Loading to GPU
    StartMemory = GetHeapMemory();
    XRay::RayTrace::CUDA::InitializeFaces(geometryBuilder.facePointers);
 
    // Msg("$[GPU Accel Structure] Remove Dublicate Vert : %llu to %llu", pVertex, geometryBuilder.vertices.size());
    // Msg("$[GPU Accel Structure] Remove Dublicate Face : %llu to %llu", pFaces, geometryBuilder.triangles.size());

    geometryBuilder.Clear();
    geometryBuilder.MemoryDealoc();
    return true;
}

