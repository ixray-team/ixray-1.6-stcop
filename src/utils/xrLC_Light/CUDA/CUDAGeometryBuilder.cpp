#include "stdafx.h"
#include "CUDAGeometryBuilder.h"

u32 OptixGeometryBuilder::AddVertex(const Fvector& v)
{
//   for (u32 i = 0; i < vertices.size(); ++i)
//   {
//       if (vertices[i].similar(v, 0.001f))
//           return i;
//   }

    vertices.push_back(v);
    return vertices.size() - 1;
}

bool OptixGeometryBuilder::BuildBLAS(OptixDeviceContext context, XRay::RayTrace::CUDA::OptixMeshBuffers& outBuffers)

{
    if (vertices.empty() || triangles.empty()) return false;

    // 1. Загружаем вершины на GPU
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outBuffers.vertexBuffer),
        sizeof(Fvector) * vertices.size()));
    CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(outBuffers.vertexBuffer),
        vertices.data(),
        sizeof(Fvector) * vertices.size(),
        cudaMemcpyHostToDevice));

    // 2. Загружаем индексы на GPU
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outBuffers.indexBuffer),
        sizeof(CDB::TRI) * triangles.size()));
    CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(outBuffers.indexBuffer),
        triangles.data(),
        sizeof(CDB::TRI) * triangles.size(),
        cudaMemcpyHostToDevice));

    // 3. Настройка входных данных для BLAS
    OptixBuildInput buildInput = {};
    buildInput.type = OPTIX_BUILD_INPUT_TYPE_TRIANGLES;

    buildInput.triangleArray.vertexFormat = OPTIX_VERTEX_FORMAT_FLOAT3;
    buildInput.triangleArray.vertexStrideInBytes = sizeof(Fvector);
    buildInput.triangleArray.numVertices = static_cast<uint32_t>(vertices.size());
    buildInput.triangleArray.vertexBuffers = &outBuffers.vertexBuffer;

    buildInput.triangleArray.indexFormat = OPTIX_INDICES_FORMAT_UNSIGNED_INT3;
    buildInput.triangleArray.indexStrideInBytes = sizeof(CDB::TRI);
    buildInput.triangleArray.numIndexTriplets = static_cast<uint32_t>(triangles.size());
    buildInput.triangleArray.indexBuffer = outBuffers.indexBuffer;

    static uint32_t flags = OPTIX_GEOMETRY_FLAG_NONE;
    buildInput.triangleArray.flags = &flags;
    buildInput.triangleArray.numSbtRecords = 1;

    // 4. Настройка параметров сборки
    OptixAccelBuildOptions accelOptions = {};
    accelOptions.buildFlags = OPTIX_BUILD_FLAG_ALLOW_COMPACTION | OPTIX_BUILD_FLAG_PREFER_FAST_TRACE;
    accelOptions.operation = OPTIX_BUILD_OPERATION_BUILD;

    // 5. Вычисление требуемой памяти
    OptixAccelBufferSizes bufferSizes;
    OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &accelOptions, &buildInput, 1, &bufferSizes));

    // 6. Выделение памяти
    CUdeviceptr tempBuffer;
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&tempBuffer), bufferSizes.tempSizeInBytes));
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&outBuffers.blasBuffer), bufferSizes.outputSizeInBytes));

    // 7. Сборка BLAS
    OPTIX_CHECK(optixAccelBuild
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
        nullptr,
        0));

    // Освобождаем временный буфер
    CUDA_CHECK(cudaFree(reinterpret_cast<void*>(tempBuffer)));

    return true;
}