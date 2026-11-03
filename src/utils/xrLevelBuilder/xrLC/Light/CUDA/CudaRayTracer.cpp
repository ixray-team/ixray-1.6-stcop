#include "stdafx.h"
#include "CudaRayTracer.h"
#include "CUDAContext.h"
#include "../../Build/Build.h"

#include <optix_function_table_definition.h>
// FX: Для данных о CUDA типах
#include "Vector3HW.cuh"

struct OptixMeshBuffers
{
	// Blas Model
	CUdeviceptr blasBuffer = 0;
	OptixTraversableHandle blasHandle = 0;

	// Tlas Model
	CUdeviceptr tlasBuffer = 0;
	OptixTraversableHandle tlasHandle = 0;
};

static bool isGPUInitialized = false;
static OptixContext optixContext;								// Постоянный буфер !
static OptixMeshBuffers CommitedScene;

// Loading Buffers
static xr_vector<Hardware_TextureData> cpu_tex_gpu;

static int size_lights;
static Hardware_Lighting* gpu_lights = nullptr;			// GPU alloc

static int size_faces;
static Hardware_FaceData* gpu_faces = nullptr;

static int size_textures;
static Hardware_TextureData* gpu_textures = nullptr;

static int size_textures_shared;
static Hardware_TextureData* gpu_textures_shared = nullptr;

void CudaRayTracer::Initialize()
{
	if (isGPUInitialized) return;

	// Однократная инициализация
	if (optixContext.Initialize())
	{
		isGPUInitialized = true;
		Msg("[OptiX] Successfully initialized OptiX context");
	}
	else
	{
		FATAL("[OptiX] Failed to initialize OptiX context");
	}
}

//////////////////////////////////////////////////////////////////////////
// Acceleration structures
static bool BuildBLAS(OptixDeviceContext context, const GpuScene& Scene, OptixMeshBuffers& outBuffers)
{
	if (Scene.Vertices.empty() || Scene.Indices.empty()) return false;

	// 0. Временные буферы для построения
	CUdeviceptr  d_tempBuffer;
	CUdeviceptr  d_tmp_vertexBuffer;
	CUdeviceptr  d_tmp_indexBuffer;

	// 1. Загружаем вершины на GPU
	CUDA_CHECK_2(cuMemAlloc(&d_tmp_vertexBuffer, sizeof(Fvector) * Scene.Vertices.size()));
	CUDA_CHECK_2(cuMemcpyHtoD(d_tmp_vertexBuffer, Scene.Vertices.data(), sizeof(Fvector) * Scene.Vertices.size()));

	// 2. Загружаем индексы на GPU
	CUDA_CHECK_2(cuMemAlloc(&d_tmp_indexBuffer, sizeof(u32) * Scene.Indices.size()));
	CUDA_CHECK_2(cuMemcpyHtoD(d_tmp_indexBuffer, Scene.Indices.data(), sizeof(u32) * Scene.Indices.size()));

	// 3. Настройка входных данных для BLAS
	OptixBuildInput buildInput = {};
	buildInput.type                                 = OPTIX_BUILD_INPUT_TYPE_TRIANGLES;

	buildInput.triangleArray.vertexFormat           = OPTIX_VERTEX_FORMAT_FLOAT3;
	buildInput.triangleArray.vertexStrideInBytes    = sizeof(Fvector);
	buildInput.triangleArray.numVertices            = static_cast<uint32_t>(Scene.Vertices.size());
	buildInput.triangleArray.vertexBuffers          = &d_tmp_vertexBuffer;

	buildInput.triangleArray.indexFormat            = OPTIX_INDICES_FORMAT_UNSIGNED_INT3;
	buildInput.triangleArray.indexStrideInBytes     = 3 * sizeof(u32);
	buildInput.triangleArray.numIndexTriplets       = static_cast<uint32_t>(Scene.Indices.size() / 3);
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
	OPTIX_CHECK(optixAccelBuild
	(
		context,
		stream,
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
	if (compactedSize != 0 && compactedSize < bufferSizes.outputSizeInBytes)
	{
		CUdeviceptr d_compactedBuffer;
		CUDA_CHECK_2(cuMemAlloc(&d_compactedBuffer, compactedSize));
		OptixTraversableHandle compactedHandle;
		OPTIX_CHECK(optixAccelCompact(
			context,
			stream,
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

static bool BuildTLAS(OptixDeviceContext context, OptixMeshBuffers& outScene)
{
	if (outScene.blasHandle == 0)
	{
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
	buildOptions.buildFlags = OPTIX_BUILD_FLAG_PREFER_FAST_TRACE;
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

//////////////////////////////////////////////////////////////////////////
// Scene data
static void UploadLights(const GpuScene& Scene)
{
	const u32 numLights = (u32)Scene.Lights.size();

	xr_vector<Hardware_Lighting> h_lights(numLights);
	for (u32 Index = 0; Index < numLights; Index++)
	{
		const GpuLight& L = Scene.Lights[Index];
		Hardware_Lighting& cuL = h_lights[Index];
		cuL.type			= L.Type;
		cuL.light_type		= L.LightType;
		cuL.diffuse			= { L.Diffuse.x, L.Diffuse.y, L.Diffuse.z };
		cuL.position		= { L.Position.x, L.Position.y, L.Position.z };
		cuL.direction		= { L.Direction.x, L.Direction.y, L.Direction.z };
		cuL.range			= L.Range;
		cuL.range2			= L.Range2;
		cuL.falloff			= L.Falloff;
		cuL.attenuation0	= L.Attenuation0;
		cuL.attenuation1	= L.Attenuation1;
		cuL.attenuation2	= L.Attenuation2;
		cuL.energy			= L.Energy;
	}

	CUDA_CHECK(cudaMalloc(&gpu_lights, sizeof(Hardware_Lighting) * numLights));
	CUDA_CHECK(cudaMemcpy(gpu_lights, h_lights.data(), sizeof(Hardware_Lighting) * numLights, cudaMemcpyHostToDevice));
	size_lights = numLights;

	Msg("[CUDA] Loaded Lighting: %u size", numLights);
}

static void UploadFaces(const GpuScene& Scene)
{
	xr_vector<Hardware_FaceData> faces_host(Scene.Faces.size());
	for (size_t IndexFace = 0; IndexFace < Scene.Faces.size(); IndexFace++)
	{
		const GpuFace& F = Scene.Faces[IndexFace];
		Hardware_FaceData& FaceGPU = faces_host[IndexFace];
		FaceGPU.bOpacue = F.Opaque;
		FaceGPU.bWater  = false;
		FaceGPU.bShared = F.Shared;
		FaceGPU.surfidx = F.SurfaceID;

		for (int Corner = 0; Corner < 3; Corner++)
		{
			FaceGPU.TC0[Corner].x = __float2half(F.TC[Corner].x);
			FaceGPU.TC0[Corner].y = __float2half(F.TC[Corner].y);
		}
	}

	size_t alloc_size = faces_host.size() * sizeof(Hardware_FaceData);
	CUDA_CHECK(cudaMalloc(&gpu_faces, alloc_size));
	CUDA_CHECK(cudaMemcpy(gpu_faces, faces_host.data(), alloc_size, cudaMemcpyHostToDevice));
	size_faces = (int)faces_host.size();

	Msg("[GPU DEVICE MEMORY] Faces[%u] Allocated: %u mb", (u32)faces_host.size(), (u32)(alloc_size / 1024 / 1024));
}

static void UploadTexturesAlpha(const GpuScene& Scene)
{
	cpu_tex_gpu.resize(Scene.Textures.size() + Scene.TexturesShared.size());
	size_t allocated = 0;
	auto GPUMoveFunc = [&allocated](Hardware_TextureData*& OutArr, int& OutSize, Hardware_TextureData* Buffer, size_t BufferSize, const xr_vector<GpuTexture>& In)
	{
		for(size_t i = 0; i < In.size(); i++)
		{
			const auto& Src = In[i];
			if(Src.Alpha.empty())
			{
				Buffer[i].pSurface = nullptr;
			} else
			{
				u8* d_alpha = nullptr;
				CUDA_CHECK(cudaMalloc(&d_alpha, Src.Alpha.size()));
				CUDA_CHECK(cudaMemcpy(d_alpha, Src.Alpha.data(), Src.Alpha.size(), cudaMemcpyHostToDevice));
				Buffer[i].pSurface = d_alpha;
				allocated += Src.Alpha.size();
			}
			Buffer[i].width = Src.Width;
			Buffer[i].height = Src.Height;
		}
		
		CUDA_CHECK ( cudaMalloc((void**)&OutArr, BufferSize * sizeof(Hardware_TextureData)) );
		CUDA_CHECK(  cudaMemcpy(OutArr,
			Buffer,
			BufferSize * sizeof(Hardware_TextureData),
			cudaMemcpyHostToDevice) );
		allocated += BufferSize * sizeof(Hardware_TextureData);
		OutSize = BufferSize;
	};
	GPUMoveFunc(gpu_textures, size_textures, cpu_tex_gpu.data(), Scene.Textures.size(), Scene.Textures);
	GPUMoveFunc(gpu_textures_shared, size_textures_shared, cpu_tex_gpu.data()+Scene.Textures.size(), Scene.TexturesShared.size(), Scene.TexturesShared);

	Msg("[GPU DEVICE MEMORY] Textures[%u] Allocate : %llu mb", (u32)cpu_tex_gpu.size(), (u64)(allocated / 1024 / 1024));
}

void CudaRayTracer::LoadScene(const GpuScene& Scene)
{
	Phase("CUDA: Initialize Raytrace Model");

	OptixDeviceContext context = optixContext.GetOptixContext();
	size_t StartMemory = GetHeapMemory();
	if (BuildBLAS(context, Scene, CommitedScene))
	{
		BuildTLAS(context, CommitedScene);
	}
	Msg("*[GPU Accel Structure] Cpu (GPU Used) Memory: %u mb", u32((GetHeapMemory() - StartMemory) / 1024 / 1024));

	UploadFaces(Scene);
	UploadLights(Scene);
	UploadTexturesAlpha(Scene);
}

void CudaRayTracer::UnloadScene()
{
	Phase("CUDA: Cleanump...");
	if (isGPUInitialized)
	{
		for (auto& T : cpu_tex_gpu)
			cudaFree(T.pSurface);

		cpu_tex_gpu.clear();

		// Вычищяем большие буферы данных
		cudaFree(gpu_lights);
		cudaFree(gpu_faces);
		cudaFree(gpu_textures);
		cudaFree(gpu_textures_shared);

		size_faces = 0;
		size_lights = 0;
		size_textures = 0;
		size_textures_shared = 0;

		// Вычищаем модель уровня из CUDA
		cudaFree(&CommitedScene.blasBuffer);
		cudaFree(&CommitedScene.tlasBuffer);
		optixContext.Destroy();
	}
	isGPUInitialized = false;
}

//////////////////////////////////////////////////////////////////////////
// Ray batches
class RayTracer
{
	// Colors (Result)
	Hardware_Color*		  h_colors = nullptr;		// CPU alloc
	Hardware_Color*		  d_colors = nullptr;		// GPU alloc

	// Positions Rays (Incoming)
	Hardware_Raytask*	  h_rays = nullptr;			// CPU alloc
	Hardware_Raytask*	  d_rays = nullptr;			// GPU alloc

	// Parrrams (.cu export __constant__ Params g_params; )
	OPTICK_Params*		  h_params = nullptr;		// CPU alloc
	OPTICK_Params*		  d_params = nullptr;		// GPU alloc

	int			  max_rays = 0;						// Макс. количество лучей в батче
	CUstream	  stream = nullptr;					// Отдельный стрим

public:
	bool isInitialized = false;

	~RayTracer()
	{
		ClearingBuffers();
	}

	void ClearingBuffers()
	{
		isInitialized = false;
	}

	void Init(int MaxRays)
	{
		max_rays = MaxRays;
		CUDA_CHECK(cudaStreamCreate(&stream));

		// parrams
		CUDA_CHECK(cudaMallocHost(&h_params,  sizeof(OPTICK_Params)));
		CUDA_CHECK(cudaMalloc(&d_params,	  sizeof(OPTICK_Params)));

		// colors
		CUDA_CHECK(cudaMallocHost(&h_colors, max_rays * sizeof(Hardware_Color)));
		CUDA_CHECK(cudaMalloc(&d_colors, max_rays * sizeof(Hardware_Color)));

		// positions
		CUDA_CHECK(cudaMallocHost(&h_rays, max_rays * sizeof(Hardware_Raytask)));
		CUDA_CHECK(cudaMalloc(&d_rays, max_rays * sizeof(Hardware_Raytask)));

		isInitialized = true;
		ClearDeviceResult();
	}

	void ClearDeviceResult()
	{
		memset(h_colors, 0, max_rays * sizeof(Hardware_Color));
		CUDA_CHECK(cudaMemset(d_colors, 0, max_rays * sizeof(Hardware_Color)));
	}

	void Trace(u8 Flags, const xr_vector<RayRecvestIndex>& Rays, xr_vector<base_color_c>& Colors)
	{
		const size_t CurrentWritedRays = Rays.size();
		for (size_t Index = 0; Index < CurrentWritedRays; Index++)
		{
			const RayRecvestIndex& Task = Rays[Index];
			h_rays[Index] =
			{
				.Position = make_float3(Task.P.x, Task.P.y, Task.P.z),
				.Direction = make_float3(Task.N.x, Task.N.y, Task.N.z),
			};
		}

		// Подготавливаем данные на хосте
		h_params[0] =
		{
			.handle = CommitedScene.tlasHandle,

			// Result Buffer
			.flags = Flags,
			.rays = d_rays,
			.colors = d_colors,

			.lights = gpu_lights,
			.counts_lights = size_lights,

			.faces = gpu_faces,
			.count_faces = size_faces,

			.textures = gpu_textures,
			.count_textures = size_textures,

			.textures_shared = gpu_textures_shared,
			.count_textures_shared = size_textures_shared,
		};

		// Копируем Стартовые параметры !!! асинхронно
		CUDA_CHECK(cudaMemcpyAsync(d_rays, h_rays, CurrentWritedRays * sizeof(Hardware_Raytask), cudaMemcpyHostToDevice, stream));
		CUDA_CHECK(cudaMemcpyAsync(d_params, h_params, sizeof(OPTICK_Params), cudaMemcpyHostToDevice, stream));

		// Запускаем трассировку
		OPTIX_CHECK(optixLaunch
		(
			optixContext.GetPipeline(),
			stream,
			reinterpret_cast<CUdeviceptr>(d_params),
			sizeof(OPTICK_Params),
			&optixContext.GetSBT(),
			(unsigned int)CurrentWritedRays, 1, 1
		));

		// Копируем результаты асинхронно
		CUDA_CHECK(cudaMemcpyAsync(h_colors, d_colors, CurrentWritedRays * sizeof(Hardware_Color), cudaMemcpyDeviceToHost, stream));

		// Синхронизируем только один раз
		CUDA_CHECK(cudaStreamSynchronize(stream));

		Colors.resize(CurrentWritedRays);
		for (size_t it = 0; it < CurrentWritedRays; it++)
		{
			const Hardware_Color& Chw = h_colors[it];
			base_color_c& C = Colors[it];
			C.hemi = Chw.hemi;
			C.sun = Chw.sun;
			C.rgb.set(Chw.rgb.x, Chw.rgb.y, Chw.rgb.z);
		}

		// Чистим списки и результаты
		ClearDeviceResult();
	}
};

static thread_local RayTracer GPURayTracer;

void CudaRayTracer::Trace(u8 Flags, const xr_vector<RayRecvestIndex>& Rays, xr_vector<base_color_c>& Colors)
{
	R_ASSERT(Rays.size() <= MaxRaysPerBatch);
	if (!GPURayTracer.isInitialized)
	{
		GPURayTracer.Init(MaxRaysPerBatch);
	}
	GPURayTracer.Trace(Flags, Rays, Colors);
}

void CudaRayTracer::ReleaseThreadData()
{
	if (GPURayTracer.isInitialized)
	{
		GPURayTracer.ClearDeviceResult();
	}
	GPURayTracer.ClearingBuffers();
}
