#include "stdafx.h"
#include "CUDARayCast.h"
#include "CUDAContext.h"
#include "CUDAGeometryBuilder.h"

#include "../xrLC_GlobalData.h"
#include "../xrMU_Model_Reference.h"

#include <optix_function_table_definition.h>
#include <embree_raytracing/EmbreeRayTrace.h>

struct FaceDataIntel;


bool XRay::RayTrace::CUDA::BuildSceneFromLCGlobalData(OptixDeviceContext context, CUstream stream, XRay::RayTrace::CUDA::OptixMeshBuffers& outScene)
{
	xrLC_GlobalData* globalData = lc_global_data();
	if (!globalData) return false;

	OptixGeometryBuilder geometryBuilder;
 	// 1. Обрабатываем статическую геометрию

	CTimer t; t.Start();
	int INDEX = 0;
	for (Face* F : globalData->g_faces())
	{
 		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow) continue;

		b_material& M = globalData->materials()[F->dwMaterial];
		b_texture& T = globalData->textures()[M.surfidx];

		bool isTransparent = !F->flags.bOpaque && T.pSurface && T.bHasAlpha;
		if (!isTransparent) {
			geometryBuilder.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
		}

		AditionalData("Processing GPU: %u/%u", INDEX, globalData->g_faces().size() );
		INDEX++;
	}
	clMsg("Processing : %u ms", t.GetElapsed_ms());

	// 2. Обрабатываем MU-референсы
	for (auto ref : globalData->mu_refs())
	{
		xr_vector<FaceDataIntel> tempBuffer;
		ref->export_cform_rcast_new(tempBuffer);

		for (auto& pF : tempBuffer) {
			Face* F = (Face*)pF.ptr;
			b_material& M = globalData->materials()[F->dwMaterial];
			b_texture& T = globalData->textures()[M.surfidx];

			bool isTransparent = !F->flags.bOpaque && T.pSurface && T.bHasAlpha;
			if (!isTransparent) {
				geometryBuilder.AddFace(F, pF.v1, pF.v2, pF.v3);
			}
		}
	}

	// 3. Строим BLAS
	if (!geometryBuilder.BuildBLAS(context, outScene))
	{
		return false;
	}

	// 4. Строим TLAS (один экземпляр BLAS)
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

	CUdeviceptr d_instances;
	CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_instances), sizeof(OptixInstance)));
	CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(d_instances), &instance, sizeof(OptixInstance), cudaMemcpyHostToDevice));

	OptixBuildInput buildInput = {};
	buildInput.type = OPTIX_BUILD_INPUT_TYPE_INSTANCES;
	buildInput.instanceArray.instances = d_instances;
	buildInput.instanceArray.numInstances = 1;

	OptixAccelBuildOptions buildOptions = {};
	buildOptions.buildFlags = OPTIX_BUILD_FLAG_ALLOW_COMPACTION | OPTIX_BUILD_FLAG_PREFER_FAST_TRACE;
	buildOptions.operation = OPTIX_BUILD_OPERATION_BUILD;

	OptixAccelBufferSizes bufferSizes;
	OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &buildOptions, &buildInput, 1, &bufferSizes));

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
		nullptr,
		0
	));

	CUDA_CHECK(cudaFree(reinterpret_cast<void*>(d_tempBuffer)));
	CUDA_CHECK(cudaFree(reinterpret_cast<void*>(d_instances)));

	return true;
}


// Пример использования:
static OptixContext optixContext;
static CUstream cudaStream = nullptr;
static XRay::RayTrace::CUDA::OptixMeshBuffers CommitedScene;
OptixTraversableHandle g_optixHandle;
XRay::RayTrace::CUDA::FaceData* d_faces;
XRay::RayTrace::CUDA::MaterialData* d_materials;
cudaTextureObject_t* d_textures;
u32 g_faceCount;

void XRay::RayTrace::CUDA::InitializeRayTracing()
{
	// Однократная инициализация
	static bool initialized = false;
	if (!initialized)
	{
		if (optixContext.Initialize())
		{
			cudaStream = OptixContext::CreateCudaStream();
			Msg("[OptiX] Successfully initialized OptiX context");
			initialized = true;
		}
		else
		{
			FATAL("[OptiX] Failed to initialize OptiX context");
		}
	}

	// Использование контекста
	OptixDeviceContext context = optixContext.GetOptixContext();
	BuildSceneFromLCGlobalData(context, cudaStream, CommitedScene);
}

// При завершении работы
void CleanupRayTracing()
{
	OptixContext::DestroyCudaStream(cudaStream);
	optixContext.Destroy();
}

void XRay::RayTrace::CUDA::InitializeTextures(xr_vector<TextureData>& gpuTextures, cudaTextureObject_t*& d_texObjects)
{
	// 1. Создаем CPU-копии текстурных данных
	gpuTextures.resize(inlc_global_data()->textures().size());

	// 2. Создаем массив для объектов текстур CUDA
	cudaTextureObject_t* texObjects = new cudaTextureObject_t[gpuTextures.size()];

	for (u32 i = 0; i < gpuTextures.size(); i++) {
		b_texture& srcTex = inlc_global_data()->textures()[i];

		// Заполняем TextureData
		TextureData& dstTex = gpuTextures[i];
		dstTex.width = srcTex.dwWidth;
		dstTex.height = srcTex.dwHeight;
		dstTex.hasAlpha = srcTex.bHasAlpha;

		// Выделяем GPU память и копируем данные
		size_t texSize = srcTex.dwWidth * srcTex.dwHeight * sizeof(u32);
		CUDA_CHECK(cudaMalloc(&dstTex.pSurface, texSize));
		CUDA_CHECK(cudaMemcpy(dstTex.pSurface, srcTex.pSurface, texSize, cudaMemcpyHostToDevice));

		// Создаем CUDA texture object
		cudaResourceDesc resDesc = {};
		resDesc.resType = cudaResourceTypePitch2D;
		resDesc.res.pitch2D.devPtr = dstTex.pSurface;
		resDesc.res.pitch2D.width = dstTex.width;
		resDesc.res.pitch2D.height = dstTex.height;
		resDesc.res.pitch2D.pitchInBytes = dstTex.width * sizeof(u32);
		resDesc.res.pitch2D.desc = cudaCreateChannelDesc<uchar4>();

		cudaTextureDesc texDesc = {};
		texDesc.addressMode[0] = cudaAddressModeWrap;
		texDesc.addressMode[1] = cudaAddressModeWrap;
		texDesc.filterMode = cudaFilterModeLinear;
		texDesc.readMode = cudaReadModeNormalizedFloat;
		texDesc.normalizedCoords = 1;

		CUDA_CHECK(cudaCreateTextureObject(&texObjects[i], &resDesc, &texDesc, nullptr));
		dstTex.texObj = texObjects[i];
	}

	// Копируем массив texture objects на GPU
	CUDA_CHECK(cudaMalloc(&d_texObjects, gpuTextures.size() * sizeof(cudaTextureObject_t)));
	CUDA_CHECK(cudaMemcpy(d_texObjects, texObjects,
		gpuTextures.size() * sizeof(cudaTextureObject_t),
		cudaMemcpyHostToDevice));

	delete[] texObjects;
}

#include <xrDeflector.h>

struct RayHitResult
{
	float t;
	int faceId;
};

struct Params
{
	OptixTraversableHandle handle;
	float3 rayOrigin;
	float3 rayDir;
	float rayMaxT;
	RayHitResult* result;
};

u64 RayTracingTime = 0;  
class RayTracer
{
	RayHitResult* d_results;  // Буфер для результатов (N лучей)
	CUdeviceptr d_params;
	CUstream stream;        // Отдельный стрим
	int max_rays;           // Макс. количество лучей в батче

	Params* h_params;
	RayHitResult* h_results;
public:
	bool isInitialized = false;

	~RayTracer()
	{
		if (h_params) cudaFreeHost(h_params);
		if (h_results) cudaFreeHost(h_results);
	}

	void Init(int max_rays)
	{
		this->max_rays = max_rays;
		CUDA_CHECK(cudaMalloc(&d_results, max_rays * sizeof(RayHitResult)));
		//CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_params), sizeof(Params)));
		CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_params), max_rays * sizeof(Params)));

		CUDA_CHECK(cudaStreamCreate(&stream));
		isInitialized = true;

		CUDA_CHECK(cudaMallocHost(&h_params, max_rays * sizeof(Params)));
		CUDA_CHECK(cudaMallocHost(&h_results, max_rays * sizeof(RayHitResult)));
	}

	// 2. Пакетная трассировка лучей
	void TraceRays(xr_vector<RayRequest>& rays)
	{
		CTimer t;
		t.Start();
		if (rays.size() > max_rays)
		{
			Msg("*** > MaxRAYS:  Start Tracing Rays: %u size", rays.size());	return;
		}

		// Подготавливаем данные на хосте
		for (size_t i = 0; i < rays.size(); ++i)
		{
			h_params[i] = {
				.handle = CommitedScene.tlasHandle,
				.rayOrigin = make_float3(rays[i].P.x, rays[i].P.y, rays[i].P.z),
				.rayDir = make_float3(rays[i].D.x, rays[i].D.y, rays[i].D.z),
				.rayMaxT = rays[i].R,
				.result = d_results + i // * sizeof(RayHitResult)
			};
		}
		// Msg("*** Processing HOST Parrams : %u ms", t.GetElapsed_ms()); t.Start();

		// Копируем на устройство
		CUDA_CHECK(cudaMemcpyAsync(
			(void*)d_params,
			h_params,
			rays.size() * sizeof(Params),
			cudaMemcpyHostToDevice,
			stream
		));

		// Msg("*** Processing Copy to GPU: %u ms", t.GetElapsed_ms()); t.Start();


		// Запускаем трассировку
		OPTIX_CHECK(optixLaunch
		(
			optixContext.GetPipeline(),
			stream,
			d_params,
			sizeof(Params),
			&optixContext.GetSBT(),
			rays.size(), 1, 1  // Запускаем N лучей
		));
		// Msg("*** Processing Run Tracing : %u ms", t.GetElapsed_ms()); t.Start();

		// Копируем результаты асинхронно
		CUDA_CHECK(cudaMemcpyAsync(
			h_results,
			(void*)d_results,
			rays.size() * sizeof(RayHitResult),
			cudaMemcpyDeviceToHost,
			stream
		));
		// Msg("*** Processing Copy Results : %u ms", t.GetElapsed_ms()); t.Start();

		// Синхронизируем только один раз
		CUDA_CHECK(cudaStreamSynchronize(stream));

		// Обновляем результаты
		for (size_t i = 0; i < rays.size(); ++i)
 			rays[i].result = h_results[i].t > 0 ? h_results[i].t : -1.0f;
 
		// Msg("*** Processing Update Results : %u ms", t.GetElapsed_ms());

		RayTracingTime += t.GetElapsed_mcs();
	}
};


thread_local RayTracer Tracer;
void XRay::RayTrace::CUDA::RayTracePack(xr_vector<RayRequest> & data)
{
  	if (!Tracer.isInitialized)
 		Tracer.Init(1024 * 32);
 	Tracer.TraceRays(data);
}

float XRay::RayTrace::CUDA::RayTrace(Fvector& P, Fvector& D, float R, Face* skip)
{
#if 0
	static RayTracer Tracer;
	static bool Init = false;

	if (!Init)
	{
		Tracer.Init(1024);
		Init = true;
	}
	Tracer.TraceRays(Data);
#else
	RayHitResult* d_result;
	RayHitResult h_result = { -1.0f, -1 };

	CUDA_CHECK(cudaMalloc(&d_result, sizeof(RayHitResult)));
	CUDA_CHECK(cudaMemcpy(d_result, &h_result, sizeof(RayHitResult), cudaMemcpyHostToDevice));

	Params h_params = {};
	h_params.handle = CommitedScene.tlasHandle;
	h_params.rayOrigin = make_float3(P.x, P.y, P.z);
	h_params.rayDir = make_float3(D.x, D.y, D.z);
	h_params.rayMaxT = R;
	h_params.result = d_result;

	CUdeviceptr d_params;
	CUDA_CHECK(cudaMalloc(reinterpret_cast<void**>(&d_params), sizeof(Params)));
	CUDA_CHECK(cudaMemcpy(reinterpret_cast<void*>(d_params), &h_params, sizeof(Params), cudaMemcpyHostToDevice));

	OPTIX_CHECK(optixLaunch
	(
		optixContext.GetPipeline(),
		cudaStream,
		d_params,
		sizeof(Params),
		&optixContext.GetSBT(),
		1, 1, 1
	));

	CUDA_CHECK(cudaStreamSynchronize(cudaStream));
	CUDA_CHECK(cudaMemcpy(&h_result, d_result, sizeof(RayHitResult), cudaMemcpyDeviceToHost));

	CUDA_CHECK(cudaFree(d_result));
	CUDA_CHECK(cudaFree((void*)d_params));

	if (h_result.t > 0)
	{
		// Можно вернуть расстояние или сам face, если нужен
		return h_result.t;
	}

	return -1.0f;
#endif
}