#include "stdafx.h"
#include "CUDARayCast.h"
#include "CUDAContext.h"
#include "CUDAGeometryBuilder.h"

#include "../xrLC_GlobalData.h"
#include "../xrMU_Model_Reference.h"

#include <embree_raytracing/EmbreeRayTrace.h>

struct FaceDataIntel;
 
bool XRay::RayTrace::CUDA::BuildSceneFromLCGlobalData(OptixDeviceContext context, CUstream stream, XRay::RayTrace::CUDA::OptixMeshBuffers& outScene)
{
	xrLC_GlobalData* globalData = lc_global_data();
	if (!globalData)
		return false;

	OptixGeometryBuilder geometryBuilder;
 	// 1. Обрабатываем статическую геометрию
	Status("Build BLAS...");
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
		else
		{
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

		for (auto& pF : tempBuffer) 
		{
			Face* F = (Face*)pF.ptr;
			b_material& M = globalData->materials()[F->dwMaterial];
			b_texture& T = globalData->textures()[M.surfidx];

			bool isTransparent = !F->flags.bOpaque && T.pSurface && T.bHasAlpha;
			if (!isTransparent) 
			{
 				geometryBuilder.AddFace(F, pF.v1, pF.v2, pF.v3);
			}
			else
			{
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

	for (u32 i = 0; i < gpuTextures.size(); i++) 
	{
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
#include "Vector3HW.h"

#include <optix_function_table_definition.h>
struct OPTICK_Params
{
	OptixTraversableHandle handle;
 	// 
	unsigned char	    flags; 
	hardware_raytask*	rays;
	hardware_color*		colors;				// Position, Direction, Color
	hardware_lighting*	lights;				// Lights
	int					counts_lights;
};

class RayTracer
{
	// Colors (Result)
	hardware_color*		  h_colors;			// CPU alloc
	hardware_color*		  d_colors;			// GPU alloc

	// Positions Rays (Incoming)
	hardware_raytask*		h_rays;			// CPU alloc
	hardware_raytask*		d_rays;			// GPU alloc


	// Lighting 
	int					  size_lights;
	hardware_lighting*    h_lights;			// CPU alloc
	hardware_lighting*	  d_lights;			// GPU alloc


	// Parrrams (.cu export __constant__ Params g_params; )
	OPTICK_Params*		  h_params;			// CPU alloc
	OPTICK_Params*		  d_params;			// GPU alloc)
	
	
	CUstream	  stream;					// Отдельный стрим
	int			  max_rays;					// Макс. количество лучей в батче
 
public:
	bool isInitialized = false;

	~RayTracer()
	{
		if (h_params) cudaFreeHost(h_params);
		if (h_rays) cudaFreeHost(h_rays);
		if (h_colors) cudaFreeHost(h_colors);

		if (d_params) cudaFree(d_params);
		if (d_rays) cudaFree(d_rays);
		if (d_colors) cudaFree(d_colors);
 	}
	
	void Init(int max_rays)
	{
		this->max_rays = max_rays;
  		CUDA_CHECK(cudaStreamCreate(&stream));	
		
		// parrams
		CUDA_CHECK(cudaMallocHost(&h_params,  sizeof(OPTICK_Params)));										// Host Alloc
		CUDA_CHECK(cudaMalloc(&d_params,	  sizeof(OPTICK_Params)));										// Device Alloc

		// colors
		CUDA_CHECK(cudaMallocHost(&h_colors, max_rays * sizeof(hardware_color)));							// Host Alloc
		CUDA_CHECK(cudaMalloc(&d_colors, max_rays * sizeof(hardware_color)));								// Device Alloc

		// positions
		CUDA_CHECK(cudaMallocHost(&h_rays, max_rays * sizeof(hardware_raytask)));							// Host Alloc
		CUDA_CHECK(cudaMalloc(&d_rays, max_rays * sizeof(hardware_raytask)));								// Device Alloc
		
		isInitialized = true;
	}

	void InitializeLights(base_lighting& Lights)
	{
		enum eType : u16
		{
			eSun, 
			eHemi, 
			eRGB
		};
 
 		auto Light = [&](R_Light& L, eType type)
		{
			hardware_lighting cuL;
 			cuL.type			= L.type;
 			cuL.light_type		= type;
			cuL.diffuse			= { L.diffuse.x, L.diffuse.y, L.diffuse.z };
			cuL.position		= { L.position.x, L.position.y, L.position.z };
			cuL.direction		= { L.direction.x, L.direction.y, L.direction.z };
			cuL.range			= L.range;
			cuL.range2			= L.range2;
			cuL.falloff			= L.falloff;
			cuL.attenuation0	= L.attenuation0;
			cuL.attenuation1	= L.attenuation1;
			cuL.attenuation2	= L.attenuation2;
			cuL.energy			= L.energy;
			return cuL;
 		};
 		
		u32 numLights = Lights.rgb.size() + Lights.hemi.size() + Lights.sun.size();
 		
		// Заполняем буфер Источников света
		//h_lights = new hardware_lighting[numLights];

		CUDA_CHECK(cudaMallocHost(&h_lights, numLights * sizeof(hardware_lighting)));

		int INDEX_LIGHT = 0;
		for (auto& RGB : Lights.rgb)
		{
			h_lights[INDEX_LIGHT] = Light(RGB, eRGB);
			INDEX_LIGHT++;
		}
		for (auto& SUN : Lights.sun)
		{
			h_lights[INDEX_LIGHT] = Light(SUN, eSun);
			INDEX_LIGHT++;
		}
		for (auto& HEMI : Lights.hemi)
		{
			h_lights[INDEX_LIGHT] = Light(HEMI, eHemi);
			INDEX_LIGHT++;
		}
 	
		CUDA_CHECK( cudaMalloc(&d_lights, sizeof(hardware_lighting) * numLights) );
		CUDA_CHECK( cudaMemcpy(d_lights, h_lights, sizeof(hardware_lighting) * numLights, cudaMemcpyHostToDevice) );
		size_lights = numLights;
	}


	u32 CurrentWritedRays = 0;
	u8  current_flags = 0;

	// Заполнять после вызова StartRayTracing (чтобы индекс начинался с 0) (при каждой новой стадии освещения)
	void WriteRayToBuffer(RayRecvestIndex& Task)
	{
 		h_rays[CurrentWritedRays] =
		{
			.Position = make_float3(Task.P.x, Task.P.y, Task.P.z),
			.Direction = make_float3(Task.N.x, Task.N.y, Task.N.z)
		};
		CurrentWritedRays++;
  	}
	
	xr_vector<base_color_c> colors;

	// Вызывать только после вызова RayTrace
 	xr_vector<base_color_c>& GetColors()
	{
  		return colors;
	}

	void ClearDeviceResult()
	{
		memset(h_colors, 0, max_rays * sizeof(hardware_color));
		CUDA_CHECK(cudaMemset(d_colors, 0, max_rays * sizeof(hardware_color)));
	}

	void TraceRaysNew()
	{
		CTimer t;t.Start();
  		// Подготавливаем данные на хосте
 		h_params[0] =
		{
			.handle = CommitedScene.tlasHandle,
			// Result Buffer
			.flags  = current_flags,
			.rays   = d_rays,
			.colors = d_colors,
			.lights = d_lights,
			.counts_lights = size_lights,
		};
 		 
		// Копируем Стартовые параметры !!! асинхронно
		CUDA_CHECK(
			cudaMemcpyAsync(
				d_rays,
				h_rays,
				CurrentWritedRays * sizeof(hardware_raytask),
				cudaMemcpyHostToDevice,
				stream
			)
		);
 		
		// Копируем на устройство
		CUDA_CHECK(cudaMemcpyAsync(
			d_params,
			h_params,
			sizeof(OPTICK_Params),
			cudaMemcpyHostToDevice,
			stream
		));

		// Запускаем трассировку
		OPTIX_CHECK(optixLaunch
		(
			optixContext.GetPipeline(),
			stream,
			reinterpret_cast<CUdeviceptr> ( d_params ),
			sizeof(OPTICK_Params),
			&optixContext.GetSBT(),
			CurrentWritedRays, 1, 1  // Запускаем N лучей
		));

		// Копируем результаты асинхронно
		CUDA_CHECK(
		cudaMemcpyAsync(
			h_colors,
			d_colors,
			CurrentWritedRays * sizeof(hardware_color),
			cudaMemcpyDeviceToHost,
			stream
			)
		);
 
		// Синхронизируем только один раз
		CUDA_CHECK(cudaStreamSynchronize(stream));

		clMsg("*** GPU Stream Processing: %u ms | RaysTasks : %u ", t.GetElapsed_ms(), CurrentWritedRays);

		// Копия цветов
		auto copy_color = [&](hardware_color& Chw, base_color_c& C)
		{
			C.hemi = Chw.hemi;
			C.sun = Chw.sun;
			C.rgb = { Chw.rgb.x, Chw.rgb.y, Chw.rgb.z };
		};

		// Добавляем результат в конец списка
		for (int it = 0; it < CurrentWritedRays; it++)
		{
			base_color_c C;
			copy_color(h_colors[it], C);
			colors.push_back(C); 
		}

		// Чистим списки и результаты
		ClearDeviceResult();
		CurrentWritedRays = 0;
	}
};

static RayTracer GPURayTracer;
  
// Raytracer Initialize
void XRay::RayTrace::CUDA::RayTraceInitialize(base_lighting& L, u8 CurrentFlags)
{
	if (!GPURayTracer.isInitialized)
	{
		GPURayTracer.Init(MAX_RAYS_PER_TASK);
		GPURayTracer.InitializeLights(L);
	}

	GPURayTracer.current_flags = CurrentFlags;
	GPURayTracer.CurrentWritedRays = 0;
	GPURayTracer.colors.clear();
}

void XRay::RayTrace::CUDA::RayTraceAddRay(RayRecvestIndex& task)
{
	GPURayTracer.WriteRayToBuffer(task);
}

void XRay::RayTrace::CUDA::RayTraceRun()
{
	GPURayTracer.TraceRaysNew();
}

xr_vector<base_color_c>& XRay::RayTrace::CUDA::RayTraceResult()
{
	return GPURayTracer.GetColors();
}
 