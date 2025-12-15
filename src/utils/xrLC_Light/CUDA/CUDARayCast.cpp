#include "stdafx.h"
#include "CUDARayCast.h"
#include "CUDAContext.h"
#include "CUDAGeometryBuilder.h"
#include "../xrLC_GlobalData.h"
#include <xrDeflector.h>
#include "Vector3HW.h"
#include <optix_function_table_definition.h>

// Пример использования:
OptixContext optixContext;
OptixTraversableHandle g_optixHandle;
XRay::RayTrace::CUDA::OptixMeshBuffers CommitedScene;


static CUstream cudaStream = nullptr;
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
 	g_optixHandle = CommitedScene.tlasHandle;

	clMsg("Processing Memory: %u mb", GetHeapMemory() / 1024 / 1024);
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
 

 
class RayTracer
{
	// Colors (Result)
	Hardware_Color*		  h_colors;			// CPU alloc
	Hardware_Color*		  d_colors;			// GPU alloc

	// Positions Rays (Incoming)
	Hardware_Raytask*		h_rays;			// CPU alloc
	Hardware_Raytask*		d_rays;			// GPU alloc


	// Lighting 
	int					  size_lights;
	Hardware_Lighting*    h_lights;			// CPU alloc
	Hardware_Lighting*	  d_lights;			// GPU alloc


	// Parrrams (.cu export __constant__ Params g_params; )
	OPTICK_Params*		  h_params;			// CPU alloc
	OPTICK_Params*		  d_params;			// GPU alloc)
	

	int			  max_rays;					// Макс. количество лучей в батче
	CUstream	  stream;					// Отдельный стрим

public:
	xr_vector<base_color_c> colors;
 	u8  current_flags = 0;
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

		// // colors
		CUDA_CHECK(cudaMallocHost(&h_colors, max_rays * sizeof(Hardware_Color)));							// Host Alloc
		CUDA_CHECK(cudaMalloc(&d_colors, max_rays * sizeof(Hardware_Color)));								// Device Alloc
		
		// positions
		CUDA_CHECK(cudaMallocHost(&h_rays, max_rays * sizeof(Hardware_Raytask)));							// Host Alloc
		CUDA_CHECK(cudaMalloc(&d_rays, max_rays * sizeof(Hardware_Raytask)));								// Device Alloc
		
		isInitialized = true;

		InitializeLights(lc_global_data()->L_static());
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
			Hardware_Lighting cuL;
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
 		CUDA_CHECK(cudaMallocHost(&h_lights, numLights * sizeof(Hardware_Lighting)));

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
 	
		CUDA_CHECK( cudaMalloc(&d_lights, sizeof(Hardware_Lighting) * numLights) );
		CUDA_CHECK( cudaMemcpy(d_lights, h_lights, sizeof(Hardware_Lighting) * numLights, cudaMemcpyHostToDevice) );
		size_lights = numLights;
	}
 
	// Вызывать только после вызова RayTrace
 	xr_vector<base_color_c>& GetColors()
	{
  		return colors;
	}

	void ClearDeviceResult()
	{
		memset(h_colors, 0, max_rays * sizeof(Hardware_Color));
		CUDA_CHECK(cudaMemset(d_colors, 0, max_rays * sizeof(Hardware_Color)));
	}
	 
	// Заполнять после вызова StartRayTracing (чтобы индекс начинался с 0) (при каждой новой стадии освещения)
	void WriteRayToBuffer(RayRecvestIndex& Task, size_t INDEX)
	{
		h_rays[INDEX] =
		{
			.Position = make_float3(Task.P.x, Task.P.y, Task.P.z),
			.Direction = make_float3(Task.N.x, Task.N.y, Task.N.z)
		};
	}
	 
	void TraceRaysNew(size_t INDEX)
	{
		size_t CurrentWritedRays = INDEX;

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
			.GpuDeflectors = nullptr,
			.DeflectorsBig = false,
			.DeflectorsBigID = 0,
		};
 		 
		// Копируем Стартовые параметры !!! асинхронно
		CUDA_CHECK(
			cudaMemcpyAsync(
				d_rays,
				h_rays,
				CurrentWritedRays * sizeof(Hardware_Raytask),
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
			CurrentWritedRays * sizeof(Hardware_Color),
			cudaMemcpyDeviceToHost,
			stream
			)
		);
 
		// Синхронизируем только один раз
		CUDA_CHECK(cudaStreamSynchronize(stream));

		// Копия цветов
		auto copy_color = [&](Hardware_Color& Chw, base_color_c& C)
		{
			C.hemi = Chw.hemi;
			C.sun = Chw.sun;
 			
			C.rgb.set(Chw.rgb.x, Chw.rgb.y, Chw.rgb.z);
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

thread_local RayTracer GPURayTracer;
 

// Raytracer Initialize
void XRay::RayTrace::CUDA::RayTraceInitialize(base_lighting& L, u8 CurrentFlags)
{
	if (!GPURayTracer.isInitialized)
 		GPURayTracer.Init(MAX_RAYS_PER_GPU);
 
	GPURayTracer.current_flags = CurrentFlags; 
}

void XRay::RayTrace::CUDA::RayTraceAddRay(RayRecvestIndex& task, size_t index)
{
	if (!GPURayTracer.isInitialized)
 		GPURayTracer.Init(MAX_RAYS_PER_GPU);
 
	GPURayTracer.WriteRayToBuffer(task, index);
}

void XRay::RayTrace::CUDA::RayTraceRun(size_t max_rays)
{
	if (!GPURayTracer.isInitialized)
 		GPURayTracer.Init(MAX_RAYS_PER_GPU);
 
	// Чистим цвета 
	GPURayTracer.colors.clear();
	GPURayTracer.TraceRaysNew(max_rays);
}

xr_vector<base_color_c>& XRay::RayTrace::CUDA::RayTraceResult()
{
	return GPURayTracer.GetColors();
}

