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
OptixMeshBuffers CommitedScene;
static CUstream cudaStream = nullptr;

// Lighting 
int					  size_lights;
Hardware_Lighting*	  gpu_lights   = nullptr;			// GPU alloc

int					  size_faces;
Hardware_FaceData*	  gpu_faces    = nullptr;

int					  size_textures;
Hardware_TextureData* gpu_textures = nullptr;

struct TextureDataCPU
{
	xr_vector<unsigned char> alpha;
	u32 Width;
	u32 Height;
};

void XRay::RayTrace::CUDA::InitializeLights()
{
	auto Lights = lc_global_data()->L_static();

	enum eType : u16
	{
		eSun,
		eHemi,
		eRGB
	};

	auto Light = [&](R_Light& L, eType type)
		{
			Hardware_Lighting cuL;
			cuL.type = L.type;
			cuL.light_type = type;
			cuL.diffuse = { L.diffuse.x, L.diffuse.y, L.diffuse.z };
			cuL.position = { L.position.x, L.position.y, L.position.z };
			cuL.direction = { L.direction.x, L.direction.y, L.direction.z };
			cuL.range = L.range;
			cuL.range2 = L.range2;
			cuL.falloff = L.falloff;
			cuL.attenuation0 = L.attenuation0;
			cuL.attenuation1 = L.attenuation1;
			cuL.attenuation2 = L.attenuation2;
			cuL.energy = L.energy;
			return cuL;
		};

	u32 numLights = Lights.rgb.size() + Lights.hemi.size() + Lights.sun.size();

	Hardware_Lighting* h_lights = nullptr;			// CPU alloc
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

	CUDA_CHECK(cudaMalloc(&gpu_lights, sizeof(Hardware_Lighting) * numLights));
	CUDA_CHECK(cudaMemcpy(gpu_lights, h_lights, sizeof(Hardware_Lighting) * numLights, cudaMemcpyHostToDevice));

	cudaFreeHost(h_lights);
	size_lights = numLights;
}

void XRay::RayTrace::CUDA::InitializeFaces(xr_vector<Face*> Faces)
{
	Hardware_FaceData* faces_host = nullptr;
	int alloc_size = Faces.size() * sizeof(Hardware_FaceData);
 	CUDA_CHECK ( cudaMallocHost(&faces_host, alloc_size) );
	
	int IndexFace = 0;
	for (auto& F : Faces)
	{
 		unsigned short surface = lc_global_data()->materials()[F->dwMaterial].surfidx;
		Hardware_FaceData& FaceGPU = faces_host[IndexFace];
		FaceGPU.bOpacue = F->flags.bOpaque;
		FaceGPU.bWater  = F->flags.bWater;

		FaceGPU.surfidx = surface;

		auto TC = F->getTC0();
		FaceGPU.TC0[0].set(TC[0].x, TC[0].y);
		FaceGPU.TC0[1].set(TC[1].x, TC[1].y);
		FaceGPU.TC0[2].set(TC[2].x, TC[2].y);

		IndexFace++;
	}

	CUDA_CHECK( cudaMalloc(&gpu_faces, alloc_size) );
	CUDA_CHECK( cudaMemcpy(gpu_faces, faces_host, alloc_size, cudaMemcpyHostToDevice) );
 	cudaFreeHost(faces_host);

	size_faces = Faces.size();

	Msg("[GPU] FACES[%u] Allocate : %llu kb", Faces.size(), Faces.size() * sizeof(Hardware_FaceData) / 1024);
}

void XRay::RayTrace::CUDA::InitializeTexturesAlpha()
{
 	u32 SizeT = lc_global_data()->textures().size();
 
	xr_vector<TextureDataCPU>  Textures;
 	for (auto& T : lc_global_data()->textures())
	{
		if (!T.pSurface || !T.bHasAlpha)
		{
 			Textures.push_back(TextureDataCPU());
 			
			TextureDataCPU& data = Textures.back();
			data.Width  = T.dwWidth;
			data.Height = T.dwHeight;
			data.alpha.clear();

			continue; 
		}

 		Textures.push_back(TextureDataCPU());

		TextureDataCPU& data = Textures.back();
		data.alpha.resize(T.dwWidth*T.dwHeight);
 		u8* ALPHA = data.alpha.data();

		const uint32_t* raw = static_cast<const uint32_t*>(T.pSurface);
		for (auto i = 0; i < T.dwWidth * T.dwHeight; i++)
		{
			uint32_t pixel = raw[i];
			uint8_t pixel_a = (pixel >> 24) & 0xFF;
			ALPHA[i] = pixel_a;
		}
		data.Width  = T.dwWidth;
		data.Height = T.dwHeight;
 	}

	// Textures (alpha only)
	xr_vector<Hardware_TextureData> cpu_tex_gpu(Textures.size());

	size_t allocated = 0;
	for (size_t i = 0; i < Textures.size(); ++i)
	{
		const auto& src = Textures[i];

		if (!src.alpha.size())
		{
			cpu_tex_gpu[i].pSurface = nullptr;
		}
		else
		{
			unsigned char* d_alpha = nullptr;
			CUDA_CHECK( cudaMalloc(&d_alpha, src.Width * src.Height * sizeof(uint8_t)) );

			CUDA_CHECK( cudaMemcpy(
				d_alpha,
				src.alpha.data(),
				src.Width * src.Height * sizeof(uint8_t),
				cudaMemcpyHostToDevice) );


			cpu_tex_gpu[i].pSurface = d_alpha;
			allocated += src.Width * src.Height;
		}

 		cpu_tex_gpu[i].width     = src.Width;
		cpu_tex_gpu[i].height    = src.Height;
	
	}
 
	CUDA_CHECK ( cudaMalloc((void**)&gpu_textures, cpu_tex_gpu.size() * sizeof(Hardware_TextureData)) );
	CUDA_CHECK(  cudaMemcpy(gpu_textures,
		cpu_tex_gpu.data(),
		cpu_tex_gpu.size() * sizeof(Hardware_TextureData),
		cudaMemcpyHostToDevice) );


	allocated += cpu_tex_gpu.size() * sizeof(Hardware_TextureData);
	size_textures = cpu_tex_gpu.size();

	Msg("[GPU] Textures[%u] Allocate : %llu kb", cpu_tex_gpu.size(), allocated / 1024);
}

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
 
	clMsg("Processing Memory: %u mb", GetHeapMemory() / 1024 / 1024);
	InitializeLights();
	InitializeTexturesAlpha();
}

// При завершении работы
void CleanupRayTracing()
{
	OptixContext::DestroyCudaStream(cudaStream);
	optixContext.Destroy();
}

class RayTracer
{
	// Colors (Result)
	Hardware_Color*		  h_colors;			// CPU alloc
	Hardware_Color*		  d_colors;			// GPU alloc

	// Positions Rays (Incoming)
	Hardware_Raytask*		h_rays;			// CPU alloc
	Hardware_Raytask*		d_rays;			// GPU alloc

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
			.Direction = make_float3(Task.N.x, Task.N.y, Task.N.z),
			.SkipFace = Task.FaseSkip
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
			.flags = current_flags,
			.rays = d_rays,
			.colors = d_colors,

			.lights = gpu_lights,
			.counts_lights = size_lights,

			.faces = gpu_faces,
			.count_faces = size_faces,

			.textures = gpu_textures,
			.count_textures = size_textures,
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

