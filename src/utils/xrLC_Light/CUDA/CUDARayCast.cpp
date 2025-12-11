#include "stdafx.h"
#include "CUDARayCast.h"
#include "CUDAContext.h"
#include "CUDAGeometryBuilder.h"
#include "../xrLC_GlobalData.h"
 
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

#include <xrDeflector.h>
#include "Vector3HW.h"

#include <optix_function_table_definition.h>
struct OPTICK_Params
{
	OptixTraversableHandle handle;

	unsigned char		flags;
	Hardware_Raytask*	rays;
	Hardware_Color*		colors;		// Раньше rays == colors

	Hardware_Lighting*	lights;
	int					counts_lights;

	CDeflector_GPU*		GpuDeflectors;
	bool				DeflectorsBig;
};

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
	 
 	u8  current_flags = 0;

	// Заполнять после вызова StartRayTracing (чтобы индекс начинался с 0) (при каждой новой стадии освещения)
	void WriteRayToBuffer(RayRecvestIndex& Task, size_t INDEX)
	{
 		h_rays[INDEX] =
		{
			.Position = make_float3(Task.P.x, Task.P.y, Task.P.z),
			.Direction = make_float3(Task.N.x, Task.N.y, Task.N.z)
		};
  	}
	
	xr_vector<base_color_c> colors;

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
			float3 temp_rgb = Chw.get_rgb_f32();
			C.rgb.set(temp_rgb.x, temp_rgb.y, temp_rgb.z);
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


class DeflectorTraces
{
	CUstream	  stream;					// Отдельный стрим

	// Lighting 
	int					size_lights;
	Hardware_Lighting*  h_lights;			// CPU alloc
	Hardware_Lighting*  d_lights;			// GPU alloc
 
	// Parrrams (.cu export __constant__ Params g_params; )
	OPTICK_Params* h_params;				// CPU alloc
	OPTICK_Params* d_params;				// GPU alloc)

	// Deflector Data

	CDeflector_GPU* h_deflector;			// В последующем используем для вычистки 
	CDeflector_GPU* d_deflector;
	 
	bool isInitialized = false;
public:
	~DeflectorTraces()
	{
		if (h_params) cudaFreeHost(h_params);
 		if (d_params) cudaFree(d_params);
  	}
	 
	void Init()
	{
		CUDA_CHECK(cudaMallocHost(&h_deflector, sizeof(CDeflector_GPU) * 256 * 1024));										// Host Alloc
		CUDA_CHECK(cudaMalloc(&d_deflector, sizeof(CDeflector_GPU) * 256 * 1024));										// Host Alloc

 		CUDA_CHECK(cudaStreamCreate(&stream));

		// parrams
		CUDA_CHECK(cudaMallocHost(&h_params, sizeof(OPTICK_Params)));										// Host Alloc
		CUDA_CHECK(cudaMalloc(&d_params, sizeof(OPTICK_Params)));											// Device Alloc
		 
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

		CUDA_CHECK(cudaMalloc(&d_lights, sizeof(Hardware_Lighting) * numLights));
		CUDA_CHECK(cudaMemcpy(d_lights, h_lights, sizeof(Hardware_Lighting) * numLights, cudaMemcpyHostToDevice));
		size_lights = numLights;
	}
	 
	int CurrentDeflectors = 0;
	void SetDeflectors(CDeflector& D)
	{
		UVTriGPU* h_deflectors_quads		= nullptr;	// CPU Alloc
		UVTriGPU* d_deflectors_quads		= nullptr;	// GPU Alloc

		Hardware_Color* h_deflectors_colors = nullptr;	// CPU Alloc
		Hardware_Color* d_deflectors_colors = nullptr;	// GPU Alloc

		unsigned char* h_deflectors_markers = nullptr;	// CPU Alloc
		unsigned char* d_deflectors_markers = nullptr;	// GPU Alloc

		// Quads
		auto SizeDeflectorUV = sizeof(UVTriGPU) * D.UVpolys.size();

 		CUDA_CHECK(cudaMallocHost(&h_deflectors_quads, SizeDeflectorUV) );							// Host Alloc


		int IndexUV = 0;
		for (auto& O : D.UVpolys)
		{
			h_deflectors_quads[IndexUV].N.set( VPUSH( O.owner->N ) );

			h_deflectors_quads[IndexUV].uv[0].set(O.uv[0].x, O.uv[0].y);
			h_deflectors_quads[IndexUV].uv[1].set(O.uv[1].x, O.uv[1].y);
			h_deflectors_quads[IndexUV].uv[2].set(O.uv[2].x, O.uv[2].y);
			
			auto V1 = O.owner->v[0]; auto V2 = O.owner->v[1]; auto V3 = O.owner->v[2];

			h_deflectors_quads[IndexUV].V[0].P.set(VPUSH(V1->P));
			h_deflectors_quads[IndexUV].V[1].P.set(VPUSH(V2->P));
			h_deflectors_quads[IndexUV].V[2].P.set(VPUSH(V3->P));
 
			h_deflectors_quads[IndexUV].V[0].N.set(VPUSH(V1->N));
			h_deflectors_quads[IndexUV].V[1].N.set(VPUSH(V2->N));
			h_deflectors_quads[IndexUV].V[2].N.set(VPUSH(V3->N));
 
			// Копия цветов
			auto copy_color = [&](Hardware_Color& Chw, base_color& C)
				{
					base_color_c cnew;	
					C._get(cnew);

					Chw.hemi = cnew.hemi;
					Chw.sun  = cnew.sun;
					Chw.set_rgb_f32( VPUSH(cnew.rgb ) );
  				};
			 
			copy_color(h_deflectors_quads[IndexUV].V[0].C, V1->C);
			copy_color(h_deflectors_quads[IndexUV].V[1].C, V2->C);
			copy_color(h_deflectors_quads[IndexUV].V[2].C, V3->C);

			IndexUV++;
  		}
  
		// Copy Quads 

		// Msg("GPU Memory Need: %u kb", SizeDeflectorUV / 1024 );
		CUDA_CHECK(cudaMalloc(&d_deflectors_quads, SizeDeflectorUV));
 		CUDA_CHECK(cudaMemcpy(d_deflectors_quads, h_deflectors_quads, SizeDeflectorUV, cudaMemcpyHostToDevice));		// Device Copy
 
		// Copy Colors 
		auto SurfaceSize = D.layer.surface.size() * sizeof(Hardware_Color);
		CUDA_CHECK(cudaMallocHost(&h_deflectors_colors, SurfaceSize));							// Host Alloc

		auto IndexSURFACE = 0;
		for (auto& O : D.layer.surface)
		{
			base_color_c C; O._get(C);
			h_deflectors_colors[IndexSURFACE].hemi = C.hemi;
			h_deflectors_colors[IndexSURFACE].sun  = C.sun;
			h_deflectors_colors[IndexSURFACE].set_rgb_f32( VPUSH( C.rgb ) );
			IndexSURFACE++;
		}

		CUDA_CHECK(cudaMalloc(&d_deflectors_colors, SurfaceSize));
		CUDA_CHECK(cudaMemcpy(d_deflectors_colors, h_deflectors_colors, SurfaceSize, cudaMemcpyHostToDevice));		// Device Copy

		// Copy Markers
		auto MarkersSize = D.layer.marker.size() * sizeof(unsigned char);
		CUDA_CHECK(cudaMallocHost(&h_deflectors_markers, MarkersSize));							// Host Alloc

		int IndexMarker = 0;
		for (auto& O : D.layer.marker)
		{
			h_deflectors_markers[IndexMarker] = O;
 			IndexMarker++;
		}
		
		CUDA_CHECK(cudaMalloc(&d_deflectors_markers, MarkersSize));
		CUDA_CHECK(cudaMemcpy(d_deflectors_markers, h_deflectors_markers, MarkersSize, cudaMemcpyHostToDevice));		// Device Copy

 		// В самом последнем 
		h_deflector[CurrentDeflectors].Width			= D.layer.width;
		h_deflector[CurrentDeflectors].Height			= D.layer.height;
 		h_deflector[CurrentDeflectors].marker_size		= D.layer.marker.size();
		h_deflector[CurrentDeflectors].surfaces_size	= D.layer.surface.size();
		h_deflector[CurrentDeflectors].UVTrisSize		= D.UVpolys.size();
 		h_deflector[CurrentDeflectors].normal.set(VPUSH(D.normal));
 		h_deflector[CurrentDeflectors].UVTris			= d_deflectors_quads;
		h_deflector[CurrentDeflectors].surfaces			= d_deflectors_colors;
		h_deflector[CurrentDeflectors].marker			= d_deflectors_markers;
		
		// Copy To GPU
		// CUDA_CHECK(cudaMalloc(&d_deflector[CurrentDeflectors], sizeof(CDeflector_GPU)));											// Device Alloc
		CUDA_CHECK(cudaMemcpy(d_deflector + CurrentDeflectors, h_deflector + CurrentDeflectors, sizeof(CDeflector_GPU), cudaMemcpyHostToDevice));		// Device Copy

		CurrentDeflectors++;

		cudaFreeHost(h_deflectors_markers);
		cudaFreeHost(h_deflectors_colors);
		cudaFreeHost(h_deflectors_quads);
 	}

	void FreeDeflectors()
	{
		for (auto I = 0; I < CurrentDeflectors; I++)
		{
			CUDA_CHECK ( cudaFree(h_deflector[I].surfaces) );
			CUDA_CHECK ( cudaFree(h_deflector[I].marker) ) ;
			CUDA_CHECK ( cudaFree(h_deflector[I].UVTris) );
		}

		CurrentDeflectors = 0;

		CUDA_CHECK(cudaFreeHost(h_deflector));										// Host Alloc
		CUDA_CHECK(cudaFree(d_deflector));

		CUDA_CHECK(cudaMallocHost(&h_deflector, sizeof(CDeflector_GPU) * 256 * 1024));										// Host Alloc
		CUDA_CHECK(cudaMalloc(&d_deflector, sizeof(CDeflector_GPU) * 256 * 1024));
	}

	// 

	void RunTracing()
	{
 		// Подготавливаем данные на хосте
		h_params[0] =
		{
			.handle = CommitedScene.tlasHandle,

			// Result Buffer
			.flags = 0,
			.rays = nullptr,
			.colors = nullptr,
			.lights = d_lights,
			.counts_lights = size_lights,
			.GpuDeflectors = d_deflector,
			.DeflectorsBig = false,
		};
		 
		// Копируем на устройство
		CUDA_CHECK(cudaMemcpyAsync(
			d_params,
			h_params,
			sizeof(OPTICK_Params),
			cudaMemcpyHostToDevice,
			stream
		));

		// Запускаем трассировку
		unsigned int WidthSize = CurrentDeflectors;		// Выстраивается кол-во тасков
 		unsigned int HeightSize = 1;
		unsigned int DepthSize = 1;

		OPTIX_CHECK(optixLaunch
		(
			optixContext.GetPipeline(),
			stream,
			reinterpret_cast<CUdeviceptr> (d_params),
			sizeof(OPTICK_Params),
			&optixContext.GetSBT(),
			WidthSize, HeightSize, DepthSize  // Запускаем N лучей
		));
 
		// Синхронизируем только один раз
		CUDA_CHECK(cudaStreamSynchronize(stream));
	}

	void RunTracingBig()
	{
		// Подготавливаем данные на хосте
		h_params[0] =
		{
			.handle = CommitedScene.tlasHandle,

			// Result Buffer
			.flags = 0,
			.rays = nullptr,
			.colors = nullptr,
			.lights = d_lights,
			.counts_lights = size_lights,
			.GpuDeflectors = d_deflector,
			.DeflectorsBig = true,
		};

		// Копируем на устройство
		CUDA_CHECK(cudaMemcpyAsync(
			d_params,
			h_params,
			sizeof(OPTICK_Params),
			cudaMemcpyHostToDevice,
			stream
		));

		// Запускаем трассировку
		unsigned int WidthSize = CurrentDeflectors;		// Выстраивается кол-во тасков
		unsigned int HeightSize = 1;
		unsigned int DepthSize = 1;

		OPTIX_CHECK(optixLaunch
		(
			optixContext.GetPipeline(),
			stream,
			reinterpret_cast<CUdeviceptr> (d_params),
			sizeof(OPTICK_Params),
			&optixContext.GetSBT(),
			WidthSize, HeightSize, DepthSize  // Запускаем N лучей
		));

		// Синхронизируем только один раз
		CUDA_CHECK(cudaStreamSynchronize(stream));
	}
};


static DeflectorTraces GPUDeflectorTrace;

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

static bool isInitlized = false;

void XRay::RayTrace::CUDA::RayTraceDeflector(CDeflector& D)
{
	if (!isInitlized)
	{
		isInitlized = true;
		GPUDeflectorTrace.Init();
	}
 	GPUDeflectorTrace.SetDeflectors(D);
}

void XRay::RayTrace::CUDA::RayTraceDeflectorsAll()
{
	GPUDeflectorTrace.RunTracing();
}
 
void XRay::RayTrace::CUDA::RayTraceDeflectorsFree()
{
	GPUDeflectorTrace.FreeDeflectors();
}


