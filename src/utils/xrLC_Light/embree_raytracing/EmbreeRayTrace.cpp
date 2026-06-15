#include "stdafx.h"

#include "EmbreeRayTrace.h"
#include "../../xrCore/Collision/xrCDB.h"

#include "xrLC_GlobalData.h"
#include "xrFace.h"
#include "xrDeflector.h"
#include "light_point.h"
#include "R_light.h"

#include "global_calculation_data.h"
#include <../xrForms/CompilersUI.h>
extern CompilersMode gCompilerMode;
extern global_claculation_data gl_data;

// Сильно ускоряет Но не нужно сильно завышать вообще 0.01f желаетельно
// Влияет на яркость на выходе (если близко к 0 будет занулятся)
#define EmbreeEnergyMAX 0.16f
thread_local RTCOccludedArguments args;
thread_local RayQueryContext data_hits;

// Main Traverser !
EmbreeRayTraceModel EmbreeMain;

// Сделать потом переключалку
inline bool CalculateEnergy(const b_texture& T, const Fvector2* cuv, float& energy, float& hit_u, float& hit_v)
{
	// barycentrics (без Fvector, сразу в скаляры)
	float Barry0 = 1.0f - hit_u - hit_v;

	// UV сразу float
	float u = cuv[0].x * Barry0 + cuv[1].x * hit_u + cuv[2].x * hit_v;
	float v = cuv[0].y * Barry0 + cuv[1].y * hit_u + cuv[2].y * hit_v;

	int U = (int)floor(u * float(T.dwWidth) + .5f);
	int V = (int)floor(v * float(T.dwHeight) + .5f);
	U %= T.dwWidth;
	if (U < 0)
	{
		U += T.dwWidth;
	}
	V %= T.dwHeight;
	if (V < 0)
	{
		V += T.dwHeight;
	}

	const u32* raw = static_cast<const u32*>(*T.pSurface);
	u32 pixel = raw[V * T.dwWidth + U];
	u32 pixel_a = (pixel >> 24) & 0xFF;

	// LUT вместо деления и sqr
	float a = float(pixel_a) / 255.f;
	float opacity = 1.f - (a * a);
	energy *= opacity;

	if (energy < EmbreeEnergyMAX)
	{
		energy = 0.f;
	}

	return energy > EmbreeEnergyMAX;
}

void FilterRayTraceOpacue(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	UserGeomData* UD = (UserGeomData*)args->geometryUserPtr;

	for (auto i = 0; i < args->N; i++)
	{
		if (!args->valid[i])
		{
			continue; // Для пакетных обезательно иначе полосы !
		}

		u32& primID = RTCHitN_primID(args->hit, args->N, i);
		auto& F = UD->dummys[primID];
		if (F == ctxt->skip[i])
		{
			continue;
		}

		ctxt->energy[i] = 0;
	}
}

#define MAX_HITS 4
void FilterRayTraceTransp(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	UserGeomData* UD = (UserGeomData*)args->geometryUserPtr;

	for (auto i = 0; i < args->N; i++)
	{
		if (!args->valid[i])
		{
			continue; // Для пакетных обезательно иначе полосы !
		}

		u32& primID = RTCHitN_primID(args->hit, args->N, i);
		auto F = UD->dummys[primID];
		if (F == ctxt->skip[i])
		{
			continue;
		}

		float& hit_u = RTCHitN_u(args->hit, args->N, i);
		float& hit_v = RTCHitN_v(args->hit, args->N, i);

		// Собираем только N хитов остальные игнорим
		if (ctxt->hits_result[i]++ >= MAX_HITS)
		{
			continue;
		}

		bool bSearching = false;
		if (UD->DummyType == 1)
		{
			auto _F = (FaceDataEmbree*)F;
			const b_material& M = gl_data.g_materials[_F->dwMaterial];
			const b_texture& T = gl_data.g_textures[M.surfidx];
			bSearching = CalculateEnergy(T, _F->getTC0(), ctxt->energy[i], hit_u, hit_v);
		}
		else if (UD->DummyType == 0)
		{
			auto _F = (Face*)F;
			const b_material& M = inlc_global_data()->materials()[_F->dwMaterial];
			const b_texture& T = inlc_global_data()->textures()[M.surfidx];
			// fetch pixel
			if (T.pSurface.Empty())
			{
				Msg("Texture[%s] is Emptry", T.name);
				ctxt->energy[i] = 0;
				continue;
			}

			bSearching = CalculateEnergy(T, _F->getTC0(), ctxt->energy[i], hit_u, hit_v);
		}

		args->valid[i] = bSearching ? 0 : -1;
	}
}

float EmbreeRayTraceModel::RaytraceEmbreeProcess(Fvector& P, Fvector& N, float range, void* skip)
{
	rtcInitRayQueryContext(&data_hits);
	rtcInitOccludedArguments(&args);

	data_hits.energy[0] = 1.0f;
	data_hits.hits_result[0] = 0;
	data_hits.skip[0] = skip;

	RTCRay Task;
	SetRay1(Task, P, N, 0.1f, range);

	// SET CONTEXT
	args.context = &data_hits;
	args.flags = RTC_RAY_QUERY_FLAG_INCOHERENT;
	rtcOccluded1(IntelScene, &Task, &args);
	return data_hits.energy[0];
}


void EmbreeRayTraceModel::RaytrraceRayPack(xr_vector<RayTask>& rays)
{
	auto ApplyColor = [](RayTask& Ray, float& Energy)
	{
		if (Ray.type == eDefRgb)
		{
			Ray.Cptr->rgb.add(Ray.attention * Energy);
		}
		else if (Ray.type == eDefSun)
		{
			Ray.Cptr->sun += Ray.attention * Energy;
		}
		else if (Ray.type == eDefHemi)
		{
			Ray.Cptr->hemi += Ray.attention * Energy;
		}
	};

	bool usePackedRays8x = CPU::ID().hasFeature(CPUFeature::AVX2) && gCompilerMode.EmbreeRays8;
	if (usePackedRays8x)
	{
		thread_local RTCOccludedArguments args;
		rtcInitOccludedArguments(&args);

		thread_local RayQueryContext ctxt;
		rtcInitRayQueryContext(&ctxt);

		// Set Args
		args.context = &ctxt;
		args.flags = RTC_RAY_QUERY_FLAG_COHERENT;

		thread_local alignas(32) RTCRay8 rays8;
		thread_local alignas(32) int valid[8];
		for (u32 rayID = 0; rayID < rays.size(); rayID += 8)
		{
			for (auto i = 0; i < 8; i++)
			{
				if (rayID + i >= rays.size())
				{
					valid[i] = 0;
					continue;
				}
				auto& ray = rays[rayID + i];
				valid[i] = -1;
				ctxt.energy[i] = 1;
				ctxt.hits_result[i] = 0;
				ctxt.skip[i] = ray.Skip;
				SetRay8(rays8, i, ray.wP, ray.wN, 0.1f, ray.Range);
			}
			rtcOccluded8(valid, EmbreeMain.IntelScene, &rays8, &args); // args
			for (auto i = 0; i < 8; i++)
			{
				if (rayID + i >= rays.size())
				{
					continue;
				}
				ApplyColor(rays[rayID + i], ctxt.energy[i]);
			}
		}
	}
	else
	{
		for (auto& ray : rays)
		{
			float energy = RaytraceEmbreeProcess(ray.wP, ray.wN, ray.Range, ray.Skip);
			ApplyColor(ray, energy);
		}
	}
}

// Filter Geometry Setup
void SetFilter(RTCGeometry geom, bool isTransp)
{
	if (isTransp)
	{
		rtcSetGeometryOccludedFilterFunction(geom, &FilterRayTraceTransp);
	}
	else
	{
		rtcSetGeometryOccludedFilterFunction(geom, &FilterRayTraceOpacue);
	}
}


// LOADING GEOMETRY
static xrCriticalSection csEmbree;


void EmbreeRayTraceModel::InitializeGeometry()
{
	Phase("Embree: Initialize Geometry");
	// Собираем треугольники (чистим от дублей)
	BuildRayTraceModel(); // Сборка Геометрии

	// Конструктор модели
	csEmbree.Enter();
	AttachGeomToScene(true, 0); // Embree-Loading
	csEmbree.Leave();
}

void EmbreeRayTraceModel::InitializeGeometry_Model(xr_vector<FaceDataEmbree>& faces)
{
	// Собираем треугольники (чистим от дублей)
	opacue_geom.ClearAll();
	transp_geom.ClearAll();
	for (auto& F : faces)
	{
		bool isOpacue = ((Face*)F.ptr)->flags.bOpaque;
		auto& buf = isOpacue ? opacue_geom : transp_geom;
		buf.AddFaceRaw((Face*)F.ptr, F.v1, F.v2, F.v3);
	}
	opacue_geom.useMsg = false;
	transp_geom.useMsg = false;
	opacue_geom.RemoveDublicates();
	transp_geom.RemoveDublicates();

	csEmbree.Enter();
	AttachGeomToScene(false, 0); // Embree-Loading
	csEmbree.Leave();
}


// Details Model
void EmbreeRayTraceModel::InitializeDetails(xr_vector<FaceDataEmbree>& faces)
{
	// Initialize Embree !
	InitializeEmbreeDevice();

	Phase("Embree: Initialize Geometry");

	opacue_geom.ClearAll();
	transp_geom.ClearAll();

	for (auto& F : faces)
	{
		auto& buf = F.bOpaque ? opacue_geom : transp_geom;
		buf.AddFaceRaw(&F, F.v1, F.v2, F.v3);
	}

	opacue_geom.RemoveDublicates(); // Обезательно вызывать иначе не будет Vertex, Tris (Убрал жрание памяти при создании)
	transp_geom.RemoveDublicates();

	// Конструктор модели
	csEmbree.Enter();
	AttachGeomToScene(true, 1); // User Diffined type == 1
	csEmbree.Leave();
}

void EmbreeRayTraceModel::RemoveGeometry()
{
	csEmbree.Enter();
	auto CleanUserData = [&](RTCGeometry geom)
	{
		UserGeomData* UD = (UserGeomData*)rtcGetGeometryUserData(geom);
		if (UD != nullptr)
		{
			UD->dummys.clear();
			UD->dummys.shrink_to_fit();
		}
	};

	if (IntelScene)
	{
		rtcReleaseScene(IntelScene);
	}
	if (IntelGeometryNormal)
	{
		CleanUserData(IntelGeometryNormal);
 		rtcReleaseGeometry(IntelGeometryNormal);
	}
	if (IntelGeometryTransp)
	{
		CleanUserData(IntelGeometryTransp);
		rtcReleaseGeometry(IntelGeometryTransp);
	}
	csEmbree.Leave();

	opacue_geom.ClearAll();
	transp_geom.ClearAll();
	IntelScene = 0;
}

void EmbreeRayTraceModel::IntelEmbereUnloadAll()
{
	RemoveGeometry();

	rtcReleaseDevice(EmbreeDevice);
	isDeviceInitialized = false;

	// AF: не нашел, где очищается, очищаю тут
	// se7kills: Инстансы только в мейн моделе 
	instanced.clear();
}

// Embree Device (Должен быть один)
RTCDevice EmbreeDevice;

void InitializeEmbreeDevice()
{
	if (isDeviceInitialized)
	{
		return;
	}

	auto fError = [](void* userPtr, enum RTCError code, const char* str)
	{
		R_ASSERT2(false, str);
	};

	EmbreeDevice = rtcNewDevice(GetDeviceConfig());
	rtcSetDeviceErrorFunction(EmbreeDevice, fError, nullptr);

	isDeviceInitialized = true;
}

const char* GetDeviceConfig()
{
	bool avx_test = CPU::ID().hasFeature(CPUFeature::AVX2);
	bool sse = CPU::ID().hasFeature(CPUFeature::SSE);

	string128 state;
	sprintf(state, "- Intilized Intel Embree %s - %s", RTC_VERSION_STRING, avx_test ? "avx" : sse ? "sse"
																								  : "default");
	Status(state);

	const char* config = "";
	if (avx_test)
	{
		config = "isa=avx2";
	}
	else if (sse)
	{
		config = "isa=sse4.2";
	}
	else
	{
		config = "isa=sse2";
	}

	return config;
}

void EmbreeRayTraceModel::UpdateSceneFlags()
{
	if (gCompilerMode.EmbreeBVHCompact)
	{
		scene_flags = scene_flags | RTC_SCENE_FLAG_COMPACT;
	}
	if (gCompilerMode.EmbreeBVHRobust)
	{
		scene_flags = scene_flags | RTC_SCENE_FLAG_ROBUST;
	}
}

EmbreeInstancedModel::~EmbreeInstancedModel()
{
	if (InstaceScene)
	{
		rtcReleaseScene(InstaceScene);
	}
	if (GeometryTransp)
	{
		rtcReleaseGeometry(GeometryTransp);
	}
	if (GeometryOpacue)
	{
		rtcReleaseGeometry(GeometryOpacue);
	}
}
