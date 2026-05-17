#include "stdafx.h"

#include "EmbreeRayTrace.h"
#include "../../xrCore/Collision/xrCDB.h"

#include "xrLC_GlobalData.h"
#include "xrFace.h"
#include "xrDeflector.h"
#include "light_point.h"
#include "R_light.h"

#include <../xrForms/CompilersUI.h>
extern CompilersMode gCompilerMode;

// Важные параметры
// INTIALIZE GEOMETRY, SCENE QUALITY TYPE
// Инициализация Основных Фишек Embree

// INTEL DATA STRUCTURE
EmbreeRayTraceModel EmbreeMain;

// Сильно ускоряет Но не нужно сильно завышать вообще 0.01f желаетельно 
// Влияет на яркость на выходе (если близко к 0 будет занулятся)
#define EmbreeEnergyMAX 0.16f
 
// Сделать потом переключалку
inline bool CalculateEnergy(float& energy, float& hit_u, float& hit_v, Face* F)
{
	const b_material& M = inlc_global_data()->materials()[F->dwMaterial];
	const b_texture&  T = inlc_global_data()->textures()[M.surfidx];

	// barycentrics (без Fvector, сразу в скаляры)
	float Barry0 = 1.0f - hit_u - hit_v;

	// UV сразу float
	const Fvector2* cuv = F->getTC0();
	float u = cuv[0].x * Barry0 + cuv[1].x * hit_u + cuv[2].x * hit_v;
	float v = cuv[0].y * Barry0 + cuv[1].y * hit_u + cuv[2].y * hit_v;

	int U = (int)floor(u * float(T.dwWidth) + .5f);
	int V = (int)floor(v * float(T.dwHeight) + .5f);
	U %= T.dwWidth;		if (U < 0) U += T.dwWidth;
	V %= T.dwHeight;	if (V < 0) V += T.dwHeight;

	// fetch pixel
	const uint32_t* raw = static_cast<const uint32_t*>(*T.pSurface);
	uint32_t pixel = raw[V * T.dwWidth + U];
	uint32_t pixel_a = (pixel >> 24) & 0xFF;
 
	// LUT вместо деления и sqr
	float a = float(pixel_a) / 255.f;
	float opacity = 1.f - (a * a);
	energy *= opacity;

	if (energy < EmbreeEnergyMAX)
		energy = 0.f;
 	 
	return energy > EmbreeEnergyMAX;
}

void FilterRayTraceOpacue(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	UserGeomData* UD = (UserGeomData*)args->geometryUserPtr;

  	for (auto i = 0; i < args->N; i++)
	{
		if (!args->valid[i]) continue;								// Для пакетных обезательно иначе полосы !

		u32& primID = RTCHitN_primID(args->hit, args->N, i);
		auto& F = UD->dummys[primID];
		if (F == ctxt->skip[i]) continue;

		ctxt->energy[i] = 0;
	}
}

#define MAX_HITS 4
void FilterRayTraceTransp(const struct RTCFilterFunctionNArguments* args)
{
 	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	UserGeomData* UD	  = (UserGeomData*)args->geometryUserPtr;
  	for (auto i = 0; i < args->N; i++)
	{
		if (!args->valid[i]) continue;								// Для пакетных обезательно иначе полосы !
	
 		u32& primID = RTCHitN_primID(args->hit, args->N, i);
		auto& F = UD->dummys[primID];
		if (F == ctxt->skip[i]) continue;

		float& hit_u = RTCHitN_u(args->hit, args->N, i);
		float& hit_v = RTCHitN_v(args->hit, args->N, i);
	
		// Собираем только 8м хитов остальные игнорим
		if (CalculateEnergy(ctxt->energy[i], hit_u, hit_v, F) && ctxt->hits_result[i]++ < MAX_HITS)
		{
			args->valid[i] = 0;
		}
	}
}
 
thread_local RTCOccludedArguments args;
thread_local RayQueryContext data_hits;
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


void EmbreeRayTraceModel::RaytrraceRayPack(xr_vector< RayTask >& rays)
{ 
	auto ApplyColor = [](RayTask& Ray, float& Energy)
	{
 		 if (Ray.type  == eDefRgb)
			 Ray.Cptr->rgb.add(Ray.attention * Energy);
		 else if (Ray.type == eDefSun)
			 Ray.Cptr->sun += Ray.attention * Energy;
		 else if (Ray.type == eDefHemi)
			 Ray.Cptr->hemi += Ray.attention * Energy;
	};

	bool usePackedRays8x = CPU::ID().hasFeature(CPUFeature::AVX2) && gCompilerMode.EmbreeRays8; 
	if (usePackedRays8x)
	{
		thread_local RTCOccludedArguments   args;
		rtcInitOccludedArguments(&args);

		thread_local RayQueryContext		ctxt;
		rtcInitRayQueryContext(&ctxt);

		// Set Args
		args.context = &ctxt;
		args.flags	 = RTC_RAY_QUERY_FLAG_COHERENT;

		thread_local alignas(32) RTCRayHit8 rays8;
		thread_local alignas(32) int		valid[8];
		for (u32 rayID = 0; rayID < rays.size(); rayID += 8)  
		{
			for (auto i = 0; i < 8; i++)
			{
				if (rayID + i >= rays.size()) { valid[i] = 0; continue; }
				auto& ray = rays[rayID + i];
				valid[i] = -1;
				ctxt.energy[i] = 1;
				ctxt.hits_result[i] = 0;
				ctxt.skip[i] = ray.Skip;
				SetRay8(rays8, i, ray.wP, ray.wN, 0.1f, ray.Range);
			}
			rtcOccluded8(valid, EmbreeMain.IntelScene, &rays8.ray, &args); // args
			for (auto i = 0; i < 8; i++)
			{
				if (rayID + i >= rays.size()) { continue; }
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

 
// LOADING GEOMETRY
static xrCriticalSection csEmbree;

// Loading Common 
void LoadGeomBuffer(RTCGeometry& geom, TriangleContainer& geom_buffer, bool isTransp)
{
	geom = rtcNewGeometry(EmbreeDevice, RTC_GEOMETRY_TYPE_TRIANGLE);
 
	if (isTransp)
	{
 		rtcSetGeometryOccludedFilterFunction(geom, &FilterRayTraceTransp);
 	}
	else
	{
 		rtcSetGeometryOccludedFilterFunction(geom, &FilterRayTraceOpacue);
 	}

	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, geom_buffer.vertex().data(), 0, sizeof(Fvector), geom_buffer.vertex().size());
	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, geom_buffer.faces().data(), 0, sizeof(Triangle), geom_buffer.faces().size());

  	UserGeomData* data = new UserGeomData();	// Кастомдата для треугольников чтобы не записывать в ctxt
	data->dummys = geom_buffer.dummy;
	rtcSetGeometryUserData(geom, data);
  
	rtcCommitGeometry(geom);
};


void EmbreeRayTraceModel::AttachGeomToScene(bool isMain)
{
	IntelScene = rtcNewScene(EmbreeDevice);
	rtcSetSceneFlags(IntelScene, scene_flags);
 	rtcSetSceneBuildQuality(IntelScene, scene_quality);

 	if (opacue_geom.faces_cnt() > 0)
	{
		LoadGeomBuffer(IntelGeometryNormal, opacue_geom, false);
		rtcAttachGeometry(IntelScene, IntelGeometryNormal);
		rtcReleaseGeometry(IntelGeometryNormal);
	}

	if (transp_geom.faces_cnt() > 0)
	{
		LoadGeomBuffer(IntelGeometryTransp, transp_geom, true);
		rtcAttachGeometry(IntelScene, IntelGeometryTransp);
		rtcReleaseGeometry(IntelGeometryTransp);
	}

	if (isMain)
		BuildRayTraceModel_Instaced();

	rtcCommitScene(IntelScene);
}

void EmbreeRayTraceModel::InitializeGeometry()
{
	if (gCompilerMode.EmbreeBVHCompact) scene_flags = scene_flags | RTC_SCENE_FLAG_COMPACT;
	if (gCompilerMode.EmbreeBVHRobust)	scene_flags = scene_flags | RTC_SCENE_FLAG_ROBUST;

	Phase("Embree: Initialize Geometry");


	// Собираем треугольники (чистим от дублей)
	BuildRayTraceModel(); // Сборка Геометрии

	// Конструктор модели
 	csEmbree.Enter();
  	AttachGeomToScene(true);  // Embree-Loading
	csEmbree.Leave();
}

void EmbreeRayTraceModel::InitializeGeometry_Model(xr_vector<FaceDataEmbree>& faces)
{
	if (gCompilerMode.EmbreeBVHCompact) scene_flags = scene_flags | RTC_SCENE_FLAG_COMPACT;
	if (gCompilerMode.EmbreeBVHRobust)	scene_flags = scene_flags | RTC_SCENE_FLAG_ROBUST;

	// Собираем треугольники (чистим от дублей)
	BuildMU_Model(faces); // Сборка Геометрии

	csEmbree.Enter();
	AttachGeomToScene(false);  // Embree-Loading
 	csEmbree.Leave();
}

void EmbreeRayTraceModel::RemoveGeometry()
{
	csEmbree.Enter();
	if (IntelScene)			 rtcReleaseScene(IntelScene);
  	if (IntelGeometryNormal) rtcReleaseGeometry(IntelGeometryNormal);
	if (IntelGeometryTransp) rtcReleaseGeometry(IntelGeometryTransp);

	csEmbree.Leave();

	opacue_geom.ClearAll();
	transp_geom.ClearAll();
	IntelScene = 0;
}

// Embree Device (Должен быть один)
void InitializeEmbreeDevice()
{
	if (isDeviceInitialized)		return;

	auto fError = [](void* userPtr, enum RTCError code, const char* str)
	{
		R_ASSERT2(false, str);
	};

	EmbreeDevice = rtcNewDevice(GetDeviceConfig()); 
	rtcSetDeviceErrorFunction(EmbreeDevice, fError, nullptr);

	isDeviceInitialized = true;
}

void EmbreeRayTraceModel::IntelEmbereUnloadAll()
{
 	RemoveGeometry();
	rtcReleaseDevice(EmbreeDevice);
}
 
const char* GetDeviceConfig()
{
	bool avx_test	= CPU::ID().hasFeature(CPUFeature::AVX2);
	bool sse		= CPU::ID().hasFeature(CPUFeature::SSE);

	string128 state;
	sprintf(state, "- Intilized Intel Embree %s - %s", RTC_VERSION_STRING, avx_test ? "avx" : sse ? "sse" : "default");
	Status(state);

	const char* config = "";
	if (avx_test)
		config = "isa=avx2";
	else if (sse)
		config = "isa=sse4.2";
	else
		config = "isa=sse2";

	return config;
}

void EmbreeInstancedModel::InitializeModel(xr_vector<FaceDataEmbree>& faces)
{
	TriangleContainer geom_builder_opacue;
	TriangleContainer geom_builder_transp;
	for (auto& F : faces)
	{
		Face* Fc = (Face*)F.ptr;
		auto& geom = Fc->flags.bOpaque ? geom_builder_opacue : geom_builder_transp;
		geom.AddFaceRaw((Face*)F.ptr, F.v1, F.v2, F.v3);
 	}
	geom_builder_opacue.useMsg = false;
	geom_builder_transp.useMsg = false;

	geom_builder_opacue.RemoveDublicates();
	geom_builder_transp.RemoveDublicates();

 	InstaceScene = rtcNewScene(EmbreeDevice);
 	rtcSetSceneBuildQuality(InstaceScene, RTC_BUILD_QUALITY_HIGH);

	if (geom_builder_opacue.faces_cnt() > 0)
	{
		LoadGeomBuffer(GeometryOpacue, geom_builder_opacue, false);
		rtcAttachGeometry(InstaceScene, GeometryOpacue);
		rtcReleaseGeometry(GeometryOpacue);
	}

	if (geom_builder_transp.faces_cnt() > 0)
	{
		LoadGeomBuffer(GeometryTransp, geom_builder_transp, true);
		rtcAttachGeometry(InstaceScene, GeometryTransp);
		rtcReleaseGeometry(GeometryTransp);
	}

	rtcCommitScene(InstaceScene);
}

void EmbreeInstancedModel::SetInstance(RTCScene scene, Fmatrix& xform)
{
	// ----------------------------------------------------
	// Instace Geometry Loading
	// ----------------------------------------------------
	RTCGeometry inst = rtcNewGeometry(EmbreeDevice, RTC_GEOMETRY_TYPE_INSTANCE);
	rtcSetGeometryInstancedScene(inst, InstaceScene);

	float matrix[16];
 	ConvertMatrix(xform, matrix);

	rtcSetGeometryTransform(inst, 0, RTC_FORMAT_FLOAT4X4_COLUMN_MAJOR, matrix);

	// ----------------------------------------------------
	// Commit instance
	// ----------------------------------------------------
	rtcCommitGeometry(inst);
 	rtcAttachGeometry(scene, inst);
 	rtcReleaseGeometry(inst);
}
