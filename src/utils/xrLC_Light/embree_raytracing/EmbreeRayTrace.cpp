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
// можно и 0.10f Было раньше так
float EmbreeEnergyMAX = 0.01f;

struct RayQueryContext
{
	RTCRayQueryContext context;
	Fvector B;

 	Face* skip			  = 0;
	vecFace* static_dummy = nullptr;
	vecFace* transp_dummy = nullptr;
 	float energy = 1.0f;
};
 
// Сделать потом переключалку
bool CalculateEnergy(RayQueryContext* ctxt, RTCHit* hit, Face* F, Fvector& B)
{
	const b_material& M = inlc_global_data()->materials()[F->dwMaterial];
	const b_texture& T  = inlc_global_data()->textures()[M.surfidx];

	// barycentrics (без Fvector, сразу в скаляры)
	float Barry0 = 1.0f - hit->u - hit->v;
 
	// UV сразу float
	const Fvector2* cuv = F->getTC0();
	float u = cuv[0].x * Barry0 + cuv[1].x * hit->u + cuv[2].x * hit->v;
	float v = cuv[0].y * Barry0 + cuv[1].y * hit->u + cuv[2].y * hit->v;
  
	int U = (int) floor(u * float(T.dwWidth) + .5f);
	int V = (int) floor(v * float(T.dwHeight) + .5f);
	U %= T.dwWidth;		if (U < 0) U += T.dwWidth;
	V %= T.dwHeight;	if (V < 0) V += T.dwHeight;

	// fetch pixel
	const uint32_t* raw = static_cast<const uint32_t*>(*T.pSurface);
	uint32_t pixel   = raw[V * T.dwWidth + U];
	uint32_t pixel_a = (pixel >> 24) & 0xFF;

	// LUT вместо деления и sqr
	float a = float(pixel_a) / 255.f;
	float opacity = 1.f - a * a;
	ctxt->energy *= opacity;
	if (ctxt->energy < EmbreeEnergyMAX)
		return false;

	return true;
}
 
void FilterRayTraceOpaque(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;
	Face* F = (*ctxt->static_dummy)[hit->primID];
	if (F != ctxt->skip)
	{
		ctxt->energy = 0;
		args->valid[0] = -1; // Приехали
		return;
	}
	args->valid[0] = 0;		 // Продолжаем Trace
}
 
void FilterRaytraceTransparent(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;

	// Собрать все
	Face* F = (*ctxt->transp_dummy)[hit->primID];
	if (F != ctxt->skip && !CalculateEnergy(ctxt, hit, F, ctxt->B))
	{
		ctxt->energy = 0;
		args->valid[0] = -1; // Приехали
		return;
	}
	args->valid[0] = 0;		 // Продолжаем Trace
}

float EmbreeRayTraceModel::RaytraceEmbreeProcess(Fvector& P, Fvector& N, float range, void* skip)
{
	// Структура для RayTracing
	RayQueryContext data_hits;
 	data_hits.skip = (Face*)skip;
	data_hits.energy = 1.0f;
	data_hits.static_dummy = &static_geom.dummy;
	data_hits.transp_dummy = &static_geom_transp.dummy;

	RTCRay ray;
	SetRay1(ray, P, N, 0.1f, range);

	RTCOccludedArguments args;
	rtcInitOccludedArguments(&args);

	RTCRayQueryContext context;
	rtcInitRayQueryContext(&context);
	 
	// SET CONTEXT
	data_hits.context = context;
	args.context = &data_hits.context; 			 
	rtcOccluded1(IntelScene, &ray, &args);
 
	return data_hits.energy;
}
 
// LOADING GEOMETRY
 
size_t GetMemory()
{
	size_t used, free, reserved;
	vminfo(&free, &reserved, &used);
	return used;
}

static xrCriticalSection csEmbree;
 
// Loading Common 
void LoadGeomBuffer(RTCDevice& EmbreeDevice, RTCGeometry& geom, RTCBuildQuality& quality, bool FilterTransp, TriangleContainer& geom_buffer)
{
	geom = rtcNewGeometry(EmbreeDevice, RTC_GEOMETRY_TYPE_TRIANGLE);
	rtcSetGeometryBuildQuality(geom, quality);

	if (FilterTransp)
		rtcSetGeometryOccludedFilterFunction(geom, &FilterRaytraceTransparent);
	else
		rtcSetGeometryOccludedFilterFunction(geom, &FilterRayTraceOpaque);

	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, geom_buffer.vertex().data(), 0, sizeof(Fvector), geom_buffer.vertex().size());
	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, geom_buffer.faces().data(), 0, sizeof(Triangle), geom_buffer.faces().size());

	rtcCommitGeometry(geom);
};

void EmbreeRayTraceModel::CommitScene()
{
	if (gCompilerMode.EmbreeBVHCompact) scene_flags = scene_flags | RTC_SCENE_FLAG_COMPACT;
	if (gCompilerMode.EmbreeBVHRobust)	scene_flags = scene_flags | RTC_SCENE_FLAG_ROBUST;

 	IntelScene = rtcNewScene(EmbreeDevice);
	rtcSetSceneFlags(IntelScene, scene_flags);

	if (static_geom.faces_cnt() > 0)
	{
		LoadGeomBuffer(EmbreeDevice, IntelGeometryNormal, scene_quality, false, static_geom);
		rtcAttachGeometryByID(IntelScene, IntelGeometryNormal, 0);
	}

	if (static_geom_transp.faces_cnt() > 0)
	{
		LoadGeomBuffer(EmbreeDevice, IntelGeometryTransp, scene_quality, true, static_geom_transp);
		rtcAttachGeometryByID(IntelScene, IntelGeometryTransp, 1);
	}
	rtcCommitScene(IntelScene);
}

void EmbreeRayTraceModel::InitializeGeometry()
{
	Phase("Embree: Initialize Geometry");
 	// Конструктор модели
 	BuildRaytraceModel();
	
	csEmbree.Enter();
	CommitScene();
	csEmbree.Leave();
}

void EmbreeRayTraceModel::InitializeGeometry_Model(xr_vector<FaceDataEmbree>& faces)
{
	BuildModel(faces);

	csEmbree.Enter();
	CommitScene();
	csEmbree.Leave();
}

void EmbreeRayTraceModel::RemoveGeometry()
{
	csEmbree.Enter();
 	if (IntelScene)			 rtcReleaseScene(IntelScene);
  	if (IntelGeometryTransp) rtcReleaseGeometry(IntelGeometryTransp);
	if (IntelGeometryNormal) rtcReleaseGeometry(IntelGeometryNormal);
 	csEmbree.Leave();

	static_geom.ClearAll();
	static_geom_transp.ClearAll();

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
  	Msg("* Intel Embree Releasing Start| Memory: %u mb", u32(GetMemory() / 1024 / 1024));
 	RemoveGeometry();
    Msg("* Intel Embree Releasing End| Memory: %u mb", u32(GetMemory() / 1024 / 1024));

	rtcReleaseDevice(EmbreeDevice);
}
 

const char* GetDeviceConfig()
{
	bool avx_test = CPU::ID().hasFeature(CPUFeature::AVX2);
	bool sse = CPU::ID().hasFeature(CPUFeature::SSE);

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
