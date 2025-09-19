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
RTCSceneFlags scene_flags = RTC_SCENE_FLAG_NONE;
RTCBuildQuality scene_quality = RTC_BUILD_QUALITY_LOW;

RTCDevice device	= 0;
RTCScene IntelScene = 0;
 
RTCGeometry IntelGeometryNormal = 0;
RTCGeometry IntelGeometryMuModels = 0;

RTCGeometry IntelGeometryTransp = 0;
RTCGeometry IntelGeometryMuModelsTransp = 0;

EmbreeData EmbreeMain;
 
// Сильно ускоряет Но не нужно сильно завышать вообще 0.01f желаетельно 
// Влияет на яркость на выходе (если близко к 0 будет занулятся)
// можно и 0.10f Было раньше так
float EmbreeEnergyMAX = 0.01f;

struct RayQueryContext
{
	RTCRayQueryContext context;
	Fvector B;

 	Face* skip = 0;
	R_Light* Light = 0;
	float energy = 1.0f;
	u32 Hits = 0;
};
 
// Сделать потом переключалку
bool CalculateEnergy(Face* F, Fvector& B, float& energy, float u, float v)
{
	// Перемещаем начало луча немного дальше пересечения
	b_material& M = inlc_global_data()->materials()[F->dwMaterial];
	b_texture& T = inlc_global_data()->textures()[M.surfidx];
	 
	// barycentric coords
	// note: W,U,V order
	B.set(1.0f - u - v, u, v);

	//// calc UV
	Fvector2*   cuv = F->getTC0();
	Fvector2	uv;
	uv.x = cuv[0].x * B.x + cuv[1].x * B.y + cuv[2].x * B.z;
	uv.y = cuv[0].y * B.x + cuv[1].y * B.y + cuv[2].y * B.z;
	int U = iFloor(uv.x * float(T.dwWidth) + .5f);
	int V = iFloor(uv.y * float(T.dwHeight) + .5f);
	U %= T.dwWidth;		if (U < 0) U += T.dwWidth;
	V %= T.dwHeight;	if (V < 0) V += T.dwHeight;

	u32* raw = static_cast<u32*>(T.pSurface);
	u32 pixel = raw[V * T.dwWidth + U];
	u32 pixel_a = color_get_A(pixel);

	float opac = 1.f - _sqr(float(pixel_a) / 255.f);

	// Дополнение Контекста
	energy *= opac;
	if (energy < EmbreeEnergyMAX)
		return false;

	return true;
}

void FilterRayTraceOpaque(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;

	Face* F = hit->geomID == 0 ? EmbreeMain.static_geom.dummy[hit->primID] : hit->geomID == 1 ? EmbreeMain.murefs_geom.dummy[hit->primID] : nullptr;
	if (F == ctxt->skip)
	{
		args->valid[0] = 0; return;
	}
	ctxt->energy = 0;
	args->valid[0] = -1; // Приехали
}
 
void FilterRaytraceTransparent(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;

	// Собрать все
	Face* F = nullptr; 

	if (hit->geomID == 2)
		F = EmbreeMain.static_geom_transp.dummy[hit->primID]; 
	else 
		F = EmbreeMain.murefs_geom_transp.dummy[hit->primID];
	 
 	if (F != ctxt->skip && !CalculateEnergy(F, ctxt->B, ctxt->energy, hit->u, hit->v))
	{
 		ctxt->energy = 0;
 		return;
	}

 	args->valid[0] = 0;
}

float EmbreeData::RaytraceEmbreeProcess(R_Light& L, Fvector& P, Fvector& N, float range, void* skip)
{
	// Структура для RayTracing
	RayQueryContext data_hits;
	data_hits.Light = &L;
	data_hits.skip = (Face*)skip;
	data_hits.energy = 1.0f;
	data_hits.Hits = 0;
 
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

 
#include <xrMU_Model.h>
#include <xrMU_Model_Reference.h>

void LoadGeomBuffer(RTCGeometry& geom, RTCBuildQuality& quality, bool FilterTransp, TriangleContainer& geom_buffer)
{
 	geom = rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE);
	rtcSetGeometryBuildQuality(geom, quality);

	if (FilterTransp)
		rtcSetGeometryOccludedFilterFunction(geom, &FilterRaytraceTransparent);
	else 
		rtcSetGeometryOccludedFilterFunction(geom, &FilterRayTraceOpaque);

	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, geom_buffer.vertex().data(), 0, sizeof(VertexEmbree), geom_buffer.vertex().size());
	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, geom_buffer.faces().data(), 0, sizeof(TriEmbree), geom_buffer.faces().size());
	geom_buffer.hashTable.clear();

	rtcCommitGeometry(geom);
}

void EmbreeData::InitializeGeometry(size_t& geom_static_mem, size_t& geom_murefs_mem)
{
 	// Конструктор модели
 	EmbreeData::GetGlobalData(geom_static_mem, geom_murefs_mem);
	CTimer t; t.Start();
   	LoadGeomBuffer(IntelGeometryNormal, scene_quality, false, static_geom);
	LoadGeomBuffer(IntelGeometryMuModels, scene_quality, false, murefs_geom);
  	LoadGeomBuffer(IntelGeometryTransp, scene_quality, true, static_geom_transp);
	LoadGeomBuffer(IntelGeometryMuModelsTransp, scene_quality, true, murefs_geom_transp);
	Status("[Embree] Loading Buffers: [%u ms]", t.GetElapsed_ms());

//	Msg("Static MODELS Transp : %u, Opacue: %u", static_geom_transp.faces_v.size(), static_geom.faces_v.size());
//	Msg("MU MODELS Transp : %u, Opacue: %u", murefs_geom_transp.faces_v.size(), murefs_geom.faces_v.size());
}

size_t EmbreeData::AttachGeometrys(bool addMU)
{
	RemoveGeometry(false);
	CTimer t;
	t.Start();

	IntelScene = rtcNewScene(device);
	rtcSetSceneFlags(IntelScene, scene_flags);
 
 	isAttached = true;
  	rtcAttachGeometryByID(IntelScene, IntelGeometryNormal, 0);
	rtcAttachGeometryByID(IntelScene, IntelGeometryTransp, 2); 
	rtcAttachGeometryByID(IntelScene, IntelGeometryMuModels, 1);
	rtcAttachGeometryByID(IntelScene, IntelGeometryMuModelsTransp, 3);

	size_t start = GetMemory();
	rtcCommitScene(IntelScene);
	BVH_size = GetMemory() - start;
	 
	AditionalData("ST: %umb | MU: %umb | BVH: %u mb", Static_size / 1024 / 1024, MU_size / 1024 / 1024, BVH_size / 1024 / 1024);

	Status("[Embree] Attach Geoms : [%u ms]",t.GetElapsed_ms());

	return (GetMemory() - start);
}

void EmbreeData::RemoveGeometry(bool isDealloc)
{
 	if (isDealloc)
	{
 		rtcReleaseScene(IntelScene);
  		static_geom.ClearAll();
		static_geom_transp.ClearAll();
		murefs_geom.ClearAll();
		murefs_geom_transp.ClearAll();
		 
		BVH_size = 0;
		Static_size = 0;
		MU_size = 0;
	}
	else
	{
		rtcReleaseScene(IntelScene);
		BVH_size = 0;
	}
 	
	IntelScene = 0;
}

void errors_embree(void* userPtr, enum RTCError code, const char* str)
{
 	R_ASSERT2(false, str);
}

 
void EmbreeData::IntializeDevice()
{
	bool avx_test = CPU::ID.hasFeature(CPUFeature::AVX2);
	bool sse = CPU::ID.hasFeature(CPUFeature::SSE);

	const char* config = "";
	if (avx_test)
		config = "threads=16,isa=avx,verbose=0";
	else if (sse)
		config = "threads=16,isa=sse4.2,verbose=0";
	else
		config = "threads=16,isa=sse2,verbose=0";

	device = rtcNewDevice(config);

	rtcSetDeviceProperty(device, RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED, 0);
 	rtcSetDeviceErrorFunction(device, &errors_embree, NULL);

	string128 state;
	sprintf(state, "- Intilized Intel Embree %s - %s", RTC_VERSION_STRING, avx_test ? "avx" : sse ? "sse" : "default");
	Status(state);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_RAY_MASK_SUPPORTED", device, RTC_DEVICE_PROPERTY_RAY_MASK_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_BACKFACE_CULLING_ENABLED", device, RTC_DEVICE_PROPERTY_BACKFACE_CULLING_ENABLED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY4_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY4_SUPPORTED);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY8_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY8_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_IGNORE_INVALID_RAYS_ENABLED", device, RTC_DEVICE_PROPERTY_IGNORE_INVALID_RAYS_ENABLED);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_TASKING_SYSTEM", device, RTC_DEVICE_PROPERTY_TASKING_SYSTEM);
}

void EmbreeData::IntelEmbereLOAD()
{
	if (!isInitialized)
	{
		IntializeDevice();
		isInitialized = true;
	}

 	Msg("- Intel Embree Loading| Memory: %u mb", u32(GetMemory()/1024/1024) );
	if (gCompilerMode.EmbreeBVHCompact)
		scene_flags = scene_flags | RTC_SCENE_FLAG_COMPACT;
	if (gCompilerMode.EmbreeBVHRobust)
		scene_flags = scene_flags | RTC_SCENE_FLAG_ROBUST;
 	 
	IntelScene = rtcNewScene(device);
	rtcSetSceneFlags(IntelScene, scene_flags);

	// LOADING NORMAL GEOM
	// size_t geom_memory, refs_memory;
 	InitializeGeometry(Static_size, MU_size);
	AttachGeometrys(true);

	//size_t BVH = AttachGeometrys(true);
 	//AditionalData("ST: %umb | MU: %umb | BVH: %u mb", geom_memory / 1024 / 1024, refs_memory / 1024 / 1024, BVH / 1024 / 1024);
}

void EmbreeData::IntelEmbereUNLOAD()
{
 	Msg("* Intel Embree Releasing Start| Memory: %u mb", u32(GetMemory() / 1024 / 1024));
 	RemoveGeometry(true);
  	Msg("* Intel Embree Releasing End| Memory: %u mb", u32(GetMemory() / 1024 / 1024));
}
