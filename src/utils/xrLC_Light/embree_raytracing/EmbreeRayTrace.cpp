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
RTCGeometry IntelGeometryTransp = 0;
 
EmbreeData EmbreeMain;
 
// Сильно ускоряет Но не нужно сильно завышать вообще 0.01f желаетельно 
// Влияет на яркость на выходе (если близко к 0 будет занулятся)
// можно и 0.10f Было раньше так
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
};

// Предвычисленный LUT для прозрачности
alignas(64) static float opacityLUT[256];
struct OpacityInit {
	OpacityInit() {
		for (int i = 0; i < 256; i++) {
			float a = float(i) / 255.f;
			opacityLUT[i] = 1.f - a * a; // 1 - (a^2)
		}
	}
} initOpacity;

// Сделать потом переключалку
bool CalculateEnergy(RayQueryContext* ctxt, RTCHit* hit, Face* F, Fvector& B)
{
	const b_material& M = inlc_global_data()->materials()[F->dwMaterial];
	const b_texture& T = inlc_global_data()->textures()[M.surfidx];

	// barycentrics (без Fvector, сразу в скаляры)
	float b0 = 1.0f - hit->u - hit->v;
	float b1 = hit->u;
	float b2 = hit->v;

	// UV сразу float
	const Fvector2* cuv = F->getTC0();
	float u = cuv[0].x * b0 + cuv[1].x * b1 + cuv[2].x * b2;
	float v = cuv[0].y * b0 + cuv[1].y * b1 + cuv[2].y * b2;

	int U = int(u * T.dwWidth + 0.5f);
	int V = int(v * T.dwHeight + 0.5f);

	// fast wrap (если pow2, иначе можно без ветки)
	if ((T.dwWidth & (T.dwWidth - 1)) == 0)
		U &= (T.dwWidth - 1);
	else {
		if (U < 0) U += T.dwWidth;
		else if (U >= T.dwWidth) U %= T.dwWidth;
	}

	if ((T.dwHeight & (T.dwHeight - 1)) == 0)
		V &= (T.dwHeight - 1);
	else {
		if (V < 0) V += T.dwHeight;
		else if (V >= T.dwHeight) V %= T.dwHeight;
	}

	// fetch pixel
	const uint32_t* raw = static_cast<const uint32_t*>(T.pSurface);
	uint32_t pixel = raw[V * T.dwWidth + U];
	uint32_t pixel_a = (pixel >> 24) & 0xFF;

	// LUT вместо деления и sqr
	ctxt->energy *= opacityLUT[pixel_a];

	/* 
  	b_material& M = inlc_global_data()->materials()[F->dwMaterial];
	b_texture& T = inlc_global_data()->textures()[M.surfidx];

	// barycentric coords
	// note: W,U,V order
	B.set(1.0f - hit->u - hit->v, hit->u, hit->v);

	//// calc UV
	Fvector2* cuv = F->getTC0();
	Fvector2	uv;
	uv.x = cuv[0].x * B.x + cuv[1].x * B.y + cuv[2].x * B.z;
	uv.y = cuv[0].y * B.x + cuv[1].y * B.y + cuv[2].y * B.z;
	int U = iFloor(uv.x * float(T.dwWidth) + .5f);
	int V = iFloor(uv.y * float(T.dwHeight) + .5f);
	U %= T.dwWidth;		if (U < 0) U += T.dwWidth;
	V %= T.dwHeight;	if (V < 0) V += T.dwHeight;
	
	u32* raw	= static_cast<u32*>(T.pSurface);
	u32 pixel	= raw[V * T.dwWidth + U];
	u32 pixel_a = color_get_A(pixel);

	float opac = 1.f - _sqr(float(pixel_a) / 255.f);

	// Дополнение Контекста
	ctxt->energy *= opac;
	*/

	if (ctxt->energy < EmbreeEnergyMAX)
		return false;

	return true;
}

void FilterRayTraceOpaque(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;

	Face* F = EmbreeMain.static_geom.dummy[hit->primID];
	if (F == ctxt->skip)
	{
		args->valid[0] = 0;  return;
	}
 	ctxt->energy = 0;
	args->valid[0] = -1; // Приехали
}
 
void FilterRaytraceTransparent(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;

	// Собрать все
	Face* F = EmbreeMain.static_geom_transp.dummy[hit->primID]; 
	 
 	if (F != ctxt->skip && !CalculateEnergy(ctxt, hit, F, ctxt->B))
	{
 		ctxt->energy = 0;
		args->valid[0] = -1; 		return;
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

// Loading Common 
void LoadGeomBuffer(RTCGeometry& geom, RTCBuildQuality& quality, bool FilterTransp, TriangleContainer& geom_buffer)
{
	geom = rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE);
	rtcSetGeometryBuildQuality(geom, quality);

	if (FilterTransp)
		rtcSetGeometryOccludedFilterFunction(geom, &FilterRaytraceTransparent);
	else
		rtcSetGeometryOccludedFilterFunction(geom, &FilterRayTraceOpaque);

	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, geom_buffer.vertex().data(), 0, sizeof(Fvector), geom_buffer.vertex().size());
	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, geom_buffer.faces().data(), 0, sizeof(Triangle), geom_buffer.faces().size());

	rtcCommitGeometry(geom);
};

void EmbreeData::InitializeGeometry()
{
 	// Конструктор модели
 	EmbreeData::BuildRaytraceModel();
   	LoadGeomBuffer(IntelGeometryNormal, scene_quality, false, static_geom);
   	LoadGeomBuffer(IntelGeometryTransp, scene_quality, true, static_geom_transp);
 
	IntelScene = rtcNewScene(device);
	rtcSetSceneFlags(IntelScene, scene_flags);

 	rtcAttachGeometryByID(IntelScene, IntelGeometryNormal, 0);
	rtcAttachGeometryByID(IntelScene, IntelGeometryTransp, 1);

	rtcCommitScene(IntelScene);
}

void EmbreeData::RemoveGeometry(bool isDealloc)
{
 	if (isDealloc)
	{
 		rtcReleaseScene(IntelScene);
  		static_geom.ClearAll();
		static_geom_transp.ClearAll();
	}
	else
	{
		rtcReleaseScene(IntelScene);
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
  	InitializeGeometry( );
}

void EmbreeData::IntelEmbereUNLOAD()
{
 	Msg("* Intel Embree Releasing Start| Memory: %u mb", u32(GetMemory() / 1024 / 1024));
 	RemoveGeometry(true);
  	Msg("* Intel Embree Releasing End| Memory: %u mb", u32(GetMemory() / 1024 / 1024));
}
