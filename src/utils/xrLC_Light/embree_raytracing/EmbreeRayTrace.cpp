#include "stdafx.h"

#include "EmbreeRayTrace.h"
#include "../../xrcdb/xrcdb.h"

#include "xrLC_GlobalData.h"
#include "xrface.h"
#include "xrdeflector.h"
#include "light_point.h"
#include "R_light.h"

//Intel Code Start
#include <atomic>

// Важные параметры
// INTIALIZE GEOMETRY, SCENE QUALITY TYPE
// Инициализация Основных Фишек Embree

// INTEL DATA STRUCTURE
int LastGeometryID = RTC_INVALID_GEOMETRY_ID;
int LastGeometryIDTransp = RTC_INVALID_GEOMETRY_ID;

RTCSceneFlags scene_flags = RTC_SCENE_FLAG_NONE;
RTCBuildQuality scene_quality = RTC_BUILD_QUALITY_LOW;

RTCDevice device	= 0;
RTCScene IntelScene = 0;

RTCGeometry IntelGeometryNormal = 0;
 
/** NORMAL GEOM **/
Embree::VertexEmbree* verticesNormal = 0;
Embree::TriEmbree* trianglesNormal = 0;
xr_vector<void*> TriNormal_Dummys;
 
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
bool CalculateEnergy(base_Face* F, Fvector& B, float& energy, float u, float v)
{
	// Перемещаем начало луча немного дальше пересечения
	b_material& M = inlc_global_data()->materials()[F->dwMaterial];
	b_texture& T = inlc_global_data()->textures()[M.surfidx];

	// barycentric coords
	// note: W,U,V order
	B.set(1.0f - u - v, u, v);

	//// calc UV
	Fvector2*	cuv = F->getTC0();
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

void FilterRaytrace(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;

	R_ASSERT(hit->geomID == 0);

 	// Собрать все
	base_Face* F = (base_Face*)TriNormal_Dummys[hit->primID];
 	if (F == ctxt->skip || !F) // SKIP делаем для таких
	{
		args->valid[0] = 0; return;
	}

	if (F->flags.bOpaque)
	{
		ctxt->energy = 0; return;
	}

	if (!CalculateEnergy(F, ctxt->B, ctxt->energy, hit->u, hit->v))
	{
		// При нахождении любого хита сразу все попали в непрозрачный Face.
		ctxt->energy = 0;
 		return;
	}

 	args->valid[0] = 0; // Задаем чтобы продолжил поиск
}

float RaytraceEmbreeProcess(R_Light& L, Fvector& P, Fvector& N, float range, void* skip)
{
	// Структура для RayTracing
	RayQueryContext data_hits;
	data_hits.Light = &L;
	data_hits.skip = (Face*)skip;
	data_hits.energy = 1.0f;
	data_hits.Hits = 0;

	// se7kills : 07.02.2025 0.001 виноват BORDER 1 был поставил 4 
	RTCRay ray;
	Embree::SetRay1(ray, P, N, 0.001f, range);

	RTCOccludedArguments args;
	rtcInitOccludedArguments(&args);

	RTCRayQueryContext context;
	rtcInitRayQueryContext(&context);
	 
	// SET CONTEXT
	data_hits.context = context;
	args.context = &data_hits.context;
	args.flags = RTC_RAY_QUERY_FLAG_INVOKE_ARGUMENT_FILTER;
	args.filter = &FilterRaytrace;
	rtcOccluded1(IntelScene, &ray, &args);
	
	return data_hits.energy;
}
 
// LOADING GEOMETRY

struct GeomInfo
{
	size_t TriangleMemory;
	size_t VertsMemory;
	size_t DummyMemory;
};


size_t GetMemory()
{
	size_t used, free, reserved;
	vminfo(&free, &reserved, &used);
	return used;
}

#include <../xrForms/CompilersUI.h>
extern CompilersMode gCompilerMode;

GeomInfo InitializeGeometryAttach(bool useForOthers)
{
	GeomInfo geomInfoData;
	
	// Первый прогон без алокаций под что нибудь просто подсчитать размер  
 	
 	size_t Predcalculated = 0;
  	Embree::GetGlobalData(Predcalculated, nullptr, nullptr, nullptr); // Без буферов !!!
 
	AditionalData( "Precalculated: %u", Predcalculated );

	// Реальная загрузка 
	// Get Buffers By Type Geometry
	xr_vector<void*>& dummy					= TriNormal_Dummys;
 	RTCGeometry& RtcGeometry				= IntelGeometryNormal;
	Embree::VertexEmbree* vertex_embree		= verticesNormal;
	Embree::TriEmbree* tri_embree			= trianglesNormal;
	
	// RtcIntilize Geoms
	RtcGeometry = rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE);

	if (gCompilerMode.EmbreeBVHCompact)
		scene_flags = RTC_SCENE_FLAG_COMPACT;

	rtcSetGeometryBuildQuality(RtcGeometry, scene_quality);

	// GET TRIANGLE (COLLECTORs Data) 
 	vertex_embree = (Embree::VertexEmbree*)
		rtcSetNewGeometryBuffer(RtcGeometry, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, sizeof(Embree::VertexEmbree), Predcalculated * 3);
	geomInfoData.VertsMemory = sizeof(Embree::VertexEmbree) * Predcalculated * 3;

	tri_embree = (Embree::TriEmbree*)
		rtcSetNewGeometryBuffer(RtcGeometry, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, sizeof(Embree::TriEmbree), Predcalculated);
	geomInfoData.TriangleMemory = Predcalculated * sizeof(Embree::TriEmbree);

	// FIX
	dummy.clear();
	dummy.resize(Predcalculated);
 	geomInfoData.DummyMemory = Predcalculated * sizeof(void*);

	// Set Buffer Data
	size_t TrianglesSize = 0;
	GetGlobalData(TrianglesSize, vertex_embree, tri_embree, &dummy, useForOthers); // Указать буферы !!!
	rtcCommitGeometry(RtcGeometry);
	LastGeometryID = rtcAttachGeometry(IntelScene, RtcGeometry);

	clMsg("[Intel Embree] Attached Geometry: IntelGeometry(%s) By ID: %d, Traingles: %u",
		"Normal",
		LastGeometryID,
		TrianglesSize);
	  
	return geomInfoData;
}

void RemoveGeoms()
{
	if (LastGeometryID != RTC_INVALID_GEOMETRY_ID)
	{
		rtcDetachGeometry(IntelScene, LastGeometryID);
		rtcReleaseGeometry(IntelGeometryNormal);

		verticesNormal = 0;
		trianglesNormal = 0;
		TriNormal_Dummys.clear();

		LastGeometryID = RTC_INVALID_GEOMETRY_ID;
	}
}
 
void IntelEmbereLOAD(bool useForOthers)
{
	Phase("Loading Embree");

	clMsg("Intel Embree Loading| Memory: %u mb", u32(GetMemory()/1024/1024) );

	if (IntelScene != nullptr)
	{
		RemoveGeoms();
	}
	else
	{
		bool avx_test = CPU::ID.hasFeature(CPUFeature::AVX2);
		bool sse	  = CPU::ID.hasFeature(CPUFeature::SSE);

		const char* config = "";
		if (avx_test)
			config = "threads=16,isa=avx2";
		else if (sse)
			config = "threads=16,isa=sse4.2";
		else
			config = "threads=16,isa=sse2";

		device = rtcNewDevice(config);
		rtcSetDeviceErrorFunction(device, &Embree::errorFunction, NULL);
		 
		string128 phase;
		sprintf(phase, "Intilized Intel Embree %s - %s", RTC_VERSION_STRING, avx_test ? "avx" : sse ? "sse" : "default");
		Status(phase);
		Embree::IntelEmbreeSettings(device, avx_test, sse);

		// Создание сцены и добавление геометрии
		IntelScene = rtcNewScene(device);
		rtcSetSceneFlags(IntelScene, scene_flags);
	}
	

// LOADING NORMAL GEOM
	auto GeomNormal = InitializeGeometryAttach(useForOthers); /// GeomID == 0 

// Пишем сколько памяти скушало: 
	u32 MemoryNormalV = GeomNormal.TriangleMemory / 1024 / 1024;
	u32 MemoryNormalT = GeomNormal.VertsMemory / 1024 / 1024;
 
 	size_t prev_used = GetMemory();
 	rtcCommitScene(IntelScene);
  	AditionalData("BVH: %umb |V:%umb|T:%umb",
		(GetMemory() - prev_used) / 1024 / 1024,
		MemoryNormalV,
		MemoryNormalT
 	);

	Phase("Start RayTracing");
}

void IntelEmbereUNLOAD()
{
 	clMsg("Intel Embree Releasing Start| Memory: %u mb", u32(GetMemory() / 1024 / 1024));

	RemoveGeoms();
	rtcReleaseScene(IntelScene);
	rtcReleaseDevice(device);
	IntelScene = 0;
	device = 0;

	clMsg("Intel Embree Releasing End| Memory: %u mb", u32(GetMemory() / 1024 / 1024));
}
