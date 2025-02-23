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

#include "embree4/rtcore.h"
#pragma comment(lib, "embree4.lib")

// Важные параметры
// INTIALIZE GEOMETRY, SCENE QUALITY TYPE
// Инициализация Основных Фишек Embree

#define USE_TRANSPARENT_GEOM
// Качество сцены желательно REFIT юзать на более низких может криво отработать 
auto geom_type = RTCBuildQuality::RTC_BUILD_QUALITY_LOW;

// 20% Гдето задержка на ROBUST
auto scene_flags = RTCSceneFlags::RTC_SCENE_FLAG_NONE; 

// Сильно ускоряет Но не нужно сильно завышать вообще 0.01f желаетельно 
// Влияет на яркость на выходе (если близко к 0 будет занулятся)
// можно и 0.10f Было раньше так
float EmbreeEnergyMAX = 0.1f;  

// Vertex, Tri Buffers
struct VertexEmbree
{
	float x, y, z;

	void Set(Fvector& vertex)
	{
		x = vertex.x;
		y = vertex.y;
		z = vertex.z;
	}

	Fvector Get()
	{
		Fvector vertex;
		x = vertex.x;
		y = vertex.y;
		z = vertex.z;
		return vertex;
	}
};

struct TriEmbree
{
	uint32_t point1, point2, point3;
	void SetVertexes(CDB::TRI& triangle, Fvector* verts, VertexEmbree* emb_verts, size_t& last_index)
	{
		point1 = last_index;
		point2 = last_index + 1;
		point3 = last_index + 2;

		int v1 = triangle.verts[0];
		int v2 = triangle.verts[1];
		int v3 = triangle.verts[2];


		emb_verts[last_index].Set(verts[v1]);
		emb_verts[last_index + 1].Set(verts[v2]);
		emb_verts[last_index + 2].Set(verts[v3]);

		last_index += 3;
	}
};

// INTEL DATA STRUCTURE
int LastGeometryTranspID = RTC_INVALID_GEOMETRY_ID;
int LastGeometryID = RTC_INVALID_GEOMETRY_ID;

RTCDevice device;
RTCScene IntelScene = 0;
RTCGeometry IntelGeometryNormal = 0;
RTCGeometry IntelGeometryTransp = 0;
/** NORMAL GEOM **/
VertexEmbree* verticesNormal = 0;
TriEmbree* trianglesNormal = 0;
xr_vector<void*> TriNormal_Dummys;

/** TRANSP GEOM **/
VertexEmbree* verticesTransp = 0;
TriEmbree* trianglesTransp = 0;
xr_vector<void*> TriTransp_Dummys;
 

// Setters

// ВАЖНЫЙ ПАРАМЕТР TNEAR Для пересечения с водой
void SetRay1(RTCRay& rayhit, Fvector& pos, Fvector& dir, float min, float max)
{
	rayhit.dir_x = dir.x;
	rayhit.dir_y = dir.y;
	rayhit.dir_z = dir.z;

	rayhit.org_x = pos.x;
	rayhit.org_y = pos.y;
	rayhit.org_z = pos.z;

	rayhit.tnear = min;
	rayhit.tfar  = max;

	rayhit.mask = (unsigned int)(-1);
	rayhit.flags = 0;
}

void SetRay1(RTCRayHit& rayhit, Fvector& pos, Fvector& dir, float min, float max)
{
	rayhit.ray.dir_x = dir.x;
	rayhit.ray.dir_y = dir.y;
	rayhit.ray.dir_z = dir.z;

	rayhit.ray.org_x = pos.x;
	rayhit.ray.org_y = pos.y;
	rayhit.ray.org_z = pos.z;

	rayhit.ray.tnear = min;
	rayhit.ray.tfar  = max;

	rayhit.ray.mask = (unsigned int)(-1);
	rayhit.ray.flags = 0;

	rayhit.hit.Ng_x = 0;
	rayhit.hit.Ng_y = 0;
	rayhit.hit.Ng_z = 0;

	rayhit.hit.u = 0;
	rayhit.hit.v = 0;

	rayhit.hit.geomID = RTC_INVALID_GEOMETRY_ID;
	rayhit.hit.instID[0] = RTC_INVALID_GEOMETRY_ID;
	rayhit.hit.primID = RTC_INVALID_GEOMETRY_ID;
}


// Сделать потом переключалку
#define MAX_PRIMITIVES 64

struct RayQueryContext
{
	RTCRayQueryContext context;
	Fvector B;

	Face* skip = 0;
	R_Light* Light = 0;
	float energy = 1.0f;
	u32 LastPremitive = 0;
	u32 HittedPrimitives = 0;
};

ICF bool CalculateEnergy(base_Face* F, Fvector& B, float& energy, float u, float v)
{
	// Перемещаем начало луча немного дальше пересечения
	b_material& M = inlc_global_data()->materials()[F->dwMaterial];
	b_texture& T = inlc_global_data()->textures()[M.surfidx];
 	if (T.pSurface == nullptr)
	{
 		F->flags.bOpaque = true;
		return false;
	}

	// barycentric coords
	// note: W,U,V order
	B.set(1.0f - u - v, u, v);

	//// calc UV
	Fvector2* cuv = F->getTC0();
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

ICF base_Face* GetGeomBuff(int GeomID, int Prim)
{
	if (GeomID == 0 && Prim != RTC_INVALID_GEOMETRY_ID)
		return (base_Face*) TriNormal_Dummys[Prim];
	else if (GeomID == 1 && Prim != RTC_INVALID_GEOMETRY_ID)
		return (base_Face*) TriTransp_Dummys[Prim];
	else
		return nullptr;
}

ICF void FilterRaytrace(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;
	RTCRay* ray = (RTCRay*)args->ray;
	// Собрать все
 	base_Face* F = GetGeomBuff(hit->geomID, hit->primID);
	if (F == ctxt->skip || !F)
	{ args->valid[0] = 0; return; }
	if (F->flags.bOpaque)
	{   ctxt->energy = 0; return; }
 	if ( !CalculateEnergy(F, ctxt->B, ctxt->energy, hit->u, hit->v) )
	{	ctxt->energy = 0; return; }

	ctxt->HittedPrimitives += 1;
	if (ctxt->HittedPrimitives > MAX_PRIMITIVES)
		return;
	args->valid[0] = 0; // Задаем чтобы продолжил поиск
}


float RaytraceEmbreeProcess(R_Light& L, Fvector& P, Fvector& N, float range, Face* skip)
{
	RayQueryContext data_hits;
	data_hits.Light = &L;
	data_hits.skip = skip;
	data_hits.energy = 1.0f;
	data_hits.LastPremitive = RTC_INVALID_GEOMETRY_ID;
	data_hits.HittedPrimitives = 0;

 	// Initialize Raytracing context
	RTCRayQueryContext context;
	rtcInitRayQueryContext(&context);
	data_hits.context = context;

	// Initialize Raytracing Args
	RTCOccludedArguments args;
	rtcInitOccludedArguments(&args);
	args.context = &data_hits.context;
	args.flags = RTCRayQueryFlags::RTC_RAY_QUERY_FLAG_COHERENT; // RTC_RAY_QUERY_FLAG_INCOHERENT;	//RTC_RAY_QUERY_FLAG_COHERENT;


	// Initialize Ray

	RTCRay ray_occluded;
	SetRay1(ray_occluded, P, N, 0.001f, range);
	rtcOccluded1(IntelScene, &ray_occluded, &args);
	
	return data_hits.energy;
}

void errorFunction(void* userPtr, enum RTCError error, const char* str)
{
	clMsg("error %d: %s", error, str);
	DebugBreak();
}

// OFF PACKED PROCESSING
void GetEmbreeDeviceProperty(LPCSTR msg, RTCDevice& device, RTCDeviceProperty prop)
{
	clMsg("EmbreeDevProp: %s : %llu", msg, rtcGetDeviceProperty(device, prop));
}

void IntelEmbreeSettings(bool avx, bool sse)
{
 	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_RAY_MASK_SUPPORTED", device, RTC_DEVICE_PROPERTY_RAY_MASK_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_BACKFACE_CULLING_ENABLED", device, RTC_DEVICE_PROPERTY_BACKFACE_CULLING_ENABLED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY4_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY4_SUPPORTED);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY8_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY8_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_IGNORE_INVALID_RAYS_ENABLED", device, RTC_DEVICE_PROPERTY_IGNORE_INVALID_RAYS_ENABLED);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_TASKING_SYSTEM", device, RTC_DEVICE_PROPERTY_TASKING_SYSTEM);
}

void InitializeGeometryAttach_CDB(CDB::CollectorPacked& packed_cb)
{
	Fvector* CDB_verts = packed_cb.getV();
	CDB::TRI* CDB_tris = packed_cb.getT();

	// NORMAL GEOM
	IntelGeometryNormal = rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE);
	rtcSetGeometryBuildQuality(IntelGeometryNormal, geom_type);

	rtcSetGeometryOccludedFilterFunction(IntelGeometryNormal, &FilterRaytrace);
	rtcSetGeometryIntersectFilterFunction(IntelGeometryNormal, &FilterRaytrace);

	xr_vector<CDB::TRI*> transparent;
	xr_vector<CDB::TRI*> Opacue;

	for (auto i = 0; i < packed_cb.getTS(); i++)
	{
		CDB::TRI* tri = (CDB::TRI*) convert_nax ( CDB_tris[i].dummy );
		base_Face* face = (base_Face*)tri;
		if (!face)
			continue;
#ifdef USE_TRANSPARENT_GEOM
		if (face->flags.bOpaque)
		{
			Opacue.push_back(&CDB_tris[i]);
		}
		else
		{
			transparent.push_back(&CDB_tris[i]);
		}
#else 
		Opacue.push_back(&CDB_tris[i]);
#endif
	}

	clMsg("Intel Initialized: Transparent: %d, Opacue: %d", transparent.size(), Opacue.size());

	verticesNormal = (VertexEmbree*)rtcSetNewGeometryBuffer(IntelGeometryNormal, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, sizeof(VertexEmbree), Opacue.size() * 3);
	trianglesNormal = (TriEmbree*)rtcSetNewGeometryBuffer(IntelGeometryNormal, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, sizeof(TriEmbree), Opacue.size());

	// FIX
	TriNormal_Dummys.clear(); TriNormal_Dummys.reserve(Opacue.size());

	size_t VertexIndexer = 0;

	for (auto i = 0; i < Opacue.size(); i++)
	{
		trianglesNormal[i].SetVertexes(*Opacue[i], CDB_verts, verticesNormal, VertexIndexer);
		TriNormal_Dummys[i] = convert_nax(Opacue[i]->dummy);
	}
	rtcCommitGeometry(IntelGeometryNormal);
	LastGeometryID = rtcAttachGeometry(IntelScene, IntelGeometryNormal);
	Opacue.clear();


#ifdef USE_TRANSPARENT_GEOM
	TriTransp_Dummys.clear(); TriTransp_Dummys.reserve(transparent.size());

	// TRANSPARENT GEOM
	IntelGeometryTransp = rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE);
	rtcSetGeometryBuildQuality(IntelGeometryTransp, geom_type);

	rtcSetGeometryOccludedFilterFunction(IntelGeometryTransp, &FilterRaytrace);
	rtcSetGeometryIntersectFilterFunction(IntelGeometryTransp, &FilterRaytrace);

	verticesTransp = (VertexEmbree*)rtcSetNewGeometryBuffer(IntelGeometryTransp, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, sizeof(VertexEmbree), transparent.size() * 3);
	trianglesTransp = (TriEmbree*)rtcSetNewGeometryBuffer(IntelGeometryTransp, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, sizeof(TriEmbree), transparent.size());

	size_t VertexIndexerTransparent = 0;
	for (auto i = 0; i < transparent.size(); i++)
	{
		trianglesTransp[i].SetVertexes(*transparent[i], CDB_verts, verticesTransp, VertexIndexerTransparent);
		TriTransp_Dummys[i] = convert_nax(transparent[i]->dummy);
	}
	rtcCommitGeometry(IntelGeometryTransp);
	LastGeometryTranspID = rtcAttachGeometry(IntelScene, IntelGeometryTransp);
	transparent.clear();
#endif

	rtcCommitScene(IntelScene);

	clMsg("[Intel Embree] Attached Geometry: IntelGeometry(Normal) By ID: %d, Transparent: %d", LastGeometryID, LastGeometryTranspID);
}


void IntelEmbereLOAD(CDB::CollectorPacked& packed_cb)
{
	if (IntelScene != nullptr && LastGeometryID != RTC_INVALID_GEOMETRY_ID)
	{
 		if (IntelGeometryNormal != nullptr)
		{
			rtcDetachGeometry(IntelScene, LastGeometryID);
			rtcReleaseGeometry(IntelGeometryNormal);

			verticesNormal = 0;
			trianglesNormal = 0;
			TriNormal_Dummys.clear();
			LastGeometryID = RTC_INVALID_GEOMETRY_ID;
		}

		if (IntelGeometryTransp != nullptr)
		{
			rtcDetachGeometry(IntelScene, LastGeometryTranspID);
			rtcReleaseGeometry(IntelGeometryTransp);

			verticesTransp = 0;
			trianglesTransp = 0;
			TriTransp_Dummys.clear();
			LastGeometryTranspID = RTC_INVALID_GEOMETRY_ID;
		}

		InitializeGeometryAttach_CDB(packed_cb);
	}
	else
	{
		bool avx_test = CPU::ID.hasFeature(CPUFeature::AVX2);
		bool sse = CPU::ID.hasFeature(CPUFeature::SSE42);

		const char* config = "";
		if (avx_test)
			config = "threads=16,isa=avx2"; 
		else if (sse)
			config = "threads=16,isa=sse4.2";
		else 
			config = "threads=16,isa=sse2";

		device = rtcNewDevice(config);
		rtcSetDeviceErrorFunction(device, errorFunction, NULL);
 

		string128 phase;
		sprintf(phase, "Intilized Intel Embree %s - %s", RTC_VERSION_STRING, avx_test ? "avx" : sse ? "sse" : "default");
		Status(phase);
		IntelEmbreeSettings(avx_test, sse);

		// Создание сцены и добавление геометрии
		// Scene
		IntelScene = rtcNewScene(device);
		rtcSetSceneFlags(IntelScene, scene_flags);

		InitializeGeometryAttach_CDB(packed_cb);
	}
}

void IntelEmbereUNLOAD()
{
	if (LastGeometryID != RTC_INVALID_GEOMETRY_ID)
	{
		rtcDetachGeometry(IntelScene, LastGeometryID);
		rtcReleaseGeometry(IntelGeometryNormal);
	}

	if (LastGeometryTranspID != RTC_INVALID_GEOMETRY_ID)
	{
		rtcDetachGeometry(IntelScene, LastGeometryTranspID);
		rtcReleaseGeometry(IntelGeometryTransp);
	}

	rtcReleaseScene(IntelScene);
	rtcReleaseDevice(device);
}
