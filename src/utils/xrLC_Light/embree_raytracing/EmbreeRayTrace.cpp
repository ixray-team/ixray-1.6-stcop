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

#define USE_TRANSPARENT_GEOM
 
// INTEL DATA STRUCTURE
int LastGeometryID = RTC_INVALID_GEOMETRY_ID;
int LastGeometryIDTransp = RTC_INVALID_GEOMETRY_ID;

RTCSceneFlags scene_flags = RTC_SCENE_FLAG_ROBUST;
RTCBuildQuality scene_quality = RTC_BUILD_QUALITY_LOW;

RTCDevice device;
RTCScene IntelScene = 0;

RTCGeometry IntelGeometryNormal = 0;
RTCGeometry IntelGeometryTransparent = 0;

/** NORMAL GEOM **/
Embree::VertexEmbree* verticesNormal = 0;
Embree::TriEmbree* trianglesNormal = 0;
xr_vector<void*> TriNormal_Dummys;

/** TRANSP GEOM **/
Embree::VertexEmbree* verticesTransp = 0;
Embree::TriEmbree* trianglesTransp = 0;
xr_vector<void*> TriTransp_Dummys;

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

ICF void* GetGeomBuff(int GeomID, int Prim)
{
	if (GeomID == 0 && Prim != RTC_INVALID_GEOMETRY_ID)
		return TriNormal_Dummys[Prim];
	if (GeomID == 1 && Prim != RTC_INVALID_GEOMETRY_ID)
		return TriTransp_Dummys[Prim];

	return nullptr;
}

ICF bool CalculateEnergy(base_Face* F, Fvector& B, float& energy, float u, float v)
{
	// Перемещаем начало луча немного дальше пересечения
	b_material& M = inlc_global_data()->materials()[F->dwMaterial];
	b_texture& T = inlc_global_data()->textures()[M.surfidx];

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

// 27.01.2025 (для тестов 16 хитов только сделал)
// Вообще рекомендовано для пересечния с прозрачностями 4-8
// Если непрозрачные 1
// LMAP стадия вроде быстрее если чисто искать Opacue (Внутри помещений вроде делает расщет)
extern XRLC_LIGHT_API int StageMAXHits = 16;

ICF void FilterRaytrace(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;
	RTCRay* ray = (RTCRay*)args->ray;

	// Собрать все
	base_Face* F = (base_Face*)GetGeomBuff(hit->geomID, hit->primID);

	if (F->flags.bOpaque)
	{
		ctxt->energy = 0; return;
	}

	if (F == ctxt->skip || !F)
	{
		args->valid[0] = 0; return;
	}

	if (!CalculateEnergy(F, ctxt->B, ctxt->energy, hit->u, hit->v))
	{
		// При нахождении любого хита сразу все попали в непрозрачный Face.
		ctxt->energy = 0;
		ctxt->Hits += 1;
		return;
	}

	ctxt->Hits += 1;
	if (ctxt->Hits > StageMAXHits)
		return;

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

	/// Непрозрачные чекаем

	// se7kills : 27.01.2025 0.001 -> 0.1
	// Чуть селнее сдвинул near
	RTCRay ray;
	Embree::SetRay1(ray, P, N, 0.1f, range);

	RTCOccludedArguments args;
	rtcInitOccludedArguments(&args);

	RTCRayQueryContext context;
	rtcInitRayQueryContext(&context);

	// SET CONTEXT
	data_hits.context = context;
	args.context = &data_hits.context;
	args.flags = RTC_RAY_QUERY_FLAG_INCOHERENT;

	rtcOccluded1(IntelScene, &ray, &args);
	return data_hits.energy;
}

xr_vector<CDB::TRI*> GetTrianglesByType(CDB::TRI* CDB_tris, u32 Size, bool Transparent)
{
	xr_vector<CDB::TRI*> RETURN;

	for (auto i = 0; i < Size; i++)
	{
		CDB::TRI* tri = (CDB::TRI*) convert_nax(CDB_tris[i].dummy);

		base_Face* face = (base_Face*)tri;
		if (!face)
			continue;

#ifdef USE_TRANSPARENT_GEOM
		if (face->flags.bOpaque && !Transparent)
			RETURN.push_back(&CDB_tris[i]);

		if (!face->flags.bOpaque && Transparent)
			RETURN.push_back(&CDB_tris[i]);
#else 
		RETURN.push_back(&CDB_tris[i]);
#endif 
	}

	return RETURN;
}


void InitializeGeometryAttach(bool isTransp, Fvector* CDB_verts, CDB::TRI* CDB_tris, u32 TS_Size)
{
	// Get Buffers By Type Geometry
	xr_vector<void*>& dummy = isTransp ? TriTransp_Dummys : TriNormal_Dummys;

	RTCGeometry& RtcGeometry = isTransp ? IntelGeometryTransparent : IntelGeometryNormal;
	Embree::VertexEmbree* vertex_embree = isTransp ? verticesTransp : verticesNormal;
	Embree::TriEmbree* tri_embree = isTransp ? trianglesTransp : trianglesNormal;

	// RtcIntilize Geoms
	RtcGeometry = rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE);
	rtcSetGeometryBuildQuality(RtcGeometry, scene_quality);

	rtcSetGeometryOccludedFilterFunction(RtcGeometry, &FilterRaytrace);
	rtcSetGeometryIntersectFilterFunction(RtcGeometry, &FilterRaytrace);

	// GET TRIANGLE (COLLECTORs Data) 

	xr_vector<CDB::TRI*> TRIS = GetTrianglesByType(CDB_tris, TS_Size, isTransp);

	vertex_embree = (Embree::VertexEmbree*)rtcSetNewGeometryBuffer(RtcGeometry, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, sizeof(Embree::VertexEmbree), TRIS.size() * 3);
	tri_embree = (Embree::TriEmbree*)rtcSetNewGeometryBuffer(RtcGeometry, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, sizeof(Embree::TriEmbree), TRIS.size());

	// FIX
	dummy.clear();
	dummy.resize(TRIS.size());

	size_t VertexIndexer = 0;
	for (auto i = 0; i < TRIS.size(); i++)
	{
		tri_embree[i].SetVertexes(*TRIS[i], CDB_verts, vertex_embree, VertexIndexer);
		dummy[i] = convert_nax (TRIS[i]->dummy);
	}
	rtcCommitGeometry(RtcGeometry);

	if (isTransp)
		LastGeometryIDTransp = rtcAttachGeometry(IntelScene, RtcGeometry);
	else
		LastGeometryID = rtcAttachGeometry(IntelScene, RtcGeometry);



	clMsg("[Intel Embree] Attached Geometry: IntelGeometry(%s) By ID: %d, Traingles: %u",
		isTransp ? "Transparent" : "Normal",
		isTransp ? LastGeometryIDTransp : LastGeometryID,
		TRIS.size());
	TRIS.clear();
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

	if (LastGeometryIDTransp != RTC_INVALID_GEOMETRY_ID)
	{
		rtcDetachGeometry(IntelScene, LastGeometryIDTransp);
		rtcReleaseGeometry(IntelGeometryTransparent);

		verticesTransp = 0;
		trianglesTransp = 0;
		TriTransp_Dummys.clear();

		LastGeometryIDTransp = RTC_INVALID_GEOMETRY_ID;
	}
}

void IntelEmbereLOAD(CDB::CollectorPacked& packed_cb)
{
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
		rtcSetDeviceErrorFunction(device, Embree::errorFunction, NULL);


		string128 phase;
		sprintf(phase, "Intilized Intel Embree %s - %s", RTC_VERSION_STRING, avx_test ? "avx" : sse ? "sse" : "default");
		Status(phase);
		Embree::IntelEmbreeSettings(device, avx_test, sse);

		// Создание сцены и добавление геометрии
		// Scene
		IntelScene = rtcNewScene(device);
		rtcSetSceneFlags(IntelScene, scene_flags);
	}

	InitializeGeometryAttach(false, packed_cb.getV(), packed_cb.getT(), packed_cb.getTS()); /// GeomID == 0
#ifdef USE_TRANSPARENT_GEOM
	InitializeGeometryAttach(true, packed_cb.getV(), packed_cb.getT(), packed_cb.getTS());	/// GeomID == 1
#endif 

	rtcCommitScene(IntelScene);
}

void IntelEmbereUNLOAD()
{
	RemoveGeoms();
	rtcReleaseScene(IntelScene);
	rtcReleaseDevice(device);
}
