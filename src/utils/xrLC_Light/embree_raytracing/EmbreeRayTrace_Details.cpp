#include "stdafx.h"
#include "EmbreeRayTrace.h"

#include "../../xrcdb/xrcdb.h"

#include "embree4/rtcore.h"
#pragma comment(lib, "embree4.lib")

// INTEL DATA STRUCTURE
int LastGeometryDetailsID = RTC_INVALID_GEOMETRY_ID;


// Качество сцены желательно REFIT юзать на более низких может криво отработать 

// Сильно ускоряет Но не нужно сильно завышать вообще 0.01f желаетельно 
// Влияет на яркость на выходе (если близко к 0 будет занулятся)
// можно и 0.10f Было раньше так

RTCDevice DeviceDetails;
RTCScene IntelSceneDetails;
RTCGeometry IntelGeometryDetails = 0;

#include "global_calculation_data.h"
#include "xrLC_GlobalData.h"
extern global_claculation_data	gl_data;

ICF b_rc_face* GetGeomBuff(int GeomID, int Prim)
{
	if (GeomID == 0 && Prim != RTC_INVALID_GEOMETRY_ID)
		return &gl_data.g_rc_faces[Prim];
	else
		return nullptr;
}

bool CalculateEnergy(int GeomID, int PrimID, Fvector& B, float& energy, float u, float v)
{
	if (gl_data.g_rc_faces.size() < PrimID)
	{
		Msg("PrimitiveID: %u > Maximal Buffer: %u", PrimID, gl_data.g_rc_faces.size());
		return false;
	}

	auto& F = gl_data.g_rc_faces[PrimID];
	// Перемещаем начало луча немного дальше пересечения
	b_material& M = gl_data.g_materials[F.dwMaterial];
	b_texture& T = gl_data.g_textures[M.surfidx];

	if (!T.bHasAlpha)
		return false;

	if (T.pSurface == nullptr)
	{
		T.bHasAlpha = true;
		return false;
	}

	// barycentric coords
	// note: W,U,V order
	B.set(1.0f - u - v, u, v);

	//// calc UV
	Fvector2* cuv = F.t;
	Fvector2	uv;
	uv.x = cuv[0].x * B.x + cuv[1].x * B.y + cuv[2].x * B.z;
	uv.y = cuv[0].y * B.x + cuv[1].y * B.y + cuv[2].y * B.z;
	int U = iFloor(uv.x * float(T.dwWidth) + .5f);
	int V = iFloor(uv.y * float(T.dwHeight) + .5f);
	U %= T.dwWidth;		if (U < 0) U += T.dwWidth;
	V %= T.dwHeight;	if (V < 0) V += T.dwHeight;

 	u32 pixel = T.pSurface[V * T.dwWidth + U];
	u32 pixel_a = color_get_A(pixel);
	float opac = 1.f - _sqr(float(pixel_a) / 255.f);

	// Дополнение Контекста
	energy *= opac;
	if (energy < 0.01f)
		return false;

	return true;
}


struct RayQueryContext
{
	RTCRayQueryContext context;
	Fvector B;

	Face* skip = 0;
	R_Light* Light = 0;
	float energy = 1.0f;
};

ICF void FilterRaytraceDetails(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;
	RTCRay* ray = (RTCRay*)args->ray;

	if (!CalculateEnergy(hit->geomID, hit->primID, ctxt->B, ctxt->energy, hit->u, hit->v))
	{
		// При нахождении любого хита сразу все попали в непрозрачный Face.
		ray->tfar = -std::numeric_limits<float>::infinity();
		ctxt->energy = 0;
		return;
	}

	args->valid[0] = 0; // Задаем чтобы продолжил поиск
}

float RaytraceEmbreeDetails(R_Light& L, Fvector& P, Fvector& N, float range)
{
	// Msg("RayTrace");
	// Структура для RayTracing
	RayQueryContext data_hits;
	data_hits.Light = &L;
	data_hits.skip = 0;
	data_hits.energy = 1.0f;

	RTCRayHit rayhit;
	Embree::SetRay1(rayhit, P, N, 0.f, range);

	RTCRayQueryContext context;
	rtcInitRayQueryContext(&context);

	RTCIntersectArguments args;
	rtcInitIntersectArguments(&args);

	data_hits.context = context;
	args.context = &data_hits.context;
	rtcIntersect1(IntelSceneDetails, &rayhit, &args);

	return data_hits.energy;
}


void InitializeGeometryAttach(Fvector* CDB_verts, CDB::TRI* CDB_tris, u32 TS_Size)
{
	// NORMAL GEOM
	IntelGeometryDetails = rtcNewGeometry(DeviceDetails, RTC_GEOMETRY_TYPE_TRIANGLE);
	rtcSetGeometryBuildQuality(IntelGeometryDetails, RTCBuildQuality::RTC_BUILD_QUALITY_REFIT);

	rtcSetGeometryOccludedFilterFunction(IntelGeometryDetails, &FilterRaytraceDetails);
	rtcSetGeometryIntersectFilterFunction(IntelGeometryDetails, &FilterRaytraceDetails);

	xr_vector<CDB::TRI*> Opacue;

	for (auto i = 0; i < TS_Size; i++)
	{
		Opacue.push_back(&CDB_tris[i]);
	}

	Embree::VertexEmbree* verticesNormal = (Embree::VertexEmbree*)rtcSetNewGeometryBuffer(IntelGeometryDetails, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, sizeof(Embree::VertexEmbree), Opacue.size() * 3);
	Embree::TriEmbree* trianglesNormal = (Embree::TriEmbree*)rtcSetNewGeometryBuffer(IntelGeometryDetails, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, sizeof(Embree::TriEmbree), Opacue.size());

	size_t VertexIndexer = 0;

	for (auto i = 0; i < Opacue.size(); i++)
	{
		trianglesNormal[i].SetVertexes(*Opacue[i], CDB_verts, verticesNormal, VertexIndexer);
	}

	rtcCommitGeometry(IntelGeometryDetails);
	LastGeometryDetailsID = rtcAttachGeometry(IntelSceneDetails, IntelGeometryDetails);
	Opacue.clear();

	rtcCommitScene(IntelSceneDetails);

	clMsg("[Intel Embree] Attached Geometry: IntelGeometry(Normal) By ID: %d", LastGeometryDetailsID);
}


void InitEmbreeDetails(Fvector* Vertexes, CDB::TRI* tris, u32 sizeTRI)
{
	bool avx_test = true;
	bool sse = false;

	const char* config = "";
	if (avx_test)
		config = "threads=16,isa=avx2";
	else if (sse)
		config = "threads=16,isa=sse4.2";
	else
		config = "threads=16,isa=sse2";

	DeviceDetails = rtcNewDevice(config);
	rtcSetDeviceErrorFunction(DeviceDetails, Embree::errorFunction, NULL);


	string128 phase;
	sprintf(phase, "Intilized Intel Embree (Details Raytracer) %s - %s", RTC_VERSION_STRING, avx_test ? "avx" : sse ? "sse" : "default");
	Status(phase);
	Embree::IntelEmbreeSettings(DeviceDetails, avx_test, sse);

	// Создание сцены и добавление геометрии
	// Scene
	IntelSceneDetails = rtcNewScene(DeviceDetails);
	rtcSetSceneFlags(IntelSceneDetails, RTCSceneFlags::RTC_SCENE_FLAG_ROBUST);

	InitializeGeometryAttach(Vertexes, tris, sizeTRI);
}

void IntelEmbereDetailsUNLOAD()
{
	if (LastGeometryDetailsID != RTC_INVALID_GEOMETRY_ID)
	{
		rtcDetachGeometry(IntelSceneDetails, LastGeometryDetailsID);
		rtcReleaseGeometry(IntelGeometryDetails);
	}

	rtcReleaseScene(IntelSceneDetails);
	rtcReleaseDevice(DeviceDetails);
}
