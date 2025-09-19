#include "stdafx.h"
#include "EmbreeRayTrace.h"

#include "../../xrCore/Collision/xrCDB.h"

// INTEL DATA STRUCTURE
int LastGeometryDetailsID = RTC_INVALID_GEOMETRY_ID;

RTCDevice DeviceDetails;
RTCScene IntelSceneDetails;
RTCGeometry IntelGeometryOpacue = 0;
 
#include "global_calculation_data.h"
#include "xrLC_GlobalData.h"
extern global_claculation_data	gl_data;

bool CalculateEnergy(int PrimID, Fvector& B, float& energy, float u, float v)
{
	auto& F			= gl_data.g_rc_faces[PrimID];
 	b_material& M	= gl_data.g_materials[F.dwMaterial];
	b_texture& T	= gl_data.g_textures[M.surfidx];

	if (!T.bHasAlpha)
		return false;

	if (T.pSurface == nullptr)
	{
		T.bHasAlpha = false;
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

	// ���������� ���������
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

ICF void FilterRaytraceD(const struct RTCFilterFunctionNArguments* args)
{
	RayQueryContext* ctxt = (RayQueryContext*)args->context;
	RTCHit* hit = (RTCHit*)args->hit;
	RTCRay* ray = (RTCRay*)args->ray;

	if (!CalculateEnergy(hit->primID, ctxt->B, ctxt->energy, hit->u, hit->v))
	{
 		ctxt->energy = 0;
		args->valid[0] = -1; // Остановится
		return;
	}

	args->valid[0] = 0;		 // Продолжить
}
 

float RaytraceEmbreeDetails(R_Light& L, Fvector& P, Fvector& N, float range)
{
  	RayQueryContext data_hits;
	data_hits.Light = &L;
	data_hits.skip = 0;
	data_hits.energy = 1.0f;

	RTCRayHit rayhit;
	SetRay1(rayhit, P, N, 0.f, range);

	RTCRayQueryContext context;
	rtcInitRayQueryContext(&context);

	RTCIntersectArguments args;
	rtcInitIntersectArguments(&args);

	data_hits.context = context;
	args.context = &data_hits.context;
	rtcIntersect1(IntelSceneDetails, &rayhit, &args);

	return data_hits.energy;
}

void LoadGeomBuffer(RTCGeometry& geom, TriangleContainer& geom_buffer)
{
	geom = rtcNewGeometry(DeviceDetails, RTC_GEOMETRY_TYPE_TRIANGLE);
	rtcSetGeometryBuildQuality(geom, RTC_BUILD_QUALITY_LOW);
	rtcSetGeometryOccludedFilterFunction(geom, &FilterRaytraceD);

	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, geom_buffer.vertex().data(), 0, sizeof(Fvector), geom_buffer.vertex().size());
	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, geom_buffer.faces().data(), 0, sizeof(TriEmbree), geom_buffer.faces().size());

	rtcCommitGeometry(geom);
};

void  EmbreeData::ConsturctGeometry()
{
	// se7kills Rewrite
	EmbreeData::BuildRaytraceModel_2();

	CTimer t; t.Start();
	LoadGeomBuffer(IntelGeometryOpacue, static_geom);
	rtcAttachGeometryByID(IntelSceneDetails, IntelGeometryOpacue, 0);
	rtcCommitScene(IntelSceneDetails);

	clMsg("$[Embree] Loading To Scene geometry : %u ms", t.GetElapsed_ms());
}


void EmbreeData::InitEmbreeDetails()
{
	Phase("Loading Embree");

	CTimer t; t.Start();

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
 
 	string128 phase;
	sprintf(phase, "Intilized Intel Embree (Details Raytracer) %s - %s", RTC_VERSION_STRING, avx_test ? "avx" : sse ? "sse" : "default");
	Status(phase);

 	// Scene
	IntelSceneDetails = rtcNewScene(DeviceDetails);
	rtcSetSceneFlags(IntelSceneDetails, RTCSceneFlags::RTC_SCENE_FLAG_NONE);

	ConsturctGeometry();

	clMsg("$[Embree] Level is Loaded : %u ms", t.GetElapsed_ms());
}

void IntelEmbereDetailsUNLOAD()
{
 	rtcDetachGeometry(IntelSceneDetails, 0);
 	rtcReleaseGeometry(IntelGeometryOpacue);
 
	rtcReleaseScene(IntelSceneDetails);
	rtcReleaseDevice(DeviceDetails);
}
