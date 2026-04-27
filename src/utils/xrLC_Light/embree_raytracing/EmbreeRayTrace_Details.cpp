#include "stdafx.h"
#include "EmbreeRayTrace.h"
#include "../../xrCore/Collision/xrCDB.h"
#include "global_calculation_data.h"
#include "xrLC_GlobalData.h"

extern global_claculation_data	gl_data;

struct RayQueryContext
{
	RTCRayQueryContext context;
	Fvector B;

	Face* skip = 0;
 	float energy = 1.0f;
};


bool CalculateEnergy(int PrimID, Fvector& B, float& energy, float u, float v)
{
	auto& F			= gl_data.g_rc_faces[PrimID];
 	b_material& M	= gl_data.g_materials[F.dwMaterial];
	b_texture& T	= gl_data.g_textures[M.surfidx];

	if (!T.bHasAlpha)
		return false;

	if (T.pSurface.Empty())
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

	u32* raw = static_cast<u32*>(*T.pSurface);
 	u32 pixel = raw[V * T.dwWidth + U];
	u32 pixel_a = color_get_A(pixel);
	float opac = 1.f - _sqr(float(pixel_a) / 255.f);

	// ���������� ���������
	energy *= opac;
	if (energy < 0.01f)
		return false;

	return true;
}
 
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
  
float EmbreeRayTraceModel::RaytraceEmbreeDetails( Fvector& P, Fvector& N, float range)
{
  	RayQueryContext data_hits;
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

// хм почемуто не хочет с другого места работать 


void errors_embree_det(void* userPtr, enum RTCError code, const char* str)
{
	R_ASSERT2(false, str);
}

RTCDevice DeviceDetails = nullptr;
void EmbreeRayTraceModel::InitEmbreeDetails()
{
	DeviceDetails = rtcNewDevice(GetDeviceConfig());;
	rtcSetDeviceErrorFunction(DeviceDetails, &errors_embree_det, nullptr);

 	// Загрузка Геометрии
	auto GeometryLoad = [&]()
	{
		this->BuildRaytraceModel_2();
 			
		IntelGeometryDetails = rtcNewGeometry(DeviceDetails, RTC_GEOMETRY_TYPE_TRIANGLE);
		rtcSetGeometryBuildQuality(IntelGeometryDetails, RTCBuildQuality::RTC_BUILD_QUALITY_LOW);
		rtcSetGeometryOccludedFilterFunction(IntelGeometryDetails, &FilterRaytraceD);

		rtcSetSharedGeometryBuffer(IntelGeometryDetails, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, static_geom.vertex().data(), 0, sizeof(Fvector), static_geom.vertex().size());
		rtcSetSharedGeometryBuffer(IntelGeometryDetails, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, static_geom.faces().data(), 0, sizeof(Triangle), static_geom.faces().size());
		rtcCommitGeometry(IntelGeometryDetails);
	};
	GeometryLoad();
	clMsg("Loading Embree : verts[%u] faces[%u]", static_geom.vertex_cnt(), static_geom.faces_cnt());

  
	IntelSceneDetails = rtcNewScene(DeviceDetails);
	rtcSetSceneFlags(IntelSceneDetails, scene_flags);
	rtcAttachGeometryByID(IntelSceneDetails, IntelGeometryDetails, 0);
	rtcCommitScene(IntelSceneDetails);

	clMsg("Scene Create");
}
