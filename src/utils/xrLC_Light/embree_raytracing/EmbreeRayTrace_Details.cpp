#include "stdafx.h"
#include "EmbreeRayTrace.h"
#include "../../xrCore/Collision/xrCDB.h"
#include "global_calculation_data.h"
#include "xrLC_GlobalData.h"

extern global_claculation_data	gl_data;

/*
bool CalculateEnergy(Face* F, Fvector& B, float& energy, float u, float v)
{
  	b_material& M	= gl_data.g_materials[F->dwMaterial];
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
	Fvector2*   cuv = F->getTC0();
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
	UserGeomData* UD = (UserGeomData*)args->geometryUserPtr;
 
	for (auto N = 0; N < args->N; N++)
	{
		if (!args->valid[N]) continue;
		
		u32 primID = RTCHitN_primID(args->hit, args->N, N);
		auto F = UD->dummys[primID];

		u32 primID = RTCHitN_primID(args->hit, args->N, N);

		Fvector B;
		if (!CalculateEnergy(F, B, ctxt->energy[N], hit->u, hit->v))
		{
			ctxt->energy[N] = 0;
			args->valid[N] = -1; // Остановится
			return;
		}

		args->valid[N] = 0;		 // Продолжить
	}
}

float EmbreeRayTraceModel::RaytraceEmbreeDetails( Fvector& P, Fvector& N, float range)
{
	// Initials
	thread_local RTCRay ray;
	thread_local RTCOccludedArguments args;
	thread_local RayQueryContext data_hits;
	rtcInitRayQueryContext(&data_hits);
	rtcInitOccludedArguments(&args);

	// Setup Info
	SetRay1(ray, P, N, 0.f, range);
	data_hits.energy[0] = 1.0f;
 	args.context = &data_hits;

	rtcOccluded1(IntelSceneDetails, &ray, &args);
	return data_hits.energy[0];
}

// хм почемуто не хочет с другого места работать 
RTCDevice DeviceDetails = nullptr;
void EmbreeRayTraceModel::InitEmbreeDetails()
{

	auto handler = [](void* userPtr, enum RTCError code, const char* str)
	{
		R_ASSERT2(false, str);
	};

 	DeviceDetails = rtcNewDevice(GetDeviceConfig());;
	rtcSetDeviceErrorFunction(DeviceDetails, handler, nullptr);

	auto BuildModel = [this]()
	{
		// Тут уже будет отфильтровано 
		opacue_geom.ClearAll();
		opacue_geom.verts_v.swap(build_data.build_verts);
		opacue_geom.faces_v.resize(build_data.build_fcnt);
		opacue_geom.dummy.resize(build_data.build_fcnt);
		
		for (auto Fid = 0; Fid < build_data.build_faces.size(); Fid++)
		{
			auto& FCDB = build_data.build_faces[Fid];
			auto& F = gl_data.g_rc_faces[Fid];
		
			opacue_geom.faces_v[Fid].point1 = FCDB.verts[0];
			opacue_geom.faces_v[Fid].point2 = FCDB.verts[1];
			opacue_geom.faces_v[Fid].point3 = FCDB.verts[2];
		}
		
		// Чистим вектора
		build_data.build_faces.clear();
		build_data.build_faces.shrink_to_fit();
		build_data.build_fcnt = 0;
		build_data.build_vcnt = 0;
	};

 	// Загрузка Геометрии
	auto GeometryLoad = [this]()
	{
 		IntelGeometryNormal = rtcNewGeometry(DeviceDetails, RTC_GEOMETRY_TYPE_TRIANGLE);
		rtcSetGeometryBuildQuality(IntelGeometryNormal, RTCBuildQuality::RTC_BUILD_QUALITY_LOW);
		// rtcSetGeometryOccludedFilterFunction(IntelGeometryDetails, &FilterRaytraceD);

		rtcSetSharedGeometryBuffer(IntelGeometryNormal, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, opacue_geom.vertex().data(), 0, sizeof(Fvector), opacue_geom.vertex().size());
		rtcSetSharedGeometryBuffer(IntelGeometryNormal, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, opacue_geom.faces().data(), 0, sizeof(Triangle), opacue_geom.faces().size());
		rtcCommitGeometry(IntelGeometryNormal);
	};

	BuildModel();
	GeometryLoad();
	clMsg("Loading Embree : verts[%u] faces[%u]", opacue_geom.vertex_cnt(), opacue_geom.faces_cnt());

  
	IntelSceneDetails = rtcNewScene(DeviceDetails);
	rtcSetSceneFlags(IntelSceneDetails, scene_flags);
	rtcAttachGeometryByID(IntelSceneDetails, IntelGeometryDetails, 0);
	rtcCommitScene(IntelSceneDetails);
	
}
*/