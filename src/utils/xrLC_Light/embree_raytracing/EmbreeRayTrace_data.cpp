#include "stdafx.h"
#include "xrDeflector.h"
#include "R_light.h"
#include "light_point.h"
#include "base_lighting.h"
#include "xrLC_GlobalData.h"

#include "EmbreeRayTrace.h"
#include "xrMU_Model_Reference.h"
#include "xrMU_Model.h"

#include <base_face.h>
#include "global_calculation_data.h"
#include "../xrLC/Build.h"
extern CBuild* pBuild;

// Для Загрузки Геометрии
#include <../xrForms/CompilersUI.h>
extern CompilersMode gCompilerMode;

// *** Ray Packed Initialize *** //
void SetRay1(RTCRay& rayhit, const Fvector& pos, const Fvector& dir, float near_, float range)
{
	rayhit.dir_x = dir.x;
	rayhit.dir_y = dir.y;
	rayhit.dir_z = dir.z;
	rayhit.org_x = pos.x;
	rayhit.org_y = pos.y;
	rayhit.org_z = pos.z;
	rayhit.tnear = near_;
	rayhit.tfar = range;
 
	rayhit.mask		= (uint32_t)(-1);
	rayhit.flags	= 0;
	rayhit.time		= 0;
	rayhit.id		= 0; 
}

void SetRay4(RTCRay4& rayhit, u32 idx, const Fvector& pos, const Fvector& dir, float near_, float range)
{
	rayhit.dir_x[idx] = dir.x;
	rayhit.dir_y[idx] = dir.y;
	rayhit.dir_z[idx] = dir.z;
	rayhit.org_x[idx] = pos.x;
	rayhit.org_y[idx] = pos.y;
	rayhit.org_z[idx] = pos.z;
	rayhit.tnear[idx] = near_;
	rayhit.tfar[idx] = range;

	rayhit.mask[idx] = (unsigned int)(-1);
	rayhit.flags[idx] = 0;
	rayhit.time[idx] = 0;
	rayhit.id[idx] = 0;
}

void SetRay8(RTCRay8& rayhit, u32 idx, const Fvector& pos, const Fvector& dir, float near_, float range)
{
	rayhit.dir_x[idx] = dir.x;
	rayhit.dir_y[idx] = dir.y;
	rayhit.dir_z[idx] = dir.z;
	rayhit.org_x[idx] = pos.x;
	rayhit.org_y[idx] = pos.y;
	rayhit.org_z[idx] = pos.z;
	rayhit.tnear[idx] = near_;
	rayhit.tfar[idx] = range;
	
	rayhit.mask[idx] = (unsigned int)(-1);
	rayhit.flags[idx] = 0;
	rayhit.time[idx] = 0;
	rayhit.id[idx] = 0;
}

// *** Building Raytracing Model for Embree *** //
void LoadGeomBuffer(RTCGeometry& geom, TriangleContainer& geom_buffer, bool isTransp, u8 ud_geom_type);

void EmbreeInstancedModel::InitializeModel(xr_vector<FaceDataEmbree>& faces)
{
	TriangleContainer geom_builder_opacue;
	TriangleContainer geom_builder_transp;
	for (auto& F : faces)
	{
		Face* Fc = (Face*)F.ptr;
		auto& geom = Fc->flags.bOpaque ? geom_builder_opacue : geom_builder_transp;
		geom.AddFaceRaw(F.ptr, F.v1, F.v2, F.v3);
	}
	geom_builder_opacue.useMsg = false;
	geom_builder_transp.useMsg = false;

	geom_builder_opacue.RemoveDublicates();
	geom_builder_transp.RemoveDublicates();

	InstaceScene = rtcNewScene(EmbreeDevice);
	rtcSetSceneBuildQuality(InstaceScene, RTC_BUILD_QUALITY_HIGH);

	if (geom_builder_opacue.faces_cnt() > 0)
	{
		LoadGeomBuffer(GeometryOpacue, geom_builder_opacue, false, 0);
		rtcAttachGeometry(InstaceScene, GeometryOpacue);
		rtcReleaseGeometry(GeometryOpacue);
	}

	if (geom_builder_transp.faces_cnt() > 0)
	{
		LoadGeomBuffer(GeometryTransp, geom_builder_transp, true, 0);
		rtcAttachGeometry(InstaceScene, GeometryTransp);
		rtcReleaseGeometry(GeometryTransp);
	}

	rtcCommitScene(InstaceScene);
}

void EmbreeInstancedModel::SetInstance(RTCScene scene, Fmatrix& xform)
{
	// ----------------------------------------------------
	// Instace Geometry Loading
	// ----------------------------------------------------
	RTCGeometry inst = rtcNewGeometry(EmbreeDevice, RTC_GEOMETRY_TYPE_INSTANCE);
	rtcSetGeometryInstancedScene(inst, InstaceScene);

	float matrix[16];
	ConvertMatrix(xform, matrix);

	rtcSetGeometryTransform(inst, 0, RTC_FORMAT_FLOAT4X4_COLUMN_MAJOR, matrix);

	// ----------------------------------------------------
	// Commit instance
	// ----------------------------------------------------
	rtcCommitGeometry(inst);
	rtcAttachGeometry(scene, inst);
	rtcReleaseGeometry(inst);
}


// World Model 
void EmbreeRayTraceModel::BuildRayTraceModel()
{
	opacue_geom.ClearAll();
	transp_geom.ClearAll();

	bool use_transp = true;
	for (auto F : lc_global_data()->g_faces())
	{
		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow)	continue;

		auto& buf = F->flags.bOpaque || !use_transp ? opacue_geom : transp_geom;
		buf.AddFaceRaw(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
	}

	if (!gCompilerMode.EmbreeInstaces)
	{
		xr_vector<FaceDataEmbree> faces;
		for (auto REF : lc_global_data()->mu_refs())
		{
			faces.clear();
			REF->export_cform_rcast_new(faces);
			for (auto& F : faces)
			{
				bool isOpacue = ((Face*)F.ptr)->flags.bOpaque;
				auto& buf = isOpacue || !use_transp ? opacue_geom : transp_geom;
				buf.AddFaceRaw((Face*)F.ptr, F.v1, F.v2, F.v3);
			}
		}
	}

	// Обезательно вызывать иначе не будет Vertex, Tris (Убрал жрание памяти при создании) 
	opacue_geom.RemoveDublicates();
 	transp_geom.RemoveDublicates();
}
 
void EmbreeRayTraceModel::BuildRayTraceModel_Instaced()
{
	if (!gCompilerMode.EmbreeInstaces) return;

 	for (auto& MU : lc_global_data()->mu_models())
	{
		instanced.emplace_back();
		instanced.back().InitializeModel(MU->EmbreeInstanceCopy());
	}

	for (auto& MU_REF : lc_global_data()->mu_refs())
	{
		instanced[MU_REF->model->m_lod_ID].SetInstance(IntelScene, MU_REF->xform);
	}
}


// Geometry Embree Loading 

void LoadGeomBuffer(RTCGeometry& geom, TriangleContainer& geom_buffer, bool isTransp, u8 ud_geom_type)
{
	extern void SetFilter(RTCGeometry geom, bool isTransp);

	geom = rtcNewGeometry(EmbreeDevice, RTC_GEOMETRY_TYPE_TRIANGLE);

	SetFilter(geom, isTransp);

	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, geom_buffer.vertex().data(), 0, sizeof(Fvector), geom_buffer.vertex().size());
	rtcSetSharedGeometryBuffer(geom, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, geom_buffer.faces().data(), 0, sizeof(Triangle), geom_buffer.faces().size());

	UserGeomData* data = new UserGeomData();	// Кастомдата для треугольников чтобы не записывать в ctxt
	data->dummys = geom_buffer.dummy;
	data->DummyType = ud_geom_type;
	rtcSetGeometryUserData(geom, data);

	rtcCommitGeometry(geom);
};


void EmbreeRayTraceModel::AttachGeomToScene(bool isMain, u8 uDataType)
{
	// Scene Flags !
	UpdateSceneFlags();

	// Initialize Scene 
	IntelScene = rtcNewScene(EmbreeDevice);
	rtcSetSceneFlags(IntelScene, scene_flags);
	rtcSetSceneBuildQuality(IntelScene, scene_quality);

	if (opacue_geom.faces_cnt() > 0)
	{
		LoadGeomBuffer(IntelGeometryNormal, opacue_geom, false, uDataType);
		rtcAttachGeometry(IntelScene, IntelGeometryNormal);
		rtcReleaseGeometry(IntelGeometryNormal);
	}

	if (transp_geom.faces_cnt() > 0)
	{
		LoadGeomBuffer(IntelGeometryTransp, transp_geom, true, uDataType);
		rtcAttachGeometry(IntelScene, IntelGeometryTransp);
		rtcReleaseGeometry(IntelGeometryTransp);
	}

	if (isMain)
		BuildRayTraceModel_Instaced();

	rtcCommitScene(IntelScene);
}
