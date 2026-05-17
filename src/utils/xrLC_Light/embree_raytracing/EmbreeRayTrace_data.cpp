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

void SetRay1(RTCRayHit& rayhit, Fvector& pos, Fvector& dir, float near_, float range)
{
	rayhit.ray.dir_x = dir.x;
	rayhit.ray.dir_y = dir.y;
	rayhit.ray.dir_z = dir.z;
	rayhit.ray.org_x = pos.x;
	rayhit.ray.org_y = pos.y;
	rayhit.ray.org_z = pos.z;
	rayhit.ray.tnear = near_;
	rayhit.ray.tfar = range;
	rayhit.ray.mask = (unsigned int)(-1);
	rayhit.ray.flags = 0;

	rayhit.hit.geomID = RTC_INVALID_GEOMETRY_ID;
	rayhit.hit.primID = RTC_INVALID_GEOMETRY_ID;

	rayhit.hit.instID[0] = RTC_INVALID_GEOMETRY_ID;
	rayhit.hit.instPrimID[0] = RTC_INVALID_GEOMETRY_ID;
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

void SetRay4(RTCRayHit4& rayhit, u32 IDX, const Fvector& pos, const Fvector& dir, float near_, float range)
{
	rayhit.ray.dir_x[IDX] = dir.x;
	rayhit.ray.dir_y[IDX] = dir.y;
	rayhit.ray.dir_z[IDX] = dir.z;
	rayhit.ray.org_x[IDX] = pos.x;
	rayhit.ray.org_y[IDX] = pos.y;
	rayhit.ray.org_z[IDX] = pos.z;
	rayhit.ray.tnear[IDX] = near_;
	rayhit.ray.tfar[IDX] = range;

	rayhit.ray.mask[IDX] = (uint32_t)(-1);
	rayhit.ray.flags[IDX] = 0;
	rayhit.ray.time[IDX] = 0;
	rayhit.ray.id[IDX] = 0;

	rayhit.hit.geomID[IDX] = RTC_INVALID_GEOMETRY_ID;
	rayhit.hit.primID[IDX] = RTC_INVALID_GEOMETRY_ID;

	rayhit.hit.instID[0][IDX] = RTC_INVALID_GEOMETRY_ID;
	rayhit.hit.instPrimID[0][IDX] = RTC_INVALID_GEOMETRY_ID;
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
 
void SetRay8(RTCRayHit8& rayhit, u32 IDX, const Fvector& pos, const Fvector& dir, float near_, float range)
{
	rayhit.ray.dir_x[IDX] = dir.x;
	rayhit.ray.dir_y[IDX] = dir.y;
	rayhit.ray.dir_z[IDX] = dir.z;
	rayhit.ray.org_x[IDX] = pos.x;
	rayhit.ray.org_y[IDX] = pos.y;
	rayhit.ray.org_z[IDX] = pos.z;
	rayhit.ray.tnear[IDX] = near_;
	rayhit.ray.tfar[IDX] = range;

	rayhit.ray.mask[IDX] = (uint32_t)(-1);
	rayhit.ray.flags[IDX] = 0;
	rayhit.ray.time[IDX] = 0;
	rayhit.ray.id[IDX] = 0;

	rayhit.hit.geomID[IDX] = RTC_INVALID_GEOMETRY_ID;
	rayhit.hit.primID[IDX] = RTC_INVALID_GEOMETRY_ID;

	rayhit.hit.instID[0][IDX] = RTC_INVALID_GEOMETRY_ID;
	rayhit.hit.instPrimID[0][IDX] = RTC_INVALID_GEOMETRY_ID;
}

// OFF PACKED PROCESSING
void GetEmbreeDeviceProperty(const char* msg, RTCDevice& device, RTCDeviceProperty prop)
{
	Msg(" - EmbreeDevProp: %s : %llu", msg, rtcGetDeviceProperty(device, prop));
}
  
extern global_claculation_data	gl_data;

// Exports Rcast Model !
void EmbreeRayTraceModel::BuildRcast()
{
	Status("Start Export Build.cform");
	TriangleContainer container;

	CTimer tStats;	tStats.Start();
	Status("[RcastModel] Capturing Faces...");
	for (auto F : lc_global_data()->g_faces())
	{
		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow)	continue;
		container.AddFaceRaw(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
	}


	for (auto ref : lc_global_data()->mu_refs())
	{
		xr_vector<FaceDataEmbree> temp_buffer;
		ref->export_cform_rcast_new(temp_buffer);
		for (auto& FaceIntel : temp_buffer)
		{
			Face* F = (Face*)FaceIntel.ptr;
			container.AddFaceRaw(F, FaceIntel.v1, FaceIntel.v2, FaceIntel.v3);
		}
	}

	container.RemoveDublicatesVertexs();	// Обезательно 
	container.RemoveDublicatesFaces();		// Обезательно 

	clMsg("Build.cform is builded at : %u ms", tStats.GetElapsed_ms());

	tStats.Start();

	string_path				fn;
	IWriter* MFS = FS.w_open(xr_strconcat(fn, pBuild->path, "build.cform"));
	xr_vector<b_rc_face>	rc_faces;
	rc_faces.resize(container.faces_cnt());

	// Prepare faces
	for (u32 k = 0; k < container.faces_cnt(); k++)
	{
		base_Face* F = container.dummy[k];

		b_rc_face& cf = rc_faces[k];
		cf.dwMaterial = F->dwMaterial;
		cf.dwMaterialGame = F->dwMaterialGame;

		Fvector2* cuv = F->getTC0();
		cf.t[0].set(cuv[0]);
		cf.t[1].set(cuv[1]);
		cf.t[2].set(cuv[2]);
	}
	MFS->open_chunk(0);

	// Header
	hdrCFORM hdr;
	hdr.version = CFormVersions::Vanilla;
	hdr.vertcount = (u32)container.vertex_cnt();
	hdr.facecount = (u32)container.faces_cnt();
	hdr.aabb = pBuild->scene_bb;

	MFS->w(&hdr, sizeof(hdr));

	// Data
	for (auto Vert : container.vertex())
	{
		MFS->w(&Vert, sizeof(Vert));
	}

	for (auto T : container.faces())
	{
		auto TRI = T.Get();
		MFS->w(&TRI, sizeof(TRI));
	}

	MFS->close_chunk();

	MFS->open_chunk(1);
	MFS->w(&*rc_faces.begin(), size_t(rc_faces.size() * sizeof(b_rc_face)));
	MFS->close_chunk();

	// size_t rqfaces_mem = rc_faces.size() * sizeof(b_rc_face);
	// size_t vertex_mem  = container.vertex_cnt() * sizeof(Fvector);
	// size_t faces_mem   = container.faces_cnt() * sizeof(CDB::TRI);
	// clMsg("Memory Vertex need: %u mb", u32(vertex_mem / 1024 / 1024));
	// clMsg("Memory Faces need: %u mb", faces_mem / 1024 / 1024);
	// clMsg("Memory RC_Face need: %u mb", rqfaces_mem / 1024 / 1024);
	// clMsg("File Saved Size: %u mb", MFS->tell() / 1024 / 1024);

	FS.w_close(MFS);

	clMsg("Build.cform is exported at : %u ms", tStats.GetElapsed_ms());
}


// Building Raytracing Model for Embree

// Instanced Geom
void EmbreeRayTraceModel::BuildMU_Model(xr_vector<FaceDataEmbree>& faces)
{
	opacue_geom.ClearAll();
	transp_geom.ClearAll();

	for (auto& F : faces)
	{
		bool isOpacue = ((Face*)F.ptr)->flags.bOpaque;
 		auto& buf = isOpacue ? opacue_geom : transp_geom;
		buf.AddFaceRaw((Face*)F.ptr, F.v1, F.v2, F.v3);
	}

	opacue_geom.RemoveDublicatesVertexs();			// Обезательно вызывать иначе не будет Vertex, Tris (Убрал жрание памяти при создании)
	opacue_geom.RemoveDublicatesFaces();

	transp_geom.RemoveDublicatesVertexs();
	transp_geom.RemoveDublicatesFaces();
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