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

// Для Загрузки Геометрии
#include <../xrForms/CompilersUI.h>
extern CompilersMode gCompilerMode;

void SetRay1(RTCRay& rayhit, Fvector& pos, Fvector& dir, float near_, float range)
{
	rayhit.dir_x = dir.x;
	rayhit.dir_y = dir.y;
	rayhit.dir_z = dir.z;
	rayhit.org_x = pos.x;
	rayhit.org_y = pos.y;
	rayhit.org_z = pos.z;
	rayhit.tnear = near_;
	rayhit.tfar = range;
	rayhit.mask = (unsigned int)(-1);
	rayhit.flags = 0;
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

// OFF PACKED PROCESSING
void GetEmbreeDeviceProperty(LPCSTR msg, RTCDevice& device, RTCDeviceProperty prop)
{
	clMsg(" - EmbreeDevProp: %s : %llu", msg, rtcGetDeviceProperty(device, prop));
}
 

IC bool	FaceEqual__(Face& F1, Face& F2)
{
	// Test for 6 variations
	if ((F1.v[0] == F2.v[0]) && (F1.v[1] == F2.v[1]) && (F1.v[2] == F2.v[2])) return true;
	if ((F1.v[0] == F2.v[0]) && (F1.v[2] == F2.v[1]) && (F1.v[1] == F2.v[2])) return true;
	if ((F1.v[2] == F2.v[0]) && (F1.v[0] == F2.v[1]) && (F1.v[1] == F2.v[2])) return true;
	if ((F1.v[2] == F2.v[0]) && (F1.v[1] == F2.v[1]) && (F1.v[0] == F2.v[2])) return true;
	if ((F1.v[1] == F2.v[0]) && (F1.v[0] == F2.v[1]) && (F1.v[2] == F2.v[2])) return true;
	if ((F1.v[1] == F2.v[0]) && (F1.v[2] == F2.v[1]) && (F1.v[0] == F2.v[2])) return true;
	return false;
}
  
void EmbreeData::BuildRaytraceModel( )
{
	static_geom.ClearAll();
	static_geom_transp.ClearAll();
 	
	CTimer t;	t.Start();
	Status("[RcastModel] Capturing Faces...");
	for (auto F : lc_global_data()->g_faces())
	{
		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow)	continue;
 					 
		b_material& M = inlc_global_data()->materials()[F->dwMaterial];
		b_texture& T = inlc_global_data()->textures()[M.surfidx];
 		if (F->flags.bOpaque || !T.pSurface || !T.bHasAlpha)
  			static_geom.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
 		else
 			static_geom_transp.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
 	}

 
	for (auto ref : lc_global_data()->mu_refs())
	{
		xr_vector<FaceDataIntel> temp_buffer;
  		ref->export_cform_rcast_new(temp_buffer);
		for (auto& FaceIntel : temp_buffer)
		{
 			Face* F = (Face*) FaceIntel.ptr;
 			b_material& M = inlc_global_data()->materials()[F->dwMaterial];
			b_texture& T = inlc_global_data()->textures()[M.surfidx];
			if (F->flags.bOpaque || !T.pSurface || !T.bHasAlpha)
				static_geom.AddFace(F, FaceIntel.v1, FaceIntel.v2, FaceIntel.v3);
			else
				static_geom_transp.AddFace(F, FaceIntel.v1, FaceIntel.v2, FaceIntel.v3);
		}
			
 	}
	Status("[RcastModel] Capturing Faces [%u ms]", t.GetElapsed_ms());

 	static_geom.RemoveDublicates();
	static_geom_transp.RemoveDublicates();
}

void EmbreeData::BuildRaytraceModel_2()
{
	CTimer t; t.Start();
	// Тут уже будет отфильтровано 
	static_geom.ClearAll();
 	static_geom.verts_v.swap(build_data.build_verts);
	static_geom.faces_v.resize(build_data.build_fcnt);

 	for (auto Fid = 0; Fid < build_data.build_faces.size(); Fid++ )
	{
		auto& FCDB = build_data.build_faces[Fid];
		static_geom.faces_v[Fid].point1 = FCDB.verts[0];
		static_geom.faces_v[Fid].point2 = FCDB.verts[1];
		static_geom.faces_v[Fid].point3 = FCDB.verts[2];
 	}

	// Чистим вектора
	build_data.build_faces.clear();
 	build_data.build_faces.shrink_to_fit();
 	build_data.build_fcnt = 0;
	build_data.build_vcnt = 0;

	clMsg("$[Embree] Loading Geometry Time: %u ms", t.GetElapsed_ms());
}

#include "../xrLC/Build.h"
extern CBuild* pBuild;

void EmbreeData::BuildRcast()
{ 
	Status("Start Export Build.cform");

   
	TriangleContainer container;
 
	CTimer t;	t.Start();
	Status("[RcastModel] Capturing Faces...");
	for (auto F : lc_global_data()->g_faces())
	{
		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow)	continue;
  		container.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
	}


	for (auto ref : lc_global_data()->mu_refs())
	{
		xr_vector<FaceDataIntel> temp_buffer;
		ref->export_cform_rcast_new(temp_buffer);
		for (auto& FaceIntel : temp_buffer)
		{
			Face* F = (Face*)FaceIntel.ptr;
			container.AddFace(F, FaceIntel.v1, FaceIntel.v2, FaceIntel.v3);
		}
 	}
	Status("[RcastModel] Capturing Faces [%u ms]", t.GetElapsed_ms());
 
	container.RemoveDublicates();

	
 	t.Start();

	string_path				fn;
	IWriter* MFS = FS.w_open(xr_strconcat(fn, pBuild->path, "build.cform"));
	xr_vector<b_rc_face>	rc_faces;
	rc_faces.resize(container.faces_cnt());
	
	// Prepare faces
	for (u32 k = 0; k < container.faces_cnt(); k++)
	{
 		base_Face* F		= container.dummy[k];
		
		b_rc_face& cf		= rc_faces[k];
		cf.dwMaterial		= F->dwMaterial;
		cf.dwMaterialGame	= F->dwMaterialGame;

		Fvector2* cuv		= F->getTC0();
		cf.t[0].set(cuv[0]);
		cf.t[1].set(cuv[1]);
		cf.t[2].set(cuv[2]);
	}
 	MFS->open_chunk(0);

	// Header
	hdrCFORM hdr;
	hdr.version		= CFORM_CURRENT_VERSION;
	hdr.vertcount	= (u32) container.vertex_cnt();
	hdr.facecount	= (u32) container.faces_cnt();
	hdr.aabb		= pBuild->scene_bb;
		
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
	MFS->w(&*rc_faces.begin(), size_t(rc_faces.size() * sizeof(b_rc_face)) );
	MFS->close_chunk();

	size_t rqfaces_mem = rc_faces.size() * sizeof(b_rc_face);
	size_t vertex_mem = container.vertex_cnt() * sizeof(Fvector);
	size_t faces_mem = container.faces_cnt() * sizeof(CDB::TRI);
	Msg("Memory Vertex need: %u mb", u32(vertex_mem / 1024 / 1024));
	Msg("Memory Faces need: %u mb", faces_mem / 1024 / 1024);
 	Msg("Memory RC_Face need: %u mb", rqfaces_mem / 1024 / 1024);
	Msg("File Saved Size: %u mb", MFS->tell() / 1024 / 1024);

	FS.w_close(MFS);

	Status("Ended Saving Faces: [%u ms]", t.GetElapsed_ms());
}

