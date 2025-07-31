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


void VertexEmbree::Set(Fvector& vertex)
{
	x = vertex.x;
	y = vertex.y;
	z = vertex.z;
}

Fvector VertexEmbree::Get()
{
	Fvector vertex;
	vertex.x = x;
	vertex.y = y;
	vertex.z = z;
	return vertex;
}

void TriEmbree::SetVertexes(CDB::TRI& triangle, Fvector* verts, VertexEmbree* emb_verts, size_t& last_index)
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
  
extern size_t GetMemory();
void EmbreeData::GetGlobalData(size_t& static_mem, size_t& murefs_mem)
{
	static_geom.ClearAll();
	static_geom_transp.ClearAll();
	murefs_geom.ClearAll();
	murefs_geom_transp.ClearAll();

  	xr_vector<Face*>			adjacent_vec(6 * 2 * 3);
	
	size_t s = GetMemory();

	Status("[RcastModel] Capturing Faces...");

	CTimer t; t.Start();

	int ProgressID = 0;
	for (auto F : lc_global_data()->g_faces())
	{
		Progress(float(ProgressID) / float(lc_global_data()->g_faces().size()));
		ProgressID++;

		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow)
			continue;

		b_material& M = lc_global_data()->materials()[F->dwMaterial];
		// Collect
		adjacent_vec.clear();
		for (int vit = 0; vit < 3; ++vit)
		{
			Vertex* V = F->v[vit];
			for (u32 adj = 0; adj < V->m_adjacents.size(); adj++)
			{
				adjacent_vec.push_back(V->m_adjacents[adj]);
			}
		}
		
		std::sort(adjacent_vec.begin(), adjacent_vec.end());
		adjacent_vec.erase(std::unique(adjacent_vec.begin(), adjacent_vec.end()), adjacent_vec.end());
		
		// Unique
		BOOL			bAlready = FALSE;
		 
		for (u32 ait = 0; ait < adjacent_vec.size(); ++ait)
		{
			Face* Test = adjacent_vec[ait];
			if (Test == F)
				continue;
			if (!Test->flags.bProcessed)
				continue;
			if (FaceEqual__(*F, *Test))
			{
				bAlready = TRUE;
				break;
			}
		}
	 
		if (!bAlready)
 		{
			F->flags.bProcessed = true;
			 
			b_material& M = inlc_global_data()->materials()[F->dwMaterial];
			b_texture& T = inlc_global_data()->textures()[M.surfidx];
 			if (F->flags.bOpaque || !T.pSurface || !T.bHasAlpha)
			{
 				static_geom.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
			}
			else
			{
				static_geom_transp.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
			}
		}
	}

	Status("[RcastModel] Capturing Faces End [%u ms]", t.GetElapsed_ms());

	static_mem = GetMemory() - s;
	Static_size = static_mem;
  
	s = GetMemory();
	Status("[RcastModel] Capturing MU-Ref Faces...");

	t.Start();

	ProgressID = 0;
	for (auto ref : lc_global_data()->mu_refs())
	{
		Progress(float(ProgressID) / float(lc_global_data()->mu_refs().size()));
		ProgressID++;

		xr_vector<FaceDataIntel> temp_buffer;
  		ref->export_cform_rcast_new(temp_buffer);
		for (auto pF : temp_buffer)
		{
			Face* F = (Face*) pF.ptr;

			b_material& M = inlc_global_data()->materials()[F->dwMaterial];
			b_texture& T = inlc_global_data()->textures()[M.surfidx];
			if (F->flags.bOpaque || !T.pSurface || !T.bHasAlpha)
				murefs_geom.AddFace(pF.ptr, pF.v1, pF.v2, pF.v3);
			else
				murefs_geom_transp.AddFace(pF.ptr, pF.v1, pF.v2, pF.v3);
		}
			
 	}
	murefs_mem = GetMemory() - s;
	MU_size = murefs_mem;

	Status("[RcastModel] Capturing MU Faces End [%u ms]", t.GetElapsed_ms());

}

#include "../xrLC/Build.h"
extern CBuild* pBuild;

void EmbreeData::BuildRcast()
{ 
	Status("Start Export Build.cform");

	CTimer t; t.Start();
 
	TriangleContainer container;
	container.AddOther(static_geom);
	container.AddOther(static_geom_transp);
	container.AddOther(murefs_geom);
	container.AddOther(murefs_geom_transp);

	
	Status("Ended Capturing Faces : [%u ms]", t.GetElapsed_ms());
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
	for (auto V : container.vertex())
	{
		auto Vert = V.Get();
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


u32 TriangleContainer::find_or_add(Fvector V)
{
	VertexEmbree new_vertex;
	new_vertex.Set(V);

	u32 ix = iFloor(V.x);
	u32 iy = iFloor(V.y);
	u32 iz = iFloor(V.z);

	// Generate hash key
	size_t hashKey = std::hash<u32>()(ix) ^ std::hash<u32>()(iy) ^ std::hash<u32>()(iz);
	auto itHash = hashTable.find(hashKey);
	if (itHash != hashTable.end())
	{
		Vertex* parsed = nullptr;
		for (auto& vertex : itHash->second)
		{
			if (vertex.V.Simular(new_vertex))
				return vertex.vertID; // Нашли похожую вершину
		}
	}

	verts_v.push_back(new_vertex);

	u32 VertexID = verts_v.size() - 1;

	Compare data;
	data.V = verts_v.back();
	data.vertID = VertexID;
	hashTable[hashKey].push_back(data);
	return VertexID;
}

u32 TriangleContainer::find_or_add(VertexEmbree v)
{
	return find_or_add( v.Get() ) ;
}
 
void TriangleContainer::AddFace(void* F, Fvector& v1, Fvector& v2, Fvector& v3)
{	
	TriEmbree triangle;
 	triangle.point1 = find_or_add(v1);
	triangle.point2 = find_or_add(v2);
	triangle.point3 = find_or_add(v3);
 	faces().push_back(triangle);
	dummy.push_back((Face*)F);
}

void TriangleContainer::AddFaceCopy(int Index, TriangleContainer& container)
{
	auto& Face = container.faces_v[Index];
	Fvector v1 = container.verts_v[Face.point1].Get();
	Fvector v2 = container.verts_v[Face.point2].Get();
	Fvector v3 = container.verts_v[Face.point3].Get();

	TriEmbree triangle;
	triangle.point1 = find_or_add(v1);
	triangle.point2 = find_or_add(v2);
	triangle.point3 = find_or_add(v3);
	faces().push_back(triangle);
 	dummy.push_back(container.dummy[Index]);
}
 
void TriangleContainer::AddOther(TriangleContainer& container)
{
 	for (int INDEX = 0; INDEX < container.faces_cnt(); INDEX++)
 		AddFaceCopy(INDEX, container);
}

void TriangleContainer::ClearAll()
{
 	hashTable.clear();
	dummy.clear();
	faces_v.clear();
	verts_v.clear();

	faces_v.shrink_to_fit();
	verts_v.shrink_to_fit();
}
 