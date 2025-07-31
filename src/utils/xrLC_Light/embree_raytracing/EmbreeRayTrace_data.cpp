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
	x = vertex.x;
	y = vertex.y;
	z = vertex.z;
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

	Status("Capturing Faces...");

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

	static_mem = GetMemory() - s;
	Static_size = static_mem;
  
	s = GetMemory();
	Status("Capturing MU-Ref Faces...");

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

	Status("Capturing MU-Ref Faces... Ended");
}
#include "../xrLC/Build.h"
void EmbreeData::BuildRcast()
{
	Phase("Export Build.cform");
	//TriangleContainer container;
	 
	CDB::CollectorPacked CPacked(pBuild->scene_bb, (int)lc_global_data()->g_vertices().size(), (int)lc_global_data()->g_faces().size());

	xr_vector<Face*>			adjacent_vec(6 * 2 * 3);
  	for (auto F : lc_global_data()->g_faces())
	{
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
			CPacked.add_face_D(F->v[0]->P, F->v[1]->P, F->v[2]->P, convert_nax(F), 0);
			//container.AddFace(F, F->v[0]->P, F->v[1]->P, F->v[2]->P);
 		}
	}
  
	for (auto ref : lc_global_data()->mu_refs())
	{
		xr_vector<FaceDataIntel> temp_buffer;
		ref->export_cform_rcast_new(temp_buffer);
		for (auto pF : temp_buffer)
		{
			Face* F = (Face*) pF.ptr;
			CPacked.add_face_D(pF.v1, pF.v2, pF.v3, convert_nax(F), 0);
  			//container.AddFace(pF.ptr, pF.v1, pF.v2, pF.v3);
		}
 	}
   
 	{
		string_path				fn;

		IWriter* MFS = FS.w_open(xr_strconcat(fn, pBuild->path, "build.cform"));
		xr_vector<b_rc_face>	rc_faces;
		rc_faces.resize(CPacked.getTS());
	
		// Prepare faces
		for (u32 k = 0; k < CPacked.getTS(); k++)
		{
 			base_Face* F		= convert_nax(k); //container.dummy[k];
		
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
		hdr.vertcount	= (u32) CPacked.getVS();
		hdr.facecount	= (u32) CPacked.getTS();
		hdr.aabb		= pBuild->scene_bb;
		
		MFS->w(&hdr, sizeof(hdr));

		// Data
 		MFS->w(CPacked.getV(), (u32)CPacked.getVS() * sizeof(Fvector));
		MFS->w(CPacked.getT(), (u32)CPacked.getTS() * sizeof(CDB::TRI));

		MFS->close_chunk();

		MFS->open_chunk(1);
		MFS->w(&*rc_faces.begin(), (u32)rc_faces.size() * sizeof(b_rc_face));
		MFS->close_chunk();

		FS.w_close(MFS);
	}
}



#include "../xrLC/Build.h"

extern CBuild* pBuild;
 
u32 TriangleContainer::find_or_add(Fvector& V)
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
 
#define CompactingVertexes
void TriangleContainer::AddFace(void* F, Fvector& v1, Fvector& v2, Fvector& v3)
{
 	int IDX = vertex().size();
	
	VertexEmbree vert1, vert2, vert3;
	vert1.Set(v1), vert2.Set(v2), vert3.Set(v3);

	TriEmbree triangle;

#ifdef CompactingVertexes
		triangle.point1 = find_or_add(v1);
		triangle.point2 = find_or_add(v2);
		triangle.point3 = find_or_add(v3);
#else 
		triangle.point1 = IDX;
		triangle.point2 = IDX + 1;
		triangle.point3 = IDX + 2;
		vertex().push_back(vert1);
		vertex().push_back(vert2);
		vertex().push_back(vert3);
#endif

	faces().push_back(triangle);
	dummy.push_back((Face*)F);
}
 
// void EmbreeData::TriangleContainer::GetFace(int ID)
// {
// 	v1 = vertex()[faces()[ID].point1].Get();
// 	v2 = vertex()[faces()[ID].point2].Get();
// 	v3 = vertex()[faces()[ID].point3].Get();
// }

void TriangleContainer::ClearAll()
{
 	hashTable.clear();
	dummy.clear();
	faces_v.clear();
	verts_v.clear();

	faces_v.shrink_to_fit();
	verts_v.shrink_to_fit();
}
 