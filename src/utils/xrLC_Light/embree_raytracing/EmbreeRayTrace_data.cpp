#include "stdafx.h"
#include "xrDeflector.h"
#include "R_light.h"
#include "light_point.h"
#include "base_lighting.h"
#include "xrLC_GlobalData.h"

#include "EmbreeRayTrace.h"

// Для Загрузки Геометрии
#include "xrface.h"
#include "xrMU_Model_Reference.h"

void Embree::VertexEmbree::Set(Fvector& vertex)
{
	x = vertex.x;
	y = vertex.y;
	z = vertex.z;
}

Fvector Embree::VertexEmbree::Get()
{
	Fvector vertex;
	x = vertex.x;
	y = vertex.y;
	z = vertex.z;
	return vertex;
}

void Embree::TriEmbree::SetVertexes(CDB::TRI& triangle, Fvector* verts, VertexEmbree* emb_verts, size_t& last_index)
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

void Embree::TriEmbree::SetVertexes(Fvector* Vs, VertexEmbree* emb_verts, size_t& INDEX)
{
 	point1 = INDEX;
	point2 = INDEX + 1;
	point3 = INDEX + 2;
	emb_verts[INDEX].Set(Vs[0]);
	emb_verts[INDEX + 1].Set(Vs[1]);
	emb_verts[INDEX + 2].Set(Vs[2]);
 	INDEX += 3;
}


void Embree::SetRay1(RTCRay& rayhit, Fvector& pos, Fvector& dir, float near_, float range)
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

void Embree::SetRay1(RTCRayHit& rayhit, Fvector& pos, Fvector& dir, float near_, float range)
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
}

// OFF PACKED PROCESSING
void GetEmbreeDeviceProperty(LPCSTR msg, RTCDevice& device, RTCDeviceProperty prop)
{
	clMsg("EmbreeDevProp: %s : %llu", msg, rtcGetDeviceProperty(device, prop));
}

void Embree::errorFunction(void* userPtr, RTCError error, const char* str)
{
	clMsg("error %d: %s", error, str);
	DebugBreak();
}


void Embree::IntelEmbreeSettings(RTCDevice& device, bool avx, bool sse)
{
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_RAY_MASK_SUPPORTED", device, RTC_DEVICE_PROPERTY_RAY_MASK_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_BACKFACE_CULLING_ENABLED", device, RTC_DEVICE_PROPERTY_BACKFACE_CULLING_ENABLED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY4_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY4_SUPPORTED);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY8_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY8_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_IGNORE_INVALID_RAYS_ENABLED", device, RTC_DEVICE_PROPERTY_IGNORE_INVALID_RAYS_ENABLED);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_TASKING_SYSTEM", device, RTC_DEVICE_PROPERTY_TASKING_SYSTEM);
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

void Embree::GetGlobalData(
	size_t& FaceIndex,
	Embree::VertexEmbree* verts_embree, 
	Embree::TriEmbree* faces_embree, 
	xr_vector<void*>* dummy, bool useForOthers
)
{
 	xr_vector<Face*>			adjacent_vec(6 * 2 * 3);

	bool isCalculate = verts_embree == nullptr || faces_embree == nullptr || dummy == nullptr;

	size_t count_verts = 0;
	int FaceCurrent = 0;
	for (auto F : lc_global_data()->g_faces())
	{
		FaceCurrent++;

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
			if (Test == F) continue;
			if (!Test->flags.bProcessed) continue;
			if (FaceEqual__(*F, *Test))
			{
				bAlready = TRUE; break;
			}
		}

		if (!bAlready)
		{
 			if (!isCalculate)
			{
				F->flags.bProcessed = true;
				Fvector verts[3];
				verts[0] = F->v[0]->P; verts[1] = F->v[1]->P; verts[2] = F->v[2]->P;
				faces_embree[FaceIndex].SetVertexes(verts, verts_embree, count_verts);
				(*dummy)[FaceIndex] = F;
			}
			FaceIndex +=1;
		}

		Progress( float (FaceCurrent) / lc_global_data()->g_faces().size() );
	}

	auto& mu_refs = lc_global_data()->mu_refs();
	xr_vector<FaceDataIntel> temp_buffer;

	int RefID = 0;
	for (auto ref : mu_refs)
	{
		RefID++;
		temp_buffer.clear();
		ref->export_cform_rcast_new(temp_buffer);

		for (auto F : temp_buffer)
		{
			if (!isCalculate)
			{
				Fvector verts[3];
				verts[0] = F.v1; verts[1] = F.v2; verts[2] = F.v3;
				(*dummy)[FaceIndex] = F.ptr;
				faces_embree[FaceIndex].SetVertexes(verts, verts_embree, count_verts);
			}
			FaceIndex += 1;
		}

		Progress(float(RefID) / mu_refs.size());
	}

	// if (useForOthers)
	// {
	// 	Status("Saving...");
	// 	string_path				fn;
	// 
	// 	IWriter* MFS = FS.w_open(xr_strconcat(fn, pBuild->path, "build.cform"));
	// 	xr_vector<b_rc_face>	rc_faces;
	// 	rc_faces.resize(FaceIndex);
	// 	
	// 	// Prepare faces
	// 	for (u32 k = 0; k < FaceIndex; k++) 
	// 	{
	// 	  	base_Face* F = (base_Face*) (*dummy)[FaceIndex];;
	// 		b_rc_face& cf = rc_faces[k];
	// 		cf.dwMaterial = F->dwMaterial;
	// 		cf.dwMaterialGame = F->dwMaterialGame;
	// 		
	// 		Fvector2* cuv = F->getTC0();
	// 		cf.t[0].set(cuv[0]);
	// 		cf.t[1].set(cuv[1]);
	// 		cf.t[2].set(cuv[2]);
	// 	}
	// 	  
	// 	MFS->open_chunk(0);
	// 
	// 	// Header
	// 	hdrCFORM hdr;
	// 	hdr.version = CFORM_CURRENT_VERSION;
	// 	hdr.vertcount = (u32)CL.getVS();
	// 	hdr.facecount = (u32)CL.getTS();
	// 	hdr.aabb	  = scene_bb;
	// 	MFS->w(&hdr, sizeof(hdr));
	// 
	// 	// Data
	// 	MFS->w(CL.getV(), (u32)CL.getVS() * sizeof(Fvector));
	// 	MFS->w(CL.getT(), (u32)CL.getTS() * sizeof(CDB::TRI));
	// 	MFS->close_chunk();
	// 
	// 	MFS->open_chunk(1);
	// 	MFS->w(&*rc_faces.begin(), (u32)rc_faces.size() * sizeof(b_rc_face));
	// 	MFS->close_chunk();
	// 
	// 	FS.w_close(MFS);
	// 
	// }

}
