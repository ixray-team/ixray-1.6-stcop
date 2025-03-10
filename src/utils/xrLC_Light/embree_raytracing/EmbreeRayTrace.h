#pragma once

#include "R_light.h"
#include "base_lighting.h"
#include "base_color.h"
#include "../../XrCDB/xrCDB.h"

#include <embree4/rtcore.h>

struct FaceDataIntel
{
	Fvector v1, v2, v3;
	void* ptr;
};

// Vertex, Tri Buffers
namespace Embree
{
	

	struct VertexEmbree
	{
		float x, y, z;

		void Set(Fvector& vertex);
		Fvector Get();
	};

	struct TriEmbree
	{
		u32 point1, point2, point3;
		void SetVertexes(CDB::TRI& triangle, Fvector* verts, VertexEmbree* emb_verts, size_t& last_index);
		// Для релизации без алокации лишней
		void SetVertexes(Fvector* Vs, VertexEmbree* emb_verts, size_t& last_index);
	};


	// ВАЖНЫЙ ПАРАМЕТР TNEAR Для пересечения с водой
	void SetRay1(RTCRay& rayhit, Fvector& pos, Fvector& dir, float near_, float range);
	void SetRay1(RTCRayHit& rayhit, Fvector& pos, Fvector& dir, float near_, float range);


	void errorFunction(void* userPtr, enum RTCError error, const char* str);
	void IntelEmbreeSettings(RTCDevice& device, bool avx, bool sse);
	void GetGlobalData(size_t& counts_faces, Embree::VertexEmbree* verts_embree, Embree::TriEmbree* faces_embree, xr_vector<void*>* dummy, bool useForOthers = false);
}