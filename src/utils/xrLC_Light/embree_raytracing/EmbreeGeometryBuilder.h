#pragma once

#include "../../../xrCore/Collision/xrCDB.h"
#include "xrFace.h"
 
struct TriEmbree
{
	u32 point1, point2, point3;
 
	CDB::TRI Get()
	{
		CDB::TRI tri;
		tri.verts[0] = point1;
		tri.verts[1] = point2;
		tri.verts[2] = point3;
		return tri;
	}

	void Set(CDB::TRI& T)
	{
		point1 = T.verts[0];
		point2 = T.verts[1];
		point3 = T.verts[2];
	}
};


struct TriangleContainer
{
  	xr_vector<Fvector>		verts_v;
	xr_vector<TriEmbree>	faces_v;
	xr_vector<Face*>		dummy;

	xr_vector<Fvector>& vertex() { return verts_v; }
	xr_vector<TriEmbree>& faces() { return faces_v; }
	u32 vertex_cnt() { return verts_v.size(); }
	u32 faces_cnt() { return faces_v.size(); }
 	
	// Add Faces
	size_t AddVertex(Fvector& V);
	void AddFace	(void* F, Fvector& v1, Fvector& v2, Fvector& v3);
  	void ClearAll();

 	// Removeing Dublicates
	struct IndexedVertex
	{
		Fvector v;
		uint32_t originalIndex;
	};

	// Face Raw
	struct FaceRaw
	{
		Fvector v[3];
		Face* F;
	};

	xr_vector<FaceRaw> raw_faces;
	void AddFaceRaw(Face* F, const Fvector& v1, const Fvector& v2, const Fvector& v3)
	{
		raw_faces.push_back({ {v1, v2, v3}, F });
	};
	void RemoveDublicates();
};