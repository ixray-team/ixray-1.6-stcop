#pragma once

#include "../../../xrCore/Collision/xrCDB.h"
#include "xrFace.h"
 
struct Triangle
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

struct CFormTriangle  
{
	u16 MaterialID;
	u16 Sector;
};


struct IndexedTri
{
	uint32_t i1, i2, i3;
	uint32_t originalIndex;

	IndexedTri(const Triangle& tri, uint32_t idx)		: originalIndex(idx)
	{
		// нормализуем пор€док вершин (сортировка трЄх чисел)
		i1 = tri.point1;
		i2 = tri.point2;
		i3 = tri.point3;

		if (i1 > i2) std::swap(i1, i2);
		if (i2 > i3) std::swap(i2, i3);
		if (i1 > i2) std::swap(i1, i2);
	}

	bool operator<(const IndexedTri& other) const
	{
		if (i1 != other.i1) return i1 < other.i1;
		if (i2 != other.i2) return i2 < other.i2;
		return i3 < other.i3;
	}

	bool similar(const IndexedTri& other) const
	{
		return i1 == other.i1 && i2 == other.i2 && i3 == other.i3;
	}
};

struct TriangleContainer
{
  	xr_vector<Fvector>				verts_v;
	xr_vector<Triangle>				faces_v;

	xr_vector<CFormTriangle>		cform_data;
	xr_vector<Face*>				dummy;

	xr_vector<Fvector>& vertex() { return verts_v; }
	xr_vector<Triangle>& faces() { return faces_v; }
	u32 vertex_cnt() { return verts_v.size(); }
	u32 faces_cnt() { return faces_v.size(); }
 	
	// Add Faces
	size_t AddVertex(Fvector& V);
	void AddFace	(void* F, Fvector& v1, Fvector& v2, Fvector& v3);
	void AddFaceMaterial (void* F, Fvector& v1, Fvector& v2, Fvector& v3, u16 Material, u16 SectorID);
	
	void ClearAll();

	CDB::TRI GetCDBMaterial(u32 IndexTriangle)
	{
		CDB::TRI tri;
		tri.verts[0] = faces_v[IndexTriangle].point1;
		tri.verts[1] = faces_v[IndexTriangle].point2;
		tri.verts[2] = faces_v[IndexTriangle].point3;

		tri.material = cform_data[IndexTriangle].MaterialID;
		tri.sector   = cform_data[IndexTriangle].Sector;
		return tri;
	}

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

		u16 material;
		u16 Sector;
 	};

	xr_vector<FaceRaw> raw_faces;
	void AddFaceRaw(Face* F, const Fvector& v1, const Fvector& v2, const Fvector& v3)
	{
		raw_faces.push_back({ {v1, v2, v3}, F, 0, 0 });
	};

	void AddFaceRawMaterial(Face* F, const Fvector& v1, const Fvector& v2, const Fvector& v3, u16 MaterialID, u16 SectorID)
	{
		raw_faces.push_back({ {v1, v2, v3}, F, MaterialID, SectorID });
	};


	void RemoveDublicates();
	void RemoveDublicatesFaces();
};