#pragma once

#include "../../../xrCore/Collision/xrCDB.h"
 
struct FaceDataEmbree
{
	void*				ptr;
	Fvector				v1, v2, v3;

	bool				bOpaque = false;
	u16					dwMaterial;
	u32					dwMaterialGame;
	Fvector2			TC[3];				// TC
	Fvector2*			getTC0() { return TC; }
 
	void SetFace(Fvector& v_1, Fvector& v_2, Fvector& v_3, void* P)
	{
		v1 = v_1;
		v2 = v_2;
		v3 = v_3;
		ptr = P;
	};

	void SetMaterial(u16 dwMt, u32 dwMtGame, Fvector2* TCn )
	{
		dwMaterial = dwMt;
		dwMaterialGame = dwMtGame;
		TC[0] = TCn[0];
		TC[1] = TCn[1];
		TC[2] = TCn[2];
	}
};

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

 	bool isDegenerated() const
	{
		return i1 == i2 || i2 == i3 || i3 == i1;
	}
};

struct TriangleContainer
{
	bool useMsg = true;
  	xr_vector<Fvector>				verts_v;
	xr_vector<Triangle>				faces_v;

 	xr_vector<void*>				dummy;

	xr_vector<Fvector>&				vertex() { return verts_v; }
	xr_vector<Triangle>&			faces() { return faces_v; }
	u32 vertex_cnt() { return verts_v.size(); }
	u32 faces_cnt() { return faces_v.size(); }
 	
	// Add Faces
	void ClearAll();
	void ClearFaces();

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
		void* Face;

		u16 material;
		u16 Sector;
 	};

	xr_vector<FaceRaw> raw_faces;
	void AddFaceRaw(void* F, const Fvector& v1, const Fvector& v2, const Fvector& v3)
	{
		raw_faces.push_back({ {v1, v2, v3}, F, 0, 0 });
	};
 
	void RemoveDublicatesVertexs();
	void RemoveDublicatesFaces();
	void RemoveDublicates()
	{
		RemoveDublicatesVertexs();
		RemoveDublicatesFaces();
	};
};