#pragma once

#include "R_light.h"
#include "base_lighting.h"
#include "base_color.h"
#include "../../xrCore/Collision/xrCDB.h"

#include "xrFace.h"
#include <embree4/rtcore.h>

struct FaceDataIntel
{
	Fvector v1, v2, v3;
	void* ptr;
};

struct VertexEmbree
{
	typedef VertexEmbree Self;
	float x, y, z;

	void Set(Fvector& vertex);
	Fvector Get();

	bool Simular(Self& v)
	{
		return _abs(x - v.x) < EPS_L && _abs(y - v.y) < EPS_L && _abs(z - v.z) < EPS_L;
	}
};

struct TriEmbree
{
	u32 point1, point2, point3;
	void SetVertexes(CDB::TRI& triangle, Fvector* verts, VertexEmbree* emb_verts, size_t& last_index);
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

// ВАЖНЫЙ ПАРАМЕТР TNEAR Для пересечения с водой
void SetRay1(RTCRay& rayhit, Fvector& pos, Fvector& dir, float near_, float range);
void SetRay1(RTCRayHit& rayhit, Fvector& pos, Fvector& dir, float near_, float range);

struct TriangleContainer
{
	// HASH MAP VertexInumerate
	std::unordered_map<size_t, u32> vertex_map;
	struct Compare
	{
		VertexEmbree V;
		u32 vertID;
	};

	std::unordered_map<size_t, xr_vector<Compare>> hashTable;
	Fvector VMmin, VMscale, scale;

	xr_vector<VertexEmbree> verts_v;
	xr_vector<TriEmbree>	faces_v;
	xr_vector<Face*>		dummy;

	u32 find_or_add(Fvector& v);
	xr_vector<VertexEmbree>& vertex()
	{
		return verts_v;
	}

	xr_vector<TriEmbree>& faces()
	{
		return faces_v;
	}

	void AddFace(void* F, Fvector& v1, Fvector& v2, Fvector& v3);
 	void ClearAll();
};

// Vertex, Tri Buffers
class EmbreeData
{
public:
	/** NORMAL GEOM **/
	

	size_t BVH_size;
	size_t Static_size;
	size_t MU_size;

	TriangleContainer			static_geom;
	TriangleContainer			static_geom_transp;

	TriangleContainer			murefs_geom;
	TriangleContainer			murefs_geom_transp;
 
 	void GetGlobalData(size_t& static_mem, size_t& murefs_mem);
	void BuildRcast();

	
	// Loading 
	bool isInitialized = false;
	void RemoveGeometry(bool isDealloc);
	float RaytraceEmbreeProcess(R_Light& L, Fvector& P, Fvector& N, float range, void* skip);
	void InitializeGeometry(size_t& geom_static, size_t& geom_murefs);

	bool isAttached = false;
	size_t AttachGeometrys(bool addMU);

	void IntializeDevice();
	void IntelEmbereLOAD();
	void IntelEmbereUNLOAD();
};

extern EmbreeData EmbreeMain;
 
void GetEmbreeDeviceProperty(LPCSTR msg, RTCDevice& device, RTCDeviceProperty prop);