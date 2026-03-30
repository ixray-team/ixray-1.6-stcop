// SkeletonX.h: interface for the CSkeletonX class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "../../xrCore/Collision/cl_intersect.h"
#include "SkeletonCustom.h"
#include "FProgressive.h"
#include "../../xrEngine/EnnumerateVertices.h"

#pragma pack( push,2 )
struct vertBoned1W			// (3+3+3+3+2+1)*4 = 15*4 = 60 bytes
{
	Fvector	P;
	Fvector	N;
	Fvector	T;
	Fvector	B;
	float	u, v;
	u32		m;
};
struct vertBoned2W			// (1+3+3 + 1+3+3 + 2)*4 = 16*4 = 64 bytes
{
	u16		m[2];
	Fvector	P;
	Fvector	N;
	Fvector	T;
	Fvector	B;
	float	w;
	float	u, v;
};
struct vertBoned3W          // 70 bytes
{
	u16		m[3];
	Fvector	P;
	Fvector	N;
	Fvector	T;
	Fvector	B;
	float	w[2];
	float	u, v;
};
struct vertBoned4W       //76 bytes
{
	u16		m[4];
	Fvector	P;
	Fvector	N;
	Fvector	T;
	Fvector	B;
	float	w[3];
	float	u, v;
};
#pragma pack(pop)

ICF void get_pos_bones(const vertBoned1W& vert, Fvector& p, CBoneInstance* BI)
{
	BI[(u16)vert.m].mRenderTransform.transform_tiny(p, vert.P);
}

ICF void get_pos_bones(const vertBoned2W& vert, Fvector& p, CBoneInstance* BI)
{
	Fvector P0, P1;
	BI[vert.m[0]].mRenderTransform.transform_tiny(P0, vert.P);
	BI[vert.m[1]].mRenderTransform.transform_tiny(P1, vert.P);
	p.lerp(P0, P1, vert.w);
}

ICF void get_pos_bones(const vertBoned3W& vert, Fvector& p, CBoneInstance* BI)
{
	Fvector	P0, P1, P2;
	BI[vert.m[0]].mRenderTransform.transform_tiny(P0, vert.P);
	BI[vert.m[1]].mRenderTransform.transform_tiny(P1, vert.P);
	BI[vert.m[2]].mRenderTransform.transform_tiny(P2, vert.P);

	p = (P0 * vert.w[0]) + (P1 * vert.w[1]) + (P2 * (1.0f - vert.w[0] - vert.w[1]));
}
ICF void get_pos_bones(const vertBoned4W& vert, Fvector& P, CBoneInstance* BI)
{
	Fvector	P0, P1, P2, P3;
	BI[vert.m[0]].mRenderTransform.transform_tiny(P0, vert.P);
	BI[vert.m[1]].mRenderTransform.transform_tiny(P1, vert.P);
	BI[vert.m[2]].mRenderTransform.transform_tiny(P2, vert.P);
	BI[vert.m[3]].mRenderTransform.transform_tiny(P3, vert.P);

	P = (P0 * vert.w[0]) + (P1 * vert.w[1]) + (P2 * vert.w[2]) + (P3 * (1.0f - vert.w[0] - vert.w[1] - vert.w[2]));
}

template<typename T_vertex>
ICF BOOL pick_bone(ref_smem<T_vertex> vertices, CBoneInstance* Bones, IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, u16* indices, CBoneData::FacesVec& faces)
{
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
			get_pos_bones(vertices[indices[idx + k]], r.tri[k], Bones);

		float u, v;
		r.dist = flt_max;
		if (CDB::TestRayTri(S, D, r.tri, u, v, r.dist, true) && (r.dist < dist))
		{
			r.normal.mknormal(r.tri[0], r.tri[1], r.tri[2]);
			return TRUE;
		};
	}
	return FALSE;
}

template <typename vertex_buffer_type>
ICF void enum_verts(vertex_buffer_type vertices, u16* indices, CBoneData::FacesVec& faces, SEnumVerticesCallback& C, CBoneInstance* Bones)
{
	Fvector P;
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
		{
			get_pos_bones(vertices[indices[idx + k]], P, Bones);
			C(P);
		}
	}
}

struct SEnumVerticesCallback;
class 	CSkeletonX : public FProgressive
{
protected:
	enum { vertRenderFVF = D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX1 };
	enum { RM_SKINNING_SOFT, RM_SINGLE, RM_SKINNING_1B, RM_SKINNING_2B, RM_SKINNING_3B, RM_SKINNING_4B};

	CKinematics* Parent = nullptr; // setted up by parent
	ref_smem<vertBoned1W> Vertices1W; // shared
	ref_smem<vertBoned2W> Vertices2W; // shared
	ref_smem<vertBoned3W> Vertices3W; // shared
	ref_smem<vertBoned4W> Vertices4W; // shared
	ref_smem<u16> BonesUsed; // actual bones which have influence on vertices
	ref_smem<u16> m_Indices; //	Index buffer replica

	u16 RenderMode = RM_SKINNING_SOFT;
	u16 ChildIDX = u16(-1);
	bool progressive_mesh = false;
	// render-mode specifics
	union {
		struct {			// soft-skinning only
			u32				cache_DiscardID;
			u32				cache_vCount;
			u32				cache_vOffset;
		};
		u32					RMS_boneid;			// single-bone-rendering
		u32					RMS_bonecount;		// skinning, maximal bone ID
	};

	void _Copy(CSkeletonX *V);
	void _Render_soft(ref_geom& hGeom, u32 vCount, u32 iOffset, u32 pCount);
	void _Render(ref_geom& hGeom, u32 vCount, u32 iOffset, u32 pCount);
	void _Load(const char* N, IReader *data, u32& dwVertCount);
	void _Load_hw(Fvisual& V, void* data);
	void _CollectBoneFaces(Fvisual* V, u32 iBase, u32 iCount);

	void fill_verts1W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, u16* indices, CBoneData::FacesVec& faces);
	void fill_verts2W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, u16* indices, CBoneData::FacesVec& faces);
	void fill_verts3W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, u16* indices, CBoneData::FacesVec& faces);
	void fill_verts4W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, u16* indices, CBoneData::FacesVec& faces);
	void _FillVertices(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount);

	BOOL _PickBone(IKinematics::pick_result &r, float range, const Fvector& S, const Fvector& D, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount);
	void _EnumBoneVertices(SEnumVerticesCallback& C, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount);
public:
	BOOL has_visible_bones();
	CSkeletonX(bool val) : progressive_mesh(val) {}

	virtual void Copy(dxRender_Visual* V)
	{
		if (progressive_mesh)
			FProgressive::Copy(V);
		else
			Fvisual::Copy(V);
		_Copy((CSkeletonX*)V);
	}
	virtual void Release()
	{
		if (progressive_mesh)
			FProgressive::Release();
		else
			Fvisual::Release();
	}

	virtual void Render(float LOD);
	virtual void Load(const char* N, IReader* data, u32 dwFlags);

	void SetParent(CKinematics* K) { Parent = K; }
	void AfterLoad(CKinematics* parent, u16 child_idx);

	ICF BOOL PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
	{
		if (progressive_mesh)
		{
			FSlideWindow& SW = nSWI.sw[0];
			return _PickBone(r, dist, start, dir, this, bone_id, iBase + SW.offset, SW.num_tris * 3);
		}
		else
			return _PickBone(r, dist, start, dir, this, bone_id, iBase, iCount);
	}
	ICF void FillVertices(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, u16 bone_id)
	{
		if (progressive_mesh)
		{
			FSlideWindow& SW = nSWI.sw[0];
			_FillVertices(view, wm, normal, size, this, bone_id, iBase + SW.offset, SW.num_tris * 3);
		}
		else
			_FillVertices(view, wm, normal, size, this, bone_id, iBase, iCount);
	}
	ICF void EnumBoneVertices(SEnumVerticesCallback& C, u16 bone_id)
	{
		if(progressive_mesh)
		{
			FSlideWindow& SW = nSWI.sw[0];
			_EnumBoneVertices(C, this, bone_id, iBase + SW.offset, SW.num_tris * 3);
		}
		else
			_EnumBoneVertices(C, this, bone_id, iBase, iCount);
	}


protected:
	void _DuplicateIndices(const char* N, IReader *data);
};
