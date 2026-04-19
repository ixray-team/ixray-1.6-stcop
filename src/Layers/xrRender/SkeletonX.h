// SkeletonX.h: interface for the CSkeletonX class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "../../xrCore/Collision/cl_intersect.h"
#include "SkeletonCustom.h"
#include "FProgressive.h"
#include "../../xrEngine/EnnumerateVertices.h"

#pragma pack( push,2 )
struct vertBoned1W // (3+3+3+3+2+1)*4 = 15*4 = 60 bytes
{
	Fvector	P;
	Fvector	N;
	Fvector	T;
	Fvector	B;
	float	u, v;
	u32		m;
};
struct vertBoned2W // (1+3+3 + 1+3+3 + 2)*4 = 16*4 = 64 bytes
{
	u16		m[2];
	Fvector	P;
	Fvector	N;
	Fvector	T;
	Fvector	B;
	float	w;
	float	u, v;
};
struct vertBoned3W // 70 bytes
{
	u16		m[3];
	Fvector	P;
	Fvector	N;
	Fvector	T;
	Fvector	B;
	float	w[2];
	float	u, v;
};
struct vertBoned4W //76 bytes
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

template<typename T_vertex>
ICF void get_pos_bones(T_vertex& vert, Fvector& p, CBoneInstance* BI)
{
	if constexpr (std::is_same_v<T_vertex, vertBoned1W>)
	{
		BI[(u16)vert.m].mRenderTransform.transform_tiny(p, vert.P);
	}
	else if constexpr (std::is_same_v<T_vertex, vertBoned2W>)
	{
		Fvector P0, P1;
		BI[vert.m[0]].mRenderTransform.transform_tiny(P0, vert.P);
		BI[vert.m[1]].mRenderTransform.transform_tiny(P1, vert.P);
		p.lerp(P0, P1, vert.w);
	}
	else if constexpr (std::is_same_v<T_vertex, vertBoned3W>)
	{
		Fvector	P0, P1, P2;
		BI[vert.m[0]].mRenderTransform.transform_tiny(P0, vert.P);
		BI[vert.m[1]].mRenderTransform.transform_tiny(P1, vert.P);
		BI[vert.m[2]].mRenderTransform.transform_tiny(P2, vert.P);

		p = (P0 * vert.w[0]) + (P1 * vert.w[1]) + (P2 * (1.0f - vert.w[0] - vert.w[1]));
	}
	else if constexpr (std::is_same_v<T_vertex, vertBoned4W>)
	{
		Fvector	P0, P1, P2, P3;
		BI[vert.m[0]].mRenderTransform.transform_tiny(P0, vert.P);
		BI[vert.m[1]].mRenderTransform.transform_tiny(P1, vert.P);
		BI[vert.m[2]].mRenderTransform.transform_tiny(P2, vert.P);
		BI[vert.m[3]].mRenderTransform.transform_tiny(P3, vert.P);

		p = (P0 * vert.w[0]) + (P1 * vert.w[1]) + (P2 * vert.w[2]) + (P3 * (1.0f - vert.w[0] - vert.w[1] - vert.w[2]));
	}
}

template<typename T_vertex>
ICF bool pick_bone(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, u16* indices, CBoneData::FacesVec& faces, ref_smem<T_vertex> vertices, CBoneInstance* BI)
{
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
			get_pos_bones(vertices[indices[idx + k]], r.tri[k], BI);

		float u, v;
		r.dist = flt_max;
		if (CDB::TestRayTri(S, D, r.tri, u, v, r.dist, true) && (r.dist < dist))
		{
			r.normal.mknormal(r.tri[0], r.tri[1], r.tri[2]);
			return true;
		};
	}
	return false;
}

template<typename T_vertex>
ICF void fill_wm_verts(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size,
	u16* indices, CBoneData::FacesVec& faces, ref_smem<T_vertex> vertices, CBoneInstance* BI)
{
	Fvector p[3];
	Fvector test_normal, UV;
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
			get_pos_bones(vertices[indices[idx + k]], p[k], BI);

		test_normal.mknormal(p[0], p[1], p[2]);
		float cosa = test_normal.dotproduct(normal);
		if (cosa < EPS) continue;
		if (CDB::TestSphereTri(wm.ContactPoint(), size, p))
		{
			CSkeletonWallmark::WMFace& F = wm.m_Faces.emplace_back();

			for (u32 k = 0; k < 3; k++)
			{
				T_vertex& vert = vertices[indices[idx + k]];
				F.vert[k] = vert.P;

				if constexpr (std::is_same_v<T_vertex, vertBoned1W>)
				{
					F.bone_id[k][0] = (u16)vert.m;
				}
				else if constexpr (std::is_same_v<T_vertex, vertBoned2W>)
				{
					F.bone_id[k][0] = vert.m[0];
					F.bone_id[k][1] = vert.m[1];
					F.weight[k][0] = vert.w;
				}
				else if constexpr (std::is_same_v<T_vertex, vertBoned3W>)
				{
					F.bone_id[k][0] = vert.m[0];
					F.bone_id[k][1] = vert.m[1];
					F.bone_id[k][2] = vert.m[2];
					F.weight[k][0] = vert.w[0];
					F.weight[k][1] = vert.w[1];
				}
				else if constexpr (std::is_same_v<T_vertex, vertBoned4W>)
				{
					F.bone_id[k][0] = vert.m[0];
					F.bone_id[k][1] = vert.m[1];
					F.bone_id[k][2] = vert.m[2];
					F.bone_id[k][3] = vert.m[3];
					F.weight[k][0] = vert.w[0];
					F.weight[k][1] = vert.w[1];
					F.weight[k][2] = vert.w[2];
				}

				Fvector2& uv = F.uv[k];
				view.transform_tiny(UV, p[k]);
				uv.x = (1.f + UV.x) * .5f;
				uv.y = (1.f - UV.y) * .5f;
			}
		}
	}
}

template <typename T_vertex, typename T_Enum>
ICF void enum_verts(T_Enum& output, u16* indices, CBoneData::FacesVec& faces, ref_smem<T_vertex> vertices, CBoneInstance* BI)
{
	if constexpr (std::is_same_v<T_Enum, xr_vector<Fvector>> || std::is_same_v<T_Enum, buffer_vector<Fvector>>)
		output.reserve(output.size()+faces.size() * 3);

	Fvector P;
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
		{
			get_pos_bones(vertices[indices[idx + k]], P, BI);

			if constexpr (std::is_same_v<T_Enum, SEnumVerticesCallback>)
				output(P);

			if constexpr (std::is_same_v<T_Enum, xr_vector<Fvector>> || std::is_same_v<T_Enum, buffer_vector<Fvector>>)
				output.push_back(P);
		}
	}
}

class CSkeletonX : public FProgressive
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

	void _Render_soft(ref_geom& hGeom, u32 vCount, u32 iOffset, u32 pCount);
	void _Render(ref_geom& hGeom, u32 vCount, u32 iOffset, u32 pCount);
	void _Load(const char* N, IReader *data, u32& dwVertCount);
	void _Load_hw(void* data);
	void _CollectBoneFaces();
	void _DuplicateIndices(IReader* data);
public:
	bool has_visible_bones();
	CSkeletonX(bool val) : progressive_mesh(val) {}

	virtual void Copy(dxRender_Visual* V);
	virtual void Release();

	virtual void Render(float LOD);
	virtual void Load(const char* N, IReader* data, u32 dwFlags);

	void SetParent(CKinematics* K) { Parent = K; }
	void AfterLoad(CKinematics* parent, u16 child_idx);

	ICF bool PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
	{
		VERIFY(Parent && (ChildIDX != u16(-1)));
		CBoneData& BD = Parent->LL_GetData(bone_id);
		CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];

		u16* indices{ nullptr };

		if (progressive_mesh)
			indices = *m_Indices + iBase + nSWI.sw[0].offset;
		else
			indices = *m_Indices + iBase;

		if (*Vertices1W)
			return pick_bone(r, dist, start, dir, indices, faces, Vertices1W, Parent->bone_instances);
		else if (*Vertices2W)
			return pick_bone(r, dist, start, dir, indices, faces, Vertices2W, Parent->bone_instances);
		else if (*Vertices3W)
			return pick_bone(r, dist, start, dir, indices, faces, Vertices3W, Parent->bone_instances);
		else if (*Vertices4W)
			return pick_bone(r, dist, start, dir, indices, faces, Vertices4W, Parent->bone_instances);

		return false;
	}

	ICF void FillWMVertices(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, u16 bone_id)
	{
		VERIFY(Parent && (ChildIDX != u16(-1)));
		CBoneData& BD = Parent->LL_GetData(bone_id);
		CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];

		u16* indices{ nullptr };

		if (progressive_mesh)
			indices = *m_Indices + iBase + nSWI.sw[0].offset;
		else
			indices = *m_Indices + iBase;

		if (*Vertices1W) fill_wm_verts(view, wm, normal, size, indices, faces, Vertices1W, Parent->bone_instances);
		else if (*Vertices2W) fill_wm_verts(view, wm, normal, size, indices, faces, Vertices2W, Parent->bone_instances);
		else if (*Vertices3W) fill_wm_verts(view, wm, normal, size, indices, faces, Vertices3W, Parent->bone_instances);
		else if (*Vertices4W) fill_wm_verts(view, wm, normal, size, indices, faces, Vertices4W, Parent->bone_instances);
	}

	template <typename T_output>
	ICF void EnumBoneVertices(T_output& m_verts, u16 bone_id)
	{
		VERIFY(Parent && (ChildIDX != u16(-1)));
		CBoneData& BD = Parent->LL_GetData(bone_id);
		CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];

		u16* indices = nullptr;
		if (progressive_mesh)
			indices = *m_Indices + iBase + nSWI.sw[0].offset;
		else
			indices = *m_Indices + iBase;

		if (*Vertices1W) enum_verts(m_verts, indices, faces, Vertices1W, Parent->bone_instances);
		else if (*Vertices2W) enum_verts(m_verts, indices, faces, Vertices2W, Parent->bone_instances);
		else if (*Vertices3W) enum_verts(m_verts, indices, faces, Vertices3W, Parent->bone_instances);
		else if (*Vertices4W) enum_verts(m_verts, indices, faces, Vertices4W, Parent->bone_instances);
	}

	u32 FacesCount(u16 bone_id)
	{
		VERIFY(Parent && (ChildIDX != u16(-1)));
		CBoneData& BD = Parent->LL_GetData(bone_id);
		CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];

		return faces.size();
	}
};
