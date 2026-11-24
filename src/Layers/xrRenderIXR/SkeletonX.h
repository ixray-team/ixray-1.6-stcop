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
public:
	friend class SkinningPass;
	enum { RM_SINGLE, RM_SKINNING_1B, RM_SKINNING_2B, RM_SKINNING_3B, RM_SKINNING_4B };

	CKinematics* Parent = nullptr; // setted up by parent
	ref_smem<vertBoned1W> Vertices1W; // shared
	ref_smem<vertBoned2W> Vertices2W; // shared
	ref_smem<vertBoned3W> Vertices3W; // shared
	ref_smem<vertBoned4W> Vertices4W; // shared
	ref_smem<u16> BonesUsed; // actual bones which have influence on vertices
	ref_smem<u16> m_Indices; //	Index buffer replica

	u16 RenderMode = RM_SINGLE;
	u16 ChildIDX = u16(-1);
	bool progressive_mesh = false;
	bool visible = true;
	u32 RMS_boneid = 0;
	u32 SkinFrame = 0;
	u32 SkinOutBase = 0;

	bool IsSkinned() const { return RenderMode != RM_SINGLE; }
	void _Render(GfxCmdList& Cmd, ref_geom& hGeom, u32 iOffset, u32 pCount);
	void _Load(const char* N, IReader *data, u32& dwVertCount);
	void _Load_hw();
	void _CollectBoneFaces();
	void _DuplicateIndices(IReader* data);

	ICF u16 get_child_id() { return ChildIDX; };
	ICF void set_simple_visible(bool val) { visible = val; };
	ICF bool get_simple_visible() { return visible; };
	ICF bool uses_bone(u16 bone_id)
	{
		if (RM_SINGLE == RenderMode)
		{
			return (u16)RMS_boneid == bone_id;
		}
		for (u32 i = 0; i < BonesUsed.size(); i++)
		{
			if (BonesUsed[i] == bone_id)
			{
				return true;
			}
		}
		return false;
	}

	bool has_visible_bones();
	CSkeletonX(bool val) : progressive_mesh(val) {}

	virtual void Copy(dxRender_Visual* V) override;
	virtual void Release() override;

	virtual void Render(GfxCmdList& Cmd, float LOD) override;
	virtual void Load(const char* N, IReader* data, u32 dwFlags) override;

	void SetParent(CKinematics* K) { Parent = K; }
	void AfterLoad(CKinematics* parent, u16 child_idx);

	ICF bool PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
	{
		VERIFY(Parent && (ChildIDX != u16(-1)));
		CBoneData& BD = Parent->LL_GetData(bone_id);
		CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];

		u16* indices = *m_Indices + (progressive_mesh ? nSWI.sw[0].offset : 0);

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

	template <typename T_output>
	ICF void EnumBoneVertices(T_output& m_verts, u16 bone_id)
	{
		VERIFY(Parent && (ChildIDX != u16(-1)));
		CBoneData& BD = Parent->LL_GetData(bone_id);
		CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];

		u16* indices = *m_Indices + (progressive_mesh ? nSWI.sw[0].offset : 0);

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
