#include "XRaySkeletonX.h"

#include "stdafx.h"
#include "../../xrCDB/xrCDB.h"
#include "XRayKinematics.h"
#include "../xrRender/cl_intersect.h"

#pragma pack(push,2)
struct vertRender// T&B are not skinned, because in R2 skinning occurs always in hardware
{
	Fvector	P;
	Fvector	N;
	float	u, v;
};
#pragma pack(pop)

void XRaySkeletonX::_Copy(XRaySkeletonX* B)
{
	Parent = NULL;
	ChildIDX = B->ChildIDX;
	Vertices1W = B->Vertices1W;
	Vertices2W = B->Vertices2W;
	Vertices3W = B->Vertices3W;
	Vertices4W = B->Vertices4W;
	BonesUsed = B->BonesUsed;

	cache_DiscardID = B->cache_DiscardID;
	cache_vCount = B->cache_vCount;
	cache_vOffset = B->cache_vOffset;
	RenderMode = B->RenderMode;
	RMS_boneid = B->RMS_boneid;
	RMS_bonecount = B->RMS_bonecount;

	m_Indices = B->m_Indices;
}
void XRaySkeletonX::AfterLoad(XRayKinematics* parent, u16 child_idx)
{
	SetParent(parent);
	ChildIDX = child_idx;
}
void XRaySkeletonX::_Load(const char* N, IReader* data, size_t& dwVertCount)
{
	xr_vector<u16>			bids;

	R_ASSERT(data->find_chunk(OGF_VERTICES));

	u16			hw_bones_cnt = u16((256- 22 - 3) / 3);


	u16			sw_bones_cnt = 0;

	size_t								dwVertType, size, it;
	u32 crc;
	dwVertType = data->r_u32();
	dwVertCount = data->r_u32();

	RenderMode = RM_SKINNING_SOFT;
	//Render->shader_option_skinning(-1);

	switch (dwVertType)
	{
	case OGF_VERTEXFORMAT_FVF_1L: // 1-Link
	case 1:
	{
		size = dwVertCount * sizeof(vertBoned1W);
		vertBoned1W* pVO = (vertBoned1W*)data->pointer();

		for (it = 0; it < dwVertCount; ++it)
		{
			const vertBoned1W& VB = pVO[it];
			u16 mid = (u16)VB.matrix;

			if (bids.end() == std::find(bids.begin(), bids.end(), mid))
				bids.push_back(mid);

			sw_bones_cnt = std::max(sw_bones_cnt, mid);
		}
#if 0
		// software
		crc = crc32(data->pointer(), size);
		Vertices1W.create(crc, dwVertCount, (vertBoned1W*)data->pointer());
#else
		if (1 == bids.size())
		{
			// HW- single bone
			RenderMode = RM_SINGLE;
			RMS_boneid = *bids.begin();
		//	Render->shader_option_skinning(0);
		}
		else
			if (sw_bones_cnt <= hw_bones_cnt)
			{
				// HW- one weight
				RenderMode = RM_SKINNING_1B;
				RMS_bonecount = sw_bones_cnt + 1;
			//	Render->shader_option_skinning(1);
			}
			else
			{
				R_ASSERT(sw_bones_cnt <= hw_bones_cnt);
				// software
				crc = crc32(data->pointer(), size);
				Vertices1W.create(crc, dwVertCount, (vertBoned1W*)data->pointer());
			//	Render->shader_option_skinning(-1);
			}
#endif        
	}
	break;
	case OGF_VERTEXFORMAT_FVF_2L: // 2-Link
	case 2:
	{
		size = dwVertCount * sizeof(vertBoned2W);
		vertBoned2W* pVO = (vertBoned2W*)data->pointer();

		for (it = 0; it < dwVertCount; ++it)
		{
			const vertBoned2W& VB = pVO[it];
			sw_bones_cnt = std::max(sw_bones_cnt, VB.matrix0);
			sw_bones_cnt = std::max(sw_bones_cnt, VB.matrix1);

			if (bids.end() == std::find(bids.begin(), bids.end(), VB.matrix0))
				bids.push_back(VB.matrix0);

			if (bids.end() == std::find(bids.begin(), bids.end(), VB.matrix1))
				bids.push_back(VB.matrix1);
		}
		R_ASSERT(sw_bones_cnt<=hw_bones_cnt);
		if (sw_bones_cnt <= hw_bones_cnt)
		{
			// HW- two weights
			RenderMode = RM_SKINNING_2B;
			RMS_bonecount = sw_bones_cnt + 1;
			//Render->shader_option_skinning(2);
		}
		else
		{
			// software
			crc = crc32(data->pointer(), size);
			Vertices2W.create(crc, dwVertCount, (vertBoned2W*)data->pointer());
			//Render->shader_option_skinning(-1);
		}
	}break;
	case OGF_VERTEXFORMAT_FVF_3L: // 3-Link
	case 3:
	{
		size = dwVertCount * sizeof(vertBoned3W);
		vertBoned3W* pVO = (vertBoned3W*)data->pointer();

		for (it = 0; it < dwVertCount; ++it)
		{
			const vertBoned3W& VB = pVO[it];
			for (int i = 0; i < 3; ++i)
			{
				sw_bones_cnt = std::max(sw_bones_cnt, VB.m[i]);

				if (bids.end() == std::find(bids.begin(), bids.end(), VB.m[i]))
					bids.push_back(VB.m[i]);
			}
		}
			R_ASSERT(sw_bones_cnt<=hw_bones_cnt);
		if ((sw_bones_cnt <= hw_bones_cnt))
		{
			RenderMode = RM_SKINNING_3B;
			RMS_bonecount = sw_bones_cnt + 1;
			//Render->shader_option_skinning(3);
		}
		else
		{
			crc = crc32(data->pointer(), size);
			Vertices3W.create(crc, dwVertCount, (vertBoned3W*)data->pointer());
		//	Render->shader_option_skinning(-1);
		}
	}break;
	case OGF_VERTEXFORMAT_FVF_4L: // 4-Link
	case 4:
	{
		size = dwVertCount * sizeof(vertBoned4W);
		vertBoned4W* pVO = (vertBoned4W*)data->pointer();

		for (it = 0; it < dwVertCount; ++it)
		{
			const vertBoned4W& VB = pVO[it];

			for (int i = 0; i < 4; ++i)
			{
				sw_bones_cnt = std::max(sw_bones_cnt, VB.m[i]);

				if (bids.end() == std::find(bids.begin(), bids.end(), VB.m[i]))
					bids.push_back(VB.m[i]);
			}
		}
			R_ASSERT(sw_bones_cnt<=hw_bones_cnt);
		if (sw_bones_cnt <= hw_bones_cnt)
		{
			RenderMode = RM_SKINNING_4B;
			RMS_bonecount = sw_bones_cnt + 1;
		//	Render->shader_option_skinning(4);
		}
		else
		{
			crc = crc32(data->pointer(), size);
			Vertices4W.create(crc, dwVertCount, (vertBoned4W*)data->pointer());
		//	Render->shader_option_skinning(-1);
		}
	}break;
	default:
		Debug.fatal(DEBUG_INFO, "Invalid vertex type in skinned model '%s'", N);
		break;
	}
#ifdef _EDITOR
	if (bids.size() > 0)
#else
	if (bids.size() > 1)
#endif
	{
		crc = crc32(&*bids.begin(), bids.size() * sizeof(u16));
		BonesUsed.create(crc, bids.size(), &*bids.begin());
	}
}



inline void 	get_pos_bones(const vertBoned1W& v, Fvector& p, XRayKinematics* Parent)
{
	const Fmatrix& xform = Parent->LL_GetBoneInstance((u16)v.matrix).mRenderTransform;
	xform.transform_tiny(p, v.P);
}

inline void 	get_pos_bones(const vertBoned2W& vert, Fvector& p, XRayKinematics* Parent)
{
	Fvector		P0, P1;

	Fmatrix& xform0 = Parent->LL_GetBoneInstance(vert.matrix0).mRenderTransform;
	Fmatrix& xform1 = Parent->LL_GetBoneInstance(vert.matrix1).mRenderTransform;
	xform0.transform_tiny(P0, vert.P);
	xform1.transform_tiny(P1, vert.P);
	p.lerp(P0, P1, vert.w);
}

inline void 	get_pos_bones(const vertBoned3W& vert, Fvector& p, XRayKinematics* Parent)
{
	Fmatrix& M0 = Parent->LL_GetBoneInstance(vert.m[0]).mRenderTransform;
	Fmatrix& M1 = Parent->LL_GetBoneInstance(vert.m[1]).mRenderTransform;
	Fmatrix& M2 = Parent->LL_GetBoneInstance(vert.m[2]).mRenderTransform;

	Fvector	P0, P1, P2;
	M0.transform_tiny(P0, vert.P); P0.mul(vert.w[0]);
	M1.transform_tiny(P1, vert.P); P1.mul(vert.w[1]);
	M2.transform_tiny(P2, vert.P); P2.mul(1.0f - vert.w[0] - vert.w[1]);

	p = P0;
	p.add(P1);
	p.add(P2);
}
inline void 	get_pos_bones(const vertBoned4W& vert, Fvector& p, XRayKinematics* Parent)
{
	Fmatrix& M0 = Parent->LL_GetBoneInstance(vert.m[0]).mRenderTransform;
	Fmatrix& M1 = Parent->LL_GetBoneInstance(vert.m[1]).mRenderTransform;
	Fmatrix& M2 = Parent->LL_GetBoneInstance(vert.m[2]).mRenderTransform;
	Fmatrix& M3 = Parent->LL_GetBoneInstance(vert.m[3]).mRenderTransform;

	Fvector	P0, P1, P2, P3;
	M0.transform_tiny(P0, vert.P); P0.mul(vert.w[0]);
	M1.transform_tiny(P1, vert.P); P1.mul(vert.w[1]);
	M2.transform_tiny(P2, vert.P); P2.mul(vert.w[2]);
	M3.transform_tiny(P3, vert.P); P3.mul(1.0f - vert.w[0] - vert.w[1] - vert.w[2]);

	p = P0;
	p.add(P1);
	p.add(P2);
	p.add(P3);
}
template<typename T_vertex, typename T_buffer >
inline BOOL pick_bone(T_buffer vertices, XRayKinematics* Parent, IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, u16* indices, CBoneData::FacesVec& faces)
{


	for (auto it = faces.begin(); it != faces.end(); it++) {

		u32 idx = (*it) * 3;
		for (u32 k = 0; k < 3; k++) {
			T_vertex& vert = vertices[indices[idx + k]];
			get_pos_bones(vert, r.tri[k], Parent);
		}
		float u, v;
		r.dist = flt_max;
		if (CDB::TestRayTri(S, D, r.tri, u, v, r.dist, true) && (r.dist < dist)) {
			r.normal.mknormal(r.tri[0], r.tri[1], r.tri[2]);
			return TRUE;
		};
	}
	return FALSE;
}
BOOL XRaySkeletonX::_PickBoneSoft1W(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertBoned1W>(Vertices1W, Parent, r, dist, S, D, indices, faces);
}

BOOL XRaySkeletonX::_PickBoneSoft2W(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertBoned2W>(Vertices2W, Parent, r, dist, S, D, indices, faces);
}

BOOL XRaySkeletonX::_PickBoneSoft3W(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertBoned3W>(Vertices3W, Parent, r, dist, S, D, indices, faces);
}

BOOL XRaySkeletonX::_PickBoneSoft4W(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertBoned4W>(Vertices4W, Parent, r, dist, S, D, indices, faces);
}


BOOL XRaySkeletonX::has_visible_bones()
{
	if (RM_SINGLE == RenderMode)
	{
		return Parent->LL_GetBoneVisible((u16)RMS_boneid);
	}

	for (u32 it = 0; it < BonesUsed.size(); it++)
		if (Parent->LL_GetBoneVisible(BonesUsed[it]))
		{
			return	TRUE;
		}
	return	FALSE;
}



void XRaySkeletonX::_DuplicateIndices(const char* N, IReader* data)
{
	VERIFY(!data->find_chunk(OGF_ICONTAINER));
	//	Index buffer replica since we can't read from index buffer in DX10
	//ref_smem<u16>			Indices;
	R_ASSERT(data->find_chunk(OGF_INDICES));
	u32 iCount = data->r_u32();

	u32 size = iCount * 2;
	u32 crc = crc32(data->pointer(), size);
	m_Indices.create(crc, iCount, (u16*)data->pointer());
}
