#include "stdafx.h"
#include "SkeletonX.h"

void CKinematics::EnumBoneVertices(SEnumVerticesCallback &C, u16 bone_id)
{
	if (bone_id == BI_NONE)
	{
		for (u16 i = 0; i < bones->size(); ++i)
		{
			for (dxRender_Visual* V : children)
				static_cast<CSkeletonX*>(V)->EnumBoneVertices(C, i);
		}
	}
	else
	{
		for (dxRender_Visual* V : children)
			static_cast<CSkeletonX*>(V)->EnumBoneVertices(C, bone_id);
	}
}
void CKinematics::EnumBoneVertices(xr_vector<Fvector>& m_vec, u16 bone_id)
{
	m_vec.clear();
	if (bone_id == BI_NONE)
	{
		for (u16 i = 0; i < bones->size(); ++i)
		{
			for (dxRender_Visual* V : children)
				static_cast<CSkeletonX*>(V)->EnumBoneVertices(m_vec, i);
		}
	}
	else
	{
		for (dxRender_Visual* V : children)
			static_cast<CSkeletonX*>(V)->EnumBoneVertices(m_vec, bone_id);
	}
}
void CKinematics::EnumBoneVertices(buffer_vector<Fvector>& m_vec, u16 bone_id)
{
	m_vec.clear();
	if (bone_id == BI_NONE)
	{
		for (u16 i = 0; i < bones->size(); ++i)
		{
			for (dxRender_Visual* V : children)
				static_cast<CSkeletonX*>(V)->EnumBoneVertices(m_vec, i);
		}
	}
	else
	{
		for (dxRender_Visual* V : children)
			static_cast<CSkeletonX*>(V)->EnumBoneVertices(m_vec, bone_id);
	}
}

u32 CKinematics::GetFacesCount(u16 bone_id)
{
	u32 faces_cnt = 0;
	if (bone_id == BI_NONE)
	{
		for (u16 i = 0; i < bones->size(); ++i)
		{
			for (dxRender_Visual* V : children)
				faces_cnt += static_cast<CSkeletonX*>(V)->FacesCount(i);
		}
	}
	else
	{
		for (dxRender_Visual* V : children)
			faces_cnt += static_cast<CSkeletonX*>(V)->FacesCount(bone_id);
	}

	return faces_cnt;
}

bool CKinematics::PickBone(const Fmatrix& parent_xform, IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
{
	Fvector S, D;
	// transform ray from world to model
	Fmatrix P;	P.invert(parent_xform);
	P.transform_tiny(S, start);
	P.transform_dir(D, dir);

	for (dxRender_Visual* V : children)
	{
		if (static_cast<CSkeletonX*>(V)->PickBone(r, dist, S, D, bone_id))
		{
			parent_xform.transform_dir(r.normal);
			parent_xform.transform_tiny(r.tri[0]);
			parent_xform.transform_tiny(r.tri[1]);
			parent_xform.transform_tiny(r.tri[2]);
			return true;
		}
	}

	return false;
}

