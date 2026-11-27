#include "stdafx.h"
#include "SkeletonX.h"
#include "../../xrEngine/Render.h"

ICF void BuildMatrix(Fmatrix& mView, float invsz, const Fvector norm, const Fvector& from)
{
	// build projection
	Fmatrix				mScale;
	Fvector				at, up, right, y;
	at.sub(from, norm);
	y.set(0, 1, 0);
	if (std::abs(norm.y) > .99f) y.set(1, 0, 0);
	right.crossproduct(y, norm);
	up.crossproduct(norm, right);
	mView.build_camera(from, at, up);
	mScale.scale(invsz, invsz, invsz);
	mView.mulA_43(mScale);
}

void CKinematics::AddWallmark(const Fmatrix* parent_xform, const Fvector3& start, const Fvector3& dir, ref_shader shader_, float size)
{
	Fvector S,D,normal;
	// transform ray from world to model
	Fmatrix P; P.invert(*parent_xform);
	P.transform_tiny(S,start);
	P.transform_dir(D,dir);
	// find pick point
	float dist = flt_max;
	bool picked = false;
	size_t bones_count = bones->size();
	buffer_vector<Fobb> cache_obb(_alloca(bones_count * sizeof(Fobb)), bones_count);
	cache_obb.resize(bones_count);
	IKinematics::pick_result r;r.normal = normal; r.dist = dist;
	for (u16 k=0; k<bones_count; k++)
	{
		CBoneData& BD = *((*bones)[k]);
		if (visimask.is(k)&&!BD.shape.flags.is(SBoneShape::sfNoPickable))
		{
			Fobb& obb = cache_obb[k];
			obb.transform(BD.obb,LL_GetBoneInstance(k).mTransform);
			if (CDB::TestRayOBB(S,D, obb))
			{
				for (dxRender_Visual* V : children)
				{
					if (static_cast<CSkeletonX*>(V)->PickBone(r, dist, S, D, k))
					{
						picked = true;
						dist = r.dist;
						normal = r.normal;
						//dynamics set wallmarks bug fix
					}
				}
			}
		}
	}
	if (!picked) return;

	// calculate contact point
	Fvector cp;	cp.mad(S,D,dist);

	// collect collide boxes
	Fsphere test_sphere{ cp,size };
	buffer_vector<u16> test_bones(_alloca(bones_count * sizeof(u16)), bones_count);
	for (u16 k=0; k< bones_count; k++)
	{
		CBoneData& BD = *((*bones)[k]);
		if (visimask.is(k)&&!BD.shape.flags.is(SBoneShape::sfNoPickable))
		{
			Fobb& obb = cache_obb[k];
			if (CDB::TestSphereOBB(test_sphere, obb))
				test_bones.push_back(k);
		}
	}

	// find similar wm
	for (u32 wm_idx=0; wm_idx<wallmarks.size(); wm_idx++)
	{
		intrusive_ptr<CSkeletonWallmark>& wm = wallmarks[wm_idx];
		if (!wm)
			continue;
		if (wm->Similar(shader_,cp,0.02f))
		{
			if (wm_idx<wallmarks.size()-1)
				wm = wallmarks.back();
			wallmarks.pop_back();
			break;
		}
	}

	// ok. allocate wallmark
	intrusive_ptr<CSkeletonWallmark> wm = new CSkeletonWallmark(this,parent_xform,shader_,cp,RDEVICE.fTimeGlobal);
	wm->m_LocalBounds.set(cp,size*2.f);
	wm->XFORM()->transform_tiny(wm->m_Bounds.P,cp);
	wm->m_Bounds.R = wm->m_LocalBounds.R;
	wm->m_Faces.reserve(16);

	Fvector tmp; tmp.invert(D);
	normal.add(tmp).normalize();

	// build UV projection matrix
	Fmatrix mView,mRot;
	BuildMatrix(mView,1/(0.9f*size),normal,cp);
	mRot.rotateZ(::Random.randF(deg2rad(-20.f),deg2rad(20.f)));
	mView.mulA_43(mRot);

	// fill vertices
	for (dxRender_Visual* V : children)
	{
		for (u16 bone_id : test_bones)
			static_cast<CSkeletonX*>(V)->FillWMVertices(mView,*wm,normal,size,bone_id);
	}

	wallmarks.push_back(wm);
}

void CKinematics::CalculateWallmarks()
{
	PROF_EVENT("Calculate Wallmarks");
	if (!wallmarks.empty() && (wm_frame != RDEVICE.dwFrame))
	{
		wm_frame = RDEVICE.dwFrame;
		for (SkeletonWMVecIt it = wallmarks.begin(); it != wallmarks.end();)
		{
			intrusive_ptr<CSkeletonWallmark>& wm = *it;
			if (!wm)
			{
				it = wallmarks.erase(it);
				continue;
			}

			float w = (RDEVICE.fTimeGlobal - wm->TimeStart()) / ps_r__WallmarkTTL;
			if (w < 1.f)
			{
				// append wm to WallmarkEngine
				if (::Render != nullptr && ::Render->ViewBase.testSphere_dirty(wm->m_Bounds.P, wm->m_Bounds.R))
				{
					RImplementation.add_SkeletonWallmark(wm);
				}
				++it;
			}
			else
			{
				// remove expired wallmark
				it = wallmarks.erase(it);
			}
		}
	}
}

void CKinematics::RenderWallmark(intrusive_ptr<CSkeletonWallmark> wm, FVF::LIT* &V)
{
	PROF_EVENT("CKinematics::RenderWallmark");
	VERIFY(wm);
	VERIFY(V);
	VERIFY2(bones,"Invalid visual. Bones already released.");
	VERIFY2(bone_instances,"Invalid visual. bone_instances already deleted.");

	if ((wm == nullptr) || (nullptr==bones) || (nullptr==bone_instances))	return;

	// skin vertices
	Fvector P, P0, P1, P2, P3;
	float w, w0, w1, w2, w3;
	int aC;
	for (CSkeletonWallmark::WMFace& F : wm->m_Faces)
	{
		w = (RDEVICE.fTimeGlobal - wm->TimeStart()) / ps_r__WallmarkTTL;
		for (u32 k=0; k<3; k++)
		{
			auto& vert = F.vert[k];
			auto& bone_id = F.bone_id[k];
			auto& weight = F.weight[k];
			if(bone_id[3] != BI_NONE)
			{
				w0 = weight[0]; w1 = weight[1]; w2 = weight[2]; w3 = 1.0f - w0 - w1 - w2;
				bone_instances[bone_id[0]].mRenderTransform.transform_tiny(P0, vert);
				bone_instances[bone_id[1]].mRenderTransform.transform_tiny(P1, vert);
				bone_instances[bone_id[2]].mRenderTransform.transform_tiny(P2, vert);
				bone_instances[bone_id[3]].mRenderTransform.transform_tiny(P3, vert);

				P = (P0 * w0) + (P1 * w1) + (P2 * w2) + (P3 * w3);
			}
			else if (bone_id[2] != BI_NONE)
			{
				w0 = weight[0]; w1 = weight[1]; w2 = 1.0f - w0 - w1;
				bone_instances[bone_id[0]].mRenderTransform.transform_tiny(P0, vert);
				bone_instances[bone_id[1]].mRenderTransform.transform_tiny(P1, vert);
				bone_instances[bone_id[2]].mRenderTransform.transform_tiny(P2, vert);

				P = (P0 * w0) + (P1 * w1) + (P2 * w2);
			}
			else if (bone_id[1] != BI_NONE)
			{
				bone_instances[bone_id[0]].mRenderTransform.transform_tiny(P0, vert);
				bone_instances[bone_id[1]].mRenderTransform.transform_tiny(P1, vert);
				P.lerp(P0, P1, weight[0]);
			}
			else
				bone_instances[bone_id[0]].mRenderTransform.transform_tiny(P, vert);

			wm->XFORM()->transform_tiny(V->p, P);
			V->t.set(F.uv[k]);
			aC = iFloor(w * 255.f);
			clamp(aC, 0, 255);
			V->color = color_rgba(128, 128, 128, aC);
			V++;
		}
	}
	wm->XFORM()->transform_tiny(wm->m_Bounds.P,wm->m_LocalBounds.P);
}

void CKinematics::ClearWallmarks()
{
//	for (SkeletonWMVecIt it=wallmarks.begin(); it!=wallmarks.end(); it++)
//		xr_delete	(*it);
	wallmarks.clear ();
}

