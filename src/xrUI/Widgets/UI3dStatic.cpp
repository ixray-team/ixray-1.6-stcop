#include "stdafx.h"
#include "UI3dStatic.h"
#include "../Include/xrRender/RenderVisual.h"

#include "../xrEngine/device.h"
#include "../xrEngine/vis_common.h"
#include "../xrEngine/Render.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrUI/UICursor.h"

CUI3dStatic::CUI3dStatic()
{
	fViewportNear = 0.2f;
	fViewportDist = 0.3f;

	fViewportFOV = deg2rad(2.0f);
	fViewportSize = fViewportNear * tanf(fViewportFOV * 0.5f);

	fViewportAspect = (float)Device.TargetHeight / (float)Device.TargetWidth;

	mProject.build_projection(fViewportFOV, fViewportAspect, fViewportNear, 20.0f);

	mView.build_camera_dir(Fidentity.c, Fidentity.k, Fidentity.j);
	mInvView.invert(mView);

	mRotate.set(Fidentity);
	fScaleFactor = 1.0f;

	pCurrentVisual = nullptr;
    m_bCaptMoving = false;
}

CUI3dStatic::~CUI3dStatic()
{
	SetVisual(nullptr);
}

void CUI3dStatic::SetBonesVisible(IKinematics* pVisual)
{
	if (!pCurrentVisual)
	{
		return;
	}

	if (auto pK = pCurrentVisual->dcast_PKinematics())
	{
		pK->LL_SetBonesVisibleAll();

		for (auto& [bonename, bone_id] : *pVisual->LL_Bones())
		{
			if (auto BoneID = pK->LL_BoneID(bonename); BoneID != BI_NONE)
			{
				pK->LL_SetBoneVisible(BoneID, pVisual->LL_GetBoneVisible(bone_id), false);
			}
		}

		pK->CalculateBones_Invalidate();
		pK->CalculateBones(true);
	}
}

void CUI3dStatic::FromScreenToItem(int x, int y, float& x_item, float& y_item)
{
	float halfwidth = UI_BASE_WIDTH * 0.5f;
	float halfheight = UI_BASE_HEIGHT * 0.5f;

	float size_y = fViewportSize;
	float size_x = size_y / fViewportAspect;

	float r_pt = float(x - halfwidth) * size_x / (float)halfwidth;
	float u_pt = float(halfheight - y) * size_y / (float)halfheight;

	x_item = r_pt * fViewportDist / fViewportNear;
	y_item = u_pt * fViewportDist / fViewportNear;
}

void CUI3dStatic::GetRealBoundBox(IKinematics* m_model, Fmatrix& xform, Fbox& bbox)
{
	Fmatrix Mbox { }, X { };
	Fvector P { }, A { }, S { };

	for (u16 BoneID = 0, BoneCount = m_model->LL_BoneCount(); BoneID < BoneCount; ++BoneID)
	{
		if (!m_model->LL_GetBoneVisible(BoneID))
		{
			continue;
		}

		auto& BiData = m_model->LL_GetData(BoneID);
		auto& Shape = BiData.shape;

		if (SBoneShape::stNone == Shape.type)
		{
			continue;
		}

		if (!Shape.Valid())
		{
			continue;
		}

		if (Shape.type == Shape.stBox)
		{
			Shape.box.xform_get(Mbox);
			S = Shape.box.m_halfsize;
		}
		else if (Shape.type == Shape.stCylinder)
		{
			auto& Cylinder = Shape.cylinder;

			Mbox.c = Cylinder.m_center;
			Mbox.k = Cylinder.m_direction;

			Fvector::generate_orthonormal_basis(Mbox.k, Mbox.j, Mbox.i);

			S.set(Cylinder.m_radius, Cylinder.m_radius, Cylinder.m_height * 0.5f);
		}
		else if (Shape.type == Shape.stSphere)
		{
			auto& Sphere = Shape.sphere;

			Mbox.translate(Sphere.P);
			S.set(Sphere.R, Sphere.R, Sphere.R);
		}

		auto& BiInstance = m_model->LL_GetBoneInstance(BoneID);

		X.mul_43(xform, BiInstance.mTransform);
		X.mulB_43(Mbox);

		A.set(-S.x, -S.y, -S.z); X.transform_tiny(P, A); bbox.modify(P);
		A.set(-S.x, -S.y, +S.z); X.transform_tiny(P, A); bbox.modify(P);
		A.set(+S.x, -S.y, +S.z); X.transform_tiny(P, A); bbox.modify(P);
		A.set(+S.x, -S.y, -S.z); X.transform_tiny(P, A); bbox.modify(P);
		A.set(-S.x, +S.y, -S.z); X.transform_tiny(P, A); bbox.modify(P);
		A.set(-S.x, +S.y, +S.z); X.transform_tiny(P, A); bbox.modify(P);
		A.set(+S.x, +S.y, +S.z); X.transform_tiny(P, A); bbox.modify(P);
		A.set(+S.x, +S.y, -S.z); X.transform_tiny(P, A); bbox.modify(P);
	}
}

void CUI3dStatic::Draw()
{
	if (pCurrentVisual)
	{
		Frect rect{};
		GetAbsoluteRect(rect);

		if (rect.x1 > UI_BASE_WIDTH || rect.x2 < 0 || rect.y1 > UI_BASE_HEIGHT || rect.y2 < 0)
		{
			return;
		}

		pCurrentVisual->dcast_PKinematics()->CalculateBones_Invalidate();
		pCurrentVisual->dcast_PKinematics()->CalculateBones(true);

		float x1, y1, x2, y2;

		FromScreenToItem(rect.left, rect.top, x1, y1);
		FromScreenToItem(rect.right, rect.bottom, x2, y2);

		Fvector2 normal_size; normal_size.set(x2 - x1, y1 - y2);

		Fbox mBox; mBox.invalidate();

		GetRealBoundBox(pCurrentVisual->dcast_PKinematics(), mRotate, mBox);

		if (!mBox.is_valid())
		{
			mBox = pCurrentVisual->getVisData().box;
			mBox.xform(mRotate);
		}

		Fmatrix translate_matrix = Fidentity;

		mBox.getcenter(translate_matrix.c);
		translate_matrix.c.mul(-1.0f);

		Fmatrix matrix = Fidentity;

		matrix.mulA_44(mRotate);
		matrix.mulA_44(translate_matrix);

		Fvector2 item_size; item_size.set(mBox.max.x - mBox.min.x, mBox.max.y - mBox.min.y);
		normal_size.div(item_size);

		float scale = 0.95f * std::min(std::abs(normal_size.x), std::abs(normal_size.y)) * fScaleFactor;

		static Fmatrix scale_matrix;

		scale_matrix.scale(scale, scale, scale);
		matrix.mulA_44(scale_matrix);

		float right_item_offset, up_item_offset;

		FromScreenToItem(rect.left + GetWidth() * 0.5f, rect.top + GetHeight() * 0.5f, right_item_offset, up_item_offset);

		translate_matrix.identity();
		translate_matrix.translate(right_item_offset, up_item_offset, fViewportDist);

		matrix.mulA_44(translate_matrix);
		matrix.mulA_44(mInvView);

		Device.vCameraTop.set(Fidentity.j);
		Device.vCameraRight.set(Fidentity.i);
		Device.vCameraDirection.set(Fidentity.k);

		Device.vCameraPosition.set(Fidentity.c);

		Device.mView = mView;
		Device.mProject = mProject;

		Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);

		UI().PushScissor(rect);
		
		::Render->set_UI(true);

		pCurrentVisual->getVisData().marker = u32(-1);

		::Render->set_Transform(matrix);
		::Render->add_Visual(pCurrentVisual, true);

		::Render->set_UI(false);
		::Render->RenderUI();

		UI().PopScissor();

		Device.vCameraPosition.set(Device.vCameraPosition_saved);

		Device.vCameraDirection.set(Device.vCameraDirection_saved);
		Device.vCameraRight.set(Device.vCameraRight_saved);
		Device.vCameraTop.set(Device.vCameraTop_saved);

		Device.mView = Device.mView_saved;
		Device.mProject = Device.mProject_saved;

		Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);

		CUIWindow::Draw();
		return;
	}
	
	inherited::Draw ();
}

Fvector CUI3dStatic::GetXYZ() const
{
	static Fvector rotate_vector{};
	mRotate.getXYZ(rotate_vector);

	return rotate_vector;
}

Fvector CUI3dStatic::GetHPB() const
{
	static Fvector rotate_vector{};
	mRotate.getHPB(rotate_vector);

	return rotate_vector;
}

void CUI3dStatic::SetVisual(const shared_str& cVisualName)
{
	if (pCurrentVisual)
	{
		::Render->model_Delete(pCurrentVisual);
		pCurrentVisual = nullptr;
	}

	if (!cVisualName)
	{
		return;
	}

    pCurrentVisual = ::Render->model_Create(*cVisualName);

	if (pCurrentVisual == nullptr)
	{
		return;
	}

	if (auto pKa = pCurrentVisual->dcast_PKinematicsAnimated())
	{
		auto MotionID = pKa->ID_Cycle_Safe("idle");

		if (!MotionID)
		{
			MotionID.set(0, 0);
		}

		pKa->PlayCycle(MotionID, false);
	}

	pCurrentVisual->dcast_PKinematics()->CalculateBones_Invalidate();
	pCurrentVisual->dcast_PKinematics()->CalculateBones(true);
}

void CUI3dStatic::SetVisual(IRenderVisual* pVisual)
{
	if (pCurrentVisual) 
	{
		::Render->model_Delete(pCurrentVisual);
		pCurrentVisual = nullptr;
	}

	if (!pVisual) 
	{
		return;
	}

	pCurrentVisual = ::Render->model_Duplicate(pVisual);
	
	if (auto pK = pCurrentVisual->dcast_PKinematics())
	{
		pK->LL_SetBonesVisible(pVisual->dcast_PKinematics()->LL_GetBonesVisible());
	}

	if (auto pKa = pCurrentVisual->dcast_PKinematicsAnimated())
	{
		auto MotionID = pKa->ID_Cycle_Safe("idle");

		if (!MotionID)
		{
			MotionID.set(0, 0);
		}

		pKa->PlayCycle(MotionID, false);
	}
}

