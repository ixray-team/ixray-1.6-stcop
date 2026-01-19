#include "stdafx.h"
#include "UI3dStatic.h"
#include "gameobject.h"
#include "HUDManager.h"

#include "../xrEngine/device.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrUI/UICursor.h"
#include "Level.h"

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

    rotate_matrix.set(Fidentity);
    ScaleFactor = 1.0f;

    SetVisual(NULL);
}

CUI3dStatic::~CUI3dStatic()
{
    SetVisual(NULL);
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

void CUI3dStatic::Draw()
{
    if (m_pCurrentItem)
    {
        Frect rect{};
        GetAbsoluteRect(rect);

        if (rect.x1 > UI_BASE_WIDTH || rect.x2 < 0 || rect.y1 > UI_BASE_HEIGHT || rect.y2 < 0)
        {
            return;
        }

        Fmatrix matrix = Fidentity;
        Fmatrix translate_matrix = Fidentity;

        translate_matrix.c.sub(m_pCurrentItem->getVisData().sphere.P);

        matrix.mulA_44(translate_matrix);
        matrix.mulA_44(rotate_matrix);

        float x1, y1, x2, y2;

        m_pCurrentItem->dcast_PKinematics()->CalculateBones_Invalidate();
        m_pCurrentItem->dcast_PKinematics()->CalculateBones(TRUE);

        FromScreenToItem(rect.left, rect.top, x1, y1);
        FromScreenToItem(rect.right, rect.bottom, x2, y2);

        Fvector2 normal_size; normal_size.set(x2 - x1, y1 - y2);

        Fbox mBox = m_pCurrentItem->getVisData().box;
        mBox.xform(matrix);

        Fvector2 item_size; item_size.set(mBox.max.x - mBox.min.x, mBox.max.y - mBox.min.y);
        normal_size.div(item_size);

        float scale = 0.95f * std::min(std::abs(normal_size.x), std::abs(normal_size.y)) * ScaleFactor;

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

        m_pCurrentItem->getVisData().marker = 0;

        ::Render->set_Transform(&matrix);
        ::Render->add_Visual(m_pCurrentItem, true);

        ::Render->set_UI(false);
        ::Render->RenderUI();

        UI().PopScissor();

        Device.vCameraPosition.set(Device.vCameraPosition_saved);

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
    rotate_matrix.getXYZ(rotate_vector);

    return rotate_vector;
}

void CUI3dStatic::SetVisual(IRenderVisual* pVisual)
{
    if (m_pCurrentItem) 
    {
        ::Render->model_Delete(m_pCurrentItem);
        m_pCurrentItem = NULL;
    }

    if (!pVisual) 
    {
        return;
    }

    m_pCurrentItem = ::Render->model_Duplicate(pVisual);
    
    if (auto pK = m_pCurrentItem->dcast_PKinematics()) 
    {
        pK->LL_SetBonesVisible(pVisual->dcast_PKinematics()->LL_GetBonesVisible());
    }

    if (auto pKa = m_pCurrentItem->dcast_PKinematicsAnimated()) 
    {
        auto MotionID = pKa->ID_Cycle_Safe("idle");

        if (!MotionID)
        {
            MotionID.set(0, 0);
        }

        pKa->PlayCycle(MotionID, false);
    }

    m_pCurrentItem->dcast_PKinematics()->CalculateBones_Invalidate();
    m_pCurrentItem->dcast_PKinematics()->CalculateBones(TRUE);
}

