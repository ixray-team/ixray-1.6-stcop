#include "stdafx.h"
#include "UI3dStatic.h"
#include "gameobject.h"
#include "HUDManager.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "..\xrEngine\device.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
#define DIST (VIEWPORT_NEAR + 0.1f)

CUI3dStatic::CUI3dStatic()
{
    m_x_angle = m_y_angle = m_z_angle = 0;
    dist = DIST;
    viewport_near = VIEWPORT_NEAR;
}

CUI3dStatic::~CUI3dStatic()
{
}

void CUI3dStatic::FromScreenToItem(int x_screen, int y_screen, float& x_item, float& y_item)
{
    int x = x_screen;
    int y = y_screen;
    //Msg("start FromScreenToItem x_screen-%f y_screen-%f",x,y);

    int halfwidth = 512;
    int halfheight = 768 / 2;

    float size_y = viewport_near * tanf(deg2rad(Device.fFOV) * 0.5f);
    float size_x = size_y / (Device.fASPECT);

    float r_pt = float(x - halfwidth) * size_x / (float)halfwidth;
    float u_pt = float(halfheight - y) * size_y / (float)halfheight;

    x_item = r_pt * dist / viewport_near;
    y_item = u_pt * dist / viewport_near;
}

//прорисовка
void CUI3dStatic::Draw()
{
    if (m_pCurrentItem)
    {
        //Msg("Start draw 3d model");
        Frect rect;
        GetAbsoluteRect(rect);

        Fmatrix translate_matrix;
        translate_matrix.identity();

        Fmatrix scale_matrix;
        scale_matrix.identity();

        Fmatrix rx_m;
        rx_m.identity();

        Fmatrix ry_m;
        ry_m.identity();

        Fmatrix rz_m;
        rx_m.identity();

        Fmatrix matrix;
        matrix.identity();

        //поместить объект в центр сферы

        translate_matrix.translate(-m_pCurrentItem->getVisData().sphere.P.x,
            -m_pCurrentItem->getVisData().sphere.P.y,
            -m_pCurrentItem->getVisData().sphere.P.z);

        matrix.mulA_44(translate_matrix);

        rx_m.rotateX(m_x_angle);
        ry_m.rotateY(m_y_angle);
        rz_m.rotateZ(m_z_angle);
        matrix.mulA_44(rx_m);
        matrix.mulA_44(ry_m);
        matrix.mulA_44(rz_m);

        float x1, y1, x2, y2;

        FromScreenToItem(rect.left, rect.top, x1, y1);
        FromScreenToItem(rect.right, rect.bottom, x2, y2);

        float normal_size;
        normal_size = _abs(x2 - x1) < _abs(y2 - y1) ? _abs(x2 - x1) : _abs(y2 - y1);

        float radius = m_pCurrentItem->getVisData().sphere.R;

        float scale = normal_size / (radius * 2);

        scale_matrix.scale(scale, scale, scale);

        matrix.mulA_44(scale_matrix);

        float right_item_offset, up_item_offset;

        /////////////////////////////// 

        FromScreenToItem(rect.left + iFloor(GetWidth() / 2), rect.top + iFloor(GetHeight() / 2), right_item_offset, up_item_offset);

        translate_matrix.identity();
        translate_matrix.translate(right_item_offset, up_item_offset, dist);

        matrix.mulA_44(translate_matrix);

        Fmatrix camera_matrix;
        camera_matrix.identity();
        camera_matrix = Device.mView;
        camera_matrix.invert();

        matrix.mulA_44(camera_matrix);
        
        ::Render->set_UI(true);

        ::Render->set_Transform(&matrix);
        ::Render->add_Visual(m_pCurrentItem);

        ::Render->RenderUI();
        ::Render->set_UI(false);
    }

    inherited::Draw ();
}

void CUI3dStatic::SetVisual(IRenderVisual* pVisual)
{
    m_pCurrentItem = pVisual;
}

