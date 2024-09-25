#pragma once
#include "UIWindow.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../Include/xrRender/Kinematics.h"

class CGameObject;

class CUI3dStatic : public CUIStatic
{
    typedef CUIWindow inherited;
private:
    //    Fmatrix matrix;

public:
    CUI3dStatic();
    virtual ~CUI3dStatic();

    void SetRotate(float x, float y, float z) {
        m_x_angle = x;
        m_y_angle = y;
        m_z_angle = z;
    }

    void SetVisual(IRenderVisual* pVisual);

    //прорисовка окна
    virtual void Draw();

protected:
    float m_x_angle, m_y_angle, m_z_angle;
    float dist, viewport_near;
    //перевод из координат экрана в координаты той плоскости
    //где находиться объект
    void FromScreenToItem(int x_screen, int y_screen, float& x_item, float& y_item);

    IRenderVisual* m_pCurrentItem;
};