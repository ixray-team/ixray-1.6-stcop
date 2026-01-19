#pragma once

#include "..\xrUI\Widgets\UIStatic.h"

class CGameObject;
class IRenderVisual;

class CUI3dStatic : public CUIStatic
{
    typedef CUIStatic inherited;

public:

    CUI3dStatic();
    virtual ~CUI3dStatic();

    IC void SetXYZ(Fvector& _xyz) 
    {
        rotate_matrix.setXYZ(_xyz);
    }

    IC void SetXYZ(float x, float y, float z) 
    {
        rotate_matrix.setXYZ(x, y, z);
    }

    Fvector GetXYZ() const;

    void SetVisual(IRenderVisual* pVisual);
    virtual void Draw();

    float ScaleFactor = 1.0f;

protected:

    Fmatrix rotate_matrix = Fidentity;

    void FromScreenToItem(int x_screen, int y_screen, float& x_item, float& y_item);

    IRenderVisual* m_pCurrentItem = NULL;

    float fViewportNear, fViewportDist, fViewportAspect;
    float fViewportFOV, fViewportSize;

    Fmatrix mView, mInvView, mProject;
};