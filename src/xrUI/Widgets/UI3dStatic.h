#pragma once
#include "UIStatic.h"

class IRenderVisual;

class UI_API CUI3dStatic : 
    public CUIStatic
{
	typedef CUIStatic inherited;

public:

					CUI3dStatic		();
	virtual			~CUI3dStatic	();

	IC void			SetXYZ			(Fvector& _xyz) { m_rot_matrix.setXYZ(_xyz); }

	IC void			SetXYZ			(float x, float y, float z) { m_rot_matrix.setXYZ(x, y, z); }
	IC void			SetHPB			(float h, float p, float b) { Fvector pos = m_rot_matrix.c; m_rot_matrix.setHPB(h, p, b); m_rot_matrix.c = pos; }
	IC void			SetHPB			(Fvector& _hpb) { Fvector pos = m_rot_matrix.c; m_rot_matrix.setHPB(_hpb.x,_hpb.y, _hpb.z ); m_rot_matrix.c = pos; }

	Fvector			GetXYZ			() const;
	Fvector			GetHPB			() const;

			void	SetVisual		(IRenderVisual* pVisual);
	virtual void	Draw			();
			void	SetScaleFactor	(float fScale) { m_fScaleFactor = fScale; }
			float	GetScaleFactor	() { return m_fScaleFactor;  }

	IRenderVisual*	GetVisual		() { return m_pCurrentItem; }
protected:

	Fmatrix m_rot_matrix = Fidentity;

	void FromScreenToItem				(int x_screen, int y_screen, float& x_item, float& y_item);

	IRenderVisual* m_pCurrentItem = NULL;

	float m_fViewportNear, m_fViewportDist, m_fViewportAspect;
	float m_fViewportFOV, m_fViewportSize;

	Fmatrix m_mView, m_mInvView, m_mProject;

	float m_fScaleFactor;
};