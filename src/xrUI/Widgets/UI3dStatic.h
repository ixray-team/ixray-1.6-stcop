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

	IC void			SetXYZ			(Fvector& _xyz) { mRotate.setXYZ(_xyz); }
	IC void			SetXYZ			(float x, float y, float z) { mRotate.setXYZ(x, y, z); }
	
	IC void			SetHPB			(Fvector& _hpb) { mRotate.setHPB(_hpb.x, _hpb.y, _hpb.z); }
	IC void			SetHPB			(float h, float p, float b) { mRotate.setHPB(h, p, b); }

	Fvector			GetXYZ			() const;
	Fvector			GetHPB			() const;

			void	SetVisual		(IRenderVisual* pVisual);
			void	SetVisual		(const shared_str& cVisualName);

	virtual void	Draw			();

			void	SetScaleFactor	(float fScale) { fScaleFactor = fScale; }
			float	GetScaleFactor	() { return fScaleFactor;  }

	IRenderVisual*	GetVisual		() { return pCurrentVisual; }

protected:


	void FromScreenToItem(int x_screen, int y_screen, float& x_item, float& y_item);

	IRenderVisual* pCurrentVisual = NULL;

	float fViewportNear, fViewportDist, fViewportAspect;
	float fViewportFOV, fViewportSize;

	Fmatrix mView, mInvView, mProject, mRotate;

	float fScaleFactor;
};