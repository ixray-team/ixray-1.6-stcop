#pragma once

#include "../Include/xrRender/FactoryPtr.h"
#include "../Include/xrRender/UIRender.h"
#include "../Include/xrRender/UIShader.h"

typedef	FactoryPtr<IUIShader>	ui_shader;

#define UI_BASE_WIDTH	1024.0f
#define UI_BASE_HEIGHT	768.0f

enum EUIItemAlign
{
	alNone	= 0x0000,
	alLeft	= 0x0001,
	alRight	= 0x0002,
	alTop	= 0x0004,
	alBottom= 0x0008,
	alCenter= 0x0010
};

struct UI_API S2DVert
{
	Fvector2	pt;
	Fvector2	uv;
				S2DVert		(){}
				S2DVert		(float pX, float pY, float tU, float tV){pt.set(pX,pY);uv.set(tU,tV);}
	void		set			(float pt_x, float pt_y, float uv_x, float uv_y){pt.set(pt_x,pt_y);uv.set(uv_x,uv_y);}
	void		set			(const Fvector2& _pt, const Fvector2& _uv){pt.set(_pt);uv.set(_uv);}
	void		rotate_pt	(const Fvector2& pivot, const float cosA, const float sinA, const float kx);
};

#define UI_FRUSTUM_MAXPLANES	12
#define UI_FRUSTUM_SAFE			(UI_FRUSTUM_MAXPLANES*4)
typedef svector<S2DVert,UI_FRUSTUM_SAFE>		sPoly2D;

class UI_API C2DFrustum
{
	svector<Fplane2, UI_FRUSTUM_MAXPLANES> planes;
	Frect						m_rect;
public:
	void		CreateFromRect	(const Frect& rect);
	sPoly2D*	ClipPoly		(sPoly2D& S, sPoly2D& D) const;
};

enum eUIDirection4
{
	eUIDirection4_None = 0,
	eUIDirection4_Left = 1,
	eUIDirection4_Right,
	eUIDirection4_Up,
	eUIDirection4_Down
};

enum eUIDirection8
{
	eUIDirection8_None = 0,
	eUIDirection8_Left = eUIDirection4_Left,
	eUIDirection8_Right = eUIDirection4_Right,
	eUIDirection8_Top = eUIDirection4_Up,
	eUIDirection8_Bottom = eUIDirection4_Down,
	eUIDirection8_TopLeft,
	eUIDirection8_TopRight,
	eUIDirection8_BottomLeft,
	eUIDirection8_BottomRight
};


template<class T>
bool MoveSelectionUp(xr_vector<T*>& wlist, T* pOldSelected, T*& pNewSelection, bool bAllowLoop)
{
	if (wlist.size() < 2)
		return false;
	if (!pOldSelected)
		return false;

	typename xr_vector<T*>::iterator fIt = std::find(wlist.begin(), wlist.end(), pOldSelected);
	R_ASSERT(fIt != wlist.end());
	if (fIt == wlist.begin())
	{
		if (bAllowLoop)
		{
			fIt = --wlist.end();
		}
		else
			return false;
	}
	else
		--fIt;

	pNewSelection = *fIt;
	return true;
}

template<class T>
bool MoveSelectionDown(xr_vector<T*>& wlist, T* pOldSelected, T*& pNewSelection, bool bAllowLoop)
{
	if (wlist.size() < 2)
		return false;
	if (!pOldSelected)
		return false;

	typename xr_vector<T*>::iterator fIt = std::find(wlist.begin(), wlist.end(), pOldSelected);
	R_ASSERT(fIt != wlist.end());
	++fIt;
	if (fIt == wlist.end())
	{
		if (bAllowLoop)
		{
			fIt = wlist.begin();
		}
		else
			return false;
	}

	pNewSelection = *fIt;
	return true;
}
extern ENGINE_API xr_atomic_bool g_bRendering; 
