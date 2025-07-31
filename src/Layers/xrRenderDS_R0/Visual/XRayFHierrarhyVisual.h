#pragma once
#include "XRayRenderVisual.h"

class CDS0_FHierrarhyVisual :
	public CDS0_RenderVisual
{
public:
	xr_vector<CDS0_RenderVisual*> children;

public:
	virtual void Load(const char* N, IReader* data, u32 dwFlags);
	virtual void Copy(CDS0_RenderVisual* from);
	virtual void Release();

	CDS0_FHierrarhyVisual();
	virtual ~CDS0_FHierrarhyVisual();

private:
	bool m_DontDelete = false;
};