#pragma once
#include "XRayRenderVisual.h"

class CDS0_FHierrarhyVisual :
	public CDS0_RenderVisual
{
public:
	xr_vector<CDS0_RenderVisual*> children;

public:
	virtual void Load(const char* N, IReader* data, u32 dwFlags) override;
	virtual void Copy(CDS0_RenderVisual* from) override;
	virtual void Release() override;

	CDS0_FHierrarhyVisual();
	virtual ~CDS0_FHierrarhyVisual() override;

private:
	bool m_DontDelete = false;
};