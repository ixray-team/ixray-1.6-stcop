#pragma once
#include "XRayFVisual.h"

class CDS0_FProgressive:
	public CDS0_FVisual
{
protected:
	FSlideWindowItem* xSWI;
	u32 last_lod;

public:
	CDS0_FProgressive();
	virtual ~CDS0_FProgressive() override;
	virtual void Load(const char* N, IReader* data, u32 dwFlags) override;
	virtual void Copy(CDS0_RenderVisual* from) override;
	virtual void Release() override;
};
