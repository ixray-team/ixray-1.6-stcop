#pragma once
#include "XRayFVisual.h"

class XRayFProgressive:public XRayFVisual
{
protected:
	FSlideWindowItem* xSWI;
	u32					last_lod;
public:
	XRayFProgressive();
	virtual ~XRayFProgressive();
	virtual void				Load(const char* N, IReader* data, u32 dwFlags);
	virtual void				Copy(XRayRenderVisual* from);
	virtual void 		Release();
	//virtual bool Render(float LOD, EShaderElement SEType, XRayObjectRender& Item);
};
