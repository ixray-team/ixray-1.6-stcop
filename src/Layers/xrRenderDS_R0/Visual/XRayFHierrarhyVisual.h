#pragma once
#include "XRayRenderVisual.h"
class XRayFHierrarhyVisual:public XRayRenderVisual
{
public:
	xr_vector<XRayRenderVisual*>		children;
public:
	virtual void				Load(const char* N, IReader* data, u32 dwFlags);
	virtual void				Copy(XRayRenderVisual* from);
	virtual void	Release();
public:
	XRayFHierrarhyVisual();
	virtual ~XRayFHierrarhyVisual();

private:
	BOOL							m_DontDelete;

};