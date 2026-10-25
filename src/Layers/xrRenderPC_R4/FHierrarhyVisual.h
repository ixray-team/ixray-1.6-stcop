// FHierrarhyVisual.h: interface for the FHierrarhyVisual class.
//
//////////////////////////////////////////////////////////////////////

#ifndef FHierrarhyVisualH
#define FHierrarhyVisualH

#pragma once

#include "FBasicVisual.h"

class  FHierrarhyVisual : public dxRender_Visual
{
public:
	xr_vector<dxRender_Visual*>		children;
	bool							bDontDelete;
public:
    				FHierrarhyVisual();
	virtual ~FHierrarhyVisual() override;

	virtual void	Load			(const char* N, IReader *data, u32 dwFlags) override;
	virtual void	Copy			(dxRender_Visual *pFrom) override;
	virtual void	Release			() override;
};

#endif //FHierrarhyVisualH
