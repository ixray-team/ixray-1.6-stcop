#pragma once
#include "../../../Include/xrRender/RenderVisual.h"

class CDS0_RenderVisual :
	public IRenderVisual
{
public:
	virtual ~CDS0_RenderVisual();
	virtual void				Load(const char* N, IReader* data, u32 dwFlags);
	virtual void				Copy(CDS0_RenderVisual* from);
	virtual void				Depart() {};
	virtual void				Spawn() {};
	virtual void				Release() {}
	virtual u32					getType();
	virtual vis_data& 	getVisData();
	virtual shared_str		getDebugName() ;

public:
	vis_data					Vis;
	u32 Type;
	shared_str DebugName;
};