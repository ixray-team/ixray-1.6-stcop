#pragma once
#include "../../../Include/xrRender/RenderVisual.h"

class CDS0_RenderVisual :
	public IRenderVisual
{
public:
	CDS0_RenderVisual();
	virtual ~CDS0_RenderVisual() override;
	virtual void Load(const char* N, IReader* data, u32 dwFlags);
	virtual void Copy(CDS0_RenderVisual* from);
	virtual void Depart() {}
	virtual void Spawn() {}
	virtual void Release() {}
	virtual u32 getType() override;
	virtual vis_data& getVisData() override;
	virtual shared_str getDebugName() override;

public:
	vis_data Vis;
	u32 Type;
	shared_str DebugName;
};