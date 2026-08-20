#pragma once

#include "TiramisuRenderTypes.h"
class CDS0_RainRender :
	public IRainRender
{
public:
	CDS0_RainRender();
	virtual void Copy(IRainRender& _in);

	virtual void Render(CEffect_Rain& owner);

	virtual const Fsphere& GetDropBounds() const;

private:
	// Пока rain draw pass не реализован, simulation использует безопасные
	// bounds исходной drop mesh вместо разыменования заглушки nullptr.
	Fsphere DropBounds = {};
};
