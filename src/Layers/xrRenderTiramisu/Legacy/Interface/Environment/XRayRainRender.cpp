#include "stdafx.h"

CDS0_RainRender::CDS0_RainRender()
{
	DropBounds.P.set(0.0f, 0.0f, 0.0f);
	DropBounds.R = 1.0f;
}

void CDS0_RainRender::Copy(IRainRender & _in)
{
}

void CDS0_RainRender::Render(CEffect_Rain& owner)
{
}
const Fsphere & CDS0_RainRender::GetDropBounds() const
{
	return DropBounds;
}
