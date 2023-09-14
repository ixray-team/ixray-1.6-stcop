#include "pch.h"

using namespace Rendering;

Fsphere Bounds;

RainRender::~RainRender()
{
}

void
RainRender::Copy(IRainRender& _in)
{
}

void 
RainRender::Render(CEffect_Rain& owner)
{
}

const Fsphere& Rendering::RainRender::GetDropBounds() const
{
	return Bounds;
}
