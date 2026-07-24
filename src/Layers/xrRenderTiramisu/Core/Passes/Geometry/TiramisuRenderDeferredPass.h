#pragma once

#include "TiramisuRenderTypes.h"

// Временный geometry pass прототипа; несмотря на имя, полноценным deferred renderer пока не является.
class TiramisuRenderDeferredPass
{
public:
	TiramisuRenderDeferredPass();
	~TiramisuRenderDeferredPass();
	void Render(nri::CommandBuffer& CurrentCommandBuffer);
};
