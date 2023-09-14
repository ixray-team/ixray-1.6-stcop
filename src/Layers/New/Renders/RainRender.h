#pragma once
#include "../../Include/xrRender/RainRender.h"

namespace Rendering
{
	class RainRender : public IRainRender
	{
	public:
		~RainRender();
		void Copy(IRainRender& _in) override;
		void Render(CEffect_Rain& owner) override;
		const Fsphere& GetDropBounds() const override;
	};
}