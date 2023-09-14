#pragma once
#include "../../Include/xrRender/ObjectSpaceRender.h"

namespace Rendering
{
	class ObjectSpaceRender : public IObjectSpaceRender
	{
	public:
		~ObjectSpaceRender();
		void Copy(IObjectSpaceRender& _in) override;
		void dbgRender() override;
		void dbgAddSphere(const Fsphere& sphere, u32 colour) override;
		void SetShader() override;
	};
}