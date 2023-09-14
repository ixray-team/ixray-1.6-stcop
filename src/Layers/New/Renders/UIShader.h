#pragma once
#include "../../Include/xrRender/UIShader.h"

namespace Rendering
{
	class UIShader : public IUIShader
	{
	public:
		~UIShader() override;
		void Copy(IUIShader& _in) override;
		void create(const char* sh, const char* tex = 0) override;
		bool inited() override;
		void destroy() override;
	};
}