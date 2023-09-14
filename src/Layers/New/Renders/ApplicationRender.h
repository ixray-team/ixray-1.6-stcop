#pragma once
#include "../../Include/xrRender/ApplicationRender.h"

namespace Rendering
{
	class ApplicationRender : public IApplicationRender
	{
	public:
		~ApplicationRender();
		void Copy(IApplicationRender& _in) override;

		void LoadBegin() override;
		void destroy_loading_shaders() override;
		void setLevelLogo(LPCSTR pszLogoName) override;
		void load_draw_internal(CApplication& owner) override;
		void KillHW()override;
	};
}