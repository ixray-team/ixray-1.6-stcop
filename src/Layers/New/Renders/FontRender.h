#pragma once
#include "../../Include/xrRender/FontRender.h"

namespace Rendering
{
	class FontRender : public IFontRender
	{
	public:
		~FontRender();
		void Initialize(LPCSTR cShader, LPCSTR cTexture) override;
		void OnRender(CGameFont& owner) override;
	};
}