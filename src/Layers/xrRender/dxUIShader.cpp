#include "stdafx.h"
#include "dxUIShader.h"

void dxUIShader::Copy(IUIShader&_in)
{
	*this = *((dxUIShader*)&_in);
}

void dxUIShader::create(const char* sh, const char* tex)
{
	hShader.create(sh,tex);
}

void dxUIShader::destroy()
{
	hShader.destroy();
}

IUIShader::TextureSurface dxUIShader::GetTextureSurface() const
{
	const auto* Element = hShader ? hShader->E[0]._get() : nullptr;
	const auto* Pass = Element && !Element->passes.empty() ? Element->passes[0]._get() : nullptr;
	if (!Pass || !Pass->T)
	{
		return {};
	}

	for (const auto& [Slot, Texture] : *Pass->T)
	{
		if (Slot == CTexture::rstPixel && Texture && Texture->pSurface)
		{
			if (auto* View = Texture->GetView())
			{
				return {View->GetRawSRV(), static_cast<float>(Texture->get_Width()), static_cast<float>(Texture->get_Height())};
			}
		}
	}

	return {};
}
