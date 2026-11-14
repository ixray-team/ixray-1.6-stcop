#include "stdafx.h"
#include "dxUIShader.h"

void dxUIShader::Copy(IUIShader&_in)
{
	*this = *((dxUIShader*)&_in);
}

void dxUIShader::create(const char* sh, const char* tex)
{
	TextureBlendShader = nullptr;
	TextureBlendKey = "";
	BlurredShadowScreen = nullptr;
	BlurredShadowWorld = nullptr;
	hShader.create(sh,tex);
}

void dxUIShader::destroy()
{
	TextureBlendShader = nullptr;
	TextureBlendKey = "";
	BlurredShadowScreen = nullptr;
	BlurredShadowWorld = nullptr;
	hShader.destroy();
}
