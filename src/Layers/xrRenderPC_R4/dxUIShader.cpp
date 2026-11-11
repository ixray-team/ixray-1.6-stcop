#include "stdafx.h"
#include "dxUIShader.h"

void dxUIShader::Copy(IUIShader&_in)
{
	*this = *((dxUIShader*)&_in);
}

void dxUIShader::create(const char* sh, const char* tex)
{
	BlurredShadowScreen = nullptr;
	BlurredShadowWorld = nullptr;
	hShader.create(sh,tex);
}

void dxUIShader::destroy()
{
	BlurredShadowScreen = nullptr;
	BlurredShadowWorld = nullptr;
	hShader.destroy();
}