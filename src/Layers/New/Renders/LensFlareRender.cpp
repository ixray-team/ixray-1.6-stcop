#include "pch.h"

using namespace Rendering;

LensFlareRender::~LensFlareRender()
{
}

void 
LensFlareRender::Copy(ILensFlareRender& _in)
{
}

void
LensFlareRender::Render(CLensFlare& owner, BOOL bSun, BOOL bFlares, BOOL bGradient)
{
}

void 
LensFlareRender::OnDeviceCreate()
{
}

void 
LensFlareRender::OnDeviceDestroy()
{
}

FlareRender::~FlareRender()
{
}

void 
FlareRender::Copy(IFlareRender& _in)
{
}

void 
FlareRender::CreateShader(LPCSTR sh_name, LPCSTR tex_name)
{
}

void 
FlareRender::DestroyShader()
{
}
