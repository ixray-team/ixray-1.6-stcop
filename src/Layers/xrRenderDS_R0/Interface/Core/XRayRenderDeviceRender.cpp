#include "stdafx.h"

CDS0_RenderDeviceRender::CDS0_RenderDeviceRender()
{
}

CDS0_RenderDeviceRender::~CDS0_RenderDeviceRender()
{

}

void CDS0_RenderDeviceRender::Copy(IRenderDeviceRender & _in)
{
	R_ASSERT(0);
}

void CDS0_RenderDeviceRender::setGamma(float fGamma)
{
}

void CDS0_RenderDeviceRender::setBrightness(float fGamma)
{
}

void CDS0_RenderDeviceRender::setContrast(float fGamma)
{
}

void CDS0_RenderDeviceRender::updateGamma()
{
}

void CDS0_RenderDeviceRender::OnDeviceDestroy(bool bKeepTextures)
{
	GRenderInterface.destroy();
}

void CDS0_RenderDeviceRender::OnDeviceCreate(LPCSTR shName)
{
	GRenderInterface.create();
}

void CDS0_RenderDeviceRender::ValidateHW()
{
}

void CDS0_RenderDeviceRender::DestroyHW()
{
}

void CDS0_RenderDeviceRender::SetupStates()
{
}

void CDS0_RenderDeviceRender::SetupGPU(bool bForceGPU_SW, bool bForceGPU_NonPure, bool bForceGPU_REF)
{
}

void CDS0_RenderDeviceRender::overdrawBegin()
{
}

void CDS0_RenderDeviceRender::overdrawEnd()
{
}

void CDS0_RenderDeviceRender::DeferredLoad(bool E)
{
}

void CDS0_RenderDeviceRender::ResourcesDeferredUpload()
{
}

void CDS0_RenderDeviceRender::ResourcesDestroyNecessaryTextures()
{
}

void CDS0_RenderDeviceRender::ResourcesStoreNecessaryTextures()
{
}

bool CDS0_RenderDeviceRender::HWSupportsShaderYUV2RGB()
{
	return true;
}

IRenderDeviceRender::DeviceState CDS0_RenderDeviceRender::GetDeviceState()
{
	return DeviceState();
}

bool CDS0_RenderDeviceRender::GetForceGPU_REF()
{
	return 0;
}

u32 CDS0_RenderDeviceRender::GetCacheStatPolys()
{
	return 0;
}

void CDS0_RenderDeviceRender::Begin()
{
}

void CDS0_RenderDeviceRender::Clear()
{
	
}

void CDS0_RenderDeviceRender::End()
{
	
}

void CDS0_RenderDeviceRender::ClearTarget()
{
}

void CDS0_RenderDeviceRender::SetCacheXform( Fmatrix& mView,  Fmatrix& mProject)
{
}

void CDS0_RenderDeviceRender::OnAssetsChanged()
{
}

void CDS0_RenderDeviceRender::GetRenderScale(float& RenderScale)
{
	RenderScale = 1.0f;
}

void CDS0_RenderDeviceRender::ResourcesDumpMemoryUsage()
{
}

void CDS0_RenderDeviceRender::ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps)
{
}

void CDS0_RenderDeviceRender::Reset(SDL_Window* window, u32& dwWidth, u32& dwHeight)
{
	SDL_SetWindowFullscreen(g_AppInfo.Window, 0);
	SDL_SetWindowSize(window, psCurrentVidMode[0], psCurrentVidMode[0]);
	dwWidth = psCurrentVidMode[0];
	dwHeight = psCurrentVidMode[1];

	const bool bCentered = !Core.ParamsData.test(ECoreParams::no_center_screen);
	if (bCentered)
	{
		SDL_SetWindowPosition(window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	}
}

void CDS0_RenderDeviceRender::Create(SDL_Window* window, u32& dwWidth, u32& dwHeight, bool)
{
}

void CDS0_RenderDeviceRender::ResourcesDeferredUnload()
{
}

void CDS0_RenderDeviceRender::SetupDefaultTarget()
{
}

void CDS0_RenderDeviceRender::SetCacheXformOld(Fmatrix& mView, Fmatrix& mProject)
{
}

const FactoryPtr<IUIShader>& CDS0_RenderDeviceRender::GetSVGShader(const std::string_view& subpath, float width, float height, SVGTintRGBA tint)
{
	(void)subpath;
	(void)width;
	(void)height;
	(void)tint;
	return m_empty_default;
}

const FactoryPtr<IUIShader>& CDS0_RenderDeviceRender::GetSVGShader(const char* pSubpath, float width, float height, SVGTintRGBA tint)
{
	(void)pSubpath;
	(void)width;
	(void)height;
	(void)tint;
	return m_empty_default;
}

const FactoryPtr<IUIShader>& CDS0_RenderDeviceRender::GetSVGDefaultShader()
{
	return m_empty_default;
}

Frect CDS0_RenderDeviceRender::GetSVGUV(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint)
{
	(void)subpath;
	(void)requested_width;
	(void)requested_height;
	(void)tint;
	return Frect();
}

void CDS0_RenderDeviceRender::PostCreate()
{

}
