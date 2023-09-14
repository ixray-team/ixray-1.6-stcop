#include "pch.h"

using namespace Rendering;

RenderDeviceRender::~RenderDeviceRender()
{
}

void 
RenderDeviceRender::Copy(IRenderDeviceRender& _in)
{
}

void 
RenderDeviceRender::setGamma(float fGamma)
{
}

void 
RenderDeviceRender::setBrightness(float fGamma)
{
}

void 
RenderDeviceRender::setContrast(float fGamma)
{
}

void 
RenderDeviceRender::updateGamma()
{
}

void 
RenderDeviceRender::OnDeviceDestroy(BOOL bKeepTextures)
{
}

void 
RenderDeviceRender::ValidateHW()
{
}

void 
RenderDeviceRender::DestroyHW()
{
}

void 
RenderDeviceRender::Reset(HWND hWnd, u32& dwWidth, u32& dwHeight)
{
}

void 
RenderDeviceRender::SetupStates()
{
}

void 
RenderDeviceRender::OnDeviceCreate(LPCSTR shName)
{
}

void 
RenderDeviceRender::Create(HWND hWnd, u32& dwWidth, u32& dwHeight, bool)
{
}

void
RenderDeviceRender::SetupGPU(BOOL bForceGPU_SW, BOOL bForceGPU_NonPure, BOOL bForceGPU_REF)
{
}

void 
RenderDeviceRender::overdrawBegin()
{
}

void 
RenderDeviceRender::overdrawEnd()
{
}

void 
RenderDeviceRender::DeferredLoad(BOOL E)
{
}

void 
RenderDeviceRender::ResourcesDeferredUpload()
{
}

void 
RenderDeviceRender::ResourcesDeferredUnload()
{
}

void 
RenderDeviceRender::ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps)
{
}

void 
RenderDeviceRender::ResourcesDestroyNecessaryTextures()
{
}

void 
RenderDeviceRender::ResourcesStoreNecessaryTextures()
{
}

void
RenderDeviceRender::ResourcesDumpMemoryUsage()
{
}

bool 
RenderDeviceRender::HWSupportsShaderYUV2RGB()
{
	return false;
}

IRenderDeviceRender::DeviceState 
RenderDeviceRender::GetDeviceState()
{
	return DeviceState::dsOK;
}

BOOL 
RenderDeviceRender::GetForceGPU_REF()
{
	return 0;
}

u32
RenderDeviceRender::GetCacheStatPolys()
{
	return u32();
}

void
RenderDeviceRender::Begin()
{
}

void 
RenderDeviceRender::Clear()
{
}

void 
RenderDeviceRender::End()
{
}

void
RenderDeviceRender::ClearTarget()
{
}

void
RenderDeviceRender::SetCacheXform(Fmatrix& mView, Fmatrix& mProject)
{
}

void
RenderDeviceRender::SetCachePrevXform(Fmatrix& mView, Fmatrix& mProject)
{
}

void
RenderDeviceRender::ResetXform(Fmatrix& mView, Fmatrix& mProject)
{
}

void
RenderDeviceRender::ResetPrevXform(Fmatrix& mView, Fmatrix& mProject)
{
}

void
RenderDeviceRender::OnAssetsChanged()
{
}
