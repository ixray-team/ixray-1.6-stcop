#include "stdafx.h"
#include "dxRenderDeviceRender.h"
#include "SVGStorage.h"

#include "dxDebugRender.h"

#include "ResourceManager.h"
#ifndef _EDITOR
#include "imgui.h"
#include "../../xrEngine/Autotest.h"
#endif

dxRenderDeviceRender::dxRenderDeviceRender()
#ifndef _EDITOR
	:	Resources(0)
{
}
#else
{}
#endif

#if !defined(_EDITOR)
#include "..\xrRenderPC_R4\OverlayAPI\DLSSWrapper.h"
#include "..\xrRenderPC_R4\OverlayAPI\FSR3Wrapper.h"
#endif

void dxRenderDeviceRender::GetRenderScale(float& RenderScale)
{
#if !defined(_EDITOR)
	if (LightingModeIsStatic())
	{
		RenderScale = 1.f;
		return;
	}
	switch(ps_r_scale_mode)
	{
		case 2:
		{
			g_DLSSWrapper.GetRenderScale(RenderScale);
		}
		case 3:
		{
			g_Fsr3Wrapper.GetRenderScale(RenderScale);
		}
		break;
	}
#else
	RenderScale = 1.0f;
#endif
}

void dxRenderDeviceRender::Copy(IRenderDeviceRender &_in)
{
	*this = *(dxRenderDeviceRender*)&_in;
}

void dxRenderDeviceRender::setGamma(float fGamma)
{
#ifndef _EDITOR
	m_Gamma.Gamma(fGamma);
#endif
}

void dxRenderDeviceRender::setBrightness(float fGamma)
{
#ifndef _EDITOR
	m_Gamma.Brightness(fGamma);
#endif
}

void dxRenderDeviceRender::setContrast(float fGamma)
{
#ifndef _EDITOR
	m_Gamma.Contrast(fGamma);
#endif
}

void dxRenderDeviceRender::updateGamma()
{
#ifndef _EDITOR
	m_Gamma.Update();
#endif
}

void dxRenderDeviceRender::OnDeviceDestroy( bool bKeepTextures)
{
#ifndef _EDITOR
#ifdef DEBUG_DRAW
	DebugRenderImpl.Shutdown();
#endif // #ifdef DEBUG_DRAW

	RCache.Invalidate();

	m_WireShader.destroy();
	m_SelectionShader.destroy();

	Resources->OnDeviceDestroy( bKeepTextures);
	RCache.OnDeviceDestroy();
#endif
}

void dxRenderDeviceRender::ValidateHW()
{
}

void dxRenderDeviceRender::DestroyHW()
{
#ifndef _EDITOR
	xr_delete(Resources);
	CImGuiManager::Instance().Destroy(true);
#endif
}

xr_task_group thm_reload_task;

void  dxRenderDeviceRender::Reset(SDL_Window* window, u32 &dwWidth, u32 &dwHeight)
{
#ifndef _EDITOR
	Resources->reset_begin	();
	Memory.mem_compact		();

	thm_reload_task.run
	(
		[this]()
		{
			OnAssetsChanged();
		}
	);

	ResourcesDeferredUnload();

	CImGuiManager::Instance().Reset();
	Device.ResizeWindow(psCurrentVidMode[0], psCurrentVidMode[1]);

	thm_reload_task.wait();
	ResourcesDeferredUpload();

	dwWidth = Device.GetSwapchainWidth();
	dwHeight = Device.GetSwapchainHeight();
	Resources->reset_end();
#endif
}

void dxRenderDeviceRender::SetupStates()
{
	Caps.Update();

#ifndef _EDITOR
	//	TODO: DX10: Implement Resetting of render states into default mode
	// SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);
	// SSManager.SetMipLodBias(ps_r__tf_Mipbias);
#endif
}

void dxRenderDeviceRender::OnDeviceCreate(const char* shName)
{
#ifndef _EDITOR
	// Signal everyone - device created
	RCache.OnDeviceCreate		();
	m_Gamma.Update				();
	Resources->OnDeviceCreate	(shName);
	::Render->create			();
	Device.Statistic->OnDeviceCreate	();

	if (!g_dedicated_server)
	{
		m_WireShader.create			("editor\\wire");
		m_SelectionShader.create	("editor\\selection");

		DUImpl.OnDeviceCreate			();
#if defined(DEBUG_DRAW)
		DebugRenderImpl.Init();
#endif
	}
#endif
}

void dxRenderDeviceRender::Create(SDL_Window* window, u32 &dwWidth, u32 &dwHeight, bool move_window)
{
#ifndef _EDITOR
	CImGuiManager::Instance().InitHardware();

	dwWidth = Device.GetSwapchainWidth();
	dwHeight = Device.GetSwapchainHeight();

	Resources = new CResourceManager();
#endif
}

void dxRenderDeviceRender::SetupGPU( bool bForceGPU_SW, bool bForceGPU_NonPure, bool bForceGPU_REF)
{
}

void dxRenderDeviceRender::overdrawBegin()
{
#ifndef _EDITOR
	//	TODO: DX10: Implement overdrawBegin
	VERIFY(!"dxRenderDeviceRender::overdrawBegin not implemented.");
#endif
}

void dxRenderDeviceRender::overdrawEnd()
{
#ifndef _EDITOR
	//	TODO: DX10: Implement overdrawEnd
	VERIFY(!"dxRenderDeviceRender::overdrawBegin not implemented.");
#endif
}

void dxRenderDeviceRender::DeferredLoad(bool E)
{
#ifndef _EDITOR
	Resources->DeferredLoad(E);
#endif
}

void dxRenderDeviceRender::ResourcesDeferredUpload()
{
#ifndef _EDITOR
	Resources->DeferredUpload();
#endif
}

void dxRenderDeviceRender::ResourcesDeferredUnload()
{
#ifndef _EDITOR
	Resources->DeferredUnload();
#endif
}

void dxRenderDeviceRender::ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps)
{
#ifndef _EDITOR
	if (Resources)
		Resources->_GetMemoryUsage(m_base, c_base, m_lmaps, c_lmaps);
#endif
}

void dxRenderDeviceRender::ResourcesStoreNecessaryTextures()
{
#ifndef _EDITOR
	dxRenderDeviceRender::Instance().Resources->StoreNecessaryTextures();
#endif
}

void dxRenderDeviceRender::ResourcesDumpMemoryUsage()
{
#ifndef _EDITOR
	dxRenderDeviceRender::Instance().Resources->_DumpMemoryUsage();
#endif
}

dxRenderDeviceRender::DeviceState dxRenderDeviceRender::GetDeviceState()
{
#ifndef _EDITOR
	//	TODO: DX10: Implement GetDeviceState
	//	TODO: DX10: Implement DXGI_PRESENT_TEST testing
	//VERIFY(!"dxRenderDeviceRender::overdrawBegin not implemented.");

#endif
	return dsOK;
}

bool dxRenderDeviceRender::GetForceGPU_REF()
{
	return false;
}

u32 dxRenderDeviceRender::GetCacheStatPolys()
{
#ifndef _EDITOR
	return RCache.stat.polys;
#else
	return 0;
#endif
}

void dxRenderDeviceRender::GetCacheStats(u32& calls, u32& verts, u32& polys, u32& static_dips)
{
#ifndef _EDITOR
	calls		= RCache.stat.calls;
	verts		= RCache.stat.verts;
	polys		= RCache.stat.polys;
	static_dips	= RCache.stat.r.s_static.dips;
#else
	calls = verts = polys = static_dips = 0;
#endif
}

void dxRenderDeviceRender::Begin()
{
#ifndef _EDITOR

	RCache.OnFrameBegin();
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_Z(true);
#endif

	if (Resources)
	{
		CSVGStorage* svg = Resources->GetSVGStorage();
		if (svg)
			svg->BeginRasterFrameCache();
	}

	GRHI->GPUStatsBegin();
}

void dxRenderDeviceRender::Clear()
{
#ifndef _EDITOR
	GRHI->ClearDepthStencil(GRHI->GetDepthStencilView(), ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);

	if (psDeviceFlags.test(rsClearBB))
	{
		GRHI->ClearTarget(RCache.get_RT());
	}
#endif
}

void DoAsyncScreenshot();

void dxRenderDeviceRender::End()
{
#ifndef _EDITOR

	{
		GPU_EVENT(GAMMA_APPLY);
		RImplementation.Target->PhaseGammaApply();
	}

	RCache.OnFrameEnd();
	{
		PROF_EVENT("Async Screenshot");
		DoAsyncScreenshot();
	}
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
	{
		PROF_EVENT("ImGui EndRender");
		CImGuiManager& MyImGui = CImGuiManager::Instance();
		MyImGui.BeginRender();

		//GRHI->SetDepthStencilView(RDepth);
		GRHI->SetRenderTargetView(RSwapchainTarget, 0, true);

		MyImGui.Render();
		MyImGui.AfterRender();

		DebugRenderImpl.m_lines.resize(0);
		GRHI->GPUStatsEnd();
	}
#else

	//GRHI->SetDepthStencilView(RDepth);
	GRHI->SetRenderTargetView(RTarget, 0, true);

#endif

	if (Autotest::Active())
		Autotest::FrameEnd();

	PROF_EVENT("Present");

#if defined(IXRAY_PROFILER_TRACY)
	PROF_GPU_CTX_COLLECT();
#endif

	GRHI->Present();
#endif
}

void dxRenderDeviceRender::ResourcesDestroyNecessaryTextures()
{
#ifndef _EDITOR
	Resources->DestroyNecessaryTextures();
#endif
}

void dxRenderDeviceRender::ClearTarget()
{
#ifndef _EDITOR
	GRHI->ClearTarget(RCache.get_RT());
#endif
}

void dxRenderDeviceRender::SetupDefaultTarget()
{
#ifndef _EDITOR
	RCache.set_RT(RImplementation.Target->rt_BackbufferLUT->pRT);
	GRHI->SetDepthStencilView(nullptr);
#endif
}

void dxRenderDeviceRender::SetCacheXform(Fmatrix &mView, Fmatrix &mProject)
{
#ifndef _EDITOR
	RCache.set_xform_view(mView);
	RCache.set_xform_project(mProject);
#endif
}

void dxRenderDeviceRender::SetCacheXformOld(Fmatrix &mView, Fmatrix &mProject)
{
	RCache.set_xform_view_old(mView);
	RCache.set_xform_project_old(mProject);
}

bool dxRenderDeviceRender::HWSupportsShaderYUV2RGB()
{
	return true;
}

void  dxRenderDeviceRender::OnAssetsChanged()
{
#ifndef _EDITOR
	Resources->m_textures_description.UnLoad();
	Resources->m_textures_description.Load();
#endif
}

void dxRenderDeviceRender::PostCreate()
{
	R_ASSERT2(Resources, "must be valid or early calling");

	if (Resources)
	{
		Resources->Initialize_SVGStorage();
	}
}

const FactoryPtr<IUIShader>& dxRenderDeviceRender::GetSVGShader(const std::string_view& subpath, float width, float height, SVGTintRGBA tint)
{
	if (Resources)
	{
		R_ASSERT(subpath.empty() == false && "must be not empty path");

		CSVGStorage* pStorage = Resources->GetSVGStorage();

		R_ASSERT(pStorage && "must be valid!");

		if (pStorage)
		{
			return pStorage->get_shader(subpath, width, height, tint);
		}
	}

	return m_empty_default_shader;
}

const FactoryPtr<IUIShader>& dxRenderDeviceRender::GetSVGShader(const char* pSubpath, float width, float height, SVGTintRGBA tint)
{
	R_ASSERT(pSubpath && "invalid string (nullptr)");
	R_ASSERT(pSubpath[0] != '\0' && "empty string");

	return GetSVGShader(std::string_view(pSubpath), width, height, tint);
}

const FactoryPtr<IUIShader>& dxRenderDeviceRender::GetSVGDefaultShader()
{
	if (Resources)
	{

	}

	return m_empty_default_shader;
}

Frect dxRenderDeviceRender::GetSVGUV(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint)
{
	if (Resources)
	{
		R_ASSERT(subpath.empty() == false && "must be not empty path");

		CSVGStorage* pStorage = Resources->GetSVGStorage();

		R_ASSERT(pStorage && "must be valid!");

		if (pStorage)
		{
			return pStorage->get_uv(subpath, requested_width, requested_height, tint);
		}
	}

	return Frect();
}
