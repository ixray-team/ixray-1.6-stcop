#include "stdafx.h"
#include "dxRenderDeviceRender.h"

#ifdef IXR_WINDOWS
#	include <dxgi1_4.h>
#endif
#include "SVGStorage.h"

#include "dxDebugRender.h"

#include "ResourceManager.h"
#include "imgui.h"
#include "../../xrEngine/Autotest.h"

dxRenderDeviceRender::dxRenderDeviceRender()
	:	Resources(0)
{
}

#include "..\xrRenderPC_R4\OverlayAPI\DLSSWrapper.h"
#include "..\xrRenderPC_R4\OverlayAPI\FSR3Wrapper.h"

void dxRenderDeviceRender::GetRenderScale(float& RenderScale)
{
	if (LightingModeIsStaticRender())
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
}

void dxRenderDeviceRender::Copy(IRenderDeviceRender &_in)
{
	*this = *(dxRenderDeviceRender*)&_in;
}

void dxRenderDeviceRender::setGamma(float fGamma)
{
	m_Gamma.Gamma(fGamma);
}

void dxRenderDeviceRender::setBrightness(float fGamma)
{
	m_Gamma.Brightness(fGamma);
}

void dxRenderDeviceRender::setContrast(float fGamma)
{
	m_Gamma.Contrast(fGamma);
}

void dxRenderDeviceRender::updateGamma()
{
	m_Gamma.Update();
}

void dxRenderDeviceRender::OnDeviceDestroy( bool bKeepTextures)
{
	Resources->WaitForTextureLoading();
#ifdef DEBUG_DRAW
	DebugRenderImpl.Shutdown();
#endif // #ifdef DEBUG_DRAW

	RCache.Invalidate();

	m_WireShader.destroy();
	m_SelectionShader.destroy();

	Resources->OnDeviceDestroy( bKeepTextures);
	RCache.OnDeviceDestroy();
}

void dxRenderDeviceRender::ValidateHW()
{
}

void dxRenderDeviceRender::DestroyHW()
{
	xr_delete(Resources);
	CImGuiManager::Instance().Destroy(true);
}

xr_task_group thm_reload_task{ ETaskPriority::Background };

void  dxRenderDeviceRender::Reset(SDL_Window* window, u32 &dwWidth, u32 &dwHeight)
{
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
}

void dxRenderDeviceRender::SetupStates()
{
	Caps.Update();

	//	TODO: DX10: Implement Resetting of render states into default mode
	// SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);
	// SSManager.SetMipLodBias(ps_r__tf_Mipbias);
}

void dxRenderDeviceRender::OnDeviceCreate(const char* shName)
{
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
}

void dxRenderDeviceRender::Create(SDL_Window* window, u32 &dwWidth, u32 &dwHeight, bool move_window)
{
	CImGuiManager::Instance().InitHardware();

	dwWidth = Device.GetSwapchainWidth();
	dwHeight = Device.GetSwapchainHeight();

	Resources = new CResourceManager();
}

void dxRenderDeviceRender::SetupGPU( bool bForceGPU_SW, bool bForceGPU_NonPure, bool bForceGPU_REF)
{
}

void dxRenderDeviceRender::overdrawBegin()
{
	//	TODO: DX10: Implement overdrawBegin
	VERIFY(!"dxRenderDeviceRender::overdrawBegin not implemented.");
}

void dxRenderDeviceRender::overdrawEnd()
{
	//	TODO: DX10: Implement overdrawEnd
	VERIFY(!"dxRenderDeviceRender::overdrawBegin not implemented.");
}

void dxRenderDeviceRender::DeferredLoad(bool E)
{
	Resources->DeferredLoad(E);
}

void dxRenderDeviceRender::ResourcesDeferredUpload()
{
	Resources->DeferredUpload();
}

void dxRenderDeviceRender::ResourcesDeferredUnload()
{
	Resources->DeferredUnload();
}

void dxRenderDeviceRender::ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps)
{
	if (Resources)
		Resources->_GetMemoryUsage(m_base, c_base, m_lmaps, c_lmaps);
}

bool dxRenderDeviceRender::GetVideoMemoryInfo(SVideoMemoryInfo& Info)
{
	if (!Resources)
	{
		return false;
	}

	Resources->_GetVideoMemoryUsage(Info.RenderTargets, Info.ShadowMaps, Info.Textures);

	Info.RenderWidth = u32(RCache.get_width());
	Info.RenderHeight = u32(RCache.get_height());

#ifdef IXR_WINDOWS
	IDXGIDevice* DxgiDevice = nullptr;
	if (RDevice && SUCCEEDED(RDevice->QueryInterface(__uuidof(IDXGIDevice), (void**)&DxgiDevice)))
	{
		IDXGIAdapter* Adapter = nullptr;
		if (SUCCEEDED(DxgiDevice->GetAdapter(&Adapter)))
		{
			IDXGIAdapter3* Adapter3 = nullptr;
			DXGI_QUERY_VIDEO_MEMORY_INFO MemoryInfo = {};

			if (SUCCEEDED(Adapter->QueryInterface(__uuidof(IDXGIAdapter3), (void**)&Adapter3)) &&
				SUCCEEDED(Adapter3->QueryVideoMemoryInfo(0, DXGI_MEMORY_SEGMENT_GROUP_LOCAL, &MemoryInfo)))
			{
				Info.Budget = MemoryInfo.Budget;
				Info.Usage = MemoryInfo.CurrentUsage;
			}
			else
			{
				DXGI_ADAPTER_DESC Desc = {};
				if (SUCCEEDED(Adapter->GetDesc(&Desc)))
				{
					Info.Budget = Desc.DedicatedVideoMemory;
				}
			}

			if (Adapter3)
			{
				Adapter3->Release();
			}

			Adapter->Release();
		}

		DxgiDevice->Release();
	}
#endif

	return true;
}

void dxRenderDeviceRender::ResourcesStoreNecessaryTextures()
{
	dxRenderDeviceRender::Instance().Resources->StoreNecessaryTextures();
}

void dxRenderDeviceRender::ResourcesDumpMemoryUsage()
{
	dxRenderDeviceRender::Instance().Resources->_DumpMemoryUsage();
}

dxRenderDeviceRender::DeviceState dxRenderDeviceRender::GetDeviceState()
{
	//	TODO: DX10: Implement GetDeviceState
	//	TODO: DX10: Implement DXGI_PRESENT_TEST testing
	//VERIFY(!"dxRenderDeviceRender::overdrawBegin not implemented.");

	return dsOK;
}

bool dxRenderDeviceRender::GetForceGPU_REF()
{
	return false;
}

u32 dxRenderDeviceRender::GetCacheStatPolys()
{
	return RCache.stat.polys;
}

void dxRenderDeviceRender::GetCacheStats(u32& calls, u32& verts, u32& polys, u32& static_dips)
{
	calls		= RCache.stat.calls;
	verts		= RCache.stat.verts;
	polys		= RCache.stat.polys;
	static_dips	= RCache.stat.r.s_static.dips;
}

void dxRenderDeviceRender::Begin()
{

	RCache.OnFrameBegin();
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_Z(true);

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
	GRHI->ClearDepthStencil(GRHI->GetDepthStencilView(), ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);

	if (psDeviceFlags.test(rsClearBB))
	{
		GRHI->ClearTarget(RCache.get_RT());
	}
}

void DoAsyncScreenshot();

void dxRenderDeviceRender::End()
{

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
}

void dxRenderDeviceRender::ResourcesDestroyNecessaryTextures()
{
	Resources->DestroyNecessaryTextures();
}

void dxRenderDeviceRender::ClearTarget()
{
	GRHI->ClearTarget(RCache.get_RT());
}

void dxRenderDeviceRender::SetupDefaultTarget()
{
	RCache.set_RT(RImplementation.Target->rt_BackbufferLUT->pRT);
	GRHI->SetDepthStencilView(nullptr);
}

void dxRenderDeviceRender::InvalidateRenderCache()
{
	// ImGui binds hardware state directly, bypassing the engine caches.
	IRHIRenderTargetView* Targets[RHI_MAX_RENDER_TARGETS]{};
	for (u32 Index = 0; Index < RHI_MAX_RENDER_TARGETS; ++Index)
	{
		Targets[Index] = GRHI->GetRenderTargetView(Index);
	}
	auto* Depth = GRHI->GetDepthStencilView();
	RCache.Invalidate();
	for (u32 Index = 0; Index < RHI_MAX_RENDER_TARGETS; ++Index)
	{
		GRHI->SetRenderTargetView(Targets[Index], Index);
	}
	GRHI->SetDepthStencilView(Depth);
	GRHI->ApplyRenderTargetChange();
}

UIIconLighting* dxRenderDeviceRender::GetUIIconLighting()
{
	return &IconLighting;
}

void dxRenderDeviceRender::SetCacheXform(Fmatrix &mView, Fmatrix &mProject)
{
	RCache.set_xform_view(mView);
	RCache.set_xform_project(mProject);
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
	Resources->ReloadTextureDescriptions();
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
