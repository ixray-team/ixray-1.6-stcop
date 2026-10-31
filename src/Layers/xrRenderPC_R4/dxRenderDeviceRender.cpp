#include "stdafx.h"
#include "dxRenderDeviceRender.h"
#include "SVGStorage.h"

#include "dxDebugRender.h"
#include "StateManager/dx10SamplerStateCache.h"

#include "ResourceManager.h"
extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;
#include "imgui.h"
#include "../../xrEngine/Autotest.h"

dxRenderDeviceRender::dxRenderDeviceRender()
	:	Resources(0)
{
}

#include "..\xrRenderPC_R4\OverlayAPI\DLSSWrapper.h"
#include "..\xrRenderPC_R4\OverlayAPI\FSR3Wrapper.h"
#include "OverlayAPI/XESSWrapper.h"

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
			g_DLSSWrapper.GetRenderScale(RenderScale, ps_render_scale_preset, ps_render_scale, Device.TargetWidth, Device.TargetHeight);
		}
        break;
		case 3:
		{
			g_Fsr3Wrapper.GetRenderScale(RenderScale, ps_render_scale_preset, ps_render_scale, Device.TargetWidth, Device.TargetHeight);
		}
		break;
		case 4:
		{
			g_XESSWrapper.GetRenderScale(RenderScale, ps_render_scale_preset, ps_render_scale, Device.TargetWidth, Device.TargetHeight);
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
	GfxCmdList& Cmd = RImplementation.Imm();
	Resources->WaitForTextureLoading();
#ifdef DEBUG_DRAW
	DebugRenderImpl.Shutdown();
#endif // #ifdef DEBUG_DRAW

	Cmd.Invalidate();

	m_WireShader.destroy();
	m_SelectionShader.destroy();

	Resources->OnDeviceDestroy( bKeepTextures);
	Cmd.OnDeviceDestroy();
}

void dxRenderDeviceRender::ValidateHW()
{
}

void dxRenderDeviceRender::DestroyHW()
{
	xr_delete(Resources);
	SSManager.Destroy();
	CImGuiManager::Instance().Destroy(true);
}

xr_task_group thm_reload_task;

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
}

void dxRenderDeviceRender::OnDeviceCreate(const char* shName)
{
	SSManager.Create();
	// Signal everyone - device created
	RImplementation.Imm().OnDeviceCreate		();
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
	return RImplementation.Imm().Stats.Polys;
}

void dxRenderDeviceRender::GetCacheStats(u32& calls, u32& verts, u32& polys, u32& static_dips)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	calls		= Cmd.Stats.Calls;
	verts		= 0;
	polys		= Cmd.Stats.Polys;
	static_dips	= 0;
}

void dxRenderDeviceRender::Begin()
{
	GfxCmdList& Cmd = RImplementation.Imm();
	Cmd.OnFrameBegin();
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetDepthEnable(true);

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
	RImplementation.Imm().ClearDepthStencil(RImplementation.Imm().GetDepthStencil(), ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);

	if (psDeviceFlags.test(rsClearBB))
	{
		RImplementation.Imm().ClearTarget(RImplementation.Imm().GetRenderTarget());
	}
}

void DoAsyncScreenshot();

void dxRenderDeviceRender::End()
{
	GfxCmdList& Cmd = RImplementation.Imm();
	{
		GPU_EVENT(GAMMA_APPLY);
		RImplementation.Target->PhaseGammaApply(Cmd);
	}

	Cmd.OnFrameEnd();
	{
		PROF_EVENT("Async Screenshot");
		DoAsyncScreenshot();
	}
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
	{
		PROF_EVENT("ImGui EndRender");
		CImGuiManager& MyImGui = CImGuiManager::Instance();
		MyImGui.BeginRender();

		//Cmd.SetDepthStencil(RDepth);
		Cmd.SetRenderTarget(RSwapchainTarget, 0, true);

		MyImGui.Render();
		MyImGui.AfterRender();

		DebugRenderImpl.m_lines.resize(0);
		GRHI->GPUStatsEnd();
	}
#else

	//Cmd.SetDepthStencil(RDepth);
	Cmd.SetRenderTarget(RTarget, 0, true);

#endif

	if (Autotest::Active())
		Autotest::FrameEnd();

	PROF_EVENT("Present");

#if defined(IXRAY_PROFILER_TRACY)
	GRHI->CollectGPUProfiler();
#endif

	GRHI->Present();
}

void dxRenderDeviceRender::ResourcesDestroyNecessaryTextures()
{
	Resources->DestroyNecessaryTextures();
}

void dxRenderDeviceRender::ClearTarget()
{
	RImplementation.Imm().ClearTarget(RImplementation.Imm().GetRenderTarget());
}

void dxRenderDeviceRender::SetupDefaultTarget()
{
	RImplementation.Imm().SetRenderTarget(RImplementation.Target->rt_BackbufferLUT->pRT);
	RImplementation.Imm().SetDepthStencil(nullptr);
}

void dxRenderDeviceRender::InvalidateRenderCache()
{
	// ImGui binds hardware state directly, bypassing the engine caches.
	IRHIRenderTargetView* Targets[RHI_MAX_RENDER_TARGETS]{};
	for (u32 Index = 0; Index < RHI_MAX_RENDER_TARGETS; ++Index)
	{
		Targets[Index] = RImplementation.Imm().GetRenderTarget(Index);
	}
	auto* Depth = RImplementation.Imm().GetDepthStencil();
	RImplementation.Imm().Invalidate();
	for (u32 Index = 0; Index < RHI_MAX_RENDER_TARGETS; ++Index)
	{
		RImplementation.Imm().SetRenderTarget(Targets[Index], Index);
	}
	RImplementation.Imm().SetDepthStencil(Depth);
	RImplementation.Imm().ApplyRenderTargets();
}

UIIconLighting* dxRenderDeviceRender::GetUIIconLighting()
{
	return &IconLighting;
}

void dxRenderDeviceRender::SetCacheXform(Fmatrix &mView, Fmatrix &mProject)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	Cmd.SetView(mView, mProject);
}

void dxRenderDeviceRender::SetCacheXformOld(Fmatrix &mView, Fmatrix &mProject)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	Cmd.SetView({ Cmd.GetView().View, Cmd.GetView().Project, mView, mProject });
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
