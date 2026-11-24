#include "stdafx.h"
#include "dxEnvironmentRender.h"
#include "dxRenderDeviceRender.h"

#ifdef IXR_WINDOWS
#	include <dxgi1_4.h>
#endif
#include "SVGStorage.h"

#include "dxDebugRender.h"
#include "StateManager/dx10SamplerStateCache.h"

#include "ResourceManager.h"
extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;
#include "imgui.h"
#include "../../xrEngine/Autotest.h"
#include "RenderThread.h"

dxRenderDeviceRender::dxRenderDeviceRender()
	:	Resources(0)
{
}

#include "..\xrRenderIXR\OverlayAPI\FSR3Wrapper.h"
#include "UpscaleNri.h"

void dxRenderDeviceRender::GetRenderScale(float& RenderScale)
{
	if (!g_RenderFeatures.Upscaling)
	{
		RenderScale = 1.f;
		return;
	}
	if (ps_r_scale_mode < 2)
	{
		return;
	}

	// A reset asks before it makes the targets: they are made for the video mode, whatever view is drawn now
	const u32 OutputWidth = psCurrentVidMode[0];
	const u32 OutputHeight = psCurrentVidMode[1];

	if (!UpscaleNri::IsWanted())
	{
		g_Fsr3Wrapper.GetRenderScale(RenderScale, ps_render_scale_preset, ps_render_scale, OutputWidth, OutputHeight);
		return;
	}

	u32 Width, Height;

	if (!UpscaleNri::Sync(OutputWidth, OutputHeight) || !UpscaleNri::RenderSize(Width, Height))
	{
		return;
	}

	// The device makes its targets of the scale, cut to whole pixels and rounded up to even ones; the upscaler
	// takes no frame larger than the one it was made for
	const auto Fit = [&](u32 Target)
	{
		const u32 Size = u32(float(Target) * RenderScale);
		return Size + Size % 2;
	};

	RenderScale = float(Height) / float(OutputHeight);

	while (RenderScale > 0.1f && (Fit(OutputWidth) > Width || Fit(OutputHeight) > Height))
	{
		RenderScale -= 0.5f / float(OutputHeight);
	}
}

namespace
{
// Made by the first frame that hands its end over, gone with the device
xr_unique_ptr<RenderThread> g_RenderThread;
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
	RenderThread::WaitIdle();
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
	m_ImGuiShader.destroy();

	Resources->OnDeviceDestroy( bKeepTextures);
	Cmd.OnDeviceDestroy();
}

void dxRenderDeviceRender::ValidateHW()
{
}

void dxRenderDeviceRender::DestroyHW()
{
	RenderThread::WaitIdle();
	g_RenderThread.reset();
	RFrame.Release();

	xr_delete(Resources);
	SSManager.Destroy();
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

void dxRenderDeviceRender::ResizeSwapChain(const RHISwapChainDesc& Desc)
{
	RenderThread::WaitIdle();
	// The passes keep wrappers of the last frames' back buffers, which would keep the old swap chain alive
	RImplementation.NriPasses.ReleaseTextures();
	GRHI->ResizeSwapChain(Desc);
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
		// Its texture is bound by hand, an image a draw
		m_ImGuiShader.create		("hud\\default", "$null");

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

	Info.RenderWidth = u32(GfxCmdList::GetWidth());
	Info.RenderHeight = u32(GfxCmdList::GetHeight());

#ifdef IXR_WINDOWS
	IUnknown* RawDevice = static_cast<IUnknown*>(GRHI->DevicePtr->RawDevice);
	IDXGIDevice* DxgiDevice = nullptr;
	if (RawDevice && SUCCEEDED(RawDevice->QueryInterface(__uuidof(IDXGIDevice), (void**)&DxgiDevice)))
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
	return RImplementation.Imm().Stats.Polys;
}

void dxRenderDeviceRender::GetCacheStats(u32& calls, u32& verts, u32& polys, u32& static_dips)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	calls		= Cmd.Stats.Calls;
	verts		= 0;
	polys		= Cmd.Stats.Polys;
	static_dips	= 0;
	for (const xr_atomic_u32& Count : R_dsgraph_structure::PacketsDrawn)
	{
		static_dips += Count.load(std::memory_order_relaxed);
	}
}

void RenderThread::WaitIdle()
{
	// The render thread's own code asks too (a console command it runs, a screenshot it ends): it does not wait for itself
	if (g_RenderThread && !RenderThread::IsCurrent())
	{
		g_RenderThread->Wait();
	}
}

// With the frames one behind the game (r_render_thread pipelined) the last frame was left to the render thread and
// the game went on to this one. They meet here: the last one is drawn and shown before this one takes anything of the
// renderer's, what the game let go meanwhile is freed, and everything this thread draws until End is recorded
void dxRenderDeviceRender::Begin()
{
	const bool IsPipelined = RenderThread::IsPipelined();

	if (IsPipelined)
	{
		const u64 Entry = SDL_GetTicksNS();

		if (!g_RenderThread)
		{
			g_RenderThread = xr_make_unique<RenderThread>();
		}

		RenderThread::WaitIdle();

		if (IsAutotestOwed)
		{
			IsAutotestOwed = false;
			Autotest::FrameEnd();
		}

		RImplementation.FrameSync();
		GRHI->BeginFrame();
		RImplementation.Pacer.Meet(Entry);
	}

	// The frame's camera, as the game left it
	RFrame.Sync();
	RFrame.SyncWeather();
	RImplementation.ViewerTrack = RImplementation.ViewerLightTrack();

	if (IsPipelined)
	{
		dxEnvironmentRender::Sync();
		RImplementation.CollectGraphsNow();
		// The render thread is idle: the frame's own begin runs here, before the collect counts on it
		BeginDraw();
		DrawRecorder::OpenList(RImplementation.FrameDraws);
		return;
	}

	BeginDraw();
}

void dxRenderDeviceRender::BeginDraw()
{
	GfxCmdList& Cmd = RImplementation.Imm();
	Cmd.OnFrameBegin();

	if (RenderThread::IsPipelined())
	{
		Cmd.SetView({ RFrame.mView, RFrame.mProject, RFrame.mView_old, RFrame.mProject_old });
	}

	for (xr_atomic_u32& Count : R_dsgraph_structure::PacketsDrawn)
	{
		Count.store(0, std::memory_order_relaxed);
	}
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

// With r_render_thread what touches the graphics device runs on the render thread, and this thread waits for it: the
// tools' widgets and windows and the autotest's steps are of the window's thread and stay here (the render thread
// plan's stage 2: the frame itself is still drawn here)
void dxRenderDeviceRender::End()
{
	const bool IsThreaded = RenderThread::IsWanted();

	if (RenderThread::IsPipelined())
	{
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
		{
			PROF_EVENT("ImGui Build");
			CImGuiManager& MyImGui = CImGuiManager::Instance();
			MyImGui.BeginRender();
			MyImGui.Build();
			MyImGui.UpdatePlatform();
		}
#endif

		DrawRecorder::CloseList();
		RImplementation.Pacer.HandOver();
		g_RenderThread->Post([this]
		{
			Recorder.Play(RImplementation.FrameDraws);
			EndScene(true);
		});
		// The autotest's steps follow the frame's readback: at the frames' meeting (Begin)
		IsAutotestOwed = Autotest::Active();
		g_RenderThread->Post([]
		{
			PROF_EVENT("Present");
			GRHI->Present();
			RImplementation.Pacer.Collect();
			RImplementation.Target->CreateSpotShadowDepths();
			RImplementation.Pacer.Land();
		});
		return;
	}

	const auto Run = [&](const std::function<void()>& Job)
	{
		RunFrame(Job);
	};

#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
	if (IsThreaded)
	{
		PROF_EVENT("ImGui Build");
		CImGuiManager& MyImGui = CImGuiManager::Instance();
		MyImGui.BeginRender();
		MyImGui.Build();
		MyImGui.UpdatePlatform();
	}
#endif

	Run([&] { EndScene(IsThreaded); });

	if (Autotest::Active())
		Autotest::FrameEnd();

	Run([]
	{
		PROF_EVENT("Present");

#if defined(IXRAY_PROFILER_TRACY)
		GRHI->CollectGPUProfiler();
#endif

		GRHI->Present();
		RImplementation.Pacer.Collect();
		RImplementation.Target->CreateSpotShadowDepths();
	});
}

void dxRenderDeviceRender::RunFrame(const std::function<void()>& Job, bool IsScene)
{
	if (!RenderThread::IsWanted())
	{
		Job();
		return;
	}

	// What the frame reads of the game's objects is read here, on the game's thread; the render thread draws from it
	if (!g_RenderThread)
	{
		g_RenderThread = xr_make_unique<RenderThread>();
	}

	// Everything of this thread is recorded from Begin to End: the job runs here, into the frame's list
	if (RenderThread::IsPipelined())
	{
		if (IsScene)
		{
			RImplementation.CollectFrame();
			RImplementation.RecordedSceneCount = 0;
		}

		Job();
		return;
	}

	// The scene's phase is the game's code, which calls the renderer for the scene and draws its interface around it:
	// it runs here, its draws kept in a list, and the render thread plays the list (the scene's own draw is one entry)
	if (IsScene)
	{
		RImplementation.CollectFrame();
		RImplementation.RecordedSceneCount = 0;

		{
			DrawRecorder::Scope Record(RImplementation.FrameDraws);
			Job();
		}

		g_RenderThread->Do([] { Recorder.Play(RImplementation.FrameDraws); });
		return;
	}

	g_RenderThread->Do(Job);
}

void dxRenderDeviceRender::EndScene(bool IsBuilt)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	{
		GPU_EVENT(GAMMA_APPLY);
		RImplementation.Target->PhaseGammaApply(Cmd);
	}
	RImplementation.NriPasses.EndFrame();
	RImplementation.Dynamic.EndFrame();
	RImplementation.Scene.EndFrame();

	Cmd.OnFrameEnd();
	{
		PROF_EVENT("Async Screenshot");
		DoAsyncScreenshot();
		Autotest::CaptureFrame();
	}
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
	{
		PROF_EVENT("ImGui EndRender");
		CImGuiManager& MyImGui = CImGuiManager::Instance();

		if (!IsBuilt)
		{
			MyImGui.BeginRender();
		}

		//Cmd.SetDepthStencil(RDepth);
		Cmd.SetRenderTarget(RSwapchainTarget, 0, true);

		if (IsBuilt)
		{
			MyImGui.Draw();
			MyImGui.RenderPlatform();
		}
		else
		{
			MyImGui.Render();
			MyImGui.AfterRender();
		}

		DebugRenderImpl.m_lines.resize(0);
		GRHI->GPUStatsEnd();
	}
#else

	//Cmd.SetDepthStencil(RDepth);
	Cmd.SetRenderTarget(RTarget, 0, true);

#endif

}

void dxRenderDeviceRender::ResourcesDestroyNecessaryTextures()
{
	RenderThread::WaitIdle();
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

UIIconLighting* dxRenderDeviceRender::GetUIIconLighting()
{
	return &IconLighting;
}

void dxRenderDeviceRender::SetCacheXform(Fmatrix &mView, Fmatrix &mProject)
{
	// The game's next frame sets its camera while the last one is drawn: the frame takes it when it begins (BeginDraw)
	if (RenderThread::IsPipelined() && !DrawRecorder::IsRecording() && !RenderThread::IsCurrent())
	{
		return;
	}

	Recorder.SetView(mView, mProject);
}

void dxRenderDeviceRender::SetCacheXformOld(Fmatrix &mView, Fmatrix &mProject)
{
	if (RenderThread::IsPipelined() && !DrawRecorder::IsRecording() && !RenderThread::IsCurrent())
	{
		return;
	}

	Recorder.Call([View = mView, Project = mProject]
	{
		GfxCmdList& Cmd = RImplementation.Imm();
		Cmd.SetView({ Cmd.GetView().View, Cmd.GetView().Project, View, Project });
	});
}

bool dxRenderDeviceRender::HWSupportsShaderYUV2RGB()
{
	return true;
}

void  dxRenderDeviceRender::OnAssetsChanged()
{
	RenderThread::WaitIdle();
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
