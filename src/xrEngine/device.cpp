#include "stdafx.h"
#include "../xrCore/FrameStackSampler.h"
#include <csignal>
#include "../xrCore/Collision/Frustum.h"

#include "x_ray.h"
#include "Render.h"
#include "EngineThreading.h"
#include "FPSCounter.h"
#include "MonsterLogicTelemetry.h"
#include "IGame_Level.h"
#include "Autotest.h"

#include "../xrCore/FS_impl.h"
#include "../xrCore/Kernel/TaskbarProgress.h"
#include "IGame_Persistent.h"
#include "IGame_Actor.h"

ENGINE_API CRenderDevice* DevicePtr = nullptr;
ENGINE_API CLoadScreenRenderer load_screen_renderer;
ENGINE_API CTimer loading_save_timer;
ENGINE_API bool loading_save_timer_started = false;
ENGINE_API xr_atomic_bool g_bRendering = false;
extern ENGINE_API float psHUD_FOV;
bool IsFpsShow = false;

bool g_bLoaded = false;
ref_light precache_light = 0;

bool CRenderDevice::Begin()
{
	PROF_EVENT("Render: Begin");

	if (g_dedicated_server)
	{
		return true;
	}

	switch (m_pRender->GetDeviceState())
	{
	case IRenderDeviceRender::dsOK:
		break;

	case IRenderDeviceRender::dsLost:
		// If the device was lost, do not render until we get it back
		Sleep(33);
		return false;
		break;

	case IRenderDeviceRender::dsNeedReset:
		// Check if the device is ready to be reset
		Reset();
		break;

	default:
		R_ASSERT(0);
	}

	m_pRender->Begin();

	g_bRendering = true;

	return true;
}

void CRenderDevice::Clear()
{
	m_pRender->Clear();
}

void CRenderDevice::End(void)
{
	PROF_EVENT("Render: End");
	if (g_dedicated_server)
	{
		return;
	}

	if (dwPrecacheFrame)
	{
		::Sound->set_master_volume	(0.f);
		dwPrecacheFrame	--;

		if (0==dwPrecacheFrame)
		{
			m_pRender->updateGamma();

			if(precache_light) precache_light->set_active	(false);
			if(precache_light) precache_light.destroy		();
			::Sound->set_master_volume						(1.f);

			m_pRender->ResourcesDestroyNecessaryTextures	();
			Memory.mem_compact								();
			// Msg("* MEMORY USAGE: %d K",Memory.mem_usage()/1024);
			Msg												("* End of synchronization A[%d] R[%d]",b_is_Active, b_is_Ready);
			if (loading_save_timer_started) 
			{
				Msg("* Game Loading Timer: Finished for %d ms", loading_save_timer.GetElapsed_ms());
				loading_save_timer_started = false;
			}
		}
	}

	g_bRendering		= false;
	// end scene

	m_pRender->End();
}

void CRenderDevice::PreCache(u32 amount, bool b_draw_loadscreen, bool b_wait_user_input)
{
	if (m_pRender->GetForceGPU_REF() || g_dedicated_server)
	{
		amount = 0;
	}

	dwPrecacheFrame = dwPrecacheTotal = amount;

	if (amount && !precache_light && g_pGameLevel && g_loading_events.empty())
	{
		precache_light					= ::Render->light_create();
		precache_light->set_shadow		(false);
		precache_light->set_position	(vCameraPosition);
		precache_light->set_color		(255,255,255);
		precache_light->set_range		(5.0f);
		precache_light->set_active		(true);
	}

	if(amount && b_draw_loadscreen && load_screen_renderer.b_registered==false)
	{
		load_screen_renderer.start	(b_wait_user_input);
	}
}

int g_svDedicateServerUpdateReate = 100;

ENGINE_API xr_list<LOADING_EVENT> g_loading_events;
ENGINE_API Ivector2 g_UiView = { 0, 0 };
ENGINE_API bool g_IsViewResize = false;
int fps_limit, main_menu_fps_limit = 144;

void CRenderDevice::time_factor(const float &time_factor)
{
	Timer.time_factor		(time_factor);
	TimerGlobal.time_factor	(time_factor);
	Sound->time_factor		(time_factor);
}

RHISwapChainDesc CRenderDevice::WantedSwapChain() const
{
	// A window with no pixels (minimised) keeps the size it had
	const bool HasSize = Window.PixelWidth > 0 && Window.PixelHeight > 0;
	const u32 Width = HasSize ? u32(Window.PixelWidth) : GRHI->SwapChain.Width;
	const u32 Height = HasSize ? u32(Window.PixelHeight) : GRHI->SwapChain.Height;
	return { Width ? Width : OutputWidth, Height ? Height : OutputHeight, u32(psFrameQueue), psDeviceFlags.test(rsVSync) };
}

void CRenderDevice::FitView()
{
	const u32 SwapWidth = std::max(GRHI->SwapChain.Width, 1u);
	const u32 SwapHeight = std::max(GRHI->SwapChain.Height, 1u);
	u32 ViewWidth = 0;
	u32 ViewHeight = 0;
	bool IsHosted = false;

#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
	IsHosted = CImGuiManager::Instance().FitGameView(OutputWidth, OutputHeight, ViewWidth, ViewHeight);
#endif

	// The window pixel for pixel, or shrunk with its proportions into the targets, and r_view_scale shrinks it more.
	// Even sizes: the half-size targets of the frame take whole pixels of them
	if (!IsHosted)
	{
		const float Scale = std::min({ 1.0f, float(OutputWidth) / float(SwapWidth), float(OutputHeight) / float(SwapHeight) }) * psViewScale;
		const auto Fit = [Scale](u32 Size, u32 Most)
		{
			return std::min(Most, std::max(2u, u32(float(Size) * Scale) & ~1u));
		};

		ViewWidth = Fit(SwapWidth, OutputWidth);
		ViewHeight = Fit(SwapHeight, OutputHeight);
	}

	// An odd side of the window leaves a line of it black rather than scale the view by a pixel
	const bool IsWhole = ViewWidth <= SwapWidth && ViewHeight <= SwapHeight && SwapWidth - ViewWidth < 2 && SwapHeight - ViewHeight < 2;
	const float Shown = IsWhole ? 1.0f : std::min(float(SwapWidth) / float(ViewWidth), float(SwapHeight) / float(ViewHeight));
	const s32 ShownWidth = std::min(s32(SwapWidth), s32(float(ViewWidth) * Shown + 0.5f));
	const s32 ShownHeight = std::min(s32(SwapHeight), s32(float(ViewHeight) * Shown + 0.5f));
	const s32 X = (s32(SwapWidth) - ShownWidth) / 2;
	const s32 Y = (s32(SwapHeight) - ShownHeight) / 2;
	PresentRect.set(X, Y, X + ShownWidth, Y + ShownHeight);

	// The render size keeps its share of the output size
	const auto Share = [](int Alloc, u32 View, u32 Whole)
	{
		const u32 Size = u32(std::ceil(float(Alloc) * float(View) / float(Whole)));
		return int(std::min(u32(Alloc), Size + (Size & 1)));
	};

	TargetWidth = ViewWidth;
	TargetHeight = ViewHeight;
	HalfTarget.set(Share(AllocHalfTarget.x, ViewWidth, OutputWidth), Share(AllocHalfTarget.y, ViewHeight, OutputHeight));
}

void CRenderDevice::on_idle		(bool IsDrawn)
{
	// The frame draws into a part of its targets, from their corner: the view, apart from the size they were made
	// with. The interface's fonts and layouts are made for a size, and making them again stops the frame for a
	// while: they follow once the view has kept its new size with the mouse released, as the level editor's view does
	if (!IsEditorMode() && AllocHalfTarget.x)
	{
		const u32 WidthBefore = TargetWidth;
		const u32 HeightBefore = TargetHeight;
		FitView();

		static u32 Rested = 0;
		Rested = TargetWidth == WidthBefore && TargetHeight == HeightBefore ? Rested + 1 : 0;

#ifdef IXR_WINDOWS
		const bool IsHeld = ImGui::GetCurrentContext() && ImGui::GetIO().MouseDown[0];
#else
		const bool IsHeld = false;
#endif

		if ((int(TargetWidth) != g_UiView.x || int(TargetHeight) != g_UiView.y) && Rested > 20 && !IsHeld && g_loading_events.empty())
		{
			g_UiView.set(int(TargetWidth), int(TargetHeight));
			g_IsViewResize = true;
			seqDeviceReset.Process<&pureDeviceReset::OnDeviceReset>();
			seqResolutionChanged.Process<&pureScreenResolutionChanged::OnScreenResolutionChanged>();
			g_IsViewResize = false;
		}

		// A window sized by its frame becomes the video mode once it rests: the targets and the interface follow it
		static u32 WindowRested = 0;
		static s32 RestedWidth = 0;
		static s32 RestedHeight = 0;
		WindowRested = Window.PixelWidth == RestedWidth && Window.PixelHeight == RestedHeight ? WindowRested + 1 : 0;
		RestedWidth = Window.PixelWidth;
		RestedHeight = Window.PixelHeight;

		const u32 WindowWidth = u32(std::max(Window.PixelWidth, 0)) & ~1u;
		const u32 WindowHeight = u32(std::max(Window.PixelHeight, 0)) & ~1u;
		const bool IsSized = WindowWidth && WindowHeight && (WindowWidth != OutputWidth || WindowHeight != OutputHeight);

		if (IsSized && !Window.IsFullscreen && !Window.IsMinimized && WindowRested > 20 && !IsHeld && g_loading_events.empty())
		{
			psCurrentVidMode[0] = WindowWidth;
			psCurrentVidMode[1] = WindowHeight;
			Reset();
		}
	}

#ifdef IXRAY_PROFILER // optick
	PROF_FRAME("CPU FRAME BEGIN");
#endif
#ifdef DEBUG_DRAW
	g_FrameProfiler.Frame();
#endif
#ifdef _WIN32
	FrameStackSampler::BeginFrame();
#endif

	PROF_EVENT("on_idle");

	if (!b_is_Ready)
	{
		Sleep(100);
		return;
	}

	const bool IsMainMenuActive = g_pGamePersistent && g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive();

	if (s_StartupActive)
	{
		const bool IsIntroActive = g_pGamePersistent && g_pGamePersistent->IsIntroActive();
		if (IsMainMenuActive || (g_pGameLevel && g_pGameLevel->bReady) || load_screen_renderer.IsActive() || IsIntroActive)
		{
			s_StartupActive = false;
		}
	}

	Platform::SetThreadName("X-Ray Primary Thread");

	Device.BeginRender();

	// The game is played in the window that shows its picture: the main one, or the Game window's own
	SDL_Window* GameWindow = CImGuiManager::Instance().GetGameWindow();
	const bool IsGameFocused = GameWindow == g_AppInfo.Window ? Window.IsFocused : SDL_GetKeyboardFocus() == GameWindow;
	const bool Focus = !s_StartupActive && !Window.IsMinimized && IsGameFocused &&
					   !CImGuiManager::Instance().IsCapturingInputs() && (psDeviceFlags.test(rsFullscreen) || !IsMainMenuActive);

	static SDL_WindowID HeldWindow = 0;
	if (HeldWindow != SDL_GetWindowID(GameWindow))
	{
		if (SDL_Window* Held = HeldWindow ? SDL_GetWindowFromID(HeldWindow) : nullptr)
		{
			SDL_SetWindowMouseGrab(Held, false);
			SDL_SetWindowRelativeMouseMode(Held, false);
		}

		HeldWindow = SDL_GetWindowID(GameWindow);
	}

	const bool IsLooking = CImGuiManager::Instance().IsMouseLook();
	const bool IsRelative = IsLooking || (Focus && !CImGuiManager::Instance().IsCapturingInputs());
	SDL_SetWindowMouseGrab(GameWindow, !g_dedicated_server && !Autotest::Active() && (Focus || IsLooking));
	SDL_SetWindowRelativeMouseMode(GameWindow, !g_dedicated_server && !Autotest::Active() && IsRelative);

	g_bEnableStatGather = psDeviceFlags.test(rsStatistic);

	if (Autotest::Active())
		Autotest::FrameBegin();

	CTaskbarProgress::Instance().Update();

	if (!g_loading_events.empty())
	{
		PROF_EVENT("LoadDraw");

		if (g_loading_events.front()())
		{
			PROF_EVENT("Loading...");
			g_loading_events.pop_front();
		}

		pApp->LoadDraw();

		return;
	}
	else 
	{
		if (g_pGamePersistent != nullptr)
		{
			PROF_EVENT("UpdatePlayDestroyParticles");
			g_pGamePersistent->UpdatePlayDestroyParticles();
		}

		if (Device.ModelDefferClear != nullptr)
		{
			PROF_EVENT("ModelDefferClear");
			Device.ModelDefferClear();
		}

		u32 tglob = Device.dwTimeGlobal;
		for (auto it = m_time_callbacks.begin(); it != m_time_callbacks.end();)
		{
			if (tglob >= it->first)
			{
				it->second();
				fast_erase(m_time_callbacks, it);
				continue;
			}

			++it;
		}

		if (GActorInterface != nullptr)
		{
			PROF_EVENT("UpdatePlayerHud");
			GActorInterface->UpdatePlayerHud();
		}

		if (BeginGraphCollect)
			BeginGraphCollect();
		PreRenderThread.Run();
		FrameMove();
	}

	if (dwPrecacheFrame!=0u)
	{
		float factor = float(dwPrecacheFrame) / float(dwPrecacheTotal);
		float angle = PI_MUL_2 * factor;
		vCameraDirection.set(std::sin(angle), 0, std::cos(angle));	vCameraDirection.normalize();
		vCameraTop.set(0, 1, 0);
		vCameraRight.crossproduct(vCameraTop, vCameraDirection);

		mView.build_camera_dir(vCameraPosition, vCameraDirection, vCameraTop);

		CalculateTransforms();
	}

	GameThread.Run();

	if (!g_dedicated_server)
	{
		Statistic->RenderTOTAL_Real.FrameStart();
		Statistic->RenderTOTAL_Real.Begin();
		if (b_is_Active && IsDrawn)
		{
			if (s_StartupActive)
			{
				DrawStartupFrame(Device.IsEditorMode());
			}
			else if (Begin())
			{
				// With r_render_thread the frame is drawn on the render thread, this one waiting (the render thread plan's
				// stage 4: the whole phase goes over, the game's code in it included)
				m_pRender->RunFrame([this]
				{
					PROF_EVENT("------ [OnRender phase] ------")
					seqRender.Process<&pureRender::OnRender>();
				}, true);

				g_MonsterLogicTelemetry.CaptureFrame(Device.fTimeDeltaContinual);
				if (IsFpsShow &&
					g_pGameLevel &&
					!IsMainMenuActive &&
					!load_screen_renderer.IsActive() &&
					!Device.Paused())
				{
					pFPSCounter->OnRender();
				}

				End();
			}
		}
	}

	PreRenderThread.Wait();
	GameThread.Wait();

	Device.EndRender();
	
#ifdef IXRAY_PROFILER_TRACY
	PROF_FRAME("idle");
#endif
	
	if (!b_is_Active || !IsDrawn)
	{
		Sleep(IsDrawn ? 1 : 10);
	}
}

void CRenderDevice::CalculateTransforms()
{
	mFullTransform.mul(mProject,mView);

	if (m_pRender)
	{
		m_pRender->SetCacheXform(mView, mProject);
	}

	mInvFullTransform.invert44(mFullTransform);
	mInv3x4FullTransform.invert(mFullTransform);

	mView_hud_old			= mView_hud;
	mProject_hud_old		= mProject_hud;
	mFullTransform_hud_old	= mFullTransform_hud;

	mView_old				= mView_saved;
	mProject_old			= mProject_saved;
	mFullTransform_old		= mFullTransform_saved;

	if (IsCameraCut())
	{
		mView_old = mView;
		mProject_old = mProject;
		mFullTransform_old = mFullTransform;
	}

	if (m_pRender)
	{
		m_pRender->SetCacheXformOld(mView_old, mProject_old);
	}

	mProject_hud.build_projection(deg2rad(psHUD_FOV), Device.fASPECT, 
		Device.fHUDViewportNear, g_pGamePersistent->Environment().CurrentEnv->far_plane);

	mView_hud.build_camera_dir(Fvector().set(0.f, 0.f, 0.f), vCameraDirection, vCameraTop);
	mFullTransform_hud.mul(mProject_hud, mView_hud);

	mFullTransform_hud_special.mul(Fmatrix().build_projection(deg2rad(psHUD_FOV), Device.fASPECT,
		Device.fViewportNear, g_pGamePersistent->Environment().CurrentEnv->far_plane), mView);
	mInv3x4FullTransform_hud_special.invert(mFullTransform_hud_special);

	mView_saved = mView;
	mProject_saved = mProject;
	mFullTransform_saved = mFullTransform;

	vCameraPosition_saved = vCameraPosition;
	vCameraDirection_saved = vCameraDirection;
	vCameraRight_saved = vCameraRight;
	vCameraTop_saved = vCameraTop;
}

volatile bool quiting = false;

// engine saves its settings and closes the shader cache cleanly.
static void device_request_quit(int)
{
	quiting = true;
}

// The renderer loads its part of a level on the load task (IGame_Level::Load) and reaches the graphics device from
// there: a frame and that load take turns. A frame that finds the load waiting or at work is not drawn (the loading
// events still run), so the load never waits for more than the frame it met
xrCriticalSection g_RenderLoadLock;
xr_atomic_u32 g_RenderLoadWaiting = 0;
bool g_IsRenderLoadFrame = false;

void CRenderDevice::message_loop()
{
	while (!quiting)
	{
		g_IsRenderLoadFrame = g_RenderLoadWaiting.load(std::memory_order_acquire) || !g_RenderLoadLock.TryEnter();

		// A minimised window is not drawn, and nothing waits for the swap chain: with no present its wait times out
		const bool IsDrawn = !Window.IsMinimized && !g_IsRenderLoadFrame;

		if (IsDrawn)
		{
			// Between frames the swap chain takes what the window and the settings ask for: its size, v-sync, queue
			if (b_is_Ready && !g_dedicated_server && !IsEditorMode())
			{
				const RHISwapChainDesc Wanted = WantedSwapChain();

				if (Wanted != GRHI->SwapChain)
				{
					m_pRender->ResizeSwapChain(Wanted);
				}
			}

			// With the frames one behind the game (r_render_thread pipelined) the last one is still drawn here: the
			// renderer begins the next, and waits for its turn, where the two meet (dxRenderDeviceRender::Begin)
			const bool IsBegunLater = VideoSettings::Effective(EVideoSetting::RenderThread) == 2 && !IsEditorMode() && !g_dedicated_server && !s_StartupActive;

			if (!IsBegunLater)
			{
				GRHI->BeginFrame();
			}

			if (PaceFrame)
			{
				PaceFrame();
			}
		}

		SDL_Event event;
		while (SDL_PollEvent(&event))
		{
			if (!on_event(event))
			{
				quiting = true;
				break;
			}
		}

		if (!quiting)
		{
			// The focus may also be in a tool window of the game's own
			const bool IsFocused = CImGuiManager::Instance().IsFocused();
			const bool IsActive = psDeviceFlags.test(rsDeviceActive) || g_dedicated_server || (IsFocused && !Window.IsMinimized);
			if (IsActive != b_is_Active)
			{
				OnWM_Activate(IsFocused, Window.IsMinimized);
			}

			on_idle(IsDrawn);
		}

		if (!g_IsRenderLoadFrame)
		{
			g_RenderLoadLock.Leave();
		}
	}
}

void CRenderDevice::Run()
{
	//	DUMP_PHASE;
	g_bLoaded = false;
	Log("Starting engine...");
	thread_name("X-Ray Primary Thread");

	// Startup timers and calculate timer delta
	dwTimeGlobal = 0;
	Timer_MM_Delta = 0;

	{
		u32 time_mm = SDL_GetTicks();
		while (SDL_GetTicks() == time_mm)
		{
			SDL_Delay(0);
		}

		u32 time_system = SDL_GetTicks();
		u32 time_local = TimerAsync();
		Timer_MM_Delta = time_system - time_local;
	}

	g_AppInfo.SetMainThread();
	std::signal(SIGTERM, device_request_quit);
	std::signal(SIGINT, device_request_quit);

	// Message cycle
	seqAppStart.Process<&pureAppStart::OnAppStart>();

	// The start screen took the window's events so far
	ReadWindowState();
	message_loop();

	seqAppEnd.Process<&pureAppEnd::OnAppEnd>();

	// Stop Balance-Threads
	PreRenderThread.Wait();
	GameThread.Wait();
	//GCThread.Wait();

	PreRenderThread.Stop();
	GameThread.Stop();
	//GCThread.Stop();

	DetailsTask.wait();
	SecondaryTasks.wait();
	async_tasks.wait();
}

u32 app_inactive_time		= 0;
u32 app_inactive_time_start = 0;
static s64 ContinualShift	= 0;
static u32 LastContinual	= 0;

struct EfficientFilteredDelta
{
	static constexpr size_t BUFFER_SIZE = 8;

	xr_array<float, BUFFER_SIZE> buffer;
	size_t current_index = 0;
	bool buffer_filled = false;

	bool median_dirty = true;
	float cached_median = 0.0f;

	EfficientFilteredDelta() { buffer.fill(0.0f); }

	void reset()
	{
		buffer.fill(0.0f);
		current_index = 0;
		buffer_filled = false;
		median_dirty = true;
		cached_median = 0.0f;
	}

	void CalculateSmoothedDelta(float& current_delta)
	{
		buffer[current_index] = current_delta;
		current_index = (current_index + 1) % BUFFER_SIZE;

		if (current_index == 0)
			buffer_filled = true;

		median_dirty = true;

		current_delta = getFilteredAverage();
	}

	float getMedian()
	{
		if (!median_dirty) return cached_median;

		size_t count = buffer_filled ? BUFFER_SIZE : current_index;
		if (count == 0) return 0.0f;

		if (!buffer_filled)
			std::sort(buffer.begin(), buffer.begin() + count);
		else
			std::sort(buffer.begin(), buffer.end());

		if (count % 2 == 0)
			cached_median = (buffer[count / 2 - 1] + buffer[count / 2]) / 2.0f;
		else
			cached_median = buffer[count / 2];

		median_dirty = false;
		return cached_median;
	}

	float getFilteredAverage()
	{
		size_t count = buffer_filled ? BUFFER_SIZE : current_index;
		if (count == 0) return 0.0f;

		float median = getMedian();
		float sum = 0.0f;
		size_t valid_count = 0;

		float threshold = calculateDynamicThreshold(median);

		for (size_t i = 0; i < count; ++i)
		{
			if (buffer[i] <= threshold)
			{
				sum += buffer[i];
				valid_count++;
			}
		}

		if (valid_count > 0)
			return sum / valid_count;
		else
			return median;
	}

	float calculateDynamicThreshold(float median)
	{
		size_t count = buffer_filled ? BUFFER_SIZE : current_index;
		if (count < 5) return median * 2.0f;

		xr_vector<float> deviations;
		deviations.reserve(count);

		for (size_t i = 0; i < count; ++i)
			deviations.push_back(std::abs(buffer[i] - median));

		std::sort(deviations.begin(), deviations.end());
		float mad = deviations[count / 2];

		return median + 3.0f * mad;
	}
} delta_filter;

bool use_smoothed_delta = false;
void CRenderDevice::FrameMove()
{
	dwFrame++;

	float dt = 0;
	
	const bool IsFixed = FixedDelta > 0.0f;
	if (!Paused())
	{
		dt = IsFixed ? FixedDelta : Timer.GetElapsed_sec();
		Timer.Start();
	}
	else
	{
		fTimeDelta = 0.0f;
		fTimeDeltaSmoothing = 0.0f;
		dt = fTimeDelta;
	}
	
	float prev_dt = fTimeDelta;
	fTimeDeltaContinual = dt * (1.f/time_factor());
	constexpr float a = .1f;
	fTimeDelta = IsFixed ? dt : a * dt + (1.f - a) * prev_dt;
	fTimeDeltaSmoothing = fTimeDelta;

	if (use_smoothed_delta && !IsFixed)
	{
		delta_filter.CalculateSmoothedDelta(fTimeDeltaSmoothing);
		fTimeDelta = fTimeDeltaSmoothing;
	}
	
	// don't touch! some hud & graphics artefacts can be without clamp.
	clamp(fTimeDelta, EPS_S, .1f);
	clamp(fTimeDeltaSmoothing, EPS_S, .1f);

	const u64 RealGlobal = TimerGlobal.GetElapsed_mcs();
	if (IsFixed && !Paused())
	{
		GlobalShift.fetch_add(static_cast<s64>(dt * 1000000.0f) - (static_cast<s64>(RealGlobal) - static_cast<s64>(LastGlobal)), std::memory_order_relaxed);
	}
	LastGlobal = RealGlobal;
	const s64 Global = std::max<s64>(static_cast<s64>(RealGlobal) + GlobalShift.load(std::memory_order_relaxed), 0);
	fTimeGlobal = static_cast<float>(static_cast<double>(Global) / 1000000.0);
	u32 _old_global = dwTimeGlobal;
	dwTimeGlobal = static_cast<u32>(Global / 1000);
	dwTimeDelta = dwTimeGlobal - _old_global;

	const u32 RealContinual = TimerMM.GetElapsed_ms() - app_inactive_time;
	ContinualShift += IsFixed && !Paused() ? s64(dwTimeDelta) - s64(RealContinual - LastContinual) : 0;
	LastContinual = RealContinual;
	dwTimeContinual = u32(s64(RealContinual) + ContinualShift);

	Statistic->EngineTOTAL.Begin();
	{
		{
			PROF_EVENT("------ [OnFrame phase] ------")
		   Device.seqFrame.Process<&pureFrame::OnFrame>();
		}
		g_bLoaded = true;
	}
	Statistic->EngineTOTAL.End();
}

void CRenderDevice::AnchorTime(u64 Mcs)
{
	GlobalShift.fetch_add(static_cast<s64>(Mcs) - static_cast<s64>(LastGlobal) - GlobalShift.load(std::memory_order_relaxed), std::memory_order_relaxed);
	ContinualShift += static_cast<s64>(Mcs / 1000) - static_cast<s64>(dwTimeContinual);
	fTimeGlobal = static_cast<float>(static_cast<double>(Mcs) / 1000000.0);
	dwTimeGlobal = dwTimeContinual = static_cast<u32>(Mcs / 1000);
}

CRenderDevice::CRenderDevice() :
	dwPrecacheTotal(0), m_pRender(nullptr), Statistic(nullptr), 
	//GCThread(XRay::Engine::GCThread, "X-Ray GC Thread"),
	PreRenderThread(XRay::Engine::PreRenderThread, "X-Ray PreRender Thread", EThreadRole::PreRender),
	GameThread(XRay::Engine::GameThread, "X-Ray Game Thread", EThreadRole::Game)
{
	b_is_Active = true;
	b_is_Ready = false;
	Timer.Start();
	m_bNearer = false;
}

ENGINE_API bool bShowPauseString = true;
void CRenderDevice::Pause(bool bOn, bool bTimer, bool bSound, const char* reason)
{
	static int snd_emitters_ = -1;

	if (g_dedicated_server)
		return;

	if (bOn)
	{
		if (!Paused())
			bShowPauseString = true;

		if (bTimer && (!g_pGamePersistent || g_pGamePersistent->CanBePaused()))
		{
			g_pauseMngr.Pause(true);
	}

		if (bSound && ::Sound)
	{
			snd_emitters_ =					::Sound->pause_emitters(true);
		}
	}
	else
	{
		if (bTimer && g_pauseMngr.Paused())
		{
			fTimeDelta						= EPS_S + EPS_S;
			g_pauseMngr.Pause(false);
		}
		
		if (bSound)
		{
			if (snd_emitters_ > 0)
			{
				snd_emitters_ =				::Sound->pause_emitters(false);
			}
		}
	}
}

bool CRenderDevice::Paused()
{
	return g_pauseMngr.Paused();
};

void CRenderDevice::OnWM_Activate(bool active, bool minimized)
{
	bool NewState = (active && (!minimized)) ? true : false;
	bool OldState = Device.b_is_Active;

	Device.b_is_Active = psDeviceFlags.test(rsDeviceActive) || NewState;

	if (Device.b_is_Active && !OldState)
	{
		Device.seqAppActivate.Process<&pureAppActivate::OnAppActivate>();
		app_inactive_time += TimerMM.GetElapsed_ms() - app_inactive_time_start;

		if (g_dedicated_server)
		{
			SDL_ShowCursor();
		}
	}
	else if (!psDeviceFlags.test(rsDeviceActive))
	{
		app_inactive_time_start = TimerMM.GetElapsed_ms();
		Device.seqAppDeactivate.Process<&pureAppDeactivate::OnAppDeactivate>();
		SDL_ShowCursor();
	}
	else
	{
		if (NewState && !g_dedicated_server) 
		{
			SDL_HideCursor();
		} 
		else 
		{
			SDL_ShowCursor();
		}
	}
}

void CRenderDevice::AddSeqFrame(pureFrame* f, bool mt)
{
	if (mt)
		seqFrameMT.Add(f, REG_PRIORITY_HIGH);
	else
		seqFrame.Add(f, REG_PRIORITY_LOW);

}

void CRenderDevice::RemoveSeqFrame(pureFrame* f)
{
	seqFrameMT.Remove(f);
	seqFrame.Remove(f);
}

CLoadScreenRenderer::CLoadScreenRenderer()
:b_registered(false)
{}

void CLoadScreenRenderer::start(bool b_user_input) 
{
	Device.seqRender.Add			(this, UI_LOAD_SCREEN);
	b_registered					= true;
	b_need_user_input				= b_user_input;
}

void CLoadScreenRenderer::stop()
{
	if(!b_registered)				return;
	Device.seqRender.Remove			(this);
	pApp->DestroyLoadingScreen();
	b_registered					= false;
	b_need_user_input				= false;
}

void CLoadScreenRenderer::OnRender() 
{
	pApp->load_draw_internal();
}
