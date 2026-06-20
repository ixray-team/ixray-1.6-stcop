#include "stdafx.h"
#include "../xrCore/Collision/Frustum.h"

#include "x_ray.h"
#include "Render.h"
#include "EngineThreading.h"
#include "FPSCounter.h"
#include "IGame_Level.h"

#include "../xrCore/FS_impl.h"
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
int fps_limit, main_menu_fps_limit = 144;

void CRenderDevice::time_factor(const float &time_factor)
{
	Timer.time_factor		(time_factor);
	TimerGlobal.time_factor	(time_factor);
	Sound->time_factor		(time_factor);
}

void CRenderDevice::on_idle		()
{
	if (!b_is_Ready) {
		Sleep(100);
		return;
	}
	
	bool main_menu_active = g_pGamePersistent						   &&
							g_pGamePersistent->m_pMainMenu			   &&
							g_pGamePersistent->m_pMainMenu->IsActive();

	PROF_FRAME("Main Thread");
	Platform::SetThreadName("X-Ray Primary Thread");

	Device.BeginRender();
	const bool Minimized = SDL_GetWindowFlags(g_AppInfo.Window) & SDL_WINDOW_MINIMIZED;
	const bool Focus = psDeviceFlags.test(rsFullscreen) || (!Minimized && !main_menu_active && !CImGuiManager::Instance().IsCapturingInputs());

	SDL_SetWindowMouseGrab(g_AppInfo.Window, !g_dedicated_server && Focus);
	SDL_SetWindowRelativeMouseMode(g_AppInfo.Window, !g_dedicated_server && Focus);

	g_bEnableStatGather = psDeviceFlags.test(rsStatistic);

	if (!g_loading_events.empty())
	{
		{
			PROF_EVENT("Loading...");
			if (g_loading_events.front()())
				g_loading_events.pop_front();
		}
		PROF_EVENT("LoadDraw");
		pApp->LoadDraw();

		return;
	}
	else 
	{
		if (g_pGamePersistent)
			g_pGamePersistent->UpdatePlayDestroyParticles();

		if (Device.ModelDefferClear)
			Device.ModelDefferClear();

		u32 tglob = Device.dwTimeGlobal;
		for (auto it = m_time_callbacks.begin(); it != m_time_callbacks.end();)
		{
		    if (tglob >= it->first)
			{
				it->second();
				fast_erase(m_time_callbacks, it);
		    }
			else
		       ++it;
		}

		if (GActorInterface != nullptr)
		{
			GActorInterface->UpdatePlayerHud();
		}

		secondary_tasks.run(&XRay::Engine::PreRenderThread);
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

	secondary_tasks.run(&XRay::Engine::GameThread);

	if (!g_dedicated_server)
	{
		Statistic->RenderTOTAL_Real.FrameStart();
		Statistic->RenderTOTAL_Real.Begin();
		if (b_is_Active)
		{
			if (Begin())
			{
				seqRender.Process<&pureRender::OnRender>();
				
				if (psDeviceFlags.test(rsCameraPos) || psDeviceFlags.test(rsStatistic) || Statistic->errors.size())
					Statistic->Show();


				if (IsFpsShow && g_pGameLevel && !main_menu_active && !load_screen_renderer.IsActive())
					pFPSCounter->OnRender();

				End();
			}
		}
		Statistic->RenderTOTAL_Real.End();
		Statistic->RenderTOTAL_Real.FrameEnd();
		Statistic->RenderTOTAL.accum = Statistic->RenderTOTAL_Real.accum;
	}
	secondary_tasks.wait();

	Device.EndRender();
	if (!b_is_Active)
	{
		Sleep(1);
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

	if (m_pRender)
	{
		m_pRender->SetCacheXformOld(mView_old, mProject_old);
	}

	mProject_hud.build_projection(deg2rad(psHUD_FOV), Device.fASPECT, 
		Device.fHUDViewportNear, g_pGamePersistent->Environment().CurrentEnv->far_plane);

	mView_hud.set(mView);
	mFullTransform_hud.mul(mProject_hud, mView_hud);

	mFullTransform_hud_special.mul(Fmatrix().build_projection(deg2rad(psHUD_FOV), Device.fASPECT,
		Device.fViewportNear, g_pGamePersistent->Environment().CurrentEnv->far_plane), mView_hud);
	mInv3x4FullTransform_hud_special.invert(mFullTransform_hud_special);

	mView_saved = mView;
	mProject_saved = mProject;
	mFullTransform_saved = mFullTransform;

	vCameraPosition_saved = vCameraPosition;
	vCameraDirection_saved = vCameraDirection;
	vCameraRight_saved = vCameraRight;
	vCameraTop_saved = vCameraTop;
}

bool quiting = false;

void CRenderDevice::message_loop()
{
	while (!quiting)
	{
		GRHI->BeginFrame();

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
			on_idle();
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

	g_AppInfo.MainThread = Platform::GetCurrentThread();
	// Message cycle
	seqAppStart.Process<&pureAppStart::OnAppStart>();

	m_pRender->ClearTarget();
	message_loop();

	seqAppEnd.Process<&pureAppEnd::OnAppEnd>();

	// Stop Balance-Threads
	secondary_tasks.wait();
	details_task.wait();
}

u32 app_inactive_time		= 0;
u32 app_inactive_time_start = 0;

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
	PROF_EVENT("Render: Frame Move");
	dwFrame++;
	dwTimeContinual	= TimerMM.GetElapsed_ms() - app_inactive_time;
	
	float smoothing_alpha = .1f; 
	float current_delta	= Timer.GetElapsed_sec(); Timer.Start();
	float previous_delta = fTimeDelta;

	fRealTimeDelta = current_delta;
	fTimeDelta = smoothing_alpha * current_delta + (1.f - smoothing_alpha) * previous_delta; 
	
	clamp(fTimeDelta, EPS_S, .1f);
	
	fTimeDeltaSmoothing = fTimeDelta;
	
	if (!Paused())
		delta_filter.CalculateSmoothedDelta(fTimeDeltaSmoothing);
	
	clamp(fTimeDeltaSmoothing, EPS_S, .1f);

	if (use_smoothed_delta)
		fTimeDelta = fTimeDeltaSmoothing;

	if (Paused())
	{
		fTimeDelta = 0.0f;
		fTimeDeltaSmoothing = 0.0f;
	}

	fTimeGlobal = TimerGlobal.GetElapsed_sec();
	u32 _old_global = dwTimeGlobal;
	dwTimeGlobal = TimerGlobal.GetElapsed_ms();
	dwTimeDelta = dwTimeGlobal - _old_global;
	
	Statistic->EngineTOTAL.Begin();
	Device.seqFrame.Process<&pureFrame::OnFrame>();
	g_bLoaded = true;
	Statistic->EngineTOTAL.End();
}

CRenderDevice::CRenderDevice() : dwPrecacheTotal(0), m_pRender(nullptr), Statistic(nullptr)
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
	Device.seqRender.Add			(this, 0);
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
