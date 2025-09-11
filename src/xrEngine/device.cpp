#include "stdafx.h"

using namespace DirectX;

#include "../xrCore/Collision/Frustum.h"

#pragma warning(disable:4995)
// mmsystem.h
#define MMNOSOUND
#define MMNOMIDI
#define MMNOAUX
#define MMNOMIXER
#define MMNOJOY
#include <mmsystem.h>
#pragma warning(default:4995)

#include "x_ray.h"
#include "Render.h"
#include "EngineThreading.h"
#include "IGame_Level.h"

#include "../xrCore/FS_impl.h"
#include "IGame_Persistent.h"

ENGINE_API CRenderDevice* DevicePtr = nullptr;
ENGINE_API CLoadScreenRenderer load_screen_renderer;
ENGINE_API CTimer loading_save_timer;
ENGINE_API bool loading_save_timer_started = false;
ENGINE_API xr_atomic_bool g_bRendering = false;
extern ENGINE_API float psHUD_FOV;

BOOL g_bLoaded = FALSE;
ref_light precache_light = 0;

BOOL CRenderDevice::Begin()
{
	PROF_EVENT("Render: Begin");

	if (g_dedicated_server)
	{
		return TRUE;
	}

	switch (m_pRender->GetDeviceState())
	{
	case IRenderDeviceRender::dsOK:
		break;

	case IRenderDeviceRender::dsLost:
		// If the device was lost, do not render until we get it back
		Sleep(33);
		return FALSE;
		break;

	case IRenderDeviceRender::dsNeedReset:
		// Check if the device is ready to be reset
		Reset();
		break;

	default:
		R_ASSERT(0);
	}

	m_pRender->Begin();

	FPU::m24r();
	g_bRendering = true;

	return TRUE;
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
int g_dwFPSlimit = 500;
void CRenderDevice::time_factor(const float &time_factor)
{
	Timer.time_factor		(time_factor);
	TimerGlobal.time_factor	(time_factor);
	Sound->time_factor		(time_factor);
}

void CRenderDevice::callback(const u32& cb_time, const std::function<void()> &func)
{
	m_time_callbacks.insert({dwTimeGlobal+cb_time,func});
}

void CRenderDevice::on_idle		()
{
	if (!b_is_Ready) {
		Sleep(100);
		return;
	}

	// FPS Limit
	if (g_dwFPSlimit > 0)
	{
		static DWORD dwLastFrameTime = 0;
		int dwCurrentTime = static_cast<int>(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now().time_since_epoch()).count());

		int selected_time = (dwCurrentTime - (int)dwLastFrameTime);
		if (selected_time >= 0 && selected_time < (1000 / g_dwFPSlimit))
			return;
		dwLastFrameTime = dwCurrentTime;
	}



	PROF_FRAME("Main Thread");
	Platform::SetThreadName("X-Ray Primary Thread");

	Device.BeginRender();
	const bool Minimized = SDL_GetWindowFlags(g_AppInfo.Window) & SDL_WINDOW_MINIMIZED;
	const bool Focus = !Minimized && !(g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive()) && !CImGuiManager::Instance().IsCapturingInputs();

	SDL_SetWindowMouseGrab(g_AppInfo.Window, !g_dedicated_server && Focus);
	SDL_SetWindowRelativeMouseMode(g_AppInfo.Window, !g_dedicated_server && Focus);

	g_bEnableStatGather = psDeviceFlags.test(rsStatistic);

	if (g_loading_events.size())
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
		{
			PROF_EVENT("Update Particles");
			if (g_pGamePersistent)
				g_pGamePersistent->UpdateParticles();

			if (Device.ModelDefferClear)
			{
				Device.ModelDefferClear();
			}
		}

		for (auto it = m_time_callbacks.begin(); it != m_time_callbacks.end();)
		{
		    if (Device.dwTimeGlobal >= it->first)
			{
				it->second();
		        it = m_time_callbacks.erase(it);
		    }
			else
		       ++it;
		}

		{
			PROF_EVENT("seqParallelBeforRender");
			for (auto& it : Device.seqParallelBeforRender)
				it();

			Device.seqParallelBeforRender.clear();
		}

		secondary_tasks.run(&XRay::Engine::PreRenderThread);
		FrameMove();
	}

	// Precache
	if (dwPrecacheFrame)
	{
		float factor					= float(dwPrecacheFrame) / float(dwPrecacheTotal);
		float angle						= PI_MUL_2 * factor;
		vCameraDirection.set			(_sin(angle),0,_cos(angle));	vCameraDirection.normalize	();
		vCameraTop.set					(0,1,0);
		vCameraRight.crossproduct		(vCameraTop,vCameraDirection);

		mView.build_camera_dir			(vCameraPosition,vCameraDirection,vCameraTop);
	}

	// Matrices
	mFullTransform.mul			( mProject,mView	);
	m_pRender->SetCacheXform(mView, mProject);

	mInvFullTransform.invert44(mFullTransform);
	
	mView_hud_old			= mView_hud;
	mProject_hud_old		= mProject_hud;
	mFullTransform_hud_old	= mFullTransform_hud;

	mView_old				= mView_saved;
	mProject_old			= mProject_saved;
	mFullTransform_old		= mFullTransform_saved;

	m_pRender->SetCacheXformOld(mView_old, mProject_old);

	mProject_hud.build_projection(deg2rad(psHUD_FOV), Device.fASPECT, 
		Device.fHUDViewportNear, g_pGamePersistent->Environment().CurrentEnv->far_plane);

	mView_hud.set(mView);
	mFullTransform_hud.mul(mProject_hud, mView_hud);

	mView_saved				= mView;
	mProject_saved			= mProject;
	mFullTransform_saved	= mFullTransform;

	vCameraPosition_saved	= vCameraPosition;

	// *** Resume threads
	// Capture end point - thread must run only ONE cycle
	// Release start point - allow thread to run

	secondary_tasks.run(&XRay::Engine::GameThread);

	if (!g_dedicated_server)
	{
		Statistic->RenderTOTAL_Real.FrameStart();
		Statistic->RenderTOTAL_Real.Begin();
		if (b_is_Active)
		{
			if (Begin())
			{
				seqRender.Process(rp_Render);
				if (psDeviceFlags.test(rsCameraPos) || psDeviceFlags.test(rsStatistic) || Statistic->errors.size())
					Statistic->Show();

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

bool quiting = false;

void CRenderDevice::message_loop()
{
	while (!quiting)
	{
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
	g_bLoaded = FALSE;
	Log("Starting engine...");
	thread_name("X-Ray Primary Thread");

	// Startup timers and calculate timer delta
	dwTimeGlobal = 0;
	Timer_MM_Delta = 0;
	{
		u32 time_mm = timeGetTime();
		while (timeGetTime() == time_mm);			// wait for next tick
		u32 time_system = timeGetTime();
		u32 time_local = TimerAsync();
		Timer_MM_Delta = time_system - time_local;
	}

	g_AppInfo.MainThread = GetCurrentThread();
	// Message cycle
	seqAppStart.Process(rp_AppStart);

	m_pRender->ClearTarget();
	message_loop();

	seqAppEnd.Process(rp_AppEnd);

	// Stop Balance-Threads
	secondary_tasks.wait();
	details_task.wait();
	ParticleWorkerCallback = nullptr;
}

u32 app_inactive_time		= 0;
u32 app_inactive_time_start = 0;

void ProcessLoading();
void CRenderDevice::FrameMove()
{
	PROF_EVENT("Render: Frame Move");
	dwFrame			++;
	dwTimeContinual	= TimerMM.GetElapsed_ms() - app_inactive_time;

	if (psDeviceFlags.test(rsConstantFPS))	{
		fTimeDelta		=	0.033f;			
		fTimeGlobal		+=	0.033f;
		dwTimeDelta		=	33;
		dwTimeGlobal	+=	33;
	} else {
		float fPreviousFrameTime = Timer.GetElapsed_sec(); Timer.Start();	// previous frame
		fTimeDelta = 0.1f * fTimeDelta + 0.9f*fPreviousFrameTime;			// smooth random system activity - worst case ~7% error

		if (fTimeDelta>.1f)    
			fTimeDelta = .1f;							// limit to 15fps minimum

		if (fTimeDelta <= 0.f) 
			fTimeDelta = EPS_S + EPS_S;					// limit to 15fps minimum

		if (Paused()) {
			fTimeDelta = 0.0f;
		}

		fTimeGlobal		= TimerGlobal.GetElapsed_sec();
		u32	_old_global	= dwTimeGlobal;
		dwTimeGlobal = TimerGlobal.GetElapsed_ms();
		dwTimeDelta		= dwTimeGlobal-_old_global;
	}

	Statistic->EngineTOTAL.Begin();
	ProcessLoading();
	Statistic->EngineTOTAL.End();
}

void ProcessLoading()
{
	Device.seqFrame.Process(rp_Frame);
	g_bLoaded = TRUE;
}

ENGINE_API BOOL bShowPauseString = TRUE;

CRenderDevice::CRenderDevice() :
	m_pRender(0)
{
	b_is_Active = true;
	b_is_Ready = FALSE;
	Timer.Start();
	m_bNearer = FALSE;
};

void CRenderDevice::Pause(BOOL bOn, BOOL bTimer, BOOL bSound, LPCSTR reason)
{
	static int snd_emitters_ = -1;

	if (g_dedicated_server)
		return;

	if (bOn)
	{
		if (!Paused())
			bShowPauseString = TRUE;

		if (bTimer && (!g_pGamePersistent || g_pGamePersistent->CanBePaused()))
		{
			g_pauseMngr.Pause(TRUE);
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
			g_pauseMngr.Pause(FALSE);
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

BOOL CRenderDevice::Paused()
{
	return g_pauseMngr.Paused();
};

void CRenderDevice::OnWM_Activate(bool active, bool minimized)
{
	BOOL NewState = (active && (!minimized)) ? TRUE : FALSE;
	bool OldState = Device.b_is_Active;

	Device.b_is_Active = psDeviceFlags.test(rsDeviceActive) || NewState;

	if (Device.b_is_Active && !OldState)
	{
		Device.seqAppActivate.Process(rp_AppActivate);
		app_inactive_time += TimerMM.GetElapsed_ms() - app_inactive_time_start;

		if (g_dedicated_server)
		{
			SDL_ShowCursor();
		}
	}
	else if (!psDeviceFlags.test(rsDeviceActive))
	{
		app_inactive_time_start = TimerMM.GetElapsed_ms();
		Device.seqAppDeactivate.Process(rp_AppDeactivate);
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
