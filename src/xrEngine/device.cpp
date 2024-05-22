#include "stdafx.h"

using namespace DirectX;

#include "../xrCDB/frustum.h"
#include "../xrCore/discord/discord.h"

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
#include "render.h"

// must be defined before include of FS_impl.h
#define INCLUDE_FROM_ENGINE
#include "../xrCore/FS_impl.h"
#include "igame_persistent.h"
#include "FPSCounter.h"

ENGINE_API CRenderDevice* DevicePtr = nullptr;
#ifndef _EDITOR
ENGINE_API CLoadScreenRenderer load_screen_renderer;
#endif
ENGINE_API CTimer loading_save_timer;
ENGINE_API bool loading_save_timer_started = false;
ENGINE_API BOOL g_bRendering = FALSE;
extern ENGINE_API float psHUD_FOV;

BOOL		g_bLoaded = FALSE;
ref_light	precache_light = 0;

BOOL CRenderDevice::Begin()
{
#ifndef _EDITOR
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
	g_bRendering = TRUE;
#endif

	return TRUE;
}

void CRenderDevice::Clear	()
{
#ifndef _EDITOR
	m_pRender->Clear();
#endif
}

void CRenderDevice::End		(void)
{
#ifndef _EDITOR
	if (g_dedicated_server) {
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
			Msg												("* MEMORY USAGE: %d K",Memory.mem_usage()/1024);
			Msg												("* End of synchronization A[%d] R[%d]",b_is_Active, b_is_Ready);
			if (loading_save_timer_started) {
				Msg("* Game Loading Timer: Finished for %d ms", loading_save_timer.GetElapsed_ms());
				loading_save_timer_started = false;
			}

#ifdef FIND_CHUNK_BENCHMARK_ENABLE
			g_find_chunk_counter.flush();
#endif
		}
	}

	g_bRendering		= FALSE;
	// end scene

	m_pRender->End();
#endif
}

#ifndef _EDITOR
volatile u32 mt_Thread_marker = 0x12345678;
void mt_Thread(void* ptr)
{
	g_AppInfo.SecondaryThread = GetCurrentThread();
	while (true)
	{
		// waiting for Device permission to execute
		Device.mt_csEnter.Enter();

		if (Device.mt_bMustExit)
		{
			Device.mt_bMustExit = FALSE;				// Important!!!
			Device.mt_csEnter.Leave();					// Important!!!
			return;
		}

		// we has granted permission to execute
		mt_Thread_marker = Device.dwFrame;

		g_Discord.Update();

		for (u32 pit = 0; pit < Device.seqParallel.size(); pit++)
			Device.seqParallel[pit]();

		Device.seqParallel.clear();
		Device.seqFrameMT.Process(rp_Frame);

		// now we give control to device - signals that we are ended our work
		Device.mt_csEnter.Leave();
		// waits for device signal to continue - to start again
		Device.mt_csLeave.Enter();
		// returns sync signal to device
		Device.mt_csLeave.Leave();
	}
}

#include "igame_level.h"
#endif
void CRenderDevice::PreCache	(u32 amount, bool b_draw_loadscreen, bool b_wait_user_input)
{
#ifndef _EDITOR
	if (m_pRender->GetForceGPU_REF() || g_dedicated_server) {
		amount = 0;
	}

	dwPrecacheFrame = dwPrecacheTotal = amount;

	if (amount && !precache_light && g_pGameLevel && g_loading_events.empty()) {
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
#endif
}


int g_svDedicateServerUpdateReate = 100;

ENGINE_API xr_list<LOADING_EVENT>			g_loading_events;
int g_dwFPSlimit = 500;
bool IsFpsShow = true;

void CRenderDevice::on_idle		()
{
#ifndef _EDITOR
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


	Device.BeginRender();
	const bool Minimized = SDL_GetWindowFlags(g_AppInfo.Window) & SDL_WINDOW_MINIMIZED;
	const bool Focus = !Minimized && !(g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive()) && !CImGuiManager::Instance().IsCapturingInputs();
	SDL_SetWindowGrab(g_AppInfo.Window, Focus);
	SDL_SetRelativeMouseMode(Focus);

	u32 FrameStartTime = TimerGlobal.GetElapsed_ms();

	g_bEnableStatGather = psDeviceFlags.test(rsStatistic);

	if (g_loading_events.size())
	{
		if (g_loading_events.front()())
			g_loading_events.pop_front();

		pApp->LoadDraw();
		return;
	}
	else 
	{
		if (g_pGamePersistent != nullptr)
		{
			g_pGamePersistent->UpdateParticles();
		}
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

	XMStoreFloat4x4(reinterpret_cast<XMFLOAT4X4*>(&mInvFullTransform),
		XMMatrixInverse(nullptr, XMLoadFloat4x4(reinterpret_cast<XMFLOAT4X4*>(&mFullTransform))));
	
	mView_hud_old			= mView_hud;
	mProject_hud_old		= mProject_hud;
	mFullTransform_hud_old	= mFullTransform_hud;

	mView_old				= mView_saved;
	mProject_old			= mProject_saved;
	mFullTransform_old		= mFullTransform_saved;

	m_pRender->SetCacheXformOld(mView_old, mProject_old);

	mProject_hud.build_projection(deg2rad(psHUD_FOV), Device.fASPECT, 
		HUD_VIEWPORT_NEAR, g_pGamePersistent->Environment().CurrentEnv->far_plane);

	mView_hud.set(mView);
	mFullTransform_hud.mul(mProject_hud, mView_hud);

	mView_saved				= mView;
	mProject_saved			= mProject;
	mFullTransform_saved	= mFullTransform;

	vCameraPosition_saved	= vCameraPosition;

	// *** Resume threads
	// Capture end point - thread must run only ONE cycle
	// Release start point - allow thread to run
	mt_csLeave.Enter			();
	mt_csEnter.Leave			();
	Sleep						(0);

	if (!g_dedicated_server) {
		Statistic->RenderTOTAL_Real.FrameStart();
		Statistic->RenderTOTAL_Real.Begin();
		if (b_is_Active) {
			if (Begin()) {
				seqRender.Process(rp_Render);
				if (psDeviceFlags.test(rsCameraPos) || psDeviceFlags.test(rsStatistic) || Statistic->errors.size())
					Statistic->Show();

				if (IsFpsShow) {
					pFPSCounter->OnRender();
				}

				End();
			}
		}
		Statistic->RenderTOTAL_Real.End();
		Statistic->RenderTOTAL_Real.FrameEnd();
		Statistic->RenderTOTAL.accum = Statistic->RenderTOTAL_Real.accum;
	}

	// *** Suspend threads
	// Capture startup point
	// Release end point - allow thread to wait for startup point
	mt_csEnter.Enter						();
	mt_csLeave.Leave						();

	// Ensure, that second thread gets chance to execute anyway
	if (dwFrame!=mt_Thread_marker)			{
		for (u32 pit=0; pit<Device.seqParallel.size(); pit++)
			Device.seqParallel[pit]			();
		Device.seqParallel.clear();
		seqFrameMT.Process					(rp_Frame);
	}

	Device.EndRender();
	if (!b_is_Active)
	{
		Sleep(1);
	}
#endif
}

bool quiting = false;

void CRenderDevice::message_loop()
{
#ifndef _EDITOR
	while (!quiting) {
		SDL_Event event;
		while (SDL_PollEvent(&event)) {
			if (!on_event(event)) {
				quiting = true;
				break;
			}
		}

		if (quiting) {
			break;
		}

		on_idle();
	}
#endif
}

void CRenderDevice::Run()
{
#ifndef _EDITOR
	//	DUMP_PHASE;
	g_bLoaded = FALSE;
	Log("Starting engine...");
	thread_name("X-RAY Primary thread");

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

	// Start all threads
	mt_csEnter.Enter();
	mt_bMustExit = FALSE;

	g_AppInfo.MainThread = GetCurrentThread();
	thread_spawn(mt_Thread, "X-RAY Secondary thread", 0, 0);

	// Message cycle
	seqAppStart.Process(rp_AppStart);

	m_pRender->ClearTarget();
	message_loop();

	seqAppEnd.Process(rp_AppEnd);

	// Stop Balance-Thread
	mt_bMustExit = TRUE;
	mt_csEnter.Leave();
	while (mt_bMustExit)	Sleep(0);
#endif
}

u32 app_inactive_time		= 0;
u32 app_inactive_time_start = 0;

void ProcessLoading(RP_FUNC *f);
void CRenderDevice::FrameMove()
{
#ifndef _EDITOR
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

	Statistic->EngineTOTAL.Begin	();
	ProcessLoading				(rp_Frame);
	Statistic->EngineTOTAL.End	();
#endif
}

void ProcessLoading				(RP_FUNC *f)
{
	Device.seqFrame.Process				(rp_Frame);
	g_bLoaded							= TRUE;
}

ENGINE_API BOOL bShowPauseString = TRUE;
#include "IGame_Persistent.h"

CRenderDevice::CRenderDevice() :
	m_pRender(0)
{
#ifndef _EDITOR
	b_is_Active = true;
	b_is_Ready = FALSE;
	Timer.Start();
	m_bNearer = FALSE;
#endif
};

void CRenderDevice::Pause(BOOL bOn, BOOL bTimer, BOOL bSound, LPCSTR reason)
{
#ifndef _EDITOR
	static int snd_emitters_ = -1;

	if (g_dedicated_server) {
		return;
	}

	if(bOn)
	{
		if(!Paused())						
			bShowPauseString				= 
#ifdef DEBUG
				!xr_strcmp(reason, "li_pause_key_no_clip")?	FALSE:
#endif // DEBUG
				TRUE;

		if( bTimer && (!g_pGamePersistent || g_pGamePersistent->CanBePaused()) )
		{
			g_pauseMngr.Pause				(TRUE);
#ifdef DEBUG
			if(!xr_strcmp(reason, "li_pause_key_no_clip"))
				TimerGlobal.Pause				(FALSE);
#endif // DEBUG
		}

		if (bSound && ::Sound) {
			snd_emitters_ =					::Sound->pause_emitters(true);
#ifdef DEBUG
//			Log("snd_emitters_[true]",snd_emitters_);
#endif // DEBUG
		}
	}else
	{
		if (bTimer && g_pauseMngr.Paused())
		{
			fTimeDelta						= EPS_S + EPS_S;
			g_pauseMngr.Pause				(FALSE);
		}
		
		if(bSound)
		{
			if(snd_emitters_>0) //avoid crash
			{
				snd_emitters_ =				::Sound->pause_emitters(false);
			} else {
#ifdef DEBUG
				Log("Sound->pause_emitters underflow");
#endif // DEBUG
			}
		}
	}
#endif
}

BOOL CRenderDevice::Paused()
{
	return g_pauseMngr.Paused();
};

void CRenderDevice::OnWM_Activate(bool active, bool minimized)
{
#ifndef _EDITOR
	BOOL NewState = (active && (!minimized)) ? TRUE : FALSE;
	bool OldState = Device.b_is_Active;

	Device.b_is_Active = psDeviceFlags.test(rsDeviceActive) || NewState;

	if (Device.b_is_Active && !OldState)
	{
		Device.seqAppActivate.Process(rp_AppActivate);
		app_inactive_time += TimerMM.GetElapsed_ms() - app_inactive_time_start;
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
#endif
}

void CRenderDevice::AddSeqFrame(pureFrame* f, bool mt)
{
	if (mt)
		seqFrameMT.Add(f, REG_PRIORITY_HIGH);
	else
		seqFrame.Add(f, REG_PRIORITY_LOW);

}

void	CRenderDevice::RemoveSeqFrame	( pureFrame* f )
{
	seqFrameMT.Remove	( f );
	seqFrame.Remove		( f );
}

#ifndef _EDITOR
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

void CLoadScreenRenderer::OnRender() { pApp->load_draw_internal(); }

void CRenderDevice::time_factor(const float& time_factor) {
	Timer.time_factor(time_factor);
	TimerGlobal.time_factor(time_factor);
	psSoundTimeFactor = time_factor; //--#SM+#--
}
#endif