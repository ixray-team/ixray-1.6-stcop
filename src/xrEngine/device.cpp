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

ENGINE_API CRenderDevice Device;
ENGINE_API CLoadScreenRenderer load_screen_renderer;
ENGINE_API CTimer loading_save_timer;
ENGINE_API bool loading_save_timer_started = false;
ENGINE_API BOOL g_bRendering = FALSE; 

BOOL		g_bLoaded = FALSE;
ref_light	precache_light = 0;

BOOL CRenderDevice::Begin	()
{
	if (g_dedicated_server) {
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

	return TRUE;
}

void CRenderDevice::Clear	()
{
	m_pRender->Clear();
}

void CRenderDevice::End		(void)
{
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

#if 0
			if(g_pGamePersistent->GameType()==1)//haCk
			{
				WINDOWINFO	wi;
				GetWindowInfo(g_AppInfo.WindowHandle,&wi);
				if(wi.dwWindowStatus!=WS_ACTIVECAPTION)
					Pause(TRUE,TRUE,TRUE,"application start");
			}
#endif
		}
	}

	g_bRendering		= FALSE;
	// end scene

	m_pRender->End();
}

volatile u32 mt_Thread_marker = 0x12345678;
void mt_Thread(void* ptr)
{
	g_AppInfo.SecondaryThread = GetCurrentThread();
	while (true)
	{
		PROFILE_BEGIN_FRAME("SeqParallel");
		// waiting for Device permission to execute
		Device.mt_csEnter.Enter();

		if (Device.mt_bMustExit)
		{
			Device.mt_bMustExit = FALSE;				// Important!!!
			Device.mt_csEnter.Leave();					// Important!!!
			PROFILE_END_FRAME();
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
		PROFILE_END_FRAME();
	}
}

#include "igame_level.h"
void CRenderDevice::PreCache	(u32 amount, bool b_draw_loadscreen, bool b_wait_user_input)
{
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
}


int g_svDedicateServerUpdateReate = 100;

ENGINE_API xr_list<LOADING_EVENT>			g_loading_events;

void CRenderDevice::on_idle		()
{
	if (!b_is_Ready) {
		Sleep(100);
		return;
	}

	Profile::BeginFrame("Frame");
	Device.BeginRender();
	const bool Minimized = SDL_GetWindowFlags(g_AppInfo.Window) & SDL_WINDOW_MINIMIZED;
	const bool Focus = !Minimized && !(g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive()) && !Device.IsCapturingInputs();
	SDL_SetWindowGrab(g_AppInfo.Window, Focus);
	SDL_SetRelativeMouseMode(Focus);

	u32 FrameStartTime = TimerGlobal.GetElapsed_ms();

	g_bEnableStatGather = psDeviceFlags.test(rsStatistic);

	if (g_loading_events.size())
	{
		if (g_loading_events.front()())
			g_loading_events.pop_front();

		pApp->LoadDraw();
		Profile::EndFrame();
		return;
	} else {
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

	vCameraPosition_saved	= vCameraPosition;
	mFullTransform_saved	= mFullTransform;
	mView_saved				= mView;
	mProject_saved			= mProject;

	// *** Resume threads
	// Capture end point - thread must run only ONE cycle
	// Release start point - allow thread to run
	mt_csLeave.Enter			();
	mt_csEnter.Leave			();
	Sleep						(0);

	if (!g_dedicated_server) {
		SCOPE_EVENT_NAME_GROUP("Render", "Render");
		if (b_is_Active) {
			if (Begin()) {
				seqRender.Process(rp_Render);
				End();
			}
		}
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

	if (!g_dedicated_server && (!g_pGameLevel || g_pGamePersistent->m_pMainMenu->IsActive()))
	{
		u32 FrameEndTime = TimerGlobal.GetElapsed_ms();
		u32 FrameTime = (FrameEndTime - FrameStartTime);

		u32 DSUpdateDelta = 1000 / g_svDedicateServerUpdateReate;
		if (FrameTime < DSUpdateDelta) {
			Sleep(DSUpdateDelta - FrameTime - 1);
		}
	}

	Device.EndRender();
	Profile::EndFrame();
	if (!b_is_Active)
		Sleep		(1);
}

bool quiting = false;

void CRenderDevice::message_loop()
{
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
}

void CRenderDevice::Run()
{
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
}

u32 app_inactive_time		= 0;
u32 app_inactive_time_start = 0;

void ProcessLoading(RP_FUNC *f);
void CRenderDevice::FrameMove()
{
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

	{
		SCOPE_EVENT_NAME_GROUP("Frame", "Engine");
		ProcessLoading(rp_Frame);
	}
}

void ProcessLoading				(RP_FUNC *f)
{
	SCOPE_EVENT_NAME_GROUP("Loading", "Engine");

	Device.seqFrame.Process				(rp_Frame);
	g_bLoaded							= TRUE;
}

ENGINE_API BOOL bShowPauseString = TRUE;
#include "IGame_Persistent.h"

CRenderDevice::CRenderDevice() :
	m_pRender(0)
#ifdef PROFILE_CRITICAL_SECTIONS
	, mt_csEnter(MUTEX_PROFILE_ID(CRenderDevice::mt_csEnter))
	, mt_csLeave(MUTEX_PROFILE_ID(CRenderDevice::mt_csLeave))
#endif // #ifdef PROFILE_CRITICAL_SECTIONS
{
	CaptureInputs = false;
	DrawUIRender = true;
	b_is_Active = true;
	b_is_Ready = FALSE;
	Timer.Start();
	m_bNearer = FALSE;
};

void CRenderDevice::Pause(BOOL bOn, BOOL bTimer, BOOL bSound, LPCSTR reason)
{
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

		if (!g_dedicated_server) 
		{
			//SDL_HideCursor();
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
		if (NewState) {
			SDL_HideCursor();
		} else {
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

void	CRenderDevice::RemoveSeqFrame	( pureFrame* f )
{
	seqFrameMT.Remove	( f );
	seqFrame.Remove		( f );
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
	pApp->destroy_loading_shaders	();
	b_registered					= false;
	b_need_user_input				= false;
}

void CLoadScreenRenderer::OnRender() { pApp->load_draw_internal(); }

void CRenderDevice::time_factor(const float& time_factor) {
	Timer.time_factor(time_factor);
	TimerGlobal.time_factor(time_factor);
	psSoundTimeFactor = time_factor; //--#SM+#--
}