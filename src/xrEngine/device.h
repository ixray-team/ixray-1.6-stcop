#ifndef xr_device
#define xr_device
#pragma once
#include <functional>

// Note:
// ZNear - always 0.0f
// ZFar  - always 1.0f

//class	ENGINE_API	CResourceManager;
//class	ENGINE_API	CGammaControl;

#include "pure.h"
//#include "hw.h"
#include "../xrcore/ftimer.h"
#include "stats.h"
//#include "shader.h"
//#include "R_Backend.h"

#define VIEWPORT_NEAR  0.2f
#define HUD_VIEWPORT_NEAR  0.05f

#define DEVICE_RESET_PRECACHE_FRAME_COUNT 10

#include "../Include/xrRender/FactoryPtr.h"
#include "../Include/xrRender/RenderDeviceRender.h"

#ifdef INGAME_EDITOR
#	include "../Include/editor/interfaces.hpp"
#endif // #ifdef INGAME_EDITOR

class engine_impl;
struct RENDERDOC_API_1_6_0;
union SDL_Event;

#pragma pack(push,4)

enum class APILevel
{
	DX9,
	DX11
};

enum D3D_FEATURE_LEVEL;

class IRenderDevice
{
public:
	virtual		CStatsPhysics*	_BCL		StatPhysics		()							= 0;								
	virtual				void	_BCL		AddSeqFrame		( pureFrame* f, bool mt )	= 0;
	virtual				void	_BCL		RemoveSeqFrame	( pureFrame* f )			= 0;

	virtual				bool				InitRenderDevice(APILevel API) = 0;
	virtual				void				DestroyRenderDevice() = 0;

	virtual				void*               GetRenderDevice() = 0;
	virtual				void*               GetRenderContext() = 0;
	virtual				void*               GetRenderTexture() = 0;
	virtual				void*               GetDepthTexture() = 0;
	virtual				void*               GetSwapchainTexture() = 0;

	virtual				void*				GetSwapchain() = 0;
	virtual				u32					GetSwapchainWidth() = 0;
	virtual				u32					GetSwapchainHeight() = 0;

	virtual				void				ResizeBuffers(u16 Width, u16 Height) = 0;

	virtual				D3D_FEATURE_LEVEL	GetFeatureLevel() = 0;
	virtual				RENDERDOC_API_1_6_0* GetRenderDocAPI() = 0;

	virtual				void				BeginRender() = 0;
	virtual				void				EndRender() = 0;
	virtual				void				DrawUI() = 0;
	virtual				void				AddUICommand(const char* Name, std::function<void()>&& Function) = 0;
	virtual				void				RemoveUICommand(const char* Name) = 0;
};

class ENGINE_API CRenderDeviceData
{

public:
	u32										TargetWidth;
	u32										TargetHeight;
	
	u32										dwPrecacheFrame;
	BOOL									b_is_Ready;
	BOOL									b_is_Active;
public:

	// Engine flow-control
	u32										dwFrame;

	float									fTimeDelta;
	float									fTimeGlobal;
	u32										dwTimeDelta;
	u32										dwTimeGlobal;
	u32										dwTimeContinual;

	Fvector									vCameraPosition;
	Fvector									vCameraDirection;
	Fvector									vCameraTop;
	Fvector									vCameraRight;

	Fmatrix									mView;
	Fmatrix									mProject;
	Fmatrix									mFullTransform;

	// Copies of corresponding members. Used for synchronization.
	Fvector									vCameraPosition_saved;

	Fmatrix									mView_saved;
	Fmatrix									mProject_saved;
	Fmatrix									mFullTransform_saved;

	float									fFOV;
	float									fASPECT;
protected:

	u32										Timer_MM_Delta;
	CTimer_paused							Timer;
	CTimer_paused							TimerGlobal;
public:

// Registrators
	CRegistrator	<pureRender			>			seqRender;
	CRegistrator	<pureAppActivate	>			seqAppActivate;
	CRegistrator	<pureAppDeactivate	>			seqAppDeactivate;
	CRegistrator	<pureAppStart		>			seqAppStart;
	CRegistrator	<pureAppEnd			>			seqAppEnd;
	CRegistrator	<pureFrame			>			seqFrame;
	CRegistrator	<pureScreenResolutionChanged>	seqResolutionChanged;
};

class	ENGINE_API CRenderDeviceBase :
	public IRenderDevice,
	public CRenderDeviceData
{
public:
};

#pragma pack(pop)
// refs
class ENGINE_API CRenderDevice: public CRenderDeviceBase
{
public:
	int Width = 0, Height = 0, PosX = 0, PosY = 0;

	CTimer									TimerMM;

	void									_Create		(LPCSTR shName);
	void									_Destroy	(BOOL	bKeepTextures);
	void									_SetupStates();

	bool InitRenderDevice(APILevel API) override;
	void DestroyRenderDevice() override;

	void* GetRenderDevice() override;
	void* GetRenderContext() override;
	void* GetRenderTexture() override;
	void* GetDepthTexture() override;
	void* GetSwapchainTexture() override;

	void* GetSwapchain() override;
	u32	GetSwapchainWidth() override;
	u32	GetSwapchainHeight() override;

	void ResizeBuffers(u16 Width, u16 Height) override;

	D3D_FEATURE_LEVEL GetFeatureLevel() override;
	RENDERDOC_API_1_6_0* GetRenderDocAPI() override;

	void BeginRender() override;
	void EndRender() override;
	void DrawUI() override;
	void AddUICommand(const char* Name, std::function<void()>&& Function) override;
	void RemoveUICommand(const char* Name) override;

public:
	LRESULT									MsgProc		(HWND,UINT,WPARAM,LPARAM);

	u32										dwPrecacheTotal;

	float									HalfTargetWidth, HalfTargetHeight;
	void									OnWM_Activate(bool active, bool minimized);

public:
	IRenderDeviceRender						*m_pRender;

	BOOL									m_bNearer;
	void									SetNearer	(BOOL enabled)
	{
		if (enabled&&!m_bNearer){
			m_bNearer						= TRUE;
			mProject._43					-= EPS_L;
		}else if (!enabled&&m_bNearer){
			m_bNearer						= FALSE;
			mProject._43					+= EPS_L;
		}
		m_pRender->SetCacheXform(mView, mProject);
	}

	void									DumpResourcesMemoryUsage() { m_pRender->ResourcesDumpMemoryUsage();}
public:
	// Registrators
	CRegistrator	<pureFrame			>			seqFrameMT;
	CRegistrator	<pureDeviceReset	>			seqDeviceReset;
	xr_vector		<fastdelegate::FastDelegate0<> >	seqParallel;

	// Dependent classes
	CStats*									Statistic;

	// Engine flow-control
	Fmatrix									mInvFullTransform;
	
	CRenderDevice			()
		:
		m_pRender(0)
#ifdef INGAME_EDITOR
		,m_editor_module(0),
		m_editor_initialize(0),
		m_editor_finalize(0),
		m_editor(0),
		m_engine(0)
#endif // #ifdef INGAME_EDITOR
#ifdef PROFILE_CRITICAL_SECTIONS
		,mt_csEnter(MUTEX_PROFILE_ID(CRenderDevice::mt_csEnter))
		,mt_csLeave(MUTEX_PROFILE_ID(CRenderDevice::mt_csLeave))
#endif // #ifdef PROFILE_CRITICAL_SECTIONS
	{
		b_is_Active			= true;
		b_is_Ready			= FALSE;
		Timer.Start			();
		m_bNearer			= FALSE;
	};

	void	Pause							(BOOL bOn, BOOL bTimer, BOOL bSound, LPCSTR reason);
	BOOL	Paused							();

	// Scene control
	void PreCache							(u32 amount, bool b_draw_loadscreen, bool b_wait_user_input);
	BOOL Begin								();
	void Clear								();
	void End								();
	void FrameMove							();
	
	void overdrawBegin						();
	void overdrawEnd						();

	// Mode control
	void DumpFlags							();
	IC	 CTimer_paused* GetTimerGlobal		()	{ return &TimerGlobal;								}
	u32	 TimerAsync							()	{ return TimerGlobal.GetElapsed_ms();				}
	u32	 TimerAsync_MMT						()	{ return TimerMM.GetElapsed_ms() +	Timer_MM_Delta; }

	// Creation & Destroying
	void ConnectToRender();
	void Create								(void);
	void Run								(void);
	void Destroy							(void);
	void Reset								(bool precache = true);

	void Initialize							(void);
	void ShutDown							(void);

public:
	void time_factor(const float& time_factor); //--#SM+#--
	
	IC	const float &time_factor			() const
	{
		VERIFY					(Timer.time_factor() == TimerGlobal.time_factor());
		return					(Timer.time_factor());
	}

	// Multi-threading
	xrCriticalSection	mt_csEnter;
	xrCriticalSection	mt_csLeave;
	volatile BOOL		mt_bMustExit;

	ICF		void			remove_from_seq_parallel	(const fastdelegate::FastDelegate0<> &delegate)
	{
		xr_vector<fastdelegate::FastDelegate0<> >::iterator I = std::find(
			seqParallel.begin(),
			seqParallel.end(),
			delegate
		);
		if (I != seqParallel.end())
			seqParallel.erase	(I);
	}

public:
			void xr_stdcall		on_idle				();
			bool xr_stdcall		on_event			(SDL_Event& Event);

private:
			void					message_loop		();
virtual		void			_BCL	AddSeqFrame			( pureFrame* f, bool mt );
virtual		void			_BCL	RemoveSeqFrame		( pureFrame* f );
virtual		CStatsPhysics*	_BCL	StatPhysics			()	{ return  Statistic ;}
#ifdef INGAME_EDITOR
public:
	IC		editor::ide			*editor				() const { return m_editor; }

private:
			void				initialize_editor	();
			void				message_loop_editor	();

private:
	using initialize_function_ptr = editor::initialize_function_ptr;
	using finalize_function_ptr = editor::finalize_function_ptr;

private:
	HMODULE						m_editor_module;
	initialize_function_ptr		m_editor_initialize;
	finalize_function_ptr		m_editor_finalize;
	editor::ide					*m_editor;
	engine_impl					*m_engine;
#endif // #ifdef INGAME_EDITOR
};

extern		ENGINE_API		CRenderDevice		Device;
extern ENGINE_API CTimer loading_save_timer;
extern ENGINE_API bool loading_save_timer_started;

#ifndef	_EDITOR
#define	RDEVICE	Device
#else
#define RDEVICE	EDevice
#endif

typedef fastdelegate::FastDelegate0<bool>		LOADING_EVENT;
extern	ENGINE_API xr_list<LOADING_EVENT>		g_loading_events;

class ENGINE_API CLoadScreenRenderer :public pureRender
{
public:
					CLoadScreenRenderer	();
	void			start				(bool b_user_input);
	void			stop				();
	virtual void	OnRender			();
	bool IsActive() const { return b_registered; }

	bool			b_registered;
	bool			b_need_user_input;
};
extern ENGINE_API CLoadScreenRenderer load_screen_renderer;

#endif