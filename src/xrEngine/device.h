#pragma once

// Note:
// ZNear - always 0.0f
// ZFar  - always 1.0f

#include "pure.h"
#include "../xrCore/FTimer.h"
#include "Stats.h"

#define DEVICE_RESET_PRECACHE_FRAME_COUNT 10

#include "../xrRHI/RHI.h"

#include "../Include/xrRender/FactoryPtr.h"
#include "../Include/xrRender/RenderDeviceRender.h"

class engine_impl;
struct RENDERDOC_API_1_6_0;
union SDL_Event;

#pragma pack(push,4)

#ifdef IXR_WINDOWS
enum D3D_FEATURE_LEVEL;
#endif

class IRenderDevice
{
public:
	virtual ~IRenderDevice() = default;
	virtual		CStatsPhysics*	_BCL		StatPhysics		()							= 0;								
	virtual				void	_BCL		AddSeqFrame		( pureFrame* f, bool mt )	= 0;
	virtual				void	_BCL		RemoveSeqFrame	( pureFrame* f )			= 0;

	virtual				bool				InitRenderDevice(ERHI_API_LAYER API) = 0;
	virtual				void				DestroyRenderDevice() = 0;

	virtual				void*               GetRenderDevice() = 0;

	virtual				void*				GetSwapchain() = 0;
	virtual				u32					GetSwapchainWidth() = 0;
	virtual				u32					GetSwapchainHeight() = 0;

	virtual				void				ResizeWindow(u32 width, u32 height) = 0;

	virtual				RENDERDOC_API_1_6_0* GetRenderDocAPI() = 0;

	virtual				void				BeginRender() = 0;
	virtual				void				EndRender() = 0;
	virtual				bool				IsEditorMode() { return false; }
};

class ENGINE_API CRenderDeviceData
{

public:
	u32										TargetWidth;
	u32										TargetHeight;
	
	u32										dwPrecacheFrame;
	bool									b_is_Ready;
	bool									b_is_Active;
public:
	struct {
		float renderZoomFactor = 1.0f;
		float renderZoomRotateFactor = 0.0f;
		bool isRenderActive = false;
		bool isRenderProcess = false;

		float renderScopeBrightnessValue = 0.0f;
		float renderScopeBrightnessJitterValue = 0.0f;

		bool IsElectronicsProblemsDecreasing = false;
		float CurrentElectronicsProblemsCnt = 0.0f;
		float TargetElectronicsProblemsCnt = 0.0f;

		float ActorHealth = -1.0f;
		float ActorOutfitCondition = -1.0f;
		float ActorWeaponCondition = -1.0f;
		float ActorWeaponLoading = 1.0f;
	} hudViewportData;

	// Engine flow-control
	u32										dwFrame;

	float									fTimeDeltaSmoothing;
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
	Fmatrix									mInvFullTransform;
	Fmatrix									mInv3x4FullTransform;

	// Copies of corresponding members. Used for synchronization.
	Fvector									vCameraPosition_saved;

	Fvector									vCameraDirection_saved;
	Fvector									vCameraRight_saved;
	Fvector									vCameraTop_saved;

	Fmatrix									mView_saved;
	Fmatrix									mProject_saved;
	Fmatrix									mFullTransform_saved;

	Fmatrix									mView_old;
	Fmatrix									mProject_old;
	Fmatrix									mFullTransform_old;

	Fmatrix									mView_hud;
	Fmatrix									mProject_hud;
	Fmatrix									mFullTransform_hud;
	Fmatrix									mFullTransform_hud_special;
	Fmatrix									mInv3x4FullTransform_hud_special;

	Fmatrix									mView_hud_old;
	Fmatrix									mProject_hud_old;
	Fmatrix									mFullTransform_hud_old;

	float									fFOV;
	float									fASPECT;
	float									fViewportNear = 0.2f;
	float									fHUDViewportNear = 0.05f;
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

#pragma pack(pop)
// refs
class ENGINE_API CRenderDevice:
	public IRenderDevice,
	public CRenderDeviceData
{
	friend class CPHWorld;

	RENDERDOC_API_1_6_0* pRDocAPI = nullptr;

public:
	int Width = 0, Height = 0, PosX = 0, PosY = 0;

	CTimer									TimerMM;

	void									_Create		(const char* shName);
	void									_Destroy	(bool	bKeepTextures);
	void									_SetupStates();

	bool InitRenderDeviceEditor();
	bool InitRenderDevice(ERHI_API_LAYER API) override;
	void DestroyRenderDevice() override;

	void* GetRenderDevice() override;

	u32 GetTimeDeltaSafe(u32 starttime);
	u32 GetTimeDeltaSafe(u32 starttime, u32 endtime);

	void* GetSwapchain() override;
	u32	GetSwapchainWidth() override;
	u32	GetSwapchainHeight() override;

	void ResizeBuffers(u32 Width, u32 Height);
	void ResizeWindow(u32 width, u32 height);

	RENDERDOC_API_1_6_0* GetRenderDocAPI() override;

	void BeginRender() override;
	void EndRender() override;

public:
	LRESULT									MsgProc		(HWND,UINT,WPARAM,LPARAM);

	u32										dwPrecacheTotal;
	void									OnWM_Activate(bool active, bool minimized);

public:
	IRenderDeviceRender						*m_pRender;

	bool									m_bNearer;
	void									SetNearer	(bool enabled)
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
	xr_vector		<xr_delegate<void()>>	seqParallel;
	xr_vector		<xr_delegate<void()>>	seqParallelRender;

	xr_delegate<void()> ModelDefferClear;

	xr_vector<xr_pair<u32,std::function<void()>>> m_time_callbacks;
	ICF void callback(u32 cb_time, const std::function<void()>& func)
	{
		m_time_callbacks.push_back({ dwTimeGlobal + cb_time,func });
	}
	// Dependent classes
	CStats*									Statistic;
	
	CRenderDevice();
	virtual ~CRenderDevice() noexcept = default;

	virtual void	Pause							(bool bOn, bool bTimer, bool bSound, const char* reason);
	bool	Paused							();

	// Scene control
	virtual void PreCache							(u32 amount, bool b_draw_loadscreen, bool b_wait_user_input);
	bool Begin								();
	virtual void Clear						();
	void End								();
	void FrameMove							();
	void CalculateTransforms				();
	void overdrawBegin						();
	void overdrawEnd						();

	// Mode control
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
	void time_factor						(const float &time_factor);
	
	IC	const float &time_factor			() const
	{
		VERIFY					(Timer.time_factor() == TimerGlobal.time_factor());
		return					(Timer.time_factor());
	}

	// Multi-threading
	xr_task_group secondary_tasks, details_task;

	ICF		void			remove_from_seq_parallel	(const xr_delegate<void()> &delegate)
	{
		xr_vector<xr_delegate<void()> >::iterator I = std::find(
			seqParallel.begin(),
			seqParallel.end(),
			delegate
		);
		if (I != seqParallel.end())
			seqParallel.erase	(I);
	}

	ICF void transform_hud2world(Fmatrix& xf)
	{
		xf.mulA_43(mFullTransform_hud_special);
		xf.mulA_43(mInv3x4FullTransform);
	}

	ICF void transform_hud2world(Fvector& pos)
	{
		mFullTransform_hud_special.transform_tiny(pos);
		mInv3x4FullTransform.transform_tiny(pos);
	}

	ICF void transform_hud2world(Fvector& pos, Fvector& dir)
	{
		dir.add(pos);
		transform_hud2world(dir);
		transform_hud2world(pos);
		dir = dir.sub(pos).normalize();
	}

	ICF void transform_world2hud(Fmatrix& xf)
	{
		xf.mulA_43(mFullTransform);
		xf.mulA_43(mInv3x4FullTransform_hud_special);
	}

	ICF void transform_world2hud(Fvector& pos)
	{
		mFullTransform.transform_tiny(pos);
		mInv3x4FullTransform_hud_special.transform_tiny(pos);
	}

	ICF void transform_world2hud(Fvector& pos, Fvector& dir)
	{
		dir.add(pos);
		transform_world2hud(dir);
		transform_world2hud(pos);
		dir = dir.sub(pos).normalize();
	}

public:
			void 		on_idle				();
			bool 		on_event			(SDL_Event& Event);

private:
			void					message_loop		();
virtual		void			_BCL	AddSeqFrame			( pureFrame* f, bool mt );
virtual		void			_BCL	RemoveSeqFrame		( pureFrame* f );
virtual		CStatsPhysics*	_BCL	StatPhysics			()	{ return  Statistic ;}
};

extern ENGINE_API CRenderDevice* DevicePtr;
extern ENGINE_API CTimer loading_save_timer;
extern ENGINE_API bool loading_save_timer_started;


#define Device (*DevicePtr)
#define	RDEVICE	Device

typedef xr_delegate<bool()>		LOADING_EVENT;
extern	ENGINE_API xr_list<LOADING_EVENT>		g_loading_events;

class ENGINE_API CLoadScreenRenderer :public pureRender
{
public:
					CLoadScreenRenderer	();
	void			start				(bool b_user_input);
	void			stop				();
	virtual void	OnRender			();

	bool IsActive() const {
		return b_registered;
	}

	bool			b_registered;
	bool			b_need_user_input;
};
extern ENGINE_API CLoadScreenRenderer load_screen_renderer;