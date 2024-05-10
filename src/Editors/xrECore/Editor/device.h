#pragma once

#include "../../../xrengine/device.h"
#include "ui_camera.h"
#include "../../../Layers/xrRender/HWCaps.h"
#include "../../../Layers/xrRender/hw.h"
#include "../../../xrEngine/pure.h"
#include "../../../xrCore/ftimer.h"
#include "estats.h"
#include "../../../xrEngine/shader_xrlc.h"
#include "../../../Layers/xrRender/shader.h"
#include "../../../Layers/xrRender/R_Backend.h"

//---------------------------------------------------------------------------
// refs
class CGameFont;
class CInifile;
class CResourceManager;
#undef CreateWindow
//------------------------------------------------------------------------------
class ECORE_API CEditorRenderDevice;
extern ECORE_API CEditorRenderDevice* EDevice;

#define REContext ((IDirect3DDevice9*)EDevice->GetRenderContext())
#define REDevice ((IDirect3DDevice9*)EDevice->GetRenderDevice())
#define RESwapchainTarget ((IDirect3DSurface9*)EDevice->GetSwapchainTexture())
#define RETarget ((IDirect3DSurface9*)EDevice->GetRenderTexture())
#define REDepth ((IDirect3DSurface9*)EDevice->GetDepthTexture())
#define RESwapchain ((IDirect3DDevice9*)EDevice->GetSwapchain())

class ECORE_API CEditorRenderDevice :
	public CRenderDevice
{
    friend class 			CUI_Camera;
    friend class 			TUI;
	HMODULE					hPSGP;
    float 					m_fNearer;

    ref_shader				m_CurrentShader;

    void					_SetupStates();
	void					_Create		(IReader* F);
	void					_Destroy	(BOOL	bKeepTextures);
public:
    ref_shader				m_WireShader;
    ref_shader				m_SelectionShader;

    Fmaterial				m_DefaultMat;
public:
	float RadiusRender;
	u32 					dwRealWidth, dwRealHeight;
    float					m_RenderArea;
    float 					m_ScreenQuality;

	u32 					dwFillMode;
    u32						dwShadeMode;
public:
    // camera
	CUI_Camera 				m_Camera;
	CRegistrator	<pureDrawUI			>			seqDrawUI;

	// Dependent classes
	CResourceManager*		Resources;	  
	//CEStats*				EStatistic;

	CGameFont* 				pSystemFont;

public:
							CEditorRenderDevice 	();
    virtual 				~CEditorRenderDevice	();

	
	virtual  bool			Paused()const { return FALSE; };
    void					time_factor		(float);
	bool 					Create			();
	void 					Destroy			();
    void 					Resize			(int w, int h,bool maximized);
	void 					ReloadTextures	();
	void 					UnloadTextures	();

    void 					RenderNearer	(float f_Near);
    void 					ResetNearer		();
	bool 					Begin();
	void 					End				();

	void 					Initialize		(void);
	void 					ShutDown		(void);
	void 					Reset			(IReader* F, BOOL bKeepTextures);

	virtual void DumpResourcesMemoryUsage() {}
    IC float				GetRenderArea	(){return m_RenderArea;}
	// Sprite rendering
	IC float 				_x2real			(float x)
    { return (x+1)*TargetWidth*0.5f;	}
	IC float 				_y2real			(float y)
    { return (y+1) * TargetHeight * 0.5f;}

	// draw
	void			   		SetShader		(ref_shader sh){m_CurrentShader = sh;}
	void			   		DP				(D3DPRIMITIVETYPE pt, ref_geom geom, u32 startV, u32 pc);
	void 					DIP				(D3DPRIMITIVETYPE pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC);

    IC void					SetRS			(D3DRENDERSTATETYPE p1, u32 p2)
    { VERIFY(b_is_Ready); CHK_DX(REDevice->SetRenderState(p1,p2)); }
    IC void					SetSS			(u32 sampler, D3DSAMPLERSTATETYPE type, u32 value)
    { VERIFY(b_is_Ready); CHK_DX(REDevice->SetSamplerState(sampler,type,value)); }

    // light&material
    IC void					LightEnable		(u32 dwLightIndex, BOOL bEnable)
    { CHK_DX(REDevice->LightEnable(dwLightIndex, bEnable));}
    IC void					SetLight		(u32 dwLightIndex, Flight& lpLight)
    { CHK_DX(REDevice->SetLight(dwLightIndex, (D3DLIGHT9*)&lpLight));}
	IC void					SetMaterial		(Fmaterial& mat)
    { CHK_DX(REDevice->SetMaterial((D3DMATERIAL9*)&mat)); }
	IC void					ResetMaterial	()
    { CHK_DX(REDevice->SetMaterial((D3DMATERIAL9*)&m_DefaultMat));}

	// update
    void					UpdateView		();
	void					FrameMove		();

    bool					MakeScreenshot	(U32Vec& pixels, u32 width, u32 height);

	void 					InitTimer		();
	// Mode control
	virtual void	Pause(BOOL bOn, BOOL bTimer, BOOL bSound, LPCSTR reason) {}
	virtual void PreCache(u32 amount, bool b_draw_loadscreen, bool b_wait_user_input) {}
	virtual void Clear() {}
public:
    Shader_xrLC_LIB			ShaderXRLC;
private:
	//virtual		CStatsPhysics* _BCL			StatPhysics();
	virtual				void	   _BCL			AddSeqFrame(pureFrame* f, bool mt);
	virtual				void	   _BCL			RemoveSeqFrame(pureFrame* f);
private:
	WNDCLASSEX m_WC;
public:
	void CreateWindow();
	void DestryWindow();
	virtual			bool				IsEditorMode() { return true; }
	virtual void Reset(bool precache);
};

// video
enum {
	rsFilterLinear		= (1ul<<20ul),
    rsEdgedFaces		= (1ul<<21ul),
	rsRenderTextures	= (1ul<<22ul),
	rsLighting			= (1ul<<23ul),
    rsFog				= (1ul<<24ul),
    rsRenderRealTime	= (1ul<<25ul),
    rsDrawGrid			= (1ul<<26ul),
    rsDrawSafeRect		= (1ul<<27ul),
    rsMuteSounds		= (1ul<<28ul),
    rsEnvironment		= (1ul<<29ul),
};

#define DEFAULT_CLEARCOLOR 0x00555555

#define		REQ_CREATE()	if (!EDevice->bReady)	return;
#define		REQ_DESTROY()	if (EDevice->bReady)	return;

//#include "../../../xrCPU_Pipe/xrCPU_Pipe.h"
//ENGINE_API extern xrDispatchTable	PSGP;

#include "../../../Layers/xrRender/R_Backend_Runtime.h"

