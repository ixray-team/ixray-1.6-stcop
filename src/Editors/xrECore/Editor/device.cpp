//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop
#include "../xrEngine/GameFont.h"
#include <sal.h>
#include "ImageManager.h"
#include "ui_main.h"
#include "render.h"
#include "../Engine/XrGameMaterialLibraryEditors.h"
#include "../Layers/xrRender/ResourceManager.h"
#include "../Layers/xrRender/dxRenderDeviceRender.h"
#include "UI_ToolsCustom.h"

CEditorRenderDevice 	*	EDevice;
bool g_bIsEditor;

extern int	rsDVB_Size;
extern int	rsDIB_Size;

//CStatsPhysics* _BCL			CEditorRenderDevice::StatPhysics() { return Statistic; }
void	   _BCL			CEditorRenderDevice::AddSeqFrame(pureFrame* f, bool mt) { seqFrame.Add(f, REG_PRIORITY_LOW); }
void	   _BCL			CEditorRenderDevice::RemoveSeqFrame(pureFrame* f) { seqFrame.Remove(f); }

ENGINE_API BOOL g_bRendering;
//---------------------------------------------------------------------------
CEditorRenderDevice::CEditorRenderDevice()
{
	RadiusRender = 400;
	psDeviceFlags.assign(rsStatistic|rsFilterLinear|rsFog|rsDrawGrid);
// dynamic buffer size
	rsDVB_Size		= 1024 * 16;
	rsDIB_Size		= 2048;
// default initialization
    m_ScreenQuality = 1.f;
	//dwMaximized = 0;
    TargetWidth 		= TargetHeight 	= 256;
	dwRealWidth = dwRealHeight = 256;
	mProject.identity();
    mFullTransform.identity();
    mView.identity	();
	m_WireShader	= 0;
	m_SelectionShader = 0;

    b_is_Ready 			= FALSE;
	b_is_Active			= FALSE;

	// Engine flow-control
	fTimeDelta		= 0;
	fTimeGlobal		= 0;
	dwTimeDelta		= 0;
	dwTimeGlobal	= 0;

	dwFillMode		= D3DFILL_SOLID;
    dwShadeMode		= D3DSHADE_GOURAUD;

    m_CurrentShader	= 0;
    //pSystemFont		= 0;

	fASPECT 		= 1.f;
	fFOV 			= 60.f;
    dwPrecacheFrame = 0;
	GameMaterialLibraryEditors = new XrGameMaterialLibraryEditors();
	PGMLib = GameMaterialLibraryEditors;

	DevicePtr = this;
	g_bIsEditor = true;
}

CEditorRenderDevice::~CEditorRenderDevice(){
	VERIFY(!b_is_Ready);
	//xr_delete(GameMaterialLibrary);
	GameMaterialLibraryEditors = nullptr;
}

//extern void Surface_Init();
#include "../../../xrCore/API/xrAPI.h"
#include "../../../Layers/xrRender/dxRenderFactory.h"
#include "../../../Layers/xrRender/dxUIRender.h"
#include "../../../Layers/xrRender/dxDebugRender.h"
#include "../xrCore/appinfo.h"

typedef void __cdecl ttapi_Done_func(void);

void CEditorRenderDevice::Initialize()
{
    m_DefaultMat.set(1,1,1);

	RenderFactory = &RenderFactoryImpl;
	UIRender = &UIRenderImpl;

#ifdef DEBUG_DRAW
	DRender = &DebugRenderImpl;
#endif

	SDL_Init(0);
	// compiler shader
    string_path fn;
    FS.update_path(fn,_game_data_,"shaders_xrlc.xr");
    if (FS.exist(fn)){
    	ShaderXRLC.Load(fn);
    }else{
    	ELog.DlgMsg(mtInformation,"Can't find file '%s'",fn);
    }
	CreateWindow();


	// Startup shaders
	Create();

    ::RImplementation.Initialize();
	UIRenderImpl.CreateUIGeom();

	Resize(EPrefs->start_w, EPrefs->start_h, EPrefs->start_maximized);
	//HW.updateWindowProps(m_hWnd);

	SDL_GetWindowSizeInPixels(g_AppInfo.Window, &Width, &Height);
	SDL_GetWindowPosition(g_AppInfo.Window, &PosX, &PosY);

	if (EPrefs->start_maximized)
		SDL_MaximizeWindow(g_AppInfo.Window);
	
	SDL_ShowWindow(g_AppInfo.Window);
}

void CEditorRenderDevice::ShutDown()
{
	UIRenderImpl.DestroyUIGeom();
	::RImplementation.ShutDown	();

	ShaderXRLC.Unload	();
	//GameMaterialLibraryEditors->Unload		();

	// destroy context
	Destroy				();
	//xr_delete			(pSystemFont);

	//if (hPSGP)
	//{
	//	ttapi_Done_func* ttapi_Done = (ttapi_Done_func*)GetProcAddress(hPSGP, "ttapi_Done");	R_ASSERT(ttapi_Done);
	//	if (ttapi_Done)
	//		ttapi_Done();
	//
	//	FreeLibrary(hPSGP);
	//	hPSGP = 0;
	//	ZeroMemory(&PSGP, sizeof(PSGP));
	//}
	// destroy shaders
//	PSLib.xrShutDown	();
}

void CEditorRenderDevice::InitTimer(){
	Timer_MM_Delta	= 0;
	{
		u32 time_mm			= clock	();
		while (clock()==time_mm);			// wait for next tick
		u32 time_system		= clock();
		u32 time_local		= TimerAsync	();
		Timer_MM_Delta			= time_system-time_local;
	}
}

void CEditorRenderDevice::Clear()
{
	u32 ClearColor = 0x0;

	if (EPrefs)
	{
		float color[3] = { color_get_B(EPrefs->scene_clear_color) / 255.f, color_get_G(EPrefs->scene_clear_color) / 255.f, color_get_R(EPrefs->scene_clear_color) / 255.f };
		ClearColor = color_rgba_f(color[0], color[1], color[2], 1.f);
	}

	CHK_DX(REDevice->Clear(0, 0, D3DCLEAR_ZBUFFER | D3DCLEAR_STENCIL | D3DCLEAR_TARGET, ClearColor, 1, 0));
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::RenderNearer(float n){
    mProject._43=m_fNearer-n;
    RCache.set_xform_project(mProject);
}
void CEditorRenderDevice::ResetNearer(){
    mProject._43=m_fNearer;
    RCache.set_xform_project(mProject);
}
//---------------------------------------------------------------------------
bool CEditorRenderDevice::Create()
{
	if (b_is_Ready)	return false;

	//Statistic = EStatistic;
	ELog.Msg(mtInformation,"Starting RENDER device...");


	//HW.CreateDevice		(m_hWnd, true);
	if (UI)
	{
		HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
		string_path 		ini_path;
		string_path			ini_name;
		xr_strcpy			(ini_name, UI->EditorName());
		xr_strcat			(ini_name, "_imgui.ini");
		FS.update_path(ini_path, "$app_data_root$", ini_name);
		
		if (!FS.exist(ini_path))
			UI->ResetUI();
		
		InitRenderDeviceEditor();
		UI->Initialize(hwnd, RDevice, ini_path);
	}
	
	// after creation
	dwFrame				= 0;

	string_path 		sh;
    FS.update_path		(sh,_game_data_,"shaders.xr");

    IReader* F			= 0;
	if (FS.exist(sh))
		F				= FS.r_open(0,sh);
	Resources			= new CResourceManager	();

    // if build options - load textures immediately
    if (strstr(Core.Params,"-build")||strstr(Core.Params,"-ebuild"))
        EDevice->Resources->DeferredLoad(FALSE);

	g_FontManager = new CFontManager();

    _Create				(F);
	FS.r_close			(F);

	::Render->create();

	g_FontManager->InitializeFonts();

	Statistic = new CEStats();

	ELog.Msg			(mtInformation, "D3D: initialized");

	return true;
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::Destroy()
{
	if (!b_is_Ready) return;
	ELog.Msg(mtInformation, "Destroying Direct3D...");

	//HW.Validate			();
	::Render->destroy();

	// before destroy
	_Destroy(FALSE);

	xr_delete(Resources);

	UI->Destroy();
	// real destroy
	//HW.DestroyDevice	();

	ELog.Msg(mtInformation, "D3D: device cleared");
	// xr_delete			(Statistic);
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::_SetupStates()
{
	//Caps.Update();
	for (u32 i=0; i<Caps.raster.dwStages; i++){
		float fBias = -1.f;
		CHK_DX(REDevice->SetSamplerState( i, D3DSAMP_MIPMAPLODBIAS, *((LPDWORD) (&fBias))));
	}
	EDevice->SetRS(D3DRS_DITHERENABLE,	TRUE				);
    EDevice->SetRS(D3DRS_COLORVERTEX,		TRUE				);
    EDevice->SetRS(D3DRS_STENCILENABLE,	FALSE				);
    EDevice->SetRS(D3DRS_ZENABLE,			TRUE				);
    EDevice->SetRS(D3DRS_SHADEMODE,		D3DSHADE_GOURAUD	);
	EDevice->SetRS(D3DRS_CULLMODE,		D3DCULL_CCW			);
	EDevice->SetRS(D3DRS_ALPHAFUNC,		D3DCMP_GREATER		);
	EDevice->SetRS(D3DRS_LOCALVIEWER,		TRUE				);
    EDevice->SetRS(D3DRS_NORMALIZENORMALS,TRUE				);

	EDevice->SetRS(D3DRS_DIFFUSEMATERIALSOURCE, D3DMCS_MATERIAL);
	EDevice->SetRS(D3DRS_SPECULARMATERIALSOURCE,D3DMCS_MATERIAL);
	EDevice->SetRS(D3DRS_AMBIENTMATERIALSOURCE, D3DMCS_MATERIAL);
	EDevice->SetRS(D3DRS_EMISSIVEMATERIALSOURCE,D3DMCS_COLOR1	);

    ResetMaterial();
}
//---------------------------------------------------------------------------
void CEditorRenderDevice::_Create(IReader* F)
{
	b_is_Ready				= TRUE;

	// General Render States
    _SetupStates		();
    
    RCache.OnDeviceCreate		();
	Resources->OnDeviceCreate	(F);
	::RImplementation.OnDeviceCreate	();

    m_WireShader.create			("editor\\wire");
    m_SelectionShader.create	("editor\\selection");

	texture_null.create("ed\\ed_nodata");
	texture_null->Load();
	UIChooseForm::SetNullTexture(texture_null->pSurface);

	// signal another objects
    UI->OnDeviceCreate			();           
//.	seqDevCreate.Process		(rp_DeviceCreate);

	//pSystemFont					= new CGameFont("hud_font_small");
//	pSystemFont					= new CGameFont("hud_font_medium");
}

void CEditorRenderDevice::_Destroy(BOOL	bKeepTextures)
{
	//xr_delete					(pSystemFont);

	b_is_Ready 						= FALSE;
    m_CurrentShader				= 0;

    UI->OnDeviceDestroy			();

	m_WireShader.destroy		();
	m_SelectionShader.destroy	();
	texture_null.destroy		();

//.	seqDevDestroy.Process		(rp_DeviceDestroy);

	::RImplementation.Models->OnDeviceDestroy	();

	Resources->OnDeviceDestroy	(bKeepTextures);

	RCache.OnDeviceDestroy		();
	::RImplementation.OnDeviceDestroy	();
}

//---------------------------------------------------------------------------
void  CEditorRenderDevice::Resize(int w, int h, bool maximized)
{
	m_RenderArea = w * h;

	dwRealWidth = w;
	dwRealHeight = h;

	Reset(false);
	UI->RedrawScene();
}

void CEditorRenderDevice::Reset(bool)
{
	u32 tm_start = TimerAsync();

	//UIChooseForm::SetNullTexture(nullptr);

	Resources->reset_begin();
	Resources->DeferredUnload();
	UI->ResetBegin();

	Memory.mem_compact();
	ResizeBuffers(dwRealWidth, dwRealHeight);
	SDL_SetWindowSize(g_AppInfo.Window, dwRealWidth, dwRealHeight);

	Resources->reset_end();
	Resources->DeferredUpload();

	UI->ResetEnd(RDevice);
	_SetupStates();

	UIChooseForm::SetNullTexture(texture_null->pSurface);

	u32 tm_end = TimerAsync();
	Msg("*** RESET [%d ms]", tm_end - tm_start);
}

void CEditorRenderDevice::Reset(IReader* F, BOOL bKeepTextures)
{
	CTimer tm;
	tm.Start();
	_Destroy(bKeepTextures);
	_Create(F);
	Msg("*** RESET [%d ms]", tm.GetElapsed_ms());
}

bool CEditorRenderDevice::Begin()
{
	VERIFY(b_is_Ready);
	mFullTransform_saved = mFullTransform;
	mProject_saved = mProject;
	mView_saved = mView;
	vCameraPosition_saved = vCameraPosition;
	//HW.Validate		();
	HRESULT	_hr = REDevice->TestCooperativeLevel();
	if (FAILED(_hr))
	{
		// If the device was lost, do not render until we get it back
		if (D3DERR_DEVICELOST == _hr) {
			Sleep(33);
			return	FALSE;
		}

		// Check if the device is ready to be reset
		if (D3DERR_DEVICENOTRESET == _hr)
		{
			Reset(false);
		}
	}

	VERIFY(FALSE == g_bRendering);
	(REDevice->BeginScene());

	Clear();

	RCache.OnFrameBegin();
	g_bRendering = TRUE;
	return		TRUE;
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::End()
{
	VERIFY(b_is_Ready);
	g_bRendering = 	FALSE;
	// end scene
	RCache.OnFrameEnd();

    (REDevice->EndScene());

	CHK_DX(REDevice->Present( NULL, NULL, NULL, NULL ));

}

void CEditorRenderDevice::UpdateView()
{
// set camera matrix
	if (!Tools->UpdateCamera())
	{
		UI->CurrentView().m_Camera.GetView(mView);
	}
    RCache.set_xform_view(mView);
    mFullTransform.mul(mProject,mView);

// frustum culling sets
    ::Render->ViewBase.CreateFromMatrix(mFullTransform,FRUSTUM_P_ALL);
}

void CEditorRenderDevice::FrameMove()
{
	dwFrame++;

	// Timer
    float fPreviousFrameTime = Timer.GetElapsed_sec(); Timer.Start();	// previous frame
    fTimeDelta = 0.1f * fTimeDelta + 0.9f*fPreviousFrameTime;			// smooth random system activity - worst case ~7% error
    if (fTimeDelta>.1f) fTimeDelta=.1f;									// limit to 15fps minimum

    fTimeGlobal		= TimerGlobal.GetElapsed_sec(); //float(qTime)*CPU::cycles2seconds;
    dwTimeGlobal	= TimerGlobal.GetElapsed_ms	();	//u32((qTime*u64(1000))/CPU::cycles_per_second);
    dwTimeDelta		= iFloor(fTimeDelta*1000.f+0.5f);
    dwTimeContinual	= dwTimeGlobal;

	if (!Tools->UpdateCamera())
	{
		UI->CurrentView().m_Camera.Update(fTimeDelta);
	}

    // process objects
	seqFrame.Process(rp_Frame);
}

void CEditorRenderDevice::DP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 vBase, u32 pc)
{
	ref_shader S 			= m_CurrentShader?m_CurrentShader:m_WireShader;
    u32 dwRequired			= S->E[0]->passes.size();
    RCache.set_Geometry		(geom);
    for (u32 dwPass = 0; dwPass<dwRequired; dwPass++){
    	RCache.set_Shader	(S,dwPass);
		RCache.Render		(pt,vBase,pc);
    }
}

void CEditorRenderDevice::DIP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	ref_shader S 			= m_CurrentShader?m_CurrentShader:m_WireShader;
    u32 dwRequired			= S->E[0]->passes.size();
    RCache.set_Geometry		(geom);
    for (u32 dwPass = 0; dwPass<dwRequired; dwPass++){
    	RCache.set_Shader	(S,dwPass);
		RCache.Render		(pt,baseV,startV,countV,startI,PC);
    }
}

void CEditorRenderDevice::ReloadTextures()
{
	string_path Path = {};

	FS.update_path(Path, _game_textures_, "");
	FS.rescan_path(Path, true);

	Msg("* Reload textures...");
	UI->Resize();
}

void CEditorRenderDevice::UnloadTextures()
{
}

void CEditorRenderDevice::time_factor(float v)
{
	 Timer.time_factor(v);
	 TimerGlobal.time_factor(v);
}

void CEditorRenderDevice::CreateWindow()
{
	int DisplayX = GetSystemMetrics(SM_CXFULLSCREEN);
	int DisplayY = GetSystemMetrics(SM_CYFULLSCREEN);

	g_AppInfo.Window = SDL_CreateWindow("IX-Ray Editor", DisplayX, DisplayY, SDL_WINDOW_RESIZABLE);
}

void CEditorRenderDevice::DestryWindow()
{
	SDL_DestroyWindow(g_AppInfo.Window);
}