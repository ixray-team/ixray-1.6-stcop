//---------------------------------------------------------------------------
#include "stdafx.h"

#include "../xrEngine/GameFont.h"
#include <sal.h>
#include "ImageManager.h"
#include "EditorRenderBackend.h"
#include "ui_main.h"
#include "render.h"
#include "../Engine/XrGameMaterialLibraryEditors.h"
#include "../Layers/xrRender/ResourceManager.h"
#include "../Layers/xrRender/dxRenderDeviceRender.h"
#include "../../../xrCore/RenderTestPolicy.h"
#include "UI_ToolsCustom.h"
#include "SoundProcessor.h"
#include "device_win_custom.h"
CEditorRenderDevice* EDevice;
bool g_bIsEditor;

namespace
{
[[nodiscard]] bool UsesTiramisuEditorRenderer() noexcept
{
	return GetEditorRenderBackend().GetKind() ==
		EEditorRenderBackendKind::Tiramisu;
}

[[nodiscard]] bool UsesHiddenEditorTestWindow() noexcept
{
	return HasRenderCommandLineFlag(
		Core.Params ? xr_string_view(Core.Params) : xr_string_view(),
		"-editor-test-hidden"
	);
}
} // namespace

void CEditorRenderDevice::AddSeqFrame(pureFrame* f, bool mt)
{
	seqFrame.Add(f, REG_PRIORITY_LOW);
}
void CEditorRenderDevice::RemoveSeqFrame(pureFrame* f)
{
	seqFrame.Remove(f);
}

ENGINE_API xr_atomic_bool g_bRendering;
//---------------------------------------------------------------------------
#include <luabind/luabind.hpp>
void EditorFillPropTextureParams(STextureParams* ThisCall, const char* base_name, xr_vector<PropItem*>& items, PropValue::TOnChange OnChangeEvent);

static LPVOID __cdecl luabind_allocator(
	luabind::memory_allocation_function_parameter const,
	void const* const pointer,
	size_t const size
)
{
	if (!size)
	{
		LPVOID non_const_pointer = const_cast<LPVOID>(pointer);
		xr_free(non_const_pointer);
		return (nullptr);
	}

	if (!pointer)
	{
		return (Memory.mem_alloc(size));
	}

	LPVOID non_const_pointer = const_cast<LPVOID>(pointer);
	return (Memory.mem_realloc(non_const_pointer, size));
}

void setup_luabind_allocator()
{
	luabind::allocator = &luabind_allocator;
	luabind::allocator_parameter = nullptr;
}


CEditorRenderDevice::CEditorRenderDevice()
{
	RenderRadius = 400;
	psDeviceFlags.assign(rsStatistic | rsFilterLinear | rsFog | rsDrawGrid);
	// default initialization
	m_ScreenQuality = 1.f;

	TargetWidth = TargetHeight = 256;
	Width = Height = 256;
	mProject.identity();
	mFullTransform.identity();
	mView.identity();
	m_WireShader = nullptr;
	m_SelectionShader = nullptr;

	b_is_Ready = false;
	b_is_Active = false;

	// Engine flow-control
	fTimeDelta = 0;
	fTimeGlobal = 0;
	dwTimeDelta = 0;
	dwTimeGlobal = 0;

	dwFillMode = D3DFILL_SOLID;
	dwShadeMode = D3DSHADE_GOURAUD;

	m_CurrentShader = nullptr;
	// pSystemFont		= 0;

	fASPECT = 1.f;
	fFOV = 60.f;
	dwPrecacheFrame = 0;
	GameMaterialLibraryEditors = new XrGameMaterialLibraryEditors();
	PGMLib = GameMaterialLibraryEditors;

	DevicePtr = this;
	g_bIsEditor = true;

	setup_luabind_allocator();
	STextureParams::FillPropImpl = EditorFillPropTextureParams;
}

CEditorRenderDevice::~CEditorRenderDevice()
{
	VERIFY(!b_is_Ready);
	GameMaterialLibraryEditors = nullptr;
}

#include "../../../xrCore/API/xrAPI.h"
#include "../../../Layers/xrRender/dxRenderFactory.h"
#include "../../../Layers/xrRender/dxUIRender.h"
#include "../../../Layers/xrRender/dxDebugRender.h"
#include "../xrCore/appinfo.h"

typedef void __cdecl ttapi_Done_func(void);

void CEditorRenderDevice::Initialize()
{
	m_DefaultMat.set(1, 1, 1);

	if (!UsesTiramisuEditorRenderer())
	{
		RenderFactory = &RenderFactoryImpl;
		UIRender = &UIRenderImpl;
#ifdef DEBUG_DRAW
		DRender = &DebugRenderImpl;
#endif
	}

	// compiler shader
	string_path fn;
	FS.update_path(fn, _game_data_, "shaders_xrlc.xr");
	if (FS.exist(fn))
	{
		ShaderXRLC.Load(fn);
	}
	else
	{
		ELog.DlgMsg(mtInformation, "Can't find file '%s'", fn);
	}
	CreateWindow();


	// Startup shaders
	Create();

	if (!UsesTiramisuEditorRenderer())
	{
		::RImplementation.Initialize();
	}
	if (!UsesTiramisuEditorRenderer())
	{
		UIRenderImpl.CreateUIGeom();
	}

	if (UsesTiramisuEditorRenderer())
	{
		Width = EPrefs->start_w;
		Height = EPrefs->start_h;
		SDL_SetWindowSize(g_AppInfo.Window, Width, Height);
	}
	else
	{
		Resize(EPrefs->start_w, EPrefs->start_h, EPrefs->start_maximized);
	}

	SDL_GetWindowSizeInPixels(g_AppInfo.Window, &Width, &Height);
	SDL_GetWindowPosition(g_AppInfo.Window, &PosX, &PosY);

	if (EPrefs->start_maximized && !UsesHiddenEditorTestWindow())
	{
		SDL_MaximizeWindow(g_AppInfo.Window);
	}

	if (UsesHiddenEditorTestWindow())
	{
		// DXGI при полностью hidden HWND может прекратить выдавать кадры.
		// Test window остаётся non-focusable и уводится за рабочий стол, но
		// считается видимым для Vulkan/D3D12 swapchain.
		SDL_SetWindowPosition(g_AppInfo.Window, -32000, -32000);
		SDL_ShowWindow(g_AppInfo.Window);
#if _WINDOWS
		ShowWindow(GetHWND(), SW_SHOWNOACTIVATE);
#endif
	}
	else
	{
		SDL_ShowWindow(g_AppInfo.Window);
		SDL_RaiseWindow(g_AppInfo.Window);
	}


	if (psDeviceFlags.test(mtSound))
	{
		Device.seqFrameMT.Add(&SoundProcessor);
	}
	else
	{
		Device.seqFrame.Add(&SoundProcessor);
	}
}

void CEditorRenderDevice::ShutDown()
{
	if (!UsesTiramisuEditorRenderer())
	{
		UIRenderImpl.DestroyUIGeom();
	}
	if (!UsesTiramisuEditorRenderer())
	{
		::RImplementation.ShutDown();
	}

	ShaderXRLC.Unload();

	// destroy context
	Destroy();

	if (psDeviceFlags.test(mtSound))
	{
		Device.seqFrameMT.Remove(&SoundProcessor);
	}
	else
	{
		Device.seqFrame.Remove(&SoundProcessor);
	}
}

void CEditorRenderDevice::InitTimer()
{
	Timer_MM_Delta = 0;
	{
		u32 time_mm = clock();
		while (clock() == time_mm)
			; // wait for next tick
		u32 time_system = clock();
		u32 time_local = TimerAsync();
		Timer_MM_Delta = time_system - time_local;
	}
}

void CEditorRenderDevice::Clear()
{
	float ClearColor[4] = {};

	if (EPrefs)
	{
		ClearColor[0] = color_get_R(EPrefs->scene_clear_color) / 255.f;
		ClearColor[1] = color_get_G(EPrefs->scene_clear_color) / 255.f;
		ClearColor[2] = color_get_B(EPrefs->scene_clear_color) / 255.f;
		ClearColor[3] = color_get_A(EPrefs->scene_clear_color) / 255.f;
	}

	GRHI->ClearDepthStencil(GRHI->GetDepthStencilView(), ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1, 0);
	GRHI->ClearTarget(GRHI->GetRenderTargetView(0), ClearColor);
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::RenderNearer(float n)
{
	mProject._43 = m_fNearer - n;
	RCache.set_xform_project(mProject);
}
void CEditorRenderDevice::ResetNearer()
{
	mProject._43 = m_fNearer;
	RCache.set_xform_project(mProject);
}
//---------------------------------------------------------------------------
bool CEditorRenderDevice::Create()
{
	if (b_is_Ready)
	{
		return false;
	}
	psDeviceFlags.set(rsVSync, true);

	TimerGlobal.Start();
	// Statistic = EStatistic;
	ELog.Msg(mtInformation, "Starting RENDER device...");


	// HW.CreateDevice		(m_hWnd, true);
	if (UI)
	{
		HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
		string_path ini_path;
		string_path ini_name;
		xr_strcpy(ini_name, UI->EditorName());
		xr_strcat(ini_name, "_imgui.ini");
		FS.update_path(ini_path, "$app_data_root$", ini_name);

		if (!FS.exist(ini_path))
		{
			UI->ResetUI();
		}

		if (!UsesTiramisuEditorRenderer())
		{
			InitRenderDeviceEditor();
		}
		UI->Initialize(hwnd, ini_path);
	}

	// after creation
	dwFrame = 0;
	if (UsesTiramisuEditorRenderer())
	{
		Resources = nullptr;
		g_FontManager = new CFontManager();
		b_is_Ready = true;
		UI->OnDeviceCreate();
		InitWindowStyle();
		::Render->create();
		R_ASSERT2(
			GetEditorRenderBackend().InitializeRendererResources(),
			"xrRenderTiramisu failed to initialize editor resources"
		);
		g_FontManager->InitializeFonts();
		Statistic = new CEStats();
		ELog.Msg(
			mtInformation,
			"Tiramisu editor device initialized without legacy D3D11/CRHI"
		);
		return true;
	}

	string_path sh;
	FS.update_path(sh, _game_data_, "shaders.xr");

	IReader* F = nullptr;
	if (FS.exist(sh))
	{
		F = FS.r_open(nullptr, sh);
	}
	Resources = new CResourceManager();

	// if build options - load textures immediately
	if (strstr(Core.Params, "-build") || strstr(Core.Params, "-ebuild"))
	{
		EDevice->Resources->DeferredLoad(false);
	}

	g_FontManager = new CFontManager();

	_Create(F);
	FS.r_close(F);

	if (!UsesTiramisuEditorRenderer() && ::Render != &::RImplementation)
	{
		::RImplementation.create();
	}
	else if (UsesTiramisuEditorRenderer())
	{
		Msg("* LevelEditor: local legacy RImplementation runtime is disabled");
	}
	::Render->create();

	g_FontManager->InitializeFonts();

	Statistic = new CEStats();

	ELog.Msg(mtInformation, "D3D: initialized");

	return true;
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::Destroy()
{
	if (!b_is_Ready)
	{
		return;
	}
	ELog.Msg(mtInformation, "Destroying Direct3D...");
	if (UsesTiramisuEditorRenderer())
	{
		UI->OnDeviceDestroy();
		b_is_Ready = false;
		UI->Destroy();
		::Render->destroy();
		GetEditorRenderBackend().FinalizeRendererShutdown();
		ELog.Msg(
			mtInformation,
			"Tiramisu editor device cleared without legacy D3D11/CRHI"
		);
		return;
	}

	::Render->destroy();
	if (!UsesTiramisuEditorRenderer() && ::Render != &::RImplementation)
	{
		::RImplementation.destroy();
	}
	// before destroy
	_Destroy(false);

	xr_delete(Resources);

	UI->Destroy();

	ELog.Msg(mtInformation, "D3D: device cleared");
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::_SetupStates()
{
	// Caps.Update();
#if 0
	for (u32 i=0; i<Caps.raster.dwStages; i++){
		float fBias = -1.f;
		CHK_DX(REDevice->SetSamplerState( i, D3DSAMP_MIPMAPLODBIAS, *((LPDWORD) (&fBias))));
	}
	EDevice->SetRS(D3DRS_DITHERENABLE,	true				);
    EDevice->SetRS(D3DRS_COLORVERTEX,		true				);
    EDevice->SetRS(D3DRS_STENCILENABLE,	false				);
    EDevice->SetRS(D3DRS_ZENABLE,			true				);
    EDevice->SetRS(D3DRS_SHADEMODE,		D3DSHADE_GOURAUD	);
	EDevice->SetRS(D3DRS_CULLMODE,		D3DCULL_CCW			);
	EDevice->SetRS(D3DRS_ALPHAFUNC,		D3DCMP_GREATER		);
	EDevice->SetRS(D3DRS_LOCALVIEWER,		true				);
    EDevice->SetRS(D3DRS_NORMALIZENORMALS,true				);

	EDevice->SetRS(D3DRS_DIFFUSEMATERIALSOURCE, D3DMCS_MATERIAL);
	EDevice->SetRS(D3DRS_SPECULARMATERIALSOURCE,D3DMCS_MATERIAL);
	EDevice->SetRS(D3DRS_AMBIENTMATERIALSOURCE, D3DMCS_MATERIAL);
	EDevice->SetRS(D3DRS_EMISSIVEMATERIALSOURCE,D3DMCS_COLOR1	);
#endif
	ResetMaterial();
}
//---------------------------------------------------------------------------
void CEditorRenderDevice::_Create(IReader* F)
{
	b_is_Ready = true;

	// General Render States
	_SetupStates();

	RCache.OnDeviceCreate();
	Resources->OnDeviceCreate(F);
	::RImplementation.OnDeviceCreate();

	m_WireShader.create("editor\\wire");
	m_SelectionShader.create("editor\\selection");
	ShaderTL.create("editor\\default");

	// signal another objects
	UI->OnDeviceCreate();
	UIChooseForm::SetNullTexture(UI->LoadTexture("ed\\ed_nodata"));
	GUIManager->SearchIcon = UI->LoadTexture("ed\\content_browser_search");

	EDevice->InitWindowStyle();
}

void CEditorRenderDevice::_Destroy(bool bKeepTextures)
{
	b_is_Ready = false;
	m_CurrentShader = nullptr;

	UI->OnDeviceDestroy();

	m_WireShader.destroy();
	m_SelectionShader.destroy();
	ShaderTL.destroy();
	::RImplementation.Models->OnDeviceDestroy();

	Resources->OnDeviceDestroy(bKeepTextures);

	RCache.OnDeviceDestroy();
	::RImplementation.OnDeviceDestroy();
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::Resize(int w, int h, bool maximized)
{
	Width = w;
	Height = h;
	if (UsesTiramisuEditorRenderer())
	{
		SDL_SetWindowSize(g_AppInfo.Window, Width, Height);
		UI->RedrawScene();
		return;
	}

	Reset(false);
	UI->RedrawScene();
}

void CEditorRenderDevice::Reset(bool)
{
	if (UsesTiramisuEditorRenderer())
	{
		SDL_SetWindowSize(g_AppInfo.Window, Width, Height);
		UI->RedrawScene();
		return;
	}
	u32 tm_start = TimerAsync();
	Resources->reset_begin();
	if (!UsesTiramisuEditorRenderer() && ::Render != &::RImplementation)
	{
		::RImplementation.reset_begin();
	}
	Resources->DeferredUnload();
	UI->ResetBegin();

	Memory.mem_compact();
	ResizeBuffers(Width, Height);
	SDL_SetWindowSize(g_AppInfo.Window, Width, Height);

	Resources->reset_end();
	if (!UsesTiramisuEditorRenderer() && ::Render != &::RImplementation)
	{
		::RImplementation.reset_end();
	}
	Resources->DeferredUpload();

	UI->ResetEnd(RDevice);
	_SetupStates();

	UIChooseForm::SetNullTexture(UI->LoadTexture("ed\\ed_nodata"));
	GUIManager->SearchIcon = UI->LoadTexture("ed\\content_browser_search");
	u32 tm_end = TimerAsync();
	Msg("*** RESET [%d ms]", tm_end - tm_start);
}

void CEditorRenderDevice::Reset(IReader* F, bool bKeepTextures)
{
	CTimer tm;
	tm.Start();
	_Destroy(bKeepTextures);
	_Create(F);
	Msg("*** RESET [%d ms]", tm.GetElapsed_ms());
}

void CEditorRenderDevice::MaximizedWindow()
{
	auto hwnd = GetHWND();
	if (EDevice->isZoomed)
	{
		ResoreWindow(false);
		return;
	}

	EDevice->isZoomed = true;

	SendMessage(hwnd, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
}

void CEditorRenderDevice::ResoreWindow(bool moving)
{
	SendMessageW(GetHWND(), WM_SYSCOMMAND, SC_RESTORE, 0);

	WINDOWPLACEMENT wp{};
	wp.length = sizeof(wp);

	if (GetWindowPlacement(GetHWND(), &wp)) // analog EDevice->NormalWinSizeSaved
	{
		RECT r = wp.rcNormalPosition;
		MoveWindow(GetHWND(), r.left, r.top, r.right - r.left, r.bottom - r.top, true);
	}

	EDevice->isZoomed = false;
}

bool CEditorRenderDevice::Begin()
{
	VERIFY(b_is_Ready);
	mFullTransform_saved = mFullTransform;
	mProject_saved = mProject;
	mView_saved = mView;
	vCameraPosition_saved = vCameraPosition;

	VERIFY(FALSE == g_bRendering);

	Clear();

	RCache.OnFrameBegin();
	g_bRendering = true;
	return true;
}

//---------------------------------------------------------------------------
void CEditorRenderDevice::End()
{
	VERIFY(b_is_Ready);
	g_bRendering = false;
	// end scene
	RCache.OnFrameEnd();
	if (UI && UI->UsesExternalMainPresentation())
	{
		// Legacy editor renderer завершил offscreen-команды в RCache.OnFrameEnd.
		// Window swapchain принадлежит внешнему Tiramisu backend, поэтому здесь
		// выполняется только его единственный Present без вызовов D3D9 API.
		UI->PresentMainFrame();
	}
	else
	{
		GRHI->Present();
	}
}

void CEditorRenderDevice::UpdateView()
{
	// set camera matrix
	if (!Tools->UpdateCamera())
	{
		UI->CurrentView().m_Camera.GetView(mView);
	}
	RCache.set_xform_view(mView);
	mFullTransform.mul(mProject, mView);

	// frustum culling sets
	::Render->ViewBase.CreateFromMatrix(mFullTransform, FRUSTUM_P_ALL);
}

void CEditorRenderDevice::FrameMove()
{
	dwFrame++;

	static const FRenderDeterministicTestPolicy DeterministicTest =
		ResolveRenderDeterministicTestPolicy(
			Core.Params ? Core.Params : ""
		);
	if (DeterministicTest.Enabled)
	{
		const u32 PreviousGlobal = dwTimeGlobal;
		fTimeDelta = DeterministicTest.FixedDeltaSeconds;
		fTimeGlobal = static_cast<float>(dwFrame - 1) * fTimeDelta;
		dwTimeGlobal = static_cast<u32>(
			fTimeGlobal * 1000.0f + 0.5f
		);
		dwTimeDelta = dwTimeGlobal - PreviousGlobal;
		dwTimeContinual = dwTimeGlobal;
	}
	else
	{
		// Timer
		const float fPreviousFrameTime = Timer.GetElapsed_sec();
		Timer.Start();
		fTimeDelta = 0.1f * fTimeDelta + 0.9f * fPreviousFrameTime;
		if (fTimeDelta > .1f)
		{
			fTimeDelta = .1f;
		}

		fTimeGlobal = TimerGlobal.GetElapsed_sec();
		dwTimeGlobal = TimerGlobal.GetElapsed_ms();
		dwTimeDelta = iFloor(fTimeDelta * 1000.f + 0.5f);
		dwTimeContinual = dwTimeGlobal;
	}

	if (!Tools->UpdateCamera())
	{
		UI->CurrentView().m_Camera.Update(fTimeDelta);
	}

	// process objects
	seqFrame.Process<&pureFrame::OnFrame>();
}

#if _WINDOWS

SDL_HitTestResult SDLCALL HitTest(
	SDL_Window* window,
	const SDL_Point* pt,
	void* data
)
{
	bool isZoomed = *(bool*)data;

	if (isZoomed)
	{
		return SDL_HITTEST_NORMAL;
	}

	int w, h;
	SDL_GetWindowSize(window, &w, &h);

	const int border = 6;

	// TOP LEFT
	if (pt->x < border && pt->y < border)
	{
		return SDL_HITTEST_RESIZE_TOPLEFT;
	}

	// TOP RIGHT
	if (pt->x > w - border && pt->y < border)
	{
		return SDL_HITTEST_RESIZE_TOPRIGHT;
	}

	// BOTTOM LEFT
	if (pt->x < border && pt->y > h - border)
	{
		return SDL_HITTEST_RESIZE_BOTTOMLEFT;
	}

	// BOTTOM RIGHT
	if (pt->x > w - border && pt->y > h - border)
	{
		return SDL_HITTEST_RESIZE_BOTTOMRIGHT;
	}

	// TOP
	if (pt->y < border)
	{
		return SDL_HITTEST_RESIZE_TOP;
	}

	// BOTTOM
	if (pt->y > h - border)
	{
		return SDL_HITTEST_RESIZE_BOTTOM;
	}

	// LEFT
	if (pt->x < border)
	{
		return SDL_HITTEST_RESIZE_LEFT;
	}

	// RIGHT
	if (pt->x > w - border)
	{
		return SDL_HITTEST_RESIZE_RIGHT;
	}

	// title bar area
	// if (pt->y < 32)
	//	return SDL_HITTEST_DRAGGABLE;

	return SDL_HITTEST_NORMAL;
}
#endif

void CEditorRenderDevice::InitWindowStyle()
{
	UI->InitWindowIcons();

#if _WINDOWS
	SDL_SetWindowBordered(g_AppInfo.Window, false);

	win_chezze_layer(GetHWND());
	SDL_SetWindowHitTest(g_AppInfo.Window, HitTest, &EDevice->isZoomed);

	if (!UsesHiddenEditorTestWindow())
	{
		SetFocus(EDevice->GetHWND());
		SetForegroundWindow(EDevice->GetHWND());
	}
#else
	SDL_SetWindowResizable(g_AppInfo.Window, SDL_TRUE);
	SDL_SetWindowHitTest(g_AppInfo.Window, HitTestCallback, 0);
#endif
}

void CEditorRenderDevice::SetShader(ref_shader sh)
{
	m_CurrentShader = sh;
	EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);
}

void CEditorRenderDevice::DP(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 vBase, u32 pc)
{
	ref_shader S = m_CurrentShader ? m_CurrentShader : m_WireShader;
	u32 dwRequired = S->E[0]->passes.size();

	for (u32 dwPass = 0; dwPass < dwRequired; dwPass++)
	{
		RCache.set_Shader(S, dwPass);
		EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);
		RCache.set_Geometry(geom);
		RCache.Render(pt, vBase, pc);
	}
}

void CEditorRenderDevice::DIP(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	ref_shader S = m_CurrentShader ? m_CurrentShader : m_WireShader;
	u32 dwRequired = S->E[0]->passes.size();
	RCache.set_Geometry(geom);

	for (u32 dwPass = 0; dwPass < dwRequired; dwPass++)
	{
		RCache.set_Shader(S, dwPass);
		EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);
		RCache.Render(pt, baseV, startV, countV, startI, PC);
	}
}

void CEditorRenderDevice::ReloadTextures()
{
	// string_path Path = {};

	// FS.update_path(Path, _game_textures_, "");
	// FS.rescan_path(Path, true);

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

HWND CEditorRenderDevice::GetHWND() const
{
	HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
	return hwnd;
}

void CEditorRenderDevice::CreateWindow()
{
	int DisplayX = GetSystemMetrics(SM_CXFULLSCREEN);
	int DisplayY = GetSystemMetrics(SM_CYFULLSCREEN);

	SDL_WindowFlags WindowFlags = SDL_WINDOW_HIDDEN | SDL_WINDOW_RESIZABLE;
	if (UsesHiddenEditorTestWindow())
	{
		WindowFlags |= SDL_WINDOW_NOT_FOCUSABLE;
	}
	g_AppInfo.Window = SDL_CreateWindow(
		"IX-Ray Editor",
		DisplayX,
		DisplayY,
		WindowFlags
	);
#if _WINDOWS
	if (!UsesHiddenEditorTestWindow())
	{
		SetForegroundWindow(EDevice->GetHWND());
	}
#endif
}

void CEditorRenderDevice::DestryWindow()
{
	SDL_DestroyWindow(g_AppInfo.Window);
}
