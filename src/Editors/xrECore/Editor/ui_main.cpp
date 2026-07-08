#include "stdafx.h"
#include "../xrEngine/xr_input.h"
#include "../xrEngine/IGame_Actor.h"
#include "UI_ToolsCustom.h"

#include "ui_main.h"
#include "D3DUtils.h"
#include "SoundManager.h"
#include "../Layers/xrRender/PSLibrary.h"
#include "../Layers/xrRender/dxRenderDeviceRender.h"

#include "UIIConPicker.h"
#include "UIEditLightAnim.h"
#include "UIImageEditorForm.h"
#include "UISoundEditorForm.h"
#include "UIMinimapEditorForm.h"
#include "UIWeatherPropForm.h"
#include "UILogForm.h"
#include "../xrEngine/gamefont.h"
#include "../XrEngine/XR_IOConsole.h"

#include "imgui_EditorEx_Icons.h"

#define TRelease(x) if (x) x->pSurface->Release()

ECORE_API extern bool bIsLevelEditor;
namespace ImGui
{
	XREUI_API ImFont* LightFont;
	XREUI_API ImFont* RegularFont;
	XREUI_API ImFont* MediumFont;
	XREUI_API ImFont* BoldFont;
}

TUI* UI = nullptr;

TUI::TUI()
{
	m_HConsole = nullptr;
	UI				= this;
	m_AppClosed = false;
	m_bAppActive 	= false;
	m_bReady 		= false;
	bNeedAbort   	= false;

	m_CurrentRStart.set(0,0,0);
	m_CurrentRDir.set(0,0,0);

	m_Flags.assign	(flResize);

	m_Pivot.set		( 0, 0, 0 );

	m_MouseCaptured = false;
	m_MouseMultiClickCaptured = false;
	m_SelectionRect = false;
	bMouseInUse		= false;

	m_bHintShowing	= false;
	m_LastHint		= "";

	int DisplayX = GetSystemMetrics(SM_CXFULLSCREEN);
	int DisplayY = GetSystemMetrics(SM_CYFULLSCREEN);

	Viewport& MainView = Views[0];
	ViewID = 0;

	m_Size.set(DisplayX, DisplayY);

	GUIManager = this;
}
//---------------------------------------------------------------------------
TUI::~TUI()
{
	VERIFY(m_ProgressItems.size()==0);
	VERIFY(m_EditorState.size()==0);

	TRelease(m_HeaderLogo);
	TRelease(m_WinMin);
	TRelease(m_WinRes);
	TRelease(m_WinMax);
	TRelease(m_WinClose);
}

ImTextureID TUI::LoadTexture(const char* Texture) const
{
	if (EDevice == nullptr || EDevice->Resources == nullptr)
	{
		return nullptr;
	}

	if (TextureStack.contains(Texture))
	{
		return TextureStack[Texture]->get_SRView()->GetRawSRV();
	}

	TextureStack[Texture] = EDevice->Resources->_CreateTexture(Texture);
	ref_texture& Tex = TextureStack[Texture];

	if (Tex->pSurface == nullptr)
	{
		Tex->apply_load(0);
	}

	return (void*)Tex->get_SRView()->GetRawSRV();
}

void TUI::OnDeviceCreate()
{
	DU_impl.OnDeviceCreate();
}

void TUI::OnDeviceDestroy()
{
	DU_impl.OnDeviceDestroy();
	TextureStack.clear();
}

bool TUI::IsModified()
{
	return ExecCommand(COMMAND_CHECK_MODIFIED);
}
//---------------------------------------------------------------------------

void TUI::EnableSelectionRect( bool flag ){
	m_SelectionRect = flag;
	m_SelEnd.x = m_SelStart.x = 0;
	m_SelEnd.y = m_SelStart.y = 0;
}

void TUI::UpdateSelectionRect( const Ivector2& from, const Ivector2& to ){
	m_SelStart.set(from);
	m_SelEnd.set(to);
}

bool  TUI::KeyDown (WORD Key, TShiftState Shift)
{
	if (!m_bReady) return false;
	if (Console->bVisible)
	{
		if (Key == 0xC0)
		{
			Console->Hide();
		}
		return true;
	}
   
	if (Key == 0xC0)
	{
		Console->Show();
		return true;
	}
//	m_ShiftState = Shift;
//	Log("Dn  ",Shift.Contains(ssShift)?"1":"0");
	if (UI->CurrentView().m_Camera.KeyDown(Key,Shift)) return true;
	return Tools->KeyDown(Key, Shift);
}

bool  TUI::KeyUp   (WORD Key, TShiftState Shift)
{
	if (!m_bReady) return false;
//	m_ShiftState = Shift;
	if (UI->CurrentView().m_Camera.KeyUp(Key,Shift)) return true;
	return Tools->KeyUp(Key, Shift);
}

bool  TUI::KeyPress(WORD Key, TShiftState Shift)
{
	if (!m_bReady) return false;
	return Tools->KeyPress(Key, Shift);
}
//----------------------------------------------------

void TUI::MousePress(TShiftState Shift, int X, int Y)
{
	if (!m_bReady) return;
	if (m_MouseCaptured) return;

	bMouseInUse = true;

	m_ShiftState = Shift;

	// camera activate
	if(!UI->CurrentView().m_Camera.MoveStart(m_ShiftState))
	{
		if (Tools->Pick(Shift)) 
			return;

		if( !m_MouseCaptured )
		{
			if (Tools->HiddenMode())
			{
				IR_GetMousePosScreen(m_StartCpH);
				m_DeltaCpH.set(0, 0);
			}
			else
			{
				m_CurrentCp = GetRenderMousePosition();
				m_StartCp = m_CurrentCp;
				UI->CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp );
				m_StartRStart = m_CurrentRStart;
				m_StartRDir = m_CurrentRDir;
			}
		   
			if(Tools->MouseStart(m_ShiftState))
			{
				if(Tools->HiddenMode()) ShowCursor( false );
				m_MouseCaptured = true;
			}
		}
	}
	RedrawScene();
}

void TUI::MouseRelease(TShiftState Shift, int X, int Y)
{
	if (!m_bReady) return;

	m_ShiftState = Shift;

	if( UI->CurrentView().m_Camera.IsMoving() ){
		if (UI->CurrentView().m_Camera.MoveEnd(m_ShiftState)) bMouseInUse = false;
	}else{
		bMouseInUse = false;
		if( m_MouseCaptured ){
			if( !Tools->HiddenMode() ){
				m_CurrentCp = GetRenderMousePosition();
				UI->CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart,m_CurrentRDir,m_CurrentCp );
			}
			bool bIsHiddenMode = Tools->HiddenMode();
			if( Tools->MouseEnd(m_ShiftState) ){
				if(bIsHiddenMode){
					SetCursorPos(m_StartCpH.x,m_StartCpH.y);
					ShowCursor( true );
				}
				m_MouseCaptured = false;
			}
		}
	}
	// update tools (change action)
	Tools->OnFrame	();
	RedrawScene		();
}
//----------------------------------------------------
void TUI::MouseMove(TShiftState Shift, int X, int Y)
{
	if (!m_bReady) return;
	m_ShiftState = Shift;
}
//----------------------------------------------------
void TUI::IR_OnMouseMove(int x, int y)
{
	if (!m_bReady) 
		return;

	bool bRayUpdated = false;

	if (!UI->CurrentView().m_Camera.Process(m_ShiftState,x,y))
	{
		if( m_MouseCaptured || m_MouseMultiClickCaptured )
		{
			if( Tools->HiddenMode() )
			{
				m_DeltaCpH.set(x,y);
				if( m_DeltaCpH.x || m_DeltaCpH.y )
				{
					Tools->MouseMove(m_ShiftState);
				}
			}
			else
			{
				m_CurrentCp = GetRenderMousePosition();
				UI->CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart,m_CurrentRDir,m_CurrentCp);
				Tools->MouseMove(m_ShiftState);
			}

			RedrawScene();
			bRayUpdated = true;
		}
	}

	if (!bRayUpdated)
	{
		m_CurrentCp = GetRenderMousePosition();
		UI->CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp);
	}
}
//---------------------------------------------------------------------------

void TUI::OnAppActivate()
{
	m_bAppActive = true;
	if (!m_bReady)return;
	if (pInput){
		m_ShiftState = ssNone;
		pInput->OnAppActivate();
		EDevice->seqAppActivate.Process	(rp_AppActivate);
	}
}
//---------------------------------------------------------------------------

void TUI::OnAppDeactivate()
{
	m_bAppActive = false;
	if (!m_bReady)return;
	if (pInput){
		pInput->OnAppDeactivate();
		m_ShiftState = ssNone;
		EDevice->seqAppDeactivate.Process(rp_AppDeactivate);
	}
	HideHint();
}
//---------------------------------------------------------------------------

bool TUI::ShowHint(const AStringVec& SS)
{
	VERIFY(m_bReady);

	if (!SS.empty() && ImGui::BeginTooltip())
	{
		for (const xr_string& Hint : SS)
		{
			ImGui::Text(Hint.c_str());
		}
		ImGui::EndTooltip();
	}

	//not_implemented();
	return m_bHintShowing;
}
//---------------------------------------------------------------------------

void TUI::HideHint()
{
	VERIFY(m_bReady);
	m_bHintShowing = false;
}
//---------------------------------------------------------------------------

void TUI::ShowHint()
{
	VERIFY(m_bReady);
	GetCursorPos(&m_HintPoint);
	AStringVec SS;
	Tools->OnShowHint(SS);

	if (!ShowHint(SS)) 
		HideHint();
}

//---------------------------------------------------------------------------

#include "..\xrEngine\IGame_Persistent.h"
void TUI::PrepareRedraw()
{
	VERIFY(m_bReady);
	if (m_Flags.is(flResize)) 			RealResize();
// set render state
	EDevice->SetRS(D3DRS_TEXTUREFACTOR,	0xffffffff);
	// fog
	u32 fog_color;
	float fog_start, fog_end;
	Tools->GetCurrentFog	(fog_color, fog_start, fog_end);

	EDevice->SetRS( D3DRS_FOGCOLOR,		fog_color			);
	EDevice->SetRS( D3DRS_RANGEFOGENABLE,	false				);
	if (Caps.bTableFog)	{
		EDevice->SetRS( D3DRS_FOGTABLEMODE,	D3DFOG_LINEAR 	);
		EDevice->SetRS( D3DRS_FOGVERTEXMODE,	D3DFOG_NONE	 	);
	} else {
		EDevice->SetRS( D3DRS_FOGTABLEMODE,	D3DFOG_NONE	 	);
		EDevice->SetRS( D3DRS_FOGVERTEXMODE,	D3DFOG_LINEAR	);
	}
	EDevice->SetRS( D3DRS_FOGSTART,	*(DWORD *)(&fog_start)	);
	EDevice->SetRS( D3DRS_FOGEND,		*(DWORD *)(&fog_end)	);
	// filter
	for (u32 k=0; k<Caps.raster.dwStages; k++){
		if( psDeviceFlags.is(rsFilterLinear)){
			EDevice->SetSS(k,D3DSAMP_MAGFILTER,D3DTEXF_LINEAR);
			EDevice->SetSS(k,D3DSAMP_MINFILTER,D3DTEXF_LINEAR);
			EDevice->SetSS(k,D3DSAMP_MIPFILTER,D3DTEXF_LINEAR);
		} else {
			EDevice->SetSS(k,D3DSAMP_MAGFILTER,D3DTEXF_POINT);
			EDevice->SetSS(k,D3DSAMP_MINFILTER,D3DTEXF_POINT);
			EDevice->SetSS(k,D3DSAMP_MIPFILTER,D3DTEXF_POINT);
		}
	}
	// ligthing
	EDevice->SetRS(D3DRS_AMBIENT,0xFFFFFFFF);

	EDevice->SetRS			(D3DRS_FILLMODE, EDevice->dwFillMode);
	EDevice->SetRS			(D3DRS_SHADEMODE,EDevice->dwShadeMode);

	RCache.set_xform_world	(Fidentity);
}

void TUI::Invalidate()
{
	UI->RT.destroy();
	UI->RT.create("$user$rt_color", UI->GetRenderWidth(), UI->GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
}

extern ENGINE_API xr_atomic_bool g_bRendering;
void TUI::Redraw()
{
	PrepareRedraw();

	Viewport& View = CurrentView();

	if (u32(View.RTSize.x * EDevice->m_ScreenQuality) != EDevice->TargetWidth || u32(View.RTSize.y * EDevice->m_ScreenQuality) != EDevice->TargetHeight || !RT->pSurface)
	{
		if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
		{
			EDevice->TargetWidth = View.RTSize.x * EDevice->m_ScreenQuality;
			EDevice->TargetHeight = View.RTSize.y * EDevice->m_ScreenQuality;

			RT.destroy();
			RTCopy.destroy();
			ZB.destroy();
			View.RTFreez.destroy();

			RTPostion.destroy();
			RTNormal.destroy();
			RTDiffuse.destroy();

			RTPostion.create("$user$position", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::R16G16B16A16_FLOAT);
			RTNormal.create("$user$normal", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::R16G16B16A16_FLOAT);
			RTDiffuse.create("$user$diffuse", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8A8_UNORM);

			RT.create("$user$rt_color", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
			View.RTFreez.create(("$user$rt_freez" + xr_string::ToString((u32)UI->ViewID)).c_str(), GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::B8G8R8X8_UNORM);
			RTCopy.create("$user$rt_color_copy", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);

			ZB.create("$user$rt_depth", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::D24_UNORM_S8_UINT);

			m_Flags.set(flRedraw, true);

			EDevice->m_fNearer = EDevice->mProject._43;
			HalfTarget.x = float(View.RTSize.x) * 0.5f;
			HalfTarget.y = float(View.RTSize.y) * 0.5f;
			EDevice->fASPECT = float(HalfTarget.y) / float(HalfTarget.x);

			EDevice->seqDeviceReset.Process(rp_DeviceReset);
			EDevice->seqResolutionChanged.Process(rp_ScreenResolutionChanged);
			RCache.set_xform_project(EDevice->mProject);
			RCache.set_xform_world(Fidentity);
		}
		else
		{
			// Soft render update when resizing window
			HalfTarget.x = float(View.RTSize.x) * 0.5f;
			HalfTarget.y = float(View.RTSize.y) * 0.5f;
			EDevice->fASPECT = float(HalfTarget.y) / float(HalfTarget.x);
			m_Flags.set(flRedraw, true);
		}
	}

	if (!UI->IsPlayInEditor())
	{
		EDevice->mProject.build_projection(deg2rad(EDevice->fFOV), EDevice->fASPECT, View.m_Camera.m_Znear, View.m_Camera.m_Zfar);
	}

	if (EDevice->Begin())
	{
		if (psDeviceFlags.is(rsRenderRealTime))
			m_Flags.set(flRedraw, true);
		if (m_Flags.is(flRedraw) || UI->IsPlayInEditor())
		{
			m_Flags.set(flRedraw, false);

			RCache.set_RT(RTNormal->pRT, 0);
			RCache.set_RT(RTDiffuse->pRT, 1);
			RCache.set_RT(RTPostion->pRT, 2);

			GRHI->SetDepthStencilView(nullptr);

			CHK_DX(REDevice->Clear(0, nullptr, D3DCLEAR_TARGET, 0x0, 1, 0));

			RCache.set_RT(RT->pRT);
			GRHI->SetDepthStencilView(ZB->pZRT);

			EDevice->Clear();

			RCache.set_RT(RTDiffuse->pRT, 1);
			RCache.set_RT(RTNormal->pRT, 2);
			RCache.set_RT(RTPostion->pRT, 3);

			RCache.set_Stencil(true, D3DCMP_ALWAYS, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			EDevice->UpdateView();
			EDevice->ResetMaterial();

			Tools->RenderEnvironment();

			for (u32 k = 0; k < Caps.raster.dwStages; k++)
			{
				if (psDeviceFlags.is(rsFilterLinear)) {
					EDevice->SetSS(k, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
					EDevice->SetSS(k, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
					EDevice->SetSS(k, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);
				}
				else {
					EDevice->SetSS(k, D3DSAMP_MAGFILTER, D3DTEXF_POINT);
					EDevice->SetSS(k, D3DSAMP_MINFILTER, D3DTEXF_POINT);
					EDevice->SetSS(k, D3DSAMP_MIPFILTER, D3DTEXF_POINT);
				}
			}

			// draw grid
			if (psDeviceFlags.is(rsDrawGrid))
			{
				DU_impl.DrawGrid();
				DU_impl.DrawPivot(m_Pivot);
			}

			Tools->Render();

			// draw selection rect
			if (m_SelectionRect)
			{
				DU_impl.DrawSelectionRect(m_SelStart, m_SelEnd);
			}

			// draw axis
			if (psDeviceFlags.test(rsDrawAxis) && !psDeviceFlags.test(rsDisableAxisCube))
				DU_impl.DrawAxis(UI->CurrentView().m_Camera.GetTransform());


			EDevice->Statistic->RenderDUMP_RT.End();
			EDevice->Statistic->Show();
			EDevice->SetRS(D3DRS_FILLMODE, D3DFILL_SOLID);

			g_FontManager->Render();

			EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);
			EDevice->seqRender.Process(rp_Render);

			if (g_pGamePersistent->OnRenderPPUI_query())
			{
				g_pGamePersistent->OnRenderPPUI_main();
			}

			RCache.set_RT(nullptr, 1);
			RCache.set_RT(nullptr, 2);
			RCache.set_RT(nullptr, 3);

			RCache.set_RT(RSwapchainTarget);
			GRHI->SetDepthStencilView(RDepth);
			
			CHK_DX(REDevice->Clear(0, nullptr, D3DCLEAR_TARGET, D3DCOLOR_RGBA(0, 0, 0, 0), 1, 0));

			RDevice->SetTextureStageState(0, D3DTSS_TEXTURETRANSFORMFLAGS, 0);
			RDevice->SetTextureStageState(0, D3DTSS_TEXCOORDINDEX, 0);
			RDevice->SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
			RDevice->SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);
		}

		EDevice->SetRS(D3DRS_FILLMODE, D3DFILL_SOLID);
		g_bRendering = false;

		// end draw
		UI->BeginFrame();

		Draw();

		UI->EndFrame();
		EDevice->End();
		UI->MDIUpdate();
	}

	for (auto& Callback : CommandList[TUI::ECommandListID::CurrentFrame])
	{
		Callback();
	}

	CommandList[TUI::ECommandListID::CurrentFrame].clear();
	std::swap(CommandList[TUI::ECommandListID::CurrentFrame], CommandList[TUI::ECommandListID::NextFrame]);
}

void TUI::RealResize()
{
	m_Flags.set			(flResize,false);
	if(m_Size.x&& m_Size.y)
	EDevice->Resize(m_Size.x, m_Size.y,m_Size_Maximize);
	ExecCommand			(COMMAND_UPDATE_PROPERTIES);
}
void TUI::RealUpdateScene()
{
	Tools->UpdateProperties	(false);
	m_Flags.set			(flUpdateScene,false);
}
void TUI::RealRedrawScene()
{

	Redraw				();         
}
void TUI::OnFrame()
{
	EDevice->FrameMove	();
	SndLib->OnFrame		();
	// tools on frame
	if (m_Flags.is(flUpdateScene)) RealUpdateScene();
	Tools->OnFrame		();

	// show hint
	ResetBreak			();

	// Progress
	ProgressDraw		();
}

bool TUI::Idle()
{
	VERIFY(m_bReady);

	MSG msg;
	do
	{
		ZeroMemory(&msg, sizeof(msg));
		if (::PeekMessage(&msg, nullptr, 0U, 0U, PM_REMOVE))
		{
			::TranslateMessage(&msg);
			::DispatchMessage(&msg);
			if (msg.message == WM_QUIT)
			{
				UI->Quit();
			}
			continue;
		}

	} while (msg.message);

	if (m_Flags.is(flResetUI))
		RealResetUI();

	Sleep(1);

	if (g_pGamePersistent)
	{
		g_pGamePersistent->UpdatePlayDestroyParticles();
	}

	OnFrame();

	if (GActorInterface != nullptr)
	{
		GActorInterface->UpdatePlayerHud();
	}

	Device.secondary_tasks.run([]()
	{
		PROF_THREAD("Secondary async")
		{
			PROF_EVENT("Sheduler")
			Engine.Sheduler.Update();
		}

		{
			PROF_EVENT("seqParallel")
			for (u32 pit = 0; pit < EDevice->seqParallel.size(); pit++)
				EDevice->seqParallel[pit]();
			EDevice->seqParallel.clear();
		}

		{
			PROF_EVENT("seqFrameMT")
			EDevice->seqFrameMT.Process(rp_Frame);
		}
	});

	if (EDevice->b_is_Active && !m_Flags.is(flNeedQuit) && !m_AppClosed)
		RealRedrawScene();

	// test quit
	if (m_Flags.is(flNeedQuit))	
		RealQuit();

	Device.secondary_tasks.wait();

	return !m_AppClosed;
}

void ResetActionToSelect()
{
	ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
}

bool TUI::OnCreate()
{
// create base class
	EDevice->InitTimer();

	EDevice->Initialize();
	// Creation
	extern CDB::COLLIDER XRC;
	XRC.ray_options(CDB::OPT_ONLYNEAREST | CDB::OPT_CULL);

	pInput			= new CInput(false, all_device_key);

	Console = new CConsole();
	Console->Initialize();

	UI->IR_Capture	();

	m_bReady		= true;

	string_path log_path;
	if (!FS.exist(log_path,_temp_,""))
	{
		VerifyPath(log_path);
	}
	if (!FS.path_exist(_local_root_)){
		ELog.DlgMsg	(mtError,"Undefined Editor local directory.");
		return 		false;
	}

	BeginEState(esEditScene);

	GetRenderWidth() = 128;
	GetRenderHeight() = 128;

	for (auto& [ID, View] : Views)
	{
		View.RTSize = { (int)GetRenderWidth(), (int)GetRenderHeight() };
		View.RTFreez.create(("$user$rt_freez" + xr_string::ToString((u32)UI->ViewID)).c_str(), GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::B8G8R8X8_UNORM);
	}

	EDevice->fASPECT = (float)GetRenderWidth() / (float)GetRenderHeight();

	EDevice->mProject.build_projection(deg2rad(EDevice->fFOV), EDevice->fASPECT, UI->CurrentView().m_Camera.m_Znear, UI->CurrentView().m_Camera.m_Zfar);
	EDevice->m_fNearer = EDevice->mProject._43;

	RCache.set_xform_project(EDevice->mProject);
	RCache.set_xform_world(Fidentity);

	RTPostion.create("$user$position", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::R16G16B16A16_FLOAT);
	RTNormal.create("$user$normal", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::R16G16B16A16_FLOAT);
	RTDiffuse.create("$user$diffuse", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::B8G8R8A8_UNORM);

	RT.create("$user$rt_color", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::B8G8R8X8_UNORM);
	RTCopy.create("$user$rt_color_copy", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::B8G8R8X8_UNORM);

	ZB.create("$user$rt_depth", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::D24_UNORM_S8_UINT);

	return true;
}

void TUI::OnDestroy()
{
	Console->Destroy();
	xr_delete(Console);

	for (auto& [ID, View] : Views)
	{
		View.RTFreez.destroy();
	}

	TextureStack.clear();

	RT.destroy();
	RTCopy.destroy();
	ZB.destroy();

	RTPostion.destroy();
	RTNormal.destroy();
	RTDiffuse.destroy();

	VERIFY(m_bReady);
	m_bReady		= false;
	UI->IR_Release	();
	xr_delete		(pInput);
	EndEState		();

	EDevice->ShutDown();    
}

SPBItem* TUI::ProgressStart(float max_val, const char* text)
{
	VERIFY(m_bReady);
	SPBItem* item = new SPBItem(text, "", max_val);
	m_ProgressItems.push_back(item);
	ELog.Msg(mtInformation, text);
	ProgressDraw();

	IsLoading = true;

	return item;
}

void TUI::ProgressEnd(SPBItem*& pbi)
{
	VERIFY(m_bReady);
	if (pbi) 
	{
		PBVecIt it = std::find(m_ProgressItems.begin(), m_ProgressItems.end(), pbi); VERIFY(it != m_ProgressItems.end());
		m_ProgressItems.erase(it);
		xr_delete(pbi);
		ProgressDraw();

		IsLoading = false;
	}
}

void TUI::ProgressDraw()
{
	SPBItem* pbi = UI->ProgressLast();
	if (pbi)
	{
		xr_string txt;
		float 		p, m;
		pbi->GetInfo(txt, p, m);
		// progress
		ProgressStatus = fis_zero(m) ? 0 : (int)((p / m) * 100);
	}
}

TUI::Viewport& TUI::CurrentView()
{
	if (!Views.contains(ViewID))
	{
		ViewID = 0;
	}

	return Views[ViewID];
}

void TUI::CreateViewport(int ID, UIRenderForm* Form)
{
	Viewport& MainView = Views[ID];
	MainView.m_Camera.SetViewport(EPrefs->view_np, EPrefs->view_fp, EPrefs->view_fov, true);
	MainView.m_Camera.SetSensitivity(EPrefs->cam_sens_move, EPrefs->cam_sens_rot);
	MainView.m_Camera.SetFlyParams(EPrefs->cam_fly_speed, EPrefs->cam_fly_alt);
	MainView.m_Camera.Reset();

	MainView.ViewportForm = Form;
	MainView.ViewGlobalIDX = ID;

	MainView.RTSize = { (int)GetRenderWidth(), (int)GetRenderHeight() };
	MainView.RTFreez.create(("$user$rt_freez" + xr_string::ToString(ID)).c_str(), GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::B8G8R8X8_UNORM);
}

void TUI::DestroyViewport(int ID)
{
	auto Iter = Views.find(ID);
	if (Iter != Views.end())
	{
		Views.erase(Iter);
		return;
	}

	VERIFY(!"Viewport not found!");
}

void TUI::InitWindowIcons()
{
	m_HeaderLogo	= chezze_svg_temporary::RasterizeSvg(IX_RAY_LOGO, 64, 64); //EDevice->Resources->_CreateTexture("ed\\bar\\win_header_logo");
	m_WinMin		= chezze_svg_temporary::RasterizeSvg(IX_MIN_ICON, 10, 10);
	m_WinMax		= chezze_svg_temporary::RasterizeSvg(IX_MAX_ICON, 10, 10);
	m_WinRes		= chezze_svg_temporary::RasterizeSvg(IX_RESTORE_ICON, 10, 10);
	m_WinClose		= chezze_svg_temporary::RasterizeSvg(IX_CLOSE_ICON, 10, 10);
}

void TUI::OnDrawUI()
{
	UIKeyPressForm::Update(EDevice->fTimeGlobal);
	UIEditLightAnim::Update();
	UIImageEditorForm::Update();
	UISoundEditorForm::Update();
	UIMinimapEditorForm::Update();
	UIWeatherPropForm::Update();
	UIIconPicker::Update();
	UILogForm::Update();
	EDevice->seqDrawUI.Process(rp_DrawUI);
}

void TUI::RealResetUI()
{
	m_Flags.set(flResetUI, false);
	string_path 		ini_path;
	if (FS.exist(ini_path, "$server_data_root$", UI->EditorName(), "_imgui_default.ini"))
	{
		UI->Resize(1280, 800);
		ImGui::LoadIniSettingsFromDisk(ini_path);
	}
}

void SPBItem::GetInfo(xr_string& txt, float& p, float& m)
{
	string256 temp_buff = {};

	if (info.size())sprintf(temp_buff, "%s (%s)", text.c_str(), info.c_str());
	else			sprintf(temp_buff, "%s", text.c_str());

	txt = temp_buff;

	p = progress;
	m = max;
}

void SPBItem::Inc(const char* info, bool bWarn)
{
	Info(info, bWarn);
	Update(progress + 1.f);
}

void SPBItem::Update(float val)
{
	progress = val;
	UI->ProgressDraw();
}

void SPBItem::Info(const char* text, bool bWarn)
{
	if (text && text[0])
	{
		info = text;
		xr_string 				txt;
		float 					p, m;
		GetInfo(txt, p, m);
		ELog.Msg(bWarn ? mtError : mtInformation, txt.c_str());
		UI->ProgressDraw();
	}
}