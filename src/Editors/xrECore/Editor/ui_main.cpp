//---------------------------------------------------------------------------

#include "stdafx.h"
#pragma hdrstop

#include "../xrEngine/xr_input.h"
#include "UI_ToolsCustom.h"

#include "UI_Main.h"
#include "d3dutils.h"
#include "SoundManager.h"
#include "../Layers/xrRender/PSLibrary.h"
#include "../Layers/xrRender/dxRenderDeviceRender.h"

#include "UIEditLightAnim.h"
#include "UIImageEditorForm.h"
#include "UISoundEditorForm.h"
#include "UIMinimapEditorForm.h"
#include "../utils/ETools/ETools.h"
#include "UILogForm.h"
#include "../xrEngine/gamefont.h"
#include "../XrEngine/XR_IOConsole.h"


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
	m_HConsole = 0;
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

	   //RECT rect;
	   //HWND taskBar = FindWindow(L"Shell_traywnd", NULL);
	   //if (taskBar && GetWindowRect(taskBar, &rect)) 
	   //{
	   //    if (rect.top > 0)
	   //        DisplayY -= rect.bottom - rect.top;
	   //
	   //    DisplayX -= rect.right - rect.left;
	   //}

	Viewport& MainView = Views.emplace_back();
	m_Size.set(DisplayX, DisplayY);
}
//---------------------------------------------------------------------------
TUI::~TUI()
{
	VERIFY(m_ProgressItems.size()==0);
	VERIFY(m_EditorState.size()==0);
}

void TUI::OnDeviceCreate()
{
	DU_impl.OnDeviceCreate();
}

void TUI::OnDeviceDestroy()
{
	DU_impl.OnDeviceDestroy();
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
				if(Tools->HiddenMode()) ShowCursor( FALSE );
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
					ShowCursor( TRUE );
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

	// Out cursor pos
	OutUICursorPos	();
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
  /*  if (SS.size()){
		xr_string S=_ListToSequence2(SS);
		if (m_bHintShowing&&(S==m_LastHint)) return true;
		m_LastHint = S;
		m_bHintShowing = true;
		if (!m_pHintWindow){
			m_pHintWindow = new THintWindow((TComponent*)0);
			m_pHintWindow->Brush->Color = (TColor)0x0d9F2FF;
		}
		TRect rect = m_pHintWindow->CalcHintRect(320,S,0);
		rect.Left+=m_HintPoint.x;    rect.Top+=m_HintPoint.y;
		rect.Right+=m_HintPoint.x;   rect.Bottom+=m_HintPoint.y;
		m_pHintWindow->ActivateHint(rect,S);
	}else{
		m_bHintShowing = false;
		m_LastHint = "";
	}*/
	not_implemented();
	return m_bHintShowing;
}
//---------------------------------------------------------------------------

void TUI::HideHint()
{
	VERIFY(m_bReady);
	m_bHintShowing = false;
}
//---------------------------------------------------------------------------

void TUI::ShowHint(const xr_string& s)
{
	VERIFY			(m_bReady);
	GetCursorPos	(&m_HintPoint);
	AStringVec 		SS;
	SS.push_back	(s);
	Tools->OnShowHint(SS);
	if (!ShowHint(SS)) HideHint();
}
//---------------------------------------------------------------------------

void TUI::ShowObjectHint()
{
	/*VERIFY(m_bReady);
	if (!EPrefs->object_flags.is(epoShowHint)){
//    	if (m_bHintShowing) HideHint();
		return;
	}
	if (UI->CurrentView().m_Camera.IsMoving()||m_MouseCaptured) return;
	if (!m_bAppActive) return;

	GetCursorPos(&m_HintPoint);
	TWinControl* ctr = FindVCLWindow(m_HintPoint);
	if (ctr!=m_D3DWindow) return;

	AStringVec SS;
	Tools->OnShowHint(SS);
	if (!ShowHint(SS)&&m_pHintWindow) HideHint();*/
}
//---------------------------------------------------------------------------
void TUI::CheckWindowPos(HWND* form)
{
	/*if (form->Left+form->Width>Screen->Width) 	form->Left	= Screen->Width-form->Width;
	if (form->Top+form->Height>Screen->Height)	form->Top 	= Screen->Height-form->Height;
	if (form->Left<0) 							form->Left	= 0;
	if (form->Top<0) 							form->Top 	= 0;*/
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
/*
	if (0==g_pGamePersistent->Environment().GetWeather().size())
	{
		g_pGamePersistent->Environment().CurrentEnv->fog_color.set	(color_get_R(fog_color),color_get_G(fog_color),color_get_B(fog_color));
		g_pGamePersistent->Environment().CurrentEnv->fog_far		= fog_end;
		g_pGamePersistent->Environment().CurrentEnv->fog_near		= fog_start;
	}
*/    
	EDevice->SetRS( D3DRS_FOGCOLOR,		fog_color			);
	EDevice->SetRS( D3DRS_RANGEFOGENABLE,	FALSE				);
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
	if (psDeviceFlags.is(rsLighting)) 	EDevice->SetRS(D3DRS_AMBIENT,0x00000000);
	else                				EDevice->SetRS(D3DRS_AMBIENT,0xFFFFFFFF);

	EDevice->SetRS			(D3DRS_FILLMODE, EDevice->dwFillMode);
	EDevice->SetRS			(D3DRS_SHADEMODE,EDevice->dwShadeMode);

	RCache.set_xform_world	(Fidentity);
}

extern ENGINE_API BOOL g_bRendering;
void TUI::Redraw()
{
	PrepareRedraw();

	try
	{
		Viewport& View = CurrentView();

		if
		(
			u32(View.RTSize.x * EDevice->m_ScreenQuality) != RT->dwWidth  || 
			u32(View.RTSize.y * EDevice->m_ScreenQuality) != RT->dwHeight || 
			!RT->pSurface
		)
		{
			if(!ImGui::IsMouseDown(ImGuiMouseButton_Left)) 
			{
				GetRenderWidth() = View.RTSize.x * EDevice->m_ScreenQuality;
				GetRenderHeight() = View.RTSize.y * EDevice->m_ScreenQuality;

				RT.destroy();
				RTCopy.destroy();
				ZB.destroy();
				View.RTFreez.destroy();

				RTPostion.destroy();
				RTNormal.destroy();
				RTDiffuse.destroy();

				RTPostion.create("$user$position", GetRenderWidth(), GetRenderHeight(), D3DFMT_A16B16G16R16F);
				RTNormal.create("$user$normal", GetRenderWidth(), GetRenderHeight(), D3DFMT_A16B16G16R16F);
				RTDiffuse.create("$user$diffuse", GetRenderWidth(), GetRenderHeight(), D3DFMT_A8R8G8B8);

				RT.create("$user$rt_color", GetRenderWidth(), GetRenderHeight(), D3DFMT_X8R8G8B8);
				View.RTFreez.create(("$user$rt_freez" + xr_string::ToString((u32)UI->ViewID)).c_str(), GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_X8R8G8B8);
				RTCopy.create("$user$rt_color_copy", GetRenderWidth(), GetRenderHeight(), D3DFMT_X8R8G8B8);

				ZB.create("$user$rt_depth", GetRenderWidth(), GetRenderHeight(), D3DFMT_D24S8);

				m_Flags.set(flRedraw, TRUE);

				EDevice->m_fNearer = EDevice->mProject._43;
				EDevice->HalfTargetWidth = float(View.RTSize.x) * 0.5f;
				EDevice->HalfTargetHeight = float(View.RTSize.y) * 0.5f;
				EDevice->fASPECT = EDevice->HalfTargetHeight / EDevice->HalfTargetWidth;

				EDevice->seqDeviceReset.Process(rp_DeviceReset);
				EDevice->seqResolutionChanged.Process(rp_ScreenResolutionChanged);
				RCache.set_xform_project(EDevice->mProject);
				RCache.set_xform_world(Fidentity);
			}
			else 
			{
				// Soft render update when resizing window
				EDevice->HalfTargetWidth = float(View.RTSize.x) * 0.5f;
				EDevice->HalfTargetHeight = float(View.RTSize.y) * 0.5f;
				EDevice->fASPECT = EDevice->HalfTargetHeight / EDevice->HalfTargetWidth;
				m_Flags.set(flRedraw, TRUE); 
			}
		}
		if (!UI->IsPlayInEditor())
		{
			EDevice->mProject.build_projection(deg2rad(EDevice->fFOV), EDevice->fASPECT, UI->CurrentView().m_Camera.m_Znear, UI->CurrentView().m_Camera.m_Zfar);
		}

		if (EDevice->Begin())
		{
			if (psDeviceFlags.is(rsRenderRealTime))
				m_Flags.set(flRedraw, TRUE);
			if (m_Flags.is(flRedraw) || UI->IsPlayInEditor())
			{
				m_Flags.set(flRedraw, FALSE);

				RCache.set_RT(RTNormal->pRT, 0);
				RCache.set_RT(RTDiffuse->pRT, 1);
				RCache.set_RT(RTPostion->pRT, 2);

				RCache.set_ZB(0);

				CHK_DX(REDevice->Clear(0, 0, D3DCLEAR_TARGET, 0x0, 1, 0));

				RCache.set_RT(RT->pRT);
				RCache.set_ZB(ZB->pRT);

				EDevice->Clear();

				RCache.set_RT(RTDiffuse->pRT, 1);
				RCache.set_RT(RTNormal->pRT, 2);
				RCache.set_RT(RTPostion->pRT, 3);

				RCache.set_Stencil(TRUE, D3DCMP_ALWAYS, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
				//EDevice->Statistic->RenderDUMP_RT.Begin();
				EDevice->UpdateView();
				EDevice->ResetMaterial();

				Tools->RenderEnvironment();

				//. temporary reset filter (      )
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
				if (psDeviceFlags.is(rsDrawGrid)) {
					DU_impl.DrawGrid();
					DU_impl.DrawPivot(m_Pivot);
				}

				try {
					Tools->Render();
				}
				catch (...) {
					ELog.DlgMsg(mtError, "Please notify AlexMX!!! Critical error has occured in render routine!!! [Type B]");
				}

				// draw selection rect
				if (m_SelectionRect) 	DU_impl.DrawSelectionRect(m_SelStart, m_SelEnd);

				// draw axis
				DU_impl.DrawAxis(UI->CurrentView().m_Camera.GetTransform());


				EDevice->Statistic->RenderDUMP_RT.End();
				EDevice->Statistic->Show();
				EDevice->SetRS(D3DRS_FILLMODE, D3DFILL_SOLID);

				g_FontManager->Render();

				//EDevice->pSystemFont->OnRender();
				EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);
				EDevice->seqRender.Process(rp_Render);

				//EDevice->Statistic->RenderDUMP_RT.End();
				//->EStatistic->Show(EDevice->pSystemFont);
				EDevice->seqRender.Process(rp_Render);
				if (g_pGamePersistent->OnRenderPPUI_query())
				{
					g_pGamePersistent->OnRenderPPUI_main();
				}

				RCache.set_RT(0, 1);
				RCache.set_RT(0, 2);
				RCache.set_RT(0, 3);

				RCache.set_RT(RSwapchainTarget);
				RCache.set_ZB(RDepth);

				RDevice->SetTextureStageState(0, D3DTSS_TEXTURETRANSFORMFLAGS, 0);
				RDevice->SetTextureStageState(0, D3DTSS_TEXCOORDINDEX, 0);
				RDevice->SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
				RDevice->SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);
			}

			try
			{
				EDevice->SetRS(D3DRS_FILLMODE, D3DFILL_SOLID);
				g_bRendering = FALSE;
				// 
				 //RCache.set_RT(RSwapchainTarget);
				 //  Draw(); 
				   // end draw
				UI->BeginFrame();

				Draw();

				EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);
				UI->EndFrame();
				EDevice->End();
			}
			catch (...)
			{
				ELog.DlgMsg(mtError, "Please notify AlexMX!!! Critical error has occured in render routine!!! [Type C]");
			}
		}
	}catch(...)
	{
		ELog.DlgMsg(mtError, "Please notify AlexMX!!! Critical error has occured in render routine!!! [Type A]");
		EDevice->End();
	}

	for (auto Callback : CommandList[TUI::ECommandListID::CurrentFrame])
		Callback();

	CommandList[TUI::ECommandListID::CurrentFrame].clear();
	std::swap(CommandList[TUI::ECommandListID::CurrentFrame], CommandList[TUI::ECommandListID::NextFrame]);

	OutInfo();
}
//---------------------------------------------------------------------------
void TUI::RealResize()
{
	m_Flags.set			(flResize,FALSE);
	if(m_Size.x&& m_Size.y)
	EDevice->Resize(m_Size.x, m_Size.y,m_Size_Maximize);
	ExecCommand			(COMMAND_UPDATE_PROPERTIES);
}
void TUI::RealUpdateScene()
{
	Tools->UpdateProperties	(false);
	m_Flags.set			(flUpdateScene,FALSE);
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
	ShowObjectHint		();
	ResetBreak			();
#if 0
	// check mail
	CheckMailslot		();
#endif
	// Progress
	ProgressDraw		();
}
bool TUI::Idle()         
{
	VERIFY(m_bReady);
   // EDevice->b_is_Active  = Application->Active;
	// input
	MSG msg;
	do
	{
		ZeroMemory(&msg, sizeof(msg));
		if (::PeekMessage(&msg, NULL, 0U, 0U, PM_REMOVE))
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
	if (m_Flags.is(flResetUI))RealResetUI();
	Sleep(1);

	OnFrame			();
	if (EDevice->b_is_Active && !m_Flags.is(flNeedQuit) && !m_AppClosed)
		RealRedrawScene();

	{
		for (u32 pit = 0; pit < EDevice->seqParallel.size(); pit++)
			EDevice->seqParallel[pit]();
		EDevice->seqParallel.clear();
		EDevice->seqFrameMT.Process(rp_Frame);
	}
	// test quit
	if (m_Flags.is(flNeedQuit))	RealQuit();
	return !m_AppClosed;
}
//---------------------------------------------------------------------------
void ResetActionToSelect()
{
	ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
}
//---------------------------------------------------------------------------

#define MIN_PANEL_HEIGHT 15


bool TUI::OnCreate()
{
// create base class
	EDevice->InitTimer();

  //  m_D3DWindow 	= w;
  //  m_D3DPanel		= p;
	EDevice->Initialize();
	// Creation
	ETOOLS::ray_options	(CDB::OPT_ONLYNEAREST | CDB::OPT_CULL);

	pInput			= new CInput(FALSE, all_device_key);

	Console = new CConsole();
	Console->Initialize();

	UI->IR_Capture	();

	m_bReady		= true;

#if 0
	if (!CreateMailslot()) {
		ELog.DlgMsg(mtError, "Can't create mail slot.\nIt's possible two Editors started.");
		return 		false;
	}
#endif
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

	int Iter = 0;
	for (Viewport& View : Views)
	{
		View.RTSize = { (int)GetRenderWidth(), (int)GetRenderHeight() };
		View.RTFreez.create(("$user$rt_freez" + xr_string::ToString((u32)UI->ViewID)).c_str(), GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_X8R8G8B8);
	}
	EDevice->fASPECT = (float)GetRenderWidth() / (float)GetRenderHeight();

	EDevice->mProject.build_projection(deg2rad(EDevice->fFOV), EDevice->fASPECT, UI->CurrentView().m_Camera.m_Znear, UI->CurrentView().m_Camera.m_Zfar);
	EDevice->m_fNearer = EDevice->mProject._43;

	RCache.set_xform_project(EDevice->mProject);
	RCache.set_xform_world(Fidentity);

	RTPostion.create("$user$position", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_A16B16G16R16F);
	RTNormal.create("$user$normal", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_A16B16G16R16F);
	RTDiffuse.create("$user$diffuse", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_A8R8G8B8);

	RT.create("$user$rt_color", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_X8R8G8B8);
	RTCopy.create("$user$rt_color_copy", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_X8R8G8B8);

	ZB.create("$user$rt_depth", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_D24S8);

	ViewID = 0;

	return true;
}

void TUI::OnDestroy()
{
	Console->Destroy();
	xr_delete(Console);

	for (Viewport& View : Views)
	{
		View.RTFreez.destroy();
	}

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

SPBItem* TUI::ProgressStart		(float max_val, LPCSTR text)
{
	VERIFY(m_bReady);
	SPBItem* item 				= new SPBItem(text,"",max_val);
	m_ProgressItems.push_back	(item);
	ELog.Msg					(mtInformation,text);
	ProgressDraw				();
	//if (!m_HConsole)
	//{
	//    AllocConsole();
	//    m_HConsole = GetStdHandle(STD_OUTPUT_HANDLE);
	//}
	return item;
}
void TUI::ProgressEnd			(SPBItem*& pbi)
{
	VERIFY(m_bReady);
	if (pbi){
		PBVecIt it=std::find(m_ProgressItems.begin(),m_ProgressItems.end(),pbi); VERIFY(it!=m_ProgressItems.end());
		m_ProgressItems.erase	(it);
		xr_delete				(pbi);
		ProgressDraw			();
		//if (m_ProgressItems.size() == 0)
		//{
		//    FreeConsole();
		//    m_HConsole = 0;
		//}
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
		int val = fis_zero(m) ? 0 : (int)((p / m) * 100);
		//string2048 out;
		//xr_sprintf(out, sizeof(out), "[%d%%]%s\r\n", val, txt.c_str());
		//Msg("[%d%%]%s\r\n", val, txt.c_str());
	   // DWORD  dw;
	   // SetConsoleTextAttribute(m_HConsole, 10);
	   // ::WriteConsoleA(m_HConsole, out, xr_strlen(out), &dw, NULL);
	}
}

void TUI::ShowConsole()
{
	//if (!m_HConsole)
	//{
	//	AllocConsole();
	//	m_HConsole = GetStdHandle(STD_OUTPUT_HANDLE);
	//	SetConsoleTextAttribute(m_HConsole, 15);
	//}
}

void TUI::WriteConsole(TMsgDlgType mt, const char* txt)
{
	//if (m_HConsole)
	//{
	//	switch (mt)
	//	{
	//	case mtError:
	//		SetConsoleTextAttribute(m_HConsole, 12);
	//		break;
	//	case mtInformation:
	//		SetConsoleTextAttribute(m_HConsole, 11);
	//		break;
	//	case mtConfirmation:
	//		SetConsoleTextAttribute(m_HConsole, 14);
	//		break;
	//	default:
	//		SetConsoleTextAttribute(m_HConsole,15);
	//		break;
	//	}
	//
	//	DWORD  dw;
	//	::WriteConsole(m_HConsole, txt, xr_strlen(txt), &dw, NULL);
	//	::WriteConsole(m_HConsole, "\r\n", 2, &dw, NULL);
	//}
}

void TUI::CloseConsole()
{
	//if (m_ProgressItems.size() == 0)
	//{
	//	FreeConsole();
	//	m_HConsole = 0;
	//}
}

TUI::Viewport& TUI::CurrentView()
{
	return Views[ViewID];
}

void TUI::CreateViewport(int ID)
{
	Viewport& MainView = Views.emplace_back();
	MainView.m_Camera.SetViewport(EPrefs->view_np, EPrefs->view_fp, EPrefs->view_fov, true);
	MainView.m_Camera.SetSensitivity(EPrefs->cam_sens_move, EPrefs->cam_sens_rot);
	MainView.m_Camera.SetFlyParams(EPrefs->cam_fly_speed, EPrefs->cam_fly_alt);
	MainView.m_Camera.Reset();

	MainView.RTSize = { (int)GetRenderWidth(), (int)GetRenderHeight() };
	MainView.RTFreez.create(("$user$rt_freez" + xr_string::ToString(ID)).c_str(), GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, D3DFMT_X8R8G8B8);
}

void TUI::OnDrawUI()
{
	UIKeyPressForm::Update(EDevice->fTimeGlobal);
	UIEditLightAnim::Update();
	UIImageEditorForm::Update();
	UISoundEditorForm::Update();
	UIMinimapEditorForm::Update();
	UILogForm::Update();
	EDevice->seqDrawUI.Process(rp_DrawUI);
}

void TUI::RealResetUI()
{
	m_Flags.set(flResetUI, FALSE);
	string_path 		ini_path;
	if (FS.exist(ini_path, "$server_data_root$", UI->EditorName(), "_imgui_default.ini"))
	{
		UI->Resize(1280, 800);
		ImGui::LoadIniSettingsFromDisk(ini_path);
	}
}

void SPBItem::GetInfo			(xr_string& txt, float& p, float& m)
{
	string256 temp_buff = {};

	if (info.size())sprintf(temp_buff, "%s (%s)",text.c_str(),info.c_str());
	else			sprintf(temp_buff, "%s",text.c_str());

	txt = temp_buff;

	p				= progress;
	m				= max;
}  
void SPBItem::Inc				(LPCSTR info, bool bWarn)
{
	Info						(info,bWarn);
	Update						(progress+1.f);
}
void SPBItem::Update			(float val)
{
	progress					= val;
	UI->ProgressDraw			();
}
void SPBItem::Info				(LPCSTR text, bool bWarn)
{
	if (text&&text[0]){
		info					= text;
		xr_string 				txt;
		float 					p,m;
		GetInfo					(txt,p,m);
		ELog.Msg				(bWarn?mtError:mtInformation,txt.c_str());
		UI->ProgressDraw		();
	}
}

