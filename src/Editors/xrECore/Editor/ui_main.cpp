#include "stdafx.h"
#include "EditorSceneSubmit.h"
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
#include "UILightingForm.h"
#include "UILogForm.h"
#include "../xrEngine/gamefont.h"
#include "../XrEngine/XR_IOConsole.h"

#include "imgui_EditorEx_Icons.h"

#define TRelease(x) \
	if (x)          \
	x->pSurface->Release()

ECORE_API extern bool bIsLevelEditor;
extern ENGINE_API u32 ps_r_scale_mode;
extern ENGINE_API u32 ps_proxy_r_scale_mode;
extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;

namespace
{
	bool IsTemporalUpscaler()
	{
		return GEditorDraw.Upscaler >= 2;
	}

	// Scene scale for the current render quality. The temporal upscalers (DLSS, FSR) run their
	// quality presets: the quality picks the preset the way vid_scale_preset 5 picks it from vid_scale.
	float GetSceneScale()
	{
		const float Quality = std::clamp(GEditorDraw.ScreenQuality, 0.25f, 1.f);
		if (!IsTemporalUpscaler())
		{
			return Quality;
		}

		// Native, quality, balanced, performance, ultra performance (CRenderDevice::Reset)
		static constexpr float ScalePresets[] = { 1.0f, 1.5f, 1.724f, 2.0f, 3.0f };

		u32 Preset = 4;
		if (Quality >= 0.9f)
		{
			Preset = 0;
		}
		else if (Quality >= 0.7f)
		{
			Preset = 1;
		}
		else if (Quality >= 0.6f)
		{
			Preset = 2;
		}
		else if (Quality >= 0.5f)
		{
			Preset = 3;
		}

		// The DLSS / FSR contexts read the preset when the render targets are created
		ps_render_scale_preset = Preset;
		ps_render_scale = Quality;

		return 1.f / ScalePresets[Preset];
	}

	// Scene resolution of a viewport (even, as the game does)
	Ivector2 GetSceneSize(const Ivector2& ViewportSize)
	{
		const float Scale = GetSceneScale();
		if (Scale >= 1.f)
		{
			return ViewportSize;
		}

		Ivector2 Size;
		Size.x = std::max(2, iFloor(float(ViewportSize.x) * Scale));
		Size.y = std::max(2, iFloor(float(ViewportSize.y) * Scale));
		Size.x += Size.x % 2;
		Size.y += Size.y % 2;
		return Size;
	}
}

namespace ImGui
{
XREUI_API ImFont* LightFont;
XREUI_API ImFont* RegularFont;
XREUI_API ImFont* MediumFont;
XREUI_API ImFont* BoldFont;
} // namespace ImGui

TUI::TUI()
{
	EContext.UI = this;
	BindEditorContext();
	m_AppClosed = false;
	m_bAppActive = false;
	m_bReady = false;
	bNeedAbort = false;

	m_CurrentRStart.set(0, 0, 0);
	m_CurrentRDir.set(0, 0, 0);

	m_Flags.assign(flResize);

	m_Pivot.set(0, 0, 0);

	m_MouseCaptured = false;
	m_MouseMultiClickCaptured = false;
	m_SelectionRect = false;
	bMouseInUse = false;

	m_bHintShowing = false;
	m_LastHint = "";

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
	VERIFY(m_ProgressItems.size() == 0);
	VERIFY(m_EditorState.size() == 0);

	TRelease(m_HeaderLogo);
	TRelease(m_WinMin);
	TRelease(m_WinRes);
	TRelease(m_WinMax);
	TRelease(m_WinClose);
}

ImTextureID TUI::LoadTexture(const char* Texture) const
{
	if (EDevice == nullptr || DEV == nullptr)
	{
		return nullptr;
	}

	ref_texture& Tex = TextureStack[Texture];
	if (!Tex)
	{
		Tex = DEV->_CreateTexture(Texture);
	}

	if (Tex->pSurface == nullptr || Tex->get_SRView() == nullptr)
	{
		Tex->apply_load(0);
	}

	if (Tex->get_SRView() == nullptr)
	{
		return nullptr;
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

void TUI::EnableSelectionRect(bool flag)
{
	m_SelectionRect = flag;
	m_SelEnd.x = m_SelStart.x = 0;
	m_SelEnd.y = m_SelStart.y = 0;
}

void TUI::UpdateSelectionRect(const Ivector2& from, const Ivector2& to)
{
	m_SelStart.set(from);
	m_SelEnd.set(to);
}

bool TUI::KeyDown(WORD Key, TShiftState Shift)
{
	if (!m_bReady)
	{
		return false;
	}
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

	if (CurrentView().m_Camera.KeyDown(Key, Shift))
	{
		return true;
	}
	return Tools->KeyDown(Key, Shift);
}

bool TUI::KeyUp(WORD Key, TShiftState Shift)
{
	if (!m_bReady)
	{
		return false;
	}

	if (CurrentView().m_Camera.KeyUp(Key, Shift))
	{
		return true;
	}
	return Tools->KeyUp(Key, Shift);
}

bool TUI::KeyPress(WORD Key, TShiftState Shift)
{
	if (!m_bReady)
	{
		return false;
	}
	return Tools->KeyPress(Key, Shift);
}
//----------------------------------------------------
void TUI::IR_OnMouseWheel(int direction)
{
	if (!m_bReady)
	{
		return;
	}
	if (Tools->MouseWheel(direction, m_ShiftState))
	{
		RedrawScene();
	}
}
//----------------------------------------------------

void TUI::MousePress(TShiftState Shift, int X, int Y)
{
	if (!m_bReady)
	{
		return;
	}
	if (m_MouseCaptured)
	{
		return;
	}

	bMouseInUse = true;

	m_ShiftState = Shift;

	// camera activate
	if (!CurrentView().m_Camera.MoveStart(m_ShiftState))
	{
		if (Tools->Pick(Shift))
		{
			return;
		}

		if (!m_MouseCaptured)
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
				CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp);
				m_StartRDir = m_CurrentRDir;
			}

			if (Tools->MouseStart(m_ShiftState))
			{
				if (Tools->HiddenMode())
				{
					ShowCursor(false);
				}
				m_MouseCaptured = true;
			}
		}
	}
	RedrawScene();
}

void TUI::MouseRelease(TShiftState Shift, int X, int Y)
{
	if (!m_bReady)
	{
		return;
	}

	m_ShiftState = Shift;

	if (CurrentView().m_Camera.IsMoving())
	{
		if (CurrentView().m_Camera.MoveEnd(m_ShiftState))
		{
			bMouseInUse = false;
		}
	}
	else
	{
		bMouseInUse = false;
		if (m_MouseCaptured)
		{
			if (!Tools->HiddenMode())
			{
				m_CurrentCp = GetRenderMousePosition();
				CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp);
			}

			bool bIsHiddenMode = Tools->HiddenMode();
			if (Tools->MouseEnd(m_ShiftState))
			{
				if (bIsHiddenMode)
				{
					SetCursorPos(m_StartCpH.x, m_StartCpH.y);
					ShowCursor(true);
				}
				m_MouseCaptured = false;
			}
		}
	}

	// update tools (change action)
	Tools->OnFrame();
	RedrawScene();
}
//----------------------------------------------------
void TUI::MouseMove(TShiftState Shift, int X, int Y)
{
	if (!m_bReady)
	{
		return;
	}
	m_ShiftState = Shift;
}
//----------------------------------------------------
void TUI::IR_OnMouseMove(int x, int y)
{
	if (!m_bReady)
	{
		return;
	}

	bool bRayUpdated = false;

	if (!CurrentView().m_Camera.Process(m_ShiftState, x, y))
	{
		if (m_MouseCaptured || m_MouseMultiClickCaptured)
		{
			if (Tools->HiddenMode())
			{
				m_DeltaCpH.set(x, y);
				if (m_DeltaCpH.x || m_DeltaCpH.y)
				{
					Tools->MouseMove(m_ShiftState);
				}
			}
			else
			{
				m_CurrentCp = GetRenderMousePosition();
				CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp);
				Tools->MouseMove(m_ShiftState);
			}

			RedrawScene();
			bRayUpdated = true;
		}
	}

	if (!bRayUpdated)
	{
		m_CurrentCp = GetRenderMousePosition();
		CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp);
	}
}
//---------------------------------------------------------------------------

void TUI::OnAppActivate()
{
	m_bAppActive = true;
	if (!m_bReady)
	{
		return;
	}
	if (pInput)
	{
		m_ShiftState = ssNone;
		pInput->OnAppActivate();
		EDevice->seqAppActivate.Process<&pureAppActivate::OnAppActivate>();
	}
}
//---------------------------------------------------------------------------

void TUI::OnAppDeactivate()
{
	m_bAppActive = false;
	if (!m_bReady)
	{
		return;
	}
	if (pInput)
	{
		pInput->OnAppDeactivate();
		m_ShiftState = ssNone;
		EDevice->seqAppDeactivate.Process<&pureAppDeactivate::OnAppDeactivate>();
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

	// not_implemented();
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
	{
		HideHint();
	}
}

//---------------------------------------------------------------------------

#include "..\xrEngine\IGame_Persistent.h"
void TUI::PrepareRedraw()
{
	VERIFY(m_bReady);
	if (m_Flags.is(flResize))
	{
		RealResize();
	}

	// set render state
	EDevice->SetRS(D3DRS_TEXTUREFACTOR, 0xffffffff);
	RCache.set_xform_world(Fidentity);
}

void TUI::Invalidate()
{
	RT.destroy();
	RT.create("$user$rt_color", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
}

extern ENGINE_API xr_atomic_bool g_bRendering;
ECORE_API xrCriticalSection temp_render_lock;

void TUI::Redraw()
{
	xrCriticalSectionGuard guard_lock(temp_render_lock);

	PrepareRedraw();

	Viewport& View = CurrentView();

	// Render quality scales the scene resolution only; the renderer upscales it to the viewport
	// with the selected upscaler
	ps_r_scale_mode = ps_proxy_r_scale_mode = GEditorDraw.Upscaler;

	const Ivector2 SceneSize = GetSceneSize(View.RTSize);

	// The temporal upscalers keep one history: start it over on another viewport, size or mode
	static int UpscaledViewID = -1;
	static u32 UpscaledMode = 0;
	bool UpscalerReset = UpscaledViewID != View.ViewGlobalIDX;

	if (u32(View.RTSize.x) != EDevice->TargetWidth || u32(View.RTSize.y) != EDevice->TargetHeight || SceneSize.x != HalfTarget.x || SceneSize.y != HalfTarget.y || UpscaledMode != GEditorDraw.Upscaler || !RT->pSurface)
	{
		if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
		{
			// The DLSS / FSR contexts are created with the render targets
			UpscaledMode = GEditorDraw.Upscaler;
			UpscalerReset = true;

			EDevice->TargetWidth = View.RTSize.x;
			EDevice->TargetHeight = View.RTSize.y;

			RT.destroy();
			RTCopy.destroy();
			View.RTFreez.destroy();

			RT.create("$user$rt_color", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
			View.RTFreez.create(("$user$rt_freez" + xr_string::ToString((u32)ViewID)).c_str(), GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
			RTCopy.create("$user$rt_color_copy", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);

			// The renderer targets follow the viewport size, the scene resolution (HalfTarget)
			// follows the render quality
			HalfTarget = SceneSize;

			::Render->reset_begin();
			::Render->reset_end();

			m_Flags.set(flRedraw, true);

			EDevice->m_fNearer = EDevice->mProject._43;
			EDevice->fASPECT = float(View.RTSize.y) / float(View.RTSize.x);

			EDevice->seqDeviceReset.Process<&pureDeviceReset::OnDeviceReset>();
			EDevice->seqResolutionChanged.Process<&pureScreenResolutionChanged::OnScreenResolutionChanged>();
			RCache.set_xform_project(EDevice->mProject);
			RCache.set_xform_world(Fidentity);
		}
		else
		{
			// Soft render update when resizing window
			EDevice->fASPECT = float(View.RTSize.y) / float(View.RTSize.x);
			m_Flags.set(flRedraw, true);
		}
	}

	if (!IsPlayInEditor())
	{
		EDevice->mProject.build_projection(deg2rad(EDevice->fFOV), EDevice->fASPECT, View.m_Camera.m_Znear, View.m_Camera.m_Zfar);
	}

	if (EDevice->Begin())
	{
		if (psDeviceFlags.is(rsRenderRealTime))
		{
			m_Flags.set(flRedraw, true);
		}

		static u32 redraw_frame = 0;

		if (m_Flags.is(flRedraw))
		{
			// The temporal upscalers converge over the jittered frames
			redraw_frame = EDevice->dwRenderFrame + (IsTemporalUpscaler() ? 32 : 3);
		}

		if (redraw_frame > EDevice->dwRenderFrame || IsPlayInEditor())
		{
			m_Flags.set(flRedraw, false);
			++EDevice->dwRenderFrame;

			EDevice->UpdateView();
			EDevice->ResetMaterial();

			// Camera of the previous frame of this viewport: the velocity pass reprojects the depth with it
			if (!View.HasPrevTransform || UpscalerReset)
			{
				View.PrevView = EDevice->mView;
				View.PrevProject = EDevice->mProject;
				View.HasPrevTransform = true;
			}

			EDevice->mView_old = View.PrevView;
			EDevice->mProject_old = View.PrevProject;
			EDevice->mFullTransform_old.mul(View.PrevProject, View.PrevView);
			RCache.set_xform_view_old(View.PrevView);
			RCache.set_xform_project_old(View.PrevProject);

			View.PrevView = EDevice->mView;
			View.PrevProject = EDevice->mProject;

			GEditorDraw.UpscalerReset = UpscalerReset;
			UpscaledViewID = View.ViewGlobalIDX;

			for (u32 k = 0; k < Caps.raster.dwStages; k++)
			{
				if (psDeviceFlags.is(rsFilterLinear))
				{
					EDevice->SetSS(k, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
					EDevice->SetSS(k, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
					EDevice->SetSS(k, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);
				}
				else
				{
					EDevice->SetSS(k, D3DSAMP_MAGFILTER, D3DTEXF_POINT);
					EDevice->SetSS(k, D3DSAMP_MINFILTER, D3DTEXF_POINT);
					EDevice->SetSS(k, D3DSAMP_MIPFILTER, D3DTEXF_POINT);
				}
			}

			// Lit scene through the game pipeline: the renderer collects the editor scene
			// (IEditorRenderHost::SubmitScene runs Tools->Render in the submit pass)
			GEditorSceneSubmit.BeginFrame();
			if (EPrefs)
			{
				RImplementation.EditorClearColor.set(EPrefs->scene_clear_color);
				RImplementation.EditorOcclusionCulling = EPrefs->OcclusionCulling;
				RImplementation.EditorHemi = EPrefs->Hemi;
				RImplementation.EditorDynamicLights = EPrefs->DynamicLights;
				RImplementation.EditorLightShadows = EPrefs->LightShadows;
			}
			::Render->Calculate();
			::Render->Render();

			GEditorDraw.UpscalerReset = false;

			// An unavailable DLSS falls back to FSR, FSR to the linear filter (phase_combine)
			if (ps_proxy_r_scale_mode != GEditorDraw.Upscaler)
			{
				GEditorDraw.Upscaler = ps_proxy_r_scale_mode;
			}

			// Helpers over the lit scene, with its depth
			RImplementation.EditorBeginOverlay();
			GEditorDraw.Pass = EEditorDrawPass::Overlay;

			// The grid is drawn by the renderer under the blended geometry (DrawSceneHelpers)
			if (psDeviceFlags.is(rsDrawGrid))
			{
				DU_impl.DrawPivot(m_Pivot);
			}

			Tools->Render();

			GEditorDraw.Pass = EEditorDrawPass::Immediate;

			// draw selection rect
			if (m_SelectionRect)
			{
				DU_impl.DrawSelectionRect(m_SelStart, m_SelEnd);
			}

			// draw axis
			if (psDeviceFlags.test(rsDrawAxis) && !psDeviceFlags.test(rsDisableAxisCube))
			{
				DU_impl.DrawAxis(CurrentView().m_Camera.GetTransform());
			}


			EDevice->Statistic->RenderDUMP_RT.End();
			EDevice->Statistic->Show();

			EDevice->SetRS(D3DRS_FILLMODE, GEditorDraw.FillMode);
			EDevice->seqRender.Process<&pureRender::OnRender>();

			if (g_pGamePersistent->OnRenderPPUI_query())
			{
				g_pGamePersistent->OnRenderPPUI_main();
			}

			g_FontManager->Render();

			// Resolve into the viewport target
			RImplementation.EditorPresent(RT->pRT);
			GRHI->CopySurface(RTCopy->pRT, RT->pRT);
		}

		g_bRendering = false;

		// end draw
		BeginFrame();

		Draw();

		ID3D11RenderTargetView* RTV = (ID3D11RenderTargetView*)RSwapchainTarget->GetRawRTV();
		RContext->OMSetRenderTargets(1, &RTV, 0);
		EndFrame();
		EDevice->End();
		MDIUpdate();
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
	m_Flags.set(flResize, false);
	if (m_Size.x && m_Size.y)
	{
		EDevice->Resize(m_Size.x, m_Size.y, m_Size_Maximize);
	}
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void TUI::RealUpdateScene()
{
	Tools->UpdateProperties(false);
	m_Flags.set(flUpdateScene, false);
}

void TUI::RealRedrawScene()
{
	Redraw();
}

void TUI::OnFrame()
{
	EDevice->FrameMove();
	SndLib->OnFrame();
	// tools on frame
	if (m_Flags.is(flUpdateScene))
	{
		RealUpdateScene();
	}
	Tools->OnFrame();

	// show hint
	ResetBreak();

	// Progress
	ProgressDraw();
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
				Quit();
			}
			continue;
		}

	} while (msg.message);

	if (m_Flags.is(flResetUI))
	{
		RealResetUI();
	}

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

	Device.SecondaryTasks.run([]()
							  {
		PROF_THREAD("Secondary async")
		{
			PROF_EVENT("Sheduler")
			Engine.Sheduler.Update();
		}

		{
			PROF_EVENT("seqParallel")
			for (u32 pit = 0; pit < EDevice->seqParallel.size(); pit++){
				EDevice->seqParallel[pit]();
}
			EDevice->seqParallel.clear();
		}

		{
			PROF_EVENT("seqFrameMT")
			EDevice->seqFrameMT.Process<&pureFrame::OnFrame>();
		} });

	if (EDevice->b_is_Active && !m_Flags.is(flNeedQuit) && !m_AppClosed)
	{
		RealRedrawScene();
	}

	// test quit
	if (m_Flags.is(flNeedQuit))
	{
		RealQuit();
	}

	Device.SecondaryTasks.wait();

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

	// The renderer executes its console commands while the device is created
	Console = new CConsole();
	Console->Initialize();
	xrRender_initconsole();

	EDevice->Initialize();
	// Creation
	extern CDB::COLLIDER XRC;
	XRC.ray_options(CDB::OPT_ONLYNEAREST | CDB::OPT_CULL);

	pInput = new CInput(false, all_device_key);

	IR_Capture();

	m_bReady = true;

	string_path log_path;
	if (!FS.exist(log_path, _temp_, ""))
	{
		VerifyPath(log_path);
	}
	if (!FS.path_exist(_local_root_))
	{
		ELog.DlgMsg(mtError, "Undefined Editor local directory.");
		return false;
	}

	BeginEState(esEditScene);

	GetRenderWidth() = 128;
	GetRenderHeight() = 128;

	for (auto& [ID, View] : Views)
	{
		View.RTSize = {(int)GetRenderWidth(), (int)GetRenderHeight()};
		View.RTFreez.create(("$user$rt_freez" + xr_string::ToString((u32)ViewID)).c_str(), GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
	}

	EDevice->fASPECT = (float)GetRenderWidth() / (float)GetRenderHeight();

	EDevice->mProject.build_projection(deg2rad(EDevice->fFOV), EDevice->fASPECT, CurrentView().m_Camera.m_Znear, CurrentView().m_Camera.m_Zfar);
	EDevice->m_fNearer = EDevice->mProject._43;

	RCache.set_xform_project(EDevice->mProject);
	RCache.set_xform_world(Fidentity);


	RT.create("$user$rt_color", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
	RTCopy.create("$user$rt_color_copy", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);

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

	VERIFY(m_bReady);
	m_bReady = false;
	IR_Release();
	xr_delete(pInput);
	EndEState();

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
		PBVecIt it = std::find(m_ProgressItems.begin(), m_ProgressItems.end(), pbi);
		VERIFY(it != m_ProgressItems.end());
		m_ProgressItems.erase(it);
		xr_delete(pbi);
		ProgressDraw();

		IsLoading = false;
	}
}

void TUI::ProgressDraw()
{
	SPBItem* pbi = ProgressLast();
	if (pbi)
	{
		xr_string txt;
		float p, m;
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

	MainView.RTSize = {(int)GetRenderWidth(), (int)GetRenderHeight()};
	MainView.RTFreez.create(("$user$rt_freez" + xr_string::ToString(ID)).c_str(), GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
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
	m_HeaderLogo = chezze_svg_temporary::RasterizeSvg(IX_RAY_LOGO, 64, 64);
	m_WinMin = chezze_svg_temporary::RasterizeSvg(IX_MIN_ICON, 10, 10);
	m_WinMax = chezze_svg_temporary::RasterizeSvg(IX_MAX_ICON, 10, 10);
	m_WinRes = chezze_svg_temporary::RasterizeSvg(IX_RESTORE_ICON, 10, 10);
	m_WinClose = chezze_svg_temporary::RasterizeSvg(IX_CLOSE_ICON, 10, 10);
}

void TUI::OnDrawUI()
{
	UIKeyPressForm::Update(EDevice->fTimeGlobal);
	UIEditLightAnim::Update();
	UIImageEditorForm::Update();
	UISoundEditorForm::Update();
	UIMinimapEditorForm::Update();
	UILightingForm::Update();
	UIIconPicker::Update();
	UILogForm::Update();
	EDevice->seqDrawUI.Process<&pureDrawUI::OnDrawUI>();
}

void TUI::RealResetUI()
{
	m_Flags.set(flResetUI, false);
	string_path ini_path;
	if (FS.exist(ini_path, "$server_data_root$", EditorName(), "_imgui_default.ini"))
	{
		Resize(1280, 800);
		ImGui::LoadIniSettingsFromDisk(ini_path);
	}
}

void SPBItem::GetInfo(xr_string& txt, float& p, float& m)
{
	string256 temp_buff = {};

	if (info.size())
	{
		sprintf(temp_buff, "%s (%s)", text.c_str(), info.c_str());
	}
	else
	{
		sprintf(temp_buff, "%s", text.c_str());
	}

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
	EContext.UI->ProgressDraw();
}

void SPBItem::Info(const char* text, bool bWarn)
{
	if (text && text[0])
	{
		info = text;
		xr_string txt;
		float p, m;
		GetInfo(txt, p, m);
		ELog.Msg(bWarn ? mtError : mtInformation, txt.c_str());
		EContext.UI->ProgressDraw();
	}
}