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
#include "EditorRenderBackend.h"

#include <RedImage/RedImage.hpp>

#define TRelease(x) \
	if (x)          \
	x->pSurface->Release()

ECORE_API extern bool bIsLevelEditor;
namespace ImGui
{
XREUI_API ImFont* LightFont;
XREUI_API ImFont* RegularFont;
XREUI_API ImFont* MediumFont;
XREUI_API ImFont* BoldFont;
} // namespace ImGui

TUI* UI = nullptr;

TUI::TUI()
{
	m_HConsole = nullptr;
	UI = this;
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
	if (!Texture || !Texture[0])
	{
		return nullptr;
	}

	IEditorRenderBackend& EditorRenderer = GetEditorRenderBackend();
	if (EditorRenderer.GetKind() == EEditorRenderBackendKind::Tiramisu)
	{
		const auto Existing = EditorTextureStack.find(Texture);
		if (Existing != EditorTextureStack.end())
		{
			return EditorRenderer.GetTextureSurface(Existing->second).ImGuiTextureId;
		}

		string_path Normalized = {};
		xr_strcpy(Normalized, Texture);
		if (char* Extension = strext(Normalized);
			Extension && (_stricmp(Extension, ".tga") == 0 ||
						  _stricmp(Extension, ".dds") == 0 ||
						  _stricmp(Extension, ".bmp") == 0 ||
						  _stricmp(Extension, ".ogm") == 0))
		{
			*Extension = 0;
		}

		string_path FileName = {};
		bool Found = FS.exist(FileName, "$level$", Normalized, ".dds") ||
					 FS.exist(FileName, "$game_saves$", Normalized, ".dds") ||
					 FS.exist(FileName, _game_textures_, Normalized, ".dds");
		if (!Found)
		{
			xr_string LooseName = xr_string(Normalized) + ".dds";
			if (FS.TryLoad(LooseName))
			{
				xr_strcpy(FileName, LooseName.c_str());
				Found = true;
			}
		}
		if (!Found)
		{
			Found = FS.exist(FileName, _game_textures_, "ed\\ed_not_existing_texture", ".dds");
		}
		if (!Found)
		{
			return nullptr;
		}

		IReader* Reader = FS.r_open(FileName);
		if (!Reader)
		{
			return nullptr;
		}
		RedImageTool::RedImage Image;
		const bool Loaded = Image.LoadFromMemory(Reader->pointer(), Reader->length());
		FS.r_close(Reader);
		if (!Loaded || Image.IsCubeMap() || Image.GetDepth() != 1)
		{
			return nullptr;
		}
		Image.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);

		FEditorTextureUpload Upload;
		Upload.Width = Image.GetWidth();
		Upload.Height = Image.GetHeight();
		Upload.RowPitch = Upload.Width * 4;
		Upload.Format = EEditorTextureFormat::Rgba8Unorm;
		Upload.Pixels = std::span(reinterpret_cast<const std::byte*>(*Image), static_cast<std::size_t>(Upload.RowPitch) * Upload.Height);
		Upload.Revision = 1;
		Upload.DebugName = Texture;
		const FEditorTextureHandle Handle = EditorRenderer.CreateTexture(Upload);
		if (!Handle.IsValid())
		{
			return nullptr;
		}
		EditorTextureStack.emplace(Texture, Handle);
		return EditorRenderer.GetTextureSurface(Handle).ImGuiTextureId;
	}

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

ImTextureID TUI::GetImGuiTexture(const ref_texture& Texture) const
{
	if (!Texture)
	{
		return nullptr;
	}
	if (GetEditorRenderBackend().GetKind() == EEditorRenderBackendKind::Tiramisu)
	{
		return Texture->cName.size() ? LoadTexture(*Texture->cName) : nullptr;
	}
	if (!Texture->pSurface)
	{
		Texture->Load();
	}
	return Texture->get_SRView() ? Texture->get_SRView()->GetRawSRV() : nullptr;
}

ImTextureID TUI::GetImGuiTexture(const FEditorTextureHandle Handle) const
{
	return GetEditorTextureSurface(Handle).ImGuiTextureId;
}

bool TUI::UpdateEditorTexture(FEditorTextureHandle& Handle, const FEditorTextureUpload& Upload) const
{
	IEditorRenderBackend& Renderer = GetEditorRenderBackend();
	if (!Handle.IsValid())
	{
		Handle = Renderer.CreateTexture(Upload);
		return Handle.IsValid();
	}
	return Renderer.UpdateTexture(Handle, Upload);
}

void TUI::DestroyEditorTexture(FEditorTextureHandle& Handle) const
{
	if (!Handle.IsValid())
	{
		return;
	}
	GetEditorRenderBackend().DestroyTexture(Handle);
	Handle = {};
}

FEditorViewportSurface TUI::GetEditorTextureSurface(
	const FEditorTextureHandle Handle
) const
{
	return GetEditorRenderBackend().GetTextureSurface(Handle);
}

bool TUI::UpdateImGuiTexture(FEditorTextureHandle& Handle, const void* Pixels, const u32 Width, const u32 Height, const u32 RowPitch, const u64 Revision, const char* DebugName, const EEditorTextureFormat Format, const bool FlipVertical) const
{
	if (!Pixels)
	{
		return false;
	}
	FEditorTextureUpload Upload;
	Upload.Width = Width;
	Upload.Height = Height;
	Upload.RowPitch = RowPitch;
	Upload.Format = Format;
	std::vector<std::byte> FlippedPixels;
	if (FlipVertical)
	{
		FlippedPixels.resize(static_cast<std::size_t>(RowPitch) * Height);
		const auto* Source = reinterpret_cast<const std::byte*>(Pixels);
		for (u32 Y = 0; Y < Height; ++Y)
		{
			memcpy(FlippedPixels.data() + static_cast<std::size_t>(Y) * RowPitch, Source + static_cast<std::size_t>(Height - Y - 1) * RowPitch, RowPitch);
		}
		Upload.Pixels = FlippedPixels;
	}
	else
	{
		Upload.Pixels = std::span(reinterpret_cast<const std::byte*>(Pixels), static_cast<std::size_t>(RowPitch) * Height);
	}
	Upload.Revision = Revision;
	Upload.DebugName = DebugName ? DebugName : "editor-ui-texture";
	return UpdateEditorTexture(Handle, Upload);
}

void TUI::DestroyImGuiTexture(FEditorTextureHandle& Handle) const
{
	DestroyEditorTexture(Handle);
}

void TUI::OnDeviceCreate()
{
	DU_impl.OnDeviceCreate();
}

void TUI::OnDeviceDestroy()
{
	DU_impl.OnDeviceDestroy();
	DestroyImGuiTexture(m_HeaderLogoEditor);
	DestroyImGuiTexture(m_WinMinEditor);
	DestroyImGuiTexture(m_WinResEditor);
	DestroyImGuiTexture(m_WinMaxEditor);
	DestroyImGuiTexture(m_WinCloseEditor);
	IEditorRenderBackend& EditorRenderer = GetEditorRenderBackend();
	for (const auto& [Name, Handle] : EditorTextureStack)
	{
		(void)Name;
		EditorRenderer.DestroyTexture(Handle);
	}
	EditorTextureStack.clear();
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
	//	m_ShiftState = Shift;
	//	Log("Dn  ",Shift.Contains(ssShift)?"1":"0");
	if (UI->CurrentView().m_Camera.KeyDown(Key, Shift))
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
	//	m_ShiftState = Shift;
	if (UI->CurrentView().m_Camera.KeyUp(Key, Shift))
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
	if (!UI->CurrentView().m_Camera.MoveStart(m_ShiftState))
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
				UI->CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp);
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

	if (UI->CurrentView().m_Camera.IsMoving())
	{
		if (UI->CurrentView().m_Camera.MoveEnd(m_ShiftState))
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
				UI->CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp);
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

	if (!UI->CurrentView().m_Camera.Process(m_ShiftState, x, y))
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
				UI->CurrentView().m_Camera.MouseRayFromPoint(m_CurrentRStart, m_CurrentRDir, m_CurrentCp);
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
	UI->RT.destroy();
	UI->RT.create("$user$rt_color", UI->GetRenderWidth(), UI->GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
}

extern ENGINE_API xr_atomic_bool g_bRendering;
ECORE_API xrCriticalSection temp_render_lock;

void TUI::Redraw()
{
	xrCriticalSectionGuard guard_lock(temp_render_lock);

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
			View.RTFreez.create(("$user$rt_freez" + xr_string::ToString((u32)UI->ViewID)).c_str(), GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);
			RTCopy.create("$user$rt_color_copy", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::B8G8R8X8_UNORM);

			ZB.create("$user$rt_depth", GetRenderWidth(), GetRenderHeight(), ERHI_FORMAT::R24G8_TYPELESS);

			m_Flags.set(flRedraw, true);

			EDevice->m_fNearer = EDevice->mProject._43;
			HalfTarget.x = float(View.RTSize.x) * 0.5f;
			HalfTarget.y = float(View.RTSize.y) * 0.5f;
			EDevice->fASPECT = float(HalfTarget.y) / float(HalfTarget.x);

			EDevice->seqDeviceReset.Process<&pureDeviceReset::OnDeviceReset>();
			EDevice->seqResolutionChanged.Process<&pureScreenResolutionChanged::OnScreenResolutionChanged>();
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
		{
			m_Flags.set(flRedraw, true);
		}

		static u32 redraw_frame = 0;

		if (m_Flags.is(flRedraw))
		{
			redraw_frame = EDevice->dwRenderFrame + 3;
		}

		if (redraw_frame > EDevice->dwRenderFrame || UI->IsPlayInEditor())
		{
			m_Flags.set(flRedraw, false);
			++EDevice->dwRenderFrame;

			float ColorRGBA[4] = {0.0f, 0.0f, 0.0f, 1};
			GRHI->ClearTarget(RTNormal->pRT, ColorRGBA);
			GRHI->ClearTarget(RTDiffuse->pRT, ColorRGBA);
			GRHI->ClearTarget(RTPostion->pRT, ColorRGBA);

			GRHI->SetDepthStencilView(nullptr);

			RCache.set_RT(RT->pRT);
			GRHI->SetDepthStencilView(ZB->pZRT);

			EDevice->Clear();

			RCache.set_RT(RTDiffuse->pRT, 1);
			RCache.set_RT(RTNormal->pRT, 2);
			RCache.set_RT(RTPostion->pRT, 3);

			RCache.set_Stencil(true, D3DCMP_ALWAYS, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			EDevice->UpdateView();
			EDevice->ResetMaterial();
			if (GetEditorRenderBackend().GetKind() ==
				EEditorRenderBackendKind::Tiramisu)
			{
				BeginEditorDebugDrawCapture();
				// The scene packet is submitted from Tools->Render(), before the
				// legacy selection rectangle draw below. Publish its screen-space
				// geometry up front so the same redraw owns both scene and overlay.
				if (m_SelectionRect && EDevice->TargetWidth != 0 &&
					EDevice->TargetHeight != 0)
				{
					constexpr std::uint32_t SelectionColor = 0x407fff7fu;
					auto MakeVertex = [&](const float X, const float Y)
					{
						FEditorOverlayVertex Vertex;
						Vertex.Position = {
							X * EDevice->m_ScreenQuality * 2.0f /
									static_cast<float>(EDevice->TargetWidth) -
								1.0f,
							1.0f - Y * EDevice->m_ScreenQuality * 2.0f /
									   static_cast<float>(EDevice->TargetHeight),
							0.0f
						};
						constexpr float Scale = 1.0f / 255.0f;
						Vertex.Color = {
							static_cast<float>((SelectionColor >> 16u) & 0xffu) * Scale,
							static_cast<float>((SelectionColor >> 8u) & 0xffu) * Scale,
							static_cast<float>(SelectionColor & 0xffu) * Scale,
							static_cast<float>(SelectionColor >> 24u) * Scale
						};
						return Vertex;
					};
					const FEditorOverlayVertex TopLeft = MakeVertex(
						static_cast<float>(m_SelStart.x),
						static_cast<float>(m_SelStart.y)
					);
					const FEditorOverlayVertex BottomLeft = MakeVertex(
						static_cast<float>(m_SelStart.x),
						static_cast<float>(m_SelEnd.y)
					);
					const FEditorOverlayVertex BottomRight = MakeVertex(
						static_cast<float>(m_SelEnd.x),
						static_cast<float>(m_SelEnd.y)
					);
					const FEditorOverlayVertex TopRight = MakeVertex(
						static_cast<float>(m_SelEnd.x),
						static_cast<float>(m_SelStart.y)
					);
					CaptureEditorOverlayTriangle(
						{{TopLeft, BottomLeft, BottomRight}}
					);
					CaptureEditorOverlayTriangle(
						{{TopLeft, BottomRight, TopRight}}
					);
				}
			}

			Tools->RenderEnvironment();

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
			if (GetEditorRenderBackend().GetKind() ==
					EEditorRenderBackendKind::Legacy &&
				psDeviceFlags.test(rsDrawAxis) &&
				!psDeviceFlags.test(rsDisableAxisCube))
			{
				DU_impl.DrawAxis(UI->CurrentView().m_Camera.GetTransform());
			}


			EDevice->Statistic->RenderDUMP_RT.End();
			EDevice->Statistic->Show();

			g_FontManager->Render();

			EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);
			EDevice->seqRender.Process<&pureRender::OnRender>();

			if (g_pGamePersistent->OnRenderPPUI_query())
			{
				g_pGamePersistent->OnRenderPPUI_main();
			}

			RCache.set_RT(0, 1);
			RCache.set_RT(0, 2);
			RCache.set_RT(0, 3);
		}

		g_bRendering = false;

		// end draw
		UI->BeginFrame();

		Draw();

		ID3D11RenderTargetView* RTV = (ID3D11RenderTargetView*)RSwapchainTarget->GetRawRTV();
		RContext->OMSetRenderTargets(1, &RTV, 0);
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
				UI->Quit();
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

	EDevice->Initialize();
	// Creation
	extern CDB::COLLIDER XRC;
	XRC.ray_options(CDB::OPT_ONLYNEAREST | CDB::OPT_CULL);

	pInput = new CInput(false, all_device_key);

	Console = new CConsole();
	Console->Initialize();

	UI->IR_Capture();

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

	ZB.create("$user$rt_depth", GetRenderWidth() * EDevice->m_ScreenQuality, GetRenderHeight() * EDevice->m_ScreenQuality, ERHI_FORMAT::R24G8_TYPELESS);

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
	m_bReady = false;
	UI->IR_Release();
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
	SPBItem* pbi = UI->ProgressLast();
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

namespace
{
void UploadEditorSvgIcon(FEditorTextureHandle& Handle, const char* SvgText, const int Width, const int Height, const char* DebugName)
{
	if (GetEditorRenderBackend().GetKind() != EEditorRenderBackendKind::Tiramisu)
	{
		return;
	}
	const auto Document = lunasvg::Document::loadFromData(SvgText);
	if (!Document)
	{
		return;
	}
	const auto Bitmap = Document->renderToBitmap(
		Width * GUIManager->GetScaleDpi(), Height * GUIManager->GetScaleDpi()
	);
	(void)UI->UpdateImGuiTexture(Handle, Bitmap.data(), Bitmap.width(), Bitmap.height(), Bitmap.width() * 4, 1, DebugName);
}
} // namespace

void TUI::InitWindowIcons()
{
	m_HeaderLogo = chezze_svg_temporary::RasterizeSvg(IX_RAY_LOGO, 64, 64); // EDevice->Resources->_CreateTexture("ed\\bar\\win_header_logo");
	m_WinMin = chezze_svg_temporary::RasterizeSvg(IX_MIN_ICON, 10, 10);
	m_WinMax = chezze_svg_temporary::RasterizeSvg(IX_MAX_ICON, 10, 10);
	m_WinRes = chezze_svg_temporary::RasterizeSvg(IX_RESTORE_ICON, 10, 10);
	m_WinClose = chezze_svg_temporary::RasterizeSvg(IX_CLOSE_ICON, 10, 10);
	UploadEditorSvgIcon(m_HeaderLogoEditor, IX_RAY_LOGO, 64, 64, "editor-window-logo");
	UploadEditorSvgIcon(m_WinMinEditor, IX_MIN_ICON, 10, 10, "editor-window-minimize");
	UploadEditorSvgIcon(m_WinMaxEditor, IX_MAX_ICON, 10, 10, "editor-window-maximize");
	UploadEditorSvgIcon(m_WinResEditor, IX_RESTORE_ICON, 10, 10, "editor-window-restore");
	UploadEditorSvgIcon(m_WinCloseEditor, IX_CLOSE_ICON, 10, 10, "editor-window-close");
}

void TUI::OnDrawUI()
{
	if (GetEditorRenderBackend().GetKind() == EEditorRenderBackendKind::Tiramisu)
	{
		GUIManager->SearchIcon = LoadTexture("ed\\content_browser_search");
		UIChooseForm::SetNullTexture(LoadTexture("ed\\ed_nodata"));
	}
	UIKeyPressForm::Update(EDevice->fTimeGlobal);
	UIEditLightAnim::Update();
	UIImageEditorForm::Update();
	UISoundEditorForm::Update();
	UIMinimapEditorForm::Update();
	UIWeatherPropForm::Update();
	UIIconPicker::Update();
	UILogForm::Update();
	EDevice->seqDrawUI.Process<&pureDrawUI::OnDrawUI>();
}

void TUI::RealResetUI()
{
	m_Flags.set(flResetUI, false);
	string_path ini_path;
	if (FS.exist(ini_path, "$server_data_root$", UI->EditorName(), "_imgui_default.ini"))
	{
		UI->Resize(1280, 800);
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
	UI->ProgressDraw();
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
		UI->ProgressDraw();
	}
}
