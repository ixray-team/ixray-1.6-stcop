#include "stdafx.h"
#include "ImGuiManager.h"
#include "IGame_Persistent.h"

#include <imgui.h>
#include <imgui_internal.h>

constexpr u32 MainViewportSlot = 0;

namespace ImGui
{
	ImFont* LightFont = nullptr;
	ImFont* RegularFont = nullptr;
	ImFont* MediumFont = nullptr;
	ImFont* BoldFont = nullptr;
}

void CImGuiManager::LoadImGuiFont(ImFont*& FontHandle, const char* Font)
{
	string_path FullPath;
	FS.update_path(FullPath, _game_fonts_, Font);
	ImFontConfig FontConfig = {};
	FontConfig.OversampleH = 2;

	if (FS.exist(FullPath))
	{
		IReader* FontReader = FS.r_open(FullPath);

		FontHandle = ImGui::GetIO().Fonts->AddFontFromMemoryTTF(FontReader->pointer(), FontReader->length(), 16.0f, &FontConfig, ImGui::GetIO().Fonts->GetGlyphRangesCyrillic());
		R_ASSERT(FontHandle);

		ImGuiFontsPtr.push_back(FontReader);
	}
}

void CImGuiManager::InitPlatform()
{
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO();
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls
	io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;         // Enable Docking
	//io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;       // Enable Multi-Viewport / Platform Windows

	ImGuiPlatformIO& platform_io = ImGui::GetPlatformIO();

	ImGuiStyle& Style = ImGui::GetStyle();
	Style.WindowPadding.x = 8;
	Style.WindowPadding.y = 8;
	Style.FramePadding.x = 5;
	Style.FramePadding.y = 5;
	Style.CellPadding.x = 2;
	Style.CellPadding.y = 2;
	Style.ItemSpacing.x = 8;
	Style.ItemSpacing.y = 4;
	Style.ItemInnerSpacing.x = 6;
	Style.ItemInnerSpacing.y = 6;
	Style.ScrollbarSize = 16;
	Style.GrabMinSize = 16;
	Style.FrameRounding = 2;
	Style.PopupRounding = 2;
	Style.ScrollbarRounding = 2;
	Style.GrabRounding = 2;
	Style.TabRounding = 2;
	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		Style.WindowRounding = 0.0f;
		Style.Colors[ImGuiCol_WindowBg].w = 1.0f;
	}

	ImVec4* colors = Style.Colors;
	colors[ImGuiCol_Text] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
	colors[ImGuiCol_TextDisabled] = ImVec4(0.26f, 0.26f, 0.26f, 1.00f);
	colors[ImGuiCol_WindowBg] = ImVec4(0.06f, 0.06f, 0.06f, 1.00f);
	colors[ImGuiCol_ChildBg] = ImVec4(0.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PopupBg] = ImVec4(0.08f, 0.08f, 0.08f, 1.00f);
	colors[ImGuiCol_Border] = ImVec4(0.16f, 0.16f, 0.18f, 0.50f);
	colors[ImGuiCol_BorderShadow] = ImVec4(0.05f, 0.05f, 0.05f, 0.00f);
	colors[ImGuiCol_FrameBg] = ImVec4(0.13f, 0.13f, 0.13f, 0.54f);
	colors[ImGuiCol_FrameBgHovered] = ImVec4(0.27f, 0.27f, 0.27f, 0.54f);
	colors[ImGuiCol_FrameBgActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_TitleBg] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_TitleBgActive] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_MenuBarBg] = ImVec4(0.06f, 0.06f, 0.06f, 1.00f);
	colors[ImGuiCol_ScrollbarBg] = ImVec4(0.02f, 0.02f, 0.02f, 0.53f);
	colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.31f, 0.31f, 0.31f, 1.00f);
	colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.41f, 0.41f, 0.41f, 1.00f);
	colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.51f, 0.51f, 0.51f, 1.00f);
	colors[ImGuiCol_CheckMark] = ImVec4(0.83f, 0.83f, 0.83f, 0.54f);
	colors[ImGuiCol_SliderGrab] = ImVec4(0.51f, 0.51f, 0.51f, 1.00f);
	colors[ImGuiCol_SliderGrabActive] = ImVec4(0.95f, 0.95f, 0.95f, 1.00f);
	colors[ImGuiCol_Button] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_ButtonHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_ButtonActive] = ImVec4(0.75f, 0.75f, 0.75f, 0.54f);
	colors[ImGuiCol_Header] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_HeaderHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_HeaderActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_Separator] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_SeparatorHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_SeparatorActive] = ImVec4(0.75f, 0.74f, 0.74f, 0.54f);
	colors[ImGuiCol_ResizeGrip] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_ResizeGripHovered] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_ResizeGripActive] = ImVec4(0.75f, 0.74f, 0.74f, 0.54f);
	colors[ImGuiCol_Tab] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_TabHovered] = ImVec4(0.30f, 0.30f, 0.30f, 1.00f);
	colors[ImGuiCol_TabActive] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_TabUnfocused] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_TabUnfocusedActive] = ImVec4(0.42f, 0.42f, 0.42f, 0.54f);
	colors[ImGuiCol_DockingPreview] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_DockingEmptyBg] = ImVec4(0.05f, 0.05f, 0.05f, 1.00f);
	colors[ImGuiCol_PlotLines] = ImVec4(0.61f, 0.61f, 0.61f, 1.00f);
	colors[ImGuiCol_PlotLinesHovered] = ImVec4(1.00f, 0.43f, 0.35f, 1.00f);
	colors[ImGuiCol_PlotHistogram] = ImVec4(0.90f, 0.70f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotHistogramHovered] = ImVec4(1.00f, 0.60f, 0.00f, 1.00f);
	colors[ImGuiCol_TableHeaderBg] = ImVec4(0.19f, 0.19f, 0.20f, 1.00f);
	colors[ImGuiCol_TableBorderStrong] = ImVec4(0.31f, 0.31f, 0.35f, 1.00f);
	colors[ImGuiCol_TableBorderLight] = ImVec4(0.23f, 0.23f, 0.25f, 1.00f);
	colors[ImGuiCol_TableRowBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
	colors[ImGuiCol_TableRowBgAlt] = ImVec4(1.00f, 1.00f, 1.00f, 0.06f);
	colors[ImGuiCol_TextSelectedBg] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_DragDropTarget] = ImVec4(1.00f, 1.00f, 0.00f, 0.90f);
	colors[ImGuiCol_NavHighlight] = ImVec4(0.26f, 0.59f, 0.98f, 1.00f);
	colors[ImGuiCol_NavWindowingHighlight] = ImVec4(1.00f, 1.00f, 1.00f, 0.70f);
	colors[ImGuiCol_NavWindowingDimBg] = ImVec4(0.80f, 0.80f, 0.80f, 0.20f);
	colors[ImGuiCol_ModalWindowDimBg] = ImVec4(0.80f, 0.80f, 0.80f, 0.35f);

	ImFontConfig FontConfig = {};
	FontConfig.OversampleH = 2;

	LoadImGuiFont(ImGui::RegularFont, "rus\\RobotoMono.ttf");
	LoadImGuiFont(ImGui::LightFont, "rus\\RobotoMono-Light.ttf");
	LoadImGuiFont(ImGui::MediumFont, "rus\\RobotoMono-Medium.ttf");
	LoadImGuiFont(ImGui::BoldFont, "rus\\RobotoMono-Bold.ttf");

	io.Fonts->Build();

	PlatformInitCallback();
}

void CImGuiManager::InitHardware()
{
	HardwareInitCallback();
}

void CImGuiManager::Reset()
{
	if (HardwareResetCallback)
	{
		HardwareResetCallback();
	}
}

void CImGuiManager::Destroy(bool HardwareOnly)
{
	if (HardwareOnly)
	{
		HardwareDestroyCallback();

		HardwareInitCallback = nullptr;
		HardwareResetCallback = nullptr;
		HardwareDestroyCallback = nullptr;
		HardwareNewFrameCallback = nullptr;
		HardwareDrawDataCallback = nullptr;

		return;
	}

	PlatformDestroyCallback();

	for (IReader* FontPtr : ImGuiFontsPtr)
	{
		FS.r_close(FontPtr);
	}

	RenderFrameData.clear();

	PlatformInitCallback = nullptr;
	PlatformDestroyCallback = nullptr;
	PlatformNewFrameCallback = nullptr;
}

void CImGuiManager::ApplyMainViewport(ImGuiCallback RenderFunction)
{
	RenderFrameData[MainViewportSlot] = { "MainViewPort", RenderFunction };
}

void CImGuiManager::Subscribe(shared_str Name, u32 RenderPriority, ImGuiCallback&& RenderFunction)
{
	if (RenderPriority == MainViewportSlot)
	{
		RenderPriority++;
	}

	do
	{
		if (!RenderFrameData.contains(RenderPriority))
		{
			RenderFrameData[RenderPriority] = { Name, RenderFunction };
			break;
		}
		RenderPriority++;
	} while (true);
}

void CImGuiManager::Unsubscribe(shared_str Name)
{
	u32 EraseID = u32(-1);
	for (auto& [ID, Data] : RenderFrameData)
	{
		if (Data.Name == Name)
		{
			EraseID = ID;
			break;
		}
	}

	if (EraseID != u32(-1))
	{
		RenderFrameData.erase(EraseID);
	}
}

void CImGuiManager::BeginRender()
{
	HardwareNewFrameCallback();
}

void CImGuiManager::AfterRender()
{
	ImGuiIO& io = ImGui::GetIO();

	// Update and Render additional Platform Windows
	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		ImGui::UpdatePlatformWindows();
		ImGui::RenderPlatformWindowsDefault();
	}
}

void CImGuiManager::Render()
{
	ImGui::NewFrame();

#ifdef DEBUG_DRAW
	if (DrawUIRender)
	{
		for (const auto& [Id, CommandData] : RenderFrameData)
		{
			CommandData.Function();
		}

		if (!CaptureInputs)
		{
			for (auto& Window : ImGui::GetCurrentContext()->Windows) 
			{
				Window->Flags |= ImGuiWindowFlags_NoInputs;
			}
		}
	}
	else 
	{
#endif
		RenderFrameData[MainViewportSlot].Function();
#ifdef DEBUG_DRAW
	}
#endif

	ImGui::Render();

	HardwareDrawDataCallback();
}

void CImGuiManager::UpdateCapture()
{
	if (ImGui::IsKeyPressed(ImGuiKey_I) && ImGui::IsKeyDown(ImGuiKey_LeftAlt))
	{
		if (ImGui::IsKeyDown(ImGuiKey_LeftCtrl))
		{
			DrawUIRender = !DrawUIRender;

			if (!DrawUIRender)
				CaptureInputs = false;
		}
		else if (DrawUIRender)
		{
			CaptureInputs = !CaptureInputs;
		}
	}

	if (CaptureInputs || g_dedicated_server)
	{
		SDL_ShowCursor();
	}
	else
	{
		SDL_HideCursor();
	}
}

bool CImGuiManager::IsCapturingInputs() const
{
	return CaptureInputs;
}

void CImGuiManager::NewPlatformFrame() const
{
	PlatformNewFrameCallback();
}

void CImGuiManager::NewHardwareFrame() const
{
	HardwareNewFrameCallback();
}

CImGuiManager& CImGuiManager::Instance()
{
	static CImGuiManager Object;
	return Object;
}
