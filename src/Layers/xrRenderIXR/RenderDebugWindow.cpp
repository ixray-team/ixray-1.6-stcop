/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include <magic_enum/magic_enum.hpp>
#include "xrRender_console.h"
#include "ImGuiNri.h"
#include "../../../shaders/common/debug_view.hlsli"

namespace
{
struct TargetView
{
	bool IsShown = false;
	bool Channels[3] = { true, true, true };
	float Brightness = 1.0f;
};

xr_map<xr_string, TargetView> TargetViews;
bool IsTiled = false;
int TileColumns = 2;

constexpr float IconSize = 28.0f;
using IconDraw = void (*)(ImDrawList& List, const ImVec2& At, ImU32 Colour);

ImVec2 IconPoint(const ImVec2& At, float X, float Y)
{
	return ImVec2(At.x + X * IconSize, At.y + Y * IconSize);
}

void IconOverdrawRaster(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	for (float Step = 0.0f; Step < 3.0f; ++Step)
	{
		List.AddRect(IconPoint(At, 0.15f + Step * 0.15f, 0.15f + Step * 0.15f), IconPoint(At, 0.55f + Step * 0.15f, 0.55f + Step * 0.15f), Colour, 0.0f, 0, 1.5f);
	}
}

void IconOverdrawShaded(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddRect(IconPoint(At, 0.15f, 0.15f), IconPoint(At, 0.55f, 0.55f), Colour, 0.0f, 0, 1.5f);
	List.AddRectFilled(IconPoint(At, 0.4f, 0.4f), IconPoint(At, 0.85f, 0.85f), Colour);
}

void IconClusters(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddRectFilled(IconPoint(At, 0.18f, 0.18f), IconPoint(At, 0.46f, 0.46f), Colour);
	List.AddRect(IconPoint(At, 0.54f, 0.18f), IconPoint(At, 0.82f, 0.46f), Colour, 0.0f, 0, 1.5f);
	List.AddRect(IconPoint(At, 0.18f, 0.54f), IconPoint(At, 0.46f, 0.82f), Colour, 0.0f, 0, 1.5f);
	List.AddRectFilled(IconPoint(At, 0.54f, 0.54f), IconPoint(At, 0.82f, 0.82f), Colour);
}

void IconTriangles(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddTriangle(IconPoint(At, 0.5f, 0.18f), IconPoint(At, 0.84f, 0.8f), IconPoint(At, 0.16f, 0.8f), Colour, 2.0f);
}

void IconInstances(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddRect(IconPoint(At, 0.18f, 0.34f), IconPoint(At, 0.66f, 0.82f), Colour, 0.0f, 0, 1.5f);
	List.AddLine(IconPoint(At, 0.18f, 0.34f), IconPoint(At, 0.36f, 0.18f), Colour, 1.5f);
	List.AddLine(IconPoint(At, 0.66f, 0.34f), IconPoint(At, 0.84f, 0.18f), Colour, 1.5f);
	List.AddLine(IconPoint(At, 0.66f, 0.82f), IconPoint(At, 0.84f, 0.66f), Colour, 1.5f);
	List.AddLine(IconPoint(At, 0.36f, 0.18f), IconPoint(At, 0.84f, 0.18f), Colour, 1.5f);
	List.AddLine(IconPoint(At, 0.84f, 0.18f), IconPoint(At, 0.84f, 0.66f), Colour, 1.5f);
}

void IconMaterials(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddCircle(IconPoint(At, 0.5f, 0.5f), 9.0f, Colour, 20, 1.5f);
	List.PathArcTo(IconPoint(At, 0.5f, 0.5f), 9.0f, 1.5708f, 4.7124f, 12);
	List.PathFillConvex(Colour);
}

void IconLights(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddCircleFilled(IconPoint(At, 0.5f, 0.5f), 4.0f, Colour, 12);

	for (u32 Ray = 0; Ray < 8; ++Ray)
	{
		const float Angle = float(Ray) * 0.7853982f;
		List.AddLine(ImVec2(At.x + 14.0f + std::cos(Angle) * 7.0f, At.y + 14.0f + std::sin(Angle) * 7.0f), ImVec2(At.x + 14.0f + std::cos(Angle) * 10.5f, At.y + 14.0f + std::sin(Angle) * 10.5f), Colour, 1.5f);
	}
}

void IconBase(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddCircleFilled(IconPoint(At, 0.5f, 0.5f), 9.0f, Colour, 20);
}

void IconNormal(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddLine(IconPoint(At, 0.15f, 0.8f), IconPoint(At, 0.85f, 0.8f), Colour, 2.0f);
	List.AddLine(IconPoint(At, 0.5f, 0.8f), IconPoint(At, 0.5f, 0.25f), Colour, 2.0f);
	List.AddTriangleFilled(IconPoint(At, 0.5f, 0.12f), IconPoint(At, 0.64f, 0.34f), IconPoint(At, 0.36f, 0.34f), Colour);
}

void IconRoughness(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	ImVec2 Points[8];

	for (u32 Point = 0; Point < 8; ++Point)
	{
		Points[Point] = IconPoint(At, 0.15f + float(Point) * 0.1f, Point & 1 ? 0.35f : 0.65f);
	}

	List.AddPolyline(Points, 8, Colour, 0, 2.0f);
}

void IconHemi(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.PathArcTo(IconPoint(At, 0.5f, 0.7f), 10.0f, 3.1416f, 6.2832f, 16);
	List.PathStroke(Colour, 0, 2.0f);
	List.AddLine(IconPoint(At, 0.12f, 0.7f), IconPoint(At, 0.88f, 0.7f), Colour, 2.0f);
}

void IconLmap(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddRect(IconPoint(At, 0.18f, 0.18f), IconPoint(At, 0.82f, 0.82f), Colour, 0.0f, 0, 1.5f);
	List.AddLine(IconPoint(At, 0.5f, 0.18f), IconPoint(At, 0.5f, 0.82f), Colour, 1.5f);
	List.AddLine(IconPoint(At, 0.18f, 0.5f), IconPoint(At, 0.82f, 0.5f), Colour, 1.5f);
	List.AddRectFilled(IconPoint(At, 0.5f, 0.18f), IconPoint(At, 0.82f, 0.5f), Colour);
}

void IconStreaming(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddLine(IconPoint(At, 0.5f, 0.15f), IconPoint(At, 0.5f, 0.6f), Colour, 2.0f);
	List.AddTriangleFilled(IconPoint(At, 0.3f, 0.5f), IconPoint(At, 0.7f, 0.5f), IconPoint(At, 0.5f, 0.74f), Colour);
	List.AddLine(IconPoint(At, 0.18f, 0.84f), IconPoint(At, 0.82f, 0.84f), Colour, 2.0f);
}

void IconLit(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddCircle(IconPoint(At, 0.5f, 0.5f), 8.0f, Colour, 16, 1.5f);
	List.PathArcTo(IconPoint(At, 0.5f, 0.5f), 8.0f, -1.5708f, 1.5708f, 12);
	List.PathFillConvex(Colour);
}

void IconHighlight(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddCircle(IconPoint(At, 0.5f, 0.5f), 9.0f, Colour, 16, 1.5f);
	List.AddCircleFilled(IconPoint(At, 0.38f, 0.38f), 3.0f, Colour, 10);
}

void IconMirror(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddLine(IconPoint(At, 0.12f, 0.5f), IconPoint(At, 0.88f, 0.5f), Colour, 1.5f);
	List.AddTriangleFilled(IconPoint(At, 0.5f, 0.14f), IconPoint(At, 0.72f, 0.44f), IconPoint(At, 0.28f, 0.44f), Colour);
	List.AddTriangle(IconPoint(At, 0.5f, 0.86f), IconPoint(At, 0.72f, 0.56f), IconPoint(At, 0.28f, 0.56f), Colour, 1.5f);
}

void IconHeightfield(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	const ImVec2 Ridge[] = { IconPoint(At, 0.12f, 0.82f), IconPoint(At, 0.42f, 0.42f), IconPoint(At, 0.58f, 0.6f), IconPoint(At, 0.88f, 0.82f) };
	List.AddPolyline(Ridge, 4, Colour, 0, 2.0f);
	List.AddTriangleFilled(IconPoint(At, 0.42f, 0.42f), IconPoint(At, 0.88f, 0.82f), IconPoint(At, 0.42f, 0.82f), Colour);
	List.AddCircleFilled(IconPoint(At, 0.2f, 0.24f), 3.5f, Colour, 10);
}

void IconTiles(ImDrawList& List, const ImVec2& At, ImU32 Colour)
{
	List.AddRect(IconPoint(At, 0.15f, 0.2f), IconPoint(At, 0.47f, 0.47f), Colour, 0.0f, 0, 1.5f);
	List.AddRect(IconPoint(At, 0.53f, 0.2f), IconPoint(At, 0.85f, 0.47f), Colour, 0.0f, 0, 1.5f);
	List.AddRect(IconPoint(At, 0.15f, 0.53f), IconPoint(At, 0.47f, 0.8f), Colour, 0.0f, 0, 1.5f);
	List.AddRect(IconPoint(At, 0.53f, 0.53f), IconPoint(At, 0.85f, 0.8f), Colour, 0.0f, 0, 1.5f);
}

// The views past DebugView_None, in their order
constexpr IconDraw ViewIcons[DebugView_Count - 1] = { IconOverdrawRaster, IconOverdrawShaded, IconClusters, IconTriangles, IconInstances, IconMaterials, IconLights, IconBase, IconNormal, IconRoughness, IconHemi, IconLmap, IconStreaming, IconLit, IconHighlight, IconMirror, IconHeightfield };

bool IconButton(const char* Id, IconDraw Draw, bool IsActive, const char* Tooltip)
{
	static const ImGuiCol Backgrounds[] = { ImGuiCol_Button, ImGuiCol_ButtonHovered, ImGuiCol_ButtonActive, ImGuiCol_ButtonActive };
	const ImVec2 At = ImGui::GetCursorScreenPos();
	const bool IsPressed = ImGui::InvisibleButton(Id, ImVec2(IconSize, IconSize));
	const bool IsHovered = ImGui::IsItemHovered();
	ImDrawList& List = *ImGui::GetWindowDrawList();
	List.AddRectFilled(At, ImVec2(At.x + IconSize, At.y + IconSize), ImGui::GetColorU32(Backgrounds[IsHovered + 2 * IsActive]), 4.0f);
	Draw(List, At, ImGui::GetColorU32(ImGuiCol_Text));

	if (IsHovered)
	{
		ImGui::SetTooltip("%s", Tooltip);
	}

	return IsPressed;
}

// The targets hold what they hold in alpha: they are drawn with no blending
void DrawTarget(ImDrawList& List, RHIObject* Opaque, CTexture& Texture, const TargetView& View, const ImVec2& Min, const ImVec2& Max, const ImVec2& UvMin = ImVec2(0.0f, 0.0f), const ImVec2& UvMax = ImVec2(1.0f, 1.0f))
{
	const ImVec4 Tint(View.Channels[0] * View.Brightness, View.Channels[1] * View.Brightness, View.Channels[2] * View.Brightness, 1.0f);
	List.AddCallback(ImGuiNri::BlendCallback, Opaque);
	List.AddImage(ImGuiNri::Image(&Texture), Min, Max, UvMin, UvMax, ImGui::GetColorU32(Tint));
	List.AddCallback(ImGuiNri::BlendCallback, nullptr);
}
} // namespace

void CRenderTarget::DrawDebugWindow()
{
	bool& IsOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Shaders)];

	if (!IsOpen)
	{
		return;
	}

	if (!ImGui::Begin("GraphicDebug", &IsOpen, ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoCollapse))
	{
		ImGui::End();
		return;
	}

	// A view an icon, the one that is on lit; pressing it again returns the frame
	for (u32 Mode = 1; Mode < DebugView_Count; ++Mode)
	{
		ImGui::PushID(int(Mode));

		if (IconButton("##view", ViewIcons[Mode - 1], ps_r_debug_view == Mode, debug_view_token[Mode].name))
		{
			ps_r_debug_view = ps_r_debug_view == Mode ? u32(DebugView_None) : Mode;
		}

		ImGui::PopID();
		ImGui::SameLine(0.0f, Mode == DebugView_OverdrawShaded || Mode == DebugView_Materials ? 16.0f : 4.0f);
	}

	ImGui::Dummy(ImVec2(12.0f, IconSize));
	ImGui::SameLine(0.0f, 4.0f);

	if (IconButton("##tiles", IconTiles, IsTiled, "Tiles on screen"))
	{
		IsTiled = !IsTiled;
	}

	if (IsTiled)
	{
		ImGui::SameLine(0.0f, 8.0f);
		ImGui::SetNextItemWidth(120.0f);
		ImGui::SliderInt("##columns", &TileColumns, 1, 4, "%d columns");
	}

	ImGui::Separator();

	CTexture* Tiles[64];
	const TargetView* TileViews[64];
	u32 TileCount = 0;

	const auto Show = [&](const ref_texture& Handle)
	{
		if (!Handle || !Handle->get_SRView())
		{
			return;
		}

		CTexture& Texture = *Handle;
		TargetView& View = TargetViews[Texture.cName.c_str()];
		ImGui::PushID(&Texture);
		ImGui::Checkbox("##show", &View.IsShown);
		ImGui::SameLine();
		ImGui::Text("%s  %s  %ux%u", Texture.cName.c_str(), magic_enum::enum_name(Texture.get_Format()).data(), u32(Texture.get_Width()), u32(Texture.get_Height()));

		if (View.IsShown)
		{
			ImGui::Checkbox("R", &View.Channels[0]);
			ImGui::SameLine();
			ImGui::Checkbox("G", &View.Channels[1]);
			ImGui::SameLine();
			ImGui::Checkbox("B", &View.Channels[2]);
			ImGui::SameLine();
			ImGui::SetNextItemWidth(160.0f);
			ImGui::SliderFloat("Brightness", &View.Brightness, 0.0f, 1.0f);

			if (IsTiled && TileCount < 64)
			{
				Tiles[TileCount] = &Texture;
				TileViews[TileCount++] = &View;
			}
			else if (!IsTiled)
			{
				const float Width = ImGui::GetContentRegionAvail().x;
				const ImVec2 Size(Width, Width * float(Texture.get_Height()) / float(Texture.get_Width()));
				const ImVec2 Min = ImGui::GetCursorScreenPos();
				ImGui::Dummy(Size);
				DrawTarget(*ImGui::GetWindowDrawList(), g_debug_blend_state, Texture, View, Min, ImVec2(Min.x + Size.x, Min.y + Size.y));

				// Under the cursor: 32 texels of the target, eight times as large
				if (ImGui::IsItemHovered())
				{
					const ImVec2 Mouse = ImGui::GetMousePos();
					const ImVec2 At((Mouse.x - Min.x) / Size.x, (Mouse.y - Min.y) / Size.y);
					const ImVec2 Half(16.0f / float(Texture.get_Width()), 16.0f / float(Texture.get_Height()));
					ImGui::BeginTooltip();
					ImGui::Text("%u, %u", u32(At.x * float(Texture.get_Width())), u32(At.y * float(Texture.get_Height())));
					const ImVec2 ZoomMin = ImGui::GetCursorScreenPos();
					ImGui::Dummy(ImVec2(256.0f, 256.0f));
					DrawTarget(*ImGui::GetWindowDrawList(), g_debug_blend_state, Texture, View, ZoomMin, ImVec2(ZoomMin.x + 256.0f, ZoomMin.y + 256.0f), ImVec2(At.x - Half.x, At.y - Half.y), ImVec2(At.x + Half.x, At.y + Half.y));
					ImGui::EndTooltip();
				}
			}
		}

		ImGui::PopID();
	};

	for (const ref_rt& Each : { rt_Generic_0, rt_Thin, rt_Ids, rt_Surface, rt_Velocity, rt_Accumulator, rt_Revealage, rt_Forward, rt_Position, rt_Back_Buffer_AA, rt_Back_Buffer, rt_BackbufferLUT, rt_Generic, rt_Generic_1,
			 rt_Generic_2, rt_upscaled_depth, rt_sslr, rt_sslr_temp, rt_sslr_data, rt_ssao_temp, rt_ui_pda, rt_Bloom_A, rt_Bloom_A2, rt_LUM_A, rt_LUM_B, rt_LUM_C, rt_LUM_D, rt_Reflection_forward })
	{
		Show(Each ? Each->pTexture : ref_texture());
	}

	Show(rt_smap_depth);
	Show(rt_smap_depth_sun);

	// The shown targets over the game's picture, a grid of them in a window of their own behind the others
	const CImGuiManager::GameViewRect Picture = CImGuiManager::Instance().GetGameViewRect();
	const ImVec2 Screen = Picture.Size;
	const ImVec2 Origin = Picture.Pos;

	// With nothing shown the window is not begun: a place set for it would go to the next window of the frame
	if (TileCount)
	{
		ImGui::SetNextWindowViewport(CImGuiManager::Instance().GetGameViewport()->ID);
		ImGui::SetNextWindowPos(Origin);
		ImGui::SetNextWindowSize(Screen);
	}

	constexpr ImGuiWindowFlags TileFlags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_NoBackground | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoFocusOnAppearing |
		ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoNav;

	if (TileCount && ImGui::Begin("##GraphicDebugTiles", nullptr, TileFlags))
	{
		ImDrawList& List = *ImGui::GetWindowDrawList();
		const u32 Columns = std::min(u32(TileColumns), TileCount);
		const u32 Rows = (TileCount + Columns - 1) / Columns;
		const ImVec2 Cell(Screen.x / float(Columns), Screen.y / float(Rows));

		for (u32 Tile = 0; Tile < TileCount; ++Tile)
		{
			CTexture& Texture = *Tiles[Tile];
			const float Fit = std::min(Cell.x / float(Texture.get_Width()), Cell.y / float(Texture.get_Height()));
			const ImVec2 Size(float(Texture.get_Width()) * Fit, float(Texture.get_Height()) * Fit);
			const ImVec2 Min(Origin.x + Cell.x * float(Tile % Columns) + (Cell.x - Size.x) * 0.5f, Origin.y + Cell.y * float(Tile / Columns) + (Cell.y - Size.y) * 0.5f);
			DrawTarget(List, g_debug_blend_state, Texture, *TileViews[Tile], Min, ImVec2(Min.x + Size.x, Min.y + Size.y));
			List.AddText(ImVec2(Min.x + 6.0f, Min.y + 4.0f), IM_COL32(255, 255, 0, 255), Texture.cName.c_str());
		}
	}

	if (TileCount)
	{
		ImGui::End();
	}

	ImGui::Separator();
	static int StackLevels = 0;
	ImGui::SliderInt("Stack Levels", &StackLevels, 0, 8);

	const auto& Stats = GRHI->GPUStats();

	for (size_t Index = 0; Index < Stats.count; ++Index)
	{
		const auto& Event = Stats.events[Index];

		if (Event.stack < StackLevels)
		{
			ImGui::Text("%*s%s: %.3fms", Event.stack * 2, " ", Event.name.c_str(), float((Event.end - Event.begin) / (Event.freq / 1000000)) * 0.001f);
		}
	}

	ImGui::End();
}
