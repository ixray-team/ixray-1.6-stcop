#include "stdafx.h"
#include "UITerrainTool.h"
#include "IconsFontAwesome6.h"
#include "../../Editor/Tools/Terrain/ESceneTerrainTools.h"

static const u32 g_TerrainRes[] = { 129, 257, 513 };

UITerrainTool::UITerrainTool()
{
	m_CreateRes = 0;
	m_CreateHeight = 0.5f;
}

UITerrainTool::~UITerrainTool()
{
}

void UITerrainTool::Draw()
{
	const float TableRowHeight = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);
	const float ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;

	if (XRay::ImGui::BeginDarkChild("TerrainToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Create Heightmap"))
		{
			const char* res_items[] = { "129 x 129", "257 x 257", "513 x 513" };
			ImGui::SetNextItemWidth(-0.01f);
			ImGui::Combo("Resolution", &m_CreateRes, res_items, sizeof(res_items) / sizeof(res_items[0]));

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderFloat("Base Height", &m_CreateHeight, 0.01f, 1.f, "%.2f");

			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			if (XRay::ImGui::Button("Create", { -0.01f, 0 }))
			{
				u32 res = g_TerrainRes[m_CreateRes];
				tool->CreateTerrain("terrain", res, res, m_CreateHeight);
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Create 1024", { -0.01f, 0 }))
			{
				tool->CreateTerrain("terrain", 1025, 1025, m_CreateHeight);
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Sculpt"))
		{
			bool inSculpt = (LTools && LTools->GetSubTarget() == ESceneTerrainTool::estTerrainSculpt);

			if (!inSculpt)
			{
				if (XRay::ImGui::Button("Enter Sculpt Mode", { -0.01f, 0 }))
				{
					ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_TERRAIN, ESceneTerrainTool::estTerrainSculpt);
					ExecCommand(COMMAND_CHANGE_ACTION, etaAdd);
				}
			}
			else
			{
				if (XRay::ImGui::Button("Exit Sculpt Mode", { -0.01f, 0 }))
				{
					ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_TERRAIN);
				}
			}

			XRay::ImGui::Separator();

			const char* modes[] = { "Raise", "Lower", "Smooth", "Flatten" };
			float colW = (ImGui::GetContentRegionAvail().x - ItemSpacingX * 3) / 4;
			for (int i = 0; i < 4; ++i)
			{
				bool active = (tool->m_BrushMode == (ESceneTerrainTool::ETerrainBrushMode)i);
				if (XRay::ImGui::ToggleButton(modes[i], &active, { colW, 0 }))
					tool->m_BrushMode = (ESceneTerrainTool::ETerrainBrushMode)i;
				if (i < 3) ImGui::SameLine(0, ItemSpacingX);
			}

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderInt("Brush Radius", &tool->m_BrushSize, 1, 200);

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderFloat("Brush Strength", &tool->m_BrushStrength, 0.001f, 0.2f, "%.3f");

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar();
		XRay::ImGui::EndDarkChild();
	}
}
