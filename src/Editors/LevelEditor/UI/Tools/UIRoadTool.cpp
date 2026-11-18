#include "stdafx.h"
#include "UIRoadTool.h"
#include "IconsFontAwesome7.h"
#include "../../Editor/Tools/Road/ESceneRoadTools.h"

// Tool panel holds actions only. Road and point parameters live in the Properties panel.

namespace
{
	void Hint(const char* Text)
	{
		if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
		{
			ImGui::SetTooltip("%s", Text);
		}
	}
}

void UIRoadTool::Draw()
{
	const float ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;
	CRoad* Road = tool->GetActiveRoad();

	if (XRay::ImGui::BeginDarkChild("RoadToolsBorder", {0, 0}, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Road"))
		{
			const bool InEdit = (LTools && LTools->GetSubTarget() == ESceneRoadTool::estRoadEdit);

			bool EditState = InEdit;
			if (XRay::ImGui::ToggleButton(InEdit ? ICON_FA_BEZIER_CURVE " Edit Points: ON" : ICON_FA_BEZIER_CURVE " Edit Points: OFF", &EditState, {-0.01f, 0}))
			{
				if (EditState)
				{
					ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_ROAD, ESceneRoadTool::estRoadEdit);
					ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
				}
				else
				{
					ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_ROAD);
				}
			}
			Hint("Select: click / frame selects points, Delete removes them\nMove: gizmo moves the selected points\nAdd: LMB on ground appends a point, Ctrl+LMB inserts,\nAlt+LMB on a point deletes it");

			if (XRay::ImGui::Button(ICON_FA_ROAD " New Road", {-0.01f, 0}))
			{
				tool->CreateRoad("road");
				ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_ROAD, ESceneRoadTool::estRoadEdit);
				ExecCommand(COMMAND_CHANGE_ACTION, etaAdd);
			}

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderFloat("##DefaultWidth", &tool->m_DefaultWidth, 1.f, 40.f, "New Road Width: %.1f m");
			Hint("Width of the first point of a new road;\nnext points inherit the width of their neighbour");

			if (InEdit)
			{
				ImGui::TextDisabled("Select / Move: points, Add: new points");
				ImGui::TextDisabled("Point properties: Properties panel");
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Actions"))
		{
			if (!Road)
			{
				ImGui::TextDisabled("Select a road object");
			}
			else
			{
				ImGui::Text("%s: %u points, %u selected", Road->GetName(), u32(Road->Spline.Points.size()), u32(Road->SelectedPoints.size()));

				const float HalfW = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;

				if (XRay::ImGui::Button("Select All", {HalfW, 0}))
				{
					Road->ClearPointSelection();
					for (u32 i = 0; i < u32(Road->Spline.Points.size()); ++i)
					{
						Road->SelectPoint(i, true);
					}
					ExecCommand(COMMAND_UPDATE_PROPERTIES);
				}
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Select None", {-0.01f, 0}))
				{
					Road->ClearPointSelection();
					ExecCommand(COMMAND_UPDATE_PROPERTIES);
				}

				if (XRay::ImGui::Button(ICON_FA_TRASH_CAN " Delete Selected", {-0.01f, 0}))
				{
					Road->DeleteSelectedPoints();
					Road->RebuildMesh();
					Scene->UndoSave();
					ExecCommand(COMMAND_UPDATE_PROPERTIES);
				}

				XRay::ImGui::Separator();

				if (XRay::ImGui::Button("Snap to Terrain", {HalfW, 0}))
				{
					tool->SnapPointsToTerrain(Road);
				}
				Hint("Move every control point onto the terrain surface");
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Rebuild Mesh", {-0.01f, 0}))
				{
					Road->RebuildMesh();
				}

				if (XRay::ImGui::Button(ICON_FA_MOUNTAIN " Conform Terrain", {-0.01f, 0}))
				{
					tool->ConformTerrain(Road);
				}
				Hint("Flatten the terrain under the road and shoulders.\nBlend distance: Road\\Conform Falloff in Properties");
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar();
		XRay::ImGui::EndDarkChild();
	}
}
