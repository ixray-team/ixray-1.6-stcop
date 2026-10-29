#include "stdafx.h"
#include "NodesCommon.h"

namespace ed = ax::NodeEditor;

namespace NodesUI
{
	void ApplyGraphStyle(bool Alternative)
	{
		ed::Style& Style = ed::GetStyle();
		Style.NodeRounding = 7.0f;
		Style.NodePadding = ImVec4(10, 6, 10, 8);
		Style.NodeBorderWidth = 1.0f;
		Style.HoveredNodeBorderWidth = 2.0f;
		Style.SelectedNodeBorderWidth = 2.5f;
		Style.LinkStrength = 120.0f;
		Style.PinRounding = 0.0f;
		Style.FlowDuration = 1.2f;
		Style.Colors[ed::StyleColor_Bg] = Alternative ? ImColor(30, 28, 36) : ImColor(32, 32, 32);
		Style.Colors[ed::StyleColor_Grid] = ImColor(70, 70, 70, 90);
		Style.Colors[ed::StyleColor_NodeBg] = ImColor(18, 18, 18, 240);
		Style.Colors[ed::StyleColor_NodeBorder] = ImColor(0, 0, 0, 220);
		Style.Colors[ed::StyleColor_HovNodeBorder] = ImColor(200, 200, 200, 160);
		Style.Colors[ed::StyleColor_SelNodeBorder] = ImColor(255, 170, 40);
		Style.Colors[ed::StyleColor_SelLinkBorder] = ImColor(255, 170, 40);
		Style.Colors[ed::StyleColor_HovLinkBorder] = ImColor(200, 200, 200, 160);
		Style.Colors[ed::StyleColor_Flow] = ImColor(255, 170, 40);
		Style.Colors[ed::StyleColor_FlowMarker] = ImColor(255, 210, 120);
		Style.Colors[ed::StyleColor_PinRect] = ImColor(0, 0, 0, 0);
		Style.Colors[ed::StyleColor_PinRectBorder] = ImColor(0, 0, 0, 0);
	}

	void DrawExecPin(ImDrawList* DrawList, ImVec2 Min, float Size, ImColor Color, bool Filled)
	{
		const float Inset = Size * 0.15f;
		const ImVec2 A = { Min.x + Inset, Min.y + Inset };
		const ImVec2 B = { Min.x + Size * 0.55f, Min.y + Inset };
		const ImVec2 C = { Min.x + Size - Inset, Min.y + Size * 0.5f };
		const ImVec2 D = { Min.x + Size * 0.55f, Min.y + Size - Inset };
		const ImVec2 E = { Min.x + Inset, Min.y + Size - Inset };
		const ImVec2 Points[] = { A, B, C, D, E };
		if (Filled)
		{
			DrawList->AddConvexPolyFilled(Points, 5, Color);
		}
		else
		{
			DrawList->AddPolyline(Points, 5, Color, ImDrawFlags_Closed, 1.5f);
		}
	}

	void DrawNodeHeader(ImDrawList* DrawList, ImVec2 Position, float Width, float HeaderHeight, ImColor Color)
	{
		const float Rounding = ed::GetStyle().NodeRounding;
		const ImVec2 Min = Position;
		const ImVec2 Max = { Position.x + Width, Position.y + HeaderHeight };
		ImColor Dark = Color;
		Dark.Value.x *= 0.25f;
		Dark.Value.y *= 0.25f;
		Dark.Value.z *= 0.25f;
		DrawList->AddRectFilled(Min, Max, Dark, Rounding, ImDrawFlags_RoundCornersTop);
		DrawList->AddRectFilled(Min, { Min.x + Rounding * 2.0f, Max.y }, Color, Rounding, ImDrawFlags_RoundCornersTopLeft);
		DrawList->AddRectFilledMultiColor({ Min.x + Rounding, Min.y }, { Max.x - Rounding, Max.y }, Color, Dark, Dark, Color);
		DrawList->AddLine({ Min.x, Max.y }, { Max.x, Max.y }, ImColor(0, 0, 0, 200), 1.0f);
	}

	void DrawCenteredHint(const char* Text)
	{
		const ImVec2 Avail = ImGui::GetContentRegionAvail();
		const ImVec2 TextSize = ImGui::CalcTextSize(Text);
		ImGui::SetCursorPos({ ImGui::GetCursorPosX() + std::max(0.0f, (Avail.x - TextSize.x) * 0.5f), ImGui::GetCursorPosY() + Avail.y * 0.45f });
		ImGui::TextDisabled("%s", Text);
	}

	void DrawChips(const xr_vector<SChip>& Chips, float Width)
	{
		ImDrawList* DrawList = ImGui::GetWindowDrawList();
		const float StartX = ImGui::GetCursorScreenPos().x;
		float LineX = 0.0f;
		bool First = true;
		for (const SChip& Chip : Chips)
		{
			const ImVec2 TextSize = ImGui::CalcTextSize(Chip.Text.c_str());
			const ImVec2 ChipSize = { TextSize.x + 10.0f, TextSize.y + 2.0f };
			if (!First && LineX + ChipSize.x <= Width)
			{
				ImGui::SameLine(0.0f, 4.0f);
			}
			else
			{
				LineX = 0.0f;
				if (!First)
				{
					ImGui::SetCursorScreenPos({ StartX, ImGui::GetCursorScreenPos().y });
				}
			}
			First = false;
			LineX += ChipSize.x + 4.0f;

			ImGui::Dummy(ChipSize);
			const ImVec2 Min = ImGui::GetItemRectMin();
			const ImVec2 Max = ImGui::GetItemRectMax();
			ImColor Fill = Chip.Color;
			Fill.Value.w = 0.18f;
			DrawList->AddRectFilled(Min, Max, Fill, 4.0f);
			DrawList->AddRect(Min, Max, Chip.Color, 4.0f);
			DrawList->AddText({ Min.x + 5.0f, Min.y + 1.0f }, Chip.Color, Chip.Text.c_str());
			if (!Chip.Tooltip.empty())
			{
				ImGui::SetItemTooltip("%s", Chip.Tooltip.c_str());
			}
		}
	}

	bool PassFilter(const char* Filter, const xr_string& Name)
	{
		if (Filter[0] == 0)
		{
			return true;
		}
		xr_string Lower = Name;
		xr_string Key = Filter;
		xr_strlwr(Lower);
		xr_strlwr(Key);
		return Lower.find(Key) != xr_string::npos;
	}

	bool InputString(const char* Label, xr_string& Value, ImGuiInputTextFlags Flags)
	{
		char Buffer[2048] = {};
		const size_t Size = std::min(Value.size(), sizeof(Buffer) - 1);
		memcpy(Buffer, Value.data(), Size);
		if (ImGui::InputText(Label, Buffer, sizeof(Buffer), Flags))
		{
			Value = Buffer;
			return true;
		}
		return false;
	}

	void PropRow(const char* Name)
	{
		const float RowHeight = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);
		ImGui::TableNextRow(ImGuiTableRowFlags_None, RowHeight + 1.0f);
		ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, ImGui::GetColorU32(ImGuiCol_ChildBg));
		ImGui::TableNextColumn();
		ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 1.0f);
		ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImGui::GetColorU32(XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelTint).Value));
		XRay::ImGui::TextFramed(Name, { -1, -1 }, { 0.0f, 0.5f }, false);

		ImGui::TableNextColumn();
		ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImGui::GetColorU32(XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::TableTint).Value));
		ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 1.0f);
		ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
	}

	bool PropFolder(const char* Label, bool DefaultOpen, const char* RemoveIcon, const char* RemoveTooltip, bool& Remove)
	{
		const float RowHeight = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);
		const ImColor Color = XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint);
		ImGui::TableNextRow(ImGuiTableRowFlags_None, RowHeight + 1.0f);
		ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, Color);
		ImGui::TableNextColumn();
		ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 1.0f);

		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, { XRay::ImGui::GetEditorSize(XRay::ImGui::ButtonTextPaddingY), XRay::ImGui::GetEditorSize(XRay::ImGui::TableTextPaddingY) });
		ImGui::PushStyleColor(ImGuiCol_Header, Color.Value);
		ImGui::SetNextItemAllowOverlap();
		const bool Open = XRay::ImGui::CollapsingHeader(Label, DefaultOpen ? ImGuiTreeNodeFlags_DefaultOpen : ImGuiTreeNodeFlags_None);
		ImGui::PopStyleColor();
		ImGui::PopStyleVar();

		if (RemoveIcon != nullptr)
		{
			ImGui::TableSetColumnIndex(1);
			ImGui::SetCursorPosX(ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x - RowHeight);
			ImGui::PushStyleColor(ImGuiCol_Button, 0);
			Remove = ImGui::Button(RemoveIcon, { RowHeight, RowHeight });
			ImGui::PopStyleColor();
			ImGui::SetItemTooltip("%s", RemoveTooltip);
		}
		return Open;
	}

	void CMinimap::TakeClick()
	{
		Nodes.clear();
		Links.clear();
		Clicked = false;

		const ImVec2 Mouse = ImGui::GetMousePos();
		if (Scale > 0.0f && ImGui::IsMouseClicked(ImGuiMouseButton_Left) && ImGui::IsWindowHovered(ImGuiHoveredFlags_ChildWindows)
			&& Mouse.x >= Min.x && Mouse.x <= Max.x && Mouse.y >= Min.y && Mouse.y <= Max.y)
		{
			ClickPoint = { (Mouse.x - Origin.x) / Scale + Bounds.x, (Mouse.y - Origin.y) / Scale + Bounds.y };
			Clicked = true;
			ImGui::GetIO().MouseClicked[ImGuiMouseButton_Left] = false;
			ImGui::GetIO().MouseDown[ImGuiMouseButton_Left] = false;
		}
	}

	void CMinimap::AddNode(ed::NodeId Id, ImColor Color)
	{
		Nodes.push_back({ Id, Color });
	}

	void CMinimap::AddLink(ed::NodeId Source, ed::NodeId Target)
	{
		Links.emplace_back(Source, Target);
	}

	bool CMinimap::Draw(ImVec2 CanvasMin, ImVec2 CanvasMax)
	{
		Scale = 0.0f;

		ImVec2 BoundsMin = { FLT_MAX, FLT_MAX };
		ImVec2 BoundsMax = { -FLT_MAX, -FLT_MAX };
		for (const SNode& Node : Nodes)
		{
			const ImVec2 Size = ed::GetNodeSize(Node.Id);
			if (Size.x < 1.0f)
			{
				continue;
			}
			const ImVec2 Pos = ed::GetNodePosition(Node.Id);
			BoundsMin = { std::min(BoundsMin.x, Pos.x), std::min(BoundsMin.y, Pos.y) };
			BoundsMax = { std::max(BoundsMax.x, Pos.x + Size.x), std::max(BoundsMax.y, Pos.y + Size.y) };
		}
		if (BoundsMin.x > BoundsMax.x)
		{
			return false;
		}

		// The visible part of the graph is in the map too
		const ImVec2 ViewMin = ed::ScreenToCanvas(CanvasMin);
		const ImVec2 ViewMax = ed::ScreenToCanvas(CanvasMax);
		BoundsMin = { std::min(BoundsMin.x, ViewMin.x), std::min(BoundsMin.y, ViewMin.y) };
		BoundsMax = { std::max(BoundsMax.x, ViewMax.x), std::max(BoundsMax.y, ViewMax.y) };

		const float MapW = 200.0f;
		const float MapH = 140.0f;
		const float Padding = 10.0f;
		const float Margin = 4.0f;
		if (CanvasMax.x - CanvasMin.x < MapW + Padding * 2.0f || CanvasMax.y - CanvasMin.y < MapH + Padding * 2.0f)
		{
			return false;
		}
		Min = { CanvasMax.x - MapW - Padding, CanvasMax.y - MapH - Padding };
		Max = { Min.x + MapW, Min.y + MapH };

		const ImVec2 Content = { std::max(BoundsMax.x - BoundsMin.x, 1.0f), std::max(BoundsMax.y - BoundsMin.y, 1.0f) };
		Scale = std::min((MapW - Margin * 2.0f) / Content.x, (MapH - Margin * 2.0f) / Content.y);
		Bounds = BoundsMin;
		Origin = { Min.x + (MapW - Content.x * Scale) * 0.5f, Min.y + (MapH - Content.y * Scale) * 0.5f };

		auto ToMap = [this](ImVec2 Point) -> ImVec2
		{
			return { Origin.x + (Point.x - Bounds.x) * Scale, Origin.y + (Point.y - Bounds.y) * Scale };
		};

		// A child window is drawn over the canvas child, the canvas draw list is transformed by the view
		const ImVec2 Cursor = ImGui::GetCursorScreenPos();
		ImGui::SetCursorScreenPos(Min);
		ImGui::BeginChild("##NodesMinimap", { MapW, MapH }, ImGuiChildFlags_None, ImGuiWindowFlags_NoBackground | ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse | ImGuiWindowFlags_NoInputs);
		ImDrawList* DrawList = ImGui::GetWindowDrawList();
		DrawList->PushClipRect(Min, Max, false);
		DrawList->AddRectFilled(Min, Max, IM_COL32(20, 20, 20, 210), 4.0f);

		for (const auto& [Source, Target] : Links)
		{
			// From the right side of the source to the left side of the target, like the links
			const ImVec2 SourcePos = ed::GetNodePosition(Source);
			const ImVec2 SourceSize = ed::GetNodeSize(Source);
			const ImVec2 TargetPos = ed::GetNodePosition(Target);
			const ImVec2 TargetSize = ed::GetNodeSize(Target);
			if (SourceSize.x < 1.0f || TargetSize.x < 1.0f)
			{
				continue;
			}
			DrawList->AddLine(ToMap({ SourcePos.x + SourceSize.x, SourcePos.y + SourceSize.y * 0.5f }), ToMap({ TargetPos.x, TargetPos.y + TargetSize.y * 0.5f }), IM_COL32(150, 230, 120, 110), 1.0f);
		}

		ed::NodeId Nearest = 0;
		float NearestDistance = FLT_MAX;
		for (const SNode& Node : Nodes)
		{
			const ImVec2 Pos = ed::GetNodePosition(Node.Id);
			const ImVec2 Size = ed::GetNodeSize(Node.Id);
			if (Size.x < 1.0f)
			{
				continue;
			}
			const ImVec2 NodeMin = ToMap(Pos);
			// A gap keeps the neighbours apart at a small scale
			const ImVec2 NodeMax = { std::max(NodeMin.x + 2.0f, ToMap({ Pos.x + Size.x, 0 }).x - 1.0f), std::max(NodeMin.y + 2.0f, ToMap({ 0, Pos.y + Size.y }).y - 1.0f) };
			DrawList->AddRectFilled(NodeMin, NodeMax, ed::IsNodeSelected(Node.Id) ? (ImU32)ImColor(255, 170, 40) : (ImU32)Node.Color, 1.0f);

			const float DX = ClickPoint.x - (Pos.x + Size.x * 0.5f);
			const float DY = ClickPoint.y - (Pos.y + Size.y * 0.5f);
			if (DX * DX + DY * DY < NearestDistance)
			{
				NearestDistance = DX * DX + DY * DY;
				Nearest = Node.Id;
			}
		}

		DrawList->AddRect(ToMap(ViewMin), ToMap(ViewMax), IM_COL32(255, 255, 255, 190), 0.0f, 0, 1.0f);
		DrawList->PopClipRect();
		DrawList->AddRect(Min, Max, IM_COL32(100, 100, 100, 200), 4.0f);
		ImGui::EndChild();
		ImGui::SetCursorScreenPos(Cursor);

		if (!Clicked || !Nearest)
		{
			return false;
		}
		ed::ClearSelection();
		ed::SelectNode(Nearest);
		ed::NavigateToSelection(false, 0.15f);
		return true;
	}
}
