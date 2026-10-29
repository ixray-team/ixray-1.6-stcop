#pragma once
#include <imgui_node_editor.h>

namespace NodesUI
{
	constexpr float PinSize = 14.0f;

	const ImColor ColorMissing = ImColor(240, 70, 70);
	const ImColor ColorWarning = ImColor(240, 185, 75);
	const ImColor ColorDim = ImColor(150, 150, 150);
	const ImColor ColorPin = ImColor(235, 235, 235);

	void ApplyGraphStyle(bool Alternative = false);

	void DrawExecPin(ImDrawList* DrawList, ImVec2 Min, float Size, ImColor Color, bool Filled);
	void DrawNodeHeader(ImDrawList* DrawList, ImVec2 Position, float Width, float HeaderHeight, ImColor Color);
	void DrawCenteredHint(const char* Text);

	struct SChip
	{
		xr_string Text;
		ImColor Color;
		xr_string Tooltip;
	};

	void DrawChips(const xr_vector<SChip>& Chips, float Width);

	bool PassFilter(const char* Filter, const xr_string& Name);
	bool InputString(const char* Label, xr_string& Value, ImGuiInputTextFlags Flags = 0);

	class CMinimap
	{
	public:
		void TakeClick();
		void AddNode(ax::NodeEditor::NodeId Id, ImColor Color);
		void AddLink(ax::NodeEditor::NodeId Source, ax::NodeEditor::NodeId Target);
		bool Draw(ImVec2 CanvasMin, ImVec2 CanvasMax);

	private:
		struct SNode
		{
			ax::NodeEditor::NodeId Id;
			ImColor Color;
		};

		xr_vector<SNode> Nodes;
		xr_vector<std::pair<ax::NodeEditor::NodeId, ax::NodeEditor::NodeId>> Links;

		bool Clicked = false;
		ImVec2 ClickPoint = { 0, 0 };

		float Scale = 0.0f;
		ImVec2 Min = { 0, 0 };
		ImVec2 Max = { 0, 0 };
		ImVec2 Origin = { 0, 0 };
		ImVec2 Bounds = { 0, 0 };
	};

	void PropRow(const char* Name);
	bool PropFolder(const char* Label, bool DefaultOpen, const char* RemoveIcon, const char* RemoveTooltip, bool& Remove);
}
