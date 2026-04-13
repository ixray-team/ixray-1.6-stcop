#include "../../xrCore/xrCore.h"
#include "NodeEditor.h"
#include <iostream>
#include "LogicLoader.h"

FNodeEditor* GNodeEditor = nullptr;

struct FPinDesc
{
	xr_string Name;
	ed::PinId Id;
	FTransition* Transition;
};

FNodeEditor::FNodeEditor()
{
	Initialize();
}

FNodeEditor::~FNodeEditor()
{
	Shutdown();
}

void FNodeEditor::Initialize()
{
	ed::Config config;
	config.SettingsFile = "NodeEditorSettings.json";

	m_Context = ed::CreateEditor(&config);
}

void FNodeEditor::Shutdown()
{
	if (m_Context)
	{
		ed::DestroyEditor(m_Context);
		m_Context = nullptr;
	}
}

void FNodeEditor::CreateDemoNodes()
{
}

void FNodeEditor::Render()
{
	if (!m_Context)
	{
		return;
	}

	ed::SetCurrentEditor(m_Context);

	RenderMainMenu();

	auto& style = ed::GetStyle();
	style.NodeRounding = 6.0f;
	style.NodeBorderWidth = 1.5f;
	style.PinRounding = 4.0f;
	style.LinkStrength = 100.0f;

	ImVec2 editorSize = ImGui::GetContentRegionAvail();

	ed::Begin("Logic Editor", editorSize);

	for (auto& [id, node] : m_Nodes)
	{
		RenderNode(node);
	}

	for (const auto& link : m_Links)
	{
		ed::Link(link.Id, link.StartPinId, link.EndPinId);
	}

	HandleConnections();
	RenderContextMenu();

	ed::End();
	ed::SetCurrentEditor(nullptr);
}

inline ed::PinId MakePinId(const xr_string& StateName, const xr_string& PinName, int Index)
{
	size_t h1 = std::hash<xr_string>{}(StateName);
	size_t h2 = std::hash<xr_string>{}(PinName);

	return (ed::PinId)(h1 ^ (h2 << 1) ^ (Index << 16));
}

inline ed::NodeId MakeNodeId(const FState& State)
{
	return (ed::NodeId)std::hash<xr_string>{}(State.StateName);
}

void FNodeEditor::RenderNode(FState& State)
{
	auto Desc = GetStateRenderDesc(State);

	ed::BeginNode(MakeNodeId(State));
	ImGui::PushID(State.StateName.c_str());

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	ImVec2 start = ImGui::GetCursorScreenPos();

	const float NODE_WIDTH = 260.0f;

	ImVec2 headerSize = ImVec2(NODE_WIDTH, 28);

	DrawList->AddRectFilled
	(
		ImVec2(start.x - 8, start.y - 8),
		ImVec2(start.x + headerSize.x + 8, start.y + headerSize.y - 8),
		IM_COL32(Desc.Color.R, Desc.Color.G, Desc.Color.B, 255),
		6.0f,
		ImDrawFlags_RoundCornersTop
	);

	ImGui::Dummy(headerSize);
	ImGui::SetCursorScreenPos({ start.x + 8, start.y });
	ImGui::TextUnformatted(Desc.Title.c_str());

	ImGui::Spacing();

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4, 4));

	int MaxRows = std::max((int)Desc.Inputs.size(), (int)State.Transitions.size());
	int idx = 0;

	float leftX = start.x + 8;
	float rightX = start.x + NODE_WIDTH - 8;

	for (int Row = 0; Row < MaxRows; ++Row)
	{
		ImVec2 rowStart = ImGui::GetCursorScreenPos();

		// ---------- INPUT ----------
		if (Row < Desc.Inputs.size())
		{
			auto& in = Desc.Inputs[Row];
			auto PinId = MakePinId(State.StateName, in, idx++);

			ImGui::SetCursorScreenPos(rowStart);

			ed::BeginPin(PinId, ed::PinKind::Input);

			ImVec2 p = ImGui::GetCursorScreenPos();
			ImGui::Dummy(ImVec2(12, 12));

			ImVec2 center = { p.x + 6, p.y + 6 };
			DrawList->AddCircleFilled(center, 4.0f, IM_COL32(150, 150, 220, 255));

			ImGui::SameLine();
			ImGui::TextUnformatted(in.c_str());

			ed::EndPin();
		}

		// ---------- OUTPUT ----------
		if (Row < State.Transitions.size())
		{
			auto& tr = State.Transitions[Row];

			xr_string pinName = tr.DebugName;
			if (!tr.TargetState.empty())
			{
				pinName += " → " + tr.TargetState;
			}

			auto pinId = MakePinId(State.StateName, pinName, idx++);

			ImVec2 textSize = ImGui::CalcTextSize(tr.DebugName.c_str());

			float x = rightX - textSize.x - 12.0f;
			if (tr.DebugName.empty())
			{
				x = rightX - 12.0f;
			}

			ImGui::SetCursorScreenPos({ x, rowStart.y });

			ed::BeginPin(pinId, ed::PinKind::Output);

			if (!tr.DebugName.empty())
			{
				ImGui::TextUnformatted(tr.DebugName.c_str());
				ImGui::SameLine();
			}

			ImVec2 p = ImGui::GetCursorScreenPos();
			ImGui::Dummy(ImVec2(12, 12));

			ImVec2 Center = { p.x + 6, p.y + 6 };
			DrawList->AddCircleFilled(Center, 4.0f, IM_COL32(220, 150, 150, 255));

			if (ImGui::IsItemHovered() && !tr.TargetState.empty())
			{
				ImGui::SetTooltip("Target: %s", tr.TargetState.c_str());
			}

			ed::EndPin();
		}

		ImGui::Dummy(ImVec2(0, 2));
	}

	ImGui::PopStyleVar();

	if (Desc.DrawBody)
	{
		Desc.DrawBody(State);
	}

	ImGui::PopID();
	ed::EndNode();
}

void FNodeEditor::HandleConnections()
{
	if (ed::BeginCreate())
	{
		ed::PinId startPinId, endPinId;
		if (ed::QueryNewLink(&startPinId, &endPinId))
		{
			if (startPinId && endPinId)
			{
				if (ed::AcceptNewItem())
				{
					FLink NewLink;
					NewLink.Id = ed::LinkId(m_NextLinkId++);
					NewLink.StartPinId = startPinId;
					NewLink.EndPinId = endPinId;
					m_Links.push_back(NewLink);

					std::cout << "Link created: " << startPinId.Get() << " -> " << endPinId.Get() << std::endl;
				}
			}
		}
	}
	ed::EndCreate();

	if (ed::BeginDelete())
	{
		ed::LinkId deletedLinkId;
		while (ed::QueryDeletedLink(&deletedLinkId))
		{
			if (ed::AcceptDeletedItem())
			{
				auto it = std::find_if(m_Links.begin(), m_Links.end(),
					[deletedLinkId](const FLink& link) { return link.Id == deletedLinkId; });

				if (it != m_Links.end())
				{
					std::cout << "Link deleted: " << it->Id.Get() << std::endl;
					m_Links.erase(it);
				}
			}
		}
	}
	ed::EndDelete();
}

void FNodeEditor::RenderContextMenu()
{
#if 0
	if (ImGui::BeginPopupContextWindow("NodeEditorContext", ImGuiPopupFlags_MouseButtonRight | ImGuiPopupFlags_NoOpenOverItems))
	{
		ImGui::EndPopup();
	}

	if (ImGui::IsMouseClicked(ImGuiMouseButton_Right))
	{
		ImGui::OpenPopup("NodeEditorContext");
	}
#endif
}

void FNodeEditor::BuildNodesLayout()
{
	if (m_Nodes.empty()) return;

	xr_hash_map<xr_string, xr_vector<xr_string>> Parents;
	xr_hash_map<xr_string, xr_vector<xr_string>> Children;

	for (auto& [key, node] : m_Nodes)
	{
		for (auto& tr : node.Transitions)
		{
			if (!tr.TargetState.empty())
			{
				Children[node.StateName].push_back(tr.TargetState);
				Parents[tr.TargetState].push_back(node.StateName);
			}
		}
	}

	xr_string Root;
	for (auto& [key, node] : m_Nodes)
	{
		if (node.StateName.find("logic") == 0)
		{
			Root = node.StateName;
			break;
		}
	}

	if (Root.empty() && !m_Nodes.empty())
	{
		Root = m_Nodes.begin()->second.StateName;
	}

	const float SpacingX = 420.0f;
	const float SpacingY = 350.0f;

	xr_hash_map<xr_string, ImVec2> Positions;
	xr_hash_set<xr_string> Visited;

	xr_hash_map<int, int> MaxDepthForRow;
	int CurrentRow = 0;

	std::function<void(const xr_string&, int, int)> BuildTree = [&](const xr_string& name, int Depth, int Row)
	{
		if (Visited.contains(name))
		{
			return;
		}
		
		Visited.insert(name);

		auto it = MaxDepthForRow.find(Row);
		if (it != MaxDepthForRow.end() && Depth < it->second)
		{
			Depth = it->second;
		}

		Positions[name] = ImVec2(Depth * SpacingX, Row * SpacingY);
		MaxDepthForRow[Row] = Depth + 1;

		auto childIt = Children.find(name);
		if (childIt == Children.end() || childIt->second.empty())
		{
			return;
		}

		const auto& childs = childIt->second;

		if (childs.size() == 1)
		{
			BuildTree(childs[0], Depth + 1, Row);
		}
		else
		{
			for (size_t i = 0; i < childs.size(); ++i)
			{
				int newRow = (i == 0) ? Row : ++CurrentRow;
				BuildTree(childs[i], Depth + 1, newRow);
			}
		}
	};

	BuildTree(Root, 0, CurrentRow);

	for (auto& [key, node] : m_Nodes)
	{
		if (!Visited.contains(node.StateName))
		{
			auto parentIt = Parents.find(node.StateName);

			if (parentIt == Parents.end() || parentIt->second.empty())
			{
				CurrentRow++;
				BuildTree(node.StateName, 1, CurrentRow);
			}
		}
	}

	for (auto& [name, pos] : Positions)
	{
		auto it = m_Nodes.find(std::hash<xr_string>{}(name));
		if (it != m_Nodes.end())
		{
			ed::SetNodePosition(MakeNodeId(it->second), pos);
		}
	}

	ed::NavigateToContent();
}

void FNodeEditor::BuildLinksFromTransitions()
{
	m_Links.clear();
	m_NextLinkId = 1;

	struct FNodePinInfo 
	{
		ed::NodeId NodeId;
		xr_vector<xr_string> InputPinNames;
		xr_vector<xr_string> OutputPinNames;
	};
	xr_hash_map<xr_string, FNodePinInfo> stateInfo;

	for (auto& [key, state] : m_Nodes)
	{
		FNodePinInfo info;
		info.NodeId = MakeNodeId(state);

		auto Desc = GetStateRenderDesc(state);
		info.InputPinNames = Desc.Inputs;
		info.OutputPinNames = Desc.Outputs;

		stateInfo[state.StateName] = info;
	}

	for (auto& [key, sourceState] : m_Nodes)
	{
		auto sourceInfo = stateInfo[sourceState.StateName];

		for (auto& tr : sourceState.Transitions)
		{
			if (tr.TargetState.empty())
			{
				continue;
			}

			auto targetIt = stateInfo.find(tr.TargetState);
			if (targetIt == stateInfo.end())
			{
				continue;
			}

			if (targetIt->second.InputPinNames.empty())
			{
				continue;
			}

			int inputCount = (int)sourceInfo.InputPinNames.size();

			int transitionIndex = 0;
			for (size_t i = 0; i < sourceState.Transitions.size(); ++i)
			{
				if (&sourceState.Transitions[i] == &tr)
				{
					transitionIndex = (int)i;
					break;
				}
			}

			int outputPinIdx = inputCount + transitionIndex;

			xr_string inputPinName = targetIt->second.InputPinNames[0];
			int inputPinIdx = 0;

			xr_string pinNameForOutput = tr.DebugName;
			if (!tr.TargetState.empty())
			{
				pinNameForOutput += " → " + tr.TargetState;
			}

			ed::PinId startPinId = MakePinId(sourceState.StateName, pinNameForOutput, outputPinIdx);
			ed::PinId endPinId = MakePinId(tr.TargetState, inputPinName, inputPinIdx);

			FLink NewLink;
			NewLink.Id = ed::LinkId(m_NextLinkId++);
			NewLink.StartPinId = startPinId;
			NewLink.EndPinId = endPinId;
			m_Links.push_back(NewLink);
		}
	}
}

void FNodeEditor::RenderMainMenu()
{
	if (ImGui::BeginMainMenuBar())
	{
		if (ImGui::BeginMenu("File"))
		{
			if (ImGui::MenuItem("Open...", "Ctrl+O"))
			{
				m_ShowFileDialog = true;
				m_FilePath[0] = '\0';
			}

			ImGui::Separator();

			if (ImGui::MenuItem("Exit", "Alt+F4"))
			{
			}

			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu("View"))
		{
			if (ImGui::MenuItem("Reset Layout"))
			{
				BuildNodesLayout();
			}

			if (ImGui::MenuItem("Navigate to Content", "F"))
			{
				ed::NavigateToContent();
			}

			ImGui::EndMenu();
		}

		ImGui::EndMainMenuBar();
	}

	if (m_ShowFileDialog)
	{
		OPENFILENAMEA ofn = {};
		ofn.lStructSize = sizeof(ofn);
		ofn.hwndOwner = nullptr;
		ofn.lpstrFile = m_FilePath;
		ofn.nMaxFile = MAX_PATH;
		ofn.lpstrFilter = "Logic files\0*.ltx;*.ini\0All files\0*.*\0";
		ofn.Flags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST;

		if (GetOpenFileNameA(&ofn))
		{
			m_ShowFileDialog = false;
			LoadLogicFile(m_FilePath);
		}
		else
		{
			m_ShowFileDialog = false;
		}
	}
}

void FNodeEditor::LoadLogicFile(const char* path)
{
	xr_string pathStr = path;
	auto states = LogicLoader::LoadFromFile(pathStr);

	if (!states.empty())
	{
		m_Nodes.clear();
		m_Links.clear();

		int count = 0;
		for (auto& st : states)
		{
			size_t key = std::hash<xr_string>{}(st.StateName);
			m_Nodes[key] = st;
			++count;
			if (count > 200) break;
		}

		BuildLinksFromTransitions();
		BuildNodesLayout();
	}
}