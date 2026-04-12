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
	auto& style = ed::GetStyle();
	style.NodeRounding = 6.0f;
	style.NodeBorderWidth = 1.5f;
	style.PinRounding = 4.0f;
	style.LinkStrength = 100.0f;


	ed::Begin("Logic Editor", ImVec2(0.0f, 0.0f));

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
	if (ImGui::BeginPopupContextWindow("NodeEditorContext", ImGuiPopupFlags_MouseButtonRight | ImGuiPopupFlags_NoOpenOverItems))
	{
		if (ImGui::MenuItem("Load logic file..."))
		{
			char fname[MAX_PATH] = {0};
			OPENFILENAMEA ofn = {};
			ofn.lStructSize = sizeof(ofn);
			ofn.hwndOwner = nullptr;
			ofn.lpstrFile = fname;
			ofn.nMaxFile = MAX_PATH;
			ofn.lpstrFilter = "Logic files\0*.ltx;*.ini\0All files\0*.*\0";
			ofn.Flags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST;

			if (GetOpenFileNameA(&ofn))
			{
				xr_string path = fname;
				auto states = LogicLoader::LoadFromFile(path);
				if (!states.empty())
				{
					m_Nodes.clear();
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
		}

		ImGui::EndPopup();
	}

	if (ImGui::IsMouseClicked(ImGuiMouseButton_Right))
	{
		ImGui::OpenPopup("NodeEditorContext");
	}
}

void FNodeEditor::BuildNodesLayout()
{
	xr_hash_map<xr_string, xr_vector<xr_string>> Graph;

	for (auto& [key, node] : m_Nodes)
	{
		auto& children = Graph[node.StateName];

		for (auto& tr : node.Transitions)
		{
			if (!tr.TargetState.empty())
			{
				children.push_back(tr.TargetState);
			}
		}
	}

	if (m_Nodes.empty())
	{
		return;
	}

	xr_string Root;

	for (auto& [key, node] : m_Nodes)
	{
		if (node.StateName.StartWith("logic"))
		{
			Root = node.StateName;
			break;
		}
	}

	if (Root.empty() && !m_Nodes.empty())
	{
		Root = m_Nodes.begin()->second.StateName;
	}

	xr_hash_set<xr_string> Visited;

	const float SpacingX = 420.0f;
	const float SpacingY = 350.0f;

	int CurrentY = 0;

	std::function<void(const xr_string&, int, int)> DFSBuilder = [&](const xr_string& name, int Depth, int y)
	{
		if (Visited.contains(name))
		{
			return;
		}

		Visited.insert(name);

		auto it = m_Nodes.find(std::hash<xr_string>{}(name));
		if (it != m_Nodes.end())
		{
			ed::SetNodePosition
			(
				MakeNodeId(it->second),
				ImVec2(Depth * SpacingX, y * SpacingY)
			);
		}

		auto& Children = Graph[name];

		if (Children.empty())
		{
			return;
		}

		if (Children.size() == 1)
		{
			DFSBuilder(Children[0], Depth + 1, y);
			return;
		}

		int BranchY = y;

		for (size_t i = 0; i < Children.size(); ++i)
		{
			if (i == 0)
			{
				DFSBuilder(Children[i], Depth + 1, BranchY);
			}
			else
			{
				CurrentY++;
				BranchY = CurrentY;
				DFSBuilder(Children[i], Depth + 1, BranchY);
			}
		}
	};

	// старт
	DFSBuilder(Root, 0, CurrentY);
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