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

ImColor GetNodeColor(const xr_string& type)
{
	if (type == "walker") return ImColor(60, 180, 75); 
	if (type == "combat") return ImColor(220, 50, 50); 
	if (type == "trader") return ImColor(70, 120, 220);
	if (type == "anim")   return ImColor(160, 90, 220);
	return ImColor(100, 100, 100);
}

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

	ImVec2 pos = ImGui::GetCursorScreenPos();

	// HEADER
	ImDrawList* dl = ImGui::GetWindowDrawList();

	ImVec2 headerSize(260, 28);

	dl->AddRectFilled
	(
		ImVec2(pos.x - 8, pos.y - 8),
		{ pos.x + headerSize.x + 8, pos.y + headerSize.y - 8},
		IM_COL32(Desc.Color.R, Desc.Color.G, Desc.Color.B, 255),
		6.0f, ImDrawFlags_RoundCornersTop
	);

	ImGui::Dummy(headerSize);
	ImGui::SetCursorScreenPos({ pos.x + 8, pos.y });
	ImGui::TextUnformatted(Desc.Title.c_str());
	ImGui::Spacing();

	// INPUT PINS (generic)
	int idx = 0;
	for (auto& in : Desc.Inputs)
	{
		auto pinId = MakePinId(State.StateName, in, idx++);

		ed::BeginPin(pinId, ed::PinKind::Input);
		ImGui::Text("● %s", in.c_str());
		ed::EndPin();
	}

	Desc.DrawBody(State);

	// OUTPUT PINS
	for (auto& tr : State.Transitions)
	{
		xr_string pinName = tr.DebugName;
		if (!tr.TargetState.empty())
		{
			pinName += " → " + tr.TargetState;
		}

		auto pinId = MakePinId(State.StateName, pinName, idx++);

		ed::BeginPin(pinId, ed::PinKind::Output);
		ImGui::Text("%s ●", tr.DebugName.c_str());
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Target: %s", tr.TargetState.c_str());
		}

		ed::EndPin();
	}

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
					std::cout << "Loaded " << count << " states from file\n";
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
	std::unordered_map<xr_string, FNodePinInfo> stateInfo;

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

			xr_string outputPinName = sourceInfo.OutputPinNames.empty() ? "Out" : sourceInfo.OutputPinNames[0];

			int outputPinIdx = 0;
			for (size_t i = 0; i < sourceInfo.OutputPinNames.size(); ++i)
			{
				if (sourceInfo.OutputPinNames[i] == outputPinName)
				{
					outputPinIdx = (int)sourceInfo.InputPinNames.size() + (int)i;
					break;
				}
			}

			xr_string inputPinName = targetIt->second.InputPinNames[0];
			int inputPinIdx = 0;

			xr_string pinNameForOutput = tr.DebugName;
			if (!tr.TargetState.empty())
				pinNameForOutput += " → " + tr.TargetState;

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