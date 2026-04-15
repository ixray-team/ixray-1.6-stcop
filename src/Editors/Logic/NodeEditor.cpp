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

	ImGui::GetIO().FontGlobalScale = 1.0f;

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

	for (auto& [nodeId, eventNode] : m_EventNodes)
	{
		RenderEventNode(eventNode, nodeId);
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
			ImGui::SetCursorScreenPos(rowStart);

			ed::BeginPin(State.InputPinId, ed::PinKind::Input);

			ImVec2 p = ImGui::GetCursorScreenPos();
			ImGui::Dummy(ImVec2(12, 12));

			ImVec2 center = { p.x + 6, p.y + 6 };
			DrawList->AddCircleFilled(center, 4.0f, IM_COL32(150, 150, 220, 255));

			ImGui::SameLine();
			ImGui::TextUnformatted(in.c_str());

			ed::EndPin();
		}

		// ---------- OUTPUT ----------
		//if (Row < State.Transitions.size())
		{
			xr_string pinName = "Out";
			ImVec2 textSize = ImGui::CalcTextSize(pinName.c_str());

			float x = rightX - textSize.x - 12.0f;

			ImGui::SetCursorScreenPos({ x, rowStart.y });

			ed::BeginPin(State.OutputPinId, ed::PinKind::Output);

				ImGui::TextUnformatted(pinName.c_str());
				ImGui::SameLine();

			ImVec2 p = ImGui::GetCursorScreenPos();
			ImGui::Dummy(ImVec2(12, 12));

			ImVec2 Center = { p.x + 6, p.y + 6 };
			DrawList->AddCircleFilled(Center, 4.0f, IM_COL32(220, 150, 150, 255));

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
		m_EventNodes.clear();
		m_EventToNodeMap.clear();

		// Сначала загружаем состояния
		for (auto& st : states)
		{
			size_t key = std::hash<xr_string>{}(st.StateName);
			m_Nodes[key] = st;

			for (auto& transition : st.Events)
			{
				FEventInfo eventInfo;
				eventInfo.EventKey = transition.EventKey;
				eventInfo.EventType = transition.EventType;
				eventInfo.Transition = transition.Transition;
				eventInfo.EventIndex = transition.EventIndex;
				CreateEventNode(eventInfo, st);
			}
		}

		// Удаляем переходы из состояний (теперь они в event-нодах)
		for (auto& [key, state] : m_Nodes)
		{
			state.Transitions.erase
			(
				std::remove_if(state.Transitions.begin(), state.Transitions.end(),
					[](const FTransition& t) {
						return t.DebugName.rfind("on_", 0) == 0 ||
							t.DebugName == "wounded" ||
							t.DebugName == "danger" ||
							t.DebugName == "meet";
					}),
				state.Transitions.end()
			);
		}

		AssignStatePins();

		BuildLinks();
		BuildNodesLayout();


		for (auto& [eventId, eventNode] : m_EventNodes)
		{
			ImVec2 parentPos = ed::GetNodePosition(eventNode.Owner);
			ed::SetNodePosition(eventId, ImVec2(parentPos.x - 350, parentPos.y));
		}
	}
}

void FNodeEditor::CreateEventNode(const FEventInfo& event, const FState& parentState)
{
	FEventNode eventNode;
	eventNode.Owner = MakeNodeId(parentState);

	eventNode.EventName = event.EventKey;
	eventNode.LinkedTransition = event.Transition; 
	eventNode.EventIndex = event.EventIndex;

	// Формируем отображаемое имя
	if (event.EventType == "timer")
	{
		eventNode.DisplayName = event.EventKey;
		if (event.Transition.Condition.Value > 0)
			eventNode.DisplayName += xr_string(" (") + xr_string::ToString(event.Transition.Condition.Value) + "s)";
		eventNode.TimerValue = event.Transition.Condition.Value;
	}
	else if (event.EventType == "info")
	{
		eventNode.DisplayName = event.EventKey;
		if (!event.Transition.Condition.InfoName.empty())
			eventNode.InfoName = event.Transition.Condition.InfoName;
	}
	else if (event.EventType == "wounded")
	{
		eventNode.DisplayName = "Wounded";
	}
	else if (event.EventType == "danger")
	{
		eventNode.DisplayName = "Danger";
	}
	else if (event.EventType == "meet")
	{
		eventNode.DisplayName = "Meet";
	}
	else
	{
		eventNode.DisplayName = event.EventKey;
	}

	eventNode.Conditions = event.Transition.ParsedConditions;
	eventNode.Effects = event.Transition.Effects;

	// Создаем ноду
	ed::NodeId nodeId = ed::NodeId(++m_NextNodeId);
	m_EventNodes[nodeId] = eventNode;
	m_EventToNodeMap[{parentState.StateName, event.EventIndex}] = nodeId;
}

void FNodeEditor::RenderEventNode(FEventNode& EventNode, ed::NodeId nodeId)
{
	ed::BeginNode(nodeId);
	ImGui::PushID(EventNode.EventName.c_str());

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	ImVec2 start = ImGui::GetCursorScreenPos();

	const float NODE_WIDTH = 200.0f;
	ImVec2 headerSize = ImVec2(NODE_WIDTH, 28);

	// Цвет
	ImU32 headerColor;
	if (EventNode.EventName.find("timer") != xr_string::npos)
		headerColor = IM_COL32(80, 120, 200, 255);
	else if (EventNode.EventName.find("info") != xr_string::npos)
		headerColor = IM_COL32(100, 180, 100, 255);
	else if (EventNode.EventName == "active")
		headerColor = IM_COL32(200, 180, 60, 255);
	else if (EventNode.EventName == "wounded")
		headerColor = IM_COL32(200, 80, 80, 255);
	else if (EventNode.EventName == "danger")
		headerColor = IM_COL32(220, 120, 40, 255);
	else
		headerColor = IM_COL32(140, 100, 180, 255);

	DrawList->AddRectFilled(
		ImVec2(start.x - 8, start.y - 8),
		ImVec2(start.x + headerSize.x + 8, start.y + headerSize.y - 8),
		headerColor, 6.0f, ImDrawFlags_RoundCornersTop
	);

	ImGui::Dummy(headerSize);
	ImGui::SetCursorScreenPos({ start.x + 8, start.y });
	ImGui::TextUnformatted(EventNode.DisplayName.c_str());

	ImGui::Spacing();
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4, 4));

	// ---------- INPUT PIN ----------
	{
		ed::PinId inPinId = ed::PinId(nodeId.Get() * 1000 + 0);

		ed::BeginPin(EventNode.InputPinId, ed::PinKind::Input);

		ImVec2 p = ImGui::GetCursorScreenPos();
		ImGui::Dummy(ImVec2(12, 12));

		ImVec2 center = { p.x + 6, p.y + 6 };
		DrawList->AddCircleFilled(center, 4.0f, IM_COL32(150, 150, 220, 255));

		ImGui::SameLine();
		ImGui::TextUnformatted("In");

		ed::EndPin();
	}

	// Условия
	if (!EventNode.Conditions.empty())
	{
		ImGui::TextColored(ImVec4(0.7f, 0.7f, 0.3f, 1.0f), "Conditions:");
		for (auto& cond : EventNode.Conditions)
		{
			ImGui::Bullet();
			switch (cond.Op)
			{
			case FParsedCondition::FuncTrue:
				ImGui::Text("%s(...)", cond.FuncName.c_str());
				break;
			case FParsedCondition::Probability:
				ImGui::Text("Probability: %d%%", cond.ProbabilityValue);
				break;
			default:
				ImGui::Text("...");
				break;
			}
		}
	}

	// Эффекты
	if (!EventNode.Effects.empty())
	{
		ImGui::TextColored(ImVec4(0.3f, 0.7f, 0.3f, 1.0f), "Effects:");
		for (auto& effect : EventNode.Effects)
		{
			ImGui::Bullet();
			switch (effect.Type)
			{
			case FParsedEffect::GiveInfo:
				ImGui::Text("+%s", effect.InfoName.c_str());
				break;
			case FParsedEffect::RemoveInfo:
				ImGui::Text("-%s", effect.InfoName.c_str());
				break;
			case FParsedEffect::CallFunction:
				ImGui::Text("= %s(...)", effect.FuncName.c_str());
				break;
			default:
				ImGui::Text("%s", effect.RawCommand.c_str());
				break;
			}
		}
	}

	ImGui::PopStyleVar();

	// ---------- OUTPUT PIN ----------
	{
		ed::BeginPin(EventNode.OutputPinId, ed::PinKind::Output);

		ImVec2 p = ImGui::GetCursorScreenPos();
		ImGui::Dummy(ImVec2(12, 12));

		ImVec2 center = { p.x + 6, p.y + 6 };
		DrawList->AddCircleFilled(center, 4.0f, IM_COL32(220, 150, 150, 255));

		ImGui::SameLine();
		ImGui::TextUnformatted("Fire");

		ed::EndPin();
	}

	ImGui::PopID();
	ed::EndNode();
}


void FNodeEditor::BuildLinks()
{
	m_Links.clear();
	m_NextLinkId = 1;

	for (auto& [id, state] : m_Nodes)
	{
		for (auto& tr : state.Transitions)
		{
			if (tr.TargetState.empty())
				continue;

			auto it = m_Nodes.find(std::hash<xr_string>{}(tr.TargetState));
			if (it == m_Nodes.end())
				continue;

			FLink link;
			link.Id = ed::LinkId(m_NextLinkId++);
			link.StartPinId = state.OutputPinId;
			link.EndPinId = it->second.InputPinId;

			m_Links.push_back(link);
		}
		for (size_t i = 0; i < state.Events.size(); ++i)
		{
			FEventTransition& Event = state.Events[i];

			// ----------------------------
			// 1. STATE → EVENT NODE
			// ----------------------------
			auto eventNodeIt = m_EventToNodeMap.find(
				{ state.StateName, Event.EventIndex }
			);
			if (eventNodeIt == m_EventToNodeMap.end())
				continue;

			ed::PinId endPinId = m_EventNodes[eventNodeIt->second].InputPinId;

			m_Links.push_back({
				ed::LinkId(m_NextLinkId++),
				state.OutputPinId,
				endPinId
				});

			// ----------------------------
			// 2. EVENT NODE → TARGET STATE
			// ----------------------------
			if (Event.Transition.TargetState.empty())
				continue;

			auto targetIt = m_Nodes.find(
				std::hash<xr_string>{}(Event.Transition.TargetState)
			);

			if (targetIt == m_Nodes.end())
				continue;

			ed::PinId eventOutPin = m_EventNodes[eventNodeIt->second].OutputPinId;
			m_Links.push_back({
				ed::LinkId(m_NextLinkId++),
				eventOutPin,
				targetIt->second.InputPinId
				});
		}
	}
}

void FNodeEditor::AssignStatePins()
{
	for (auto& [id, state] : m_Nodes)
	{
		state.InputPinId = ed::PinId(m_NextPinId++);
		state.OutputPinId = ed::PinId(m_NextPinId++);
	}

	for (auto& [id, eventNode] : m_EventNodes)
	{
		eventNode.InputPinId = ed::PinId(m_NextPinId++);
		eventNode.OutputPinId = ed::PinId(m_NextPinId++);
	}
}