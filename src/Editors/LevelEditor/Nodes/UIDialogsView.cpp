#include "stdafx.h"
#include "UIDialogsView.h"
#include "../Editor/UI_LevelTools.h"
#include "../../../xrEngine/string_table.h"

DialogEditor* DialogEditor::Instance = nullptr;

namespace
{
ImColor NodeHeaderColor(CDialogNode* Node)
{
	if (Node->IsFinal)
	{
		return ImColor(110, 200, 120);
	}
	if (Node->HasInfo.size() || Node->DontHasInfo.size() || Node->Precondition.size())
	{
		return ImColor(200, 160, 90);
	}
	return ImColor(90, 130, 210);
}

ImColor PinColorOut = ImColor(150, 230, 120);
ImColor LinkColor = PinColorOut;

void DrawPinArrow(bool Filled)
{
	const float Size = 12.0f;
	ImVec2 Pos = ImGui::GetCursorScreenPos();
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	ImColor White(255, 255, 255, 255);
	if (Filled)
	{
		DrawList->AddTriangleFilled(
			ImVec2(Pos.x, Pos.y + 8),
			ImVec2(Pos.x, Pos.y + 8 + Size),
			ImVec2(Pos.x + Size, Pos.y + 8 + Size * 0.5f),
			White
		);
	}
	else
	{
		DrawList->AddTriangle(
			ImVec2(Pos.x, Pos.y + 8),
			ImVec2(Pos.x, Pos.y + 8 + Size),
			ImVec2(Pos.x + Size, Pos.y + 8 + Size * 0.5f),
			ImColor(255, 255, 255, 200),
			1.5f
		);
	}
	ImGui::Dummy(ImVec2(Size, Size));
}
} // namespace

DialogEditor::DialogEditor()
{
	Instance = this;
	Editor = ed::CreateEditor();
	SelectedNode = nullptr;
	IsOpen = false;

	Properties = new UIPropertiesForm();
	Properties->TabIndex = 1;

	ContentBrowser = new CContentView("Content Browser##DE");
	ContentBrowser->Init();
	ContentBrowser->TabIndex = 1;
	UI->Push(ContentBrowser, false);
}

DialogEditor::~DialogEditor()
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	Nodes.clear();

	if (Editor != nullptr)
	{
		ed::DestroyEditor(Editor);
		Editor = nullptr;
	}
}

void DialogEditor_DrawInputBox(DialogEditor::InputBoxState& Box)
{
	if (!Box.IsOpen)
	{
		return;
	}

	ImVec2 Center = ImGui::GetMainViewport()->GetCenter();
	ImGui::SetNextWindowPos(Center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

	if (ImGui::BeginPopupModal("DialogInputBox", NULL, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Enter name:");
		ImGui::InputText("##input", Box.Buffer, IM_ARRAYSIZE(Box.Buffer));

		if (ImGui::Button("OK", ImVec2(120, 0)))
		{
			Box.IsOpen = false;
			Box.HasResult = true;
			ImGui::CloseCurrentPopup();
		}

		ImGui::SetItemDefaultFocus();
		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120, 0)))
		{
			Box.Buffer[0] = '\0';
			Box.IsOpen = false;
			ImGui::CloseCurrentPopup();
		}

		ImGui::EndPopup();
	}
}

void DialogEditor::Draw()
{
	if (!IsOpen)
	{
		return;
	}

	if (ImGui::Begin("Dialogs Editor", &IsOpen))
	{
		if (ImGui::BeginChild("Dialogs in file", {IsOpenList ? 300.f : 20.f, 0}))
		{
			if (IsOpenList)
			{
				ImGui::SetNextItemWidth(300);
				if (ImGui::BeginListBox("##ItemsInFile", {0, ImGui::GetWindowSize().y - 30}))
				{
					for (const auto& [ID, Node] : Dialogs)
					{
						bool bSelect = false;
						if (ImGui::Selectable(ID.c_str(), &bSelect))
						{
							OpenDialog(ID, Node);
							IsOpenList = !IsAutoHide;
							LastOpenDialog = ID;
						}
					}
					ImGui::Separator();
					ImGui::EndListBox();
				}

				if (ImGui::Button("Save"))
				{
					SaveDialog();
				}

				ImGui::SameLine();
				if (ImGui::Button("+"))
				{
					InputBox.Mode = InputBoxMode::DialogName;
					InputBox.Buffer[0] = '\0';
					InputBox.IsOpen = true;
				}

				ImGui::SameLine();
				ImGui::Checkbox("Auto Hide", &IsAutoHide);

				ImGui::SameLine();
				ImGui::SetCursorPosX(282);

				if (ImGui::Button("<"))
				{
					IsOpenList = false;
				}
			}
			else if (ImGui::Button(">"))
			{
				IsOpenList = true;
			}
		}
		ImGui::EndChild();
		ImGui::SameLine();

		ed::SetCurrentEditor(Editor);

		if (NeedLayout)
		{
			LayoutNodes();
			NeedLayout = false;
		}

		bool minimapClicked = false;
		ImVec2 minimapClickCanvas;
		{
			float inf = std::numeric_limits<float>::infinity();
			ImVec2 bMin(inf, inf), bMax(-inf, -inf);
			for (auto* Node : Nodes)
			{
				auto it = NodeIds.find(Node);
				if (it == NodeIds.end())
				{
					continue;
				}
				ImVec2 pos = ed::GetNodePosition(it->second);
				ImVec2 size = ed::GetNodeSize(it->second);
				if (pos.x < bMin.x)
				{
					bMin.x = pos.x;
				}
				if (pos.y < bMin.y)
				{
					bMin.y = pos.y;
				}
				if (pos.x + size.x > bMax.x)
				{
					bMax.x = pos.x + size.x;
				}
				if (pos.y + size.y > bMax.y)
				{
					bMax.y = pos.y + size.y;
				}
			}
			if (bMin.x < inf)
			{
				float mapW = 180.0f, mapH = 130.0f, padding = 8.0f;
				ImVec2 avail = ImGui::GetContentRegionAvail();
				ImVec2 cMin = ImGui::GetCursorScreenPos();
				ImVec2 mPos(cMin.x + avail.x - mapW - padding, cMin.y + avail.y - mapH - padding);

				ImVec2 mp = ImGui::GetMousePos();
				if (ImGui::IsMouseClicked(0) &&
					mp.x >= mPos.x && mp.x <= mPos.x + mapW &&
					mp.y >= mPos.y && mp.y <= mPos.y + mapH)
				{
					float cW = bMax.x - bMin.x;
					float cH = bMax.y - bMin.y;
					if (cW < 1.0f)
					{
						cW = 1.0f;
					}
					if (cH < 1.0f)
					{
						cH = 1.0f;
					}
					float margin = 20.0f;
					float sx = mapW / (cW + margin * 2);
					float sy = mapH / (cH + margin * 2);
					float s = (sx < sy) ? sx : sy;
					float dW = cW * s;
					float dH = cH * s;
					ImVec2 dO(mPos.x + (mapW - dW) * 0.5f, mPos.y + (mapH - dH) * 0.5f);

					float cx = (mp.x - dO.x) / s + bMin.x;
					float cy = (mp.y - dO.y) / s + bMin.y;
					minimapClickCanvas = ImVec2(cx, cy);
					minimapClicked = true;

					ImGui::GetIO().MouseDown[0] = false;
				}
			}
		}

		ed::Begin("Canvas");
		{
			if (PendingSelect)
			{
				ed::SelectNode(PendingSelect);
				PendingSelect = ed::NodeId(0);
			}

			if (FocusFirstNode && !Nodes.empty())
			{
				ed::NodeId FirstNode = NodeIds[Nodes[0]];
				ed::SelectNode(FirstNode);

				if (ed::GetNodeSize(FirstNode).x > 1.0f)
				{
					ed::NavigateToSelection();
					FocusFirstNode = false;
				}
			}

			if (minimapClicked)
			{
				ed::NodeId bestId = 0;
				float bestDist = std::numeric_limits<float>::infinity();
				for (auto* Node : Nodes)
				{
					auto it = NodeIds.find(Node);
					if (it == NodeIds.end())
					{
						continue;
					}

					ImVec2 pos = ed::GetNodePosition(it->second);
					ImVec2 size = ed::GetNodeSize(it->second);
					float dx = minimapClickCanvas.x - (pos.x + size.x * 0.5f);
					float dy = minimapClickCanvas.y - (pos.y + size.y * 0.5f);
					float d = dx * dx + dy * dy;
					if (d < bestDist)
					{
						bestDist = d;
						bestId = it->second;
					}
				}
				if (bestId)
				{
					ed::ClearSelection();
					ed::SelectNode(bestId);
				}
			}

			UpdateSelection();

			ConnectedPins.clear();
			for (const auto& Link : Links)
			{
				ConnectedPins.insert(Link.Start);
				ConnectedPins.insert(Link.End);
			}

			for (auto* Node : Nodes)
			{
				DrawNode(Node);
			}

			for (const auto& Link : Links)
			{
				ed::Link(Link.Id, Link.Start, Link.End, LinkColor, 2.0f);
			}

			HandleLinkCreation();
			HandleDeletion();
		}
		ed::End();

		if (minimapClicked && ed::GetSelectedObjectCount() > 0)
		{
			ed::NavigateToSelection(true, 0.0f);
		}

		DrawMinimap();

		if (ImGui::IsMouseReleased(1))
		{
			ContextMouseCanvas = ed::ScreenToCanvas(ImGui::GetMousePos());
			ImGui::OpenPopup("##dialogscontextmenu");
		}

		if (ImGui::BeginPopup("##dialogscontextmenu"))
		{
			if (ImGui::MenuItem("Create Node"))
			{
				InputBox.Mode = InputBoxMode::NodeName;
				InputBox.Buffer[0] = '\0';

				if (!Phrases.empty())
				{
					int NodeID = atoi(*Phrases.back().first) + 1;
					xr_string Suggestion = xr_string::ToString(NodeID);
					strcpy_s(InputBox.Buffer, Suggestion.c_str());
				}
				InputBox.IsOpen = true;
			}

			if (ImGui::MenuItem("Create Branch Node"))
			{
				InputBox.Mode = InputBoxMode::BranchName;
				InputBox.Buffer[0] = '\0';
				InputBox.IsOpen = true;
			}

			if (SelectedNode != nullptr && ImGui::MenuItem("Delete Node"))
			{
				DeleteNode(SelectedNode);
			}

			ImGui::EndPopup();
		}

		if (InputBox.IsOpen)
		{
			ImGui::OpenPopup("DialogInputBox");
			DialogEditor_DrawInputBox(InputBox);
		}
		else if (InputBox.HasResult)
		{
			InputBox.HasResult = false;

			auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
									 { return LastOpenDialog == Pair.first; });

			if (InputBox.Mode == InputBoxMode::NodeName || InputBox.Mode == InputBoxMode::BranchName)
			{
				if (Iter != Dialogs.end())
				{
					if (XML_NODE* RootNode = File.NavigateToNode(Iter->second, "phrase_list"))
					{
						XML_NODE* NewNode = RootNode->ToElement()->InsertNewChildElement("phrase");
						NewNode->ToElement()->SetAttribute("id", InputBox.Buffer);

						CDialogNode* MacroNode = Nodes.emplace_back(new CDialogNode(InputBox.Buffer));
						MacroNode->ParentNode = NewNode;

						if (InputBox.Mode == InputBoxMode::BranchName)
						{
							MacroNode->IsBranch = true;
							NewNode->ToElement()->SetAttribute("branch", "1");
						}

						BuildIdMaps();
						ed::SetNodePosition(NodeIds[MacroNode], ContextMouseCanvas);
						PendingSelect = NodeIds[MacroNode];

						SelectNodeEvent(MacroNode);
					}
				}
			}
			else
			{
				XML_NODE* RootNode = nullptr;

				if (Iter != Dialogs.end())
				{
					RootNode = Iter->second->Parent();
				}
				else if (!Dialogs.empty())
				{
					RootNode = Dialogs.front().second->Parent();
				}

				if (RootNode != nullptr)
				{
					NewDialog(RootNode);
				}
			}
		}

		ImGui::End();
	}


	if (ImGui::Begin("Properties"))
	{
		Properties->Draw();
	}
	ImGui::End();
}

void DialogEditor::Show(bool State)
{
	IsOpen = State;
}

void DialogEditor::NewDialog(XML_NODE* RootDialogNode)
{
	XML_NODE* NewDialog = RootDialogNode->ToElement()->InsertNewChildElement("dialog");
	NewDialog->ToElement()->SetAttribute("id", InputBox.Buffer);
	NewDialog->ToElement()->InsertNewChildElement("phrase_list");

	Dialogs.emplace_back(InputBox.Buffer, NewDialog);
	OpenDialog(InputBox.Buffer, NewDialog);
}

void DialogEditor::SaveDialog()
{
	File.Save();
}

void DialogEditor::OpenDialog(const shared_str& Str, XML_NODE* Node)
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	Nodes.clear();
	Links.clear();
	Phrases.clear();
	SelectedNode = nullptr;

	XML_NODE* RootNode = File.NavigateToNode(Node, "phrase_list");
	if (RootNode == nullptr)
	{
		return;
	}

	LastOpenDialog = Str;

	XML_NODE* PhraseNode = RootNode->FirstChildElement();

	xr_map<CDialogNode*, xr_vector<shared_str>> NodeGraph;

	auto MakeListStringFromNode = [](shared_str& Value, shared_str Text)
	{
		if (Value.size() > 0)
		{
			Value = make_string<shared_str>("%s, %s", *Value, *Text);
		}
		else
		{
			Value = Text;
		}
	};

	while (PhraseNode != nullptr)
	{
		shared_str NodeID = PhraseNode->ToElement()->Attribute("id");
		if (NodeID.size() == 0)
		{
			PhraseNode = PhraseNode->NextSibling();
			continue;
		}

		CDialogNode* MacroNode = Nodes.emplace_back(new CDialogNode(*NodeID));
		MacroNode->ParentNode = PhraseNode;

		if (const char* BranchAttr = PhraseNode->ToElement()->Attribute("branch"))
		{
			MacroNode->IsBranch = (xr_string(BranchAttr) == "1");
		}

		XML_NODE* ChildNode = PhraseNode->FirstChildElement();

		while (ChildNode != nullptr)
		{
			xr_string NodeName = ChildNode->Value();
			shared_str NodeText = ChildNode->ToElement() ? ChildNode->ToElement()->GetText() : nullptr;

			if (NodeText.size() == 0)
			{
				ChildNode = ChildNode->NextSibling();
				continue;
			}

			if (NodeName == "text")
			{
				MacroNode->Text = NodeText;
				MacroNode->TextNode = ChildNode;
			}
			else if (NodeName == "dont_has_info")
			{
				MakeListStringFromNode(MacroNode->DontHasInfo, NodeText);
				MacroNode->DontHasInfoNode.push_back(ChildNode);
			}
			else if (NodeName == "has_info")
			{
				MakeListStringFromNode(MacroNode->HasInfo, NodeText);
				MacroNode->HasInfoNode.push_back(ChildNode);
			}
			else if (NodeName == "is_final")
			{
				MacroNode->IsFinal = NodeText == "1";
				MacroNode->IsFinalNode = ChildNode;
			}
			else if (NodeName == "give_info")
			{
				MakeListStringFromNode(MacroNode->GiveInfo, NodeText);
				MacroNode->GiveInfoNode.push_back(ChildNode);
			}
			else if (NodeName == "precondition")
			{
				MacroNode->Precondition = NodeText;
				MacroNode->PreconditionNode = ChildNode;
			}
			else if (NodeName == "action")
			{
				MacroNode->Action = NodeText;
				MacroNode->ActionNode = ChildNode;
			}
			else if (NodeName == "next")
			{
				NodeGraph[MacroNode].push_back(NodeText);
				MacroNode->NextNodes.push_back(ChildNode);
			}

			Phrases.emplace_back(NodeID, MacroNode->Text);
			ChildNode = ChildNode->NextSibling();
		}
		PhraseNode = PhraseNode->NextSibling();
	}

	RootNode = RootNode->Parent()->FirstChildElement();
	while (RootNode != nullptr)
	{
		if (RootNode->ToElement() != nullptr)
		{
			xr_string UpperNodeName = RootNode->Value();

			if (UpperNodeName == "has_info")
			{
				shared_str NodeText = RootNode->ToElement()->GetText();
				MakeListStringFromNode(HasInfo, NodeText);
				NodeHasInfo = RootNode;
			}
			else if (UpperNodeName == "dont_has_info")
			{
				shared_str NodeText = RootNode->ToElement()->GetText();
				MakeListStringFromNode(DontHasInfo, NodeText);
				NodeDontHasInfo = RootNode;
			}
			else if (UpperNodeName == "precondition")
			{
				shared_str NodeText = RootNode->ToElement()->GetText();
				Precondition = NodeText;
				NodePrecondition = RootNode;
			}
		}

		RootNode = RootNode->NextSiblingElement();
	}

	BuildIdMaps();

	for (auto& [Node, ContactsList] : NodeGraph)
	{
		int Slot = 0;
		for (const shared_str& NodeName : ContactsList)
		{
			for (CDialogNode* TryNode : Nodes)
			{
				if (TryNode->NodeName == *NodeName)
				{
					NodeLink Link;
					Link.Id = ed::LinkId(NextLinkId++);
					Link.Start = Node->IsBranch ? OutputPins[Node][Slot] : OutputPins[Node][0];
					Link.End = InputPins[TryNode];
					Link.Source = Node;
					Link.Target = TryNode;
					Link.NextNode = nullptr;

					if (Node->IsBranch && Slot < (int)Node->NextNodes.size())
					{
						Link.NextNode = Node->NextNodes[Slot];
					}
					else
					{
						for (XML_NODE* Child = Node->ParentNode->FirstChildElement(); Child != nullptr; Child = Child->NextSiblingElement())
						{
							if (shared_str(Child->Value()) == "next" && shared_str(Child->ToElement()->GetText()) == TryNode->NodeName.c_str())
							{
								Link.NextNode = Child;
								break;
							}
						}
					}

					Links.push_back(Link);
					break;
				}
			}
			++Slot;
		}
	}

	std::sort(Phrases.begin(), Phrases.end(), [](auto L, auto R)
			  {
		xr_string NameA = *L.first;
		xr_string NameB = *R.first;
		return NameA < NameB; });

	NeedLayout = true;
	FocusFirstNode = true;
	SelectNodeEvent(nullptr);
}

void DialogEditor::SelectNodeEvent(CDialogNode* Node)
{
	PropItemVec items;
	Properties->ClearProperties();

	if (Node == nullptr)
	{
		PHelper().CreateRText(items, "Preconditions\\Has Info", &HasInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeHasInfo);
		PHelper().CreateRText(items, "Preconditions\\Don't Has Info", &DontHasInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeDontHasInfo);
		PHelper().CreateRText(items, "Preconditions\\Lua Precondition", &Precondition)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangePrecondition);

		for (const auto& [ID, String] : Phrases)
		{
			xr_string Name = "Phrases\\";
			Name += *ID;
			PHelper().CreateCaption(items, Name.c_str(), *String);
		}

		Properties->AssignItems(items);
		SelectedNode = nullptr;
		return;
	}

	SelectedNode = Node;

	PHelper().CreateRText(items, "Preconditions\\Has Info", &SelectedNode->HasInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeHasInfo);
	PHelper().CreateRText(items, "Preconditions\\Don't Has Info", &SelectedNode->DontHasInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeDontHasInfo);
	PHelper().CreateRText(items, "Preconditions\\Lua Precondition", &SelectedNode->Precondition);

	PHelper().CreateRText(items, "Actions\\Give Info", &SelectedNode->GiveInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeGiveInfo);
	PHelper().CreateRText(items, "Actions\\Lua Action", &SelectedNode->Action);

	PHelper().CreateRText(items, "Text\\String ID", &SelectedNode->Text);

	static shared_str TranslateStr;

	if (SelectedNode->Text.size() > 0)
	{
		TranslateStr = Platform::ANSI_TO_UTF8(*g_pStringTable->translate(*SelectedNode->Text)).c_str();
		PHelper().CreateCaption(items, "Text\\Translated", TranslateStr);
	}

	Properties->AssignItems(items);
}

void DialogEditor::OpenFile(const xr_path& Path)
{
	if (Instance == nullptr)
	{
		return;
	}

	Instance->OpenFileInstance(Path);
}

void DialogEditor::OpenFileInstance(const xr_path& Path)
{
	Dialogs.clear();
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	Nodes.clear();
	Links.clear();
	IsOpenList = true;

	File.Load(CONFIG_PATH, "gameplay", Path.xstring().c_str());

	XML_NODE* Node = File.GetRoot();
	if (Node == nullptr)
	{
		return;
	}

	if (!IsOpen)
	{
		Show(true);
	}

	XML_NODE* ChildNode = Node->FirstChildElement();

	while (ChildNode != nullptr)
	{
		if (ChildNode->ToElement() == nullptr)
		{
			ChildNode = ChildNode->NextSibling();
			continue;
		}

		shared_str NodeID = ChildNode->ToElement()->Attribute("id");
		if (NodeID.size() == 0)
		{
			ChildNode = ChildNode->NextSibling();
			continue;
		}

		Dialogs.emplace_back(NodeID, ChildNode);
		ChildNode = ChildNode->NextSibling();
	}

	std::sort(Dialogs.begin(), Dialogs.end(), [](std::pair<shared_str, XML_NODE*>& L, std::pair<shared_str, XML_NODE*>& R)
			  {
		xr_string NameA = *L.first;
		xr_string NameB = *R.first;
		return NameA < NameB; });

	// Switch to Dialog Editor tab
	UI->ActiveTabIndex = 1;
}

void DialogEditor::ChangeHasInfo(PropValue*)
{
	if (NodeHasInfo == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
								 { return LastOpenDialog == Pair.first; });

		if (Iter != Dialogs.end())
		{
			NodeHasInfo = Iter->second->ToElement()->InsertNewChildElement("has_info");
		}
	}

	NodeHasInfo->ToElement()->SetText(*HasInfo);
}

void DialogEditor::ChangeDontHasInfo(PropValue*)
{
	if (NodeDontHasInfo == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
								 { return LastOpenDialog == Pair.first; });

		if (Iter != Dialogs.end())
		{
			NodeDontHasInfo = Iter->second->ToElement()->InsertNewChildElement("dont_has_info");
		}
	}

	NodeDontHasInfo->ToElement()->SetText(*DontHasInfo);
}

void DialogEditor::ChangePrecondition(PropValue*)
{
	if (NodePrecondition == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
								 { return LastOpenDialog == Pair.first; });

		if (Iter != Dialogs.end())
		{
			NodePrecondition = Iter->second->ToElement()->InsertNewChildElement("precondition");
		}
	}

	NodePrecondition->ToElement()->SetText(*Precondition);
}

void DialogEditor::ChangeNodeHasInfo(PropValue*)
{
	if (SelectedNode == nullptr)
	{
		return;
	}

	SelectedNode->ValidateNodes(SelectedNode->HasInfo, "has_info");
}

void DialogEditor::ChangeNodeDontHasInfo(PropValue*)
{
	if (SelectedNode == nullptr)
	{
		return;
	}

	SelectedNode->ValidateNodes(SelectedNode->DontHasInfo, "dont_has_info");
}

void DialogEditor::ChangeNodeGiveInfo(PropValue*)
{
	if (SelectedNode == nullptr)
	{
		return;
	}

	SelectedNode->ValidateNodes(SelectedNode->GiveInfo, "give_info");
}

void DialogEditor::BuildIdMaps()
{
	auto Contains = [](const xr_vector<CDialogNode*>& V, CDialogNode* N)
	{
		for (CDialogNode* X : V)
		{
			if (X == N)
			{
				return true;
			}
		}
		return false;
	};

	for (auto It = NodeIds.begin(); It != NodeIds.end();)
	{
		if (!Contains(Nodes, It->first))
		{
			It = NodeIds.erase(It);
		}
		else
		{
			++It;
		}
	}
	for (auto It = InputPins.begin(); It != InputPins.end();)
	{
		if (!Contains(Nodes, It->first))
		{
			It = InputPins.erase(It);
		}
		else
		{
			++It;
		}
	}
	for (auto It = OutputPins.begin(); It != OutputPins.end();)
	{
		if (!Contains(Nodes, It->first))
		{
			It = OutputPins.erase(It);
		}
		else
		{
			++It;
		}
	}

	{
		xr_set<ed::PinId> ValidPins;
		for (auto& [Node, Pins] : OutputPins)
		{
			for (ed::PinId P : Pins)
			{
				ValidPins.insert(P);
			}
		}
		for (auto It = OutputPinXml.begin(); It != OutputPinXml.end();)
		{
			if (ValidPins.find(It->first) == ValidPins.end())
			{
				It = OutputPinXml.erase(It);
			}
			else
			{
				++It;
			}
		}
	}

	PinNode.clear();
	PinIsOutput.clear();
	OutputPinSlot.clear();

	for (auto& [Node, Pin] : InputPins)
	{
		PinNode[Pin] = Node;
		PinIsOutput[Pin] = false;
	}
	for (auto& [Node, Pins] : OutputPins)
	{
		int k = 0;
		for (ed::PinId Pin : Pins)
		{
			PinNode[Pin] = Node;
			PinIsOutput[Pin] = true;
			OutputPinSlot[Pin] = k++;
		}
	}
	for (auto& [Node, Pins] : OutputPins)
	{
		if (!Node->IsBranch)
		{
			continue;
		}
		for (int k = 0; k < (int)Pins.size(); ++k)
		{
			if (OutputPinXml.find(Pins[k]) == OutputPinXml.end() && k < (int)Node->NextNodes.size())
			{
				OutputPinXml[Pins[k]] = Node->NextNodes[k];
			}
		}
	}

	for (CDialogNode* Node : Nodes)
	{
		if (NodeIds.find(Node) != NodeIds.end())
		{
			continue;
		}

		NodeIds[Node] = ed::NodeId(NextNodeId++);

		ed::PinId InputPin = ed::PinId(NextPinId++);
		InputPins[Node] = InputPin;
		PinNode[InputPin] = Node;
		PinIsOutput[InputPin] = false;

		int NumOutputs = Node->IsBranch ? std::max(1, (int)Node->NextNodes.size()) : 1;
		xr_vector<ed::PinId> OutPins;
		for (int k = 0; k < NumOutputs; ++k)
		{
			ed::PinId Pin = ed::PinId(NextPinId++);
			OutPins.push_back(Pin);
			PinNode[Pin] = Node;
			PinIsOutput[Pin] = true;
			OutputPinSlot[Pin] = k;
			if (Node->IsBranch && k < (int)Node->NextNodes.size())
			{
				OutputPinXml[Pin] = Node->NextNodes[k];
			}
		}
		OutputPins[Node] = OutPins;
	}
}

CDialogNode* DialogEditor::NodeFromId(ed::NodeId Id)
{
	for (auto& [Node, NodeIdValue] : NodeIds)
	{
		if (NodeIdValue == Id)
		{
			return Node;
		}
	}
	return nullptr;
}

CDialogNode* DialogEditor::NodeOwningPin(ed::PinId Id)
{
	auto It = PinNode.find(Id);
	return It != PinNode.end() ? It->second : nullptr;
}

void DialogEditor::LayoutNodes()
{
	const float ColumnWidth = 360.0f;
	const float RowHeight = 230.0f;
	const float OriginX = 40.0f;
	const float OriginY = 40.0f;

	if (Nodes.empty())
	{
		return;
	}

	xr_map<CDialogNode*, xr_vector<CDialogNode*>> Outgoing;
	xr_map<CDialogNode*, bool> HasIncoming;
	for (const auto& Link : Links)
	{
		if (Link.Source == nullptr || Link.Target == nullptr)
		{
			continue;
		}
		Outgoing[Link.Source].push_back(Link.Target);
		HasIncoming[Link.Target] = true;
	}

	xr_map<CDialogNode*, int> Depth;
	xr_vector<CDialogNode*> Queue;
	for (CDialogNode* Node : Nodes)
	{
		if (!HasIncoming[Node])
		{
			Queue.push_back(Node);
		}
	}

	if (Queue.empty())
	{
		Queue = Nodes;
	}

	for (CDialogNode* Node : Queue)
	{
		if (Depth.find(Node) == Depth.end())
		{
			Depth[Node] = 0;
		}
	}

	size_t Head = 0;
	while (Head < Queue.size())
	{
		CDialogNode* Current = Queue[Head++];
		int CurrentDepth = Depth[Current];
		auto It = Outgoing.find(Current);
		if (It != Outgoing.end())
		{
			for (CDialogNode* Next : It->second)
			{
				if (Depth.find(Next) == Depth.end())
				{
					Depth[Next] = CurrentDepth + 1;
					Queue.push_back(Next);
				}
			}
		}
	}

	int MaxDepth = 0;
	for (const auto& [Node, D] : Depth)
	{
		MaxDepth = std::max(MaxDepth, D);
	}
	for (CDialogNode* Node : Nodes)
	{
		if (Depth.find(Node) == Depth.end())
		{
			Depth[Node] = ++MaxDepth;
		}
	}

	xr_map<int, int> RowCounter;
	for (CDialogNode* Node : Nodes)
	{
		int Column = Depth[Node];
		int Row = RowCounter[Column]++;
		ed::SetNodePosition(NodeIds[Node], ImVec2(OriginX + Column * ColumnWidth, OriginY + Row * RowHeight));
	}
}

void DialogEditor::AddBranchOutput(CDialogNode* Node)
{
	if (Node == nullptr || !Node->IsBranch)
	{
		return;
	}

	ed::PinId Pin = ed::PinId(NextPinId++);
	OutputPins[Node].push_back(Pin);
	PinNode[Pin] = Node;
	PinIsOutput[Pin] = true;
	OutputPinSlot[Pin] = (int)OutputPins[Node].size() - 1;
}

void DialogEditor::DeleteNode(CDialogNode* Node)
{
	for (CDialogNode* Other : Nodes)
	{
		if (Other == Node)
		{
			continue;
		}

		XML_NODE* Child = Other->ParentNode->FirstChildElement();
		while (Child != nullptr)
		{
			shared_str ChildValue = Child->Value();
			shared_str ChildText = Child->ToElement()->GetText();
			if (ChildValue == "next" && ChildText.size() > 0 && ChildText == Node->NodeName.c_str())
			{
				XML_NODE* Next = Child->NextSiblingElement();
				Other->ParentNode->ToElement()->DeleteChild(Child);
				Child = Next;
			}
			else
			{
				Child = Child->NextSiblingElement();
			}
		}
	}

	if (Node->ParentNode != nullptr && Node->ParentNode->Parent() != nullptr)
	{
		Node->ParentNode->Parent()->DeleteChild(Node->ParentNode);
	}

	Nodes.erase(std::remove(Nodes.begin(), Nodes.end(), Node), Nodes.end());

	Links.erase(
		std::remove_if(Links.begin(), Links.end(), [Node](const NodeLink& L)
					   { return L.Source == Node || L.Target == Node; }),
		Links.end()
	);

	if (SelectedNode == Node)
	{
		SelectedNode = nullptr;
	}

	xr_delete(Node);
	BuildIdMaps();
}

void DialogEditor::UpdateSelection()
{
	int Count = ed::GetSelectedObjectCount();
	if (Count > 0)
	{
		xr_vector<ed::NodeId> Selected;
		Selected.resize(Count);
		int NodeCount = ed::GetSelectedNodes(Selected.data(), Count);

		CDialogNode* Node = nullptr;
		if (NodeCount > 0)
		{
			Node = NodeFromId(Selected[0]);
		}

		if (Node != SelectedNode)
		{
			SelectNodeEvent(Node);
		}
	}
	else if (SelectedNode != nullptr)
	{
		SelectNodeEvent(nullptr);
	}
}

void DialogEditor::DrawNode(CDialogNode* Node)
{
	ed::NodeId NodeIdValue = NodeIds[Node];

	ed::BeginNode(NodeIdValue);

	float Top = ImGui::GetCursorPos().y;
	ImGui::PushID(NodeIdValue.AsPointer());

	ImGui::TextUnformatted(Node->NodeName.c_str());
	float HeaderHeight = ImGui::GetCursorPos().y - Top;

	const float MaxBodyWidth = 310.0f;
	float BodyWidth = ComputeNodeBodyWidth(Node, MaxBodyWidth);

	const auto& OutPins = OutputPins[Node];

	ed::PushStyleColor(ed::StyleColor_PinRect, ImColor(0, 0, 0, 0));
	ed::PushStyleColor(ed::StyleColor_PinRectBorder, ImColor(0, 0, 0, 0));
	ed::BeginPin(InputPins[Node], ed::PinKind::Input);
	ed::PinPivotAlignment(ImVec2(0.0f, 0.5f));
	ed::PinPivotSize(ImVec2(0, 0));
	DrawPinArrow(ConnectedPins.find(InputPins[Node]) != ConnectedPins.end());
	ed::EndPin();
	ed::PopStyleColor(2);

	ImGui::SameLine();
	ImGui::Dummy(ImVec2(BodyWidth - 30, 0.0f));

	ImGui::SameLine();
	ed::PushStyleColor(ed::StyleColor_PinRect, ImColor(0, 0, 0, 0));
	ed::PushStyleColor(ed::StyleColor_PinRectBorder, ImColor(0, 0, 0, 0));
	if (!OutPins.empty())
	{
		ed::BeginPin(OutPins[0], ed::PinKind::Output);
		ed::PinPivotAlignment(ImVec2(1.0f, 0.5f));
		ed::PinPivotSize(ImVec2(0, 0));
		DrawPinArrow(ConnectedPins.find(OutPins[0]) != ConnectedPins.end());
		ed::EndPin();
	}
	ed::PopStyleColor(2);

	for (size_t k = 1; k < OutPins.size(); ++k)
	{
		ImGui::NewLine();
		ImGui::Dummy(ImVec2(BodyWidth, 0.0f));
		ImGui::SameLine();
		ed::PushStyleColor(ed::StyleColor_PinRect, ImColor(0, 0, 0, 0));
		ed::PushStyleColor(ed::StyleColor_PinRectBorder, ImColor(0, 0, 0, 0));
		ed::BeginPin(OutPins[k], ed::PinKind::Output);
		ed::PinPivotAlignment(ImVec2(1.0f, 0.5f));
		ed::PinPivotSize(ImVec2(0, 0));
		DrawPinArrow(ConnectedPins.find(OutPins[k]) != ConnectedPins.end());
		ed::EndPin();
		ed::PopStyleColor(2);
	}

	if (Node->IsBranch)
	{
		ImGui::NewLine();
		ImGui::Dummy(ImVec2(BodyWidth, 0.0f));
		ImGui::SameLine();
		if (ImGui::Button("Add output"))
		{
			AddBranchOutput(Node);
		}
	}

	ImGui::NewLine();
	if (!Node->IsBranch)
	{
		DrawNodeBody(Node, BodyWidth);
	}

	ImGui::PopID();
	ed::EndNode();

	DrawNodeHeaderBackground(NodeIdValue, HeaderHeight, NodeHeaderColor(Node));
}

float DialogEditor::ComputeNodeBodyWidth(CDialogNode* Node, float MaxWidth)
{
	const float InputWidth = 220.0f;
	const char* Labels[] = {
		"Text:", "Has Info:", "Don't Has Info:", "Give Info(Y/N):", "Action:", "Precondition:"
	};

	float Width = 60.0f;
	for (const char* Label : Labels)
	{
		float W = ImGui::CalcTextSize(Label).x + InputWidth + 8.0f;
		if (W > Width)
		{
			Width = W;
		}
	}

	return std::min(Width, MaxWidth);
}

void DialogEditor::DrawNodeBody(CDialogNode* Node, float BodyWidth)
{
	Node->DrawInlineEditor(BodyWidth);
}

void DialogEditor::DrawNodeHeaderBackground(ed::NodeId Id, float HeaderHeight, ImColor Color)
{
	ImVec2 Position = ed::GetNodePosition(Id);
	ImVec2 Size = ed::GetNodeSize(Id);
	if (Size.x < 1.0f)
	{
		return;
	}

	float TopPadding = ed::GetStyle().NodePadding.y;
	float Rounding = ed::GetStyle().NodeRounding;
	ImVec2 TopLeft = Position;
	ImVec2 BottomRight = ImVec2(Position.x + Size.x, Position.y + TopPadding + HeaderHeight);

	ImDrawList* DrawList = ed::GetNodeBackgroundDrawList(Id);
	DrawList->AddRectFilled(TopLeft, BottomRight, Color, Rounding, ImDrawFlags_RoundCornersTop);
	DrawList->AddLine(ImVec2(TopLeft.x, BottomRight.y), ImVec2(BottomRight.x, BottomRight.y), ImColor(255, 255, 255, 70), 1.0f);
}

void DialogEditor::HandleLinkCreation()
{
	if (ed::BeginCreate(ImColor(255, 255, 255), 2.0f))
	{
		ed::PinId StartPin = 0;
		ed::PinId EndPin = 0;

		if (ed::QueryNewLink(&StartPin, &EndPin))
		{
			CDialogNode* First = NodeOwningPin(StartPin);
			CDialogNode* Second = NodeOwningPin(EndPin);

			if (First != nullptr && Second != nullptr && First != Second)
			{
				bool StartIsOutput = (PinIsOutput.find(StartPin) != PinIsOutput.end() && PinIsOutput[StartPin]);
				bool EndIsOutput = (PinIsOutput.find(EndPin) != PinIsOutput.end() && PinIsOutput[EndPin]);

				if (StartIsOutput != EndIsOutput)
				{
					CDialogNode* Source = StartIsOutput ? First : Second;
					CDialogNode* Target = StartIsOutput ? Second : First;
					ed::PinId OutPin = StartIsOutput ? StartPin : EndPin;

					if (ed::AcceptNewItem(ImColor(128, 255, 128), 4.0f))
					{
						XML_NODE* NextNode = nullptr;
						if (Source->IsBranch)
						{
							NextNode = OutputPinXml[OutPin];
							if (NextNode == nullptr)
							{
								NextNode = Source->ParentNode->ToElement()->InsertNewChildElement("next");
								OutputPinXml[OutPin] = NextNode;
							}
							NextNode->ToElement()->SetText(Target->NodeName.c_str());
						}
						else
						{
							NextNode = Source->ParentNode->ToElement()->InsertNewChildElement("next");
							NextNode->ToElement()->SetText(Target->NodeName.c_str());
						}

						Source->MakeOutNode(Target, true);

						NodeLink Link;
						Link.Id = ed::LinkId(NextLinkId++);
						Link.Start = OutPin;
						Link.End = InputPins[Target];
						Link.Source = Source;
						Link.Target = Target;
						Link.NextNode = NextNode;
						Links.push_back(Link);
					}
				}
				else
				{
					ed::RejectNewItem(ImColor(255, 0, 0), 2.0f);
				}
			}
			else
			{
				ed::RejectNewItem(ImColor(255, 0, 0), 2.0f);
			}
		}

		ed::PinId NewNodePin = 0;
		if (ed::QueryNewNode(&NewNodePin))
		{
			ed::AcceptNewItem();
		}
	}
	ed::EndCreate();
}

void DialogEditor::HandleDeletion()
{
	if (ed::BeginDelete())
	{
		ed::NodeId NodeIdValue = 0;
		while (ed::QueryDeletedNode(&NodeIdValue))
		{
			CDialogNode* Node = NodeFromId(NodeIdValue);
			if (Node != nullptr && ed::AcceptDeletedItem())
			{
				DeleteNode(Node);
			}
		}

		ed::LinkId LinkIdValue = 0;
		while (ed::QueryDeletedLink(&LinkIdValue))
		{
			auto Iter = std::find_if(Links.begin(), Links.end(), [LinkIdValue](const NodeLink& L)
									 { return L.Id == LinkIdValue; });

			if (Iter != Links.end() && ed::AcceptDeletedItem())
			{
				if (Iter->NextNode != nullptr && Iter->Source != nullptr && Iter->Source->ParentNode != nullptr)
				{
					Iter->Source->ParentNode->ToElement()->DeleteChild(Iter->NextNode);
				}

				for (auto& [Pin, Xml] : OutputPinXml)
				{
					if (Xml == Iter->NextNode)
					{
						Xml = nullptr;
					}
				}

				Links.erase(Iter);
			}
		}
	}
	ed::EndDelete();
}

void DialogEditor::DrawMinimap()
{
	if (Nodes.empty())
	{
		return;
	}

	float inf = std::numeric_limits<float>::infinity();
	ImVec2 BoundsMin(inf, inf);
	ImVec2 boundsMax(-inf, -inf);

	for (auto* Node : Nodes)
	{
		auto Iter = NodeIds.find(Node);
		if (Iter == NodeIds.end())
		{
			continue;
		}

		ImVec2 Pos = ed::GetNodePosition(Iter->second);
		ImVec2 Size = ed::GetNodeSize(Iter->second);

		if (Pos.x < BoundsMin.x)
		{
			BoundsMin.x = Pos.x;
		}
		if (Pos.y < BoundsMin.y)
		{
			BoundsMin.y = Pos.y;
		}
		if (Pos.x + Size.x > boundsMax.x)
		{
			boundsMax.x = Pos.x + Size.x;
		}
		if (Pos.y + Size.y > boundsMax.y)
		{
			boundsMax.y = Pos.y + Size.y;
		}
	}

	if (BoundsMin.x == inf)
	{
		return;
	}

	float MapW = 180.0f;
	float MapH = 130.0f;
	float Padding = 8.0f;

	ImVec2 Avail = ImGui::GetContentRegionAvail();
	ImVec2 CanvasMin = ImGui::GetCursorScreenPos();
	ImVec2 MapPos(CanvasMin.x + Avail.x - MapW - Padding, CanvasMin.y + Avail.y - MapH - Padding);

	float ContentW = boundsMax.x - BoundsMin.x;
	float ContentH = boundsMax.y - BoundsMin.y;
	
	if (ContentW < 1.0f)
	{
		ContentW = 1.0f;
	}

	if (ContentH < 1.0f)
	{
		ContentH = 1.0f;
	}

	float Margin = 20.0f;
	float ScaleX = MapW / (ContentW + Margin * 2);
	float ScaleY = MapH / (ContentH + Margin * 2);
	float Scale = (ScaleX < ScaleY) ? ScaleX : ScaleY;

	float DrawW = ContentW * Scale;
	float DrawH = ContentH * Scale;
	ImVec2 DrawOrigin(MapPos.x + (MapW - DrawW) * 0.5f, MapPos.y + (MapH - DrawH) * 0.5f);

	ImGui::SetNextWindowPos(MapPos);
	ImGui::SetNextWindowSize(ImVec2(MapW, MapH));
	ImGui::SetNextWindowBgAlpha(0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 4.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_Alpha, 1.0f);

	ImGui::Begin("##minimap_overlay", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse | ImGuiWindowFlags_NoBackground | ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoInputs);

	ImDrawList* DrawList = ImGui::GetWindowDrawList();

	DrawList->AddRectFilled(MapPos, ImVec2(MapPos.x + MapW, MapPos.y + MapH), IM_COL32(30, 30, 30, 200), 4.0f);
	DrawList->AddRect(MapPos, ImVec2(MapPos.x + MapW, MapPos.y + MapH), IM_COL32(100, 100, 100, 200), 4.0f);

	for (auto* Node : Nodes)
	{
		auto it = NodeIds.find(Node);
		if (it == NodeIds.end())
		{
			continue;
		}

		ImVec2 pos = ed::GetNodePosition(it->second);
		ImVec2 size = ed::GetNodeSize(it->second);

		ImVec2 nMin(DrawOrigin.x + (pos.x - BoundsMin.x) * Scale, DrawOrigin.y + (pos.y - BoundsMin.y) * Scale);
		ImVec2 nMax(nMin.x + size.x * Scale, nMin.y + size.y * Scale);

		ImU32 color = IM_COL32(90, 130, 210, 255);
		if (Node->IsFinal)
		{
			color = IM_COL32(110, 200, 120, 255);
		}
		else if (Node->HasInfo.size() || Node->DontHasInfo.size() || Node->Precondition.size())
		{
			color = IM_COL32(200, 160, 90, 255);
		}

		if (Node == SelectedNode)
		{
			color = IM_COL32(255, 255, 100, 255);
		}

		DrawList->AddRectFilled(nMin, nMax, color, 1.0f);
	}

	for (const auto& Link : Links)
	{
		if (Link.Source == nullptr || Link.Target == nullptr)
		{
			continue;
		}

		auto itS = NodeIds.find(Link.Source);
		auto itT = NodeIds.find(Link.Target);
		if (itS == NodeIds.end() || itT == NodeIds.end())
		{
			continue;
		}

		ImVec2 sPos = ed::GetNodePosition(itS->second);
		ImVec2 sSize = ed::GetNodeSize(itS->second);
		ImVec2 tPos = ed::GetNodePosition(itT->second);
		ImVec2 tSize = ed::GetNodeSize(itT->second);

		ImVec2 p1(DrawOrigin.x + (sPos.x + sSize.x * 0.5f - BoundsMin.x) * Scale, DrawOrigin.y + (sPos.y + sSize.y * 0.5f - BoundsMin.y) * Scale);
		ImVec2 p2(DrawOrigin.x + (tPos.x + tSize.x * 0.5f - BoundsMin.x) * Scale, DrawOrigin.y + (tPos.y + tSize.y * 0.5f - BoundsMin.y) * Scale);

		DrawList->AddLine(p1, p2, IM_COL32(150, 230, 120, 120), 1.0f);
	}

	ImVec2 ScreenSize = ed::GetScreenSize();

	ImVec2 vpMin(DrawOrigin.x + (-BoundsMin.x) * Scale, DrawOrigin.y + (-BoundsMin.y) * Scale);
	ImVec2 vpMax(DrawOrigin.x + (ScreenSize.x - BoundsMin.x) * Scale, DrawOrigin.y + (ScreenSize.y - BoundsMin.y) * Scale);

	vpMin.x = std::max(vpMin.x, MapPos.x);
	vpMin.y = std::max(vpMin.y, MapPos.y);
	vpMax.x = std::min(vpMax.x, MapPos.x + MapW);
	vpMax.y = std::min(vpMax.y, MapPos.y + MapH);

	DrawList->AddRect(vpMin, vpMax, IM_COL32(255, 255, 255, 180), 0.0f, 0, 1.0f);

	ImGui::End();
	ImGui::PopStyleVar(4);
}
