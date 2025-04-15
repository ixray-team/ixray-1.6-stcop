#include "stdafx.h"
#include "UIDialogsView.h"
#include "../Editor/UI_LevelTools.h"
#include "../../../xrEngine/string_table.h"

namespace detail
{
	bool show_modal_input_box = false;
	char input_buffer[256] = "";
	bool HasResult = false;

	void ShowModalInputBox()
	{
		if (show_modal_input_box)
		{
			HasResult = false;

			// Always center this window when appearing
			ImVec2 center = ImGui::GetMainViewport()->GetCenter();
			ImGui::SetNextWindowPos(center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

			if (ImGui::BeginPopupModal("InputBox", NULL, ImGuiWindowFlags_AlwaysAutoResize))
			{
				// Input text field
				ImGui::Text("Enter something:");
				ImGui::InputText("##input", input_buffer, IM_ARRAYSIZE(input_buffer));

				// Buttons
				if (ImGui::Button("OK", ImVec2(120, 0)))
				{
					// Clear the input and close the modal
					show_modal_input_box = false;
					ImGui::CloseCurrentPopup();
					HasResult = true;
				}

				ImGui::SetItemDefaultFocus();
				ImGui::SameLine();

				if (ImGui::Button("Cancel", ImVec2(120, 0)))
				{
					input_buffer[0] = '\0';
					show_modal_input_box = false;
					ImGui::CloseCurrentPopup();
				}

				ImGui::EndPopup();
			}
		}
	}
}

CUIDialogView::CUIDialogView()
{
	NodeSelectCallback = xr_make_delegate(this, &CUIDialogView::SelectNodeEvent);
	bOpen = false;
}

CUIDialogView::~CUIDialogView()
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
}

void CUIDialogView::Draw()
{
	if (!bOpen)
		return;

	if (ImGui::Begin("Dialogs Editor", &bOpen))
	{
		if (ImGui::BeginChild("Dialogs in file", { (IsOpenList ? 300.f : 20.f), 0 }))
		{
			if (IsOpenList)
			{
				ImGui::SetNextItemWidth(300);
				if (ImGui::BeginListBox("##ItemsInFile", { 0, ImGui::GetWindowSize().y - 30 }))
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
					InputBoxMode = DialogInputBoxMode::DialogName;
					detail::show_modal_input_box = true;
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

		int HoveredNodeID = GetHoveredMode();

		if (ImGui::IsMouseReleased(1))
		{
			ImGui::OpenPopup("##nodesdialogscontextmenumacro");
		}

		if (ImGui::BeginPopup("##nodesdialogscontextmenumacro"))
		{
			if (ImGui::MenuItem("Create Node"))
			{
				InputBoxMode = DialogInputBoxMode::NodeName;
				detail::show_modal_input_box = true;
				std::memset(detail::input_buffer, 0, sizeof(detail::input_buffer));

				if (!Phrases.empty())
				{
					int NodeID = atoi(*Phrases.back().first) + 1;
					std::memcpy(detail::input_buffer, xr_string::ToString(NodeID).c_str(), sizeof(detail::input_buffer));
				}
			}

			if (HoveredNodeID != -1 && ImGui::MenuItem("Delete Node"))
			{
				auto SelectedNode = std::find_if(Nodes.begin(), Nodes.end(), [HoveredNodeID](INodeUnknown* TestingNode)
				{
					return TestingNode->NodeID == HoveredNodeID;
				});

				if (SelectedNode != Nodes.end())
				{
					CDialogNode* DialogNode = (CDialogNode*)*SelectedNode;
					DialogNode->ParentNode->Parent()->DeleteChild(DialogNode->ParentNode);
					Nodes.erase(SelectedNode);
					DialogNode->DestroyContacts();
					xr_delete(DialogNode);
				}
			}

			ImGui::EndPopup();
		}

		if (detail::show_modal_input_box)
		{
			ImGui::OpenPopup("InputBox");
			detail::ShowModalInputBox();
		}
		else if (detail::HasResult)
		{
			detail::HasResult = false;

			auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
			{
				return LastOpenDialog == Pair.first;
			});

			if (InputBoxMode == DialogInputBoxMode::NodeName)
			{
				if (Iter != Dialogs.end())
				{
					if (XML_NODE* RootNode = File.NavigateToNode(Iter->second, "phrase_list"))
					{
						XML_NODE* NewNode = RootNode->ToElement()->InsertNewChildElement("phrase");
						NewNode->ToElement()->SetAttribute("id", detail::input_buffer);

						CDialogNode* MacroNode = (CDialogNode*)Nodes.emplace_back(new CDialogNode(detail::input_buffer));
						MacroNode->ParentNode = NewNode;
						const ImVec2 click_pos = ImGui::GetMousePosOnOpeningCurrentPopup();
						MacroNode->SetStartPos(click_pos.x, click_pos.y);
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
		CNodeViewport::Draw();
	}

	ImGui::End();

	CNodeViewport::DrawEnd();
}

void CUIDialogView::Show(bool State)
{
	bOpen = State;
}

void CUIDialogView::NewDialog(XML_NODE* RootDialogNode)
{
	XML_NODE* NewDialog = RootDialogNode->ToElement()->InsertNewChildElement("dialog");
	NewDialog->ToElement()->SetAttribute("id", detail::input_buffer);
	NewDialog->ToElement()->InsertNewChildElement("phrase_list");

	Dialogs.emplace_back(detail::input_buffer, NewDialog);
	OpenDialog(detail::input_buffer, NewDialog);
}

void CUIDialogView::SaveDialog()
{
	File.Save();
}

void CUIDialogView::OpenDialog(const shared_str& Str, XML_NODE* Node)
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	Nodes.clear();
	Phrases.clear();
	LastClickedDialogNode = nullptr;

	XML_NODE* RootNode = File.NavigateToNode(Node, "phrase_list");
	if (RootNode == nullptr)
		return;

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

		CDialogNode* MacroNode = (CDialogNode*)Nodes.emplace_back(new CDialogNode(*NodeID));
		MacroNode->ParentNode = PhraseNode;

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

	if (NodeGraph.empty())
		return;

	float NodeOffsetXIterator = 300;
	float NodeOffsetYIterator = 0;

	using GraphData = std::pair<CDialogNode*, xr_vector<shared_str>>;
	xr_vector<GraphData> vec(NodeGraph.begin(), NodeGraph.end());

	std::sort(vec.begin(), vec.end(), [](GraphData L, GraphData R)
		{
			return L.first->NodeName < R.first->NodeName;
		});

	for (auto& [Node, ContactsList] : vec)
	{
		int ContackID = Node->GetContactLink(true);
		NodeOffsetYIterator = Node->StartPostion.y;
		NodeOffsetXIterator = Node->StartPostion.x + 300;

		for (const shared_str& NodeName : ContactsList)
		{
			for (INodeUnknown* TryNode : Nodes)
			{
				if (TryNode->NodeName == *NodeName)
				{
					TryNode->SetStartPos(NodeOffsetXIterator, NodeOffsetYIterator);

					int NextID = TryNode->GetContactLink();
					Node->CreateContactLink(ContackID, NextID);
					Node->MakeOutNode(TryNode, true);
					TryNode->MakeInNode(Node);

					NodeOffsetYIterator += 230;
				}
			}
		}

		NodeOffsetXIterator += 300;
	}

	std::sort(Phrases.begin(), Phrases.end(), [](auto L, auto R)
	{
		xr_string NameA = *L.first;
		xr_string NameB = *R.first;

		return NameA < NameB;
	});

	SelectNodeEvent(nullptr);
}

void CUIDialogView::SelectNodeEvent(INodeUnknown* Node)
{
	PropItemVec items;
	UIPropertiesForm* Properties = LTools->GetProperties();
	Properties->ClearProperties();

	if (Node == nullptr)
	{
		PHelper().CreateRText(items, "Preconditions\\Has Info", &HasInfo)->OnChangeEvent = xr_make_delegate(this, &CUIDialogView::ChangeHasInfo);
		PHelper().CreateRText(items, "Preconditions\\Don't Has Info", &DontHasInfo)->OnChangeEvent = xr_make_delegate(this, &CUIDialogView::ChangeDontHasInfo);
		PHelper().CreateRText(items, "Preconditions\\Lua Precondition", &Precondition)->OnChangeEvent = xr_make_delegate(this, &CUIDialogView::ChangePrecondition);

		for (const auto& [ID, String] : Phrases)
		{
			xr_string Name = "Phrases\\";
			Name += *ID;

			PHelper().CreateCaption(items, Name.c_str(), *String);
		}

		Properties->AssignItems(items);
		LastClickedDialogNode = nullptr;
		return;
	}

	LastClickedDialogNode = (CDialogNode*)Node;

	PHelper().CreateRText(items, "Preconditions\\Has Info", &LastClickedDialogNode->HasInfo)->OnChangeEvent = xr_make_delegate(this, &CUIDialogView::ChangeNodeHasInfo);
	PHelper().CreateRText(items, "Preconditions\\Don't Has Info", &LastClickedDialogNode->DontHasInfo)->OnChangeEvent = xr_make_delegate(this, &CUIDialogView::ChangeNodeDontHasInfo);
	PHelper().CreateRText(items, "Preconditions\\Lua Precondition", &LastClickedDialogNode->Precondition);

	PHelper().CreateRText(items, "Actions\\Give Info", &LastClickedDialogNode->GiveInfo)->OnChangeEvent = xr_make_delegate(this, &CUIDialogView::ChangeNodeGiveInfo);
	PHelper().CreateRText(items, "Actions\\Lua Action", &LastClickedDialogNode->Action);

	PHelper().CreateRText(items, "Text\\String ID", &LastClickedDialogNode->Text);

	static shared_str TranslateStr;

	if (LastClickedDialogNode->Text.size() > 0)
	{
		TranslateStr = Platform::ANSI_TO_UTF8(*g_pStringTable->translate(*LastClickedDialogNode->Text)).c_str();
		PHelper().CreateCaption(items, "Text\\Translated", TranslateStr);
	}

	Properties->AssignItems(items);
}

void CUIDialogView::OpenFile(const xr_path& Path)
{
	static CUIDialogView Viewer;
	Viewer.Dialogs.clear();

	for (auto Node : Viewer.Nodes)
	{
		xr_delete(Node);
	}
	Viewer.Nodes.clear();
	Viewer.IsOpenList = true;

	Viewer.File.Load(CONFIG_PATH, "gameplay", Path.xstring().c_str());

	XML_NODE* Node = Viewer.File.GetRoot();
	if (Node == nullptr)
		return;

	if (!Viewer.bOpen)
	{
		Viewer.Show(true);
		UI->Push(&Viewer, false);
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

		Viewer.Dialogs.emplace_back(NodeID, ChildNode);
		ChildNode = ChildNode->NextSibling();
	}

	std::sort(Viewer.Dialogs.begin(), Viewer.Dialogs.end(), [](std::pair<shared_str, XML_NODE*>& L, std::pair<shared_str, XML_NODE*>& R)
	{
		xr_string NameA = *L.first;
		xr_string NameB = *R.first;

		return NameA < NameB;
	});
}

void CUIDialogView::ChangeHasInfo(PropValue*)
{
	if (NodeHasInfo == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
			{
				return LastOpenDialog == Pair.first;
			});

		if (Iter != Dialogs.end())
		{
			NodeHasInfo = Iter->second->ToElement()->InsertNewChildElement("has_info");
		}
	}

	NodeHasInfo->ToElement()->SetText(*HasInfo);
}

void CUIDialogView::ChangeDontHasInfo(PropValue*)
{
	if (NodeDontHasInfo == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
			{
				return LastOpenDialog == Pair.first;
			});

		if (Iter != Dialogs.end())
		{
			NodeDontHasInfo = Iter->second->ToElement()->InsertNewChildElement("dont_has_info");
		}
	}

	NodeDontHasInfo->ToElement()->SetText(*DontHasInfo);
}

void CUIDialogView::ChangePrecondition(PropValue*)
{
	if (NodePrecondition == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
			{
				return LastOpenDialog == Pair.first;
			});

		if (Iter != Dialogs.end())
		{
			NodePrecondition = Iter->second->ToElement()->InsertNewChildElement("precondition");
		}
	}

	NodePrecondition->ToElement()->SetText(*Precondition);
}

void CUIDialogView::ChangeNodeHasInfo(PropValue*)
{
	if (LastClickedDialogNode == nullptr)
		return;

	LastClickedDialogNode->ValidateNodes(LastClickedDialogNode->HasInfo, "has_info");
}

void CUIDialogView::ChangeNodeDontHasInfo(PropValue*)
{
	if (LastClickedDialogNode == nullptr)
		return;

	LastClickedDialogNode->ValidateNodes(LastClickedDialogNode->DontHasInfo, "dont_has_info");
}

void CUIDialogView::ChangeNodeGiveInfo(PropValue*)
{
	if (LastClickedDialogNode == nullptr)
		return;

	LastClickedDialogNode->ValidateNodes(LastClickedDialogNode->GiveInfo, "give_info");
}