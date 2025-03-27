#include "stdafx.h"
#include "UIDialogsView.h"

CUIDialogView::CUIDialogView()
{
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
	if (!IsOpen)
		return;

	if (ImGui::Begin("Dialogs Editor", &IsOpen))
	{
		if (ImGui::BeginChild("Dialogs in file", { (IsOpenList ? 300.f : 20.f), 0}))
		{
			if (IsOpenList)
			{
				ImGui::SetNextItemWidth(300);
				if (ImGui::BeginListBox("##ItemsInFile", { 0, ImGui::GetWindowSize().y - 10 }))
				{
					for (const auto& [ID, Node] : Dialogs)
					{
						bool bSelect = false;
						if (ImGui::Selectable(ID.c_str(), &bSelect))
						{
							OpenDialog(ID, Node);
							IsOpenList = false;
						}
					}

					ImGui::EndListBox();
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
			ImGui::OpenPopup("##nodesviewportcontextmenumacro");
		}

		if (ImGui::BeginPopup("##nodesviewportcontextmenumacro"))
		{
			if (ImGui::BeginMenu("Create Node"))
			{

				ImGui::EndMenu();
			}

			if (HoveredNodeID != -1 && ImGui::MenuItem("Remove"))
			{
				Nodes.erase
				(
					std::find_if(Nodes.begin(), Nodes.end(), [HoveredNodeID](INodeUnknown* Val)
					{
						return Val->NodeID == HoveredNodeID;
					})
				);
			}

			ImGui::EndPopup();
		}
		CNodeViewport::Draw();
	}

	ImGui::End();

	CNodeViewport::DrawEnd();
}

void CUIDialogView::Show(bool State)
{
	IsOpen = State;
}

void CUIDialogView::OpenDialog(const shared_str& Str, XML_NODE* Node)
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	Nodes.clear();

	XML_NODE* RootNode = File.NavigateToNode(Node, "phrase_list");
	XML_NODE* PhraseNode = RootNode->FirstChildElement();

	xr_map<CDialogNode*, xr_vector<shared_str>> NodeGraph;

	while (PhraseNode != nullptr)
	{
		shared_str NodeID = PhraseNode->ToElement()->Attribute("id");
		if (NodeID.size() == 0)
			continue;

		CDialogNode* MacroNode = (CDialogNode*)Nodes.emplace_back(new CDialogNode(*NodeID));
		XML_NODE* ChildNode = PhraseNode->FirstChildElement();

		while (ChildNode != nullptr)
		{
			xr_string NodeName = ChildNode->Value();
			shared_str NodeText = ChildNode->ToElement()->GetText();

			if (NodeName == "text")
			{
				MacroNode->Text = NodeText;
			}
			else if (NodeName == "give_info")
			{
				MacroNode->GiveInfo = NodeText;
			}
			else if (NodeName == "has_info")
			{
				MacroNode->HasInfo = NodeText;
			}
			else if (NodeName == "give_info")
			{
				MacroNode->GiveInfo = NodeText;
			}
			else if (NodeName == "precondition")
			{
				MacroNode->Precondition = NodeText;
			}
			else if (NodeName == "action")
			{
				MacroNode->Action = NodeText;
			}
			else if (NodeName == "next")
			{
				NodeGraph[MacroNode].push_back(NodeText);
			}

			ChildNode = ChildNode->NextSibling();
		}
		PhraseNode = PhraseNode->NextSibling();
	}

	if (NodeGraph.empty())
		return;

	float NodeOffsetXIterator = 300;
	float NodeOffsetYIterator = 0;

	using GraphData = std::pair<CDialogNode*, xr_vector<shared_str>>;
	xr_vector<GraphData> vec(NodeGraph.begin(), NodeGraph.end());

	// Сортировка по убыванию NodeName
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
					NodeOffsetYIterator += 200;
				}
			}
		}

		NodeOffsetXIterator += 300;
	}

	IterateChild({ 350, 0 });
}

float CUIDialogView::IterateChild(Fvector2 Offset)
{
	return 0;
}

void CUIDialogView::OpenFile(const xr_path& Path)
{
	static CUIDialogView Viewer;

	Viewer.File.Load(CONFIG_PATH, "gameplay", Path.xstring().c_str());

	XML_NODE* Node = Viewer.File.GetRoot();
	if (Node == nullptr)
		return;

	Viewer.Show(true);
	UI->Push(&Viewer, false);

	XML_NODE* ChildNode = Node->FirstChildElement();

	while (ChildNode != nullptr)
	{
		shared_str NodeID = ChildNode->ToElement()->Attribute("id");
		if (NodeID.size() == 0)
			continue;

		Viewer.Dialogs[NodeID] = ChildNode;
		ChildNode = ChildNode->NextSibling();
	}
}
