#include "stdafx.h"
#include "Builder.h"
#include "../xrEUI/ImNodeEditor/imnodes.h"

CNodeViewport* CurrentViewport = nullptr;

void RegNode(size_t NodeID, ELinkType Type)
{
	if (CurrentViewport != nullptr)
	{
		CurrentViewport->LinksStorage[NodeID] = Type;
	}
}

int& GetLinkDrawCounter()
{
	VERIFY(CurrentViewport);
	return CurrentViewport->LinkDrawCounter;
}

int CNodeViewport::GetHoveredMode() const
{
	int HoveredNodeID = -1;
	for (auto Node : Nodes)
	{
		if (ImNodes::IsNodeSelected(Node->NodeID))
		{
			HoveredNodeID = Node->NodeID;
			if (LastSelectedNodeID != HoveredNodeID && NodeSelectCallback)
			{
				NodeSelectCallback(Node);
			}
			LastSelectedNodeID = HoveredNodeID;
			break;
		}
	}

	return HoveredNodeID;
}

bool CNodeViewport::CanCreateLink(size_t LinkID, size_t RightID)
{
	bool CheckLeft = LinksStorage.contains(LinkID);
	bool CheckRight = LinksStorage.contains(RightID);

	if (CheckLeft)
	{
		if (!CheckRight)
			return false;

		return LinksStorage[LinkID] == LinksStorage[RightID];
	}

	return CheckLeft == CheckRight;
}

CNodeViewport::CNodeViewport()
{
	Context = ImNodes::CreateContext();
}

CNodeViewport::~CNodeViewport()
{
	ImNodes::DestroyContext((ImNodesContext*)Context);
}

void CNodeViewport::Draw()
{
	CurrentViewport = this;
	LinkDrawCounter = 0;

	ImNodes::BeginNodeEditor();

	for (auto Node : Nodes)
	{
		Node->Draw();
	}

	for (int i = 0; i < Links.size(); ++i, LinkDrawCounter++)
	{
		const std::pair<int, int>& p = Links[i];
		// in this case, we just use the array index of the link
		// as the unique identifier
		ImNodes::Link(LinkDrawCounter, p.first, p.second);
	}

	ImGui::SetWindowFontScale(0.9f); // ����������� ����������
	ImNodes::MiniMap();
	ImNodes::EndNodeEditor();

	int start_attr, end_attr;
	if (ImNodes::IsLinkCreated(&start_attr, &end_attr))
	{
		if (CanCreateLink(start_attr, end_attr))
		{
			INodeUnknown* Left = nullptr;
			INodeUnknown* Right = nullptr;

			for (INodeUnknown* Node : Nodes)
			{
				if (Node->ContactLinkIn.ID == start_attr || Node->ContactLinkIn.ID == end_attr)
				{
					Left = Node;
				}

				if (Node->ContactLinkOut.ID == start_attr || Node->ContactLinkOut.ID == end_attr)
				{
					Right = Node;
				}

				if (Left != nullptr && Right != nullptr)
					break;
			}

			if (Left != nullptr && Right != nullptr)
			{
				Right->OutNodes.push_back(Left);
			}

			Links.emplace_back(start_attr, end_attr);
		}
	}

}

void CNodeViewport::DrawEnd()
{
	LinksStorage.clear();
}
