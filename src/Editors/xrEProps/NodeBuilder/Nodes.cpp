#include "stdafx.h"
#include "Nodes.h"
#include "../xrEUI/ImNodeEditor/imnodes.h"

size_t NodeIDs = 0;
size_t LinkIDs = 0;

INodeUnknown::INodeUnknown(const char* Name) :
	NodeName(Name) 
{
	NodeID = NodeIDs++;
}

void INodeUnknown::Draw()
{
	if (!WeStarted)
	{
		ImNodes::SetNodeScreenSpacePos(NodeID, StartPostion);
		WeStarted = true;
	}
}

void INodeUnknown::AddContactLink(const xr_string& Name, bool IsOut)
{
	//ImNodesPinShape_Triangle
	if (IsOut)
	{
		ContactLinkOut = { Name, LinkIDs++, eDefault , ImColor(0, 0, 0), true };
	}
	else
	{
		ContactLinkIn = { Name, LinkIDs++, eDefault , ImColor(0, 0, 0), true };
	}
}

void INodeUnknown::AddOutLink(const xr_string& Name, ELinkType Type, ImColor Color)
{
	OutLinks.push_back({ Name, LinkIDs++, Type, Color, true});
}

void INodeUnknown::AddInLink(const xr_string& Name, ELinkType Type, ImColor Color)
{
	InLinks.push_back({ Name, LinkIDs++, Type, Color, false});
}

int INodeUnknown::GetContactLink(bool IsOut) const
{
	if (IsOut)
	{
		if (ContactLinkOut.IsIn)
		{
			return ContactLinkOut.ID;
		}
	}
	else
	{
		if (ContactLinkIn.IsIn)
		{
			return ContactLinkIn.ID;
		}
	}

	return -1;
}

void INodeUnknown::DrawHeader()
{
	ImNodes::PushColorStyle(ImNodesCol_NodeBackground, Background);
	ImNodes::PushColorStyle(ImNodesCol_TitleBar, Header);
	ImNodes::BeginNode(NodeID);

	ImNodes::BeginNodeTitleBar();
		ImGui::TextUnformatted(NodeName.c_str());
	ImNodes::EndNodeTitleBar();

	DrawLinks(true);
}

void INodeUnknown::CreateContactLink(int Parent, int Child)
{
	LinkStorage.emplace_back(Parent, Child);
}

void INodeUnknown::AddChild(INodeUnknown* Node, ELinkType Type)
{
	Childs.push_back(Node);

	int ContackID = Node->GetLink(Type, true);
	int ParentID = GetLink(Type);

	if (ContackID == -1 || ParentID == -1)
		return;

	LinkStorage.emplace_back(ParentID, ContackID);
}

int INodeUnknown::GetLink(ELinkType Type, bool IsOut)
{
	xr_vector<LinkData>* LinksGroup = nullptr;

	if (IsOut)
	{
		LinksGroup = &OutLinks;
	}
	else
	{
		LinksGroup = &InLinks;
	}

	for (auto& Link : *LinksGroup)
	{
		if (Link.Type == Type)
			return Link.ID;
	}

	return -1;
}

void INodeUnknown::SetStartPos(float x, float y)
{
	StartPostion = { x, y };
}

void INodeUnknown::DrawEnd()
{
	ImGui::Separator();
	DrawLinks(false);

	ImNodes::EndNode();
	ImNodes::PopColorStyle();
	ImNodes::PopColorStyle();

	int& Iterator = GetLinkDrawCounter();

	for (int i = 0; i < LinkStorage.size(); ++i, Iterator++)
	{
		const std::pair<int, int>& p = LinkStorage[i];
		// in this case, we just use the array index of the link
		// as the unique identifier
		ImNodes::Link(Iterator, p.first, p.second);
	}
}

void INodeUnknown::DrawLinks(bool Header)
{
	auto Size = ImGui::GetItemRectSize();

	if (Header)
	{
		ImNodes::PushColorStyle(ImNodesCol_Pin, ImColor(255, 255, 255));
		if (ContactLinkIn.IsIn)
		{
			ImNodes::BeginInputAttribute(ContactLinkIn.ID, ImNodesPinShape_Triangle);
			ImGui::Text(ContactLinkIn.Name.c_str());
			ImNodes::EndInputAttribute();
			ImGui::SameLine(Size.x - 40);
		}

		if (ContactLinkOut.IsIn)
		{
			ImNodes::BeginOutputAttribute(ContactLinkOut.ID, ImNodesPinShape_Triangle);
			ImGui::Text(ContactLinkOut.Name.c_str());
			ImNodes::EndOutputAttribute();

		}
		ImNodes::PopColorStyle();

		return;
	}

	for (auto& Link : OutLinks)
	{
		ImNodes::PushColorStyle(ImNodesCol_Pin, Link.Color);
			ImNodes::BeginOutputAttribute(Link.ID);
				RegNode(Link.ID, Link.Type);
				ImGui::Text(Link.Name.c_str());
			ImNodes::EndOutputAttribute();
		ImNodes::PopColorStyle();
	}

	for (auto& Link : InLinks)
	{
		ImNodes::PushColorStyle(ImNodesCol_Pin, Link.Color);
			ImNodes::BeginInputAttribute(Link.ID);
				RegNode(Link.ID, Link.Type);
				ImGui::Text(Link.Name.c_str());
			ImNodes::EndInputAttribute();
		ImNodes::PopColorStyle();
	}
}