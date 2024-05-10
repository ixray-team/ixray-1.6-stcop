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

void INodeUnknown::AddContactLink(const xr_string& Name, bool IsOut)
{
	//ImNodesPinShape_Triangle
	ContactLink.push_back({ Name, LinkIDs++, eDefault , ImColor(0, 0, 0), !IsOut });
}

void INodeUnknown::AddOutLink(const xr_string& Name, ELinkType Type, ImColor Color)
{
	OutLinks.push_back({ Name, LinkIDs++, Type, Color, true});
}

void INodeUnknown::AddInLink(const xr_string& Name, ELinkType Type, ImColor Color)
{
	InLinks.push_back({ Name, LinkIDs++, Type, Color, false});
}

int INodeUnknown::GetContactLink(bool IsOut)
{
	for (auto& Contact : ContactLink)
	{
		if (Contact.IsIn == !IsOut)
		{
			return Contact.ID;
		}
	}

	return -1;
}

void INodeUnknown::DrawHeader()
{
	ImNodes::PushColorStyle(ImNodesCol_NodeBackground, Background);
	ImNodes::BeginNode(NodeID);

	ImNodes::BeginNodeTitleBar();
	ImGui::TextUnformatted(NodeName.c_str());
	ImNodes::EndNodeTitleBar();

	DrawLinks(true);
}

void INodeUnknown::DrawEnd()
{
	ImGui::Separator();
	DrawLinks(false);

	ImNodes::EndNode();
	ImNodes::PopColorStyle();
}

void INodeUnknown::DrawLinks(bool Header)
{
	auto Size = ImGui::GetItemRectSize();

	if (Header)
	{
		for (auto& Link : ContactLink)
		{
			ImNodes::PushColorStyle(ImNodesCol_Pin, ImColor(255, 255, 255));
			if (Link.IsIn)
			{
				ImNodes::BeginInputAttribute(Link.ID, ImNodesPinShape_Triangle);
				ImGui::Text(Link.Name.c_str());
				ImNodes::EndInputAttribute();
			}
			else
			{
				ImNodes::BeginOutputAttribute(Link.ID, ImNodesPinShape_Triangle);
				ImGui::Text(Link.Name.c_str());
				ImNodes::EndOutputAttribute();

				ImGui::SameLine(Size.x - 30);
			}
			ImNodes::PopColorStyle();
		}

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