#include "stdafx.h"
#include "DialogNode.h"

CDialogNode::CDialogNode(const xr_string Name) :
	INodeUnknown(Name.data())
{
	AddContactLink("Out", true);
	AddContactLink("In");
};

void CDialogNode::RenderItemString(const char* RawName, const char* Name, shared_str& Data, XML_NODE*& Node, size_t Size)
{
	ImGui::Text(Name);
	ImGui::SameLine();
	ImGui::PushItemWidth(Size);
	string256 Value1 = {};

	if (Data.size() > 0)
	{
		strcpy(Value1, *Data);
	}

	if (ImGui::InputText((xr_string("##") + NodeName + Name).c_str(), (char*)&Value1, sizeof(Value1)))
	{
		Data = Value1;

		if (Node == nullptr)
		{
			Node = ParentNode->ToElement()->InsertNewChildElement(RawName);
		}

		Node->ToElement()->SetText(Value1);
	}
	ImGui::PopItemWidth();
}

void CDialogNode::RenderItemString(const char* RawName, const char* Name, shared_str& Data, xr_vector<XML_NODE*>& Node, size_t Size)
{
	ImGui::Text(Name);
	ImGui::SameLine();
	ImGui::PushItemWidth(Size);
	string256 Value1 = {};

	if (Data.size() > 0)
	{
		strcpy(Value1, *Data);
	}

	if (ImGui::InputText((xr_string("##") + NodeName + Name).c_str(), (char*)&Value1, sizeof(Value1)))
	{
		Data = Value1; ValidateNodes(Data, RawName);
	}
	ImGui::PopItemWidth();
}

void CDialogNode::ValidateNodes(shared_str& Data, const char* RawName)
{
	auto NodesValues = xr_string(*Data).Split(',');

	xr_hash_map<XML_NODE*, bool> NodesToDelete;
	xr_hash_map<xr_string, bool> NodesToCreate;

	{
		XML_NODE* Child = ParentNode->FirstChildElement();
		while (Child != nullptr)
		{
			shared_str NodeName = Child->Value();
			if (!*NodeName || NodeName != RawName)
			{
				Child = Child->NextSibling();
				continue;
			}
			NodesToDelete[Child] = true;

			Child = Child->NextSibling();
		}
	}

	for (xr_string Value : NodesValues)
	{
		Value = Value.RemoveWhitespaces();
		if (Value.empty())
			continue;

		NodesToCreate[Value] = true;

		for (auto& [Node, Check] : NodesToDelete)
		{
			shared_str NodeText = Node->ToElement()->GetText();
			if (!*NodeText)
			{
				continue;
			}

			if (*NodeText == Value)
			{
				Check = false;
				NodesToCreate[Value] = false;
				break;
			}
		}
	}

	for (auto& [Node, Check] : NodesToDelete)
	{
		if (Check)
		{
			ParentNode->ToElement()->DeleteChild(Node);
		}
	}

	for (auto& [Node, Check] : NodesToCreate)
	{
		if (Check)
		{
			XML_NODE* NewNode = ParentNode->ToElement()->InsertNewChildElement(RawName);
			NewNode->ToElement()->SetText(Node.c_str());
		}
	}
}

void CDialogNode::Draw()
{
	INodeUnknown::Draw();

	DrawHeader();

	RenderItemString("has_info", "Has Info:", HasInfo, HasInfoNode, 154);
	RenderItemString("dont_has_info", "Don't Has Info:", DontHasInfo, DontHasInfoNode, 120);
	RenderItemString("precondition", "Precondition:", Precondition, PreconditionNode, 131);

	ImGui::Separator();

	RenderItemString("text", "Text:", Text, TextNode, 175);
	ImGui::Separator();

	RenderItemString("action", "Action:", Action, ActionNode, 164);
	RenderItemString("give_info", "Give Info:", GiveInfo, GiveInfoNode, 146);
	
	if (ImGui::Checkbox((xr_string("Is Final##") + NodeName).c_str(), &IsFinal))
	{
		if (IsFinal)
		{
			if (IsFinalNode == nullptr)
			{
				IsFinalNode = ParentNode->ToElement()->InsertNewChildElement("is_final");
			}
			IsFinalNode->ToElement()->SetText("1");
		}
		else
		{
			ParentNode->ToElement()->DeleteChild(IsFinalNode);
		}
	}

	DrawEnd();
}

void CDialogNode::AddContactLink(const xr_string& Name, bool IsOut)
{
	INodeUnknown::AddContactLink(Name, IsOut);
}

void CDialogNode::MakeOutNode(INodeUnknown* Node)
{
	INodeUnknown::MakeOutNode(Node);
	CDialogNode* TryNode = (CDialogNode*)Node;

	if (ParentNode != nullptr)
	{
		XML_NODE* NewNextNode = ParentNode->ToElement()->InsertNewChildElement("next");
		NewNextNode->ToElement()->SetText(TryNode->NodeName.c_str());
	}
}