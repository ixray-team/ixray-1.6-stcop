#include "stdafx.h"
#include "DialogNode.h"
#include "IconsFontAwesome6.h"

CDialogNode::CDialogNode(const xr_string Name) :
	INodeUnknown(Name.data())
{
	AddContactLink("Out", true);
	AddContactLink("In");
};

void CDialogNode::RenderItemString(const char* RawName, const char* Name, shared_str& Data, XML_NODE*& Node, float Width)
{
	ImGui::Text(Name);
	ImGui::SameLine();
	float ItemWidth = Width - ImGui::CalcTextSize(Name).x - ImGui::GetStyle().ItemSpacing.x;
	if (ItemWidth < 1.0f)
		ItemWidth = 60.0f;
	ImGui::PushItemWidth(ItemWidth);
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

void CDialogNode::RenderItemString(const char* RawName, const char* Name, shared_str& Data, xr_vector<XML_NODE*>& Node, float Width)
{
	ImGui::Text(Name);
	ImGui::SameLine();
	float ItemWidth = Width - ImGui::CalcTextSize(Name).x - ImGui::GetStyle().ItemSpacing.x;
	if (ItemWidth < 1.0f)
		ItemWidth = 60.0f;
	ImGui::PushItemWidth(ItemWidth);
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

bool (*CDialogNode::FindTranslation)(const shared_str& Id, xr_string& OutText) = nullptr;
void (*CDialogNode::RequestLocalization)(CDialogNode* Node) = nullptr;

void CDialogNode::SetText(const shared_str& NewText)
{
	Text = NewText;
	if (ParentNode == nullptr)
	{
		return;
	}

	if (TextNode == nullptr)
	{
		TextNode = ParentNode->ToElement()->InsertNewChildElement("text");
	}

	TextNode->ToElement()->SetText(Text.c_str());
}

void CDialogNode::SetElement(const char* RawName, const shared_str& Value, XML_NODE*& Node)
{
	if (ParentNode == nullptr)
	{
		return;
	}

	if (Value.size() == 0)
	{
		if (Node != nullptr)
		{
			ParentNode->ToElement()->DeleteChild(Node);
			Node = nullptr;
		}
		return;
	}

	if (Node == nullptr)
	{
		Node = ParentNode->ToElement()->InsertNewChildElement(RawName);
	}
	Node->ToElement()->SetText(Value.c_str());
}

void CDialogNode::SetFinal(bool Value)
{
	IsFinal = Value;
	if (IsFinal)
	{
		if (IsFinalNode == nullptr)
		{
			IsFinalNode = ParentNode->ToElement()->InsertNewChildElement("is_final");
		}
		IsFinalNode->ToElement()->SetText("1");
	}
	else if (IsFinalNode != nullptr)
	{
		ParentNode->ToElement()->DeleteChild(IsFinalNode);
		IsFinalNode = nullptr;
	}
}

void CDialogNode::RenderField(const char* RawName, const char* Name, shared_str& Data, XML_NODE*& Node, float LabelWidth, float Width)
{
	ImGui::AlignTextToFramePadding();
	ImGui::TextDisabled("%s", Name);
	ImGui::SameLine(LabelWidth);
	ImGui::SetNextItemWidth(Width - LabelWidth);

	string256 Value = {};
	if (Data.size() > 0)
	{
		xr_strcpy(Value, *Data);
	}

	if (ImGui::InputText((xr_string("##") + RawName).c_str(), Value, sizeof(Value)))
	{
		Data = Value;
		if (Node == nullptr)
		{
			Node = ParentNode->ToElement()->InsertNewChildElement(RawName);
		}
		Node->ToElement()->SetText(Value);
	}
}

void CDialogNode::RenderField(const char* RawName, const char* Name, shared_str& Data, xr_vector<XML_NODE*>& Node, float LabelWidth, float Width)
{
	ImGui::AlignTextToFramePadding();
	ImGui::TextDisabled("%s", Name);
	ImGui::SameLine(LabelWidth);
	ImGui::SetNextItemWidth(Width - LabelWidth);

	string256 Value = {};
	if (Data.size() > 0)
	{
		xr_strcpy(Value, *Data);
	}

	if (ImGui::InputTextWithHint((xr_string("##") + RawName).c_str(), "comma separated", Value, sizeof(Value)))
	{
		Data = Value;
		ValidateNodes(Data, RawName);
	}
	ImGui::SetItemTooltip("Comma separated list");
}

void CDialogNode::RenderTranslation(float Width)
{
	const ImVec4 WarningColor = { 0.95f, 0.72f, 0.3f, 1.f };

	if (Text.size() == 0)
	{
		ImGui::TextDisabled("No text");
		ImGui::SameLine();
		if (RequestLocalization != nullptr && ImGui::SmallButton(ICON_FA_LANGUAGE " Add text"))
		{
			RequestLocalization(this);
		}
		return;
	}

	xr_string Translation;
	if (FindTranslation != nullptr && FindTranslation(Text, Translation))
	{
		// Long phrases are cut: the full text is in the tooltip
		size_t MaxLength = 220;
		const bool IsCut = Translation.size() > MaxLength;
		if (IsCut)
		{
			// Do not split a UTF-8 sequence
			while (MaxLength > 0 && (static_cast<u8>(Translation[MaxLength]) & 0xC0) == 0x80)
			{
				--MaxLength;
			}
		}
		xr_string Shown = IsCut ? Translation.substr(0, MaxLength) + "..." : Translation;

		ImGui::PushStyleColor(ImGuiCol_Text, { 0.86f, 0.88f, 0.92f, 1.f });
		ImGui::PushTextWrapPos(ImGui::GetCursorPosX() + Width);
		ImGui::TextUnformatted(Shown.c_str());
		ImGui::PopTextWrapPos();
		ImGui::PopStyleColor();

		if (IsCut)
		{
			ImGui::SetItemTooltip("%s", Translation.c_str());
		}
		return;
	}

	ImGui::TextColored(WarningColor, ICON_FA_TRIANGLE_EXCLAMATION " Not localized");
	ImGui::SameLine();
	if (RequestLocalization != nullptr && ImGui::SmallButton(ICON_FA_LANGUAGE " Localize..."))
	{
		RequestLocalization(this);
	}
}

void CDialogNode::DrawInlineEditor(float Width)
{
	if (ParentNode == nullptr)
		return;

	static const char* Labels[] = { "Text", "Has Info", "Don't Has Info", "Give Info", "Action", "Precondition" };
	float LabelWidth = 0.f;
	for (const char* Label : Labels)
	{
		LabelWidth = std::max(LabelWidth, ImGui::CalcTextSize(Label).x);
	}
	LabelWidth += ImGui::GetStyle().ItemSpacing.x * 2.f;

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, { ImGui::GetStyle().ItemSpacing.x, 3.f });

	RenderField("text", "Text", Text, TextNode, LabelWidth, Width);
	RenderTranslation(Width);
	ImGui::Dummy({ Width, 4.f });

	RenderField("has_info", "Has Info", HasInfo, HasInfoNode, LabelWidth, Width);
	RenderField("dont_has_info", "Don't Has Info", DontHasInfo, DontHasInfoNode, LabelWidth, Width);
	RenderField("precondition", "Precondition", Precondition, PreconditionNode, LabelWidth, Width);
	ImGui::Dummy({ Width, 4.f });

	RenderField("give_info", "Give Info", GiveInfo, GiveInfoNode, LabelWidth, Width);
	RenderField("action", "Action", Action, ActionNode, LabelWidth, Width);

	bool Final = IsFinal;
	if (ImGui::Checkbox("Final phrase", &Final))
	{
		SetFinal(Final);
	}
	ImGui::SetItemTooltip("The dialog ends after this phrase");

	ImGui::PopStyleVar();
}

void CDialogNode::AddContactLink(const xr_string& Name, bool IsOut)
{
	INodeUnknown::AddContactLink(Name, IsOut);
}

void CDialogNode::MakeOutNode(INodeUnknown* Node, bool Silent)
{
	INodeUnknown::MakeOutNode(Node);
	CDialogNode* TryNode = (CDialogNode*)Node;

	if (!Silent && ParentNode != nullptr)
	{
		XML_NODE* NewNextNode = ParentNode->ToElement()->InsertNewChildElement("next");
		NewNextNode->ToElement()->SetText(TryNode->NodeName.c_str());
	}
}

void CDialogNode::DestroyContacts()
{
	for (INodeUnknown* Contact : InNodes)
	{
		CDialogNode* DialogContactNode = (CDialogNode*)Contact;
		auto XMLElement = DialogContactNode->ParentNode->FirstChildElement();
		
		while (XMLElement != nullptr)
		{
			shared_str TextNode = XMLElement->Value();
			if (TextNode == "next")
			{
				shared_str Value = XMLElement->GetText();
				if (*Value == NodeName)
				{
					auto NextNode = XMLElement->NextSiblingElement();
					XMLElement->Parent()->DeleteChild(XMLElement);
					XMLElement = NextNode;
					continue;
				}
			}

			XMLElement = XMLElement->NextSiblingElement();
		}
	}
}