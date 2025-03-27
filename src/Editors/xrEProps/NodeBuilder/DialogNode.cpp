#include "stdafx.h"
#include "DialogNode.h"

CDialogNode::CDialogNode(const xr_string Name) :
	INodeUnknown(Name.data())
{
	AddContactLink("Out", true);
	AddContactLink("In");
};

void CDialogNode::Draw()
{
	INodeUnknown::Draw();

	DrawHeader();

	auto RenderItemString = [this](const char* Name, shared_str& Data)
	{
		ImGui::Text(Name);
		ImGui::SameLine();
		ImGui::PushItemWidth(120);
		string256 Value1 = {};

		if (Data.size() > 0)
		{
			strcpy(Value1, *Data);
		}

		if (ImGui::InputText((xr_string("##") + NodeName + Name).c_str(), (char*)&Value1, sizeof(Value1)))
		{
			Data = Value1;
		}
		ImGui::PopItemWidth();
	};

	RenderItemString("Has Info:", HasInfo);
	RenderItemString("Don't Has Info:", DontHasInfo);
	RenderItemString("Precondition:", Precondition);
	ImGui::Separator();

	RenderItemString("Text: ", Text);
	ImGui::Separator();

	RenderItemString("Action:", Action);
	RenderItemString("Give Info:", GiveInfo);

	DrawEnd();
}