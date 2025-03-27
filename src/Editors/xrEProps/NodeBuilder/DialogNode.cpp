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

	auto RenderItemString = [this](const char* Name, shared_str& Data, size_t Size)
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
		}
		ImGui::PopItemWidth();
	};

	RenderItemString("Has Info:", HasInfo, 154);
	RenderItemString("Don't Has Info:", DontHasInfo, 120);
	RenderItemString("Precondition:", Precondition, 131);

	ImGui::Separator();

	RenderItemString("Text:", Text, 175);
	ImGui::Separator();

	RenderItemString("Action:", Action, 164);
	RenderItemString("Give Info:", GiveInfo, 146);

	DrawEnd();
}