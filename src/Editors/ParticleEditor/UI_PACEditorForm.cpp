#include "stdafx.h"
#include "UI_PACEditorForm.h"

UIPACEditorForm* UIPACEditorForm::Form = nullptr;

UIPACEditorForm::UIPACEditorForm()
{
}

UIPACEditorForm::~UIPACEditorForm()
{
}

void UIPACEditorForm::Draw()
{
    if (ImGui::Begin("PAC Editor", 0))
    {}
	
	ImGui::End();
}

void UIPACEditorForm::Open(PS::CPACDef* EditedPAC)
{
	VERIFY(!Form);

	Form = new UIPACEditorForm();
	Form->EditedPAC = EditedPAC;
}

void UIPACEditorForm::Update()
{
	if (Form && !Form->IsClosed())
	{
		ImGui::OpenPopup("Particle Animation Curve Editor");
		ImGui::SetNextWindowSize(ImVec2(400, 500), ImGuiCond_::ImGuiCond_FirstUseEver);
		if (ImGui::BeginPopupModal("Particle Animation Curve Editor", nullptr,0))
		{
			Form->Draw();
			ImGui::EndPopup();
		}
	}
}
