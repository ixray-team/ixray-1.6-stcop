#include "stdafx.h"
UIKeyPressForm* UIKeyPressForm::Form = nullptr;
UIKeyPressForm::UIKeyPressForm()
{
	m_Ok = true;
}

UIKeyPressForm::~UIKeyPressForm()
{
}

void UIKeyPressForm::Draw()
{
	if (!bOpen)
		return;

	if (!ImGui::BeginPopupModal("Press any key", 0, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove, true))
	{
		ImGui::EndPopup();
		return;
	}

	if (fmod(m_TimeGlobal*1000, 1000.f) >500.f)
	{
		ImGui::TextColored(ImVec4(1,0,0,1),"Press any key to continue.");
	}
	else
	{
		ImGui::Text("Press any key to continue.");
	}
	if (ImGui::Button("Cancel", ImVec2(-1, 0)))
	{
		m_Ok = false;
		bOpen = false;
	}
	ImGui::EndPopup();
	/*else
	{
		;
		ImGui::CloseCurrentPopup();
	}*/
}

void UIKeyPressForm::Update(float timeGlobal)
{
	if (Form)
	{
		Form->m_TimeGlobal = timeGlobal;
		Form->Draw();
	}
}

void UIKeyPressForm::Show()
{
	VERIFY(!Form);
	Form = new UIKeyPressForm();
}

bool UIKeyPressForm::SetResult(const xr_shortcut& result)
{
	if (Form && !Form->IsClosed())
	{
		 Form->m_Resutl= result;
		 Form->bOpen = false;
		 return true;
	}
	return false;
}

bool UIKeyPressForm::GetResult(bool& Ok, xr_shortcut& result)
{
	if (Form && Form->IsClosed())
	{
		Ok = Form->m_Ok;
		result = Form->m_Resutl;
		xr_delete(Form);
		return true;
	}
	return false;
}
