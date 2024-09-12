#include "stdafx.h"
UIPropertiesModal* UIPropertiesModal::Form = nullptr;
UIPropertiesModal::UIPropertiesModal()
{
	m_Props = new UIPropertiesForm();
	m_Result = R_Cancel;
}

UIPropertiesModal::~UIPropertiesModal()
{
	xr_delete(m_Props);
}

void UIPropertiesModal::Draw()
{
	m_Props->Draw();
	if (ImGui::Button(g_pStringTable->translate("ed_st_ok").c_str()))
	{
		m_Result = R_Ok;
		bOpen = false;
	}ImGui::SameLine();
	if (ImGui::Button(g_pStringTable->translate("ed_st_cancel").c_str()))
	{
		bOpen = false;
	}
}

void UIPropertiesModal::Update()
{
	if (Form && !Form->IsClosed())
	{
		ImGui::SetNextWindowSize(ImVec2(400, 400), ImGuiCond_FirstUseEver);
		if (ImGui::BeginPopupModal(g_pStringTable->translate("ed_st_props_modal").c_str(), &Form->bOpen, 0, true))
		{
			Form->Draw();
			ImGui::EndPopup();
		}
	}
}

bool UIPropertiesModal::GetResult(bool& ok)
{
	if (!Form->bOpen)
	{
		ok = Form->m_Result == R_Ok;
		xr_delete(Form);
		return true;
	}
	return false;
}

void UIPropertiesModal::Show(PropItemVec& items)
{
	VERIFY(!Form);
	Form = new UIPropertiesModal();
	Form->m_Props->AssignItems(items);
}
