#include "stdafx.h"
#include "UIGroupTool.h"

#include <algorithm>

UIGroupTool::UIGroupTool()
{
	m_ChooseGroup = false;
	m_selPercent = true;
}

UIGroupTool::~UIGroupTool()
{
}
void UIGroupTool::Draw()
{
	ImGui::Separator();
	{
		ImGui::BulletText(g_pStringTable->translate("ed_st_commands").c_str(), ImGuiDir_Left);
		if (ImGui::BeginPopupContextItem(g_pStringTable->translate("ed_st_commands").c_str(), 1))
		{
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_group_obj").c_str()))
			{
				ParentTools->GroupObjects();
			}
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_ungroup").c_str()))
			{
				ParentTools->UngroupObjects();
			}
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_thumbnail").c_str()))
			{
				ParentTools->MakeThumbnail();
			}
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_save_as").c_str()))
			{
				ParentTools->SaveSelectedObject();
			}
			ImGui::EndPopup();
		}
		ImGui::OpenPopupOnItemClick(g_pStringTable->translate("ed_st_commands").c_str(), 0);
	}
	ImGui::Separator();
	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode(g_pStringTable->translate("ed_st_current_obj").c_str()))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			ImGui::SetNextItemWidth(-1);
			float size = float(ImGui::CalcItemWidth());
			{
				if (ImGui::Button(g_pStringTable->translate("ed_st_select").c_str(), ImVec2(size / 2, 0)))
				{
					string_path ObjectPath = {};

					FS.update_path(ObjectPath, _groups_, "");
					FS.rescan_path(ObjectPath, true);

					UIChooseForm::SelectItem(smGroup, 1, m_Current.c_str());
					m_ChooseGroup = true;
				}
				ImGui::SameLine(0, 2);
				if (ImGui::Button(g_pStringTable->translate("ed_st_reload_refs").c_str(), ImVec2(size / 2, 0)))
				{
					ParentTools->ReloadRefsSelectedObject();
					//bForceInitListBox = TRUE;
					Tools->UpdateProperties(TRUE);
				}
			}
			ImGui::Text(g_pStringTable->translate("ed_st_current").c_str(), m_Current.c_str() ? m_Current.c_str() : "");
		}
		ImGui::Separator();
		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode(g_pStringTable->translate("ed_st_ref_select").c_str()))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			ImGui::Text(g_pStringTable->translate("ed_st_sel_by_curr").c_str()); ImGui::SameLine(); if (ImGui::Button(" +")) { SelByRefObject(true); } ImGui::SameLine(); if (ImGui::Button(" -")) { SelByRefObject(false); }
			ImGui::Text(g_pStringTable->translate("ed_st_sel_by_sel").c_str()); ImGui::SameLine(); if (ImGui::Button("=%")) { MultiSelByRefObject(true); } ImGui::SameLine(); if (ImGui::Button("+%")) { MultiSelByRefObject(false); } ImGui::SameLine(); ImGui::SetNextItemWidth(-ImGui::GetTextLineHeight() - 8); ImGui::DragFloat("%", &m_selPercent, 1, 0, 100, "%.1f");
		}
		ImGui::Separator();
		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode(g_pStringTable->translate("ed_st_pivot_align").c_str()))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			if(ImGui::Button(g_pStringTable->translate("ed_st_center_to_group").c_str(), ImVec2(-1, 0))) { ParentTools->CenterToGroup(); }
			if (ImGui::Button(g_pStringTable->translate("ed_st_align_to_obj").c_str(), ImVec2(-1, 0))) { ParentTools->AlignToObject(); }
		}
		ImGui::Separator();
		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
}

void UIGroupTool::OnDrawUI()
{
	if (m_ChooseGroup)
	{
		xr_string in;
		bool resutl;
		if (UIChooseForm::GetResult(resutl,in))
		{
			if (resutl)
			{
				m_Current = in;
				ParentTools->SetCurrentObject(m_Current.c_str());
			}
			m_ChooseGroup = false;
		}
		UIChooseForm::Update();
	}
}

void UIGroupTool::MultiSelByRefObject(bool clear_prev)
{
	ObjectList 	objlist;
	LPU32Vec 	sellist;
	if (Scene->GetQueryObjects(objlist, OBJCLASS_GROUP, 1, 1, -1)) {
		for (ObjectIt it = objlist.begin(); it != objlist.end(); it++) {
			LPCSTR N = ((CGroupObject*)*it)->RefName();
			ObjectIt _F = Scene->FirstObj(OBJCLASS_GROUP);
			ObjectIt _E = Scene->LastObj(OBJCLASS_GROUP);
			for (; _F != _E; _F++) {
				CGroupObject* _O = (CGroupObject*)(*_F);
				if ((*_F)->Visible() && _O->RefCompare(N)) {
					if (clear_prev) {
						_O->Select(false);
						sellist.push_back((u32*)_O);
					}
					else {
						if (!_O->Selected())
							sellist.push_back((u32*)_O);
					}
				}
			}
		}
		std::sort(sellist.begin(), sellist.end());
		sellist.erase(std::unique(sellist.begin(), sellist.end()), sellist.end());
		random_shuffle(sellist.begin(), sellist.end());
		int max_k = iFloor(float(sellist.size()) / 100.f * float(m_selPercent) + 0.5f);
		int k = 0;
		for (LPU32It o_it = sellist.begin(); k < max_k; o_it++, k++) {
			CGroupObject* _O = (CGroupObject*)(*o_it);
			_O->Select(true);
		}
	}
}

void UIGroupTool::SelByRefObject(bool flag)
{
	ObjectList objlist;
	
	if (m_Current.empty()) {
		LPCSTR N = m_Current.c_str();
		ObjectIt _F = Scene->FirstObj(OBJCLASS_GROUP);
		ObjectIt _E = Scene->LastObj(OBJCLASS_GROUP);
		for (; _F != _E; _F++) {
			if ((*_F)->Visible()) {
				CGroupObject* _O = (CGroupObject*)(*_F);
				if (_O->RefCompare(N)) _O->Select(flag);
			}
		}
	}
}
