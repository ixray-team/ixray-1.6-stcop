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
			float	ItemSpacingX	= ImGui::GetStyle().ItemSpacing.x;
	const	float	TableRowHeight	= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);
	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Commands"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			if (XRay::ImGui::Button("Group", { SizeX, 0 })) ParentTools->GroupObjects();
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Ungroup", { SizeX, 0 })) ParentTools->UngroupObjects();
			ImGui::Separator();
			if (XRay::ImGui::Button("Make Thumbnail", { SizeX, 0 })) ParentTools->MakeThumbnail();
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Save As ...", { SizeX, 0 })) ParentTools->SaveSelectedObject();
			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Current Object"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			if (XRay::ImGui::Button("Select ...", { SizeX, 0 }))
			{
				string_path ObjectPath = {};

				FS.update_path(ObjectPath, _groups_, "");
				FS.rescan_path(ObjectPath, true);

				UIChooseForm::SelectItem(smGroup, 1, m_Current.c_str());
				m_ChooseGroup = true;
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Reload Refs", { SizeX, 0 }))
			{
				ParentTools->ReloadRefsSelectedObject();
				//bForceInitListBox = TRUE;
				Tools->UpdateProperties(TRUE);
			}
			XRay::ImGui::TextFramed("Current: %s", { -0.01, 0 }, { 0, 0.5f}, true, m_Current.c_str() ? m_Current.c_str() : "");

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Reference Select"))
		{
			if (XRay::ImGui::BeginTable("##objecttools_refselect", 4, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
			{
												ImGui::TableSetupColumn("Key", ImGuiTableColumnFlags_WidthFixed);					ImGui::TableSetupColumn("-", ImGuiTableColumnFlags_WidthFixed);														ImGui::TableSetupColumn("--", ImGuiTableColumnFlags_WidthFixed);													ImGui::TableSetupColumn("--", ImGuiTableColumnFlags_WidthStretch);
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	XRay::ImGui::TextFramed("Select by Current: ");		XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button(" +", { GUIManager->ScaleByDpi(24.f), 0})) { SelByRefObject(true); }		XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button(" -", { GUIManager->ScaleByDpi(24.f), 0})) { SelByRefObject(false); }
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	XRay::ImGui::TextFramed("Select by Selected: ");	XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button("=%", { GUIManager->ScaleByDpi(24.f), 0})) { MultiSelByRefObject(true); }	XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button("+%", { GUIManager->ScaleByDpi(24.f), 0})) { MultiSelByRefObject(false); }	XRay::ImGui::TableNextColumn();		ImGui::SetNextItemWidth(-TableRowHeight); ImGui::DragFloat("%", &m_selPercent, 1, 0, 100, "%.1f");
				XRay::ImGui::EndTable();
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Pivot Alignment"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			{
				if (XRay::ImGui::Button("Center To Group", { SizeX, 0 })) { ParentTools->CenterToGroup(); }
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Align To Object...", { SizeX, 0 })) { ParentTools->AlignToObject(); }
			}
			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
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
