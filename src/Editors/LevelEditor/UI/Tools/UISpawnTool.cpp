#include "stdafx.h"

UISpawnTool::UISpawnTool()
{
	m_selPercent = 100;
	m_Current = nullptr;
	m_SpawnList = new UIItemListForm();
	m_SpawnList->SetOnItemFocusedEvent({this, &UISpawnTool::OnItemFocused});
	RefreshList();
	m_AttachObject = false;
}

UISpawnTool::~UISpawnTool()
{
	xr_delete(m_SpawnList);
}

void UISpawnTool::Draw()
{
	const float ItemSpacingX = ImGui::GetStyle().ItemInnerSpacing.x;
	const float TableRowHeight = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		if (XRay::ImGui::BeginExpand("Reference Select"))
		{
			if (XRay::ImGui::BeginTable("##objecttools_refselect", 4, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
			{
												ImGui::TableSetupColumn("Key", ImGuiTableColumnFlags_WidthFixed);					ImGui::TableSetupColumn("-", ImGuiTableColumnFlags_WidthFixed);													ImGui::TableSetupColumn("--", ImGuiTableColumnFlags_WidthFixed);												ImGui::TableSetupColumn("--", ImGuiTableColumnFlags_WidthStretch);
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	XRay::ImGui::TextFramed("Select by Current: ");		XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button(" +", { GUIManager->ScaleByDpi(24.f), 0})) { SelByRefObject(true); }		XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button(" -", { GUIManager->ScaleByDpi(24.f), 0})) { SelByRefObject(false); }
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	XRay::ImGui::TextFramed("Select by Selected: ");	XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button("=%", { GUIManager->ScaleByDpi(24.f), 0})) { MultiSelByRefObject(true); }	XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button("+%", { GUIManager->ScaleByDpi(24.f), 0})) { MultiSelByRefObject(false); }	XRay::ImGui::TableNextColumn();		ImGui::SetNextItemWidth(-TableRowHeight); ImGui::DragFloat("%", &m_selPercent, 1, 0, 100, "%.1f");
				XRay::ImGui::EndTable();
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Commands"))
		{
			float size = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			if (XRay::ImGui::ToggleButton("Attach Object...", m_AttachObject, { size, 0 })) {
				if (m_AttachObject) ExecCommand(COMMAND_CHANGE_ACTION, etaAdd);
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (ImGui::Button("Detach Object", { size, 0 })) {
				ObjectList lst;
				if (Scene->GetQueryObjects(lst, OBJCLASS_SPAWNPOINT, 1, 1, 0)) {
					for (ObjectIt it = lst.begin(); it != lst.end(); it++) {
						CSpawnPoint* O = smart_cast<CSpawnPoint*>(*it); R_ASSERT(O);
						O->DetachObject();
					}
				}
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}

void UISpawnTool::DrawObjectsList()
{
	if (ImGui::Begin("Edit Group Items"))
	{
		m_SpawnList->Draw();
	}

	ImGui::End();
}

void UISpawnTool::SelByRefObject(bool flag)
{
	ObjectList objlist;
	const char* N = Current();
	if (N) {
		ObjectIt _F = Scene->FirstObj(OBJCLASS_SPAWNPOINT);
		ObjectIt _E = Scene->LastObj(OBJCLASS_SPAWNPOINT);
		for (; _F != _E; _F++) {
			if ((*_F)->Visible()) {
				CSpawnPoint* _O = (CSpawnPoint*)(*_F);
				if (_O->RefCompare(N)) _O->Select(flag);
			}
		}
	}
}
void UISpawnTool::MultiSelByRefObject(bool clear_prev)
{
	ObjectList 	objlist;
	LPU32Vec 	sellist;
	if (Scene->GetQueryObjects(objlist, OBJCLASS_SPAWNPOINT, 1, 1, -1)) {
		for (ObjectIt it = objlist.begin(); it != objlist.end(); it++) {
			const char* N = ((CSpawnPoint*)*it)->RefName();
			ObjectIt _F = Scene->FirstObj(OBJCLASS_SPAWNPOINT);
			ObjectIt _E = Scene->LastObj(OBJCLASS_SPAWNPOINT);
			for (; _F != _E; _F++) {
				CSpawnPoint* _O = (CSpawnPoint*)(*_F);
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
			CSpawnPoint* _O = (CSpawnPoint*)(*o_it);
			_O->Select(true);
		}
	}
}

void UISpawnTool::RefreshList()
{
	ListItemsVec items;
	LHelper().CreateItem(items, RPOINT_CHOOSE_NAME, 0, 0, 0);
	LHelper().CreateItem(items, ENVMOD_CHOOSE_NAME, 0, 0, 0);
	CInifile::Root& sections = pSettings->sections();
	for (CInifile::Sect& sect : sections)
	{
		shared_str& sect_name = sect.Name;
		const char* val;
		if (sect.line_exist("$spawn", &val))
		{
			shared_str caption = pSettings->r_string_wb(sect_name, "$spawn");
			if (caption.size())
			{
				ListItem* I = LHelper().CreateItem(items, caption.c_str(), 0, ListItem::flDrawThumbnail, (LPVOID)sect_name.c_str());
				//m_caption_to_sect[caption] = sect_name;
			}
		}
	}

	// Sort motions 
	std::sort(items.begin(), items.end(), [](ListItem* ItemA, ListItem* ItemB)
	{
		const std::string_view NameA = ItemA->Key();
		const std::string_view NameB = ItemB->Key();
		const size_t BLen = NameB.length();
		const size_t ALen = NameA.length();

		for (size_t Iter = 0; Iter < ALen; Iter++)
		{
			if (Iter >= BLen)
				return false;

			if (NameA[Iter] > NameB[Iter])
				return false;
			else if (NameA[Iter] < NameB[Iter])
				return true;
		}
	});

	m_SpawnList->AssignItems(items);
}

void UISpawnTool::OnItemFocused(ListItem* item)
{
	m_Current = nullptr;

	if (item)
	{
		if (strcmp(item->Key(), RPOINT_CHOOSE_NAME) == 0)
			m_Current = RPOINT_CHOOSE_NAME;
		else if (strcmp(item->Key(), ENVMOD_CHOOSE_NAME) == 0)
			m_Current = ENVMOD_CHOOSE_NAME;
		else
			m_Current = (const char*)item->m_Object;
	}
	ExecCommand(COMMAND_RENDER_FOCUS);
}
