#include "stdafx.h"


UIItemListForm::UIItemListForm():m_RootItem("")
{
	m_Flags.zero();
}

UIItemListForm::~UIItemListForm()
{
	ClearList();
}

void UIItemListForm::Draw()
{

	static ImGuiTableFlags flags = ImGuiTableFlags_BordersV | ImGuiTableFlags_BordersH | ImGuiTableFlags_RowBg | ImGuiTableFlags_NoBordersInBody | ImGuiTableFlags_ScrollY | ImGuiTableFlags_ScrollX | ImGuiTableFlags_SizingFixedFit;

	if (ImGui::BeginTable("objects", 1, flags))
	{
		ImGui::TableSetupColumn("Name", ImGuiTableColumnFlags_WidthFixed);
		ImGui::TableHeadersRow();

		m_RootItem.DrawRoot();
		ImGui::EndTable();
	}
}

void UIItemListForm::AssignItems(ListItemsVec& items, const char* name_selection , bool clear_floder, bool save_selected)
{
	ClearList();
	int Index = 0;
	for (auto& i : items)
	{
		xr_string Name = i->key.c_str();
	
		xr_string RealName;
		if (strrchr(Name.c_str(), '\\'))
		{
			RealName = strrchr(Name.c_str(), '\\') + 1;
		}
		else
		{
			RealName = Name.c_str();
		}	
		Name.append("*");
		auto Item = (UIItemListFormItem*)m_RootItem.AppendItem(Name.c_str());
		Item->Object = i;
		Item->Text = RealName.c_str();
		Item->Index = Index++;
	}
	m_RootItem.Sort();
}
void UIItemListForm::ClearList()
{

	for (ListItem* item : m_Items)
	{
		xr_delete(item);
	}
	m_Items.clear();
	m_RootItem = UIItemListFormItem("");
	m_RootItem.Form = this;
}

void UIItemListForm::SelectItem(const char* name)
{
	xr_string Name = name;
	Name.append("*");
	UIItemListFormItem *Item = (UIItemListFormItem*)m_RootItem.FindItem(Name.c_str());
	if (Item)
	{

		m_RootItem.ClearSelection();
		Item->bSelected = true;
		m_SelectedItem = Item;
	}

}
void UIItemListForm::ClearSelected()
{
	m_SelectedItem = nullptr;
}
bool UIItemListForm::GetSelected(RStringVec& items) const
{
	if (m_SelectedItem)
	{
		items.push_back(m_SelectedItem->Object->Key());
		return true;
	}
	return false;
}

void UIItemListForm::UpdateSelected(UIItemListFormItem* NewSelected)
{
	m_SelectedItem = NewSelected;
	if (m_SelectedItem)
		OnItemFocusedEvent(m_SelectedItem->Object);
	else
		OnItemFocusedEvent(nullptr);
}
