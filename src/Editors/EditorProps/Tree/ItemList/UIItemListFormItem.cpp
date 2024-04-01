#include "stdafx.h"

UIItemListFormItem::UIItemListFormItem(shared_str Name) :Object(nullptr), UITreeItem(Name)
{
	bIsFavorite = false;
	m_bIsMixed = false;
	bSelected = false;
	Index = -1;
}

UIItemListFormItem::~UIItemListFormItem()
{
}

void UIItemListFormItem::Draw()
{
	ImGui::PushID(this);
	ImGui::TableNextRow();
	ImGui::TableNextColumn();
	if (Object)
	{
		ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
		if (bSelected)
		{
			Flags |= ImGuiTreeNodeFlags_Bullet;
		}
		if (Form->m_SelectedItem == this)
		{
			Flags |= ImGuiTreeNodeFlags_Selected;
		}
		if (Form->m_Flags.test(cfMultiSelect))
		{
			if (ImGui::Checkbox("##checkbox", &bIsFavorite))
			{
				if (bSelected)
				{
					Form->m_RootItem.SelectedToFavorite(bIsFavorite);
				}
				else
				{
					Form->m_RootItem.ClearSelection();
					Form->m_SelectedItem = this;
					bSelected = true;
				}
				Form->m_RootItem.CheckFavorited();
			}
			ImGui::SameLine(0, 0);
		}
		ImGui::TreeNodeEx(Text.c_str(), Flags);
		if (ImGui::IsItemClicked())
		{
			if (Form->m_Flags.test(cfMultiSelect))
			{
				if (ImGui::GetIO().KeyShift)
				{
					if (Form->m_SelectedItem)
					{
						Form->m_RootItem.Selected(Index, Form->m_SelectedItem->Index);
					}
					bSelected = true;
				}
				else if (ImGui::GetIO().KeyCtrl)
				{
					bSelected = true;
					Form->UpdateSelected(this);
				}
				else
				{
					Form->m_RootItem.ClearSelection();
					bSelected = true;
					Form->UpdateSelected(this);
				}
			}
			else
			{
				Form->m_RootItem.ClearSelection();
				bSelected = true;
				Form->UpdateSelected(this);
			}
		}
	}
	else
	{
		ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_OpenOnArrow;
		if (Form->m_Flags.test(cfMultiSelect))
		{
			ImGui::PushItemFlag(ImGuiItemFlags_MixedValue, m_bIsMixed);
			bool CheckChange = ImGui::Checkbox("##checkbox", &bIsFavorite);
			ImGui::PopItemFlag();
			if (CheckChange)
			{
				Form->m_RootItem.ClearSelection();
				Form->m_SelectedItem = nullptr;
				bSelected = false;
				SetFavorite(bIsFavorite);
				Form->m_RootItem.CheckFavorited();
			}
			ImGui::SameLine(0, 0);
		}
		if (ImGui::TreeNodeEx(Name.c_str(), Flags))
		{
			for (UITreeItem* Item : Items)
			{
				((UIItemListFormItem*)Item)->Draw();
			}
			ImGui::TreePop();
		}
	}

	ImGui::PopID();



}

void UIItemListFormItem::DrawRoot()
{
	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->Draw();
	}
}

void UIItemListFormItem::Sort()
{
	for (UITreeItem* Item : Items)
	{
		std::sort(Items.begin(), Items.end(), [](UITreeItem* Right, UITreeItem* Left)->bool
			{
				UIItemListFormItem* pRight = ((UIItemListFormItem*)Right);
				UIItemListFormItem* pLeft = ((UIItemListFormItem*)Left);
				if (pRight->Object && !pLeft->Object)
				{
					return false;
				}
				if (!pRight->Object && pLeft->Object)
				{
					return true;
				}
				return xr_strcmp(pRight->Name.c_str(), pLeft->Name.c_str()) < 0;
			});
	}


	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->Sort();
	}
}

void UIItemListFormItem::CheckFavorited()
{
	if (Items.size() == 0)return;
	if (!Form->m_Flags.test(cfMultiSelect))
	{
		return;
	}
	m_bIsMixed = false;
	bool bIsLastFavorited = ((UIItemListFormItem*)Items.front())->bIsFavorite;
	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->CheckFavorited();
		m_bIsMixed |= ((UIItemListFormItem*)Item)->m_bIsMixed;
		if (bIsLastFavorited != ((UIItemListFormItem*)Item)->bIsFavorite)
		{

			bIsLastFavorited = false;
			m_bIsMixed = true;
		}
	}
	bIsFavorite = bIsLastFavorited;
}

void UIItemListFormItem::SetFavorite(bool bFavorited)
{
	if (!Form->m_Flags.test(cfMultiSelect))
	{
		return;
	}
	bIsFavorite = bFavorited;
	m_bIsMixed = false;
	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->SetFavorite(bFavorited);
	}
}

void UIItemListFormItem::FillFavorited(xr_vector<ListItem*>& Favorited)
{
	if (!Form->m_Flags.test(cfMultiSelect))
	{
		return;
	}
	if (Object && bIsFavorite)
	{
		Favorited.push_back(Object);
	}
	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->FillFavorited(Favorited);
	}
}

void UIItemListFormItem::CheckFavorited(xr_vector<ListItem*>& Favorited)
{
	if (!Form->m_Flags.test(cfMultiSelect))
	{
		return;
	}
	if (Object)
	{
		bIsFavorite = std::find_if(Favorited.begin(), Favorited.end(), [&](ListItem* I) {return Object == I; }) != Favorited.end();
	}
	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->CheckFavorited(Favorited);
	}
}

void UIItemListFormItem::ClearSelection()
{
	bSelected = false;
	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->ClearSelection();
	}
}

void UIItemListFormItem::Selected(int Start, int End)
{

	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->Selected(Start, End);
	}
	if (!Object)
	{
		return;
	}

	if (Start > End)
		std::swap(Start, End);
	if (Index >= Start && Index <= End)
		bSelected = true;
	else
		bSelected = false;
}

void UIItemListFormItem::SelectedToFavorite(bool Favorite)
{
	if (!Form->m_Flags.test(cfMultiSelect))
	{
		return;
	}
	for (UITreeItem* Item : Items)
	{
		((UIItemListFormItem*)Item)->SelectedToFavorite(Favorite);
	}
	if (bSelected)
	{
		SetFavorite(Favorite);
	}
}

UITreeItem* UIItemListFormItem::CreateItem(shared_str Name)
{
	auto Item = xr_new<UIItemListFormItem>(Name);
	Item->Form = Form;
	return Item;
}
