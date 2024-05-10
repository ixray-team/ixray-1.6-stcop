#include "stdafx.h"

UIObjectListItem::UIObjectListItem(shared_str Name):UITreeItem(Name)
{
	bIsSelected = false;
	Object = nullptr;
}

UIObjectListItem::~UIObjectListItem()
{
}

void UIObjectListItem::Draw()
{
	
	if (UIObjectList::Form->m_Filter[0]&& Object)
	{
		if (strstr(Object->GetName(), UIObjectList::Form->m_Filter) == 0)return;
	}
	switch (UIObjectList::Form->m_Mode)
	{
	case UIObjectList::M_All:
		break;
	case UIObjectList::M_Visible:
		if (!Object->Visible())return;
		break;
	case UIObjectList::M_Inbvisible:
		if (Object->Visible())return;
		break;
	default:
		break;
	}

	ImGui::TableNextRow();
	ImGui::TableNextColumn();


	ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;

	if (Object)
	{
		if (Object->Selected())
		{
			Flags |= ImGuiTreeNodeFlags_Bullet;
		}
		if (bIsSelected)
		{
			Flags |= ImGuiTreeNodeFlags_Selected;
		}
	}
	ImGui::TreeNodeEx(Name.c_str(), Flags);
	if (Object&&ImGui::IsItemClicked())
	{
		if (ImGui::GetIO().KeyShift)
		{
			bool bStart = UIObjectList::Form->m_LastSelected && UIObjectList::Form->m_LastSelected->Owner == Owner;
			if (UIObjectList::Form->m_Filter[0])
			{
				if (strstr(UIObjectList::Form->m_LastSelected->Object->GetName(), UIObjectList::Form->m_Filter) == 0)bStart = false;
			}
			if (bStart)
			{
				UIObjectList::Form->m_Root.ClearSelcted();
				Scene->SelectObjects(false, OBJCLASS_DUMMY);
				bIsSelected = true;
				{


					size_t StartIndex = 0;
					size_t EndIndex = 0;
					for (UITreeItem* Item : Owner->Items)
					{
						UIObjectListItem* RItem = (UIObjectListItem*)Item;
						if (RItem == UIObjectList::Form->m_LastSelected)
						{
							break;
						}
						StartIndex++;
					}
					for (UITreeItem* Item : Owner->Items)
					{
						EndIndex++;
						UIObjectListItem* RItem = (UIObjectListItem*)Item;
						if (RItem == this)
						{
							break;
						}
					}
					if (StartIndex >= EndIndex)
					{
						std::swap(StartIndex, EndIndex);
						StartIndex--;
						EndIndex++;
					}

					for (size_t i = StartIndex; i < EndIndex; i++)
					{
						UIObjectListItem* RItem = (UIObjectListItem*)Owner->Items[i];
						if (UIObjectList::Form->m_Filter[0])
						{
							if (strstr(RItem->Object->GetName(), UIObjectList::Form->m_Filter) == 0)continue;
						}
						RItem->Object->Select(true);
						RItem->bIsSelected = true;
					}
				}
				UIObjectList::Form->m_LastSelected = this;
			}

		}
		else if (ImGui::GetIO().KeyCtrl)
		{
			Object->Select(true);
			bIsSelected = true;
			UIObjectList::Form->m_LastSelected = this;
			
		}
		else
		{
			UIObjectList::Form->m_Root.ClearSelcted();
			Scene->SelectObjects(false, OBJCLASS_DUMMY);
			bIsSelected = true;
			Object->Select(true);
			UIObjectList::Form->m_LastSelected = this;
		}
	
	}
}

void UIObjectListItem::DrawRoot()
{
	for (UITreeItem* Item : Items)
	{
		((UIObjectListItem*)Item)->Draw();
	}
}

void UIObjectListItem::ClearSelcted(UIObjectListItem* Without)
{
	for (UITreeItem* Item : Items)
	{
		((UIObjectListItem*)Item)->bIsSelected = false;
		if(Without != (UIObjectListItem*)Item)
			((UIObjectListItem*)Item)->ClearSelcted();
	}
}


UITreeItem* UIObjectListItem::CreateItem(shared_str Name)
{
	return xr_new< UIObjectListItem>(Name);
}
