#include "stdafx.h"

UIObjectListItem::UIObjectListItem(shared_str Name):UITreeItem(Name)
{
	bIsSelected = false;
	Object = nullptr;
}

UIObjectListItem::~UIObjectListItem()
{
}

float UIObjectListItem::Draw()
{
	float Result = -1;

	if (UIObjectList::Form->m_Filter[0]&& Object)
	{
		if (strstr(Object->GetName(), UIObjectList::Form->m_Filter) == 0)
			return Result;
	}
	switch (UIObjectList::Form->m_Mode)
	{
	case UIObjectList::M_All:
		break;
	case UIObjectList::M_Visible:
		if (!Object->Visible())
			return Result;
		break;
	case UIObjectList::M_Inbvisible:
		if (Object->Visible())
			return Result;
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

		static CCustomObject* LastFocusObject = nullptr;

		
		if (Object->Selected() && LastFocusObject != Object)
		{
			if (LastFocusObject != Object)
			{
				float ContaincePos = ImGui::GetCursorPosY();

				float BeginPos = ImGui::GetScrollY() + 30;
				float EndPos = ImGui::GetWindowSize().y + ImGui::GetScrollY();
				if (BeginPos < ContaincePos && ContaincePos > EndPos || BeginPos > ContaincePos)
				{
					Result = ContaincePos - 30;
					LastFocusObject = Object;
				}
			}
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
				if (UIObjectList::Form->m_LastSelected != nullptr && strstr(UIObjectList::Form->m_LastSelected->Object->GetName(), UIObjectList::Form->m_Filter) == 0)
				{
					bStart = false;
				}
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

	return Result;
}

void UIObjectListItem::DrawRoot()
{
	float ScrollPos = -1;
	size_t SelectedObjectsCounter = 0;

	//if (strlen(UIObjectList::Form->m_Filter) != 0)
	{
		for (UITreeItem* Item : Items)
		{
			UIObjectListItem* CastedItem = ((UIObjectListItem*)Item);

			float NewScrollPos = CastedItem->Draw();

			if (NewScrollPos != -1)
			{
				ScrollPos = NewScrollPos;
			}

			if (CastedItem->Object->Selected())
			{
				SelectedObjectsCounter++;
			}
		}
	}
	//else
	//{
	//	ImGuiListClipper clipper;
	//	clipper.Begin((int)Items.size());
	//
	//	while (clipper.Step())
	//	{
	//		for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; ++i)
	//		{
	//			UIObjectListItem* CastedItem = (UIObjectListItem*)Items[i];
	//
	//			float NewScrollPos = CastedItem->Draw();
	//
	//			if (NewScrollPos != -1)
	//			{
	//				ScrollPos = NewScrollPos;
	//			}
	//
	//			if (CastedItem->Object->Selected())
	//			{
	//				SelectedObjectsCounter++;
	//			}
	//		}
	//	}
	//}

	if (ScrollPos > 0 && SelectedObjectsCounter < 2)
	{
		ImGui::SetScrollY(ScrollPos);
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
	return new UIObjectListItem(Name);
}
