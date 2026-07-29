#include "StdAfx.h"
#include "UIInventoryInvalidation.h"

#include "UICellItem.h"
#include "UIDragDropListEx.h"

namespace UIInventoryInvalidation
{
	void PrepareCellForDestroy(CUICellItem* cell)
	{
		if (cell == nullptr)
		{
			return;
		}

		if (CUICellItem::m_mouse_selected_item == cell)
		{
			CUICellItem::m_mouse_selected_item = nullptr;
		}

		const u32 childCount = cell->ChildsCount();
		for (u32 i = 0; i < childCount; ++i)
		{
			PrepareCellForDestroy(cell->Child(i));
		}

		// Container owns cell widgets; never AutoDelete. Clear inventory binding so
		// ~CUICellItem does not touch a possibly already-destroyed PIItem
		cell->m_b_destroy_childs = false;
		cell->m_pData = nullptr;
		cell->SetOwnerList(nullptr);
	}

	void DestroyCell(CUICellItem*& cell)
	{
		if (cell == nullptr)
		{
			return;
		}

		PrepareCellForDestroy(cell);
		xr_delete(cell);
	}

	void EndDragSession()
	{
		CUIDragDropListEx::EndDragSession();
	}

	void BeginListContentReset(CUIDragDropListEx& list)
	{
		list.DestroyDragItem();
		list.BumpContentGeneration();
		list.ClearSelectedItem();

		if (list.m_f_content_reset)
		{
			list.m_f_content_reset(&list);
		}
	}
}
