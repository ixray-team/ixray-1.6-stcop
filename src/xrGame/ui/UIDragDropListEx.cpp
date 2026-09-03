#include "StdAfx.h"
#include "UIDragDropListEx.h"
#include "../../xrUI/Widgets/UIScrollBar.h"
#include "object_broker.h"
#include "UICellItem.h"
#include "UIInventoryInvalidation.h"
#include "../../xrUI/UICursor.h"
#include "../Inventory.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrEngine/xr_input.h"

CUIDragItem* CUIDragDropListEx::m_drag_item = nullptr;

namespace
{
// Horizontal UV span for one cell when inventory grid is disabled (must match GetTexUVLT sliding room).
constexpr float kInventoryCellUSpanGridDisabled = 0.23f;

xr_vector<CUIDragItem*> s_pendingDragItemDestroy;

void QueuePendingDragItemDestroy(CUIDragItem* dragItem)
{
	if (dragItem == nullptr)
	{
		return;
	}

	dragItem->UnregisterDeviceSequences();
	s_pendingDragItemDestroy.push_back(dragItem);
}
}

void CUIDragDropListEx::FlushPendingDragItemDestroy()
{
	if (s_pendingDragItemDestroy.empty())
	{
		return;
	}

	xr_vector<CUIDragItem*> pending;
	pending.swap(s_pendingDragItemDestroy);

	for (CUIDragItem* item : pending)
	{
		xr_delete(item);
	}
}

void CUIDragDropListEx::BumpContentGeneration()
{
	++m_contentGeneration;
	if (m_contentGeneration == 0)
	{
		m_contentGeneration = 1;
	}
}

void CUIDragDropListEx::EndDragSession()
{
	if (m_drag_item == nullptr)
	{
		FlushPendingDragItemDestroy();
		return;
	}

	CUICellItem* parentItem = m_drag_item->ParentItem();
	CUIDragDropListEx* owner = parentItem != nullptr ? parentItem->OwnerList() : nullptr;
	if (owner == nullptr)
	{
		owner = m_drag_item->BackList();
	}

	if (owner != nullptr)
	{
		owner->DestroyDragItem();
	}
	else
	{
		CUIDragItem* dragItem = m_drag_item;
		m_drag_item = nullptr;
		QueuePendingDragItemDestroy(dragItem);
	}

	FlushPendingDragItemDestroy();
}

void CUICell::Clear()
{
	m_bMainItem = false;
	if(m_item)	m_item->SetOwnerList(nullptr);
	m_item		= nullptr; 
}

CUIDragDropListEx::CUIDragDropListEx()
{
	m_flags.zero				();
	m_container					= new CUICellContainer(this);
	m_vScrollBar				= new CUIScrollBar();
	m_vScrollBar->SetAutoDelete	(true);
	m_selected_item				= nullptr;
	m_bConditionProgBarVisible	= false;

	m_selectorFrame				= new CUIFrameWindow();
	m_selectorFrame->SetVisible(false);
	AttachChild					(m_selectorFrame);

	SetCellSize					(Ivector2().set(50,50));
	SetCellsCapacity			(Ivector2().set(0,0));

	AttachChild					(m_container);
	AttachChild					(m_vScrollBar);

	m_vScrollBar->SetWindowName	("scroll_v");
	Register					(m_vScrollBar);
	AddCallbackStr				("scroll_v",	SCROLLBAR_VSCROLL,				CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnScrollV)		);
	AddCallbackStr				("cell_item",	DRAG_DROP_ITEM_DRAG,			CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemStartDragging)	);
	AddCallbackStr				("cell_item",	DRAG_DROP_ITEM_DROP,			CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemDrop)			);
	AddCallbackStr				("cell_item",	DRAG_DROP_ITEM_SELECTED,		CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemSelected)			);
	AddCallbackStr				("cell_item",	DRAG_DROP_ITEM_LBUTTON_CLICK,	CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemLButtonClick)			);
	AddCallbackStr				("cell_item",	DRAG_DROP_ITEM_RBUTTON_CLICK,	CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemRButtonClick)			);
	AddCallbackStr				("cell_item",	DRAG_DROP_ITEM_DB_CLICK,		CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemDBClick)			);
	AddCallbackStr				("cell_item",	DRAG_DROP_ITEM_FOCUSED_UPDATE,	CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemFocusedUpdate)			);
	AddCallbackStr				("cell_item",	WINDOW_FOCUS_RECEIVED,			CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemFocusReceived)			);
	AddCallbackStr				("cell_item",	WINDOW_FOCUS_LOST,				CUIWndCallback::void_function		(this, &CUIDragDropListEx::OnItemFocusLost)			);

	back_color = 0xFFFFFFFF;
}

CUIDragDropListEx::~CUIDragDropListEx()
{
	DestroyDragItem();
	FlushPendingDragItemDestroy();

	delete_data					(m_container);

	xr_delete					(m_selectorFrame);
}

void CUIDragDropListEx::SetAutoGrow(bool b)						
{
	m_flags.set(flAutoGrow,b);
}
bool CUIDragDropListEx::IsAutoGrow()								
{
	return !!m_flags.test(flAutoGrow);
}
void CUIDragDropListEx::SetGrouping(bool b)						
{
	m_flags.set(flGroupSimilar,b);
}
bool CUIDragDropListEx::IsGrouping()
{
	return !!m_flags.test(flGroupSimilar);
}
void CUIDragDropListEx::SetCustomPlacement(bool b)
{
	m_flags.set(flCustomPlacement,b);
}

bool CUIDragDropListEx::GetCustomPlacement()
{
	return !!m_flags.test(flCustomPlacement);
}
void CUIDragDropListEx::SetVerticalPlacement(bool b)
{
	m_flags.set(flVerticalPlacement,b);
}

void CUIDragDropListEx::SetAlwaysShowScroll(bool b)
{
	m_flags.set(flAlwaysShowScroll,b);
}

bool CUIDragDropListEx::GetVerticalPlacement()
{
	return !!m_flags.test(flVerticalPlacement);
}

void CUIDragDropListEx::SetVirtualCells(bool b)
{
	m_flags.set(flVirtualCells,b);
}

bool CUIDragDropListEx::GetVirtualCells()
{
	return !!m_flags.test(flVirtualCells);
}

void CUIDragDropListEx::UpdateSelector()
{
	if (!HasCells())
	{
		return;
	}

	m_container->ValidateSelector();
	m_selected_item = m_container->GetCellAt(m_container->GetSelectorArea().lt).m_item;

	// Check if selector is visible
	const Irect& selectorArea = m_container->GetSelectorArea();
	const int selAreaW = selectorArea.width();
	const int selAreaH = selectorArea.height();
	const Ivector2& cellSize = CellSize();
	const Ivector2& cellSpacing = CellsSpacing();
	const Ivector2& capacity = m_container->CellsCapacity();

	Frect wndRect = { 0, 0, GetWndSize().x, GetWndSize().y };
	wndRect.grow(1, 1);

	Frect selectorBare;
	selectorBare.x1 = selectorArea.x1 * (cellSize.x + cellSpacing.x);
	selectorBare.y1 = selectorArea.y1 * (cellSize.y + cellSpacing.y);
	selectorBare.x2 = selectorBare.x1 + selAreaW * cellSize.x + (selAreaW - 1) * cellSpacing.x;
	selectorBare.y2 = selectorBare.y1 + selAreaH * cellSize.y + (selAreaH - 1) * cellSpacing.y;

	Frect selector = selectorBare;
	selector.add(m_container->GetWndPos().x, m_container->GetWndPos().y);

	// If not fully visible, need to autoscroll
	if (!(wndRect.in(selector.lt) && wndRect.in(selector.rb)))
	{
		if (selector.y1 > 0) // Scroll down
			m_vScrollBar->SetScrollPos(selectorBare.y2 - GetWndSize().y); 
		else
			m_vScrollBar->SetScrollPos(selectorBare.y1); // Scroll up

		m_container->SetWndPos(Fvector2().set(m_container->GetWndPos().x, float(-m_vScrollBar->GetScrollPos())));
	}

	// Update frame size and position
	m_selectorFrame->SetWidth(selectorBare.width());
	m_selectorFrame->SetHeight(selectorBare.height());

	Fvector2 cellPos = { 
		(float)selectorArea.x1 * (float)cellSize.x + (float)selectorArea.x1 * (float)cellSpacing.x, 
		selector.y1 
	};
	m_selectorFrame->SetWndPos(cellPos);

}

void CUIDragDropListEx::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUIDragDropListEx::InitDragDropList(Fvector2 pos, Fvector2 size)
{
	inherited::SetWndPos				(pos);
	inherited::SetWndSize				(size);
	m_vScrollBar->InitScrollBar			(Fvector2().set(size.x, 0.0f), size.y, false);
	m_vScrollBar->SetWndPos				(Fvector2().set(m_vScrollBar->GetWndPos().x - m_vScrollBar->GetWidth(), m_vScrollBar->GetWndPos().y));
}

void CUIDragDropListEx::OnScrollV(CUIWindow* w, void* pData)
{
	m_container->SetWndPos		(Fvector2().set(m_container->GetWndPos().x, float(-m_vScrollBar->GetScrollPos())));
}

void CUIDragDropListEx::CreateDragItem(CUICellItem* itm)
{
	FlushPendingDragItemDestroy();
	R_ASSERT							(!m_drag_item);
	m_drag_item							= itm->CreateDragItem();

	if ( m_drag_item )
	{
		GetParent()->SetCapture			(m_drag_item, true);
	}
}

void CUIDragDropListEx::DestroyDragItem()
{
	CUIDragItem* dragItem = m_drag_item;
	if (dragItem == nullptr)
	{
		return;
	}

	m_drag_item = nullptr;

	if (CUIWindow* parent = GetParent())
	{
		if (parent->GetMouseCapturer() == dragItem)
		{
			parent->SetCapture(nullptr, false);
		}
	}

	QueuePendingDragItemDestroy(dragItem);
}

Fvector2 CUIDragDropListEx::GetDragItemPosition()
{
	return m_drag_item->GetPosition();
}

void CUIDragDropListEx::OnDragEvent(CUIDragItem* drag_item, bool b_receive)
{
	if (m_f_drag_event)
	{
		m_f_drag_event(drag_item, b_receive);
	}
}

void CUIDragDropListEx::OnItemStartDragging(CUIWindow* w, void* pData)
{
	OnItemSelected(w, pData);
	CUICellItem* itm = w->ui_cast_cell_item();

	if (itm != m_selected_item)
	{
		return;
	}

	if (m_f_item_start_drag && m_f_item_start_drag(itm))
	{
		return;
	}

	CreateDragItem(itm);
}

void CUIDragDropListEx::OnItemDrop(CUIWindow* w, void* pData)
{
	OnItemSelected(w, pData);
	CUICellItem* itm = w->ui_cast_cell_item();
	VERIFY(itm->OwnerList() == itm->OwnerList());

	if (m_f_item_drop && m_f_item_drop(itm))
	{
		DestroyDragItem();
		return;
	}

	CUIDragDropListEx* old_owner = itm->OwnerList();
	CUIDragDropListEx* new_owner = m_drag_item->BackList();

	bool b = (old_owner == new_owner) && !GetCustomPlacement();

	if (old_owner && new_owner && !b)
	{
		CUICellItem* i = old_owner->RemoveItem(itm, (old_owner == new_owner));
		while (i->ChildsCount())
		{
			CUICellItem* _chld = i->PopChild(nullptr);
			new_owner->SetItem(_chld, old_owner->GetDragItemPosition());
		}

		new_owner->SetItem(i, old_owner->GetDragItemPosition());
	}

	DestroyDragItem();
}

void CUIDragDropListEx::OnItemDBClick(CUIWindow* w, void* pData)
{
	OnItemSelected(w, pData);
	CUICellItem* itm = w->ui_cast_cell_item();

	if (m_f_item_db_click && m_f_item_db_click(itm))
	{
		DestroyDragItem();
		return;
	}

	CUIDragDropListEx* old_owner = itm->OwnerList();
	VERIFY(m_drag_item == nullptr);
	VERIFY(old_owner == this);

	if (old_owner && old_owner->GetCustomPlacement())
	{
		CUICellItem* i = old_owner->RemoveItem(itm, true);
		old_owner->SetItem(i);
	}

	DestroyDragItem();
}

void CUIDragDropListEx::OnItemSelected(CUIWindow* w, void* pData)
{
	m_selected_item = w->ui_cast_cell_item();
	VERIFY(m_selected_item);

	if (m_f_item_selected)
	{
		m_f_item_selected(m_selected_item);
	}
}

void  CUIDragDropListEx::OnItemFocusReceived(CUIWindow* w, void* pData)
{
	if (m_f_item_focus_received)
	{
		CUICellItem* itm = w->ui_cast_cell_item();
		m_f_item_focus_received(itm);
	}
}

void  CUIDragDropListEx::OnItemFocusLost(CUIWindow* w, void* pData)
{
	if (m_f_item_focus_lost)
	{
		CUICellItem* itm = w->ui_cast_cell_item();
		m_f_item_focus_lost(itm);
	}
}

void  CUIDragDropListEx::OnItemFocusedUpdate(CUIWindow* w, void* pData)
{
	if (m_f_item_focused_update)
	{
		CUICellItem* itm = w->ui_cast_cell_item();
		m_f_item_focused_update(itm);
	}
}

void CUIDragDropListEx::OnItemRButtonClick(CUIWindow* w, void* pData)
{
	CUICellItem* itm = w->ui_cast_cell_item();
	if (m_f_item_rbutton_click)
	{
		m_f_item_rbutton_click(itm);
	}
}

void CUIDragDropListEx::OnItemLButtonClick(CUIWindow* w, void* pData)
{
	CUICellItem* itm = w->ui_cast_cell_item();
	if (m_f_item_lbutton_click)
	{
		m_f_item_lbutton_click(itm);
	}
}

void CUIDragDropListEx::GetClientArea(Frect& r)
{
	GetAbsoluteRect				(r);
	if(m_vScrollBar->GetVisible() || m_flags.test(flAlwaysShowScroll))
		r.x2 -= m_vScrollBar->GetWidth	();
}

// FFx0001
void CUIDragDropListEx::ClearAll(bool bDestroy, xr_vector<u16> IgnoredItemsIds)
{
	if (bDestroy)
	{
		UIInventoryInvalidation::BeginListContentReset(*this);
	}
	else
	{
		// Compact / soft clear: keep cell widgets and menu selection unless drag is active
		DestroyDragItem();
		ClearSelectedItem();
	}

	FlushPendingDragItemDestroy();
	m_container->ClearAll	(bDestroy, IgnoredItemsIds); // FFx0001
	m_selected_item			= nullptr;

	// IgnoredItemsIds survivors keep widgets; restamp owner token after generation bump
	const u32 count = ItemsCount();
	for (u32 i = 0; i < count; ++i)
	{
		CUICellItem* itm = GetItemIdx(i);
		itm->SetOwnerList(this);
		const u32 childCount = itm->ChildsCount();
		for (u32 j = 0; j < childCount; ++j)
		{
			itm->Child(j)->SetOwnerList(this);
		}
	}

	m_container->SetWndPos	(Fvector2().set(0,0));
	if (count == 0)
	{
		ResetCellsCapacity();
	}
}

void CUIDragDropListEx::Compact()
{
	xrCriticalSectionGuard guard(m_container->csUi);
	CUIWindow::WINDOW_LIST& wl = m_container->GetChildWndList();
	ClearAll(false);

	for (CUIWindow* child : wl)
	{
		CUICellItem* itm = child->ui_cast_cell_item();
		SetItem(itm);
	}
}

void CUIDragDropListEx::Draw()
{
	FlushPendingDragItemDestroy();
	inherited::Draw				();

	if(0 && bDebug){
		CGameFont* F		= UI().Font().pFontDI;
		F->SetAligment		(CGameFont::alCenter);
		F->SetHeight		(0.02f);
		F->OutSetI			(0.f,-0.5f);
		F->SetColor			(0xffffffff);
		Ivector2			pt = m_container->PickCell(GetUICursor().GetCursorPosition());
		F->OutNext			("%d-%d",pt.x, pt.y);
	};

}

void CUIDragDropListEx::Update()
{
	FlushPendingDragItemDestroy();
	inherited::Update			();

	if( m_drag_item ){
		Frect	wndRect;
		GetAbsoluteRect(wndRect);
		Fvector2 cp			= GetUICursor().GetCursorPosition();
		if(wndRect.in(cp)){
			if(nullptr==m_drag_item->BackList())
				m_drag_item->SetBackList(this);
		}else
			if( this==m_drag_item->BackList() )
				m_drag_item->SetBackList(nullptr);
	}
	m_selectorFrame->SetVisible(m_selector_shown && pInput->GetControllerMode());
}

void CUIDragDropListEx::ReinitScroll()
{
		float h1 = m_container->GetWndSize().y;
		float h2 = GetWndSize().y;
		VERIFY						(_valid(h1));
		VERIFY						(_valid(h2));
		float dh = h1-h2;
		m_vScrollBar->Show			( (dh > 0) || m_flags.test(flAlwaysShowScroll) );
		m_vScrollBar->Enable		( (dh > 0) || m_flags.test(flAlwaysShowScroll) );

		if ( dh < 0 )
		{
//			dh = 0;
			m_vScrollBar->SetRange	(0, 0);
		}
		else
		{
			m_vScrollBar->SetRange	(0, iFloor(dh));
		}
		m_vScrollBar->SetScrollPos	(0);
		m_vScrollBar->SetStepSize	(CellSize().y/3);
		m_vScrollBar->SetPageSize	( 1/*CellSize().y*/ );
		m_vScrollBar->SetWndSize({ m_vScrollBar->GetWndSize().x, h2 });
		m_container->SetWndPos		(Fvector2().set(0,0));
}

bool CUIDragDropListEx::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	bool b = inherited::OnMouseAction		(x,y,mouse_action);

	if(m_vScrollBar->IsShown())
	{
		switch(mouse_action){
		case WINDOW_MOUSE_WHEEL_DOWN:
			for( u8 i = 0; i < 4; ++i )		{	m_vScrollBar->TryScrollInc();	}
			return true;
			break;

		case WINDOW_MOUSE_WHEEL_UP:
			for( u8 i = 0; i < 4; ++i )		{	m_vScrollBar->TryScrollDec();	}
			return true;
			break;
		}
	}
	return b;
}

bool CUIDragDropListEx::HasCells() const
{
	return m_container->HasCells();
}

const Ivector2& CUIDragDropListEx::CellsCapacity()
{
	return m_container->CellsCapacity();
}

void CUIDragDropListEx::SetCellsCapacity(const Ivector2 c)
{
	m_container->SetCellsCapacity(c);

	// Autohide selector if we are empty now
	if (!m_container->HasCells())
	{
		m_selector_shown = false;
	}
}

const Ivector2& CUIDragDropListEx::CellSize()
{
	return m_container->CellSize();
}
const Ivector2& CUIDragDropListEx::CellsSpacing()
{
	return m_container->CellsSpacing();
}

void CUIDragDropListEx::SetCellSize(const Ivector2 new_sz)			
{
	m_container->SetCellSize(new_sz);
}

void CUIDragDropListEx::SetCellsSpacing(const Ivector2& new_sz)
{
	m_container->SetCellsSpacing(new_sz);
}

int CUIDragDropListEx::ScrollPos()
{
	return m_vScrollBar->GetScrollPos();
}

void CUIDragDropListEx::SetItem(CUICellItem* itm) //auto
{
	if(m_container->AddSimilar(itm)){
		return;
	}

	Ivector2 dest_cell_pos =	m_container->FindFreeCell(itm->GetGridSize());

	SetItem						(itm,dest_cell_pos);
}

bool CUIDragDropListEx::SetItem(CUICellItem* itm, Fvector2 abs_pos) // start at cursor pos
{
	if (m_container->AddSimilar(itm))
		return true;

	const Ivector2 dest_cell_pos = m_container->PickCell(abs_pos);

	if (m_container->ValidCell(dest_cell_pos) && m_container->IsRoomFree(dest_cell_pos, itm->GetGridSize()))
		SetItem(itm, dest_cell_pos);
	else
		SetItem(itm);

	return true;
}

void CUIDragDropListEx::SetItem(CUICellItem* itm, Ivector2 cell_pos) // start at cell
{
	if(m_container->AddSimilar(itm))	return;
	R_ASSERT						(m_container->IsRoomFree(cell_pos, itm->GetGridSize()));

	m_container->PlaceItemAtPos	(itm, cell_pos);

    const PIItem iitem = static_cast<PIItem>(itm->m_pData);
    itm->SetScaleFactor(iitem->m_3d_static_scale);
    Fvector fRot = iitem->m_3d_static_rotate;
    if (GetVerticalPlacement())
        fRot.x += deg2rad(90.f);
    itm->SetXYZ(fRot);

    itm->SetWindowName			("cell_item");
	Register					(itm);
	itm->SetOwnerList			(this);
}
bool CUIDragDropListEx::CanSetItem(CUICellItem* itm){
	if (m_container->HasFreeSpace(itm->GetGridSize()))
		return true;
	Compact();

	return m_container->HasFreeSpace(itm->GetGridSize());
}

CUICellItem* CUIDragDropListEx::RemoveItem(CUICellItem* itm, bool force_root)
{
	if (pInput->GetControllerMode() && itm == m_selected_item && itm->ChildsCount() == 0)
		DeselectSelected();

	CUICellItem* i				= m_container->RemoveItem		(itm, force_root);
	i->SetOwnerList				((CUIDragDropListEx*)nullptr);
	return						i;
}

u32 CUIDragDropListEx::ItemsCount()
{
	xrCriticalSectionGuard guard(m_container->csUi);
	return (u32)m_container->GetChildWndList().size();
}

bool CUIDragDropListEx::IsOwner(CUICellItem* itm){
	return m_container->IsChild(itm);
}

CUICellItem* CUIDragDropListEx::GetItemIdx(u32 idx)
{
	R_ASSERT(idx < ItemsCount());

	xrCriticalSectionGuard guard(m_container->csUi);
	WINDOW_LIST_it it = m_container->GetChildWndList().begin();
	std::advance(it, idx);

	return (*it)->ui_cast_cell_item();
}

void CUIDragDropListEx::clear_select_armament()
{
	m_container->clear_select_armament();
}
void CUIDragDropListEx::SetCellsVertAlignment(xr_string alignment)
{
	if(strchr(alignment.c_str(), 't'))
	{
		m_virtual_cells_alignment.y = 0;
		return;
	}
	if(strchr(alignment.c_str(), 'b'))
	{
		m_virtual_cells_alignment.y = 2;
		return;
	}
	m_virtual_cells_alignment.y = 1;
}
void CUIDragDropListEx::SetCellsHorizAlignment(xr_string alignment)
{
	if(strchr(alignment.c_str(), 'l'))
	{
		m_virtual_cells_alignment.x = 0;
		return;
	}
	if(strchr(alignment.c_str(), 'r'))
	{
		m_virtual_cells_alignment.x = 2;
		return;
	}
	m_virtual_cells_alignment.x = 1;
}

Ivector2 CUIDragDropListEx::PickCell(const Fvector2& abs_pos) 
{
	return m_container->PickCell(abs_pos);
};

CUICell& CUIDragDropListEx::GetCellAt(const Ivector2& pos) 
{
	return m_container->GetCellAt(pos);
};

bool CUIDragDropListEx::MoveSelector(eUIDirection4 dir)
{
	R_ASSERT(HasCells());

	bool bResult = m_container->MoveSelector(dir);
	UpdateSelector();
	return bResult;
}

bool CUIDragDropListEx::MoveSelectorToItem(CUICellItem* pItem)
{
	R_ASSERT(HasCells());

	if (!pItem || !m_container->IsChild(pItem))
		return false;

	Irect newSelector;
	const Ivector2& itemSize = pItem->GetGridSize();
	newSelector.lt = m_container->GetItemPos(pItem);
	newSelector.x2 = newSelector.x1 + itemSize.x;
	newSelector.y2 = newSelector.y1 + itemSize.y;

	m_container->TrySetSelector(newSelector);
	return true;
}

void CUIDragDropListEx::InitSelector(const char* texture_name)
{
	m_selectorFrame->InitTexture(texture_name, false);
}

void CUIDragDropListEx::SetControllerFocusIn(Irect selector)
{
	if (m_container->HasCells())
	{
		m_container->TrySetSelector(selector);
		UpdateSelector();
		m_selector_shown = true;
	}
	else
	{
		m_selector_shown = false;
	}
}
void CUIDragDropListEx::SetControllerFocusOut()
{
	DeselectSelected();
	m_selector_shown = false;
}


void CUIDragDropListEx::DeselectSelected()
{
	if (m_selected_item)
	{
		if (m_f_item_focus_lost)
			m_f_item_focus_lost(m_selected_item);
		m_selected_item = nullptr;
	}
}

CUICellItem* CUIDragDropListEx::GetItemAtPos(Fvector2 abs_pos)
{
	Ivector2 cell = PickCell(abs_pos);
	if (m_container->ValidCell(cell))
	{
		return m_container->GetCellAt(cell).m_item;
	}
	return nullptr;
}

// =================================================================================================

CUICellContainer::CUICellContainer(CUIDragDropListEx* parent)
	: m_pParentDragDropList(parent)
	, m_isInventoryGridDisabled(EngineExternal()[EEngineExternalUI::DisableInventoryGrid])
{
	if (m_isInventoryGridDisabled)
	{
		hShader->create("hud\\fog_of_war", "ui\\ui_grid_alt");
	}
	else
	{
		hShader->create("hud\\fog_of_war", "ui\\ui_grid");
	}
//	hShader_selected->create	( "hud\\fog_of_war", "ui_grid_selected" );
	m_cellSpacing.set			( 0, 0 );

	m_selectorArea.left = 0;
	m_selectorArea.top = 0;
	m_selectorArea.right = 1;
	m_selectorArea.bottom = 1;
}

CUICellContainer::~CUICellContainer()
{
}

bool CUICellContainer::AddSimilar(CUICellItem* itm)
{
	if (!m_pParentDragDropList->IsGrouping())
		return false;

	const PIItem iitem = static_cast<PIItem>(itm->m_pData);
	if (iitem && iitem->m_pInventory && iitem->m_pInventory->ItemFromSlot(iitem->BaseSlot()) == iitem)
	{
		const bool allowSlottedGrenadeStack =
			(iitem->BaseSlot() == GRENADE_SLOT) && (iitem->cast_grenade() != nullptr);
		if (!allowSlottedGrenadeStack)
			return false;
	}

	if (!iitem->CanStack())
		return false;

	CUICellItem* i = FindSimilar(itm);
	if (i == nullptr || i == itm || itm->ChildsCount() > 0)
		return false;

	const PIItem iitem_parent = static_cast<PIItem>(i->m_pData);
	if (iitem && iitem->BaseSlot() != GRENADE_SLOT && iitem->CurrSlot() != iitem_parent->CurrSlot())
	{
		return false;
	}

	i->PushChild(itm);
	itm->SetOwnerList(m_pParentDragDropList);

	return true;
}

CUICellItem* CUICellContainer::FindSimilar(CUICellItem* itm)
{
	xrCriticalSectionGuard guard(csUi);
	for (CUIWindow* child : m_ChildWndList)
	{
#ifdef DEBUG
		CUICellItem* i = child->ui_cast_cell_item();
#else
		CUICellItem* i = (CUICellItem*)(child);
#endif
		R_ASSERT(i != itm);
		if (i->EqualTo(itm))
		{
			return i;
		}
	}

	return nullptr;
}

void CUICellContainer::PlaceItemAtPos(CUICellItem* itm, Ivector2& cell_pos)
{
	Ivector2 cs				= itm->GetGridSize();
	if(m_pParentDragDropList->GetVerticalPlacement())
		std::swap(cs.x,cs.y);

	for(int x=0; x<cs.x; ++x)
	{
		for(int y=0; y<cs.y; ++y)
		{
			CUICell& C		= GetCellAt(Ivector2().set(x,y).add(cell_pos));
			C.SetItem		(itm,(x==0&&y==0));
		}
	}
	// without this will be double compression on wide screens
	// solution works only for "quads" cells
	if (m_pParentDragDropList->GetVerticalPlacement())
	{
		itm->SetWndSize            ( Fvector2().set( (m_cellSize.y*cs.x),        (m_cellSize.y*cs.y)         )    );
	}
	else
	{
		itm->SetWndSize            ( Fvector2().set( (m_cellSize.x*cs.x),        (m_cellSize.y*cs.y)         )    );    
	}

	if (!m_pParentDragDropList->GetVirtualCells()) {
		// FX: (Отступ + размер) * позиция грида... Логично
		Fvector2 ValidItemPos = { float((m_cellSpacing.x + m_cellSize.x) * cell_pos.x), float(((m_cellSpacing.y + m_cellSize.y) * cell_pos.y)) };
		itm->SetWndPos(ValidItemPos);
	}
	else
	{
		Fvector2 AlignPos = m_pParentDragDropList->GetWndSize();;

		// FX: We get the coordinates from the center of the window, taking into account the size of the item
		AlignPos.sub(Fvector2().set(m_cellSize.x * cs.x,m_cellSize.y * cs.y));
		AlignPos.div(2);

		itm->SetWndPos(AlignPos);
	}


	AttachChild				(itm);
	itm->OnAfterChild		(m_pParentDragDropList);
}

CUICellItem* CUICellContainer::RemoveItem(CUICellItem* itm, bool force_root)
{
	{
		xrCriticalSectionGuard guard(csUi);
		for (WINDOW_LIST_it it = m_ChildWndList.begin(); m_ChildWndList.end() != it; ++it)
		{
			CUICellItem* i = (CUICellItem*)(*it);

			if (i->HasChild(itm))
			{
				CUICellItem* iii = i->PopChild(itm);
				R_ASSERT(0 == iii->ChildsCount());
				return				iii;
			}
		}
	}

	if(!force_root && itm->ChildsCount())
	{
		CUICellItem* iii	=	itm->PopChild(nullptr);
		R_ASSERT			(0==iii->ChildsCount());
		return				iii;
	}

	Ivector2 pos			= GetItemPos(itm);
	Ivector2 cs				= itm->GetGridSize();

	if(m_pParentDragDropList->GetVerticalPlacement())
		std::swap(cs.x,cs.y);

	for(int x=0; x<cs.x;++x)
		for(int y=0; y<cs.y;++y)
		{
			CUICell& C		= GetCellAt(Ivector2().set(x,y).add(pos));
			C.Clear			();
		}

	itm->SetOwnerList		(nullptr);
	DetachChild				(itm);
	return					itm;
}

Ivector2 CUICellContainer::FindFreeCell	(const Ivector2& _size)
{
	Ivector2 tmp;
	Ivector2 size = _size;

	if(m_pParentDragDropList->GetVerticalPlacement())
		std::swap(size.x, size.y);

	for(tmp.y=0; tmp.y<=m_cellsCapacity.y-size.y; ++tmp.y )
		for(tmp.x=0; tmp.x<=m_cellsCapacity.x-size.x; ++tmp.x )
			if(IsRoomFree(tmp,_size))
				return  tmp;

	if(m_pParentDragDropList->IsAutoGrow())
	{
		Grow	();
		return							FindFreeCell	(size);
	}else{
		m_pParentDragDropList->Compact		();
		for(tmp.y=0; tmp.y<=m_cellsCapacity.y-size.y; ++tmp.y )
			for(tmp.x=0; tmp.x<=m_cellsCapacity.x-size.x; ++tmp.x )
				if(IsRoomFree(tmp,_size))
					return  tmp;

		R_ASSERT2		(0,"there are no free room to place item");
	}
	return			tmp;
}

bool CUICellContainer::HasFreeSpace		(const Ivector2& _size)
{
	Ivector2 tmp;
	Ivector2 size = _size;

	if(m_pParentDragDropList->GetVerticalPlacement())
		std::swap(size.x, size.y);

	for(tmp.y=0; tmp.y<=m_cellsCapacity.y-size.y; ++tmp.y )
		for(tmp.x=0; tmp.x<=m_cellsCapacity.x-size.x; ++tmp.x )
			if(IsRoomFree(tmp,_size))
				return true;

	return false;
}

bool CUICellContainer::IsRoomFree(const Ivector2& pos, const Ivector2& _size)
{
	Ivector2 tmp;

	Ivector2 size = _size;
	if(m_pParentDragDropList->GetVerticalPlacement())
		std::swap(size.x, size.y);

	for(tmp.x =pos.x; tmp.x<pos.x+size.x; ++tmp.x)
		for(tmp.y =pos.y; tmp.y<pos.y+size.y; ++tmp.y)
		{
			if(!ValidCell(tmp))		return		false;

			CUICell& C				= GetCellAt(tmp);

			if(!C.Empty())			return		false;
		}
	return true;
}

void CUICellContainer::GetTexUVLT(Fvector2& uv, u32 col, u32 row, u8 select_mode)
{
	float sliceStart = 0.0f;
	switch (select_mode)
	{
	case 0:
		sliceStart = 0.00f;
		break;
	case 1:
		sliceStart = 0.25f;
		break;
	case 2:
		sliceStart = 0.50f;
		break;
	case 3:
		sliceStart = 0.75f;
		break;
	default:
		sliceStart = 0.00f;
		break;
	}

	if (m_isInventoryGridDisabled)
	{
		// Slide the sampling window within each 0.25 select strip so repeating vertical atlas detail does not line up across cells.
		const u32 mix = col * 0x9E3779B9u + row * 0x85EBCA6Bu;
		const float t = float(mix % 1024u) / 1023.0f;
		const float slideRoom = 0.25f - kInventoryCellUSpanGridDisabled;
		uv.set(sliceStart + t * slideRoom, 0.0f);
	}
	else
	{
		uv.set(sliceStart, 0.0f);
	}
}


void CUICellContainer::SetCellsCapacity(const Ivector2& c)
{
	R_ASSERT(c.x >= 0 && c.y >= 0);
	m_cellsCapacity				= c;
	m_cells.resize				(c.x*c.y);
	ReinitSize					();
}

void CUICellContainer::SetCellSize(const Ivector2& new_sz)
{
	m_cellSize					= new_sz;
	ReinitSize					();
}

void CUICellContainer::SetCellsSpacing(const Ivector2& c)
{
	m_cellSpacing				= c;
	ReinitSize					();
}

Ivector2 CUICellContainer::TopVisibleCell()
{
	return Ivector2().set	(0, iFloor(m_pParentDragDropList->ScrollPos()/float(CellSize().y+m_cellSpacing.y)));
}

CUICell& CUICellContainer::GetCellAt(const Ivector2& pos)
{
	R_ASSERT			(ValidCell(pos));
	CUICell&	c		= m_cells[m_cellsCapacity.x*pos.y+pos.x];
	return				c;
}

Ivector2 CUICellContainer::GetItemPos(CUICellItem* itm)
{
	for(int x=0; x<m_cellsCapacity.x ;++x)
		for(int y=0; y<m_cellsCapacity.y ;++y){
			Ivector2 p;
			p.set(x,y);
		if(GetCellAt(p).m_item==itm)
			return p;
		}

		R_ASSERT(0);
		return Ivector2().set(-1,-1);
}

u32 CUICellContainer::GetCellsInRange(const Irect& rect, UI_CELLS_VEC& res)
{
	res.resize(0);
	for(int x=rect.x1;x<=rect.x2;++x)
		for(int y=rect.y1;y<=rect.y2;++y)
			res.push_back	(GetCellAt(Ivector2().set(x,y)));

	res.erase(std::unique(res.begin(), res.end()), res.end());
	return (u32)res.size();
}

void CUICellContainer::ReinitSize()
{
	Ivector2							sz;
	sz.add								(CellsSpacing(), CellSize());
	sz.mul								(CellsCapacity());
	sz.sub								(CellsSpacing());

	SetWndSize							(Fvector2().set(sz.x,sz.y));
	m_pParentDragDropList->ReinitScroll	();

	if (HasCells())
	{
		ResetSelector();
	}
}

void CUICellContainer::Grow()
{
	SetCellsCapacity	(Ivector2().set(m_cellsCapacity.x,m_cellsCapacity.y+1));
}

void CUICellContainer::Shrink()
{
}

bool CUICellContainer::ValidCell(const Ivector2& pos) const
{
	return !(pos.x<0 || pos.y<0 || pos.x>=m_cellsCapacity.x || pos.y>=m_cellsCapacity.y);
}

// FFx0001 add support ignore items by ids
void CUICellContainer::ClearAll(bool bDestroy, xr_vector<u16> IgnoredItemsIds)
{
	m_selectorArea = { 0,0,1,1 };

	bool DeepSearch = false;
	size_t cnt = IgnoredItemsIds.size();

	if (!IgnoredItemsIds.empty())
	{
		DeepSearch = true;
	}

	for (CUICell& cell : m_cells)
	{
		bool IsIgnored = false;
		if (DeepSearch)
		{
			if (CUICellItem* ci = cell.m_item)
			{
				if (PIItem item = (PIItem)(ci->m_pData))
				{
					u16 ItemId = item->object_id();
					for (size_t i = 0; i < cnt; i++)
					{
						if (IgnoredItemsIds[i] == ItemId)
						{
							IsIgnored = true;
							break;
						}
					}
				}
			}
		}

		if (!IsIgnored)
		{
			cell.Clear();
		}
	}

	xrCriticalSectionGuard guard(csUi);

	auto it = m_ChildWndList.rbegin();

	while (it != m_ChildWndList.rend())
	{
		CUIWindow* w = *it;
		CUICellItem* wc = w != nullptr ? w->ui_cast_cell_item() : nullptr;
		VERIFY(!wc->IsAutoDelete());
		
		if (!wc) {
			++it;
			continue;
		}

		bool IsIgnored = false;
		if (DeepSearch) {
			u16 ItemId = ((PIItem)(wc->m_pData))->object_id();
			for (size_t i = 0; i < cnt; i++)
			{
				if (IgnoredItemsIds[i] == ItemId)
				{
					IsIgnored = true;
					break;
				}
			}
		}

		if (!IsIgnored) {
			DetachChild(wc);

			while (wc->ChildsCount())
			{
				CUICellItem* ci = wc->PopChild(nullptr);
				R_ASSERT(ci->ChildsCount() == 0);

				if (bDestroy)
				{
					UIInventoryInvalidation::PrepareCellForDestroy(ci);
					delete_data(ci);
				}
			}

			if (bDestroy)
			{
				UIInventoryInvalidation::PrepareCellForDestroy(wc);
				delete_data(wc);
			}
		}

		++it;
	}
}

Ivector2 CUICellContainer::PickCell(const Fvector2& abs_pos)
{
	Ivector2 res;
	Fvector2 ap;
	GetAbsolutePos	(ap);
	ap.sub			(abs_pos);
	ap.mul			(-1);
	res.x			= iFloor(ap.x/(m_cellSize.x+m_cellSpacing.x*(m_cellsCapacity.x-1)/m_cellsCapacity.x));
	res.y			= iFloor(ap.y/(m_cellSize.y+m_cellSpacing.y*(m_cellsCapacity.y-1)/m_cellsCapacity.y));
	if(!ValidCell(res))		
		res.set(-1, -1);
	return res;
}


void CUICellContainer::Draw()
{
	Frect clientArea;
	m_pParentDragDropList->GetClientArea(clientArea);

	Ivector2			cell_cnt = m_pParentDragDropList->CellsCapacity();
	if					(cell_cnt.x==0 || cell_cnt.y==0)	return;

	Ivector2			cell_sz = CellSize();
	cell_sz.add			(m_cellSpacing);

	Irect				tgt_cells;
	tgt_cells.lt		= TopVisibleCell();
	tgt_cells.x2		= iFloor( (float(clientArea.width())+float(cell_sz.x)-EPS)/float(cell_sz.x)) + tgt_cells.lt.x;
	tgt_cells.y2		= iFloor( (float(clientArea.height())+float(cell_sz.y)-EPS)/float(cell_sz.y)) + tgt_cells.lt.y;

	clamp				(tgt_cells.x2, 0, cell_cnt.x-1);
	clamp				(tgt_cells.y2, 0, cell_cnt.y-1);

	Fvector2			lt_abs_pos;
	GetAbsolutePos		(lt_abs_pos);

	Fvector2					drawLT;
	drawLT.set					(lt_abs_pos.x+tgt_cells.lt.x*(cell_sz.x+m_cellSpacing.x), lt_abs_pos.y+tgt_cells.lt.y*(cell_sz.y+m_cellSpacing.y));
	UI().ClientToScreenScaled	(drawLT, drawLT.x, drawLT.y);

	const Fvector2 pts[6] =		{{0.0f,0.0f},{1.0f,0.0f},{1.0f,1.0f},
								 {0.0f,0.0f},{1.0f,1.0f},{0.0f,1.0f}};
	const float texUSpan = m_isInventoryGridDisabled ? kInventoryCellUSpanGridDisabled : 0.25f;
	const float texVSpan = 1.0f;
	const Fvector2 uvs[6] =		{{0.0f,0.0f},{texUSpan,0.0f},{texUSpan,texVSpan},
								 {0.0f,0.0f},{texUSpan,texVSpan},{0.0f,texVSpan}};

	// calculate cell size in screen pixels
	Fvector2 f_len, sp_len;
	UI().ClientToScreenScaled(f_len, float(CellSize().x), float(CellSize().y) );
	UI().ClientToScreenScaled(sp_len, float(CellsSpacing().x), float(CellsSpacing().y) );

	GetCellsInRange(tgt_cells,m_cells_to_draw);

	// fill cell buffer
	u32 max_prim_cnt = ((tgt_cells.width()+1)*(tgt_cells.height()+1)*6);
	UIRender->StartPrimitive	(max_prim_cnt, IUIRender::ptTriList, UI().m_currentPointType);

//	u32 cell_i = 0;
	for ( int x = 0; x <= tgt_cells.width(); ++x )
	{
		for ( int y = 0; y <= tgt_cells.height(); ++y/*, ++cell_i*/ )
		{
			Fvector2			rect_offset;
			rect_offset.set		( (drawLT.x + f_len.x*x + sp_len.x*x), (drawLT.y + f_len.y*y + sp_len.y*y) );

			Ivector2 cpos;
			cpos.set( x, y );
			cpos.add( TopVisibleCell() );
			CUICell& ui_cell = GetCellAt( cpos );
			
			u8 select_mode = 0;
			if ( !ui_cell.Empty() )
			{
				if ( ui_cell.m_item->m_cur_mark )
				{
					select_mode = 2;
				}
				else if ( ui_cell.m_item->m_selected )
				{
					select_mode = 1;
				}
				else if ( ui_cell.m_item->m_select_armament )
				{
					select_mode = 3;
				}
				else if (ui_cell.m_item->m_select_equipped)
				{
					select_mode = 2;
				}
			}
			
			Fvector2			tp;
			GetTexUVLT			(tp, tgt_cells.x1+x, tgt_cells.y1+y, select_mode);

			//for (u32 k=0; k<6; ++k,++pv)
			for ( u32 k = 0; k < 6; ++k )
			{
				const Fvector2& p	= pts[k];
				const Fvector2& uv	= uvs[k];
				//pv->set			(iFloor(drawLT.x + p.x*(f_len.x) + f_len.x*x)-0.5f, 
				//				 iFloor(drawLT.y + p.y*(f_len.y) + f_len.y*y)-0.5f, 
				//				 0xFFFFFFFF,tp.x+uv.x,tp.y+uv.y);
				UIRender->PushPoint(iFloor( rect_offset.x + p.x*(f_len.x) )-0.5f, 
									iFloor( rect_offset.y + p.y*(f_len.y) )-0.5f,
									0,
									m_pParentDragDropList->back_color,
									tp.x+uv.x, tp.y+uv.y);
			}//for k
		}//for y
	}// for x
	UI().PushScissor					(clientArea);

	UIRender->SetShader( *hShader );
	UIRender->FlushPrimitive();

	//draw shown items in range
	if ( m_cells_to_draw.size() )
	{
		UI_CELLS_VEC_IT it = m_cells_to_draw.begin();
		for ( ; it != m_cells_to_draw.end(); ++it ) // all cells
		{
			CUICell& cell = (*it);
			if ( !cell.Empty() && (cell.m_item->m_drawn_frame != Device.dwFrame) )
			{
				cell.m_item->Draw();
			}
		}
	}

	UI().PopScissor			();
}

void CUICellContainer::clear_select_armament()
{
	UI_CELLS_VEC_IT itb = m_cells.begin();
	UI_CELLS_VEC_IT ite = m_cells.end();
	for ( ; itb != ite; ++itb )
	{
		CUICell& cell = (*itb);
		if ( cell.m_item )
		{
			cell.m_item->m_select_armament = false;
		}
	}
}

void CUICellContainer::ResetSelector()
{
	R_ASSERT(m_cells.size() > 0 && m_cellsCapacity.x > 0 && m_cellsCapacity.y > 0);

	CUICell& ui_cell = GetCellAt({ 0, 0 });
	if (!ui_cell.m_item)
	{
		m_selectorArea = {0,0,1,1};
	}
	else
	{
		const Ivector2 itemSize = ui_cell.m_item->GetGridSize();
		m_selectorArea = { 0,0,itemSize.x,itemSize.y };
	}
}


void CUICellContainer::TrySetSelector(const Irect& selector)
{
	R_ASSERT(m_cells.size() > 0 && m_cellsCapacity.x > 0 && m_cellsCapacity.y > 0);

	m_selectorArea = selector;
	ValidateSelector();
}

// Return true if selector has been moved
bool CUICellContainer::MoveSelector(eUIDirection4 dir)
{
	R_ASSERT(m_cells.size() > 0 && m_cellsCapacity.x > 0 && m_cellsCapacity.y > 0);

	Ivector2 selectorPos = m_selectorArea.lt;
	CUICell& ui_cell = GetCellAt(selectorPos);

	int newX = m_selectorArea.x1;
	int newY = m_selectorArea.y1;

	if (m_selectorArea.width() > 1 && dir == eUIDirection4_Right)
		newX += m_selectorArea.width() - 1;

	if (m_selectorArea.height() > 1 && dir == eUIDirection4_Down)
		newY += m_selectorArea.height() - 1;

	switch (dir)
	{
	case eUIDirection4_Down: 
		newY += 1;
		break;
	case eUIDirection4_Up:
		newY += -1;
		break;
	case eUIDirection4_Left:
		newX += -1;
		break;
	case eUIDirection4_Right:
		newX += 1;
		break;
	}

	if (newX < 0)
		newX = 0;
	else if (newX >= m_cellsCapacity.x)
		newX = m_cellsCapacity.x - 1;

	if (newY < 0)
		newY = 0;
	else if (newY >= m_cellsCapacity.y)
		newY = m_cellsCapacity.y - 1;


	CUICell& ui_cell_new = GetCellAt({ newX, newY });
	if (ui_cell.m_item == ui_cell_new.m_item && ui_cell.m_item != NULL)
		return false;

	Irect oldSelectorArea = m_selectorArea;

	if (ui_cell_new.Empty())
	{
		m_selectorArea.x1 = newX;
		m_selectorArea.y1 = newY;
		m_selectorArea.x2 = m_selectorArea.x1 + 1;
		m_selectorArea.y2 = m_selectorArea.y1 + 1;
	}
	else
	{
		// Check object in this cell and how much cells it occupies
		m_selectorArea.lt = GetItemPos(ui_cell_new.m_item);
		const Ivector2 itemSize = ui_cell_new.m_item->GetGridSize();
		m_selectorArea.x2 = m_selectorArea.x1 + itemSize.x;
		m_selectorArea.y2 = m_selectorArea.y1 + itemSize.y;
	}

	return !oldSelectorArea.cmp(m_selectorArea);
}

void CUICellContainer::ValidateSelector()
{
	// Check selector is still inside of the bounds (width, height)
	if (m_selectorArea.x1 >= m_cellsCapacity.x)
		m_selectorArea.x1 = m_cellsCapacity.x-1;

	if (m_selectorArea.y1 >= m_cellsCapacity.y)
		m_selectorArea.y1 = m_cellsCapacity.y-1;

	if (m_selectorArea.x2 > m_cellsCapacity.x)
		m_selectorArea.x2 = m_cellsCapacity.x;

	if (m_selectorArea.y2 > m_cellsCapacity.y)
		m_selectorArea.y2 = m_cellsCapacity.y;

	R_ASSERT(m_selectorArea.valide());

	// Check that all cells have the same item in them
	bool bAllTheSame = true;
	Ivector2 pos = { m_selectorArea.x1, m_selectorArea.y1 };
	CUICellItem* pFirstCellItem = GetCellAt(pos).m_item;
	for (; pos.x < m_selectorArea.x2 && bAllTheSame; ++pos.x)
	{
		for (; pos.y < m_selectorArea.y2; ++pos.y)
		{
			R_ASSERT(ValidCell(pos));
			CUICell& c = m_cells[m_cellsCapacity.x * pos.y + pos.x];
			if (c.m_item != pFirstCellItem)
			{
				bAllTheSame = false;
				break;
			}
		}
	}

	if (!bAllTheSame)
	{
		if (pFirstCellItem == nullptr)
		{
			m_selectorArea = { m_selectorArea.x1, m_selectorArea.y1, m_selectorArea.x1 + 1, m_selectorArea.y1 + 1 };
		}
		else
		{
			m_selectorArea.lt = GetItemPos(pFirstCellItem);
			const Ivector2 itemSize = pFirstCellItem->GetGridSize();
			m_selectorArea.x2 = m_selectorArea.x1 + itemSize.x;
			m_selectorArea.y2 = m_selectorArea.y1 + itemSize.y;
		}
	}
	else
	{
		if (pFirstCellItem)
		{
			// Wrap selector around this object
			Ivector2 itemPos = GetItemPos(pFirstCellItem);
			Ivector2 itemSize = pFirstCellItem->GetGridSize();
			m_selectorArea.lt = itemPos;
			m_selectorArea.x2 = m_selectorArea.x1 + itemSize.x;
			m_selectorArea.y2 = m_selectorArea.y1 + itemSize.y;
		}
		else
		{
			// All the same but empty - shrink to 1 cell
			// this might be a feature (dont shrink)
			if (m_selectorArea.width() > 1 || m_selectorArea.height() > 1)
				m_selectorArea = { m_selectorArea.x1, m_selectorArea.y1, m_selectorArea.x1 + 1, m_selectorArea.y1 + 1 };
		}
	}
}

#undef ty
#undef tx