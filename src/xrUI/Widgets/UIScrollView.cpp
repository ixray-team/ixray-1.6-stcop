#include "stdafx.h"
#include "UIScrollView.h"
#include "UIScrollBar.h"
#include "../ui_base.h"
#include "../UICursor.h"
#include "../../xrEngine/xr_input.h"	

CUIScrollView::CUIScrollView()
{
	m_rightIndent		= 0.0f;
	m_leftIndent		= 0.0f;
	m_vertInterval		= 0.0f;
   	m_upIndent			= 0.0f;
	m_downIndent		= 0.0f;
	m_flags.zero		();
	SetFixedScrollBar	(true);
	m_pad				= nullptr;
	m_VScrollBar		= nullptr;
	m_visible_rgn.set	(-1,-1);
}
CUIScrollView::CUIScrollView(CUIScrollBar* scroll_bar)
{
	m_rightIndent		= 0.0f;
	m_leftIndent		= 0.0f;
	m_vertInterval		= 0.0f;
   	m_upIndent			= 0.0f;
	m_downIndent		= 0.0f;
	m_flags.zero		();
	SetFixedScrollBar	(true);
	m_pad				= nullptr;
	m_visible_rgn.set	(-1,-1);

	m_VScrollBar = scroll_bar;
	m_VScrollBar->SetAutoDelete(true);
	AttachChild(m_VScrollBar);
	Register(m_VScrollBar);
	AddCallback(m_VScrollBar,	SCROLLBAR_VSCROLL, CUIWndCallback::void_function(this, &CUIScrollView::OnScrollV));
}

CUIScrollView::~CUIScrollView()
{
	Clear	();
}

void CUIScrollView::SendMessage	(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
	if (CHILD_CHANGED_SIZE == msg && m_pad->IsChild(pWnd))
		m_flags.set			(eNeedRecalc,true);
}

void CUIScrollView::ForceUpdate()
{
	m_flags.set			(eNeedRecalc,true);
	RecalcSize			();
}

bool CUIScrollView::_createOrInitScrollBar()
{
	const Fvector2 scrollPos = Fvector2().set(GetWndSize().x, 0.0f);
	const char* profile = m_scrollbar_profile.size() ? *m_scrollbar_profile : nullptr;

	if (m_VScrollBar && m_VScrollBar->IsInitialized())
	{
		return true;
	}

	if (!m_VScrollBar)
	{
		m_VScrollBar = new CUIScrollBar();
		m_VScrollBar->SetAutoDelete(true);
		AttachChild(m_VScrollBar);
		Register(m_VScrollBar);
		AddCallback(m_VScrollBar, SCROLLBAR_VSCROLL, CUIWndCallback::void_function(this, &CUIScrollView::OnScrollV));
	}

	if (!CUIScrollBar::InitForProfile(*m_VScrollBar, scrollPos, GetWndSize().y, false, profile))
	{
		DetachChild(m_VScrollBar);
		m_VScrollBar = nullptr;
		return false;
	}

	return true;
}

void CUIScrollView::_applyScrollBarLayout()
{
	if (!m_VScrollBar)
	{
		return;
	}

	const Fvector2 scPos = { m_VScrollBar->GetWndPos().x - m_VScrollBar->GetWndSize().x, m_VScrollBar->GetWndPos().y };
	m_VScrollBar->SetWndPos(scPos);
	m_VScrollBar->SetWindowName("scroll_v");
	m_VScrollBar->SetStepSize(std::max(1, iFloor(GetHeight() / 10)));
	m_VScrollBar->SetPageSize(iFloor(GetHeight()));
}

void CUIScrollView::InitScrollView()
{
	if (!m_pad)
	{
		m_pad = new CUIWindow();
		m_pad->SetAutoDelete(true);
		AttachChild(m_pad);
	}
	m_pad->SetWndPos(Fvector2().set(0, 0));

	if (!_createOrInitScrollBar())
	{
		return;
	}

	_applyScrollBarLayout();
}

void CUIScrollView::SetScrollBarProfile(const char* profile)
{
	m_scrollbar_profile = profile;
}

bool CUIScrollView::ReinitScrollBar()
{
	if (!m_pad || GetWndSize().y <= 0.0f)
	{
		return false;
	}

	int scrollPos = 0;
	if (m_VScrollBar)
	{
		scrollPos = m_VScrollBar->GetScrollPos();
	}
	else
	{
		scrollPos = iFloor(-m_pad->GetWndPos().y);
	}

	if (m_VScrollBar)
	{
		DetachChild(m_VScrollBar);
		m_VScrollBar = nullptr;
	}

	if (!_createOrInitScrollBar())
	{
		return false;
	}

	_applyScrollBarLayout();
	_applyScrollPos(scrollPos);
	UpdateScroll();

	return true;
}

void CUIScrollView::AddWindow			(CUIWindow* pWnd, bool auto_delete)
{
	if(auto_delete)		pWnd->SetAutoDelete	(true);

	m_pad->AttachChild	(pWnd);
	m_flags.set			(eNeedRecalc,true);
}

bool CUIScrollView::HasWindow		(CUIWindow* pWnd)
{
	return m_pad->IsChild(pWnd);
}

void CUIScrollView::RemoveWindow		(CUIWindow* pWnd)
{
	m_pad->DetachChild	(pWnd);
	m_flags.set			(eNeedRecalc,true);
}

void CUIScrollView::Clear				()
{
	m_pad->DetachAll	();
	m_flags.set			(eNeedRecalc,true);
	ScrollToBegin		();
}

bool CUIScrollView::TryClear()
{
	// InitScrollView() creates m_pad; if XML init failed, treat as non-clearable (not "empty").
	if (!m_pad)
	{
		return false;
	}

	if (!m_pad->csUi.TryEnter())
	{
		return false;
	}

	while (!m_pad->GetChildWndList().empty())
	{
		m_pad->DetachChild(m_pad->GetChildWndList().back());
	}
	m_pad->csUi.Leave();

	m_flags.set(eNeedRecalc, true);
	ScrollToBegin();
	return true;
}

Fvector2  CUIScrollView::GetPadSize()									
{
	if(m_flags.test	(eNeedRecalc) )
		RecalcSize			();

	return m_pad->GetWndSize();
}

void CUIScrollView::Update				()
{
	if(m_flags.test	(eNeedRecalc) )
		RecalcSize			();

	inherited::Update();
}

void CUIScrollView::RecalcSize			()
{
	if(!m_pad)			return;
	Fvector2			pad_size;
	pad_size.set		(0.0f, 0.0f);

	Fvector2			item_pos;
	item_pos.set		(m_rightIndent, m_vertInterval + m_upIndent);
	pad_size.y			+= m_upIndent;
	pad_size.y			+= m_downIndent;

	xrCriticalSectionGuard guard(m_pad->csUi);
	if(m_sort_function)
	{
		//. m_pad->GetChildWndList().sort(m_sort_function);
		std::sort(m_pad->GetChildWndList().begin(), m_pad->GetChildWndList().end(), m_sort_function);
	}

	if(GetVertFlip()){
		for(WINDOW_LIST::reverse_iterator it = m_pad->GetChildWndList().rbegin(); m_pad->GetChildWndList().rend() != it; ++it)
		{
			(*it)->SetWndPos		(item_pos);
			item_pos.y				+= (*it)->GetWndSize().y;
			item_pos.y				+= m_vertInterval; 
			pad_size.y				+= (*it)->GetWndSize().y;
			pad_size.y				+= m_vertInterval;
			pad_size.x				= std::max(pad_size.x, (*it)->GetWndSize().x);
		}

	}else{
		for(WINDOW_LIST_it it = m_pad->GetChildWndList().begin(); m_pad->GetChildWndList().end() != it; ++it)
		{
			(*it)->SetWndPos		(item_pos);
			item_pos.y				+= (*it)->GetWndSize().y;
			item_pos.y				+= m_vertInterval; 
			pad_size.y				+= (*it)->GetWndSize().y;
			pad_size.y				+= m_vertInterval;
			pad_size.x				= std::max(pad_size.x, (*it)->GetWndSize().x);
		}
	};

	m_pad->SetWndSize			(pad_size);


	if(m_flags.test(eInverseDir) )
		m_pad->SetWndPos		(Fvector2().set(m_pad->GetWndPos().x, GetHeight()-m_pad->GetHeight()));

	UpdateScroll				();

	m_flags.set					(eNeedRecalc,false);
	m_visible_rgn.set			(-1,-1);
}

void CUIScrollView::_applyScrollPos(int scrollPos)
{
	if (!m_pad)
	{
		return;
	}
	Fvector2 wPos = m_pad->GetWndPos();
	float scrollPosReal = scrollPos;
	clamp(scrollPosReal, (float)m_VScrollBar->GetMinRange(), (float)m_VScrollBar->ScrollSize());
	m_pad->SetWndPos(Fvector2().set(wPos.x, float(-scrollPosReal)));
	m_visible_rgn.set(-1, -1);
}

void CUIScrollView::_updateVerticalScrollState()
{
	if (!m_VScrollBar || !m_pad)
	{
		return;
	}

	const Fvector2 wPos = m_pad->GetWndPos();
	if (!m_VScrollBar->IsFixedLayout())
	{
		m_VScrollBar->SetHeight(GetHeight());
	}
	m_VScrollBar->SetRange(0, iFloor(m_pad->GetHeight() * Scroll2ViewV()));
	m_VScrollBar->SetScrollPos(iFloor(-wPos.y));

	const bool showScroll = NeedShowScrollBar();
	m_VScrollBar->Show(showScroll);
	m_VScrollBar->Enable(showScroll);
}

void CUIScrollView::UpdateScroll()
{
	_updateVerticalScrollState();
}

float CUIScrollView::Scroll2ViewV	(){
	float h = m_VScrollBar->GetHeight();
	return (h + GetVertIndent())/h;
}

void CUIScrollView::SetFixedScrollBar(bool b)
{
	m_flags.set(eFixedScrollBar, b);
}

void CUIScrollView::Draw()
{
	if (m_flags.test(eNeedRecalc))
	{
		RecalcSize();
	}

	for (CUIWindow* child : m_ChildWndList)
	{
		if (!child || child == m_pad || child == m_VScrollBar)
		{
			continue;
		}
		if (!child->IsShown() || child->GetCustomDraw())
		{
			continue;
		}
		child->Draw();
	}

	Frect visible_rect;
	GetAbsoluteRect(visible_rect);
	visible_rect.top += m_upIndent;
	visible_rect.bottom -= m_downIndent;
	UI().PushScissor(visible_rect);

	xrCriticalSectionGuard guard(m_pad->csUi);
	WINDOW_LIST_it it = m_pad->GetChildWndList().begin();

	if (!Empty() && m_visible_rgn.x != -1)
	{
		std::advance(it, m_visible_rgn.x);
		for (int idx = m_visible_rgn.x; idx <= m_visible_rgn.y; ++it, ++idx)
		{
			CUIScrollView* sw = (*it)->ui_cast_scroll_view();
			VERIFY(sw == nullptr);

			if ((*it)->GetVisible())
			{
				(*it)->Draw();
			}
		}
	}
	else for (int idx = 0; it != m_pad->GetChildWndList().end(); ++it, ++idx)
	{
		Frect item_rect;
		(*it)->GetAbsoluteRect(item_rect);
		if (visible_rect.intersected(item_rect))
		{
			if (m_visible_rgn.x == -1) //first visible
			{
				m_visible_rgn.x = idx;
			}

			m_visible_rgn.y = idx;

			if ((*it)->GetVisible())
			{
				(*it)->Draw();
			}
		}
		else if (m_visible_rgn.x != -1)
		{
			break;
		}
	}

	UI().PopScissor();

	if (m_VScrollBar && m_VScrollBar->IsShown())
	{
		m_VScrollBar->Draw();
	}
}

bool CUIScrollView::NeedShowScrollBar()
{
	if (!m_VScrollBar || !m_pad)
	{
		return false;
	}

	if (m_flags.test(eFixedScrollBar))
	{
		return true;
	}

	const float visibleHeight = GetHeight() - GetVertIndent();
	return m_pad->GetHeight() > visibleHeight + 1.0f;
}

void CUIScrollView::OnScrollV(CUIWindow*, void*)
{
	_applyScrollPos(m_VScrollBar->GetScrollPos());
}

bool CUIScrollView::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if(inherited::OnMouseAction(x,y,mouse_action)) return true;
	bool res = false;
	int prev_pos	= m_VScrollBar->GetScrollPos();
	switch (mouse_action){
		case WINDOW_MOUSE_WHEEL_UP:
			m_VScrollBar->TryScrollDec(true);
			res = true;
		break;
		case WINDOW_MOUSE_WHEEL_DOWN:
			m_VScrollBar->TryScrollInc(true);
			res = true;
		break;
		case WINDOW_MOUSE_MOVE:
			if( pInput->iGetAsyncBtnState(0) ){
				Fvector2	curr_pad_pos = m_pad->GetWndPos	();
				curr_pad_pos.y				+= GetUICursor().GetCursorPositionDelta().y;
				
				float max_pos = m_pad->GetHeight() - GetHeight();
				max_pos							= std::max(0.0f,max_pos);
				clamp							(curr_pad_pos.y,-max_pos,0.0f);
				m_pad->SetWndPos				(curr_pad_pos);
				UpdateScroll					();
				res = true;
			}
		break;
	};
	if(prev_pos	!= m_VScrollBar->GetScrollPos())
		m_visible_rgn.set			(-1,-1);

	return res;
}

int CUIScrollView::GetMinScrollPos()
{
	return m_VScrollBar->GetMinRange();
}

int CUIScrollView::GetMaxScrollPos()
{
	return m_VScrollBar->GetMaxRange();
}
int CUIScrollView::GetCurrentScrollPos()
{
	return m_VScrollBar->GetScrollPos();
}

void CUIScrollView::SetScrollPos(int value)
{
	if (m_flags.test(eNeedRecalc))
	{
		RecalcSize();
	}

	clamp(value, GetMinScrollPos(), GetMaxScrollPos());
	m_VScrollBar->SetScrollPos(value);
	_applyScrollPos(value);
}

void CUIScrollView::ScrollToBegin()
{
	if (m_flags.test(eNeedRecalc))
	{
		RecalcSize();
	}

	m_VScrollBar->SetScrollPos(m_VScrollBar->GetMinRange());
	_applyScrollPos(m_VScrollBar->GetScrollPos());
}

void CUIScrollView::ScrollToEnd()
{
	if (m_flags.test(eNeedRecalc))
	{
		RecalcSize();
	}

	m_VScrollBar->SetScrollPos(m_VScrollBar->GetMaxRange());
	_applyScrollPos(m_VScrollBar->GetScrollPos());
}

void CUIScrollView::ScrollToItem(CUIWindow *pItem, int addVerticalOffset)
{
	if (m_flags.test(eNeedRecalc))
		RecalcSize();

	// Check we have this item on the list
	WINDOW_LIST &items = m_pad->GetChildWndList();
	WINDOW_LIST_it it = std::find(items.begin(), items.end(), pItem);
	if (it != items.end())
	{
		const Fvector2&	pos = pItem->GetWndPos();
		SetScrollPos(m_upIndent + pos.y + addVerticalOffset);
	}
}

void CUIScrollView::SetRightIndention	(float val)
{
	m_rightIndent		= val;
	m_flags.set			(eNeedRecalc,true);
}

void CUIScrollView::SetLeftIndention	(float val)
{
	m_leftIndent			= val;
	m_flags.set			(eNeedRecalc,true);
}

void CUIScrollView::SetUpIndention(float val){
	m_upIndent			= val;
	m_flags.set			(eNeedRecalc,true);
}

void CUIScrollView::SetDownIndention(float val){
	m_downIndent			= val;
	m_flags.set			(eNeedRecalc,true);
}

u32 CUIScrollView::GetSize				()
{
	return m_pad->GetChildNum		();
}

CUIWindow* CUIScrollView::GetItem		(u32 idx)
{
	if(m_pad->GetChildNum() <= idx)
		return nullptr;

	xrCriticalSectionGuard guard(m_pad->csUi);
	WINDOW_LIST_it it = m_pad->GetChildWndList().begin();
	std::advance(it, idx);
	return (*it);
}

float CUIScrollView::GetDesiredChildWidth(){
	if (NeedShowScrollBar())
        return GetWidth() - m_VScrollBar->GetWidth() - m_rightIndent - m_leftIndent;
	else
		return GetWidth() - m_rightIndent - m_leftIndent;
}

float CUIScrollView::GetHorizIndent(){
	return m_rightIndent + m_leftIndent;
}

float CUIScrollView::GetVertIndent(){
	return m_upIndent + m_downIndent;
}

void CUIScrollView::SetSelected(CUIWindow* w)
{
	if (!m_flags.test(eItemsSelectabe))
	{
		return;
	}

	xrCriticalSectionGuard guard(m_pad->csUi);
	for(WINDOW_LIST_it it = m_pad->GetChildWndList().begin(); m_pad->GetChildWndList().end()!=it; ++it)
	{
		smart_cast<CUISelectable*>(*it)->SetSelected(*it==w);
	}
}

CUIWindow* CUIScrollView::GetSelected()
{
	if (!m_flags.test(eItemsSelectabe))
	{
		return nullptr;
	}

	xrCriticalSectionGuard guard(m_pad->csUi);
	for (CUIWindow* child : m_pad->GetChildWndList())
	{
		if (child->ui_cast_selectable()->GetSelected())
		{
			return child;
		}
	}

	return nullptr;
}

void CUIScrollView::UpdateChildrenLenght()
{
	float len = GetDesiredChildWidth();
	xrCriticalSectionGuard guard(m_pad->csUi);

	for (CUIWindow* child : m_pad->GetChildWndList())
	{
		child->SetWidth(len);
	}
}

bool CUIScrollView::MoveSelectionUp(bool bAllowLoop)
{
	CUIWindow* pNewSelection = nullptr;
	if (!::MoveSelectionUp<CUIWindow>(Items(), GetSelected(), pNewSelection, bAllowLoop))
		return false;

	R_ASSERT(pNewSelection);
	SetSelected(pNewSelection);
	return true;
}

bool CUIScrollView::MoveSelectionDown(bool bAllowLoop)
{
	CUIWindow* pNewSelection = nullptr;
	if (!::MoveSelectionDown<CUIWindow>(Items(), GetSelected(), pNewSelection, bAllowLoop))
		return false;

	R_ASSERT(pNewSelection);
	SetSelected(pNewSelection);
	return true;
}
