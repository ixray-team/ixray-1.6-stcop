#include "stdafx.h"
#include "UIWindow.h"
#include "../UICursor.h"
#include "../ui_base.h"
#include "../ui_defs.h"
#include "../UILayout.h"

#include "../Include/xrRender/DebugRender.h"
#include "../Include/xrRender/UIRender.h"
#include "../xrEngine/IGame_Persistent.h"
#include "UIBtnHint.h"

#ifdef DEBUG_DRAW
namespace
{
xrCriticalSection s_liveUiWindowsLock;
xr_hash_set<CUIWindow*> s_liveUiWindows;

void RegisterLiveUIWindow(CUIWindow* wnd)
{
	if (wnd == nullptr)
	{
		return;
	}
	xrCriticalSectionGuard guard(s_liveUiWindowsLock);
	s_liveUiWindows.insert(wnd);
}

void UnregisterLiveUIWindow(CUIWindow* wnd)
{
	if (wnd == nullptr)
	{
		return;
	}
	xrCriticalSectionGuard guard(s_liveUiWindowsLock);
	s_liveUiWindows.erase(wnd);
}
} // namespace

bool IsLiveUIWindow(CUIWindow* wnd)
{
	if (wnd == nullptr)
	{
		return false;
	}
	xrCriticalSectionGuard guard(s_liveUiWindowsLock);
	return s_liveUiWindows.contains(wnd);
}
#endif

namespace
{
thread_local int s_anchorAbsRectDepth = 0;

struct AnchorAbsRectDepthGuard
{
	AnchorAbsRectDepthGuard()
	{
		++s_anchorAbsRectDepth;
	}
	~AnchorAbsRectDepthGuard()
	{
		--s_anchorAbsRectDepth;
	}
};

constexpr int AnchorAbsRectDepthLimit = 32;

void LogInvalidAnchorToOnce(CUIWindow* startWnd, LPCSTR reason)
{
	static bool hasLogged = false;
	if (hasLogged || startWnd == nullptr)
	{
		return;
	}
	hasLogged = true;
	LPCSTR startName = startWnd->WindowName().size() != 0
		? startWnd->WindowName().c_str()
		: startWnd->WindowNodeName().c_str();
	CUIWindow* parentWnd = startWnd->GetParent();
	LPCSTR parentName = "root";
	if (parentWnd != nullptr)
	{
		parentName = parentWnd->WindowName().size() != 0
			? parentWnd->WindowName().c_str()
			: parentWnd->WindowNodeName().c_str();
	}
	Msg("! UI anchor_to: %s (widget [%s], parent [%s])", reason, startName, parentName);
}

void ApplyAnchorReferenceFallback(CUIWindow* parentWnd, Frect& anchorRect)
{
	if (parentWnd != nullptr)
	{
		parentWnd->GetAbsoluteRect(anchorRect);
	}
	else
	{
		UI().GetSafeAreaRootRect(anchorRect);
	}
}

// Returns true when anchor_to links under parentWnd revisit a window or exceed the depth limit.
bool HasInvalidAnchorToGraph(CUIWindow* startWnd, CUIWindow* parentWnd)
{
	if (startWnd == nullptr || parentWnd == nullptr)
	{
		return false;
	}

	const shared_str& startAnchorTo = startWnd->GetAnchorTo();
	if (startAnchorTo.size() != 0)
	{
		if (startAnchorTo == startWnd->WindowName())
		{
			LogInvalidAnchorToOnce(startWnd, "self anchor_to is invalid");
			return true;
		}
		CUIWindow* startTarget = parentWnd->FindAnchorTargetUnderParent(startAnchorTo);
		if (startTarget == startWnd)
		{
			LogInvalidAnchorToOnce(startWnd, "anchor_to resolves to self");
			return true;
		}
	}

	xr_set<CUIWindow*> visited;
	CUIWindow* w = startWnd;
	while (w != nullptr)
	{
		const shared_str& anchorToName = w->GetAnchorTo();
		if (anchorToName.size() == 0)
		{
			return false;
		}
		if (visited.size() >= (size_t)AnchorAbsRectDepthLimit)
		{
			LogInvalidAnchorToOnce(
				startWnd,
				"reference chain exceeds depth limit (possible misconfiguration)");
			return true;
		}
		if (!visited.insert(w).second)
		{
			LogInvalidAnchorToOnce(startWnd, "cycle detected while resolving reference rect");
			return true;
		}

		CUIWindow* target = parentWnd->FindAnchorTargetUnderParent(anchorToName);
		if (target == nullptr)
		{
			return false;
		}
		if (target == w || target == startWnd)
		{
			LogInvalidAnchorToOnce(startWnd, "cycle detected while resolving reference rect");
			return true;
		}
		if (startWnd->IsDescendantWindow(target))
		{
			LogInvalidAnchorToOnce(
				startWnd,
				"anchor_to target must not be a descendant of the anchored widget");
			return true;
		}
		if (!visited.insert(target).second)
		{
			LogInvalidAnchorToOnce(startWnd, "cycle detected while resolving reference rect");
			return true;
		}
		w = target;
	}
	return false;
}
} // namespace

// #define LOG_ALL_WNDS
#ifdef LOG_ALL_WNDS
	int ListWndCount = 0;
	struct DBGList{
		int				num;
		bool			closed;
	};
	xr_vector<DBGList>	dbg_list_wnds;
	void UI_API dump_list_wnd(){
		Msg("------Total  wnds %d",dbg_list_wnds.size());
		xr_vector<DBGList>::iterator _it = dbg_list_wnds.begin();
		for(;_it!=dbg_list_wnds.end();++_it)
			if(!(*_it).closed)
				Msg("--leak detected ---- wnd = %d",(*_it).num);
	}
#else
UI_API void dump_list_wnd(){}
#endif

xr_vector<Frect> g_wnds_rects;
UI_API BOOL g_show_wnd_rect2 = FALSE;

UI_API void clean_wnd_rects()
{
#ifdef DEBUG
	DRender->DestroyDebugShader(IDebugRender::dbgShaderWindow);
#endif // DEBUG
}

void add_rect_to_draw(Frect r)
{
	g_wnds_rects.push_back(r);
}
void draw_rect(Frect& r, u32 color)
{
#ifdef DEBUG

	DRender->SetDebugShader(IDebugRender::dbgShaderWindow);

//.	UIRender->StartLineStrip	(5);
	UIRender->StartPrimitive	(5, IUIRender::ptLineStrip, UI().m_currentPointType);

	UIRender->PushPoint(r.lt.x, r.lt.y, 0, color, 0,0);
	UIRender->PushPoint(r.rb.x, r.lt.y, 0, color, 0,0);
	UIRender->PushPoint(r.rb.x, r.rb.y, 0, color, 0,0);
	UIRender->PushPoint(r.lt.x, r.rb.y, 0, color, 0,0);
	UIRender->PushPoint(r.lt.x, r.lt.y, 0, color, 0,0);

//.	UIRender->FlushLineStrip();
	UIRender->FlushPrimitive();
	
#endif // DEBUG
}
UI_API void draw_wnds_rects()
{
	if(0==g_wnds_rects.size())	return;

	xr_vector<Frect>::iterator it = g_wnds_rects.begin();
	xr_vector<Frect>::iterator it_e = g_wnds_rects.end();

	for(;it!=it_e;++it)
	{
		Frect& r = *it;
		UI().ClientToScreenScaled(r.lt, r.lt.x, r.lt.y);
		UI().ClientToScreenScaled(r.rb, r.rb.x, r.rb.y);
		draw_rect				(r,color_rgba(255,0,0,255));
	};

	g_wnds_rects.clear();
}

void CUIWindow::SetPPMode()
{
	m_bPP = true;
	g_pGamePersistent->m_pMainMenu->RegisterPPDraw(this);
	Show(false);
}

void CUIWindow::ResetPPMode()
{
	if(	GetPPMode() )
	{
		g_pGamePersistent->m_pMainMenu->UnregisterPPDraw(this);
		m_bPP							= false;
	}
}

CUIWindow::CUIWindow()
:m_pParentWnd(nullptr),
m_pFont(nullptr),
m_pMouseCapturer(nullptr),
m_pMessageTarget(nullptr),
m_pKeyboardCapturer(nullptr),
m_bAutoDelete(false),
m_bCursorOverWindow(false),
m_bPP(false),
m_dwFocusReceiveTime(0),
m_bCustomDraw(false),
m_bLoggedMissingAnchorTo(false),
m_pLayout(nullptr),
_dirtyFlags(0)
{
	Show					(true);
	Enable					(true);
#ifdef DEBUG_DRAW
	RegisterLiveUIWindow(this);
#endif
#ifdef LOG_ALL_WNDS
	ListWndCount++;
	m_dbg_id = ListWndCount;
	dbg_list_wnds.push_back(DBGList());
	dbg_list_wnds.back().num		= m_dbg_id;
	dbg_list_wnds.back().closed		= false;
#endif
}

CUIWindow::~CUIWindow()
{
	VERIFY( !(GetParent()&&IsAutoDelete()) );

	CUIWindow* parent	= GetParent();
	bool ad				= IsAutoDelete();
	if( parent && !ad )
		parent->CUIWindow::DetachChild( this );

	if (!m_ChildWndList.empty()) {
		DetachAll();
	}

	if (GetPPMode() && g_pGamePersistent != nullptr)
		g_pGamePersistent->m_pMainMenu->UnregisterPPDraw(this);

	xr_delete(m_pLayout);

#ifdef DEBUG_DRAW
	UnregisterLiveUIWindow(this);
	if (m_pUI_core != nullptr)
	{
		UI().LastFrameWidgets.erase(this);
	}
#endif

#ifdef LOG_ALL_WNDS
	xr_vector<DBGList>::iterator _it = dbg_list_wnds.begin();
	bool bOK = false;
	for(;_it!=dbg_list_wnds.end();++_it){
		if( (*_it).num==m_dbg_id && !(*_it).closed){
			bOK = true;
			(*_it).closed = true;
			dbg_list_wnds.erase(_it);
			break;
		}
		if( (*_it).num==m_dbg_id && (*_it).closed){
			Msg("--CUIWindow [%d] already deleted", m_dbg_id);
			bOK = true;
		}
	}
	if(!bOK)
		Msg("CUIWindow::~CUIWindow.[%d] cannot find window in list", m_dbg_id);
#endif
}



void CUIWindow::Draw()
{
	xrCriticalSectionGuard guard(csUi);
	UIScaleModeScope scaleModeScope(&UI(), GetScaleMode());

#ifdef DEBUG_DRAW
	if (IsShown())
	{
		UI().LastFrameWidgets.emplace(this);
	}
#endif

	for (CUIWindow* W : m_ChildWndList)
	{
		if (!W)		continue;
		if (!W->IsShown())		continue;
		if (W->GetCustomDraw())	continue;
		W->Draw();
	}

#ifdef DEBUG
	if (g_show_wnd_rect2)
	{
		Frect r;
		GetAbsoluteRect(r);
		add_rect_to_draw(r);
	}
#endif
}

void CUIWindow::SetLayout(ILayoutProvider* layout)
{
	if (m_pLayout == layout)
		return;
	m_pLayout = layout;
	const u32 flags = UiDirtyMask(EUIDirtyFlags::Layout) | UiDirtyMask(EUIDirtyFlags::AbsoluteRect);
	_dirtyFlags |= flags;
	MarkParentLayoutDirty(flags);
}

void CUIWindow::MarkDirty(u32 flags)
{
	if (!ParticipatesInUILayoutDirtyPropagation())
		return;
	_dirtyFlags |= flags;
}

void CUIWindow::ClearDirty(u32 flagsToClear)
{
	_dirtyFlags &= ~flagsToClear;
}

void CUIWindow::MarkParentLayoutDirty(u32 flags)
{
	for (CUIWindow* parent = m_pParentWnd; parent != nullptr; parent = parent->m_pParentWnd)
		parent->MarkDirty(flags);
}

void CUIWindow::NotifyChildLayoutChanged(CUIWindow* child, u32 flags)
{
	R_ASSERT(child);
	VERIFY(IsChild(child));
	(void)child;
	MarkDirty(flags);
	MarkParentLayoutDirty(flags);
}

void CUIWindow::MarkDirtyOnParticipatingSiblingsUnderSameParent(u32 flags)
{
	CUIWindow* parent = GetParent();
	if (parent == nullptr)
		return;
	xrCriticalSectionGuard guard(parent->csUi);
	for (CUIWindow* sibling : parent->m_ChildWndList)
	{
		if (sibling != nullptr && sibling->ParticipatesInUILayoutDirtyPropagation())
			sibling->MarkDirty(flags);
	}
}

void CUIWindow::SetWindowName(LPCSTR wn)
{
	shared_str next = wn ? wn : "";
	if (next == m_windowName)
		return;
	m_windowName = next;
	const u32 anchorFlags = UiDirtyMask(EUIDirtyFlags::Layout) | UiDirtyMask(EUIDirtyFlags::AbsoluteRect);
	MarkDirty(anchorFlags);
	MarkParentLayoutDirty(anchorFlags);
	MarkDirtyOnParticipatingSiblingsUnderSameParent(anchorFlags);
}

void CUIWindow::Draw(float x, float y)
{
	SetWndPos(Fvector2().set(x, y));
	Draw();
}

void CUIWindow::Update()
{
	// Resolve auto size for non-anchored elements (layout resolves its children)
	if (!GetUseAnchors() && (GetSizeModeWidth() == UI_SIZE_MODE_AUTO || GetSizeModeHeight() == UI_SIZE_MODE_AUTO))
	{
		ResolveAutoSize();
	}

	if (m_pLayout)
	{
		m_pLayout->LayoutChildren(this);
	}

	// Resolve auto size and update anchor offsets for anchored elements with wrap content
	if (GetUseAnchors() && (GetSizeModeWidth() == UI_SIZE_MODE_AUTO || GetSizeModeHeight() == UI_SIZE_MODE_AUTO))
	{
		ResolveAutoSize();
		SyncAnchorOffsetsFromSize(GetAnchorData(), GetWidth(), GetHeight());
	}

	if (GetUseAnchors())
	{
		ApplyAnchoredRelativeGeometry();
	}

	if (GetUICursor().IsVisible())
	{
		bool cursor_on_window;

		Fvector2			temp = GetUICursor().GetCursorPosition();
		Frect				r;
		GetAbsoluteRect(r);
		cursor_on_window = !!r.in(temp);

#ifdef DEBUG
		if (cursor_on_window && g_show_wnd_rect2) {
			Frect rect{};
			GetAbsoluteRect(rect);
			add_rect_to_draw(rect);
			// Sort windows without name
			if (bDebug && m_windowNodeName.c_str() != nullptr) {
				CGameFont* F = UI().Font().pFontDI;
				F->SetAligment(CGameFont::alCenter);
				F->SetColor(0xffffffff);
				F->OutNext("NodeName: [%s],Size: [w:%.2f] [h:%.2f], Pos: [x:%.f] [y:%.f]", m_windowNodeName.c_str(), GetWndSize().x, GetWndSize().y, GetWndPos().x, GetWndPos().y);
			}
		}
#endif

		// RECEIVE and LOST focus
		if (m_bCursorOverWindow != cursor_on_window)
		{
			if (cursor_on_window)
				OnFocusReceive();
			else
				OnFocusLost();
		}
	}
	xrCriticalSectionGuard guard(csUi);
	for (WINDOW_LIST_it it = m_ChildWndList.begin(); m_ChildWndList.end() != it; ++it) {
		if (!(*it)->IsShown()) continue;
		(*it)->Update();
	}
}

void CUIWindow::AttachChild(CUIWindow* pChild)
{
	R_ASSERT(pChild);
	if (!pChild) return;

	R_ASSERT(!IsChild(pChild));
	pChild->SetParent(this);

	xrCriticalSectionGuard guard(csUi);
	m_ChildWndList.push_back(pChild);
}

void CUIWindow::DetachChild(CUIWindow* pChild)
{
	R_ASSERT(pChild);
	if (nullptr == pChild)
		return;

	if (m_pMouseCapturer == pChild)
		SetCapture(pChild, false);

	{
		xrCriticalSectionGuard guard(csUi);

		WINDOW_LIST_it it = std::find(m_ChildWndList.begin(), m_ChildWndList.end(), pChild);
		if (it != m_ChildWndList.end())
		{
			m_ChildWndList.erase(it);
		}
	}

	pChild->SetParent(nullptr);

	if (pChild->IsAutoDelete())
		xr_delete(pChild);
}

void CUIWindow::DetachAll()
{
	xrCriticalSectionGuard guard(csUi);

	while( !m_ChildWndList.empty() ){
		DetachChild( m_ChildWndList.back() );	
	}
}

void CUIWindow::ComputeAnchoredAbsoluteRect(Frect& outAbsolute) const
{
	if (GetParent() == nullptr)
	{
		Frect parentRect;
		UI().GetSafeAreaRootRect(parentRect);
		ComputeAnchoredRect(parentRect, GetAnchorData(), outAbsolute);
		return;
	}

	AnchorAbsRectDepthGuard depthGuard;
	if (s_anchorAbsRectDepth > AnchorAbsRectDepthLimit)
	{
		LogInvalidAnchorToOnce(
			const_cast<CUIWindow*>(this),
			"GetAbsoluteRect recursion depth exceeded (possible anchor_to cycle)");
		Frect refRect;
		ApplyAnchorReferenceFallback(GetParent(), refRect);
		ComputeAnchoredRect(refRect, GetAnchorData(), outAbsolute);
		return;
	}

	Frect anchorRect;
	const_cast<CUIWindow*>(this)->ResolveAnchorReferenceRect(anchorRect);
	ComputeAnchoredRect(anchorRect, GetAnchorData(), outAbsolute);
}

void CUIWindow::ApplyAnchoredRelativeGeometry()
{
	Frect ourRect;
	ComputeAnchoredAbsoluteRect(ourRect);

	if (GetParent())
	{
		Frect parentRect;
		GetParent()->GetAbsoluteRect(parentRect);
		SetWndPos(Fvector2().set(ourRect.x1 - parentRect.x1, ourRect.y1 - parentRect.y1));
		SetWndSize(Fvector2().set(ourRect.width(), ourRect.height()));
	}
	else
	{
		SetWndPos(Fvector2().set(ourRect.x1, ourRect.y1));
		SetWndSize(Fvector2().set(ourRect.width(), ourRect.height()));
	}
}

void CUIWindow::GetAbsoluteRect(Frect& r)
{
	if (GetParent() == nullptr)
	{
		if (GetUseAnchors())
		{
			ComputeAnchoredAbsoluteRect(r);
		}
		else
		{
			GetWndRect(r);
		}
		return;
	}

	if (GetUseAnchors())
	{
		ComputeAnchoredAbsoluteRect(r);
		return;
	}

	GetParent()->GetAbsoluteRect(r);

	Frect rr;
	GetWndRect(rr);
	r.left += rr.left;
	r.top += rr.top;
	r.right = r.left + GetWidth();
	r.bottom = r.top + GetHeight();
}

//реакция на мышь
//координаты курсора всегда, кроме начального вызова 
//задаются относительно текущего окна

#define DOUBLE_CLICK_TIME 250

bool CUIWindow::OnMouseAction(float x, float y, EUIMessages mouse_action)
{	
	Frect	wndRect = GetWndRect();

	cursor_pos.x = x;
	cursor_pos.y = y;


	if( WINDOW_LBUTTON_DOWN == mouse_action )
	{
		static u32 _last_db_click_frame		= 0;
		u32 dwCurTime						= Device.dwTimeContinual;

		if( (_last_db_click_frame!=Device.dwFrame) && (dwCurTime-m_dwLastClickTime < DOUBLE_CLICK_TIME) )
		{
            mouse_action			= WINDOW_LBUTTON_DB_CLICK;
			_last_db_click_frame	= Device.dwFrame;
		}

		m_dwLastClickTime = dwCurTime;
	}

	if(GetParent()== nullptr)
	{
		if(!wndRect.in(cursor_pos))
            return false;
		//получить координаты относительно окна
		cursor_pos.x -= wndRect.left;
		cursor_pos.y -= wndRect.top;
	}


	//если есть дочернее окно,захватившее мышь, то
	//сообщение направляем ему сразу
	if(m_pMouseCapturer)
	{
		m_pMouseCapturer->OnMouseAction(cursor_pos.x - m_pMouseCapturer->GetWndRect().left, 
								  cursor_pos.y - m_pMouseCapturer->GetWndRect().top, 
								  mouse_action);
		return true;
	}

	// handle any action
	switch (mouse_action){
		case WINDOW_MOUSE_MOVE:
			OnMouseMove();							break;
		case WINDOW_MOUSE_WHEEL_DOWN:
			OnMouseScroll(WINDOW_MOUSE_WHEEL_DOWN); break;
		case WINDOW_MOUSE_WHEEL_UP:
			OnMouseScroll(WINDOW_MOUSE_WHEEL_UP);	break;
		case WINDOW_LBUTTON_DOWN:
			if(OnMouseDown(MOUSE_1))				return true;	break;
		case WINDOW_RBUTTON_DOWN:
			if(OnMouseDown(MOUSE_2))				return true;	break;
		case WINDOW_CBUTTON_DOWN:
			if(OnMouseDown(MOUSE_3))				return true;	break;
		case WINDOW_LBUTTON_DB_CLICK:
			if (OnDbClick())						return true;	break;
		default:
            break;
	}

	//Проверка на попадание мыши в окно,
	//происходит в обратном порядке, чем рисование окон
	//(последние в списке имеют высший приоритет)
	xrCriticalSectionGuard guard(csUi);
	WINDOW_LIST::reverse_iterator it = m_ChildWndList.rbegin();
	WINDOW_LIST::reverse_iterator first = m_ChildWndList.rend();

	for (u32 i = 0; it != first; ++it, i++)
	{
		CUIWindow* w = (*it);
		if (!w)
		{
			Msg("! Founded incorrect child window in [%s] childlist(%d)", *m_windowName, i);
		}
		else
		{
			Frect wndRect_ = w->GetWndRect();
			if (wndRect_.in(cursor_pos))
			{
				if (w->IsEnabled())
				{
					if (w->OnMouseAction(cursor_pos.x - w->GetWndRect().left,
						cursor_pos.y - w->GetWndRect().top, mouse_action))return true;
				}
			}
			else if (w->IsEnabled() && w->CursorOverWindow())
			{
				if (w->OnMouseAction(cursor_pos.x - w->GetWndRect().left,
					cursor_pos.y - w->GetWndRect().top, mouse_action))return true;
			}
		}
	}


	return false;
}


void CUIWindow::OnMouseMove(){
}

void CUIWindow::OnMouseScroll(float iDirection){
}

bool CUIWindow::OnDbClick(){
	if (GetMessageTarget())
		GetMessageTarget()->SendMessage(this, WINDOW_LBUTTON_DB_CLICK);
	return false;
}

bool CUIWindow::OnMouseDown(int mouse_btn){
	return false;
}

void CUIWindow::OnMouseUp(int mouse_btn){
}

void CUIWindow::OnFocusReceive()
{
	m_dwFocusReceiveTime	= Device.dwTimeContinual;
	m_bCursorOverWindow		= true;	

	if (GetMessageTarget())
        GetMessageTarget()->SendMessage(this, WINDOW_FOCUS_RECEIVED, nullptr);
}

void CUIWindow::OnFocusLost()
{
	m_dwFocusReceiveTime	= 0;
	m_bCursorOverWindow		= false;	

	if (GetMessageTarget())
        GetMessageTarget()->SendMessage(this, WINDOW_FOCUS_LOST, nullptr);
}


//Сообщение, посылаемое дочерним окном,
//о том, что окно хочет захватить мышь,
//все сообщения от нее будут направляться только
//ему в независимости от того где мышь
void CUIWindow::SetCapture(CUIWindow *pChildWindow, bool capture_status)
{
	if(GetParent())
	{
		GetParent()->SetCapture(this, capture_status);
	}

	if(capture_status)
	{
		//оповестить дочернее окно о потере фокуса мыши
		if(nullptr!=m_pMouseCapturer)
			m_pMouseCapturer->SendMessage(this, WINDOW_MOUSE_CAPTURE_LOST);

		m_pMouseCapturer = pChildWindow;
	}
	else
	{
			m_pMouseCapturer = nullptr;
	}
}


//реакция на клавиатуру
bool CUIWindow::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	bool result;

	//если есть дочернее окно,захватившее клавиатуру, то
	//сообщение направляем ему сразу
	if(nullptr!=m_pKeyboardCapturer)
	{
		result = m_pKeyboardCapturer->OnKeyboardAction(dik, keyboard_action);
		
		if(result) return true;
	}
	xrCriticalSectionGuard guard(csUi);
	WINDOW_LIST::reverse_iterator it = m_ChildWndList.rbegin();

	for(; it!=m_ChildWndList.rend(); ++it)
	{
		if((*it)->IsEnabled())
		{
			result = (*it)->OnKeyboardAction(dik, keyboard_action);
			
			if(result)	return true;
		}
	}
	return false;
}

//реакция на геймпад (кнопки)
bool CUIWindow::OnGamepadKeyAction(int key, EUIMessages gamepad_action)
{
	bool result;

	//если есть дочернее окно,захватившее клавиатуру, то
	//сообщение направляем ему сразу
	if(nullptr!=m_pKeyboardCapturer)
	{
		result = m_pKeyboardCapturer->OnGamepadKeyAction(key, gamepad_action);
		
		if(result) return true;
	}
	xrCriticalSectionGuard guard(csUi);
	WINDOW_LIST::reverse_iterator it = m_ChildWndList.rbegin();

	for(; it!=m_ChildWndList.rend(); ++it)
	{
		if((*it)->IsEnabled())
		{
			result = (*it)->OnGamepadKeyAction(key, gamepad_action);
			
			if(result)	return true;
		}
	}
	return false;
}

//реакция на геймпад (стики)
bool CUIWindow::OnGamepadStickAction(int key, Fvector2 value, EUIMessages gamepad_action)
{
	bool result;

	//если есть дочернее окно,захватившее клавиатуру, то
	//сообщение направляем ему сразу
	if (nullptr != m_pKeyboardCapturer)
	{
		result = m_pKeyboardCapturer->OnGamepadStickAction(key, value, gamepad_action);

		if (result) return true;
	}
	xrCriticalSectionGuard guard(csUi);
	WINDOW_LIST::reverse_iterator it = m_ChildWndList.rbegin();

	for (; it != m_ChildWndList.rend(); ++it)
	{
		if ((*it)->IsEnabled())
		{
			result = (*it)->OnGamepadStickAction(key, value, gamepad_action);

			if (result)	return true;
		}
	}
	return false;
}

bool CUIWindow::OnGamepadKeyHold(int dik)
{
	bool result;

	if (nullptr != m_pKeyboardCapturer)
	{
		result = m_pKeyboardCapturer->OnKeyboardHold(dik);

		if (result) return true;
	}
	xrCriticalSectionGuard guard(csUi);
	WINDOW_LIST::reverse_iterator it = m_ChildWndList.rbegin();

	for (; it != m_ChildWndList.rend(); ++it)
	{
		if ((*it)->IsEnabled())
		{
			result = (*it)->OnGamepadKeyHold(dik);

			if (result)	return true;
		}
	}

	return false;
}

bool CUIWindow::OnKeyboardHold(int dik)
{
	bool result;

	if(nullptr!=m_pKeyboardCapturer)
	{
		result = m_pKeyboardCapturer->OnKeyboardHold(dik);
		
		if(result) return true;
	}
	xrCriticalSectionGuard guard(csUi);
	WINDOW_LIST::reverse_iterator it = m_ChildWndList.rbegin();

	for(; it!=m_ChildWndList.rend(); ++it)
	{
		if((*it)->IsEnabled())
		{
			result = (*it)->OnKeyboardHold(dik);
			
			if(result)	return true;
		}
	}

	return false;
}

void CUIWindow::SetKeyboardCapture(CUIWindow* pChildWindow, bool capture_status)
{
	if(nullptr != GetParent())
		GetParent()->SetKeyboardCapture(this, capture_status);

	if(capture_status)
	{
		//оповестить дочернее окно о потере фокуса клавиатуры
		if(nullptr!=m_pKeyboardCapturer)
			m_pKeyboardCapturer->SendMessage(this, WINDOW_KEYBOARD_CAPTURE_LOST);
			
		m_pKeyboardCapturer = pChildWindow;
	}
	else
		m_pKeyboardCapturer = nullptr;
}


//обработка сообщений 
void CUIWindow::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	xrCriticalSectionGuard guard(csUi);
	//оповестить дочерние окна
    for(int i = 0; i < m_ChildWndList.size(); ++i)
    {
        if(m_ChildWndList[i]->IsEnabled())
            m_ChildWndList[i]->SendMessage(pWnd,msg,pData);
    }
}

CUIWindow* CUIWindow::GetCurrentMouseHandler(){
	return GetTop()->GetChildMouseHandler();
}

CUIWindow* CUIWindow::GetChildMouseHandler(){
	xrCriticalSectionGuard guard(csUi);

	CUIWindow* pWndResult;
	WINDOW_LIST::reverse_iterator it = m_ChildWndList.rbegin();

	for(; it!=m_ChildWndList.rend(); ++it)
	{
		Frect wndRect = (*it)->GetWndRect();
		// very strange code.... i can't understand difference between
		// first and second condition. I Got It from OnMouseAction() method;
		if (wndRect.in(cursor_pos) )
		{
			if((*it)->IsEnabled())
			{
				return pWndResult = (*it)->GetChildMouseHandler();				
			}
		}
		else if ((*it)->IsEnabled() && (*it)->CursorOverWindow())
		{
			return pWndResult = (*it)->GetChildMouseHandler();
		}
	}

    return this;
}

//для перевода окна и потомков в исходное состояние
void CUIWindow::Reset()
{
	m_pMouseCapturer = nullptr;

	g_btnHint->Discard();
	g_statHint->Discard();
}

void CUIWindow::ResetAll()
{
	xrCriticalSectionGuard guard(csUi);

	for(WINDOW_LIST_it it = m_ChildWndList.begin(); m_ChildWndList.end()!=it; ++it)
	{
		(*it)->Reset();
	}
}

CUIWindow* CUIWindow::GetMessageTarget()
{
	return m_pMessageTarget?m_pMessageTarget:GetParent();
}

bool CUIWindow::IsChild(CUIWindow *pPossibleChild) const
{
	xrCriticalSectionGuard guard(const_cast<xrCriticalSection&>(csUi));
	WINDOW_LIST::const_iterator it = std::find(m_ChildWndList.begin(), m_ChildWndList.end(), pPossibleChild);
	return it != m_ChildWndList.end();
}


void CUIWindow::SetAnchorTo(LPCSTR targetName)
{
	shared_str nextName = targetName ? targetName : "";
	if (nextName != m_anchorToWindowName)
	{
		m_bLoggedMissingAnchorTo = false;
	}
	m_anchorToWindowName = nextName;
	const u32 anchorFlags = UiDirtyMask(EUIDirtyFlags::Layout) | UiDirtyMask(EUIDirtyFlags::AbsoluteRect);
	MarkDirty(anchorFlags);
	MarkParentLayoutDirty(anchorFlags);
}

void CUIWindow::LogMissingAnchorToTargetOnce()
{
	if (m_bLoggedMissingAnchorTo)
	{
		return;
	}
	m_bLoggedMissingAnchorTo = true;
	Msg(
		"! UI anchor_to: target window [%s] not found under parent (widget node [%s])",
		m_anchorToWindowName.c_str(),
		m_windowNodeName.c_str());
}

void CUIWindow::ResolveAnchorReferenceRect(Frect& anchorRect)
{
	CUIWindow* parentWnd = GetParent();
	if (parentWnd == nullptr)
	{
		UI().GetSafeAreaRootRect(anchorRect);
		return;
	}
	if (m_anchorToWindowName.size() == 0)
	{
		parentWnd->GetAbsoluteRect(anchorRect);
		return;
	}
	if (HasInvalidAnchorToGraph(this, parentWnd))
	{
		ApplyAnchorReferenceFallback(parentWnd, anchorRect);
		return;
	}
	CUIWindow* targetWnd = parentWnd->FindAnchorTargetUnderParent(m_anchorToWindowName);
	if (targetWnd != nullptr)
	{
		targetWnd->GetAbsoluteRect(anchorRect);
		return;
	}
	ApplyAnchorReferenceFallback(parentWnd, anchorRect);
	LogMissingAnchorToTargetOnce();
}

CUIWindow* CUIWindow::FindAnchorTargetUnderParent(const shared_str& name) const
{
	if (name.size() == 0)
	{
		return nullptr;
	}
	xrCriticalSectionGuard guard(const_cast<xrCriticalSection&>(csUi));
	for (CUIWindow* child : m_ChildWndList)
	{
		if (child->WindowName() == name)
		{
			return child;
		}
	}
	return nullptr;
}

bool CUIWindow::IsDescendantWindow(CUIWindow* wnd) const
{
	if (wnd == nullptr)
	{
		return false;
	}
	xrCriticalSectionGuard guard(const_cast<xrCriticalSection&>(csUi));
	for (CUIWindow* child : m_ChildWndList)
	{
		if (child == wnd || child->IsDescendantWindow(wnd))
		{
			return true;
		}
	}
	return false;
}

CUIWindow*	CUIWindow::FindChild(const shared_str name)
{
	if(WindowName()==name)
		return this;

	xrCriticalSectionGuard guard(csUi);
	WINDOW_LIST::const_iterator it = m_ChildWndList.begin();
	WINDOW_LIST::const_iterator it_e = m_ChildWndList.end();
	for(;it!=it_e;++it){
		CUIWindow* pRes = (*it)->FindChild(name);
		if(pRes != nullptr)
			return pRes;
	}
	return nullptr;
}

void CUIWindow::SetParent(CUIWindow* pNewParent) 
{
	R_ASSERT( !(m_pParentWnd && m_pParentWnd->IsChild(this)) );

	if (m_pParentWnd == pNewParent)
		return;

	CUIWindow* oldParent = m_pParentWnd;
	const u32 geometryFlags = UiDirtyMask(EUIDirtyFlags::Layout) | UiDirtyMask(EUIDirtyFlags::AbsoluteRect);

	MarkDirty(geometryFlags);
	if (oldParent != nullptr)
	{
		oldParent->MarkDirty(geometryFlags);
		for (CUIWindow* ancestor = oldParent->m_pParentWnd; ancestor != nullptr; ancestor = ancestor->m_pParentWnd)
			ancestor->MarkDirty(geometryFlags);
	}

	m_pParentWnd = pNewParent;

	MarkDirty(geometryFlags);
	MarkParentLayoutDirty(geometryFlags);
}

void CUIWindow::ShowChildren(bool show)
{
	xrCriticalSectionGuard guard(csUi);

	for(WINDOW_LIST_it it = m_ChildWndList.begin(); m_ChildWndList.end()!=it; ++it)		
			(*it)->Show(show);
}

static bool is_in( Frect const& a, Frect const& b ) //b in a
{
	return (a.x1 < b.x1) && (a.x2 > b.x2) && (a.y1 < b.y1) && (a.y2 > b.y2);
}

UI_API bool fit_in_rect(CUIWindow* w, Frect const& vis_rect, float border, float dx16pos ) //this = hint wnd
{
	float const cursor_height	= 43.0f;
	Fvector2 cursor_pos			= GetUICursor().GetCursorPosition();
	if ( UI().is_widescreen() )
	{
		cursor_pos.x -= dx16pos;
	}

	if ( !vis_rect.in(cursor_pos) )
	{
		return false;
	}

	Frect	rect;
	rect.set( -border, -border, w->GetWidth() - 2.0f*border, w->GetHeight() - 2.0f*border );
	rect.add( cursor_pos.x, cursor_pos.y );

	rect.sub( 0.0f, rect.height() - border );
	if ( !is_in( vis_rect, rect ) ) {	rect.sub( rect.width() - border, 0.0f                   );	}
	if ( !is_in( vis_rect, rect ) ) {	rect.add( 0.0f                 , rect.height() - border );	}
	if ( !is_in( vis_rect, rect ) ) {	rect.add( rect.width() - border, cursor_height          );	}

	float yn = rect.top - vis_rect.height() + rect.height( ) - border + cursor_height;
	if ( !is_in( vis_rect, rect ) ) {	rect.sub( 0.0f                 , yn                     );	}
	if ( !is_in( vis_rect, rect ) ) {	rect.sub( rect.width() - border, 0.0f                   );	}

	w->SetWndPos( rect.lt );
	return true;
}

// we need this func for controller UI
// to display info window next to the item icon
UI_API bool fit_infownd_in_rect(CUIWindow* wInfo, Frect & stick_to_rect, Frect fit_in_rect, float border, float dx16pos)
{
	//float const cursor_height = 0;// 43.0f;
	if (UI().is_widescreen())
	{
		stick_to_rect.x1 -= dx16pos;
	}

	fit_in_rect.shrink(border, border);

	if (!fit_in_rect.intersected(stick_to_rect))
	{
		return false;
	}

	Frect	rect;
	rect.set(0, 0, wInfo->GetWidth(), wInfo->GetHeight());
	rect.add(fit_in_rect.x1, fit_in_rect.y1);

	// Check horizontally
	if (stick_to_rect.x2 + wInfo->GetWidth() > fit_in_rect.x2)
		rect.set(stick_to_rect.x1 - wInfo->GetWidth(), rect.y1, stick_to_rect.x1, rect.y2 ); // on the left
	else
		rect.set(stick_to_rect.x2, rect.y1, stick_to_rect.x2 + wInfo->GetWidth(), rect.y2); // on the right

	// Check vertically
	if (stick_to_rect.y1 + wInfo->GetHeight() > fit_in_rect.y2)
		rect.set(rect.x1, fit_in_rect.y2 - wInfo->GetHeight(), rect.x2, fit_in_rect.y2);
	else
		rect.set(rect.x1, stick_to_rect.y1, rect.x2, stick_to_rect.y1 + wInfo->GetHeight());

	wInfo->SetWndPos(rect.lt);
	return true;
}
