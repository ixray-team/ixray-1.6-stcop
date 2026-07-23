#include "stdafx.h"
#include "UIScrollBar.h"
#include "UI3tButton.h"
#include "UIScrollBox.h"
#include "UIFrameLineWnd.h"

CUIScrollBar::CUIScrollBar()
{
	_decButton = new CUI3tButton();
	_decButton->SetAutoDelete(true);
	AttachChild(_decButton);

	_incButton = new CUI3tButton();
	_incButton->SetAutoDelete(true);
	AttachChild(_incButton);

	_frameBackground = new CUIFrameLineWnd();
	_frameBackground->SetAutoDelete(true);
	AttachChild(_frameBackground);

	_scrollBox = new CUIScrollBox();
	_scrollBox->SetAutoDelete(true);
	AttachChild(_scrollBox);

	_fixedThumb = new CUI3tButton();
	_fixedThumb->SetAutoDelete(true);
}

CUIScrollBar::~CUIScrollBar() = default;

float CUIScrollBar::GetDecSpan() const
{
	if (!_decButton->IsShown())
	{
		return 0.0f;
	}
	return _isHorizontal ? _decButton->GetWidth() : _decButton->GetHeight();
}

float CUIScrollBar::GetIncSpan() const
{
	if (!_incButton->IsShown())
	{
		return 0.0f;
	}
	return _isHorizontal ? _incButton->GetWidth() : _incButton->GetHeight();
}

float CUIScrollBar::mainBarSpan() const
{
	return _isHorizontal ? GetWidth() : GetHeight();
}

float CUIScrollBar::crossBarSpan() const
{
	return _isHorizontal ? GetHeight() : GetWidth();
}

float CUIScrollBar::thumbViewOffset() const
{
	return GetDecSpan() + scrollBoxInset();
}

float CUIScrollBar::scrollBoxInset() const
{
	if (_layoutMode != ScrollLayoutMode::Fixed)
	{
		return 0.0f;
	}
	return _isHorizontal ? _scrollBoxOffset.x : _scrollBoxOffset.y;
}

void CUIScrollBar::RecalcWorkArea(float thickness)
{
	if (_layoutMode == ScrollLayoutMode::Fixed)
	{
		const float inset = 2.0f * scrollBoxInset();
		_scrollWorkArea = std::max(0, iFloor(mainBarSpan() - GetDecSpan() - GetIncSpan() - inset));
	}
	else
	{
		float workArea = mainBarSpan() - GetDecSpan() - GetIncSpan();
		if (workArea <= 0.0f)
		{
			workArea = mainBarSpan() - 2.0f * thickness;
		}
		_scrollWorkArea = std::max(1, iFloor(workArea));
	}
}

CUIWindow* CUIScrollBar::GetThumbWindow() const
{
	if (_partFlags.hasThumb && _fixedThumb->IsShown())
	{
		return _fixedThumb;
	}
	if (_scrollBox->IsShown())
	{
		return _scrollBox;
	}
	return nullptr;
}

void CUIScrollBar::SetWidth(float width)
{
	if (_layoutMode == ScrollLayoutMode::Fixed)
	{
		return;
	}
	if (width <= 0.0f)
	{
		width = 1.0f;
	}
	ScrollBarBase::SetWidth(width);
	if (_isHorizontal)
	{
		RecalcWorkArea(_profileConfig.thickness);
	}
	UpdateScrollBar();
}

void CUIScrollBar::SetHeight(float height)
{
	if (_layoutMode == ScrollLayoutMode::Fixed)
	{
		return;
	}
	if (height <= 0.0f)
	{
		height = 1.0f;
	}
	ScrollBarBase::SetHeight(height);
	if (!_isHorizontal)
	{
		RecalcWorkArea(_profileConfig.thickness);
	}
	UpdateScrollBar();
}

void CUIScrollBar::SetStepSize(int step)
{
	_stepSize = step;
	UpdateScrollBar();
}

void CUIScrollBar::SetRange(int minPos, int maxPos)
{
	_minPos = minPos;
	_maxPos = maxPos;
	VERIFY(maxPos >= minPos);
	if (maxPos < minPos)
	{
		_maxPos = minPos;
	}
	UpdateScrollBar();
}

void CUIScrollBar::Show(bool show)
{
	if (!_enabled)
	{
		return;
	}
	ScrollBarBase::Show(show);
}

void CUIScrollBar::Enable(bool enable)
{
	if (!_enabled)
	{
		return;
	}
	ScrollBarBase::Enable(enable);
}

void CUIScrollBar::layoutThumbGeometry(CUIWindow* thumb, float boxSz)
{
	float clamped = boxSz;
	const float maxMain = mainBarSpan() - GetIncSpan() - GetDecSpan();
	const float minMain = std::min(crossBarSpan(), maxMain);
	clamp(clamped, minMain, maxMain);

	if (_layoutMode == ScrollLayoutMode::Fixed)
	{
		const float inset = 2.0f * scrollBoxInset();
		const float fixedMin = std::min(crossBarSpan(), maxMain - inset);
		clamp(clamped, fixedMin, maxMain - inset);
	}

	if (_isHorizontal)
	{
		thumb->SetWidth(clamped);
		if (thumb == _scrollBox)
		{
			thumb->SetHeight(GetHeight());
		}
	}
	else
	{
		thumb->SetHeight(clamped);
		if (thumb == _scrollBox)
		{
			thumb->SetWidth(GetWidth());
		}
	}

	const int pos = PosViewFromScroll(iFloor(clamped), iFloor(thumbViewOffset()));
	if (_isHorizontal)
	{
		thumb->SetWndPos(Fvector2().set(float(pos), thumb->GetWndRect().top));
	}
	else
	{
		thumb->SetWndPos(Fvector2().set(thumb->GetWndRect().left, float(pos)));
	}

	PositionIncButton(0.0f);
}

void CUIScrollBar::UpdateScrollBar()
{
	CUIWindow* thumb = GetThumbWindow();
	if (!thumb)
	{
		return;
	}

	if (IsShown())
	{
		if (_minPos == _maxPos)
		{
			_maxPos++;
		}

		const float boxSz = float(_scrollWorkArea) * float(_pageSize ? _pageSize : 1) / float(_maxPos - _minPos);
		layoutThumbGeometry(thumb, boxSz);
	}

	const u32 thumbColor = IsRelevant()
		? color_rgba(255, 255, 255, 255)
		: color_rgba(255, 255, 255, 200);
	if (_fixedThumb->IsShown())
	{
		_fixedThumb->SetTextureColor(thumbColor);
	}
	else if (_scrollBox->IsShown())
	{
		_scrollBox->SetTextureColor(thumbColor);
	}

	ClampByViewRect();
}

void CUIScrollBar::SetPosScrollFromView(float viewPos, float viewWidth, float viewOffs)
{
	const int scrollSize = ScrollSize();
	const float pos = viewPos - viewOffs;
	const float workSize = float(_scrollWorkArea) - viewWidth;
	SetScrollPosClamped(workSize ? iFloor(((pos / workSize) * scrollSize) + _minPos) : 0);

	if (_layoutMode == ScrollLayoutMode::Fixed)
	{
		UpdateScrollBar();
	}
}

int CUIScrollBar::PosViewFromScroll(int viewSize, int viewOffs)
{
	const int workSize = _scrollWorkArea - viewSize;
	const int scrollSize = ScrollSize();
	return scrollSize ? (_scrollPos * workSize + scrollSize * viewOffs - _minPos * workSize) / scrollSize : 0;
}

void CUIScrollBar::SetScrollPosClamped(int pos)
{
	_scrollPos = pos;
	clamp(_scrollPos, _minPos, _maxPos - _pageSize + 1);
}

void CUIScrollBar::NotifyScrollChanged()
{
	if (!GetMessageTarget())
	{
		return;
	}
	if (_isHorizontal)
	{
		GetMessageTarget()->SendMessage(this, SCROLLBAR_HSCROLL);
	}
	else
	{
		GetMessageTarget()->SendMessage(this, SCROLLBAR_VSCROLL);
	}
}

void CUIScrollBar::SendMessage(CUIWindow* wnd, s16 msg, void* data)
{
	if (wnd == _decButton)
	{
		if (msg == BUTTON_CLICKED || msg == BUTTON_DOWN)
		{
			TryScrollDec();
		}
	}
	else if (wnd == _incButton)
	{
		if (msg == BUTTON_CLICKED || msg == BUTTON_DOWN)
		{
			TryScrollInc();
		}
	}
	else if (wnd == _scrollBox || wnd == _fixedThumb)
	{
		if (msg == SCROLLBOX_MOVE)
		{
			HandleThumbMove();
		}
	}
	ScrollBarBase::SendMessage(wnd, msg, data);
}

void CUIScrollBar::TryScrollInc(bool byScrollbox)
{
	if (ScrollInc(byScrollbox))
	{
		NotifyScrollChanged();
	}
}

void CUIScrollBar::TryScrollDec(bool byScrollbox)
{
	if (ScrollDec(byScrollbox))
	{
		NotifyScrollChanged();
	}
}

bool CUIScrollBar::ScrollDec(bool byScrollbox)
{
	if (_scrollPos > _minPos)
	{
		if (_scrollPos > _stepSize)
		{
			if (byScrollbox)
			{
				SetScrollPos(_scrollPos - _stepSize * 4);
			}
			else
			{
				SetScrollPos(_scrollPos - _stepSize);
			}
		}
		else
		{
			SetScrollPos(0);
		}
		return true;
	}
	return false;
}

bool CUIScrollBar::ScrollInc(bool byScrollbox)
{
	if (_scrollPos <= (_maxPos - _pageSize + 1))
	{
		if (byScrollbox)
		{
			SetScrollPos(_scrollPos + _stepSize * 4);
		}
		else
		{
			SetScrollPos(_scrollPos + _stepSize);
		}
		return true;
	}
	return false;
}

void CUIScrollBar::Reset()
{
	ResetAll();
	ScrollBarBase::Reset();
}

bool CUIScrollBar::IsRelevant() const
{
	const bool canInc = (_scrollPos <= (_maxPos - _pageSize));
	const bool canDec = (_scrollPos > _minPos);
	return canInc || canDec;
}

void CUIScrollBar::Draw()
{
	if (_frameBackground->IsShown())
	{
		const float trackMain = mainBarSpan() - GetDecSpan() - GetIncSpan();
		if (_isHorizontal)
		{
			_frameBackground->SetWndSize(Fvector2().set(trackMain, GetHeight()));
			_frameBackground->SetWndPos(Fvector2().set(GetDecSpan(), 0.0f));
		}
		else
		{
			_frameBackground->SetWndSize(Fvector2().set(GetWidth(), trackMain));
			_frameBackground->SetWndPos(Fvector2().set(0.0f, GetDecSpan()));
		}
	}
	ScrollBarBase::Draw();
}

void CUIScrollBar::SyncThumbFromScrollPos()
{
	CUIWindow* thumb = GetThumbWindow();
	if (thumb)
	{
		SendMessage(thumb, SCROLLBOX_MOVE, nullptr);
	}
}

bool CUIScrollBar::InitForProfile(CUIScrollBar& bar, Fvector2 pos, float stretchLength, bool isHorizontal, const char* profile)
{
	const char* profileName = (profile && profile[0]) ? profile : "default";

	ScrollLayoutMode layoutMode = ScrollLayoutMode::Stretch;
	if (QueryProfileLayout(profileName, isHorizontal, layoutMode) && layoutMode == ScrollLayoutMode::Fixed)
	{
		if (bar.InitScrollBar(pos, isHorizontal, profileName))
		{
			return true;
		}
	}

	return bar.InitScrollBar(pos, stretchLength, isHorizontal, profileName);
}
