#include "stdafx.h"
#include "UIScrollBar.h"
#include "UI3tButton.h"
#include "UIScrollBox.h"
#include "../UICursor.h"

ScrollHitZone CUIScrollBar::hitTestScrollZone() const
{
	CUIWindow* thumb = GetThumbWindow();
	if (!thumb)
	{
		return ScrollHitZone::None;
	}

	Fvector2 cursorPos = GetUICursor().GetCursorPosition();
	Frect boxRect;
	Frect decRect;
	Frect incRect;
	thumb->GetAbsoluteRect(boxRect);
	_decButton->GetAbsoluteRect(decRect);
	_incButton->GetAbsoluteRect(incRect);

	if (_decButton->IsShown() && decRect.in(cursorPos) && (_mouseState != 2))
	{
		return ScrollHitZone::DecButton;
	}

	if (_incButton->IsShown() && incRect.in(cursorPos) && (_mouseState != 1))
	{
		return ScrollHitZone::IncButton;
	}

	Frect dec2Rect;
	Frect inc2Rect;
	if (_isHorizontal)
	{
		dec2Rect.set(decRect.x2, decRect.y1, boxRect.x1, boxRect.y2);
		inc2Rect.set(boxRect.x2, boxRect.y1, incRect.x1, incRect.y2);
	}
	else
	{
		dec2Rect.set(decRect.x1, decRect.y2, boxRect.x2, boxRect.y1);
		inc2Rect.set(boxRect.x1, boxRect.y2, incRect.x2, incRect.y1);
	}

	if (_decButton->IsShown() && dec2Rect.in(cursorPos) && (_mouseState != 2))
	{
		return ScrollHitZone::TrackBefore;
	}

	if (_incButton->IsShown() && inc2Rect.in(cursorPos) && (_mouseState != 1))
	{
		return ScrollHitZone::TrackAfter;
	}

	return ScrollHitZone::None;
}

bool CUIScrollBar::applyHitZone(ScrollHitZone zone)
{
	switch (zone)
	{
	case ScrollHitZone::DecButton:
		TryScrollDec();
		_mouseState = 1;
		return true;
	case ScrollHitZone::IncButton:
		TryScrollInc();
		_mouseState = 2;
		return true;
	case ScrollHitZone::TrackBefore:
		TryScrollDec(true);
		_mouseState = 1;
		return true;
	case ScrollHitZone::TrackAfter:
		TryScrollInc(true);
		_mouseState = 2;
		return true;
	default:
		return false;
	}
}

bool CUIScrollBar::OnKeyboardHold(int dik)
{
	if (dik == MOUSE_1 && (_lastHoldTimeMs + _holdDelay) < Device.dwTimeContinual)
	{
		if (OnMouseDownEx())
		{
			_lastHoldTimeMs = Device.dwTimeContinual;
			return true;
		}
	}
	return ScrollBarBase::OnKeyboardHold(dik);
}

bool CUIScrollBar::handleFixedLayoutMouseAction(CUIWindow* thumb, EUIMessages mouseAction)
{
	switch (mouseAction)
	{
	case WINDOW_LBUTTON_UP:
		SetCapture(thumb, false);
		_mouseState = 0;
		return true;
	case WINDOW_LBUTTON_DOWN:
		SetCapture(thumb, true);
		return true;
	case WINDOW_MOUSE_MOVE:
	{
		const bool imCapturer = (GetMouseCapturer() == thumb);
		Fvector2 cursorPos = GetUICursor().GetCursorPosition();
		Frect boxRect;
		thumb->GetAbsoluteRect(boxRect);
		const bool cursorOver = boxRect.in(cursorPos);

		if (imCapturer && cursorOver)
		{
			Fvector2 thumbPos = thumb->GetWndPos();
			Fvector2 delta = GetUICursor().GetCursorPositionDelta();
			if (_isHorizontal)
			{
				thumbPos.x += delta.x;
			}
			else
			{
				thumbPos.y += delta.y;
			}
			thumb->SetWndPos(thumbPos);
			HandleThumbMove();
		}
		if (!cursorOver)
		{
			SetCapture(thumb, false);
		}
		return true;
	}
	default:
		return false;
	}
}

bool CUIScrollBar::OnMouseAction(float x, float y, EUIMessages mouseAction)
{
	switch (mouseAction)
	{
	case WINDOW_MOUSE_WHEEL_DOWN:
		TryScrollInc(true);
		return true;
	case WINDOW_MOUSE_WHEEL_UP:
		TryScrollDec(true);
		return true;
	default:
		break;
	}

	CUIWindow* thumb = GetThumbWindow();
	if (_layoutMode == ScrollLayoutMode::Fixed && thumb)
	{
		if (handleFixedLayoutMouseAction(thumb, mouseAction))
		{
			return true;
		}
	}

	if (mouseAction == WINDOW_LBUTTON_UP)
	{
		_mouseState = 0;
	}

	return ScrollBarBase::OnMouseAction(x, y, mouseAction);
}

bool CUIScrollBar::OnMouseDown(int mouseBtn)
{
	if (mouseBtn == MOUSE_1 && OnMouseDownEx())
	{
		return true;
	}
	return ScrollBarBase::OnMouseDown(mouseBtn);
}

bool CUIScrollBar::OnMouseDownEx()
{
	return applyHitZone(hitTestScrollZone());
}

void CUIScrollBar::OnMouseUp(int mouseBtn)
{
	(void)mouseBtn;
	_mouseState = 0;
}

void CUIScrollBar::ClampByViewRect()
{
	CUIWindow* thumb = GetThumbWindow();
	if (!thumb)
	{
		return;
	}

	const float decEdge = GetDecSpan() + scrollBoxInset();
	const float incEdge = _incButton->IsShown()
		? (_isHorizontal ? _incButton->GetWndPos().x : _incButton->GetWndPos().y) - scrollBoxInset()
		: mainBarSpan();

	const Frect thumbRect = thumb->GetWndRect();
	if (_isHorizontal)
	{
		if (thumbRect.left <= decEdge)
		{
			thumb->SetWndPos(Fvector2().set(decEdge, thumbRect.top));
		}
		else if (thumbRect.right >= incEdge)
		{
			thumb->SetWndPos(Fvector2().set(incEdge - thumb->GetWidth(), thumbRect.top));
		}
	}
	else
	{
		if (thumbRect.top <= decEdge)
		{
			thumb->SetWndPos(Fvector2().set(thumbRect.left, decEdge));
		}
		else if (thumbRect.bottom >= incEdge)
		{
			thumb->SetWndPos(Fvector2().set(thumbRect.left, incEdge - thumb->GetHeight()));
		}
	}
}

void CUIScrollBar::HandleThumbMove()
{
	CUIWindow* thumb = GetThumbWindow();
	if (!thumb)
	{
		return;
	}

	ClampByViewRect();
	if (_isHorizontal)
	{
		SetPosScrollFromView(thumb->GetWndPos().x, thumb->GetWidth(), thumbViewOffset());
	}
	else
	{
		SetPosScrollFromView(thumb->GetWndPos().y, thumb->GetHeight(), thumbViewOffset());
	}
	NotifyScrollChanged();
}
