#include "stdafx.h"
#include "UIScrollBar.h"
#include "UI3tButton.h"
#include "UIScrollBox.h"
#include "UIStatic.h"
#include "UIFrameLineWnd.h"
#include "UIXmlInit.h"
#include "../UICursor.h"
#include "../../xrEngine/xr_input.h"

namespace ScrollBarStyledXml
{
bool ParseColor(const char* Value, u32& Out)
{
	if (!Value || !Value[0])
	{
		return false;
	}

	int Red = 255;
	int Green = 255;
	int Blue = 255;
	int Alpha = 255;
	const int Count = sscanf(Value, "%d,%d,%d,%d", &Red, &Green, &Blue, &Alpha);
	if (Count < 3)
	{
		return false;
	}
	Out = color_rgba(u32(clampr(Red, 0, 255)), u32(clampr(Green, 0, 255)), u32(clampr(Blue, 0, 255)), u32(clampr(Alpha, 0, 255)));
	return true;
}

void ReadColors(CUIXml& Xml, const char* Path, ScrollBarStateColors& Out)
{
	Out = ScrollBarStateColors{};
	if (!Path || !Path[0])
	{
		return;
	}

	ParseColor(Xml.ReadAttrib(Path, 0, "color", nullptr), Out.Normal);
	Out.Hover = Out.Normal;
	ParseColor(Xml.ReadAttrib(Path, 0, "color_hover", nullptr), Out.Hover);
	Out.Pressed = Out.Hover;
	ParseColor(Xml.ReadAttrib(Path, 0, "color_pressed", nullptr), Out.Pressed);
	Out.Disabled = subst_alpha(Out.Normal, color_get_A(Out.Normal) * 200 / 255);
	ParseColor(Xml.ReadAttrib(Path, 0, "color_disabled", nullptr), Out.Disabled);
}

u32 ScaleAlpha(u32 Color, float Factor)
{
	return subst_alpha(Color, u32(clampr(iFloor(float(color_get_A(Color)) * Factor + 0.5f), 0, 255)));
}
}

CUIWindow* ScrollBarSkinPart::Window() const
{
	if (Static)
	{
		return Static;
	}
	return Frame;
}

void ScrollBarSkinPart::SetColor(u32 Color) const
{
	if (Static)
	{
		Static->SetTextureColor(Color);
	}
	if (Frame)
	{
		Frame->SetTextureColor(Color);
	}
}

void ScrollBarSkinPart::Destroy(CUIWindow* Owner)
{
	if (CUIWindow* Wnd = Window())
	{
		Owner->DetachChild(Wnd);
	}
	Static = nullptr;
	Frame = nullptr;
}

bool CUIScrollBar::ResolveStyledPath(CUIXml& xmlDoc, const char* profile, const char* axisNode, const char* part, string_path& outPath) const
{
	xr_strconcat(outPath, profile, ":", axisNode, ":", part);
	if (xmlDoc.NavigateToNode(outPath, 0))
	{
		return true;
	}
	xr_strconcat(outPath, profile, ":", part);
	if (xmlDoc.NavigateToNode(outPath, 0))
	{
		return true;
	}
	outPath[0] = 0;
	return false;
}

void CUIScrollBar::ParseStyle(CUIXml& xmlDoc, const char* profile, const char* axisPath)
{
	const auto attrib = [&xmlDoc, profile, axisPath](const char* name) -> const char*
	{
		if (axisPath)
		{
			if (const char* value = xmlDoc.ReadAttrib(axisPath, 0, name, nullptr))
			{
				return value;
			}
		}
		return xmlDoc.ReadAttrib(profile, 0, name, nullptr);
	};
	const auto readFloat = [&attrib](const char* name, float fallback)
	{
		const char* value = attrib(name);
		return value && value[0] ? float(atof(value)) : fallback;
	};

	_style = ScrollBarStyle{};
	_style.Thickness = readFloat("thickness", _profileConfig.thickness > 0.0f ? _profileConfig.thickness : _style.Thickness);
	_style.HoverThickness = readFloat("hover_thickness", 0.0f);
	_style.TrackCross = readFloat("track_cross", 0.0f);
	_style.ThumbCross = readFloat("thumb_cross", 0.0f);
	_style.ThumbMargin = readFloat("thumb_margin", 0.0f);
	_style.TrackInset = readFloat("track_inset", 0.0f);
	_style.MinThumb = readFloat("min_thumb", _style.MinThumb);
	_style.WheelLines = readFloat("wheel_lines", _style.WheelLines);
	_style.IdleAlpha = clampr(readFloat("idle_alpha", 1.0f), 0.0f, 1.0f);
	_style.FadeSpeed = readFloat("fade_speed", _style.FadeSpeed);
	_style.ResizeSpeed = readFloat("resize_speed", _style.ResizeSpeed);
	_style.IdleDelay = u32(readFloat("idle_delay", float(_style.IdleDelay)));
	_style.RepeatDelay = u32(readFloat("repeat_delay", float(_style.RepeatDelay)));

	const char* trackClick = attrib("track_click");
	_style.TrackClick = trackClick && 0 == _stricmp(trackClick, "jump") ? ScrollTrackClick::Jump : ScrollTrackClick::Page;

	const char* crossAlign = attrib("cross_align");
	if (crossAlign && 0 == _stricmp(crossAlign, "near"))
	{
		_style.CrossAlign = ScrollCrossAlign::Near;
	}
	else if (crossAlign && 0 == _stricmp(crossAlign, "center"))
	{
		_style.CrossAlign = ScrollCrossAlign::Center;
	}
	else
	{
		_style.CrossAlign = ScrollCrossAlign::Far;
	}
}

bool CUIScrollBar::InitStyledPart(CUIXml& xmlDoc, const char* path, ScrollBarSkinPart& part)
{
	string256 texturePath;
	xr_strconcat(texturePath, path, ":texture");
	const char* texture = xmlDoc.Read(texturePath, 0, nullptr);
	if (!texture || !texture[0])
	{
		return false;
	}

	if (IsFrameLineTextureBase(texture))
	{
		CUIFrameLineWnd* frame = new CUIFrameLineWnd();
		frame->SetAutoDelete(true);
		AttachChild(frame);
		if (!CUIXmlInit::InitFrameLine(xmlDoc, path, 0, frame, false))
		{
			DetachChild(frame);
			return false;
		}
		frame->SetHorizontal(_isHorizontal);
		frame->Enable(false);
		part.Frame = frame;
		return true;
	}

	CUIStatic* image = new CUIStatic();
	image->SetAutoDelete(true);
	AttachChild(image);
	if (!CUIXmlInit::InitStatic(xmlDoc, path, 0, image, false))
	{
		DetachChild(image);
		return false;
	}
	image->SetStretchTexture(true);
	image->Enable(false);
	part.Static = image;
	return true;
}

bool CUIScrollBar::InitStyledLayout(CUIXml& xmlDoc, const char* profile, Fvector2 pos, float length, bool isHorizontal)
{
	_layoutMode = ScrollLayoutMode::Styled;
	_profileConfig.layoutMode = ScrollLayoutMode::Styled;
	_isHorizontal = isHorizontal;
	_holdDelay = _profileConfig.holdDelay;
	_scrollBoxOffset = Ivector2{};
	ResetPartFlags();
	StopStyledDrag();
	_holdButtonDir = 0;
	_holdTrackDir = 0;

	detachFixedThumbChild();
	_scrollBox->Show(false);
	_frameBackground->Show(false);
	_styledTrack.Destroy(this);
	_styledThumb.Destroy(this);

	const char* axisNode = isHorizontal ? "horizontal" : "vertical";
	string_path axisPath;
	xr_strconcat(axisPath, profile, ":", axisNode);
	ParseStyle(xmlDoc, profile, xmlDoc.NavigateToNode(axisPath, 0) ? axisPath : nullptr);

	const float crossSize = std::max(_style.Thickness, _style.HoverThickness);
	R_ASSERT2(crossSize > 0.0f, profile);
	_profileConfig.thickness = crossSize;

	ScrollBarBase::SetWndPos(pos);
	ScrollBarBase::SetWndSize(isHorizontal ? Fvector2().set(length, crossSize) : Fvector2().set(crossSize, length));

	string_path partPath;
	_decButton->Show(false);
	if (ResolveStyledPath(xmlDoc, profile, axisNode, "dec", partPath) && CUIXmlInit::Init3tButton(xmlDoc, partPath, 0, _decButton, false))
	{
		_decButton->Show(true);
		_partFlags.hasDec = true;
	}

	_incButton->Show(false);
	if (ResolveStyledPath(xmlDoc, profile, axisNode, "inc", partPath) && CUIXmlInit::Init3tButton(xmlDoc, partPath, 0, _incButton, false))
	{
		_incButton->Show(true);
		_partFlags.hasInc = true;
	}

	if (ResolveStyledPath(xmlDoc, profile, axisNode, "track", partPath) && InitStyledPart(xmlDoc, partPath, _styledTrack))
	{
		ScrollBarStyledXml::ReadColors(xmlDoc, partPath, _style.TrackColors);
		_partFlags.hasTrack = true;
	}

	if (ResolveStyledPath(xmlDoc, profile, axisNode, "thumb", partPath) && InitStyledPart(xmlDoc, partPath, _styledThumb))
	{
		ScrollBarStyledXml::ReadColors(xmlDoc, partPath, _style.ThumbColors);
		_partFlags.hasThumb = true;
	}

	_styledCross = _style.Thickness;
	_styledAlpha = 1.0f;
	_styledActivityTime = Device.dwTimeGlobal;

	RecalcWorkArea(crossSize);
	LayoutStyled();
	ApplyStyledColors();
	return _partFlags.anyPart();
}

float CUIScrollBar::StyledTrackStart() const
{
	return GetDecSpan() + _style.TrackInset;
}

float CUIScrollBar::StyledTrackLength() const
{
	return std::max(0.0f, mainBarSpan() - GetDecSpan() - GetIncSpan() - 2.0f * _style.TrackInset);
}

int CUIScrollBar::StyledScrollSpan() const
{
	return std::max(0, (_maxPos - _pageSize + 1) - _minPos);
}

float CUIScrollBar::StyledThumbLength() const
{
	const float track = StyledTrackLength();
	const float content = float(std::max(1, _maxPos - _minPos));
	const float ratio = _pageSize > 0 ? clampr(float(_pageSize) / content, 0.0f, 1.0f) : 1.0f;
	return clampr(std::max(track * ratio, _style.MinThumb), 0.0f, track);
}

float CUIScrollBar::StyledThumbStart() const
{
	const int span = StyledScrollSpan();
	const float travel = StyledTrackLength() - StyledThumbLength();
	const float progress = span > 0 ? clampr(float(_scrollPos - _minPos) / float(span), 0.0f, 1.0f) : 0.0f;
	return StyledTrackStart() + travel * progress;
}

void CUIScrollBar::SetStyledThumbStart(float start)
{
	const int span = StyledScrollSpan();
	const float travel = StyledTrackLength() - StyledThumbLength();
	if (span <= 0 || travel <= EPS_L)
	{
		return;
	}

	const float progress = clampr((start - StyledTrackStart()) / travel, 0.0f, 1.0f);
	ScrollByAmount(_minPos + iFloor(progress * float(span) + 0.5f) - _scrollPos);
}

float CUIScrollBar::StyledCrossOffset(float size) const
{
	const float cross = crossBarSpan();
	switch (_style.CrossAlign)
	{
	case ScrollCrossAlign::Near:
		return 0.0f;
	case ScrollCrossAlign::Center:
		return (cross - size) * 0.5f;
	default:
		return cross - size;
	}
}

float CUIScrollBar::CursorAlong()
{
	Frect rect;
	GetAbsoluteRect(rect);
	const Fvector2 cursor = GetUICursor().GetCursorPosition();
	return _isHorizontal ? cursor.x - rect.x1 : cursor.y - rect.y1;
}

void CUIScrollBar::LayoutStyled()
{
	const float scale = _style.Thickness > EPS_L ? _styledCross / _style.Thickness : 1.0f;
	const float trackCross = (_style.TrackCross > 0.0f ? _style.TrackCross : _style.Thickness) * scale;
	const float thumbCross = std::max(1.0f, (_style.ThumbCross > 0.0f ? _style.ThumbCross : _style.Thickness - 2.0f * _style.ThumbMargin) * scale);
	const float bandCross = std::max(trackCross, thumbCross);
	const float bandOffset = StyledCrossOffset(bandCross);

	const auto place = [this](CUIWindow* wnd, float along, float cross, float alongSize, float crossSize)
	{
		if (!wnd)
		{
			return;
		}
		if (_isHorizontal)
		{
			wnd->SetWndPos(Fvector2().set(along, cross));
			wnd->SetWndSize(Fvector2().set(alongSize, crossSize));
		}
		else
		{
			wnd->SetWndPos(Fvector2().set(cross, along));
			wnd->SetWndSize(Fvector2().set(crossSize, alongSize));
		}
	};

	place(_styledTrack.Window(), StyledTrackStart(), bandOffset + (bandCross - trackCross) * 0.5f, StyledTrackLength(), trackCross);
	place(_styledThumb.Window(), StyledThumbStart(), bandOffset + (bandCross - thumbCross) * 0.5f, StyledThumbLength(), thumbCross);

	const auto placeButton = [this](CUI3tButton* button, bool atEnd)
	{
		if (!button->IsShown())
		{
			return;
		}
		const Fvector2 size = button->GetWndSize();
		const float crossSize = _isHorizontal ? size.y : size.x;
		const float alongSize = _isHorizontal ? size.x : size.y;
		const float along = atEnd ? mainBarSpan() - alongSize : 0.0f;
		const float cross = StyledCrossOffset(crossSize);
		button->SetWndPos(_isHorizontal ? Fvector2().set(along, cross) : Fvector2().set(cross, along));
	};
	placeButton(_decButton, false);
	placeButton(_incButton, true);
}

void CUIScrollBar::ApplyStyledColors()
{
	const bool relevant = IsRelevant();
	const ScrollBarStateColors& track = _style.TrackColors;
	const ScrollBarStateColors& thumb = _style.ThumbColors;

	u32 trackColor = !relevant ? track.Disabled : (_styledHover || _styledDragging ? track.Hover : track.Normal);
	u32 thumbColor = !relevant ? thumb.Disabled : (_styledDragging ? thumb.Pressed : (_thumbHover ? thumb.Hover : thumb.Normal));

	_styledTrack.SetColor(ScrollBarStyledXml::ScaleAlpha(trackColor, _styledAlpha));
	_styledThumb.SetColor(ScrollBarStyledXml::ScaleAlpha(thumbColor, _styledAlpha));
}

void CUIScrollBar::ScrollByAmount(int delta)
{
	if (!delta)
	{
		return;
	}

	const int previous = _scrollPos;
	SetScrollPos(_scrollPos + delta);
	if (_scrollPos == previous)
	{
		return;
	}
	_styledActivityTime = Device.dwTimeGlobal;
	NotifyScrollChanged();
}

void CUIScrollBar::PageTowardCursor()
{
	const float along = CursorAlong();
	const float thumbStart = StyledThumbStart();
	const float thumbEnd = thumbStart + StyledThumbLength();
	const int page = std::max(1, _pageSize > 0 ? _pageSize : _stepSize);

	if (_holdTrackDir < 0 && along < thumbStart)
	{
		ScrollByAmount(-page);
	}
	else if (_holdTrackDir > 0 && along > thumbEnd)
	{
		ScrollByAmount(page);
	}
	else
	{
		_holdTrackDir = 0;
	}
}

void CUIScrollBar::StopStyledDrag()
{
	if (!_styledDragging)
	{
		return;
	}
	_styledDragging = false;
	if (GetParent() && GetParent()->GetMouseCapturer() == this)
	{
		GetParent()->SetCapture(this, false);
	}
}

void CUIScrollBar::UpdateStyled()
{
	const u32 now = Device.dwTimeGlobal;
	Fvector2 cursor = GetUICursor().GetCursorPosition();

	Frect barRect;
	GetAbsoluteRect(barRect);
	_styledHover = IsShown() && barRect.in(cursor);

	_thumbHover = false;
	if (CUIWindow* thumb = _styledThumb.Window())
	{
		Frect thumbRect;
		thumb->GetAbsoluteRect(thumbRect);
		_thumbHover = _styledHover && thumbRect.in(cursor);
	}

	if (!pInput->LeftMouseButtonPressed())
	{
		_holdButtonDir = 0;
		_holdTrackDir = 0;
		StopStyledDrag();
	}
	else if (_styledDragging)
	{
		SetStyledThumbStart(CursorAlong() - _dragGrab);
	}
	else if ((_holdButtonDir || _holdTrackDir) && now - _holdStartTime >= _style.RepeatDelay && float(now - _holdLastTime) >= _holdDelay)
	{
		_holdLastTime = now;
		if (_holdButtonDir > 0)
		{
			TryScrollInc();
		}
		else if (_holdButtonDir < 0)
		{
			TryScrollDec();
		}
		else
		{
			PageTowardCursor();
		}
	}

	if (_styledHover || _styledDragging || _holdButtonDir || _holdTrackDir || _scrollPos != _styledLastPos)
	{
		_styledLastPos = _scrollPos;
		_styledActivityTime = now;
	}

	const float delta = Device.fTimeDelta;
	const float targetCross = _style.HoverThickness > 0.0f && (_styledHover || _styledDragging) ? _style.HoverThickness : _style.Thickness;
	_styledCross += (targetCross - _styledCross) * std::min(1.0f, delta * _style.ResizeSpeed);

	const bool active = now - _styledActivityTime < _style.IdleDelay;
	const float targetAlpha = active ? 1.0f : _style.IdleAlpha;
	const float step = _style.FadeSpeed * delta;
	_styledAlpha = targetAlpha > _styledAlpha ? std::min(targetAlpha, _styledAlpha + step) : std::max(targetAlpha, _styledAlpha - step);

	LayoutStyled();
	ApplyStyledColors();
}

bool CUIScrollBar::StyledMouseAction(float x, float y, EUIMessages mouseAction)
{
	if (_styledDragging)
	{
		switch (mouseAction)
		{
		case WINDOW_MOUSE_MOVE:
			SetStyledThumbStart(CursorAlong() - _dragGrab);
			return true;
		case WINDOW_LBUTTON_UP:
		case WINDOW_MOUSE_CAPTURE_LOST:
			StopStyledDrag();
			return true;
		default:
			return true;
		}
	}

	if (mouseAction == WINDOW_LBUTTON_UP)
	{
		_holdButtonDir = 0;
		_holdTrackDir = 0;
	}

	if (mouseAction != WINDOW_LBUTTON_DOWN && mouseAction != WINDOW_LBUTTON_DB_CLICK)
	{
		return ScrollBarBase::OnMouseAction(x, y, mouseAction);
	}

	const float along = CursorAlong();
	const float trackStart = StyledTrackStart();
	const float trackEnd = trackStart + StyledTrackLength();
	const float thumbStart = StyledThumbStart();
	const float thumbLength = StyledThumbLength();
	_styledActivityTime = Device.dwTimeGlobal;

	if (_styledThumb.Window() && along >= thumbStart && along <= thumbStart + thumbLength)
	{
		_dragGrab = along - thumbStart;
	}
	else if (along >= trackStart && along <= trackEnd)
	{
		if (_style.TrackClick == ScrollTrackClick::Page || !_styledThumb.Window())
		{
			_holdTrackDir = along < thumbStart ? -1 : 1;
			_holdStartTime = Device.dwTimeGlobal;
			_holdLastTime = _holdStartTime;
			PageTowardCursor();
			return true;
		}
		_dragGrab = thumbLength * 0.5f;
		SetStyledThumbStart(along - _dragGrab);
	}
	else
	{
		return ScrollBarBase::OnMouseAction(x, y, mouseAction);
	}

	_styledDragging = true;
	if (GetParent())
	{
		GetParent()->SetCapture(this, true);
	}
	return true;
}
