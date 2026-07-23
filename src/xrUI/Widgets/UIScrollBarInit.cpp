#include "stdafx.h"
#include "UIScrollBar.h"
#include "UI3tButton.h"
#include "UIScrollBox.h"
#include "UIXmlInit.h"
#include "UIStatic.h"
#include "UIFrameLineWnd.h"
#include "UITextureMaster.h"

namespace
{
bool LayoutFromAttrib(const char* layoutStr, ScrollLayoutMode& outMode)
{
	if (!layoutStr || !layoutStr[0])
	{
		return false;
	}
	if (0 == _stricmp(layoutStr, "stretch"))
	{
		outMode = ScrollLayoutMode::Stretch;
		return true;
	}
	if (0 == _stricmp(layoutStr, "fixed"))
	{
		outMode = ScrollLayoutMode::Fixed;
		return true;
	}
	return false;
}

bool ThumbFromAttrib(const char* thumbStr, bool isFixedLayout, bool& thumbAsButton)
{
	if (!thumbStr || !thumbStr[0] || 0 == _stricmp(thumbStr, "auto"))
	{
		thumbAsButton = isFixedLayout;
		return true;
	}
	if (0 == _stricmp(thumbStr, "button"))
	{
		thumbAsButton = true;
		return true;
	}
	if (0 == _stricmp(thumbStr, "box"))
	{
		thumbAsButton = false;
		return true;
	}
	return false;
}

bool IsFrameLineTextureBase(const char* textureBase)
{
	if (!textureBase || !textureBase[0])
	{
		return false;
	}

	string256 backName;
	string256 beginName;
	string256 endName;
	xr_strconcat(backName, textureBase, "_back");
	xr_strconcat(beginName, textureBase, "_b");
	xr_strconcat(endName, textureBase, "_e");
	return CUITextureMaster::ItemExist(backName)
		&& CUITextureMaster::ItemExist(beginName)
		&& CUITextureMaster::ItemExist(endName);
}

bool ReadPartTextureName(CUIXml& xmlDoc, const char* nodePath, string256& outTexture)
{
	string256 texturePath;
	xr_strconcat(texturePath, nodePath, ":texture");
	const char* texture = xmlDoc.Read(texturePath, 0, nullptr);
	if (!texture || !texture[0])
	{
		outTexture[0] = 0;
		return false;
	}

	xr_strcpy(outTexture, texture);
	return true;
}

bool NodeHasButtonTextures(CUIXml& xmlDoc, const char* nodePath)
{
	string256 texturePath;
	xr_strconcat(texturePath, nodePath, ":texture_e");
	const char* texture = xmlDoc.Read(texturePath, 0, nullptr);
	return texture && texture[0];
}
} // namespace

bool CUIScrollBar::QueryProfileLayout(const char* profile, bool isHorizontal, ScrollLayoutMode& outMode)
{
	CUIXml xmlDoc;
	if (!xmlDoc.Load(CONFIG_PATH, UI_PATH, "scroll_bar.xml"))
	{
		return false;
	}

	ScrollBarProfileConfig config{};
	if (!ParseProfile(xmlDoc, profile, isHorizontal, config))
	{
		return false;
	}

	outMode = config.layoutMode;
	return true;
}

bool CUIScrollBar::ParseProfile(CUIXml& xmlDoc, const char* profile, bool isHorizontal, ScrollBarProfileConfig& out)
{
	if (!xmlDoc.NavigateToNode(profile, 0))
	{
		return false;
	}

	out = ScrollBarProfileConfig{};

	const char* layoutStr = xmlDoc.ReadAttrib(profile, 0, "layout", "auto");
	ScrollLayoutMode explicitMode = ScrollLayoutMode::Stretch;
	const bool hasExplicitLayout = LayoutFromAttrib(layoutStr, explicitMode);

	const float width = xmlDoc.ReadAttribFlt(profile, 0, "width", 0.0f);
	const float height = xmlDoc.ReadAttribFlt(profile, 0, "height", 0.0f);
	const float widthV = xmlDoc.ReadAttribFlt(profile, 0, "width_v", 0.0f);
	const float heightV = xmlDoc.ReadAttribFlt(profile, 0, "height_v", 0.0f);

	if (hasExplicitLayout)
	{
		out.layoutMode = explicitMode;
	}
	else if (isHorizontal)
	{
		out.layoutMode = (width > 0.0f && height > 0.0f) ? ScrollLayoutMode::Fixed : ScrollLayoutMode::Stretch;
	}
	else
	{
		out.layoutMode = (widthV > 0.0f && heightV > 0.0f) ? ScrollLayoutMode::Fixed : ScrollLayoutMode::Stretch;
	}

	out.thickness = xmlDoc.ReadAttribFlt(profile, 0, isHorizontal ? "height" : "height_v", 0.0f);
	// SoC default often has only `height` (shared thickness for both axes).
	if (out.thickness <= 0.0f && !isHorizontal)
	{
		out.thickness = height;
	}
	if (out.thickness <= 0.0f && isHorizontal)
	{
		out.thickness = heightV > 0.0f ? heightV : height;
	}
	if (out.thickness <= 0.0f)
	{
		out.thickness = (out.layoutMode == ScrollLayoutMode::Fixed) ? 17.0f : 16.0f;
	}

	out.holdDelay = xmlDoc.ReadAttribFlt(profile, 0, "hold_delay", 50.0f);
	out.scrollBoxOffset.x = xmlDoc.ReadAttribInt(profile, 0, "scroll_box_offset_x", 0);
	out.scrollBoxOffset.y = xmlDoc.ReadAttribInt(profile, 0, "scroll_box_offset_y", 0);

	const char* thumbStr = xmlDoc.ReadAttrib(profile, 0, "thumb", "auto");
	ThumbFromAttrib(thumbStr, out.layoutMode == ScrollLayoutMode::Fixed, out.thumbAsButton);

	return true;
}

bool CUIScrollBar::ResolvePartPath(CUIXml& xmlDoc, const char* profile, ScrollBarPart part, bool isHorizontal, string_path& outPath) const
{
	static const char* kDecV[] = { "up_arrow", "dec", "decrease", nullptr };
	static const char* kIncV[] = { "down_arrow", "inc", "increase", nullptr };
	static const char* kDecH[] = { "left_arrow", "dec", "decrease", nullptr };
	static const char* kIncH[] = { "right_arrow", "inc", "increase", nullptr };
	static const char* kTrackV[] = { "back_v", "track", "back", nullptr };
	static const char* kTrackH[] = { "back", "track", nullptr };
	static const char* kThumbV[] = { "box_v", "thumb", "box", nullptr };
	static const char* kThumbH[] = { "box", "thumb", nullptr };

	const char** names = nullptr;
	switch (part)
	{
	case ScrollBarPart::Dec:
		names = isHorizontal ? kDecH : kDecV;
		break;
	case ScrollBarPart::Inc:
		names = isHorizontal ? kIncH : kIncV;
		break;
	case ScrollBarPart::Track:
		names = isHorizontal ? kTrackH : kTrackV;
		break;
	case ScrollBarPart::Thumb:
		names = isHorizontal ? kThumbH : kThumbV;
		break;
	}

	for (int i = 0; names[i] != nullptr; ++i)
	{
		xr_strconcat(outPath, profile, ":", names[i]);
		if (xmlDoc.NavigateToNode(outPath, 0))
		{
			return true;
		}
	}

	outPath[0] = 0;
	return false;
}

bool CUIScrollBar::LoadScrollBarXml(CUIXml& xmlDoc, const char* profile)
{
	return xmlDoc.Load(CONFIG_PATH, UI_PATH, "scroll_bar.xml") && xmlDoc.NavigateToNode(profile, 0);
}

void CUIScrollBar::ResetPartFlags()
{
	_partFlags = ScrollBarPartFlags{};
}

void CUIScrollBar::detachFixedThumbChild()
{
	if (IsChild(_fixedThumb))
	{
		DetachChild(_fixedThumb);
	}
	_fixedThumb->Show(false);
}

void CUIScrollBar::prepareFixedLayoutChildren()
{
	_scrollBox->Show(false);
	detachFixedThumbChild();
}

void CUIScrollBar::PositionIncButton(float anchorLength)
{
	if (!_incButton->IsShown())
	{
		return;
	}

	if (_isHorizontal)
	{
		const float x = anchorLength > 0.0f ? anchorLength - _incButton->GetWidth() : GetWidth() - _incButton->GetWidth();
		_incButton->SetWndPos(Fvector2().set(x, 0.0f));
	}
	else
	{
		const float y = anchorLength > 0.0f ? anchorLength - _incButton->GetHeight() : GetHeight() - _incButton->GetHeight();
		_incButton->SetWndPos(Fvector2().set(0.0f, y));
	}
}

void CUIScrollBar::ApplyStaticThumbHack(CUIStatic* tempStatic, CUIWindow* targetWnd)
{
	if (!tempStatic || !tempStatic->IsShown() || !targetWnd)
	{
		return;
	}

	CUIFrameLineWnd* frame = smart_cast<CUIFrameLineWnd*>(targetWnd);
	if (!frame)
	{
		return;
	}

	const Frect texRect = tempStatic->GetTextureRect();
	Fvector2 size = tempStatic->GetWndSize();
	if (size.x <= 0.0f)
	{
		size.x = texRect.width() > 0.0f ? texRect.width() : crossBarSpan();
	}
	if (size.y <= 0.0f)
	{
		size.y = texRect.height() > 0.0f ? texRect.height() : crossBarSpan();
	}

	if (_isHorizontal)
	{
		size.y = GetHeight();
	}
	else
	{
		size.x = GetWidth();
	}

	frame->InitFrameLineWnd(tempStatic->GetWndPos(), size, _isHorizontal);
	frame->SetShader(tempStatic->GetShader());
	frame->SetTextureRect(texRect, CUIFrameLineWnd::flBack);
	frame->SetTextureRect({ 0, 0, 0, 0 }, CUIFrameLineWnd::flFirst);
	frame->SetTextureRect({ 0, 0, 0, 0 }, CUIFrameLineWnd::flSecond);
	frame->SetTextureVisible(true);
	frame->Show(true);
}

bool CUIScrollBar::InitFramePartFromSingleTexture(CUIFrameLineWnd* frame, const char* textureName, Fvector2 pos, Fvector2 size)
{
	if (!frame || !textureName || !textureName[0])
	{
		return false;
	}

	ui_shader shader;
	Frect texRect;
	if (!CUITextureMaster::InitTexture(textureName, "hud\\default", shader, texRect, false))
	{
		return false;
	}

	if (size.x <= 0.0f)
	{
		size.x = texRect.width() > 0.0f ? texRect.width() : crossBarSpan();
	}
	if (size.y <= 0.0f)
	{
		size.y = texRect.height() > 0.0f ? texRect.height() : crossBarSpan();
	}

	frame->InitFrameLineWnd(pos, size, _isHorizontal);
	frame->SetShader(shader);
	frame->SetTextureRect(texRect, CUIFrameLineWnd::flBack);
	frame->SetTextureRect({ 0, 0, 0, 0 }, CUIFrameLineWnd::flFirst);
	frame->SetTextureRect({ 0, 0, 0, 0 }, CUIFrameLineWnd::flSecond);
	frame->SetTextureVisible(true);
	frame->Show(true);
	return true;
}

void CUIScrollBar::ApplyStaticTrackHack(CUIStatic* tempBackground)
{
	if (!tempBackground || !tempBackground->IsShown())
	{
		return;
	}

	if (_isHorizontal)
	{
		SetHeight(tempBackground->GetHeight());
	}
	else
	{
		SetWidth(tempBackground->GetWidth());
	}

	_frameBackground->InitFrameLineWnd(Fvector2().set(0.0f, 0.0f), GetWndSize(), _isHorizontal);
	_frameBackground->SetShader(tempBackground->GetShader());
	_frameBackground->SetTextureRect(tempBackground->GetTextureRect(), CUIFrameLineWnd::flBack);
	_frameBackground->SetTextureRect({ 0, 0, 0, 0 }, CUIFrameLineWnd::flFirst);
	_frameBackground->SetTextureRect({ 0, 0, 0, 0 }, CUIFrameLineWnd::flSecond);
	_frameBackground->SetTextureVisible(true);
	_frameBackground->Show(true);
}

bool CUIScrollBar::InitThumbAsBox(CUIXml& xmlDoc, const char* nodePath)
{
	_fixedThumb->Show(false);
	if (IsChild(_fixedThumb))
	{
		DetachChild(_fixedThumb);
	}

	_scrollBox->SetHorizontal(_isHorizontal);

	string256 textureName;
	const bool hasTexture = ReadPartTextureName(xmlDoc, nodePath, textureName);
	const bool preferFrameLine = hasTexture && IsFrameLineTextureBase(textureName);

	if (preferFrameLine && CUIXmlInit::InitFrameLine(xmlDoc, nodePath, 0, _scrollBox, false))
	{
		_scrollBox->Show(true);
		_partFlags.hasThumb = true;
		return true;
	}

	if (hasTexture)
	{
		const float width = xmlDoc.ReadAttribFlt(nodePath, 0, "width", 0.0f);
		const float height = xmlDoc.ReadAttribFlt(nodePath, 0, "height", 0.0f);
		const float x = xmlDoc.ReadAttribFlt(nodePath, 0, "x", 0.0f);
		const float y = xmlDoc.ReadAttribFlt(nodePath, 0, "y", 0.0f);
		if (InitFramePartFromSingleTexture(_scrollBox, textureName, Fvector2().set(x, y), Fvector2().set(width, height)))
		{
			_partFlags.hasThumb = true;
			return true;
		}
	}

	if (xmlDoc.NavigateToNode(nodePath, 0))
	{
		// HACK: SoC/CS static thumb fallback
		CUIStatic* tempStatic = new CUIStatic();
		bool ok = false;
		if (CUIXmlInit::InitStatic(xmlDoc, nodePath, 0, tempStatic, false))
		{
			tempStatic->Show(true);
			ApplyStaticThumbHack(tempStatic, _scrollBox);
			_scrollBox->Show(true);
			_partFlags.hasThumb = true;
			ok = true;
		}
		xr_delete(tempStatic);
		return ok;
	}

	return false;
}

bool CUIScrollBar::TryInitPart(CUIXml& xmlDoc, const char* nodePath, ScrollBarPart part, const ScrollBarProfileConfig& config)
{
	if (!nodePath[0] || !xmlDoc.NavigateToNode(nodePath, 0))
	{
		switch (part)
		{
		case ScrollBarPart::Dec:
			_decButton->Show(false);
			break;
		case ScrollBarPart::Inc:
			_incButton->Show(false);
			break;
		case ScrollBarPart::Track:
			_frameBackground->Show(false);
			break;
		case ScrollBarPart::Thumb:
			_scrollBox->Show(false);
			_fixedThumb->Show(false);
			break;
		}
		return false;
	}

	CUIStatic* tempStatic = nullptr;
	bool ok = false;

	switch (part)
	{
	case ScrollBarPart::Dec:
		ok = CUIXmlInit::Init3tButton(xmlDoc, nodePath, 0, _decButton, false);
		if (ok)
		{
			_decButton->Show(true);
			_partFlags.hasDec = true;
		}
		break;

	case ScrollBarPart::Inc:
		ok = CUIXmlInit::Init3tButton(xmlDoc, nodePath, 0, _incButton, false);
		if (ok)
		{
			_incButton->Show(true);
			_partFlags.hasInc = true;
		}
		break;

	case ScrollBarPart::Track:
	{
		string256 textureName;
		const bool hasTexture = ReadPartTextureName(xmlDoc, nodePath, textureName);
		const bool preferFrameLine = hasTexture && IsFrameLineTextureBase(textureName);

		if (preferFrameLine && CUIXmlInit::InitFrameLine(xmlDoc, nodePath, 0, _frameBackground, false))
		{
			_frameBackground->SetHorizontal(_isHorizontal);
			_frameBackground->Show(true);
			_partFlags.hasTrack = true;
			ok = true;
		}
		else if (hasTexture)
		{
			const float width = xmlDoc.ReadAttribFlt(nodePath, 0, "width", 0.0f);
			const float height = xmlDoc.ReadAttribFlt(nodePath, 0, "height", 0.0f);
			Fvector2 size = Fvector2().set(width, height);
			if (_isHorizontal)
			{
				if (size.y <= 0.0f)
				{
					size.y = GetHeight();
				}
				if (size.x <= 0.0f)
				{
					size.x = GetWidth();
				}
			}
			else
			{
				if (size.x <= 0.0f)
				{
					size.x = GetWidth();
				}
				if (size.y <= 0.0f)
				{
					size.y = GetHeight();
				}
			}

			if (InitFramePartFromSingleTexture(_frameBackground, textureName, Fvector2().set(0.0f, 0.0f), size))
			{
				if (_isHorizontal)
				{
					SetHeight(size.y);
				}
				else
				{
					SetWidth(size.x);
				}
				_partFlags.hasTrack = true;
				ok = true;
			}
		}

		if (!ok)
		{
			// HACK: SoC/CS static track fallback
			tempStatic = new CUIStatic();
			tempStatic->SetWndRect(GetWndRect());
			if (CUIXmlInit::InitStatic(xmlDoc, nodePath, 0, tempStatic, false))
			{
				tempStatic->Show(true);
				ApplyStaticTrackHack(tempStatic);
				_partFlags.hasTrack = true;
				ok = true;
			}
			xr_delete(tempStatic);
		}
		break;
	}

	case ScrollBarPart::Thumb:
		if (config.thumbAsButton && NodeHasButtonTextures(xmlDoc, nodePath))
		{
			if (!IsChild(_fixedThumb))
			{
				AttachChild(_fixedThumb);
			}
			_scrollBox->Show(false);
			ok = CUIXmlInit::Init3tButton(xmlDoc, nodePath, 0, _fixedThumb, false);
			if (ok)
			{
				_fixedThumb->Show(true);
				_partFlags.hasThumb = true;
			}
			else
			{
				ok = InitThumbAsBox(xmlDoc, nodePath);
			}
		}
		else
		{
			ok = InitThumbAsBox(xmlDoc, nodePath);
		}
		break;
	}

	return ok;
}

bool CUIScrollBar::InitPartsFromProfile(CUIXml& xmlDoc, const char* profile, bool isHorizontal, float incAnchorLength)
{
	string_path partPath;

	if (ResolvePartPath(xmlDoc, profile, ScrollBarPart::Dec, isHorizontal, partPath))
	{
		TryInitPart(xmlDoc, partPath, ScrollBarPart::Dec, _profileConfig);
		_decButton->SetWndPos(Fvector2().set(0, 0));
	}
	else
	{
		_decButton->Show(false);
		_partFlags.hasDec = false;
	}

	if (ResolvePartPath(xmlDoc, profile, ScrollBarPart::Inc, isHorizontal, partPath))
	{
		TryInitPart(xmlDoc, partPath, ScrollBarPart::Inc, _profileConfig);
		PositionIncButton(incAnchorLength);
	}
	else
	{
		_incButton->Show(false);
		_partFlags.hasInc = false;
	}

	if (ResolvePartPath(xmlDoc, profile, ScrollBarPart::Thumb, isHorizontal, partPath))
	{
		TryInitPart(xmlDoc, partPath, ScrollBarPart::Thumb, _profileConfig);
	}

	if (ResolvePartPath(xmlDoc, profile, ScrollBarPart::Track, isHorizontal, partPath))
	{
		TryInitPart(xmlDoc, partPath, ScrollBarPart::Track, _profileConfig);
	}

	return _partFlags.anyPart();
}

bool CUIScrollBar::InitStretchLayout(CUIXml& xmlDoc, const char* profile, Fvector2 pos, float length, bool isHorizontal)
{
	_layoutMode = ScrollLayoutMode::Stretch;
	ResetPartFlags();
	if (!ParseProfile(xmlDoc, profile, isHorizontal, _profileConfig))
	{
		return false;
	}

	_profileConfig.layoutMode = ScrollLayoutMode::Stretch;

	const char* thumbStr = xmlDoc.ReadAttrib(profile, 0, "thumb", "auto");
	ThumbFromAttrib(thumbStr, false, _profileConfig.thumbAsButton);

	_holdDelay = _profileConfig.holdDelay;
	_scrollBoxOffset = _profileConfig.scrollBoxOffset;
	_isHorizontal = isHorizontal;

	detachFixedThumbChild();

	const float thickness = _profileConfig.thickness;
	R_ASSERT(thickness > 0.0f);

	ScrollBarBase::SetWndPos(pos);
	_frameBackground->SetHorizontal(_isHorizontal);

	if (_isHorizontal)
	{
		ScrollBarBase::SetWndSize(Fvector2().set(length, thickness));
	}
	else
	{
		ScrollBarBase::SetWndSize(Fvector2().set(thickness, length));
	}

	const bool hasParts = InitPartsFromProfile(xmlDoc, profile, isHorizontal, length);
	RecalcWorkArea(thickness);
	UpdateScrollBar();

	return hasParts;
}

bool CUIScrollBar::InitFixedLayout(CUIXml& xmlDoc, const char* profile, Fvector2 pos, bool isHorizontal)
{
	_layoutMode = ScrollLayoutMode::Fixed;
	ResetPartFlags();
	if (!ParseProfile(xmlDoc, profile, isHorizontal, _profileConfig))
	{
		return false;
	}

	_profileConfig.layoutMode = ScrollLayoutMode::Fixed;

	_holdDelay = _profileConfig.holdDelay;
	_scrollBoxOffset = _profileConfig.scrollBoxOffset;
	_isHorizontal = isHorizontal;

	prepareFixedLayoutChildren();

	const float width = xmlDoc.ReadAttribFlt(profile, 0, "width", 17.0f);
	const float height = xmlDoc.ReadAttribFlt(profile, 0, "height", 17.0f);
	const float widthV = xmlDoc.ReadAttribFlt(profile, 0, "width_v", 17.0f);
	const float heightV = xmlDoc.ReadAttribFlt(profile, 0, "height_v", 17.0f);

	ScrollBarBase::SetWndPos(pos);

	if (_isHorizontal)
	{
		ScrollBarBase::SetWndSize(Fvector2().set(width, height));
		InitPartsFromProfile(xmlDoc, profile, true, 0.0f);
		RecalcWorkArea(height);
	}
	else
	{
		ScrollBarBase::SetWndSize(Fvector2().set(widthV, heightV));
		InitPartsFromProfile(xmlDoc, profile, false, 0.0f);
		RecalcWorkArea(widthV);
	}

	UpdateScrollBar();

	return _partFlags.anyPart();
}

bool CUIScrollBar::InitScrollBar(Fvector2 pos, float length, bool isHorizontal, const char* profile)
{
	CUIXml xmlDoc;
	if (!LoadScrollBarXml(xmlDoc, profile))
	{
		_initialized = false;
		return false;
	}

	const bool result = InitStretchLayout(xmlDoc, profile, pos, length, isHorizontal);
	_initialized = result;
	return result;
}

bool CUIScrollBar::InitScrollBar(Fvector2 pos, bool isHorizontal, const char* profile)
{
	CUIXml xmlDoc;
	if (!LoadScrollBarXml(xmlDoc, profile))
	{
		_initialized = false;
		return false;
	}

	const bool result = InitFixedLayout(xmlDoc, profile, pos, isHorizontal);
	_initialized = result;
	return result;
}
