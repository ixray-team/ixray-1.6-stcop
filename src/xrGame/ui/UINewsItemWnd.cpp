#include "StdAfx.h"
#include "UINewsItemWnd.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIStackPanel.h"
#include "../game_news.h"
#include "../date_time.h"
#include "UIInventoryUtilities.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrEngine/string_table.h"
#include "../InventoryOwner.h"

namespace
{
constexpr float kItemHeightPadding = 6.f;

bool IsVerticalStackAlignment(CUIStackPanel::EStackPanelAlignment alignment)
{
	return alignment == CUIStackPanel::eTop
		|| alignment == CUIStackPanel::eBottom
		|| alignment == CUIStackPanel::eCenterVert;
}

float CalcVerticalStackContentHeight(CUIStackPanel* stack, float spacing)
{
	if (!stack)
	{
		return 0.f;
	}

	float total = 0.f;
	bool hasChild = false;
	for (CUIWindow* child : stack->GetChildWndList())
	{
		if (!child || !child->IsShown())
		{
			continue;
		}

		if (hasChild)
		{
			total += spacing;
		}
		hasChild = true;
		total += child->GetWndSize().y;
	}
	return total;
}

float CalcHorizontalStackContentHeight(CUIStackPanel* stack)
{
	if (!stack)
	{
		return 0.f;
	}

	float maxHeight = 0.f;
	for (CUIWindow* child : stack->GetChildWndList())
	{
		if (!child || !child->IsShown())
		{
			continue;
		}

		maxHeight = std::max(maxHeight, child->GetWndSize().y);
	}
	return maxHeight;
}

float CalcStackContentHeight(CUIStackPanel* stack, float spacing)
{
	if (!stack)
	{
		return 0.f;
	}

	if (IsVerticalStackAlignment(stack->SPAlignment))
	{
		return CalcVerticalStackContentHeight(stack, spacing);
	}

	return CalcHorizontalStackContentHeight(stack);
}
} // namespace

CUINewsItemWnd::CUINewsItemWnd()
{
}

CUINewsItemWnd::~CUINewsItemWnd()
{
}

void CUINewsItemWnd::Init(CUIXml& uiXml, const char* startFrom, bool allowStackLayout)
{
	CUIXmlInit::InitWindow(uiXml, startFrom, 0, this);

	XML_NODE* storedRoot = uiXml.GetLocalRoot();
	XML_NODE* node = uiXml.NavigateToNode(startFrom, 0);
	uiXml.SetLocalRoot(node);

	if (allowStackLayout)
	{
		const bool hasItemStack = uiXml.NavigateToNode("logs_itm_stack", 0) != nullptr;
		const bool hasRowStack = uiXml.NavigateToNode("logs_row_stack", 0) != nullptr;
		const bool hasTextStack = uiXml.NavigateToNode("logs_text_stack", 0) != nullptr;

		if (hasTextStack && !hasRowStack && !hasItemStack)
		{
			VERIFY2(false, "logs_item: logs_text_stack requires logs_row_stack or logs_itm_stack");
			InitLegacyFromXml(uiXml);
		}
		else if (hasItemStack || hasRowStack)
		{
			if (!InitStackedFromXml(uiXml))
			{
				InitLegacyFromXml(uiXml);
			}
		}
		else
		{
			InitLegacyFromXml(uiXml);
		}
	}
	else
	{
		InitLegacyFromXml(uiXml);
	}

	uiXml.SetLocalRoot(storedRoot);
}

void CUINewsItemWnd::InitLegacyFromXml(CUIXml& uiXml)
{
	_layout = ELayout::Legacy;
	_itemStack = nullptr;
	_rowStack = nullptr;
	_textStack = nullptr;
	CreateNewsStatics(uiXml, this, this, this, this);

	if (uiXml.NavigateToNode("dialog_replica_line", 0))
	{
		_dialogReplicaLine = UIHelper::CreateStatic(uiXml, "dialog_replica_line", this, false);
		_hasDialogReplicaLayout = true;
	}
}

bool CUINewsItemWnd::InitStackedFromXml(CUIXml& uiXml)
{
	const bool hasItemStack = uiXml.NavigateToNode("logs_itm_stack", 0) != nullptr;
	const bool hasRowStack = uiXml.NavigateToNode("logs_row_stack", 0) != nullptr;

	CUIWindow* dateParent = nullptr;
	CUIWindow* captionParent = nullptr;
	CUIWindow* textParent = nullptr;
	CUIWindow* imageParent = nullptr;

	if (hasItemStack)
	{
		_layout = ELayout::SingleStack;
		_itemStack = UIHelper::CreateStackPanel(uiXml, "logs_itm_stack", this, false);
		if (!_itemStack)
		{
			return false;
		}
		_itemStackSpacing = ReadStackSpacing(uiXml, "logs_itm_stack");
		_itemStack->Show(true);
		dateParent = _itemStack;
		captionParent = _itemStack;
		textParent = _itemStack;
		imageParent = _itemStack;
	}
	else if (hasRowStack)
	{
		_layout = ELayout::NestedStack;
		_rowStack = UIHelper::CreateStackPanel(uiXml, "logs_row_stack", this, false);
		if (!_rowStack)
		{
			return false;
		}
		_rowStackSpacing = ReadStackSpacing(uiXml, "logs_row_stack");
		_rowStack->Show(true);
		imageParent = _rowStack;

		if (uiXml.NavigateToNode("image", 0))
		{
			_uiImage = UIHelper::CreateStatic(uiXml, "image", _rowStack, false);
		}

		if (uiXml.NavigateToNode("logs_text_stack", 0))
		{
			_textStack = UIHelper::CreateStackPanel(uiXml, "logs_text_stack", _rowStack, false);
			if (!_textStack)
			{
				return false;
			}
			_textStackSpacing = ReadStackSpacing(uiXml, "logs_text_stack");
			_textStack->Show(true);
			dateParent = _textStack;
			captionParent = _textStack;
			textParent = _textStack;
		}
		else
		{
			dateParent = _rowStack;
			captionParent = _rowStack;
			textParent = _rowStack;
		}
	}
	else
	{
		return false;
	}

	if (_layout != ELayout::NestedStack)
	{
		CreateNewsStatics(uiXml, dateParent, captionParent, textParent, imageParent);
	}
	else
	{
		if (!_uiImage && uiXml.NavigateToNode("image", 0))
		{
			_uiImage = UIHelper::CreateStatic(uiXml, "image", imageParent, false);
		}

		const char* textPath = uiXml.NavigateToNode("text_static", 0) ? "text_static" : "text_cont";
		if (uiXml.NavigateToNode("date_static", 0) || uiXml.NavigateToNode("date_text_cont", 0))
		{
			const char* datePath = uiXml.NavigateToNode("date_static", 0) ? "date_static" : "date_text_cont";
			_uiDate = UIHelper::CreateStatic(uiXml, datePath, dateParent, false);
		}
		if (uiXml.NavigateToNode("caption_static", 0))
		{
			_uiCaption = UIHelper::CreateStatic(uiXml, "caption_static", captionParent, false);
		}
		else
		{
			_legacyMode = true;
		}
		if (uiXml.NavigateToNode(textPath, 0))
		{
			_uiText = UIHelper::CreateStatic(uiXml, textPath, textParent, false);
		}
	}

	if (uiXml.NavigateToNode("dialog_replica_line", 0))
	{
		CUIWindow* replicaParent = this;
		if (_textStack)
		{
			replicaParent = _textStack;
		}
		else if (_itemStack)
		{
			replicaParent = _itemStack;
		}
		_dialogReplicaLine = UIHelper::CreateStatic(uiXml, "dialog_replica_line", replicaParent, false);
		_hasDialogReplicaLayout = true;
	}

	UpdateStackedLayoutHeight();
	return true;
}

void CUINewsItemWnd::CreateNewsStatics(
	CUIXml& uiXml,
	CUIWindow* dateParent,
	CUIWindow* captionParent,
	CUIWindow* textParent,
	CUIWindow* imageParent)
{
	if (uiXml.NavigateToNode("image", 0))
	{
		_uiImage = UIHelper::CreateStatic(uiXml, "image", imageParent, false);
	}

	if (uiXml.NavigateToNode("caption_static", 0))
	{
		_uiCaption = UIHelper::CreateStatic(uiXml, "caption_static", captionParent, false);
	}
	else
	{
		_legacyMode = true;
	}

	const char* textPath = uiXml.NavigateToNode("text_static", 0) ? "text_static" : "text_cont";
	if (uiXml.NavigateToNode(textPath, 0))
	{
		_uiText = UIHelper::CreateStatic(uiXml, textPath, textParent, false);
	}

	const char* datePath = uiXml.NavigateToNode("date_static", 0) ? "date_static" : "date_text_cont";
	if (uiXml.NavigateToNode(datePath, 0))
	{
		_uiDate = UIHelper::CreateStatic(uiXml, datePath, dateParent, false);
	}
}

float CUINewsItemWnd::ReadStackSpacing(CUIXml& uiXml, const char* path) const
{
	return uiXml.ReadAttribFlt(path, 0, "spacing", 0.f);
}

void CUINewsItemWnd::UpdateStackedLayoutHeight()
{
	if (_layout == ELayout::Legacy)
	{
		return;
	}

	if (_textStack)
	{
		const float textHeight = CalcStackContentHeight(_textStack, _textStackSpacing);
		if (textHeight > 0.f)
		{
			_textStack->SetHeight(textHeight);
		}
	}

	if (_rowStack)
	{
		const float rowHeight = CalcStackContentHeight(_rowStack, _rowStackSpacing);
		if (rowHeight > 0.f)
		{
			_rowStack->SetHeight(rowHeight);
		}
	}

	if (_itemStack)
	{
		const float itemHeight = CalcStackContentHeight(_itemStack, _itemStackSpacing);
		if (itemHeight > 0.f)
		{
			_itemStack->SetHeight(itemHeight);
		}
	}

	float stackedHeight = GetWndSize().y;
	if (_itemStack)
	{
		stackedHeight = _itemStack->GetWndSize().y;
	}
	else if (_rowStack)
	{
		stackedHeight = _rowStack->GetWndSize().y;
	}

	if (stackedHeight > GetWndSize().y)
	{
		SetWndSize(Fvector2().set(GetWndSize().x, stackedHeight));
	}
}

void CUINewsItemWnd::ApplyNewsTexture(GAME_NEWS_DATA& newsData)
{
	if (!_uiImage)
	{
		return;
	}

	_uiImage->InitTexture(newsData.texture_name.c_str());

	Frect emptyRect = Frect().set(0.f, 0.f, 0.f, 0.f);
	if (!newsData.tex_rect.cmp(emptyRect))
	{
		Frect textureRect;
		textureRect.lt.set(newsData.tex_rect.x1, newsData.tex_rect.y1);
		textureRect.rb.set(newsData.tex_rect.x2, newsData.tex_rect.y2);
		textureRect.rb.add(textureRect.lt);
		_uiImage->SetTextureRect(textureRect);
	}
}

void CUINewsItemWnd::Setup(GAME_NEWS_DATA& newsData)
{
	if (_layout == ELayout::Legacy)
	{
		SetupLegacy(newsData);
		return;
	}

	SetupStacked(newsData);
}

void CUINewsItemWnd::SetupLegacy(GAME_NEWS_DATA& newsData)
{
	if (_hasDialogReplicaLayout && newsData.m_type == GAME_NEWS_DATA::eTalk)
	{
		const shared_str timeOnly = InventoryUtilities::GetTimeAsString(newsData.receive_time, InventoryUtilities::etpTimeToMinutes);
		_uiDate->SetText(timeOnly.c_str());
		_uiDate->AdjustWidthToText();

		if (_uiCaption)
		{
			if (strstr(newsData.news_caption.c_str(), ":lname_"))
			{
				_uiCaption->SetText(TranslateName(newsData.news_caption.c_str()).c_str());
			}
			else
			{
				_uiCaption->SetText(g_pStringTable->ParseStringFromScript(newsData.news_caption).c_str());
			}

			Fvector2 pos = _uiCaption->GetWndPos();
			pos.x = _uiDate->GetWndPos().x + _uiDate->GetWndSize().x + 5.0f;
			_uiCaption->SetWndPos(pos);
			_uiCaption->SetWidth(std::min(_uiText->GetWidth() - _uiDate->GetWndSize().x - 5.0f, _uiCaption->GetWidth()));
		}

		_uiText->Show(false);
		if (_dialogReplicaLine)
		{
			_dialogReplicaLine->Show(true);
			_dialogReplicaLine->SetText(g_pStringTable->ParseStringFromScript(newsData.news_text).c_str());
			_dialogReplicaLine->AdjustHeightToText();
		}

		ApplyNewsTexture(newsData);

		float hTop = _uiDate->GetWndPos().y + std::max(_uiDate->GetHeight(), _uiCaption ? _uiCaption->GetHeight() : _uiDate->GetHeight());
		float hReplica = _dialogReplicaLine ? (_dialogReplicaLine->GetWndPos().y + _dialogReplicaLine->GetHeight()) : hTop;
		float hImg = _uiImage->GetWndPos().y + _uiImage->GetHeight();
		float h = std::max(hReplica, hImg);
		SetHeight(h + kItemHeightPadding);
		return;
	}

	_uiText->Show(true);
	if (_dialogReplicaLine)
	{
		_dialogReplicaLine->Show(false);
	}

	shared_str timeStr = InventoryUtilities::GetTimeAndDateAsString(newsData.receive_time, _legacyMode);
	xr_string str = timeStr.c_str();
	if (!_legacyMode)
	{
		str += " -";
	}
	_uiDate->SetText(str.c_str());
	_uiDate->AdjustWidthToText();

	if (_uiCaption)
	{
		if (strstr(newsData.news_caption.c_str(), ":lname_"))
		{
			_uiCaption->SetText(TranslateName(newsData.news_caption.c_str()).c_str());
		}
		else
		{
			_uiCaption->SetText(g_pStringTable->ParseStringFromScript(newsData.news_caption).c_str());
		}
		Fvector2 pos = _uiCaption->GetWndPos();
		pos.x = _uiDate->GetWndPos().x + _uiDate->GetWndSize().x + 5.0f;
		_uiCaption->SetWndPos(pos);
		_uiCaption->SetWidth(std::min(_uiText->GetWidth() - _uiDate->GetWidth() - 5.0f, _uiCaption->GetWidth()));
	}

	_uiText->SetText(g_pStringTable->ParseStringFromScript(newsData.news_text).c_str());
	_uiText->AdjustHeightToText();
	float h1 = _uiText->GetWndPos().y + _uiText->GetHeight() + kItemHeightPadding;

	ApplyNewsTexture(newsData);

	float h3 = _uiImage->GetWndPos().y + _uiImage->GetHeight();
	h1 = std::max(h1, h3);
	SetHeight(h1);
}

void CUINewsItemWnd::SetupStacked(GAME_NEWS_DATA& newsData)
{
	const bool isTalkReplica = _hasDialogReplicaLayout && newsData.m_type == GAME_NEWS_DATA::eTalk;

	if (isTalkReplica)
	{
		const shared_str timeOnly = InventoryUtilities::GetTimeAsString(newsData.receive_time, InventoryUtilities::etpTimeToMinutes);
		if (_uiDate)
		{
			_uiDate->SetText(timeOnly.c_str());
			_uiDate->AdjustWidthToText();
		}

		if (_uiCaption)
		{
			if (strstr(newsData.news_caption.c_str(), ":lname_"))
			{
				_uiCaption->SetText(TranslateName(newsData.news_caption.c_str()).c_str());
			}
			else
			{
				_uiCaption->SetText(g_pStringTable->ParseStringFromScript(newsData.news_caption).c_str());
			}
		}

		if (_uiText)
		{
			_uiText->Show(false);
		}
		if (_dialogReplicaLine)
		{
			_dialogReplicaLine->Show(true);
			_dialogReplicaLine->SetText(g_pStringTable->ParseStringFromScript(newsData.news_text).c_str());
			_dialogReplicaLine->AdjustHeightToText();
		}
	}
	else
	{
		if (_uiText)
		{
			_uiText->Show(true);
		}
		if (_dialogReplicaLine)
		{
			_dialogReplicaLine->Show(false);
		}

		shared_str timeStr = InventoryUtilities::GetTimeAndDateAsString(newsData.receive_time, _legacyMode);
		xr_string str = timeStr.c_str();
		if (!_legacyMode)
		{
			str += " -";
		}
		if (_uiDate)
		{
			_uiDate->SetText(str.c_str());
			_uiDate->AdjustWidthToText();
		}

		if (_uiCaption)
		{
			if (strstr(newsData.news_caption.c_str(), ":lname_"))
			{
				_uiCaption->SetText(TranslateName(newsData.news_caption.c_str()).c_str());
			}
			else
			{
				_uiCaption->SetText(g_pStringTable->ParseStringFromScript(newsData.news_caption).c_str());
			}
		}

		if (_uiText)
		{
			_uiText->SetText(g_pStringTable->ParseStringFromScript(newsData.news_text).c_str());
			_uiText->AdjustHeightToText();
		}
	}

	ApplyNewsTexture(newsData);
	UpdateStackedLayoutHeight();

	float stackedHeight = GetWndSize().y;
	if (_itemStack)
	{
		stackedHeight = _itemStack->GetWndSize().y;
	}
	else if (_rowStack)
	{
		stackedHeight = _rowStack->GetWndSize().y;
	}

	SetHeight(stackedHeight + kItemHeightPadding);
}
