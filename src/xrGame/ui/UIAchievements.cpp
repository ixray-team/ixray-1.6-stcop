#include "StdAfx.h"
#include "pch_script.h"

#include "UIAchievements.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIStackPanel.h"
#include "../../xrUI/UICursor.h"

#include "../ai_space.h"
#include "../Level.h"

#include "../../xrEngine/string_table.h"

namespace
{
constexpr float kDescriptionHeightPadding = 30.f;

bool ReadLayoutElementSize(CUIXml& xml, const char* elementPath, float& outWidth, float& outHeight)
{
	if (!xml.NavigateToNode(elementPath, 0))
	{
		return false;
	}

	outWidth = xml.ReadAttribFlt(elementPath, 0, "width", 0.f);
	outHeight = xml.ReadAttribFlt(elementPath, 0, "height", 0.f);
	return outWidth > 0.f || outHeight > 0.f;
}

void MergeLayoutDefaultsFromNode(CUIXml& xml, const char* layoutPath, CUIAchievements::SLayoutDefaults& defaults)
{
	XML_NODE* layoutNode = xml.NavigateToNode(layoutPath, 0);
	if (!layoutNode)
	{
		return;
	}

	XML_NODE* savedRoot = xml.GetLocalRoot();
	xml.SetLocalRoot(layoutNode);

	float width = 0.f;
	float height = 0.f;

	if (ReadLayoutElementSize(xml, "icon", width, height))
	{
		if (width > 0.f) { defaults.iconWidth = width; }
		if (height > 0.f) { defaults.iconHeight = height; }
	}

	if (ReadLayoutElementSize(xml, "name", width, height))
	{
		if (width > 0.f) { defaults.nameWidth = width; }
		if (height > 0.f) { defaults.nameHeight = height; }
	}

	if (ReadLayoutElementSize(xml, "descr", width, height))
	{
		if (width > 0.f) { defaults.descrWidth = width; }
		if (height > 0.f) { defaults.descrHeight = height; }
	}

	if (ReadLayoutElementSize(xml, "hint_wnd", width, height))
	{
		if (width > 0.f) { defaults.hintWidth = width; }
		if (height > 0.f) { defaults.hintHeight = height; }
	}

	xml.SetLocalRoot(savedRoot);
}

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

CUIAchievements::CUIAchievements(CUIScrollView* parent)
	: m_parent(parent)
{
}

CUIAchievements::~CUIAchievements()
{
	xr_delete(m_hint);
}

void CUIAchievements::init_from_xml(CUIXml& xml)
{
	CUIXmlInit::InitWindow(xml, "achievements_itm", 0, this);

	XML_NODE* storedRoot = xml.GetLocalRoot();
	XML_NODE* itemNode = xml.NavigateToNode("achievements_itm", 0);
	R_ASSERT(itemNode);
	xml.SetLocalRoot(itemNode);

	m_layoutDefaults = {};
	{
		XML_NODE* documentRoot = xml.GetRoot();
		if (documentRoot)
		{
			xml.SetLocalRoot(documentRoot);
			MergeLayoutDefaultsFromNode(xml, "achievements_layout", m_layoutDefaults);
		}
		xml.SetLocalRoot(itemNode);
		MergeLayoutDefaultsFromNode(xml, "achievements_layout", m_layoutDefaults);
	}

	const bool hasItemStack = xml.NavigateToNode("achievements_itm_stack", 0) != nullptr;
	const bool hasRowStack = xml.NavigateToNode("achievements_row_stack", 0) != nullptr;
	const bool hasTextStack = xml.NavigateToNode("achievements_text_stack", 0) != nullptr;

	if (hasTextStack && !hasRowStack && !hasItemStack)
	{
		VERIFY2(false, "achievements_itm: achievements_text_stack requires achievements_row_stack or achievements_itm_stack");
		initLegacyFromXml(xml);
	}
	else if (hasItemStack || hasRowStack)
	{
		if (!initStackedFromXml(xml))
		{
			initLegacyFromXml(xml);
		}
	}
	else
	{
		initLegacyFromXml(xml);
	}

	m_hint = UIHelper::CreateHint(xml, "hint_wnd");
	applyHintLayoutSize(xml);

	xml.SetLocalRoot(storedRoot);
	Show(false);
}

void CUIAchievements::initLegacyFromXml(CUIXml& xml)
{
	m_layout = ELayout::Legacy;
	m_itemStack = nullptr;
	m_rowStack = nullptr;
	m_textStack = nullptr;
	createAchievementStatics(xml, this, this, this);
	applyLayoutDefaults(xml);
}

bool CUIAchievements::initStackedFromXml(CUIXml& xml)
{
	const bool hasItemStack = xml.NavigateToNode("achievements_itm_stack", 0) != nullptr;
	const bool hasRowStack = xml.NavigateToNode("achievements_row_stack", 0) != nullptr;

	CUIWindow* nameParent = nullptr;
	CUIWindow* descrParent = nullptr;
	CUIWindow* iconParent = nullptr;

	if (hasItemStack)
	{
		m_layout = ELayout::SingleStack;
		m_itemStack = UIHelper::CreateStackPanel(xml, "achievements_itm_stack", this, false);
		if (!m_itemStack)
		{
			return false;
		}
		m_itemStackSpacing = readStackSpacing(xml, "achievements_itm_stack");
		m_itemStack->Show(true);
		nameParent = m_itemStack;
		descrParent = m_itemStack;
		iconParent = m_itemStack;
	}
	else if (hasRowStack)
	{
		m_layout = ELayout::NestedStack;
		m_rowStack = UIHelper::CreateStackPanel(xml, "achievements_row_stack", this, false);
		if (!m_rowStack)
		{
			return false;
		}
		m_rowStackSpacing = readStackSpacing(xml, "achievements_row_stack");
		m_rowStack->Show(true);
		iconParent = m_rowStack;

		// Icon must be attached before the text stack so eLeft row layout places it on the left.
		if (xml.NavigateToNode("icon", 0))
		{
			m_icon = UIHelper::CreateStatic(xml, "icon", m_rowStack, false);
		}

		if (xml.NavigateToNode("achievements_text_stack", 0))
		{
			m_textStack = UIHelper::CreateStackPanel(xml, "achievements_text_stack", m_rowStack, false);
			if (!m_textStack)
			{
				return false;
			}
			m_textStackSpacing = readStackSpacing(xml, "achievements_text_stack");
			m_textStack->Show(true);
			nameParent = m_textStack;
			descrParent = m_textStack;
		}
		else
		{
			nameParent = m_rowStack;
			descrParent = m_rowStack;
		}

		if (xml.NavigateToNode("name", 0))
		{
			m_name = UIHelper::CreateStatic(xml, "name", nameParent, false);
		}
		if (xml.NavigateToNode("descr", 0))
		{
			m_descr = UIHelper::CreateStatic(xml, "descr", descrParent, false);
		}
	}
	else
	{
		return false;
	}

	if (m_layout != ELayout::NestedStack)
	{
		createAchievementStatics(xml, nameParent, descrParent, iconParent);
	}
	applyLayoutDefaults(xml);
	updateStackedLayoutHeight();
	return true;
}

void CUIAchievements::createAchievementStatics(
	CUIXml& xml,
	CUIWindow* nameParent,
	CUIWindow* descrParent,
	CUIWindow* iconParent)
{
	if (xml.NavigateToNode("icon", 0))
	{
		m_icon = UIHelper::CreateStatic(xml, "icon", iconParent, false);
	}
	if (xml.NavigateToNode("name", 0))
	{
		m_name = UIHelper::CreateStatic(xml, "name", nameParent, false);
	}
	if (xml.NavigateToNode("descr", 0))
	{
		m_descr = UIHelper::CreateStatic(xml, "descr", descrParent, false);
	}
}

float CUIAchievements::readStackSpacing(CUIXml& xml, const char* path) const
{
	return xml.ReadAttribFlt(path, 0, "spacing", 0.f);
}

void CUIAchievements::applyLayoutDefaults(CUIXml& xml)
{
	applyStaticLayoutSize(xml, "icon", m_icon, m_layoutDefaults.iconWidth, m_layoutDefaults.iconHeight);
	applyStaticLayoutSize(xml, "name", m_name, m_layoutDefaults.nameWidth, m_layoutDefaults.nameHeight);
	applyStaticLayoutSize(xml, "descr", m_descr, m_layoutDefaults.descrWidth, m_layoutDefaults.descrHeight);
}

void CUIAchievements::applyStaticLayoutSize(
	CUIXml& xml,
	const char* path,
	CUIStatic* wnd,
	float defaultWidth,
	float defaultHeight)
{
	if (!wnd || !xml.NavigateToNode(path, 0))
	{
		return;
	}

	const float xmlWidth = xml.ReadAttribFlt(path, 0, "width", 0.f);
	const float xmlHeight = xml.ReadAttribFlt(path, 0, "height", 0.f);
	const bool forceLayoutSize = xml.ReadAttribBool(path, 0, "use_layout_size", false);

	float width = wnd->GetWndSize().x;
	float height = wnd->GetWndSize().y;

	if (forceLayoutSize)
	{
		if (defaultWidth > 0.f) { width = defaultWidth; }
		if (defaultHeight > 0.f) { height = defaultHeight; }
	}
	else
	{
		if (defaultWidth > 0.f && xmlWidth <= 0.f) { width = defaultWidth; }
		if (defaultHeight > 0.f && xmlHeight <= 0.f) { height = defaultHeight; }
	}

	if (width > 0.f && height > 0.f)
	{
		wnd->SetWndSize(Fvector2().set(width, height));
	}
}

void CUIAchievements::applyHintLayoutSize(CUIXml& xml)
{
	if (!m_hint)
	{
		return;
	}

	const bool forceLayoutSize = xml.ReadAttribBool("hint_wnd", 0, "use_layout_size", false);
	const float xmlWidth = xml.ReadAttribFlt("hint_wnd", 0, "width", 0.f);
	const float xmlHeight = xml.ReadAttribFlt("hint_wnd", 0, "height", 0.f);

	float width = m_hint->GetWndSize().x;
	float height = m_hint->GetWndSize().y;

	if (forceLayoutSize)
	{
		if (m_layoutDefaults.hintWidth > 0.f) { width = m_layoutDefaults.hintWidth; }
		if (m_layoutDefaults.hintHeight > 0.f) { height = m_layoutDefaults.hintHeight; }
	}
	else
	{
		if (m_layoutDefaults.hintWidth > 0.f && xmlWidth <= 0.f) { width = m_layoutDefaults.hintWidth; }
		if (m_layoutDefaults.hintHeight > 0.f && xmlHeight <= 0.f) { height = m_layoutDefaults.hintHeight; }
	}

	if (width > 0.f && height > 0.f)
	{
		m_hint->SetWndSize(Fvector2().set(width, height));
	}
}

void CUIAchievements::updateStackedLayoutHeight()
{
	if (m_layout == ELayout::Legacy)
	{
		return;
	}

	if (m_textStack)
	{
		const float textHeight = CalcStackContentHeight(m_textStack, m_textStackSpacing);
		if (textHeight > 0.f)
		{
			m_textStack->SetHeight(textHeight);
		}
	}

	if (m_rowStack)
	{
		const float rowHeight = CalcStackContentHeight(m_rowStack, m_rowStackSpacing);
		if (rowHeight > 0.f)
		{
			m_rowStack->SetHeight(rowHeight);
		}
	}

	if (m_itemStack)
	{
		const float itemHeight = CalcStackContentHeight(m_itemStack, m_itemStackSpacing);
		if (itemHeight > 0.f)
		{
			m_itemStack->SetHeight(itemHeight);
		}
	}

	float stackedHeight = GetWndSize().y;
	if (m_itemStack)
	{
		stackedHeight = m_itemStack->GetWndSize().y;
	}
	else if (m_rowStack)
	{
		stackedHeight = m_rowStack->GetWndSize().y;
	}

	const float descrHeight = m_descr ? m_descr->GetWndSize().y + kDescriptionHeightPadding : 0.f;
	const float newHeight = std::max(stackedHeight, descrHeight);
	if (newHeight > GetWndSize().y)
	{
		SetWndSize(Fvector2().set(GetWndSize().x, newHeight));
	}
}

void CUIAchievements::Update()
{
	if (!IsGameTypeSingle())
	{
		return;
	}

	if (ParentHasMe() && !m_repeat)
	{
		return;
	}

	luabind::functor<bool> f;

	if (ai().script_engine().functor(m_functor_str, f) && f())
	{
		if (!ParentHasMe())
		{
			m_parent->AddWindow(this, false);
			Show(true);
		}
	}
	else
	{
		if (ParentHasMe())
		{
			m_parent->RemoveWindow(this);
			Show(false);
		}
	}
}

bool CUIAchievements::ParentHasMe()
{
	xrCriticalSectionGuard guard(m_parent->csUi);

	WINDOW_LIST::const_iterator it = std::find(m_parent->Items().begin(), m_parent->Items().end(), this);
	return it != m_parent->Items().end();
}

void CUIAchievements::SetName(const char* name)
{
	if (m_name)
	{
		m_name->SetTextST(name);
	}
}

void CUIAchievements::SetDescription(const char* desc)
{
	if (!m_descr)
	{
		return;
	}

	m_descr->SetTextST(desc);
	m_descr->AdjustHeightToText();

	if (m_layout == ELayout::Legacy)
	{
		Fvector2 descrSize = m_descr->GetWndSize();
		descrSize.y += kDescriptionHeightPadding;
		if (descrSize.y > GetWndSize().y)
		{
			SetWndSize(Fvector2().set(GetWndSize().x, descrSize.y));
		}
		return;
	}

	updateStackedLayoutHeight();
}

void CUIAchievements::SetHint(const char* hint)
{
	if (m_hint)
	{
		m_hint->set_text(g_pStringTable->translate(hint).c_str());
	}
}

void CUIAchievements::SetIcon(const char* icon)
{
	if (m_icon)
	{
		m_icon->InitTexture(icon);
	}
}

void CUIAchievements::SetFunctor(const char* func)
{
	xr_sprintf(m_functor_str, sizeof(m_functor_str), "%s", func);
}

void CUIAchievements::SetRepeatable(bool repeat)
{
	m_repeat = repeat;
}

void CUIAchievements::DrawHint()
{
	Frect r;
	GetAbsoluteRect(r);
	Fvector2 pos = UI().GetUICursor().GetCursorPosition();
	if (r.in(pos) && m_hint)
	{
		m_hint->Draw();
	}
}

void CUIAchievements::Reset()
{
	if (ParentHasMe())
	{
		m_parent->RemoveWindow(this);
		Show(false);
	}
	inherited::Reset();
}
