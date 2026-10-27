////////////////////////////////////////////////////////////////////////////
//	Module 		: UIInvUpgradeProperty.cpp
//	Created 	: 22.11.2007
//  Modified 	: 13.03.2009
//	Author		: Evgeniy Sokolov, Prishchepa Sergey
//	Description : inventory upgrade property UIWindow class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "UIInvUpgradeProperty.h"
#include "UIInvUpgradeInfo.h"

#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"

#include "ai_space.h"
#include "alife_simulator.h"
#include "inventory_upgrade_manager.h"
#include "inventory_upgrade.h"
#include "inventory_upgrade_property.h"
#include "Level.h"
#include "../../xrUI/UIHelper.h"

void UIProperty::init_from_xml(CUIXml& ui_xml)
{
	m_ui_icon = new CUIStatic();
	m_ui_text = new CUIStatic();
	AttachChild(m_ui_icon);
	AttachChild(m_ui_text);
	m_ui_icon->SetAutoDelete(true);
	m_ui_text->SetAutoDelete(true);

	CUIXmlInit::InitWindow(ui_xml, "properties", 0, this);
	SetWndPos(Fvector2().set(0, 0));
	CUIXmlInit::InitStatic(ui_xml, "properties:icon", 0, m_ui_icon);
	CUIXmlInit::InitStatic(ui_xml, "properties:text", 0, m_ui_text);
}

bool UIProperty::init_property(shared_str const& property_id)
{
	m_property_id = property_id;
	if (!get_property())
	{
		return false;
	}
	m_ui_icon->InitTexture(get_property()->icon_name());
	m_ui_icon->SetTextureColor(get_property()->icon_color());
	return true;
}

UIProperty::Property_type* UIProperty::get_property()
{
	if (!ai().get_alife())
	{
		return nullptr;
	}
	Property_type* proper = Level().m_upgrade_manager->get_property(m_property_id);
	VERIFY(proper);
	return proper;
}

bool UIProperty::read_value_from_section(const char* section, const char* param, float& result)
{
	result = 0.0f;
	if (!section || !pSettings->section_exist(section))
	{
		return false;
	}

	if (pSettings->line_exist(section, param) && *pSettings->r_string(section, param))
	{
		result = pSettings->r_float(section, param);
		return true;
	}
	return false;
}

bool UIProperty::compute_value(ItemUpgrades_type const& item_upgrades)
{
	if (!get_property())
	{
		return false;
	}

	int prop_count = 0;
	string2048 buf; buf[0] = 0;
	ItemUpgrades_type::const_iterator ib_upg = item_upgrades.begin();
	ItemUpgrades_type::const_iterator ie_upg = item_upgrades.end();
	for (; ib_upg != ie_upg; ++ib_upg)
	{
		Upgrade_type* upgr = Level().m_upgrade_manager->get_upgrade(*ib_upg);
		VERIFY(upgr);
		for (u8 i = 0; i < inventory::upgrade::max_properties_count; i++)
		{
			if (upgr->get_property_name(i)._get() == m_property_id._get())
			{
				const char* upgr_section = upgr->section();
				if (prop_count > 0)
				{
					xr_strcat(buf, sizeof(buf), ", ");
				}
				xr_strcat(buf, sizeof(buf), upgr_section);
				++prop_count;
			}
		}
	}
	if (prop_count > 0)
	{
		return show_result(buf);
	}
	return false;
}

bool UIProperty::show_result(const char* values)
{
	if (get_property() && get_property()->run_functor(values, m_text))
	{
		m_ui_text->SetText(m_text);
		return true;
	}
	else
	{
		m_ui_text->SetText("");
		return false;
	}
}

float UIProperty::GetRequiredWidth()
{
	m_ui_text->TextItemControl()->SetTextComplexMode(false);
	m_ui_text->AdjustWidthToText();
	return m_ui_text->GetWndPos().x + m_ui_text->GetWidth() + 1.0f;
}

void UIProperty::FitToWidth(float Width, bool WrapText)
{
	SetWidth(Width);
	m_ui_text->SetWidth(std::max(1.0f, Width - m_ui_text->GetWndPos().x - 1.0f));
	m_ui_text->TextItemControl()->SetTextComplexMode(WrapText);
	m_ui_text->SetHeight(m_ui_icon->GetHeight());
	if (WrapText)
	{
		m_ui_text->TextItemControl()->ParseText(true);
		m_ui_text->AdjustHeightToText();
		m_ui_text->SetHeight(std::max(m_ui_text->GetHeight(), m_ui_icon->GetHeight()));
	}
	SetHeight(std::max(m_ui_icon->GetWndPos().y + m_ui_icon->GetHeight(), m_ui_text->GetWndPos().y + m_ui_text->GetHeight()) + 1.0f);
}

// =================== UIPropertiesWnd =====================================================

UIInvUpgPropertiesWnd::UIInvUpgPropertiesWnd()
{
	m_properties_ui.reserve(15);
	m_temp_upgrade_vector.reserve(1);
}

void UIInvUpgPropertiesWnd::UpdateStatsPos(float& h, Fvector2& pos, UIProperty* pWnd, int& counter) const
{
	// Если элемент четный, размещаем его в правом столбце
	if ((counter) % 2 == 0)
	{
		pos.x = 0.0f; // Левый столбец
	}
	else
	{
		pos.x = m_fsec_col_pos; // Правый столбец
	}
	// Увеличиваем счетчик
	PendingRowHeight = std::max(PendingRowHeight, pWnd->GetHeight());
	counter += 1;

	// Устанавливаем вертикальное положение
	pos.y = h;

	// Если оба столбца заполнены (четное количество элементов), переходим на следующую строку
	if (counter % 2 == 0)
	{
		h += std::max(m_fnext_line_pos, PendingRowHeight);
		PendingRowHeight = 0.0f;
	}

	// Устанавливаем позицию элемента
	pWnd->SetWndPos(pos);
}

UIInvUpgPropertiesWnd::~UIInvUpgPropertiesWnd()
{
	delete_data(m_properties_ui);
}

void UIInvUpgPropertiesWnd::init_from_xml(const char* xml_name)
{
	CUIXml ui_xml;
	if (!ui_xml.Load( CONFIG_PATH, UI_PATH, xml_name ))
		return;
	
	XML_NODE* stored_root = ui_xml.GetLocalRoot();
	XML_NODE* node = ui_xml.NavigateToNode( "upgrade_info", 0 );
	ui_xml.SetLocalRoot( node );

	CUIXmlInit::InitWindow(ui_xml, "properties", 0, this);

	if (ui_xml.NavigateToNode("properties:upgr_line"))
	{
		m_Upgr_line = UIHelper::CreateStatic(ui_xml, "properties:upgr_line", this);
	}

	BaseWidth = GetWidth();
	AvailableWidth = BaseWidth;

	m_fsec_col_pos = ui_xml.ReadAttribFlt("properties", 0, "sec_col_pos", UI().is_widescreen() ? 105.f : 130.f);
	m_fnext_line_pos = ui_xml.ReadAttribFlt("properties", 0, "next_line_pos", 20.f);

	const char* properties_section = "upgrades_properties";

	shared_str property_id;

	if (pSettings->section_exist(properties_section))
	{
		CInifile::Sect& inv_section = pSettings->r_section(properties_section);

		for (const auto& section_data : inv_section.Data)
		{
			UIProperty* ui_property = new UIProperty(); // load one time !!
			ui_property->init_from_xml(ui_xml);

			property_id._set(section_data.first);
			if (!ui_property->init_property(property_id))
			{
				Msg("! Invalid property <%s> in inventory upgrade manager!", property_id.c_str());
				xr_delete(ui_property);
				continue;
			}

			m_properties_ui.push_back(ui_property);
			AttachChild(ui_property);
		} // for ib
	}
	ui_xml.SetLocalRoot(stored_root);
}

void UIInvUpgPropertiesWnd::SetAvailableWidth(float Width, bool AllowExpansion)
{
	AvailableWidth = std::max(1.0f, Width);
	CanExpand = AllowExpansion;
}

void UIInvUpgPropertiesWnd::set_info(ItemUpgrades_type const& item_upgrades)
{
	xr_vector<UIProperty*> VisibleProperties;
	float RequiredColumnWidth = 0.0f;
	for (auto* Property : m_properties_ui)
	{
		Property->Show(false);
		if (Property->compute_value(item_upgrades))
		{
			VisibleProperties.push_back(Property);
			RequiredColumnWidth = std::max(RequiredColumnWidth, Property->GetRequiredWidth());
		}
	}
	m_iNumUpgr = (int)VisibleProperties.size();
	PendingRowHeight = 0.0f;
	float Height = m_Upgr_line ? m_Upgr_line->GetWndSize().y + 1.0f : 0.0f;
	const float MinWidth = std::max(BaseWidth, AvailableWidth);
	const float ColumnWidth = CanExpand ? std::max(MinWidth * 0.5f, RequiredColumnWidth + 1.0f) : MinWidth * 0.5f;
	SetWidth(ColumnWidth * 2.0f);
	m_fsec_col_pos = ColumnWidth;

	for (u32 Index = 0; Index < VisibleProperties.size(); Index += 2)
	{
		const bool HasRight = Index + 1 < VisibleProperties.size();
		UIProperty* Left = VisibleProperties[Index];
		Left->FitToWidth(HasRight ? ColumnWidth : GetWidth(), !CanExpand);
		Left->SetWndPos(Fvector2().set(0.0f, Height));
		Left->Show(true);
		float RowHeight = Left->GetHeight();

		if (HasRight)
		{
			UIProperty* Right = VisibleProperties[Index + 1];
			Right->FitToWidth(ColumnWidth, !CanExpand);
			Right->SetWndPos(Fvector2().set(ColumnWidth, Height));
			Right->Show(true);
			RowHeight = std::max(RowHeight, Right->GetHeight());
		}
		Height += std::max(m_fnext_line_pos, RowHeight);
	}
	SetHeight(Height);
}

void UIInvUpgPropertiesWnd::set_upgrade_info(Upgrade_type& upgrade)
{
	if (!upgrade.is_known())
	{
		SetWndSize(Fvector2().set(0, 0));
		return;
	}

	m_temp_upgrade_vector.resize(0);
	m_temp_upgrade_vector.push_back(upgrade.id());
	set_info(m_temp_upgrade_vector);
}

void UIInvUpgPropertiesWnd::set_item_info(CInventoryItem& item)
{
	set_info(item.upgardes());
}
