////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_upgrade.cpp
//	Created 	: 01.11.2007
//  Modified 	: 27.11.2007
//	Author		: Evgeniy Sokolov
//	Description : inventory upgrade class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "../xrEngine/string_table.h"

#include "inventory_upgrade.h"
#include "inventory_upgrade_manager.h"
#include "inventory_upgrade_group.h"
#include "inventory_upgrade_root.h"
#include "inventory_upgrade_property.h"

using namespace inventory::upgrade;

void Upgrade::construct(const shared_str& upgrade_id, Group& parental_group, Manager& manager_r)
{
	inherited::construct(upgrade_id, manager_r);
	m_parent_group = &parental_group;

	// name : StringTable(); icon; description;
	m_name = g_pStringTable->translate(pSettings->r_string(id(), "name"));
	m_description = g_pStringTable->translate(pSettings->r_string(id(), "description"));
	m_icon._set(pSettings->r_string(id(), "icon"));

	// section --------------------------------------------------------------------------
	const char* section_str = pSettings->r_string(id(), "section");
	VERIFY2(pSettings->section_exist(section_str), make_string<const char*>("Upgrade <%s> : settings section [%s] not exist!", id_str(), section_str));
	VERIFY2(pSettings->line_count(section_str), make_string<const char*>("Upgrade <%s> : settings section [%s] is empty !", id_str(), section_str));

	m_section._set(section_str);

	// precondition_functor
	const char* precondition_functor_str = pSettings->r_string(id(), "precondition_functor");
	m_preconditions.parameter = ""; //Dead Param
	m_preconditions.parameter2 = m_section.c_str();
	R_ASSERT2(ai().script_engine().functor(precondition_functor_str, m_preconditions.functr), make_string<const char*>("Failed to get precondition functor in section[%s], functor[%s]", id_str(), precondition_functor_str));
	m_preconditions();

	// effect_functor
	const char* effect_functor_str = pSettings->r_string(id(), "effect_functor");
	m_effects.parameter = ""; // Dead param
	m_effects.parameter2 = m_section.c_str();
	m_effects.parameter3 = 1;
	R_ASSERT2(ai().script_engine().functor(effect_functor_str, m_effects.functr), make_string<const char*>("Failed to get effect functor in section[%s], functor[%s]", id_str(), effect_functor_str));
	m_effects();

	// prereq_functor (1,2) : m_prerequisites
	const char* prereq_functor_str = pSettings->r_string(id(), "prereq_functor");
	m_prerequisites.parameter = ""; //Dead param
	m_prerequisites.parameter2 = m_section.c_str();
	R_ASSERT2(ai().script_engine().functor(prereq_functor_str, m_prerequisites.functr), make_string<const char*>("Failed to get prerequisites functor in section[%s], functor[%s]", id_str(), prereq_functor_str));
	m_prerequisites();

	// effects = groups
	const char* groups_str = pSettings->r_string(id(), "effects");
	if (groups_str)
	{
		add_dependent_groups(groups_str, manager_r);
	}

	m_known = pSettings->read_if_exists<bool>(id(), "known", false);

	shared_str properties = pSettings->r_string(id(), "property");
	VERIFY2(properties.size(), make_string<const char*>("Upgrade <%s> : property is empty !", id_str()));

	string256 buffer = {};
	for (u8 i = 0; i < max_properties_count; i++)
	{
		shared_str prop = _GetItem(properties.c_str(), i, buffer);
		if (prop.size())
		{
			m_properties[i] = prop;
			VERIFY2(manager_r.get_property(prop), make_string<const char*>("Upgrade <%s> : property [%s] is unknown (not found in upgrade manager) !", id_str(), prop.c_str()));
		}
	}

	m_scheme_index.set(-1, -1);
	m_scheme_index = pSettings->r_ivector2(id(), "scheme_index");
}

#ifdef DEBUG

void Upgrade::log_hierarchy(const char* nest)
{
	u32 sz = (xr_strlen(nest) + 4) * sizeof(char);
	char* nest2 = (char*)_alloca(sz);
	xr_strcpy(nest2, sz, nest);
	xr_strcat(nest2, sz, "   ");
	Msg("%s<u> %s", nest2, id_str());

	inherited::log_hierarchy(nest2);
}

#endif // DEBUG

void Upgrade::RefreshTranslations()
{
	m_name = g_pStringTable->translate(pSettings->r_string(id(), "name"));
	m_description = g_pStringTable->translate(pSettings->r_string(id(), "description"));
}

void Upgrade::fill_root_container(Root* root)
{
	R_ASSERT(root);
	root->add_upgrade(this);
	inherited::fill_root_container(root);
}

UpgradeStateResult Upgrade::can_install(CInventoryItem& item, bool loading)
{
	UpgradeStateResult res = inherited::can_install(item, loading);
	if (res != result_ok)
	{
		return res;
	}

	res = m_parent_group->can_install(item, *this, loading);
	if (loading)
	{
		return result_ok; // later script check
	}

	if (res == result_ok)
	{
		int script_res = m_preconditions();

		switch (script_res)
		{
		case result_script_ok:
		{
			return res;
		}break;
		case result_script_e_cant_do:
		{
			const static bool isLegacyUpgrade = EngineExternal()[EEngineExternalGame::EnableLegacyUpgradeSystem];
			if (isLegacyUpgrade)
			{
				if (res != result_ok)
				{
					return res;
				}

				return result_e_precondition_money;
			}

			return result_e_cant_do;
		} break;
		case result_script_e_precondition_any:
		{
			if (res != result_ok)
			{
				return res;
			}

			return result_e_precondition_quest;
		} break;
		}
	}
	return res;
}

bool Upgrade::check_scheme_index(Ivector2 const& scheme_index) const
{
	return (m_scheme_index.x == scheme_index.x && m_scheme_index.y == scheme_index.y);
}

const char* Upgrade::get_prerequisites()
{
	return m_prerequisites();
}

void Upgrade::run_effects(bool loading)
{
	m_effects.parameter3 = loading ? 1 : 0;
	m_effects();
}

void Upgrade::set_highlight(bool value)
{
	m_highlight = value;
}

void Upgrade::highlight_up()
{
	set_highlight(true);

	for (const auto& depended_group : m_depended_groups)
	{
		depended_group->highlight_up();
	}
}

void Upgrade::highlight_down()
{
	set_highlight(true);
	m_parent_group->highlight_down();
}