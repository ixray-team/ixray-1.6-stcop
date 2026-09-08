#include "StdAfx.h"
#include "PowerCell.h"
#include "Inventory.h"
#include "InventoryOwner.h"
#include "IPowerManager.h"
#include "PowerBank.h"
#include "UICellItem.h"
#include "../xrEngine/string_table.h"

void PowerCell::Load(const char* section)
{
    CInventoryItemObject::Load(section);

    m_power_cell_data.section = section;
    m_power_cell_data.max_power = READ_IF_EXISTS(pSettings, r_float, section, "power_cell_max_energy_value", 0.0f);
    m_power_cell_data.current_power = READ_IF_EXISTS(pSettings, r_float, section, "power_cell_current_energy_value", 0.0f);
    m_flags.set(FUsingCondition, true);
}

void PowerCell::save(NET_Packet& output_packet)
{
    CInventoryItemObject::save(output_packet);

    output_packet.w_stringZ(m_power_cell_data.section);
    output_packet.w_float(m_power_cell_data.max_power);
    output_packet.w_float(m_power_cell_data.current_power);
}

void PowerCell::load(IReader& input_packet)
{
    CInventoryItemObject::load(input_packet);

    input_packet.r_stringZ(m_power_cell_data.section);
    m_power_cell_data.max_power = input_packet.r_float();
    m_power_cell_data.current_power = input_packet.r_float();
}

bool PowerCell::FillUseActions(
	CUIPropertiesBox* box,
	const UseActionContext& context
)
{
	const static bool enable_power_cell_context_menu =
		EngineExternal()[EEngineExternalGame::EnablePowerCellContextMenu];

	if (!enable_power_cell_context_menu ||
		box == nullptr ||
		context.inventory == nullptr ||
		context.owner == nullptr ||
		context.item == nullptr ||
		context.item->parent_id() != context.owner->object_id())
	{
		return false;
	}

	bool added = false;
	xr_map<shared_str, PIItem> targets_by_section;
	const PowerCellData power_cell_data = GetPowerCellData();

	for (PIItem target : context.inventory->m_all)
	{
		if (target == nullptr ||
			target == context.item ||
			target->parent_id() != context.owner->object_id())
		{
			continue;
		}

		bool can_install = false;

		if (IPowerManager* power_manager =
				smart_cast<IPowerManager*>(target))
		{
			// IstallPowerCell accepts only an empty device. Such a device
			// has 0% charge and therefore is strictly below 100%.
			can_install =
				power_manager->GetUsePowerCell() &&
				!power_manager->IsPowerCellInstalled() &&
				power_manager->IsPowerCellInWhiteList(
					power_cell_data.section
				);
		}
		else if (PowerBank* power_bank =
				 smart_cast<PowerBank*>(target))
		{
			can_install =
				power_bank->m_power_cells.size() <
					power_bank->m_max_count_power_cells &&
				power_bank->GetCalculatedCondition() < 1.0f &&
				power_bank->IsPowerCellInWhiteList(
					power_cell_data.section
				);
		}

		if (!can_install)
		{
			continue;
		}

		targets_by_section.emplace(
			target->m_section_id,
			target
		);
	}

	for (const auto& target_by_section : targets_by_section)
	{
		PIItem target = target_by_section.second;
		shared_str text =
			g_pStringTable->translate("st_install_power_cell");

		text.printf(
			"%s %s",
			text.c_str(),
			target->m_name.c_str()
		);

		box->AddItem(
			text.c_str(),
			target,
			ATTACH_POWER_CELL
		);

		added = true;
	}

	return added;
}

using namespace luabind;
#pragma optimize("s",on)
void PowerCell::script_register(lua_State* L)
{
    module(L)
        [
            class_<PowerCell, CGameObject>("PowerCell")
                .def(constructor<>())
        ];
}
