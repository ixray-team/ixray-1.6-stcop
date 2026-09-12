#pragma once
#include "StdAfx.h"
#include "IPowerManager.h"
#include "Actor.h"
#include "Inventory.h"
#include "ai_object_location.h"
#include "alife_simulator_base.h"
#include "alife_simulator.h"
#include "UICellItem.h"
#include "../xrUI/UIXmlInit.h"
#include "../xrEngine/string_table.h"

IPowerManager::IPowerManager()
{
	Device.seqFrame.Add(this, REG_PRIORITY_LOW - 5000);
}

IPowerManager::~IPowerManager()
{
	Device.seqFrame.Remove(this);
}

void IPowerManager::Load(const char* section, CInventoryItem* iitem)
{
	if (!initialized)
	{
		return;
	}

	SetUsePowerCell(READ_IF_EXISTS(pSettings, r_bool, section, "use_power_cells", false));
	if (GetUsePowerCell())
	{
		m_allowed_power_cells_sections.clear();
		if (pSettings->line_exist(section, "allowed_power_cells_sections"))
		{
			m_allowed_power_cells_sections = xr_string(pSettings->r_string(section, "allowed_power_cells_sections")).RemoveWhitespaces().Split(',');
			if (!m_allowed_power_cells_sections.empty())
			{
				for (xr_string& section : m_allowed_power_cells_sections)
				{
					iitem->m_HiglightRelatedItemSections.push_back(section.c_str());
				}
			}
		}
	}

	SetUsePowerBank(READ_IF_EXISTS(pSettings, r_bool, section, "use_power_bank", false));
	SetPowerDrainValue(READ_IF_EXISTS(pSettings, r_float, section, "power_drain_value", 0.f));
}

PowerBank* IPowerManager::GetPowerBank()
{
	if (CActor* act = Actor())
	{
		if (PIItem item_from_slot = act->inventory().ItemFromSlot(POWER_BANK_SLOT))
		{
			return smart_cast<PowerBank*>(item_from_slot);
		}
	}

	return nullptr;
}

void IPowerManager::DrainPower()
{
	if (!initialized || !is_enabled || GetPowerDrainValue() <= 0.f)
	{
		return;
	}

	if (GetUsePowerBank())
	{
		PowerBank* pwr_bank = GetPowerBank();
		if (pwr_bank != nullptr)
		{
			float power = pwr_bank->GetPower();
			if (power > 0.f)
			{
				pwr_bank->DecreasePower(GetPowerDrainValue());
				return;
			}
		}
	}

	if (GetUsePowerCell() && IsPowerCellInstalled())
	{
		m_power_cell.current_power -= power_drain_value;
		clamp(m_power_cell.current_power, 0.f, m_power_cell.max_power);
	}
}

float IPowerManager::GetLeftPowerValue()
{
	if (!initialized)
	{
		return 0.f;
	}

	if (GetUsePowerBank())
	{
		PowerBank* pwr_bank = GetPowerBank();
		if (pwr_bank != nullptr)
		{
			float power = pwr_bank->GetPower();
			if (power > 0.f)
			{
				return power;
			}
		}
	}

	if (GetUsePowerCell() && IsPowerCellInstalled())
	{
		return m_power_cell.current_power;
	}

	return 0.f;
}

bool IPowerManager::IsPowerCellInWhiteList(shared_str power_cell_section)
{
	size_t count = m_allowed_power_cells_sections.size();
	for (size_t i = 0; i < count; ++i)
	{
		if (!m_allowed_power_cells_sections[i].empty() && m_allowed_power_cells_sections[i] == power_cell_section.c_str())
		{
			return true;
		}
	}

	return false;
}

bool IPowerManager::IstallPowerCell(PowerCell* oPowerCell)
{
	if (!initialized || !GetUsePowerCell())
	{
		return false;
	}

	if (!IsPowerCellInWhiteList(oPowerCell->GetPowerCellData().section))
	{
		return false;
	}

	if (!IsPowerCellInstalled())
	{
		m_power_cell = oPowerCell->GetPowerCellData();
		oPowerCell->DestroyObject();
		is_power_cell_installed = true;

		return true;
	}

	return false;
}

extern CSE_Abstract* CALifeSimulator__spawn_item2(
	CALifeSimulator* self_,
	const char* section,
	const Fvector& position,
	u32 level_vertex_id,
	GameGraph::_GRAPH_ID game_vertex_id,
	ALife::_OBJECT_ID id_parent
);


bool IPowerManager::UnistallPowerCell()
{
	if (!initialized || !GetUsePowerCell())
	{
		return false;
	}

	if (IsPowerCellInstalled())
	{
		m_parent = GetSelfObject()->cast_game_object()->H_Parent();
		if (m_parent != nullptr)
		{
			CInventoryOwner* nvOwner = m_parent->cast_inventory_owner();
			CInventoryBox* nvBox = m_parent->cast_inventory_box();
			if (nvOwner == nullptr && nvBox == nullptr)
			{
				return false;
			}

			CALifeSimulator* sim = const_cast<CALifeSimulator*>(&ai().alife());
			if (CSE_Abstract* s_obj = CALifeSimulator__spawn_item2(
					sim,
					m_power_cell.section.c_str(),
					m_parent->Position(),
					m_parent->cast_game_object()->ai_location().level_vertex_id(),
					m_parent->cast_game_object()->ai_location().game_vertex_id(),
					m_parent->ID()
				))
			{
				await_object_id = s_obj->ID;

				return true;
			}
		}
	}

	return false;
}

void IPowerManager::OnFrame()
{
	if (!initialized)
	{
		return;
	}

	DrainPower();

	if (!GetUsePowerCell())
	{
		return;
	}

	if (await_object_id != u32(-1))
	{
		if (CObject* co = Level().Objects.net_Find(await_object_id))
		{
			if (CInventoryItem* io = co->cast_inventory_item())
			{
				io->SetCondition(((m_power_cell.current_power * 100) / m_power_cell.max_power) / 100);
				if (PowerCell* pcell = smart_cast<PowerCell*>(io))
				{
					pcell->SetPowerCellData(m_power_cell);
					await_object_id = u32(-1);
					is_power_cell_installed = false;
					m_power_cell.current_power = 0;
					m_power_cell.max_power = 0;
				}
			}
		}
	}
}

bool IPowerManager::FillUseActions(CUIPropertiesBox* box, const UseActionContext& context)
{
	if (box == nullptr)
	{
		return false;
	}

	bool added = false;

	if (GetUsePowerCell() && IsPowerCellInstalled())
	{
		box->AddItem(
			"detach_power_cell",
			nullptr,
			DETACH_POWER_CELL
		);

		return true;
	}

	const static bool enable_power_cell_context_menu =
		EngineExternal()[EEngineExternalGame::EnablePowerCellContextMenu];

	if (!enable_power_cell_context_menu ||
		!GetUsePowerCell() ||
		context.inventory == nullptr)
	{
		return false;
	}

	xr_map<xr_string, PowerCell*> best_by_section;

	for (PIItem candidate : context.inventory->m_ruck)
	{
		PowerCell* power_cell = smart_cast<PowerCell*>(candidate);
		if (power_cell == nullptr ||
			power_cell->parent_id() != context.owner->object_id())
		{
			continue;
		}

		const PowerCellData candidate_data =
			power_cell->GetPowerCellData();

		// Проверка совместимости с устройством или PowerBank.
		if (!IsPowerCellInWhiteList(candidate_data.section))
		{
			continue;
		}

		const xr_string section = candidate_data.section.c_str();
		const auto found = best_by_section.find(section);

		if (found == best_by_section.end())
		{
			best_by_section.emplace(section, power_cell);
			continue;
		}

		PowerCell* current_best = found->second;
		const PowerCellData best_data =
			current_best->GetPowerCellData();

		const float candidate_charge =
			candidate_data.max_power > 0.0f
				? candidate_data.current_power /
					  candidate_data.max_power
				: 0.0f;

		const float best_charge =
			best_data.max_power > 0.0f
				? best_data.current_power /
					  best_data.max_power
				: 0.0f;

		if (candidate_charge > best_charge)
		{
			found->second = power_cell;
		}
	}

	for (const auto& [section, power_cell] : best_by_section)
	{
		const PowerCellData data =
			power_cell->GetPowerCellData();

		const int charge_percent =
			data.max_power > 0.0f
				? iFloor(
					  data.current_power /
						  data.max_power *
						  100.0f +
					  0.5f
				  )
				: 0;

		shared_str text =
			g_pStringTable->translate(
				"st_install_power_cell"
			);

		text.printf(
			"%s %s (%d%%)",
			text.c_str(),
			power_cell->NameItem(),
			charge_percent
		);

		box->AddItem(
			text.c_str(),
			power_cell,
			ATTACH_POWER_CELL
		);

		added = true;
	}

	return added;
}

bool IPowerManager::OnProcessPropertiesBoxClicked(CUIPropertiesBox* m_UIPropertiesBox)
{
	if (GetUsePowerCell() && IsPowerCellInstalled())
	{
		switch (m_UIPropertiesBox->GetClickedItem()->GetTAG())
		{
			case DETACH_POWER_CELL:
				UnistallPowerCell();
				return true;
		}
	}

	return false;
}

void IPowerManager::net_save(NET_Packet& output_packet)
{
	output_packet.w_u8(is_power_cell_installed ? 1 : 0);
	output_packet.w_u8(is_enabled ? 1 : 0);
	output_packet.w_stringZ(m_power_cell.section);
	output_packet.w_float(m_power_cell.current_power);
	output_packet.w_float(m_power_cell.max_power);
	output_packet.w_u32(await_object_id);
}

void IPowerManager::net_load(IReader& input_packet)
{
	is_power_cell_installed = input_packet.r_u8() == 1 ? true : false;
	is_enabled = input_packet.r_u8() == 1 ? true : false;
	input_packet.r_stringZ(m_power_cell.section);
	m_power_cell.current_power = input_packet.r_float();
	m_power_cell.max_power = input_packet.r_float();
	await_object_id = input_packet.r_u32();
}
