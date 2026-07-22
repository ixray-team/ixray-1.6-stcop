#include "StdAfx.h"
#include "PowerBank.h"
#include "PowerCell.h"
#include "Actor.h"
#include "Inventory.h"
#include "ai_object_location.h"
#include "alife_simulator_base.h"
#include "alife_simulator.h"
#include "../xrUI/Widgets/UIProgressBar.h"
#include "UICellItem.h"
#include "../xrUI/UIXmlInit.h"

PowerBank::PowerBank()
{
	Device.seqFrame.Add(this, REG_PRIORITY_LOW - 5000);
}

PowerBank::~PowerBank()
{
	Device.seqFrame.Remove(this);
}

void PowerBank::Load(const char* section)
{
	CInventoryItemObject::Load(section);

	m_flags.set(FCanStack, false);
	m_max_count_power_cells = READ_IF_EXISTS(pSettings, r_u32, section, "max_count_power_cells", 0);

	m_allowed_power_cells_sections.clear();
	if (pSettings->line_exist(section, "allowed_power_cells_sections"))
	{
		m_allowed_power_cells_sections = xr_string(pSettings->r_string(section, "allowed_power_cells_sections")).RemoveWhitespaces().Split(',');
		if (!m_allowed_power_cells_sections.empty())
		{
			for (xr_string& section : m_allowed_power_cells_sections)
			{
				m_HiglightRelatedItemSections.push_back(section.c_str());
			}
		}
	}
}

bool PowerBank::IsPowerCellInWhiteList(shared_str power_cell_section)
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

bool PowerBank::InsertPowerCell(PowerCell* powerCell)
{
	if (m_power_cells.size() < m_max_count_power_cells)
	{
		m_power_cells.push_back(powerCell->GetPowerCellData());
		powerCell->DestroyObject();

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

void PowerBank::EjectPowerCells()
{
	if (m_power_cells.empty())
	{
		return;
	}

	if (CObject* obj_parent = H_Parent())
	{
		CInventoryOwner* nvOwner = obj_parent->cast_inventory_owner();
		CInventoryBox* nvBox = obj_parent->cast_inventory_box();
		if (nvOwner == nullptr && nvBox == nullptr)
		{
			return;
		}

		CALifeSimulator* sim = const_cast<CALifeSimulator*>(&ai().alife());

		for (PowerCellData& power_cell : m_power_cells)
		{
			if (CSE_Abstract* s_obj = CALifeSimulator__spawn_item2(
				sim,
				power_cell.section.c_str(),
				obj_parent->Position(),
				obj_parent->cast_game_object()->ai_location().level_vertex_id(),
				obj_parent->cast_game_object()->ai_location().game_vertex_id(),
				obj_parent->ID()
			))
			{
				AwaitAlifeObject& await_object = m_await_objects_apply_params.emplace_back();
				await_object.id = s_obj->ID;
				await_object.pc_data = power_cell;
				await_object.target_condition = ((power_cell.current_power * 100) / power_cell.max_power) / 100;
				await_object.used = false;
			}
		}

		m_power_cells.clear();
	}
}


void PowerBank::UpdateCL()
{
	CInventoryItemObject::UpdateCL();
}

void PowerBank::OnFrame()
{
	if (!m_await_objects_apply_params.empty()) 
	{
		bool all_used = true;
		for (AwaitAlifeObject& await_object : m_await_objects_apply_params)
		{
			if (await_object.used)
			{
				continue;
			}

			all_used = false;
			if (CObject* co = Level().Objects.net_Find(await_object.id))
			{
				if (CInventoryItem* io = co->cast_inventory_item())
				{
					io->SetCondition(await_object.target_condition);
					if (PowerCell* pcell = smart_cast<PowerCell*>(io))
					{
						pcell->SetPowerCellData(await_object.pc_data);
						await_object.used = true;
					}
				}
			}
		}

		if (all_used)
		{
			m_await_objects_apply_params.clear();
		}
	}
}

float PowerBank::GetPower()
{
	float result = 0.0f;
	for (PowerCellData& power_cell : m_power_cells)
	{
		result += power_cell.current_power;
	}
	
	return result;
}

float PowerBank::GetCalculatedCondition()
{
	float last_power = GetPower();
	float max_power = 0.0f;
	for (PowerCellData& power_cell : m_power_cells)
	{
		max_power += power_cell.max_power;
	}

	if (max_power > 0 && last_power > 0)
	{
		return ((last_power * 100) / max_power) / 100;
	}

	return 0.0f;
}

void PowerBank::DecreasePower(float value)
{
	if (m_power_cells.empty())
	{
		return;
	}

	if (value <= 0.0f)
	{
		return;
	}

	u32 count_not_empty = 0;
	for (PowerCellData& power_cell : m_power_cells)
	{
		if (power_cell.current_power > 0)
		{
			count_not_empty += 1;
		}
	}

	for (PowerCellData& power_cell : m_power_cells)
	{
		if (power_cell.current_power > 0)
		{
			power_cell.current_power -= (count_not_empty > 1) ? (value / count_not_empty) : value;
			clamp(power_cell.current_power, 0.0f, power_cell.max_power);
		}
	}
}

bool PowerBank::IsEquiped()
{
	return GetEquipedPowerBank() != nullptr;
}

CInventoryItem* PowerBank::GetEquipedPowerBank()
{
	if (CActor* act = Actor())
	{
		if (PIItem item_from_slot = act->inventory().ItemFromSlot(POWER_BANK_SLOT))
		{
			return item_from_slot->cast_inventory_item();
		}
	}

	return nullptr;
}

void PowerBank::save(NET_Packet& output_packet)
{
	CInventoryItemObject::save(output_packet);

	u32 cnt = m_power_cells.size();
	output_packet.w_u32(cnt);

	if (cnt > 0)
	{
		for (size_t i = 0; i < cnt; i++)
		{
			output_packet.w_stringZ(m_power_cells[i].section);
			output_packet.w_float(m_power_cells[i].max_power);
			output_packet.w_float(m_power_cells[i].current_power);
		}
	}
}

void PowerBank::load(IReader& input_packet)
{
	CInventoryItemObject::load(input_packet);

	u32 cnt = input_packet.r_u32();
	if (cnt > 0)
	{
		m_power_cells.reserve(cnt);
		for (size_t i = 0; i < cnt; i++)
		{
			PowerCellData& cell_data = m_power_cells.emplace_back();
			input_packet.r_stringZ(cell_data.section);
			cell_data.max_power = input_packet.r_float();
			cell_data.current_power = input_packet.r_float();
		}
	}
}

bool PowerBank::OnPropertiesBoxForUsing(CUIPropertiesBox* m_UIPropertiesBox)
{
	if (m_power_cells.size() > 0)
	{
		m_UIPropertiesBox->AddItem(
			"detach_power_cell",
			nullptr,
			DETACH_POWER_CELL
		);

		return true;
	}

	return false;
}

bool PowerBank::OnProcessPropertiesBoxClicked(CUIPropertiesBox* m_UIPropertiesBox)
{
	if (m_power_cells.size() > 0)
	{
		switch (m_UIPropertiesBox->GetClickedItem()->GetTAG())
		{
		case DETACH_POWER_CELL:
			EjectPowerCells();
			return true;
		}
	}

	return false;
}


using namespace luabind;
#pragma optimize("s",on)
void PowerBank::script_register(lua_State* L)
{
	module(L)
		[
			class_<PowerBank, CGameObject>("PowerBank")
				.def(constructor<>())
		];
}