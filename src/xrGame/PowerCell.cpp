#include "StdAfx.h"
#include "PowerCell.h"
#include "UICellItem.h"

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
