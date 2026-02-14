#pragma once

#include "inventory_item_object.h"
#include "../xrScripts/script_export_space.h"

// CLSID_IITEM_POWER_CELL PWR_CELL

struct PowerCellData {
	shared_str section;
	float max_power = 0.0f;
	float current_power = 0.0f;
};

class PowerCell final : public CInventoryItemObject
{
private:
	PowerCellData m_power_cell_data = {};

public:
	PowerCell() = default;
	~PowerCell() = default;

	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;

	virtual void Load(LPCSTR section) override;
	PowerCellData GetPowerCellData() { return m_power_cell_data; };
	void SetPowerCellData(PowerCellData power_cell_data) { m_power_cell_data = power_cell_data; };

	DECLARE_SCRIPT_REGISTER_FUNCTION
};