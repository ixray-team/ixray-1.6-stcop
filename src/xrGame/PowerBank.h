#pragma once

#include "inventory_item_object.h"
#include "../xrScripts/script_export_space.h"
#include "PowerCell.h"
#include "pch_script.h"
#include "../xrUI/Widgets/UIPropertiesBox.h"
#include "../xrUI/Widgets/UIListBoxItem.h"

class CUIProgressBar;

// CLSID_IITEM_POWER_BANK PWR_BANK
// POWER_BANK_SLOT

struct AwaitAlifeObject {
	u32 id = -1;
	float target_condition = 0.0f;
	PowerCellData pc_data;
	bool used = false;
};

class PowerBank final :
	public CInventoryItemObject,
	public pureFrame
{
private:
	u32 m_max_count_power_cells = 0;
	xr_vector<xr_string> m_allowed_power_cells_sections;
	xr_vector<PowerCellData> m_power_cells;
	xr_vector<AwaitAlifeObject> m_await_objects_apply_params;
	xr_vector<CUIProgressBar*> m_pCellsConditions;

public:
	PowerBank();
	virtual ~PowerBank();

	virtual void Load(const char* section) override;
	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;
	virtual void UpdateCL() override;
	virtual void OnFrame() override;

	bool InsertPowerCell(PowerCell* powerCell);
	void DecreasePower(float value);
	float GetPower();
	float GetCalculatedCondition();
	bool IsEquiped();
	CInventoryItem* GetEquipedPowerBank();
	bool IsPowerCellInWhiteList(shared_str power_cell_section);
	bool OnPropertiesBoxForUsing(CUIPropertiesBox* m_UIPropertiesBox);
	bool OnProcessPropertiesBoxClicked(CUIPropertiesBox* m_UIPropertiesBox);
	void EjectPowerCells();

	void CellUpdate(CUICellItem* oCUICellItem, Ivector2 cell_size, Ivector2 cell_space, Ivector2 itm_grid_size);
	void OnCellsDestroy(CUICellItem* oCUICellItem);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};