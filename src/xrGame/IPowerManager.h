#pragma once
#include "inventory_item_object.h"
#include "../xrScripts/script_export_space.h"
#include "PowerCell.h"
#include "pch_script.h"
#include "..\xrUI\Widgets\UIPropertiesBox.h"
#include "..\xrUI\Widgets\UIListBoxItem.h"
#include "UICellItem.h"
#include "PowerBank.h"

class CUIProgressBar;

class IPowerManager: 
	public pureFrame
{
private:
	bool use_power_cells = false;
	bool use_power_bank = false;
	float power_drain_value = 0.f;
	PowerCellData m_power_cell;
	bool is_power_cell_installed = false;
	CInventoryItem* selfObject = nullptr;
	CObject* m_parent = nullptr;
	u32 await_object_id = -1;
	xr_vector<xr_string> m_allowed_power_cells_sections;
	bool initialized = false;
	bool is_enabled = false;
	CUIProgressBar* m_progress_bar;

public:
	IPowerManager();
	~IPowerManager();

	void SetEnabled(bool enabled) { is_enabled = enabled; }
	PowerBank* GetPowerBank();

	void SetUsePowerCell(bool state) { use_power_cells = state; };
	bool GetUsePowerCell() { return use_power_cells; }

	void SetUsePowerBank(bool state) { use_power_bank = state; };
	bool GetUsePowerBank() { return use_power_bank; }

	CInventoryItem* GetSelfObject() { return selfObject; }
	void SetSelfObject(CInventoryItem* item, CObject* parent) { selfObject = item; m_parent = parent; initialized = true; }

	float GetPowerDrainValue() { return power_drain_value; }
	void SetPowerDrainValue(float value) { power_drain_value = value; }

	bool IsPowerCellInstalled() { return is_power_cell_installed; }

	bool IsAllow() { return GetUsePowerCell() || GetUsePowerBank(); }
	bool IsPowerCellInWhiteList(shared_str power_cell_section);

	float GetLeftPowerValue();

	void Load(const char* section, CInventoryItem* iitem);

	virtual void OnFrame() override;
	void DrainPower();

	void net_save(NET_Packet& output_packet);
	void net_load(IReader& input_packet);

	bool IstallPowerCell(PowerCell* oPowerCell);
	bool UnistallPowerCell();
	bool OnPropertiesBoxForUsing(CUIPropertiesBox* m_UIPropertiesBox);
	bool OnProcessPropertiesBoxClicked(CUIPropertiesBox* m_UIPropertiesBox);
	void CellUpdate(CUICellItem* oCUICellItem, Ivector2 cell_size, Ivector2 cell_space, Ivector2 itm_grid_size);
	void OnCellsDestroy(CUICellItem* oCUICellItem);
};