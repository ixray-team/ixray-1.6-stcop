#pragma once

#include "inventory_item_object.h"
#include "../xrScripts/script_export_space.h"
#include "IPowerManager.h"
#include "EffectorNightVision.h"

// CLSID_IITEM_NIGHT_VISION D_NVG  NVG_SLOT

class CNightVisionEffector;

class CNVG final : 
	public CInventoryItemObject,
	public IPowerManager
{
private:
	bool m_equiped = false;
	bool m_is_enabled = false;
	bool m_last_state = false;
	shared_str m_night_vision_effector_section;
	CNightVisionEffector* m_nvg_effector;

public:
	CNVG() = default;
	~CNVG() = default;
	virtual void Load(LPCSTR section) override;
	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;
	virtual void UpdateCL() override;
	virtual void OnFrame() override;

	bool IsNvgEnabled() { return m_is_enabled; }
	void SetNvgEnabled(bool state) { m_is_enabled = state; }

	bool IsNvgEquiped() { return m_equiped; }
	void SetNvgEquiped(bool state) { m_equiped = state; }
	shared_str GetNightVisionSection() { return m_night_vision_effector_section; }

	void OnNvgStart();
	void OnNvgLoop();
	void OnNvgStop();

	bool StartNvg();
	bool StopNvg();
	void NVGSwitch(bool state);
	

	void OnItemRuck();
	void OnItemToSlot();
	void OnItemDrop();

	bool OnVNGPropertiesBoxForUsing(CUIPropertiesBox* m_UIPropertiesBox);
	bool OnVNGPropertiesBoxClicked(CUIPropertiesBox* m_UIPropertiesBox);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};