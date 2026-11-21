#pragma once

#include "inventory_item_object.h"

class CTacticalFlashLaser final : public CInventoryItemObject
{
	using inherited = CInventoryItemObject;
public:

	CTacticalFlashLaser() = default;
	~CTacticalFlashLaser() override = default;

	void Load(const char*) override;
	void UpdateCL() override;
	void OnH_A_Chield() override;
	void OnMoveToRuck(const SInvItemPlace& prev) override;
	void UpdateLights();

	void save(NET_Packet& output_packet) override;
	void load(IReader& input_packet) override;
	void Serialize(ISaveObject& Object) override;

	void SwitchLaser(bool status);
	void SwitchTorch(bool status);

	bool TorchStatus = false;
	bool LaserStatus = false;
};