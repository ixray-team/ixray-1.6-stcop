#pragma once

#include "inventory_item_object.h"
#include "Descs/AttachmentDesc.h"

struct CCartridge;

class CMagazine final : public CInventoryItemObject
{
	using inherited = CInventoryItemObject;

	const SMagazineDesc* CurrentMagazineDesc = nullptr;

public:
	xr_vector<CCartridge> Cartridges = {};
	u8 CurrentSize = 0u;

	CMagazine() = default;
	~CMagazine() override = default;

	void Load(const char*) override;

	bool net_Spawn(CSE_Abstract*) override;
	void net_Import(NET_Packet&) override;
	void net_Export(NET_Packet&) override;

	u32 Cost() const override;
	float Weight() const override;

	const SMagazineDesc& MagazineDesc() const { VERIFY(CurrentMagazineDesc); return *CurrentMagazineDesc; }
};